## Q / Python Comparison

A selection of 50 or so features (some of which I consider fundamental) effortlessly available out-of-the-box with Q, which either don't exist with Python, or only possible with cumbersome add-ons, sometimes half-a-dozen competing versions (eg for named tuples), none of which are as effective.

(Q is my own scripting language, not as dynamic as Python, but that fact makes some of these features possible. Python is *too* dynamic.)

#### Single File Implementation

The Q compiler/interpreter is a single executable, currently 0.8MB, which includes the standard library (admittedly not too big, however it will still be a single file).

#### Compiled Programs are Single Bytecode Files

Compile the lead module of any application, and the result will always be a single bytecode file representing the whole program. Distributing that involves two files: the bytecode file, and the above interpreter. (And those can be combined into one file.)

Contrast the vast sprawling Python implemementations, and applications consisting of multiple assorted files, all of which requires special programs to be installed to achieve a tidy, compact distributable file.

#### Blazing Fast Bytecode Compiler

Q programs need all modules known at compile-time, and all must be byte-code compiled before execution can start. This needs a very fast compiler. The one used works at between 500,000 and 1,000,000 lines per second. This makes run-from-source viable too, but usually programs are run from pre-compiled byte-code.

#### Common Syntax

The syntax of Q, and of the implementation language M, is 95% common. Compare C and Python.

#### Indents Not Significant

All code blocks in Q require a closing delimiter. This means that indentations are not crucial, it's always clear where a block ends, and some errors (such as accidentally deleting a tab) don't result in undetectable changes in logic.

That different kinds of statement can use different closing delimiters is another way to detect block errors.

#### Named Constants

A very simple feature that many languages eschew: assign a name to a numeric constant:
```
  const a = 100
  const b = 200
  const c = a+b
```
Such constants can help reduce expressions, be used in switch cases, or anywhere that a compile-time expression is required. It is impossible to assign to such names. Compare with Python's:

    math.pi = "Pie"

#### Scope Control

Names declared in a module are local to that module, unless exported:
```
   var abc              # not exported
   global var def       # exported
```
In Python, all such variables, types anf functions are visible from any module that imports this one.

#### Numeric Separators

Numbers can be split up using any of _ ' and \`. (I understand recent Pythons now have separators.)

#### Numeric Bases

Constants can specify any base from 2 to 16. This applies to both integers and floating point.

#### Numeric suffixes

This mean being able to write values such as '4 million' (and without 'million' infringing on user name space)

#### Case Insensitive

Q is case-insensitive, which is a more user-friendly way to do things especially in an informal scripting language.

#### Character Constants

Constants such 'A' or 'ABCDEFGH' are integer constants in Q. (In Python, they are strings. To convert to integers,
requires typing ord('A'), a runtime operation.)

#### Procs and Functions

Functions that don't return a result are called Procs and need a **proc** keyword rather than **function**. This makes you think more carefully about what such a routine does, and can help catch errors.

#### Start and Main Functions

Unlike most languages of this kind, declarations (of functions, types etc) are not executable statements. Executable code
must go within inside a function (other than initialisation data for module-scope variables, which is executed at program load-time).

The entry point for a module is the function start(); if present, then that is called upon loading. If the module is the main modulem and a main() function is supplied, it will call that instead.

This takes care of the 'if __name__="main"' or whatever is used in Python.

#### Static Variables

These are variables inside a function, which are declared with a 'static' attribute. They are initialised once at load-time,
then keep whatever values they have been assigned in-between calls. Doing this with Python is tricky.

#### Enumerations

These are very simple affairs, one step up from 'const':
```
    enum (red, green, blue)        # same as const red=1, green=2, blue=3
    enum (a=100, b, c)             # same as const a=100, b=101, c=102
```
They declare only named integer constants.

#### Goto

**goto** is in all my languages, because it can be very handy, even if it is rarely used (eg. to port an algorithm using goto or a language that used goto)

#### Reference Parameters

These allow a function to change the value of its caller's data, in a way not possible with Python.

So you could write a swap() function in Q, but not in Python.

#### Optional, Default and Keyword Parameters

These I believe are available in Python (although I remember default parameters had some funny issues). But in Python, they are dealt with at runtime.

In Q, the compiler will sort out what needs to be passed, without any extra runtime overhead (limited when function pointers are used).

#### Pointers

These are a little used, low-level feature (pointers are used internally to implemenent reference parameters and operations such as +:= and swap). But like goto, when you need them, you need them. For example, to pass data to/from external C API functions.

#### Address-of Operator

Used to set up pointers to objects or data:
```
     p := ^a             # pointer to variant
     q := &a             # pointer to underlying packed type
```


#### Extra Loop Controls

Q allows **exit**, **next**, **redo** and **restart*. **exit** corresponds to **break** in Python.

All of them work with nested loops too, unlike pretty much every other language.

#### Conditional Statements

Some simple control-flow statements - **exit** **next** **redo** **restart** **return** **goto** **stop** - can optionally be followed by a condition:
```
    exit if x=7
    return 0 when s=""
    stop unless status=OK
```
This saves wrapping an if-statement around them, and makes the statement more prominent.
    
#### Assignment and Equality Operators

Q uses := for assignment, and = for equality. This allows using := inside expressions with little chance of the confusion there is in language like C.

I understand that Python can now use := inside expressions, but equality is still ==, and = is still needed for regular assigments. So it is still a bit messy.

#### Extra Loop Statements

Q has endless loops and repeat-n-times loops, as well as repeat-until:
```
    do
        stmts
    end
    to N do
        stmts
    end
    repeat
        stmts
    until cond
```
These are covenient to use, especially 'to N' which otherwise requires a for-loop with a dummy variable.
  
#### For, Forall, Foreach Statements
 
**forall** in Q corresponds to **for** in Python, in looping over values, but is a simpler implementation. For example:
```
    forall i,x in A do
        ...
```
The extra 'i' control variable tells it to expose the loop index. (Needs enumerate() in Python.)
 
**for** in Q interates over two integer values only.
 
#### Switch Statement

This is well-known:
```
    switch expr
    when a, b then
       ....
    when c then
       ....
    when d..e, f then
       ....
    else
       ....
    end switch
```
But it requires 'when' expressions to be compile-time expressions, in order to be able to use a fast jump-table.
This makes switch impractical in Python, as any named values must always be looked up at runtime.

A variation is **doswitch**, which loops.

#### Case Statement

If a switch-like statement is needed, but the when expressions are not integers, or are not constants, or the range of values is too wide, then a **case** statement can be used:
```
    case expr
    when a, b then
      ....
    end case            # or just 'esac'
```
However the expressions are tested one by one until the first match. "=" is applied between *expr* and each when-value, or "in" if the value is a range. The looping version is called **docase**.

#### Records
These are simply defined like this:
```
    record date =
       var day
       var month
       var year
    end
```
This record is always a list of 3 named elements, although they can also be indexed.

Field names are not attributes (in Python, you can write d.monthabsxyz=0, and it will work without error).
Using the wrong field is an error. Fields are mutable.

As stated, Python either uses the undisciplined attributes, or uses some variety of named tuple extension.

#### Integer Sets
These are Pascal-style set of bits:
```
     alpha := ['A'..'Z','a'..'z']
     digits := ['0'..'9']
     whitespace := [' ','\t',13,10,12]
     map := [0..16383] + [32768..65535]
     if a in map then
     if c in alpha+digits then ...
```

#### Packed Types
While the standard numeric types are int, word and real (all 64 bits), Q also supports 'packed types', which are a range of fixed-size types as used in languages such as C.

Such types are used when constructing arrays, structs, pointers to such types, and interfacing to foreign functions.

#### Arrays and Bit Arrays

Normal lists are variants, which are can be of any type. Arrays are lists of the same packed type. Bit arrays are sequences of 1, 2 or 4 bits. A billion-element bit array occupies 128MB. (I've no idea what with support Python has for such arrays, if any, but I remember is has byte-arrays.)

#### Structs
A struct is a C-like record (although always constructed with 'pack(1)' as padding is never added). It is defined like this:
```
    type cdate = struct
        byte day, month
        int16 year
    end
```
cdate occupies 4 bytes. It is used just like a record (except elements can't be indexed at present).

This is necessary for foreign function interfacing. Python's version of this is done with addons and is very clunky with structs defined via function calls.

#### Bit/Bitfield Indexing
To select a single bit, or bit sequence, of an int:
```
    a := 0x12345
    println a.[3]         # display bit 3 of a (when bit 0 is lsb) so 0
    println a.[11..4]     # (or 4..11) display bits 4..11, so 52 (ie. 0x34)
```
Can be used for lvalues too:
```
    a.[0] := 0            # set lsb to 0
    println a:"h"         # display 12344
```
#### Slicing
Slicing is well-known, it can be used to extract substrings sequences from any indexable object:
```
    s := "ABCDEFGH"
    a := (10,20,30,40,50,60,70,80)
    println s[3..6]    # (or s.[3..6]) display CDEF
    println a[3..6]    # display (30,40,50,60)
```
In Q, all slices are 'views' of the original data (in Python, slices used to involve copying the data).

#### 2-Way Select
This is C's ?: operator or Python's ... whatever the syntax is (I can never remember):
```
    x := (cond|a|b)
```
x is set to a or b depending on cond. Parenthese are always needed. It can be an lvalue too:
```
    (cond|x|y) := a
```
This sets either x or y to a, depending on cond.

#### N-Way Select
This is an extension of the 2-way select:
```
    x := (n | a,b,c,... |z)
```
x is set to one of a,b,c... depending on n=1,2,3 (as Q is 1-based). When n is out of range, then the default z is assigned. Only one rhs value is evaluated, so this can't easily be emulated in Python.

#### Increment and Decrement
These just the usual ++a and ++b, plus the ability to use them inside expressions, where --a and a-- are different. Python does't have these, although ++a and --b are valid expressions - they just do different things (+(+a) and -(-b)).

#### Swap
Python has a form of swap via multiple assignment, but this one is tidier:
```
    swap(a[i],a[i+1])
```
because each term is only written once. That's more efficient, avoids problems when a term has a side-effect, is immediately obvious that it is a swap without having to double check (try: a\[i\*3+2\], a\[j\*2+3\] = a\[j\*3+2\],a\[i\*2+3\]), and there is less opportunity to get it wrong, as demonstrated in that last example.

#### Maths Operators
A handful of operators are built-in, and they are actual operators, not functions. Which means among other things that no parenthese are needed. The built-in operators are:
```
    sqr
    sqrt
    sin, cos, tan
    asin, acos, atan
    atan2
    floor, ceil, round
    fmod, fract
    lg, ln, log
    sign
```
Also included are constants such as **pi** which, unlike Python, can't be assigned to.

#### User Types
Records have their own syntax. Other types are mainly to define packed types, and are used like this:
```
    type colours = (red, green, blue)
    type handle  = ref byte
    type point   = struct (int32 x,y)
    type mat     = [12]real64
```

#### Type Punning
Normal casting is done with int(x). Type-punning is done with @ like this:
```
    x=1.0
    print int(x)         # display 1
    print int@(x)        # display 4607182418800017408
    print int@(x):"h"    # display 3FF0000000000000
    a:=3FF0000100000000
    print real@(a)       # display 1.00015
```
So an @ cast reinterprets the bits of an expression with no conversion.

#### String Include
String include allows any text or binary file to be incorporated as a string constant:
```
    s := strinclude "zip.exe"
```
This is done at compile-time, so the file data is part of the byte-code.

#### Table Data
Table data is a way of declared linked, parallel arrays of data, usually with enums declared in the first column. Example:

tabledata() []ichar colournames, []word colourvalues =
    (red,       $,      0xFF'00'00),
    (green,     $,      0x00'FF'00),
    (blue,      "Blue", 0x00'00'FF),
end

#### Print Statements
Q retains print and println as actual statements, meaning no parentheses needed.

These days, 'print' in a scripting language will give you a newline at the end whether you want it or not, and you have to find out what scheme is used to suppress it.

Q uses **print** to output things without a newline, and **println** to add a newline. Very simple. (Although Q does insert spaces between items, and has an ugly scheme of its own to suppress that space.)

**print** makes use of the tostr() function, which is useful when you want to use the string conversions inside expressions.

As for printing to a file handle f:
```
    println @f,a,b,c
```
For quick diagnostic displays:
```
   println =a, =b, =c         # display something like A=10 B=20 C=30
```

#### Read Statements
Many languages no longer have simple line input statements like this:
```
    print "Type 3 numbers: "
    readln a, b, c
    println "You typed:",a,b,c
```
Try that 'readln a,b,c' in Python.

#### Keyboard Entry
For simple use cases, Q has waitkey() to input a single key from the keyboard, and testkey() to tell you if a key has been pressed. This is a fiddly add-on in Python.


#### Foreign Functions and DLLs/Shared Libraries
The only mechanism provided to talk to the outside world is via the DLL (shared library) interface.

It is possible to directly declare foreign functions in a language, and specify exactly the library they can be found in:
```
    importdll msvcrt =
        clang function puts(string)int
        clang function printf(string, ...)int
    end
    ...
    printf("hello, world\n")
```
In this example, "msvcrt", the C library used on Windows is special; on Linux it will be mapped to libc.so.6.

#### Host Functions

This is calling native code functions inside the host, either the interpreter, or perhaps an application with
an embedded version of the interpreter. It can be done in two ways:
```
    host proc fna
    fna(10,20,30)          # or:
    host.fnb(30,40)
```
No other declarations are needed. Inside the host, callable functions like this are simply marked as 'export' rather than 'global'; then interface data is automatically generated by the compiler of the interpreter (a feature of the language used), complete with all parameter info.


#### Big Numbers
Q's big integers are poorly integrated: they are a separate type from normal integers (usally 64-bit signed, sometimes unsigned). Arithmetic doesn't automatically overflow into big integers. Mixed arithmetic exists but in a limited form.

Since big integers are actually rarely needed, I wanted to keep most arithmetic efficient for the 99.99% of operations that will fit into 64 bits. It is also useful for many int operations to work like they do in C, where if you shift '1' left enough times, you eventually get zero. In Python, it never will (you'll just exhaust memory with bigger and bigger numbers).

(Note Q's big numbers now also represent floating point, so more like Python's Decimal type.)

#### Unsigned integers
As mentioned, the default integer type (unless a number is too big) is int64. But an unsigned word64 type (ie. uint64) also exists. This helps with some algorithms ported from C. And allows numbers between 2\*\*63 and 2\*\*64-1 without needing a Bignum.

#### Operator and Type Constants
Writing (+) forms a special kind of operator constant. It can be used like this:
```
    op = (*)
    if op = (-) then ...
    println applyop(op, 3,4)       # display 12
```
And writing 'int' by itself produces a type constant:
```
    t = int
    if a.type = int ...
    if a.type = t
```

#### Stop
That is all it is:
```
    stop          # terminate process with code 0
    stop 123      # terminate with code 123
```
It's just very convenient to write.

#### Performance

Integer algorithms are generally faster executing in Q than in CPython.

Q's interpreter also has an accelerator that can be switched in when executing on x64. It can make some code 2-3 times as fast.

The performance of PyPy is variable, and can be very good with lots of loops. But I suspect it is not still executing bytecode at that point as Q's accelerator still does. PyPy is also fantastically more complex than Q's interpreter, whose accelerator is part of the same program:
```
    pc program           # normal execution
    pc -asm program      # use accelerator
```
Of course, performance is at least a magnitude slower than statically typed and compiled code.
