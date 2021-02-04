## Q Overview

### Q versus Python

I've called Q a dynamic, interpreted language, which it is. But compared to Python, very little is actually dynamic: only the types of variables.

And while Python offers full classes, overrides via class methods, immutable tuples and strings, integrated big integers, full dicts, exceptions, generators, iterators, executable and conditional 'def', 'class' and 'import' statements, eval/exec functions, rebinding of any names at any time, decorators, lambdas and closures, * and ** parameter handling, Numpy plus huge numbers of other libraries, arbitrary run-time attributes, 'with', 'collections' data types, rationals, decimals plus a whole raft of other advanced features, plus myriad implementations including tracing-JIT optimised versions, Q has none of that.

Python is huge and incredibly comprehensive. Q is tiny and fairly limited, especially in connectivity as mentioned in [Overview](Overview.md).

I'm not selling Q very well so far. But if you want to use Python (or any of a dozen other such languages) like everyone else then do so.

I know Q is still capable of plenty: I've used much cruder, more limited versions in 80s and 90s to add functionality to my CAD applications, and they were a joy to use. You don't *need* all that stuff that comes with Python. There is an advantage to working with a small language that you know all the ins and outs of.

And Q does have plenty of small features of its own that can make working with it very comfortable.

### Some Q Features

Here is a selection of features, mainly chosen to compare favourably with Python. Python can probably be made to do some of this stuff, but sometimes via half a dozen, incompatible, bolted-on modules none of which do the job properly, or which do so with complete overkill. Doing things way is fine; but doing it effortlessly without having to download extras is harder.

#### Blazing Fast Bytecode Compiler

Q programs need all modules known at compile-time, and all must be byte-code compiled before execution can start. This needs a very fast compiler. The one used works at between 500,000 and 1,000,000 lines per second.

#### Compiled Programs are Single Bytecode Files

Compile the lead module of any application, and the result will always be a single bytecode file representing the whole program. Distributing that involves two files: the bytecode file, and the interpreter, which is always a single file. (And there are ways to combine those two.)

Contrast the vast sprawling Python implemementations, and applications consisting of multiple assorted files, all of which requires special programs to be installed to achieve a tidy, compact distributable file.

Q programs are also designed to be distributed as bytecode rather than source.

#### Common Syntax

The syntax of Q, and of the implementation language M, is 95% common. Compare C and Python.

#### Indents Not Significant

All code blocks require a closing delimiter. This means that indentations are not crucial, it's always clear where a block ends, and some errors (such as accidentally deleting a tab) don't result in undetectable changes in logic.

That different kinds of statement can use different closing delimiters is another way to detect block errors.

#### Named Constants

A very simple feature that many languages eschew: assign a name to a numeric constant:
```
  const a = 100
  const b = 200
  const c = a+b
```
Such constants can help reduce expressions, be used in switch cases, or anywhere that a compile-time expression is required. It is impossible to assign to such names.

#### Scope Control

Names declared in a module are local to that module, unless exported:
```
   var abc              # not exported
   global var def       # exported
```
In Python, all such names are visible from any module that imports this one.

#### Automatic Qualifiers

If module A exports function F, you don't need to access is as A.F(), but can just use F(). This is provided there is no ambiguity with any other exported F.

#### Numeric Separators

Numbers can be split up using any of _ ' and \`. (I understand recent Pythons now have separators.)

#### Numeric Bases

Constants can specify any base from 2 to 10, or 16 (The other language allows anything from 2 to 16, may need to upgrade this).

This applies to both integers and floating point.

#### Numeric suffixes

This mean being able to write values such as '4 million'. (When used for CAD, it was possible to write 4mm + 5m + 6cm to end up with 5064.0. Now, a space is required, and those physical units have not yet been reinstated.) The suffix is a multiplier that scales the number.

#### Case Insensitive

This language and M are both case-insensitive, which is a more user-friendly way to do things.

#### Character Constants

Constants such 'A' or 'ABCDEFGH' are integer constants in Q. (In Python, they are strings. To convert to integers,
requires typing ord('A'), a runtime operation.)

#### Procs and Functions

Functions that don't return a result are called Procs and need a **proc** keyword rather than **function**. This makes you think more carefully about what such a routine does, and can help catch errors.

#### Start and Main Functions

Unlike most languages of this kind, declarations (of functions, types etc) are not executable statements. Executable code
must go within inside a function (other than initialisation data for module-scope variables, which is executed at program load-time).

The executable code other than declarations that, in Python, would be scattered through the module, must be gathered into a start() or main() function, or both.

start() is automatically called when the module is 'imported'. Unless this is the main program, the lead module, then main() is called if one has been provided.

This takes care of the 'if __name__="main"' or whatever is used in Python.

#### Static Variables

These are variables inside a function, which are declared with a 'static' attribute. They are initialised once at load-time,
then keep whatever values they have been assigned in-between calls.

#### Enumerations

These are very simple affairs, one step up from 'const':
```
    enum (red, green, blue)        # same as const red=1, green=2, blue=3
    enum (a=100, b, c)             # same as const a=100, b=101, c=102
```
They declare only named integer constants. These names are 'open' and can clash with each other. To wrap a type around them:
```
    type colours = (red, amber, green)
```
Now they have to be disambiguated with colours.red, etc. For a more advanced treatment, see Tabledata.

#### Goto or Go To

Yes, **goto** is in all my languages. You will appreciate it when you need to use it. (Eg. porting an algorithm that either used goto, or uses control flow that can only be expressed with goto.)

#### Reference Parameters

These allow a function to change the value of its callers data:
```
    proc double(&a) =
        a  := a*2
     end
```
This doubles the caller's argument. Python can only do this for limited types such as lists, and can only modify them, it can't change the list to something else by assigning to the caller's argument:
```
    proc exchange(&a, &b) =
      temp := a; a := b; b := temp
    end
    ...
    a := 1234
    b := "dog"
    exchange(a,b)
    print a, b            # output is dog 1234
```
#### Optional, Default and Keyword Parameters

These I believe are available in Python with some work (although I remember default parameters had some funny issues). But in Python, they are dealt with at runtime.

In Q, the compiler will sort out what needs to be passed, without any extra runtime overhead.

#### Pointers

These are a little used, low-level feature (pointers are used internally to implemenent reference parameters and operations such as +:= and swap). Like goto, when you need them, you need them.

They might be used more, if working with arrays and structs, and need a pointer to an array or struct or one of their elements for interfacing with foreign functions.

#### Address-of Operator

Used to set up pointers to objects or data:
```
     p := ^a
     q := &a
```
The ^ form is to create a pointer to variant object (so a pointer to any type that a variant can hold).

The & form creates a pointer to the underlying packed data, and is a pointer to a specific packed type. So here:
```
     s := "ABC"
     p := ^s       # p is pointer-to-variant
     q := &s       # q is pointer-to-byte
     println p^    # (^ means dereference here) display "ABC"
     println q^    # display 65
     ++q
     println q^    # display 66
```

#### Extra Loop Controls

Q allows **exit**, **next**, **redo** and **restart*. **exit** corresponds to **break** in Python.

All of them work with nested loops (unlike pretty much every other language):
```
    to n do
        to m do
            exit all when a>b
        od
    od
```
#### Conditional Statements

Some simple control-flow statements - **exit** **next** **redo** **restart** **return** **goto** **stop** - can be optionally be followed by a condition:
```
    exit if x=7
    return 0 when s=""
    stop unless status=OK
```
This save wrapping a if-statement around them, and makes the statement more prominent.
    
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
 These are covenient to use, especially to N which otherwise requires a for-loop with a dummy variable.
  
 #### For, Forall, Foreach Statements
 
 **forall** in Q corresponds to **for** in Python, in looping over values, but is a simpler implementation. For example:
 ```
     forall i,x in A do
         ...
 ```
 The extra 'i' control varible tells it to expose the loop index. (Needs enumerate() in Python.)
 
 **for** in Q interates over two integer values only:
 ```
     for i to 10 do               #  1 .. 10 inclusive
     for i:=A to B do             #  A .. B  inclusive
     for i:=9 downto 1 by 2 do    #  iterates over 9,7,5,3,1
     for i in A..B do             # alternate syntax
 ```
 
While **forall** iterates over indexable objects, multiple values where A\[i\[ is allowed, there is also **foreach** which iterates over values that are considered a single entity, such as a record, integer, or string. Ones where A.\[i\] needs to be used (see Dot Indexing).

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
'when' expressions must be integer constants (actual constants, named constants, or expressions reduced to a constant) known at compile-time, which makes it awkward to do in Python. This is always implemented as a jump-table.

A variation is **doswitch**, which loops.

#### Case Statement

If switch-like statement is needed, but the when expressions are not integers, not constants, or the range of values is too wide, then a **case** statement can be used:
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
This record is always a list of 3 named elements, although they can also be indexed:
```
    d := new(record)         # d will be (void, void, void)
    e := date(25,12,1999)    # d is a new date record
    println e                # display (25,12,1999)
    println e.month          # display 12
    println d.[3]            # display 1999
```
Field names are not attributes (in Python, you can write d.monthabsxyz=0, and it will work without error).
Using the wrong field is an error. Fields are mutable.

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
However, negative elements are not allowed. The set consists of an array of bits indexed from 0, up to the largest value present.

#### Packed Types
While the standard numeric types are int, word and real (all 64 bits), Q also supports 'packed types', which are fixed-size types as used in languages such as C. The following packed types are available:
```
    i8, i16, i32, i64, u8, u16, u32, u64, r32, r64
    int8, int16, int32, int64, word8, word16, word32, word64, real32, real64
    byte, c8, intm, wordm, refm      # last three depended on host machine word/pointer size
    u1, u2, u4, bit, bit2, bit4      # for bit arrays
```
such types are used when constructing arrays, structs, pointers to such types, and interfacing to foreign functions.

#### Arrays and Bit Arrays
Normal lists are variants, which are can be of any type. Arrays are lists of the same packed type:
```
    a := new(array, byte, 1'000'000, 0
```
a is a byte-array of 1 million elements indexed 1..1000000, initialised to 0. It will occupy about 1MB, unlike a list which would need 16MB. Arrays can be of any packed types, or even of user types (structs and other arrays).

A bit is created the same way:
```
    b := new(array, bit, 100 million,0)   # 'bits' can be used in place of 'array'
```
b has 100 million elements, and occupies 12.5MB (a list of 100M elements needs 1600MB).

Bit arrays can have elements of 1, 2 or 4 bits.

#### Structs
A struct is a C-like record (although always constructed with 'pack(1)' as padding is never added). It is defined like this:
```
    type cdate = struct
        byte day, month
        int16 year
    end
```
cdata occupies 4 bytes. It is used just like a record (except elements can't be indexed at present):
```
     d := cdata(15,8,2018)
     println d.year          # display 2018
     println d               # display (15,8,2018)
```

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
Slicing is well-known, it can be used to extra substrings sequences from any indexable object:
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
x is set to one of a,b,c... depending on n=1,2,3 (as Q is 1-based). When n is out of range, then the default z is assigned. Elements can be any expressions. It looks a little like indexing a list:
```
    x := (a,b,c,...)[n]
```
except that only *one* expression is evaluated (not all of them as in this list), and there is a default value when n is out of range. This ought to work as an lvalue (but I need to check that).

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
Also included are constants such as *pi* which, unlike Python, can't be assigned to.

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
Table data is way of declared linked, parallel arrays of data, usually with enums declared in the first column. Example:

tabledata() []ichar colournames, []word colourvalues =
    (red,       $,      0xFF'00'00),
    (green,     $,      0x00'FF'00),
    (blue,      "Blue", 0x00'00'FF),
end

(See Table Data in [mdocs.md](mdocs.md) which expands on this.)

#### Print Statements
Q retains print and println as actual statements, meaning no parentheses needed.

These days, 'print' in a scripting language will give new a newline at the end whether you want it or not, and you have to find out what scheme is used to suppress it.

Q uses **print** to output things without a newline, and **println** to add a newline. Very simple. (Although Q does insert spaces between items, and has an ugly scheme of its own to suppress that space.)

**print** makes use of the tostr() function, which is useful when you wnt to use the string conversions inside expressions.

As for printing to a file handle f:
```
    println @f,a,b,c
```
For quick diagnostic displays:
```
   println =a, =b, =c         # display something like A=10 B=20 C=30
```

#### Read Statements
Many language no longer have simple line input statements like this:
```
    print "Type 3 numbers: "
    readln a, b, c
    println "You typed:",a,b,c
```
Try that 'readln a,b,c' in Python.

#### Keyboard Entry
For simple use cases, Q has waitkey() to input a single key from the keyboard, and testkey() to tell you if a key has been pressed:
```
	println "Press any key to stop:"
	repeat print "*" until testkey()
	println "\nYou pressed", chr(waitkey())
```
Using single-key entry is out of function in many languages, at least in console applications, most likely due to being built on top of C, where normally don't deliver any input until Enter is pressed. There are ways around it, but here it is built-in.

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

#### Big Numbers
Q's big integers are poorly integrated: they are a separate type from normal integers (usally 64-bit signed, sometimes unsigned). Arithmetic doesn't automatically overflow into big integers. Mixed arithmetic exists but in a limited form.

Since big integers are actually rarely needed, I wanted to keep most arithmetic efficient for the 99.99% of operations that will fit into 64 bits.

The implementation of big integers is not fast either. However, they do also deal with big floating point numbers (at present,
experimental), and are now called Bignums rather than Bigints. And their implementation uses decimal rather than binary (Python has a whole add-on type called Decimal to achieve the same.)

#### Unsigned integers
As mentioned, the default integer type (unless a number is too big) is int64. But an unsigned word64 type (ie. uint64) also exists. This helps with some algorithms ported from C. And allows numbers between 2**63 and 2**64-1 without needing a Bignum.

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

Q's interpreter also has an accelerator that can be switched in when executing on x64 using M's native code. It can make some code 2-3 times as fast.

The performance of PyPy is variable, and can be very good with lots of loops. But I suspect it is not still executing bytecode at that point as Q's accelerator still does. PyPy is also fantastically more complex than Q's interpreter, whose acceletator is part of the same program:
```
    pc program           # normal execution
    pc -asm program      # use accelerator
```
Of course, performance it at least a magnitude slower than statically typed and compiled code.

### Conclusion

This is a list of around 50 features that are built-in to the language and can be effortlessly used. 

It makes actual programming (not just invoking a series of library calls to do the work), much easier.

Even the currently incomplete list (there's a lot of stuff in there!) makes me feel happier about using this language.
