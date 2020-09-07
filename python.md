### Annoying Things about Python

This is from the perspective of my own dynamic language 'Q', so is not entirely fair is Python is considerably more dynamic and higher level.

Yet, it lacks a number of fundamentals that I take for granted, and find their absence from Python a nuisance. Here I won't shy away from mentioned what I own in my Q language.

#### Add-ons to fix every problem

For every complaint about Python, someone will point to some add-on library that gives some half-arsed solution. And an extra dependency. But this is often about things I expect to be build-in.

#### Sprawing depedendcy

Any Python program will have a big, sprawling dependency: a Python installation. Yes there are sometimes add-ons that will leave you package things into one, usually unwieldly executable. (Q has one dependency, an interpeter, about 0.3MB)

#### Packaging Applications

A Python application consists assorted .py modules, that can be in multiple directories. Again, there are various third party solutions. One mentioned on Reddit was to put the files into a ZIP, and run it from there. (Q precompiles any application, no matter how many modules, into ONE byte-code file; just compile the lead module. So distributing an app involves two files, the interpreter, and the bytecode program, and they can be combined into one).

#### Binary Distributions

* Related to that last point, Q programs are intended to be distributed as binary, so the source is kept private. I's sure that are plenty of ways to do that in Python ... (see first point)

#### Indentation

Significant indendentation and missing block-end markers. Sorry, but I need something concrete to tell me I'm at the end of a block, not just the absence of a marker. (Where, if I'm at the bottom of a screen, I can't be sure there aren't more lines if I scroll down further.)

The indentation scheme is fragile (accidentally delete or insert a tab, and you've changed the program but you don't known because the syntax is still valid).

#### Everything is exportable

Top-level names in a module are always public (Q requires a 'global' prefix to mark a name for export.)

#### Literals

Old version of Python didn't have numeric separators (I believe now available). I think they've now also fixed the problem with octal 0100 notation, but only by making 0100 invalid syntax (in Q, 0100 means 100). I don't know whether it allows binary literals. (Q allows bases from 2 to 16 for integers and floats, and can print numbers in any base).

#### Named Constants

No proper named constants; every name you assign to can be reassigned. This means not being able to reduce constant expressions, and would put paid to the possibility of a jump-table-based Switch statement. (Q has this, where 'a' has the value 300 that is impossible to change:)
````
     const a = b+c
     const b = 100
     const c = 200
````

#### Case Sensitive

An unfriendly feature IMO what is purportedly a beginner's language


#### Character Constants

Constants such 'A' or 'ABCDEFGH' are strings. Python needs ord('A') to get the integer code. (In Q they are integer values)

#### Procs and Functions

These are mixed up (as they are in most languages). I consider the difference significant enough to give them different names.

#### Start and Main Functions

You need this business of 'if __main__="main" or some thing thing, to establish that this is a main module. (In Q, just supply a main() proc, and that will be the entry point.)


#### Static Variables inside functions

These are local variables that keep their values across invocations. Not available AFAIK, unless through some obscure hack.

#### Enumerations

There are no SIMPLE enumerations, only complicated addons. (In Q, enum (A,B,C) created named constants A,B,C with values 1,2,3.)


#### No Goto

This is a big deal if trying to port code using goto, porting code with control structures that need to be emulated, or considering using Python as a target language.

#### Reference Parameters

No reference parameters, so you can't write functions such as swap(A,B) or read(A,B,C)

#### Optional, Default and Keyword Parameters

These I believe are available in Python with some work (although I remember default parameters had some funny issues). But in Python, they are dealt with at runtime. (In Q, the compiler will sort out what needs to be passed, without any extra runtime overhead.)

#### Loops

No breaks out of nested loops. No dedicated endless or N-times loop (easy to get around, but you have to stop and type them, and invent a pointless loop index).


#### Assignment and Equality Operators

I think that now, assignment is allowed inside expressions, but using ":="? (Q will have had this forever.)

#### Increment and Decrement Ops

Don't try ++A or --A in Python, as it won't do what you think

ing. Ones where A.\[i\] needs to be used (see Dot Indexing).

#### No Switch Statement

Self-explanatory. This is actually a big deal in an *interpreted* language, as a jump-table-based switch can be very fast.

No 'Case' statement either, which I have as a sequentially-tested variation of Switch, but still comparing one thing against lots of values.


#### Records (not C Structs)

There is nothing like Q's:

```
    record date =
       var day
       var month
       var year
    end
```
Built-in. There tuples, named tuples (with mutable/non-mutable versions via endless add-ons), and ad-hoc attributes for objects.

The latter have no discipline at all, you can write X.day=1, whether or not X is a date type or not, or write X.ddddddday=1.

#### Integer Sets

These are Pascal-style set of bits: alpha:=\['A'..'Z','a'..'z'\], which I've used forever, but are not present.

#### Packed Types And C-Style Structs

By this I mean raw basic types such as 32-but ints or 8-bit types, used for interfacing. Python needs libraries to make it all work, and some arounds for creating structs. In Q:
````
type date = struct
    byte day, month
    int16 year
end

d:=date(25,12,2020)
print d.month
````
They can be used directly in Q code as well as passed to external libraries.


#### Arrays and Bit Arrays

Arrays

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
