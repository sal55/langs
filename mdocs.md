## M Language Docs

This is a somewhat random collection of topics, largely to remind myself of what's in the language, and how it works.

Just bear in mind the original context of this language was an in-house one developed to compile on and for 8-bit microprocessors, based on my experience of using Algol, Pascal, Fortran and assembly in the late 70s. It has been become more accomplished but not more sophisticated - a language to do a job of work.

### Quick Overview

* Originally created in 1980s for 8-bit and 16-bit microcomputers but has gone through several updates
* Now a hybrid language:
  * Primarily a systems programming language with low-level types
  * Also includes some scripting features in the form of variant types
* Currently targets 64-bit x64 processors running Windows with default 64-bit data types.
* Can also target C source code that is then compiled for 32/64 bits, ARM/x86, Windows/Linux, but restricted to a language subset
* Can be used as a superior alternative to C (and has been since early 80s)
* Non-brace syntax inspired by Algol-68, Pascal and Ada
* Uses a fast whole-project compiler, written in itself, run as one 0.6MB self-contained executable.
* Minimal standard library

### What's Missing

Most new-fangled language ideas of the last 30+ years. The ideas that *are* present
are simple, practical and easy to understand. But to list a few things not included:

 * Most OOP features
 * Language-building features such as templates, or parametric types, or operator overloads, or generic functions...
 * Other meta-programming features
 * Build systems (not needed)
 * Extensive libraries
 * Automatic linking to external libraries (it is necessary to painstakingly create interfaces to such libraries)

### +Scripting Language

While some scripting features are part of M, all modules of a program still have to be formally compiled into
a monolithic binary before it can start executions. A proposed scripting language would be more dynamic,
allowing programs inside a string, or a file, to be run from source during execution. This would run via
an embedded interpreter

### Downloading

Binaries for Win64 can be provided. But, to avoid Anti-Virus issues, it can also be built from source.

Since it is written in itself and needs an existing M compiler, I normally distribute this by getting M
to generate a one-file C source version. Then only a C compiler is needed (no other build-tools).

### M on Linux

Note that this language is primarily for Windows. Programs can be written to run on Linux, but
that involves getting M to generate C code which is then passed through a C compiler.

To maintain the fast development cycle, it is recommended that Tiny C is used, otherwise after using the M compiler to convert all .m files of a project one one .c files, it will hit a brick wall as soon as gcc is invoked. This is made worse since the output will be one large C file.

### M Compiler Versions

|    |OS     |Target|Output|Size|
|------|--------|--------|---------|------|
|mm|Windows|x64|.exe file|	64 bits|
|mc|Windows|C source|one .c file|64 bits|
|mu|Linux|C source|one .c file|64 bits|

Not all features are supported by the C target. I may decide to drop that or only
support it for demonstration versions. 32-bit targets have been dropped.

### Basic Types

Primitive Types:
````
  int        8/16/32/64/128 bits (signed)
  word       8/16/32/64/128 bits (unsigned)
  real       32/64 bits
  char       8/16/64 bits (middle size will be 16 or 32)
  void       Only as target of pointer
````
Composite Types:
````
  array      Fixed or unbounded array of packed types
  pointer    Pointer to packed type or to variant
  records    Record of packed types
  slice      'View' of array, subarray, string or substring (pointer + length)
  proc       Only as target of pointer
````
Variant Type:
````
  var        Dynamically allocated, tagged type
````
### Variant Types

A Variant has a tagged, dynamic type which is assigned at runtime. It can contain these types:
````
  int        64 bits
  word       64 bits
  real       64 bits
  decimal    Arbitrary precision, decimal-based integer and float type
  string     Flex 8-bit string type
  list       Flex array of variants
  set        Flex Pascal-style bit-set
  dict       Flex collection of key:value pairs
  record     Record of variant types
  struct     Record of packed types
  array      Flex array of packed types
  range      64+64 bits pair of integers
  pointer    Pointer to variant
  ref        Pointer to packed type
  void       Unassigned variant
````
Apart from the basic numeric types, these are unavailable in this form outside of variant type.

### Hello World
```
    proc start =
        println "Hello, World!"
    end
```

### Program Structure

A program is a collection of source modules. Each module has this structure:

* Imports
* Function, variable, named const, type and macro definitions

All such names will have 'module scope'. One module defines a function start() or main(), which is the entry point.

### Out of Order Definitions

Functions can be written in any order. Code can refer to a function later in a module without any prior declaration.

Variables can be declared in any order (although at module level, they normally go before any functions). Inside a function, local variables can even all be defined at the end of the function.

The same with types, named constants, and macros.

Modules can be imported in any order. Circular and mutual imports are allowed.

Only import statements must got at the top of the file before anything else (to allow determining the module structure by peeking at the beginning of each module).

### M is Case Insensitive

In M, the identifiers 'abc' and 'ABC' are equivalent. As are 'Abc', 'ABc', 'aBC' and 'abC'.

Names imported from outside may need to be declared inside quotes unless they are all lower-case. But once declated, the name can be written in any combination of case.

### Identifiers

Thes are case-insensitive as mentioned, and consist of A-Z, a-z, 0-9, $ and \_. They cannot start with 0-9.

### Source Character Set

Input files are expected to be 8-bits. Program code and identifiers uses ASCII. UTF8 sequences can be used inside comments and string constants, but M otherwise knows nothing about UTF8.

### Function Scope

M doesn't have block scopes like many languages. The body of a function forms exactly one scope. And one namespace.

(Compare with C with an unlimited number of scopes inside a function, so that the same identifier can be reused any number of times even inside nested blocks, and which has three separate namespaces: normal, struct/enum tags, and labels.)

### Static Variables

All variables at module scope will be 'static'. Inside a function, a static variable needs 'static':
```
    int abc
    int def = 400

    proc fred =
        static int ghi = 500
        int j,k
    end
```

These all start off as all-zeros unless initialised. Any initialisation expression must be reducible to a compile-time constant. Inside a function, a static variable keeps its last value from last time it was called.

The ghi variable above will only stay at 500 until it's modified or reassigned. If fred() was recursive with multiple invocations and multiple instances of j and k, there will only be one ghi variable instance.

Note all static initialisations use "=". Runtime assignments are done with ":=".

### Variable Declarations
All variables need declaring, examples:
```
    int a, b, c
    mut real x, y
    word u, v

    int d := 100
    static int e = 200
```
Exceptions: for-loop index variables don't need declaring. Possible, a compile option will allow 'var' local variables to not need declaring, to match how it works in script languages.

All variables are read-write, or mutable, and technically each of these should have 'mut' in front like 'mut real x,t', but 'mut' is optional and is assumed to be present.


### Read-only Variables
These are declared with 'let':
```
    let int f:=300
```
These need to be initialised, but then cannot be assigned to again, unless that piece of code is re-executed.

M doesn't try too hard to stop a Let variable being written to: it doesn't like it on the left of an assignment, and doesn't allow its address to be taken, which has its own problems, such as being unable to pass a Let array to a function (although that can be done with slices).

So Let is more of a token. But by using Let in place of a regular variable, then there will be a benefit if and when read-only data is taken more seriously.

M has no concept of a read-only attribute as part of the type system, such as 'const' in C.

Note that for-loop variables, where auto-declared, will be declared as Let. I have considered 'Let' to be used for ordinary or 'in' parameters to functions (Let itself can't be used as that syntax is only for regular declarations), but at the moment parameters are read/write.

### Named Constants

True read-only values are declared as:
```
   const e = 200                   ! type is optional; taken from the expression
   const real f = e+100
```
The expression has to be fully evaluated at compile-time. Note the use of "=" which means this is not a runtime assignment. Such named constants are mainly used for integers, reals, and strings. The type of the constant is worked out from the expression, or it can be added in.

Such constants are limited to int, real, and string types. (Anything else, the line being constant and a let variable is blurred. There is a tentative idea of a 'table' constant, for array data that goes into read-only memory. but I haven't done anything with that yet.)

Both const and enum names can be used where a compile-time expression is needed: fixed-length array bounds, and switch-when expressions.

### Enum Names
These are integer-only named constants where the names normally form a connected set:
```
   enum (a, b, c = 100, d, e)
```
The default value starts at 1, and is incrememented by 1, except where overridden. The values here are 1, 2, 100, 101, 102.
The type of such values is always int:
```
    int x := b
```
Such enum names are 'open', and will clash with the same names in a different enum. Enums can be put inside a type:
```
    type colours = (red, green, blue)
    type lights = enum (red, amber, green)             ! 'enum' optional here
```
However, now each name needs to be qualified:
```
    print colours.green, lights.green                  !2 and 3
```
The type system is not advanced enough to able to do this:
```
    colours c := green          ! not possible
    lights  l := green          ! not possible
```
Where it can work out which green to use. M is not Ada! You have to do:
```
    int c := colours.green      ! etc
```

Note that such enums are actually little used; mostly they are declared as part of a 'tabledata' block, which defines enums  plus associated data and even enum names, as parallel arrays of data. See later on.


### The Module System

Module-level names can be exported (made available to other modules) by prefixing with 'global':
```
    global proc fred ...
    global int abc
```
If the above is in source file B.m, then those names can be imported into file A.m like this:
```
    import b
```
Module A can now use names fred and abc, without any qualifiers. Only if other modules exporting fred and abc are also
imported by A, causing ambiguity, is it necessary to use b.fred and b.abc.

### Define Everything Once

No separate declarations are required in M. Just define a module-level entity X on one place, and it's visible in any part of the same module, or in any part of the program by making it 'global', and importing the module containing X.

Declarations are still needed for entities imported from outside the program. This will be names from external DLL shared libraries. Even then, only one declaration is needed.

Usually, such names are declared inside an 'importdll' block. The names in such a block are automatically made global to other modules that import this module. Then those just need something like this:
```
    import clib     # clib.m contains some C runtime imports
'''

