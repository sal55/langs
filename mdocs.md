## M Language Docs

This is a somewhat random collection of topics, largely to remind myself of what's in the language, and how it works.

### Quick Overview

* Originally created in 1980s for 8-bit and 16-bit microcomputers but has gone through several updates
* Now a hybrid language:
  * Primarily a systems programming language with low-level types
  * Also includes some scripting features in the form of variant types
* Currently targets 64-bit x64 processors running Windows.
* Default 64-bit types
* Can also target C source code that is then compiled for 32/64 bits, ARM/x86, Windows/Linux, but restricted to a language subset
* Can be used as a superior alternative to C
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
 * Automatic linking to external libraries (it is necessarily to painstakingly create an interface to such libraries)

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

To maintain the fast development cycle, it is recommended that Tiny C is used.

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
  int        8/16/32/64/128 bits
  word       8/16/32/64/128 bits
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

### Example Program

