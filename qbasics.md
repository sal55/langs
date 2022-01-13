## Some Q Features

This is a set of features and characteristics of my Q language which are or have been missing from Python. Some I consider fundamental.

Python may have acquired some or makes them possible via add-on modules, although often via several incompatible add-ons none of which do the whole job.

* Case insensitive (I consider that more user-friendly)

* Named constants (`const abc = 100`)

* Static identifiers (identifiers known to be function names, modules, variables, named constants,  enums, types, macros, labels, etc; in Python everything is a variable)

* Algol-style syntax with a proper **end** to mark the end of a block, and separate **proc, function** prefixes

* Built-in support for 'pack' types (my name for C-style primitive types)

* Built-in support for C-style structs

* Built-in FFI for C-based APIs (includes APIs for own systems language; see below)

* Simple enumerations (`enum (red, green, blue)`; like named constants, these allow for constant expression reductions, use as switch-cases, etc)

* `'A'` up to `'ABCDEFGH'` character constants stored as integers (`'A'` is the value 65)

* Pascal-like bit-sets with constructors (`['A'..'Z', '0'..'9']`)

* Bit-arrays built-in, including arrays of 1, 2 and 4 bits

* Built-in arrays of pack-types

* Bit and bit-field indexing: `A.[i], A.[i..j]`, as rvalues or lvalues

* Built-in mutable records with named fields, and constructors: `date(31, 12, 1999)`

* Print/Println as *statements*

* Read/ Readln as *statements*: `readln a, b, c`

* Static variables inside functions

* Well-behaved default values for function parameters

* `start()` and `main()` functions. `start` is automatically executed in any module. `main` is only executed for the lead module (Python uses `__main__` etc)

* Goto

* Loop break, continue (I call them `exit` and `next`) plus `redo` from nested loops

* Dedicated statements for endless loops and repeat-N-times loops

* Swap operator (`swap(a[i], a[i+1])` instead of `a[i], a[i+1] = a[i+1], a[i]`)

* Reference parameters

* Pointer and address-of operators for low-level manipulations

* Switch statement, using jump-tables

* Case statement for sequentially tested cases

* 2-way and N-way selectors (eg. `(n | a, b c | z)`; only one expr evaluated)

* View-based slicing of strings, lists, arrays

* `strinclude` feature incorporates any text of binary file as a string literal

* Basic maths are built-in: `sin(x)` not `math.sin(x)`, as is `pi`

* Increment ops such as `++a`

* Assignments and increments inside expressions (recently added to Python)

* Interchangeable expressions and statements throughout the syntax

* Scope control: use `global` attribute to export a top-level name from a module

* Built-in arbitrary-precision decimal integer/float type (regular numbers have i64/f64 types)

* New (2021) module system uses header blocks to define the structure of a project. This eliminates `import` statements from most modules

* Built-in feature to combine all modules/files of an application into one source file. An app can then be distributed as one file, plus the one-file intepreter.

* `tabledata` feature to define parallel sets of enums and corresponding lists of data

* Shares the same syntax as its statically typed implementation langage

* That language can be used to create add-on functionality which runs at native code speed. The interfacing is taken care of automaticaly (a WIP).
