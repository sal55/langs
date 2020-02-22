#### M Language

M is a language doing roughly the same job as C but coming from different origins and with different syntax.

This is a list of language features that can be considered enhancements over C:

* Uses consistent block delimiters (eg. always if-then-endif not braces which may or may not be present)

* This avoids problem in missing, extra, or badly matched braces

* Dangling else avoided

* Can't mistakenly add a statement in a block that has no braces

* Each type of statement can have dedicated delimiter to avoid mixups (endif, end while etc)

* Semicolons are largely eliminated from source as end-of-line usually serves as semicolon

* Line comment syntax has no scope for bugs like C's //

* Any numeric base from 2 to 16 can be used, using consistent Nx syntax (2x, 3x etc)

* Leading zeros can be used on decimal numbers

* Numeric constants can have separators (_ ' or `)

* Numeric constants can have scale factors (like, 5 million)

* Raw string constants where '\' is not interpreted as escape sequence

* Uses := and = for assignment and equality

* Multi-dimensional indexing can be written A\[i,j,k\]

* Not case sensitive (so no need to remember exact pattern of upper/lower case)

* Multi-character constants well-defined, up to 'ABCDEFGH' for 64 bit, and 'ABCDEFGHIJKLMNOP' for 128-bit

* Vastly simpler type declarations, written left to right

* Simple set of built-in numeric types of fixed widths

* Signed types are called int8/16/32/64/128 with 'int' aliase for int64

* Unsigned types are called word8/16/32/64/128 with 'word' for word64

* Dedicated 'byte' type, alias for word8

* Separate 'char' type

* Floating point types are called real32/64 with 'real' alias for real64

* Short type forms include u8/u16/u32/u64/128, i8/i16/i32/i64/128 and r32/r64

* Ref int p,q,r declares three pointers not like C's int* p,q,r

* A numeric type's value limits are obtained with T.minvalue and T.maxvalue (C uses INT_MAX etc)

* .minvalue and .maxvalue also work on expressions: X.minvalue and X.maxvalue

* The name of a type can be accessed using T.typestr or X.typestr (yields a string constant)

* The type itself can be accessed as T.type or X.type (yields an int constant representing internal ordinal for the type).

* Types can have reprentations in an expression: int.type is an internval value for 'int'. (Example: 'if x.type = int then ...' however x.type is always a compile-time constant)

* Arrays are manipulated by value

* Arrays cannot be accessed like pointers

* Pointers cannot be indexed like arrays

* Named parameter lists don't need to repeat the type: (int a,b,c, real x)

* The byte-size of a type or expression uses T.bytes or X.bytes, no brackets

* The bit-width of a type or expression uses T.bitwidth or X.bitwidth

* An array's length is determined with T.len or X.len (type or expression)

* Arrays are normally 1-based, but lower bounds can be any value, including 0 or 1.

* Array bounds are obtained with .lwb and .upb

* Function definitions start with a **function** keyword to make them stand out

* Void functions use **proc** to make them clearer

* Functions in module can be declared in any order. No forward declarations are needed

* Function parameters can be optional with a default value

* Function calls can use keyword parameters as well as positional.

* Function parameters can optionally use call-by-reference

* There are proper named constants

* Uses an module import system, no header files

* The C library can be accessed using 'import clib', not dozens of tiny headers

* Some import library names are conditional, and used to pull in modules specific to the target OS (so 'import oslib' will actually import one of oswindows, oslinux, or osnos).

* Modules introduce namespaces, allowing modules to export the same name as another, without clashes.

* Functions, variables and other things declared at module scope are always local; not exported

* To export a function or variable, use a 'global' prefix

* Can also export types, structs, enums, named constants using 'global'

* To import things exported (ie. defined as global) from another module, import it

* Most things, functions, variables, types etc, are only declared and defined in one place

* Things can only be declared/defined once (C is not so bothered)

* The names declared in an enum can form part of type, rather than be 'open'

* There are no struct tags or enum tags

* There are no separate namespaces for struct/enum tags and labels

* There are no block scopes in a function

* Variables and types can be declared anywhere in a function, even at the end. (However, initiaised variables must be at the start of the function, as the initialisation is done on entry)

* There is a 'raw' function type with no entry or exit code (called **threaded proc** as it is used for threaded 'functions' in my interpreter).

* There is an 'equivalence' feature so that two variables are located at the same address

* Dereference syntax uses a consistent postfix '^': A^\[i\]; P^.m; F^(a,b)

* There is a proper iterative for-loop

* A for-loop can have a 'when' clause to enable individual iterations

* There is a dedicated repeat-n-times loop

* There is a dedicated endless loop

* Loop controls include **restart**, **redo**, **next** and **exit**

* Loop controls can be used with nested loops

* Loops can have an **else** part executed on normal exit

* The loop **exit** doesn't clash with **switch** statement as in C

* **switch** statements are properly structured compared with C switch

* **when** exprs in switch (case labels) can use ranges and lists

* **when** blocks in switch don't need 'break' to get to the end of the switch

* Switch default label is just an **else** part (which can't be silently misspelled)

* There is a looping form of switch: **doswitch**

* There is a **case** version of switch which works with any types, any when exprs and any values

* There is a looping form of case: **docase**

* There is a **swap** operator to exchange two values

* There is a \*\* operator to raise to the power

* There are **min**/**max** operators built-in

* **min**/**max** can be used as a min:= b

* **clamp** operator, clamp(x,a,b) is same as min(max(x,a),b)

* There five main precedence levels to C's 10, plus one for \*\* 

* Precedence levels are more intuitive: a<<3+b will work as expected

* A conditional expression is written as (a | b | c), rather than a ? b : c

* An n-way expression is written as (n | a,b,c,d... |z); one expr is evaluated

* Conditional expressions can be chained: a=b=c meams a=b and b=c

* Logical operators are written as **and** and **or**

* Type casts are written as T(X) (C uses (T)X which is not so clear)

* A cast can also be written as cast(X), and it will apply whatever type is called for.

* Type punning is written as T@(X), and can apply to rvalues as well as lvalues.

* **print** and **println** are built-in statements: print a,b,c or println a,b,c

* No format codes are needed (unless calling a C function such as sprintf()

* Print can use this: **println** =a  which displays A = 1234 etc for quick debug printing

* **fprint"" and **fprintln** provided formatted printing

* Records (structs) can only formally be defined as a record statement (not anywhere like C)

* Records can also contain named constants, static vars, types and actual functions

* If T is a record type, then those entities can be accessed as T.K, T.V. T.F() etc

* If X is an instance of T, then access to such members can also be done as X.K, X.V, or X.F() (the latter is equivalent to T.F(X))

* Includes **unless** statement, with opposite logic to **if**

* Some kinds of statements end with a conditional stub, eg. **return when** a=b. Can use **when**, **if* or **unless**, and mainly applies to control of transfer instructions: **return**, **goto**, **exit** etc.

* An operator written in parenthese gives it a numeric value: int op := (+)

* Most stand alone expressions are illegal as a statement: a+b or x=5 or just 768. Only call, assignment and increment can be used as statements by themselves.

* Uses **eval** prefix if it is necessary to evaluate such expressions

* Mixed arithmetic uses signed integers (C used unsigned)

* Function definitions can be nested (but no access to auto-vars in outer scopes)

* There is a **tabledata** feature for parallel sets of enums and arrays

* There is still a textual 'include' feature, but this is rarely used

* There is a **strinclude** feature to import any text file as a string constant

* Special types intm and wordm for machine word and pointer sizes

* **stop** statement used to terminate a program: stop or stop 123

* Inline assembly is always available with a simple interface

* Foreign functions from Windows and C external libraries can be declared within special import blocks **importdll** ... **end**.

* Bit indexing on ints, eg. A.\[i\] to get the i'th bit. Also A.odd, A.even

* Bitfield indexing on ints, eg. A.\[0..7\]. Also A.lsb etc

* Fast whole-project compiler (M compiler builds itself from sources to .exe in 0.25 seconds.)

* Compile a project by just submitting the main module - no make files needed

* Optional C target, will automatically convert whole project to single, self-contained C source file. (Feature dropped)

* Module imports can be circular or mutual

* Multiple return values from functions: a,b,c := f() \[experimental, limited to three scalar types\]

* Multiple assignment: (a,b,c) := (d,e,f)

* Accessible function data (tables of names and addresses of all functions in project can be scanned by applications)

* Dynamically typed companion language is available using *the same syntax*.

* Range syntax:  if a in 10..20 then ..., used in array bounds, switch cases, and for 'in' operator

* Set syntax: if a in \[10,20..30\] then ... (Limited to values of 0 to 127. A set construct denotes an integer constant of 64 or 128 bits.)

* Slice type, implemented as 128-bit (pointer,length) allows arrays and sub-arrays to be passed together with their lengths. Also allows counted strings using char arrays.

