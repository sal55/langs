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

* Extra numeric bases (includes 2,8,10,16, also has 3,4,5,6,7,9)

* Leading zeros can be used on decimal numbers

* Numeric constants can have separators

* Numeric constants can have scale factors

* Raw string constant

* Uses := and = for assignment and equality

* Multi-dim indexing can be written A\[i,j,k\]

* Not case sensitive (so no need to remember exact pattern of upper/lower case)

* Multi-char constants well-defined

* Vastly simpler type declarations, written left to right

* Simple set of built-in numeric types of fixed widths

* Signed types are called int8/16/32/64 with int (int32) and dint (int64) aliases

* Unsigned types are called word8/16/32/64 with word (word32) and dword (word64) aliases

* Dedicated 'byte' type equivalent to word8

* 'char' type equivalent to byte or word8

* Floating point types are called real32/64 with real (real64) alias

* Short type forms include u8/u16/u32/u64, i8/i16/i32/i64 and r32/r64

* (Same naming scheme includes names for 128-bit types but little support for those right now)

* Ref int p,q,r declares three points not like C's int* p,q,r

* A type's limits are obtained with T.minvalue and T.maxvalue (C uses INT_MAX etc)

* .minvalue and .maxvalue also work on expressions: X.minvalue and X.maxvalue

* Arrays are manipulated by value

* Named parameter lists don't need to repeat the type: (int a,b,c, real x)

* The byte-size of a type or expression uses T.bytes or X.bytes, no brackets

* The bit-width of a type or expression uses T.bitwidth or X.bitwidth

* An array's length is determined with T.len or X.len (type or expression)

* Array lower bounds can be any value

* Array bounds are obtained with .lwb and .upb

* Function definitions start with a **function** keyword to make them stand out

* Void functions use **proc** to make them clearer

* Functions in module can be declared in any order. No forwards declarations needed

* Function parameters can be optional with a default value

* Functions can use keyword parameters

* Function parameters can optionally use call-by-reference

* There is a proper named constant feature

* Uses an module import system, no header files

* The C library can be accessed using 'import clib', not dozens of small headers

* Some import library names are conditional, and used to pull modules specific to the target OS.

* Modules introduce namespaces

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

* Variables and types can be declared anywhere in a function (even at the end)

* There is an 'equivalence' feature so that two variables are located at same address

* Dereference syntax uses a consistent postfix '^': A^\[i\]; P^.m; F^(a,b)

* There is a proper iterative for-loop

* A for-loop can have a 'when' clause to enable individual iterations

* There is a dedicated repeat-n-times loop

* There is a dedicated endless loop

* Loop controls include **restart**, **redo**, **next** and **exit**

* Loop controls can be used with nested loops

* Loops can have an **else** part executed on normal exit

* The loop **exit** doesn't clash with 'switch' statement as in C

* **switch** statements are properly structured compared with C switch

* **when** exprs in switch (case labels) can use ranges and lists

* **when** blocks in switch don't need 'break' to get to the end of the switch

* Switch default label is just an **else** part (which can't be silently misspelled)

* There is a looping form of switch: **doswitch**

* **case** version of switch which works with any types, any when exprs and any values

* There us a looping form of case: **docase**

* There is **swap** operator to exchange two values

* There is a ** operator to raise to the power

* There are **min**/**max** operators built-in

* **min**/**max** can be used as a min:= b

* **clamp** operator, clamp(x,a,b) is same as min(max(x,a),b)

* There five main precedence levels to C's 10, plus one for * 

* Precedence levels are more intuitive: a<<3+b will work as expected

* A conditional expression is written as (a | b | c), rather than a ? b : c

* An n-way expression is written as (n | a,b,c,d... |z); one expr is evaluated

* Conditional expressions can be chained: a==b==c meams a==b and c==d

* Logical operators are written as **and** and **or**

* Type casts are written as T(X) (C uses (T)X which is not so clear)

* A cast can also be written as cast(X), and it will use the required type

* Type punning is written as T@(X)

* **Print** is a built-in statement: print a,b,c or println a,b,c

* No format codes are used (unless calling a C function such as sprintf()

* Print can use this: println =a  which displays A = <value> for quick debug printing

* Records (structs) can only formally be defined as a record statement (not anywhere like C)

* Records can also contain named constants, static vars, types and actual functions

* If T is a record type, then those entities can be accessed as T.K, T.V. T.F() etc

* If X is an instance of T, then access uses X.K, or X.F() [X becomes the 1st arg]

* Uses **unless** statement, with opposite logic to **if**

* Some kinds of statements end with a conditional stub, eg. return when a=b

* An operator written in parenthese gives it a numeric value: int op := (+)

* A type name in an expression maps to a ordinal integer for the type: a := int

* Some expressions are illegal as a statement: a+b or x=5 or just 768

* Uses **eval** prefix if it is necessary to evaluate such expressions

* Mixed arithmetic uses signed integers (C used unsigned)

* Function definitions can be nested (but no access to auto-vars in outer scopes)

* There is a **tabledata** feature for parallel sets of enums of arrays

* There is still a textual 'include' feature, but this is rarely used

* There is a **strinclude** feature to import any text file as a string constant

* Special types intm, wordm, intp and wordp exist for machine word and pointer sizes

* **stop** statement used to terminate a program: stop or stop 123

* 'typeof' can be used to extract the type of an expression, typeof(x) a,b,c

* Inline assembly is always available with a simple interface

* Foreign functions from Windows and C are can be declared in special import blocks

* Bit and bitfield indexing on ints, eg. A.[i] to get the i'th bit.

* Fast whole-project compiler

* Compile project by just submitting the main module

* Module imports can be circular or mutual

* Multiple return values from functions: a,b,c := f() [experimental]

* Multiple assignment: a,b,c := d,e,f

* Accessible function data (tables of names and addresses of all functions in project)

* Dynamically typed companion language is available using the same syntax.

Above features are included and either work or should work.

About a dozen more features elided because they are not implemented, but are still viable in this level of language.

They include sets, ranges, slices, and bit-arrays of the types u1, u2 and u4.
