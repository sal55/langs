### B Language

This had been an actual new language compiler, a new attempt to create a hybrid of my Q and M languages.

Many applications need to be a combination of both, but interaction between the two language is awkward. Lots of things would be untidy too.

Eventually the new language got scrapped (after a brand new lexer, parser and resolver).

The 'B' project still exists, but in a different form:

* It is based on the Q language, compiler and interpreter
* Optional type annotations will added, which are usually ignored
* Most code is still compiled to PCL and interpreted
* Some functions can be marked for conversion to native
* Those will need all parameters and locals annotated
* Such functions will have ASTs and PCL code annotated too
* Interpreted functions (Dprocs) can call compiled code (Sprocs) via a mechanism similar to FFI calls
* Only certain annotated types will work: Int, Real, Arrays, Strings, Structs

The idea is that speed critical code is placed in those functions, but the program design must arrange to use suitable types like those arrays and structs.

I expect this to give a 10x speed-up, while compiled code still has access to a rich, shared global environment.
