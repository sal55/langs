## Q Language 2022

'M' and 'Q' are my static and dynamic, compiled and interpreted languages.

Attempts to combine these into one language ('M5' described elsewhere) ran into problems. While the M5 compiler is unfinished, it still works fine for what it does, and is currently used as my production compiler.

This new project uses the Q language as a start point:

* I originally had a two-language solution for writing applications: Q and M, with the latter written as separate modules and accessed from Q via shared library mechanism
* The aim had been to incorporate the dynamic elements of Q into M, so that I could write in one language
* This new project will still need two languages, since a systems-level that produces EXEs is needed to implement the interpreter of the other
* But Q will acquire more static features, such as functions that compile to native code, so that a lot more can be one with the one language
* So it will be a 1.5 language solution to writing applications

### Q Types

Currently Q has **V**, **T** and **B** types.

Broadly, V are tagged, variant types. T are low-level static types, as might be found in lower-level APIs. B are bit types.

Everything is Q is done via V types, which include:

* Arrays of V (Lists)
* Records of V
* Pointers to V
* Arrays of T and B
* Records of T (structs)
* Pointers to T and B

Effectively, everything is 'boxed'. Unboxed types exist within data structures, but to be operated on, will be boxed into an enclosing V type.

This is what makes Q much slower compared to a compiled language working with T types. Even compiling to Q to native code will not significantly speed it up, of V types are still used.

### Q Compiled Functions

The aim of this new project is to add a new category of compiled function:

**sub, fun** Procedures and functions which are intepreted and use V types (so T and V are only used within a V type).

**proc, func/function** Procedures and functions which are compile to in-memory native code, and use T types

Call those **S** and **F** functions respectively (Slow and Fast; I don't yet have a snappy designation).

To simplify the project, F functions only use T types, not mixed V and T as I attempted within the M5 project, which got very hairy.

For calls between one S and F functions, conversions are needed. Unboxing V types to a compatible T type if there is one (eg. Array of T to Slice of T), or boxing T types to a suitable V type. (B bit-types are mainly used with a V type.)

I'm going to try and keep the language the same, except that F functions will need type annotations. S functions can have optional type annotations, to allow a function to be flipped from one to the other, provided the types will work on both.

### Implementation

Q normally compiles programs to in-memory bytecode and runs it immediately. (I no longer have an on-disk bytecode representation.)

For embedded F functions, those are converted to in-memory x64 code. This part will be challenging; static code normlly uses a hefty type-analysis phase, which is missing from Q. So I will try and perform any interventions that are needed, eg. type conversions, within the code generator.

Efficient x64 code generation also used a considerable backend (to IL then to x64 representation then to actual machine code). Here, I may start off with naive x64 code generation. Not as performant, but such code working on T types should still be much faster then interpreted code on V types.

If it looks promising, then I can incorporate the more efficient code generator.

What I'm trying to avoid is getting into complicated varieties of JIT. The approach above is a simple form of it. I'm not dynamically analysing interpreted code looking for 'hot' paths and turning that into native code. The programmer designates which functions will be compiled to native code.

Other, semi-automatic approaches can be investigated at some later time.
