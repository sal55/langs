## Q Language 2022

'M' and 'Q' are my static and dynamic, compiled and interpreted languages.

Attempts to combine these into one language ('M5' described elsewhere) ran into problems. While the M5 compiler is unfinished, it still works fine for what it does, and is currently used as my production compiler.

This new project uses the Q language as a start point:

* I originally had a two-language solution for writing applications: Q and M, with the latter written as separate modules and accessed from Q via the usual mechanisms for dynamic libraries (except that, for technical reasons, I use a private shared library format, not DLL)
* The aim had been to incorporate the dynamic elements of Q into M, so that I could write in one language
* This new project will still need two languages, since a systems-level one that produces EXEs is needed to implement the interpreter of the other
* But Q will acquire more static features, such as functions that compile to native code, so that a lot more can be done with the one language
* So it will be a 1.5 language solution to writing applications

### Q Types

Currently Q has support for type categories I will designate as **V**, **T** and **B**

Broadly, V are tagged (and 'boxed') variant types. T are simple ('unboxed') static types, as might be found in lower-level APIs. B are bit types.

Everything is Q is done via V types, which includes:

* Arrays of V (Lists)
* Records of V
* Pointers to V
* Arrays of T and B
* Records of T (structs)
* Pointers to T and B

Effectively, everything is boxed. Unboxed types exist within data structures, but to do any operations on them, will be boxed into an enclosing V type. And those V types need single and double type dispatching to deal with.

This is what makes Q much slower compared to a compiled language working with T types. Even compiling Q to native code will not significantly speed it up, if V types are still used.

### Q Compiled Functions

The aim of this new project is to have special functions that work with T types and compile to native code.

To that end, Q will effectively incorporate another language, that I will call **Mlite** for now.

Q functions will use **fun sub**, Mlite uses **func proc**. Mlite requires type declarations, which will be allowed in Q functions too, but there they are optional. This to allow suitable functions to be flipped between Q and Mlite, by having compatible syntax.

Mlite will only deal with T types, not mixed V and T as I'd attempted within the M5 project which got very hairy.

Q functions can call MLite ones, and I think Mlite functions will be to call Q, but that is less common and probably less useful (eg. used for callbacks from external libraries: they will call Mlite that passes control to Q). Q calling Mlite is little different to calling FFI functions.

### Implementation

Q normally compiles programs to in-memory bytecode and runs it immediately. (I no longer have an on-disk bytecode representation.)

For embedded Mlite functions, those are converted to in-memory x64 code. This part will be challenging; static code normlly uses a hefty type-analysis phase, which is missing from Q. So I will try and perform any interventions that are needed, eg. type conversions, within the code generator.

Efficient x64 code generation also used a considerable backend (to IL then to x64 representation then to actual machine code). Here, I'm planning a new, simpler,  streamlined backend (however they always start off simple...)

This may not be as performant, but such code working on T types should still be much faster than interpreted code on V types.

What I'm trying to avoid is getting into complicated varieties of JIT. The approach above is a simple form of it. I'm not dynamically analysing interpreted code looking for 'hot' paths and turning that into native code. The programmer designates which functions will be compiled to native code, and those functions will (1) need suitable type annotations; and (2) can only contain code that can work as T types.

There may be other constraints too. But overall, it should still be more convenient writing such functions within a Q module, with the ability to share global types, enums, external libraries and so on, then having to collect them into a separate M module, compile it to a shared library, and also lose access to the Q program's environment.

