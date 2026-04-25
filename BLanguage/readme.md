### B Language

This had been a new attempt to create a hybrid of my Q and M languages. But after several experiments, all have been abandoned.

This is the current state:

* The B language is basically Q, but with optional type annotations
* There may be certain features of Q which can be used in B, or work differently
* QQ (Q interpreter) has been modified to accept those type annotations, but ignores them
* So, the idea is that a valid B program, is also a valid Q program, and can simply interpreted as before
* What's left of the B project at this point, which started off by taking the sources, is this:
  * The runtime backend is in its own project, in the form of a library that can work with VARIANT types and data structures
  * The frontend can compile B code into PCL
* The library is called VAR, and the front end compiler is called BB

These are the plans:

* The BB frontend, instead of compiling to PCL bytecode, will instead compiler M source code
* The generated code will deal with variants and involve lots of function calls, including into the VAR library
* The generated M code, which is in one file, will be compiled with then VAR library into an executable

So, of the B code will be compiled into inefficient native code. However I expect execution to be more brisk than with the interpreter.

At this point, this is already useful: effectively, Q programs can be packed into tidy binaries.

However, the potential for further work is extensive:

* There various speedups that can be done to improve then generated M, even without types annotations
* With type info, some sequences can be compiled into more or less normal, efficient M code
* I may be able to have hybrid B/M programs more easily
* Callbacks should be easier
* DLL calls can be more efficient

So it sounds very promising. If it works, the BB frontend might directly use M's backend.
