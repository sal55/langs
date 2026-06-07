### B Language

This had been a new attempt to create a hybrid of my Q and M. But after several experiments, all have been abandoned.

This is the current state:

* The B language is now basically Q, but with optional type annotations
* There may be certain features of Q which can't be used in B, or work differently (try-except for example, or anything involving the front-end's symbol table)
* QQ (Q interpreter) has been modified to accept those type annotations, but ignores them
* A BB transpiler has been created from the front-end of QQ, which generates a version QQ's 'PCL' bytecode, which is then written as M language source code.
* A library called VAR exists, derived from the backend of QQ, providing all the support needed for managing Q's variant dynamic types. This is a set of nearly 20 M modules.

BB takes a Q program prog.q say (it can be multiple modules), and generates a single M source file prog.m. To that is prepended a project info block that incorporates the VAR library.

prog.m is then compiled by MM (the M compiler) into prog.exe.

So far, this all seems to work. However:

* The type annotations are not used
* All generated functions have a VARIANT interface
* The generated M code consists of sequences of function calls that effectively handled each PCL instruction

The speed of prog.exe is on a par with running prog.q directly.

