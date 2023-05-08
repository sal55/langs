### 'PCL' Intermediate Language

This is the IL (or 'IR') I'm planning to use as the target of my lower level language compiler.

I've tried versions of it in the past, but it was usually internal to the compiler, and I had a hard time converting it native code. It added chaos rather than reducing it. I'm hoping this this new attempt will stick.

This is a work-in-progress and what I have now is as follows:

* A version of my 'M' compiler that targets PCL (using an internal API), which than gets written to a textual PCL source file.
* A standalone PCL interpreter, 'PCI' that reads the source code into an internal data structure (a sort of wide bytecode), and runs it.
* That PCI program runs all of my major M programs, except my Q interpreter where some some GUI libraries (involving callbacks from WinAPI) will not work.
* I also spent a few hours converting the backend of my C compiler to generate PCL. I got as far as translating programs programs such as fib.c, but then lost interest. I assume it will work, but C also presents a few challenges. I will use this version to try out some constructs that are uncommon in my own language, such as value-returning augmented assignments.

The next stages are to complete the coverage of PCI's internal opcodes, and then to start the native code version, a project likely to be called `PCC`.

### General Purpose Use

I had hoped this IL by used by anyone, but it's clear that this not really practical. The language is not pure enough, and there are too many issues, espcially regarding the PCI interpreter.

It's fine however for my own projects and as a demonstration of what such a product can be like. For example, the standalone interpreter is under 100KB (ie. 0.1MB or 0.0001GB), and it can run all my compilers expressed as single file .pcl sources. (It can run itself; more on that below.) But otherwise, if were to be used as a C backend for example:

* A PCL file is expected to represent an entire program, which suits my whole-program compiler. But C for is usually compiled a module at time.
* PCL's execution core is based on 64 bits, but most C implementations use a 32-bit int type. (This can be emulated in PCL by truncating every intermediate op, but that's ideal
* C's `setjmp/longjmp` are not supported (that would need a compile of extra intrinsics)
* PCI has limitations with function pointers and callbacks passed via FFIs. Arbitrary C code will use those all the time. (This would not affect the PCC native code version.)

### The 'PCI' Interpreter

This project is an interpreter for a revised version of PCL. It was also my first attempt at interpreting static code, and I was curious as to how fast I could make it. In the end, performance was disappointing, even using specially fixed-up data structures.

I decided to bother with that part of it. I don't even use special data structures: PCL code read from source is read into a linear array of 32-byte records, with links into a symbol. This is executed directly, with only minor fixed necessary for execution.

The priority is given to correctly running code, and providing fuller coverage of my M language that my current compiler achieves.

### Problems with Interpreting Static Code

Apart from performance:

**Function pointers** Local ones are indices to bytecode; external ones are addresses of actual machine code on the other side of the FFI. Because the language on both sides could be the same, I can't isolate them. Callbacks from FFI functions will expect a function address pointing to native code; they won't get it!

At present it's manageable: local function pointers have a value distinguishable from real ones, and I can do the appropriate call. But this doesn't solve the callback problem. I think it is solveable, but it's probably not worthwhile for this project. PCI is more about improving the quality and coverage of the register-based code it will eventually producte; it doesn't have to run every program.

**Inline Assembly** This won't work in PCI (`assem` is a PCL instruction, but it is not supported by PCI). Fortunately my programs tend to have ASM in specific places that also have a HLL alternative. (It won't work in discrete PCC either, but it will work with PCC incorporated into the compiler.)

**The LIBFFI Problem** This is something I hadn't anticipated. Calling FFI functions from an interpreter involves synthesising function calls at runtime; I come across those in my Q bytecode compiler.

The general solution is `LIBFFI`, a complex C cross-platform C library, which I would not be able to build, and which I don't want as a dependency anyway: my stuff is self-sufficient. I normally use a 50-line function which has some inline ASM. For used in PCI, there is a HLL workaround, but that has many limitations. Enough works however that I can use PCI to run PCI expressed as PCL.

