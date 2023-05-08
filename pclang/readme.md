### 'PCL' Intermediate Language

This is the IL (or 'IR') I'm planning to use as the target of my lower level language compiler.

I've tried versions of it in the past, but it was usually internal to the compiler, and I had a hard time converting it native code. It added chaos rather than reducing it. I'm hoping this this new attempt will stick.

This is a work-in-progress and what I have now is as follows:

* A version of my 'M' compiler that targets PCL (using an internal API), which than gets written to a textual PCL source file.
* A standalone PCL interpreter, 'PCI' that reads the source code into an internal data structure (a sort of wide bytecode), and runs it.
* That PCI program runs all of my major M programs, except my Q interpreter where some some GUI libraries (involving callbacks from WinAPI) will not work.
* I also spent a few hours converting the backend of my C compiler to generate PCL. I got as far as translating programs programs such as fib.c, but then lost interest. I assume it will work, but C also presents a few challenges. I will use this version to try out some constructs that are uncommon in my own language, such as value-returning augmented assignments.

The next stages are to complete the coverage of PCI's internal opcodes, and then to start the native code version, a project likely to be called `PCC`.

Links:

* [PCL Description](pcl_overview.md)
* [PCL Opcode Reference](pcl_opcodes.md)


### General Purpose Use

I had hoped this IL could by used by anyone, but it's clear that this not really practical. The language is not pure enough, and there are too many issues, espcially regarding the PCI interpreter.

It's fine however for my own projects and as a demonstration of what such a product can be like. For example, the standalone interpreter is under 100KB (ie. 0.1MB or 0.0001GB), and it can run all my compilers expressed as single file .pcl sources. (It can run itself; more on that below.) But otherwise, if were to be used as a C backend for example:

* A PCL file is expected to represent an entire program, which suits my whole-program compiler. But C for is usually compiled a module at time.
* PCL's execution core is based on 64 bits, but most C implementations use a 32-bit int type. (This can be emulated in PCL by truncating every intermediate op, but that's not ideal)
* C's `setjmp/longjmp` are not supported (that would need a compile of extra intrinsics)
* PCI has limitations with function pointers and callbacks passed via FFIs. Arbitrary C code will use those all the time. (This would not affect the PCC native code version.)

### The 'PCI' Interpreter

This project is an interpreter for a revised version of PCL. It was also my first attempt at interpreting static code, and I was curious as to how fast I could make it. In the end, performance was disappointing, even using specially fixed-up data structures.

I decided to bother with that part of it. I now don't even use special data structures: PCL code read from source is read into a linear array of 32-byte records, with links into a symbol table. This is executed directly, with only minor fix-upsnecessary for execution.

The priority is given to correctly running code, and providing fuller (and less buggy) coverage of my M language than my current compiler achieves. I also want my compiler to be less of a shambles.

* I know the performance is poor, since my Q interpreter for *dynamic bytecode* was faster! On certain benchmarks.
* On the other hand, PCI is 2-3 magnitudes faster than Pico C, a C interpreter.

### Problems with Interpreting Static Code

Apart from performance:

**Function pointers** Local ones are indices to bytecode; external ones are addresses of actual machine code on the other side of the FFI. Because the language on both sides could be the same, I can't isolate them. Callbacks from FFI functions will expect a function address pointing to native code; they won't get it!

At present it's manageable: local function pointers have a value distinguishable from real ones, and I can do the appropriate call. But this doesn't solve the callback problem. I think it is solveable, but it's probably not worthwhile for this project. PCI is more about improving the quality and coverage of the register-based code it will eventually producte; it doesn't have to run every program.

**Inline Assembly** This won't work in PCI (`assem` is a PCL instruction, but it is not supported by PCI). Fortunately my programs tend to have ASM in specific places that also have a HLL alternative. (It won't work in discrete PCC either, but it will work with PCC incorporated into the compiler.)

**The LIBFFI Problem** This is something I hadn't anticipated. Calling FFI functions from an interpreter involves synthesising function calls at runtime; I come across those in my Q bytecode compiler.

The general solution is `LIBFFI`, a complex C cross-platform C library, which I would not be able to build, and which I don't want as a dependency anyway: my stuff is self-sufficient. I normally use a 50-line function which has some inline ASM. For used in PCI, there is a HLL workaround, but that has many limitations. Enough works however that I can use PCI to run PCI expressed as PCL.

**Command Line Parameters** This is a silly one, but if, instead of typing `prog a b c` to invoke program `prog` with 3 inputs, you instead run `pci prog.pcl a b c`, then all the inputs are pushed up by one.

In my own programs, I can allow for this (there is a special global `$cmdskip` which is filled in by some external agency, eg. an emulator of this program's code), but if `prog` is someone else's program, and they directly call `__getmainargs` or `GetCommandLine` say, the inputs will be offset.

### Testing with PCI

I used my MM7 compiler (which only has the new PCL target) on these language projects (all others are smaller)

* AA (x64 assembler)
* CC (C subset compiler)
* MM6 (current M compiler, with no IL)
* QC/PC Q compiler and byte code interpreter
* MM7 (new M compiler)
* PCI (PCL interpreter)
* MS (MM6 configured to run programs from source and in-memory)

In each case, a single .pcl file was produced, for example:
````
    mm7 cc                   # build C compiler as cc.pcl
    pci cc sql               # Run cc.pcl via PCI, on input sql.c to produce sql.exe
    sql                      # run sql.exe
````
This takes takes a magnitude longer than normal.

In the case of PC, which runs bytecode, I was running a bytecode compiler with an interpreter that was itself bytecode compiled! That one also ran, but I didn't attempt anything involving WinAPI libraries.

To run GUI via PC requires requires callbacks to deal with WinAPI message processing; I make a special exception for `MainWndProc`, which resides in `pc.exe`, and which then reenters the the interpreter dispatch loop temporarily. Using pc.pcl, that MainWndProc, which is the callback function, is itself bytecode.

The most fun was this, running a Fibonacci test which prints `fib(1)` to `fib(36)`. This way you always see a stream of output, no matter how slow, but it starts to slow down at difference points depending on performance.

A normal run of the test takes 0.25 seconds on my machine, for optimised native code:
````
    fib                   # fib.exe took 0.25 seconds
    pci fib               # fib.pcl took 3 seconds via pci
    pci pci fib           # Here, pci interprets pci.pcl, which runs fib.pcl; this took 75 seconds
    pci pci pci fib       # The last started slowing at N=31, this at N=23
    pci pci pci pci fib   # This took 7 seconds to show N=1, then slowed at N=15
````

A bit silly but it's also an indication of healthy code.

### Aims and Benefits of this IL

* Cleaner, less buggy mid-section of my M compiler
* Easier to provide coverage of M features (generating PCL is much easier than direct native code, also less messy and less buggy)
* A refined design suitable also for both interpreting and native code
* A cleaner backend to turn PCL into native code. Previous attempts were a disaster, even if they worked. I will probably make use the dispatching used within PCI
* Allow some more refinements to the M language

PCI already provides features than MM6 has, but doesn't implement, for example `(a,b) := c divrem d` (`divrem` is an operator returning two values). 

