### 'PCL' Intermediate Language

This is the IL (or 'IR') I'm planning to use as the target of my lower level language compiler.

This is a work-in-progress, but this is what works right now:

````
    mm7.exe            Compiles an M application to a .pcl source file
    pci.exe            Runs a .pcl source file via an interpreter
````

There are of course existing M compilers like `mm6` that directly turn M applications into .exe or .asm files, assemblers and so on. This is what I'm planning next:
````
    pcc.exe            Turns a .pcl source file into register-based native code, producing .exe, .asm,
                       or running it in-memory
    mm7.exe            Incorporates most of pcc.exe so it functions as a self-contained compiler like mm6.
````

The following describes the PCL language and that PCI interpreter for it. Note that all programs mentioned: compilers, interpreters, assemblers, are written in my established 'M' systems languages.


### Supplied Files For Windows

This is not something I intend to support, but if someone wants to have a go, or just wants to see what it's like, these should be available to click on above:

* [pci.exe](pci.exe) PCL interpreter as Windows binary
* [pci.c](pci.c) Transpiled C version (Windows)
* [pcilin.c](pcilin.c) Transpiled C version (Linux)
* [hello.pcl](hello.pcl) Programs to try; this hello is handwritten, run as `pci hello`
* [fib.pcl](fib.pcl) This is MM7 output using a minimal system library; run as `pci fib`
* [pci.pcl](pci.pcl) Run PCI on itself: `pci pci fib`

If `pci.exe` doesn't make it through your AV software, then you might try building from `pci.c`. This is transpiled from my language (it needing a few tweaks to make it work - C support is being downgraded). Build instructions at the top (basically `gcc pci.c -opci.exe`).

(If not on Windows, you might try `pcilin.c` (this is also set up to use `libc.so.6` not `msvcrt`). Here you'll need the build instructions. I managed to get `./pci fib` working under WSL, but not `./pci pci fib` since `pci.pcl` has calls to Windows-specific functions built-in. This stuff gets complicated quickly.)

### Characteristics

* Low-level intermediate language for my systems language compiler
* Stack-based virtual machine
* Primitive types of `i8-i64 u8-u64 r32-r64` and N-byte blocks (no array, struct or pointer types)
* 64-bit execution model and 64-bit stack
* A PCL program (whether as one .pcl file, or created via an API) always represents a whole program

These characteristics mean it is probably not a good fit for, for example, a C compiler target, since C compiles a module at a time and is primarily 32-bit based.

### Hello.pcl

Here is the simplest program in PCL:

    proc main
        loadimm "Hello, World!\n"
        printstr
        return
    end

Although this is a bit of a cheat since `printstr` is a debugging opcode. A better one, as might be generated from a HLL, is:

    proc main
        loadimm "Hello, World!\n"
        callp puts 1
        return
    end

    extproc puts
        extparam u64
    extend

This uses C's `puts` from the C library in Windows' `msvcrt.dll`, which is automatically linked. If it wasn't then the top of the program would have needed: `linkdll msvcrt` at the top. This is how non-standard libraries are made known.

### Opcode List

See: [PCL Opcode Reference](pcl_opcodes.md)


### The 'PCI' Interpreter

This project is an interpreter for the revised version of PCL. It is also my first attempt at interpreting static code, and I was curious as to how fast I could make it. In the end, performance was disappointing, even using specially fixed-up data structures (\*\*).

I decided not to bother with that part of it. I now don't even use special data structures: PCL code is read from .pcl files into a linear array of 32-byte records, with links into a symbol table. This is executed directly, with only minor fix-ups necessary for execution (roughly the same as running from AST instead of bytecode).

The priority is given to correctly running code, and providing fuller and less buggy coverage of my M language than my current compiler achieves. I also want my compiler to be less of a shambles.

(\*\* I know the performance is poor, since my Q interpreter for *dynamic bytecode* was faster! On certain benchmarks at least.
On the other hand, PCI is still 2-3 magnitudes faster than Pico C, a C interpreter.)

### Problems with Interpreting Static Code

Apart from performance, there are issues with function pointers that cross FFI boundaries (callbacks etc); no support for inline assembly; the 'LIBFFI` problem (synthesise function calls at runtime); and also dealing with command line parameters (since there is now an unexpected parameter: the interpreter name, so that everything is offset).

I can work with these for own programs or use my own simple solutions; I'm not complaining! But it means PCI cannot be a general purpose tool.

### Testing with PCI

I used my MM7 compiler, which produces new-style PCL code, on these language projects:

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
It just takes a magnitude longer than normal. (Instead of 0.5 seconds, it takes 6 seconds to build that 250Kloc program. But that is still 3 times as fast as gcc-O0!)

In the case of PC, an interpreter that executes dynamic bytecode, I was running it with a program that was itself interpreted bytecode! That one also ran, but I didn't attempt anything involving WinAPI callbacks as normally needed for graphics and GUI.

The most fun was the following, running a Fibonacci test which prints `fib(1)` to `fib(36)` via increasingly nested instances of PCI. With this test you always see a stream of output, no matter how slow, but it starts to slow down at difference points depending on performance, and eventually it takes a while to even get started:

````
    fib                   # Normally compiled fib.exe took 0.25 seconds
    pci fib               # fib.pcl took 3 seconds via pci (note my dynamic Q interpreter runs it in 1.3 seconds)
    pci pci fib           # Here, pci interprets pci.pcl, which runs fib.pcl; this took 75 seconds
    pci pci pci fib       # The last started slowing at N=31, this at N=23 (I didn't wait for it to finish)
    pci pci pci pci fib   # This took 7 seconds to start displaying output, then slowed at N=15
````

A bit silly but it's also an indication of healthy code.

### Aims and Benefits of this IL

* To provide a tidier, less buggy and more complete M compiler
* To provide a near-100% complete reference implementation (barring callback issues, but those are application-related)
* Easier to provide coverage of M language features, and easier to add more enhancements

It also opens up new possibilities for debugging and profiling, although debugging would need extra links to the original HLL source.

### The PCC Project

This is the next product. It will now be easier to develop this, given the fully working PCI interpreter.

However it will rely on extra hinting opcodes in the PCL to help out. These are to do with dealing with platform ABIs, and there's also an issue when there are multiple code paths to produce a result (with a stack it's easy; with registers, each result could end up in a different place!)

Such extra opcodes are ignored by PCI, but the need for them is another reason why PCL would be a poor general purpose solution.


