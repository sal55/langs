## 'M' Programming Language

Lower level systems programming language long used as a tidier alternative to C.

Current implementation is for 64-bit Windows only.

Summary of [features](../mfeatures.md)

**Other links below may not work**

### Characteristics

* M is a systems language at roughly the level of C, and can be used for the same sorts of tasks
* It uses an entirely different syntax from C, and has some modern features, such as proper modules
* The compiler for M is the single, self-contained file `mm.exe`, about 0.6MB.
* M is designed for whole-program compilation
* It is pretty fast (compiling some 0.5M lines per second on my PC, producing up to 5MB of code per second)

### Status

Currently I use this as a private language, mainly used for language programs, including the interpreter for my scripting language which is what I prefer for applications.

While anyone is welcome to play with it, or borrow any useful-looking features, it is not really suitable for general use: docs are patchy, coverage (of combinations of features, operations and types) is not 100%, and it is buggy. No user support can be provided.

It's also volatile, in the sense that I have a tendency to make breaking changes.

A binary for the compiler (`mm.exe`) is available in this folder, and combined sources (a snapshot) are available for perusal in `mm.qa`.

### Building from Source

M is self-hosted so it will need an M compiler to build. If happy downloading a binary (see below), download the files [mm.exe](mm.exe) and [mm.qa](mm.qa) (right-click on the links). You can build a second version from source code using:
```
    mm mm.ma -out:mm2               # compile mm.ma to mm2.exe
```
But if running mm.exe is OK, then no need to do that except as a test or demonstration that it works.

**AV Issues** If you are a developer, then you will already know to deal with AV matters for your own projects.

I may make available a version supplied as two programs: `mm.mx`, a binary in a special format, and a loader `run.c`, a 600-line program which when compiled locally can be used to run the compiler like this: `run mm hello`. This ought to attract less AV attention.

(This approach makes possible compiling *some* programs to run on x64 Linux, without needing to use Linux ABIs or write ELF formats; this is something I'm looking into.)

### Example Programs

Some small programs are here: [Examples](../Examples)

### Building Executables

You will need the compiler mm.exe. Given an example program such as hello.m, build as follows:

    mm hello                    # compile hello.m to hello.exe
    mm -run hello               # compile hello.m to in-memory code then execute

For a larger program comprising multiple modules, submit only the lead module (the one containing header listing the modules).

For other options, use `mm -help` or see the Options file above.

### History

These are some version of M starting from around 40 years ago. All had compilers running *on* the machine in question not just generating code from it:

**Z80** An 8-bit processor. Initially working on my homemade boards with other software, later on business machines I was developing.

**8088** (16-bit processor) For the the IBM PC, under MSDOS

**80386** (16-bit mode with 32-bit features) under MSDOS, later Windows

**x86** (32-bit) Windows

**x64** (64-bit) Windows

Most experimental versions have been done under x64, including:

* 64-bit mode but using 32-bit pointers
* Ones using intermediate C that could run under Linux including on ARM processors
* Several schemes for dealing with modules
* Switching to whole-program compilers

### Bootstrapping

Earlier versions of the M compiler were written in assembly, especially for a new target. Otherwise they were generally bootstrapped using a previous version of the M compiler. I can safely say that C source code has never been involved in the process, except in the more recent experiments involving Linux and ARM, where C was used an an intermediate language.

Nor have any other languages been used, except for ASM, and even then I wrote the first assembler in machine code.

M compilers have generally also used my own auxiliary tools, such as fast loaders (replacing traditional linkers), but there was a period when I relied on both an external assembler, and external linker. 

These caused problems later on, eg. the assembler couldn't cope with the large output files of my whole-program compiler, and the linkers, borrowed from various C compilers, were temperamental. Both also dwarfed my compiler in size. So those dependencies were eliminated.

I first developed my own fast assembler, which also did the job of linking. Now the compiler directly generates executable files.
