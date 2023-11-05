## M Compiler Summary and Timeline

M was never a general-purpose, cross-platform language. It always targetted whatever platform was current, in the world of personal and small business computers.

The following is a rough summary of the various targets of my systems language compilers as they evolved over 40+ years. All are for my 'M' language except for the first, included as a contrast, which was for a kind of kind of HLA/HLL hybrid. The `int` and `real` columns show the widths of those primitive types
:
Processor | Bits| OS | `int` | `real` | Pointer size | Year | Other
--- | --- | --- | --- | --- | --- | --- | ---
PDP10 | 36 | TOPS10?  | `i36` | -- | 18 | 1980 | College mainframe
Z80 | 16 | Bare metal | `i16` | `f24` | 16 | 1981 | Home-made machine
Z80 | 16 | CP/M Clone | `i16` | `f32` | 16 | Early 80s | Business machine
8086 | 16 | MSDOS | `i16` | `f32` | 32 | Mid 80s | (IBM PC; segmented address)
80386 | 16 | MSDOS | `i16` | `f32` | 32 | Late 80s |
80486 | 16 | Windows | `i16` | `f32` | 32 | 1992 |
80486 | 32 | Windows | `i32` | `f32` | 32 | 2002 | (flat address)
x64   | 64 | Windows | `i64` | `f64` | 64 | 2012 | Whole program compiler

With Z80 it was either my own design, or one that I helped my company develop, although one of apps was also sold commercially for the Amstrad PCW 8256 Z80-based machines.

All these compilers ran on their respective targets, except when crossing from one generation to another, where the details have been forgotten.

All have ended up self-hosted, bootstrapped from an earlier version, except:

* The first Z80 version, written in assembly, with an assembler I wrote in hex, with a hex editor itself written in actual binary (using homemade switches)
* For one, possibly two reboots, I believe I also started from assembly. But the details have long been lost.

Most versions implemented the 'full stack', in going from source to executable using 100% my code, but there were exceptions:

* Late 90s to 2012 I generated NASM source and used a linker belonging to a C compiler
* During part of that period, the M compiler was written in my scripting language

Currently it is again full stack and 100% self-hosted, at least for the current Win64 platform. 

### Independent Compilation, Object Files and Linkers

Until the 64-bit compiler, I compiled a module at a time. Until the later 90s, I used my own object file format and my own loader to turn multple object files into an executable. This was a straightforeward process that could be done as fast as files could be read from a disk.

The NASM period wasn't satisfactory: that assembler was slow, but still usable. (It took 5 times a long to assembler an ASM file, as my compiler took to generate it.)

With whole-program compilation however, single, verty large ASM files were generated, and NASM got exponentially slower (like taking a minute to assemble). There were also problems with linkers. So eventually the assembler was replaced (with my own, although I now know other assemblers were available without that problem). And after a while, the linker was replaced too: my assembler can generate EXE files.

### Module Schemes

Modules were not handled directly by the language until the 2010s. Before that, I'd always used an IDE of some sort to manage a project. Not the huge glossy ones used now, but a simple console program that read a project file listing the modules of a project, and displayed that list for easy browsing, editing and compiling.

Early compilers were memory resident and part of the IDE together with an editor, as either there was no storage, or it was floppy-based, and I needed a quick development cycle. 40 years ago, I still use pretty much the same scheme! But the editors and compilers are separate programs.

So, there was no need for the compiler to know the modules, and it would just compiler whatever was submitted. It was necessary however for declarations of what was exported from other modules to be known when compiling any module. There were various schemes for these, which involved manually written interfaces (and all were better than C's headers for the projects I was involved with).

It's now only been a couple of years that a fully automatic, hassle-free module scheme with no restrictions (eg. circular imports are not problem) has been available. So a project (ie. a single EXE or library) can be build by submitting the lead module to the compiler.

### Optimisations

I've never bothered with these until recently. And then it was mainly to do with keeping some local variable in registers. That made an impressive difference with small benchmarks, but very little difference with real programs.

However, for the sorts of apps the M compiler is now used for (compilers, assemblers, interpreters), the difference between unoptimised and optimised (ie. transpiling to C and applying gcc or clang/llvm) might only be 30-50%. That is, optimised code take 23-33% less runtime. But typical runtimes (for compiling/assembling) are only about 0.1 seconds anyway, so it's irrelevant.

(For interpreting, runtimes depend on input programs and can be anything, but here I can enable the accelerated ASM dispatcher. Then most programs run faster than applying gcc-O3 to transpiled C.)

### Intermediate Languages

Most code-generation has been ad-hoc, yet performance hasn't been terrible. I'd never used an intermediate language (perhaps a stylised, more orthogonal version of the native code target sometimes), until recently. I played with a few different kinds, and have now settled on stack-based IL, called PCL. (Also the name of my interpreter's bytecode, which was not a problem, until I attempted a hybrid language combining both.)

I've experimented with versions of PCL thjat could exist as a separate, independent language expressed textual source code. One version had a problem that could turn PCL into EXE (basically, what LLVM does, but in a 0.25MB program). Another had an interpreter.

Both had limitations (eg. no inline assembly was possible, or problems interfacing between native code libraries and interpretec codes. Eventually that was dropped; the current 'PCL' is an internal language only, as it is in the Q interpreter.

### Debuggers

I've never bothered. But I've also gotten quite adept at hunting down bugs. In any case, with language-related development, you might be dealing with multiple sets of sources; which one do you single-step through? The bug might lie in the scripting code; in the interoreter code; in the compiler used to build the interpreter; or in the previous generation of that compiler.

### Language Server

I guess I hardly need to point out that no such thing exists. My tools are firmly 80s-style and console-based.

### Packaging and Deployment

I can't remember about older versions, but for the last few years, each compiler (and assember or interpreter) has been a single, self-contained EXE file. This is certainly true of the M compiler (my C compiler has recently changed from one file to a 2/3-file solution: compiler, assembler, and windows.h).

One recent feature is the ability of the compiler to take a project of dozens of modules and support files, and package them into a single `.ma` file. Then the compiler can direcly build from this file, without needing to extract the files (unlike .zip for example). So to supply everything for someone else to build your app from source, only two files are needed: compiler and source file bundle:

    mm qq.ma                    # build Q interpreter using mm.exe and qq.ma

No 'make' files. Nothing stops anyone using makefiles to manage the wider aspects of a complete application than building one binary, but it's not something I use. I use my IDE, project files and possible shell scripts for my own needs.

### Edit-Compile_Run Times

I can't remember this taking more than a few seconds, even to re-compile every module, no matter what era and what hardware was in use. (Of course, older, slower hardware would also be running smaller programs!)

And usually, you were familiar enough with your project to know which files required recompiles of all others. Maybe, interface files shared by all modules. So typically, only edited modules needed to be recompiled.

With the whole-program compilers now, all modules are recompiled, but typical build times these days are around 0.1 seconds on projects up to 40/50Kloc.
