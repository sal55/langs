## M Compiler Summary and Timeline

M was never a general-purpose, cross-platform language. It always targetted whatever platform was current, in the world of personal and small business computers.

Processor | Bits| OS | `int` | `real` | Pointer size | Year | Other
--- | --- | --- | --- | --- | --- | --- | ---
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

I've never bothered with these until re
