## Language Development

I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, source backups, benchmark results, and the odd binary. Most projects are written in my M language, but the master sources and development are on my home PC.

Currently I'm working on a version 8 of my M compiler (codenamed BB below), but it's been going in various different directions for months.  It is gradually converging towards a scheme where AST is directly converted to native code, but using the operand model of the stack IL.

### Current Set of Languages
````
M          Lower level systems language (see below)
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL        Stack-based IL used by MM and BCC compilers (this can be a complete standalone language)

C-subset   The partly non-standard subset of C used by the BCC roject. The subset is also
           the output of the MC product (MM configured with a C backend).
````

### Active Projects and Tools
````
Tool    Folder
BB      BX        (In development) M compiler. This works without an IL
MM      MX        M compiler using PCL-IL (configured for X64/Windows target)
MC      MX        M compiler targeting linear C via PCL (Windows/Linux target)
QQ      QX        Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run
                  to some extent on Linux by compiling with MC
AA      AX        Single-file assembler for x64-subset
BCC     CX        C-subset compiler using PCL-IL
RUNMX   MX        Stub program for MX binaries produced by MM/AA/BCC
ZZ      ZX        Z80 Assembler (and, sometime, emulator)
````

### Old Projects, still in some  use
````
AA6    Older AA assembler, can assemble multiple ASM files into one binary. Needed to 'link' individual ASM files produced by BCC,
       for multi-module projects
QQ6    Older Q interpreter with twin dispatchers: slow function-table dispatcher, and fast
       threaded-code/ASM-based dispatcher. (QQ7 uses HLL code only using faster techniques.)
````

### M Language

This was first created in 1981 for Z80, and then evolved over several generations of x86 up to x64. There was recently at attempt to port it to ARM64, but that got abandoned.

It has steadily evolved, but has also been deliberately kept low level. It has been self-hosted (written in itself or a previous version) in a continuous chain going to back to the 1980s, with original versions (there were a couple of reboots) written in assembly. For the very first version, I also wrote the assembler (in hex machine code).

This has then been my primary language for over 40 years. However, I haven't done any commercial work with it since the early 2000s; it is just a hobby now.

I specialise now in small, single-file, self-contained and very fast compilers with minimal external dependencies.

M is basically C but with utterly different syntax, and several modern conveniences. See [M Features](Mfeatures.md).
