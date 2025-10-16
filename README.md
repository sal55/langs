## Language Development

I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, source backups, benchmark results, and the odd binary. Most projects are written in my M language, but the master sources and development are on my home PC.

Currently I'm working on a version 8 of my M compiler (codenamed BB below).

### Current Set of Languages
````
M          Lower level systems language (see below)
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL7       Stack-based IL used by MM and BCC compilers (this can be a complete standalone language)
PCL8       IL used by BB project

C-subset   The partly non-standard subset of C used by the BCC/MCC projects

````

### Active Projects and Tools
````
Tool    Folder
BB      BX        (In development) M compiler using PCL8-IL
MM      MX        M compiler using PCL7-IL (configured for X64/Windows target)
MC      MX        M compiler targeting linear C via PCL (Windows/Linux target)
QQ      QX        Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run
                  to some extent on Linux by compiling with MC
AA      AX        Single-file assembler for x64-subset
BCC     CX        C-subset compiler using PCL7-IL
MCC     CX        Version of MCC configured for one output: ASM in 'GAS' format.
RUNMX   MX        Stub program for MX binaries produced by MM/AA/BCC
ZZ      ZX        Z80 Assembler (and, sometime, emulator)

Upcoming:
DD      DX        Version of BCC/MCC based around PCL8-IL (BCC8/MCC8)
??      ??        Original C version of MCC8
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

It has steadily evolved, but has also been deliberately kept low level. It has been self-hosted (written in itself or a previous version) in a continuous chain going to back to the 1980s.

This has then been my primary language for over 40 years. However, I haven't done any commercial work with it since the early 2000s; it is just a hobby now.

I specialise now in small, single-file, self-contained and very fast compilers with minimal external dependencies.

M is basically C but with utterly different syntax, and several modern conveniences. See [M Features](Mfeatures.md).
