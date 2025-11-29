## Language Development

I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, source backups, benchmark results, and the odd binary. Most projects are written in my M language, but the master sources and development are on my home PC.

Currently I'm working on a version 8 of my M compiler (codenamed BB below).

### Current Set of Languages
````
M          Lower level systems language (see below), in M7 and M8 mostly compatible versions
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL7       Stack-based IL used by MM and BCC compilers (this can be a complete standalone language)
PCL8       Lighter IL used by BB project (this is used internally only)

C-subset   The partly non-standard subset of C used by the BCC/MCC projects

````

### Active Language Projects and Tools
````
Tool    Folder   Written in
BB      BX       M           M8 compiler using PCL8-IL. This lacks Linear-C targets, and PCL interpreter
QQ      QX       M           Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run
                             to some extent on Linux by compiling with MC
MM      MX       M           M7 compiler using PCL7-IL (configured for X64/Windows target)
MC      MX       M           M7 compiler targeting linear C via PCL7 (Windows/Linux target)
AA      AX       M           Single-file assembler for x64-subset
BCC     CX       M           C-subset compiler using PCL7
MCC     CX       M           Version of BCC configured for one output: ASM in 'GAS' format.
'CC'    DX       M           (No identity yet) Self-contained fork of MCC with all extranous code removed.
                             This is intended as the basis for a version of MCC written in C
RUNMX   MX       M/C         Stub program for MX binaries produced by MM/AA/BCC. M and C versions exist
ZZ      ZX       Q           Z80 Assembler (and, sometime in the future, emulator)

````


### M Language

This was first created in 1981 for Z80, and then evolved over several generations of x86 up to x64. There was recently at attempt to port it to ARM64, but that got abandoned.

It has steadily evolved, but has also been deliberately kept low level. It has been self-hosted (written in itself or a previous version) in a continuous chain going to back to the 1980s.

This has then been my primary language for over 40 years. However, I haven't done any commercial work with it since the early 2000s; it is just a hobby now.

I specialise now in small, single-file, self-contained and very fast compilers with minimal external dependencies.

M is basically C but with utterly different syntax, and several modern conveniences. See [M Features](Mfeatures.md).
