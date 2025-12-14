## Language Development

I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as source backups, or the odd binary. Most projects are written in my M language, but the master sources and development are on my home PC.

'M' is mostly used for compilers, interpreters, assemblers, emulators, and any support libraries that are needed.

### Current Set of Languages
````
M          Lower level systems language (see below), in M7 and M8 mostly compatible versions
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL7       Stack-based IL used by MM and BCC compilers (this can be a complete standalone language)
PCL8       Lighter IL used by BB project (this is used internally only)
C-subset   The partly non-standard subset of C used by the BCC/MCC projects
ZA         Z80 assembler, with some tweaks on Zilog syntax
````

### Current Language Projects and Tools
````
Tool    Folder   Written in
BB      BX       M           Streamlined M8 compiler using PCL8-IL
QQ      QX       M           Bytecode compiler + interpreter for Q
MM      MX       M           M7 compiler for x64/Windows using PCL7
MC      MX       M           M7 transpiler for linear C via PCL7
AA      AX       M           Single-file assembler for x64-subset
BCC     CX       M           C-subset compiler using PCL7
MCC     CX       M           Version of BCC configured for one output: ASM in 'GAS' format.
'CC'    DX       M           (No identity yet) Self-contained fork of MCC with all extranous code removed.
                             This is intended as the basis for a version of MCC written in C
ZA      ZX       Q           Z80 Assembler
ZZ      ZX       M           Z80 Emulator

Shelved projects or possibilities:

MMA     MX       M           M7 compiler for ARM64/Linux via PCL7 (abandoned in early stages; it may be picked up
                             again for M8, and possibly adapted for ARM64/Windows)
MMZ     --       M           Cutdown M8 compiler targeting Z80 via PCL8; experimental
PC      MX       M           Processor for standalone PCL7 programs
````

### M Language

This was first created in 1981 for Z80, and then evolved over several generations of x86 up to x64. There was recently an attempt to port it to ARM64, but that got abandoned.

It has steadily evolved, but has also been deliberately kept low level. It has been self-hosted (written in itself or a previous version) in a continuous chain going to back to the 1980s.

This has then been my primary language for over 40 years. However, I haven't done any commercial work with it since the early 2000s; it is just a hobby now.

I specialise now in small, single-file, self-contained and very fast compilers with minimal external dependencies.

M is basically C but with utterly different syntax, and several modern conveniences. See [M Features](Mfeatures.md).
