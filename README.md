I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, charts, source backups, benchmark results, the odd binary. Most projects are written in my M language, but the master sources and development are on my home PC.

### Current Set of Languages
````
M          Lower level systems language
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL        Stack-based IL used by MM and BCC compilers (this can be a complete standalone language)

C-subset   The partly non-standard subset of C used by the BCC/MCC project. The subset is also
           the output of the MC product (MM configured with a C backend).
````

### Active Projects and Tools
````
Tool    Folder
BB      BX        (In development) M compiler. This works without an IL
MM      MX        M compiler using PCL (configured for X64/Windows target)
QQ      QX        Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run
                  to some extent on Linux
AA      AX        Single-file assembler for x64
BCC     CX        C-subset compiler using PCL
RUNMX   MX        Stub program for MX binaries produced by MM/AA/BCC/PC
ZZ      ZX        Z80 Assembler (and, sometime, emulator)
````

### Recently Dropped Projects
````
TCL        New 3AC-based IL, and various planned backends
BB/TCL     M compiled based on TCL
MCC/TCL    C-ubset compiled based on TCL
````

### Old Projects, still in some  use
````
AA6    Older AA assembler, can assemble multiple ASM files into one binary. Used by BCX,
       and needed to 'link' individual ASM files produced by BCC, for multi-module projects
QQ6    Older Q interpreter with twin dispatchers: slow function-table dispatcher, and fast
       threaded-code/ASM-based dispatcher. (QQ7 uses HLL code only using faster techniques.)
````

