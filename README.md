I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, charts, source backups, benchmark results, the odd binary. Most projects are written in my M language, but the master sources and development are on my home PC.

### Current Set of Languages
````
M          Lower level systems language
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL        Stack-based IL used by MM compilers (this can be a complete standalone language)
TCL        3AC-based IL used by experimental BB compiler (an internal language that can
           only be dumped to textual form).
C-subset   The partly non-standard subset of C used by the BCC/MCC project. The subset is also
           the output of the MC product (MM configured with a C backend).
````

### Active Projects and Tools
````
Tool    Folder
BB      BX        (In development) M compiler using new TCL
MCC     DX        (In development) C-subset compiler using new TCL

MM      MX        M compiler using PCL (configured for X64/Windows target)
QQ      QX        Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run
                  to some extent on Linux
AA      AX        Single-file assembler for x64
BCC     CX        C-subset compiler using PCL
RUNMX   MX        Stub program for MX binaries produced by MM/AA/BCC/PC
ZZ      ZX        Z80 Assembler (and, sometime, emulator)
````

### TCL: Planned Backend Targets
````               
          OS             Outputs supported
  x64R    W       0%    .exe .dll .obj .asm .nasm .mx Run
  x64M    W      99%    .exe .dll .obj .asm .nasm .mx Run
  LinC    W/L    95%    .c
  RUN     W/L    95%
  A64R    L      10%    .s
  TCL     -      99%    .tcl   (Dump TCL generated via API to textual form)

-R    Temporaries and/or some locals are register-based
-M    Temporaries and locals are memory-based only (can be quite slow)

W/L   Can work on either OS (may need a -Linux option to switch in suitable OS-speficic module).
      Linear C requires a C compiler for the platform
      Interpreted IL requires a compiler that includes the interpreter, to be compiled via Linear C for the platform.
      But applications will either work on one OS only, unless they are OS-agnostic, or use cross-platform libraries
````

All TCL stuff is under development

### Old Projects (still in use)
````
AA6    Older AA assembler, can assemble multiple ASM files into one binary. Used by BCX,
       and needed to 'link' individual ASM files produced by BCC, for multi-module projects
QQ6    Older Q interpreter with twin dispatchers: slow function-table dispatcher, and fast
       threaded-code/ASM-based dispatcher. (QQ7 uses HLL code only using faster techniques.)
````

