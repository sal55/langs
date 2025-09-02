I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, charts, source backups, benchmark results, the odd binary. It had been used also for random stuff related to posts made to my Reddit account, but that has now stopped.

### Current Set of Languages
````
M          Lower level systems language
Q          Dynamic scripting language
AA         x64-subset assembler using my syntax
PCL        Stack-based IL used by MM compilers (this can be a complete standalone language)
TCL        3AC-based IL used by experimental BB compiler (an internal language that can
           only be dumped to textual form).
C-subset   The partly non-standard subset of C used by the BCC project. The subset is also
           the output of the MC projects.
````

### Mature Projects

````
MM      M compiler using PCL.
QQ      Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run
        to some extent on Linux
AA      Single-file assembler for x64
BCC     C-subset compiler x64/Win64 ABI. Can only handle one file at a time
PC      PCL processor for standalone PCL source files (to be deprecated)
RUNMX   Stub program for MX binaries produced by MM/AA/BCC/PC
````

### Supported Backend Targets
````               
                             (PCL)   (PCL)   -      -       (TCL)   (TCL)
Output         CPU   OS      MM      BCC     AA      RUNMX   BB      MCC

EXE file       x64   W       Y       Y       Y       -       Y       Y    
DLL file       x64   W       Y       Y       Y       -       Y       Y
OBJ file       x64   W       Y       Y       Y       -       Y       Y
MX/ML file     x64   W       Y       Y       Y       -       Y       Y
ASM AA file    x64   W       Y       Y       Y       -       Y       Y
ASM NASM file  x64   W       Y*      Y       Y       -       Y*      Y*
Linear C       -     W/L     Y*      -       -       -       Y*      Y*
PCL file       -     -       Y       Y       -       -       -       -
TCL file       -     -       -       -       -       -       Y       Y
RUN in-mem     x64   W       Y+      Y       Y       Y       -       -
Interpret IL   -     W/L     Y+      Y       -       -       (Y)     (Y)  (New project)
ASM AS file    A64   L       -       -       -       -       Y       -    (Shelved project)

*     means the product needs to be specially configured for that output
-     means the product could be specially configured so it is supported directly, or it can just
      remain an option on the main program.

W/L   Can work on either OS (may need a -Linux option to switch in suitable OS-speficic module).
      Linear C requires a C compiler for the platform
      Interpreted IL requires a compiler that includes the interpreter, to be compiled via Linear C for the platform.
      But applications will either work on one OS only, unless they are OS-agnostic, or use cross-platform libraries
````
The columns under (TCL) are current development. A 'Y' means at least partial support

### Projects In Development

````
BB      M compiler using TCL
MCC     C-subset compiler using PCL
TCL     New 3AC intermediate language and API (developed within BB/MCC projects)
````
Most development actually revolves around supporting different backend sub-projects

### Old Projects
````
BCX    Original C-subset compiler, can compile multiple modules into EXE, uses integrated
      AA6 assembler
AA6    Older AA assembler, can assemble multiple ASM files into one binary. Used by BCX,
       and needed to 'link' individual ASM files produced by BCC, for multi-module projects
QQ6    Older Q interpreter with twin dispatchers: slow function-table dispatcher, and fast
       threaded-code/ASM-based dispatcher. (QQ7 uses HLL code only using faster techniques.)
````

### Program Representations

This is for 'M' applications. There is the original source, and the final binary, but there are in-between representations too:
````
File type      Process with    Description

prog.m         MM or BB        Original source file of project. This will be the lead module of possibly many modules
prog.ma        MM or BB        Single, amalgamated, human readable source file. Created with -ma options
prog.pcl       PC              IL file in discrete source format. Created with -p option
prog.tcl       --              IL file as readable text dump, created with -t option. (For human use only)
prog.c         <C compiler>    Transpiled to C via PCL or TCL
prog.asm       AA              x64 native code as ASM source format in 'AA' format, created with -a
prog.masm      NASM            x64 native code as ASM source format, in NASM, created with -a when configured
prog.s         AS              A64 natiive code in AT&T format
prog.obj       <linker>        Object code file (-obj option)
prog.mx        RUNMX           Binary executable in MX format
prog.exe       <OS launcher)   Normal executable
````
These also list the possible forms of distribution. All have problems: .m/.ma need my compiler (a binary), while .EXE is also a binary that can
attract attention from AVs.

Viable formats are .c files, or .nasm files
