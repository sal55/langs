## 'M' Compiler Suite

All tools run on and for x64 with Windows.

**'MM' M Systems Compiler**
````
.m/.ma ────┬─> [mm.exe] ─┬────> EXE/DLL File (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> ML/MX Files
                         ├────> OBJ File
                         ├────> Run native code in memory
                         ├────> ASM File
                         ├────> NASM File
                         ├────> PCL IL File
                         ├────> Interpret IL code
                         ├────> MA File
                         └────> LIST/PROJ Files (info for my IDE)
````
**'AA' x64 Assembler/linker**
````
.asm ──────┬─> [aa.exe] ─┬────> EXE/DLL Files
.ml/.dll ──┘             ├────> ML/MX Files
                         └────> OBJ File
````
**'CC' C Subset Compiler (One Module only)**
````
.c/.h ────┬─> [cc.exe] ──┬────> EXE/DLL File
.ml/.dll ──┘             ├────> ML/MX Files
                         ├────> OBJ File
                         ├────> Run native code in memory
                         ├────> ASM File
                         ├────> NASM File
                         ├────> PCL IL File
                         └────> Interpret IL code
````
(For conventional multi-module C programs, a driver program BCC is used, which invokes CC with ASM output for each module, and submits all to AA to produce EXE etc.)

**'QQ' Q Interpreter**
````
.q/.qa ───> [qq.exe] ──┬────> Run
              ↑	       └────> QA File
.ml/.dll ─────┘ 

````
**'RUNMX' Launch MX Programs**
````
.mx ───────┬─> [runmx.exe] ───> Run
.ml/.dll ──┘
 ````

### Packaging

All the above programs are single-file, self-contained executables, and all are under 1MB. The current set of programs are:
````
mm.exe        385 KB
aa.exe         96 KB
mcc.exe       279 KB
qq.exe        574 KB
runmx.exe      13 KB
mmp.exe       318 KB
pci.exe        90 KB
mc.exe        319 KB
````


### Implementation

All products are written in my M language and built with **MM**. Single-file source amalgamations (MA files) can be generated for any project.

Building all of the above executables from source takes under 0.6 seconds.

