## 'M' Compiler Suite

All tools run on and for x64 with Windows.

**'MM' M Systems Compiler (MM7)**
````
.m/.ma ────┬─> [mm.exe] ─┬────> .exe/.dll Files (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm File (syntax for my AA assembler)
                         ├────> .nasm File (NASM syntax)
                         ├────> .pcl IL File
                         ├────>  Interpret (IL code in memory)
                         ├────> .ma File (create single amalgamated source file)
                         └────> .list/.proj Files (info for my IDE)
````
**'CC' C Subset Compiler (One Module only)**
````
.c/.h ────┬─> [cc.exe] ──┬────> .exe/.dll File
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run native code in memory
                         ├────> .asm File
                         ├────> .nasm File
                         ├────> .pcl IL File
                         └────>  Interpret IL code
````
(For conventional multi-module C programs, a driver script is used, which invokes CC with ASM output for each module, and submits all to AA6 to produce EXE etc. AA7, the version described below, accepts one module only.)

**'PC' PCL Processor**
````
.pcl ──────┬─> [pc.exe] ─┬────> .exe/.dll Files
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run native code in memory
                         ├────> .asm File
                         ├────> .nasm File
                         ├────> .pcl IL File (uses .pct extension)
                         └────>  Interpret IL code                      
````
**'AA' x64 Assembler/linker (AA7)**
````
.asm ──────┬─> [aa.exe] ─┬────> .exe/.dll Files
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm File (syntax for my AA assembler) (uses .aa extension)
                         └────> .nasm File (NASM syntax)                      
````

**'QQ' Q Interpreter**
````
.q/.qa ───> [qq.exe] ──┬────> Run (compile to internal bytecode and immediately interpret)
              ↑	       └────> .qa File (create single amalgamated source file)
.ml/.dll ─────┘ 

````
**'RUNMX' Launch MX Programs**
````
.mx ───────┬─> [runmx.exe] ───> Run (Load, fix up, and execute the MX-format executable)
.ml/.dll ──┘
 ````

### Packaging

All the above programs are single-file, self-contained executables, and all are under 1MB. The current set of programs are:
````
mm.exe        394 KB           Includes std library sources
aa.exe        120 KB
cc.exe        333 KB           Includes std headers (windows.h is separate)
qq.exe        578 KB           Includes std lib sources
pc.exe        180 KB           Fully loaded (smaller configurations can be done, eg. interpret only)
runmx.exe      60 KB
````
The above are built with MM6; MM7 described above doesn't have the small optimising step that reduces the code size, so they might be 10% bigger.

### Implementation

All products are written in my M language and built with **MM**. Single-file source amalgamations (MA files) can be generated for any project.

Building all of the above executables from source takes under 0.4 seconds (just over 0.3 seconds if I optimise the M compiler via C).

