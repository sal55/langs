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
**'BCC' C Subset Compiler (One Module only)**
````
.c/.h ────┬─> [bcc.exe] ─┬────> .exe/.dll File
.ml/.dll ─┘              ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run native code in memory
                         ├────> .asm File
                         ├────> .nasm File
                         ├────> .pcl IL File
                         └────>  Interpret IL code
````
(For conventional multi-module C programs, a driver script is used, which invokes BCC with ASM output for each module, and submits all to AA6 to produce EXE etc. AA7, the version described below, accepts one module only.)

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
**MM6** Previous M Compiler

**AA6** Previous x64 Assembler

This version is retained because it can process multiple ASM input files into one output (so performs a link function) which is needed when BCC needs to built multi-module projects.

### Packaging

All the above programs are single-file, self-contained executables, and all are under 1MB. The current set of programs are:
````
mm.exe        388 KB           Includes std library sources
aa.exe        121 KB
bcc.exe       321 KB           Includes std headers (windows.h is separate)
qq.exe        553 KB           Includes std lib sources
pc.exe        183 KB           Fully loaded (smaller configurations can be done, eg. interpret only)
runmx.exe      57 KB           (Includes diagnostic display)
````
The above are built with MM7, which can now match MM6 in code quality.

### Implementation

All products are written in my M language and built with **MM**. Single-file source amalgamations (MA files) can be generated for any project.

Building all of the above executables from source takes under 0.4 seconds in total.

