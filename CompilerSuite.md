## 'M' Compiler Suite

All tools run on and for x64 with Windows.

**'MM' M Systems Compiler (MM7)**
````
.m/.ma ────┬─> [mm.exe] ─┬────> .exe/.dll Files (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm File (syntax for my AA assembler)
                         ├────> .nasm File (NASM syntax, if configured)
                         ├────> .pcl IL File
                         ├────>  Interpret (IL code in memory)
                         ├────> .ma File (create single amalgamated source file)
                         ├────> .c File via PCL (available via 'MC' config; see below)
                         └────> .list/.proj Files (info for my IDE)
````

**'PC' PCL Processor**
````
.pcl ──────┬─> [pc.exe] ─┬────> .exe/.dll Files
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run native code in memory
                         ├────> .asm File
                         ├────> .nasm File (if configured)
                         ├────> .pcl IL File (uses .pct extension)
                         ├────> .c File (if configured)
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

**'QQ' Q Interpreter (QQ7)**
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
**'MC' M to C Transpiler (MC7)**
````
.m/.ma ──────> [mc.exe] ──────> .c File (linear, stripped C generated via PCL/IL)

````

### Packaging

All the above programs are single-file, self-contained executables, and all are under 1MB. The current set of programs are:
````
mm.exe        403 KB           Includes std library sources
aa.exe        121 KB
qq.exe        508 KB           Includes std lib sources
pc.exe        184 KB           Fully loaded (smaller configurations can be done, eg. interpret only)
runmx.exe      57 KB           (Includes diagnostic display)
````

### Implementation

All products are written in my M language and built with **MM**. Single-file source amalgamations (MA files) can be generated for any project.

Building all of the above executables from source takes about 1/3 second in total.

### Versions

All products will share the same major version number 7. This ensures that can all work together.
