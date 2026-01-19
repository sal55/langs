## 'M' Compiler Suite

All tools run on and for x64 with Windows.

**'BB' M Systems Compiler (v8 using PCL8)**
````
.m/.ma ────┬─> [bb.exe] ─┬────> .exe/.dll Files (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm File (syntax for my AA assembler)
                         ├────> .asm File (AT&T/GAS syntax, if configured)
                         ├────> .ma File (create single amalgamated source file)
                         └────> .list/.proj Files (info for my IDE)
````

**'MM' M Systems Compiler (v7 using PCL7)**
````
.m/.ma ────┬─> [mm.exe] ─┬────> .exe/.dll Files (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm File (syntax for my AA assembler)
                         ├────> .asm File (AT&T/GAS syntax, if configured)
                         ├────> .pcl IL File
                         ├────>  Interpret (IL code in memory)
                         ├────> .ma File (create single amalgamated source file)
                         ├────> .c File via PCL (available via 'MC' config; see below)
                         └────> .list/.proj Files (info for my IDE)
````
**'AA' x64 Assembler/linker (AA7)**
````
.asm ──────┬─> [aa.exe] ─┬────> .exe/.dll Files
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm File (syntax for my AA assembler) (uses .aa extension)
                         └────> .asm File (AT&T/GAS syntax when configured)                      
````

**'QQ' Q Interpreter**
````                 
.q/.qa ────┬─> [qq.exe] ──┬───> Run (compile to internal bytecode and immediately interpret)
.ml/.dll ──┘              └───> .qa File (create single amalgamated source file)
````
**'MZ' M Systems Compiler for Z80 (Derived from BB)**
````
.m/.ma ──────> [mz.exe] ─┬────> .za Z80 Assembly file
                         ├────> .z Z80 binary (via ZA)
                         └────> Run (Z80 binar via ZZ emulator)

.za ─────────> [za.q] ────────> .z Z80 binary

.z ──────────> [zz.exe] ──────> Run (Z80 binary via emulator)
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

All the above programs are single-file, self-contained executables, and all are under 1MB. All (except za.q) are written in my M systems language, and can be compiled with MM or BB.
