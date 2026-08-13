## 'M' Compiler Suite

All tools run on and for x64 with Windows.


**'MM7' M Systems Compiler (v7 using PCL7)**
````
.m/.ma ────┬─> [mm.exe] ─┬────> .exe/.dll Files (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm (AA/AS/NASM/ML360 depends on config)
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

**'PC' PCLv7 Processor**
````
.pcl ────────> [pc.exe] ─┬────> .exe/.dll Files
                         ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm (AA/AS/NASM/ML360 depends on config)
                         ├────> .pct IL File
                         └────>  Interpret (IL code in memory)
````
**'MC' M to C Transpiler via PCLv7**
````
.m/.ma ──────> [mc.exe] ──────> .c Self-contained C source file
````
        
**'QQ' Q Interpreter**
````                 
.q/.qa ────┬─> [qq.exe] ──┬───> Run (compile to internal bytecode and immediately interpret)
.ml/.dll ──┘              └───> .qa File (create single amalgamated source file)
````
**'MZ' M Systems Compiler for Z80 (Derived from MM8 and PCL v8)**
````
.m/.ma ──────> [mz.exe] ─┬────> .za Z80 Assembly file
                         ├────> .z Z80 binary (via ZA)
                         └────> Run (Z80 binar via ZZ emulator)

.za ─────────> [za.q] ────────> .z Z80 binary

.z ──────────> [zz.exe] ──────> Run (Z80 binary via emulator)
````

**'BCC' C-subset Compiler (using PCLv7)**
````
.c ───────┬─> [bcc.exe] ─┬────> .exe/.dll Files
.ml/.dll ─┘              ├────> .ml/.mx Files
                         ├────> .obj File
                         ├────>  Run (native code in memory)
                         ├────> .asm (AA/AS/NASM/ML360 depends on config)
                         ├────> .pcl IL File
                         └────>  Interpret (IL code in memory)
````
The C subset is somewhere between C90 and C99. The compiler is non-conforming in many ways, but it can compile the
output of the MC product for example.

BCC is based around the MM7 backend which is designed for a whole-program compiler. C needs independent compilation, so BCC
can only build or compile one C source file at a time, and can only produce EXE/DLL for a one-module program.

For multi-module C programs, they are compiled one at a time to .asm files, then AA6 (which can still process multiple .asm
files) is used to assemble to EXE. Or another ASM format is chosen, then external assemblers and linkers can be used.

**'RUNMX' Launch MX Programs**
````
.mx ───────┬─> [runmx.exe] ───> Run (Load, fix up, and execute the MX-format executable)
.ml/.dll ──┘
````
### MM8
This was a streamlined version of MM7, with a leaner PCLv8 IL. However products like MC, BCC, AA depended on the MM7 backend, and
it was too much work to port them across. The PCL interpreter was also missing. MM7 also still managed somewhat smaller executables.

So although it was a tidier product, it was dropped and I moved back to MM7

### Packaging

All the above programs are single-file, self-contained executables, and all are under 1MB. All (except za.q) are written in my M systems language, and can be compiled with MM7 or MM8.
