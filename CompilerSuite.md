## 'M' Compiler Suite

All tools run on and for x64 with Windows.

**'MM' M Systems Compiler**
````
.m/.ma ────┬─> [mm.exe] ─┬────> EXE/DLL File (+ M/Q Interface module for DLL/ML)
.ml/.dll ──┘             ├────> [via aa.exe] ──> ML/MX Files
                         ├────> [via aa.exe] ──> OBJ File
                         ├────> ASM File
                         ├────> MA File
                         ├────> LIST/PROJ Files (info for my IDE)
                         └────> Run (immediately from memory)
````
**'AA' x64 Assembler/linker**
````
.asm ──────┬─> [aa.exe] ─┬────> EXE/DLL Files
.ml/.dll ──┘             ├────> ML/MX Files
                         └────> OBJ File
````
**'MCC' C Subset Compiler**
````
.c/.h ─────┬─> [mcc.exe] ──┬──> [via aa.exe] ───┬────> EXE/DLL Files
.dll ──────┘               │                    ├────> ML/MX Files
                           │                    └────> OBJ File
                           ├─────────────────────────> ASM File
                           ├─────────────────────────> I File (preprocessed)
                           └─────────────────────────> M/Q Interface modules (from .h files)
````
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
**'MMP' M Compiler to IL and Interpreter (Experimental)**
````
.m/.ma ───> [mmp.exe] ──────> PCL File

.pcl ─────> [pci.exe] ──────> Run
````
**'MC' M Compiler to C (Deprecated)**
````
.m/.ma ───> [mc.exe] ──────> C File
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

