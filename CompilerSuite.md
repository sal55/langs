## 'M' Compiler Suite
  

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
There are no external dependencies other than what is provided by Windows. Applications compiled or run with these may need external libraries. Some outputs (eg. OBJ, C) may need external tools to process further. (C may need an optimising compiler, or C could be generated to run on Linux.)

Interdependencies between these programs are:
* **MCC** needs **AA** to produce binary files
* **MM** needs **AA** to produce OBJ/ML/MX files

### Implementation

All products are written in my M language and built with **MM**. Single-file source amalgamations (MA files) can be generated for any project.

Building all of the above executables from source takes under 0.6 seconds.

#### Notes

* All products are for x64 processor running under Win64 ABI
* Any output files are always a single primary output file, one of EXE, DLL, OBJ, MA, ML, MX, PCL, C, ASM, with these exceptions:
   * **MCC**, being a C compiler, supports independent compilation. So there can be multiple OBJ, ASM, I output files each corresponding to one input file
   * **MM** producing DLL, ML can also write a corresponding exports or interface file that provides bindings for my M/Q languages
* With C-derived libraries used by my M/Q languages, I either write the necessary bindings by hand, or use the **MCC** compiler to generate as much as it can automatically. However this process is not 100%; a lot of manual work will still be needed.
* The **AA** assembler is unusual. Its input can be multiple .asm files, and it can generate one .exe file without requiring any linker. Or, the output can be ONE .obj file (not one per input file as is common).
* None of my tools take OBJ files as inputs, necessary for working with the outputs of other languages and compilers. An external linker is needed. Only **AA** can produce OBJ files.
* MA files are a single-file 'amalgamation' of all the source and support files used by a project. It is a convenient way to package distribute the source files, and can be directly built by **MM**.
* QA files are a similar thing for the Q language. Here there is no binary format so it is a convenient way to distribute an application
* ML and MX files, with the **RUNMX** program, may be dropped. ML was introduced to take the place of DLL files which for a year or two were faulty. Both ML/MX have some useful characteristics, but that might not be enough to maintain their use.

