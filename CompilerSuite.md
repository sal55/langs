## 'M' Compiler Suite
  

**'MM' M Systems Compiler**
````
.m/.ma ────┬─> [mm.exe] ─┬────> EXE File
.ml/.dll ──┘             ├────> ML/MX/DLL Files (+ M/Q Interface modules)
                         ├────> ASM File
                         ├────> MA File
                         └────> Run
````
**'AA' x64 Assembler/linker**
````
.asm ──────┬─> [aa.exe] ─┬────> EXE File
.ml/.dll ──┘             ├────> ML/MX/DLL Files
                         └────> OBJ File
````
**'MCC' C Subset Compiler**
````
.c/.h ─────┬─> [mcc.exe] ──┬──> [via aa.exe] ───┬────> EXE File
.dll ──────┘               │                    ├────> ML/MX/DLL Files
                           │                    └────> OBJ File
                           ├─────────────────────────> ASM File
                           ├─────────────────────────> I File (preprocessed)
                           └─────────────────────────> M/Q Files (interface modules from .h files)
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

#### Notes

* All products are for x64 processor running wunder Win64 ABI
* x64 code is generated for low-memory (first 2GB). This can cause problems for DLLs if loaded high, and for OBJ files is submitted to a linker that uses a high image-base.
* Nevertheless, DLL output has been reinstated (it still mainly works), and OBJ is still an option. The high memory thing will be fixed at some point.
* When generating ML/DLL library files, only MM, working from M code, can also generate a corresponding import module that automatically provides the bindings for use from M/Q languages.
* With C-derived libraries, I either write the necessary bindings by hand, or use the MCC compiler to generate as much as it can automatically. However this process is not 100%; a lot of manual work will still be needed.
* The **AA** assembler is unusual. Its input can be multiple .asm files, and it can generate one .exe file without requiring any linker. Or, the output can be ONE .obj file (not one per input file as is common). However none of my tools will take .obj files as input (or .o, .a, .lib etc). For working with the outputs of other compilers and languages, OBJ must be generated from AA, and an external linker used (when the 2GB thing has been fixed).
* **RUNMX** is needed to run MX programs, as otherwise this is not a format that Windows knows how to launch. (I can do file-association, but that would only tell Windows to use RUNMX.) MX files are a by-product of ML files, and might be of benefit in being less visible to AV software.

