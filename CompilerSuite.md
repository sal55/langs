
  

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
.c ────────┬─> [mcc.exe] ──┬──> [via aa.exe] ───┬────> EXE File
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

All compilers generate native code for x64 for Win64 ABI. Currently limited to low 2GB for code.

`DLL` is reinstated, as it generally works, but it needs the high code fix to work consistently.

`OBJ` is available but needs an external linker that will create programs with an image base in low memory. (I will fix this low/high memory problem in due course.)

With `QQ`, external libraries are accessed on-demand rather than on program load.

When generating ML/DLL library files, only MM, working from M code, will also generate a corresponding import module that provides the bindings for use from M/Q languages.

With C-derived libraries, I either

