## 'M' Compiler Suite
  

**'MM' M Systems Compiler**
````
.m/.ma ────┬─> [mm.exe] ─┬────> EXE/DLL Files
.ml/.dll ──┘             ├────> ML/MX Files (+ M/Q Interface modules)
                         ├────> ASM File
                         ├────> MA File
                         └────> Run
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
* When generating ML/DLL library files, only MM, working from M code, can also generate a corresponding import module that automatically provides the bindings for use from M/Q languages.
* With C-derived libraries, I either write the necessary bindings by hand, or use the MCC compiler to generate as much as it can automatically. However this process is not 100%; a lot of manual work will still be needed.
* The **AA** assembler is unusual. Its input can be multiple .asm files, and it can generate one .exe file without requiring any linker. Or, the output can be ONE .obj file (not one per input file as is common). However none of my tools will take .obj files as input (or .o, .a, .lib etc). For working with the outputs of other compilers and languages, OBJ must be generated from AA, and an external linker used.
* **RUNMX** is needed to run MX programs, as otherwise this is not a format that Windows knows how to launch. (I can do file-association, but that would only tell Windows to use RUNMX.) MX files are a by-product of ML files, and might be of benefit in being less visible to AV software.

Another advantage of the MX format is that it can use ML shared libraries (as well as DLL of course), as easily as EXE can use DLL. An EXE can't directly use an ML library without some special code inside it (qq.exe has that code, but libraries are for access from Q programs).

ML libraries have an advantage over DLL in that they share the same environment as the host. You can't for example use a file handle created in the host, and close it in a DLL library, as they will use different instances of MSVCRT.DLL.

(Since the DLL/OBJ low-memory problems have been fixed, there is less need for ML files. So they may eventually be dropped. They were a by-product of the fixups need to run code directly in memory - the `Run` option in the chart for MM/MCC compilers.)
