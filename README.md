## Summary of Language Projects 2023

August 2023

These are private languages of mine. This is where docs and summaries about them come together.

### Languages
```
Language  Description (Notes)

M         Lower level systems language
Q         Lower level dynamic scripting language
ASM       x64 assembly in my take on Intel syntax
PCL       Intermediate language similar to the IL of the M compiler (1)
ZA        Z80 assembly based around Zilog syntax (2)
MS        New embedded scripting language based on a cut-down Q language (9)
```
### Tools
```
Tool     Impl In  Description

mm.exe   M        M Compiler
qq.exe   M        Q bytecode compiler and interpreter
aa.exe   M        ASM assembler
pci.exe  M        Interpreter of of PCL files
zz.qa    Q        ZA assembler

mmx.exe   M       M compiler that runs programs from source (3)
mmp.exe  M        Independent M compiler that generates textual PCL files
run.exe  M        Stub program to run .mx executables (4)
```

### File Extensions
```
Extension  Represents  Description

.m         Module      M source file
.ma        Program     M amalgamated source file (5)
.q         Module      Q source file
.qa        Program     Q amalgamated source file (6)
.ms        Program     MS source file
.pcl       Program     PCL source code
.asm       Program*    AA assembly source file (*Module when generated by BCC compiler)
.za        Program     ZA assembly source file (there are no modules)
.ml        Library     Private shared library format (4)
.mx        Program     Private executable code format
.exp       Library     Temporary output file of -ml/-mx options: exports info

.exe       Program     Windows executable file (PE+)
.dll       Library     Windows shared dynamic library (PE+) (7)
.obj       Program*    Object file (COFF64/PE+) (*Can be module) (8)
```

### Inputs and Outputs
```
Tool      Input   N   Output   Option    Description

mm.exe    .m .ma  1   .exe     -exe      (Default) Compile to executable
                      .asm     -asm      Write single-file assembly file
                      .ma      -ma       Write amalgamated source file
                               -mas      Same but includes standard library files
                      .ml      -ml       Write shared library file in private format
                      .mx      -mx       Write executable in private format
                      Run      -run      Compile and run in-memory (ms.exe is better for this)

qq.exe    .q .qa  1   Run                (Default) Compile and run bytecode in-memory
                      .qa      -qa       Write amalgamated source file ...
                               -qas      ... with standard library

ms.exe    .ms     1   Run                Run MS script in standalone or embedded interpreter

aa.exe    .asm    N  .exe      -exe      (Default) Create executable
                     .obj      -obj      Write single object file (6)
                     .dll      -dll      Write DLL file (7)

mmx.exe   .m .ma  1  Run                 Run M program from source

mmp.exe   .m .ma  1  .pcl                Compile to single PCL source file

pci.exe   .pcl    1  Run                 Interpret PCL program

run.exe   .mx     1  Run                 Run .mx application

zz.qa     .za     1  -         -asm      (Default) Assemble to in-memory machine code
                     Run       -run      (Not ready) Assemble and run
```

N indicates the number of input files accepted. Only `bcc.exe`, and `aa.exe` because it was used to combine .asm outputs of BCC, accept multiple source files. All my own languages generally work from a single submitted module.

### Notes

**(1)** `PCL` was the name of the discrete IL I'd planned for the M compiler, and I also worked on an interpreter for it. But the project became too much and was dropped. The special M compiler `mmp.exe` and the interpreter `pci.exe` are retained as an independent project. I had ideas of using it as a debugging tool for M programs

**(2)** This was a project involving a Z80 assembler (completed), and an emulator (just started). It was shelved when I realised how much effort was needed to properly emulate Z80.

**(3)** `mmx.exe` is simply a renamed `mm.exe`. The M compiler checks the name of the executable, and if it is `mmx`, it will automatically invoke the `-run` option (and uses less verbose output, to make the compilation process transparent and the illusion of running a script language better)

**(4)** (See **7**) Because of problems with DLLs, I created my own simpler shared library format using `.ml` files. These are accepted by Q programs, but in M, can only be transparently used by `.mx` files, or accessed via a special API from regular `.exe`.

The format can also be used to write complete apps, with `.mx` extension, but being unknown to Windows, those must be started by a stub program `run.exe`

**(5)** The `-ma` option of M compiler combines all source files and support files (from `include` and `strinclude`) into a single file. This is easy to backup, transmit, copy etc. And it can be directly be compiled in that form: `mm prog.ma`.

**(6)** The `-qa` option does a similar job for Q programs, and there is is necessary for packaging applications, as there is no longer a binary file format for bytecode

**(7)** `mm.exe` used to produce DLL files, which could be relocated with the low 2GB of virtual address space. But sometimes it can be loaded higher, and I think that caused the problems I sometimes had. Loading above 2GB requires RIP-relative addressing, and other code-generation to choices to get around the problem of absolute addresses above 2GB not fitting into a 32-bit field. My code generators don't yet do that.

**(8)** OBJ files can still be produced by `aa.exe`, to allow combining with other software. But they need a third party linker, and these now like to create images that can also be loaded above 2GB. So at present DLL and OBJ outputs are shelved until I can sort that out.

**(9)** MS is a new project just started. Short for M-Script.


