## Summary of Language Projects 2023

October 2023

These are private languages of mine. This is where docs and summaries about them come together.

### Languages
```
Language  Description (Notes)

M         Lower level systems language
Q         Lower level dynamic scripting language
ASM       x64 assembly in my take on Intel syntax
'C'       C subset used by mcc compiler
```
The following are intermediate languages (not exposed and not independent):
```
PCL(M)    Intermediate Language used by M compiler
PCL(Q)    Bytecode language used b the Q interpreter
```
### Tools
```
Tool       Purpose         Inputs             Outputs

mm.exe     M Compiler      .m .ma .dll       .exe .dll .ml. .mx .asm .ma RUN
aa.exe     x64 Assembler   .asm .dll         .exe .dll .obj .ml .mx
mcc.exe    C Compiler      .c .dll           .asm .i (-e option) .m (-mheaders/-qheaders)
                                             .exe .dll .obj .ml .mx (by invoking aa.exe)
qq.exe     Q Interpreter   .q .qa            RUN
runmx.exe  Launch MX File  .ml               RUN

### Tool Locations

```
Tool        Folder  Impl In  Description

mm.exe      mx      M        M Compiler
runmx.exe   mx      M        Stub program to run .mx executables
aa.exe      ax      M        x64 assembler
qq.exe      qx      M        Q bytecode compiler and interpreter
mcc.exe     cx      M        C subset compiler
```

### File Extensions
```
Extension  Represents  Description

.m         Module      M source file
.ma        Program     M amalgamated source file
.ml        Library     Private shared library format
.mx        Program     Private executable code format
.asm       Program*    AA assembly source file (*Module when generated by MCC compiler)
(.nasm     Program*    Proposed assembly source file using NASM syntax)
.q         Module      Q source file
.qa        Program     Q amalgamated source file (6)
```
