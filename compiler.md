My systems language compiler.

````
    Inputs             Intermediates                                                              Outputs
                                                 A         B        C       D               E
    Ext Libs      ───>───────────────────────────┐
    Source File   ─┬─> AST1 ─> AST2 ─┬─> AST3 ─┬─┴──> PCL ─┬─> MCL ─┬─> SS ─┬─> EXE Image ──┬───> EXE File
    Include Files ─┘                 │         │           │        │       │               └───> DLL File
    Strinclude    ───>───────────────┘         │           │        │       ├───────────────────> OBJ File
                                               │           │        │       └─> MCU ─┬─> MCB ───> ML/MX Files
                                               │           │        │                └─> MCX ───> (RUN native code) 
                                               │           │        ├───────────────────────────> ASM File
                                               │           │        └───────────────────────────> NASM File (Config option)
                                               │           ├────────────────────────────────────> (RUNP Interpret PCL)
                                               │           ├────────────────────────────────────> PCL Source File
                                               │           └────────────────────────────────────> C Source File (Config option)
                                               ├────────────────────────────────────────────────> MA File
                                               └────────────────────────────────────────────────> LIST/PROJ Files

Key:
Source File  Lead module of application (whole-program compiler, it will discover all other modules)
ASTn         Different stages of abstract syntax tree
PCL          Intermediate language (internal data structure)
MCL          Native code (internal data structure)
SS           Native code (actual binary plus relocation info)
EXE/DLL      Executable binary in official OS format        
MCU etc      Executable binary in in-memory format
MA           Bundled source and support files for whole app
Ext Libs     External DLL/.so dependencies

````
