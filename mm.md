````
    Inputs             Intermediates                                                              Outputs

    Ext Libs      ───>───────────────────────────────────────────────────┐
    Source File   ─┬─> AST1 ─> AST2 ─┬─> AST3 ─┬─> PCL ─┬─> MCL ─┬─> SS ─┴─┬─> EXE Image ──┬────> EXE File
    Include Files ─┘                 │         │        │        │         │               ├────> DLL File
    Strinclude    ───>───────────────┘         │        │        │         │               └────> OBJ File
                                               │        │        │         └─> MCU ─┬─> MCB ────> ML/MX Files
                                               │        │        │                  └─> MCX ────> (RUN native code) 
                                               │        │        ├──────────────────────────────> ASM File
                                               │        │        └──────────────────────────────> NASM File
                                               │        ├───────────────────────────────────────> (RUNP Interpret PCL)
                                               │        └───────────────────────────────────────> PCL Source File
                                               ├────────────────────────────────────────────────> MA File
                                               └────────────────────────────────────────────────> LIST/PROJ Files
````
(This is slightly out of date. Those external libs need to enter the process just before PCL, to allow for IL to be interpreted. Otherwise they are not needed after after SS as shown above.)
