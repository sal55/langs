                                   
M Compiler  ──>──┐
                 ├──>─ PCL ──>──┬─── PCL text (dump)
C Compiler  ──>──┘ 

    Inputs             Intermediates                                                            Outputs

    Ext Libs      ───>─────────────────────────────────────────────────┐
    Source Files  ─┬─> AST1 ─> AST2 ─┬─> AST3 ─┬─> PCL ─> MCL ─┬─> SS ─┴─┬─> EXE Image ──┬────> EXE File
    Include Files ─┘                 │         │               │         │               ├────> DLL/EXP Files
    Strinclude    ───>───────────────┘         │               │         │               └────> OBJ File (via AA)
                                               │               │         └─> MCU ─┬─> MCB ─┬──> ML/EXP Files (via AA)
                                               │               │                  │        │
                                               │               │                  │        └──> MX File (via AA)
                                               │               │                  └─> MCX ────> (Run)
                                               │               └──────────────────────────────> ASM File
                                               ├──────────────────────────────────────────────> MA File
                                               └──────────────────────────────────────────────> LIST/PROJ Files


PCL Text

PCI (Interpreter)

Win-x64    EXE/DLL/OBJ/MX Binary files
           ASM/NASM Text Files
           RUN

Lin-x64    MX Binary file
           NASM Text File
           RUN

Lin-ARM64  AT&T ASM file

Z80 

Linear C
           


````

#### Inputs
````
Source Files:     There is only ever one `.m` source file submitted
