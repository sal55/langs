## B Compiler Structure

````
    Inputs              Intermediates                                                     Outputs
    Ext Libs (DLL) ───>─────────────────────────────────────────────────────┐
    Source File ────┬─> AST1 ──> AST2 ─┬──┬─> AST3 ──> PCL ──> MCL ─┬─> SS ─┴─ ─> MCX ──> Run Native
    Include Files ──┘                  │  │                         └───────────────────> (ASM file)
    Strinclude ───────>────────────────┘  └─> PCL ──────────────────────────────────────> Run bytecode
````



