## B Compiler Structure

````
    Inputs              Intermediates                                                     Outputs
    Ext Libs (DLL) ───>─────────────────────────────────────────────────────┐
    Source File ────┬─> AST1 ──> AST2 ─┬──┬─> AST3 ──> PCL ──> MCL ─┬─> SS ─┴─ ─> MCX ──> Run Native
    Include Files ──┘                  │  │                         └───────────────────> (ASM file)
    Strinclude ───────>────────────────┘  └─> PCL ──────────────────────────────────────> Run bytecode
````


(Heavily out of date. Latest version goes from AST to M source code, and that is experimental.)
