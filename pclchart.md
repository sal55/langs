

````
MM ───────┬─/─> IL/API ──┬───────────────────────> IL Source (Input to PC)
PC ───────┘              ├───────────────────────> Run from source via interpreter
                         ├────/──────────────────> C Source (Input to C compiler)
                         └──┬─/──> Win/x64 ──┬───> EXE/DLL
AA ───────────>─────────────┘                ├───> OBJ (Input to external linker)
                                             ├───> ASM (Input to AA)
                                             ├───> NASM (Input to NASM)
                                             ├───> MX/ML (Input to RUNMX)
                                             └───> Run from source
````

This chart is for PCL7 (what I now call the IL used by MM7 compiler).

The WIP MM8 compiler for ARM64 uses PCL8, a 3AC-based IL rather than PCL7's stack-based.
