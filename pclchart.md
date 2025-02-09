

````

MM   ──┬──> IL ──┬────────────────────> IL Source (Input to PC)
BCC ───┤         ├────────────────────> IL Interpreter
PC ────┘         └─┬──> Win/x64 ──┬───> EXE/DLL
AA ───────>────────┘              ├───> OBJ (Input to external linker)
                                  ├───> ASM (Input to AA)
                                  ├───> NASM (Input to NASM)
                                  ├───> MX/ML (Input to RUNMX)
                                  └───> Run
````

