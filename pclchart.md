

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

