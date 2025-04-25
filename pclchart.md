

````
MM ───────┬─/─> IL/API ──┬───────────────────────> IL Source (Input to PC)
BCC ──────┤              ├───────────────────────> Run from source via interpreter
PC ───────┘              ├────/──────────────────> C Source (Input to C compiler)
                         └──┬─/──> Win/x64 ──┬───> EXE/DLL
AA ───────────>─────────────┘                ├───> OBJ (Input to external linker)
                                             ├───> ASM (Input to AA)
                                             ├───> NASM (Input to NASM)
                                             ├───> MX/ML (Input to RUNMX)
                                             └───> Run from source
````

