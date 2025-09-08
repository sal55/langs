

````
MM ───────┬─/─> IL/API ──┬───────────────────────> IL Source (Input to PC*)
MCC───────┤              ├───────────────────────> Run from source via interpreter
PC ───────┘              ├────/──────────────────> C Source (Input to C compiler)
                         └──┬─/──> Win/x64 ──┬───> EXE/DLL
AA ───────────>─────────────┘                ├───> OBJ (Input to external linker)
                                             ├───> ASM (Input to AA)
                                             ├───> NASM (Input to NASM)
                                             ├───> MX/ML (Input to RUNMX)
                                             └───> Run from source
````

\*This chart is for PCL IL, the stack-based VM. I'm now migrating to TCL, a 3AC-based one.

There, there is no 'PC' front-end since the textual IL is not a viable language by itself.
