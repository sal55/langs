````
Front-ends             IR/IL             Back-ends             

M Compiler  ──>──┐                ┌────> PCL (Text Dump)
C Compiler  ──>──┼────> PCL ──>───┼────> PCI (Interpret PCL)
(PCL text) ───>──┤                ├────> Windows-x64 ────> EXE/DLL/OBJ/MX/ASM/NASM/RUN
(Other) ──────>──┘                ├────> (Linux-x64 ──────> NASM/MX/RUN)
                                  ├────> (Linux-ARM64 ────> AT&T-ASM)
                                  ├────> (Z80 8-bit ──────> ZASM)
                                  ├────> (32-bit targets)
                                  └────> (Linear C)
Key:
 EXE/DLL/OBJ  Windows' PE-format binaries
 MX           My private, portable (and simple) binary format. This
              requires a small program compiled locally to launch.
 ASM          x64 assembly in the syntax used by my AA assembler/linker
 NASM         x64 assembly in NASM syntax (NASM runs under Linux too)
 ZASM         My Z80 assembly syntax, mostly similar to Zilogs.
              This needs an emulator to run (a WIP)
 Linear C     Crude, unstructured C source code
````
