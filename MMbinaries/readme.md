## M Compiler Backups

This is intended as a backup for the binary of my Windows M compiler 'MM'. Since my language is unique, and compilers for it exist on no one else's machine, losing it would be catastrophic.

The main binary needed is **mm7.exe**, since all projects are written in M and can be recovered from their source code (a separate backup folder).

However, Windows may flag my EXE file (ie. any EXEs produced by the M compiler) as having a virus, stopping it from downloading. So a few other representations are provided just in case:

* **mm.c** This is a transpilation to very low level, linear C, derived from MM's PCL7 IL. It needs gcc to compile (tcc won't do as `fstrict-no-aliasing` is needed)
* **mm.mx** A binary in my own executable format, which seems to be immune to AV probing. But it needs a small loader program "runmx.exe". This one (compiled via Tiny C) seems to clear AV, but the C source, ported from M, also provided in "runmx.c". (gcc can't be used as MX files run in low memory, but gcc-compiled binaries run in high memory, and allocations are high too)
* **mm.asm** Compiled to ASM in NASM format. This must be assembled with `nasm -fwin64`, then linked (eg. using gcc or ld)
* **mm.txt** This is mm7.exe converted to text: one byte per line (but after processing by UPX to reduce the size). A simple script in any language will read each number as a byte then write all the bytes to an EXE file.
 
