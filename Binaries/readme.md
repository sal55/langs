## M Compiler Backups

These are intended as a backup for the binaries of my Windows M compiler 'MM'. Since my language is unique, and compilers for it exist on no one else's machine, losing it would be catastrophic.

No other binaries are needed, since all projects are written in M and can be recovered from their source code (a separate backup folder), only the M compiler is esssential.

However, Windows is likely to flag my EXE files (ie. any EXEs produced by the M compiler) as having a virus, stopping it from downloading. So a textual Hex representation is used: each binary byte of the EXE is stored as two hex digits, so byte 0x37 for example is "37" in the file.

A trivial program in any language can be used to reconstruct the original EXEs.

* There are two versions stored here: v7 as mm.exe, and v8 as bb.exe
* Files are UPX-compressed before being converted to hex

Other possibilities considered were:

* MX private binary format (more immune to AV s/w) which needs runmx.c/runmx.exe to launch
* A transpiled one-file C version
* An ASM version in NASM or AT&T format
* A text version where each byte is written as one decimal byte per line
* Also having the actual binaries and hoping I got lucky with downloads

I decided to settle on one solid, fairly compact representation with minimal dependencies.

 
