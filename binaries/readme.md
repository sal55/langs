## Binary Backups

This is intended as backups for my Windows binaries. Since my language is unique, and compiler exists on no one else's machine (unlike compilers for C for example), losing it would be catastrophic.

The main binary needed is mm.exe, or officially mm7.exe, so all others are written in M and can be recovered from their source code (a separate backup folder).

The problem however is that Windows will flag my EXE file as having a virus. So there are a few other options explored here. The possibilities are:

* Keep mm.exe here, but as I said, it can give trouble with AV.
* Restore from the source archive mm.ma/mmp.ma, but that needs an M compiler - a binary
* Transpile sources to C (a very crude C created from the compilers IL). This requires gcc to compile to EXE
* Generate assembly source in NASM syntax. This reqires NASM plus a linker (eg. gcc or gcc's ld) to compile to EXE
* Generate an MX file which is my own executable. This needs a small program RUNMX to run, but it has its own problems, see below. MX files apparently are not seen as a threat
* I might try and simply convert the bytes of MM.EXE into text via a simple script. Then turning those bytes into MM.EXE can be done with virtually any language. (To keep the size down, I might run UPX on the binary first.)

**Using RUNMX**

If try and provide RUNMX.EXE from my compiler, there are still AV problems. However, RUNMX also exists as a C version, which can be compiled with Tiny C. It's RUNMX.EXE is not seen as a threat! So that is provided, as well as sources for RUNMX.

But, gcc can't be used to compile runmx.c: MX was a format designed to run in low memory (below 2GB). So the executable memory that needs to be allocated must be below 2GB. But programs compiled with gcc tend to run in high memory, and allocation routines also return high memory addresses too.

