## 'M' Compilers Summary and Timeline

M was never a general-purpose, cross-platform language. It targetted one platform at a time, whatever was current in the world of personal and small business computers. So this progressed from 8-bit machines through generations of PCs, ending up with Windows running on 64-bit desktop machines.

It doesn't do other platforms, other than Linux which is only possible via a C backend.

The following is a rough summary of the various targets of my 'M' systems language compilers as they evolved over 40+ years. The `int` and `real` columns show the widths of those primitive types
:
Processor | Bits| OS | `int` | `real` | Pointer size | Year | Comments
--- | --- | --- | --- | --- | --- | --- | ---
Z80 | 8 | Bare metal | `i16` | `f24` | 16 bits | 1981 | Home-made machine
Z80 | 8 | CP/M Clone | `i16` | `f32` | 16 | Early 80s | Business machine
8086 | 16 | MSDOS | `i16` | `f32` | 32 (16+16)| Mid 80s | (IBM PC; segmented address)
80386 | 16 | MSDOS | `i16` | `f32` | 32 | Late 80s |
80486 | 16 | Windows | `i16` | `f32` | 32 | 1992 |
80486 | 32 | Windows | `i32` | `f32` | 32 | 2002 | (flat address)
x64   | 64 | Windows | `i64` | `f64` | 32 | 2012 | Experimental with 32-bit pointers
x64   | 64 | Windows | `i64` | `f64` | 64 | 2012? | Whole program compiler
x64   | 64 | Windows | `i64` | `f64` | 64 | Current | Latest with IL, 'optimiser' and new modules scheme

All these compilers ran on their respective targets. All have ended up self-hosted, bootstrapped from an earlier version as they evolved.

The first Z80 version was written in assembly, with an assembler I wrote in hex, with a hex editor itself written in actual binary, using switches to toggle bits on and off, on a home-made machine.

Assembly may have been used for rebooting the language between 8 and 16 bits; I can't remember the process. I was at the time involved with developing 8- and 16-bit hardware as well as devising compilers for it.

Most versions implemented the 'full stack', in going from source to executable using 100% my own code, but there were exceptions, for example, from late 90s to 2012 I generated NASM source and used a linker belonging to a C compiler. Currently it is again full stack and 100% self-hosted.

All compilers have been amateur efforts written by me for personal use, or as an in-house tool to help in my work. They were not professional products. Yet for quite a few years they were also used to write applications sold commercially.

Other than when using the NASM backend, and for a period when my own replacement for NASM generated OBJ files, my products never used a conventional linker. Pre-NASM, I would use my own OBJ format and had a program called a Loader to combine multiple object files into an executable.

Post-NASM and after the OBJ-generating phase, my own assembler generated EXE files directly. The back-end of the assembler was eventually incorporated into the compiler. ASM output was then used only during debugging and development.

No version used an optimisation stage, except for the most recent, but that made little difference to real applications (only to benchmarks!). It does make programs smaller though.

I considered it part of my job to ensure a quick edit-compile-run cycle, and did whatever was necessary to streamline the process. So full recompilation never took more than a few seconds, even using floppy disks, and usually that wasn't necessary. The latest compiler can do a full re-build of projects up to 40-50KLoc in around 1/10th of a second.

