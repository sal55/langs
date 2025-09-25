## 'BCC' C-subset Compiler

I've lost track of the different versions of my 2017 C compiler. Mostly they all use the same buggy, non-conforming front-end.

That also lacks a lot of features now popular, lile VLAs, compound literals, designated initialisers, proper bitfields. But this would have been the same in the 2023 'cc64' version.

cc64 I considered to have poor, buggy code-generation. I was hoping to improve that, but I don't know if it's any better (maybe just a different set of bugs).

The last working version (setting aside current late-2025 experiments in new backends) has these specs:

* Uses a stack-based IL/intermediate language called 'PCL'. This is shared with a my M-language compiler
* PCL is normally converted to x64 code using the Win64 ABI
* x64 code is normally written out as EXE or DLL files
* It can also be written to OBJ files (but may be buggy if multiple OBJ files are linked, if working in high-memory)
* Programs can also be run directly in memory (making the compiler work like a scripting language in running programs from source)
* PCL can also be interpreted directly (but programs will be slow; this was more for fault-finding). This also allows it to work on Linux
* x64 code is normally designed to work in low-memory below 2GB, but can also generate code for high-memory, which happens for DLL/OBJ outputs
* x64 code can also be dumped to ASM format (mine), or NASM
* There is a mild optimiser which keeps some local variables in registers. This can make some programs both faster and smaller

**Note** Most of these facilities are provided by the PCL backend which is designed for whole-program compilers like mine.

 C however normally requires independent compilation. So all the above only applies for single-file C applications.

 Multi-module C programs must use the ASM output, and some linking process. (I use a special assembler AA6 for ASM files in my format, or an external linker like gcc/ld for NASM format.)

 The current C compiler is a single self-contained executable of 330KB, including standard headers. (windows.h is no longer included.) All the above features are included, except that only one ASM output is configured (a build will generate either ASM or NASM source files)

 A version that *only* generates ASM, of either syntax, can be much more compact.
 

 
