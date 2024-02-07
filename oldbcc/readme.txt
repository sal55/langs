### 'MCC' C Compiler Project

#### 'Bare-bones' C Compiler.

The first version of the C compiler has been withdrawn.

It has been replaced with an updated version (version 8.10 onwards), in mcc64.c.

**Not much is different (it still generates asm output and chunks of the language and headers are still missing) but it has:**

* Revised code generator
* Code generator is much less buggy and runs applications better. But it is also much slower, and generates a larger volume of ASM (putting more pressure on the already Nasm). (This code generator not intended for production use.)
* A new library file mcclib.asm must be downloaded and assembled (if setjmp/longjmp is used)
* Passing and returning structs has been added

**Mcc now compiles and runs (using cursory testing) the following significant programs:**

* Tiny C compiler
* Lua interpreter
* Tex program (up to main prompt anyway)
* Itself (compiles generated C code as original source is not C)
* One other compiler and interpreter (compiles generated C code)

**Running arbitrary C programs can run into these problems:**

* Standard headers provided are not complete
* They may need MS headers such as windows.h, of which only a minimal version is provided (added to as needed program by program)
* Programs may assume standard compilers such as gcc or MSVC
* Programs run on Linux
* Programs expect to be built using standard tools and makefiles for which Mcc is a poor fit
* Some C features used are missing
* There may be bugs in the implementation
* Even if it works, it may not be fast enough as I don't have a production quality code generator

However, there should be fewer problems when used for new programs written within the limitations.

**Not really into the C compiler at the minute (I want to get on with my own stuff), but future plan plans for MCC include:**

* The next significant upgrade will tighten up the output code and make it smaller and faster (the code generator needs to be used in my own language compiler too)
* Hopefully complete the headers enough to allow Seed7 to be compiled and to hopefully run (but that has some quite tough test suites I understand)
* Eliminate the external Nasm and linker dependencies (by directly generating .exe for example, or some simpler format of my own). This is big stuff but is also needed for my own new compiler so Mcc could benefit. (However, that language is designed for whole-project compilation, while C is the opposite.)
