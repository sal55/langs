## 'PCL' Intermediate Language

This is my attempt at creating a standalone, intermediate language for a compiler backend. It's a sort of tiny version of LLVM. There are a few other similar products around, but for various reasons they are not practical for me. (Also, I like to create 100% of my tools.)

### Characteristics

* Stack-based VM
* Byte-code-like instruction set, but with types
* Supports lower-level languages perhaps up to a couple of steps beyond C
* Datatypes supported are u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64 and Block.
* Block types implement fixed size arrays and structs
* 128-bit support is sparse
* Designed for whole program compilers, so the entire program is generated as PCL then converted
* Multiple PCL inputs (needed for languages such as C with independent compilation) is a possibility, but all files would need submitted at once
* Rich set of instructions targeted at my own system language

### Inputs

* PCL code can be generated as textual source code, then submitted to the standalone PC program
* There is also an API that can be used (via a DLL and some C headers) to generate code programmatically. However that is not ready
* Mostly I use it vias the API with elements of PC compiled as part of my main compiler. (Via a DLL is possible here too, but that is not ready either.)
* Part of the original spec was to have a binary version of PCL code (called PCB files) which can be processed more quickly, but that is unlikely to be done. The recommended way to generate code is via the API

### Outputs

Here it is easy to get carried away, but the outputs that either work, or have been tested experimentally, are:

* PCL source output. Intended for when code is generated via the API, to examine the output. (Also possible to have PCL source input and PCL source output, taking care to use different names, to tidy up source code)
* ASM source output. This is for x64 targets on Windows. It's in my own syntax, requiring my AA assembler to turn into EXE/DLL/OBJ, but it can just be used to examine the quality of the code (not high)
* EXE runnable binary, again for x64 on Windows
* DLL binary, another variation
* C source code. This is highly experimental, and has been tried on smallish programs just to show that it can work. (Also, to give extra input to the PCL design which has been tweaked)

No other targets are in the pipeline (next could be x64 on Linux, and possibly ARM64 at some future point). Outputs which were briefly considered:

* Binary PCL files
* PCL interpreter (every such project seems to have one of these). I look to do these properly and with performance in mind, but it would take too much effort. Maybe, if was done in-hand with a debugger.
* Run in-memory. Fixing up native code to run immediately instead of creating an EXE file. However, writing an EXE then invoking the executable is two lines of code, so little benefit.
* OBJ format, sometimes of use to combine my code with other languages'. This can actually be added, but it would need my AA assembler. 

### Demo Program

See pcdemo.m. I had hoped to write this in C, but that part is not ready. So it's in my language. It writes a basic Hello program (using C's printf to minimise runtime support).

The program generates the instructions in memory, then various optional lines can generate:

* PCL source; see test.pcl.
* ASM source; see test.asm
* C source; see test.c (needs pclhdr.h to build)
* EXE binary, which can be run from the demo

There *is* a C demo; see cdemo.c. This just writes the PCL code as text. The output of this program is the rather flat:


