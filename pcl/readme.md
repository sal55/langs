## 'PCL' Intermediate Language

This is my attempt at creating a standalone, intermediate language for a compiler backend. It's a sort of tiny version of LLVM. There are a few other similar products around, but for various reasons they are not practical for me. (Also, I like to create 100% of my tools.)

The quality is not good enough for others to use (and the support is not practical). So it's just being presented here as another example of what such a product can look like.

### Characteristics

* Stack-based VM
* Byte-code-like instruction set, but with types
* Supports lower-level languages perhaps up to a couple of steps beyond C
* Datatypes supported are u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64 and Block.
* Block types implement fixed size arrays and structs
* 128-bit support is sparse
* Designed for whole program compilers, so the entire program is generated as PCL then converted
* Multiple PCL inputs (needed for languages such as C with independent compilation) is a possibility, but all files would need submitted at once
* Designed for 64-bit targets
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

### Size of the PC.EXE

I wanted this project to be a small, single, self-contained executable. And so far it is. The current size is about 216KB, which can generate all the files in the examples, and might end up as 250KB. (Doing the C output requires a 30KB addition, but a C backend is unlikely to exist in the same program.)

Not separate assemblers or linkers are needed; PC.EXE does it all. (OBJ output will need my AA assembler, a 150KB program.)

The processing speed of PCL source code was about 1.2 million lines per second, for PCL -> EXE, but bear in mind the line count might be twice as high as the original source code. Generating PCL text is also time-consuming. Using a binary PCL file format is more efficient, but the intention is that programs mainly use the API to generate in-memory PCL, then straight to binary.

### PCL Syntax

This was based on the bytecode I've long used for my dynamic intepreters. It is very easy generate, but not so easy to write in.

Perhaps it looks also like ASM code, but for a far more capable and orthoginal processor.

For examples, see test.pcl and fib.pcl. For a bigger example, see mm.pcl, which is my new compiler that incorporates the PCL backend, compiling itself to PCL (140K lines, 4MB). This is that compiler in action:
````
C:\mxp>mm -pcl mm                         # create separate PCL file
Compiling mm.m---------- to mm.pcl

C:\mxp>pc -exe mm                         # Use PC to turn that into an executable
Processing mm.pcl to mm.exe

C:\mxp>mm hello                           # Use that new compiler to turn hello.m into hello.exe,                                          
Compiling hello.m------- to hello.exe     # via in-memory PCL this time, no discrete PCL file

C:\mxp>hello                              # Test that it works
Hello, World! 21:59:54
````

### Demo Program

See pcdemo.m. I had hoped to write this in C, but that part is not ready. So it's in my language. It writes a basic Hello program (using C's printf to minimise runtime support and keep the code size to 20 lines instead of 2000).

The program generates the instructions in memory, then various optional lines can generate:

* PCL source; see test.pcl
* ASM source; see test.asm
* C source; see test.c (needs pclhdr.h to build)
* EXE binary, which can be run from the demo

There *is* a C demo; see cdemo.c. This just writes the PCL code as text. The output of this program is the rather flat:
````
extproc printf i32
extparam u64
extvariadics
endext
proc test.start::
procentry
setargs 1 0
push "Hello World! [C]\n" u64
callproc &printf
push 0
stop
end
endprogram
````
By invoking PC on this input and telling it to write a new PCL file, it will look more like test.pcl above.

However, the original contains everything needed to generate ASM or EXE. The ASM produced is again similar to test.asm above.

