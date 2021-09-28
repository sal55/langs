## 'PCL' Intermediate Language

This is my attempt at creating a standalone, intermediate language for a compiler backend.

It's a sort of tiny version of LLVM. There are a few other similar products around, but for various reasons they are not practical for me. (Also, I like to create 100% of my tools and in my own language.)

The quality is not good enough for others to use (and the support needed is not practical). So it's just being presented here as another example of what such a product can look like. People are welcome to take away anything that sounds useful.

### Components

The programs I've been working on are:

**pc.exe** A program that can take PCL source code and turn it into ASM/EXE/DLL (or even C). No separate assemblers or linkers are needed for EXE or DLL targets.

**pc.dll** A library version which is what would most likely be used from other languages. While this can also read PCL source, probably the API will be used to generate in-memory PCL more efficiently. (Not ready.)

I wanted this project to be a small, single, self-contained executable. And so far it is (when using pc.exe or pc.dll). The current size is something over 0.2MB, which can generate all the files in the examples, and might end up as 0.25MB. (Doing the C output requires a 30KB addition, but a C backend is unlikely to co-exist in the same program.)

While PC.EXE can process PCL source into EXE at some 1.2M lines/second and 4MB of code per second even on my slow PC, generating PCL source is likely tp be a bottleneck. The recommended way is to use the API. However generating a source file is the simplest way to use the produce (see cdemo.c).

Associated programs are:

**mm.exe** The compiler for my systems language that has been adapted to generate PCL. However the necessary parts of PC have been compiled into the executable (DLL interface not ready) and it uses the API. Possible outputs are ASM/EXE/DLL/PCL Source.

**aa.exe** My assembler/linker, necessary to turn ASM source into EXE/DLL/OBJ.

I will use 'PC' to refer to the software (be it in pc.exe, pc.dll or incorporated) to process PCL code)

### Characteristics

* Stack-based VM
* Byte-code-like instruction set, but with types
* Supports lower-level languages perhaps up to a couple of steps beyond C (anything else must be implemented on top)
* Datatypes supported are u8 u16 u32 u64 u128, i8 i16 i32 i64 i128, f32 f64 and Block.
* 128-bit support is sparse
* Block types implement fixed size arrays and structs
* Designed for whole program compilers, so the entire program is generated as PCL then converted
* Multiple PCL inputs (needed for languages such as C with independent compilation) is a possibility, but all files would need submitting at once
* Designed for 64-bit targets
* Rich set of instructions targeted at my own system language

### Targets

I only support x64 native code running on Windows. There is an experimental backend that can generate attrocious C code. That one could be useful to create programs for Linux and/or ARM processors, but it needs a lot more work. Obviously, there would be a C compiler dependency.

No other targets are in the pipeline (possibilities would have been direct x64 on Linux, and maybe ARM64 at some future point).

### Inputs

* PCL code can be generated as textual source code, then submitted to PC
* The API can be used generate code programmatically.
* Part of the original spec was to have a binary version of PCL code (called PCB files) which can be generated and processed more quickly, but that is unlikely to be done. The recommended way to generate code is via the API.

The API would be used by external languages via DLL when that is ready. In my language I just compile in the various components of PC using import statements. (I could use DLL too but I like my products to be self-contained)

### Outputs

Here it is easy to get carried away, but the outputs that either work, or have been tested experimentally, are:

* PCL source output. Intended for when code is generated via the API, to examine the output. (Also possible to have PCL source input and PCL source output, taking care to use different names, to tidy up source code)
* ASM source output. This is for x64 targets on Windows. It's in my own syntax, requiring my AA assembler to turn into EXE/DLL/OBJ, but it can just be used to examine the quality of the code (not high)
* EXE runnable binary, again for x64 on Windows
* DLL binary, another variation
* C source code. This is highly experimental, and has been tried on smallish programs just to show that it can work. (Also, to give a different perspective to help refine the PCL design.)

Outputs which were briefly considered:

* Binary PCL files
* PCL interpreter (every such project seems to have one of these). I like to do these properly and with performance in mind, but it would take too much effort. However, if this was implemented in a language like C, it could mean being able to run PCL programs on virtually any (64-bit) machine.
* Run in-memory: fixing up native code to run immediately instead of creating an EXE file. However, writing an EXE then invoking the executable is two lines of code, so of little benefit.
* OBJ format, sometimes of use to combine my code with other languages'. This can actually be added, but it would need my AA assembler. 

### PCL Syntax

This was based on the bytecode I've long used for my dynamic intepreters. It is very easy generate, but not so easy to write in.

It resembles also ASM code, but for a far more capable and orthoginal processor.

Effectively this is a new, low-level language, and PC can be used as a compiler or assembler for it.

For examples, see test.pcl and fib.pcl. For a bigger example, see pc.pcl (50Kloc), and mm.pcl (115Kloc), my new compiler that incorporates the PCL backend. These are the MM and PC products in action on themselves:

````
C:\mxp>mm -pcl pc                       # Generate PCL file for PC project
Compiling pc.m---------- to pc.pcl

C:\mxp>pc pc -out:pc2.exe               # Invoke PC backend on PC.PCL to get 2nd generation PC2.EXE
Processing pc.pcl to pc2.exe

C:\mxp>mm pc                            # In practice, it's done in one go using in-memory PCL code,
Compiling pc.m---------- to pc.exe      # no discrete PCL file
````

### Demo Program

See pcdemo.m. I had hoped to write this in C, but that part (needing DLL and headers) is not ready. So it's in my language. It writes a basic Hello program (using C's printf to minimise runtime support and keep the code size to 20 lines instead of 2000).

The program generates the instructions in memory, then various optional lines can generate:

* PCL source; see test.pcl
* ASM source; see test.asm
* C source; see test.c (needs pclhdr.h to build)
* EXE binary, which can be run from the demo

There *is* a simple C demo; see cdemo.c. This just writes the PCL code as text. The output of this program is the rather flat:
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

### Used as C Compiler Backend

I started modifying my C compiler to generate PCL, however I stopped because at the moment it's not worthwhile. (It needs a new front-end first!)

It would probably work (the current backend generates terrible code), but PCL would need some tweaks:

* C mostly still uses a 32-bit int, so most ops are 32-bits. Currently most PCL ops (add, etc) are 64 bits, so special support is needed to provide 32 bit ones.
* Some support for doing setjmp/longjmp is needed in PCL
* I thought I'd need special handling for Switch, as that feature is so chaotic in C, but it might be OK
* C compiles a module at a time, so extra support is needed for multiple modules, which have to be submitted all at the same time. (Probably, there are devious ways to get around this, by pretending each module is a whole program with imports and exports, generating multiple ASM files from multiple PCL, and using my AA assembler linker. But clearly that's a lot of work)

### Improvement in my M Systems Compiler

A version of PCL was already part of my recent compilers, but tightly integrated. Working on this separate version showed how much of a mess it was, and it is now considerably tidied up.

The discipline of deciding what belongs this side of PCL or the other side was useful too.

However, because I really need the PCL components to be part of the compiler for efficiency, it has put considerable pressure on M's module system. So my next project is to overhaul that.

### The Runtime Library

For M, this is split into functions called explicitly, and those support functions called implicitly. But where do they belong? How do they fit in with PCL?

Most such functions are compiled as normal, with the rest of an application, into PCL. Some functionality however, was moved to PCL, examples:

* 128-bit arithmetic
* Integer ** (power) operators
* Function reflection

These become the headache of PCL (they are variously implemented as special RTS functions, or as x64 code; depends on target).

### Purity of PCL

PCL is intended to be oblivious to both source language (eg. M or C), and target. But that is not quite the case.

So a few opcodes remain used as hints to help out. In particular, SETARGS is used at the start of call-sequence, due to the special needs of the Win64 ABI.

### Types and Opcodes in Detail

See the file pc_tables. There are no separate docs at present.

### The API

See the file api.m. This is extracted from an exports file used for DLL; these are functions in the PC project marked as exported for use via a DLL.

The pcdemo.m gives an idea of how a PCL program is synthesised. As an example actual PCL-generating code from my MM compiler, this function deals with most binary operators:
````
proc do_bin(unit p, a, b) =
    evalunit(a)
    evalunit(b)

    pcl_gen(p.pclop)
    setmode_u(p)
end
````
(Code to deal with messy pointer arithmetic elided.) 'p' is the AST mode for the operator; 'a' and 'b' are left and right operands. p.pclop contains the PCL code (eg. ADD, or kadd in the sources) put there during parsing. setmode_u() takes the result type (here assumed to be identical to the that of a and b) and converts it to a basic PCL type.

### What's Missing

Absolutely loads. Enough of the various combinations of opcodes and types are populated to enable me to use it for all my main language projects. However it can't take over from my current compiler until:

* I transfer over the optimiser, such as it is, for the x64 code (that won't take long)
* I figure out how to deal with the inline ASM of my M language.
* There are various tweaks to with block-handling. (PCL nominally deals with block by value, but the ABI specifies they are handled by reference.)

### Some Special Features of PCL

At least I assume they are; I don't know enough about other products (LLVM's 2000MB installation must do something good.)

Special support exists for:

* Multiple function return values
* 128-bit integers (not extensive, but the basics should work)
* Bit-indexing and Bitfield slicing of integers
* Label addresses
* Switch
* Conditionals like A in B..C and A in \[B, C, D\]
* My case-when statements
* N-times-loops and iterative for-loops
* Slices
* Clear (set to zero) memory blocks
* By-value manipulation of structs and arrays (via blocks)
* Swap()
* Min/max operators, also min:=/max:=
* Common math(s) operators which are built-in, also sqr (square) and power
* Setting up and allowing access to a program's function tables

### The Diagram

Here's the obligatory diagram which I've seen also on other products:

````
                            PC
                       _______________
                      |               |
                      |               |
API -------------->---|     PCL       |
                      |               |
PCL source file -->---|   in-memory   |-->--- PCL source file
                      |               |
                      |    format     |
                      |               |
                      |---------------|
                      |   C backend   |-->--- C source file (-> Linux)
                      |---------------|
                      |          | SS |-->--- EXE binary (all Win64)
                      |     MCL  |    |
                      |          |    |-->--- DLL binary
                      |          |____|
                      |               |
                      |    backend    |-->--- ASM source -> EXE/DLL/OBJ
                      |_______________|
                      |               |
                      | [Interpreter] |
                      |_______________|
````

MCL = in-memory representation of x64 native code.

SS = tables of data, machine code and symbols suitable
for conversion to PE EXE, DLL and OBJ formats, although OBJ
generation is not working inside PC.

All file inputs and outputs are single files.

The interpreter part is just a proposal.
