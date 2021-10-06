## 'PCL' Intermediate Language

This is my attempt at creating a standalone, intermediate language for a compiler backend.

It's a sort of tiny version of LLVM. There are a few other similar products around, but for various reasons they are not practical for me. (Also, I like to create 100% of my tools and in my own language.)

The quality is not good enough for others to use (and the support needed is not practical). So it's just being presented here as another example of what such a product can look like. People are welcome to take away anything that sounds useful.

This one is primarily aimed at x64 machines running Windows.

### Components

The programs I've been working on are:

* **pc.exe** A program that can take PCL source code and turn it into ASM/EXE/DLL (or even C). No separate assemblers or linkers are needed for EXE or DLL targets.

* **pc.dll** A library version which is what would most likely be used from other languages. This can process PCL source but it also has an API to build PCL code via function calls.

I wanted this project to be a small, single, self-contained executable. The current size of either pc.exe or pc.dll is something under 0.25MB, which can generate all the files in the examples. (Except test.c which used a different backend component.)

While PC.EXE can process PCL source into EXE at several MB of binary code per second even on my slow PC, the recommended way is to use the API to generate in-memory PCL. However generating a source file is the simplest way to use the product (see cdemo.c).

Associated programs are: **mm.exe**, the compiler for my systems language that has been adapted to generate PCL. This incorporates the necessary parts of PC (I like it to be self-contained) and it uses the API. **aa.exe** yy assembler/linker, necessary to turn ASM source into EXE/DLL/OBJ.

I will use 'PC' to refer to the software (be it in pc.exe, pc.dll or incorporated) to process PCL code.

### Characteristics

* Stack-based VM
* Byte-code-like instruction set, but with types
* Supports lower-level languages perhaps up to a couple of steps beyond C (anything else must be implemented on top)
* Datatypes supported are u8 u16 u32 u64 u128, i8 i16 i32 i64 i128, f32 f64 and Block.
* Designed for whole program compilers, so the entire program is generated as PCL then converted.
* Designed for 64-bit targets
* Rich set of instructions targeted at my own system language

### Comparisons With Other Products

In the case of MIR, a direct comparison is possible with the Sieve example here: https://github.com/vnmakarov/mir/tree/v_0_1.

sieve.m shows the version of that C program in my language, and sieve.pcl is the generated PCL code.

PCL doesn't have so many things happening on one line, so has a higher line count (100 vs 38). (This makes it easy to generate programmatically as the syntax is less fussy.)

However, PCL source is also bigger overall. Partly to do with the output being tabulated to line things up, but also because every name has to be fully qualified, eg: sievemir.sieve.n instead of just 'n'.

PCL does compare better with the LLVM code produced via Clang: sieve.ll. This is 360 lines of which which just sieve() is over 100 lines.

Generally:

* PCL code is linear, has simpler instructions with fewer operands (0 or 1) and is written more vertically than horizontally
* Is stack-based rather than use registers or temporaries
* PCL has no local scopes so each name must be fully qualified
* A PCL file is a complete, 100% representation of an entire program
* PCL is more limited in scope with only one real target (Win64)
* PCL has no real optimiser
* PCL has no interest in JIT

On the latter point, it's designed to do ahead-of-time compilation only, and to do it quickly.

(On my machine, pc.exe \[for fixed Win64 asm/exe/dll target\] builds from source in around 0.1 seconds, for some 18KLoC. I think I once estimated that building LLVM from source would take 6-12 hours on the same machine.)

### Targets

I only support x64 native code running on Windows. There is an experimental backend that can generate attrocious C code. That one could be useful to create programs for Linux and/or ARM processors, but it needs a lot more work. Obviously, there would be a C compiler dependency.

No other targets are in the pipeline (possibilities would have been direct x64 on Linux, and maybe ARM64 at some future point).

### Inputs

* PCL code can be generated as textual source code, then submitted to PC.
* The API can be used generate code programmatically.

There is no binary PCL file format. If PCL source is too slow, the API is recommended.

The API would be used by external languages via DLL when that is ready. In my language I just compile in the various components of PC using import statements.

### Outputs

The outputs that either work, or have been tested experimentally, are:

* PCL source output. Intended for when code is generated via the API, to examine the output. (Can also be used to tidy up or normalise PCL source input.)
* ASM source output. This is for x64 targets on Windows. It's in my own syntax.
* EXE runnable binary, again for x64 on Windows
* DLL binary, another variation
* C source code. This is highly experimental, and has been tried on smallish programs just to show that it can work. (Also, to give a different perspective to help refine the PCL design.)

OBJ binary, which can be useful for interacting with other languages, is possible via ASM. It needs aa.exe, and an external linker.

Outputs which were briefly considered:

* Binary PCL files
* PCL interpreter
* Run in-memory (it was easier to generate EXE than just run that)

### PCL Syntax

This was based on the bytecode I've long used for my dynamic intepreters. It is very easy generate, but not so easy to write in.

It resembles also ASM code, but for a far more capable and orthoginal processor.

PCL is a complete, low-level, portable language.

For examples, see test.pcl and fib.pcl. For a bigger example, see pc.pcl (50Kloc), and mm.pcl (115Kloc), my new compiler that incorporates the PCL backend.
### Files

**test.pcl** As generated by pcdemo.m

**test.asm** Generated from test.pcl

**test.c** Generated from test.pcl

**fib.pcl** A slightly more elaborate program

**pc.pcl, mm.pcl** Much bigger example programs, representing PC.EXE and MM.EXE

**pcdemo.m** Demo which uses the PCL API, using compiled-in components. This generates test.pcl, and then can choose between ASM/EXE outputs

**pcdemoc.c** C version, with mocked-up declarations as thart part is not ready. This uses PC.DLL.

**cdemo.c** C program that generates a version of test.pcl directly as text, then invokes PC.EXE via system()

**sieve.m** My M version of the MIR example at the link

**sieve.pcl** The PCL from sieve.m

**sieve.ll** LLVM intermediate from generated by clang, from the sieve C example at the MIR link

**pc_tables.m** Some source code from PC listing types and PCL opcodes

**api.m** List of API functions exported from PC

**pc.exe** Working binary for anyone to try (runs on Win64 OS or, possibly, 'Wine'). Error messages need some work...

**pclhdr.h** Header needed if anyone wants to build test.c

### Demo Program (M)

See pcdemo.m (a C version is below), which is my language. It writes a basic Hello program (using C's printf to minimise runtime support and keep the code size to 20 lines instead of 2000).

The program generates the instructions in memory, then various optional lines can generate:

* PCL source; see test.pcl
* ASM source; see test.asm
* C source; see test.c (needs pclhdr.h to build)
* EXE binary, which can be run from the demo

These are the MM and PC products in action on some real programs, themselves:
````
C:\mxp>mm -pcl pc                       # Generate PCL file for PC project
Compiling pc.m---------- to pc.pcl

C:\mxp>pc pc -out:pc2.exe               # Invoke PC backend on PC.PCL to get 2nd generation PC2.EXE
Processing pc.pcl to pc2.exe

C:\mxp>mm pc                            # In practice, it's done in one go using in-memory PCL code,
Compiling pc.m---------- to pc.exe      # no discrete PCL file
````
If you can run the pc.exe program here, try it on itself:
````
pc -asm pc                              # produces pc.asm from pc.pcl
````
### Demo Program (C)

I mocked up the necessary declarations to make this work (as I don't have an automatic way yet to generate the 100s of necessary declarations).

See pcdemoc.c. This is like pcdemo.m, but not quite as sweet as C lacks default arguments. It does the same thing, but uses pc.dll. It's built and run like this:
````
C:\c>bcc pcdemoc pc.dll
Compiling pcdemoc.c to pcdemoc.exe

C:\c>pcdemoc                       # Create PCL file, use PC API to create test.exe, and run it
Hello, World! [C/demo]
````
(bcc is my C compiler I mention elsewhere.) pcdemoc.exe is about 3.5KB, pc.dll is 230KB. 

A simpler demo that generates only text is in cdemo.c. This just writes the PCL code as text, and then invokes pc.exe via system().
The output of this program is the rather flat:
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

I started modifying my C compiler to generate PCL, however I stopped because at the moment it's not so useful and needs other work anyway.

But PCL would need some tweaks:

* C mostly still uses a 32-bit int, so most ops are 32-bits. Currently most PCL ops (add, etc) are 64 bits, so more 32-bit ops must be supported
* * Some support for doing setjmp/longjmp is needed in PCL
* I thought I'd need special handling for Switch, as that feature is so chaotic in C, but it might be OK
* C compiles a module at a time, so extra support is needed for multiple modules, which also have to be submitted all at the same time. (Probably, there are devious ways to get around this, but it's something to get back to.)

### Improvement in my M Systems Compiler

A version of PCL was already part of my recent compilers, but tightly integrated. Working on this separate version showed how much of a mess it was, and it is now considerably tidied up. The discipline of deciding what belongs this side of PCL or the other side was useful too.

However, because I really need the PCL components to be part of the compiler for efficiency, it has put considerable pressure on M's module system. So my next project is to overhaul that.

### The Runtime Library

This is M's runtime which is a few thousand lines, with many functions providing language support and called behind the scenes. The question with some of those was, do they stay this side of PCL, or move to the other side? That is, get implemented as special PCL instructions.

Some were moved to PCL, for example:

* 128-bit arithmetic
* Integer ** (power) operators
* Function reflection

These then become the headache of PCL. (Where they are variously implemented as inline x64 code, or generated x64 functions, or special RTS functions which are added to the main user program by PC. But it depends on target too.)

### Purity of PCL

PCL is intended to be oblivious to both source language (eg. M or C), and target. But that is not quite the case.

So a few opcodes remain used as hints to help out. In particular, SETARGS is used at the start of call-sequence, due to the special needs of the Win64 ABI.

### Types and Opcodes in Detail

See the file pc_tables. There are no separate docs at present.

### The API

See the file api.m. This is extracted from an exports file written when PC.DLL is created.

The pcdemo.m gives an idea of how a PCL program is synthesised. As an example of actual PCL-generating code from my MM compiler, this function deals with most binary operators:
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

Absolutely loads. Enough of the various combinations of opcodes and types are populated to enable me to use it for all my main language projects.

At the current time, it can do everything my current compiler can do.


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
                      |          |    |-->--- EXE binary (all Win64)
                      |     MCL  | SS |
                      |          |    |-->--- DLL binary
                      |          |____|
                      |               |
                      |    backend    |-->--- ASM source -> EXE/DLL/OBJ
                      |---------------|
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

### How PCL is Used in my Compilers

That diagram was fun so here are similar ones, showing how PCL is used or could be used with my language projects:
````
(Finished)             MM.EXE (Compile M apps to Win64 binaries)
                       _______________
                      |     CLI       |
                      |---------------|
M source code:  -->---| M Compiler(m) |
                      |---------------|
                      |     PCL(p)    |-->--- PCL source file
                      |---------------|
                      |               |-->--- ASM source file
                      |     MCL(p)    |-->--- EXE/DLL binary
                      |_______________|


(Proposed)             MC.EXE (Compiler M apps to C64 sources)
                       _______________
                      |     CLI       |
                      |---------------|
M source code:  -->---| M Compiler(m) |
                      |---------------|
                      |     PCL(p)    |-->--- PCL source file
                      |---------------|
                      |    Clang(p)   |-->--- C source file
                      |_______________|


(Finished)             PC.EXE (Compile PCL source to Win64 binaries)
                       _______________
                      |     CLI       |
                      |---------------|
PCL source file -->---|     PCL(p)    |-->--- PCL source file
                      |---------------|
                      |               |-->--- ASM source file
                      |     MCL(p)    |-->--- EXE/DLL binary
                      |_______________|


(Existing)             AA.EXE (Assemble ASM source to Win64 binaries)
                       _______________
                      |     CLI       |
                      |---------------|
ASM source file -->---| Assembler(a)  |
                      |---------------|
                      |     MCL(a)    |-->--- EXE/DLL/OBJ binary
                      |_______________|


(Finished)             PC.DLL (PCL/API Library)
                       _______________
API -----------<-->---|     PCL(p)    |-->--- PCL source file or string
                      |---------------|
                      |               |-->--- ASM source file or string
                      |     MCL(p)    |-->--- EXE/DLL binary
                      |_______________|


(Existing)             BCC.EXE (C subset compiler)
                       _______________
                      |     CLI       |
                      |---------------|
C source code:  -->---|  C Compiler   |
                      |---------------|
                      | Assembler(a)  |
                      |---------------|
                      |               |-->--- ASM source file
                      |     MCL(a)    |-->--- EXE/DLL binary
                      |_______________|


(Proposed)             BCC.EXE (C subset compiler)
                       _______________
                      |     CLI       |
                      |---------------|
C source code:  -->---|  C Compiler   |
                      |---------------|
                      |     PCL(p)    |-->--- PCL source file
                      |---------------|
                      |               |-->--- ASM source file
                      |     MCL(p)    |-->--- EXE/DLL binary
                      |_______________|
````
**(m)** indicates component of M compiler

**(p)** indicates components of PCL project

**(a)** indicates modules belonging to the assembler (which does its own thing)

**CLI** is present on all EXE projects, and is the user-interface (that is, command line parameters and options)

While all projects can directly generate binary code, they can also generate intermediate PCL and intermediate ASM:
````
    Source code -> PCL source -> ASM source -> Binary
````    
But, usually, the intermediates are kept as internal binary, and only written as source code for development, debugging, testing, curiosity, or when any special requirement comes up. (Other diagnostic outputs, AST/ST data etc, not shown)

Everything shown in a box contains those components in a single EXE or DLL file. When I revamp my module system, hopefully those components can be included in a project as simply as:
````
    import pcl
    import mcl
````
However, at the moment, it's very messy.

### Incorporating PCL into other Languages

The only mechanism I have is to provide a binary PC.DLL library. (It can built from my M source using mm.exe.)

That needs a C header file that lists the API functions and the various enumerations used. Since my C is always plain, that can also serve as docs for building bindings for
other languages.

I could create that by hand, but there is no mechanism to keep it synched with the original sources. So I would rather create a option in my MM compiler automatically generate C-style headers when creating DLLs.

Generating a .obj file is another possibly; this can allow the project to be statically linked.

At the moment, providing a C version is not on the cards. The 'Clang' backend of PCL, if completed and applied to itself, would generate very poor, unstructured and largely type-free C code. It might just suffice for compiling into an executable or DLL. But it would still need the properly generated C headers.
