Blog Post.

I develop small-scale lower-level languages and tools on my Windows PC. This year I tried a number of experimental projects but none has gone anywhere. Even a working production compiler has been abandoned; the previous version was better and did more!

It doesn't look like I have anything new to add here, so I'm consolidating what I have so far. I've settled on a set of 6 tools that I will keep, which are in the general class of "Assemblers, Compilers, Interpreters". I haven't included emulators which is a separate area.

So, these languages and tools are what are summarised here.

**Languages**
````
'M'          Systems language
'Q'          Dynamic scripting language (both of these are lower level than most)
ASM          x64 Assembly, but specifically my syntax
C            C subset, roughly C90-C99
PC/IL        IR/IL used by the compiler products, which also exists as
             a textual source format
````

**One-File** This is a common theme with these languages and tools:

* Each tool is a single, self-contained executable file
* Each tool accepts only one input file:
  * For M and Q languages, it will be the lead module; the module scheme finds
    the rest. These are also **whole-program compilers**
  * For AA and PC tools, the input represents a complete program
  * For the C language, the input file must be a complete program to make use of
    all the output options. If not, then ASM or OBJ outputs must be used

* Each tool, if it writes a file, will generate a single, monolithic file.
  Those include .asm, .pcl, .obj and .c files. The C file will be self-contained
  with no includes.

* The M compiler can also generate a .ma text source, which is a self-contained
  amalgamation of all source and support files. This file can be directly
  compiled by mm.exe.

**Combinations** The following chart shows the outputs possible for each tool. A lot is possible as MM, MC, BCC, PC share the same backend. As does AA, but it does not use the IL parts.
````
Lang    Tool       Interp Run    EXE    PC     ASM    MX/ML  OBJ    C

M       mm.exe     Y      Y      Y      Y      Y      Y      Y      -
M       mc.exe     -      -      -      -      -      -      -      Y
PC/IL   pc.exe     Y      Y      Y      (Y)    Y      Y      Y      -
ASM     aa.exe     -      Y      Y      -      Y[1]   Y      Y      -
Q       qq.exe     Y      -      Y[2]   -      -      -      -      -
C       bcc.exe    Y      Y      Y      Y      Y      Y      Y      -

Outputs:
Interp      Interpret either the PC intermediate language, or the Q bytecode
Run         Generate native code into memory and run immediately
EXE         Write EXE or DLL binary
PC          Write PC/IL as a textual source file
ASM         Write native as an assembly source. There are 4 syntax choices:
            AA/NASM/GAS/ML360, I normally choose AA which is mine
MX/ML       An obsolete binary format, which is a far simpler alternative
            to EXE/DLL, which I am maintaining. (These things have a habit
            of suddenly becoming useful!)
OBJ         PE+ COFF format object file. Needs external tools to process
C           There is a C transpiler of sorts that converts my PC/IL into
            poor quality linear C.
````
**[1]** Yes, AA can turn ASM into ASM! This is because it shares the compiler backend.
However this can also be used to convert programs my AA syntax into NASM/GAS/ML360.

**[2]** Q is a dynamic language that is always interpreted. While there was an experiment
to turn bytecode into native code, this EXE option is different: `qq -exe hello` will
create a file hello.exe that comprises the Q interpreter, .q source files, and any libraries.

**Size, Speed and Implementation** All tools are written in my M systems language,
itself long self-hosted. No other languages were used, except perhaps for assembly
for initial bootstrap.
````
            Size     (Libs)        Throughput (to EXE)

aa.exe      106 KB   --            2-3M lines per second
bcc.exe     328 KB   (33 KB)       0.5M lps
mc.exe      324 KB   (107 KB)      0.4M lps (to .c file)
mm.exe      446 KB   (107 KB)      0.5M lps
pc.exe      182 KB   --            1.7M lps
qq.exe      520 KB   (280KB)       1.5M lps (to bytecode)
````
The (Libs) figure is how much of the size is due to the bundled source files or headers. Throughput figures are for a single core on my PC, and might be twice as fast on a better one.

**Optimisation** This is not something my compilers do. But as can be seen above, it
doesn't really slow them down.

**'PC' IL Processor and LLVM** The PC product had fallen into disuse, as I had no real need for
it, but it has been made to work again for completeness. Originally it was my answer to LLVM, as being magnitudes smaller, simpler, and faster (in compile-time).

As can be seen it is a standalone product of under 0.2MB. It can be built from source in under 60ms:
````
c:\mx>tim mm pc
Compiling pc.m to pc.exe
Time: 0.058
````
It can take an IL source file for an entire program, and do just about everything with it from the same pc.exe tool (see above).

Here are some runtime comparisons with LLVM for the Fannkuch(11) benchmark:
````
                  
LLVM   2   secs   via clang -O3 -S -emit-llvm fann.c, then clang -O3 fann.ll
LLVM   4   secs       clang     -S -emit-llvm fann.c, then clang -O3 fann.ll
PC     2.4 secs       pc fann.pcl
````
PC's code is only a little slower, provided the LLVM IR is itself optimised first. That part is not clear: how much optimisation your front-end compiler is
expected to do. Here Clang does that task.

**JIT** For my systems language, JIT is not needed: programs can generally be run from source as effortlessly as a scripting language, as native code too.

For the scripting one, this is something I looked at, but it is beyond me. I'm also not convinced of the likely speedups. My Q interpreter is anyway already quite brisk compared with most.

So I'll concentrate on a two-language approach to applications. With the ability to package everything into an EXE file, this can be extended to helper libraries that would normally reside in a DLL. So an app using both languages can still be built into a single tidy, binary.

**Links** I don't really do these any more. These products are anyway for my personal use and are not supported. Some many identify me, or my stuff, from previous accounts. But if interested, google for 'github' and 'sal55', where you can see some outdated docs.
