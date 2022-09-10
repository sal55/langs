### My Languages 2022

I use two complementary languages, plus a support one:

Name | Written In | Description
--- | --- | ---
**M** | M | Systems programming language, statically typed and compiled to native code
**Q** | M | Dynamic, interpreted scripting language
**ASM** | M | x64 assembler with built-in linker

All products work on Windows 64 only. I decided not to continue experiments on Linux although M and Q have both worked there in the past.

### Principles

Since this stuff is more of a hobby now, I can concentrate on certain principles for my languages and compilers:

Simple, Accessible, Small, Informal, Fast, Self-contained, Zero-dependencies, One-file

Languages make all sorts of such claims about themselves, but some of these can at least be quantified:

**Small** My assembler is 160KB; My M compiler is 400-500KB; the Q interpreter is 400-700KB. The higher figure includes bundled library sources. The whole suite would just fit on a 1.44MB floppy still. There are much smaller products, but for equivalent languages, the relevant tools are usually bigger.

**Fast** This refers to compilation speed. The M compiler has a throughput up to 0.7M lines per second; the Q bytecode compiler up to 1.5Mlps; and the assembler upwards of 2Mlps, all on a low-end PC.

**Self-contained** All three products are run from a single executable file (that is, one each!). No other support files are needed.

**Zero-dependencies** No external languages, compilers or other tools are needed to build or to use. And no add-on libraries. There is a dependency on some C functions within msvcrt.dll, but this is part of Windows. The M compiler is self-hosted, and other tools are written in M.

**One-File** This applies to several aspects of what I do:

* Installations are single-file, self-contained installations as I've said
* Any outputs from these tools are always single files:
  * A single EXE, ASM, OBJ, MX/ML file (or a PCL file at one time)
  * A single .ma or .qa file representing amalgamated source files
* Inputs can also be a single .ma or .qa or .asm source file representing an entire application
* When I used to distribute my programs as C source versions, it was as a single C source file automatically generated, designed to be as simple to build as hello.c.


### History

The M language started a *very* long time ago when working with 8-bit hardware. Actually, about a decade before the first time I tried C (and promptly decided to stick with my language). The language has evolved, but it only really targets one platform at a time, currently 64-bit Windows running on x64.

The Q language started off as an add-on scripting language for my applications.

The x64 assembler was created out of necessity (like the above actually), since external tools such as Nasm and LD had all sorts of issues. The backend of the assembler, which can directly generate EXE files, was incorporated into the M compiler.

### M vs C

For 2022, M is almost embarrassingly low level and crude compared to the current crop of what are termed 'systems' languages. However, it is at about the same level of C, which still remains very popular. It shows there is a still an appetite for this kind of language.

But while M and C do the same things can be used for the same tasks, M has these differences:

* Algol-style syntax without braces and without begin-end either (which are as bad as braces)
* Case-insensitive syntax
* 1-based indexing with option to use 0-based or N-based
* Module scheme
* Out-of-order definitions throughout
* Default 64-bit data types (integers, floats, pointers)
* Built-in `print` and `read` statements
* Slices
* Embed (the C23 name for features I've long had)
* Very fast, single-file and self-contained whole-program compiler
* The ability to run programs directly run from source
* Can create compilable one-file source amalgamations of projects
* Does not need a build system like `make` (submit only the lead module, it will discover program structure automatically)

Among dozens more. It provides a much more comfortable coding experience compared with C.

Actually, my disdain for C has developed to an active dislike (far too much fuss is made of a language with many appalling and unnecessarily dangerous design choices). So, while my M compilers supported a C target for a while (helping portability and benefiting from optimised code), I have decided against that for now. I will use C as necessary as it comes up in APIs, but I will rely on it as little as possible.

###  Availablity

In the past, my languages and compilers were in-house tools created for productivity, now are hobbyist projects.

As such, while not exactly private or proprietory, their standard of implementation is not good enough for general use; they cannot be supported. (They're a little *too* informal!) However anyone is welcome to take from them whatever they wish.

### The Current M Compiler.

The current stable one is numbered M5, and I've played with M6 and M7 this year. But those all used an IL/IR stage, which I decided to eliminate in the latest M8 compiler.

This last one is about 10% smaller and 10% faster. I've just got it to the point where it can build and run all my language projects (Assembler, Interpreter, C compiler, PCL \[independent IL project\] processor (last two maintained as test programs) and the M5-M8 M compilers), but there's lots more to do in:

* Testing and breaking in
* Improving coverage of combinations of features (but this won't be 100% for a personal language used for a a limited set of applications)
* Tidying up the code
* Seeing if I can make it a bit faster, but without a formal optimiser. My compilers can run very fast even unoptimised, so that is low priority

### Future Development

There's quite a lot I can think of to put into M that is suited to the level of language (unlike C and VLAs, say), and that would straightforward to implement and that can be easily appreciated.

But it would not be worthwhile since the range of use of M in my hands is limited. Mainly for language projects and libraries. It's too much work just for box-ticking.

I did try earlier this year to combine M and Q into one language; it didn't work. (Resulting in combining the disadvantages of each rather then the advantages!)

Then I tried to embed a version of M ('M lite`) into Q. That got a fair way, but I code stuck in the code generator for M, which would have involved incorporating the whole backend of M, into the Q interpreter. There were also too many boundary issues to sort (switching between interpreted and dynamic code, and converting from high level dynamic types to and from M's simpler ones).

The new M8 compiler, with one less pass, would be simpler, but the result is still unwieldy.

Q can already call M via its FFI mechanism. When M is compiled to a shared library, it can generate the FFI info automatically, in the form of a Q import module. But that can't easily share non-executable entities (types, macros, named constants, enum tables, variables), which anyway really need to originate in Q.

So the solution I'm looking at is for Q and M to be able to share modules that describe only such entities, and not executable code like functions. That allows modules containing M code (the full language now), to be processed with the M compiler; not half of an M compiled hacked into the interpreter.

The challenge is to make use of the dual language intuitive and effortless.


### Further Info

Not ready.
