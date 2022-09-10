## My Languages 2022, Overview

I use two complementary languages, plus a support one:

Name | Written In | Description
--- | --- | ---
**M** | M | Systems programming language, statically typed and compiled to native code
**Q** | M | Dynamic, interpreted scripting language
**ASM** | M | x64 assembler with built-in linker

### M Language

* Originally created for 8-bit microprocessors, as a systems language, now evolved and used to create standalone programs and libraries for Windows 64
* Is embarrassingly low level and crude compared with contemporary languages for 'systems' use, but so is C, which remains popular and in demand
* See M vs C below; my product gives a more comfortable programming experience
* Self-contained: everything is in one 0.5MB executable file
* Whole program compiler generating .exe files
* Fast compilation at up to 0.7M lines per second on low end PC
* Self-hosted, builds itself in 80msec
* Zero external dependencies other than what comes with Windows
* Can generate and directly compile one-file amalgamations of any project

### Q Language

* Dynamically typed, byte-code interpreted
* Started off as an add-on scripting language for my applications, now standalone
* Unusual scripting language which is a lot less dynamic than most, but also faster
* Self-contained interpreter in one 0.8MB executable for Windows 64
* Has excellent built-in support for FFIs and the types encountered in such APIs
* Bytecode compiler works at up to 1.5M lines per second
* Built-in accelerator which can double the speed of some programs
* Shared syntax with the M language
* Can generate and directly compile one-file amalgamations of any project

### ASM x64 Assembler

* Created to overcome various problems with mainstream products like Nasm and 'ld'
* Lean, cut-down assembler designed for the output of my compilers (during developement, as these normally bypass ASM)
* Incorporates a simple linker, can directly generate .exe files
* Works at 2M lines per second and up
* Assembler is a self-contained 0.16MB executable

### M vs C

While the languages largely do the same things, mine varies in significant ways:

* Algol-style syntax without braces and without begin-end either (which are as bad as braces)
* Case-insensitive syntax
* 1-based indexing with option to use 0-based or N-based
* Module scheme
* Out-of-order definitions throughout
* Default 64-bit data types (integers, floats, pointers)
* Built-in `print` and `read` statements
* Slices
* Has 'Embed' (the C23 name for features I've long had)
* Very fast, single-file and self-contained whole-program compiler
* Can create compilable one-file source amalgamations of projects
* Does not need a build system like `make` (submit only the lead module, it will discover program structure automatically)

Plus dozens more, including many micro-features.


###  Availability

These were in-house tools, now are hobbyist projects. As such, while not exactly private or proprietory, their standard of implementation is not good enough for general use; they cannot be supported. (They're a little *too* informal!)

However anyone is welcome to take from them whatever they wish.

### The Current M Compiler.

I've been used an IL stage for a years, but have decided to do away with that. For my limited targets, it brought me few benefits.

The last one is about 10% smaller and 10% faster. I've just got it to the point where it can build and run all my language projects (Assembler, Interpreter, C compiler, PCL \[independent IL project\] processor (last two maintained as test programs) and all active M compilers), but there's lots more to do in:

* Testing and breaking in
* Improving coverage of combinations of features (but this won't be 100% for a personal language used for a a limited set of applications)
* Tidying up the code
* Seeing if I can make it a bit faster, but without a formal optimiser. My compilers can run very fast even unoptimised, so that is low priority

### Future Development

There's quite a lot I can think of to put into M that is suited to the level of language (unlike C and VLAs, say), and that would straightforward to implement and that can be easily appreciated.

But it would not be worthwhile since the range of use of M in my hands is limited. Mainly for language projects and libraries. It's too much work just for box-ticking.

I did try earlier this year to combine M and Q into one language; it didn't work. (Resulting in combining the disadvantages of each rather then the advantages!)

Then I tried to embed a version of M ('M Lite`) into Q. That got a fair way, but I got stuck in the code generator for M, which would have involved incorporating the whole backend of M, into the Q interpreter. There were also too many boundary issues to sort out (jumping between interpreted and dynamic code, and converting Q's high level dynamic types to and from M's simpler ones).

The new M compiler, with one less pass, would be simpler, but the result is still unwieldy.

Q can already call M via its FFI mechanism. When M is compiled to a shared library, it can generate the FFI info automatically, in the form of a Q import module. But that can't easily share non-executable entities (types, macros, named constants, enum tables, variables), which anyway really need to originate in Q.

So the solution I'm looking at is for Q and M to be able to share modules that describe only such entities, and not executable code like functions. That allows modules containing M code (the full language now), to be processed with the M compiler; not half of an M compiled hacked into the interpreter.

The challenge is to make use of the dual language intuitive and effortless.


### Further Info

Not ready.
