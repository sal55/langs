### My Languages 2022

Name | Description
--- | ---
**M** | Systems programming language, statically typed and compiles to native code
**Q** | Dynamic, interpreted scripting language
**ASM** | x64 assembler with built-in linker

(There was also **PCL**, an intermediate language used by my M compilers for a while, which was turned into an independent language. This 250KB application, which turned .pcl files into .exe, was partly a protest against the complexities of LLVM. It has now been dropped, and the M compiler no longer uses even an internal IL.)

For 2022, M is almost embarrassingly low level and crude compared to the current crop of what are termed 'systems' languages. However, it is at about the same level of C, which remains very popular. It shows there is a still an appetite for this kind of language. I believe that M implements it better.

### Principles

Since this stuff is more of a hobby now, I can concentrate on certain principles for my languages and compilers:

**Simple** I need to understand the languages inside out, which rules out complex ideas and elaborate type systems. This also makes them **Accessible**

**Small** The executables range from 150KB for the assembler, to about 400KB each for M and Q, excluding bundled libraries. There are smaller products, but M and Q are still comprehensive languages, and competing language are far bigger. All three products were just about still fit on a single floppy disk.

**Informal** I don't need to explain this

**Fast** This is mainly about compilation speed. On a low-end AMD Ryzen 3 processor, the M compiler works at up to 0.7M lines per second; Q's bytecode compiler up to 1.5M lps, and the assembler upwardds of 2M lps.

**Self-contained** Each of the three products is a single EXE file with no dependencies, only what is provided by the OS. M and Q will bundle standard libraries within the same file.

**One-File** 

### M 

This started a *very* long time ago when working with 8-bit hardware. The language has evolved, but it only really targets one platform at a time, currently 64-bit Windows running on x64.

### Comparison With C

Here are the main differences:

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
* Programs can also be run directly run from source
* Can create compilable one-file source amalgamations of projects

So there are few of the advanced features that people are so keen on these days, but it still provides, for my purposes, a much more comfortable coding experience compared with C.

### Dislike of C

What had been an indifference to C years ago has now turned into an active dislike and disdain. This is yet another reason why my M language still exists. Not only that, but it strives to be self-contained and self-sufficient. No other tools are used. It does use the C library, but that is just another binary DLL the same as 100 other libraries; the other side of an FFI is just binary machine code.

### The Current Compiler.

The current working one is numbered M5, and I've played with M6 and M7 this year. But those all used an IL/IR stage, which I decided to eliminate in the latest M8 compiler.

This last one is about 10% smaller and 10% faster. I've just got it to the point where it can build and run all my language projects (Assembler, Interpreter, C compiler, PCL processor (last two maintained as test programs) all M compilers including itself), but there's lots more to do in:

* Testing
* Improving coverage of combinations of features (but this won't be 100% for a private language used for a a limited set of applications)
* Tidying up the code
* Seeing if I can make it a bit faster, but without a formal optimiser. My compilers can run very fast even unoptimised, so that is low priority


