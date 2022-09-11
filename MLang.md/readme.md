## My Languages 2022, Overview

I use two complementary languages, plus a support one:

Language | Tool | Target | Description
--- | --- | --- | ---
M | mm.exe (compiler) | Win64 | Lower level systems language
Q | qq.exe (interpreter) | Win64 | Dynamic scripting language
ASM | aa.exe (assembler) | Win64 | x64 assembler/linker

This describes those three tools in more detail:

Tool | Size |Installation | Max throughput | Written in | Sources | Build time | Dependencies
--- | --- | --- | --- | --- | --- | --- | ---
mm.exe | 500KB | 1 File | 0.7Mlps |M |  31Kloc | 80ms | None
qq.exe | 800KB | 1 File | 1.5Mlps | M |  41Kloc | 100ms | None
aa.exe | 160KB | 1 File | 2Mlps+ | M | 13Kloc | 50ms | None

Basically, they are smallish, self-contained, very fast and can be built from scratch more or less instantly. M is self-hosted, the others are implemented in M.

M itself is embarrassinly dated and unsophisticated compared with the current crop of advanced languages that are used for 'systems' programming these days.

Yet it works at roughly the same level as C, which is still popular and still seems to be in demand. That makes me feel better in describing my own take on such a language. The main differences are listed below.
lgamations of any project

### M vs C

While the languages largely do the same things, mine varies in significant ways:

* Algol-style syntax without braces and without begin-end either (which are as bad as braces), and largely semi-colon free. So, uncluttered
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

Plus many more micro-features.

###  Availability

These were in-house tools, now are hobbyist projects used as personal tools. As such, while not exactly private or proprietory, their standard of implementation is not good enough for general use; they cannot be supported.

However anyone is welcome to take from them whatever they wish.

### The Current M Compiler.

I update this every so often. The current one just completed has one away with the IL stage I've used for a few years, which makes it about 10% smaller and 10% faster. I've just got it to the point where it can build and run all my language projects (Assembler, Interpreter, C compiler, PCL \[independent IL project\] processor (last two maintained as test programs) and all active M compilers), but there's lots more to do in:

* Testing and breaking in
* Improving coverage of combinations of features (but this won't be 100% for a personal language used for a limited set of applications)
* Tidying up the generated code
* Seeing if I can make it a bit faster, but without a formal optimiser. My compilers can run very fast even unoptimised, so that is low priority

### History

Both M and Q have very long histories. I'm hesitant to say exactly how long, since they might have been expected to have evolved a lot more by now! Let's say the first version of M was developed for and ran on 8-bit microprocessors with tiny memories.

Actually, they have evolved, but along lines that I considered more useful:

* They have progressed from 8-bit targets to 16, 32 and now 64-bit targets
* Host OS has evolved from None (bare metal), to CP/M, MS-DOS and Windows
* (Experimental versions have also run on Linux, but that involved targetting C and using a C compiler, which was unsatisfactory)
* Both have acquired module schemes
* Both are whole-program compilers and both compile code at very fast speeds, allowing more or less instant build times
* This allows M to be used as a scripting language
* Both have super-simple single-file installations
* Both can generate single-file amalgamations for easy distribution of application source code
* They have acquired a plethora of small features that I consider more handy then elaborate type systems

### Future Development

There's quite a lot I can think of to put into M that is suited to the level of language (unlike C and VLAs, say), and that would straightforward to implement and that can be easily appreciated. This is on top of improving covering of existing features.

But it would not be worthwhile since the range of use of M in my hands is limited. Mainly for language projects and libraries. It's too much work just for box-ticking.

I did try earlier this year to combine M and Q into one language; it didn't work. (Resulting in combining the disadvantages of each rather then the advantages!)

Then I tried to embed a version of M ('M Lite`) into Q. That got a fair way, but I got stuck in the code generator for M, which would have involved incorporating the whole backend of M, into the Q interpreter. There were also too many boundary issues to sort out (jumping between interpreted and dynamic code, and converting Q's high level dynamic types to and from M's simpler ones).

The new M compiler, with one less pass, would be simpler, but the result is still unwieldy.

Q can already call M via its FFI mechanism. When M is compiled to a shared library, it can generate the FFI info automatically, in the form of a Q import module. But that can't easily share non-executable entities (types, macros, named constants, enum tables, variables), which anyway really need to originate in Q.

So the solution I'm looking at is for Q and M to be able to share modules that describe only such entities, and not executable code like functions. That allows modules containing M code (the full language now), to be processed with the M compiler; not half of an M compiled hacked into the interpreter.

The challenge is to make use of the dual language intuitive and effortless.


### Further Info

Not ready.
