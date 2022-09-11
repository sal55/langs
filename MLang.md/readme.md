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
* Built-in 'Embed' (Only just being introduced into C23)
* Built-in 'tabledata` (A superior approach to 'X-macros`)
* Very fast, single-file and self-contained whole-program compiler
* Can create compilable one-file source amalgamations of projects
* Does not need a build system like `make` (submit only the lead module, it will discover program structure automatically)

Plus many more micro-features.

###  Availability

These were in-house tools, now are hobbyist projects used as personal tools. As such, while not exactly private or proprietory, their standard of implementation is not good enough for general use; they cannot be supported; and documentation is erratic.

However anyone is welcome to take from them whatever they wish.

### The Current M Compiler.

I revise this every so often. The current one just completed has done away with the IL stage I've used for a few years, which makes it about 10% smaller and 10% faster. I've just got it to the point where it can build and run all my language projects (Assembler, Interpreter, C compiler, PCL (independent IL project) processor and all active M compilers), but there's lots more to do in:

* Testing and breaking in
* Improving coverage of combinations of features (but this won't be 100% for a personal language used for a limited set of applications)
* Tidying up the generated code
* Seeing if I can make it a bit faster, but without a formal optimiser. My compilers can run very fast even unoptimised, so that is low priority

### History

M started off some 40 years ago, and Q perhaps 35 years ago. You might have expected them to have evolved a lot more than they have!

But they were just tools I devised to help in my main work, and worked well enough.

(Actually, they have evolved and become more polished, but also along lines that I considered more useful than such as whole program compilation and applying my 'one-file' principle as much as possible: one-file installations, outputs, amalgmations, distributions.)

### Future Development

There's quite a lot that be put into M while not making it significantly higher level, but as my personal use of it is limited, most would not be worthwile. As it is, it can still do anything that C can do.

I did try earlier this year to combine M and Q into one language; it didn't work. (Resulting in combining the disadvantages of each rather then the advantages!)

Then I tried to embed a version of M ('M Lite`) into Q, but it got unwieldy, and there were many boundary issues to be resolved still.

Q can already call M via its FFI mechanism, and the M compile can automatically generate the interface modules Q needs. But this does deal with shared entities such as types, records, enums, named constants, macros and variables, which are needed for an application to make proper use of both.

(In the 1990s, M was used for apps, and an early version of Q was used as an add-on scripting language implemented within the app. There, it had the rich, shared environment of that 3D graphics- and GUI-intensive application. Now Q is a standalone language.)

So a solution I'm looking at is for Q and M to be able to share modules that describe only such entities, and not executable code like functions. That allows modules containing M code (the full language now), to be processed with the discrete M compiler.

The challenge then is to make use of the dual language intuitive and effortless.

### Further Info on Tools

For more details of how these three main tools work, see [Tools](Tools.md).

### Further Info

For more details about these three languages ... Not ready.

### What's Missing

There is a huge range of things that people expect from a language these days. Other than features within the language, which are the easy bits, the following are the difficulties with trying to maintain your own language:

* No editor will understand your syntax so there's no syntax highlighting (on code editors, github, paste-bin, pretty much everything)
* Not IDE will know your language. (I understand there are things like language servers, but that's probably a bigger project than my language, and beyond me). Suffice that my source code will be shown as plain text and with the wrong tabbing
* Lack of libraries. The standard libraries are tiny. There are huge numbers of external libraries, but I don't have an army of people writing bindings for my language. Neither do I have a tool that can take a bunch of C headers and creating bindings from that. (There is one, but it doesn't work; C - actual C - was terrible choice for such APIs)
For general use, there need to be large numbers of people trying out your language, testing the boundaries, uncovering bugs and omissions and design faults
* Decent documentation such as tutorials and reference manuals, and presented in a manner that is easy on the eye. While github markdown is better than plain text, it still looks unappealing. And it still needs to be written

