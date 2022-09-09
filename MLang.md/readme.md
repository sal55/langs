## M Language

'M' is my lower-level systems language. I'm finally decided to stop messing about with it, and try and get a stable version.

This is not a sophisticated language. It is well below the level of ones like Rust, Java, Zig, C++, C#.

I would have let it die, but since it is not much higher level than C, which is is still tremendously popular, showing there is still a demand for a simpler language of this kind.

This is an overview of what it is, what it does and what I've been doing with it.

### My Languages

**M** The systems language discussed here, which is statically typed and compiles to native code

**Q** My dynamic, interpreter scripting language, described elsewhere (or it will be). Again, this is lower level and less dynamic than typical scripting languages, but also tend to be faster. It shares syntax with M, and future development involves making them work together better

**ASM** This is my assembler (with built-in linker) for x64, created because the available third party products were so woeful

**PCL** A dead project, this was an Intermediate Language used by the M compilers, that could be used standalone or via an API. I wanted to see what LLVM could be like if I had a go. This serves as a test program now.

**C** Clearly not mine, but one of my projects is a C compiler, which works with a large subset of the language, whose compiler is used as another test program.

### M History

This started a *very* long time ago when working with 8-bit hardware. The language has evolved, but it only really targets one platform at a time, currently 64-bit Windows running on x64.

### Comparison With C

I said it's about the same level as C; the data types are similar, you can do the same sorts of things, and yet it is also different. Here are the main differences:

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

So few advanced features that people are keen on these days, but it still provides a very comfortable coding experience compared with C.

### The Current Compiler.

The current working one is numbered M5, and I've played with M6 and M7 this year. All those used an IL/IR stage, but I decided to do away with that.

The previous compilers used stages like this:
````
    Source -> AST1 -> AST2 -> AST3 -> PCL -> MCL -> ASM/EXE/MX/RUN
````
M8 removed that middle PCL step, so:
````
    Source -> AST1 -> AST2 -> AST3 -> MCL -> ASM/EXE/MX/RUN
````
It just did not give enough benefits. The M8 compiler is about 10% smaller and 10% faster. I've just got it to the point where it can build and run all my language projects (Assembler, Interpreter, C compiler, all M compilers including itself), but there's lots more to do in:

* Testing
* Improving coverage of combinations of features (but this won't be 100% for a private language used for a a limited set of applications)
* Tidying up the code
* Seeing if I can make it a bit faster, but without a formal optimiser. My compilers can run very fast even unoptimised, so that is low priority

I also want to improve how it interacts with Q; see below

I've just got M8 to a point where it can build my main projects
