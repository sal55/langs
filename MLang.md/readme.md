## M Language

'M' is my lower-level systems language, well below the level of ones like Rust, Java, Zig, C++, C#, at roughly the same level of C regarding its type system and thje sorts of things it can do.

I would have let it die, but C's continuing popularity shows a need for this kind of language. That makes me happier about maintaining my own take on such a language.

This is an overview of what it is, what it does and what I've been doing with it.

### My Languages

Name | Description
--- | ---
**M** | The systems language discussed here, which is statically typed and compiles to native code
**Q** | My dynamic, interpreter scripting language, described elsewhere (or it will be).
Plus: | 
**ASM** | This is my assembler (with built-in linker) for x64
**PCL** | A now discontinued IL used by the M compileers

M and Q are my primary languages, sharing the same syntax, current development is about allowing them to interact better.

### M History

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


