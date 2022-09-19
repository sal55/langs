## 'M' and 'Q' Languages

These are my private languages as of 2022:

**M** My systems programming language

**Q** My dynamic scripting language

**AA** My take on x64 assembly

### Description

* Old-fashioned (both devised in 1980s, and little changed)
* Embarrassingly unsophisticated, they are devoid of most trendy modern features
* Primitive type systems, in particular no proper sum types
* No lambdas, closures, continuations, currying ...
* Unsafe (eg. everything is mutable)
* No 'language servers' or other ways to get syntax highlighting
* No proper references or tutorials attractively presented (ie. something better than Github markdown)
* Minimal and buggy compiler implementations
* No optimising compilers and no static analysis
* Poor error reporting
* No debuggers or other such tools
* No ecosystem
* Not tested on tens of million of lines of code by an army of users
* No community (I'm the only user, and even I don't use them that much!)
* Minimal, practically non-existent libraries, and no easy way of creating bindings to more
* Insular, incapable of working with other software except via DLL libraries - or files
* Work only on Windows 64, not on Linux, and can never work on anything like Android
* Personal languages with patchy and buggy coverage of combinations of features, types and operations
* No Unicode support, except what works by accident through UTF8
* No formal grammar, which is not practical anyway as syntax has ambiguities
* No binaries available to use, which cannot be supported anyway

Basically, by any comparison to anything else, they are rubbish. So, for anyone still reading, why am I writing about them?

I'm primarily documenting what I've done for my own benefit, since there are various odd features where I can never remember how they work and in what language, and would like to finally have a stable set of languages as I've spent too long tinkering.

This might be of interest to other people, or there might be things that someone can take away, even it's to help avoid making the same mistakes.

### So, Are There Any Upsides?

From my perspective, yes:

* One of my languages will always be my first choice in any project (and has been for, ahem, about 40 years...)
* I understand them extremely well
* I rarely have to do battle with the language; they will generally let me do what I want
* Naturally, they have a syntax I like
* The tools (compiler, interpreter, assembler) are smallish, lean, self-contained and very fast
* There are satisfyingly few external dependencies. Basically, they need a Windows OS, and that's it.

(The first crude version of M didn't even use an OS. I wrote a long article about my native code compilers [here](../mycompilers.md).)

So I have the advantage, which I know will not be shared by anyone else, of having language and tools  tailored to my needs. But I'm also not bothered that the languages and tools might be considered unbelievably crude by many (perhaps I'd better not say anything about the text editor and IDE I use).

M and Q have both in the past worked on Linux, so that box has been ticked. But that relied on using a C target for the M compiler, so not satisfactory.

### M Overview

The nearest mainstream alternative would be C, with a roughly 80% match, in how the type system works, and what the languages can do. But M varies in some significant ways:

* Algol-style syntax with no braces
* Case-insensitive
* Line-oriented and largely semicolon-free
* Primarily 1-based indexing
* Module scheme
* Out-of-order definitions
* 64-bit-based types
* Expression-based (interchangeable expressions and statements)
* Designed for whole-program compilation
* Companion scripting language (Q) with identical syntax

So, compared with C at least, M comes out well. The characteristics of the M compiler are:

* Completely self-contained in a 0.5MB executable
* Whole-program compiler works at up to 0.7M lines per second on my low-end PC
* Can run programs from source
* Can build itself in 0.08 seconds

### Q Overview

The nearest mainstream language might be Python, at a 30% match, if comparing capability within the language and ignoring all the stuff listed above

* Identical syntax to M as stated above, and shares many common features
* While it has dynamic typing, it is much less dynamic than Python
* Built-in FFI and direct support for C-style type systems
* Executes more briskly than many non-JIT interpreted/dynamic languages, and has a built-in accelerator
* Includes many features not present in Python (proper named constants, `switch`, embedded text/binary files, proper records etc etc)
* Interpreter is a self-contained 0.8MB executable.

Both M and Q can produce one-file source amalgamations of any application.

### AA Overview

The is the assembler. The nearest mainstream asssembler might be Nasm, at a 80% match. However my AA product is faster (literally 1000 times faster on one test, although that is more of an issue with Nasm, and one of the reasons I developed my product; AA is still 10 times faster than YASM.)

However, AA supports only a subset of all the x64 instructions, mainly the ones I used myself or by my code-generator.

* Lean, minimal design intended for machine-generated code
* Can use alternative, more consistent register naming and ordering
* Can directly assemble multiple ASM files to one EXE file with no intermediate object files
* Can do the job of a linker
* Self-contained in a 160KB executable
* Throughput is 2M+ lines per second

### Further Docs

Some information about how the three main tools work: [Tools](Tools.md).

Documenting most features of M: [M Features](Mfeatures.md).

Info about the module scheme used by M and Q is not ready.

Docs for AA and Q are also not ready, but much of those M docs apply to Q too.

However, this is a list of Q features which are missing or poorly supported in Python: [QBasics](../QLang/QBasics.md)

### Future Development

M and Q have been gradually converging, but several attempts to turn them into one language have failed, even though they are so similar. For example, M's compiler could now run applications directly from source just like Q.

But they will be kept as discrete languages with separate implementations.

What I will concentrate on is making them work together more smoothly, and allowing them to share the complex environment of the same application.

(This is exactly how the original Q language, an add-on scripting language then called 'MPL', was used within my graphical applications. Those had a very rich environment more like an mini-OS. But that was for a dedicated application with an embedded version of the interpreter.)

So, this will be my 'Two Language` solution. Well, if it works...

### Demos

I said there'd be no binaries, but probably I will supply at least `mm.exe` (M compiler) and `mm.ma` (M sources in one file) to demonstrate how it can build itself from scratch, using only two files. But this will need some preparation.
