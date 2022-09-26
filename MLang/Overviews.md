### Shared Characteristics of All My Languages

* Each is implemented as a smallish (0.2MB to 1.0MB), single, self-contained executable, which includes any libraries needed.
* No installation needed other than copying that one file anywhere, and just running it
* No dependencies to use the tools, only the relevant host OS. (Certain applications and libraries may have extra dependencies.)
* Original sources can be provided as a single, buildable amalgamated source file (.ma files)
* Generated-C versions can be provided as a single C source file
* Very fast with throughput from 0.7M lines per second (M compiler) to 2M+ lines per second (assembler). (Building all 3 tools from scratch takes about 0.2 seconds)
* All are written in my M language, and the M compiler itself is self-hosted

### M Overview

M is my lower-level systems programming language, which was first developed for 8-bit Z80 machines in the early 1980s.

Now it works mainly on 64-bit Windows machines, but there is a way now to make it work on Linux.

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

### Q Overview

The nearest mainstream language might be Python, at a 30% match, if comparing capability within the language.

* Identical syntax to M as stated above, and shares many common features
* While it has dynamic typing, it is much less dynamic than Python
* Built-in FFI and direct support for C-style type systems
* Executes more briskly than many non-JIT interpreted/dynamic languages, and has a built-in accelerator
* Includes many features not present in Python (proper named constants, `switch`, embedded text/binary files, proper records etc; see link in home page)

Both M and Q can produce one-file source amalgamations of any application.

### AA Overview

'AA` is actually the name of the tool, the language as such is just 'ASM' but that is rather generic.

The nearest mainstream asssembler might be Nasm, at a 80% match. However my AA product is faster (literally 1000 times faster on one test, although that is more of an issue with Nasm, and one of the reasons I developed my product; AA is still 10 times faster than YASM.)

AA supports only a subset of all the x64 instructions, mainly the ones I use myself or used by my code-generator. 

* Lean, minimal design intended for machine-generated code
* Can use alternative, more consistent register naming and ordering
* Can directly assemble multiple ASM files to one EXE file with no intermediate object files
* Can do the job of a linker

