### Shared Characteristics

* Each is implemented as a single, smallish (0.2MB to 1.0MB), self-contained executable, which includes any libraries needed
* All are very fast:
  * **mm.exe** 0.5MB, up to 0.7M lines per second throughput: source to EXE (on my low-end PC, unoptimised)
  * **qq.exe** 0.8MB, up to 1.5M lines per second throughput (source to runnable bytecode)
  * **aa.exe** 0.16MB, 2M+ lines per second throughput (source to EXE)
* No installation needed other than downloading or copying that one file anywhere, and just running it
* No dependencies to use the tools, only the relevant host OS. (Certain applications and libraries may have extra dependencies.)
* Original sources of each tool can be provided as a single, buildable amalgamated source file (.ma files)
* Generated-C versions can be provided as a single C source file
* All are written in my M language, and the M compiler itself is self-hosted

### M Overview

M is my lower-level systems programming language, which was first developed for 8-bit microprocessors. Now it works mainly on 64-bit Windows machines, but there is a way now to make it work on Linux.

The nearest mainstream alternative would be C, with similarities in their type systems and what the languages can do. But M varies in some significant ways:

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

Contemporary languages that are used for systems programming (Java, D, C#, Go, C++, Rust, Zig) are much more advanced with modern type systems and all those things listed in my Missing summary.

Still, for *my* systems programming, this is my first choice.

### Q Overview

* Interpreted scripting language
* Dynamically typed only; much less dynamic than typical scripting languages like Python
* Identical syntax to M, and shares many of the same features
* Built-in FFI and direct support for C-style type systems
* Executes more briskly than many non-JIT interpreted/dynamic languages, and has a built-in accelerator
* Includes many fundamental features that I value, which are not present in Python see [Q versus Python](QBasics.md).
* Both M and Q can produce one-file source amalgamations of any application.

### AA Overview

'AA' is actually the name of the tool, the language as such is just 'ASM' but that is rather generic.

The nearest mainstream assembler might be Nasm. However my AA product is much faster.

AA supports only a subset of all the x64 instructions, mainly the ones I use myself or used by my code-generator. 

* Lean, minimal design intended for machine-generated code
* Can use alternative, more consistent register naming and ordering
* Can directly assemble multiple ASM files to one EXE file with no intermediate object files
* Can do the job of a linker

