### Limitations of MCC C Compiler


* Doesn't support the full language. A fuller description of the subset of C that is compiled is in [C Specs](C%20Specs.md).

* Supports only x64 target

* Generates only asm source in Nasm format (suitable assembler and linker not provided)

* Compiles only one C source file at a time

* Generates code for Win64 call convention only (not Linux64)

* Code generation was currently very poor, and from 8.10 is less than half the speed. (However, after compiling itself, the resulting program can still compile C at 200,000 lines per second.)

* Incorporates many hard limits for symbol table size, nested blocks, number of parameters per function and so on

* Provides standard headers for between 30 and 40 or so C headers (some may be for POSIX, as many apps require them even on Windows), but these are largely unpopulated. (They were supposed to be filled in as I want along, but my test programs are remarkably undemanding in the range of standard features needed.)

* (Also provides a windows.h header, but currently that is less than 1K lines long.)

* Doesn't come with any headers for other libraries, and doesn't support POSIX

* Will generally not work with headers belonging to other compilers.

* Not all constant expressions will collapse to a single value (this support is done as needed)

* Some checks such as for duplicate function definitions are not in place (I just forgot)

* Compiler will abort at first error; currently there are no warnings.

* Because variables are directly written as Nasm identifiers, some names can clash with Nasm reserved words (eg. "ch" which is a register name). This is a problem with targeting Nasm.

* Testing so far has concentrated on compiling correct programs not incorrect ones. So error detection and error reporting is poor. (Especially so with code-generator errors, but a such errors are usually internal ones so should appear rarely.)
