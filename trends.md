(Adjunct to my Reddit post.)


Feature | My Style | Popular | Notes
--------|--------|----------- | ----------------
**Case sensitive** | Case Insensitive | Case Sensitive
Example | abc, Abc, ABC are same name | abc, Abc, ABC etc are distinct
**Array Lower Bounds** | N-based, default 1 | 0-based only
Example | Use any of 1, 0 or N |
**Text (code) editors** | Use hard line-stops | Use soft line-stops | 'Soft' Left/Right etc do stop at line beginning or end, even though source code is line-oriented
**Compiler Strictness** | Unequivocal | Depends on options | Typical C compilers are very lax unless you twist their arm to fail programs
**Compiler options** | <10 | Loads | (Clang has 800; gcc has 1000s)
**Need file extension** | Optional | Yes
Example: | bcc hello | gcc hello.c, lua hello.lua
**Default output file** | Based on first input file | Varies | gcc hello.c produces a.out or a.exe
Example | bcc hello => hello.exe
**Linker needed** | Designed out | Yes
Exception | Linker can be used when .obj outpus chosen
**Tool deployment** | Self-contained binary file | Typically multi-file, multi-directory
**Make files** | Not used | Everything uses make files |Make files introduce complications and dependencies that rarely work on Windows
Example | All done with the compiler: compile lead module of application
**Binary distribution** | Typically one executable file | Various
**Installation** | None needed, use binary as | Usually complex
**Source distribution** | One .c file rendering or one .ma file | Complex
**Building from source** | Compile the one file directly | Typically makefiles used| Or worse (ie. Linux-dependent auto-conf scripts), or CMake
**Macros** | Hardly ever used | In C, hardly ever not used |
**Assemblers** | Always simple | Always complicated | Most assemblers make a big deal of macros and endless unnecessary features
**LIB files** | Never used | Common | LIB files are .lib/.a files, collections of object files, often mere interfaces to DLL/.so files
Example | Use DLL directly
**Dependencies** | Minimised | Maximised (seems mandatory on Linux)
**Block delimiters** | if then else fi | if () {} else {} | or 'if () else' or 'if () {} else' or 'if () else {}
Alternate | if then else endif/end if | if: else: |
**Function def returning a value** | function F => T | T F(void) | Return type is T
Alternate  | | def F()
**Function def not returning a value (ie. subroutine or procedure)** | proc G | void G(void)
Alternate  | | def F()
**Reading 3 values from the user** | readln a, b, c  | Complex | 3 values *on the same line*
**Compilation unit** | Whole program | Module

Language-related experience:

* Devised own systems language developed since eary 80s, self-hosting since 1982
* Devised own scripting language developed since mid 80s (the two now share syntax and are converging)
* Written compilers targeting PDP10 (at college); Z80; 8086; x86; x64 (16/32/64 bit generations)
* Written interpreters for all x86 generations (also ported to ARM, but since that involved targeting C, that doesn't count).
* Written standalone assemblers for Z80; 8035/51 (forget which); 80186; 8086; x64 (x86 one was inline to HLL).
* Written linkers and text editors.

