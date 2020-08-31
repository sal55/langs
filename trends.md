(Adjunct to my Reddit post.)

**Case sensitivity**



Feature | My Style | Popular | Notes
--------|--------|----------- | ----------------
**Case sensitive** | Case Insensitive | Case Sensitive
Example | abc, Abc, ABC are same name | abc, Abc, ABC are distinct | etc
**Array Lower Bounds** | N-based, default 1 | 0-based
Example | Use any of 1, 0 or N | Use 0 only
**Text (code) editors** | Use hard-line stops | Soft-line stops | Means Left/Right etc do not stop at line beg or end
**Compiler Strictness** | Unequivocal | Depends on options | Typical C compilers are very lax unless you twist their arm
**Compiler options** | <10 | Loads | (Clang has 800; gcc has 1000s)
**Need file extension** | Optional | Yes
Example: | bcc hello | gcc hello.c
**Default output file** | Based on first input file | Varies | gcc generates a.out or a.exe
Example | bcc hello => hello.exe
**Linker needed** | Designed out | Yes
Exception | Linker can be used with .obj outputs
**Tool deployment** | Self-contained binary file | Typically multi-file, multi-directory
**Make files** | Not used | Everything uses make files |Make files introduce complications and dependencies that rarely work on Windows
Example | All done with the compiler
**Binary distribution** | Typically one executable file | Various
**Installation** | None needed, use binary as | Usually complex
**Source distribution** | One .c file rendering or one .ma file | Complex
**Building from source** | Compile the one file directly | Typically makefiles|  
**Macros** | Hardly ever used | In C, hardly ever not used |
**Assemblers** | Always simple | Always complicated
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


(My background:
CS degree

Built own 8-bit machine around microprocessor, starting from binary machine code through to primitive HLL that was used for 3D graphics and image processing.

Become an electronics engineer designing microprocessor-based 8 & 16-bit computers and video boards; wrote low-end CAD programs for 8-bit machines then PCs.

Devised a systems language 'M', evolved from first Z80 version; and a script language (various) evolved from x86 version.

Written compilers targeting PDP10 (at college); Z80; 8086; x86; x64 (16/32/64 bit generations). My 'M' compiler has been self-hosting (AFAICR) since the Z80 version.

Written interpreters for all x86 generations (also ported to ARM, but since that involved targeting C, that doesn't count).

Written standalone assemblers for Z80; 8035/51 (forget which); 80186; 8086; x64 (x86 one was inline to HLL).

Written linkers and text editors.)

