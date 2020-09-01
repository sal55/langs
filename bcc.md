## BCC 'C' Compiler

This was an experimental compiler for a subset of C created at the start of 2017.

It has since been abandoned as a serious compiler that can deal with any program thrown at it. I realised that would be years of effort. But it still has some uses shown below.

It was started before the existing Tiny C compiler was upgraded with a more complete C99 implementation and better code.

### Aims

To see if a C implementation can be created that:

* Can be presented as the simplest possible kind of open source project: a single file. \[A single-file C rendering is possible; example in [bcc64.c](https://raw.githubusercontent.com/sal55/langs/master/bcc64.c)\]

* Can be built effortlessly from source, as simply as compiling a Hello, World program, and as fast. \[Build as, eg. 'gcc bcc64.c -obcc.exe'; with tcc this builds in 1/8th second, and with bcc, 1/5th second\]

* Can be downloaded, copied, run as a single, self-contained executable with no dependencies, other than what is provided by the OS. \[Entire implementation is in the file bcc.exe, including standard headers and facilities for assembling and linking\]

* Can be used without any libraries of its own (uses msvcrt.dll or msvcr*.dll versions)

* Can be extremely fast (despite largely using simple, linear data structures). \[Currently about half the speed of Tiny C. It's still fast.\]

* Can generate reasonable quality code even with no optimiser. \[Currently, on a par with Tiny C, perhaps a bit faster\]

* Can result in a small implementation. \[Tiny C has an executable smaller than 0.2MB; bcc.exe is 0.6MB, however 1/4 of that is bundled headers. Tiny C's headers are about 1.2MB, but are far more complete than bcc's.\]

### Main Specs

* Compile multiple C source files to single .exe file (necessary assembly and linking features incorporated)

* Targets Windows/x64 only

* Supports large subset of C; main omissions listed below

* The bcc64.c example is an OS-neutral version, builds on Windows or Linux. On Linux, -e, -s and -c options can be used to generate .i (preprocessed), .asm and .obj files per input module. Windows is needed for .exe output.

### Omissions

* VLAs and variable types
* Designated Initialisers
* Compound literals
* Many restrictions on initialisation data
* Full treatment of Bools and bitfields 
* Full API compliance (this means that callback functions, those called from code not compiled with bcc, need CALLBACK or #pragma $callback attribute)
* Standard headers incomplete; only a skeleton windows.h provided
* Some C features deliberately not implemented (eg. int/long are not distinct; only 2 char types instead of 3; {...} data initialisers need to exactly match the shape of the type; and miscellaneous quirks)

### Uses, mostly Experimental

* Trying out new features
* Converting interfaces in C headers to a format suitable for my own languages (partly done, needs to be finished by hand)
* Translating C source into my syntax (for viewing only, as work incomplete and semantics are different)
* Trying out fast compile techniques and new code generators
* Making sense of C type declarations (diagnostics show types in readable syntax)
* Second opinion when compiling C code (bcc is considerably less lax than compilers like gcc)
* Systems programming in a language that looks like C.