## BCC 'C' Compiler

This was an experimental compiler for a subset of C created at the start of 2017.

It has since been abandoned as a serious compiler that can deal with any program thrown at it. I realised that would be years of effort. But it still has some uses shown below.

It was started before the existing Tiny C compiler was upgraded with a more complete C99 implementation and better code.

### Aims

To see if a C implementation can be created that:

* Can be presented as the simplest possible kind of open source project: a single file. \[A single-file C rendering had been possible, and was used for sharing the project. A single-file amalgamation is still possible, but in the original language.\]

* Can be built effortlessly from source, as simply as compiling a Hello, World program, and as fast. \[Build C rendering as, eg. 'tcc bcc.c'; with tcc this builds in 1/8th second, and with bcc, 1/5th second; In the original language and original modules, it's built as 'mm cc', in also 0.2 seconds.\]

* Can be a single, self-contained executable file with no dependencies, other than what is provided by the OS. \[Entire implementation is in the file bcc.exe, including standard headers and facilities for assembling and linking\]

* Can be used without any libraries of its own (uses msvcrt.dll library that comes with Windows)

* Can be extremely fast (despite largely using simple, linear data structures). \[Currently about half the speed of Tiny C. It's still fast, although handicapped by having to use intermediate ASM code\]

* Can generate reasonable quality code even with no optimiser. \[Currently, somewhat faster Tiny C's code, but not by much.\]

* Can result in a small implementation. \[Tiny C has an executable smaller than 0.2MB; bcc.exe is under 0.4MB, but includes std headers within bcc.exe which adds 0.6MB. Tiny C's headers are discrete files of about 1.2MB, but are far more complete than bcc's.\]

### Main Specs

* Compile multiple C source files to single .exe file (necessary assembly and linking features incorporated)

* Targets Windows/x64 only

* Supports large subset of C; main omissions listed below

* Not written in C, but in my own systems language. Versions of that could target C, and that is how the one-file C 'rendering' was created. True sources comprise about 25 non-library modules (they are listed in bcc.c).

### Downloads

Not recommended for real work. There were some links but not recently updated. I may come back to the project in late 2021, with a revised backend that fixes some of the omissions below.

### Main Omissions

* VLAs and variable types
* Designated Initialisers
* Compound literals
* Many restrictions on initialisation data
* Full treatment of Bools and bitfields 
* Full API compliance (this means that callback functions, those called from code not compiled with bcc, need CALLBACK or #pragma $callback attribute)
* Other restrictions exist for some type of parameter passing, mostly involving structs
* Standard headers incomplete; only a minimal windows.h provided, although it accounts for 0.5MB of the executable size.
* Some C features deliberately not implemented (eg. int/long are not distinct; only 2 char types instead of 3; {...} data initialisers need to exactly match the shape of the type; and miscellaneous quirks)

### Uses, mostly Experimental

* Trying out new features
* Converting interfaces in C headers to a format suitable for my own languages (partly done, needs to be finished by hand)
* Making use of its built-in preprocessor
* Translating C source into my syntax (for viewing only, as work incomplete and semantics are different)
* Trying out fast compile techniques and new code generators
* Making sense of C type declarations (diagnostics show types in readable syntax)
* Second opinion when compiling C code (bcc is considerably less lax than compilers like gcc in some areas)
* Can be used to see how C maps to native code, and much more clearly and simply than gcc (use -s/-S option)
* For learning more about C. (An awful lot, mostly not favourable, has already been learnt. I didn't like C before starting it, and liked it less afterwards, but admired existing C compilers considerably more for being able to deal with 50 years' accumulation of billions of lines of some terrible C code.)


