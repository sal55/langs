## mm.exe M Compiler

Input and Output files of the M compiler.

Each output file represents an entire program of this whole-program compiler.

### Input Files

* **.m** M source file for one module
* **.ma** All source and support files for a whole 'program' (ie. a single .exe, .mx or .ml file)

A .m source file can be a Header file (lists files comprising the project), or a Module file (contains the usual program code), or can contain both.

(Commands to the M can also be put into an '@' file, and submitted using `mm @file`, but that doesn't count.)

### Output Files (Binary)

* **.exe** PE+ format, Windows executable file (.dll no longer supported)
* **.mx** Private executable format
* **.ml** Private dynamic library format

**.obj** files (COFF64) are only generated from my **aa.exe** assembler. For that, choose **.asm** output option.

**.dll** files can be generated using a third party linker, from a **.obj** files from **aa.exe**.

### Output Files (Source Code or Docs)

* **.pcl** PCL Intermediate Language source files. This can is for browsing, or can be processed with **pc.exe** which itself has most of the same output options as the compiler. At the moment, PCL does not support inline ASM code from the compiler. PCL files with ASM instructions can be produced, but **pc.exe** cannot process them.
* **.asm** ASM x64 source code. This is in my own format and needs my **aa.exe** assembler to process further. Or it can be just for perusal, or debugging.
* **.m** This will be an interface/exports module produced as a by-product of the **-ml** option when applied to a library. That file can be imported into an M program, allowing it to use the library without needing to write manual declarations.
* **.ma** This combines all the **.m** and other support files (used with **include stdinclude bininclude**) in a single text file. This format is convenient to distribute.
* **.txt** Not quite ready, this produces a listing of functions that have doc-strings, together with those strings
* **.q** Similar to **.m** when compiling a library, this produces an interface/exports module for the companion Q scripting language




