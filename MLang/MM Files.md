## mm.exe M Compiler

Input and Output files of the M compiler.

### A 'Program' Unit

A Program in the context used here means a program normally represented by a single .exe executable on Windows, or a library normally contained within a .dll shared library.

Both my M and Q compilers are whole-program compilers, compiling all sources at the same time. And all output files, including .mx, .ml and .obj, also represent the the whole program.

### Input Files

Extension | Description
---  | ---
**.m** | M source file for one module
**.ma** | All source and support files for a whole program

A .m source file can be a Header file (lists files comprising the project), or a Module file (contains the usual program code), or can contain both.

### Output Files (Binary)

Extension | Description
---  | ---
**.exe** | Windows binary executable file (PE+ format)
**.mx** | Private executable file (which is also portable across OSes)
**.ml** | Private dynamic library file

**.obj** files (COFF64) are only generated from my **aa.exe** assembler. For that, choose **.asm** output option.

**.dll** generation is no longer directly support as the output is buggy. **.dll** files can be generated using a third party linker, from **.obj** files from **aa.exe**.

### Output Files (Source Code or Docs)

Extension | Description
---  | ---
**.pcl** | PCL Intermediate Langauge source file
**.asm** | x64 assembly source file
**.ma** | Combines all .m and other support files into a single source file
**.m** | An interface/exports file generated at same time as .ml library
**.q** | An interface/exports file for Q generated at same time as .ml library
**.txt** | Listing of function signatures together doc-strings


