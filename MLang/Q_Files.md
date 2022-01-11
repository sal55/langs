## Q Intepreter Files

Input and Output files of the Q interpreter `qq.exe`.

### A 'Program' Unit

This is similar to that of the M language: while there are no generated binaries, a Program is defined by a collection Q source modules and support files, which must all be compiled ahead of time into bytecode, before execution commences.


### Input Files

These work the same way as the M compiler:

Extension | Description
---  | ---
**.q** | Q source file for one module
**.qa** | All source and support files for a whole program

A .q source file can be a Header file (lists files comprising the project), or a Module file (contains the usual program code), or can contain both.

.qa files are always generated automatically from the compiler (ie. the compiler front-end of the interpreter) using the -ma option, to produce a convenient single-file source representation.

### Output Files

Extension | Description
---  | ---
**.qa** | Combines all . and other support files into a single source file
**.txt** | Listing of function signatures together doc-strings


