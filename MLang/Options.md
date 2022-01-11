## M Compiler Options

### Main Options

Option | Description (Note)
--- | ---
**-exe** | (Default) Compile to .exe binary
**-mx** | Compile to .mx binary (main program)
**-ml** | Compile to .ml binary (dynamic library)
**-run** | Compile to in-memory 'mx' format and execute
--- | ---
**-pcl** | Compile to IL source code in .pcl file
**-asm** | Compile to ASM source code in .asm file (1)
--- | ---
**-ma** | Combine all input files into single .ma source file
**-mas** | Like `-ma` but include also library modules (2)
--- | ---
**-docs** | Write out exported functions with docs to .txt file
**-export** |
**-lib** | Set library mode as used with `-ml` (3)
--- | ---
**-out:file** | Set name of output file (4)
**-outpath:path** | Set path containing output file (5)
**-ext** | Don't use M library files inside mm.exe (6)
--- | ---
**-opt1** | Store some locals in registers
**-opt2 -opt** | Also do some peephole optimisation
--- | ---
**-help -h** | Display some of these options
--- | ---
**-q** | Quiet mode
**-v** | More verbose
**-unused** | List declared functions/variables that are not used (7)

**Notes**

(1) ASM format for x64, while Intel-based, used my own syntax and tends to use my own register designations. It can be used to examine the generated code, but to assembler requires my `aa.exe` assembler.

(2) mm.exe usually compiles the standard library sources as part of every program. These are excluded in the .ma file produced with `-ma`, since the sources are usually part of mm.exe. Using `-mas` includes those sources too. Then it doesn't matter if not part of mm.exe, or if a later mm.exe has incompatible versions.

(3) The `-ml` option sets library mode anyway. But because this mode affects the generated code in small ways, using `-lib` means this can be seen when `-pcl` and `-asm`.

(4) You don't need to specify the file extension unless overriding, as this is taken from default output file. In most cases, the default output file is taken from the name of the input file, with the appropriate extension

(5) While `-out` can specify a different path for the output file, you'd have to give the filename too. `-outpath` allows you to specify just the path. It doesn't need to end with `\` or `/`.

(6) With `-ext`, it ignores the library files inside mm.exe, and looks elsewhere. The default path is the compiler development path (which is only relevant on my machine), but can be set with Header directives.

(7) This is a work in progress. The reflective features, which store a list of all functions, make it hard to determine if a function is really unused, as it may be accessed via that list.



### Development Options

A selection of options used for development, that could be useful elsewhere too.


Option | Description (Note)
--- | ---
**-nosys** | Don't include standard library (1)
**-minsys** | Use minimal standard library
**-sys** | (Default) Include standard library
**-norts** | Don't include PCL's runtime library (2)

