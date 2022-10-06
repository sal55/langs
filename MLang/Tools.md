## Language Tools

This documents how the tools are used when run from a command line.

Each tool is a self-contained executable file. It can be installed and run from anywhere.

Most take a single file as input, and, if there is output, will write a single file too. Usually no file extensions are needed, but can be added for clarity.

### M Compiler `mm.exe` for Windows

For all examples, `prog.m` is the lead (or only) module of the application; `prog.ma` is the amalgamated sources of the application.

    mm      prog        Compile to prog.exe (assumes -exe option)
    mm -exe prog        Compile to prog.exe
    mm -asm prog        Compile to prog.asm
    mm -mx  prog        Compile to prog.mx (private exe/dll formats)
    mm -ml  prog        Compile to prog.ml and prog_exp.m (prog.q also planned)
    mm -dll prog        Compile to prog.dll (temporarily suspended)
    mm      prog.ma     Compile amalgamation prog.ma to prog.exe (above options can be used too)

    mm -run prog ...    Compile in-memory then run immediately (... represents any params of target program)

    mm -ma  prog        Create prog.ma amalgamation of all modules and support files, but not standard library files
    mm -mas prog        Same, but include standard library files
    mm -docs prog       (Auxialary option) Write prog.txt contains functions with docstrings (WIP)

Other options:

    -out:file           Write output to file.exe, file.ma etc (or supply extension)
    -outpath:path       Write output file in given folder path, if not using -out
    -nosys              Do not include standard libraries
    -opt/-opt2          Optimisation (works only on M5 compiler; temporarily suspected on current M6)
    -opt1
    -help               Show selection of options
    -v/-vv              More verbose messages
    -q                  Less verbose messages
    -time               Report internal compilation time and LPS throughput
    @file               Read options/inputs from given file

(Options used in debugging the compiler not shown here; they are shown in the source code only)

### M Compiler `mc.exe` for Windows

Generates C source code, or can go further and invoke a C compiler to produce an EXE. Special options when different from `mm.exe`:

    mc      prog        Compile to prog.c, then invoke gcc on prog.c to produce prog.exe
    mc -opt prog        Same, but use gcc -O3
    mc -tcc prog        Compile to prog.c, then invoke tcc on prog.c to produce prog.exe
    mc -c   prog        Compile to prog.c, do not invoke C compiler

(tcc is the recommended companion C compiler, but if `-opt` is needed, which is the main reason to use `mc`, then only `gcc` supports that.)

A single C-source file is produced for an entire application. This can be compiled pretty much like hello.c.

The reasons for a C version are:

* Allow optimising compilers like gcc, to get the fastest production versions of M applications
* Allow source code to be shared or built by other people who don't want to rely on my `mm.exe` binary
* Using `mu -c` (when done on Windows), creates C versions that can be compiled and run on Linux
* When using `mu` on Linux, this is the only way to get the M compiler to run directly on Linux

### M Compiler `mu.exe` for Linux

Like `mc.exe`, but runs on Linux:

    ./mu      prog        Compile to prog.c, then invoke gcc on prog.c to produce `prog` (Linux executable)
    ./mu -opt prog        Same, but use gcc -O3
    ./mu -tcc prog        Compile to prog.c, then invoke tcc on prog.c to produce `prog`
    ./mu -c   prog        Compile to prog.c

### M Compiler `ms.exe` or Windows

This runs just like `mm.exe`, but defaults to the `-run` option. No separate version is provided; just copy mm.exe to ms.exe:
````
    copy mm.exe ms.exe
    ms hello              Should compile hello.m into memory and run it immediately
````
The compiler checks whether the name of the executable is `ms` or not.

### Assembler aa.exe

This was created to replace unreliable and slow third party assemblers and linkers. Initially it generated OBJ files (still requiring a linker to produce an EXE), later it generated EXE directly.

Eventually, the backend was directly incorporated into the M compiler. But the AA assembler is still used for:

* Development of a compiler, since it is much easier to debug when it produces ASM
* When an OBJ file is needed, which may be necessary to produce DLLs and work with object files from other languages
* My C compiler


Input is usually a single ASM file (from my whole-program compiler), but it can also cope with multiple ASM files. This is still necessary for my C compiler as that uses independent compilation.

    aa      prog            Assemble prog.asm to prog.exe (assumes -exe option)
    aa -exe prog            Same thing
    aa -obj prog            Assemble to prog.obj (not used for a while so may need checking)
    aa -dll prog            Assemble to prog.dll (temporarily suspended)
    aa      one two three   Assemble one.asm, two.asm, three.asm to one.exe
    aa      prog lib        Assemble prog.asm to prog.exe, with lib.dll used for symbol imports


Other options:

    -out:file               Use file.exe etc (or supply the extension)
    -help                   Show selection of options
    -v                      More verbose messages
    -q                      Less verbose messages
    @file                   Read options/inputs from given file


### Q Compiler/Interpreter qq.exe

This compiles Q programs to in-memory bytecode and runs it immediately; here `prog` means `prog.q`, the lead module of the application:

    qq       prog          Compile and run program headed by prog.q; uses default -asm option
    qq -asm  prog          (Default) use ASM-base dispatcher with some extra bytecodes
    qq -asm2 prog          Same thing
    qq -asm1 prog          Use ASM-base dispatcher (no extra bytecodes)
    qq -fn   prog          Use slower HLL-only dispatcher (function-table-based)
    qq       prog.qa       Compile and run amalgamation prog.q; the lead module inside must be prog.q

    qq -qa   prog          Create prog.qa amalgamation of all modules and support files, but not standard library files
    qq -qas  prog          Same, but include standard library files

Other options:

    -nosys                 Don't include standard library

#### Using Amalgamated Source Files

The `-ma` or `-qa` options on both MM and QQ take a normal application of multiple modules and support files that may exist in different folders, and represent then in a single `.ma` or `.qa` source file. This then allows:

* Easy distribution: upload, download, email
* Easy backup
* For Q programs, which no longer compile to monolithic binary bytecode files, apps must be distributed as sources. This feature allows any app to be tidily packaged into a single file. (Further bundling the .qa files into a copy of the one-file interpreter is a possibility.)
* MM can directly compile apps from .ma files, and QQ can directly run from .qa files too (this would be a problem using ZIP)

Such files do not include the language's standard libraries, which are expected to be part of mm.exe or qq.exe. Those can be included using `-mas` or `-qas` instead. That also ensures the correct version of the library will be used.

There are some restrictions:

* Any directory structure in the application sources is flattened; this means all file names must be unique
* Embedded binary files currently can't be included, as .ma/.qa is text format
* Any support files (ie. using `include` or `strinclude`) cannot includes lines starting with "=== " since this is used as a file separator. (I can fix this, but it means a more elaborate file structure with separate directory info.)

Example of use:
```
    c:\mx>mm -ma mm
    Compiling mm.m---------- to mm.ma
    Writing  mm.ma
```
That created a single 37Kloc/0.7MB file containing the compiler sources. This can now be easily copied and compiled:
````
c:\demo>copy \mx\mm.ma
    1 file(s) copied.
c:\demo>mm mm.ma
Compiling mm.ma--------- to mm.exe
````

### ML and MX Files

These are a private binary format that can be used to replace EXE and DLL files (and developed mainly because of issues with the latter).

But their use is still being explored. Aspects of them are used to enable the `-run` option of the M compiler.
