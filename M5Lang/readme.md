## 'M5' Language

This new language is a, for me, ambitious project that is intended to replace these two languages:

**M** Lower level systems programming language; evolving since about 1982; statically typed; compiles to native code

**Q** Lower level scripting languages; evolving since about 1990; dynamically typed; runs as interpreted bytecode

The new language will be **M**, but is M5 for now to distinguish it from the current M language.

Actually, M5 in the form of M5S - static only code - is already in use and has taken over the used of M. It is the enhanced compiled and runtime for M5, written using M5S subset, that is what is under development to form the hybrid language.


### Aims

* The new language will replace both M and Q languages as stated
* Programs can mixed strict static typing, with dynamic typing
* They can also mix static coding style (eg. declare everything) with informal scripting style (declarations optional)
* Any program can directly from source, with no intermediate binary file), just like a scripting language
* Any program, even using 100% dynamic typing, can be compiled into a standalone executable
* As usual, the implementation will be a single, self-contained binary; a compiler between 0.5MB and 1.0MB
* As usual, this is a whole-program compiler, with a throughput of at least 0.5M files per second
* Performance of 100% static code will be typically 50% slower than equivalent C code compiled with gcc-O3 (for typical applications that I would write)
* Performance of 100% dynamic code is expected to be at least as good as the non-accelerated (ie. 100% HLL) Q interpreter; which is also typically brisker than non-JITed Python and Lua. (Not however as good as accelerated Q)

### Products
I'm likely to end up using two forms of the M5 compiler:
```
    mm prog            # Compile and run the application from source; no intermediate binaries produced
    mc prog            # Compile the application to prog.exe
```
mm.exe and mc.exe are actually identical binaries; the name of the executable is used to set the default option. The complete set of language tools will be:

Tool | Description
--- | ---
**mm.exe**  | Run app from source
**mc.exe** | Build to executable
**aa.exe** | Assembler ASM to executable
**run.exe** | Run MX files (see below); this is a stub file, intended to be used behind the scenes only

### Inputs

For an example application 'prog':

Input | Description
--- | ---
prog.m file | Lead module of application
prog.ma file | 'One-file' representation of application

The 'one-file' representation contains all source modules and support files (strinclude/include) of an application. It is generated from discrete files using the -ma/-mas options of mc.exe

### Outputs

Output File| Option | Description
--- | ---
<run> | mc -run, or mm | No ouput; application is compiled/run from memory
 .exe | mm -exe | Produce Windows PE+ executable file
 .ma | mm -ma, mm -mas | Make one-file representation (.mas includes std lib)
 .mx | mm -mx | Produce private binary format (run with run.exe)
 .ml | mm -ml | Produce private shared library format (use from mx only)
 .mexe | mm -mexe | Produce one .exe file that bundles run.exe+prog.mx
.asm | mm -asm | Produce .asm file for whole program; assemble with aa.exe
.pcl | mm -pcl | Produce .pcl IL representation (debugging only) 





    

