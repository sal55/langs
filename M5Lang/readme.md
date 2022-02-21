## 'M5' Language

* The new language will replace both my current 'M' statically typed, native-code compiled systems language ...
* ... and my 'Q' dynamically types, bytecode interpreted scripting language
* Programs can mixed strict static typing, with dynamic typing
* They can also mix static coding style (eg. declare everything) with informal scripting style (declarations optional)
* Any program can directly from source, with no intermediate binary file), just like a scripting language
* Any program, even using 100% dynamic typing, can be compiled into a standalone executable
* As usual, the implementation will be a single, self-contained binary; a compiler between 0.5MB and 1.0MB
* As usual, this is a whole-program compiler, with a throughput of at least 0.5M files per second
* Performance of 100% static code will be typically 50% slower than equivalent C code compiled with gcc-O3 (for typical applications that I would write)
* Performance of 100% dynamic code is expected to be at least as good as the non-accelerated (ie. 100% HLL) Q interpreter; which is also typically brisker than non-JITed Python and Lua. (Not however as good as accelerated Q)

'M' has evolved since around 1982, and 'Q' since about 1990, with many versions, changes of names, different targets and increasing capabilities.

Although they had been slowly converging, this is still a significant departure after decades of maintaing two languages. Some attempts have been made to combine before, but it never really work. But now it ~~should~~ will.

### Status

A version of M5, with some of the changes necessary to make the above possible, is working with a static-only type system. It is my production language and compiler. A few more weeks should yield a language with dynamic capabilities and able to be used for scripting.

### Products

Tool | Description
--- | ---
**mm.exe**  | Run M5 app from source
**mc.exe** | Build M5 app to executable
**aa.exe** | Assemble ASM to executable
**run.exe** | Run MX files (see below); this is a stub file

`mm.exe` and `mc.exe` are actually identical binaries. The executable name is used to determine the default option (-run or -exe).

### Inputs

For an example application 'prog':

Input | Description
--- | ---
prog.m file | Lead module of application
prog.ma file | 'One-file' representation of application

The 'one-file' representation contains all source modules and support files (strinclude/include) of an application. It is generated from discrete files using the `-ma/-mas` options of `mc.exe`

### Outputs

Output File| `mc` Option | Description
--- | --- | ---
 (run) | `-run` | (Or use mm) No ouput; application is compiled/run from memory
 .exe | `exe` | Produce Windows PE+ executable file
 .ma | `-ma`, `-mas` | Make one-file representation (.mas includes std lib)
 .mx | `-mx` | Produce private binary format (run with run.exe)
 .ml | `-ml` | Produce private shared library format (use from mx only)
 .exe | `-mexe` | Produce one .exe file that bundles run.exe+prog.mx
.asm | `-asm` | Produce .asm file for whole program; assemble with aa.exe
.pcl | `-pcl` | Produce .pcl IL representation (debugging only) 





    

