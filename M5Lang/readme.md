## 'M' Language

**M** is my new 2022 language, a hybrid that combines and replaces my systems and scripting languages.

It is a 3-in-one language:

* Dynamic scripting language
* Static systems language
* Inline assembly language

Dynamic and static code can be mixed, in a manner I believe is called 'gradual typing'.

Applications, of any mix of stypes, can be run directly from source code just like a scripting language. Or they can be compiled to normal executables.

It is also, for now, called **M5** to distinguish it from previous versions: **M4** (static-only lower level systems language) and **M3** using an older module scheme.

### History

M5 replaces both the older M (systems) and Q (scripting) languages evolved since early versions in the 1980s. These had gradually been converging anyway.

All have been used as personal or in-house languages during that time. Actually, I have used only my own languages for nearly all my programming. (I did try to give C a go but I found it dire.)

### Characteristics

* Case insensitive
* Naturally 1-based, also N-based
* Algol/Pascal/Ada-style syntax (basically, no braces nor significant indentation)
* Self-contained one-file implementation, typically 0.5MB to 1.0MB
* Whole-program compiler
* Very fast compilation, at least 0.5M lines per second and generating 5MB of code per second (on low-end AMD Ryzen 3)
* Targetting Windows 64
* No build system needed
* Can run applications from source
* Accessible language simple enough for anyone to understand
* 'One-File' principle (see below)

**The implementation:**

* Self-hosted, 100% written in itself, in a chain going back to the original version in early 80s
* Always bootstrapped using the previous version (original was written using *my* assembler on a home-made machine)
* Currently builds itself from source in about 0.1 seconds (some 40Kloc)

**Dependencies:**

* Compiler: none (minimal requirements are some CRT functions in msvcrt.dll, which is part of Windows)
* Applications: none, other than the external libraries a program may choose to make use of (eg. OpenGL)
* Other tools (eg. assembler, linker, building, script): none
* Other languages: none

A special tool is sometimes used to help convert C-style APIs into M bindings, but that tool (basically a C compiler) is written in M.

M covers the entire compiler 'stack'
 
### Comparisons With Other Languages

* Clunky 1980s style with few more modern or advanced features
* Static part is somewhat higher level than C
* Dynamic part is much lower level than Python, and not very dynamic either

Basically, it doesn't need to please anyone except me.

### 'One-File'

This is now a common theme in my languages:

* The implemention is a single executable file
* The output of compilation is always a single file (exe, asm, pcl, even C if and when that is a target again)
* The source code of a project can also a single text file

To demonstrate that last point, this creates a one-file version of the 76 files comprising my C compiler project:
```
    mm -mas cc
```
It produces a file cc.ma that is 50KLoc, which includes headers. `mas` means it includes std library files for M. This is convenient for backup or distribution, and can be compiled directly:
```
    mm cc.ma
```
Produces cc.exe, which is itself one-file C compiler.

### Availability

While executables exist to try out, this is a personal tool. A product suitable for others to use would be a huge amount of extra work and require considerable support.

It is presented here to demonstrate how a language can be tidily packaged, and what can be achievable in a smallish self-contained language with no dependencies and using the simplest algorithms. For example, while there is no proper optimiser to speak of, typical programs might only be 50% slower than gcc-O3.

There may also be interesting features or ideas for people to take away. So I'm not 'selling' my language or its tools.

### Products

Tool | Written In | Description
--- | --- | ---
**mm.exe**  | M | Run M app from source
**mc.exe** | M | Build M app to EXE, MX/ML or ASM
-- | -- | Auxiliary Tools
**aa.exe** | M | Assembler-linker (ASM -> EXE/DLL/OBJ)
**run.exe** | M | Run MX binaries

`mm.exe` and `mc.exe` are actually identical binaries (so still 'one file'!); . The executable name is used to determine the default option (-run or -exe).

### Inputs

Input is a single file name:
```
    mm prog          # run program whose lead module is prog.m
    mm prog.ma       # (or `prog` if no clash) run the one-file project prog.ma
```
`mm` will run the program by compiling to in-memory native code; `mc` will compile to EXE; see below.

### Outputs

Output File| `mc` Option | Description
--- | --- | ---
 (run) | `-run` | (Or use mm) No ouput; application is compiled/run from memory
 .exe | `exe` | Produce Windows PE+ executable file
 .ma | `-ma`, `-mas` | Make one-file representation (.mas includes std lib)
 .mx | `-mx` | Produce private binary format (run with run.exe)
 .ml | `-ml` | Produce private shared library format (use from mx only)
 .exe | `-mexe` | Produce one .exe file that bundles run.exe+prog.mx (not ready)
.asm | `-asm` | Produce .asm file for whole program; assemble with aa.exe
.pcl | `-pcl` | Produce .pcl IL representation (debugging only) 

### Examples

(Not ready)

However, Hello World is:
```
   println "Hello, World!"
```

### Features

While there are very few big, advanced features, there are lots of smaller ones that make everyday coding a pleasant experience. Feature summary:

(Not ready)
