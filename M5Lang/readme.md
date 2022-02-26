## 'M' Language

**M** is my new 2022 language, a hybrid that combines and replaces my systems and scripting languages.

It is a 3-in-one language:

* Dynamic scripting language
* Static systems language
* Inline assembly language

Dynamic and static code can be mixed, in a manner I believe is called 'gradual typing'.

Applications, of any mix of stypes, can be run directly from source code just like a scripting language. Or they can be compiled to normal executables.

### History

M/2022 replaces both the older M (systems) and Q (scripting) languages evolved since early versions in the 1980s. These had gradually been converging anyway.

All have been used as personal or in-house languages during that time. Actually, I have used only my own languages for nearly all my programming. (I did try to give C a go but I found it dire.)

### Characteristics

* Case insensitive
* Naturally 1-based, also N-based
* Algol/Pascal/Ada-style syntax (basically, no braces nor significant indentation)
* Self-contained one-file implementation, typically 0.5MB to 1.0MB
* Very fast compilation, at least 0.5M lines per second and generating 5MB of code per second
* Targetting Windows 64
* No build system needed
* Can run applications from source
* Accessible language simple enough for anyone to understand

The implementation:

* Self-hosted, 100% written in itself, in a chain going back to the original version in early 80s
* Always bootstrapped using the previous version (original was written using *my* assembler on a home-made machine)
* Currently builds itself from source in about 0.1 seconds (some 40Kloc)

Dependencies:

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

### Availability

While executables exist to try out, I can't do the huge amount of work and support needed for it to usable by other people, and there is a huge amount missing that people will expect from a quality product. For example, docs.

This is purely a personal tool. It is presented here to demonstrate how a language could be tidily packaged, and what can be achievable compared with tools such as LLVM (yes you can get close to LLVM's code, while being 100 times smaller and 100 times faster to compile).

There may also be interesting ideas for people to take away.

So I'm not 'selling' my language or its tools. (Think of it as a custom-made car, or a tasty recipe; they don't mean I want to start manufacturing automobiles or go into mass production of ready-meals.)

### Products

Tool | Written In | Description
--- | --- | ---
**mm.exe**  | M | Run M app from source
**mc.exe** | M | Build M app to EXE, MX/ML or ASM
**aa.exe** | M |Assemble ASM to EXE or OBJ (and hence to DLL)
**run.exe** | M | Run MX files (see below); this is a stub file

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

### Examples

Informal, dynamic style:
```
    fun fib(n) =
        if n<3 then
            1
        else
            fib(n-1)+fib(n-2)
        fi
    end
```
Static style:
```
    function fib(int n) =
        if n<3 then
            1
        else
            fib(n-1)+fib(n-2)
        fi
    end
```

### Features

TBD
