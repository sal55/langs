## 'M5' Language

**M** is my new 2022 language, a hybrid that combines and replaces my systems and scripting languages.

It is a 3-in-one language:

* Dynamic scripting language
* Static systems language
* Inline assembly language

Dynamic and static code can be mixed, in a manner I believe is called 'gradual typing'.

Applications, of any mix of stypes, can be run directly from source code just like a scripting language. Or they can be compiled to normal executables.

I call it M5 for now to distinguish it from the older systems-only language also called **M**, or sometimes M4 to avoid confusion.

### History

The first M systems language was 40 years ago, running on 8-bit Z80 microprocessors. It has changed significantly since then.

The first Q scripting language was in the form of an add-on language for my GUI 3D graphics applications, from the later part from the 80s, which run on IBM PCs.

M has been self-hosted via a chain of versions going back decades, possibly as long as 40 years. The first experimental version was written in ASM. I also wrote that assembler in hex machine code (and actually wrote the hex editor in binary, and even built that first machine).

But the chain was broken early on as it must have been rebooted at least once, from ASM; I can't remember.

Regardless, I have used no other languages than my own for nearly all my programming. I have tried to switch to C a few times, but I just couldn't hack it.

### How High Level is M5?

Not very. The static side of M5 is a little above C in level. The dynamic part is a long way below Python, and is not that dynamic either.

There are few fancy features that most people now expect:

* Most of them I don't understand or can't use
* Some I do appreciate, but are too hard to implement
* Some I could implement, but I would not use them enough to make it worthwhile
* A few might be interesting box-ticking exercises.

In particular, the type system is as basic and unesoteric as I could make it. 

### Any Interesting Features

Special features that I find useful are listed elsewhere. But the characteristics of my languages have long been:

* Case insensitive
* Naturally 1-based, also N-based
* Self-contained one-file implementations, typically 0.5MB to 1.0MB
* Very fast compilation, at 0.5M lines per second and generating 5MB of code per second
* Targetting Windows 64
* No build system needed
* Can run from source (a recent feature)
* Accessible language simple enough for anyone to understand
* Builds itself from source in about 0.1 seconds (some 40Kloc)

### Products

Tool | Description
--- | ---
**mm.exe**  | Run M5 app from source
**mc.exe** | Build M5 app to EXE, MX/ML or ASM
**aa.exe** | Assemble ASM to EXE or OBJ (and hence to DLL)
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

### Can Anyone Use It?

That is not practical as it would be a huge amount of work to properly support such a product.

It just wouldn't have been used extensively enough to ensure decent coverage of all the combinations of features, and to find all the bugs, design flaws and ambiguities. It's also still volatile, and aimed at my own preferences rather than general use. That has always been the case since it was an in-house tool.

But the ideas in the language, the way it is packaged and the effortless way it is designed to work, are all things that can be taken away from this project.

Another might be in seeing how well a small project with a crude code generator can generate code compared with massive toolchains like LLVM. Yes you can get within 50% of gcc-O3 without that much effort! (Depending on application...)

Executables also exist for experimentation. As do source-code snapshots for perusal.

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

