## 'M' Programming Language

Lower level systems programming language long used as a tidier alternative to C.

Current implementation is for 64-bit Windows only.

Summary of [features](../mfeatures.md)

**Other links below may not work**

### Summary

* M is a systems language at roughly the level of C, and can be used for the same sorts of tasks
* It uses an entirely different syntax from C, and has some modern features, such as proper modules
* The compiler for M is the single, self-contained file `mm.exe`, about 0.6MB.
* M is designed for whole-program compilation

### Status

Currently I use this as a private language, mainly used for language programs, including the interpreter for my scripting language which is what I prefer for applications.

While anyone is welcome to play with it, or borrow any useful-looking features, it is not supported for general use: docs are patchy, coverage (of combinations of features, operations and types) is not 100%, and it is buggy.

It's also volatile, in the sense that I have a tendency to make breaking changes.

A binary for the compiler (`mm.exe`) is available in this folder, and combined sources are available for perusal in `mm.qa`.

### Building from Source

M is self-hosted so it will need an M compiler to build. If happy downloading a binary (and can cope with the attentions of AV software), download the files mm.exe and mm.qa (right-click on the links). You can build a second version from source code using:
```
    mm mm.ma -out:mm2
```
But if running mm.exe is OK, then no need to do that except as a test or demonstration that it works.

(I may make available a version supplied as two programs: `mm.mx`, a binary in a special format, and a loader `run.c`, a 600-line program which when compiled locally can then run the compiler like this: `run mm hello`. This ought to have fewer AV issues. However if you are a developer, then you will already have needed to deal with AV for your own projects.

This approach makes possible compiling *some* programs to run on x64 Linux, without needing to use Linux ABIs or write ELF formats; this is something I'm looking into.)

### Example Programs

Some small programs are here: [Examples](../Examples)

### Building Executables

You will need the compiler mm.exe. Given an example program such as hello.m, build as follows:

    mm hello

This compiles hello.m into hello.exe. Programs can also be run from source without creating an executable:

    mm -run hello

For a larger program comprising multiple modules, only the lead module (the one containing the `start()` entry point function) is needed; if the lead module is prog.m:

    mm prog

then this will locate and compile *all* sources into prog.exe (mm is a pretty fast compiler). For other options use:

    mm -help
 
 or see the Options file above.

