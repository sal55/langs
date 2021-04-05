## Mosaic Programming Language

Low level language long used as a tidier alternative to C.

Current implementation is for 64-bit Windows only.

Summary of [features](../mfeatures.md)

### Status

(While anyone is welcome to play with it, borrow features etc, at the moment it's more of a private project since I'm not in a position do any support. Health reasons etc.)

* Download mm.exe from the link below. This is for Windows 64, but someone reported it as worked under Wine on Linux
* Or try compiling [mu.c](../mu.c), which is a one-file C rendering of an old version, targeting Linux. It works by generating C code, then invoking a C compiler. Type ./mu for options.

### Download Binaries

Available from www.bcas.freeuk.com/mm.exe

This is the actual compiler, not an installer. Simplest way to download possibly to right-click on the link, and select 'Save Link As' (depends on browser), and specify a location to put it in.

This 0.6MB executable is completely self-contained with no support files required.

Because it is likely to exist outside of normal application areas (C:\"Program Files" etc) some AV settings may need to be adjusted to allow it to run unimpeded. But probably the same as already needed if you are a developer creating executables now.

### Download Sources

Since Mosaic is written in itself, you can't build from sources without first using the binary above.

However the sources are here: [sources](../sources), in the file bb.ma, which is a single file encapsulation of all the modules. This can be built directly using, for example:

    mm bb.ma

This will generate bb.exe (so not overwriting mm.exe).

To extract the individual files, build this program with mm: [extract](../Examples/extract.m), which will write the separate files into ./sources


### Example Programs

Some small programs are here: [Examples](../Examples)

### Building Executables

You will need the compiler mm.exe. Given an example program such as hello.m, build as follows:

    mm hello

This compiles hello.m into hello.exe. For a larger program comprising multiple modules, only the lead module (the one containing the start() entry point function) is needed; if the lead module is prog.m:

    mm prog

then this will locate and compile *all* sources into prog.exe (mm is a pretty fast compiler). For other options use:

    mm -help

### Other Applications

Mosaic has been used with certain C libraries, for example:

https://github.com/pluckyporcupine/raylib-mosaic

This builds a set of bindings around existing libraries that use a C API.

