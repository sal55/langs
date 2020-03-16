## Mosaic Programming Language

Low level language long used as a tidier alternative to C.

Current implementation is for 64-bit Windows only.

Summary of [features](../mfeatures.md)

### Download Binaries

Available from www.bcas.freeuk.com/mm.exe

This is the actual compiler, not an installer. Simplest way to download possibly to right-click on the link, and select 'Save Link As' (depends on browser), and specify a location to put it in.

This 0.6MB executable is completely self-contained with no support files required.

Because it is likely to exist outside of normal application areas (C:\"Program Files" etc) some AV settings may need to be adjusted to allow it to run unimpeded. But probably the same as already needed if you are a developer creating executables now.

### Download Sources

Since Mosaic is written in itself, you can't build from sources without first using the binary above. (At one time it was possible to create a one-file C version so that it could be built with a C compiler, but that is not currently available. Hopefully a C target can be made available again at some point.)

However the sources are here: [sources](../sources), in the file mm.ma, which is a single file encapsulation of all the modules. This can be build directly using, for example:

    mm mm.ma -out:mm2.exe

This needs to take care not to overwrite the existing mm.exe which Windows doesn't allow anyway, hence the -out option.

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

Mosaic is starting to be used with certain C libraries, for example:

https://github.com/pluckyporcupine/raylib-mosaic

This builds a set of bindings around existing libraries that use a C API.
