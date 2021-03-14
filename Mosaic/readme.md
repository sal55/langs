## Mosaic Programming Language

Low level language long used as a tidier alternative to C.

Current implementation is for 64-bit Windows only.

Summary of [features](../mfeatures.md)

### Status

(While anyone is welcome to play with it, borrow features etc, at the moment it's more of a private project since I'm not in a position do any support. Health reasons etc.)

### Download Binaries

Available from www.bcas.freeuk.com/bb.exe

(Problems have been reported downloading this via Opera (not a valid executable). Possibly 'bb' has some special meaning to Opera. If so try via www.bcas.freeuk.com/compilers.zip.)

This is the actual compiler, not an installer. Simplest way to download possibly to right-click on the link, and select 'Save Link As' (depends on browser), and specify a location to put it in.

This 0.6MB executable is completely self-contained with no support files required.

Because it is likely to exist outside of normal application areas (C:\"Program Files" etc) some AV settings may need to be adjusted to allow it to run unimpeded. But probably the same as already needed if you are a developer creating executables now.

### Download Sources

Since Mosaic is written in itself, you can't build from sources without first using the binary above.

However the sources are here: [sources](../sources), in the file bb.ma, which is a single file encapsulation of all the modules. This can be built directly using, for example:

    bb bb.ma -out:bb2.exe

This needs to take care not to overwrite the existing bb.exe which Windows doesn't allow anyway, hence the -out option.

To extract the individual files, build this program with bb: [extract](../Examples/extract.m), which will write the separate files into ./sources

(Note: current amalgamations have stripped comments so are for viewing only.)

### Example Programs

Some small programs are here: [Examples](../Examples)

### Building Executables

You will need the compiler bb.exe. Given an example program such as hello.m, build as follows:

    bb hello

This compiles hello.m into hello.exe. For a larger program comprising multiple modules, only the lead module (the one containing the start() entry point function) is needed; if the lead module is prog.m:

    bb prog

then this will locate and compile *all* sources into prog.exe (bb is a pretty fast compiler). For other options use:

    bb -help

### Other Applications

Mosaic has been used with certain C libraries, for example:

https://github.com/pluckyporcupine/raylib-mosaic

This builds a set of bindings around existing libraries that use a C API.

