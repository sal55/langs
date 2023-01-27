('What should a compiler do')

...And then, I had a closer look at my own compiler (for my lower-level systems language), and I realised it could also go quite a way outside that remit. Other products may have their own 'extra-curricular' features, so this is purely what *my* compiler does, much of it untypical I think.

**It's a single file** Literally, it is the file `mm.exe` on Windows, currently some 440KB, which contains *everything* need to do its primary job. Most other products cannot be easily pinned down: `gcc.exe` is 1.2MB, but within a 1000MB installation of 10,000 files; which bits are the essential parts of the compiler?

What can I do with such a feature? For a start, if I see the file `mm.exe`, I know that everything is there. I can email someone that file, copy it to a pen drive etc. And run it from anywhere. Or, delete it and I know it's gone.

**It provides the standard library** My small standard library is a bunch of 5 modules which are compiled from source as part of the application. Where is that library? It's part of `mm.exe`, otherwise it would be only 370KB. (`mm.exe` without that library would require alternate access to those files; see `ma` files below)

**It's a Linker** Or rather, it doesn't need a conventional one, because this is a whole program compiler, all modules are known and compiled at the same time. External libraries are accessed dynamically in the form of DLL shared libraries. (You will say, `gcc` also links; no, it invokes the `ld` linker on a bunch of object files; I don't use those either.)

**It's a build system** No need for external tools and means to list the constituent modules or show their relationships. You submit only the lead module of the application: `mm prog` will discover all modules and support files used.

**It generates an EXE file** Finally, it's doing its advertised job! But doesn't every compiler do that? I think most do, although some work as drivers which invoke other programs; some primarily generate intermediate files such as object files or ASM files.

**It generates private library files** This one is unique to mine; I used to generate DLL shared library files as an option, but that was buggy. I devised by own 'ML' library format which is simpler and better than DLL, and more easily shares the host's environment. (I use this format from my dynamic language to run code in the static one)

**It writes interface files** When generating DLL files (it still can, just), or ML, it also writes an exports file for that library, which forms an import module that be directly used in any of my applications. (This could be extended to write that in other forms, eg. in C header syntax, but I haven't needed to yet.)

**It can generates docs** (Functions can have doc-strings attached, and the idea is that with a `-docs` option, all those functions are written out with their doc-strings. But not a feature I used much so is immature.)

**It's an Assembler** The language has an in-line assembler, so you could write programs primarily in assembly, while benefiting from HLL features (functions, scope rules, declarations, module imports). Output can be EXE. (I have a separate assembler `aa.exe`, but it lacks HLL conveniences.)

**It can create amalgamated source files** The `-ma` or `-mas` options take all module sources and support files, and combines (and flattens) them into a single `.ma` text file. This can be directly built in that form: `mm prog.ma`. Such files are convenient for uploading, emailing etc. (The `-mas` option includes also standard library sources, ensuring the correct version, but it also means the leaner `mm.exe` could be used.)

**It can run the programs it compiles!** This is exactly the thing I said a compiler shouldn't do, but here the demarcation is still there: it compiles the program into memory, and only then passes control to the program's entry point. The purpose is to allow natively compiled applications to be run as effortlessly as any scripting programs. This can done as `mm -run prog`, but if I rename the compiler to `ms.exe`, it can be just `ms prog` (M-script).

**It's a transpiler** Well not quite, since I've chosen to have that as a separately configured executable called `mc.exe`. `mc prog` will transpile my source code into a single C source file then apply a C compiler, or `-c` will just stop with the C file. (This option applied to my compiler lets me use an optimising C compiler and/or lets it work on Linux, but it only supports 95% of my language.)

**It's fast** Not really a feature, but some of the above (eg run from source) is only practical when the compiler doesn't hang about, so it is something I like to keep on top of (unlike some others). A self-hosted build of `mm.exe` is about 70ms.

I had wondered why my compilers had a higher line count than some others, now I know why!

