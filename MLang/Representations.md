## Program Representations

How the sources, binaries and intermediates can be represented, distrubuted, compiled and run.

### Multiple Source Files

This is just the normal way of keeping the various source modules and support files of a project, which may also span directories

I tend to this on my machine, as doing anything with it in this form is untidy. The only practical way to share or copy the files is to form a ZIP file, but there is no automatic procedure or script to collect only the necessary files.

In this form also, if a project can work on multiple targets or OSes, all the resources are there to make it possible; it's the master version.

### Amalgamated Source files ('OneSource')

A feature of **mm** compiler is to be able to take all input source and support files, and write them out as a single text file: `mm -ma prog` starts from `prog.m` which is the lead module, and produces `prog.ma`, containing all files needed. This is then very easy to upload, download, backup, email etc.

I tend to use this form for distribution. Such a program is built using `mm prog.ma` instead of `mm prog`.

But, where Multiple Sources include the optional modules when there is a choice (eg. special Windows or Linux modules), the OneSource version currently only includes the files needed for a specific target. (Because the is generated a couple of passes after the choice has been made, as it is to discover the support files needed, only known after parsing.)

So I may tweak it to include all optional modules (and do something about the support files, used for **include strinclude bininclude**). Then the same OneSource file can work for more than one target; the choice is defered until the .ma file is compiled.

### PCL Format (Intermediate Language)

This is the source format of my IL, which is an optional output of my compiler.

While source format (Multiple/OneSource) can still be portable across OSes and machines, PCL needs to be specific to an OS (because the choice of support module has been made, unless a more restricted OS-neutral module is used).

PCL however stays portable across machines and ABIs, although at present it only fully works for x64 running on Windows (such can programs still run on Linux-x64; see below).

Yet, I probably will use PCL very little; it was mainly useful to help enforce the separation between compile backend and frontend.

Also, PCL does not support inline assembly in the original source. (It only works when the PCL backend is integrated into the compiled, as the x64 backend needs to communicate with the frontend).

### ASM Format

This is yet another optional output, used for debugging, or for generating OBJ files which are only supported by my **aa** assembler. But I don't intended to use it for anything else unless a special need comes up.

The syntax is specific to my assembler so is no good in helping bootstrapping using other people's assemblers.

### MX/ML Portable Binary Formats

This format was recently created when I found problems in the DLL (shared library) output of my compiler. It is a much, much simpler binary representation, and easy to write and to read.

I discovered that it can also replace EXE format, resulting in two versions: **mx** files for applications; and **ml** files for dynamic libraries, which can replace the use of EXE/DLL within my applications. The fact that .ml libraries can only be loaded from .mx applications, without a *lot* of work to make them usable from EXE, mean they are used a little differently:

* To use .ml libraries, my main application has to be output as .mx not .exe.
* I can't run a .mx file directly, I need to use a special loader program run.exe. (This is quite small, it can be as little as 13KB.)
* This format is probably portable across OSes and targets provided, usually, that the included binary code is for that target. That is I can use the same file format and forget about EXE, ELF, DLL and SO.
* However, for Linux on x64, I find it is possible to run Windows x64 code directly on Linux for certain programs. (It depends on what external libraries are used.) That means being able to run the identical binary between Windows on x64 and Linux on x64. What's different is the **run** program, a normal executable, used on each.
* This format can also be generated in-memory, allowing programs to be generated, and executed immediately, without creating binaries on disk

### EXE Binary Format

This is the standard executable, in PE+ format, which is still supported. An EXE is always needed to get things started. But EXE doesn't support my .ml libraries, and it doesn't run on Linux, even if it contains Linux-specific code (other than under WSL).


