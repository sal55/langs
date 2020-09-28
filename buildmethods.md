## Project Build Systems

Some discussions on reddit have made it clear that way I build my programs is rather different from anyone else's. I get the impression also that people think there are only two alternatives - either use 'make', or type everything at the command line every time.

This describes the approaches I've used over the last few decades, but split into several different requirements.

### Tools

I predominantly use my own languages (roughly, one systems language 'M', and one scripting language 'Q') and my own compilers and tools. I have sometime offloaded tasks to third party products (eg. Nasm, for the linker of C compiler), but those were unsatisfactory.

Development has also been, since the mid-80s, on MSDOS then Windows.

### General Development


This is everyday programming where I might want to build and run a program hundreds of times a day.

#### Pre 2010s

Mainly I used a very simple IDE which worked from a project file that listed program modules and support files. This allowed me to navigate, edit, compile, link and run. Or compile all the modules. I was usually familiar enough with the project structure to know when changing a module required a full recompile. (The language had headers, but more organised than the haphazard include files of C.)

Individual compilation was always very fast, even on the limited hardware. A full build might have taken a few seconds.

#### Scripting

My commercial apps had about 3/4 of the modules (say 75 out of 100) as script modules. Those didn't need to all be built together. And actually I usually worked on them (edit, compile, run) from within the application. Building the natively compiled main application wasn't needed as often.


#### 2010s

Here, I started developing whole-program compilers - instead of compiling a module at a time, it switched to a program at a time (ie. the modules comprising one .exe file or sometimes one .dll file).

Now, a project file was no longer necessary, since the compiler could find the modules and support files for itself, starting from the lead module, example:
````
    mm cc                  # Build cc.exe, a C compiler, starting from the module cc.m
````
However, a project file is still maintained, so as to have something to navigate looking for modules and files to edit. A project file is also use to specify test runs and various inputs and options, so extra info and docs.

Such a compiler no longer needs object files, nor a linker (as there is effectively one module).

### Making Production Programs

Ie. for distribution to clients. This was was done with scripts. Either Windows Batch, or I used the scripting language of my app. This including putting the files in the correct place eg. on a floppy, or later for uploading.

This process always recompiled everything. It was not time sensitive as it was rarely done, but probably took a fraction of a minute.

I also liked tidy distributions, eg. comprising only a handful of files, so might run a script for example to collect all bytecode files into a single file. The main app would know to look in there for a script file. Example graphical app from 20 years ago:
````
    370,288 M7.EXE                     Main program
     45,056 M7.DAT                     Binary resources (icons, images etc using the app's data file format)
      7,434 M7.INI                     Config data
      8,541 M7CMD.INI                  Commands (eg. CLI commands plus tablet layouts)
    705,376 M7.PCA                     Scripts
````

### Making 'Open-source' Projects Available to Others

The first problem here is that, with projects written only in my private language, nobody could build them unless they had a binary of my compiler. Few will download a random binary. The key here was to add a C target to my compiler.

I also wanted to produce a *single* C source file. With separate compilation, this required collating dozens of C source, very messy and fiddly. But with the whole program compiler, the output was always a single file anyway. For example:
````
    mc -c cc
````
produced a one-file C version of my C compiler (starting from cc.m, builds dozens of modules into cc.c, which includes supporting text files (encoded as long strings), such as standard headers. This process takes about 1/6th of a second, marginally faster than a normal build.

Now I can make that file available (perhaps renamed to bcc64.c), and it will contain, via an automatically included comment block, build instructions. Which usually come down to something like:
````
    gcc bcc64.c -obcc.exe
````
The idea is that it is as simple to build as Hello, World. I started doing this a protest against temperamental build systems, complex makefiles and sprawling  source trees. However, no one was really impressed; they all prefer 'make'.


### Making Sources More Portable

I main making a compact single-file representation of a set of sources, that sometimes span directories. ZIP can do this, but it is a binary format, and can't be used without decompressing and recreating the same layout.

I devised a container file format which concatenates all the files needed to build a program, into a single text file, complete with directory info to extra the discrete files. This does not form a standalone module, but it is still a file that can be directly built with my 'mm' compiler. The file is created like this:
````
    mm -ma cc
````
This turns the dozens of source and support files into a single cc.ma file, in this case formed of 73 files and totalling 37,000 lines. It can be built like this:
````
    mm cc.ma
````
I found this useful to be able to effectively manipulating a single source file instead of many.


### Building C Projects

Mainly other people's projects (since mine tend to have just one file!). Since I refuse to use problematical makefiles (and anyway prefer to use my C compiler with its unconventional behaviour), this involves extracting the build info needed: the C files that must be submitted to the C compiler, often a trial and error process.

Once known, I often keep this list as an '@' file, an options file supported by many C compilers. I try to keep only files in there, and options elsewhere, so that the same file works for several compilers.

I also have an experimental feature in my C compiler which will automatically locate the .c files corresponding to any #includes, and add in those to the list of modules. But this only works when the C modules are written as .h/.c pairs. Most existing C code is much more chaotic.

However, here is a simple project where it works. First, the normal way by submitting all necessary modules:
````
C:\c>bcc cipher hmac sha2
Compiling cipher.c to cipher.asm
Compiling hmac.c to hmac.asm
Compiling sha2.c to sha2.asm
Assembling to cipher.exe
````
And now using the new feature, activated with '-auto':
````
C:\c>bcc -auto cipher
   1 Compiling cipher.c     to cipher.asm       (Pass 1)
*  2 Compiling hmac.c       to hmac.asm         (Pass 2)
*  3 Compiling sha2.c       to sha2.asm         (Pass 2)
Assembling to cipher.exe
````
However, if I am actively work on a C project of my own, then I will just use a project file.


### Conditional Code and Configuration

I no longer have conditonal directives for arbitrary chunks of code. The only conditional building occurs at the module level: import module A or B, or map, sometimes conditionally, module A to B. I use this to create multiple versions of my 'mm' compiler, but using different lead modules which specify the mappings. These are small stub modules of 20 lines or so, but because the executable is named after the lead module, they result in multiple versions of the program.

For example I have:
````
    mm.exe        Compile M source files to x64 native code
    mc.exe        Compile to one C source file that runs on Windows
    mu.exe        Compile to one C source file that runs on Linux (this one can run on Windows with the -c option to avoid
                  invoking a C compiler, but the C file must be built on Linux)
    mn.exe        Compile to one C source file that runs on any OS (with some reduced capability as some features are not available)
````
There were also 32-bit C versions, but those have been dropped.

And sadly I will probably drop all C target versions, as my M language is evolving and it is increasingly harder to express everything in C.

### Current Set of Tools
````
ff.pc     100KB       Mini IDE (bytecode file)
edit.pc   100KB       Editor (bytecode file)
ax.exe    140KB       x64 assembler and linker, turns .asm files to one .exe or .obj
cc.exe    495KB       C compiler (self-contained with embedded system headers, otherwise 370KB), turns .c files to .exe
mm.exe    570KB       M compiler (self-contained with library sources), turns M project into .exe. Whole project compiler.
qc.exe    520KB       Bytecode compiler (self-contained with library sources, otherwise 230KB).
                      Whole project compiler (turns multiple Q source files into self-contained .pc file)
pc.exe    220KB       Bytecode interpreter
````
The set needed for building M projects (mm, ff, edit, and pc) total 1MB. They would fit on one 1.44MB floppy disk.
