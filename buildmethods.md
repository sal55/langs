## Project Build Systems

Some discussions on reddit have made it clear that way I build my programs is rather different from anyone else's. I get the impression also that there are only two alternative - either use 'make', or type everything at the command line every time.

This describes the approaches I've used over the last few decades, but split into several different requirements.

### Tools

It should be made clear that I predominantly use my own languages (roughly, one systems language and one scripting language) and my own compilers and tools. The tools were already crude in the 1980s, and haven't really changed. However this is not about that.

Development has also been, since the mid-80s, on MSDOS then Windows.

### General Development

This is everyday programming where I might want to build and run a program hundreds of times a day.

Mainly I used a very simple IDE which worked from a project file that listed program modules and support files. This allowed me to navigate, compile modules, link and run. Or compile all the modules. I was usually familiar enough with a project to know when changing a module required a full recompile.

Individual compilation was always very fast, even on the limited hardware. A full build might have taken a few seconds.

#### Scripting

My main apps had about 3/4 of the modules (say 75 out of 100) as script modules. Those didn't need to all be built together. And actually I usually worked on them (edit, compile, run) from within the application. Building the natively compiled main application wasn't needed as often.

#### 2010s

Here, I started developing whole-program compilers - instead of compiling a module at a time, it switched to a program at a time (ie. the modules comprising one .exe file or sometimes on .dll file).

Now, a project file was no longer necessary, since the compiler could find the modules and support files for itself, starting from the lead module, example:
````
    mm cc                  # cc.m is lead module of my C compiler; build cc.exe.
````
However, a project file is still maintained, so as to have something to navigate looking for modules and files to edit. A project file is also use to specify test runs and various inputs and options.

Such a compiler no longer needs object files, nor a linker (as there is effectively one module).

### Making Production Programs

Ie. for distribution to clients. This was was done with scripts. Either Windows Batch, or I used the scripting language of my app. This including putting the files in the correct place eg. on a floppy, or later for uploading.

This process always recompiled everything. It was not time sensitive as it was rarely done, but probably took a fraction of a minute.

### Making 'Open-source' Projects Available to Others

The first problem here is that, with projects written only in my private language, nobody could built them unless they had a binary of my compiler. Few will download a random binary.

The key here was to add a C target to my compiler (also used for other purposes such as running on Linux, or using optimising C compilers).

I also wanted to produce a *single* C source file. With separate compilation, this required collating dozens of C source, very messy and fiddly.

But with the whole program compiler, the output was always a single file anyway (one .exe, one .asm. one .c, or one .obj depending on options). For example:
````
    mc -c cc
````
produced a one-file C version of my C compiler (starting from cc.m, builds dozens of modules into cc.c, which includes supporting text files (encoded as long strings), such as standard headers. This process takes about 1/6th of a second, marginally faster than a normal build.

Now I can make that file (eg. as bcc64.c) available, and it will contains, via an automatically included comment block, build instructions. Which usually come down to something like:
````
    gcc bcc64.c -obcc.exe
````
The idea is that it is as simple to build as Hello, World. I started doing this a protest against temperamental build systems, complex makefiles and sprawling  source trees. However, no one was really impressed; they all like 'make'.



### Making Sources More Portable

I mean physically like moving across machines, so might need to be done via the internet, pen drive etc.

My sources will comprise dozens of modules and support files. Sometimes in more than one directory. It's messy. I devised a container file format which concatenates all the files needed to build a program, into a single text file, complete with directory info to extra the discrete files.

This does not form a standalone module, but it is still a file that can be directly built with my 'mm' compiler. The file is created like this:
````
    mm -ma cc
````
This turns the dozens of source and support files into a single cc.ma file, in this case formed of 73 files and totalling 37,000 lines. It can be built like this:
````
    mm cc.ma
````
I found this useful when experimenting with Linux where it was not possible to share folders. Of course, ZIP might do something similar (and also compress the file), but to use it, it must be decompressed and the directory structure recreated.


### Building C Projects

Mainly other people's projects (since mine tend to have just one file!). Since I refuse to use problematical makefiles (and anyway prefer to use my C compiler with its unconventional behaviour), this involves extracting the build info needed: the C files that must be submitted to the C compiler, often a trial and error process.

Once known, I often keep this list as an '@' file, an options file supported by many C compilers. I try to keep only files in there, and options elsewhere, so that the same file works for several compilers.

### Projects I couldn't Build

(Again, other's people's open source programs.) These include the GMP library, as binaries for Windows are too elusive to rely on, if you want to distribute programs that make use of it. This is one of those that started off with a 20 or 30Kloc configure script, which on Windows needs CYGWIN, MSYS or WSL. It never worked.

Another is CPython, which is so complicated, the Windows version doesn't bother with gcc/make/configure, but relies on VS2017, and GIT, and SVN, and MSBUILD tools, all giant downloads. Needless to say that didn't work.

The CPython one is interesting. I have my own scripting language, and while it is appreciably smaller and simpler, it can build like this from the one-file C version:
````
    tcc qq.c
````
It manages this in 0.15 seconds, a version that compiles on either Linux or Windows.

I must be doing something right I think...


### Conditional Code and Configuration

I no longer have conditonal directives for arbitrary chunks of code. The only conditional building occurs at the module level: import module A or B, or map, sometimes conditionally, module A to B. I use this to create multiple versions of my 'mm' compiler, but using a different lead modules which specify the mappings. These are small stub modules of 20 lines or so, but because the executable is named after the lead module, they result in multiple versions of the program.

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


