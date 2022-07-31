## Summary of My Compiler Projects

1979 to Present.

This lists compilers for lower-level languages. It omits the bytecode compilers for my interpreted languages, and my C compiler project.

All of them apart from the first are for my 'M' lower-level systems language. While that has evolved significantly since the first 8-bit version, the current 64-bit product would still be regarded as quite low level compared with the current crop of 'systems' languages. It is somewhat higher level than C89.

This describes also some of the challenges faced decades ago, which might be of historical interest.

### 'Babbage' Compiler

This was my very first compiler, done as a college project (circa 1979). 'Babbage' was a machine-oriented language (now called a High Level Assembler) for GEC 4000 and the task was to port that to the college's PDP10 mainframe machine.

This was written in assembly (using MACRO 10, DEC's assembler for PDP10), and generated assembly output. I tested it by using it to implement itself ('self-hosting').

I found it quite challenging. (Bear in mind that I could only work on it from a terminal that you had to book for an hour at a time.)

### Z80 Compiler (1)

This was for the first crude language I devised, called 'M' (around 1981). It was also written on, and for, my homemade Z80 computer, a second version which had extra memory, a real keyboard, and a text (and later graphical) display.

There was no storage however, except for slow (1200 bps) and unreliable cassette tape. For this reason the compiler (together with a simple editor plus the source code of the application) were resident in memory.

This was written using a primitive assembler, itself written in hex machine code (and the program to allow that was itself written in actual binary, using switches and LEDs - real bootstrapping!).

To avoid everything being wiped out in the event of a program crash, the 32KB main memory was split into two 16KB banks, one of which could be write-protected via a switch. Then the compiler, editor and source could be made safe.

(As for the sorts of programs I was writing, I built an add-on graphics circuit for this. This could display either 256x256x1-bit, or 128x128x4-bit greyscale - thanks to a homemade DAC, which could be externally synced ('genlocked') to a TV signal, so that I could use it as a frame-grabber (requiring a homemade ADC).

Some programs played with 3D vector graphics (the language had `byte int real` types, that is `u8 i16 f24` - 24-bit floats), which also required some maths support. Remember there was zero existing software and there was no internet. Other programs worked with frame-grabbing and image-processing, like trying to record captured video on audio cassette.)

### Z80 Compiler (2)

This was created to help with my job as hardware engineer developing business computers and prototyping endless crazy ideas of my boss.

I can't remember the details of the bootstrapping process (whether I used an available assembler, or wrote one somehow). But this compiler was also memory-resident. The reason was two-fold: floppy disks were too slow for the turnaround I wanted (commercially available C compilers for example would take minutes for the simplest program; my small programs compiled near-instantly).

But also I was doing a lot of work developing new hardware with embedded processors and the compiled code had to be somehow downloaded to the test board.

At this time, the language didn't have modules at all.

### Z80 Compiler (3)

The details are now very hazy (I wish I'd kept some docs, sources or even photos). But the compiler become self-hosted, acquired separate modules, and was used as an almost traditional compiler with independent compilation.

The relocatable binary files produced (it did not generate ASM) were in my own simple format, and I used a fast 'loader' program to combine these into an executable acceptable to the OS (our own version of CP/M). I never really saw the point of linkers which IME were big, slow, cumbersome programns.

However, the compiler may still have been memory-resident, part of the mini-IDE-like program which displayed the list of modules of the project, and allowed browsing and editing. Source code now resided on floppy.

While mostly used in-house (this was a very small company), this compiler was also used to produce commercial software: a simple drawing package, for machines like the Amstrad PCW. (By this point we stopped trying to develop our own machines.)

### 8088/86 Compiler (1)

It now starts to change quickly. Again I forget the method I used to bootstrap the first 8086 version of the language (this is now for IBM PCs and compatibles). I do remember writing some apps in a private assembler, so maybe that's what I used.

Most M versions also had inline assembly. That was essential for the stuff I was doing. But also, since there was no optimiser, it was needed for bottlenecks.

Still, I vaguely remember my 8086 compilers outperforming C compilers, the few times I compared them. Then C compilers acquired optimisers (gcc first appeared around 1987).

### x86 (80386) Compiler (2)

Around 1992 I decided to give up my own languages, and switch to C. (I also wanted to get a different job.) I bought a Visual C compiler from MS, for some £160 (the first and last time I paid for a compiler).

Using Windows, DLLs and so on, all meant new tools anyway. The trouble is, I hadn't looked at C in detail before, and I thought it was dreadful!

I decided to persevere with my own language, but needed a new compiler for Windows. I also forgot about changing jobs; I become self-employed. The C compiler was given away.

This new compiler generated 16-bit code still but for the 80386 processor. That had a useful feature: even in 16-bit mode, it was possible to make use of 32-bit registers, 32-bit operations, and extra 32-bit address modes, via prefix bytes, something not possible on x64.

(You couldn't just switch to full 32-bit mode since the OS still ran as 16-bits. Some products got around that, DOS-extenders and such, but that was beyond me.)

### x86 Compiler (3)

At some point, I switched from generating my object file format (or it maybe it happened above), to generating ASM files in NASM format. For linking, I used one belonging to a C compiler.

I'm not proud of this period, but it worked. Although I found it odd that it took NASM 5 times as long to assemble my ASM file, as it took my compiler to create it, a more difficult task.

At some time, I think 2002, I made it a full 32-bit compiler.

### x64 Compiler (1)

At my first attempt at an x64 compiler (these are all for Windows), I tried something unusual: pointers would stay at 32 bits, while ints etc would be 64 bits. This was to avoid wasting memory since most programs worked within 2GB of memory still.

But this proved troublesome without OS cooperation, and eventually I went with 64-bit pointers.

At this point, the compiler still used independent compilation, still generated NASM output, and still needed a linker.

### x64 Compiler (2)

I wanted to create a whole-program compiler, but if the output was ASM, it would be a single ASM file. While NASM had gotten faster, it had some bug or other that made it exponentially slower on large inputs. Like taking a minute or more to assemble a 100Kloc file. (I didn't know at this time about YASM.)

I developed my own x64 assembler, which meant learning about the OBJ file format. This made possible whole-program compilation. (There was also parallel develoment with creating new module schemes for the language.)

My assembler was much faster (it worked at millions of lines per second). However, it still relied on an external linker, and they were all troublesome:

* Using `gcc` meant a 0.5GB dependency for my 0.5MB compiler. I also had no idea what it might be linking in to my executable
* Extracting its `ld` linker was better, it was 1-2MB plus a handful of DLLs. But it gave mysterious errors on my laptop
* The tiny GoLink (47KB) was also tried. But it used peculiar ways of dealing with imported DLL names, that required different code generation. (It also generated EXEs that attracted the attention of AV software, more so than normal, and which were mysteriously double the size they ought to have been.)

So...

### x64 Compiler (3)

I had to bite the bullet and get my assembler to directly generate EXE files; a soul-destroying few weeks.

By this time, the backend of the assembler was also incorporated into the compiler; there was no discrete ASM representation, unless it was
requested. Now any application could be easily built using a one-file self-contained compiler (it included also library sources):
```
mm qq
```

qq.m is the lead module of my interpreter. This builds that 40Kloc project in about 1/10th of a second.

### x64 Compiler (4)

I added DLL output to the compiler. But I eventually discovered that it was buggy. Because it goes wrong somewhere inside the OS, it was hard to find the cause.

I decided to create my own shared-library format, called ML, using .ml files. This was much simpler than DLL, and more reliable. (Also it shared 
the environment of the host application, something uncertain with DLLs.) Of course, it could only be used from my own M applications.

This idea was extended to my own executable format, called MX and using .mx files. However Windows doesn't understand MX files, so to run those it needs a conventional stub program, about 12KB.

(Since ML/MX are portable formats, one possible use would be on Linux, where I don't need to learn about ELF format; I just need a separate loader, say written in C, to run MX applications. Which in fact, I've already written, to try it out on Windows. I wanted to run my code for x64, without even needing to adjust it to use the SYS V ABI, which is just about possible for certain programs, but then I lost interest: WSL can run EXEs anyway...

Another by-product of the fixups needed for MX/ML files, is to fix up generated code to run directly in memory, without needing to write those files.

This nows allows the compiler to run code directly from source, just like a scripting language. Take these 3 files:
```
c:\demo>dir
04/12/2021  15:07                24 hello.q
18/07/2022  11:07           716,194 mm.ma
18/07/2022  11:06         1,016,555 qq.ma
```
mm.qq is the source for my M compiler; qq.ma is the source for my Q compiler/interpreter; both are one-file amalgamations produced via the `-ma` options of the M compiler. Here, the first `mm` is the `mm.exe` M compiler, located elsewhere:
```
c:\demo>mm -run mm -run qq hello
Compiling mm.m to memory
Compiling qq.m to memory
Hello, World!
```
This compiles the sources for the M compiler, runs it from memory, which compiles the sources for the Q interpreter, then runs that in memory which executes that hello.q script. This takes under 0.2 seconds and needs to write 0 bytes to disk.

The equivalent with mainstream products would be for gcc to first build itself from scratch, then use that new version to build CPython from scratch, then run hello.py.)

### Intermediate Language/Representation

Early compilers used ad hoc code generation without an IL. For the last few years I've used an IL, trying both a stack-based one, and a three-address-code version. At one point, the IL could also be used as a separate language in its own right, with a source syntax for input files, and even a discrete program to turn such a file into an EXE. (This was my protest project against the complexity and size of LLVM.)

ILs are what I've been working on recently, trying to see which one is best for moving things forward.

After many weeks and to-ing and froing, my decision was: to discard both! I decided having an IL at all didn't buy me much.

For non-executable code, it's an unnecessary extra layer. For helping improve the generated code, my latest ideas involved either doing something before the IL was generated, or as part of the next stage. So I'm going back to an earlier model. It might take me a week to get that working on my code-base.

### Using a C Target

For a period I supported an optional C target:

* To get the benefit of an optimising compiler
* To allow my programs to run on Linux

But that was not really satisfactory. Also, some M languages features couldn't be used. While admittedly highly useful, I decided to withdraw the feature; purity and self-sufficiency is too attractive.

### What Were They Used For

The very first was just an appealing project that I had a genuine interest in.

The next (Z80) was the hobby one. I got a kick of being able to write HLL code on my home £100 computer, that a few years earlier I need to use a £500,000 mainframe.

The subsequent compilers for Z80 then x86 were used for a range of low-level code. It should be remembered that in the world of home and small business microcomputers and PCs, for most of the 80s and half the 90s, the OS was was very primitive and did very little: basically keyword, console display, and file system.

Everything else, applications had to program themselves, or find some library that could do so, especially concerning graphics. In my case, since I used zero other software other than the meagre OS, that included:

* Boot loaders
* Hardware test programs
* Drivers for various devices: serial, floppy disks, interrupts...
* Printer and plotter drivers
* Low-level graphics support (my job for a while was designing new video circuits)
* Vector-drawing graphics libraries
* Basic GUIs
* Interfacing to input devices (mainly tablet pointing devices)
* Floating point emulation
* Maths libraries
* 3D graphics support
* Bitmap font support
* Reading and writing myriad different file formats
* Memory allocators
* ...

The sort of stuff that people think only C is capable of. Plus of course writing compilers, editors, loaders, assemblers, interpreters. 

Other than that, they were used for commercial apps.

So it is clear that my compilers were tools intended to get things done. They were secondary to my main job.

For the last 10-20 years however, when I haven't actually worked, little has been done with them, except for tinkering and hobbyist stuff, mostly language-related. I decided not to just let the language die. Some interesting ideas have been experimented with over the last few years, even if the language itself is old-fashioned.

### Comparison of M and C

That would be a long article, but I'm not going there. Regarding their compilers however, there are many differences:

* Mine were in-house or private tools, only used by myself.
* Mine were not intended to be portable across 100 architectures. They were usually created for one target at a time, until the technology and/or the market changed. Hence the progression from Z80, and then generations of x86, whatever was the latest product that ordinary consumers (ie. my customers) could buy.
* Mine didn't need to provide 100% coverage of all combinations of features, since my codebase was limited. Eg. to 100-200Kloc per generation. C compilers have to cope with billions of lines of existing code (as I found out with my C compiler project, now in a state of disuse.)

So, I can cut corners, I can work around bugs or limitations. I wasn't after all employed as a compiler writer; they were just a useful tool, good enough to benefit my work, and, as it turned out, my company (I think we might have sold some $10m of software built with those compilers, poor as they were).


### Optimisation

I mentioned in a recent thread I made that I did not regard this as a priority. The targets I've used over the years have probably gotten 1000 times faster or more. But even the best optimisation would only give a one-off speed-up of 2 times, for a typical program of mine.

(It depends on the actual program, even the actual processor model, as they all have different characteristics.)

Against that is the cost. So it's more of an annoyance that on the same hardware, my language plus my compiler will be disadvantaged when compared with other products that have the benefit of a fully optimised build.

Still, I will look at the issue again shortly, to the minimise the difference. But I'm still not going to have as effective an optimiser as top end products. Not unless I reinstate a C target (and compromise my language to make it work).

Instead, I concentrate on language design: making it easier to generate efficient code, because the features make it clear what is being attempted. For example:

    swap(A[i], B[i])
 
 instead of having to write, even hidden behind a macro:
 
    temp := A[i]; A[i] := B[i]; B[i] := temp
 
Or not having an implementation that builds multiple layers of code which then relies on a decent optimiser to remove all the redundancy.

Here's a last anecdote: I didn't bother with comparing performance of language tools until recently. But in 1981, I did measure the speed of my first simple Z80 assembler: it could process 1800 lines per second on my 2.5MHz Z80.

That 2Kloc is not far off what I've measured for quite a few compilers, especially in optimising mode, and even assemblers (yes, Nasm), which run on 64-bit machines with a 1000x higher clock, 250,000 times more RAM and infinitely more storage. So I put more emphasis on writing efficient software rather than just depending on an optimiser to take care of sloppy code.

Optimisation should be a bonus, not an essential.

### Assemblers

There were some assemblers along the way too. The following are standalone assemblers that I actually remember writing:

* Z80 Assembler - this is the one mentioned above
* 80186/88 Assembler - The 80186/188 is a version of the 8086/88 with enhanced instruction set and integrated peripherals. (We were going to use it for a portable machine - in 1984. Possibly this was the basis for my later 8086 assembler)
* 8051/8035 Assembler - I forget exactly which microcontroller this was. But I found it primitive - the Z80 was a supercomputer by comparison. Apparently a C compiler existed for it - kudos to them.
* x64 Assembler - this is the much more recent one that I reluctantly created, which also does the job of linker

Most assembly however was written inline within a HLL.
