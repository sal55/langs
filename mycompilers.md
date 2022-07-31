## Summary of My Compiler Projects

This concentrates on compilers for lower-level langauges. It omits details of my bytecode compilers for my interpreted languages, or my C compiler project.

All of them apart from the first are for my 'M' lower-level systems language. While that has evolved significantly since the first 8-bit version, the current 64-bit product would be still be regarded as lower level compared with the current crop of 'systems' languages. It is somewhat higher level than C89.

This describes also some of the challenges faced decades ago, which might be of historical interest.

### 'Babbage' Compiler

This was my very first compiler, done as a college project (circa 1979). 'Babbage' was a machine-oriented language (now called a High Level Assembler) for GEC 4080 and the task was to port that to the college's PDP10 mainframe machine.

This was written in assembly (using MACRO 10, DEC's assembler for PDP10), and generated assembly output. I tested it by using it to implement itself ('self-hosting').

I found it quite challenging. (Bear in mind also that I could only work on it from a terminal that you had to book for an hour at a time. I could only work on it off-line from a printouts.).

### Z80 Compiler (1)

This was for the first crude language I devised, called 'M'.

It was also written on, and for, my homemade Z80 computer, a second version which had extra memory, a real keyboard, and a text (and later graphical) display.

There was no storage however, except for slow (1200 bps) and unreliable cassette tape. For this reason the compiler (together with a simple editor plus the source code of the application) were resident in memory.

This was written in a primitive assembler, itself written in hex machine code (and the program to allow that was itself written in actual binary, using switches and LEDs - real bootstrapping!).

To avoid everything being wiped out in the event of a program going haywire, the 32KB main memory was split into two 16KB banks, one of which could be write-protected via a switch. Then the compiler, editor and source could be made safe.

(As for the sorts of programs I was writing, I built an add-on graphics circuit for this. This could display either 256x256x1-bit, or 128x128x4-bit greyscale - thanks to a homemade DAC, which could be externally synched ('genlocked') to a TV signal, so that I could use it as a frame-grabber.

Some programs played with 3D vector graphics (the language had `u8`, `i16` and `f24` (not `f32`) data types, called `byte int real`), which also required some maths support. Remember this had zero available software, zero libraries, and there was no internet. Others worked with frame-grabbing and image-processing, like trying to recorded captured video on cassette (about 0.2Fps full-screen, 4-5fps at small frame sizes, not that great).)

This would have been 1981.

### Z80 Compiler (2)

This was created done to help with my job as hardware engineer developing business computers and prototyping endless crazy ideas of my boss.

I can't remember the details of the bootstrapping process (whether I used an available assembler, or wrote one somehow). But this compiler was also memory-resident. The reason was two-fold: floppy disks were too slow for the turnaround I wanted (commercially available C compilers for example would take minutes for the simplest program; my small programs compiled near-instantly).

But also I was doing a lot of work on new hardware with an embedded processor and the compiled code has to be somehow downloaded to the test board.

At this time, the compiler didn't have modules at all.

### Z80 Compiler (3)

The details are now very hazy (I wish I'd kept some docs, sources or even photos). But the compiler become self-hosted, acquired separate modules, and was used as an almost traditional compiler with independent compilation.

The relocatable binary files produced (it did not generated ASM) were in my own simple format, and I used a fast 'loader' program to combine these into an executable acceptable to the OS (our own version of CP/M). I never really saw the point of linkers which IME were big, slow, cumbersome programns.

However, the compiler may still have been memory-resident, part of the mini-IDE-like program which display the list of modules of the project, and allowed browsing and editing. Source code now resided on floppy.

While mostly used in-house (this was a very small company), this compiler was also used to produce commercial software, a simple drawing package, for machines like the Amstrad PCW. (By this point we stopped trying to develop our own machines.)

### 8088/86 Compiler (1)

It now starts to go very quickly. Again I forget the method I used to bootstrap the first 8086 version of the language (this is now for IBM PCs and compatibles). I do remember writing some apps in a private assembler, so may that's what I used.

Most M versions also had inline assembly. That was essential for the stuff I was doing. But also, since there was no optimiser, it was needed for bottlenecks.

Still, I vaguely remember my 8086 compilers outperforming C compilers, the few times I compared them. Then C compilers acquired optimisers (gcc first appeared in around 1987).

### x86 (80386) Compiler (2)

Around 1992 I decided to give up own languages, and switch to C. (I also wanted to get a different job.) I bought a Visual C compiler from MS, for some £160 (the first and last time I paid for a compiler).

Using new MSDOS, using DLLs, using Windows, all meant different tools anyway. The trouble is, I hadn't looked at C in detail before, and it was dreadful!

I decided to perserve with my own language, but needed a new compiler for Windows. I also forget about changing jobs; I become self-employed. The C compiler was given away.

This new compiler generated 16-bit code still but for the 80386 processor. Thad had a useful feature: even in 16-bit mode, it was possible to make use of 32-bit registers, 32-bit operations, and extra 32-bit address modes, via prefix bytes.

(You couldn't just switch to full 32-bit mode since the OS still ran as 16-bits. Some products got around that, DOS-extenders and such, but that was beyond me.)

### x86 Compiler (3)

At some point, I switched from generating my object file format (or it maybe it happened above), to generating ASM files in NASM format. For linking, I used one belonging to a C compiler.

I'm not proud of this period, but it worked. Although I found it odd that it took NASM 5 times as long to assemble my ASM file, as it took my compiler to create it, a more difficult task.

At some time, I think 2002, I made it a full 32-bit compiler.

### x64 Compiler (1)

At first attempt at an x64 compiler (these are all for Windows), tried something unusual: pointers would stay at 32 bits, while ints etc would be 64 bits. This was to avoid wasting memory since most programs worked with 2GH of memory still.

But this provided troublesome without OS cooperation, and eventually I went with 64-bit pointers.

At this point, the compiler still used independent compilation, still generated NASM output, and still needed a linker.

### x64 Compiler (2)

I wanted to create a whole-program compiler, but if the output was ASM, it would be a single ASM file. While NASM had gotten faster, it had some bug or other than made it exponentially slower on large inputs. Like taking a minute or more to assemble a 100Kloc file. (I didn't know at this time about YASM.)

I developed my own x64 assembler, which meant learning about the OBJ file format. This made possible whole-program compilations. (There was also parallel develoment with creating new module schemes for the language.)

My assembler was faster (it worked at millions of lines per second). However, it still relied on an external linker, and they were all troublesome:

* Using `gcc` meant a 0.5GB dependency for my 0.5MB compiler. I also had no idea what it might be linking in to my executable
* Extracting its `ld` linker was better, but it was 1-2MB, and used a handful of DLLs. But it gave mysterious errors on my laptop
* The tiny GoLink (47KB) was also tried. But it used peculiar ways of dealing with imported DLL names, that required different code generation. (It also generated EXEs that attracted the attention of AV software, more so than normal, and which were mysteriously double the size they ought to have been.)

So...

### x64 Compiler (3)

I had to bite the bullet and get my assembler to directly generate EXE files; a soul-destroying few weeks.

By this time, the backend of the assembler was also incorporated into the compiler; there was no discrete ASM represented, unless it was
requested. Now any application could be easily built using a one-file self-contained compiler (it included also library sources):
```
mm qq
```

qq.m is the lead module of my interpreter. This builds that 40Kloc project in 1/8 or 1/10th of a second.

### x64 Compiler (4)

I added DLL output to the compiler. But I eventually discovered that it was buggy. Because it goes wrong somewhere inside the OS, it was hard to find the cause.

I decided to create my own shared-library format, called ML, using .ml files. This was much simpler than DLL, and more reliable. (Also it shared 
the environment of the host application.) Of course, it can only be used from my own M applications.

This idea was extended to my own executable format, called MX and using .mx files. However Windows doesn't understand MX files, so to run those it needs conventional stub program, about 12KB.

(Since ML/MX are portable formats, one possible use would be on Linux, where I don't need to learn about ELF format; I just need a separate loader, say written in C. Which in fact, I've already written, to try it out on Windows. I wanted to run my code for x64, without even needing to adjust it to use the SYS V ABI, which is just about possible for certaimn programs, but then I lost interest: WSL can run EXEs anyway...)

Another by-product of the fixups needed for MX/ML files, is to fix up generated code to run directly in memory, without needing to write those files.

This nows allows the compiler to run code directly from source, just like a scripting language. Take these 3 files:
```
c:\demo>dir
04/12/2021  15:07                24 hello.q
18/07/2022  11:07           716,194 mm.ma
18/07/2022  11:06         1,016,555 qq.ma
```
mm.qq is the source for my M compiler; qq.ma is the source for my Q compiler/interpreter; both are one-file amalgations produced via the `-ma` options of the M compiler. Here, the first `mm` is the `mm.exe` M compiler, located elsewhere:
```
c:\demo>mm -run mm -run qq hello
(Building mm.ma)
Compiling mm.m to memory
(Building qq.ma)
Compiling qq.m to memory
Hello, World!
```
This compiles the sources for the M compiler, runs it from memory, which compiles the sources for the Q interpreter, runs that on memory which executes that hello.q script. This takes under 0.2 seconds and needs to write 0 bytes to disk. (I wonder how long it would take, time and storeage, with gcc first building itself then building CPython).

### Compiler Internals

I haven't said much about this, partly because I can't really remember. But all of them, from the late Z80 version to now, create ASTs with 2-3 passes:
````
    AST1     Produced by the parser
    AST2     Name-resolved version (for the last decade; before that these were combined)
    AST3     Type-analysed
````
Early compilers used ad hoc code generation without an IL. For the last few years I've used an IL, trying both a stack-based one, and a three-address-code version.

The IL is actually what I've been working on recently. I've just decided to remove it completely as it doesn't pay its way.

### Language Changes Affecting Compilation

This is other than switching targets.

* Different modules schemes
* Out-of-order compilation (the reasomn for the separate name-resolving pass)
* Whole-program compilation (eg. all modules must be parsed before progressing to the next stage)
* Different backend schemes

### Using a C Target

For a period I supported an optional C target:

* To get the benefit of an optimising compiler
* To allow my programs to run on Linux

But that was not really satisfactory. Also, 



