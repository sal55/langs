**Computer** Yes, at one point, while unemployed after college, I had to make my own computer. Not as hard as it sounds, this was 1981 not 1941, so I could just buy a ready-made CPU chip, a device that was only 2x1", about the size of your thumb. Of course, it had to be built into a circuit with other components, and using only a multi-meter for fault-finding.

But eventually, after a first prototype version, I had a Z80-based computer with 32KB memory, 8KB graphics memory, switchable betweeen 4-bit greyscale and 1-bit vector, 1KB text display, and 2KB EPROM (home-programmed with a Heath-Robinson setup). But of course zero software, or tools to create the software. This all had to be done.

**Video** I found work with a small company making 8-bit business computers, and ended up designing the motherboards. Which meant I was programming on machines I had helped design. I also created lots of experimental prototypes many revolving around graphics boards, so much of my graphics and other programming for a few years was in hardware I had created. 

**The OS** My company also created its own CP/M-compatiable disk OS, in order to avoid paying licence fees. I wasn't involved in that, but that was the kind of culture I was working in.

**Assembler (Z80, 1981)** This was a crude affair, written in hex machine code (via a program itself written in actual binary). As I said, the machine had nothing).

**Systems Language and Compiler** The first version for that 1981 machine was crude. A second version was created that ran on the business machines, which had the luxury of using floppy disks for storage.

The compiler was initially written in the above assembler, and later it was self-hosted. The language was later ported to x86, 80386 and x64, where it still runs today.

Why not C? I'd had no experience of it; I didn't like the look of it; and my boss wouldn't have paid for a compiler anyway; I was an engineer not a programmer. And by all accounts, they would have been terribly slow to work with. I still think it is a terrible language: not for being low-level (so is mine!), but for being designed so badly.

**Assembler (8035? 1980s)** One project involved an Intel microcontroller, because it was a cheap device. Probably the company could have purchased an assembler for it if I'd asked, but a few days of my time to write one probably cost a lot less. It was also a product that generated the output in exactly the form I wanted and could be integrated into my development program.

**Assembler (80188, 1980s)** The 80188 was an 8088 with integrated peripherals and extra instructions, to be used in a proposed portable computer. There may well have been an existing 8088 assembler that ran on PCs, but not for this version, and not running on Z80 as we were still using. I wrote a lovely cross-assembler for that device.

**Assembler (x64, 2010s)** From around 2000, I did use a third party assembler, NASM, then 16/32 bits. It wasn't fast, I thought it odd that it took 5 times as long to assemble my generated code, as it took my compiler to create it!

But in the 2010s, attempting whole-program compilers for x64, the output would have been a single large ASM file. However, at sizes of 20K lines and above, NASM got exponentially slow. A 100Kloc file might take take a minute or more to assemble. Something needed to be done! So I created my own x64-subset assembler that generated OBJ files (PE+ COFF64 format). Currently that works at 2-3M lines per second.

(There are faster products than NASM, but I didn't know about them, and mine is still much faster.)

**Linker (Z80, x86, 1980s/90s)** My compiler then generated object files, one per module. They needed combining into one executable, so I wrote a program called a Loader. It loaded multiple OBJ files (then using my own simple format), did the fixups needed, and wrote out the executable. I really could never understand the fuss about linkers. In the 1970s running a linker on a mainframe took up most of the machine's resources.

**Linker (x64, 2020s)** An actual linker was still needed to process the OBJ files produced by NASM or by first x64 assembler, and the choices were limited: use `gcc` which invokes `ld`, using *fifty* options so who knows what extra C-specific crap is being added to my executables. Or I can run `ld` directly, but that had problems: mysterious errors when run on my laptop, I think also some issues in direct linking with DLL shared libraries.

Something needed to be done, so I bit the bullet and got my assembler to directly generate EXE files. No linker needed at all!

(My C compiler still needs to compile modules independently, but there the assembler can take multiple ASM files and produce one EXE file. It does the linker's job, via ASM files not OBJ.)

The later passes of my assembler were subsequently ported to my native code compilers, so they can now compile from source to binary in one program. This doubled compiler throughput, compared with needing to to through intermediate textual ASM.

**Compiler Native Code Backends** These have mostly been ad hoc. The funny thing is, during the 1980s, the generated code of my compilers was as fast and sometimes faster than that of typical C compilers.

Then those started being optimised. And yet, even today, optimised code isn't that much faster than my ad hoc code, perhaps 1-2 times as fast depending on application, but bigger factors on benchmarks or programs with obvious bottlenecks. But on assemblers, compilers and interpreters, optimised native could is typically 40% faster than mine.

Still, 40% is 40%. I wouldn't mind using someone else's backend, if the product was simple, small, self-contained and, for non-optimised code, fast.

The obvious product is LLVM, but it doesn't meet any of those requirements. The next one is to use intermediate C, which I have done, but it's got its own problems. In all it's probably simpler all around to wave goodbye to that potential 40%; it means my systems language takes 0.02 seconds longer to self-compile.

**Scripting Language** The need first came up in the late 80s, for a language *built-in* to my user-applications. There weren't as many around then, so it was natural to just create and implemement it myself. Now, it is an independent language, and has many features you just don't find in scripting languages. I'm quite proud of it.

**GMP Library** This is a big-number library. I wouldn't mind using it, given the easy availability of the 0.5-1MB DLL, but there is no repository for that. You have to build it yourself, using a build-process that involves running 30Kloc BASH configure script - on Windows. You need to use CYGWIN or MSYS2 or (now) WSL. I tried a few times, and failed.

So, what to do? I don't want to build-in a dependency on a library that is slow elusive. I wrote my own library, about 1.6Kloc. It's not as fast, but is guaranteed to be available. (There is now a product called mini-GMP that provides reduced functionality, using only 2 C files. However it is apparently no faster than my library.)

