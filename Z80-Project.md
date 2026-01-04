## Language Tools for the Z80

The Z80 is an 8-bit microprocessor from the 1970s that I haven't used for some decades. I wanted to have a go at programing the device again.

The longer aim was to see if I could target it from my 'M' systems language, normally 64-bit-based and targeting the x64 processor.

Since I don't have Z80 hardware, and I've long lost the knack of building such systems, my Z80 will be emulated.

### A Z80 Assembler

This seemed easiest to start with, to refamiliarise myself with the architecture. This was written in my interpreted scripting language, since performance was not an issue. The largest program that can be submitted might be 64Kloc of 1-byte instructions, and that can be assembled in about 1/6th of a second. Also needed was a disassembler, to display, for cross-checking, a listing of machine code with its disassembly.

At this point, the project was shelved for a year or two, as there were too many further obstacles. I needed an emulator program, but even for such a simple processor, it could run into thousands of lines of intricate code, from the C examples I'd seen. I no longer had Z80 coding skills, and targeting it from a HLL seemed a mammoth task.

### The Emulator

I had actually started a simple emulator, as part of the assembler, that implemented a handful of instructions. But in late 2025 I decided the project should be taken more seriously; it needed to be written in my systems language in order to get a decent emulation speed.

Also, I'd never written a CPU emulator before, so I wanted to exercise my language which had special features for such dispatch loops.

One tricky part of the Z80, apart from its almost total non-orthogonality, was getting the flags right. And for this I extracted bits of code from a C emulator, so that I could do all 256x256 combinations of 8-bit ADD say, using my code, and see if I ended up with the same set of flags as the C code. Calculating flags was also something that threatened to slow it all down. Probably 90% of emulation time for 8-bit add was working out the flags. (Fortunately common instructions like LD don't set flags.)

Anyway, it eventually went very well, although I didn't cover 100% of instructions. I left out odd ones I was never likely to use, BIT instructions, and all the unofficial ones.

So, at this point, I could write small Z80 assembly programs manually, and assemble and run them. And sometimes, programs from other sources, if I tweaked the assembly. (As it happens, the Clang-Z80 C compiler, which I ran on godbolt to give me ASM examples to test, likes to use unofficial instructions, which was nuisance. While "SDCC -mz80" generated non-standard syntax anyway.)

### Emulating a 'System'

A computer isn't just the CPU, there will be memory, support chips, peripherals, ports, possibly a memory-mapped video display.

For my project I decided to keep it simple and have only the CPU plus 64KB memory. For getting output, I added a SYSCALL feature to call into the host emulator, to enable various kinds of 'print', or for primitive heap allocation. Later much of that would be handled by Z80 code.

### Adapting my 'M' Compiler for the Z80

#### Changes to the IL

I wanted to use the same IL as used for the x64 target (and also briefly on an ARM64 target, and also for my C compiler project). Originally it was specific to my language which was 64-bit-centric. A year ago or more I revised it to be general: operations could work on any types (so ADD i16  for example, not just ADD i64). There was also no implicit widening of operands; it had to be explicit.

I had had a potential small-device target (eg. Z80) in mind when I made those changes. More recently, the IL was simplified; it was given a smaller role; and the number of instructions were almost halved. Now, a Z80 port seemed more practical.

#### Changes to the Front-end M Compiler

Simply having an IL-Z80 backend wouldn't work. Z80 can't practically support 64-bit or 32-bit data. 'int' and 'word', signed/unsigned types that defaulted to 64 bits, would have to be 16 bits.

But also, my M language normally widens narrow integers to the default 'int' size for calculations. This would be too costly if every 8-bit value had to be widened. So I dropped this for the Z80 target. Widening only happens for mixed-size arithmetic, or if explicitly converted.

I decided also not to deal with floating point for now, and only reserved a suitable type (f32, which would likely only work with 24 of those bits).

Supported data types are therefore `u8 i16 u16`, plus pointers, plus aggregate types.

### A Three-Program Solution

My compilers have generally been self-contained one-file executables. For this Z80 project, it actually needs 4 programs, not even three:

* The front-end compiler MZ turns M programs into ZA source code
* The ZA assembler turns Z80 source code into a 'Z' binary format (needs need that fourth program, the interpreter for the scripting language)
* The ZZ emulator which runs that binary code

This can all be orchestrated, transparently, from the MZ compiler. This is not a production system, it is just for interest.

BTW that 'Z' executable format is probably as simple as you can get. The assembler generates code and data within an in-memory 64KB memory image. It then writes that out as a 64KB binary. The emulator loads that 64KB file into a 64KB block of memory which becomes the Z80's system RAM. It starts executing from location 0000 in that RAM.

Compare with formats like EXE. (There is no linking involved; that's not relevant for my compiler, even when there are multiple modules.)

### Emulation Speed

Even using my non-optimised language (but with its special feature) and my low-spec PC, emulation speed is fast. Z80 programs run, on the PC, hundreds of times faster than on the actual chip.

The original Z80 was available with clock speeds of 2.5, 4, 6 and I think 8MHz. Most of the time I used a 4MHz part.

The emulation however is equivalent to a Z80 running at 1.5GHz to 4.5GHz. Most of the time it is around 2GHz. It can be calculated exactly as the emulator keeps a precise tally of the clock-ticks for each instruction.

So Z80 programs can run at very fast speeds on the PC, not so fast on the original hardware, but it is partly down to the quality of the code generated by my backend. It's not great, but not that terrible either.

I'd also forgotten how slow the Z80 could be! My current interpreted language can run the same program three magnitudes faster on the PC, and my systems language four magnitudes faster. *Plus* they are working with 64-bit quantities not 16-bit.

And yet, an early 80s Z80-based computer could still loads lots of useful work.

**Note** Some instructions are fiddly to emulate but only took a small number of clock ticks in the original hardware. Others are easy but took lots of clicks. So emulation speed depends on the mix of instructions.

### Example Program

This uses the Fannkuch benchmark, with a low N of 9 to make it suitable for 16 bits (N=10 or more would overlow it). First it runs the program using my x64 compiler, then it runs *the same source file* on the emulated Z80:
````
c:\zx>tm mm -r fann               # run-from-source in x64 Windows PC
Compiling fann.m to fann.(run)
Pfannkuchen(9) = 8629 30
TM: 0.06

c:\zx>mz -r -ss fann              # Do the same via Z80 compiler and emulator (-ss shows size)
Compiling fann.m to fann.za
Assembling fann.za to fann.z
 Code size:   1089 bytes
Run fann.z:
--------------------------------------------------
Pfannkuchen(9) = 8629 30
--------------------------------------------------
Stopped @ PC:               0476
Clock ticks:       4,086,582,027
Instrs:              346,079,427
Time on PC:                 2.10 secs
Time on 4MHz Z80:        1021.65 secs
Emulated Z80 runs at:       1946 MHz
````
I don't too have many examples of small programs; bigger ones may use floating point, need wider integers, or need library routines or files.

But I have experimented with a simple lexer; a toy Pascal interpreter (just the backend working on precompiled bytecode); and a toy decimal bigint library, that implements only unsigned ADD and MUL. (Apparently working out 1000! - a 2500-digit number - would take 40 minutes on a 4MHz Z80, although that's with my poor code, and poor algorithm.)
### Inline Assembly

This is a feature that I recently got rid of from my main compiler. There was too little need for it on a modern PC. For the Z80 however it would be indispensible.

I've no plans to add that. Since the MZ backend is simplistic, in that it generates ASM source code, it would be easy enough to simply append separately-written Z80 assembly code to the output of the compiler, which is always one ASM file.

(The MZ compiler still retains most features of the x64 compiler, which include a module scheme, and whole-program compilation. Then output is always one file. But the IL-Z80 backend currently has some restrictions.)

### Comparison With the 1981 Compiler

That was my first attempt at a compiler for my own language. The Z80 system was also homemade, with no reliable means of storage. So the compiler was memory-resident.

I can't remember the size, but the compiler binary, editor binary, and source of the program being developed, shared 16KB of RAM. That RAM could be write-protected when doing test runs. Another 16KB contained the compiled binary, and memory needed for its data. (Plus 9KB more of video memory.)

My MZ compiler is 200KB. It needs a 45KB assembler, and the emulator is 55KB.

Further, the 1981 system supported 24-bit floating point type. (The programs I played with included 3D vector graphics and basic image processing.)

In all, it's much more impressive than my 2025 version! Given that it had to run on the Z80 itself. (I later did more sophisticated versions, on systems with floppy storage and more memory, that were somewhat more conventional. But they still ran on the same chip.)


### Further Work

This is not clear ATM. I wasn't sure about the point of this at first, another than as a decent test for the design of my IL, in being able to target something so diverse.

But seeing the Z80 come alive on my PC was great.

Perhaps I will emulate a more complete system, with its own terminal and graphical windows. I might also see if I can slow down the emulation to the original speed.

(I had thought briefly about running the Z80 emulator itself on the Z80, but there would be certain problems to solve. One of which is how I can emulate a 64KB system, on a machine which only has 64KB in total.)

### Related Projects

The Z80 is a device I used extensively. There were other microprocessors of that era which I never got round to using, since the 8086 started to take over. So perhaps try and emulate one of those. But probably not the 6502: that looks hard work just to code in assembly, let alone be a HLL target.
