## Language Tools for the Z80

The Z80 is an 8-bit microprocessor from the 1970s that I haven't used for some decades. I wanted to have a go at programing the device again.

The longer aim was to see if I could target it from my 'M' systems language, normally 64-bit-based and targeting the x64 processor.

Since I don't have Z80 hardware, and I've long lost the knack of creating such systems (which is anyway more awkward these days), my Z80 will be emulated. Which is another obstacle to deal with.

### A Z80 Assembler

This seemed easiest to do, to refamiliarise myself with the architecture. This was written in my interpreted scripting language, since performance was not an issue. The largest program that can be submitted might 64Kloc of 1-byte instructions, and that can be assembled in about 1/6th of a second. Also needed was a disassembler, to display a listing of machine code with its disassembly.

At this point, the project was shelved for a year or two, as there were too many further obstacles. I needed an emulator program, but even for such a simple processor, it could run into thousands of lines of intricate code, from the C examples I'd seen. I also had no Z80 coding skills, and targeting from a HLL seemed a mammoth task.

### The Emulator

I had an actually started a simple emulator, also in scripting code, that implemented a handful of instructions. But in late 2025 I decided the project should be taken more seriously, and needed to be written in my systems language in order to get a decent emulation speed.

Also, I'd never written a CPU emulator before, so I wanted to exercise my language which had special features for such dispatch loops.

One tricky part of the Z80, apart from its almost total non-orthogonality, was getting the flags right. And for this I extracted bits of code from a C emulator, so that I could do 256x256 combinations of 8-bit ADD say, using my code, and see if I ended up with the same set of flags as the C code. Calculating flags was also something that threatened to slow it all down. Probably 90% of emulation time for 8-bit add was working out the flags. (Fortunately common instructions like LD don't set flags.)

Anyway, it eventually went very well, although I didn't cover 100% of instructions. I left out odd ones I was never likely to use, BIT instructions, and all the undocumented ones.

So, at this point, I could write small Z80 assembly programs manually, and assemble and run them. And sometimes, programs from other sources, if I tweaked the assembly. (As it happens, the Clang-Z80 C compiler, which I ran on godbolt to give me ASM examples to test, likes to use unofficial instructions, which was nuisance. While "SDCC -mz80" generated non-standard syntax anyway.)

### Emulating a 'System'

A computer isn't just the CPU, there will be memory, support chips, peripherals, ports, possibly a memory-mapped video display.

For my project I decided to keep it simple and have only the CPU plus 64KB memory. For getting output, I added a SYSCALL feature to call into the host emulator, to enable various kinds of 'print', or for primitive heap allocation. Later much of that would be handled by Z80 code, but I'd still need to decide what to about a proper display for example, or even storage.

### Adapting my 'M' Compiler for the Z80

#### Changes to the IL

I wanted to use the same IL as used for the x64 target (and also briefly on an ARM64 target, and also for my C compiler project). Originally it was specific to my language which was 64-bit-centric. A year ago or more I revised it to be general: operations could work on any types (so ADD I16 for example, not just ADD I64). There was also no implicit widening of operands; it had to be explicit.

I had had a potential small-device target (eg. Z80) in mind when I made those changes. More recently, the IL was simplified, it was given a smaller role, and the number of instructions were almost halved.

Now, a Z80 port seemed more practical.

#### Changes to the Front-end M Compiler

Simply having an IL-Z80 backend wouldn't work. Z80 can't support 64-bit or 32-bit data. 'int' and 'word', signed/unsigned types that defaulted to 64 bits, would have to be 16 bits.

But also, my M language normally widens narrow integers to the default 'int' size for calculations. This would be too costly if every 8-bit value had to be widened. So I dropped this for the Z80 target. Widening only happens for mixed-size arithmetic, or if explicitly converted.

I decided also not to deal with floating point for now, and only reserved a suitable type (f32, which would likely only work with 24 of those bits).

### A Three-Program Solution

My compilers have generally been self-contained one-file executables. For this Z80 project, it actually needs 4 programs, not even three:

* The front-end compiler MZ turns M programs into ZA source code
* The ZA assembler turns ZA source code into a 'Z' binary format (needs need that fourth program, the interpreter for the scripting language)
* The ZZ emulator which runs that binary code

This can all be orchestrated, transparently, from the MZ compiler. This is not a production system, it is just for interest.

### Emulation Speed

Even using my non-optimised language (but with its special feature) and my low-spec PC, emulation speed is fast. Z80 programs run, on the PC, hundreds of times faster than the actual chip.

The original Z80 was available in speeds of 2.5, 4, 6 and I think 8MHz. Most of the time I used a 4MHz part.

The emulation however is equivalent to a Z80 running at 1.5GHz to 4.5GHz. Most of the time it is something over 2GHz. It can be calculated exactly as the emulator keeps a precise tally of the clock-ticks for each instruction.

So Z80 programs can run at very fast speeds on the PC, not so fast on the original hardware, but is partly down to the quality of the code generated by my backend. It's not great, but not
that terrible either.

I'd forgotten how slow the Z80 could be. My current interpreted language can run the same program three magnitudes faster on the PC, and my systems language four magnitudes faster. *Plus* they are working with 64-bit quantities not 16-bit.

And yet, an early 80s Z80-based computer could so lots of useful work.

### Example Program

This uses the Fannkuch benchmark, with a low N of 9 to make it suitable for 16 bits (10 or more would overlow 16 bits). First it runs the program using my x64 compiler, then it runs *the same source file* on the emulated Z80:















