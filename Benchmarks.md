## Benchmarks

These are a selection of programs and tasks to compare my new 'BB' compiler for my M systems language, with gcc-O3.

I've spent about 10 days trying to do the simplest and most obvious optimisations, and here are the results.

The primary aim was faster code: narrow the gap between BB and GCC, and widen it between BB and TCC. But a secondary one that also seems useful is to reduce the code size, and just make it less terrible.

The compilers tested are:

Compiler | ...
--- | ---
**GCC-O3** | Version 8.1.0 on Windows, always invoked with -O3
**BB-opt** | Latest version of my new M compiler
**BB-orig** | Original from when I started the process
**BCC** | (My current C compiler)
**TCC** | (Tiny C compiler)

The last two are included because I plan to port the new code-generator of BB to BCC, since at present,
while TCC is faster at compiling than BCC, BCC doesn't make up for it by generating significantly faster code.

### Benchmark Programs

Program | Decription
--- | ---
**Jpeg** | Jpeg decoder
**Clex** | Crude basic lexer for C source
**Pi** | Calculate pi digits using my bignum library
**Mandel** | Plot Mandelbrot set (added this to have some floating point tests)
**AX** | x64 assembler
**BCC** | C compiler
**PC** | 'PC' bytecode interpreter
**MM** | M compiler (M is my systems language. MM is the current compiler (special cut-down version that can be C-transpiled), BB is the new one.)
**(Misc)** | (Collection of very small benchmarks (fibonacci etc))

### Tasks

These have been tweaked from the original, to give more equal weighting.

Benchmark | Tasks
--- | ---
**Jpeg/87M** | Decode 87MPixel colour image
**Clex/SQL** | Scan 219Kloc sqlite3.c (100 times) and count chars/lines/tokens
**Pi/2K** | Calculate 2000 digits of pi
**Mandel/6M** | Plot into 3072x2048 8-bit greyscale image
**AX/2M** | Assemble 2M lines of 'mov eax,\[ebx+ecx\*2+123456\]'
**BCC/SQL** | Compile sqlite3.c+shell.c (246Kloc) into sqlite3.exe (5 times)
**BCC/500K** | Compile 500K lines of 'a=b+c\*d';
**BCC/Lua** | Compile 34 modules of Lua sources to lua.exe (10 times)
**PC/Clex** | Scan sqlite3.c (10 times)
**PC/Jpeg/2M** | Decode 2MPixel colour image
**MM/1M** | Compile 1M lines of 'a:=b+c\*d'

(Benchmarking my compilers/assemblers with real programs is difficult because
they are already so fast.)

### Testing

All programs are written in my M language. They are compiled with both BB, the new compiler, and with GCC (also TCC and BCC) using a version of the M compiler (MC) that translates them to C then invokes gcc, example:

    C:\ax>mc -opt ax
    Compiling ax.m to ax.exe
    Invoking C compiler: gcc -m64 -O3 -oax.exe ax.c

So, none of the programs are in pure C, but generated C. This could possibly affect the ability of gcc to optimise. However I have a version of the 'pi' benchmark in pure C, and that gives the same timing.

### Benchmark Results.

Benchmark | GCC-O3 | BB-Opt | BB-Orig | BCC | TCC | Notes
--- | --- | --- | --- | --- | --- | ---
**Jpeg/87M** | 4.3 | 6.5 | 9.2 | 11.3 | 18.0 | Seconds, rounded to 0.1 seconds
**Clex/SQL** | 3.0 | 4.0 | 4.1 | 4.7 | 6.7 | 
**Pi/2K** | 0.84 | 4.1 | 4.6 | 4.4 | 4.8 | 
**Mandel/6M** | 3.0 | 4.2 | 5.3 | 6.1 | 5.9 |
**AX/2M** | 1.1 | 1.4 | 1.6 | 1.6 | 1.8 | 
**BCC/SQL** | 2.2 | 2.9 | 3.2 | 3.3 | 3.7 | 
**BCC/500K** | 2.5 | 3.5 | 4.7 | 4.3 | 4.8 | 
**BCC/Lua** | 3.0 | 3.6 | 3.9 | 3.9 | 4.4 | 
**PC/Jpeg/2M** | 3.5| 5.2 | 7.0 | 6.5 | 9.0 | BB-Opt is 1.9 seconds, accelerated mode (see note)
**PC/Clex** | 4.8 | 7.6 | 9.9 | 9.4 | 12.6 | BB-Opt is 2.3 seconds, accelerated mode 
**MM/1M**  | 3.2 | 4.2 | 4.9 | 4.9 | 5.4 | 
**Misc** | 3.1 | 4.8 | 6.8 | 7.7 | 8.8 | (Misc micro-benchmarks, 20% of actual value)
--- | --- | --- | --- | --- | --- | 
**Average**  | 2.9 | 4.35 | 5.4  | 5.7 | 7.1  | seconds
**Average (excl 'pi')** | 3.1  | 4.37 | 5.5  | 5.8 | 7.4 |('Pi' result is not a typical program for me; needs further investigation)
**Rel to GCC** | 1.00  | 1.50 | 1.86 | 1.97 | 2.45 | How many times as slow as gcc-O3
**Rel to GCC (excl 'pi')** | 1.00 | 1.41 | 1.77 | 1.87 | 2.39
**Total EXE sizes** | 2426 | 1593 | 1793 | 1980 | 2283 | KB (not KiB)

**Notes**

* All executables include the M language's runtime libraries. Those compiled with BB or BCC do not include any C libraries (they use an external DLL MSVCRT.DLL). I don't know what is included in the programs compiled with GCC or TCC.

* (GCC-O1 is roughly 10% slower on these programs than -O3. That means BB-opt would be just over 30% slower on that second average.)

* GCC in all cases compiles a single monolithic C file. For the larger programs, this gives it the opportunity to do whole-program optimisations not otherwise possible when split across dozens of modules. So this might give it a small advantage.
* (PC program had one loop manually unrolled to take advantage of something in BB's code generator. This made an insignificant difference in gcc's timing, but a more useful one, 11%, in BB's timing for these two benchmarks.)

* The **PC** interpreter is normally used in 'accelerated' mode, which makes uses of an ASM overlay module. For the two tests here, they run about twice as fast as the gcc-compiled version, as shown above. They show that solutions using BB can be sometimes be faster than gcc.


### Summary

I spent a week and a half making the simplest kinds of optimisations. I didn't want to get
into serious academic algorithms, or get involved in the details of x64 instruction scheduling,
nor add dozens of new passes, nor significantly slow down the compiler, nor spend years on the process.

Just to get something a bit more respectable.

The general results are:

* Improved runtimes by 20% on average
* Reduced executable sizes by 12% from the original BB (and by 21% compared with my current M compiler)
* Much more compact-looking code
* No significant slowdown in compilation (10% slower estimated for final compiler).
* Compared with GCC-O3, I've gone about 1/3 of the way to matching GCC performance, for most of the programs here.

I've not made much headway with the 'Pi' benchmark; I will have to come back to that another time.
Part of it is that gcc optimises integer division by a constant (into multiplies and shifts), but I just do the division.

Note that for most of the programs I'm running (eg. compilers), the difference between typical BB and GCC runtimes
might be 0.1 seconds or less. However gcc would take dozens of times longer to build the program.

### Next

My compilers are whole-program ones so could offer some advanced optimisations including the ones GCC may have taken advantage of as mentioned above.

But that is something to come back to in the future. Maybe.




