## Benchmarks

These are a selection of programs and tasks to compare my new 'BB' compiler for my M systems language, with gcc-O3:

Compiler | ...
--- | ---
**BB** | My new M compiler, currently unoptimised
**GCC** | Version 8.1.0 on Windows, always invoked with -O3

### Benchmark Programs

Program | Decription
--- | ---
**jpeg** | Jpeg decoder
**clex** | Crude basic lexer for C source
**pi** | Calculate pi digits using my bignum library
**mandel** | Plot Mandelbrot set (added this to have some floating point tests)
**ax** | x64 assembler
**bcc** | C compiler
**pc** | 'PC' bytecode interpreter
**mm** | M compiler (M is my systems language. MM is the current compiler (special cut-down version that can be C-transpiled), BB is the new one.)
**(Misc)** | (Collection of very small benchmarks (fibonacci etc))

### Tasks

Benchmark | Tasks
--- | ---
**JPEG/87M** | Decode 87MPixel colour image
**CLEX/SQL** | Scan 219Kloc sqlite3.c (100 times) and count chars/lines/tokens
**PI/2K** | Calculate 2000 digits of pi
**MANDEL/6M** | Plot into 3072x2048 8-bit greyscale image
**AX/2M** | Assemble 2M lines of 'mov eax,\[ebx+ecx\*2+123456\]'
**BCC/SQL** | Compile sqlite3.c+shell.c (246Kloc) into sqlite3.exe
**BCC/1M** | Compile 1M lines of 'a=b+c\*d';
**BCC/LUA** | Compile 34 modules of Lua sources to lua.exe
**PC/CLEX** | Scan sqlite3.c (10 times)
**PC/JPEG/2M** | Decode 2MPixel colour image
**MM/1M** | Compile 1M lines of 'a:=b+c\*d'

(Benchmarking my compilers/assemblers with real programs is difficult because
they are already so fast. However the purpose of these tests is to make the
/generated/ code faster.)

### Testing

All programs are written in my M language. They are compiled with both BB, the new compiler, and with gcc using version of the M compiler (MC)
that translates themn to C then invokes gcc, example:

    C:\ax>mc -opt ax
    Compiling ax.m to ax.exe
    Invoking C compiler: gcc -m64 -O3 -oax.exe ax.c

So, none of the programs are in pure C, but generated C. This could possibly affect the ability of gcc to optimise. However I have a version of the 'pi' benchmark in pure C, and that gives the same timing.

### Benchmark results.

Results in are seconds of runtime (and, for CLEX benchmarks, in lines/second).

Benchmark | BB | BB-Opt | GCC | TCC
--- | --- | --- | --- | ---
**JPEG/87M** | 9.3 | 6.9 |4.4| 18.0 |
**CLEX/SQL** |  4.3/5.1Mlps| 4.1/5.4Mlps | 3.1/7.2Mlps | 6.0/3.6Mlps |
**PI/2K** |   4.5 | 4.2 | 0.8 | 4.8 |
**MANDEL/6M** |   5.3 | 4.4  |  3.0 | 5.9 |
**AX/2M**  |  1.6 | 1.4  |  1.1 | 1.7 |
**BCC/SQL** |   0.7 | 0.6  |  0.5  | 0.7 |
**BCC/1M** |  8.1 |7.2 |    5.1 |  9.6 |
**BCC/LUA** |   0.4 | 0.5 |    0.3  | 0.5 |
**PC/CLEX** |   9.9/222Klps | 8.7/252Klps | 4.7/462Klps | 12.3/179Klps |
**PC/JPEG/2M** |  7.0 |6.4 |    3.7 | 8.9 |
**MM/1M**  |  5.0 |4.2 |    3.5 | 5.4 |
**Totals**  |  56.1|49.2  |30.3 |  74.0 |
**(MISC)** | 34.0 |25.2|  15.6  | 43.3

(Timings vary by 1% or so all the time, and the figures here are rounded to one decimal. So I've tried to adjust them to be consistent, but they may not add up exactly. BB-Opt is the latest version, and at present it's 63% or so slower than gcc based going from the totals. It started at 85% slower.

Individual programs will vary considerably. I will need to create more tests where gcc is much better, like the 'PI' program here.)


### Conclusion

I'm wrapping this up after 8 days or so. I've seen no significant increase in speed for about 4 days. But the code is now much tidy and leaner, and no longer so embarrassing to look at. Executables are about 1/6th smaller than the ones from my current working compiler.

Still lots of small stuff that can be done, mainly to do with more efficiently working with register-based variables. But I think x64 processors have already got that taken care of; they do a good job of making poor-looking code run faster than it ought.

Current set of comparisons presented here relative to gcc, to allow a better weighted total since some programs only took 0.4 seconds while some took 20 times as long:

Benchmark | GCC-O3 | BB-opt | BB original | TCC
--- | --- | --- | --- | ---
**JPEG/87M** | 1.0 | 1.55 | 2.11 | 4.09
**CLEX/SQL** | 1.0 | 1.33 | 1.38 | 1.93
**(PI/2K)** | 1.0 | 5.0 | 5.6 | 6.00
**MANDEL/6M** | 1.0 | 1.47 | 1.76 | 3.00
**AX/2M**  |1.0 | 1.28 | 1.45 | 1.54
**BCC/SQL** |1.0 | 1.36 | 1.40 | 1.40
**BCC/1M** | 1.0 | 1.46 | 1.59 | 1.88
**BCC/LUA** | 1.0 | 1.19 | 1.30 | 1.67
**PC/CLEX** | 1.0 | 1.74 | 2.10 | 2.61
**PC/JPEG/2M** | 1.0 | 1.63 | 1.90 | 2.40
**MM/1M**  | 1.0 | 1.34 | 1.42 | 1.54
**MISC**  | 1.0 | 1.61 | 2.17 | 2.77
**Average** | 1.0 | 1.45 | 1.68 | 2.25

Average excludes the PI/2K benchmark.

BB-opt is with optimisation enabled, but a lot of it happens regardless; turning it off only makes it 10% slower. (-opt enables a second pass on each function when translating intermediate code to native code. The first pass gathers info to allow it to determine which parameters and locals can be kept in registers).

Weighted total is 1.45 as slow than gcc, not including the PI/2K benchmark. That one will need looking at in detail at some point to see what's going on; it won't be affected by the little tweaks I've been doing. (One factor there is that gcc optimises division by a constant, which I don't deal with. But that would only improve it by an estimated 25%)

Note that the PC program is normally run in ASM-accelerated mode, only available right now in my non-optimised MM current compiler (BB does not support inline ASM yet). In that mode, the tests perform at 0.6 and 0.5 respectively compared with gcc (may be even better when used with BB-opt).

Overall, perhaps 15% faster peformance compared with my current compiler, and coincidentally about 15% smaller executables.

### Aims

* Produce a worthwhile speed-up to my compiler, to narrow the gap between it and gcc-O3, and to widen it between it and Tiny C
* Do so without significantly slowing down the compiler (I estimate 10% slow-down on the final compiler)
* Do so without using any complicated, academic algorithms or dozens of passes; using only the most basic techniques
* Do so without getting involved in the gritty details of processor intruction schedule. (However, one micro-benchmark is 30% slower with -opt than without!)
* To reduce program size (it wasn't an original aim; but it is now)
* To produce more respectable-looking code
