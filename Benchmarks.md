## Benchmarks

This runs some a selection of programs+tasks to compare my new 'BB' compiler for my M systems language, with gcc-O3:

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
**mandel** | Plot Mandelbrot set
**ax** | x64 assembler
**bcc** | C compiler
**pc** | 'PC' bytecode interpreter
**mm** | M compiler (M is my systems language. MM is the current compiler (special cut-down version that can be C-transpiled), BB is new one.)
**(Misc)** | (Collection of very small benchmarks (fibonacci etc))

### Tasks

Benchmark | Tasks
--- | ---
**JPEG/87M** | Decode 87MPixel colour image
**CLEX/SQL** | Scan sqlite3.c (100 times) and count chars/lines/tokens
**PI/2K** | Calculate 2000 digits of pi
**MANDEL/6M** | Plot into 3072x2048 8-bit greyscale
**AX/2M** | Assemble 2M lines of 'mov eax,[ebx+ecx*2+123456]'
**BCC/SQL** | Compile sqlite3.c+shell.c (246Kloc) into sqlite3.exe
**BCC/1M** | Compile 1M lines of a=b+c*d;
**BCC/LUA** | Compile 34 modules of Lua sources to lua.exe
**PC/CLEX** | Scan sqlite3.c (10 times)
**PC/JPEG/2M** | Decode 2MPixel colour image
**MM/1M** | Compile 1M lines of 'a:=b+c*d'

(Benchmarking my compilers/assemblers with real programs is difficult because
they are already so fast. However the purpose of these tests is to make the
/generated/ code faster.)

### Testing

All programs are written in my M language. I use a version of my M compiler (MC)
to translate all those to C, and compile with gcc, example:

    C:\ax>mc -opt ax
    Compiling ax.m to ax.exe
    Invoking C compiler: gcc -m64 -O3 -oax.exe ax.c

So, none of the programs are in pure C, but generated C. This could
possibly affect the ability of gcc to optimise. However I have a version
of the 'pi' benchmark in pure C, and that gives the same timing.

All gcc tests are with -O3 option.

### Benchmark results.

Results in are seconds of runtime (and, for CLEX benchmarks, in lines/second).

Benchmark | BB | GCC | Notes
--- | --- | --- | ---
**JPEG/87M** | 9.3 | 4.4| 
**CLEX/SQL** |  4.3/5.1Mlps | 3.1/7.2Mlps | 
**PI/2K** |   4.5 |   0.8 | 
**MANDEL/6M** |   5.3  |  3.0 | 
**AX.2M**  |  1.6 |   1.2 | 
**BCC/SQL** |   0.7  |  0.5  |  (GCC takes 11-60 secs, TCC had errors)
**BCC/1M** |  8.1 |   5.1 |   (TCC takes 1.6 secs, GCC was aborted)
**BCC/LUA** |   0.4 |   0.3  |  (TCC also 0.3 secs, GCC 7-15 secs)
**PC/CLEX** |   9.9/222Klps | 4.7/462Klps | 
**PC/JPEG/2M** |  7.0 |   3.7 | 
**MM/1M**  |  5.0 |   3.5 | 
**(MISC)** | 35.1 |  16.4  |

(For the BCC tests, as it's a C compiler, that can also be compared directly with using gcc and tcc. Tcc is pretty fast, bit it's a one-pass compiler AIUI, while BCC is 2-3 passes, plus using intermediate ASM. MM has no intermediate ASM, but has more passes.)

