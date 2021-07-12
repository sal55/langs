## Compiler Tests 1

This is a re-presentation of my initial compiler tests at [compiletest.txt](compilertest.txt).

This is a very simple test where a compiler is given repeated lines of:

    a = b + c * d
 
or equivalent, as either 20K, 100K, 500K or 2000K lines of code, and seeing what happens. The code will either be the whole program, or be in one function. When declared, a, b, c, d will be integers, and initialised to 1, 2, 3, 4. 

Table has been sorted in lines/second order.

Implementation | Language | Max LoC | Compile time | Lines/second | Notes
--- | --- | --- | --- | --- | ---
Clox | Lox | 2000K | 1.0 | 2000 Klps | 
LuaJIT | Lua | 2000K | 1.6 | 1250 Klps |  \*
Wren | Wren | 2000K | 2.2 | 909 Klps | 
QQ | Q | 2000K | 2.4 | 833 Klps | \* 
Lua | Lua | 2000K | 2.8 | 714 Klps | \*
TinyC | C | 2000K | 3.3 | 606 Klps | \*
Vox | Vox | 2000K | 5.4 | 370 Klps | 
MM | M | 2000K | 6.9 | 288 Klps | \* 
Ruby192 | Ruby | 2000K | 17.7 | 113 Klps | 
Perl | Perl | 2000K | 19.6 | 102 Klps | 
eui | Euphoria | 2000K | 21.0 | 95 Klps | 
CLISP | Lisp | 2000K | 22.0 | 91 Klps | 
Python3.8.1 | Python | 500K | 6.6 | 76 Klps | \* Timed out at 2000K
Go | Go | 2000K | 31.0 | 64 Klps | 
lccwin | C | 500K | 8.7 | 57 Klps |  Machine OOM at 2000K
gcc-opt | C | 500K | 12.0 |  42 Klps | Timed out at 2000K
g++8.1.0-opt | C | 100K | 2.7 | 37 Klps |  (Not tested above 100K)
PyPy | Python | 500K | 15.3 | 33 Klps | * Timed out/became unstable at 2000K
lccwin-opt | C | 2000K | 67.0 |  30 Klps
Clang | C | 500K | 22.5 | 22 Klps |  Machine OOM at 2000K
MSVC | C | 100K | 6.2 | 16 Klps |\*  Timed out at 500K
MSVC-opt | C | 100K | 6.2 | 16 Klps |\*  Timed out at 500K
PellesC | C | 500K | 35.0 | 14 Klps |  Reported OOM at 2000K
Clang-opt | C | 500K | 73.0 | 6.8 Klps | 
A68G | Algol68 | 10K | 1.5 | 6.7 Klps |  (OOM on 20K)
Dart | Dart | 500K | 74.5 | 6.7 Klps | (2000K not attempted)
g++8.1.0 | C | 100K | 19.3 | 5.2 Klps |  (Not tested above 100K)
V | Vlang | 100K | 20.0 | 5 Klps | (500K+ not attempted)
Nim-CC | Nim | 100K |  25.0 | 4 Klps | Timed out (Nim to C only)
Julia | Julia | 500K | 132.0 | 3.8 Klps |  (2000K not attempted)
Rustc-O | Rust | 20K | 5.20 | 3.8 Klps |  Timed out at 100
gcc-8.1.0 | C | 500K | 137.0 | 3.6 Klps |   Machine OOM at 2000K
Zig | Zig | 100K | 40.0 | 2.5 Klps |  Machine OOM on 500K
DMC | C | 10K | 4.5 | 2.2 Klps | * Crashed on 20K
FBC | Basic | 20K | 9.0 | 2.2 Klps |  Timed out at 100K
Javac | Java | 5K | 3.2 | 1.5 Klps |  'Code too large' on 20K
Nim+CC | Nim | 20K | 16.0 | 1.2 Klps |  Out of memory (Nim to C + C compilation)
PellesC-opt | C | 20K | 25.0 | 0.8 Klps |  Timed out at 100K
DMD | D | 20K | 28.7 | 0.7 Klps |  Timed out on 100K
Rustc | Rust | 20K | 30.7 | 0.65 Klps | Timed out at 100K
DMC | C | 5K | 20.0 | 0.25 Klps | * Crashed on bigger inputs 
Pico-C | C | Fail | - | 0 | * Various errors
FPC | Pascal | Fail | - | 0 |  (Proc too complex)

### Max LoC

The size of the biggest input it managed from 20K, 100K, 500K or 2000K lines of code. Some products that failed 20K were tested with smaller inputs.

### Compile Time

How long it took to compile its input. These values were not directly comparable, as not all managed the full 2000K lines (or it would have taken too long).

It is not possible either to extrapolate the times to how long it *would* have taken to compile 2M lines of input, since compile time sometimes increases exponentially.

### Lines per Second

The code in this benchmark probably represents a higher code density than is typical. So true lines/second figures are likely to be higher.

But it is anyway just to give an idea of how an implementation copes.

### Test Machine

All test were run on a 2010 Windows PC with an AMD processor and spinning hard drive. Not high end, but all were tested on the same hardware. Tests are repeated to benefit from file caching.

### Assemblers

These haven't been included. It is assumed that 'a=b+c\*d' can be written on one line with all these HLLs, but in an assembler can take multiple lines, depending on how well a HLL compiler can generate code. Assemblers need their own benchmarks.

### Optimised Compilers

Some get much better results when optimised, but probably they are discarding a lot of the code so get an unfair advantage. Real code is not redundant like this is.

### Date of Test

Not recorded, but tests have been done over past 18 months. Newer versions may be faster or may cope with bigger inputs.

"\*" in Notes column indicates tests done July 2021.

### 'M' and 'Q' Languages

These are my own languages (M compiles to native code, Q is an interpreter), both compilers are built with MM. This has a poor optimiser which puts them at a little disadvantage. (Transpiling to C and using an optimising compiler is a possibility - I assume most others are the best available production compilers too, but is less satisfactory.)

### Interpreters

Some are interpreters. Here the timings usually include running the program, but this part is usually neglible.

### Lox/Clox

Clox does extremely well here (Lox interpreter in C). I believe [Lox](https://craftinginterpreters.com/the-lox-language.html) is a simple, limited language designed for education, so it's possible that that has helped here. Or it may just be a very fast interpreter! It makes for something to aim for anyway, and shows how effortless this stuff can be compared with some implementations that appear to struggle.

### General

Some implementations such as Rustc and Julia have improved significantly, especially Rust (which used to take 20 seconds for only 1K lines when I first tried it). Maybe there is a purpose to such apparently pointless benchmarks, as they can highlight issues not apparent with smaller and more sensible inputs.

This benchmark is a form of stress test. While not real code, monolithic blocks of code inside one function like this, can be created with machine translation.

