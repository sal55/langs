## Compiler Tests 1

This is a re-presentation of my initial compiler tests at [compiletest.txt](compilertest.txt).

This is a very simple test where a compiler is given repeated lines of:

    a = b + c * d
 
or equivalent, as either 20K, 100K, 500K or 2000K lines of code, and seeing what happens. The code will either be the whole program, or be in one function. When declared, a, b, c, d will be integers, and initialised to 1, 2, 3, 4. 

Table has been sorted in lines/second order.

Implementation | Language | 20K | 100K | 500K | 2000K | Compile time | Lines/second | Notes
---         | ---   | --- | --- | --- | ---  | ---   | ---   | ---
Clox        | Lox   | Y | Y | Y | Y | 1.0   | 2000 Klps     | 
LuaJIT      | Lua   | Y | Y | Y | Y | 1.6   | 1250 Klps     |  \*
Wren        | Wren  | Y | Y | Y | Y | 2.2   | 909 Klps  | 
QQ          | Q     | Y | Y | Y | Y |  2.4  | 833 Klps  | \* 
Lua         | Lua   | Y | Y | Y | Y | 2.8   | 714 Klps  | \*
TinyC       | C     | Y | Y | Y | Y | 3.3   | 606 Klps  | \*
Vox         | Vox   | Y | Y | Y | Y | 5.4   | 370 Klps  | 
MM          | M     | Y | Y | Y | Y | 6.9   | 288 Klps  | \* 
Ruby192     | Ruby  | Y | Y | Y | Y | 17.7  | 113 Klps  | 
Perl        | Perl  | Y | Y | Y | Y | 19.6  | 102 Klps  | 
eui         | Euphoria  |  Y | Y | Y | Y    | 21.0  | 95 Klps   | 
CLISP       | Lisp  | Y | Y | Y | Y | 22.0  | 91 Klps   | 
Python3.8.1 | Python | Y | Y | Y |  Y | 6.6     | 76 Klps   | \* Timed out at 2000K
Go          | Go    | Y | Y | Y | Y | 31.0  | 64 Klps   | 
lccwin      | C     | Y | Y | Y | - | (34.8)   | 57 Klps   |  Machine OOM at 2000K
gcc-opt     | C     | Y | Y | Y | - | (48)  |  42 Klps  | Timed out at 2000K
g++8.1-opt  | C     | Y | Y | - | - | (54)   | 37 Klps   |  (Not tested above 100K)
PyPy        | Python    | Y | Y | Y | -     | (61.2)  | 33 Klps   | * Timed out/became unstable at 2000K
lccwin-opt  | C     | Y | Y | Y | Y | 67  |  30 Klps
Clang       | C     | Y | Y | Y | - | (90)  | 22 Klps   |  Machine OOM at 2000K
MSVC        | C     | Y |  Y | - | -    | (124)   | 16 Klps   |\*  Timed out at 500K
MSVC-opt    | C     | Y | Y | - | - | (124)   | 16 Klps   |\*  Timed out at 500K
PellesC     | C     | Y | Y | Y | - | (140)  | 14 Klps   |  Reported OOM at 2000K
Gnat        | Ada   | Y | Y | - | - | (260)  | 7.7 Klps   | Abandoned at 500K at 3 mins
Clang-opt   | C     | Y | Y | Y | - | (292)  | 6.8 Klps  | 
A68G        | Algol68   | - | - | - | - | (300)   | 6.7 Klps  | For 10K (OOM on 20K)
Dart        | Dart  | Y | Y | Y | - | (298)  | 6.7 Klps  | (2000K not attempted)
g++8.1.0    | C     | Y | Y | Y | - | (386)  | 5.2 Klps  |  (Not tested above 100K)
V           | Vlang     |  Y | Y | - | - | (400)  | 5 Klps    | (500K+ not attempted)
Nim-CC      | Nim   | Y | Y | - | - |  (500)     | 4 Klps    | Timed out (Nim to C only)
Rustc       | Rust  | Y | - | - | - | (520)  | 3.8 Klps  |  Timed out at 100
Julia       | Julia     | Y | Y | Y | - | (528)     | 3.8 Klps  |  (2000K not attempted)
gcc-8.1.0   | C     | Y | Y | Y | - | (548) | 3.6 Klps  |   Machine OOM at 2000K
Zig         | Zig   | Y | Y | - | - | (800)  | 2.5 Klps  |  Machine OOM on 500K
DMC         | C     | - | - | - | - | ---   | 2.2 Klps  | * Crashed on 20K
FBC         | Basic     | Y | - | - | - | (900)   | 2.2 Klps  |  Timed out at 100K
Javac       | Java  | - | - | - | - |--- | 1.5 Klps     |  'Code too large' on 20K
Nim+CC      | Nim   | Y | - | - | - | (1600) | 1.2 Klps  |  Out of memory (Nim to C + C compilation)
PellesC-opt | C     | Y | - | - | - | (2500)  | 0.8 Klps  |  Timed out at 100K
DMD         | D     | Y | - | - | - | (2870)  | 0.7 Klps  |  Timed out on 100K
Rustc-O     | Rust  | Y | - | - | -     | (3070)  | 0.65 Klps     | Timed out at 100K
DMC         | C     | - | - | - | -     | --- | 0.25 Klps     | * Crashed on bigger inputs 
Pico-C      | C     | - | - | - | -     | -     | 0     | * Various errors
FPC         | Pascal    | - | - | - | - | -     | 0     |  (Proc too complex)



### 20K 100K 500K 2000K

A **Y** in that column indicates it managed to complete. Some products that failed 20K were tested with smaller inputs.

### Compile Time

How long it took, or would have taken, to compile 2000K lines.

Values in brackets are linearly extrapolated, as not all managed the full 2000K lines (or it would have taken too long). However, that assumes compile-time increases linearly; many will have exponential increases in compile-time as input sizes get bigger.

### Lines per Second

The code in this benchmark probably represents a higher code density than is typical. So true lines/second figures are likely to be higher.

But it is anyway just to give an idea of how an implementation copes.

### Test Machine

All test were run on a 2010 Windows PC with an AMD processor and spinning hard drive. Not high end, but all were tested on the same hardware. Tests are repeated to benefit from file caching.

### Assemblers

These haven't been included. It is assumed that 'a=b+c\*d' can be written on one line with all these HLLs, but in an assembler can take multiple lines, depending on how well a HLL compiler can generate code. Assemblers need their own benchmarks.

### Optimising Compilers

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

