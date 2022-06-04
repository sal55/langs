## Compiler Tests III

### Testing compilation speed on large source files.

This test is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt); any missing ones can be found at the former link.

The benchmark is 50-100 lines, but is repeated 10,000 times for a single source file of 500K-1000K lines approx. In addition, each of the 10,000 copies of the function, which have random names, are called. Not calling them would mean some compilers cutting corners.

I've concentrated on compilers generating native code, as interpreted languages need different criterea, but I've included also Seed7 and Julia since those can generate programs that can run at native code speed.

Implem | Language | Time (Note) | Funcs/sec | Exe Size 
--- | --- | --- | --- | ---
**Rustc -O** | Rust  | 79000 secs (1) | 0.13 | 10MB
**Zig -O ReleaseFast** | Zig | 11000 (9) | 1.1 | 7.5MB
**Julia**    | Julia  | 1320 (2) |  7.60  | ---
**Clang -O3**        | C | 780 | 13 | 16MB
**Clang -O2**        | C | 650 | 15 |  ---
**Zig** | Zig | 440 |22.7 | 20MB
**Rustc** | Rust  | 330 | 30 | 40MB
**Clang -O1**        | C | 310 | 15 | ---
**Dart**          | Dart | 235 (8)| 42 | 27MB
**DMD -O**       | D | 156 | 64 | 15MB
**MSVC /O2**          | C | 155 |64 | 0.2MB
**Odin -opt**        | Odin | 104 (3) | 96 | ---
**gcc -O3**           | C            | 85 | 118 | 0.93MB
**gcc**           | C            | 67 | 150 | 10MB
**Go**            | Go | 40 | 250 |  10MB
**DMD**           | D | 32 | 310 | 16MB
**Clang**         | C | 30 | 330 | 12MB
**s7c -O2**       | Seed7 | 27 (4) | 370 |---
**Javac**      | Java | 25 (5) | 400 | 0.12MB
**MSVC**          | C | 12  |830 | 9.2MB
**Odin**        | Odin | 14 | 720 | ---
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 5.7 | 1750 | 10MB
**bcc** (mm)     | C     | 4.2 (6) | 2400 | 8.0MB
**MM -opt** (mm)   | M  | 2.4 (6)| 4200 | 6.1MB
**MM** (mm)       | M        | 2.1 | 4800 | 8.0MB
**Tiny C** (tcc)  | C    | 1.9 (7) | 5100 | 10MB
**Tiny C** (bcc)  | C        | 1.5 | 6700 | 10MB
**Tiny C**        | C        | 1.1 | 9100 | 10MB
**Tiny C**  (gcc) | C        | 0.9 | 11100 | 10MB

### Time

How many seconds it took to compile

### Funcs/sec

Because some took more lines than others, the table is sorted in terms of functions/second throughput. Fastest speed in Lines/second was something like 900Klps (tcc), and slowest around 1.2Klps (optimised Clang; I don't include the 0.01Klps of optimised Rust). Code density varies, but I think tcc still comes out on top.

### Optimisation

Optimised versions tried where the option existed and I knew how to to turn it on.

### Exe Size

The size of the binary executable. They range from 6.6MB to 40MB. There are some smaller ones: 0.9MB from gcc-O3, and 0.2MB from MSVC/O2, but they have most likely culled a lot of code.

### (1) Rust

Figures extrapolated from 1000 functions

### (2) Julia

The figures have been extrapolated from 1000 functions, as the compile time seems to increase linearly. The 4.0 runtime is with -O3 optimising, which surprisingly does not seem to affect compile times.

### (3) Odin

This failed my 10,000-function tests. It crashes on larger inputs. The figures shown are based on the 1000-function test

### (4) Seed7

The compile time here is how long it took s7c to turn the .sd7 source file into a C intermediate file. Fully building involves running a C compiler on the result, but the intermediate C contains 3.3M lines compared to 0.87M of the Seed7 source code.

### (5) Java

The executable (or rather the .class file) produced is suspiciously small at 120KB, or about 12 bytes per function. If I tried to get it to retain all the functions (using res+=fxxxx(5) on each call), then it aborted with 'Code too large', also at about 25 seconds. However, compiling 100 functions took 4 seconds (but maybe most of those are eliminated too) and Hello, World took 2 seconds. So maybe it's 50 functions/second. Output of Java is probably JVM code not native.

### (6) 'M'

MM is the compiler for my own systems language. BCC is my own C compiler.

(mm) against my compilers means it was compiled with my mm which only has a modest optimiser. MM compiled with gcc-O3, if it was expressed as C (no longer possible) would probably make it 30% faster.

### (7) Tiny C

This has several entries, which it deserves being the fastest compiler in the list; even the slowest is faster than mine.

(tcc) means it was compiled with itself; (bcc) compiled with my bcc; and (gcc) compiled with gcc-O3. The remaining entry is as it was downloaded, as a prebuilt binary.

### (8) Dart

This compiler took 6 seconds just to compile a Hello, World program (which generated a 5MB executable, which probably explains it!).

### (9) Zig

I think this ran out of memory on the unoptimised build, but it eventually completed and that timing is shown. The time for optimised is estimated based on it taking 25 times as long on 1000 functions compared with unoptimised.

The exe sizes are extrapolated from the 1000-function version (partly because I'd deleted the 10K function ones before recording the sizes).

### Host

All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

### Sorted by Runtime

To get an idea of the trade-offs involved, this lists how long each took for a single call to `fannkuch(11)` in a program with just that one function:

Implem | Language | Runtime
--- | --- | ----
**Zig Opt** | Zig | 2.2 secs
**Clang -O3**        | C | 2.45 secs
**Clang -O2**        | C | 2.5
**MSVC /O2**          | C | 2.6
**Clang -O1**        | C | 2.6
**MM -opt** (mm)   | M | 2.64
**Go**            | Go | 2.9
**Rustc -O** | Rust | 3.2
**gcc -O3**           | C  | 3.3
**Julia -O3**         | Julia | 4.0
**Javac**      |Java | 4.0
**DMD -O**       | D | 4.1
**Dart**          | Dart | 6.2
**Vox**           | Vox | 6.3
**MM** (mm)       | M   | 6.8
**gcc**           | C   | 8.7
**bcc** (gcc)     | C   | 9.0
**MSVC**          | C   | 9.6
**DMD**           | D   | 9.7
**Tiny C**        | C   | 10.1
**Clang**         | C   | 10.2
**Seed7 -O2**     | Seed7 | 13.0
**Zig** | Zig | 17.6
**Rustc**         | Rust| 37.8

One of my compilers (MM-opt) does surprisingly well. It's just a fluke (it's typically 50% slower than gcc-O3, not faster!), however that is the genuine timing for this test.

