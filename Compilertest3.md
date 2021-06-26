## Compiler Tests III

### Testing compilation speed on large source files.

This test is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt); any missing ones can be found at the former link.

The benchmark is 50-100 lines, but is repeated 10,000 times for a single source file of 500K-1000K lines approx. In addition, each of the 10,000 copies of the function, which have random names, are called. Not calling them would mean some compilers cutting corners.

I've concentrated on compilers generating native code, as interpreted languages need different criterea, but I've included also Seed7 and Julia since those can generate programs that can run at native code speed.

There is also information on the generated binary size, and an idea of the installation size.

Implem | Language | Time (Note) | Funcs/sec | Runtime | Exe Size | Inst Files | Inst MB
--- | --- | --- | --- | --- | --- | --- | ---
**Rustc -O** | Rust  | 79000 secs (1) | 0.13 | 3.2 secs| 10MB | 56800+14600 | 2000MB + 2800MB
**Julia**    | Julia  | 1320 (2) |  7.60  | 4.0  | --- | 1700 | 480MB
**Clang -O3**        | C | 780 | 13 | 2.45 | 16MB | 350 | 1600MB
**Clang -O2**        | C | 650 | 15 | 2.5 | --- | 350 | 1600MB
**Rustc** | Rust  | 330 | 30 | 37.8 | 40MB | 56800+14600 | 2000MB+2800MB
**Clang -O1**        | C | 310 | 15 | 2.6 | --- | 350 | 1600MB
**Dart**          | Dart (8) | 235| 42 | 6.2 | 27MB | 500 | 490MB
**DMD -O**       | D | 156 | 64 | 4.1 | 15MB | 4000 | 300MB 
**MSVC /O2**          | C | 155 |64 | 2.6 | 0.2MB | 14600 | 2800MB
**Odin -opt**        | Odin | 104 (3) | 96 | 2.7 | --- | 200 | 140MB
**gcc -O3**           | C            | 85 | 118 | 3.3 | 0.93MB | 4800 | 550MB
**gcc**           | C            | 67 | 150 | 8.7 | 10MB | 4800 | 550MB
**Go**            | Go | 40 | 250 | 2.9 | 10MB | 9200 | 350MB
**DMD**           | D | 32 | 310 | 9.7 | 16MB | 4000 | 300MB
**Clang**         | C | 30 | 330 | 10.2 | 12MB | 350 | 1600MB
**s7c -O2**       | Seed7 | 27 (4) | 370 | 13 | --- | 1400 | 550MB
**Javac**      | Java (5) | 25 | 400 | 4.0  |0.12MB | 400 | 330MB
**MSVC**          | C | 12  |830 | 9.6 | 9.2MB | 14600 | 2800MB
**Odin**        | Odin | 14 | 720 | 27.3 | --- | 200 | 140MB
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 5.7 | 1750 | [6.4](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx) | 10MB | 1 | 2.4MB (0.75MB upx)
**bcc** (mm)     | C (6)     | 4.2 | 2400 | 9.0 | 8.0MB | 1 | 1.0MB (0.32MB upx)
**MM -opt** (mm)   | M (6)  | 2.5 | 4000 | 3.1 | 6.6MB | 1 | 0.6MB (0.17MB upx)
**MM** (mm)       | M        | 2.2 | 4500 | 6.8 | 7.8MB | 1 | 0.6MB (0.17MB upx)
**Tiny C** (tcc)  | C (7)    | 1.9 | 5100 | 10.1 | 10MB | 120 | 1.8MB
**Tiny C** (bcc)  | C        | 1.5 | 6700 | 10.1 | 10MB | 120 | 1.8MB
**Tiny C**        | C        | 1.1 | 9100 | 10.1 | 10MB | 120 | 1.8MB (download version)
**Tiny C**  (gcc) | C        | 0.9 | 11100 | 10.1 | 10MB | 120 | 1.8MB

### Time

How many seconds it took to compile

### Funcs/sec

Because some took more lines than others, the table is sorted in terms of functions/second throughput. Fastest speed in Lines/second was something like 900Klps (tcc), and slowest around 1.2Klps (optimised Clang; I don't include the 0.01Klps of optimised Rust). Code density varies, but I think tcc still comes out on top.

### Optimisation

Optimised versions tried where the option existed and I knew how to to turn it on.

### Runtimes

This column has been added so that some trade-offs can be compared. Runtime is how long it took to execute:

    fann(11)
    
in a program containing just the one function.

### Exe Size

The size of the binary executable. They range from 6.6MB to 40MB. There are some smaller ones: 0.9MB from gcc-O3, and 0.2MB from MSVC/O2, but they have most likely culled a lot of code.

### Installation Files/MB

This gives an idea of the magnitude of the installation. The very large ones will come with lots of libraries, headers etc, but it is not practical to isolate what is needed to run this test.

Where the entire installation is one self-contained executable, then figures using UPX compression are shown as well.

### (1) Rust

Difficult to believe, but Rustc used to be a lot slower.

* Rust needs (or did when I tried it) MS VC++ build tools to work; so the installation size includes those tools

* The runtime for unoptimised Rust is poor: 10 times as slow as optimised code; I don't know why

* The optimised compile was aborted after 20 minutes. 100 functions took 18 seconds; 1000 functions took 1200 seconds. From this I extrapolated a figure of 79200 or 22 hours for a full optimised compile of 10,000 functions.

* The optimised executable size was extrapolated from that of the 1000-function version

### (2) Julia

The figures have been extrapolated from 1000 functions, as the compile time seems to increase linearly. The 4.0 runtime is with -O3 optimising, which surprisingly does not seem to affect compile times.

### (3) Odin

This failed my 10,000-function tests. It crashes on larger inputs. The figures shown are based on the 1000-function test

### (4) Seed7

The compile time here is how long it took s7c to turn the .sd7 source file into a C intermediate file. Fully building involves running a C compiler on the result, but the intermediate C contains 3.3M lines compared to 0.87M of the Seed7 source code. Even Tiny took over 20 seconds to compile it (but I couldn't link it). I didn't try gcc which is the normally invoked C compiler.

### (5) Java

The executable (or rather the .class file) produced is suspiciously small at 120KB, or about 12 bytes per function. If I tried to get it to retain all the functions (using res+=fxxxx(5) on each call), then it aborted with 'Code too large', also at about 25 seconds. However, compiling 100 functions took 4 seconds (but maybe most of those are eliminated too) and Hello, World took 2 seconds. So maybe it's 50 functions/second. Output of Java is probably JVM code not native.

### (6) 'M'

MM is the compiler for my own systems language. BCC is my own C compiler.

(mm) against my compilers means it was compiled with my mm which only has a modest optimiser. MM compiled with gcc-O3, if it was expressed as C (no longer possible) would probably make it 30% faster.

### (7) Tiny C

This has several entries, which it deserves being the fastest compiler in the list; even the slowest is faster than mine.

(tcc) means it was compiled with itself; (bcc) compiled with my bcc; and (gcc) compiled with gcc-O3. The remaining entry is as it was downloaded, as a prebuilt binary.

Tcc beats my bcc compiler (even unoptimised!) probably because it is single pass and goes direct to native. My bcc is handicapped by multiple passes, and a discrete ASM intermediate form, which here means a 50MB ASM file to process. However, my compilers produce somewhat faster code.

### (8) Dart

This compiler took 6 seconds just to compile a Hello, World program (which generated a 5MB executable, which probably explains it!).

### Host

All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

### Sorted by Runtime

Remember this is just for that one small benchmark (a single call to fannkuch(11) in a program with just that one function):

Implem | Language | Runtime
--- | --- | ----
**Clang -O3**        | C | 2.45 secs
**Clang -O2**        | C | 2.5
**MSVC /O2**          | C | 2.6
**Clang -O1**        | C | 2.6
**Go**            | Go | 2.9
**MM -opt** (mm)   | M | 3.1
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
**Rustc**         | Rust| 37.8

One of my compilers (MM-opt) does surprisingly well. It's just a fluke (it's typically 50% slower than gcc-O3, not faster!), however that is the genuine timing for this test.

