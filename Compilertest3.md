## Compiler Tests III

This is a revised version of [these tests](Compilertest2.md), as I found that if the 10,000 functions were not called, some compilers didn't do as much work. The new test will call each of the functions once.

I've also concentrated on compilers generating native code, as ones which are interpreted, or do not generate a discrete binary, deserve their own benchmarks with different criteria.

Again, this is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt); any missing ones can be found at the former link.

The benchmark function is repeated 10,000 times with random names, and each called once inside the main program. (With parameter 5, but that is only of significance for interpreted code which needs to execute the whole thing.)

I've added also information on the generated binary size, and an idea of the installation size.

Implem | Language | Time (secs) | Funcs/sec | Runtime | Exe Size | Inst Files | Inst MB
--- | --- | --- | --- | --- | --- | --- | ---
**Rustc -O** | Rust  | 79000 \*\* | 0.13 | 3.2 secs| 10MB \*\* | 12+14600 | 100MB + 2800MB
**Clang -O3**        | C | 800 \*\* | 12.5 | 2.45 | 17MB \*\* | 350 | 1600MB
**Rustc** | Rust  | 330 | 30 | 37.8 | 40MB | 12+14600 | 100MB+2800MB
**Dart**          | Dart | 235| 42 | 6.2 | 27MB | 500 | 490MB
**DMD -O**       | D | 156 | 64 | 4.1 | 15MB | 4000 | 300MB 
**MSVC /O2**          | C | 155 |64 | 2.6 | 0.2MB | 14600 | 2800MB
**gcc -O3**           | C            | 85 | 118 | 3.3 | 0.93MB | 4800 | 550MB
**gcc**           | C            | 67 | 150 | 8.7 | 10MB | 4800 | 550MB
**Go**            | Go | 40 | 250 | 2.9 | 10MB | 9200 | 350MB
**DMD**           | D | 32 | 310 | 9.7 | 16MB | 4000 | 300MB
**Clang**         | C | 30 | 330 | 10.2 | 12MB | 350 | 1600MB
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 15 | 670 | [6.3](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx) | 10MB | 1 | 2.4MB
**MSVC**          | C | 12  |830 | 9.6 | 9.2MB | 14600 | 2800MB
**bcc** (gcc)     | C        | 3.0 | 3300 | 9.0 | 8.0MB | 1 | 0.7MB
**BB -opt** (bb)   | M        | 2.5 | 4000 | 3.1 | 6.6MB | 1 | 0.6MB
**BB** (bb)       | M        | 2.2 | 4500 | 6.8 | 7.8MB | 1 | 0.6MB
**MM** (gcc)      | M        | 1.75 | 5700 | 7.0 | 10MB | 1 | 0.8MB
**Tiny C**        | C        | 1.1 | 9100 | 10.1 | 10MB | 120 | 1.8MB

### Time

How many seconds it took to compile

### Funcs/sec

Because some took more lines than others, the table is sorted in terms of functions/second throughput.

### Runtimes

This column has been added so that some trade-offs can be compared. Runtime is how long it took to execute:

    fann(11)
    
in a program containing just the one function. (This has been changed from fann(10) is timings were getting too small. That was chosen originally because I'd had interpreted languages too, and fann(11) was too challenging for some.)

### Exe Size

The size of the binary executable. They range from 6.6MB to 40MB. There are some smaller ones: 0.9MB from gcc-O3, and 0.2MB from MSVC/O2, but they have most likely culled a lot of code.

### Installation Files/MB

This gives an idea of the magnitude of the installation. The very large ones will come with lots of libraries, headers etc, but it is not practical to isolate what is needed to run this test.

### \*\*Rust

I've managed to make this work, and the good news is that, with a new update, it's a bit faster: 330 seconds unoptimised. And I finally have timings, but:

* Rust needs MS VC++ build tools to work; so the installation size includes those tools

* The runtime for unoptimised Rust is poor: 10 times as slow as optimised code; I don't know why

* The optimised compile was aborted after 20 minutes. 100 functions took 18 seconds; 1000 seconds took 1200 seconds. From this I extrapolated a figure of 79200 or 22 hours for a full optimised compile of 10,000 functions.

* The optimised executable size was extrapolated from that of the 1000-function version

So Rust has some problems in my opinion. Even that 18 seconds for an optimised build of 100-functions or 8000 lines is only 0.4K lines per second - microcomputer territory.

### \*\*Clang (using LLVM)

The figures for this are also extrapolated from a 1000-function version. Using -O2 insted of -O3 makes it 25% faster to build (but still slow), and runtime increases to 0.25 seconds (but the 0.23 figure is the best of all the compilers).

### Notes

**Lines/second** Fastest speed in LPS was something like 900Klps (tcc), and slowest around 1.2Klps (optimised Clang; I don't include the 0.01Klps of optimised Rust). Code density varies, but I think tcc still comes out on top.

**Optimisation** Optimised versions tried where the option existed and I knew how to to turn it on.

**Host** All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

**Dart** This compiler took 6 seconds just to compile a Hello, World program (which generated a 5MB executable, which probably explains it!). This suggest another possible measure - overheads that apply even to a minimal program. But I haven't really seen it in others, except I think Zig, which is not part of my test (too much effort to try and get the benchmark written). There may anyway be options to control that which I don't know about, so I won't try that yet.


### My Compilers

These are BB and the older MM, both for my M systems language. Also BCC for C.

(gcc) means it was transpiled to C and compiled with gcc-O3, which gives a faster time than otherwise (but a bigger compiler executable).

(bb) means it was compiled with BB; so not quite as fast, but a smaller executable.

I don't know why some of the compilers in the list are so slow. But my 'bcc' C compiler is slowed down by having to generate a 50MB ASM intermediate file, which then has to be assembled. It would otherwise be faster than BB/MM where that step has been eliminated. (Actually, that was done in order to give better results for comparisons like this, as for most real apps, compilation was not an issue. It just felt the proper approach.)

