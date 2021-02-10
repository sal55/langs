## Compiler Tests III

This is a revised version of [these tests](Compilertest2.md), as I found that if the 10,000 functions were not called, some compilers didn't do as much work. The new test will call each of the functions once.

I've also concentrated on compilers generating native code, as ones which are interpreted, or do not generate a discrete binary, deserve their own benchmarks with different criteria.

Again, this is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt); any missing ones can be found at the former link.

The benchmark function is repeated 10,000 times with random names, and each called once inside the main program. (With parameter 5, but that is only of significance for interpreted code which needs to execute the whole thing.)

I've added also information on the generated binary size, and an idea of the installation size.

Implem | Language | Time (secs) | Funcs/sec | Runtime (secs) | Exe Size | Installation Files, MB
--- | --- | --- | --- | --- | --- | ---
**Rustc -O** | Rust  | 22 **HOURS** \*\* | 1/8th | 0.30| 10MB \*\* | 12 Files, 100MB + 14600 Files, 2800MB
**Rustc** | Rust  | 330 secs | 30 | 3.1 | 40MB | 12 Files, 100MB + 14600 Files, 2800MB
**Dart**          | Dart | 235| 42 | 0.6 | 27MB | 500 Files, 490MB
**DMD -O**       | D | 156 | 64 | 0.32 | 15MB | 4000 Files, 300MB 
**MSVC /O2**          | C | 155 |64 | 0.25 | 0.2MB | 14600 Files, 2800MB
**gcc -O3**           | C            | 85 | 118 | 0.30 | 0.93MB | 4800 Files, 550MB
**gcc**           | C            | 67 | 150 | 0.71 | 10MB | 4800 Files, 550MB
**Go**            | Go | 40 | 250 | 0.27 | 10MB | 9200 Files, 350MB
**DMD**           | D | 32 | 310 | 0.75 | 16MB | 4000 Files, 300MB
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 15 | 670 | 0.53 [Source](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx) | 10MB | 1 Files, 2.4MB
**MSVC**          | C | 12  |830 | 0.78 | 9.2MB | 14600 Files, 2800MB
**bcc** (gcc)     | C        | 3.0 | 3300 | 0.75 | 8.0MB | 1 File, 0.7MB
**BB -opt** (bb)   | M        | 2.5 | 4000 | 0.28 | 6.6MB | 1 File, 0.6MB
**BB** (bb)       | M        | 2.2 | 4500 | 0.56 | 7.8MB | 1 File, 0.6MB
**MM** (gcc)      | M        | 1.75 | 5700 | 0.60 | 10MB | 1 File, 0.8MB
**Tiny C**        | C        | 1.1 | 9100 | 0.79 | 10MB | 120 Files, 1.8MB

### Time

How many seconds it took to compile

### Funcs/sec

Because some took more lines than others, the table is sorted in terms of functions/second throughput.

### Runtimes

This column has been added so that some trade-offs can be compared. Runtime is how long it took to execute:

    fann(10)
    
in a program containing just the one function.

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

### Notes

**Lines/second** Fastest speed in LPS was something like 0.9Mlps (tcc), and slowest around 2.4Klps (unoptimised Rust; I don't include the 10lps of optimised Rust). Code density varies, but I think tcc still comes out on top.

**Optimisation** Optimised versions tried where the option existed and I knew how to to turn it on.

**Host** All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

**Dart** This compiler took 6 seconds just to compile a Hello, World program (which generated a 5MB executable, which probably explains it!). This suggest another possible measure - overheads that apply even to a minimal program. But I haven't really seen it in others, except I think Zig, which is not part of my test (too much effort to try and get the benchmark written). There may anyway be options to control that which I don't know about, so I won't try that yet.


### My Compilers

These are BB and the older MM, both for my M systems language. Also BCC for C.

(gcc) means it was transpiled to C and compiled with gcc-O3, which gives a faster time than otherwise (but a bigger compiler executable).

(bb) means it was compiled with BB; so not quite as fast, but a smaller executable.

I don't know why some of these are so slow. But my 'bcc' C compiler is slowed down by having to generate a 50MB ASM intermediate file, which then has to be assembled. It would otherwise be faster than BB/MM where that step has been eliminated. (Actually, in order to give better results for comparisons like this, as for most real apps, compilation was not an issue.)

