## Compiler Tests III

This is a revised version of [these tests](Compilertest2.md), as I found that if the 10,000 functions were not called, some compilers didn't do as much work. The new test will call each of the functions once.

I've also concentrated on compilers generating native code, as ones which are interpreted, or do not generate a discrete binary, deserve their own benchmarks with different criteria.

Again, this is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt); any missing ones can be found at the former link.

The benchmark function is repeated 10,000 times with random names, and each called once inside the main program. (With parameter 5, but that is only of significance for interpreted code which needs to execute the whole thing.)

I've added also information on the generated binary size, and an idea of the installation size.

Implem | Language | Time (secs) | Funcs/sec | Runtime | Exe Size | Installation Files/MB
--- | --- | --- | --- | --- | --- | ---
**Rustc** | Rust  | 396 | 25 | ---| --- | 12/68MB?
**Dart**          | Dart | 235| 42 | 0.6 | 27MB | 500/490MB
**gcc-O3**           | C            | 85 | 118 | 0.30 | 1MB | 4800/550MB
**DMD -O**       | D | 156 | 64 | 0.32 | 15MB | 4000 files/300MB 
**gcc-O0**           | C            | 67 | 150 | 0.71 | 10MB | 4800/550MB
**Go**            | Go | 40 | 250 | 0.27 | 10MB | 9200/350MB
**DMD**           | D | 32 | 310 | 0.75 | 16MB | 4000/300MB
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 670 | 1800 | 0.53 [Source](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx) | 10MB | 1/2.4MB
**bcc** (gcc)     | C        | 3.0 | 3300 | 0.75 | 8MB | 1/0.7MB
**BB-opt** (bb)   | M        | 2.5 | 4000 | 0.28 | 7MB | 1/0.6MB
**BB** (bb)       | M        | 2.2 | 4500 | 0.56 | 8MB | 1.0/6MB
**MM** (gcc)      | M        | 1.75 | 5700 | 0.60 | 10MB | 1/0.8MB
**Tiny C**        | C        | 1.1 | 9100 | 0.79 | 10MB | 120/1.8MB


### Time

How many seconds it took to compile

### Funcs/sec

Because some took more lines than others, the table is sorted in terms of functions/second throughput.

### Runtimes

This column has been added so that some trade-offs can be compared. Runtime is how long it took to execute:

    fann(10)
    
in a program containing just the one function.

### Exe Size

The size of the binary executable. The smallest is gcc-O3 with the unfeasibly low 1MB (100 bytes per function), probably the result of some culling. Otherwise they range from 6.6MB to 27MB.

### Installation Files/MB

This gives an idea of the magnitude of the installation. The very large ones will come with lots of libraries, headers etc, but it is not practical to isolate what is needed to run this test.

The Rust installation is rather mysterious; I found 12 exe files, of identical sizes, but there may be more. It is incomplete anyway as it relies on MS Build Tools, which exist but it can't manage to link up to (so I couldn't link to create executables, or to run the results).

### Notes

**Lines/second** Fastest speed in LPS was something like 0.9Mlps (tcc), and slowest around 2Klps (Rust, which excluded linking because that part didn't work). Code density varies, but I think tcc still comes out on top.

**Optimisation** Optimised versions tried where the option existed and I knew how to to turn it on.

**Host** All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

### My Compilers

These are BB and the older MM, both for my M systems language. Also BCC for C.

(gcc) means it was transpiled to C and compiled with gcc-O3, which gives a faster time than otherwise (but a bigger compiler executable).

(bb) means it was compiled with BB; so not quite as fast, but a smaller executable.

