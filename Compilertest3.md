## Compiler Tests III

This is a revised version of [these tests](Compilertest2.md), as I found that if
the 10,000 files were not called, some compilers didn't do as much work. The new test will call
each of the functions once.

I've also concentrated on compilers generating native code, as ones which are interpreted, or
do not generate a discrete binary, deserve their own benchmarks with different criteria.

Again, this is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt).

The benchmark function is repeated 10,000 times with random names.

I've added also information on the generated binary size

Implem | Language | Time (secs) | Funcs/sec | Runtime | Exe Size | Installation Files/MB
--- | --- | --- | --- | --- | --- | ---
**Rustc** | Rust  | 396 | 25 | ---| size | ---
**Dart**          | Dart | 235| 42 | 0.6 | 27MB | 500/490MB
**DMD -O**       | D | 156 | 64 | 0.32 | 15MB | 4000 files/300MB 
**gcc-O0**           | C            | 67 | 150 | 0.71 | 10MB | 4800/550MB
**gcc-O3**           | C            | 85 | 118 | 0.71 | 1MB | 4800/550MB
**Go**            | Go | 40 | 250 | 0.27 | 10MB | 9200/350MB
**DMD**           | D | 32 | 310 | 0.75 | 16MB | 4000/300MB
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 5.5 | 1800 | 0.53 [Source](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx) | 10MB | 1/2.4MB
**bcc** (gcc)     | C        | 3.0 | 3300 | 0.75 | 8MB | 1/0.7MB
**BB-opt** (bb)   | M        | 2.5 | 4000 | 0.28 | 7MB | 1/0.6MB
**BB** (bb)       | M        | 2.2 | 4500 | 0.56 | 8MB | 1.0/6MB
**MM** (gcc)      | M        | 1.75 | 5700 | 0.6 | 10MB | 1/0.8MB
**Tiny C**        | C        | 1.1 | 9100 | 0.79 | 10MB | 120/1.8MB


### Notes

**Time** How many seconds it took to compile

**Func/sec** Because some took more lines than others, the table is sorted in terms of functions/second throughput.

**Lines/second** Fastest speed in LPS was something above 1.2Mlps (my MS bytecode compiler), and slowest around 3.4Klps (Rust, which excluded linking because that part didn't work)

**Files Sizes** Roughly 500K to 1000K lines. More than is typical for a single module, but timings for only 1000 functions (50-100K lines) were usually proportionately less. (Any smaller, and the faster products would be hard to measure.)

**Optimisation** Optimisation was usually off except where it made an appreciable difference, and was felt it actually did something. gcc-O3 took the same time as gcc-O0, suggesting it hadn't bothered.

**Host** All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

### Runtimes

This column has been added, so that some trade-offs can be compared. Runtime is how long it took to execute:

    fann(10)
    
in a program containin just the one function.

**Notes:**

* Rust doesn't link on my machine so couldn't be run

* In the case of my MS, which is unfinished, 15.7 is actual time of unaccelerated code. Projections taken from the previous product suggest: 10 seconds when optimised via gcc-03; and 4.1 seconds when accelerated (not using JIT; it still executes bytecode).)

* Don't read into much into the figures of one, small, tight benchmark, which some optimisers and especially tracing-JITs can optimise agressively.

### My Compilers

The current ones are BB (M language) and MS (MS language). The latter is a new embedded scripting language.

I've now added 'bcc', which is my C compiler. This had had problems with its ST organisation (each set of 10,000 duplicate local names was stored in one list, and sometimes it would scan all of it). This program is still hampered by having an intermediate stage that is ASM source code, which slows down otherwise it would be faster than BB. Note that the timing is of a version built with gcc 5.1.0 -O3, otherwise it would be 40% slower.

I've also addede 'MM', which is an older version of BB. This had the advantage of being able to be transpiled to C, allowing a proper optimising compiler to be used.

The (bb) against my implentations means compiled with 'bb -opt'. (gcc) means transpiled to C and compiled with 'gcc -O3'.
