## Compiler Tests

My old set of tests: [compilertests.txt](compilertest.txt) from the start of 2020 were a bit of laugh, but it was still interesting to see how compilers coped.

That test was 20K to 2M lines of 'a=b+c*d' in a single function, which is far from realistic.

This is one is slightly more realistic; it takes a benchmark function, 50-100 lines long, and repeats it 10000 times in one module. Naturally, each one is named differently (and randomly to avoid spurious results due to bad hashing). The test is how long it takes an implementation to build each module.

The benchmark is called 'fannkuch-redux' and described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt).

Some compilers here produce bytecode rather than executables.

Implem | Language | Time (secs) | Funcs/sec | Runtime (secs)
--- | --- | --- | --- | ---
**Rustc** | Rust  | 222 | 45 | ---
**DMD-opt**       | D | 155 | 64 | 0.32
**Julia**    | Julia  | 136 | 73 | 0.88
**gcc** | C            | 61 | 160 | 0.71 (0.30 opt)
**Go**            | Go | 38 | 263 | 0.27
**Dart**          | Dart | 30 | 330 | 0.60 (source is on shootout site)
**DMD**            | D | 29 | 340 | 0.75
**PyPy**      | Python | 16 | 625 | 1.2
**CPython**  | Python | 8.2 | 1200 | 37.4
**Vox**         | [Vox](https://github.com/MrSmith33/vox) | 5.5 | 1800 | 0.53 [Source](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx)
**bcc** (gcc) | C        | 3.0 | 3300 | 0.75
**BB-opt** (bb) | M        | 2.5 | 4000 | 0.28
**s7**          | Seed7     |2.2 | 4500 | 64 (Seed7 interpreter)
**BB** (bb)            | M | 2.1 | 4700 | 0.56
**MM** (gcc)          | M | 1.7 | 5800 | 0.6
**Tiny C**        | C | 1.1 | 9100 | 0.79
**Lua**         | Lua | 0.9 | 11000 | 11.5
**QQ** (bb)     | Q  | 0.7 | 14000 | (in development; 4.5 projected)
**LuaJIT**      | Lua | 0.6 | 16600 | 0.75

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

The current ones are BB (M language) and QQ (Q scripting language). QQ'is a new product but which can't yet run code. Time is for compiling to internal bytecode. Internal timing is 0.6 seconds, or 16700 functions/second, or 1.15Mlps.

I've now added 'bcc', which is my C compiler, that had had problems with this benchmark. It is still hampered by having an intermediate stage that is ASM source code, which slows down otherwise it would be faster than BB. Note that the timing is of a version built with gcc 5.1.0 -O3, otherwise it would be 40% slower.

I've also addede 'MM', which is an older version of BB. This had the advantage of being able to be transpiled to C, allowing a proper optimising compiler to be used.

The (bb) against my implentations means compiled with 'bb -opt'. (gcc) means transpiled to C and compiled with 'gcc -O3'.


