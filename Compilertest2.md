## Compiler Tests

My old set of tests: [compilertests.txt](compilertest.txt) from the start of 2020 were a bit of laugh, but it was still interesting to see how compilers coped.

That test was 20K to 2M lines of 'a=b+c*d' in a single function, which is far from realistic.

This is one is slightly more realistic; it takes a benchmark function, 50-100 lines long, and repeats it 10000 times in one module. Naturally, each one is named differently (and randomly to avoid spurious results due to bad hashing). The test is how long it takes an implementation to build each module.

The benchmark is called 'fannkuch-redux' and described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt).

Implem | Language | Time | Funcs/sec
--- | --- | --- | ----
**Rustc** | Rust  | 222 | 45
**DMD-opt**       | D | 155 | 64
**Julia**    | Julia  | 136 | 73
**gcc** | C            | 61 | 160
**Go**            | Go | 38 | 263
**DMD**            | D | 29 | 340
**PyPy**      | Python | 16 | 625
**CPython**  | Python | 8.2 | 1200
**BB-opt** | M        | 2.5 | 4000
**BB**            | M | 2.1 | 4700
**Tiny C**        | C | 1.1 | 9100
**Lua**         | Lua | 0.9 | 11000
**LuaJIT**      | Lua | 0.6 | 16600
**MS**          | MS | 0.55 | 18000


### Notes

**Time** How many seconds it took to compile

**Func/sec** Because some took more lines than others, the table is sorted in terms of functions/second throughput

**Files Sizes** Roughly 500K to 1000K lines. More than is typical for a single module, but timings for 1000 functions (50-100K lines) were usually proportionately less. (Any smaller, and the faster products would be hard to measure.)

**Optimisation** Optimisation is usually off except in where it made an appreciable difference, and was felt it actually did something. gcc-O3 took the same time as gcc-O3, suggesting it hadn't bothered.

### My Compilers

These are BB (M language) and MS (MS language). The latter is a new embedded scripting language.

BB had a poor optimiser, but nevertheless was used to build the BB and MS compilers. With full optimisation such as with gcc-O3, they would have been faster.

