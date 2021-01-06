## Compiler Tests

My old set of tests: [compilertests.txt](compilertest.txt) from the start of 2020 were a bit of laugh, but it was still interesting to see how compilers coped.

That test was 20K to 2M lines of 'a=b+c*d' in a single function, which is far from realistic.

This is one is slightly more realistic; it takes a benchmark function, 50-100 lines long, and repeats it 100, 1000 or 10000 times in one module. Naturally, each one is named differently. The test is how long it takes an implementation to build each module.

The benchmark is called 'fannkuch-redux' and described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt).

Implem | Language | Time | Funcs/sec
--- | --- | --- | ----
**Rustc** | Rust  | 222 | 45
DMD-opt       | D | 155 | 64
Julia    | Julia  | 136 | 73
gcc | C            | 61 | 160
Go            | Go | 38 | 263
DMD            | D | 29 | 340
PyPy      | Python | 16 | 625
CPython  | Python | 8.2 | 1200
BB-opt | M        | 2.5 | 4000
BB            | M | 2.1 | 4700
Tiny C        | C | 1.1 | 9100
Lua         | Lua | 0.9 | 11000
LuaJIT      | Lua | 0.6 | 16600
MS          | MS | 0.55 | 18000

### Notes

**x100/1000/10000** The number in each column is how long it took to compile or build each of three programs, containing 100-10000 copies of the function. For interpreters, runtime is not included (the fann1() function is not called).

**Bytes/func** The size of the EXE divided by 100-10000 gives approx code bytes for the function. Some optimising compilers discard all but the one function that is called, hence the 6 bytes/function of gcc-opt, and the surprisingly fast compilation. No value shown for interpreters.

**Runtime** This is testing compiler performance, not of the generated code, but this can be useful to see how effective different trade-offs can be. The time is for one call of fannkuch(11), which should give a result of (556355, 51).

**Klps** Thousands of lines/second compilation speed. Best shown (two values shown for DMD). Note that source file sizes vary (see below) so can only be roughly compared.

**Rust** Rust timings exclude linking as that didn't work on my machine. (Benchmark was tested on rextester.com.)

**Ruby** Ruby was one of the languages, but not included here. (I just found it a bit dull. Its runtime was 150 seconds.)

**ASM** I've added a couple of assemblers (input is generated from BB and gcc). I would have liked to have included Nasm, which is a slow one, but not practical.

**Line-counts** For x10000, the source file was 500K to 1000Kloc. (Many versions have one declaration per line, increasing line count, and making the Klps figures a little better than they actually are. The Lua code is very dense so true Klps probably higher than shown.) ASM versions were just over 2Mloc.

### Observations

Generally, implementations performed much better than on the a=b+c\*d test. (Except for mine, see below.) It's good to see Rust working at practical speeds, although it's still at the bottom end of the range.

But there is still about 100:1 between them.

Such stress tests can be good at picking up problems that can go unnoticed in normal programs, and that was the case with mine. Out of 5 programs, 3 showed serious problems. One (ax) I've fixed (it had the wrong hash function).

The other two are a C compiler, and bytecode interpreter, not shown as the results are too embarrassing. (The problem there is up to 10,000 duplicates of the same local variables. The symbol tables stored all instances of the same identifier in one linked list; it's not supposed to search the whole thing! But the BB program does the same, and that one is OK. So may need to be fixed.)

(All my programs - BB, ax, MS - are built with BB which has a limited optimiser. Otherwise they could be up to 30% faster.)

As a small bonus, the [file](fannkuch.txt) I linked to acts as a mini-Rosetta-Code for those 10 languages.
