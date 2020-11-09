## Compiler Tests

My old set of tests: [compiletests.txt](compiletests.txt) from the start of 2020 were a bit of laugh, but it was still interesting to see how compilers coped.

That test was 20K to 2M lines of 'a=b+c*d' in a single function, which is far from realistic.

This is one is slightly more realistic; it takes a benchmark function, 50-100 lines long, and repeats it 100, 1000 or 10000 times in one module. Naturally, each one is named differently. The test is how long it takes an implementation to build each module.

The benchmark is called 'fannkuch-redux' and described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt].

Implem | Language | x 100 | x 1000 | x 10000 | Bytes/func | Runtime | Klps | Notes
--- | --- | --- | --- | --- | --- | --- | --- | ---
**Tiny C** | C			| 0.1 sec | 0.2 sec | 1.1 sec | 1000 bytes | 10.6 sec | 900 Klps
**gcc** | C             | 0.9 | 6.1 | 63  | 970  | 8.7  | 15.5
**gcc-opt** | C			| 0.4 | 0.7 | 5.3 | 6 | 3.3   | 185 | All functions discarded from exe except one
**DMC** | C				| 0.2 | 0.4 | OOM | 730 | 2.6-8.0 | 245 
**DMD** | D         | 0.8 | 1.7 | 29.2 | 30/1800 | 9.5 | 57/33 | (1.8KB in obj file, 30 bytes in exe.
**DMD-opt** | D     | 2.2 | 14.0| 145 |  30/1800 | 4.0 | 6.7  | All but one function discard)
**Nim** | Nim		| 1.0 | 2.5 | 21.6 | 1300 | 15.5  | 43
**Nim-opt** | Nim	| 1.0 | 2.7 | 22.8 | 15 | 4.4  | 41
**Lua** | Lua       | 0.1 | 0.2 | 0.8  | na      | 170 |  520
**LuaJIT** |Lua     | 0.1 | 0.1 | 0.6 | na       | 9.6 | 700
**CPython** |Python | 0.2 | 1.0 | 8.5 | na | 517 | 73
**PyPy** | Python       | 0.3 | 1.8 | 17.4 | na  | 11.5 | 35
**Go** | Go				| 1.4 | 4.6 | 36.5 | 920b | 2.80 | 25 
**Julia** | Julia		| 1.7 | 13.9 | 137 | na | 3.7/19.6 | 6.3
**Rustc** | Rust        | 2.4 | 13.6 | 136 | ---  | ---- | 5.5
**A68G** | Algol68		| 0.7 | 8.0 | OOM | na    | 1070 | 9.3
... | ... | ... | ...| ... | ... | ... | ... | ...
**BB** | M          | 0.1 | 0.3 | 2.1 | 880  | 6.9 | 350
**BB-opt** | M      | 0.1 | 0.3 | 2.4 | 690  | 3.1 | 310
**MS** | MS         | 0.1 | 0.1 | 0.5  | na  | --- | 1300  | (Incomplete project)
... | ... | ... | ...| ... | ... | ... | ... | ...
**ax**  | ASM       | 0.1 | 0.2 | 1.5 | na | na |  1500
**gcc/as** | ASM(S) | --- | 0.6 | 8.3 | na | na | 300

### Notes

**x100/1000/10000** The number in each column is how long it took to compile or build each of three programs, containing 100-10000 copies of the function. For interpreters, runtime is not included (the fann1() function is not called).

**Bytes/func** The size of the EXE divided by 100-10000 gives approx code bytes for the function. Some optimising compilers discard all but the one function that is called, hence the 6 bytes/function of gcc-opt, and the surprisingly fast compilation. No value shown for interpreters.

**Runtime** This is testing compiler performance, not of the generate code, but this can be useful to look at effective different trade-offs can be. The time is for one call of fannkuch(11), which should give a result of (556355, 51).

**Klps** Thousands of lines/second compilation speed. Best shown (two values shown for DMD). Note that source file sizes vary (see below) so can only be roughly compared.

**Rust** Rust timings exclude linking as that didn't work on my machine. (Benchmark was tested on rextester.com.)

**Ruby** Ruby was one of the languages, but not included here. (I just found it a bit dull. Its runtime was 150 seconds.)

**ASM** I've added a couple of assemblers (input is generated from BB and gcc). I would have liked to have included Nasm, which is a slow one, but not practical.

**Line-counts** For x10000, the source file was 500K to 1000Kloc. (Most versions, except mine, have one declaration per line, increasing line count, and making the Klps figure a little better than it actually is. The Lua code is very dense so true Klps probably higher than shown.) ASM versions were just over 2Mloc.

### Observations

Generally, implementations performed much better than on the a=b+c*d test. (Except for mine, see below.) It's good to see Rust working at practical speeds.

But there is still about 100:1 between them.

Such stress tests can be good at picking up problems that can go unnoticed in normal programs, and that was the case with mine. Out of 5 programs, 3 showed serious problems. One (ax) I've fixed (it had been using an old hash function which bunched together similar identifiers such 'fann1' to 'fann10000').

The other two are a C compiler, and bytecode interpreter, not shown as the results are too embarrassing. (The problem there is up to 10,000 duplicates of the same local variables. The symbol tables stored all instances of the same identifier in one linked list; it's not supposed to search the whole thing! But the BB program does the same, and that one is OK. So may need to be fixed.)

(All my programs are built with BB which has a limited optimiser. Otherwise they could be up to 30% faster.)

As a mini-bonus, the file I linked to acts as a mini-Rosetta-Code for those 10 languages.
