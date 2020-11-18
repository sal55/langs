## Compiler Tests

My old set of tests: [compilertests.txt](compilertest.txt) from the start of 2020 were a bit of laugh, but it was still interesting to see how compilers coped.

That test was 20K to 2M lines of 'a=b+c*d' in a single function, which is far from realistic.

This is one is slightly more realistic; it takes a benchmark function, 50-100 lines long, and repeats it 100, 1000 or 10000 times in one module. Naturally, each one is named differently. The test is how long it takes an implementation to build each module.

The benchmark is called 'fannkuch-redux' and described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt).

Implem | Language | x 100 | x 1000 | x 10000 | Bytes/func | Runtime | Klps | Notes
--- | --- | --- | --- | --- | --- | --- | --- | ---
**Rustc** | Rust        | 2.4 secs | 13.6 secs | 136 secs | ---  | ---- secs | 5.5 Klps | Can't link or run Rust on my PC
**Julia** | Julia		| 1.7 | 13.9 | 137 | na | 3.7/19.6 | 6.3 | Runtime is opt/unopt; compile times about the same
**DMD-opt** | D     | 2.2 | 14.0| 145 |  30/1800 | 4.0 | 6.7  | (30 bytes in exe; 1800 in obj)
**A68G** | Algol68		| 0.7 | 8.0 | OOM | na    | 1070 | 9.3 | Interpreter
**gcc** | C             | 0.9 | 6.1 | 63  | 970  | 8.7  | 15.5
**gcc-opt** | C		    	| 0.8 | 6.0 | 62 | 10.5 | 3.3   | 15.8| Functions are non-static otherwise misleading results.
**Go** | Go				| 1.4 | 4.6 | 36.5 | 920 | 2.80 | 25 
**PyPy** | Python       | 0.3 | 1.8 | 17.4 | na  | 11.5 | 35
**Nim-opt** | Nim	| 1.0 | 2.7 | 22.8 | 15 | 4.4  | 41
**Nim** | Nim		| 1.0 | 2.5 | 21.6 | 1300 | 15.5  | 43
**DMD** | D         | 0.8 | 1.7 | 29.2 | 30/1800 | 9.5 | 57/33 | (30 bytes in exe; 1800 in obj)
**CPython** |Python | 0.2 | 1.0 | 8.5 | na | 517 | 73
**DMC** | C				| 0.2 | 0.4 | OOM | 730 | 2.6-8.0 | 245 | Runtime is opt/unop; compile times about the same
**gcc/as** | ASM(S) | --- | 0.6 | 8.3 | na | na | 300 | (ASM is output of gcc -S on C versions)
**BB-opt** | M      | 0.1 | 0.3 | 2.4 | 690  | 3.1 | 310 | (M is my language)
**BB** | M          | 0.1 | 0.3 | 2.1 | 880  | 6.9 | 350
**Lua** | Lua       | 0.1 | 0.2 | 0.8  | na      | 170 |  520
**LuaJIT** |Lua     | 0.1 | 0.1 | 0.6 | na       | 9.6 | 700
**Tiny C** | C			| 0.1 | 0.2 | 1.1 | 1000 | 10.6 | 900
**MS** | MS         | 0.1 | 0.1 | 0.5  | na  | --- | 1300  | (Intrerpeter/Unfinished project)
**ax**  | ASM       | 0.1 | 0.2 | 1.5 | na | na |  1500 | (ASM is output of BB -asm on M versions)


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
