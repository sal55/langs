## Compiler Tests III

This is a revised version of [these tests](Compilertest2.md), as I found that if the 10,000 functions were not called, some compilers didn't do as much work. The new test will call each of the functions once.

I've also concentrated on compilers generating native code, as ones which are interpreted, or do not generate a discrete binary, deserve their own benchmarks with different criteria.

Again, this is the 'fannkuch-redux' benchmark described [here](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/fannkuchredux.html). Versions I've written in 10 languages (plus two of mine) are listed [here](fannkuch.txt); any missing ones can be found at the former link.

The benchmark function is repeated 10,000 times with random names, and each called once inside the main program. (With parameter 5, but that is only of significance for interpreted code which needs to execute the whole thing.)

I've added also information on the generated binary size, and an idea of the installation size.

Implem | Language | Time (secs) | Funcs/sec | Runtime | Exe Size | Inst Files | Inst MB
--- | --- | --- | --- | --- | --- | --- | ---
**Rustc -O** | Rust  | 79000 \*\* | 0.13 | 3.2 secs| 10MB \*\* | 12+14600 | 100MB + 2800MB
**Clang -O3**        | C | 780 | 13 | 2.45 | 16MB | 350 | 1600MB
**Clang -O2**        | C | 650 | 15 | 2.5 | --- | 350 | 1600MB
**Rustc** | Rust  | 330 | 30 | 37.8 | 40MB | 12+14600 | 100MB+2800MB
**Clang -O1**        | C | 310 | 15 | 2.6 | --- | 350 | 1600MB
**Dart**          | Dart | 235| 42 | 6.2 | 27MB | 500 | 490MB
**DMD -O**       | D | 156 | 64 | 4.1 | 15MB | 4000 | 300MB 
**MSVC /O2**          | C | 155 |64 | 2.6 | 0.2MB | 14600 | 2800MB
**Odin -opt**        | Odin | 104 ++ | 96 | 2.7 | --- | 200 | 140MB
**gcc -O3**           | C            | 85 | 118 | 3.3 | 0.93MB | 4800 | 550MB
**gcc**           | C            | 67 | 150 | 8.7 | 10MB | 4800 | 550MB
**Go**            | Go | 40 | 250 | 2.9 | 10MB | 9200 | 350MB
**DMD**           | D | 32 | 310 | 9.7 | 16MB | 4000 | 300MB
**Clang**         | C | 30 | 330 | 10.2 | 12MB | 350 | 1600MB
**Javac**      | Java | 25 | 400 | 4.0  |0.12MB | 400 | 330MB
**MSVC**          | C | 12  |830 | 9.6 | 9.2MB | 14600 | 2800MB
**Odin**        | Odin | 14 | 720 | 27.3 | --- | 200 | 140MB
**Vox**           | [Vox](https://github.com/MrSmith33/vox) | 5.7 | 1750 | [6.4](https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef#file-fannkuch-vx) | 10MB | 1 | 2.4MB (0.75MB upx)
**bcc** (bb)     | C        | 4.2 | 2400 | 9.0 | 8.0MB | 1 | 1.0MB (0.32MB upx)
**BB -opt** (bb)   | M        | 2.5 | 4000 | 3.1 | 6.6MB | 1 | 0.6MB (0.17MB upx)
**MM** (bb)      | M        | 2.3 | 4300 | 7.0 | 10MB | 1 | 0.8MB (0.16MB upx)
**BB** (bb)       | M        | 2.2 | 4500 | 6.8 | 7.8MB | 1 | 0.6MB (0.17MB upx)
**Tiny C** (tcc)  | C        | 1.9 | 5100 | 10.1 | 10MB | 120 | 1.8MB
**Tiny C** (bcc)  | C        | 1.5 | 6700 | 10.1 | 10MB | 120 | 1.8MB
**Tiny C**        | C        | 1.1 | 9100 | 10.1 | 10MB | 120 | 1.8MB
**Tiny C**  (gcc) | C        | 0.9 | 11100 | 10.1 | 10MB | 120 | 1.8MB

### Time

How many seconds it took to compile

### Funcs/sec

Because some took more lines than others, the table is sorted in terms of functions/second throughput.

### Runtimes

This column has been added so that some trade-offs can be compared. Runtime is how long it took to execute:

    fann(11)
    
in a program containing just the one function. (This has been changed from fann(10) as timings were getting too small. That was chosen originally because I'd had interpreted languages too, and fann(11) was too challenging for some.)

### Exe Size

The size of the binary executable. They range from 6.6MB to 40MB. There are some smaller ones: 0.9MB from gcc-O3, and 0.2MB from MSVC/O2, but they have most likely culled a lot of code.

### Installation Files/MB

This gives an idea of the magnitude of the installation. The very large ones will come with lots of libraries, headers etc, but it is not practical to isolate what is needed to run this test.

Where the entire installation is one self-contained executable, then figures using UPX compression are shown. (UPX is a utility that reduces the size of a .exe file, but it still runs as normal, although with about 0.1 seconds extra overhead.) By this measure, a Rust installation needs 17,000 times more space than my bb.exe.


### \*\*Rust

I've managed to make this work, and the good news is that, with a new update, it's a bit faster: 330 seconds unoptimised. And I finally have timings, but:

* Rust needs MS VC++ build tools to work; so the installation size includes those tools

* The runtime for unoptimised Rust is poor: 10 times as slow as optimised code; I don't know why

* The optimised compile was aborted after 20 minutes. 100 functions took 18 seconds; 1000 functions took 1200 seconds. From this I extrapolated a figure of 79200 or 22 hours for a full optimised compile of 10,000 functions.

* The optimised executable size was extrapolated from that of the 1000-function version

So Rust has some problems in my opinion. Even that 18 seconds for an optimised build of 100-functions or 8000 lines is only 0.4K lines per second - microcomputer territory.

### ++Odin

This failed my 10,000-function tests. It crashes on larger inputs. The figures shown are based on the 1000-function test

### Java

The executable (or rather the .class file) produced is suspiciously small at 120KB, or about 12 bytes per function. If I tried to get it to retain all the functions (using res+=fxxxx(5) on each call), then it aborted with 'Code too large', also at about 25 seconds. However, compiling 100 functions took 4 seconds (but maybe most of those are eliminated too) and Hello, World took 2 seconds. So maybe it's 50 functions/second. Output of Java is probably JVM code not native.

### Notes

**Lines/second** Fastest speed in LPS was something like 900Klps (tcc), and slowest around 1.2Klps (optimised Clang; I don't include the 0.01Klps of optimised Rust). Code density varies, but I think tcc still comes out on top.

**Optimisation** Optimised versions tried where the option existed and I knew how to to turn it on.

**Host** All tests were done on an old Windows 7 PC, 64 bits, with spinning hard drive. Number of cores available was 2 (doubt any used more than one). Not the most up-to-date hardware, but all compilers ran on the same machine.

**Dart** This compiler took 6 seconds just to compile a Hello, World program (which generated a 5MB executable, which probably explains it!). This suggest another possible measure - overheads that apply even to a minimal program. But I haven't really seen it in others, except I think Zig, which is not part of my test (too much effort to try and get the benchmark written). There may anyway be options to control that which I don't know about, so I won't try that yet.


### My Compilers

These are BB and the older MM, both for my M systems language. Also BCC for C.

(bb) means it was compiled with BB; so not quite as fast, but a smaller executable. I've removed the (gcc) timings where the source was transpiled to C and passed through gcc-O3.

This handicaps my compilers a little, as it is assumed most others will use fully optimising compilers (but see below), but I also want to show what is possible with a modest home-made compiler that doesn't rely on heavy-duty tools. My compilers still dominate the fast end of the table, except for Tiny C.

### Tiny C

This has several entries, which it deserves being the faster compiler in the list; even the slowest is faster than mine.

(tcc) means it was compiled with itself.

(bcc) compiled with my bcc

(gcc) compiled with gcc-O3

The remaining entry is as it was downloaded, as a prebuilt binary.

Tiny C is fast for several reasons:

* It is a one-pass compiler (AIUI)
* It compiles C which lends itself to one-pass compilation (this test further doesn't use large headers or any use of the CPP)
* It beats my M compilers, because M has out-of-order definitions which are not suitable for one pass, and M compilers are multi-pass anyway
* It badly beats my own BCC C compiler, because that also uses multi-passes, but here also generates a 50MB intermediate ASM file, which then has to be assembled.

However, my compilers produce somewhat faster code.

### Sorted by Runtime

Remember this is just for that one small benchmark (a single call to fannkuch(11) in a program with just that one function):

Implem | Language | Runtime
--- | --- | ----
**Clang -O3**        | C | 2.45 secs
**Clang -O2**        | C | 2.5
**MSVC /O2**          | C | 2.6
**Clang -O1**        | C | 2.6
**Go**            | Go | 2.9
**BB -opt** (bb)   | M | 3.1
**Rustc -O** | Rust | 3.2
**gcc -O3**           | C  | 3.3
**Javac**      |Java | 4.0
**DMD -O**       | D | 4.1
**Dart**          | Dart | 6.2
**Vox**           | Vox | 6.3
**BB** (bb)       | M   | 6.8
**MM** (gcc)      | M   | 7.0
**gcc**           | C   | 8.7
**bcc** (gcc)     | C   | 9.0
**MSVC**          | C   | 9.6
**DMD**           | D   | 9.7
**Tiny C**        | C   | 10.1
**Clang**         | C   | 10.2
**Rustc**         | Rust| 37.8

One of my compilers (bcc-opt) does surprisingly well. It's just a fluke (it's typically 50% slower than gcc-O3, not faster!), however that is the genuine timing for this test.

