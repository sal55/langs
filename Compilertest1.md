## Compiler Tests 1

This is a re-presentation of my initial compiler tests at [compiletest.txt](compilertest.txt).

This is a very simple test where a compiler is given repeated lines of:

    a = b + c * d
 
 or equivalent, as either 20K, 100K, 500K or 2000K lines of code, and seeing what happens. The code will either be the whole program, or be in one function. When declared, a, b, c, d will be integers, and initialised to 1, 2, 3, 4.
 
 **Note** Most timings have been taken over the last 18 months. Some may be out of date, in that newer versions may perform better (or worse).
 
 Table has been sorted in lines/second order.

Implementation | Language | Max LoC | Compile time | Lines/second | Notes
--- | --- | --- | --- | --- | ---
PellesC | C | 500K | 35.0 | 14Klps |  reported OOM at 2000K
PellesC-opt | C | 20K | 25.0 | 0.8Klps |  Timed out at 100K
lccwin | C | 500K | 8.70 | 57Klps |  machine OOM at 2000K
lccwin-opt | C | 2000K | 67 |  30Klps
DMC | C | Fail | 0.0 | 0 |  Crash on 20K, timed out on 100K
gcc-8.1.0 | C | 500K | 137.0 | 3.6Klps |   machine OOM at 2000K
gcc-opt | C | 500K | 12 |  42Klps | timed out at 2000K
g++8.1.0 | C | 100K | 19.3 | 5.2Klps |  (not tested above 100K)
g++8.1.0-opt | C | 100K | 2.70 | 37Klps |  (not tested above 100K)
TinyC | C | 2000K | 3.30 | 606Klps | 
MSVC | C | 100K | 11.60 | 8.6Klps |  timed out at 500K
MSVC-opt | C | 100K | 11.6 | 8.6Klps |  timed out at 500K
Clang | C | 500K | 22.50 | 22Klps |  Machine OOM at 2000K
Clang-opt | C | 500K | 73.0 | 6.8Klps | 
Pico-C | C | Fail | 0.0 | 0 |  reported OOM at 20K
DMD | D | 20K | 28.70 | 0.7Klps |  Timed out on 100K
Lua | Lua | 2000K | 6.30 | 317Klps | 
Python3.7 | Python | 500K | 6.6 | 76Klps |  Timed out at 2000K
PyPy | Python | 100K | 5.0 | 20Klps |  Reported out of memory at 500K
Ruby192 | Ruby | 2000K | 17.70 | 113Klps | 
Perl | Perl | 2000K | 19.60 | 102Klps | 
Rustc | Rust | 20K | 30.70 | 0.65Klps | Timed out at 100K
Rustc-O | Rust | 20K | 5.20 | 0 |  Timed out at 100
Nim+CC | Nim | 20K | 16.0 | 1.2Klps |  Out of memory (Nim to C + C compilation)
Nim-CC | Nim | 100K |  25.0 | 4Klps | Timed out (Nim to C only)
FPC | Pascal | Fail | 0.0 | 0 |  (Proc too complex)
FBC | Basic | 20K | 9.0 | 2.2Klps |  Timed out at 100K
Go | Go | 2000K | 31.0 | 64Klps | 
CLISP | Lisp | 2000K | 22.0 | 91Klps | 
A68G | Algol68 | 10K | 1.50 | 6.7Klps |  (OOM on 20K)
Zig | Zig | 100K | 40.0 | 2.5Klps |  machine OOM on 500K
eui | Euphoria | 2000K | 21.0 | 95Klps | 
Julia | Julia | 100K | 189.0 | 0.5Klps |  Timed out 500K
LuaJIT | Lua | 2000K | 2.0 | 1000Klps | 
Clox | Lox | 2000K | 1.0 | 2000Klps | 
V | Vlang | 100K | 5.20 | 19.2Klps | (500K+ not attempted)
Dart | Dart | 500K | 74.50 | 6.7Klps | (2000K not attempted)
Vox | Vox | 2000K | 5.40 | 370Klps | 
Wren | Wren | 2000K | 2.20 | 909Klps | 
Wasm | Wasm | 500K | 2.70 | 185Klps | File too big on 2000K (Note 500K=3000K lines of Wasm)
Javac | Java | 5K | 3.20 | 1.5Klps |  'Code too large' on 20K
MM | M | 2000K | 7.20 | 277Klps | 
QQ | Q | 2000K | 2.40 | 833Klps | 
