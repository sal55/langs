## Compiler Tests 1

This is a re-presentation of my initial compiler tests at [compiletest.txt](compilertest.txt).

This is a very simple test where a compiler is given repeated lines of:

    a = b + c * d
 
 or equivalent, as either 20K, 100K, 500K or 2000K lines of code, and seeing what happens. The code will either be the whole program, or be in one function. When declared, a, b, c, d will be integers, and initialised to 1, 2, 3, 4.
 
 **Note** Most timings have been taken over the last 18 months. Some may be out of date, in that newer versions may perform better (or worse).
 
 Table has been sorted in lines/second order.

Implementation | Language | Max LoC | Compile time | Lines/second | Notes
--- | --- | --- | --- | --- | ---
Clox | Lox | 2000K | 1.0 | 2000 Klps | 
LuaJIT | Lua | 2000K | 2.0 | 1000 Klps | 
Wren | Wren | 2000K | 2.20 | 909 Klps | 
QQ | Q | 2000K | 2.40 | 833 Klps | 
TinyC | C | 2000K | 3.30 | 606 Klps | 
Vox | Vox | 2000K | 5.40 | 370 Klps | 
Lua | Lua | 2000K | 6.30 | 317 Klps | 
MM | M | 2000K | 7.20 | 277 Klps | 
Wasm | Wasm | 500K | 2.70 | 185 Klps | File too big on 2000K (Note 500K=3000K lines of Wasm)
Ruby192 | Ruby | 2000K | 17.70 | 113 Klps | 
Perl | Perl | 2000K | 19.60 | 102 Klps | 
eui | Euphoria | 2000K | 21.0 | 95 Klps | 
CLISP | Lisp | 2000K | 22.0 | 91 Klps | 
Python3.7 | Python | 500K | 6.6 | 76 Klps |  Timed out at 2000K
Go | Go | 2000K | 31.0 | 64 Klps | 
lccwin | C | 500K | 8.70 | 57 Klps |  Machine OOM at 2000K
gcc-opt | C | 500K | 12 |  42 Klps | Timed out at 2000K
g++8.1.0-opt | C | 100K | 2.70 | 37 Klps |  (Not tested above 100K)
lccwin-opt | C | 2000K | 67 |  30 Klps
Clang | C | 500K | 22.50 | 22 Klps |  Machine OOM at 2000K
PyPy | Python | 100K | 5.0 | 20 Klps |  Reported out of memory at 500K
V | Vlang | 100K | 5.20 | 19.2 Klps | (500K+ not attempted)
PellesC | C | 500K | 35.0 | 14 Klps |  Reported OOM at 2000K
MSVC | C | 100K | 11.60 | 8.6 Klps |  Timed out at 500K
MSVC-opt | C | 100K | 11.6 | 8.6 Klps |  Timed out at 500K
Clang-opt | C | 500K | 73.0 | 6.8 Klps | 
A68G | Algol68 | 10K | 1.50 | 6.7 Klps |  (OOM on 20K)
Dart | Dart | 500K | 74.50 | 6.7 Klps | (2000K not attempted)
g++8.1.0 | C | 100K | 19.3 | 5.2 Klps |  (Not tested above 100K)
Nim-CC | Nim | 100K |  25.0 | 4 Klps | Timed out (Nim to C only)
Rustc-O | Rust | 20K | 5.20 | 3.8 Klps |  Timed out at 100
gcc-8.1.0 | C | 500K | 137.0 | 3.6 Klps |   Machine OOM at 2000K
Zig | Zig | 100K | 40.0 | 2.5 Klps |  Machine OOM on 500K
FBC | Basic | 20K | 9.0 | 2.2 Klps |  Timed out at 100K
Javac | Java | 5K | 3.20 | 1.5 Klps |  'Code too large' on 20K
Nim+CC | Nim | 20K | 16.0 | 1.2 Klps |  Out of memory (Nim to C + C compilation)
PellesC-opt | C | 20K | 25.0 | 0.8 Klps |  Timed out at 100K
DMD | D | 20K | 28.70 | 0.7 Klps |  Timed out on 100K
Rustc | Rust | 20K | 30.70 | 0.65 Klps | Timed out at 100K
Julia | Julia | 100K | 189.0 | 0.5 Klps |  Timed out 500K
DMC | C | Fail | 0.0 | 0 |  Crash on 20K, timed out on 100K
Pico-C | C | Fail | 0.0 | 0 |  Reported OOM at 20K
FPC | Pascal | Fail | 0.0 | 0 |  (Proc too complex)
