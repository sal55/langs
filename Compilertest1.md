## Compiler Tests 1

This is a re-presentation of my initial compiler tests at [compiletest.txt](compilertest.txt).

This is a very simple test where a compiler is given repeated lines of:

    a = b + c * d
 
 or equivalent, as either 20K, 100K, 500K or 2000K lines of code, and seeing what happens. The code will either be the whole program, or be in one function. When declared, a, b, c, d will be integers, and initialised to 1, 2, 3, 4.
 
 **Note** Most timings have been taken over the last 18 months. Some may be out of date, in that newer versions may perform better (or worse).
 
 Table has been sorted in lines/second order.

Implementation | Language | Max LoC | Compile time | Lines/second | Notes
--- | --- | --- | --- | --- | ---
PellesC | C | 500K | 35.0 | 0 |  reported OOM at 2000K
PellesC-opt | C | 20K | 25.0 | 0 |  Timed out at 100K
lccwin | C | 500K | 8.70 | 0 |  machine OOM at 2000K
lccwin-opt | C | 2000K | 67 |  0
DMC | C | 0 | 0.0 | 0 |  Crash on 20K, timed out on 100K
gcc-8.1.0 | C | 500K | 137.0 | 0 |   machine OOM at 2000K
gcc-opt | C | 500K | 12 |  0 | timed out at 2000K
g++8.1.0 | C | 100K | 19.3 | 0 |  (not tested above 100K)
g++8.1.0-opt | C | 100K | 2.70 | 0 |  (not tested above 100K)
TinyC | C | 200K | 3.30 | 0 | 
MSVC | C | 100K | 11.60 | 0 |  timed out at 500K
MSVC-opt | C | 100K | 11.6 | 0 |  timed out at 500K
Clang | C | 500K | 22.50 | 0 |  Machine OOM at 2000K
Clang-opt | C | 500K | 73.0 | 0 | 
Pico-C | C | Fail | 0.0 | 0 |  reported OOM at 20K
DMD | D | 20K | 28.70 | 0 |  Timed out on 100K
Lua | Lua | 2000K | 6.30 | 0 | 
Python3.7 | Python | 500K | 6.6 | 0 |  Timed out at 2000K
PyPy | Python | 100K | 5.0 | 0 |  Reported out of memory at 500K
Ruby192 | Ruby | 2000K | 17.70 | 0 | 
Perl | Perl | 2000K | 19.60 | 0 | 
Rustc | Rust | 20K | 30.70 | 0 | Timed out at 100K
Rustc-O | Rust | 20K | 5.20 | 0 |  Timed out at 100
Nim+CC | Nim | 20K | 16.0 | 0 |  Out of memory (Nim to C + C compilation)
Nim-CC | Nim | 100K |  25.0 | 0 | Timed out (Nim to C only)
FPC | Pascal | Fail | 0.0 | 0 |  (Proc too complex)
FBC | Basic | 20K | 9.0 | 0 |  Timed out at 100K
Go | Go | 2000K | 31.0 | 0 | 
CLISP | Lisp | 2000K | 22.0 | 0 | 
A68G | Algol68 | 10K | 1.50 | 0 |  (OOM on 20K)
Zig | Zig | 100K | 40.0 | 0 |  machine OOM on 500K
eui | Euphoria | 2000K | 21.0 | 0 | 
Julia | Julia | 100K | 189.0 | 0 |  Timed out 500K
LuaJIT | Lua | 2000K | 2.0 | 0 | 
Clox | Lox | 2000K | 1.0 | 0 | 
V | Vlang | 100K | 5.20 | 0 | (500K+ not attempted)
Dart | Dart | 500K | 74.50 | 0 | (2000K not attempted)
Vox | Vox | 2000K | 5.40 | 0 | 
Wren | Wren | 2000K | 2.20 | 0 | 
Wasm | Wasm | 500K | 2.70 | 0 | File too big on 2000K (Note 500K=3000K lines of Wasm)
Javac | Java | 5K | 3.20 | 0 |  'Code too large' on 20K
MM | M | 2000K | 7.20 | 0 | 
QQ | Q | 2000K | 2.40 | 0 | 