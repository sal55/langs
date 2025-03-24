## Q Interpreter Benchmarks

This is testing old and new versions of my Q interpreter ('A' and 'B' in the Reddit post).

### The Benchmarks

The following benchmarks were tested, mostly integer or float based:

````
ack             Ackermann function (ack(3,9) 3 times)
basic           Simple BASIC interpreter running a loop
bigint          Simple big-integer library (implemented in Q; not built-in bignums)
binary          Binary trees (N=15)
bitops          Count '1' bits in 256 bit-patterns, 10.5K times
bubble          Bubble sort (N=25, 50K times)
clex            Simple C Lexer, input is 980Kloc/22MB fike
collatz         Work out steps for N up to 300K
cray            Simple ray-tracing (64x64 greyscale image, write PGM)
fann            Fannkuch benchmark (N=9, twice)
fib             Fibonacci for N in 1..36
for             Empty for-loop (N=400M)
jpeg            JPEG decoder; input is 84KB/0.5Mpixel
kiss64          PRNG test
manboy          Man-or-boy test (N=0 to 13, 200 times)
mandelbrot      Create fractal over 250x400 greyscale image, write PGM
nbody           NBody test (N=128K)
nsieve          Sieve benchmark (for larger N=3M)
neg             Invert bits in 25MB data (extracted from image library)
nums            'Countdown' number puzzle solver
poly            ?
pythag          Find Pythagorean triplets up to N=400
queens          ?
readx           (Dump/disassemble EXE file to text file; input is 400KB file)
search          ?
shell           Shell sort
sieve           Sieve benchmark (small N of 10000, times 1000)
spectral        'Spectral' benchmark
sqrt            ?
to              Empty to-loop (N=300M)
while           While-loop (N=500M)
````
'?' means I can't remember exactly what it does. The important thing is it does some task that can be used to compare performance.

### Results

(Note the ordering of A2/A3 columns is reversed to how they are shown in Reddit post.)

````
Runtimes are in milliseconds:                    Relative to A1 (bigger is faster):

              A1     A3     A2     B1     B2       A1     A3     A2     B1     B2
           ----------------------------------------------------------------------
ack         2835   2601    382    538    460 |   1.00   1.09   7.42   5.27   6.16
basic       1522   1039    944    991    663 |   1.00   1.46   1.61   1.54   2.30
bigint      1507   1085    491    585    445 |   1.00   1.39   3.07   2.58   3.39
binary      1569   1241    632    882    570 |   1.00   1.26   2.48   1.78   2.75
bitops      1648   1382    444    569    601 |   1.00   1.19   3.71   2.90   2.74
bubble      3413   2288    585   1085    695 |   1.00   1.49   5.83   3.15   4.91
clex        2131   1617    647    851    600 |   1.00   1.32   3.29   2.50   3.55
collatz     2694   2039    866    819    773 |   1.00   1.32   3.11   3.29   3.49
cray         600    507    445    397    273 |   1.00   1.18   1.35   1.51   2.20
fann        1803   1304    507    741    445 |   1.00   1.38   3.56   2.43   4.05
fib         2241   1585    475    648    601 |   1.00   1.41   4.72   3.46   3.73
for         1803   1601    866    882   1101 |   1.00   1.13   2.08   2.04   1.64
jpeg        1210    898    397    491    398 |   1.00   1.35   3.05   2.46   3.04
kiss64      2600   1757    460    757    679 |   1.00   1.48   5.65   3.43   3.83
manboy      1100    898    741    632    491 |   1.00   1.22   1.48   1.74   2.24
mandelbrot  1538   1164    381    539    350 |   1.00   1.32   4.04   2.85   4.39
nbody       1304    976    694    819    538 |   1.00   1.34   1.88   1.59   2.42
nsieve      1007    820    413    444    398 |   1.00   1.23   2.44   2.27   2.53
neg          460    272     70    194    210 |   1.00   1.69   6.57   2.37   2.19
nums        1241    929    397    554    382 |   1.00   1.34   3.13   2.24   3.25
poly        1210    866    429    538    398 |   1.00   1.40   2.82   2.25   3.04
pythag      2975   2648    491    710    523 |   1.00   1.12   6.06   4.19   5.69
queens      1506   1148    257    366    335 |   1.00   1.31   5.86   4.11   4.50
readx        554    382    382    381    289 |   1.00   1.45   1.45   1.45   1.92
search      1319   1038    210    398    335 |   1.00   1.27   6.28   3.31   3.94
shell       1881   2320    475    617    444 |   1.00   0.81   3.96   3.05   4.24
sieve       2522   1507    585    788    617 |   1.00   1.67   4.31   3.20   4.09
spectral    1350    898    257    428    288 |   1.00   1.50   5.25   3.15   4.69
sqrt        1367    975   1007    991    914 |   1.00   1.40   1.36   1.38   1.50
to          1335   1225    710    679    632 |   1.00   1.09   1.88   1.97   2.11
while       1929   1600    273    319    273 |   1.00   1.21   7.07   6.05   7.07
           ----------------------------------------------------------------------
Totals:    52174  40610  15913  19633  15721
Averages:                                        1.00   1.32   3.77   2.76   3.47
````
### Comparisons with CPython 3.14 and Lua 5.41

The following were also tested under CPython 3.14 with the same parameters:

````
ack         5.9   seconds
binary      5.9
bitops      3.0
bubble      3.3
clex        9.5   (100Klps, best of several versions; B2 managed 1700Klps)
fann        2.3
fib         6.6
for         6.6
jpeg        1.7
manboy      5.1
mandelbrot  2.8
nbody       1.5
nsieve      1.3
pythag      5.2
shell       1.7
sieve       1.9
while       4.3
````

The following were also tested under Lua 5.41:
````
binary      4.8   seconds
clex/alex  14.7   (66Klps; two versions exist)
clex/slex  22.3   (44Klps)
fann        0.6
fib         3.3
manboy      3.0
nbody       0.5
nsieve      1.9
pythag      1.0
while       2.9
````
### Test Environment
* Windows 11
* AMD Ryzen 3 x64 processor

### Some of the benchmarks:
````
# to-loop
proc main=
    for i in 1..400 million do
    end
end

# for-loop
proc main =
    to 300'000'000 do
    od
end

# while-loop
proc main=
    i:=0
    while i<100'000'000 do
        ++i
    end
    println i
end
````

(Q uses dedicated bytecodes for `to` and `for` loops, each needs only a single instruction per iteration. `while`, as written, uses 4 instructions, but the last three are optimised into a single compound instruction.)

