## Q Interpreter Benchmarks

This is testing old and new versions of my Q interpreter 'A' and 'B':

### The Benchmarks

The following benchmarks were tested, mostly integer or float based:

````
ack             Ackermann function (ack(3,9) 3 times)
basic           Simple BASIC interpreter running a loop
bigint          Simple big-integer library (implemented in Q; not built-in bignums)
binary          Binary trees (N=15)
bitops          Count '1' bits in 256 bit-patterns, 10.5K times
blur            Blurring 1x20Mpix linear greyscale image (adapted from image library)
bubble          Bubble sort (N=25, 50K times)
clex            Simple C Lexer, input is 980Kloc/22MB fike
collatz         Work out steps for N up to 300K
comments        Strip comments from 8MB C file and write out result
cray            Simple ray-tracing (64x64 greyscale image, write PGM)
fann            Fannkuch benchmark (N=9, twice)
fib             Fibonacci for N in 1..34
for             Empty for-loop (N=400M)
jpeg            JPEG decoder; input is 84KB/0.5Mpixel
kiss64          PRNG test
manboy          'Man-or-boy' closure emulation test (N=0 to 13, 200 times)
mandelbrot      Create fractal over 250x400 greyscale image, write PGM
nbody           NBody test (N=128K)
nsieve          Sieve benchmark (for larger N=3M)
neg             Invert bits in 25MB data (extracted from image library)
nums            'Countdown' number puzzle solver
poly            ?
pythag          Find Pythagorean triplets up to N=400
queens          ?
readx           (Dump/disassemble EXE file to text file; input is 400KB file)
rubik           Represent Rubik's cube and apply 1M 90-deg rotations of one face
runint          Interpret Pascal P-code program evaluating Fibonacci for N=1..27
search          ?
shell           Shell sort
sieve           Sieve benchmark (small N of 10000, times 1000)
spectral        'Spectral' benchmark
sqrt            ?
sud             Solve Sudoku puzzle (200 times)
to              Empty to-loop (N=300M)
while           While-loop (N=500M)
````
'?' means I can't remember exactly what it does. The important thing is it does some task that can be used to compare performance.

### Results

The tests are done somewhat differently than before:

* AA represents the old Q interpreter usng the fastest -asmopt ASM-assisted dispatcher
* BB is the new interpreter with 100% HLL code and using new 'doswitchx' statement (built with my MM compiler)
* DD is the same code, transpiled to C, and compiled with gcc-O3

BB and DD are tested separately against AA. Raw runtimes in msec are shown, then AA result is normalised to 1.0, and the BB/DD result is shown relative to that; bigger is faster:
````
                       AA     BB       AA     BB
Running: ack          382    413 |   1.00   0.92
Running: basic        898    991 |   1.00   0.91
Running: bigint       491    601 |   1.00   0.82
Running: binary       632    960 |   1.00   0.66
Running: bitops       428    539 |   1.00   0.79
Running: blur        1381   1351 |   1.00   1.02
Running: bubble       585   1148 |   1.00   0.51
Running: clex         694    694 |   1.00   1.00
Running: collatz      882    804 |   1.00   1.10
Running: comments     569    632 |   1.00   0.90
Running: cray         445    413 |   1.00   1.08
Running: fann         522    741 |   1.00   0.70
Running: fib          476    491 |   1.00   0.97
Running: for          835    867 |   1.00   0.96
Running: kiss64       428    679 |   1.00   0.63
Running: manboy       710    694 |   1.00   1.02
Running: mandelbrot   398    522 |   1.00   0.76
Running: nbody        648    726 |   1.00   0.89
Running: nsieve       381    429 |   1.00   0.89
Running: neg           70    194 |   1.00   0.36
Running: nums         429    538 |   1.00   0.80
Running: poly         460    475 |   1.00   0.97
Running: pythag       445    663 |   1.00   0.67
Running: queens       288    335 |   1.00   0.86
Running: readx        351    382 |   1.00   0.92
Running: rubik       1288   1538 |   1.00   0.84
Running: runint       772   1070 |   1.00   0.72
Running: search       194    460 |   1.00   0.42
Running: shell        429    616 |   1.00   0.70
Running: showg        413    445 |   1.00   0.93
Running: sieve        569    726 |   1.00   0.78
Running: spectral     288    382 |   1.00   0.75
Running: sqrt         975    851 |   1.00   1.15
Running: sud         1507   1757 |   1.00   0.86
Running: to           678    679 |   1.00   1.00
Running: while        257    319 |   1.00   0.81

Totals:             21198  25125

Averages:                            1.00   0.84
````

Now for DD:
````
                       AA     DD       AA     DD
Running: ack          398    366 |   1.00   1.09
Running: basic        913    647 |   1.00   1.41
Running: bigint       460    414 |   1.00   1.11
Running: binary       616    538 |   1.00   1.14
Running: bitops       476    600 |   1.00   0.79
Running: blur        1429   1069 |   1.00   1.34
Running: bubble       585    726 |   1.00   0.81
Running: clex         694    616 |   1.00   1.13
Running: collatz      835    788 |   1.00   1.06
Running: comments     585    398 |   1.00   1.47
Running: cray         460    257 |   1.00   1.79
Running: fann         475    460 |   1.00   1.03
Running: fib          492    428 |   1.00   1.15
Running: for          882   1038 |   1.00   0.85
Running: kiss64       429    616 |   1.00   0.70
Running: manboy       695    413 |   1.00   1.68
Running: mandelbrot   397    320 |   1.00   1.24
Running: nbody        663    492 |   1.00   1.35
Running: nsieve       366    397 |   1.00   0.92
Running: neg           54    179 |   1.00   0.30
Running: nums         397    351 |   1.00   1.13
Running: poly         444    382 |   1.00   1.16
Running: pythag       444    554 |   1.00   0.80
Running: queens       273    288 |   1.00   0.95
Running: readx        366    288 |   1.00   1.27
Running: rubik       1288    882 |   1.00   1.46
Running: runint       757    757 |   1.00   1.00
Running: search       225    289 |   1.00   0.78
Running: shell        475    429 |   1.00   1.11
Running: showg        397    335 |   1.00   1.19
Running: sieve        523    772 |   1.00   0.68
Running: spectral     288    226 |   1.00   1.27
Running: sqrt         960    835 |   1.00   1.15
Running: sud         1507   1585 |   1.00   0.95
Running: to           679    679 |   1.00   1.00
Running: while        257    272 |   1.00   0.94

Totals:             21184  19686

Averages:                            1.00   1.09
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
fib         2.5
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
fib         1.3
manboy      3.0
nbody       0.5
nsieve      1.9
pythag      1.0
while       2.9
````
### Test Environment
* Windows 11
* AMD Ryzen 3 x64 processor

