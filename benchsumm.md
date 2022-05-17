### Benchmark Results.

Figures are in seconds of runtime.

Benchmark | GCC-O3 | BB-Opt | BB-Orig | BCC | TCC | Description
--- | --- | --- | --- | --- | --- | ---
**Jpeg/87M** | 4.3 | 6.5 | 9.2 | 11.3 | 18.0 | Decode 87Mpixel jpeg file
**Clex/SQL** | 3.0 | 4.0 | 4.1 | 4.7 | 6.7 | Scan 219Kloc sqlite3.c (100 times) and count chars/lines/tokens
**Pi/2K** | 0.84 | 4.1 | 4.6 | 4.4 | 4.8 | Calculate 2000 digits of pi
**Mandel/6M** | 3.0 | 4.2 | 5.3 | 6.1 | 5.9 | Mandelbrot set into 2048x3072 greyscale image
**AX/2M** | 1.1 | 1.4 | 1.6 | 1.6 | 1.8 | Assemble 2M lines of 'mov eax,\[ebx+ecx\*2+123456\]'
**BCC/SQL** | 2.2 | 2.9 | 3.2 | 3.3 | 3.7 | Compile sqlite3.c+shell.c (246Kloc) into sqlite3.exe (5 times)
**BCC/500K** | 2.5 | 3.5 | 4.7 | 4.3 | 4.8 | (Compile 500K lines of a:=b+c\*d)
**BCC/Lua** | 3.0 | 3.6 | 3.9 | 3.9 | 4.4 | Compile 34 modules of Lua sources to lua.exe (10 times)
**PC/Jpeg/2M** | 3.5| 5.2 | 7.0 | 6.5 | 9.0 | Run my interpreter on jpeg decoder on 2Mpix
**PC/Clex** | 4.8 | 7.6 | 9.9 | 9.4 | 12.6 | Run interpreter on lexer with sqlite3.c (10 times)
**MM/1M**  | 3.2 | 4.2 | 4.9 | 4.9 | 5.4 |  Run my M compiler on 1M lines of a:=b+c\*d
**Misc** | 3.1 | 4.8 | 6.8 | 7.7 | 8.8 | (Miscellaneous micro-benchmarks)
--- | --- | --- | --- | --- | --- | 
**Average**  | 2.9 | 4.35 | 5.4  | 5.7 | 7.1  | seconds
**Rel to GCC** | 1.00  | 1.50 | 1.86 | 1.97 | 2.45 | How many times as slow as gcc-O3

