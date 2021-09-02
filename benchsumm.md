### Benchmark Results.

Benchmark | GCC-O3 | BB-Opt | BB-Orig | BCC | TCC | Notes
--- | --- | --- | --- | --- | --- | ---
**Jpeg/87M** | 4.3 | 6.5 | 9.2 | 11.3 | 18.0 | Seconds, rounded to 0.1 seconds
**Clex/SQL** | 3.0 | 4.0 | 4.1 | 4.7 | 6.7 | 
**Pi/2K** | 0.84 | 4.1 | 4.6 | 4.4 | 4.8 | 
**Mandel/6M** | 3.0 | 4.2 | 5.3 | 6.1 | 5.9 |
**AX/2M** | 1.1 | 1.4 | 1.6 | 1.6 | 1.8 | 
**BCC/SQL** | 2.2 | 2.9 | 3.2 | 3.3 | 3.7 | 
**BCC/500K** | 2.5 | 3.5 | 4.7 | 4.3 | 4.8 | 
**BCC/Lua** | 3.0 | 3.6 | 3.9 | 3.9 | 4.4 | 
**PC/Jpeg/2M** | 3.5| 5.2 | 7.0 | 6.5 | 9.0 | BB-Opt is 1.9 seconds, accelerated mode (see note)
**PC/Clex** | 4.8 | 7.6 | 9.9 | 9.4 | 12.6 | BB-Opt is 2.3 seconds, accelerated mode 
**MM/1M**  | 3.2 | 4.2 | 4.9 | 4.9 | 5.4 | 
**Misc** | 3.1 | 4.8 | 6.8 | 7.7 | 8.8 | (Misc micro-benchmarks, 20% of actual value)
--- | --- | --- | --- | --- | --- | 
**Average**  | 2.9 | 4.35 | 5.4  | 5.7 | 7.1  | seconds
**Average (excl 'pi')** | 3.1  | 4.37 | 5.5  | 5.8 | 7.4 |('Pi' result is not a typical program for me; needs further investigation)
**Rel to GCC** | 1.00  | 1.50 | 1.86 | 1.97 | 2.45 | How many times as slow as gcc-O3
**Rel to GCC (excl 'pi')** | 1.00 | 1.41 | 1.77 | 1.87 | 2.39
**Total EXE sizes** | 2426 | 1593 | 1793 | 1980 | 2283 | KB (not KiB)
