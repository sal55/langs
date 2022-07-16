Description | gcc-O3 | mm/bcc | Factor | Notes
--- | --- | --- | --- | ---
Cipher test      | 1.26   | 4.3  | 3.4 | Encrpyt/decrypt sql.c
Calculate *pi*   |  0.52 | 1.46 | 2.8 | Big-number test
Jpeg coder       | 2.2   | 5.0  | 2.3  | Decode 80Pixel image
Small benchmarks | 22.6 | 44.6 | 2.0 | Uses gcc-O1 as -O3 optimises some to nothing
Lua/Fibonacci    | 6     | 12   | 2.0  | (Build Lua from source) Run Lua on Fib (from an old machine)
QQ/Jpeg          | 2.5    | 3.8 | 1.5  | (Build my Q interpreter) Run Q on Jpeg decoded for 2Mpix
C compiler       | 0.31   | 0.44 | 1.4 | (Build bcc.exe) Run bcc on sql.c (250Kloc)

