rem %1 is an optional optimisation setting; default to -O0
gcc %1 jpeg.c -o..\jpeg_gcc.exe
gcc %1 clex.c -o..\clex_gcc.exe
gcc %1 pi.c -o..\pi_gcc.exe
gcc %1 mandelbrot.c -o..\mandelbrot_gcc.exe
gcc %1 ax.c -o..\ax_gcc.exe
gcc %1 cc.c -o..\cc_gcc.exe
gcc %1 pc.c -o..\pc_gcc.exe
gcc %1 mm.c -o..\mm_gcc.exe
