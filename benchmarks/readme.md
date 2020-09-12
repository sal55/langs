### Benchmark Programs

The C directory shows the one-file C versions of the 8 programs. buildall.bat shows how they were compiled (but very straightforward, eg. 'gcc -O3 prog.c -oprog.exe').

These renderings target Windows 64-bit machines and will not build on Linux (as they include some WinAPI) calls. Linux versions can be provided.

Four of the smaller original source M versions are shown above. The other four are bigger applications; older versions of those can be seen in the 'sources' sub-directory as one-file representations. (qq.ma includes a byte-code compiler and the interpreter - the 'pc' benchmark, as these can be split.)

How the programs are invoked varies, eg. 'jpeg' has the filename hard-coded within the 'start()' driver function. (Normally that program is built into a DLL and accessed via the loadjpeg() function.).

