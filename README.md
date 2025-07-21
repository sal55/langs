I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as docs, charts, source backups, benchmark results, the odd binary. It been used also for random stuff related to posts made to my Reddit account, but that has now stopped.

### Current Set of Languages
````
M          Lower level systems language
Q          Dynamic scripting language
AA         x64 assembly using my syntax
PCL7       Stack-based IL used by v7 compilers (this can be a complete standalone language)
PCL8       3AC-based IL used by experimental v8 compiler (an internal language that can only be dumped to textual form).
C-subset   The partly non-standard subset of C used by the BCC project. The subset is also the output of the MC projects.
````

### Current Projects

````
MM8     WIP: M compiler for ARM64 running Linux; uses PCL8
MM7     M compiler for x64/Win64 ABI, uses PCL7
QQ7     Bytecode compiler + interpreter for Q. Runs mostly on Windows but can also run to some extent on Linux
AA7     AA single-file assembler for x64
BCC     C-subset compiler x64/Win64 ABI. Can only handle one file at a time
PC      PCL7 processor for standalone PCL7 source files
MC7     M compiler that generates low level, linear C source code from PCL7 IL
RUNMX   Stub program for MX binaries produced by MM7/AA7/BCC/PC

````

### Old Projects
````
BCX    Original C-subset compiler, can compile multiple modules into EXE, uses integrate AA6 assembler
AA6    Older AA assembler, can assemble multiple ASM files into one binary. Used by BCX, and needed
       to 'link' individual ASM files produced by BCC, for multi-module projects
QQ6    Older Q interpreter with twin dispatchers: slow function-table dispatcher, and fast
       threaded-code/ASM-based dispatcher. (QQ7 uses HLL code only using faster techniques.)

````
