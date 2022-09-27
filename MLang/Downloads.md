## Downloads for MM, QQ, AA Tools

Each download is one self-contained file. Either a Windows binary `mm.exe`, or a `.ma` amalgamated source file, or a single C source file.

### Windows

Tool | Download File | Description | Build as
--- | --- | --- | ---
`mm.exe`	|	`mm.exe`	|	M Compiler for Windows, direct EXE generation | --
-- |	`mm.c`	|	(Alternative) | `gcc mm.c -omm.exe`
`mc.exe`	|	`mc.ma`	|	M Compiler for Windows, via C intermediate | `mm mc.ma`
`aa.exe`	|	`aa.ma`	|	x64-subset Assembler/Linker for Windows | `mm aa.ma`
`qq.exe`	|	`qq.ma`	|	Q interpreter for Windows | `mm qq.ma`
`bcc.exe`	|	`bcc.ma`	|	C-subset compiler for Windows | `mm bcc.ma`

### Linux
Tool | Download File | Description | Build as
--- | --- | --- | ---
`mu`	|		`mu.c`	|	M Compiler for Linux, via C intermediate | `gcc mu.c -omu -lm`
`qu`	|		`qu.c`	|	Q interpreter for Linux | `gcc qu.c -oqu -lm -ldl`

### Download Files

Only `mm.exe`, the M compiler, is supplied as a binary. If the supplied `mm.exe` cannot be used (eg. gives AV problems or just not trusted), try
compiling from `mm.c` using a C compiler.

Other binaries are built from `.ma` amalgamated source files using `mm.exe`, or from `.c` files using `gcc` or `tcc`.

Use `-O2` or `-O3` as desired if using gcc, to get best performance. When `tcc` (Tiny C) is used, it may need `-luser32` option on Windows.

(`.ma` files have been generated from original, discrete source modules using `mm -ma`; `.c` files have been generated using `mc -c` or (using version of `mu` running on Windows), `mu -c`.)

### Testing Binaries

This will test whether installed tools are working properly. Tools and inputs are assumed to be in the same place in the examples, otherwise just fill in their paths.

Download [hello.m](hello.m) or [hello.q](hello.q), although both just consist of this one line:
````
    println "Hello, World!", $date, $time
````

**Testing mm.exe:**
````
    mm hello                 # compile hello.m to hello.exe
    hello                    # run it
````
To test on something bigger, try building qq.exe:
````
    mm qq.ma
````
**Testing qq.exe:**
````
    qq hello
````
**Testing aa.exe:**
No ASM files in my format are provided. You can create one like this:
````
    mm -asm hello            # compile hello.m to hello.asm
    aa hello                 # assemble hello.asm to hello.exe
    hello                    # run executable
````
**Testing bcc.exe:**
Download any C Hello program, or use this:
````
#include <stdio.h>

int main(void) {
    printf("Hello, World!\n");
}
````
Run the compiler like this:
````
    bcc hello                # compile hello.c to hello.exe
    hello                    # run it
````
For something bigger, try:
````
    bcc mm                   # compile mm.c to mm.exe (note will overwrite original mm.exe if in same place)
````
