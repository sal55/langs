## Downloads for MM, QQ, AA Tools
(Page under construction, links not ready.)

Each download is one self-contained file. Either a Windows binary `mm.exe`, or a `.ma` amalgamated source file, or a single C source file.







### Languages

Language | Description | Tool
--- | --- | ---
**M** | Lower level systems language | `mm.exe` compiler (Windows)
 | | `mc.exe` compiler via C (Windows)
 | | `mu` compiler via C (Linux)
**Q** | Lower level scripting language | `qq.exe` interpreter (Windows)
 | | `qu` interpreter (Linux)
**ASM** | x64-subset assembly language | `aa.exe` assembler/linker
**C** | C-language subset | `bcc.exe` compiler

### Windows Downloads

Tool | Download File | Description | Build as
--- | --- | --- | ---
`mm.exe`	|	`mm.exe`	|	M Compiler for Windows, direct EXE generation | --
-- |	`mm.c`	|	(Alternative) | `gcc mm.c -omm.exe`
`mc.exe`	|	`mc.ma`	|	M Compiler for Windows, via C intermediate | `mm mc.ma`
`aa.exe`	|	`aa.ma`	|	x64-subset Assembler/Linker for Windows | `mm aa.ma`
`qq.exe`	|	`qq.ma`	|	Q interpreter for Windows | `mm qq.ma`
`bcc.exe`	|	`bcc.ma`	|	C-subset compiler for Windows | `mm bcc.ma`

### Linux Downloads
Tool | Download File | Description | Build as
--- | --- | --- | ---
`mu`	|		`mu.c`	|	M Compiler for Linux, via C intermediate | `gcc mu.c -omu -lm`
`qu`	|		`qu.c`	|	Q interpreter for Linux | `gcc qu.c -oqu -lm -ldl`

### Download Files

* Only `mm.exe`, the M compiler, is supplied as a binary. If the supplied `mm.exe` cannot be used (eg. gives AV problems or just not trusted), try
building from from `mm.c`
* Other binaries are built from `.ma` amalgamated source files using `mm.exe`
* Or from `.c` files using `gcc` or `tcc`. Use `-O2` or `-O3` as desired if using gcc, to get best performance.

Build instructions are shown above: just directly run `mm.exe` or `gcc`.

### Using Tiny C

This is very fast C compiler. If installed, it can be invoked automatically by using `mc -tcc` or `./mu -tcc`, which applies the necessary options.

For directly building the C sources files provided, use these options (examples):
```
    tcc mm.c -fdollars-in-identifiers -luser32 -lkernel32                 # Windows
    tcc mu.c -omu -fdollars-in-identifiers -luser32 -lkernel32 -lm -ldl   # Linux

(Generated C makes extensive use of '$', but for some reason Tiny C doesn't support them without that funny option.)

## Building `.ma` files

### Installation

There is no install process.

Just download the executable to wherever you want and run it from there. Or build the source bundle file wherever it's most convenient.

### Testing Binaries (Windows)

This will test whether installed tools are working properly. Tools and inputs are assumed to be in the same place in the examples, otherwise just fill in their paths.

Download [hello.m](Examples/hello.m) or [hello.q](../QLang/Examples/hello.q), although both just consist of this one line:
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


### Testing Binaries (Linux)

(Not ready.)

