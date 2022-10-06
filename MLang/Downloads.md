## Downloads for M, Q and Support Languages
(Page under construction, links not ready.)

Language | Description | Tool
--- | --- | ---
**M** | Lower level systems language | `mm.exe` M compiler (Windows)
 | | | `mc.exe` M compiler via C (Windows)
 | | | `mu` M compiler via C (Linux)
**Q** | Lower level scripting language | `qq.exe` Q interpreter (Windows)
 | | | `qu` Q interpreter (Linux)
**ASM** | x64-subset assembly language | `aa.exe` ASM assembler/linker
**C** | C-language subset | `bcc.exe` C-subset compiler

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

### Building From Source

All the software here is written in my M language.

Only `mm.exe` is provided as a pre-built binary for Windows. Other versions need to be built from source: either actual M sources (the .ma files) or from a C rendering (the .c files), whatever is provided. (Note that github is primarily for source code, hence binaries are kept to a minimum.)

If `mm.exe` can't be used (AV issues or not trusted), then try building from `mm.c`.

Build instructions are shown above for `mm` and `gcc`. For the latter, add `-O2` or `-O3` as desired.

#### Using Tiny C

This is a very fast C compiler. If installed, it can be invoked automatically by using `mc -tcc` or `./mu -tcc`, which applies the necessary options.

For directly building the C sources files provided, use these options (examples):
```
    tcc mm.c -fdollars-in-identifiers -luser32 -lkernel32                 # Windows
    tcc mu.c -omu -fdollars-in-identifiers -luser32 -lkernel32 -lm -ldl   # Linux
```
(Generated C makes extensive use of '$', but for some reason Tiny C doesn't support them without that funny option.)

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

