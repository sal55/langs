## Downloads for MM, QQ, AA Tools

Each download is one self-contained file. Either a Windows binary `mm.exe`, or a .ma amalgamated source file (build with `mm.exe`), or a single C source file (build with `gcc` or `tcc`).

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

Use `-O2` or `-O3` as desired if using gcc, to get best performance. Tiny C (tcc) can also be used, but may need `-luser32` option on Windows.
