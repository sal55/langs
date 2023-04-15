## Language Projects 2023

A summary of Languages and Tools that are still active or are being developed.

### Languages

Name | Description
--- | ---
**M** | Lower level static systems language
**Q** | Lower level dynamic scripting language
**PCL** | Intermediate Language (6)
**ASM** | x64 subset in my syntax
**(C)**  | (C subset used by the BCC project)
**ZA** | Z80 assembly (project shelved)

### Tools Summary

Name | Description
--- | ---
`mm.exe` | M compiler for Windows, M6 and M7 versions
`qc.exe` | Q bytecode compiler (writes bytecode)
`pc.exe` | Q interpreter (runs bytecode, or source via `qc.exe`)
`pci.exe` | PCL IL interpreter
`aa.exe` | ASM assembler
`bcc.exe` | C subset compiler
. |
`mcc.exe` | M subset to C transpiler (part of M6 only) (3)
`qq.exe` | Run Q from source (1)
`ms.exe` | Run M from source (2)
`mc.exe` | Execute `.mc` file (4)
`za.pc` | Z80 assembler (7)

### File Formats

Extension | Description
--- | ---
`.m` | M source files
`.ma` | M amalgamated source files in one source file (5)
`.q`  | Q source files
`.pcl` | PCL source file
`.asm` | x64 assembly in my extensively tweaked Intel syntax
`.exe` | Windows executable binary, PE+ format
`.obj` | Windows object file, COFF64 format
`.pc`  | Binary bytecode file (represents complete application)
`.mc` | Private executable format; mainly used in place of `.dll`
`.za` | Z80 assembly (tweaked Zilog syntax)

### Tools: Inputs and Outputs

Tool | Input files | Outputs  | Description
---| --- | --- | ---
`mm.exe` | `.m .ma`  | `.exe` | M compiler default output
. |             | `.pcl` | `-pcl` for IL output (M7 only)
. |             | `.asm` | `-asm` for ASM output
. |             | `.mc` | `-mc`; generate private binary format
. |             | `.asm` | `-asm` for ASM output
. |             | `.ma .mas`  | `-ma`; produce amalgamated source file. (`-mas` includes std lib sources)
. |             | Run  | `-run` execute code immediately
------- | | 
`qc.exe` | `.q` | `.pc`  | Q compiler output
------- | |
`pc.exe` | `.pc .q` | Run | Run bytecode program (default), or run Q source via `qc.exe`
------- | |
`pci.exe` | `.pcl` | Run | Interpret PCL program
------- | |
`aa.exe` | `.asm` | `.exe` | Assemble to EXE (default)
. |               | `.obj` | Single file COFF output
------- | |
`mc.exe` | `.mc`   | Run | Execute `.mc` file when it is a complete program
------- | |
`bcc.exe` | `.c`   | `.exe` | C compiler default output
. |               | `.asm` | `-s` Produce ASM files
. |               | `.obj`  | `-c` Produce COFF files
. |               | `.pcl`  | `-pcl` Produce PCL files (experimental)
. |               | `.i`  | `-e` Produce preprocessed output
. |               | `.m` | `-mheader` Turn C headers into M syntax declarations (experimental)
------- | |
`ms.exe` | `.m`  | Run | Run M program from source ('M Script') (2)
------- | |
`qq.exe` | `.q`    | Run | Run Q program from source (1)

### Dependencies

* Most programs run on Windows OS
* No external tools needed except when generated .c or .obj files require further processing
* Unless an application requires a specific library (eg. SDL), no external libraries are used other than parts of the C library in `msvcrt.dll`, part of Windows. Other libraries need a suitable DLL binary, plus (this is the hard part) an API in the form of a set of bindings in my syntax.
* All programs here are written in my M language, and the compiler for that is self-hosted.


### Notes

**(1)** `ms.exe` is a copy of `mm.exe`. With that name, it defaults to `-run` option

**(2)** `qq.exe` is a copy of `pc.exe`. With that name, input files default to `.q` extension rather than `.pc`, so that it emulates the old `qq.exe` interpreter which was an all-in-one program; use `qq hello` instead of `pc hello.q`.

**(3)** A C target was an option of the M6 implementation; it has been dropped from M7. It did not support the full M language, so that programs such as the M compiler were tweaked to be able to transpile. This enabled the use of an optimising C compiler to get the best throughputs.

**(4)** The 'MC' binary formerly (formerly 'MX' for apps and 'ML' for libraries) is my own simpler format intended to replace DLL, since DLL-generation worked badly. It can also produce standalone executables, but since they are not recognised by Windows, they need a stub program `mc.exe` to execute.

 A pure C version of `mc.exe` exists, and one of several possibilities was to generate Linux binaries for x64, without needing to use external tools like `as`, `nasm` and `ld`. I only need to build the C stub program. (The format is portable across the two OSes, but the code contained will be ABI-specific.)
 
 **(5)** The M compiler can combine all the source and support files of an application into a single 'amalgamated' `.ma` source file. This one file can be built directly using `mm prog.ma` or run using `ms prog.ma`. A similar ability for the Q compiler has been dropped now that it can produce monolithic `.pc` bytecode files.
 
 **(6)** original 'PCL' IL was dropped in M6 and is being reinstated in revised form in M7. Here there will be a serious attempt to have this as an independent language, and to implement an interpreter which could serve as a reference implementation. The backend of M7 which turns PCL into x64 code has been put on hold, as the PCL IL is being refined.
 
There are a few possibilities with PCL (including replacing the C compiler backend to generate PCL, to allow testing on external programs), but the interpreter has to be completed first; it's harder than I expected!

**(7)** This is a Z80 assembler written Q, which was supposed to have an emulator but that part has been shelved.
