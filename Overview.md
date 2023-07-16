## Language Projects 2023

A summary of Languages and Tools that are still active or are being developed.

Updated July 2023

### Languages

Name | Description
--- | ---
**M** | Lower level static systems language
**Q** | Lower level dynamic scripting language
**ASM** | x64 subset in my syntax
**(C)**  | (C subset used by the BCC project)
**PCL** | Intermediate Language (1)
**ZA** | Z80 assembly (project shelved)

### Tools Summary

Name | Description
--- | ---
`mm.exe` | M compiler for Windows, M6 version
`qq.exe` | Q bytecode compiler and interpreter
`aa.exe` | ASM assembler
`bcc.exe` | C subset compiler
. |
`ms.exe` | Run M from source (2)
`mmp.exe` | Compile M code to PCL intermediate language
`pci.exe` | Interpret PCL files
`run.exe` | Run .mx file (3)
`za.pc` | Z80 assembler (4)

### File Formats

Extension | Description
--- | ---
`.m` | M source files
`.ma` | M amalgamated source files in one text file (5)
`.q`  | Q source files
`.asm` | x64 assembly in my extensively tweaked Intel syntax
`.exe` | Windows executable binary, PE+ format
`.obj` | Windows object file, COFF64 format
`.pc`  | Binary bytecode file (represents complete application)
`.ml/.mx` | Private executable format; mainly used in place of `.dll`
`.pcl` | PCL source file
`.za` | Z80 assembly (tweaked Zilog syntax)

### Tools: Inputs and Outputs

Tool | Input files | Outputs  | Description
---| --- | --- | ---
`mm.exe` | `.m .ma`  | `.exe` | M compiler default output
. |             | `.asm` | `-asm` for ASM output
. |             | `.ml` | `-ml`; generate private binary format (libs)
. |             | `.mx` | `-mx`; generate private binary format (executables)
. |             | `.ma .mas`  | `-ma`; produce amalgamated source file. (`-mas` includes std lib sources)
. |             | Run  | `-run` execute code immediately
------- | | 
`qq.exe` | `.q` | Run  | Q compiler compilers and runs Q programs
------- | |
`aa.exe` | `.asm` | `.exe` | Assemble to EXE (default)
. |               | `.obj` | Single file COFF output
`pci.exe` | `.pcl` | Run | Interpret PCL program (1)
------- | |
------- | |
`run.exe` | `.mx`   | Run | Execute `.mx` file (3)
------- | |
`bcc.exe` | `.c`   | `.exe` | C compiler default output
. |               | `.asm` | `-s` Produce ASM files
. |               | `.obj`  | `-c` Produce COFF files
. |               | `.pcl`  | `-pcl` Produce PCL files (experimental) (1)
. |               | `.i`  | `-e` Produce preprocessed output
. |               | `.m` | `-mheader` Turn C headers into M syntax declarations (experimental)
------- | |
`ms.exe` | `.m`  | Run | Run M program from source ('M Script') (2)
------- | |
`mmp.exe` | `.m`  | .pcl| Compile M program to `.pcl` textual IL format
------- | |

### Dependencies

* Most programs run on Windows OS
* Unless an application requires a specific library (eg. SDL), no external libraries are used other than parts of the C library in `msvcrt.dll`, part of Windows. Other libraries need a suitable DLL binary, plus (this is the hard part) an API in the form of a set of bindings in my syntax.
* All programs here are written in my M language, and the compiler for that is self-hosted.


### Notes


**(1)** 'PCL' is a now abandoned, discrete Intermediate Language. The special M compiler `mmp.exe` is maintained to generate textual PCL files, and the
PCL interpreter `pci.exe` is used to run them. There had been ideas to use this as a basis for a debugger.

**(2)** `ms.exe` is a copy of `mm.exe`. With that name, it defaults to `-run` option

**(3)** The 'ML'/'MX' binary format is my own simpler format intended to replace DLL, since DLL-generation is faulty. 'ML' is used for
libraries. 'MX` can represent complete executables, but since it is not recognised by Windows, needs a stub program `run.exe` to run them. (I 
haven't found a solid use for this yet!)

**(4)** This is a Z80 assembler written Q, which was supposed to have an emulator but that part has been shelved.

**(5)** The M compiler can combine all the source and support files of an application into a single 'amalgamated' `.ma` source file. This one file can be built directly using `mm prog.ma` or run using `ms prog.ma`. Currently binary support files can't be included unless I go back to an earlier format.
 
Bart ('till-one` on Reddit)

