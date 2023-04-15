## Overview of Languages and Tools

### Languages

Name | Description
--- | ---
**M** | Lower level static systems language
**Q** | Lower level dynamic scripting language
**PCL** | Intermediate Language (6)
**ASM** | x64 subset in my syntax
**C**  | C subset used by the BCC project

### Tools Summary

Name | Description
--- | ---
`mm.exe` | M compiler for Windows, M6 and M7 versions
`qc.exe` | Q bytecode compiler
`pci.exe` | PCL interpreter
`aa.exe` | ASM assembler
`bcc.exe` | C subset compiler
-- |
`mcc.exe` | M subset to C transpiler (part of M6 only) (3)
`qq.exe` | (1)
`ms.exe` | (2)
`mc.exe` | Execute `.mc` file (4)

### File Formats

Extension | Description
--- | ---
`.m` | M source files
`.ma` | M amalgamated source files in one source file (5)
`.q`  | Q source files
`.pcl` | PCL source file
`.asm` | x64 assembly in my syntax
`.exe` | Windows executable binary
`.pc`  | Binary bytecode file (represents complete application)
`.mc` | Private executable formats; mainly used in place of `.dll`

### Tools

Input files | Tool | Output  | Description
---| --- | --- | ---
`.m`  | `mm.exe` | `.exe` | M compiler default output
-- |             | `.pcl` | `-pcl` for IL output (M7 only)
-- |             | `.asm` | `-asm` for ASM output
-- |             | `.mc` | `-mc`; generate private binary format
-- |             | `.asm` | `-asm` for ASM output
-- |             | `.ma .mas`  | `-ma`; produce amalgamated source file. (`-mas` includes std lib sources)
-- |             | Run  | `-run` execute code immediately
-- | |
`.q` | `qc.exe`  | `.pc`  | Q compiler output
`.pc` | `pc.exe` | Run | Run bytecode program
`.q`|             | Run | (Invole `qc.exe` first then run)
`.pcl` | `pci.exe` | Run | Interpret PCL program
`.asm` | `aa.exe`  | `.exe` | Assemble to EXE (default)
-- |               | `.obj` | Single file COFF output
`.mc` | `mc.exe`   | Run | Execute `.mc` file when it is a complete program
`.c` | `bcc.exe`   | `.exe` | C compiler default output
-- |               | `.asm` | `-s` Produce ASM files
-- |               | `.obj`  | `-c` Produce COFF files
-- |               | `.pcl`  | `-pcl` Produce PCL files (experimental)
-- |               | `.i`  | `-e` Produce preprocessed output
-- |               | `.m` | `-mheader` Turn C headers into M syntax declarations (experimental)
`.m` | `ms.exe`    | Run | Run M program from source ('M Script') (2)
`.q` | `qq.exe`    | Run | Run Q program from source (1)

### Notes

**(1)** `ms.exe` is a copy of `mm.exe`. With that name, it defaults to `-run` option

**(2)** `qq.exe` is a copy of `pc.exe`. With that name, input files default to `.q` extension rather than `.pc`, so that it emulates the old `qq.exe` interpreter which was an all-in-one program

**(3)** A C target was an option of the M6 implementation; it has been dropped from M7. It did not support the full M language, so that programs such as the M compiler were tweaked to be able to transpile. This enabled the use of an optimising C compiler to get the best throughputs.

**(4)** The 'MC' binary (formerly 'MX' for apps and 'ML' for libraries) is my own simpler format intended to replace DLL, since DLL-generation worked badly. It can also produce standalone executables, but since they are not recognised by Windows, they need a stub program `mc.exe` to execute.

 A pure C version of `mc.exe` exists, and of several possibilities was to generate Linux binaries for x64, without needing to use external tools like `as`, `nasm` and `ld`. I only need to build the C stub program. (So, the format is portable across the two OSes, but the code contained will be ABI-specific.)
 
 **(5)** The M compiler can combine all the source and support files of an application into a single 'amalgamated' `.ma` source file. This one file can be built directly using `mm prog.ma` or run using `ms prog.ma`. A similar ability for the Q compiler has been dropped now that it can produce monolithic `.pc` bytecode files.
 
 **(6)** The 'PCL' IL was dropped in M6 and being reinstated in M7. Here there will be a serious attempt to have this as independent language, and to implement an interpreter which could serve as a reverence implementation. The backend of M7 which turns PCL into x64 code has been put on hold, as the PCL IL is being refined.
 
 There are a few possibilities with PCL (including replacing the C compiler backend to generate PCL, to allow testing on external programs), but the interpreter was to be completed first; it's harder than I expected!
