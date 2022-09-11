## Language Tools

### M Compiler

This is a 0.5MB executable which incorporates everything needed to compiler M programs, including the sources of M's small standard library. Currently these are just compiled in to each application.

No dependencies are involved in building M apps, except for the msvcrt.dll library that provides some C functions, but this is part of Windows. Plus any external libraries that application itself might needed, such as opengl.dll.

No separate build system is needed, only the compiler.

Inputs | Description
--- | ---
`.m` file | Lead module of application. Other modules/support files will be discovered
`.ma` file | Amalgamated source file of aplication created with -ma option
`.dll` file | (Specified in project header) External libraries
`.ml` file | (Specified in project header) External libraries

External libraries can only be DLL or ML files. Some - msvctrt, user32, gdi32, kernel32 - are included automatically. Others are listed in the project header that describes the applications module structure. The project header is in the lead module.

It it not possible to directly include object, lib or archive files of other languages. This may be possible by generating .asm outputs, and getting the assembler to produce a conventional .obj file (which will represent the entire program). But this then requires external tools.


Outputs | Option | Description
--- | --- | ---
`.exe` file | `-exe` | (Default) Compiler to executable file
`.asm` file | `-asm` | Generate single ASM file representing entire app
`.mx` file | `-mx` | Create private executable format
`.ml` file | `-ml` | Create private shared library format
Run | `-run` | Compile and immediately execute application in-memory
-- | --
`.ma` file | `-ma` | Create amalgamated source file (excludes standard library)
`.ma` file | `-mas` | Create amalgamated source file (includes standard library)
`.m` file | `-exp` | Create exports (interface) file when creating `.ml` shared library
`.q` file | `-expq` | Create exports (interface) file when creating `.ml` shared library
`.txt` file | `-docs` | Create Documentation file of functions with doc-strings

M can no longer directly generated DLL files, as the output turned out to be buggy for certain programs, and I was unable to establish the reason.

Instead, a simpler ML library format was devised, for use only with M applications. To generate an actual DLL, it may be possible by generating ASM, then producing an OBJ file from that, then using an external tool such as GCC.

### AA Assembler

This was created to replace unreliable and show third party assemblers and linkers. Initially it generated OBJ files (still requiring a linker tor produce an EXE), later it generated EXE directly.

Eventually, the backend was directly incorporated into the M compiler. But the AA assembler is still used for:

* Development of a compiler, since it is much easier to debug when it produces ASM
* When an OBJ file is needed (not supported by the compiler), which may be necessary to produce DLLs and work with object files from other languages
* My C compiler (although it dispenses with the CLI front end of the assembler)

Input to AA is usually a single ASM file (from my whole-program compiler), but it can also cope with multiple ASM files. This is still necessary for my C compiler as that uses independent compilation.

AA can 'link' multiple ASM modules (no discrete linker is needed) and direct generate a single EXE or OBJ file. (Also a DLL file, but that has been shown to be buggy.)



