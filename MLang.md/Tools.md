## Language Tools

### M Compiler

This is a 0.5MB executable which incorporates everything needed to compiler M programs, including the sources of M's small standard library. Currently these are just compiled in to each application.

No dependencies are involved in building M apps, except for the msvcrt.dll library that provides some C functions, but this is part of Windows.

Plus any external libraries that application itself might needed, such as opengl.dll.

Inputs | Description
--- | ---
`.m` file | Lead module of application. Other modules/support files will be discovered
`.ma` file | Amalgamated source file of aplication created with -ma option
`.dll` file | (Specified in project header) External libraries
`.ml` file | (Specified in project header) External libraries

External libraries can only be DLL files. Some - msvctrt, user32, gdi32, kernel32 - are included automatically. Others are listed in the project header that describes the applications module structure. The project header is in the lead module.

It it not possible to directly include object, lib or archive files of other languages. This may be possible by generating .asm outputs, and getting the assembler to produce a conventional .obj file (which will represent the entire program). But this then requires external tools.


Outputs | Option | Description
--- | --- | ---
`.exe` file | `-exe` | (Default) Compiler to executable file
`.asm` file | `-asm` | Generate single ASM file representing entire app
`.mx` file | `-mx` | Create private executable format
`.ml` file | `-ml` | Create private shared library format
Run | `-run` | Compiler and immediately execute application in-memory
-- | --
`.ma` file | `-ma` | Create amalgamated source file (excludes standard library)
`.ma` file | `-mas` | Create amalgamated source file (includes standard library)
`.m` file | `-exp` | Create exports (interface) file when creating `.ml` shared library
`.q` file | `-expq` | Create exports (interface) file when creating `.ml` shared library
`.txt` file | `-docs` | Create Documentation file of functions with doc-strings
