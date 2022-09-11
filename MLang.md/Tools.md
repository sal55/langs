## Language Tools

### M Compiler mm.exe

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

### Assembler aa.exe

This was created to replace unreliable and slow third party assemblers and linkers. Initially it generated OBJ files (still requiring a linker tor produce an EXE), later it generated EXE directly.

Eventually, the backend was directly incorporated into the M compiler. But the AA assembler is still used for:

* Development of a compiler, since it is much easier to debug when it produces ASM
* When an OBJ file is needed (not supported by the compiler), which may be necessary to produce DLLs and work with object files from other languages
* My C compiler

aa.exe is a 160KB executable. Input is usually a single ASM file (from my whole-program compiler), but it can also cope with multiple ASM files. This is still necessary for my C compiler as that uses independent compilation.

AA can 'link' multiple ASM modules (no discrete linker is needed) and direct generate a single EXE or OBJ file. (Also a DLL file, but that has been shown to be buggy.)

Inputs | Description
--- | ---
.asm files | One or more ASM files in my own syntax
.dll files | Any DLL files required
.ml files | Any ML files (specified via directives)

AA will automatically search through msvcrt user32 gdi32 kernel32 DLLs. Any other DLLs needed to listed either on the command line or with an `importdll` directive within the ASM source. Any ML files are specified with `importlib` directives.

Outputs | Option | Description
--- | --- | ---
`.exe` | `-exe` | (Default) Assemble to EXE format
`.obj` | `-obj` | Assemble to single OBJ file
(`.dll` | `-dll` | Assemble to single DLL file - deprecated)

Options to produce ML, MX files, or to directly run the result, have been removed. These can be used from the M compiler only. (These features were initially developed on the assembler before becoming part of the M compiler.)

### Q Compiler/Interpreter qq.exe

This is a 0.8MB executable, including 350KB of sources file of Q's standard library. It runs Q applications directly from source by submitted the lead module.

Inputs | Description
--- | ---
`.q` file | Lead module of application. Other modules/support files will be discovered
`.qa` file | Amalgamated source file of aplication created with -qa option

Outputs | Option | Description
--- | --- | ---
Run | -- | (Default) Compile and immediately execute application in-memory
-- | --
`.qa` file | `-qa` | Create amalgamated source file (excludes standard library)
`.qa` file | `-qas` | Create amalgamated source file (includes standard library)
`.txt` file | `-docs` | Create Documentation file of functions with doc-strings

### Usage Examples

These all follow the same pattern (all products use optional extensions):
```
    mm prog            # compile M program prog.m to EXE
    qq prog            # run Q program prog.q
    aa prog            # assembler ASM program to prog.exe
```
For M and Q progs, the module scheme will let it discover all modules that comprise the app.

Extensions are not usually needed. (mm knows its job is to compile .m modules; qq nows its only purpose is to run .q files; and similarly for aa. Funny how few other tools seem to get it.) They are advised however when building an amalgamated file such as prog.qa (the presence of prog.q will make it choose the latter).

For other options, just type `mm`, `qq` or `aa` with no inputs, and it show them, or show how to get a listing.

#### Option Placement

Generally the tools don't care whether options go before the input files, after, or both. But there are exceptions:

* `mm` used with `-run`: the `-run` option and any others must go before the input file, of which there should only be one. This is because anything following is interpreted as inputs to the program being run. See example below.
* `qq` needs any options before the filename, as anything following are command line parameters picked up by the program being run. (However qq has few options.)

#### `mm` and `-run`

This example:
```
    c:\mx>mm -run mm hello
    Compiling mm.m to memory
    Compiling hello.m------- to hello.exe
```
Compiles the mm compiler into memory, and runs it immediately with input of hello. Such programs can be stacked:
```
    c:\mx>mm -run mm -run hello
    Compiling mm.m to memory
    Compiling hello.m to memory
    Hello World!
```
This builds the M compiler, runs it on hello.m, and runs that.

I'm looking at ways to automaticaly how this behaviour (but building a special version of mm.exe, or or it detected a special renamed copy of mm.exe) without having to use `-run` option. This would then allow M programs to be run just like Q scripts. If I mock this up, then this line:
```
   mm mm mm mm mm mm mm mm mm mm mm hello
```
Compiles 10 successive generations of the compiler, before running hello.m. It takes 0.75 seconds (running unoptimised code on a cheap PC).

### ML and MX Files

As I said, generated DLL files were buggy, and I couldn't find the problem. But they also had other issues: it was never clear whether their environment was compatible with the host application (if not, it meant memory allocations and file handles could not be shared cross the interface). And accessing shared variables required an extra mechanism with the EXE file.

So I devised a private binary format called ML, and using a `.ml` extension. I call this LIBs, separate from DLLs, which are still supported to allow external libraries to be used.

ML is a much simpler format than DLL; it allows shared variables more easily; and its environment is alway shared with the host. But dynamic linking with ML files must be taken care of with my own code; the OS won't recognise them.

(I then found that ML can be used to represent an standalone application too. These I call MX files. But because the format is not known to the OS, they need a special stub program, `run.exe` to execute them, roughly 12KB minimum size. `run.exe` is a conventional executable.

However, the uses of an MX format are not so obvious. One might be, if someone is struggling to generate EXE files, they can write the much simpler MX instead. They will need `run.exe`, but that can bundled with the MX file into a normal EXE. Also, the MX format can be portable across Windows and Linux.

It was anyway an interesting by-product of ML files.)

ML files will form a key part of making M and Q programs work together.

