## Language Tools

These all work on and for 64-bit Windows.

I'm documenting only the basic compilers. For things like editors and IDEs, everyone will have their own preferences. No one would interested in my stone-age text-based tools anyway (but I will mention that they are of course implemented in my languages; here I used my Q scripting language).

### M Compiler mm.exe

This is a 0.5MB executable which incorporates everything needed to compile M programs, including the sources of M's small standard library. Currently these are just compiled as part of each application.

No dependencies are involved in building M apps, except for the msvcrt.dll library that provides some C functions, but this is part of Windows. Plus any external libraries that an application itself might needed, such as opengl.dll.

No separate build system is needed, only the compiler.

Inputs | Description
--- | ---
`.m` file | Lead module of application. Other modules/support files will be discovered
`.ma` file | Amalgamated source file of aplication created with -ma option
`.dll` file | (Specified in project header) External libraries
`.ml` file | (Specified in project header) External libraries

External libraries can only be DLL or ML files. Some - msvctrt.dll, user32.dll, gdi32.dll, kernel32.dll - are included automatically. Others are listed in the project header that describes the application's module structure. The project header is in the lead module.

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
`.m` file | -- | Create exports (interface) file when using `-ml`
`.q` file | -- | Create exports (interface) file when `-ml`
`.txt` file | `-docs` | Create Documentation file of functions with doc-strings

M can no longer directly generate DLL files, as the output turned out to be buggy for certain programs, and I was unable to establish the reason.

Instead, a simpler ML library format was devised, for use only with M applications. To generate an actual DLL, it may be possible by generating ASM, then producing an OBJ file from that, then using an external tool such as GCC.

### Assembler aa.exe

This was created to replace unreliable and slow third party assemblers and linkers. Initially it generated OBJ files (still requiring a linker to produce an EXE), later it generated EXE directly.

Eventually, the backend was directly incorporated into the M compiler. But the AA assembler is still used for:

* Development of a compiler, since it is much easier to debug when it produces ASM
* When an OBJ file is needed, which may be necessary to produce DLLs and work with object files from other languages
* My C compiler

aa.exe is a 160KB executable. Input is usually a single ASM file (from my whole-program compiler), but it can also cope with multiple ASM files. This is still necessary for my C compiler as that uses independent compilation.

AA can 'link' multiple ASM modules (no discrete linker is needed) and direct generate a single EXE or OBJ file. (Also a DLL file, but that is now unreliable.)

Inputs | Description
--- | ---
.asm files | One or more ASM files in my own syntax
.dll files | Any DLL files required
.ml files | Any ML files (specified via directives)

AA will automatically search through msvcrt user32 gdi32 kernel32 DLLs. Any other DLLs need to be listed either on the command line or with an `importdll` directive within the ASM source. Any ML files are specified with `importlib` directives.

Outputs | Option | Description
--- | --- | ---
`.exe` | `-exe` | (Default) Assemble to EXE format
`.obj` | `-obj` | Assemble to single OBJ file
(`.dll` | `-dll` | Assemble to single DLL file - deprecated)

Options to produce ML, MX files, or to directly run the result, have been removed. These can be used from the M compiler only. (These features were initially developed on the assembler before becoming part of the M compiler.)

### Q Compiler/Interpreter qq.exe

This is a 0.8MB executable, including 350KB of source file of Q's standard library. It runs Q applications directly from source by submitted the lead module.

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

These all follow the same pattern:
```
    mm prog            # compile M program prog.m to EXE
    qq prog            # run Q program prog.q
    aa prog            # assembler ASM program to prog.exe
```
For M and Q progs, the module scheme will let it discover all modules that comprise the app.

Extensions are not usually needed. (mm knows its job is to compile .m modules; qq nows its only purpose is to run .q files; and similarly for aa. Funny how few other tools seem to get it.) They are advised however when building an amalgamated file such as prog.qa (the presence of prog.q will make it choose the latter).

For other options, just type `mm`, `qq` or `aa` with no inputs to show them, or show how to get a listing.

#### Option Placement

Generally the tools don't care whether options go before the input files, after, or both. But there are exceptions:

* `mm` used with `-run`: the `-run` option and any others must go before the input file, of which there should only be one. This is because anything following is interpreted as inputs to the program being run. See example below.
* `qq` needs any options before the filename, as anything following are command line parameters picked up by the program being run. (However qq has few options.)

#### Using `mm` with `-run`

This example:
```
    c:\mx>mm -run mm hello
    Compiling mm.m to memory
    Compiling hello.m------- to hello.exe
```
Compiles the mm compiler into memory, and runs it immediately with input of hello. Such programs can be stacked:
```
    c:\mx>mm -run mm -run mm -run hello
    Compiling mm.m to memory
    Compiling mm.m to memory
    Compiling hello.m to memory
    Hello World!
```
This builds the M compiler, runs it to build a second generation compiler, then runs that on hello.m (and does it faster than it takes gcc just to conventionally compile hello.c; that is, without gcc building itself first, not even once).

#### Running M Scripts

This can be done with the `-run` option as shown, but that gets in the way. Nicer to not need it. Well, if `mm.exe` is copied to `ms.exe`, it will detect that special file name ('MS' for 'M Script') and invoke -run automatically (here it suppresses the Compiling message to keep the magic):
```
    c:\mx>ms hello
    Hello World!
```

#### Using Amalgamated Source Files

The `-ma` or `-qa` options on both MM and QQ take a normal application of multiple modules and support files that may exist in different folders, and represent then in a single `.ma` or `.qa` source file. This then allows:

* Easy distribution: upload, download, email
* Easy backup
* For Q programs, which no longer compile to monolithic binary bytecode files, apps must be distributed as sources. This feature allows any app to be tidily packaged into a single file. (Further bundling the .qa files into a copy of the one-file interpreter is a possibility.)
* MM can directly compile apps from .ma files, and QQ can directly run from .qa files too (this would be a problem using ZIP)

Such files do not include the language's standard libraries, which are expected to be part of mm.exe or qq.exe. Those can be included using `-mas` or `-qas` instead. That also ensures the correct version of the library will be used.

There are some restrictions:

* Any directory structure in the application sources is flattened; this means all file names must be unique
* Embedded binary files currently can't be included, as .ma/.qa is text format
* Any support files (ie. using `include` or `strinclude`) cannot includes lines starting with "=== " since this is used as a file separator. (I can fix this, but it means a more elaborate file structure with separate directory info.)

Example of use:
```
    c:\mx>mm -ma mm
    Compiling mm.m---------- to mm.ma
    Writing  mm.ma
```
That created a single 37Kloc/0.7MB file containing the compiler sources. This can now be easily copied and compiled:
````
c:\demo>copy \mx\mm.ma
    1 file(s) copied.
c:\demo>mm mm.ma
Compiling mm.ma--------- to mm.exe
````

### ML and MX Files

As I said, generated DLL files were buggy, and I couldn't find the problem. But they also had other issues: it was never clear whether their environment was compatible with the host application (if not, it meant memory allocations and file handles could not be shared cross the interface). And accessing shared variables required an extra mechanism with the EXE file.

So I devised a private binary format called ML, and using a `.ml` extension. I call these LIBs, separate from DLLs, which are still supported to allow external libraries to be used; they just can't be created reliably.

ML is a much simpler format than DLL; it allows shared variables more easily; and its environment is alway shared with the host. But dynamic linking with ML files must be taken care of with my own code; the OS won't recognise them.

(I then found that ML format can be used to represent an standalone application too. These I call MX files. But because the format is not known to the OS, they need a special stub program, `run.exe` to execute them, roughly 12KB minimum size. `run.exe` is a conventional executable.

However, the uses of an MX format are not so obvious. One might be, if someone is struggling to generate EXE files, they can write the much simpler MX instead. They will need `run.exe`, but that can bundled with the MX file into a normal-looking single EXE. Also, the MX format can be portable across Windows and Linux.

It was anyway an interesting by-product of ML files.)

ML files will form a key part of making M and Q programs work together.
