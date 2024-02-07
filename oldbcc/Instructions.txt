## 'MCC' C Compiler

For now, Mcc will concentrate on compiling programs for 64-bit Windows.

This interim version will compile a single .c source file, and write an .asm output file in Nasm format.

There are many omissions and various bugs; see [Limitations](Limitations.md). However, enough is working for mcc to compile itself, or a reasonable selection of C programs. Effectively, a subset of the language is compiled, although that is not rigorously defined (if it works, then it's in the subset, otherwise it is't!).

### Building Mcc

* Download the source file [mcc64.c](mcc64.c) (everthing is in this one file, including standard headers.)

* Compile a 64-bit version using, for example (for Windows):
```
  gcc -O3 -m64 mcc64.c -omcc.exe
```
(I don't support a 32-bit built at the minute.)

### Running Mcc

Use either of:
```
  mcc program
  mcc program.c
```
to compile program.c to program.asm. Typing:
```
  mcc -help
```
will list some other options, mostly of use for debugging or testing the compiler.

### Dependencies

To do anything with the asm output requires using other tools. See [Dependencies](Dependencies.md).

That side tends to be untidy so I want to keep that part separate.

But also, later versions of Mcc that can run programs directly, won't need those tools.

### Extracting the Standard Headers

Use:
```
  mcc -writeheaders
```
to write all the headers out as a series of .hdr files. These can then be viewed or patched as needed.
(They are not .h files to avoid overwriting any existing .h files or overriding other headers as the current directory is searched first.)

Rename to .h, and preferably move elsewhere, to make use of them from Mcc. The -ext and -i options will be needed.

### Compiling Itself
 
 Once mcc works on hello.c, you might try it on itself:
 ```
     mcc mcc64
 ```
Which should produce the file mcc64.asm64.

### Compilation Speed

The mcc64.c file is just about big enough to get an idea of speed (no matter that Nasm will take ages to assemble the output, literally 1000 times slower or worse for big modules):
```
    mcc -time mcc64
    mcc -gen1 -time mcc64
```
The -gen1 option excludes the asm source generation which is time-consuming, especially with the newer code generator that produces much more ASM (generating binary directly should be faster so final speed ought to be between the two figures). Timings shown exclude overheads of starting and terminating the process.

### Bugs

See [Limitations](Limitations.md).

### Linux

This should build on Linux:
```
    gcc -O3 -m64 mcc64.c -omcc -lm -ldl
```
but the resulting program will be a cross-compiler, producing .asm files with code that will only run on Windows.

However, it can still be usefully run using -E to produce tokenised output, or to try out on some C code to see what happens. Although C code that normally runs on Linux will probably expect Linux headers.
