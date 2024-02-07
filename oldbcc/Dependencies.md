### MCC External Dependencies

If you want to do anything with the output of Mcc (like, being able to execute it) then currently external tools are needed.

#### Dealing with .asm files

This is one of those untidy aspects of compilation that it is hoped the next phase (running programs in-memory) will do away with.

But these need to be dealt with for now, or if a conventional executable is wanted. These extra resources are needed:

* The [Nasm assembler](http://www.nasm.us/)

* A linker that can deal with the object files produced by Nasm. See Linkers below.

* The asm include file '[mccasm.inc](mccasm.inc)' (should be on this site).

* The asm file '[mcclib.asm](mcclib.asm)' (should be on this site), which is needed for some features (setjmp, longjmp, and unsigned int to float conversions). Assemble this once using: nasm -fwin64 mcclib.asm, and update when mcc is updated.

Then the process becomes:
```
  mcc hello                               # hello.c to hello.asm
  nasm -fwin64 hello.asm                  # hello.asm to hello.obj
  (Linking: see below)
  hello                                   # run hello.exe
 ```

#### Linkers

I've been using an old version of 'ld' derived from gcc/tdm/mingw. But I've found that the latest version has problems. So the recommendations are currently these:

* Use [golink](http://www.godevtool.com/)

* Use gcc itself (you probably used it to compile and link mcc)

Examples to link the above file:
```   
    golink /console /entry main -s /fo hello.exe hello.obj c:\windows\system32\msvcrt.dll

    gcc -m64 -s -ohello.exe hello.obj
```    
The use of gcc is unsatisfactory. It invokes ld, and that used to be able to used standalone without the rest of gcc. But ld now seems to ignore .dll files; it needs access to gcc's normal libraries.

#### C Runtime Library

It doesn't have one; it will use msvcrt.dll. However, this is now classed as an internal library used by Windows. Better to link with something like msvcr100.dll or msvcr120.dll (see which is available on your system). These are also more up-to-date.

#### Integration

A typical C compiler will drive the compilation, assembly (if used) and linking steps from the same compiler program.

Mcc is not integrated like this (although it is possible to do things with batch files probably).

It is a 'bare-bones' compiler. Everything is open rather than hidden away as it with a big compiler like gcc:

* Mcc itself only produces the .asm file

* It won't assemble it; you need Nasm

* It won't link it; you need golink, gcc, or perhaps an older version of ld

* There is no runtime library; it uses msvcrt.dll (or newer versions).

* There are no hidden libraries that are automatically added (only mcclib.asm, which is a few dozen lines, and you have to add it explicitly).

(When used from my own IDE, which does the assembly step transparently, then it works just like any other C compiler. Many also have special requirements for linking. However, the extraordinary time it sometimes take to run Nasm on the output gives the game away.)
