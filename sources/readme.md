## Source Code of Language Projects

The various source files of my projects are too numerous to upload individually.

The files here are amalgamations created by my compiler. They can be compiled directly in that form, or with a simple tool can be extracted again.

* The .ma extension apparently means something to github, so the text is highlighted in a funny way.
* All sources have been detabbed (hard tabs converted to 4-space tabs)
* All have been stripped of comments (most were actually used for ugly debugging code)
* These files are snapshots of the actual sources

### Guide to Projects

Language | Product  | Project | Written In | Lead Module | Amalgamation
--- | --- | --- | --- | --- | ---
M |  mm.exe | mx |  M  | mm.m | mm.ma 
Q |  qq.exe | qx |  M  | qq.m | qq.ma
ASM |  aa.exe |  ax  | M |  aa.m | aa.ma
C |  bcc.exe | cx |  M  | cc.m | cc.ma


Product | Description
--- | ---
aa | x64-subset Assembly/Linker
bcc | Experimental C-subset compiler 
mm | M compiler
qq | Q compiler/interpeter

Language | Description
--- | ---
M | My systems programming language
Q | My dynamic, interpreted, embeddable scripting language
ASM | In this context, the source format of my x64-subset assembly language
C  | Here, it means the large subset of C that my bcc product compiles

#### Notes

* **'Product'** is the name of the executable. For these programs, it is always a single, self-contained file
* **'Project'** is the name of the project folder on my machine

### Building the Projects

A binary `mm.exe` has been provided that can be used to turn any of those amalgamations into an executable.

This needs Windows 64 to run. Alternatively, anything that can run Windows executables and that can provide msvcrt.dll.

But the main obstacle is likely to be getting it past your AV software. This is a hazard when generating your own EXEs, in that it might advertently create false positives when AV checked. (Alternatives I've used have been to supply C-source versions, then the onus is on your C compiler. I no longer do that.)

Note that `mm.exe` has been UPX-compressed, from 450KB to 130KB, with slightly longer start-up time.

#### Build Instructions

Get mm.exe and those .ma files in the same place. Then:
```
mm aa.ma                      # Build asssembler aa.exe
mm cc.ma                      # Build C compiler cc.exe
mm qq.ma                      # Build Q intepreter qq.exe
mm mm.ma -out:mm2.exe         # Build another M compiler mm2.exe
```
The last is a little different, as Windows won't let you overwrite an active executable, so the output is a different file. But this one will be uncompressed. You can test the new compiler on those other .ma files (or use it on hello.m: `mm hello`)


For testing the assembler aa.exe try this:
```
mm -asm mm                    # Compile M compiler to mm.asm (note .ma extension optional if not ambiguous)
aa mm                         # Assemble mm.asm to mm.exe (note this will overwrite the original mm.exe)
mm aa                         # Perhaps check that new mm.exe can compile the asssembler
```
For testing qq.exe, try `qq hello`. (Note both `hello` programs include date and time display; this is to ensure the latest just-compiled version is the one being run.)

For testing cc.exe, I'd recommend renaming it, eg to bcc.exe, to avoid clashes with other programs called 'cc'). This C compiler has fallen into disuse, but should still work on a hello.c; download from anywhere if stuck.
