


**Assembler (x64, 2010s)** Actually from around 2000, I did use a third party assembler, NASM, then 16/32 bits.

It wasn't fast, I thought it odd that it took 5 times as long to assemble my generated code, as it took my compiler to create it! But it was workable. And of course it need a linker, and I used one from a C compiler. I'm not proud of this period!

But in the 2010s, I was compiling for x64, and wanted to move to whole-program compilers, where the output would have been a single ASM file. But at sizes of 20K lines and above, NASM got exponentially slow. A 100Kloc file might take take a minute or more to assemble. Something needed to be done! So I created my own x64-subset assembler that generated OBJ files (PE+ COFF64 format).

The hardest part was figuring out that format, but at least the problem was solved; my assembler could tackle a 100Kloc file in a fraction of a second. 

Currently, my assembler can turn 2-3M lines per second into EXE. I've since found there is YASM, a faster version of NASM, but mine is still 10 times faster, and YASM would still need a linker.

**Linker (x64, 2020s)** So a third party linker is still needed, and the choices are limited: use `gcc` which invokes `ld`, adding *fifty* options so who knows what extra C-specific crap is being added to my executables. Or I can run `ld` directly, but that had problems: mysterious errors when run on my laptop, I think also some issues in direct linking with DLL shared libraries.

It was also an untidy dependency; either a sprawling 0.5GB gcc installation, or `ld.exe` plus a set of `.dll` files. Plus, it was silly to need a *linker* to only ever link *one* OBJ file! (The DLLs were dynamically linked at load time.)

There were a couple of other options: MS's linker, but the problem there is that I can never locate it! Somewhere inside the labyrinthine folders of VS/MS build tools, with mysterious dependencies. And there was a tiny product called `Golink`, however that had some serious issues.

Something needed to be done, so I bit the bullet and got my assembler to directly generate EXE files. This took weeks: it would go wrong constantly, with zero indication from the OS of why my EXE had failed. But eventually it worked.

Note that my assembler is also used as the backend of my C compiler, and C uses independent compilation. So the assembler can take multiple ASM files as inputs, and generate a single EXE file as output: it does the job of linker in that case, but via ASM files not OBJ.

The later passes of my assembler were subsequently ported to my native code compilers, so they can now compile from source to binary in one program. This doubled compiler throughput, compared with needing to to through intermediate textual ASM.

