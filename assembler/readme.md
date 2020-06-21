## Assembler Sources

These are the source files for my 'AX' assembler/linker for x64 running Windows. They are written in my systems language.

The relevant files that write COFF and PE formats are:

* **ax_writeobj.m**   Write object file. Start at writess() which calls writecof()
* **ax_writeexe.m**   Write executable file. Start at writeexe()
* **ax_objdecls.m**   Contains some struct and enum definitions used by the above

These obj/exe modules take input from a set of 'ss' data objects created in the rest of the assembler, but this is not important just to get an idea of how obj/exe images are constructed (which is in a memory block, later written as a file).

(Sources should be viewed with 4-character tabs. Lines starting "!" are comments. Details on this language are elsewhere on this site, look for mfeatures.md, or just ask.)

Some more notes about how this works:

* **ax** uses its own ASM syntax, roughly similar to Intel format (see ax.asm for an example; this file is the result of compiling the assembler and telling it to produce the .asm intermediate)
* The format is intended for generated ASM code, although is also used for inline ASM code in my systems language), so has few frills.
* Names such as \`abc allow case-sensitive identifiers (normally case-insensitive), and allow opcode and register names to be used as identifiers
* Names such as abc\* indicate external names imported from DLL libraries
* Labels with "::" indicate global names that are exported. (I can't remember off-hand, but I think * and :: are used with multiple-module programs to import and export between them, as well as importing DLL names)
* **ax** will automatically look inside msvcrt.dll, user32.dll, gdi32.dll and kernel32.dll for imported symbols. Others can be specified on the command line (but it will reset the list so msvcrt etc have to be specified explicitly if needed.)
* **ax** can't link modules in .o, .obj, .lib or .a formats created by other software. It can only dynamically link to DLL files.
* **ax** uses either 'start' or 'main' as the entry point (this is needed when writing .exe files)
* These was another scheme for denoting imports inside the ASM source, so a name like "msvcrt.printf" will mean it is importing name "printf", and is expected to be found inside "msvcrt.dll". So this obviates the need list the DLL, and also it could more efficiently search in that one file, instead of across all DLLs. But this is not currently used.
* **ax** supports only a subset of the vast x64 instruction set. Only enough of the SIMD/SSE and x87 instructions are supported as are needed by my compilers
* My assembler supports regular register names RAX etc, but also uses a different naming scheme such as D0..D15, which is more consistent, and with an  ordering more suited to the Win64 ABI (so D0..D2 are volatile; D3..D9 are preserved across function calls, D10..D13 used for parameter passing, plus D14/Dframe and D15/Dstack)
* PE files include any number of sections/segments, but I only use 3: CODE (.text), IDATA (.data), and ZDATA (.bss). There should be a fourth for read-only data, but I haven't done that.


Other tools that I think are essential are ones to read and display .obj and .exe formats. I have used PEDUMP before, but no longer have a working version. I also have my own versions, but ones from other sources are useful for cross-checking. Mine includes a disassembler, as does **ax**, which itself is sometimes cross-checked with on-line disassemblers. (x64 instruction encoding is a nightmare, as is the PE file format.)

If you want to try out **ax**, there is a binary here: www.bcas.freeuk.com/ax.exe (right-click on link to download). A suitable test might be this (save to hello.asm):

````
!x64 output for hello.c
	align     16
!------------------------------------
`main::
	sub       Dstack,	8
! -------------------------------------------------
	sub       Dstack,	32
	mov       D10,	KK1
	call      `printf*
	add       Dstack,	32
L1:
! -------------------------------------------------
	sub       Dstack,	32
	mov       D10,	0
	call      exit*

!String Table
	segment idata
	align 8
KK1:db "Hello, World!",10,0
````

Assemble and run using this (but be warned your AV software may object, or send it off to be checked):

    ax hello
    hello

If that works, try 'ax ax' (download ax.asm from above), although to avoid overwriting the current ax.exe while it runs, you'd have to write:

    ax ax -out:ax2.exe
    
