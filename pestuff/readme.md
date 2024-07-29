### Writing PE EXE, DLL and OBJ Files

These files are part of my x64 assembler:

**aa_decls.m** Selected declarations relevant to writing EXE or OBJ files.

**aa_objdecls.m** Records (structs) corresponding to data structures used in PE format

**aa_writeobj.m** Code to write an object file in PE/COFF format (call `writess()`)

**aa_writeobj.m** Code to write an EXE or DLL file in PE/EXE format (call `initsectiontable(); genexe(); writeexe()`)

The earlier part of the assembler will have generated machine code and data into these three data blocks:
````
ss_zdata        (.bss segment; static data block with no data that is zeroed on loading)
ss_idata        (.data segment; initialised static data)
ss_code         (.text segment; executable code)

ss_idatarelocs  (items in .data that are relocatable)
ss_coderelocs   (items in .code that are relocatable)

ss_symboltable  (named symbols some of which will be written: some imported, some exported)
````

Writing OBJ files is simpler. This is what I did first. I still needed an external linker, but decent ones are elusive and few. The one I used got troublesome, so eventually I wrote the `aa_writeexe` module to remove that dependency.

This allowed me to dispense with a linker completely. This requires that either the ASM file represents the whole program, which it usually does, or the 'linking' capability was done by the assembler (this is only needed for the output of my C compiler). That is, multiple ASM files were combined to form one program.

Also, it is not possible to statically link with external object files or libraries. External libraries are used via DLLs (shared dynamic libraries). If such linking *is* needed, then I can still generate OBJ files as needed.

These backend modules were also incorporated, slightly modified, directly into my HLL compilers.

#### Notes on the source code

* It is written in my own systems language
* `!` starts a line comment
* `int` has type `i64`; `word` has type `u64`; `byte`/`char` have type `u8`.
* It is case-insensitive

#### High-loading code

Initially, all code was expected to run below 2GB in Windows' virtual address space. This allowed 32-bit absolute addresses and offsets to be used.

But, external linkers now like to generate EXEs that can be loaded at arbitrarily high addresses. This means 32-bit absolute addresses can't be used. RIP-relative is commonly used. The same applies to generating DLLs since they may be loaded high.

This required changes in two places:

* In the assembler, so that absolute addresses (that aren't displacements to registers) use the special RIP-relative address mode
* In my compilers, so that address modes like `[rax+d32]` are not used, when `d32` is a static address; any such `d32` address is loaded into a separate register.

So you might see some bolted-on code (controlled by a `highmem` option, which must be set for OBJ/DLL output) which deals with those RIP conversions.
