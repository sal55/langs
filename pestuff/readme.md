### Writing PE EXE, DLL and OBJ Files

These files are part of my x64 assembler:

**aa_decls.m** Selected declarations relevant to writing EXE or OBJ files.

**aa_objdecls.m** Records (structs) corresponding to data structures used in PE format

**aa_writeobj.m** Code to write an object file in PE/COFF format (call `writess()`)

**aa_writeobj.m** Code to write an EXE or DLL file in PE/EXE format (call `initsectiontable(); genexe(); writeexe()`)

The earlier part of the assembler will have generated machine code and data into this three data blocks:
````
ss_zdata        (.bss segment; static data block with no data that is zeroed on loading)
ss_idata        (.data segment; initialised static data)
ss_code         (.text segment; executable code)
ss_idatarelocs  (items in .data that are relocatable)
ss_coderelocs   (items in .code that are relocatable)
ss_symboltable  (named symbols some of which will be written: some imported, some exported)
````

Writing OBJ files is simpler. This is what I did first. But I still needed an external linker; decent ones are elusive. The one I uses got troublesome, so eventually I write the `aa_writeexe` module.

This allowed me to dispense with a linker completely. (This requires that either the ASM file represents the whole program, which it usually does, or the 'linking' capability was done by done the assembler. That is, multiple ASM files were combined to form one program.)

Also, it is not possible to statically link with external object files or libraries. External libraries are used via DLLs (shared dynamic libraries).

These backend modules were also incorporated, slightly modified, directly into my HLL compilers.

#### Notes on the source code

* It is written in my own systems language
* `!` starts a line comment
* `int` has type `i64`; `word` has type `u64`; `byte`/`char` have type `u8`.
* It is case-insensitive

