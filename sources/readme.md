## Amalgamated Source Code

The individual source files of my projects are too numerous to upload individually. (And I don't use special github apps.)

The files here are amalgamations created by my compiler. They can be compiled directly in that form, or with a simple tool can be extracted again (the directory at the start gives file size and offset within the file).

Notes:

* The .ma extension apparently means something to github, so the text is highlighted in a funny way. All sources assume 4-character tabs)
* The uploads done on 4-Jan-2021 have had most line comments elided (to tidy up the source), so cannot be compiled nor extract (because the offsets in headers will be wrong)


#### Guide to Projects

**ax** x64 Assembler (designed for generated asm source code, and supported only a subset of x64)

**bb** M compiler (used to compile all .m source files, and .ma amalgamated/application files)

**qq** Q compiler/interpreter (this product is being replaced by smaller, tighter version, but that is still being developed)

**cc** C subset compiler

