## Amalgamated Source Code

The individual source files of my projects are too numerous to upload individually.

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
