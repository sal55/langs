(Source backups via amalgamated source file bundles.)

The .ma files each represent one project, and are an amalgamation of its source files:
````
mmp.ma       is my 'M' systems language compiler
ccp.ma       is my C subset compiler (both of these use the same 'pc' backend)
pcp.ma       is a standalone IL processor (textual IL to binary)
aap.ma       is my x64 assembler
qqp.ma       is a separate interpreter project
````
At the end of each file is a list of the modules. The ones for 'pcp' are shared also by 'mmp' and 'ccp' (well, the .ma files contain their own copies), and most of them also by 'aap'.

This is a guide to the module prefixes used:
````
mm      M compiler front end
cc      C compiler front end (you might see a pattern here!)
aa      Assembler front end
pp      PC (IL/IR) processor front end (this standalone product is little used)
pc      IL/IR API modules (normally incorporated into each front-end, but can
        also be built into a standalone DLL library)
mc      Native code API and code generation from IL (here for Win-x64)
mx      Support for in-memory execution 
````
View files using 4-space tabs. "!" starts line-comments. Source is case-insensitive so some bits may be in upper-case.

To build any of these requires the 'mm.exe' compiler that runs on Windows. I haven't provided that; distributing binaries is troublesome.

I happened to prepare this diagram the other day which shows the organisation:

https://github.com/sal55/langs/blob/master/pclchart.md

The middle part are the pp-files; the Win64 backend are the mc/mx files; the rest relate to the four front-ends shown. When all four are built, it produces the four self-contained binaries 'mm.exe bcc.exe (after renaming) pc.exe and aa.exe'.
