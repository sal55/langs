````
Language   Tool     Category      Written  Lead module  Project   Description
                                  in                    Folder
M          mm.exe   Compiler      M        mm.m         mx        Systems Language
Q          qq.exe   Interpreter   M        qq.m         qx        Scripting Language
ASM        aa.exe   Assembler     M        aa.m         ax        x64 Assembler
C subset   cc.exe   Compiler      M        cc.m         cx        C-subset compiler (production version is bcc.exe)
Z          zz.exe   Emulator      M        zz.m         zx        Z80 Emulator (.z files are binary; 'ZA' is Z80 assembly)
ZA         za.q     Assembler     Q        za.q         zx        Z80 Assembler
````

Examples of language, tool and project naming. Some production versions may have a version suffix.

Each project is a collection of modules, of which the lead module lists the modules describing the project.

The lead modules will have the same name as the executable (since compiling lead module xx.m will generate xx.exe). Some of these are shown here.

Modules aa.m and cc.m both import the same backend called 'pcl' (as pclax.m and pcl.m respectively), an older version from a previous version of 'mm'.
