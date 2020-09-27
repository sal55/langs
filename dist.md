## Distributing Language Projects

My language programs are as follows:

* **ax** x64 assembler/linker
* **cc** C-subset compiler (but it will build these four files)
* **mm** My main language compiler (module-based, whole program compiler)
* **qq** Bytecode compiler/interpreter

These are my current 4 language apps, when rendered as single C source files:
````
C:\myprogs>dir *.c
27/09/2020  11:42           417,731 ax.c
27/09/2020  11:43         1,273,845 cc.c
27/09/2020  11:49         1,540,734 mm.c
27/09/2020  11:46         1,626,401 qq.c
````

About 137,000 lines in total, representing 108 modules and 65 support files in the original sources.

This is a batch file to build all of those C programs:
````
C:\myprogs>type build.bat
tcc ax.c
tcc cc.c
tcc mm.c
tcc qq.c
````

And this is how long it takes to build those 4 programs (tm is a timing tool):
````
C:\myprogs>tm build
C:\myprogs>tcc ax.c
C:\myprogs>tcc cc.c
C:\myprogs>tcc mm.c
C:\myprogs>tcc qq.c

TM: 0.36
````
Just over 1/3 of a second to build four major programs. These are the results:
````
C:\myprogs>dir *.exe
27/09/2020  12:06           194,560 ax.exe
27/09/2020  12:06           653,312 cc.exe
27/09/2020  12:06           760,320 mm.exe
27/09/2020  12:06           856,576 qq.exe
````

Each executable is self-contained and complete.

Makefiles are totally irrelevant here.

(BTW, C sources can be generated for Windows OS, Linux OS, or Neutral OS. Those files above are neutral, they will compile on either OS, but capabilities will be limited. But they will be anyway because these target Windows. Only QQ can run fully on both.)

So those are for ease of distribution and building. What about everyday building? That's not much different:
````
C:\cx>tm mm cc
Compiling cc.m to cc.exe

TM: 0.15
````
This builds the C compiler by submitting the lead module (cc.m) to the M compiler (mm.exe). This is not quite as fast as tcc, but it still doesn't take long!
