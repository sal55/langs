## New 'B' Language

This is an experimental project to design a new language which combines my static M language and scripting Q language.

It has the working title 'B'. (If it manages to replace 'M', then it will be renamed M).

**Features**

* Includes Dynamic types of Q, and Static types of M, which can mixed in any code in any function
* Compiles first to internal PCL code, which can include static and dynamic elements
* PCL code can then be 100% interpreted
* Or PCL can be 100% compiled to native code
* I wanted to avoid mixing interpreted and native code, but is necessary in some cases (eg. functions with inline ASM, or callback functions called from external libraries, need to be native code)
* Any program can be run-from-source just like Q scripts are now; either by interpreting, or generating in-memory native code
* There should be no noticeable delay in compiling when run from source (compilation speed will be 0.5 to 1.0 million lines per second)
* Features of Q for informal, rapid developement will be retained, such as optional local declarations
* I'm planning the ability to write external binary files too (as EXE, MX/ML), but some reflection features (eg. the compiler symbol table) may not be available.
* Self-hosting might therefore be a possibility, which means it could replace both the other languages
* Performance of native code depends on how much of the code depends on dynamic features. But it is expected that any bottlenecks will have some static typing applied


