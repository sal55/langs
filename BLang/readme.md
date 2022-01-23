## New 'B' Language

This is an experimental project to design a new language which combines my static M language and scripting Q language.

It has the working title 'B'. (If it manages to replace 'M', then it will be renamed M).

**Features**

* Includes Dynamic types of Q, and Static types of M, which can mixed in any code in any function
* Compiles first to internal PCL code, which can include static and dynamic elements
* PCL code can then be 100% interpreted
* Or PCL can be 100% compiled to native code (this does not guarantee native code speed: see below)
* I wanted to avoid mixing interpreted and native code, but is necessary in some cases (eg. functions with inline ASM, or callback functions called from external libraries, need to be native code)
* Any program can be run-from-source just like Q scripts are now; either by interpreting, or generating in-memory native code
* There should be no noticeable delay in compiling when run from source (compilation speed will be 0.5 to 1.0 million lines per second)
* Features of Q for informal, rapid developement will be retained, such as optional local declarations
* I'm planning the ability to write external binary files too (as EXE, MX/ML), but some reflection features (eg. the compiler symbol table) may not be available.
* Self-hosting might therefore be a possibility, which means it could replace both the other languages
* Performance of native code depends on how much of the code relies on dynamic features. But it is expected that any bottlenecks will have some static typing applied

The main problem with the two-language solution, was that it was two languages. You need to constantly think about where any function should go, about their interface, and bear in mind the limitations of each language. A resulting application could also be messy as it will involve Q compiler, Q interpreter, Q sources, M application, and likely also a RUN program if the solution involves my new MX/ML binary formats.

The new language should generate a single EXE, without needing to use utilities to combine diverse components into a pretend single EXE.

Attempts at combining these two languages were made at least twice before, but they didn't really work. The first try, with separate compilation, lacked the spontaneity and rapid development style of proper scripting. The last, was rather unwieldy.

But both were based on the static language, which always compiled to native code in an EXE file, with dynamic elements.

This one will start being primarily run-from-source and using an interpreter, but with lots of static elements. So it can be used for informal, rapid development, but will have the capability to be fast enough to match C.
