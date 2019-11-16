### MCC Project Aims


#### Pretty much already achieved:

To see if a C implementation can be created that:

* Can be presented as the simplest possible kind of open source project (a single file)

* Can be built effortlessly from source, as simply as compiling a Hello, World program

* Can be run as a single executable with no dependencies, other than what is provided by the OS (standard headers are incorporated)

* Can be used without any libraries of its own (uses msvcrt.dll or msvcr*.dll versions)

* Can be extremely fast (despite largely using simple, linear data structures)

* [Can generate reasonable quality code even with no optimiser (and that is better than Tiny C's!)] This has been dropped for the time being. Getting correctly running programs was more important. \[Added Nov 2019: at this time I'd switched to a dumb code generator with very poor output. It's since been switched to a better one. And generably outperforms tcc, but not by much\]

#### To Do:

* Make it self-contained, to eliminate need for external assembler and linker

* Be able to compile all modules of a project from source, link and run in-memory

* Improve the code quality

* Fill out the standard headers

* Be able to compile more arbitrary C programs

* Fill in missing features

* Fix the bugs (some of them anyway, as there will always be some!)

* Possibly, add an interesting new target such as ARM

#### What won't be done:

* Produce a regular, complete, general purpose compiler that will compile anything thrown at it. That would take years of effort.

The work on it has shown that C, while deceptively small and simple, is actually a large, sprawling, incredibly messy language, when you look at actual implementations. Far from being portable, many headers and even programs are patched together with special macros, #ifs and #ifdefs, and even then a program may not work with just any compiler.

I don't want to produce yet another hacked-together implementation of C. It's an experiment in producing a small, fast, informal and tidy compiler. It needs a small and tidy language to go with it!
