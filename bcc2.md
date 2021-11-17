## BCC 'C' Compiler

I've given up on this as an actual C compiler. However the project is retained as it still have some practical and potential uses:

* I don't have many significant applications for main M language, so this is counts as a major test program. If I change M's compiler, use it to rebuild bcc, and bcc can still build and run the 250,000-line sql.c test, then that's one indicator that something is still working right!
* It is still my first choice when writing small C programs of my own
* It's also my first choice when trying out smallish C programs of other people's, provided that don't depend on make systems or use features I don't support
* It's my first choice when compiling *generated* C programs, which tend to be large
* I can use its preprocessor, as that is fairly decent
* It has an option -mheaders which translates C headers to declarations in my M syntax. That's not 100% (any macros need to be translated manually).
* I want to add -qheaders to do the same thing for my Q syntax (dynamic) language, which is a little different
* An experimental fork of it served as a visualisation tool for C, translating to M syntax. I want to bring that into the main compiler, and improve it to generate more semantically correct M. But that would still only be a starting point for the difficult process of properly porting code to my language
* I have used to translate fiddly type declarations into their C equivalents ...
* ... and I've used it to translate C type declarations into a form I can understand
* I may have a go at changing the backend to use my 'PCL' intermediate language ...
* ... however that is designed for either whole-program compilers, or single modules. So it would only work for programs in one file.
* (Multiple-module programs using PCL may just be possible, by converting each PCL file into ASM, each ASM file into OBJ, and using an external linker to build. That's no way to run a nippy, self-contained C compiler, but it may be enoough for that path to be possible.)
* Sometimes, it can serve as second opinion after using a mainstream C compiler, as bcc's messages tend to be blunt (there are no warnings, just hard errors)
* Seeing how C maps to native code, as it can produce output cleaner than gcc's.
* Experimenting with techniques for fast C compilation

