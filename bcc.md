## BCC 'C' Compiler

This is one I created in 2017. It has long been withdrawn because:

* It omits some major features such as VLAs, compound literals, dynamically initialised arrays and structs, designated initialisers and bitfields. (These weren't that common back in 2017; they are now!)
* There are myriad ways in which the compiler does not conform, too many to list (eg. `long` is a synonym for `int`; `char` is a synonym for `signed char`, both incorrect; my rules for {...} initialisers are stricter than C allows; ...)
* There is a major problem with callback functions (functions called from external code need to be marked as $callback or #pragma callback)
* It is too buggy in terms of the generated code
* The supplied headers are incomplete
* The preprocessor is not up to standard (it will do for most things, but not the esoteric stuff that some like to use the preprocessor for)

I also realised creating a C compiler that will tackle any input, even aside from the omissions, is years of work.

It's kept alive for the following reasons:

* It is still my first choice when writing small C programs of my own
* It's also my first choice when trying out smallish C programs of other people's, provided that don't depend on make systems or use features I don't support
* It's my first choice when compiling *generated* C programs, which tend to be large (although a significant bug has crept in that I need to get around to)
* I don't have many significant applications for own 'M' language, so this counts as a major test program. If I change M's compiler, use it to rebuild bcc, and bcc can still build and run the 250,000-line sql.c test, then that's one indicator that something is still working right!
* I can use its preprocessor, as that is fairly decent (apart from the proviso above)
* It has an option -mheaders which translates C headers to declarations in my M syntax. That's not 100% (any macros need to be translated manually).
* I want to add -qheaders to do the same thing for my Q syntax (dynamic) language, which is a little different
* An experimental fork of it served as a visualisation tool for C, translating to M syntax. I want to bring that into the main compiler, and improve it to generate more semantically correct M. But that would still only be a starting point for the difficult process of properly porting code to my language
* I've used it to translate C type declarations into a form I can understand
* Sometimes, it can serve as a second opinion after using a mainstream C compiler, as bcc's messages tend to be blunt (there are no warnings, just hard errors)
* Seeing how C maps to native code, as it can produce output cleaner than gcc's.
* Experimenting with techniques for fast C compilation
* Sometimes, trying experimental C features

### Download

BCC runs on and targets 64-bit Windows. A binary can be found here:

https://github.com/sal55/langs/blob/master/bcc.exe

This the complete self-contained compiler, about 1MB but UPX-compressed here to some 0.3MB. It incomporates standard headers plus windows.h.

I can't support it nor promise it will work.

