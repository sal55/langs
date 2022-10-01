## BCC 'C' Compiler

This is one I created in 2017 to target Windows 64. It has long been withdrawn because:

* It omits some major features such as VLAs, compound literals, dynamically initialised arrays and structs, designated initialisers and bitfields. (These weren't that common back in 2017; they are now!)
* There are myriad ways in which the compiler does not conform, too many to list (eg. `long` is a synonym for `int`; `char` is a synonym for `signed char`, both incorrect; my rules for `{...}` initialisers are stricter than C allows; mixed-sign arithmetic uses different rules; ...)
* There is a major problem with callback functions (functions called from external code need to be marked as `$callback` or `#pragma callback` because the code is not fully ABI compliant)
* It is too buggy in terms of the generated code
* The supplied headers are incomplete
* The preprocessor is not up to standard (it will do for most things, but not the esoteric stuff that some like to use the preprocessor for)

I also realised creating a C compiler that will tackle any input, even aside from the omissions, is years of work.

It's kept alive as a private tool for the following reasons:

* It is still my first choice when writing small C programs of my own
* It's also my first choice when trying out smallish C programs of other people's, provided that don't depend on make systems or use features I don't support
* It's my first choice when compiling *generated* C programs, which tend to be large (although a significant bug has crept in that I need to get around to)
* I don't have many significant applications for own 'M' language, so this counts as a major test program. If I change M's compiler, use it to rebuild bcc, and bcc can still build and run the 250,000-line sql.c test, then that's one indicator that something is still working right!
* I can use its preprocessor, as that is fairly decent (apart from the proviso above)
* It has an option `-mheaders` which translates C headers to declarations in my M syntax. That's not 100% (any macros need to be translated manually). This will be extended to my Q language too (unless their syntaxes converge first)
* An experimental fork of it served as a visualisation tool for C, translating to M syntax.
* I've used it to translate C type declarations into a form I can understand
* Sometimes, it can serve as a second opinion after using a mainstream C compiler, as bcc's messages tend to be blunt (there are no warnings, just hard errors)
* Seeing how C maps to native code, as it can produce output cleaner than gcc's.
* Experimenting with techniques for fast C compilation
* Sometimes, trying experimental C features

The original aims were:

To see if a C implementation could be created that:

* Can be presented as the simplest possible kind of open source project (a single file)
* Can be built effortlessly from source, as simply as compiling a Hello, World program
* Can be run as a single executable with no dependencies, other than what is provided by the OS (standard headers are incorporated)
* Can be used without any libraries of its own (uses msvcrt.dll)
* Can be extremely fast (despite largely using simple, linear data structures)
* Can generate reasonable quality code even with no optimiser (and that is better than Tiny C's!)

These were largely achieved. But since starting it, Tiny C 0.9.27 came out, with many bug fixes, and a near-complete support for C99. tcc is also about half the size of bcc (counting code, not headers) and perhaps twice as fast. However bcc produces marginally faster code. So Tiny C occupies the space that bcc was vaguely aiming for.
