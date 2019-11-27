## C3 Notes

I've made here some comparisons between working or tentative featueres of [C3](http://www.c3-lang.org/), and similar ones in my own systems language 'M'. (That one is now more of a private language.)

### C to C3 Conversion

I made an attempt at a C to M conversion, but it was very poor. It the end it was simply used as a way to view C source, in my own more Algol-like M syntax, but the resulting code can't run as the semantics are too different. As one example, C's chaotic 'switch' statement cannot always be reliably translated.

I do make use of the translator for C library headers, to get a starting point for a manually tweaked version to end up with an interface file in my language. One killer here is dealing with C's crude textual macros (my language doesn't have text-based macros).

### Transpile-compile via gcc, tcc, clang

My M compiler only directly targets x64 native code for Windows. For anything else, I have reintroduced a C source target, which can be compiled with gcc or tcc (clang doesn't work on my machine), and which extends the range to 32-bit machines, Linux, and ARM processors.

But they have always had difficulties, and the C target doesn't support many features of mine, so it effectively works for a subset only. (One big difficulty was using printf format codes, either used implicity, or explicitly (when M calls printf as a foreign function. What format to use for my int64 types? To work across platforms, it needs to be a macro like PRId64, but that only exists if coding directly in C. Use of *printf functions had to be all but eliminated.)

Another problem was that, while the M compiler is very fast (near instant for programs a few tens of thousands of lines) it would hit a brick wall as soon as gcc was invoked, so the favoured compiler is tcc.

(This is significant because M is a whole-program compiler; all modules are compiled, and the output file, whether .exe or .c, is a single file. So for a C target, it will produce a large monolithic C file.)

### Built-in Macros

#### @bitcast

This is type-punning. While M normally casts as T(X) to convert X to T, type-punning usings T@(X) to reinterpret bits without conversion, which seems to be what @bitcast does.

It also works for expressions, while the equivalent in C only works for lvalues (eg. *(T*)&(X)), another thing that won't easily translate to C.

(I recently made changes which mean that T(X) syntax is ambiguous - T can introduce a declaration, or a cast. So except for the simplest cases, casts are now written as cast(X,T) and cast@(X,T) for type-punning.

Note that this also works as cast(X) where it will automatically cast to whatever type is called for, which is incredibly handy.)
