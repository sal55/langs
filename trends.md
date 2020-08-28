(Adjunct to my Reddit post.)

**Case sensitivity**

Never using Unix, and being used to languages like Algol, Fortran and Pascal, I've got into the case-sensitive vibe.

I always find it an almighty pain, and, since in real life it would be ludicrous that the 16 case variations of "bart" are all distinct names, it is equally so in source code. All my languages are case-insensitive.

**C Language**

I devised my own systems language long before I tried C, and I was unimpressed. I've listed 100 different annoying aspects of C, all of which I have fixed.

**C Compilers**

My own compilers are far less equivocal about what is a correct or incorrect program. They have as few options as possible, just a handful (compare with 800 in clang and 1000s in gcc).

When I generate C code, I compile it with 'gcc prog.c' and it passes with flying colours, and works fine; while on options and generate 1000s of warnings, or errors with extra options. So was my program correct or not? Apparently I get to choose.

**LIB Files**

Never used them; can't see the point. To package object files, just create a new bigger object file. To interface to shared libraries, just supply the shared library.

**Complicated Linkers**

I've written linkers that were simple enough, that I called them Loaders; they ran as fast as they could load object files from disk. At one time I had to use third party linkers (because I generated Nasm code), but started being troublesome. I've pretty much eliminated the need for them now.

**Garbage Collection**

Always wondered what that was about, even though I'd long been maintaining my dynamic language; it was designed to not need it. However recent versions switched over to reference-counting. It made some new approaches possible, but overall was no faster.

**OOP**

Same here. What exactly could OOP, as in C++, actually do? Turned out to what I'd been doing for years in my dynamic language.

**Make Systems**

And again. I admit some build systems can be elaborate, but for many open source projects, they bring unnecessary complications. And they usually don't work, not on Windows. I build all my own projects (involving up to 50 files), in one invocation of a compiler, nothing else.

**Macros**

This is a funny one: C absolutely relies on its macros, but my systems language used for the same tasks, has never needed them. (There used to be a simple text macro replacements with no parameters, but long gone now; too crude.)

(I'm now experimenting with AST-based macros, but still they are rarely used.)

**Macro Assemblers**

Why do people who write assemblers make them so complicated? Why are macros such a big deal? The ones I've written (as standalone assemblers) are simple and have never had macros. My most recent one is designed for generated ASM; it doesn't need any bells and whistles. It replaced Nasm which became impossibly slow on large inputs (like 1-2 minutes).

**Debuggers**

<Shrug> Not something I've ever had an opportunity to use. They are much more difficult from one's own language anyway, unless you write it yourself.

In any case, for the language-related stuff I do, which involves multiple lots of source code (eg, compiler source, interpreter source, target program), which source would it debug? Would it debug native code or byte-code?

(I think my record was 6 levels of source code: New 'M' compiler source (1), building current M compiler source (2), building C compiler sources (3), building Seed7 (4), interpreting a Basic interpreter in Seed 7 (5), running a Basic program (6). If the program didn't work, in which lot of source would I look? Debuggers are useless here.)

**Linux/Unix-like**

On the suggestion that only such systems are good for development. Well, I earned a living writing software under MSDOS and Windows, so clearly it can be done, and with little trouble.

**Tools needing file extensions**

All I can say is that, if a tool or application of mine has a clear file-type that it normally works on, then that will be the default extension if one is omitted. That works very well and is more user-friendly. How much time has been wasted typing extensions, or pressing tab keys to navigate to right file? (Alhough this is a popular pastime according to the upvotes this suggestion got on Reddit.)

**Text Editors**

I use my own crude editors. Nevertheless, they have hard line stops: Left/Right or Delete Left/Right don't proceed past the beginning/end of the current line.

** Readln A,B,C**

This was trivial to do in the first languages I learned, and I copied it in my own languages, and using statements not functions.

Yet it is remarkably difficult in many languages. Try it in C (an old language but coming from Unix with its alien mindset); the specification is to read A,B,C (according to their types) on one line. If missing on the same line, read 0 or "", not proceed to the next.

Try it in Python. It's lack of reference params means you can write a function like read(a,b,c).

**LLVM**

Lots of time I've wasted on this. It probably comes the closest to 'The Emperor's New Clothes'. What exactly is it? Every new thing I read about it gives a different answer: a compiler front-end; a back-end; the middle part of as compiler. Why is is so massive? I already said tiny solutions are possible, but none exist. Anyway let's not waste even more time.


