(Adjunct to my Reddit post.)

**Case sensitivity**

Feature | M Style | Popular | Notes
--------|--------|----------- | ----------------
Case sensitive | Case Insensitive | Case Sensitive
Example | abc, Abc, ABC are same name | abc, Abc, ABC are distinct | etc
Array Lower Bounds | N-based, default 1 | 0-based
Example | Use any of 1, 0 or N | Use 0 only
Text (code) editors | Use hard-line stops | Soft-line stops | Means Left/Right etc do not stop at line beg or end
Compiler Strictness | Unequivocal | Depends on options | Typical C compilers are very lax unless you twist their arm
Compiler options | <10 | Loads | (Clang has 800; gcc has 1000s)
Need file extension | Optional | Yes
Example: | bcc hello | gcc hello.c
Default output file | Based on first input file | Varies | gcc generates a.out or a.exe
Example | bcc hello => hello.exe



1 | 2 |3
4 | 5 | 6


Markdown | Less | Pretty
--- | --- | ---
*Still* | `renders` | **nicely**
1 | 2 | 3


Never using Unix, and being used to languages like Algol, Fortran and Pascal, I've never got into the case-sensitive vibe.

I always find it an almighty pain, and, since in real life it would be ludicrous that the 16 case variations of "bart" are all distinct names, it is equally so in source code. All my languages are case-insensitive.

**C Language**

I devised my own systems language long before I tried C, and I was unimpressed. I've listed 100 different annoying aspects of C, all of which I have fixed at some point.

**C Compilers**

My own compilers are far less equivocal about what is a correct or incorrect program. They have only a handful of options (compare with 800 in clang and 1000s in gcc).

When I generate C code, I compile it with 'gcc prog.c' and it passes with flying colours, and works fine. But pile on options and it can generate 1000s of warnings or errors. So was my program correct or not? Apparently I get to choose; nice language!

**LIB Files**

Never used them; can't see the point. To package object files, just create a new bigger object file. To interface to shared libraries, just supply the shared library.

**Complicated Linkers**

I've written linkers that were simple enough, that I called them Loaders; they ran as fast as they could load object files from disk. At one time I had to use third party linkers (because I generated Nasm code), but they started being troublesome. I've pretty much eliminated the need for them now.



**Garbage Collection**

Always wondered what that was about, even though I'd long been maintaining my dynamic language; it was designed to not need it. However recent versions switched over to reference-counting. It made some new approaches possible, but overall was no faster.

**OOP**

Same here. What exactly could OOP, as in C++, actually do? The first examples I looked at (first-class string handling) turned out to what I'd been doing for years in my dynamic language.

**Make Systems**

And again. I admit some build systems can be elaborate, but for many open source projects, they bring unnecessary complications. And they usually don't work, not on Windows. I build all my own projects (involving up to 50 files), in one invocation of a compiler; nothing else is needed.

**Macros**

This is a funny one: C absolutely relies on its macros, but my systems language, used for the same sorts of tasks, has never needed them. (There used to be a simple text macro replacements with no parameters, but long gone now; too crude.)

(I'm now experimenting with AST-based macros, but still they are rarely used.)

**Macro Assemblers**

Why do people who write assemblers make them so complicated with so many features? Why are macros such a big deal? The ones I've written (as standalone assemblers) are simple and have never had macros; they did the job.

**Debuggers**

<Shrug> Not something I've ever had an opportunity to use. They are much more difficult from one's own language anyway, unless you write it yourself.

In any case, for the language-related stuff I do, which involves multiple levels of source code (eg. application source, source of the compiler, source of the compiler of the compiler), which source would it debug? Would it debug native code or byte-code?

(I think my record was 6 levels of source code: New 'M' compiler source (1), building current M compiler source (2), building C compiler sources (3), building Seed7 (4), interpreting a Basic interpreter in Seed 7 (5), running a Basic program (6). If the Basic program didn't work, in which lot of source would I look? Debuggers are useless here.)

**Linux/Unix-like**

On the suggestion that only such systems are good for development. Well, I earned a living writing software under MSDOS and Windows, so clearly it can be done, and with little trouble.

**Tools needing file extensions**

All I can say is that, if a tool or application of mine has a clear file-type that it normally works on, then that will be the default extension if one is omitted. That has worked very well and is more user-friendly. How much time has been wasted typing extensions, or pressing tab keys to navigate to the right file? (Alhough this is a popular pastime according to the upvotes this suggestion got on Reddit!)

**Text Editors**

I use my own crude editors. Nevertheless, they have hard line stops: Left/Right or Delete Left/Right don't proceed past the beginning/end of the current line.

** Readln A,B,C**

This was trivial to do in the first languages I learned, and I copied it in my own languages, using statements not functions.

Yet it is remarkably difficult in many languages. Try it in C (an old language but coming from Unix with its alien mindset); the specification is to read A,B,C (according to their types) on one line. If missing on the same line, read 0 or "", do not proceed to the next line (which in interactive mode, means that the program appears to hang).

Try it in Python. Its lack of reference params means you can write a function like read(a,b,c).

**LLVM**

This probably comes the closest to 'The Emperor's New Clothes' for me. What exactly is it? Every new thing I read about it gives a different answer: a compiler front-end; a back-end; the middle part of a compiler; a whole compiler Why is is so massive? I already said tiny solutions are possible, but none exist. Anyway let's not waste more time.


(My background:
CS degree

Built own 8-bit machine around microprocessor, starting from binary machine code through to primitive HLL that was used for 3D graphics and image processing.

Become an electronics engineer designing microprocessor-based 8 & 16-bit computers and video boards; wrote low-end CAD programs for 8-bit machines then PCs.

Devised a systems language 'M', evolved from first Z80 version; and a script language (various) evolved from x86 version.

Written compilers targeting PDP10 (at college); Z80; 8086; x86; x64 (16/32/64 bit generations). My 'M' compiler has been self-hosting (AFAICR) since the Z80 version.

Written interpreters for all x86 generations (also ported to ARM, but since that involved targeting C, that doesn't count).

Written standalone assemblers for Z80; 8035/51 (forget which); 80186; 8086; x64 (x86 one was inline to HLL).

Written linkers and text editors.)

