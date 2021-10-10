## Summary of Language Projects

I will shortly be wrapping up further development of my personal languages, so this is a summary of what they are up to.

These are not modern languages with all the lastest ideas in advanced features. Just simple, down-to-earth tools to do a job of work.

Development is done on Windows, and implementations are mainly for Windows

### The Languages

Language  | Program | Project name | Written In | Description
--- | --- | --- | --- | ---
M | mm.exe | MX | M | My systems programmming language
Q  | qq.exe | QX  | M | My dynamic, interpreted scripting language
PCL | pc.exe | MX | M | Portable intermediate language, used as target by mm.exe
ASM | aa.exe | AX | M | Assembly language for x64 using my syntax
(C  | bcc.exe | CX | M | C subset)

The first two languages are designed for writing programs in. The next two, while they could be used for writing whole applications in, are intended as code-generation targets. (And mainly for testing, as usually textual intermediate forms are by-passed.)

I've included the line about C, which is clearly not my language, since it rounds off the set of language tools. bcc.exe has a number of uses, but an important one is for testing: by building bcc.exe with a new version of mm.exe, I can test it on a wide range of C source files.

### Availabilty

I class these as private languages, although anyone can try out the .exe files whenever I upload them, because:
* I can't do the support that would be needed for general use
* There are no proper docs
* They haven't been tested enough with lots of people applying them to diverse applications, to iron out bugs, highlight shortcomings, and fill in missing features
* The languages have also been volatile as I'm always tweaking

For my limited codecase, I can deal with bugs, or unimplemented combinations of types and operators, as they are encountered. And for a breaking update, there's not much of codebase to modify.

### The Installations

If I copy all the files of all these language installations into one place and list them, I get this:
```
C:\mylangs>dir
16/09/2021  12:05           149,504 aa.exe
29/08/2021  15:03         1,046,528 bcc.exe
10/10/2021  14:37           553,984 mm.exe
10/10/2021  15:30           220,672 pc.exe
07/10/2021  17:58           654,336 qq.exe
```
5 products - 5 self-contained executables. Each is just one file. The largest ones are include source files which bloats their size:

* bcc.exe includes the C standard headers, plus windows.h (the latter accounts for half the size)
* mm.exe includes sources for its standard library, as it is usually compiled in
* qq.exe includes sources for *its* standard library (including a graphics library for Win32)

This makes it easy to copy an implementation, transfer it via USB drive, email it, run it (from anywhere, it doesn't really need installing etc), or even just check that you have everything you need, or maintain different versions since there are no dependencies.

(Note: mm.exe includes most of pc.exe; the latter is used for processing standalone .pcl files, or, in pc.dll form, not shown, for use from other languages.)

### Building My Compilers

I'll give a demo of how it looks. First, I will use a special feature of mm.exe to create a single-file amalgamation of a project's source files, which turn multiple .m and support files into one .ma files. If I put those .ma files into one place, they look like this:
```
C:\demo>dir *.ma 
10/10/2021  15:44           247,276 aa.ma
10/10/2021  15:43         1,323,990 cc.ma
10/10/2021  15:44           872,039 mm.ma
10/10/2021  15:44           353,251 pc.ma
10/10/2021  15:46           964,856 qq.ma
               5 File(s)      3,761,412 bytes
```
(These sources also included the embedded source files that form part of the executables.)

Now I'll build with a simple script:
```
C:\demo>type build.bat
mm cc.ma
mm aa.ma
mm pc.ma
mm qq.ma
mm mm.ma
```
Now I run it (tm is a timing tool):
```
C:\demo>tm build
C:\demo>mm cc.ma
Compiling cc.m---------- to cc.exe
C:\demo>mm aa.ma
Compiling aa.m---------- to aa.exe
C:\demo>mm pc.ma
Compiling pc.m---------- to pc.exe
C:\demo>mm qq.ma
Compiling qq.m---------- to qq.exe
C:\demo>mm mm.ma
Compiling mm.m---------- to mm.exe
TM: 0.62
```
So building **all** my compilers, assemblers etc, from source code, takes 0.6 seconds, on my very ordinary PC. (Timings take advantage of any file-cacheing.)



### So, What's Special about these Languages?

If you've followed the last two sections, you will get an idea of what I value in a language implementation, and which I consider as important as what's inside a language: being small, simple, self-contained, effortless to use, and very fast.

I don't like large, sprawling implementations that take forever to build a program.

However, below I will highlight some of the features I do have inside some of these. But first...

### Features I don't Support

This is not to imply a criticism of such features, I just don't have them.

(This is avoid downvotes on Reddit as many of these are very popular there. Another reason why probably no one would use my languages anyway.)


### A Selection of Characteristics

A few characteristics and some features from M and Q that might be unique, uncommon or unpopular:

* Case-insensitive source code
* Line-oriented source code, so semicolons etc are rarely needed. (Most language are line-oriented, but many don't take advantage)
* 1-based arrays as well as 0-based and even N-based
* Use of whole-program compilers (always compile all sources from scratch; it's OK, these are fast compilers)
* No build system needed - just submit the lead module to the compiler; output is an EXE or DLL file
* Numeric literals can have scale factors such as '5 million'
* Out-of-order definitions throughout (no separate declarations needed of anything that is defined in the program)
* Circular and mutual module imports
* **strinclude** to import any text file (any binary file with Q) as a string literal
* **tabledata** to define parallel sets of enums and array data, or just parallel arrays
* No block scopes, only one function-wide scope
* Uses := for assignment, and = for equality
* Bit and bitfield indexing: A.\[i\] or A.\[i..j\], used as rvalues or lvalues
* 128-bit integer support (M only); not extensive, but better than gcc's.
* N-way selection: (n|a, b, c| z); this evaluates one expression only
* Interchangeable statements and expressions. Use any statement/expression as an lvalue, where meaningful
* *Easy-to-use* inline assembly (M only)
* Dedicated N-times repeat and repeat forever statements
* Function reflection: lists of all functions and references available to user code
* The systems language M, and its scripting language Q, *use the same syntax*
* Type punning using T@(X) compared with T(X) for normal casts
* Read/Print facilities built-in as statements, not library functions
* Ability to combine all source and support files into a single, directly compilable source file (.ma or .qa file)
* Basic maths function are built-in as *operators*
* 'stop' statement

### Features of M

M is probably best described as an alternative to C, although it was created independently. Here's a smaller summary of differences or extras:

* M has Algol68-inspired syntax
* Sane, left-to-right type declarations
* No C-style macro system
* Out-of-order declarations; no forward declarations ever needed
* Module system and build system: just compile the main module
* Case-insensitive
* Primarily 1-based; optionally 0-based
* Slices
* Fewer, saner operator precedences
* Smaller, tidier, more consistent and logical set of primitive types
* Built-in Print
* Strinclude and Tabledata features (see list of characteristics for these and other unusual feature)
* Better language choices overall (eg. non-global (static) by default)

Basically, M can do everything C can, but using an alternative, more comfortable syntax

### Features of Q

I will here compare Q to Python, as I'm most familiar with that, Python is a monstrously large and complex language by comparison, with a million add-ons available

* Q needs ahead-of-time compilation of all modules to bytecode before execution starts (fortunately it has a very fast compiler)
* Most things are actually static; in Python, nearly everything is dynamic
* Only variables have dynamic types
* Identifiers are classified, at compile-time, as functions, variables, labels, types/classes, enums, named constants, macros, and cannpt change. In Python, every identifier is a variable
* Named constants that cannot be changed, and that allow reduction of constant expressions
* Switch statement
* Properly defined mutable records (structs)
* References to objects, including pass-by-reference
* Most objects are mutable
* Goto
* More loop statements and support for nested breaks
* Built-in support for packed primitive types
* Built-in C-style structs
* Built-in homegeneous arrays of the same primitives types or structs
* Built-in bit-arrays
* Built-in FFI for libraries with C-style APIs
* There is no longer a single, monolithic bytecode file as output, simplifying distribution of applications. But there is a single-file .qa file (generated with 'qq -qa'), that can be run directly
* Self-contained installation comprising a single .exe file, including standard libraries.
* Static variables inside functions
* Built-in maths operators, and constants like 'pi'. (Python lets you change 'math.pi)
* ++ and -- ops
* Character constants (''A') and multi-character ones like 'ABCDEFGHI', which form integer literals.
* start() and main() functions that are run automartically (no messing with things like '__main__')
* Built-in *simple* enumerations
* Built-in read and print (see above characteristics list for more features)
* Oh, and Q usually runs much more briskly than CPython. (Sometimes, faster than PyPy.)
