## Summary of Language Projects

I will shortly be wrapping up further development of my personal languages, so this is a summary of what they are up to.

These are not modern languages with all the lastest ideas in advanced features. Just simple, down-to-earth tools to do a job of work.

Development is done on Windows, and implementations are mainly for Windows

### The Languages

Language  | Program | Project name | Written In | Description
--- | --- | --- | --- | ---
Q  | qq.exe | QX  | M | My dynamic, interpreted scripting language
M | mm.exe | MX | M | My systems programming language
PCL | pc.exe | MX | M | Portable intermediate language, used as target by mm.exe
ASM | aa.exe | AX | M | Assembly language for x64 using my syntax
(C  | bcc.exe | CX | M | C subset)

The first two languages are designed for writing programs in. The next two, while they could be used for writing whole applications in, are intended as code-generation targets. (And mainly for testing, as usually textual intermediate forms are by-passed.)

I've included the line about C, which is clearly not my language, since it rounds off the set of language tools. bcc.exe has a number of uses, including helping to turn C headers into my M syntax, but an important one is for testing: by building bcc.exe with a new version of mm.exe, I can test it on a wide range of C source files.

### Availability

I class these as private languages, although anyone can try out the .exe files whenever I upload them, because:
* I can't do the support that would be needed for general use
* There are no proper docs
* The error reporting is poor
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
5 products - 5 self-contained executables. Each is just one file. The largest ones include source files which bloats their size:

* bcc.exe includes the C standard headers, plus windows.h (the latter accounts for half the size)
* mm.exe includes sources for its standard library, as it is usually compiled into the application
* qq.exe includes sources for *its* standard library (including a small graphics library for Win32)

This makes it easy to copy an implementation, transfer it via USB drive, email it, run it (from anywhere, it doesn't really need installing etc), or even just check that you have everything you need, or maintain different versions since there are no dependencies.

(Note: mm.exe includes most of pc.exe; the latter is used for processing standalone .pcl files, or, in pc.dll form, not shown, for use from other languages.)

### Building My Compilers

I'll give a demo of how it looks. First, I will use a special feature of mm.exe to create a single-file amalgamation of a project's source files, which turns multiple .m and support files into one .ma file. If I put those .ma files into one place, they look like this:
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

This is a simple script to build them, as I want to time it:
```
C:\demo>type build.bat
mm cc.ma
mm aa.ma
mm pc.ma
mm qq.ma
mm mm.ma
```
Now I can run it (tm is a timing tool):
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
So building **all** my compilers, assemblers etc, from source code, takes 0.6 seconds, on my very ordinary PC. (In total, 175,000 lines, and just over 200 modules and files. Timings take advantage of any file-cacheing.)


### So, What's Special about these Languages?

The last two sections should illustrate what I value in a language implementation, and which I consider as important as what's inside a language: being small, simple, self-contained, effortless to use, and very fast.

I don't like large, sprawling implementations that take forever to build a program.

Below I will highlight some of the features I do have inside some of these. But first...

### Features I don't Support

This is not to imply a criticism of such features, I just don't have them.

I was going to do a list, but just assume dozens of advanced features regularly touted as must-haves in Reddits r/PL forum; and think of most things bundled into languages such as C++, Rust, and every FP language around.

Most, I simply don't understand and can't get my head around. Others are too hard to implement or not worthwile for me, as I likely won't use them. Some I just don't like, like over-elaborate type systems, or language-building features (eg. templates and advanced macros).

I get by with A two-level language solution; some things don't exist in M, but I can use them in Q.

### A Selection of Characteristics

Some characteristics and a few features from M and Q that are either unique to me, uncommon or unpopular:

* Case-insensitive source code
* Line-oriented source code, so semicolons etc are rarely needed. (Most language are line-oriented, but many don't take advantage)
* 1-based arrays as well as 0-based and even N-based
* Use of whole-program compilers (always compile all sources from scratch; it's OK, these are fast compilers)
* No build system needed - just submit the lead module to the compiler; output is an EXE or DLL file
* Out-of-order definitions throughout (no separate declarations needed of anything that is defined in the program)
* Circular and mutual module imports
* Most products are implemented as one self-contained executable
* **strinclude** to import any text file (any binary file with Q) as a string literal
* **tabledata** to define parallel sets of enums and array data, or just parallel arrays
* No block scopes, only one function-wide scope
* Uses := for assignment, and = for equality
* Bit and bitfield indexing: A.\[i\] or A.\[i..j\], used as rvalues or lvalues
* Numeric literals can have scale factors such as '5 million'
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

M is probably best described as an alternative to C, although it was created independently. Here's a smal summary of differences or extras, that are not already listed above:

* M has Algol68-inspired syntax
* Sane, left-to-right type declarations
* No C-style macro system
* Module system and build system: just compile the main module
* Slices
* Fewer, more sensible operator precedences
* Smaller, tidier, more consistent and logical set of primitive types
* Better language choices overall (eg. non-global (static) by default)

Basically, M can do everything C can, but using an alternative, more comfortable syntax

### Features of Q

Q is lower level than typical scripting languages, and more static. But since I'm most familiar with Pythin, here I will list many useful features (in addition to any above) that are not present or not native in Python, and some characteristics:

* Q needs ahead-of-time compilation of all modules to bytecode before execution starts (fortunately it has a very fast compiler)
* Most things are actually static; in Python, nearly everything is dynamic
* Only variables have dynamic types
* Identifiers are one of module, function, variable, label, type/classe, enum, named constant or macro, and cannot change. In Python, every identifier is a variable
* Named constants define quantities that cannot be changed; they allow reduction when used in expressions; and make practical switch statements:
* Switch statement
* Built-in support for user-defined mutable records with named fields (why do so few script languages support something so practical, yet they all do closures, whatever those are?)
* References to objects, including pass-by-reference
* Most objects are mutable
* Goto
* More loop statements and support for nested breaks
* Built-in support for packed primitive types and C-style structs
* Built-in arrays of the same primitives types or structs
* Built-in bit-arrays
* Built-in FFI for libraries with C-style APIs
* Applications typically distributed as a tidy, single .qa file (generated with 'qq -qa'), that can be run directly. (I no longer generate bytecode files.)
* Static variables inside functions
* Character constants like 'A' and multi-character ones like 'ABCDEFGHI', which form integer literals.
* start() and main() functions that are run automatically (no messing with things like '\_\_main\_\_')
* Built-in *simple* enumerations
* Oh, and Q usually runs much more briskly than CPython. (Sometimes, faster than PyPy.)

### Some Features of PCL

This intermediate code is described in more depth [here](../pcl)

### Some Features of ASM

Most of the syntax of this x64 assembler is fairly standard. To summarise:

* Intel-style instruction formats
* Case-insensitive (uses ` prefix for a case-sensitive name or to use a reserved word)
* Uses :: for labels to export them (so no GLOBAL directive needed)
* Uses a * suffix for identifiers to import them (so no EXTERN directive needed)
* Can assemble multiple ASM files into a single EXE or DLL file, so can do the job of linker with no .obj intermediates
* Can also generate a *single* OBJ, useful for combining with other software (needs third party linker)
* Very fast assembler, some 2M lines per second or more
* Lacks some common features like macros, or complex expressions, as it is not meant for writing in directly. (The inline version within M provides a better ASM experience.)
* Supports standard register names, but they are so inconsistently and messily named, that I use my own naming scheme:
```
    D0 to D15         64-bit registers
    A0 to A15         32-bit registers
    W0 to W15         16-bit registers
    B0 to B19         8-bit registers (the extra four are for AH BH CH DH)
```
The ordering is also different, since the official ones are all over the place. Here, the ordering is optimised for the Win64 ABI:
```
    D0        D0-D2 are volatile registers (can be trashed by a function call)
    D1
    D2
    D3        D3-D9 are non-volatile (must be preserved by a function call
    D4
    D5
    D6
    D7
    D8
    D9
    D10       D10-D13 are for parameter-passing, also non-volatile
    D11
    D12
    D13
    D14       (Also Dframe) Frame pointer
    D15       (Also Dstack) Stack pointer
```
My AA assembler is designed to process the generated code of my compilers, so supports only a subset of x64 instructions, which otherwise go on for ever (with SSE2, AVX etc).

### Examples

Some Hello World examples are above. Further examples:

[**M Examples**](../Examples)

[**Q Examples**](../QExamples)

[**PCL Examples**](../pcl)


### Targets

The primary target now is Win64 using the x64 processor. (When I started developing M, the target was 8-bit Z80.)

I have had Q and M running on Linux, but they used a version of mm.exe that could target C source code. I no longer have that, but probably I will have to add it again.

A C target lets me benefit from the better optimising of C compiler, and allows code to run on Linux, and on ARM devices.

I had planned to have C as a target of my PCL, but the generated code is absolutely appalling, and *needs* an optimising compiler to get decent performance.

But, I don't consider that acceptable quality, even if it can work. So at some point I will add a proper C translator working from my AST rather than PCL. The generated C code will be readable; smaller; and it will compile faster, with acceptable results even with Tiny C. However, I will only support a 64-bit target.

### Further Development

I want to wind this down, so there is only one big thing I want to do:

* Revise the module system of M to be sub-program-based rather than module-based. I started seeing problems in combining assemblies of modules used across different projects
* That change might be rolled out to the Q language which used the same scheme as M
* Continue with on-going work implementing missing features and fixing bigs, and doing minor refinements
* Possibly (gasp) doing some documentation...

There were some big ideas, including, for Q, allowing multiple programs to work under the same runtime environment.

(The version from 25 years ago, used as a scripting language for my commercial GUI apps, was more dynamic. Modules were compiled independently to bytecode, and hot-loaded into a running application. There they shared the environment and global state of that application.

But I'll have to see how it works out with my rather more staid version.)






