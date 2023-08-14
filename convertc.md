## Translating C Programs

A new project I've started has the aim of translating C programs into my own 'M' systems language. That is, doing a one-off conversion so that the new program is maintained in my language.

There are lots of problems, many of which are explored here. It could well be that in the end, the idea will be dropped. Even if viable, it's unlikely that a 100% perfect translation can be achieved without manual intervention, except for the simplest programs.

### My 'BCC' C Compiler

Any new tools will need to use this product as a start point. Any C programs need to be acceptable to this compiler, but there are lots of features not supported, which have become popular since I started that project, like VLAs, compound literals, designated initialisers. Plus it is buggy.

For use as translator however, only the front end is needed. It is possible those features can be added, but I don't need to generate code for them.

### Two Kinds of Translator

I already have two existing tools that can do translation of C:

* Translating headers into binders for my two languages. This works via options `-mheaders` and `-qheaders` applied to BCC. The resulting outputs is 90% there, but need manual finishing off.
* Translating acual programs into my M syntax. This was a fork of the BCC compiler, called 'CCM'. The output was purely for visualisation, as obstruse C syntax can suddenly become crystal clear, but the M code will not usually compile, and semantic diferences are ignored.

This project is a development of the latter CCM tool.

A typical C module, as seen by a compiler when a source file is submitted, will contain these three kinds of code:

**(1)** Declarations of functions from an external library. That is, functions not defined in the other modules of this program

**(2)** Declarations of functions defined in other modules of this program

**(3)** Local function definitions

(I've left out variables for simplicity. Typedefs, structs, enums, macros and so on will usually need to be seen by both translator kinds.)

A program translating APIs like the `-mheaders` option of BCC, is only interested in **(1)**. A program translating code like `CCM` is mainly interested in **(3)**. There is no need to do **(2)**: those will be done when the module that defines them is processed.

An example of **(1)** is a program like this:

    #include <sdl.h>

There will be no definitions of SDL functions in this program, but M requires an interface module.

Unfortunately, C syntax does not distinguish between `**(1)**` and `**(2)**` declarations. Those cannot be determined from a single module. Only after analysing the results of translating all the modules of a project.

One possibility is marking `includes` that refer to external libraries, or listing those headers either in the C source, or in an auxiliary file attached to the project.

### Case-Insensitivity

My syntax is case-insensitive; C isn't. There are two approaches I can use in M:

* For external function names (**(1)**), I can define the names in quotes, and subsequently call them in any case. This is prefered for translating an API that I will then use for newly written M code, although there will be the odd clash.
* For translating **(3)** code, there will just be too many clashes. Here I can use a backtick:

    `Abc `ABC `type

The backtick preserves case so that `Abc` and `ABC` and `abc` are distinct identifiers. Also, I can use identifiers that are reserved words in my language, like `type`, another big program.

The downside is that using backticks everywhere will make the resulting M code unreadable and unmaintainable. So here, I may need to do some analysis of all identifiers used in a whole program, and only use backticks where there is ambiguity.



