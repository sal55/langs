## Translating C Programs

A new project I've started has the aim of translating C programs into my own 'M' systems language. That is, doing a one-off conversion so that the new program is maintained in my language.

There are lots of problems, many of which are explored here. It could well be that in the end, the idea will be dropped. Even if viable, it's unlikely that a 100% perfect translation can be achieved without manual intervention, except for the simplest programs.

### My 'BCC' C Compiler

Any new tools will need to use this product as a start point. Any C programs need to be acceptable to this compiler, but there are lots of features not supported, which have become popular since I started that project, like VLAs, compound literals, designated initialisers. Plus it is buggy.

For use as translator however, only the front end is needed. It is possible those features can be added, but I don't need to generate code for them.

### Two Kinds of Translator

I already have two existing tools that can do translation of C:

* Translating headers into bindings for my two languages. This works via options `-mheaders` and `-qheaders` applied to BCC. The resulting outputs are 90% there, but need manual finishing off.
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

Unfortunately, C syntax does not distinguish between **(1)** and **(2)** declarations. Those cannot be determined from a single module. Only after analysing the results of translating all the modules of a project (and seeing which are missing, very hi-tech).

One possibility is marking `includes` that refer to external libraries, or listing those headers either in the C source, or in an auxiliary file attached to the project.

### Case-Insensitivity

My syntax is case-insensitive; C isn't. There are two approaches I can use in M:

* For external function names as in **(1)**, I can define the names in quotes, and subsequently call them in any case. This is prefered for translating an API that I will then use for newly written M code, although there will be the odd clash.
* For translating **(3)** code, there will just be too many clashes. Here I can use a backtick:
````
    `Abc `ABC `type
````
The backtick preserves case so that `Abc` and `ABC` and `abc` are distinct identifiers. Also, I can use identifiers that are reserved words in my language, like `type`, another big problem.

So a simple approach is to apply backtick to every identifier.

The downside is that using backticks everywhere will make the resulting M code unreadable and unmaintainable. So here, I may need to do some analysis of all identifiers used in a whole program, and only use backticks where there is ambiguity.

Sometimes the program I want to translate is a library written in C, that I then want to use as a library from normal M programs. And there I don't want to use backticks. If backticks are used in the name of a function, I have to use them everywhere.

I believe however that I can omit backticks for identifiers (not M reserved words) in lower case, and only use them (and then if unambiguous) for mixed or upper case.

### Operator Precedence

C's operator precedences are ignored by the CCM program to keep readability high. Fixing them would mean parentheses everywhere.

So here, I'd need to use algorithms to minimise them: only apply them to correctly apply the priortities of my language. So `a << b + c` in C, which means `a << (b + c)` in that language, would have to be `a << (b + c)` in mine, but I don't need `(a << (b + c))`.

C functions such as `abs labs llabs fabs` are operators in M, and should really be specially detected and converted. Some, like `sqrt(x)` which are also functions, would be parsed as operators anyway, so no special conversion is needed.

However, being able to use function points (`&sqrt` or `sqrt`) will not be possible.

### Macros

Macros will be minefield. Simple ones `#define A 100`, `#define B 100` should really not be expanded in the M code, which otherwise will sea of meaningless constants, and you can't tell whether any `100` means `A`, `B`, another `#define`, or is an actual constant.

More elaborate needs need expanding, but this risks the M code becoming unreadable. They can't be left as macros as, while M has a macro scheme, it works on well-formed expressions only, and expansion is done at a later stage.

### Constants

CCM will need to remember whether the constant 65 for example was written as `65`, `0x41` or `'A'`, and output it in the original form.

This applies also to enum names; given `enum {A, B, C}`, then `B` should appear as `B` not `1`.

Constant expressions such as `2 + 3 * 4` should not be reduced to `14`, but appear in original form. This means also that `C+1` (see above) is `C+1` and not `3`.

Another problem is using `0` as a null pointer value; M requires `nil` here.

### Block Scopes

M has no block scopes; there is one function-wide scope only. This would mean that many local identifiers would need a block-id suffix. But I think that in most cases, identifiers are not re-used. So analysis can be used, and suffixes are only added when an identifier has more that one instance in any function.

There is an issue with shadowing; suppose `A` exists with global scope:
````
     int A;                  // 1
     void F(void) {
         A;                  // 2
         {int A;             // 3
          A;                 // 4
````
Here, `A(2)` is the global `A`, but `(3)` and `(4)` are local. In M, those last two `A`s have function-wide scope; it is not possible to refer to the global 'A'. So more analysis is needed to see if local are shadowing a global, and apply block-suffixes.

### Switch

I'm sure that most uses of `switch` are properly structured, but C's `switch` can also be chaotic, with case labels nested deep inside statements, and `default:` appearing at the start! There is also fallthrough and `break` to deal with. It's going to be messy.

Also, C-switch may have widely-spaced case values. Here I would need to detect this, and switch instead to M's `case` statement which is more flexible. Except that the constants used for case-labels might not be available, because of no const-expr reduction. But I will most will be simple constants.

### Augmented Assignment

Operators like `+` return a value in C: `a = (b += c)`. M's versions don't. I may need to translate to value-returning `b += c` to `(b +:= c; b)`, taking care if evaluating `b` twice.
