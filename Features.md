## Features Compendium

**Introduction**

My languages are pretty much devoid of most advanced, modern features and type systems that practically everyone else
regards as indispensable.

This is partly a consequence of having started devising them when my main influences had been Pascal, Algol,and Fortran, and then working
in isolation for the next few decades.

That made it harder to later adopt new ideas. Either they were hard to understand, too hard to implement, or just didn't fit it. The fact
is that they rarely solved the problems I actually encountered.

So I have my own primitive feature-set. Here I'm just going to list the more unusual, unpopular, or
quirky ones, or ideas I consider characteristic of what I do.

There are some 'big' features, but most will come across as ordinary or even trivial (like `stop`). But this is a list of things I value and prioritise:
give me my `strinclude` over higher-order functions any day.

(I might refer to specific languages of mine as 'M': static, compiled; `Q`: dynamic, interpreted, and also `ASM`: my assembly syntax).


### Scripting Language
You might say this is not a feature, nor unusual. But I came up with the idea of an add-on scripting language
for my applications, sometime in the second half of the 1980s. It solved a number of problems with using just the one language, and I ended
up with a two-language solution, one I still use now. (The scripting language is now not tied to an application).

So they are two complementary languages, which *share the same syntax*, have largely the same features, and are designed to interact with
each other. But one is static, the other dynamic. (Imagine if C and Python had the same syntax.)

### Case insensitivity
 This might seen an odd one, but since modern languages tend to be case-sensitive, to me case-insensitivity confers
a number of advantages, that I won't go into (I'm not fighting that battle). The downside: if you want to use `abc Abc ABC` to all mean different things, then you're out of luck.

### Backtick
 This is the \` character that can be prepended to any identifier, which gives two useful attributes:

* The backtick is case-preserving. This means that \`abc \`Abc \`ABC are all distinct identifiers.
* It allows reserved words to be used as identifiers. This feature is heavily used in my assembler, but it also means I can write this, not
otherwise possible since `exit` is a reserved word:
```
     `exit(0)           # call C's exit()
```

### Named constants
Yeah, this just `const abc = 100`. Surely this is everywhere, yet Python doesn't have such a feature (my Q language does),
and even C has to emulate it using one of `#define, enum, const int, constexpr (C23)` none of which quite manage it 100%

### Flexible `end`
This merely refers to being able to use, for example `end`, `endif`, `end if` and `fi` to mark the end of a statement.
Again it provides a style choice. The first three work on all statements that need an `end` keyword, but some have extra ones (eg. `fi od esac` which
come from Algol68).

### `enumdata` and `tabledata`
I originally had just the latter to do both jobs, but found it useful to split into two.

Both allow an easy and maintainable to way to initialise parallel arrays of data. `enumdata` additionally defines a set of simple enumerations
in the first column. See [Example](MLang/Examples/aa_tables.m).

### Character Constants
These are constants such as `'A'` and `'ABC'`. Nothing unusual you might think. But you will struggle to find a scripting
language that directly supports `'A'` (in Python it's `ord("A")`, in Lua it's `string.byte('A')`).

Multi-char versions are even less common. Even in C, they are implementation-defined, and are still generally limited to 'ABCD' (because `int`
usually has `i32` type). I support 'A' up to 'ABCDEFG', yielding an integer-type constant (it can be used like a `u64` type).

### Goto
`goto` is around, although it seems to be on the endangered list as modern languages tend to omit it. (Try transpiling arbitrary control-flow
statements to a language which doesn't directly support that statement, and which doesn't have `goto`.)

But what's a little different how I do it is that it can be written in two ways (three if `go to` counts):

    goto error
    error

Yeah, you can leave out the actual `goto`! Useful if you prefer others not to see that you've stooped that low. Another unusual feature (you
will see this in gnu C) is to do with label pointers:

    p := (cond | L1 | L2)
    goto p                    # Jump to L1 or L2

(Label pointers are mostly intended for faster bytecode dispatching in interpreters, using tables of label pointers.)

### Bitsets
These are Pascal-style bitsets, which I've never seen anywhere outside of Pascal (if they're in Ada, then I don't recall).
Not quite as accomplished (I have a weaker type system than Pascal), I can still do this:

     letters :=  ['A'..'Z', 'a'..'z']
     digits :=   ['0'..'9']
     alphanum := letters + digits

Or just `[1..1'000'000'000]` - this billion-element bitset occupies 128MB.

### Bit Arrays
My Q language has arrays of 1, 2 or 4 bits. Other languages have bit-arrays, but I think that 2/4-bit elements
are uncommon. 1-bit arrays are a bit like bitsets, but they don't support logical operations, while bitsets don't have slicing.

### Print
I have `print`/`println` as built-in *statements*. Most languages like to implement them as library add-ons, but that needs advanced
features (generic functions with variable parameters, overloads etc) to make them as convenient as how I have them. Example:

    println =a, =b, =c

The `=` prefix (itself a micro-feature) adds a label; the output from this might be: `A= 100 B= 71.2 C= 007B0458`.

### Read
There is also `read`/`readln`, which although much less used, is a joy to use when needed. Code like this:

    print "Enter 3 numbers: "
    readln a, b, c

was routine in the 1970s, now languages are progressively making it harder. (Try the equivalent in C or Python; input must
be from one line only)

### `exit, redo, next`
These are loop controls. `exit` is better known as `break`, `next` as `continue`, with `redo` re-running the
same iteration. All can work with nested loops.

Nothing usual, but there is a surprising amount of variety across languages: they don't support `break` at all; or do so for this loop
only, or do so across nested loops. This stuff is really so easy to implement, it should be a no-brainer.


### endless loop
This one is rare because it is trivial to emulate with a normal loop. But if it's so trivial to add, and is quite commonly need, then
why not simply build it in? Mine looks like this:

    do
        body
    end

I use it at lot, becomes sometimes I'm not sure when I start, how a loop will be terminated, so will start off with an empty `do-end` loop, then fill
in the details later.

I just get fed up of having to write `while (1), while 1:, while (true), while (1=1), for(;;` and other such workarounds.

### repeat-N-times loop
 Like the endless loop, you often have emulate it with a loop that iterates over a range or counts down to zero. Here it
is just:

    to N do
        body
    end

No dummy loop index needed as in C's `for (int x = 0; x <= N; ++x)`.

### Swap
This ought to be common, but I don't really remember seeing it anywhere. Typically, multiple assignments are used to get the same effect:

    a[i], b[i+1] = b[i+1], a[i]

But this has a problem: each term is written twice, so there is a chance of error (and issues with side-effects). Also it's not clear if exchanging values is the intention: you
need to check carefully. If it's not quite exchanging terms, was that a typo?

Or worse, an intermediate temporary needs to be employed.

I would just write `swap(a[i], b[i+1])` - job done. That `swap` gives a significant hint as to what was intended! And less change of error.

### `switch` and `case`
These are two kinds of statements, usually implemented as one. My `switch` is for integer values
and requires contant test values, and is intended to be implemented as a jump table. If the test values are two wide-ranging
then `case` must be used. `case` is more general purpose, works with any types, and test values can be variable. However in the latter the test
values are compared one at a time, not all at once.

### `doswitch` and `docase`
These are looping versions of `switch` and `case`, but heavily used. (Yeah, it saves having to wrap the statement
inside `do ... end` and saves one indent level, to me it's worth it. No need even to optimise out that superflous jump.)

### N-way selection
 2-way selection is `a ? b : c` in C, or `(a | b | c)` in my syntax (which is just an alias for `if a then b else c fi`).
I extend it to to N-ways like this:

     (n | a, b, c, ... | z)

`n` is a 1-based indexed, and selects the n'th expression listed. If out of range, it selects `z`. This is unlike simple list
indexing like `(a, b, c, ...)[n]`, as that evaluates all terms; N-way selection evaluates only one. And the list deals differently with
an out-of-bounds index.


### String Include
`strinclude "file.txt"` incorporates any text file as a single string constant. I use this extensively (it is used
to add library source code to my compilers, or include system headers for my C compiler). If this program is called `prog.m`, then:

    proc main =
        println strinclude "prog.m"
    end

will print itself. I also use it for help text: `println strinclude "mm_help.txt"`. Now I can maintain the text just like an ordinary
text file.

(My Q language allows any binary files, including executable, to be incorporated. The M languages use `bininclude` for that, but
it's poorly implemented.)


### Out-of-order Definitions
Other than project directives, I allow out-of-order definitions for everything.
Variables in a function can be declared at the start of the function, or dispersed within it, or all the end.

### One Function Scope
Many, many languages use block scopes. That is, the start of any nested block, such as in the branches of `if-then-else`, introduces
a new scope. So there are only a couple of scope levels across 100,000 lines of code outside a function, but inside a 10-line function, you can have as many
as you like.

I only allow one scope inside a function. If anyone doesn't like that, then too bad!


### Expression-based
Algol68 was the biggest influence, so my static compiler was expression-based for many years. That is, expressions
and statements were interchangeable. But I rarely took advantage, so switched to statement-based to match the scripting language.

Now both languages are expression-based. I still don't use the feature much, but it's just cool.

### Bit-indexing
This is being able to extract and insert bits and bitfields from integers. If `A` is integer, then `A.[i]` extracts one bit as 0 or 1,
and `A.[i..j]` extracts a bitfield, so `A.[56..63]` extracts the most signicant byte.

Further, `A.[63] := x` sets the top bit to `x`, and `A.[0..7]:=255` sets the least significant byte to all ones. (Although for those, the
language directly has built-ins such as `.msbit` and `.lsb`.)

Other languages rely on shifts and masks, or write functions or macros or templates using them, which means building a bunch of code that
has to be reduced down again to basic instructions via optimisers (see Pure Speed below)

### Dot-indexing
Bit-indexing is a special case of dot-indexing, or `X.[i]`. Normal indexing selects elements of objects considered multiple
values, like lists and arrays. Dot-indexing selects components of objects considered a single, composite value.

So in `X.[i]`, `X` can be a record (allowing access by index as well as by name), string or integer. (The syntax I think was taken
from DEC Algol-60, where it was used to extract the characters of a string, where strings were first-class objects.)

### Multiple Main functions
My entry point function is called `main()`, which is very common. However any module in a program can have a `main`
function. The one in the first module encountered (generally the first one containing code) will be the entry point.

The other `main`s are for when those other modules are compiled into independent programs. Or (for scripting) when that module is run directly.

### Start functions
Any module can have a `start()` function, if so it will be called automatically at program entry, to provide any needed
initialisations. (The order they are called is determining by the module ordering in the project information which lists the modules.)

### `b..c` AND `[b,c,d]`
These constructs create first class range/set types in the Q language, so not very interesting. In M however they
are commonly used in conditions:

    if a in b..c               # true when A is in that include range
    if a in [b,c,d]            # true when A is equal to any of B, C, D

I don't know what other languages use to compare `a` against a small number of other values, *without repeating* `a`. On a larger scale, there is `switch` for example.
I might use it as `if c in [cr, lf , tab] then`.

### N-based arrays
All my languages have a strong-preference for 1-based everything (array lower bounds, enumeration start, N-way selection etc).

But they do allow 0-based as well as N-based arrays. This is not the only language that has definable lower-bounds, but it does
appear to be an unpopular characteristic. (The last discussion on this wasn't so much about being 1-based, as allowing *a choice*. I didn't
realise it was that controversial.)


### Scale factors
I have a handful of suffixes which are used as `3 million`, `4 billion` and so on, which just apply a scale factor
to yield `3'000'000` and `4'000'000'000` respectively.

(I used to have a lot more of those, used for physical dimensions such `0.3048 m`, and `1 ft`, both yielding 304.8 since I used millimetres
for lengths, but that's when Q was used as a scripting language for my engineering-based GUI applications.)

Those `million` suffixes are used all the time in benchmarking code. BTW `million` can still be a user identifier, as they don't clash in the syntax.


### Numeric separators
These are slowly becoming routine. But I've had them for a while and, as expected, there is a choice:
`123'456'789` or `123_456_789` or mix them up.

### Access inside Functions
This one might raise eyebrows. Given this function:

    func F =
        static int A
        const B = 100
    end

Those entities can be accessed from outside using `F.A` or `F.B`. Anything can be, except for parameters and stack-frame variables (since
either they will not exist, or there could be multiple instances). This is just treating a function as an encapsulation/namespace mechanism
like a record can be.

Actual uses include when functions `F` and `G` want to share private, persistent data between them.


### `func` and `proc`
I'm puzzled that so few languages make any strong distinguish between routines that return values, and those that don't.

At best a procedure is just a function that notionally returns a `void` value. To me, that is not a strong enough visual difference; the keyword
is the same, you have to peer carefully at the syntax, and isolate the return type, or whatever it might be. But in dynamic code with no types,
there will be no difference. Then you have to look at the body of the function, or its docs.

My languages always use `func` for a function returning an actual value, and `proc` for routines that don't. Or you can choose
`function` and `procedure`.

This way forces you to think more clearly about which kind you are creating, and it will be instantly obvious to a casual reader. (In C of course,
you can barely tell the difference between function, procedure or variable: you need to infer it from the exact pattern of parentheses.)


### Default and Keyword Parameters
Nothing new. But what I might do differently is that I can superimpose these on API functions written in a language
that doesn't use the feature. Since I have to write bindings for FFI functions in my syntax anyway, why not upgrade them? That means I can just
write this:

    messageboxa(message:"Hi There")

without needing to worry about how many parameters it takes, or their ordering (or the exact true capitalisation of `messageboxa`).

### Clear
Unlike C, I can't partially initialise arrays and records so that the rest of them are all-zeros. I don't want to use `memset()`, which
would involve an actual function call. So I experimented with these three different ways to zero a fixed-size object:

    [10]int A := empty, B, C
    B := empty
    clear C

I used that `clear` method most often, and may drop `empty'.


### FFI in Scripting Language
Every script language seems to have an ad hoc arrangement for calling FFI functions. That is, using APIs with lower-level types that
include structs with very specific layouts. It usually seems to be an afterthought.

My Q language natively supports such low-level types and APIs, which can be specified as easily as within a statically-type language.

### Maths functions
Slightly unusual, the dozen or so maths functions that are built-in, are operators, not functions. That can mean
I could write `sin x`, without parentheses, although it looks better with. But they can also be properly overloaded as needed. (However
most only work on float64 anyway.)

### `min/max` and Augmented assignments
`min` and `max` are built-in operators. Unusually, they can be written as `a max b` or `max(a, b)`.
I nearly always write the latter, but the first allows this form of augmented assignment:

    a min:= b            # a := min(a, b), but `a` is evaluated once


### Unary Augmented Assignments
This is common for binary operators; I think less common for unary ones:

    -:= a                # a := -a
    abs := a             # a := abs(a)


### Type Punning
I write my type conversions as `T(x)` or `cast(x, T)` (syntax ambiguities mean I can only use the first form when `T` is simple).

A type-punning conversion (reinterpreting bits without doing any conversions) would be written as `T@(x)` or `cast@(x, T`).

Unusual? Well, compared to C, this works happily with rvalues: `T(x*2+1)`. As for type-punning itself, it won't be unknown, though I can imagine
the Rust compiler having kittens if you tried to do some of this stuff.

### `if-then-else` and `(||)`
Expression-based means that `if-then-else-fi` directly be used inside an expression, but that syntax is too
heavy for that purpose IMO. So there is a more compact syntax for that purpose:

    if a then b else c fi
    ( a | b | c)

These are exactly equivalent, and the compact form can be used for standalone expressions. (This is not one of my ideas, it's a rip-off
of Algol68, which went further with compact syntax for `if-then-elsif-else`, but I felt that went too far; too easy to write cryptic code.

However its compact syntax for its `case` statement (different from my `case`) formed the basis for my N-way select.)

### unless
This is just `if` with the opposite logic:

    unless cond then
        a
    else
        b
    end

But there is no `elsif`, as I wouldn't be able to tell you what it might mean! I suspect it might need to be `elsunless` anyway, but even more
puzzling.

### `elsif`, `elsecase` and `elseswitch`
I have these three conditional statements `if case switch` with multiple branches (N-way select is only used in expressions).

All have an `else` branch, but only `if` has `elsif`. However it sometimes happens that I test some things using `if`, `case` or `switch`
then want to switch to one of the others. That can be done with `else` followed by a new, nested statement, but it didn't feel right - the
task is not really nested. So, why not combine them?

    if a then
        ....
    elsif b then
        ....
    elsecase c
    when x, y then
        ....
    elseswitch d
    when i, j, k then
        ....
    else
        ....
    end if

It looks ungainly, but is actually quite handy.


### `stop`
This is one of my favourite features, because it's so simple and to the point:

    stop             Stop the program (with exit code 0)
    stop n           Stop the programs with return code n

(In C for example, you write it as `exit(0)`, but you have to add `#include <stdlib.h>` at the top of the module before you have that privilege.)

(In my original Q language, when used as an application scripting language, `stop` would terminate the current module, and return control
to whatever code invoked this module.)

### Conditional suffix
I use these simple statements changing control flow: `goto return exit redo next stop recase`. Any of these
can be made conditional like these:

     return 0 when n>100
     stop unless n=777

It's exactly identical to wrapping in an `if`/`unless' statement, but allows for one-liners, and the control statement is more prominent.


### Equivalence
This is an old feature of Fortran I once copied and still have:

    int A
    real X @ A

This just means that `A` and `X` share the same memory. (Not quite like C's `union`, as you can't refer to those values independently.)

### Naked Functions
These are defined as:

    threadproc F =
    end

(Since their main use was for my threaded-code intepreter.) They have no entry or exit code, and no stackframe. Unusual? I don't know
know (hard to imagine Rust having anything like this; I think it doesn't even have regular `goto`)

### Inline Assembler
A consistent feature of my systems language for 40 years, for me it is nothing remarkable.

You will come across inline assembly in other lower level languages, but often with ghastly implementations, for example having to write
your assembly code within a string literal; having no access to locals (you need to hardcode stack-frame offsets), or the recent gcc one
where you first to have to define an interface between ASM and C using a mysterious set of directives, so that it can work within its optimiser.

Mine by contrast has always been easy-to-use, and alway puts you in control:

    int a, b, c
    assem
         mov D0, [b]
         add D0, [c]
         mov a,  D0
    end

### Function tables
All functions in a program can be enumerated at runtime. Another extensively used feature, this is used, for
example, to populate function tables for interpreter bytecode handlers. The function name has a particular pattern that is looked for.

This allows functionality to be added or removed or rearranged, without needing to manually maintain function tables (or use an external
script as I used to do).

### Function Metadata
That refers to this in Q:

    func F <"abc def"> (a, ) =
    end

The angle brackets contain a string that is associated within the function, and can be retrieved at runtime given a function reference. This
feature is still in use, but is deprecated. I would now encode any special info within the function name; see Function Tables above.


### `One File`
This is a principle I've been working with over the last few years (in the past, hardware wasn't quite up to it):

* Each tool (compiler, interpreter, assembler) is a single, self-contained executable
* Each tool, if it produces output, will always generate a single file representing the whole program
* The M and Q tools can take the source and support files of any application, and write out a single amalgamated source file
* M and Q can compile/run code directly from that single amalgamation
* If producing C distributions (using a new M compiler where this is being reinstated), the output is a single C source file
representing the whole application. 

You can imagine how simple this makes things. A typical installation might consist of 1000s of files disseminated across your file system.
Is every essential file present? Who knows! What is the minimal subset needed. How do you copy the installation onto a pen drive etc.

With one file, either it's present or not present. Copying, deleting, sending, comparing is trivial.

With a C distribution, building a 50,000-line app on Windows is exactly the same process a building a 5-line hello.c. No make files, CMake
or other nonsense is involved. (On Linux, you might need `-lm` and `-ldl`)

But is this a language feature? Yes I think it is, because it's all by design.

### Whole-program compilation
To follow on from one-file, both languages are designed to be whole-program-compiled.

I don't know how common this is; independent compilation is traditional. Whole-program compilation is troublesome with slow-to-compile
languages, and there is the question of scalability. But my products can build 50,000-line apps in some 0.1 seconds, and they're not
much bigger than that.

(To put it other way, the granulity of compilation units moves from module-at-a-time to program-at-a-time. Which could also simplify
whole-program compilation, if I ever get into that.)

### Run from source
This refers to my static language, which can now run applications from source. I used to think that was little different
from writing an EXE then the compiler invoking the result. This is subtly different, there is no EXE, and it's all done in-memory, just
like an interpreted scripting language does.

To make it practical requires a fast (whole-program) compiler, but to make it properly effortless requires it to work just like a scripting language.

Normally the `-run` option is applied for this, but the compiler detects if its name is `ms` (M-Script), and applies that automatically. Then I can do this:

    qq hello               # run hello.q
    ms hello               # run hello.m

### Pure Speed
One aspect I've cultivated is for my tools to be fast. I'm a little handicapped by not having an optimising compiler (my static language is self-hosted) and my machine is low end, but take this example:

    C:\mx>ms ms \qx\qq \qx\hello
    Hello, World!

What's going on here is this:

* `ms` (compile-and-run compiler) is building a fresh version of itself into memory, and running it
* The new compiler is than used build my Q interpreter into memory, and running it
* That new interpreter is then used to run hello.q

This takes 0.2 seconds (to process 80,000 lines; actually, gcc takes 0.22 seconds to compile a 4-line hello.c).

An equivalent example using mainstream languages would be for gcc to first build itself from scratch, then that
new version is used to build CPython, which then runs hello.py, and do it all in one core.

So, what's the feature here? I suppose I could run my compiler from source each time (it adds 80ms to build-time), but that's not it.
The features I've chosen (or rather, the ones I haven't chosen) have been conducive to a fast compiler.

### Module Scheme
One aspect of it is like this. Suppose a program consists of these four modules:

    P.m A.m B.m C.m

The first, `P.m` contains information about the module structure. But if it *only* contains that, then it can be considered to
not be part of the source code proper, not even part of the language, but is build information that tells the compiler what
modules constitute the application.

It's more like a 'make' or project file, but vastly simpler, and not requiring an extra tool. This also makes it easier to have multiple versions
of P.m for different configurations (one or more modules can be different).

### Private Binary Formats
My M compiler can write to alternatives to `EXE/DLL` binary files, which I call `MX/ML`. Originally because my
DLL generation was buggy, so I created the far simpler ML format to be used as from my languages. That was naturally extended to MX
for applications, but that needs a helper program to launch it from the OS.

I'm still working out the possibilities for these products, but they offer some advantages:

* ML makes sharing variables easier than with DLL
* ML is guaranteed to share the same environment as the host (so can share file handles and memory allocations for example)
* MX/ML format ought to be portable to Linux-x64 (although the code inside still needs targeting at the right ABI)

## Old Features

These existed in older versions and have since been dropped.

### `int*4` and `int:32`
This is syntax directly copied from DEC Fortran, where `INTEGER*4` specified a 4-byte or 32-bit integer.
I also used `int:32` to specify the width in bits not bytes (so `int:24` is possible), while `byte*8` was used for unsigned types.
There was also `real*8`.

I now prefer single token token type names, and also realised I'm never going to support odd-sized types (bit-fields are a separate
feature).

### `stack` and `unstack`
`stack a, b` would push those expresssion onto the hardware stack. `unstack c,d` would pop them. No type checking
was done. This was used to save and restore values (arrays and records were pushed by value), or to do forms of type-punning, or it
could be used to swap two values. Or to individually push arguments for a synthesised function call.

(It sounds handy; maybe I can bring it back! But modern ABIs wouldn't like it.)

### `inp(n)` and `outp(n)`
These were for reading from and writing to hardware ports. Something not really practical under Windows 10.

### @block
This was my idea for being able to share common bits of code within a function, a sort of lightweight local function:

        case x
        when a then
    fred::
            println "A"
        when b then
            println "B"
            @fred
            println "C"
        esac

I can't remember the exact syntax, but any block could be labeled, and `@fred` would execute that block then return. When `x=b` here, output
is `B A C`. Blocks could also be formally declared:

    block fred =
    ...
    end

So more like a local function, but no call/return, no stack, no locals.

### `/` Translation operator
In the script language, `/"Length"` would apply a translation function to the string, used for internationalisation.

A separate utility would periodically scan source code for translatable string constants to maintain a dictionary of messages. Source code
used English messages (with hints when there was ambiguity), the running program would turn those into the local language.

(This could have been just a function, but this was clearer, less obtrusive, and easy for a script to scan for.)

### Auto-deref Control

I use `ref` to create a pointer type, and `^` to dereference a pointer value. I used to have strict transparency,
so that the number of `^` needed to exactly match the number of `ref`s:

    Declaration           Access int value using

    int A                 A
    ref int B             B^
    ref ref int C         C^^

I devised this syntax:

    ref. ref int D        D^
    ref ref. int E        E

These variables would be automatically dereferenced up to the "." dot. An ordinary `ref T` declaration was considered equivalent to `.ref T`.

Uses for these included, emulating how by-reference parameters are used now (where the ^ is implicit). Or allowing functions accessed
via function pointers, to be called as though they were regular functions.

Not really needed now as I allow some degree of auto-deref anyway, and I have by-ref parameters (although those `A B C` examples would still need derefs).

### Bytecode Binaries

For most of its evolution, my scripting language generated discrete bytecode files, one per module. Later versions would produce a single,
monolithic file for the whole application (the One File principle).

The advantage was a tidy packaging of the application, and the source code was not visible. However I've recently brought it into
line with how other scripting languages work, which is usually direct from source.

That leaves the problem of packaging applications, but that is done now by generating a .qa single-file amalgamation. Source stays
visible however.
