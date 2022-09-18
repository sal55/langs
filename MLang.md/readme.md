## My Languages 2022, Overview

I maintain and use these two complementary languages:

**'M'** Lower level systems language

**'Q'** Dynamic, interpreted scripting language

Plus:

**ASM** My take on x64 assembly code.

### Features

... that people of expect of a language and implementation these days:

*Sum types, pattern-matching, lambdas, closures, OOP, inheritance, type inference, exotic type systems, syntax-highlighting, language servers, static analysers, optimising compilers, debugging tools, tutorials and reference materials attractively presented, tested over tens of millions of lines of code, threading, parallel execution, networking support, IDE support, robust implementations, great error reporting, available on multiple targets, ...*

Unfortunately my languages support none of those. They:

* Are embarrassingly crude and unsophisticated compared with any modern languages
* Are full of unpopular design choices (eg. are case-insensitive)
* Have minimal implementations
* Do not have optimising compilers
* Have no support tools such as debuggers
* Have next to no libraries, with no easy path to creating bindings to more
* They are insular and cannot talk to any other software except via DLL functions (or files). They can't even statically link to other languages without a lot of trouble.
* Only work on Windows, not on Linux, and will never work on anything like Android
* Have no proper docs
* Have patchy and buggy coverage of combinations of types, operations and features
* Have no generally available implementations, which cannot be supported anyway
* Are largely unsafe (eg. nearly everything is mutable)
* Have ambiguous grammars (or would have it I tried to attempt them)
* Are not stable, although that is something I currently trying to do something about

I'm not selling them very well, am I? But the above is an honest summary. What I'm doing, in the PL world, is basically insignificant.

But the purpose here is not to offer working products, but to write about my languages because someone might find it interesting, or take something away from it, even it's just to avoid making the same mistakes.

So, having got that out of the way, and for anyone still reading, I'll continue.

### Background

M and Q both have very long histories I won't go into. (Although I've written a more detailed summary of my various native code compilers [here](../mycompilers.md).)

Despite that, they haven't really evolved much; they still have clunky 1980s designs, which I've decided to stay with.

M was a tool, developed part-time, to help in my main work, and it worked well enough that when the opportunity arose to switch to C, I decided to stay with M.

Q started as an add-on scripting language for my applications, which other people used to create extra functionality and even add-on products. The current version is a standalone language.

Currently, the nearest mainstream alternative to M is C, with about an 80% match (in things like type systems, and what the languages can do).

With Q, perhaps Python at 30% match. (Python is at least one level higher, and *much* more dynamic; it also has most of the stuff in that list. OK, better make that 10% match then!)

My ASM language is probably a 90% match with Nasm (it's just 1000 times faster at assembling; literally so on some tests).

### Future Development

I've decided to stop individual development of both languages for a while. Document what there is, make sure both languages work the same way for common features, just create stable working versions. I've found the this of documenting the features invaluable for that; previously I kept it all in my head and could never remember what worked, or where.

Since M and Q had seemed to converge over the years, I made several attempts to combine then, which failed.

So I will keep them as two discrete languages, and try and make them work together better, which will focus on them being able to share complex environments.

With M especially, lots of things could be added that would suit that level of language, but since my utilisation of it is limited, mainly language apps and support libraries, it just wouldn't be worth it. (Another reason it is not ready for general use.)

### Further Info on Tools

For more details of how these three main tools work, see [Tools](Tools.md).

### Further Info

For more details about these three languages:

**M** [Mfeatures.md](mfeatured.md]

**Q** Not Ready

**ASM** Not Ready

**Module Scheme** Not Ready

### M versus Q

The M documentation largely applies to both languages. I said they were converging, so what exactly *is* the difference?

* M, while it can be run from source, usually compilers into discrete binary files in EXE format. Q only runs from source, and executes bytecode

* M has a static type systeem, Q has a dynamic one

* M has only primitive types, with data structures based on fixed-size array, records and pointers; Q has higher level, flexible types, and things like Bignum arithmetic

* M includes inline assembly

* M can also be compiled into shared libraries; Q doesn't have such a feature

* M code can run a magnitude faster than Q bytecode
