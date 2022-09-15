## My Languages 2022, Overview

I maintain and use these two complementary languages:

**'M'** Lower level systems language

**'Q'** Dynamic, interpreted scripting language

Plus:

**ASM** My take on x64 assembly code.

### Features

... that people of expect of a language and implementation these days:

*Sum types, pattern-matching, lambdas, closures, OOP, inheritance, type inference, exotic type systems, syntax-highlighting, language servers, static analysers, optimising compilers, debugging tools, tutorials and reference materials attractively presented, testing performed tens of millions of lines of code by armies of beta testers, threading, parallel execution, networking support, IDE support, robust implementations, great error reporting, available on multiple targets...*

Unfortunately my meagre suite of languages support none of that. They are embarrassingly crude compared with any modern languages, they have minimal implementations, and next to no libraries, with no easy path to creating bindings to more. They are insular and cannot talk to any other software except via DLL functions. They can't even link to other languages without a lot of trouble.

They also only work on Windows and not Linux. There are no proper docs. Coverage of combinations of types, operations and features is patchy, and very buggy.

Neither are my implementations good enough for general use, so I cannot provide binaries, which I could not support anyway.

So, having got that out of the way, and for anyone still reading, I'll continue.

### Background

M started off in the early 80s, and Q came along towards the late 80s. You might have expected them to have evolved a lot more than they have!

But they were just tools I devised, part-time, to help in my main work, and they worked well enough (enough that when the opportunity arose to switch to C, I decided to stay with M).

Actually, they *have* evolved and become more polished, but along lines that I considered more useful. The languages have been kept simple. I've written a more detailed summary of my various native code compilers [here](../mycompilers.md).

Currently, the nearest mainstream alternative to M is C, with about an 80% match (in things like type systems, and what the languages can do).

With Q, perhaps Python at 30-40% match. (Python is at least one level higher, and *much* more dynamic; it also has most of the stuff in my list. OK, make that 10% match then!)

My ASM language is probably a 90% match with Nasm (it's just 1000 times faster at assembling; literally so on some tests).

### Future Development

I've decided to stop individual development of both languages for a while. Document what there is, make sure both languages work the same way for common features, just create stable working versions.

Several previous attempts to combine M and Q (since they seemed to be converging over the years) have failed. I will instead try and make them work together better, which will focus on them being able to share complex environments.

With M especially, lots of things could be added that would suit that level of language, but since my utilisation of it is limited, mainly language apps and support libraries, it just wouldn't be worth it. (Another reason it is not ready for general use.)

### Further Info on Tools

For more details of how these three main tools work, see [Tools](Tools.md).

### Further Info

For more details about these three languages ... Not ready.
