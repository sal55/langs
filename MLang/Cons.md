## 'M' and 'Q' Languages

* Old-fashioned (both devised in 1980s, and little changed)
* Embarrassingly unsophisticated, they are devoid of most trendy modern features
* Primitive type systems, in particular no proper sum types
* No lambdas, closures, continuations, currying ...
* Do not work with threads, no support for parallel execution
* Unsafe (eg. everything is mutable)
* No 'language servers' or other ways to get syntax highlighting
* No proper references or tutorials attractively presented (ie. something better than Github markdown)
* Minimal and buggy compiler implementations
* No optimising compilers and no static analysis
* Poor error reporting
* No debuggers or other such tools
* No ecosystem
* Not tested on tens of million of lines of code by an army of users
* No community (I'm the only user, and even I don't use them that much!)
* Minimal, practically non-existent libraries, and no easy way of creating bindings to more
* Insular, incapable of working with other software except via DLL libraries - or files
* Work only on Windows 64, not on Linux, and can never work on anything like Android
* Personal languages with patchy and buggy coverage of combinations of features, types and operations
* No Unicode support, except what works by accident through UTF8
* No formal grammar, which is not practical anyway as syntax has ambiguities
* No binaries available to use, which cannot be supported anyway (however, downloads are now possible)

Basically, by any comparison to anything else, they are rubbish. So, for anyone still reading, why am I writing about them and posting links?

I'm primarily documenting what I've done for my own benefit, since there are various odd features where I can never remember how they work and in what language, and would like to finally have a stable set of languages as I've spent too long tinkering.

This might be of interest to other people, or there might be things that someone can take away, even if it's to help avoid making the same mistakes.

While short on advanced types, both the HLLs have a rich syntax with dozens of useful minor features including some experimental ones.

**So, Are There Any Upsides?**

From my perspective, yes:

* One of my languages will always be my first choice in any project (and has been since 1981, although the choice then was *very* limited)
* I understand them extremely well and can usually get them to do whatever I want, and in the syntax I prefer
* The tools (compiler, interpreter, assembler) are small, self-contained, very fast and to the point
* There are satisfyingly few external dependencies, they self-contained, and I wrote everything myself.

The requirements for a personal language with a limited target are very different than for a mainstream language. A *lot* of corners can be cut!

