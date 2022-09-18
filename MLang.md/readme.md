## M/Q Languages

These are my private languages:

**M** My systems programming language

**Q** My dynamic scripting language

**AA** My take on x64 assembly

The nearest mainstream equivalent to M is probably C, at an 80% match (in their type systems and what they do)

The nearest to Q might be Python at 30% (but Python is least one level higher, is much more dynamic, and doesn't have the downsides I've listed)

AA is 90% equivalent to Nasm

### Description

* Old-fashioned (both devised in 1980s, and little changed)
* Devoid of most trendy modern features (actually, they are embarrassingly unsophisticated for 2022)
* Primitive type systems
* No lambdas, closures, continuations, currying ...
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
* No Unicode support
* No formal grammar, which is not practical anyway as syntax has ambiguities
* No binaries available to use, which cannot be supported anyway

Basically, by any comparison to anything else, they are rubbish. So, for anyone still reading, why am I posting this?

I'm primarily documenting what I've done for my own benefit, since I can never remember what features work and in what language, and would like to finally have a stable set of languages as I've spent too long tinkering.

This might be of interest to other people, or there might be things that someone can take away, even it's to help making the same mistakes.




