## 'M/Q' Languages - What's Missing

* Embarrassingly unsophisticated, they are devoid of most trendy modern features
* Primitive type systems, in particular no proper sum types
* No lambdas, closures, continuations, currying ...
* No support for threads, or parallel execution
* Few safety features, eg. most things are mutable
* No 'language servers' or other ways to get syntax highlighting
* No proper references or tutorials attractively presented (ie. something better than Github markdown)
* Compiler implementations are buggy and minimal
* No optimising passes and no static analysis
* No extensive error reporting
* No debuggers or other such tools
* No ecosystem
* Not tested on tens of million of lines of code by an army of users
* No community (I'm the only user, and even I don't use them that much!)
* Minimal, practically non-existent libraries, and no easy way of creating bindings to more
* Insular, incapable of working with other software except via DLL libraries - or files
* Don't work on anything except indows 64 (with experimental Linux versions)
* Patchy and buggy coverage of combinations of features, types and operations
* No Unicode support, except what works by accident through UTF8
* No formal grammar, which is not practical anyway as syntax has ambiguities
* No support possible (but binaries, or ways to create binaries are now available)

### Unpopular or Untypical Features

* Case-insensitive syntax
* 1-based indexing (this is the default; N-based possible in some cases)
* Line-based (at least outside of scripting languages)
* No block-scopes (only one identifier scope within each function)

Note that the requirements for a personal language with limited targets are very different than for a mainstream language; a *lot* of corners can be cut.

