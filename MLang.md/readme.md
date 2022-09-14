## My Languages 2022, Overview

I use two complementary languages, plus a support one:

Language | Tool | Target | Description | Nearest Mainstream Equivalent (% match)
--- | --- | --- | --- | ---
M | mm.exe (compiler) | Win64 | Lower level systems language | C (80%)
Q | qq.exe (interpreter) | Win64 | Dynamic scripting language | Python (40%)
ASM | aa.exe (assembler) | Win64 | x64 assembler/linker | Nasm (90%)

Those three tools in more detail:

Tool | Size |Installation | Max throughput | Written in | Sources | Build time | Dependencies
--- | --- | --- | --- | --- | --- | --- | ---
mm.exe | 500KB | 1 File | 0.7Mlps |M |  31Kloc | 80ms | None
qq.exe | 800KB | 1 File | 1.5Mlps | M |  41Kloc | 100ms | None
aa.exe | 160KB | 1 File | 2Mlps+ | M | 13Kloc | 50ms | None

Basically, they are smallish, self-contained, very fast and can be built from scratch more or less instantly. M is self-hosted, the others are implemented in M.

M itself is embarrassingly dated and unsophisticated compared with the current crop of 'systems' languages, yet it works at roughly the same level as C, which is still popular and still seems to be in demand.

My product is better, in my opinion. The main differences from C are listed below.

### M vs C

Although the languages largely do the same things, mine varies in significant ways:

* Algol-style syntax without braces and without begin-end either (which are as bad as braces), and largely semicolon free
* Case-insensitive syntax
* 1-based indexing with option to use 0-based or N-based
* Module scheme
* Out-of-order definitions for everything
* Tidy, consistent set of types with 64-bit ints and constants by default
* Built-in `print` and `read` statements
* Slices
* Embedded text and binary files (only just being introduced into C23)
* Built-in 'tabledata' (A superior approach to 'X-macros')
* Very fast, single-file and self-contained whole-program compiler - nothing else is needed
* Can create compilable one-file source amalgamations of projects
* Does not need a build system like `make` (submit only the lead module, it will discover program structure automatically)

Plus lots more.

### The Context

While I'm pleased that I do a somewhat better job than C (anyone could I think!), I'm aware that what I'm doing is just a microcosm of what languages are about these days. My products:

* Lack most trendy modern features and advanced type systems
* Are bare, unoptimising compilers that do no analysis
* Only work on Windows64, and can only generate self-contained EXEs for that OS
* Don't have tons of ready-to-use libraries, and creating new bindings is a chore
* Have not been tested by 1000s of people on tens of millions of lines of code across diverse applications
* There are no debugging tools, no language servers, no ready-made syntax highlighting
* There are no tutorials or reference materials that are attractively presented (that means something Github markdown)
* Having been used with no other software, only simple file operating systems, for the first 10-15 years, interaction with other software has always been poorly developed
* The result is an insular suite of languages that can only communicate with the world via DLL libraries, and only via the simplest APIs
* Some aspects have not been touched at all, such as threading, parallel execution, networking

In short, they are small, personal limited languages, with implementations that are not complete nor robust enough for general use. I have in the past provided binaries, but I'm not happy about doing that, and doing the support necessary is not practical.

So, why am I posting about this stuff? Since some have complained about this before. It's just for interest. Maybe there is something of value that people can take away from this. 


### History

M started off in the early 80s, and Q came along towards the late 80s. You might have expected them to have evolved a lot more than they have!

But they were just tools I devised, part-time, to help in my main work, and worked well enough.

(Actually, they *have* evolved and become more polished, but along lines that I considered more useful. The languages have been kept simple. I've written a more detailed summary of my various native code compilers [here](../mycompilers.md).) 

### Future Development

There's quite a lot that could done to M, that would be in keeping with that level of language, but as my personal use of it is limited, most would not be worthwile. As it is, it can still do pretty much anything that C can do.

I have made several attempts to combine the M and Q lanuguages, but the attempts were over-ambitious, and the results poor and unwieldy.

The next project is to make them work together better, while still keeping them separate languages with discrete implementations.

As such, I'm trying to get stable, part-documented versions of both projects first, since I have tried all sorts of ideas and can't remember which have stuck, or whether they're implemented in one or the other, or both. Hence articles like these, whose purpose is partly to get me to test the tools to see what actually works, or work as I thought they did.

### Further Info on Tools

For more details of how these three main tools work, see [Tools](Tools.md).

### Further Info

For more details about these three languages ... Not ready.
