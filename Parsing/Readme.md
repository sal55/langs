#### Lexing and Parsing Speed

I'm not sure what's going on with your code versus my code. Maybe your machine is just slower? Mine is a low-end PC (AMD Ryzen 2650U 2.6Ghz, if that means anything to you).

To get to the bottom of it, the tests have to be simpler. So use single-core and one thread only (you can reinstate multi-core after you have a decent speed with one core).

The 24-file test is fiddly. One test I use is a file containing from 20K to 2M lines of `a := b + c * d` (see [Compilertest1](../Compilertest1.md)). However you may run into trouble with too much code in one function; lots of compilers too. Basically, just keep it in one file.

First test is for lexing, although it sounds like you already have that under control. Nevertheless, above should be clex.c, a benchmark I used to use. It's a simple, incomplete lexer for C-like syntax, and this file has been transpiled from my language (which uses Windows) to work under Linux. Build as follows:

    gcc clex.c -oclex -lm -ldl -fno-builtin -O3

Run it like this:

    ./clex

It processes sqlite3.c (another file above, or change line 1071 in clex.c to use your own input). The file is scanned 10 times and the results shown. (Note these may be 1000 times bigger or smaller than intended if your Linux' clock() uses 1000000 ticks per second instead of 1000, but that should be obvious!)

On my WSL Linux, that reports 9.5M lines per second for basic tokening (no symbol table lookups).

(The same tool that produced clex.c from my language, could translate my compiler to run on Linux (where it can do the frontend but not backend), but that is in its early stages, so it would need a lot more testing.)

### The AST

It looks like this might be the problem area. First create a suitable input (say, 100,000 or 500,000 lines of `a=b+c*d`, in multiple functions if that helps, whatever gives a convenient timing and that doesn't put pressure on machine memory.).

With my compiler, 100K lines takes 22ms to parse, and 500K lines about 160ms (unoptimised compiler).

If your programs take substantially more (factoring in the difference between my machine of yours; this was another purpose of the clex test), then this is what needs attention.

How many AST nodes are needed for each line? I use 6 (for 3M for 500Klines). How many bytes for each (I use 64 for the main compiler, but the simpler bytecode compiler uses 32 I think).

How long does a program take which does nothing but allocate that number of AST nodes?

My compiler takes 62ms to allocate 3M notes cleared to zero (that bit is important). (I notice I use a block allocator for ASTs: I allocate 32K of them at a time in a single block, which are cleared to zero in one lot too. However the advantage is not great, maybe 15% faster.)

Basically, it's narrowing done what is the bottleneck. If the above tests are OK, then look at the parsing code itself; are there inefficiences there like using long chains of if-else statements, or is there too much recursion.

(I'd post one of my parsers, but they tend to be 4000 lines or so, so probably not too enlightening. I don't really understand myself.)




    
 
