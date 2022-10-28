#### Lexing and Parsing Speed

I'm not sure what's going on with your code versus my code. Maybe your machine is just slower? Mine is a low-end PC (AMD Ryzen 2650U 2.6Ghz, if that means anything to you)).

To get to the bottom of it, the tests have to be simpler. So use single-core and one thread only (you can reinstate multi-core after you have a decent speed with one core).

The 24-file test is fiddly. One test I use is a file containing from 20K to 2M lines of `a := b + c * d` (see [Compilertest1](Compilertest1.md)). However you may run into trouble with too much code in one function; lots of compilers too. Basically, just keep it in one file.

