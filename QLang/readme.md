## Q Language 2022

'M' and 'Q' are my static and dynamic, compiled and interpreted languages.

I wanted to revise Q so as to embedded an 'M lite` version of the static language. This was to make it far more convenience to write applications which mix code from both languages. (No need to write separate M modules and compile those to libraries accessed from an FFI. And the global entities can be more easily shared.)

I started embedding a separate M-lite compiler into Q, but ran into trouble. It was just too much work, and too unwieldy. Too many corners would have to be cut too.

Then it also become clear that the M backend, the bit I was trying to incorporate, was still incredibly messy. I decided that the use of a stack-based VM for the IL, while very easy to generate and inspired the the VM of Q's bytecode, was probably unsuitable for a native code compiler.

### The TCL Project

This revises the M compiler with a new middle-end, ie. a new IL based on Three-Address Code (TCL). I'd abandoned my last attempt at it because of some problems: it was slower than alternatives (in compilation speed); it made use of huge numbers of temps (4 million in one function on one of my test inputs); and the generated code was poor.

But I think it will be easier to try and fix these, then stay with the stack-based IL. And there are advantages:

* It doesn't have the restrictions of the stack structure
* The generated code actually doesn't use a stack (only for function calls with 5 or more arguments)
* It would be very easy to transpile to C is necessary

### Compiling the Hybrid Language

That is, Q source with embedded M functions. There are cruder ways of achieving this:

* Make the M compiler able to compile Q source code: it will ignore Q functions and dynamic variables, and process only static data
* The M compiler can then make an ML file (dynamic library), complete with FFI declaration block needed
* The composite source is then processed with the Q compiler, which will do the fixups the embedded M functions
* Possibly, the Q compiler could invoke the M compiler, to produce the needed ML file

The above involves generating a separate disk file. Another approach:
* Make the M compiler, or a special version of it, into an ML library
* Q can then load the library, and tell the M compiler to produce in-memory native code

What's important, is that there still just two compilers to maintain, and not a third, inferior one.

