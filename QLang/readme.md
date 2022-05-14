## Q Language 2022

'M' and 'Q' are my static and dynamic, compiled and interpreted languages.

I wanted to revise Q so as to embed an 'M lite` version of the static language. This was to make it far more convenient to write applications which mix code from both languages. For example, so they can have a shared environment more easily.

I started embedding a separate M-lite compiler into Q, but ran into trouble. It was just too much work, and too unwieldy. Too many corners would have to be cut too.

It also become clear that the M backend, the bit I was trying to incorporate, just wasn't right. I decided that the use of a stack-based VM for the IL, while very easy to generate and inspired by the VM of Q's bytecode, was less suitable for a native code compiler.

### Starting Again

Well, not quite. The Q compiler can still parse embedded M code, it just can't do much with it (interpret it, possibly; since this is statically typed, it wil be faster than normal bytecode, but not spectacularly)

This project is not just about getting a working product, it's also about getting it done in a way I'm happy with. So:

* I've decided to replace M's stack-based IL, with one based on '3-address-code' I prefered this, but certain practical problems like getting decent code put me off. I think it will be better to fix those problems.
* The new IL is called `TCL`, and has its own advantages. One is that it makes a C target much simpler should I ever want to do that.
* I still have the aim of using a hybrid language which is primarily Q, and secondarily M. But not, for the first version, by creating a third, less-capable M compiler which is part of the Q interpreter.

There is already a mechanism I can mix Q and M in an application: I duplicate any shared global entities (enums etc) in an M program. I write M functions in M modules. I compile that to a shared library. And create (possibly automatically) an interface module providing the necessary FFI.

But that is messy. I want to put everything into Q source files, and just run the Q as a normal script.

There are crude ways of doing this: let Q identify the static elements of a program, write them out in an M module, invoke the M compiler and tell it to write an ML shared library, which is loaded dynamically. Rather messy (especially chopping the source code up, and having to re-parse in the M compiler), but it can still be done transpatently, and done fast.

There are also was to invoke the M compiler itself as a shared library, and tell it to put the compiled code direcly into memory accessible from Q.

If the hybrid language works, then I can look at a more integrated solution.
