PART I

Glossary:
````
Q        My dynamically typed and interpreted scripting language
QQ       (qq.exe) Q bytecode compiler and interpreter
M        My statically typed systems language
MM       (mm.exe) M compiler (generates x64 code for Windows
BB       (bb.exe) Q to M transpiler. This is described below
````

I have two languages: Q and M as listed above. I prefer to write apps using the former, but programs run more slowly due to Q being dynamica and interpreted. (I guess this is extremely common!)

Having mixed Q/M applications is unwieldy. So I'm looking at ways of speeding up Q significantly. I tried various approaches; none really worked, or they got too complex (eg. mixing bytecode and native code in the same product). I don't want to go down the JIT route which is even harder, outside my experience and skillset, and without guarantees of what can be achieved outside of benchmarks.

I decided to add optional type annotations to the Q languages, which has been done. Currenty that is parsed by QQ but is otherwise ignored.

There are two major stages that come next:

**(1)** Turning my Q code, designed to be interpreted via bytecode, into native code

**(2)** Making use of any type annotations to write efficient native code that can run perhaps a magnitude faster

I have just completel **(1)**, and that's what this post is about. The next stage (the harder one) is still to come, and the results will be described in Part II if and when it is completed. (I didn't put 'Part I' in the title in case it doesn't happen; titles can't be edited!)

**Transpilation** A native code backend even a normal compiler can get very hairy. I decided for my proof-of-concept to generate textual HLL. And here, I also wanted to try something new: using my own M systems language as a compiler target. I'd never done this before, and it has worked extremely well.

The QQ pipeline was something like this:
````
Source -> Parse -> AST1 -> Name resolve -> AST2 ->
  Codegen -> PCL (my bytecode) -> Fixup -> Run
````
I wanted to generate M code direct from AST2, but that wasn't practical, and not scalable to the more complicated things needed later. So I generated 'PCL' bytecode still, or a version of it since the needs are somewhat different, then converted a bytecode instruction at a time to M code.

**Type Annotations**
By themselves, these only add concrete type info to AST terminals such as Name or Constant. To be useful, they need to propagate upwards. This requires an additional pass, so the pipeline becomes:
````
Source -> Parse -> AST1 -> Name resolve -> AST2 ->
  Type analysis -> AST3 -> PCL Gen -> PCL (my bytecode) ->
  M Gen -> M source
````
The type analysis is primitive right now, and many things are temporarily suppressed, such as type conversion. So for now, most nodes have 'Variant' type which is my tagged dynamic type.

**Bytecode Changes**
The 'PCL' bytecode need to change quite a bit; for example:
* Each instruction type info (as stated, most will be 'Var' for variant)
* Rather than have a program-wide bytecode sequence, each function (and initialised data) has its own PCL sequence

**Control Flow**
Things like function calls, gotos, conditionals are implemented as M HLL features; they are not interpreted. Each Q function becomes an M function, with a decorated name to implement Q's modules and namespaces within M. Function signatures however

**The Interpreter Stack**
This software stack no longer exists. Function calls and local stack frames will use the usual hardware stack. A mini-stack does exist within each function (it might appear as `[4]varrec Stack` in a function), and is used to evaluate expressions. I still need the concept of 'pushing' and 'popping' to/from the stack since is where reference counting is managed.

This means some features that depended on the stack, such as exception-handling, can't be used. But that only existed in experimental form.

**CallBacks**
Callbacks are function references passed to external native code libraries, which can then call those functions within your. They won't work with bytecode; they need to native code functions. Well, Q functions are now native, but I still can't use them because currently all Q functions still have variant-based signatures. So the same workarounds (currently used for Q to work with Windows graphics) remain in place. But the mechanism needed for interpreter to be reentrant is no longer needed.

**Compiler Symbol Table** This had been accessible from Q programs, and function references, member lookups etc used ST entries. This is now longer available. It could have made available - various other tables are - but it would be too complicated. Alternate solutions are in place.

**Error Reporting** In the interpreter, it was easy to report error locations. That info does not exist in the M code. Instead, a global position variable it kept updated within the M code, but it optional to keep the code size down.

**FFI**
This is very well developed in Q, with support for the low-level types used already existing. Calls to FFI functions needed to use a 'LIBFFI' table-driven solution. Native code would allow them to be called directly, but the mechanisms for that are not yet in place, even the new type-annotations are not needed here. The table-driven method is therefore still used.
**Speed**
**Code Size**
