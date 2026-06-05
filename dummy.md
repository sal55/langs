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
This software stack no longer exists

**CallBacks**
**Speed**
**Code Size**
