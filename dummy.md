**Glossary**
````
Q        My dynamic and interpreted scripting language
QQ.exe   Q bytecode compiler and interpreter
M        My statically typed systems language
MM.exe   M compiler (generates x64 code for Windows
BB.exe   Q to M transpiler, the project described here.
````
I like to write apps 100% in my Q scripting language it is too slow for that. I'm looking at ways of making it faster.

Various approaches have been tried but they all got unwieldly. I don't want to JIT, as it is much harder, beyond my capabilities, and no guarantees of what can be achieved beyond benchmarks.

I decided to add optional type annotations to the Q language, which has been done. Currenty that is parsed by QQ but is otherwise ignored. There are two major stages that follow:

**(I)** Turning my Q code, normally run as interpreted bytecode, into native code

**(II)** Making use of any type annotations to write more efficient native code that can run perhaps a magnitude faster

I have just completed **(I)**, and that's what this post is about. The next stage is still to come, and the results will be described in Part II if and when it is completed.

**Transpilation** A native code backend for even a normal compiler can get very hairy. I decided for my proof-of-concept to generate textual HLL. And here, I also wanted to try something new: using my own M systems language as a compiler target. I'd never done this before, and it has worked extremely well.

The QQ pipeline was something like this:
````
Source -> Parse -> AST1 -> Name resolve -> AST2 ->
  Codegen -> PCL (my bytecode) -> Fixup -> Run
````
I wanted to generate M code direct from AST2, but that wasn't practical, and not scalable to the later needs. So I generate 'PCL' bytecode still, or a version of it, then convert a bytecode instruction at a time to M code.

**Type Annotations**
By themselves, these would only add concrete type info to AST terminals. To be useful, they need to propagate upwards. This requires an additional pass, so the pipeline, with the M generation added, becomes:
````
Source -> Parse -> AST1 -> Name resolve -> AST2 ->
  Type analysis -> AST3 -> PCL Gen -> PCL (my bytecode) ->
  M Gen -> M source
````
The type analysis is primitive right now, and many things are temporarily suppressed, such as type conversion. So for now, most nodes have 'Var' type which is my 'Variant' tagged dynamic type.

**Bytecode Changes**
The 'PCL' bytecode need to change quite a bit; for example:
* Each instruction has type info (as stated, most will be 'Var' for variant)
* Rather than have one program-wide bytecode sequence, each function (and initialised data item) has its own PCL sequence
* (Internally, a linked list is also used to chain instructions. Interpretation needs them in one contiguous array.)

**Control Flow**
Things like function calls, gotos, conditionals are implemented as M HLL features; they are not interpreted. Each Q function becomes an M function, with a decorated name to implement Q's modules and namespaces within M. Function signatures however will be Variant-based until Part II.

**The Interpreter Stack**
This global software stack no longer exists. Function calls and local stack frames will use the usual hardware stack. A mini-stack does exist within each function (see example below), and is used to evaluate expressions. I still need the concept of 'pushing' and 'popping' to/from the stack since this is where reference counting is managed.

This means some features that depended on the stack, such as exception-handling, can't be used. But that was only experimental.

**CallBacks**
Callbacks are function references passed to external native code libraries. They won't work with bytecode; they need to be native code functions. Well, Q functions are now native, but I still can't use them because currently all Q functions still have variant-based signatures. So the same workarounds (currently used for Q to work with Windows graphics) remain in place. But the mechanisms needed for the interpreter to be reentrant are no longer needed.

**Compiler Symbol Table** This had been accessible from Q programs, and function references, member lookups etc used ST entries. This is no longer available. It could have been - various other tables are - but it would be too complicated. Alternate solutions are in place.

**Error Reporting** In the interpreter, it was easy to report error locations. That info does not exist in the M code. Instead, a global position variable it kept updated within the M code, but it is optional to keep the code size down when not debugging.

**FFI**
This is very well developed in Q, with support for the low-level types used already existing. Calls to FFI functions needed to use a 'LIBFFI' table-driven solution. Native code would allow them to be called directly, but the mechanisms for that are not yet in place, even though the new type-annotations are not needed here. The table-driven method is therefore still used.

**Code Size** The generated M code is sprawling, and then generated EXE files are quite large: perhaps 60 bytes per line of Q code, more if inlining is used. It is more typically 10 bytes per line of M code, but this generated M is also dense: each line is a function call. Still, the size is not signicantly higher than the bytecode size would be in-memory.

**Execution Speed** I've done experiments along these lines in the past, and already knew the code was not going to be magically fast just because it was 'native code'. On the whole it is roughly the same speed as interpreted Q code. This set of results is from running the Fannkuch(10) benchmark. It is compared to some other products too:
````
          Seconds
QQ -no     5.2       Regular bytecode
QQ         4.1       Uses extended bytecode
BB         5.2       Generated from regular bytecode
BB         4.3       Uses inlining for some handler functions

Python    13.6       CPython 3.14
Lua        6.9       Lua 5.5
Lua        0.8       LuaJIT
Python     0.6       PyPy
M          0.21      mm.exe
C          0.17      gcc -O3
````
(Note: all timings involving QQ/BB/M use the MM compiler which generates unoptimised code. MM builds QQ, BB, itself, and the BB-generated program.)

So, BB gives the same 5.2s timing as QQ via the regular bytecode. But QQ bytecode is normally optimised to use an extended set of instructions which do common short sequences. Many are speculative, aborting early if the fast path is not viable, and cannot be translated easily to native.

I'm hoping that the next stage will make things significantly faster, such as 5-10 times for such benchmarks, and should be on a par with those JIT products. The difference is I will need type annotations, which are not always practical: sometimes generic code is needed.

**Compilation Speed** QQ's compiler works at 1.5Mlps, and M's at 0.5Mlps. So having to do type analysis, writing M source files then compiling dense, long-winded M sources, will be much slower, eg. 0.2Mlps. Doesn't sound too bad, but the line-count may be 4-5 times bigger.

While it is expected that QQ is used for development, and BB for one-off buillds, if this product works, the native code generation can be taken directly inside BB. It could even run direct from source (QQ runs from source and MM can be made to). Then 'BB' becomes a drop-in replacement for QQ, that runs programs a magnitude faster.

**Examples** To keep things short, the example is very simple:
`````
# Q code (I've added the type annotations as they will appear but currently not used)
fun add(int x, y)int = x + y

# Bytecode from BB:
Proc add
     pushm  x         var
     pushm  y         var
     add              var
     setret           var
End

# M code that BB generates from that (t_ is the Q module name):
proc t_add*(variant $Result, variant x, variant y) =
    k_push(&$T1, x)        # $T1 is an alias for Stack[1]
    k_push(&$T2, y)
    k_add(&$T1, &$T2)

    k_move($Result, &$T1)
    k_unshare(x)
    k_unshare(y)
    [2]varrec Stack        # declared at end as size not known until here
end

# varrec is a 16-byte (tag, value/pointer) descriptor
# variant is a reference to varrec
# The '*' is an M feature that puts the function into an internal table
#   for access by apps, in this base the Q language support.
# M language allows out-of-order declarations.
````
This is the full generated code for the Fannkuch example. This was compiled without a standard library (not needed for text apps) as others that 10Kloc of Q code would have added to 30Kloc to the M file:

https://github.com/sal55/langs/blob/master/fann.m

