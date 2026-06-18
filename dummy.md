This is a follow-up to [this thread](https://www.reddit.com/r/Compilers/comments/1txw0cb/compiling_dynamic_code_to_native_pt_i/).

At that point I had a project that could translate programs in my dynamic, interpreted scripting language, into the source code of my static systems language.

Programs generally ran at about speed as the interpreter, or a little slower. The next stage, this Part II, was to make use of type annotations to help generate more efficient and more specific code.

This actually hasn't been completed, but I've got some results which are detailed below. There are were various things I wasn't happy about: the scripting language and its implementation really needs overhauling and simplifying. The idea of speeding up a program by adding random annotations is unsatisfactory, and the process that that involves is very clunky, even if ultimately the pipeline could be tightened up.

I've also gotten interested in making use of more type-inference and possibly looking at a more JIT-like approach, but that would require some design changes in the scripting language.

**Type Analysis** This had been a small extra pass on the AST, but this it turns this part is essential, and has to be done properly. There is actually lots of static type info present even in dynamic code without annotations (eg. a literal 1234 has 'int' type; the result of 'a < b' has type 'bool') and that has to be managed.

I had thought this could be switched in and out, but that's not possible; it has to be all or nothing; I can't choose to just ignore either implicit or explicit type information.

**Boxed and Unboxed Data** 'Boxing' means objects and values wrapped in a descriptor that provides a dynamic type tag. Unboxed is the raw data.

Annotated primitive types, such as ints and floats, exist as unboxed data as global, locals, and parameters. Interacting with boxed data (eg. passing an unboxed in to a function taking an untyped, boxed argument) requires conversion.

Annotated object types, such as strings and arrays, will stay boxed. One layer of boxing could have been removed (they don't need the dynamic type tag), but that was something was to be left until later.

**Integer-only Benchmarks** The first tests involved a handful of small benchmarks that only involved integers, and no arrays. So a program like this:
````
 a := b + c
````
would generate this static code if untyped that correspond to the byte code 'push b; push c; add; pop a' (only one declaration shown):
````
    varrec a
    k_init(&a)
    k_push(&$T1, &b)      # $T1 and $T2 refer to two 'stack' slots
    k_push(&$T2, &c)
    k_add(&$T1, &$T2)
    k_pop(&a, &$T1)
````
If I declare those variables using `int a, b, c`, then the same line becomes:
````
    int a
    a := 0
    a := (b + c)
````
With such annotations, I could speed-ups of 5-10 times over these benchmarks. With the one show below, because loop indices are autodeclared to 'int' anyway, I got a 16x speedup even without having to annotate the 'count' variable, since the increment is infrequent. (Interpreted: 10.6s, vs. 0.6s transpiled to native using implicit type info, vs. 0.5s optimised C.)

However what I found was that, with type annotations in place, these programs then become valid programs in my systems language - I could just compile them directly without transpiling! (The one below needs 'int count' added for that.) So it is lessens the achievement, especially as its compiler can also run them from source anyway.

**Benchmarks using Arrays** Setting up arrays is done differently between the two languages so here the transpilation is needed. I expected critical speedups to occur using lists and arrays, and also pointers.

'Lists' are heterogeneous arrays of variant types. Those cannot be optimised. I would first need to switch to 'Arrays', which are homogeneous arrays of the same unboxed type.

I didn't get as far this this because here is where I decided I need to step back and look at the bigger picture. But I did take a 'Sieve' benchmark, change it from using a List to an Array of bytes, took the static code generated and manually modified it to what have been generated when annotated. Timings were as follows (using N=100K, and the whole thing repeated 1300 times):
````
  Interpreted    10.6 seconds      (Both pure interpreter, and transpiled/compiled version)
  Transpiled      1.7 seconds      (Mocked-up static code version which knows a byte-array is used)
                  0.8 seconds      (When static code is further transpiled to C then using gcc -O2)
  Compiled        0.8 seconds      (Written directly in my static language)
                  0.5 seconds      (Written directly in C then using gcc -O2)
  CPython        27.6 seconds
  PyPy            1.3 seconds
  Lua 5.5         5.0 seconds      (Note 5.5 speed has improved significantly over 5.4)
  LuaJIT          0.7 seconds
````
So, it's promising. It might need a bit more work to get decent code using only my compiler's backend. But there would still be a big question as to how much difference it would make to a real application, and how much effort it would take to find all the bottlenecks and add the necessary annotations.

````
# Count Pythagorean triples up to N
    const n = 1000

    count := 0

    for a in 1 .. n do
        for b in a .. n do
            for c in b .. n*2 do
                if sqr(a) + sqr(b) = sqr(c) then
                    ++count
                end
            end
        end
    end

    println "Count=", count
````




