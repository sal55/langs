**Stack-based or Non-stack-based.**

For a few years I've been using a stack-based intermediate language (which I called 'PCL') for my systems language compiler. I chose it because I'd long used a similar language for the bytecode of my interpreter using dynamic typing.

It is very easy to generate and looks extremely clean. But I've recently had doubts about its suitability for a typed systems language:

* It can be awkward to deal with because of the stack structure
* Its stack interferes with the hardware stack of the target
* It takes some 6500 lines, excluding tables, to get from 'PCL' to the next stage which is 'MCL' (a representation of x64 native code).

So I'm working on an alternative, a form sometimes called 'Three-address Code', and which I call 'TCL'. I've tried this a few times in the past, but could not manage to get decent code out of it without a lot of work.

The last attempt was in 2000, which was going reasonably well, just a bit slower to compile code with, until I tried it on one of my simplest stress-testing benchmarks, which is a function containing up to 2 million lines of `a := b+c*d`. Then I discovered that it needed 4 million temporaries. At which point I decided to abandon it.

This latest attempt is not complete (I've still to do the TCL to MCL translator, but confident I can knock 2000 lines off that 6500, and still have reasonable code). It does however perform better on that benchmark; instead of 4 million temporaries, it only uses 2 (that is just two, not 2 million!).

This kind of IL is more typical among such backends, and actually is not dissimilar to LLVM IL.

Here are comparisons between PCL (my current IL), TCL (the new one), and LLVM, for this program implementing that benchmark:
````
    My language             C version submitted to Clang

    int a,b:=2,c:=4,d:=4    int a,b=2,c=3,d=4;   # Note my ints are 64 bits
    
    a := b+c*d              a = b+c*d;
    ....                    ....                 # 1999998 lines not shown
    a := b+c*d              a = b+c*d;
````
'PCL' stack-based IL (all show only declarations, and code for the final `a:=b+c*d` line):
````
    local          a          i64 
    local          b          i64 
    local          c          i64 
    local          d          i64 
    ....
    push           b          i64 
    push           c          i64 
    push           d          i64 
    mul                       i64 
    add                       i64 
    pop            a          i64 
````

'TCL' temp-based IL:
````
    local    a                 (i64) 
    local    b                 (i64) 
    local    c                 (i64) 
    local    d                 (i64) 
    temp     T1                8 
    temp     T2                8 
    ....
    mul      T1:=mul(c,d)     (i64,i64,i64)
    add      T2:=add(b,T1)    (i64,i64,i64)
    move     a:=T2            (i64,i64)

````

LLVM as produced by Clang -S -emit-llvm:
````
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  ....
  %10000000 = load i32, i32* %2, align 4
  %10000001 = load i32, i32* %3, align 4
  %10000002 = load i32, i32* %4, align 4
  %10000003 = mul nsw i32 %10000001, %10000002
  %10000004 = add nsw i32 %10000000, %10000003
  store i32 %10000004, i32* %1, align 4
````

* Both PCL/MPL examples are display formats only. They are not input formats. (PCL *had* been an actual input syntax too, but no longer. The LLVM example can be used as textual input for subsequence processing)
* It seems that the LLVM has the same problem I had, but worse: 10 million instead of 4 million temporaries
* I got around the proliferation of temps by reusing them (which makes some subsequent processing trickier, but I believe I can get around that)
* (The 6500 lines I quoted above to get from PCL to MCL is partly because the source language has a rich syntax, which needs support within the IL, and lots of operations are built-in to the core language, rather than in libraries in other languages)
