### 'PCL' Intermediate Language

This is the IL (or 'IR') I'm planning to use as the target of my lower level language compiler.

I've tried versions of it in the past, but it was usually internal to the compiler, and I had a hard time converting it native code. It added chaos rather than reducing it. I'm hoping this this new attempt will stick.

This is a work-in-progress and what I have now is as follows:

* 'MM7', a version of my 'M' compiler that targets PCL (using an internal API), which than gets written to a textual PCL source file.
* A standalone PCL interpreter, 'PCI' that reads the source code into an internal data structure (a sort of wide bytecode), and runs it.
* That PCI program runs all of my major M programs, except my Q interpreter where some some GUI libraries (involving callbacks from WinAPI) will not work.
* I also spent a few hours converting the backend of my C compiler to generate PCL. I got as far as translating programs programs such as fib.c, but then lost interest. I assume it will work, but C also presents a few challenges. I will use this test version to try out some constructs that are uncommon in my own language, such as value-returning augmented assignments.

The next stages are to complete the coverage of PCI's internal opcodes, and then to start the native code version, a project likely to be called `PCC`.

Links:

* [PCL Description](pcl_overview.md)
* [PCL Opcode Reference](pcl_opcodes.md)


### General Purpose Use

I had hoped this IL could by used by anyone, but it's clear that this not really practical. The language is not pure enough, and there are too many issues, espcially regarding the PCI interpreter.

It's fine however for my own projects and as a demonstration of what such a product can be like. For example, the standalone interpreter is under 100KB (ie. 0.1MB or 0.0001GB), and it can run all my compilers expressed as single file .pcl sources. (It can run itself; more on that below.) But otherwise, if were to be used as a C backend for example:

* A PCL file is expected to represent an entire program, which suits my whole-program compiler. But C for is usually compiled a module at time.
* PCL's execution core is based on 64 bits, but most C implementations use a 32-bit int type. (This can be emulated in PCL by truncating every intermediate op, but that's not ideal)
* C's `setjmp/longjmp` are not supported (that would need a compile of extra intrinsics)
* PCI has limitations with function pointers and callbacks passed via FFIs. Arbitrary C code will use those all the time. (This would not affect the PCC native code version.)

### The 'PCI' Interpreter

This project is an interpreter for a revised version of PCL. It was also my first attempt at interpreting static code, and I was curious as to how fast I could make it. In the end, performance was disappointing, even using specially fixed-up data structures (\*\*).

I decided not to bother with that part of it. I now don't even use special data structures: PCL code is read from .pcl files into a linear array of 32-byte records, with links into a symbol table. This is executed directly, with only minor fix-ups necessary for execution.

The priority is given to correctly running code, and providing fuller (and less buggy) coverage of my M language than my current compiler achieves. I also want my compiler to be less of a shambles.

(\*\* I know the performance is poor, since my Q interpreter for *dynamic bytecode* was faster! On certain benchmarks.
On the other hand, PCI is 2-3 magnitudes faster than Pico C, a C interpreter.)

### Problems with Interpreting Static Code

Apart from performance:

**Function pointers** Local ones are indices to bytecode; external ones are addresses of actual machine code on the other side of the FFI. Because the language on both sides could be the same, I can't isolate them. Callbacks from FFI functions will expect a function address pointing to native code; they won't get it!

At present it's manageable: local function pointers have a value distinguishable from real ones, and I can do the appropriate call. But this doesn't solve the callback problem. I think it is solveable, but it's probably not worthwhile for this project. PCI is more about improving the quality and coverage of the register-based code it will eventually produce; it doesn't have to run every program.

**Inline Assembly** This won't work in PCI (`assem` is a PCL instruction, but it is not supported by PCI). Fortunately my programs tend to have ASM in specific places that also have a HLL alternative. (It won't work in discrete PCC either, but it will work with PCC incorporated into the compiler.)

**The LIBFFI Problem** This is something I hadn't anticipated. Calling FFI functions from an interpreter involves synthesising function calls at runtime; I come across those in my Q bytecode compiler.

The general solution is `LIBFFI`, a complex cross-platform C library, which I would not be able to build, and which I don't want as a dependency anyway: my stuff is self-sufficient. I normally use a 50-line function which has some inline ASM. For used in PCI, there is a HLL workaround, but that has many limitations. Enough works however that I can use PCI to run PCI expressed as PCL.

**Command Line Parameters** This is a silly one, but if, instead of typing `prog a b c` to invoke program `prog` with 3 inputs, you instead run `pci prog.pcl a b c`, then all the inputs are pushed up by one.

In my own programs, I can allow for this (there is a special global which, if needed, is filled in by some external agency, eg. an emulator of this program's code), but if `prog` is someone else's program, and they directly call `__getmainargs` or `GetCommandLine` say, the inputs will be offset.

### Testing with PCI

I used my MM7 compiler (which only has the new PCL target) on these language projects (all others are smaller)

* AA (x64 assembler)
* CC (C subset compiler)
* MM6 (current M compiler, with no IL)
* QC/PC Q compiler and byte code interpreter
* MM7 (new M compiler)
* PCI (PCL interpreter)
* MS (MM6 configured to run programs from source and in-memory)

In each case, a single .pcl file was produced, for example:
````
    mm7 cc                   # build C compiler as cc.pcl
    pci cc sql               # Run cc.pcl via PCI, on input sql.c to produce sql.exe
    sql                      # run sql.exe
````
It just takes a magnitude longer than normal. (Instead of 0.5 seconds, it takes 6 seconds to build that 250Kloc program. But that is still 3 times as fast as gcc-O0!)

In the case of PC, an interpreter that executes dynamic bytecode, I was running it with an interpreter that was itself bytecode compiled! That one also ran, but I didn't attempt anything involving WinAPI libraries.

(To run GUI via PC requires requires callbacks to deal with WinAPI message processing; I make a special exception for `MainWndProc`, which resides in `pc.exe`, but where, `MainWndProc` is itself bytecode. I'd have to make a further exception for PCI, and then have code then reenters bytecode loops into two nested interpreters.)

The most fun was the following, running a Fibonacci test which prints `fib(1)` to `fib(36)` via increasingly nested numbers of PCI. With this test you always see a stream of output, no matter how slow, but it starts to slow down at difference points depending on performance:

````
    fib                   # Normally compiled fib.exe took 0.25 seconds
    pci fib               # fib.pcl took 3 seconds via pci
    pci pci fib           # Here, pci interprets pci.pcl, which runs fib.pcl; this took 75 seconds
    pci pci pci fib       # The last started slowing at N=31, this at N=23 (I didn't wait for it to finish.)
    pci pci pci pci fib   # This took 7 seconds to show N=1, then slowed at N=15
````

A bit silly but it's also an indication of healthy code.

### Aims and Benefits of this IL

* Cleaner, less buggy middle section of my M compiler
* Easier to provide coverage of M features (generating PCL is much easier than direct native code, also less messy and less buggy), and easier to provide more enhancements.
* A refined design suitable also for both interpreting and native code
* A cleaner backend to turn PCL into native code. Previous attempts were a disaster, even if they worked. I will probably make use the dispatching used within PCI.
* Provide a reference implementation, especially using PCI.

PCI already provides features than MM6 has, but doesn't implement, for example `(a,b) := c divrem d` (`divrem` is an operator returning two values). 

### The PCC Project

This turns PCL code into register-based x64 native code. I have done this before, but it got extremely messy. This revised PCL has one import difference: I used opcode names based on `load` and `store` instead of `push` and `pop`. This removes much of the confusion with x64's hardware stack, which in the new backend will be little, only for ABI conformance.

For PCC, the 'stack' used by PCL is not an actual, dynamic stack; it is an operand-stack managed by the compiler, and traversed in a linear fashion.

This introduces some issues, for example where code follows multiple paths to get one result:
````
    x := if c1 then A elsif c2 then B else C end
````
Here `A B C` represent arbitrarily complex expressions. With a true stack target, each of `A B C' will end up in the same place: the op of stack. With register based, each value could end up in a diferent register. Hence the hints that PCL needs (codes `startmx resetmx endmx`); ignored by PCI, here they are needed.

There are also ABI headaches. Ideally PCL would isolate you from those, but the stack format requires a lot of work to isolate each argument to call. This is necessary to provide the correct stack alighnment, and place args into the right places. While PCL doesn't need the details, it makes the backend's job much easier with hints. These are `setcall` and `setarg`, which tell it the span of each call, and marks each argument.

All this means PCL isn't a pure IL, and whoever/whatever writes needs to provide that extra info. But it is intended as a practical tool.

### Comparisons With Other ILs

Apart from using no IL at all (workable, but produces a poorly engineered compiler IMV), I've used a 3-address-code IL of my own, and I've also looked at LLVM IR. (Only looked, since the tools for that are fantastically complex. I can only do that since Clang can be used to turn C into LLVM .ll files.)

Here is my PCL code for a recursive `fib()` function (hints omitted; not needed for PCI):
````
    proc fib.fib
        rettype  i64   
        param    i64   .n 

        load     i64   .n 
        loadimm  i64   3 
        jumpge   i64   #3 
        loadimm  i64   1 
        jump           #2 
    #3:
        load     i64   .n 
        loadimm  i64   1 
        sub      i64   
        callf    i64   fib.fib 1 0
        load     i64   .n 
        loadimm  i64   2 
        sub      i64   
        callf    i64   fib.fib 1 0
        add      i64   
    #2:
        jump           #1         ; (I'll fix this at some point)
    #1:
        return         
    end
````
And this is LLVM IR for the C equivalent (except C used i32 not i64), from `clang -O0 -S -emit-llvm:

    define dso_local i32 @fib(i32 %0) #0 {
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i32, align 4
      %5 = alloca i32, align 4
      store i32 %0, i32* %3, align 4
      %6 = load i32, i32* %3, align 4
      %7 = icmp slt i32 %6, 3
      br i1 %7, label %8, label %9

    8:                                                ; preds = %1
      store i32 1, i32* %2, align 4
      br label %17

    9:                                                ; preds = %1
      %10 = load i32, i32* %3, align 4
      %11 = sub nsw i32 %10, 2
      %12 = call i32 @fib(i32 %11)
      %13 = load i32, i32* %3, align 4
      %14 = sub nsw i32 %13, 1
      %15 = call i32 @fib(i32 %14)
      %16 = add nsw i32 %12, %15
      store i32 %16, i32* %2, align 4
      br label %17
   
    17:                                               ; preds = %9, %8
      %18 = load i32, i32* %2, align 4
      ret i32 %18
    }

This doesn't need hints, because the syntax for a call is: `call T F(T x, T y)`; the info is implicit. The trouble is that that looks like a HLL, which was more or less the starting point in the original source, so little progress has been made.

I tried my own temp-based IL, the 3-address form I mentiomed, and that looks like this for the same program (that one has no textual format, this is the display format for debugging, but an actual syntax might not be far off this):

      1 Proc fib: 1,1
      2     param           n                            (i64)
      3     procentry
      4
      5     jumpcc          if n >= 3 then goto L3       i64(, i64, i64)
      6     move            T1 := 1                      (i64, i64)
      7     jump            goto L2                      ()
      8 L3:
      9     sub             T3 := sub(n,1)               (i64, i64, i64)
     10     callfn          T2 := &fib(T3)               (i64, u64 -> i64)
     11     sub             T5 := sub(n,2)               (i64, i64, i64)
     12     callfn          T4 := &fib(T5)               (i64, u64 -> i64)
     13     add             T1 := add(T2,T4)             (i64, i64, i64)
     14 L2:
     15 
     16 L1:
     17     retfn           T1                           (i64)
     18 End

So I can do it like LLVM too. This looks very enticing, but generating reasonable code from all those temps is a lot harder than it looks; I've tried several times. And interpreting such a language, with so many opcode combinations per instruction, would be a nightmare.

