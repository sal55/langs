### PCL Intermediate Language

Characteristics:

* Low-level intermediate language for my systems language compiler
* Stack-based virtual machine
* Primitive types of `i8-i64 u8-u64 r32-r64` and N-byte blocks (no array, struct or pointer types)
* 64-bit execution model and 64-bit stack
* A PCL program (whether as one .pcl file, or created via an API) always represents a whole program

'PCL' is actually the name of the bytecode language of my dynamic language; I adapted it for compilers because it was so easy to write code for.

This version uses static type annotations for each instruction, and the types are much more primitive. It also uses instructions based on `load` and `store` concepts rather than `push` and `pop`, here to avoid confusion with x64's hardware `push` and `pop`.

#### PCL Hello World

Here is the simplest program in PCL:

    proc main
        loadimm "Hello, World!\n"
        printstr
        return
    end

Although this is a bit of a cheat since `printstr` is a debugging opcode. A better one, as might be generated from a HLL, is:

    proc main
        loadimm "Hello, World!\n"
        callp puts 1
    end

    extproc puts
        extparam u64
    extend

This used C's `puts` from the C library in Windows' `msvcrt.dll`, which is automatically linked. If it wasn't then the top of the program would have needed:

    linkdll msvcrt
 
 #### Generating PCL
 
 This can be done textually, without using a PCI: just write text to a file. My MM7 compilers however does use an API to generate a PCL representation. In this early form, that is then dumped to a file in PCL dump format.
 
 A production compiler wouldn't have a textual intermediate form; whatever backend turned internal PCL into native cpde, that would be incorporated into the compiler.
 
