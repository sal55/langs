>Are your languages available?

Not really; they're now personal projects. They also only run on Windows, and for x64. (There is an ARM64 port but it's a WIP.) Plus, they are self-hosted, so makes it hard for make available as source.

However, I can try this:

    c:\mx>mc -linux mm -o:mu
    Compiling mm.m to mu.c
    
    c:\mx>wsl
    root@DESKTOP-11:/mnt/c/mx# gcc mu.c -omu -lm -ldl -O3 -fno-strict-aliasing
    root@DESKTOP-11:/mnt/c/mx# ./mu -i hello
    Compiling hello.m to hello.(int)
    Hello, World!
    root@DESKTOP-11:/mnt/c/mx#

'mc' is a version of the compiler which transpiles the IL produced into attrocious low-level C code (even more low level than C normally is!). "-linux" ensures it uses the correct OS-specific module.

That can be built under Linux, but because it still generates Win64 ABI code, it won't fully work. But some options will work to some extent (use `./mu -h` to list all):

    -i    Interpret the IL produced
    -p    Write the IL as a textual file
    -a    Generate x64 assembly file (non-standard format)
    -obj  Generate PE-format object file

If you want to try it, see: [https://github.com/sal55/langs/blob/master/mu.c](https://github.com/sal55/langs/blob/master/mu.c)

(Note: 2.2MB file; click 'View raw' or download link.) Build as shown above, and run as shown above.

Hello World is this (use .m file extensions):

    proc main =
        println "Hello, World!"
    end

The example I posted (in Reddit thread) was:

    proc main =
        byte x := 65
        char y := 'A'
        println x, y
    end
