### Q Interpreter

qq.exe: Q bytecode compiler/interpreter for Win64

qc.c: C source version, build instructions are at the top, but basically `gcc qc.c -oqq.exe`. This is 100% HLL so doesn't have the accelerator mode of the provided qq.exe.

Run tests as follows:

    qq hello
    qq show file.jpg                    # Load and display JPEG image
    qq basic file.bas                   # Run simple Basic program

Code can be written outside of a function, but recommended to put it inside a function like this:
````
    proc main =
        println "Hello"
    end
````

due to scoping rules making inadvertent shadowing very likely when functions are present.
