## (Compiler Testing)

This is a set of simple compiler tests I did last year:

  https://github.com/sal55/langs/blob/master/compilertest.txt

This is similar to your 1.2M 'hello world's but less challenging (yet
many languages/compilers have problems with it).

In that table, my products are 'bcc', 'MM' and 'QQ' (with 'QS' being
an abandoned product).

My programs mainly run on Windows. My static/native code compiler is here:

  https://github.com/sal55/langs/tree/master/Mosaic

With a link to a Windows binary. It's not practical to run under Linux at the moment.

The Hello World for mm.exe is this (test.m):

    proc start=
        println "Hello, World"
    end

Run that compiler on Windows as:

    mm test                     # compile test.m to test.exe

The test I do at my link would be this for mm.exe (test.m):
```
proc start =
    int a, b:=2, c:=3, d:=4
    
    a := b+c*d
    a := b+c*d
    .....         # repeat as much as you like
    
    println a
end
```

For something runnable on Linux, this is a one-file
C rendering of my dynamic/byte-code compiler/interpreter:

  https://raw.githubusercontent.com/sal55/langs/master/qq64.c

Build as follows (ignore outdated instructions in the file):

    gcc -O3 qq64.c -oqq -lm -ldl

The Hello World program is the same as above, but call it test.q. Compile and run that in Linux as:

    ./qq test.q                # compile test.q to test.pc bytecode and run

The a:=b+c*d test program is also identical, except change "int" to "var" (or just delete that line, and initialise b,c,d if it is to be run). The file should be called test.q.

Neither compiler will like 1.2M string literals (I need to look at that, but I can tell you that identical strings are not pooled).

If doing other tests, then mm.exe will have an expandable symbol table, but qq.exe has a fixed size (limited to 128K entries I think, so would not cope with 1.2M unique function names, which I believe was your other test for V).
