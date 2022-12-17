## M Compiler on Linux Demo

* Download files mc.c, mc.ma, hello.m, extract.m to the same directory in Linux
  (must be 64-bit Linux and default 64-bit gcc)

* An M compiler is needed first. Create this from the C file as follows:
````
    gcc mc.c -omc -lm -ldl -fno-builtin
````
* Test it on hello.m:
````
    ./mc hello
    ./hello
````
* Now try building the M compiler from the true sources in mc.ma:
````
    ./mc mc -out:mc2
````
* Try the new compiler:
````
    ./mc2 mc hello
````
* Need the true sources? Try:
````
    ./mc extract              # extraction program
    md fred                   # where the modules will go
    ./extract                 # should show files extracted
    ls fred                   # test they are there
````
* Try building from those discrete files:
````
    cd fred
    ../mc mc
    ./mc ../hello
````

Notes:

* The M compiler on Linux relies on intermediate C. So run-from-source
  (-run option) is not available

* For faster throughput, use:
````
    ./mc -tcc prog
````
This needs Tiny C to be installed
* For optimised code (via gcc-O3 only) use:
````
    ./mc -opt prog
````
* The `mc.ma` file has been detabbed (converted to 4-space tabs) and decommented.
