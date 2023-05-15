### PCL Demo

Abandoned, self-contained project:

`mm.exe` compiles M programs to single `.pcl` file

`pci.exe` interpreters that `.pcl` file



```
    mm prog              # compile prog.m to prog.pci
    pci prog             # run via interpreter
    mm -run prog         # or compile and run immediately
    
    mm mm                # compile mm.ma (compiler) to mm.pcl
    pci mm -run fib      # interpret mm.pcl via pci, compile fib.m, run it via another pci
```
