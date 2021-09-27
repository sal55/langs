!Write simple 'Hello World' program as PCL-code

importpath "/mxp/"
import* pci_core
import* pc_win64

proc start=
    pcl_start()

    pcl_gent(kextproc, tpi32, pcl_genname("printf"))
        pcl_gent(kextparam, tpu64)
        pcl_gen(kextvariadics)
    pcl_gen(kendextproc)

    pcl_gen(kprocdef, pcl_genname("h.start"))
        pcl_setexported(1)
        pcl_gen(kprocentry)

        pcl_genxy(ksetargs, 1,0)
        pcl_gent(kpush, tpu64, pcl_genstring("Hello, World! [pcl2]\n"))
        pcl_gen(kcallproc, pcl_gennameaddr("printf"))

        pcl_gen(kpush, pcl_genint(0))
        pcl_gen(kstop)
    pcl_gen(kendproc)

    pcl_endprog(0,1)

!   pcl_writeclangfile("test.c")       # Generate C source file (needs pcl_endprog(0,0))
!   pcl_writepclfile("test.pcl")       # Write the generated in-memory PCL as a text
!   pcl_writeasmfile("test.asm")       # Use x64 backend and generate ASM source
    pcl_writeexefile("test.exe")       # Use x64 backend and write EXE file
!   pcl_writedllfile("test.dll")       # Write DLL file
!   system("test")                     # (For EXE file, try running it)
end
