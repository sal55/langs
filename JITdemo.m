proc main=
    psymbol p, a, b, c
    tclopnd t1, t2

    p:=tc_makesymbol("add3", export_id)
    p.mode:=tpi64

    tc_addproc(p)
    tc_currfunc(p)

    a:=addparam("a")
    b:=addparam("b")
    c:=addparam("c")

    tcl_start()

        t1:=tc_gent(kadd, tc_genmem(a), tc_genmem(b))
        tc_setmode(tpi64)

        t2:=tc_gent(kadd, t1, tc_genmem(c))
        tc_setmode(tpi64)

        tc_gen(kretfn, t2)
        tc_setmode(tpi64)

    p.code:=tcl_end()

    tc_currfunc(nil)

    scanproctemps(p)                ! determine temp lifetimes
    reducetemps(p)                  ! renumber temps by re-using

!-------------------------------------

!   println tcl_writepst()          ! display symbol table
!   println tcl_writetcl()          ! display general IL code
!   println tcl_writeasm()          ! display generated native code
!   tcl_writeexe("demo.exe")        ! write EXE file

    tcl_exec(run:0)                 ! normally used to execute in-mem

    ref func(int,int,int)int fnptr := findsymbol("add3")

    if fnptr then
        println fnptr(10,20,30)
    fi
end

func addparam(ichar name, int mode=tpi64)psymbol=
    psymbol d

    d:=tc_makesymbol(name, param_id)
    d.mode:=mode
    d.used:=1
    tc_addparam(d)
    d
end

func findfunc(ichar name)ref proc =

    for i to nsymbols do
        if eqstring(name, symbolnametable[i]) then
            return symboladdress[i]
        fi
    od

    nil
end
