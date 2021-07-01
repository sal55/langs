global function os_calldllfunction(ref proc fnaddr, int retcode, nargs, ref[]i64 args)word64 =
    word64 a
    real64 x
    int oddstack, nextra, pushedbytes

    oddstack:=nextra:=0

    assem
        test Astack,8
        jz L1
        mov byte [oddstack],1
L1:
    end

    if oddstack then
        if nargs<5 then
            nextra:=5-nargs
        elsif nargs.even then
            nextra:=1
        fi

    else
        if nargs<4 then
            nextra:=4-nargs
        elsif nargs.odd then
            nextra:=1
        fi
    fi

    pushedbytes:=(nextra+nargs)*8

    to nextra do
        asm push 0
    od

    for i:=nargs downto 1 do
        a:=args[i]                  !get generic 64-bit value to push
        asm push word64 [a]
    od

!load first 4 args to registers; this first version will blindly load 4 args
!(even if there are less) to both integer and xmm registers. Should be int/pointer
!types to integer regs; float types to xmm; and variadic to both
    assem
        mov D10,[Dstack]
        movq XMM0,[Dstack]
        mov D11,[Dstack+8]
        movq XMM1,[Dstack+8]
        mov D12,[Dstack+16]
        movq XMM2,[Dstack+16]
        mov D13,[Dstack+24]
        movq XMM3,[Dstack+24]
    end

    if retcode='I' then
        a:=((ref function:int64(fnaddr))^())
        asm add Dstack,[pushedbytes]
        return a
    else
        x:=((ref function:real64(fnaddr))^())
        asm add Dstack,[pushedbytes]
        return word64@(x)
    fi
end 
