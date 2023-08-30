!Notes:
! word64 is u64 (I can just write u64 too)
! real64 is f64
! word64@(x) is a type-punning cast (reinterpret bits)
! D10/11/12/13 are tidier aliases for rcx, rdx, r8, r9 x64 registers
! DStack is an alias for D15 which is an alias for rbp
! ^ is a dereference operator

export function os_calldllfunction(ref proc fnaddr,
        int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
    word64 a
    real64 x
    int nextra, pushedbytes

    nextra:=0

!The stack is guaranteed to be 16-byte aligned at this point

    if nargs<4 then
        nextra:=4-nargs         !need at least 4 slots for shadow space
    elsif nargs.odd then        !need one more for a 16-byte-aligned stack
        nextra:=1
    fi

    pushedbytes:=(nextra+nargs)*8

    to nextra do
        asm push 0
    od

    for i:=nargs downto 1 do
        a:=args[i]                  !get generic 64-bit value to push
        asm push word64 [a]
    od

!load first 4 args to registers; this version will blindly load all 4 args
!(even if there are less, or none) to both integer and xmm registers (even if
!all ints). That info is in argcodes[], currently not used.
!Loading to both regs is necessary for any variadic functions

!However, these 8 instructions are likely faster than either a rolled or unrolled loop
!that checks arg numbers and flags, and loads the correct register

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
