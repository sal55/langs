export function os_calldllfunction(ref proc fnaddr,
		int retcode, nargs, ref[]i64 args, ref[]byte argcodes)u64 =
	u64 a
	r64 x
	int nextra, pushedbytes

	nextra:=0

!The stack is guaranteed to be 16-byte aligned at this point

	if nargs<4 then
		nextra:=4-nargs			!need at least 4 slots for shadow space
	elsif nargs.odd then		!need one more for a 16-byte-aligned stack
		nextra:=1
	fi

	pushedbytes:=(nextra+nargs)*8

	to nextra do
		asm push 0
	od

	for i:=nargs downto 1 do
		a:=args[i]					!get generic 64-bit value to push
		asm push u64 [a]
	od

!blindly load first 4 args to both int/float regs

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
		x:=((ref function:r64(fnaddr))^())
		asm add Dstack,[pushedbytes]
		return u64@(x)
	fi
end	
