import clib
import mlib

global function os_calldllfunction(ref proc fnaddr,
		int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
	word64 a
	real64 x
	int oddstack, nextra, pushedbytes

	oddstack:=nextra:=0

	assem
		test astack,8
		jz L100
		mov byte [oddstack],1
L100:
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
		assem
			push 0
		end
	od

	for i:=nargs downto 1 do
		a:=args^[i]					!get generic 64-bit value to push
		assem
			push word64 [a]
		end
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

global function os_pushargs(ref[]word64 args, int nargs, nextra,
					ref proc fnaddr, int isfloat)word64=
!implements central part of 'callapplproc' which needs to be in asm
	word64 a
	real64 x

	to nextra do
		asm	push 0
	end

	for i to nargs do
		a:=args[i]
		asm push word64 [a]
	od

	if isfloat then
		x:=((ref function:real64(fnaddr))^())
		a:=int64@(x)
	else
		a:=((ref function:int64(fnaddr))^())
	fi

	return a
end
