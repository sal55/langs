global [0..pclnames.upb]ref proc jhandlertable

!global fun asmavailable:int= 0
global func asmavailable:int=
	0
end

global function disploop_asm:ref int =
	ABORTPROGRAM("-ASM NOT AVAILABLE")
	return nil
end

global proc initjhandlers=
end
