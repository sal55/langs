project =
	module pc_api
	module pc_decls

	module pc_diags
!	module pc_diags_dummy
	module pc_reduce

! Interpreter
	module pc_run
	module pc_runaux
!	module pc_run_dummy

! Tables (eg. types and IL opcodes)
	module pc_tables

	module mc_GenMCL
	module mc_AuxMCL
	module mc_LibMCL
	module mc_StackMCL
	module mc_Optim

	module mc_GenSS

	module mc_Decls as md
	module mc_OBJdecls
	module mc_WriteASM
!	module mc_WriteGAS
!	module mc_WriteNASM
!	module mc_WriteMASM
!!
	module mc_WriteEXE
!	module mc_WriteOBJ
!	module mc_WriteOBJ_dummy

!	module mc_writess
!	module mc_disasm
!	module mc_writess_dummy

	module mx_decls
	module mx_run
	module mx_lib
	module mx_write

end

export byte pc_userunpcl=0

export byte asmonly=0			!for cx/gas
export byte pdcc=0

export function writessdata(int fexe)ref strbuffer=
	nil
end

global proc writecoff(ichar outfile)=end
