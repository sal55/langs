project =

	$sourcepath "c:/mx7/"
	module pc_api
	module pc_decls

	module pc_diags
!	module pc_diags_dummy

!	module pc_run
!	module pc_runaux
!	module pc_run_dummy

	module pc_tables

!	module pc_genc
!	module pc_auxc
!	module pc_libc

!	module mc_GenMCL_dummy
!	module mc_AuxMCL
	module mc_LibMCL
!	module mc_StackMCL

	module mc_GenSS

	module mc_Decls as md
	module mc_OBJdecls
!	module mc_WriteASM
	module mc_WriteGAS
!	module mc_WriteNASM

	module mc_WriteEXE
	module mc_WriteOBJ

	module mc_writess
	module mc_disasm
!	module mc_writess_dummy

	module mx_decls
	module mx_run
	module mx_lib
	module mx_write
!	module mx_run_dummy
end

export byte pc_userunpcl=0

global proc genmcl(ichar dummy=nil)=end


