project =

	module mm_decls

	module mm_cli				! Command line interface

	module mm_lex				! Lexer, produces tokens
	module mm_parse				! Parser, produces ST, TT and AST1
	module mm_name				! Name resolution, AST1 to AST2
	module mm_type				! Type analysis, AST2 to AST3

	module mm_diags				! Diagnostics

!Embedded SYSLIB sources
	module mm_libsources		! Embedded Syslib sources

	module mm_modules			! Module handling

	module mm_support			! Source file handling

	module mm_tables			! Enumerations
	module mm_lib				! Support functions

!AST to PCL IL
	module mm_genpcl			! ST/AST -> PST/PCL
	module mm_blockpcl			! AST -> PCL

!PCL Support Library

	module pcl_decls			! Data structures, vars, enums and tables
	module pcl_api				! PCL-generating API and support routines
	module pcl_diags			! Dump PCL code by function

!PCL->MCL Inbetween modules
	module mcl_decls			! Data structures, vars, enums, and tables
	module mcl_gen				! ST/PCL to MCL AND some support
	module mcl_genaux			! Other support routines
	module mcl_stack			! PCL stack management (and some diags)
	module mcl_block			! PCL to MCL per functions
	module mcl_blockaux			! Support routines for PCL to MCL
!
!!MCL Support Library
	module mcx_decls			! Data structures, vars, enums, and tables
	module mcx_asm				! Dump generated MCL as ASM source code
!	module mcx_gas				! Dump generated MCL as GAS source code

	module mcx_lib				! Low-level MCL-generating API and other support

	module mcx_pedecls			! For PE format + some extra records
	module mcx_optim			! Peephole MCL Optimiser

!
	module mcx_genss			! MCL to SS binary code and data
	module mcx_exe				!
!
!MCL Run In Memory
	module mcx_run
end

global proc showrun(filehandle f)=end
global const pclpresent=1
global proc writeexports(ichar basefile, modulename)=end
