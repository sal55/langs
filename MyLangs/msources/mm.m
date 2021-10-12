!mapmodule mm_libsources => mm_libsources_dummy
!mapmodule mm_diags => mm_diags_dummy

!import msys
!import mlib
!import oslib
!import clib
!
!import mm
!import mm_blockpcl
!import mm_decls
!import mm_diags
!import mm_export
!import mm_genpcl
!import mm_lex
!import mm_lib
!import mm_libsources
!import mm_name
!import mm_parse
!import mm_pcl
import mm_start
!import mm_support
!import mm_tables
!import mm_type

proc start=

!	addmodulemapping("msys","msystemp")

	addmodulemapping("mlib","mlibp")
	addmodulemapping("clib","mclib")

	addmodulemapping("oslib","mwindowsp")
!	addmodulemapping("oslib","oswindows")

	addmodulemapping("osdll","mwindllp")
!	addmodulemapping("osdll","mwindllx")
!	addmodulemapping("osdll","mwindllc")

!	addmodulemapping("pc_assem","pc_assemc")

	start_common('W','X64')
end
