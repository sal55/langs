!mapmodule mm_libsources => mm_libsources_dummy
!mapmodule mm_diags => mm_diags_dummy
mapmodule ma_writess => ma_writess_dummy



import mm_decls
import mm_start

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
