global const mcxsig = 'MCX\e'

global tabledata() [0:]ichar mcxdirnames =
    (pad_dir = 0,       $),     ! nothing follows except next tag; for padding/alignment
    (version_dir,       $),     ! STR string follows with version code
    (code_dir,          $),     ! N(u32) then N bytes of code data
    (idata_dir,         $),     ! N(u32) then N bytes init data
    (zdata_dir,         $),     ! N(u32) (no data follows)
    (reloc_dir,         $),     ! N(u32) then N records follow
    (dlls_dir,          $),     ! N(u32) then N STR items, the DLL base names
    (libs_dir,          $),     ! N(u32) then N STR items, the MCX base names
    (importsymbols_dir, $),     ! N(u32) then N STR items, the imported names
    (exportsymbols_dir, $),     ! N(u32) then N STR items, the exported names
    (exportsegs_dir,    $),     ! N(u32) then N u8 items, each is a segment code
    (exportoffsets_dir, $),     ! N(u32) then N u32 items, each an offset in the segment
    (entry_dir,         $),     ! N(u32) N is a byte offset within code segment for entry point
    (end_dir,           $),     ! nothing follows; end of file
end

!Relocation codes

global tabledata() [0:]ichar mcxrelocnames =
    (no_rel = 0,        $),

    (locabs32_rel,  "locabs32"),        ! add target segment address to 32-bit offset
    (locabs64_rel,  "locabs64"),        ! add target segment address to 64-bit offset

    (impabs32_rel,  "impabs32"),        ! replace 32-bit 0-field with address of imported symbol
    (impabs64_rel,  "impabs64"),        ! replace 64-bit 0-field with address of imported symbol

    (imprel32_rel,  "imprel32"),        ! replace 32-bit 0-field with offset of thunk entry for symbol
end

export tabledata() []ichar segmentnames =
    (code_seg,      "code"),
    (idata_seg,     "idata"),
    (zdata_seg,     "zdata"),
    (rodata_seg,    "rodata"),
    (impdata_seg,   $),
end

global record librec=
    ichar version

    int codesize            ! bytes in code block, excluding thunk/addr tables
    int idatasize           ! bytes in idata block
    int zdatasize           ! bytes in zdata block (no data; created on fixup)

    int nrelocs             ! size of reloctable
    int ndlllibs            ! size of imported dll names
    int nlibs               ! size of imported libnames
    int nimports            ! size of imports/importlib tables
    int nexports            ! size of exports/exportsegs/exportoffsets tables

    ref byte codeptr        ! executable code block (includes thunk/addr table)
    ref byte idataptr       ! initialised data block

    ref[]mcxreloc   reloctable      ! table of reloc entries
    ref[]ichar      dllnames        ! base names of imported dll files (no extension)
    ref[]ichar      libnames        ! base names of imported mcx files (no extension)
    ref[]ichar      importnames     ! names of imported symbols
    ref[]ichar      exports         ! names of exported symbols
    ref[]byte       exportsegs      ! segment where each is located
    ref[]u64        exportoffsets   ! offsets within each segment

    u64 entryoffset                 ! offset within code block where execution will start
                                    ! value of 0xFFFFFFFF (is u32 in file) means not set

!The next section is filled in after loading

    ref byte zdataptr               ! zeroed data block
    int codexsize                   ! bytes in thunk/addr tables that follow code
    ref[]u64        exportaddr      ! fully fixed-up addresses of exported symbols (not in file)
    ref[]int16      importxreftable ! map symbol index to global one

    ichar           filespec        !full path
    ichar           libname         !base name of library
    ref byte        entryaddr       !start address (left at nil when entryoffset not set)
    int             libno           !index of this entry in progtable
end

global record mcxreloc =
    u32     offset          ! Offset with .segment of the reloc item
    union
        u16     stindex         ! For Imp-codes, index into global import tables
        byte    targetsegment   ! For Loc-codes, target segment refered to
    end
    byte    segment         ! Segment containing the reloc item
    byte    reloctype       ! Reloc code (see enums); also sets size of reloc item
end

global const maxdlls =      20
global const maxlibs =      20
global const maxsymbols =   3000

!Global DLL tables

global [maxdlls]ichar       dllnametable
global [maxdlls]u64         dllinsttable
global int ndlllibs

!Global Lib table

global [maxlibs]ichar       libnametable
global [maxlibs]ref librec  libtable
global [maxlibs]byte        libdefined      !1 when already defined
global int nlibs

!Global Import tables

global [maxsymbols]ichar    symbolnametable ! Name of symbol
global [maxsymbols]byte     symboldefined   ! 1 when fully resolved with address
global [maxsymbols]ref void symboladdress   ! Abs address
global [maxsymbols]int16    symbollibindex  ! Lib index where defined
global [maxsymbols]byte     symboldllindex  ! DLL index of library where found
global int nsymbols
