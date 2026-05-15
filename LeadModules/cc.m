project =
    module cc_cli

!Global Data and Tables

    module cc_decls
    module cc_tables

!Lexing and Parsing
    module cc_lex
    module cc_parse

!Generate PCL
    module cc_genpcl
    module cc_blockpcl
    module cc_libpcl

!General

    module cc_lib
    module cc_support

!Bundled headers

    module cc_headers
!   module cc_headersx

!Diagnostics
    module cc_show
!    module cc_showdummy

!IL Backend
    $sourcepath "c:/mx7/"
    import pcl
!   import pclint

end


