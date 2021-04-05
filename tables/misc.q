global tabledata() bsnames, bscat, bswidths=
    (bs_none=0,     $,  0,  ws_rect(0,0,0,0)),          !no border
!   (bs_windows,    $,  'W',    ws_rect(0,0,0,0)),          !windows-drawn, but no own-drawn border
    (bs_simplew,    $,  'W',    ws_rect(1,1,1,1)),          !single 1-pixel black line, windows drawn
    (bs_simple,     $,  'X',    ws_rect(1,1,1,1)),          !single 1-pixel black line
    (bs_thick,      $,  'X',    ws_rect(2,2,2,2)),          !2-pixel border
    (bs_panel,      $,  'X',    ws_rect(1,1,1,1)),          !raised panel, 1-pixel
    (bs_inset,      $,  'X',    ws_rect(1,1,1,1)),          !inset panel, 1-pixel
    (bs_ownsimple,  $,  'I',    ws_rect(0,0,0,0)),          !included inset panel, 1-pixel (drawn as part of client area)
    (bs_ownpanel,   $,  'I',    ws_rect(0,0,0,0)),          !included inset panel, 1-pixel (drawn as part of client area)
    (bs_owninset,   $,  'I',    ws_rect(0,0,0,0)),          !included inset panel, 1-pixel
    (bs_testext,    $,  'X',    ws_rect(10,10,10,10)),
    (bs_testint,    $,  'I',    ws_rect(8,8,8,8)),
    (bs_dummy,      $,  0,  ws_rect(0,0,0,0))
end

global tabledata() windowclassnames, defaultborderstyles = begin
    (no_class=0,        $,  bs_none),           !Unassigned
    (window_class,      $,  wbs_resize),        !Main window
    (memwindow_class,   $,  wbs_none),          !memory backup to any window
    (popup_class,       $,  wbs_thick),         !Pop-up window (forms a stack)
    (float_class,       $,  bs_thick),          !Independent window
    (bitmap_class,      $,  bs_none),           !(image handling)
    (screen_class,      $,  bs_none),           !Describes the desktop screen (not owned by my app)
    (printer_class,     $,  bs_none),           !Used for printing

    (group_class,       $,  bs_inset),          !Used mainly for grouping other buttons (eg. for Smdefblock)
    (panel_class,       $,  bs_inset),          !General purpose panel for drawing in etc
!   (button_class,      $,  bs_panel),          !Click button
    (button_class,      $,  bs_simplew),        !Click button
    (toggle_class,      $,  bs_none),           !Toggle button (can be composite, eg mark and label)
    (select_class,      $,  bs_none),           !Select from several choices
    (editbox_class,     $,  bs_simplew),        !Single-line edit control
    (scrollbar_class,   $,  bs_simplew),        !Hoz or vert scroll bar (Some windows can also have Windows-drawn scroll bars)
    (listbox_class,     $,  bs_simplew),        !List of options (scrollable usually)
    (dropdown_class,    $,  bs_none),           !Button revealing attached listbox when clicked
    (framebar_class,    $,  bs_panel),          !Left or right full-height panel used for toolboxes etc
    (statusbar_class,   $,  bs_panel),          !Top or bottom full-width panel used for scrollbars
    (tooltip_class,     $,  bs_simplew),        !Tooltops displayed when hovering over enabled buttons
    (arrow_class,       $,  bs_ownpanel),       !Click button normally displaying an error in one of 4 orientations
    (mark_class,        $,  bs_none),           !Toggle or select mark
    (label_class,       $,  bs_none),           !Contains unclickable text usually
    (dummy_class,       $,  bs_none)
end

global tabledata() actionnames, actionhandlertable=
    (draw_w,        $,      ()),
    (update_w,      $,      ()),
    (last_w,        $,      ()),
end

global tabledata() colournames, colourvalues = begin
!                      BB'GG'RR
    (black,     $,  0x_00'00'00),
    (red,       $,  0x_00'00'C0),
    (dkred,     $,  0x_00'00'90),
    (red3,      $,  0x_00'00'70),
    (green,     $,  0x_00'C0'00),
    (dkgreen,   $,  0x_00'90'00),
    (green3,    $,  0x_00'70'00),
    
    (blue,      $,  0x_C0'00'00),
    (dkblue,    $,  0x_90'00'00),
    (blue3,     $,  0x_70'00'00),

    (cyan,      $,  0x_c0'c0'00),
    (dkcyan,    $,  0x_90'90'00),
    (cyan3,     $,  0x_70'70'00),

    (magenta,   $,  0x_c0'00'c0),
    (dkmagenta, $,  0x_90'00'90),
    (magenta3,  $,  0x_70'00'70),

    (yellow,    $,  0x_00'C0'C0),
    (dkyellow,  $,  0x_00'90'90),
    (yellow3,   $,  0x_00'70'70),
    (yellow4,   $,  0x_00'50'50),

    (white,     $,  0x_FF'FF'FF),
    (ltgrey,    $,  0x_C0'C0'C0),
    (grey,      $,  0x_90'90'90),
    (dkgrey,    $,  0x_70'70'70),

    (ltorange,  $,  0x_00'A0'FF),
    (orange,    $,  0x_00'60'FF),
    (flesh,     $,  0x_70'85'EE),
    (pink,      $,  0x_9A'32'DB),
    (dkpink,    $,  0x_72'24'A9),
    (brown,     $,  0x_46'43'7D),
    (blue4,     $,  0x_B7'1C'5E),
    (blue5,     $,  0x_6F'3D'0D),
    (olive,     $,  0x_05'A0'88),
    (ltbrown,   $,  0x_00'70'B0),

    (blue6,     $,  0x_9C'63'1C),
    (green4,    $,  0x_12'51'11),
    (purple,    $,  0x_5E'0D'73),
    (blue7,     $,  0x_E6'27'1C),
    (crimson,   $,  0x_15'2A'D3),
    (violet,    $,  0x_54'16'A0),
    (blue8,     $,  0x_86'68'1E),
    (dkorange,  $,  0x_25'6A'D4),
    (green5,    $,  0x_09'46'41),
    (blue9,     $,  0x_65'0A'1D),

    (ltred,     $,  0x_00'00'FF),
    (ltgreen,   $,  0x_00'FF'00),
    (ltblue,    $,  0x_FF'00'00),
    (ltcyan,    $,  0x_FF'FF'00),
    (ltmagenta, $,  0x_FF'00'FF),
    (ltyellow,  $,  0x_00'FF'FF),

!The following are the Windows system colours, set up as indices
!Init needs need to retrieve the values and set up the rgb values in this table
    (button_col,    $,  0),     !button colour
    (window_col,    $,  0),     !window colour
    (text_col,      $,  0),     !text in windows
end

global tabledata() datanames, dataloc, basedatafiles, datatypes=

    (kstock,        "Stock",                'D',"stock.ini",    ccistock),
    (ksuppliers,    "Supplier",             'D',"supplier.ini", cciaddr),
    (kcustomers,    "Customer",             'D',"customer.ini", cciaddr),
    (ktariffs,      "Tariffs",              'D',"tariffs.ini",  ccitariff),
    (ksalesgirls,   "Sales Person",         'S',"shop.ini",     ccicode),
    (krates,        "Exchange Rate",        'D',"rates.ini",    ccimoney),
    (kcountries,    "Countries",            'D',"lists.ini",    ccicode),
    (kvessels,      "Airline/Ship",         'D',"lists.ini",    string),
    (kports,        "Ports",                'D',"lists.ini",    string),
    (kcostcodes,    "Costcode",             'D',"costcode.ini", string),
    (ksales,        "Sales Invoice",        'S',"sales.dat",    cciinv),
    (kpurchases,    "Purchase Invoice",     'S',"purchase.dat", cciinv),
    (ksaleitems,    "Sales Item",           'S',"sitems.dat",   cciitem),
    (kpurchitems,   "Purchase Item",        'S',"pitems.dat",   cciitem),
    (kinvitems,     "Invoice Item",         'S',"fred.dat",     cciitem),
    (kmoney,        "Exchange Rate",        'D',"rates.ini",    ccimoney),
    (kbankaccounts, "Bank Accounts",        'D',"lists.ini",    string),
    (kstaff,        "Employees",            'D',"staff.dat",    ccistaff),
    (kwages,        "Wages",                'D',"wages.dat",    cciwages),
    (kcategories,   "Categories",           'D',"category.dat", ccicode),
    (kshop,         "Shop File",            'S',"shop.ini",     string),
    (klists,        "Lists Files",          'D',"lists.ini",    string),
    (kaccustomers,  "Account Customers",    'S',"shop.ini",     ccicode),
end

global tabledata() cmdnames, key1table, key2table, cmdextra=
    (cmd_error,                 $,  "",             "",         0),
    (cmd_quit,                  $,  "*escape",      "",         0),
    (cmd_insert_char,           $,  "",             "",         0),     !keyparam=char code
    (cmd_functionkey,           $,  "",             "",         0),     !keyparam=function index
    (cmd_insert_tab,            $,  "*tab",         "",         0),
    (cmd_insert_space,          $,  "*space",       "",         0),
    (cmd_insert_enter,          $,  "*enter",       "",         0),

    (cmd_left_char,             $,  "*left",        "",         0),
    (cmd_right_char,            $,  "*right",       "",         0),
    (cmd_up_line,               $,  "*up",          "",         0),
    (cmd_down_line,             $,  "*down",        "",         0),

    (cmd_deleteleft_char,       $,  "*backspace",   "",         0),
    (cmd_deleteright_char,      $,  "*delete",      "",         0),
    (cmd_beg_line,              $,  "*home",        "",         0),
    (cmd_end_line,              $,  "*end",         "",         0),
    (cmd_up_page,               $,  "*pageup",      "",         0),
    (cmd_down_page,             $,  "*pagedown",    "",         0),

    (cmd_undo_line,             $,  "*ca",          "",         0),
    (cmd_beg_line2,             $,  "*cb",          "",         0),
    (cmd_beg_file2,             $,  "*scb",         "",         0),
    (cmd_deleteleft_line,       $,  "*cd",          "",         0),
    (cmd_end_line2,             $,  "*ce",          "",         0),
    (cmd_end_file2,             $,  "*sce",         "",         0),
    (cmd_find_string,           $,  "*cf",          "",         0),
    (cmd_find_string2,          $,  "*scf",         "",         1),
    (cmd_goto_line,             $,  "*cg",          "",         0),
    (cmd_detab_line,            $,  "*ch",          "",         0),
    (cmd_join_line,             $,  "*cj",          "",         0),
    (cmd_setblock_file,         $,  "*ck",          "*a",       0),
    (cmd_setblock_beg,          $,  "*ck",          "*b",       0),
    (cmd_copy_clipboard_block,  $,  "*ck",          "*c",       0),
    (cmd_copy_block,            $,  "*ck",          "*d",       0),
    (cmd_read_file,             $,  "*ck",          "*f",       0),
    (cmd_rot13_block,           $,  "*ck",          "*g",       0),
    (cmd_sort_block,            $,  "*ck",          "*h",       0),
    (cmd_setblock_end,          $,  "*ck",          "*k",       0),
    (cmd_read_temp,             $,  "*ck",          "*l",       0),
    (cmd_move_block,            $,  "*ck",          "*m",       0),
    (cmd_read_block,            $,  "*ck",          "*r",       0),
    (cmd_write_temp,            $,  "*ck",          "*s",       0),
    (cmd_setblock_clear,        $,  "*ck",          "*u",       0),
    (cmd_write_block,           $,  "*ck",          "*w",       0),
    (cmd_paste_clipboard,       $,  "*ck",          "*v",       0),
    (cmd_delete_block,          $,  "*ck",          "*y",       0),
    (cmd_write_file,            $,  "*ck",          "*z",       0),
    (cmd_findnext,              $,  "*cl",          "",         0),
    (cmd_findnext2,             $,  "*scl",         "",         1),
    (cmd_mark_set,              $,  "*cm",          "",         0),
    (cmd_insert_line,           $,  "*cn",          "",         0),
    (cmd_decr_tabwidth,         $,  "*co",          "*1",       0),
    (cmd_incr_tabwidth,         $,  "*co",          "*2",       0),
    (cmd_decr_indent_block,     $,  "*co",          "*3",       0),
    (cmd_incr_indent_block,     $,  "*co",          "*4",       0),
    (cmd_toggle_tabbars,        $,  "*co",          "*b",       0),
    (cmd_spacestotabs_file,     $,  "*co",          "*t",       0),
    (cmd_find_proc,             $,  "*cp",          "",         0),
    (cmd_replace_string,        $,  "*cq",          "*r",       0),
    (cmd_save_exit_run,         $,  "*cr",          "",         0),
    (cmd_deleteright_line,      $,  "*ct",          "",         0),
    (cmd_deleteleft_line2,      $,  "*sct",         "",         0),
    (cmd_undo_delete_line,      $,  "*cu",          "",         0),
    (cmd_goto_mark,             $,  "*cv",          "",         0),
    (cmd_save_exit,             $,  "*cx",          "",         0),
    (cmd_delete_line,           $,  "*cy",          "",         0),
    (cmd_save_exit_compile,     $,  "*cz",          "",         0),

    (cmd_beg_file,              $,  "*chome",       "",         0),
    (cmd_end_file,              $,  "*cend",        "",         0),
    (cmd_up_proc,               $,  "*cpageup",     "",         0),
    (cmd_down_proc,             $,  "*cpagedown",   "",         0),
    (cmd_up_lineten,            $,  "*cup",         "",         0),
    (cmd_down_lineten,          $,  "*cdown",       "",         0),
    (cmd_left_word,             $,  "*cleft",       "",         0),
    (cmd_right_word,            $,  "*cright",      "",         0),

    (cmd_deleteleft_word,       $,  "*cbackspace",  "",         0),
    (cmd_deleteright_word,      $,  "*cdelete",     "",         0),

    (cmd_goto_lastjump,         $,  "*aa",          "",         0),
    (cmd_compile,               $,  "*ab",          "",         0),
    (cmd_replacenext_string,    $,  "*ac",          "",         0),
    (cmd_copy_line,             $,  "*ad",          "",         0),
    (cmd_find_string_begfile,   $,  "*af",          "",         0),
    (cmd_toggle_syntax_hilite,  $,  "*ah",          "",         0),
    (cmd_define_macro,          $,  "*ak",          "",         0),
    (cmd_lowercase_char,        $,  "*al",          "",         0),
    (cmd_insert_bullet,         $,  "*ao",          "",         0),
    (cmd_end_proc,              $,  "*ap",          "",         0),
!   (cmd_deleteleft_word,       $,  "*ar",          "",         0),
    (cmd_uppercase_char,        $,  "*au",          "",         0),
    (cmd_invertcase_char,       $,  "*av",          "",         0),
    (cmd_end_page,              $,  "*aw",          "",         0),
    (cmd_uncomment_line,        $,  "*a0",          "",         0),
    (cmd_comment_line,          $,  "*a1",          "",         0),

    (cmd_delete_line2,          $,  "*adelete",     "",         0),

!   (cmd_,      $,  "*",            "",         0),
end

global tabledata() tknames,tkcolours=
    (tkvar  ,       $,  con_black),
    (tknumber,      $,  con_blue),
    (tkstring,      $,  con_dkmagenta),
    (tkbracket,     $,  con_black),
    (tkcomment,     $,  con_dkgreen),
    (tkspaces   ,   $,  con_white),
    (tktab  ,       $,  con_white),
    (tkpunct,       $,  con_dkcyan),
    (tkfunction,    $,  con_dkred),
    (tkkeyword,     $,  con_dkblue),
    (tktype,        $,  con_dkyellow),
    (tkdeclname,    $,  con_dkred),
    (tkregname  ,   $,  con_dkyellow),
    (tkopcodename,  $,  con_dkcyan),
    (tkoperator,    $,  con_dkblue)
end

