include "c:/bx/varlib.m"    !Contains project info that makes this the lead module

! M Source Output
macro $T1 = Stack[1]
macro $T2 = Stack[2]
macro $T3 = Stack[3]
macro $T4 = Stack[4]
macro $T5 = Stack[5]

proc fann_fannkuch*(variant $Result, variant n) =
    varrec p
    varrec q
    varrec s
    varrec signx
    varrec maxflips
    varrec sum
    varrec i
    varrec q1
    varrec flips
    varrec qq
    varrec j
    varrec sx
    varrec tt

    k_init(&p)
    k_init(&q)
    k_init(&s)
    k_init(&signx)
    k_init(&maxflips)
    k_init(&sum)
    k_init(&i)
    k_init(&q1)
    k_init(&flips)
    k_init(&qq)
    k_init(&j)
    k_init(&sx)
    k_init(&tt)

    k_pushci(&$T1, 0)
    k_makelist(&$T1, 1, 1)
    k_push(&$T2, n)
    k_pushci(&$T3, 1)
    k_add(&$T2, &$T3)
    k_mul(&$T1, &$T2)
    k_pop(&p, &$T1)
    k_pushci(&$T1, 0)
    k_makelist(&$T1, 1, 1)
    k_push(&$T2, n)
    k_pushci(&$T3, 1)
    k_add(&$T2, &$T3)
    k_mul(&$T1, &$T2)
    k_pop(&q, &$T1)
    k_pushci(&$T1, 0)
    k_makelist(&$T1, 1, 1)
    k_push(&$T2, n)
    k_pushci(&$T3, 1)
    k_add(&$T2, &$T3)
    k_mul(&$T1, &$T2)
    k_pop(&s, &$T1)
    k_pushci(&$T1, 1)
    k_pop(&signx, &$T1)
    k_pushci(&$T1, 0)
    k_pop(&maxflips, &$T1)
    k_pushci(&$T1, 0)
    k_pop(&sum, &$T1)
    k_pushci(&$T1, 0)
    k_pop(&i, &$T1)
    goto L3
L2:
    k_push(&$T2, &p)
    k_push(&$T3, &i)
    k_push(&$T1, &i)
    k_popix(&$T1, &$T2, &$T3)
    k_push(&$T2, &q)
    k_push(&$T3, &i)
    k_push(&$T1, &i)
    k_popix(&$T1, &$T2, &$T3)
    k_push(&$T2, &s)
    k_push(&$T3, &i)
    k_push(&$T1, &i)
    k_popix(&$T1, &$T2, &$T3)
L3:
    k_pushref(&$T1, &i)
    k_incr(&$T1)
    k_push(&$T1, &i)
    k_push(&$T2, n)
    if k_le(&$T1, &$T2) then goto L2 end
L4:
L5:
    k_push(&$T1, &p)
    k_pushci(&$T2, 1)
    k_index(&$T1, &$T2)
    k_pop(&q1, &$T1)
    k_push(&$T1, &q1)
    k_pushci(&$T2, 1)
    if k_eq(&$T1, &$T2) then goto L7 end
    k_pushci(&$T1, 1)
    k_pop(&i, &$T1)
    goto L9
L8:
    k_push(&$T1, &p)
    k_push(&$T2, &i)
    k_index(&$T1, &$T2)
    k_push(&$T2, &q)
    k_push(&$T3, &i)
    k_popix(&$T1, &$T2, &$T3)
L9:
    k_pushref(&$T1, &i)
    k_incr(&$T1)
    k_push(&$T1, &i)
    k_push(&$T2, n)
    if k_le(&$T1, &$T2) then goto L8 end
L10:
    k_pushci(&$T1, 1)
    k_pop(&flips, &$T1)
L11:
    k_push(&$T1, &q)
    k_push(&$T2, &q1)
    k_index(&$T1, &$T2)
    k_pop(&qq, &$T1)
    k_push(&$T1, &qq)
    k_pushci(&$T2, 1)
    if k_ne(&$T1, &$T2) then goto L13 end
    k_push(&$T1, &signx)
    k_push(&$T2, &flips)
    k_mul(&$T1, &$T2)
    k_pushref(&$T2, &sum)
    k_binto(&$T2, &$T1, kkadd)
    k_push(&$T1, &flips)
    k_push(&$T2, &maxflips)
    if k_le(&$T1, &$T2) then goto L14 end
    k_push(&$T1, &flips)
    k_pop(&maxflips, &$T1)
L14:
    goto L12
L13:
    k_push(&$T2, &q)
    k_push(&$T3, &q1)
    k_push(&$T1, &q1)
    k_popix(&$T1, &$T2, &$T3)
    k_push(&$T1, &q1)
    k_pushci(&$T2, 4)
    if k_lt(&$T1, &$T2) then goto L15 end
    k_pushci(&$T1, 2)
    k_pop(&i, &$T1)
    k_push(&$T1, &q1)
    k_pushci(&$T2, 1)
    k_sub(&$T1, &$T2)
    k_pop(&j, &$T1)
L16:
    k_push(&$T1, &q)
    k_push(&$T2, &i)
    k_indexref(&$T1, &$T2)
    k_push(&$T2, &q)
    k_push(&$T3, &j)
    k_indexref(&$T2, &$T3)
    k_swap(&$T1, &$T2)
    k_pushref(&$T1, &i)
    k_incr(&$T1)
    k_pushref(&$T1, &j)
    k_decr(&$T1)
L17:
    k_push(&$T1, &i)
    k_push(&$T2, &j)
    if k_lt(&$T1, &$T2) then goto L16 end
L18:
L15:
    k_push(&$T1, &qq)
    k_pop(&q1, &$T1)
    k_pushref(&$T1, &flips)
    k_incr(&$T1)
    goto L11
L12:
L7:
    k_push(&$T1, &signx)
    k_pushci(&$T2, 1)
    if k_ne(&$T1, &$T2) then goto L19 end
    k_push(&$T1, &p)
    k_pushci(&$T2, 2)
    k_indexref(&$T1, &$T2)
    k_push(&$T2, &p)
    k_pushci(&$T3, 1)
    k_indexref(&$T2, &$T3)
    k_swap(&$T1, &$T2)
    k_pushci(&$T1, -1)
    k_pop(&signx, &$T1)
    goto L20
L19:
    k_push(&$T1, &p)
    k_pushci(&$T2, 2)
    k_indexref(&$T1, &$T2)
    k_push(&$T2, &p)
    k_pushci(&$T3, 3)
    k_indexref(&$T2, &$T3)
    k_swap(&$T1, &$T2)
    k_pushci(&$T1, 1)
    k_pop(&signx, &$T1)
    k_pushci(&$T1, 2)
    k_pop(&i, &$T1)
    goto L22
L21:
    k_push(&$T1, &s)
    k_push(&$T2, &i)
    k_index(&$T1, &$T2)
    k_pop(&sx, &$T1)
    k_push(&$T1, &sx)
    k_pushci(&$T2, 1)
    if k_eq(&$T1, &$T2) then goto L24 end
    k_push(&$T1, &sx)
    k_pushci(&$T2, 1)
    k_sub(&$T1, &$T2)
    k_push(&$T2, &s)
    k_push(&$T3, &i)
    k_popix(&$T1, &$T2, &$T3)
    goto L23
L24:
    k_push(&$T1, &i)
    k_push(&$T2, n)
    if k_ne(&$T1, &$T2) then goto L25 end
    k_push(&$T1, &sum)
    k_push(&$T2, &maxflips)
    k_makelist(&$T1, 2, 1)
    goto L1
L25:
    k_push(&$T2, &s)
    k_push(&$T3, &i)
    k_push(&$T1, &i)
    k_popix(&$T1, &$T2, &$T3)
    k_push(&$T1, &p)
    k_pushci(&$T2, 1)
    k_index(&$T1, &$T2)
    k_pop(&tt, &$T1)
    k_pushci(&$T1, 0)
    k_pop(&j, &$T1)
    goto L27
L26:
    k_push(&$T2, &j)
    k_pushci(&$T3, 1)
    k_add(&$T2, &$T3)
    k_push(&$T1, &p)
    k_index(&$T1, &$T2)
    k_push(&$T2, &p)
    k_push(&$T3, &j)
    k_popix(&$T1, &$T2, &$T3)
L27:
    k_pushref(&$T1, &j)
    k_incr(&$T1)
    k_push(&$T1, &j)
    k_push(&$T2, &i)
    if k_le(&$T1, &$T2) then goto L26 end
L28:
    k_push(&$T3, &i)
    k_pushci(&$T4, 1)
    k_add(&$T3, &$T4)
    k_push(&$T2, &p)
    k_push(&$T1, &tt)
    k_popix(&$T1, &$T2, &$T3)
L22:
    k_pushref(&$T1, &i)
    k_incr(&$T1)
    k_push(&$T1, &i)
    k_push(&$T2, n)
    if k_le(&$T1, &$T2) then goto L21 end
L23:
L20:
    goto L5
L6:
    k_pushci(&$T1, 0)
    k_pushci(&$T2, 0)
    k_makelist(&$T1, 2, 1)
L1:

    k_move($Result, &$T1)
    k_unshare(n)
    k_unshare(&p)
    k_unshare(&q)
    k_unshare(&s)
    k_unshare(&signx)
    k_unshare(&maxflips)
    k_unshare(&sum)
    k_unshare(&i)
    k_unshare(&q1)
    k_unshare(&flips)
    k_unshare(&qq)
    k_unshare(&j)
    k_unshare(&sx)
    k_unshare(&tt)
    [4]varrec Stack
end

proc main*() =
    varrec n
    varrec sum
    varrec flips

    k_init(&n)
    k_init(&sum)
    k_init(&flips)

    staticinit()

    os_initwindows()

    k_pushci(&$T1, 10)
    k_pop(&n, &$T1)
    k_pushvoid(&$T1)
    k_push(&$T2, &n)
    fann_fannkuch(&$T1, &$T2)
    k_expand(&$T1, 2)
    k_pop(&sum, &$T2)
    k_pop(&flips, &$T1)
    pch_startprintcon()
    k_push(&$T1, &sum)
    pch_print_nf(&$T1)
    pch_println()
    pch_endprint()
    pch_startprintcon()
    k_pushcs(&$T1, "Pfannkuchen(#) = #")
    pch_setformat(&$T1)
    k_pushvoid(&$T1)
    k_push(&$T2, &n)
    pch_print(&$T2, &$T1)
    k_pushvoid(&$T1)
    k_push(&$T2, &flips)
    pch_print(&$T2, &$T1)
    pch_println()
    pch_endprint()
L29:

    k_unshare(&n)
    k_unshare(&sum)
    k_unshare(&flips)
    [2]varrec Stack
end


proc staticinit =


end

global int ntypes =38

global tabledata [0:]ichar ttname, [0:]i16 ttbasetype, [0:]i16 tttarget,
        [0:]int ttlower, [0:]int ttlength, [0:]int ttsize, [0:]byte ttbitwidth =
    ("void"          ,    0,    0,     1,      0,      0,    0),    ! 0
    ("int"           ,    1,    0,     1,      0,      8,   64),    ! 1
    ("word"          ,    2,    0,     1,      0,      8,   64),    ! 2
    ("real"          ,    3,    0,     1,      0,      8,   64),    ! 3
    ("decimal"       ,    4,    0,     1,      0,      0,    0),    ! 4
    ("range"         ,    5,    0,     1,      0,      8,   64),    ! 5
    ("set"           ,    6,    0,     1,      0,      0,    0),    ! 6
    ("dict"          ,    7,    0,     1,      0,      0,    0),    ! 7
    ("vector"        ,    8,    0,     1,      0,      0,    0),    ! 8
    ("bits"          ,    9,    0,     1,      0,      0,    0),    ! 9
    ("string"        ,   10,    0,     1,      0,      0,    0),    ! 10
    ("list"          ,   11,    0,     1,      0,      0,    0),    ! 11
    ("array"         ,   12,    0,     1,      0,      0,    0),    ! 12
    ("record"        ,   13,    0,     1,      0,      0,    0),    ! 13
    ("struct"        ,   14,    0,     1,      0,      0,    0),    ! 14
    ("refvar"        ,   15,    0,     1,      0,      8,   64),    ! 15
    ("refbit"        ,   16,    0,     1,      0,     16,  128),    ! 16
    ("refpack"       ,   17,    0,     1,      0,      8,   64),    ! 17
    ("symbol"        ,   18,    0,     1,      0,      8,   64),    ! 18
    ("type"          ,   19,    0,     1,      0,      8,   64),    ! 19
    ("operator"      ,   20,    0,     1,      0,      8,   64),    ! 20
    ("retaddr"       ,   21,    0,     1,      0,      0,    0),    ! 21
    ("except"        ,   22,    0,     1,      0,      0,    0),    ! 22
    ("var"           ,   23,   23,     1,      0,     16,  128),    ! 23
    ("any"           ,   24,    0,     1,      0,      0,    0),    ! 24
    ("bool"          ,   25,    0,     1,      0,      0,    0),    ! 25
    ("i8"            ,   26,    0,     1,      0,      1,    8),    ! 26
    ("i16"           ,   27,    0,     1,      0,      2,   16),    ! 27
    ("i32"           ,   28,    0,     1,      0,      4,   32),    ! 28
    ("u8"            ,   29,    0,     1,      0,      1,    8),    ! 29
    ("u16"           ,   30,    0,     1,      0,      2,   16),    ! 30
    ("u32"           ,   31,    0,     1,      0,      4,   32),    ! 31
    ("r32"           ,   32,    0,     1,      0,      4,   32),    ! 32
    ("u1"            ,   33,    0,     1,      0,      0,    1),    ! 33
    ("u2"            ,   34,    0,     1,      0,      0,    2),    ! 34
    ("u4"            ,   35,    0,     1,      0,      0,    4),    ! 35
    ("packstrc"      ,   36,    0,     1,      0,      0,    0),    ! 36
    ("packstrz"      ,   37,    0,     1,      0,      0,    0),    ! 37
    ("stringz"       ,   38,    0,     1,      0,      8,   64),    ! 38
end

global tabledata []ref proc dllprocaddr, []ichar dllnames,[]u16 dllretmode, []u16 dllnparams, []u16 dllpmindex =
    (nil, nil, 0, 0, 0),
end

global []u16 dllparamtable = (
)

global tabledata []ichar sttnames, []u16 sttmodes, []byte sttnfields, []u16 sttntopfields, []u16 sttfindex =
    (nil, 0, 0, 0, 0),
end

global tabledata []ichar fdnames, []u16 fdmodes, []u32 fdoffsets, []byte fdtopfield =
    (nil, 0, 0, 0),
end

global []genfieldrec genfieldtable = (
)

global []fieldrec recfieldtable = (
)

global []methodrec methodtable = (
)

global []ichar moduletable = (
    "fann.q",   ! 1
)

global ichar q_inputfile = "fann.q"

! End of M Source Output
