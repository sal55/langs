enumdata =
    opconst=0,
    opload,
    opstore,
    opadd,
    opne,
    opdrop,
    oplog,
    opjmpif,
    opend,
end

macro nextop = (++ip)^

proc execute(ref i64 raw_ops, int num_locals) =
    [0:1024]i64 stack := empty
    
    ref i64 ip:=raw_ops
    ref i64 sp:=&stack[0]
    sp+:=num_locals

    static [0:]ref label jump_table = (
        exec_op_const,
        exec_op_load,
        exec_op_store,
        exec_op_add,
        exec_op_ne,
        exec_op_drop,
        exec_op_log,
        exec_op_jmpif,
        exec_op_end )

    i64 tmp_0:=0
    i64 tmp_1:=0
    i64 tmp_2:=0
    i64 tmp_3:=0

    goto jump_table[ip^]

exec_op_const::
    sp++^ := nextop
    goto jump_table[nextop]

exec_op_load::
    tmp_0 := nextop
    sp++^ := stack[tmp_0]
    goto jump_table[nextop]

exec_op_store::
    stack[nextop] := (--sp)^
    goto jump_table[nextop]

exec_op_add::
    tmp_0 := (--sp)^
    (sp-1)^ +:= tmp_0
    goto jump_table[nextop]

exec_op_ne::
    tmp_0 := (--sp)^
    (sp-1)^ := int(tmp_0 <> (sp-1)^)
    goto jump_table[nextop]

exec_op_drop::
    --sp
    goto jump_table[nextop]

exec_op_log::
    println (--sp)^
    goto jump_table[nextop]

exec_op_jmpif::
    tmp_0 := (--sp)^
    if tmp_0 then
        tmp_1 := nextop
        ip +:= tmp_1
    else
        ++ip
    fi
    goto jump_table[nextop]

exec_op_end::
    return
end

proc main =
    static [0:]i64 ops = (
        opconst, 100 million,
        opstore, 0,
        opconst, 1,
        opstore, 1,
        opconst, 42,
        opconst, 56,
        opadd,
        opdrop,
        opload, 2,
        opload, 1,
        opadd,
        opstore, 2,
        opload, 0,
        opload, 2,
        opne,
        opjmpif, -20,
        opend
    )

    execute(&.ops, 4)

end
