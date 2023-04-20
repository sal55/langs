!Switch with compiler-generated jumptable

i64 x=0
i64 y=1

enumdata =
    kloadimm,
    kload,
    kstore,
    kadd,
    kjumpne,
    kunload,
    kstop,
end

tabledata []byte code, []int data =
    (kloadimm,      42),
    (kloadimm,      56),
    (kadd,          0),
    (kunload,       0),

    (kload,         int(&x)),
    (kload,         int(&y)),
    (kadd,          0),
    (kstore,        int(&x)),

    (kload,         int(&x)),
    (kloadimm,      100'000'000),
    (kjumpne,       1),
    (kstop,         0),
end

proc run=
    [100]i64 stack
    ref i64 a
    ref i64 sp
    int pc

    pc:=1
    sp:=cast(&stack)

    doswitchu code[pc]
    when kloadimm then
        ++sp
        sp^:=data[pc]
        ++pc

    when kload then
        a:=cast(data[pc])
        ++sp
        sp^:=a^
        ++pc

    when kstore then
        a:=cast(data[pc])
        a^:=sp^
        --sp
        ++pc

    when kadd then
        --sp
        sp^ +:= (sp+1)^
        ++pc

    when kjumpne then
        sp:=sp-2
        if (sp+1)^ <> (sp+2)^ then
            pc := data[pc]
        else
            ++pc
        fi

    when kunload then
        --sp
        ++pc
        
    when kstop then
        exit

    else
        println "Umimpl op"
        stop
    end

    println "Stop",=x,=y
    stop

end

proc main=
    run()
end
