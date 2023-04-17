[100]i64 stack
int pcindex

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

# Note: this 'byte' uses separate, parallel arrays for opcodes and immediate operands:

tabledata []byte code, []int data =
    (kloadimm,      42),              # set up for x and y is not done, only main loop
    (kloadimm,      56),
    (kadd,          0),
    (kunload,       0),

    (kload,         int(&x)),         # variables are static ones as no stack frame for this test
    (kload,         int(&y)),
    (kadd,          0),
    (kstore,        int(&x)),

    (kload,         int(&x)),
    (kloadimm,      100'000'000),
    (kjumpne,       1),               # 1 is index of first instruction, not relative offset
    (kstop,         0),
end

macro nextinstr = goto jumptable[code[pc]]

proc run =
    ref i64 a
    ref i64 sp
    int pc
    static []ref label jumptable=(
        jloadimm,
        jload,
        jstore,
        jadd,
        jjumpne,
        junload,
        jstop)

    pc:=1
    sp:=cast(&stack)

    nextinstr

jloadimm::

    ++sp
    sp^:=data[pc]
    ++pc
    nextinstr

jload::
    a:=cast(data[pc])
    ++sp
    sp^:=a^
    ++pc
    nextinstr


jstore::
    a:=cast(data[pc])
    a^:=sp^
    --sp
    ++pc
    nextinstr

jadd::
    --sp
    sp^ +:= (sp+1)^
    ++pc
    nextinstr

jjumpne::
    sp:=sp-2
    if (sp+1)^ <> (sp+2)^ then
    pc := data[pc]
    else
        ++pc
    fi
    nextinstr

junload::
    --sp
    ++pc
    nextinstr

jstop::
    println "Stop",=x,=y
    stop
end

proc main =
    run()
end
