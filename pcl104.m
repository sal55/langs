# Note: this 'bytecode' uses separate arrays for opcodes and immediate operands

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

[]byte code = (
    kloadimm,
    kloadimm,
    kadd,
    kunload,

    kload,
    kload,
    kadd,
    kstore,

    kload,
    kloadimm,
    kjumpne,
    kstop)

[]int data = (
    42,
    56,
    0,
    0,

    int(&x),
    int(&y),
    0,
    int(&x),

    int(&x),
    100'000'000,
    1,
    0)

macro nextinstr = goto jumptable[code[pc]]

proc run=
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

proc main=
    run()
end
