function readexpression:unit=
    return readterm1()
end

function readterm1:unit=
    unit p,q
    p:=readterm2()

    while lx.symbol=lbracksym do
        lex()
        if lx.symbol<>rbracksym then
            q:=readexpression()
        else
            q:=nil
        fi
        p:=createunit2(j_callfn,p,q)
        checksymbol(rbracksym)
        lex()
    end
    return p
end

function readterm2:unit=
    unit p

    if lx.symbol=namesym then
        p:=createname(lx.symptr)
        p.pos:=lx.pos
        lex()
    else
        serror("readterm?")
    end
    return p
end
