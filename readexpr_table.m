function readunit:unit=
    return readfactor(8)
end

function readfactor(int level)unit=
    unit p,q,r
    int opc,opprio,lineno,isassign

    if level<=1 then
        p:=readterm2()
    else
        p:=readfactor(level-1)
    fi

    doswitch lx.symbol
    when opsym, assignsym, addrsym, rangesym then
        opc:=lx.subcode
        lineno:=lx.lineno

        if nextlx.symbol=assignsym then
            lex()
            isassign:=1
            opprio:=jtagpriotable[j_assignx]
            opc:=getoptocode(opc)
        else
            isassign:=opc=j_assignx
            opprio:=jtagpriotable[opc]
        fi
        if opprio<>level then exit fi

        lex()

        if isassign then                !assign is right-to-left but also special
            q:=readunit()
        elsif opc=j_power then          !power is right-to-left
            q:=readfactor(level)
        else
            q:=readfactor(level-1)
        fi
        p:=createunit2(opc,p,q)
        p^.lineno:=lineno

    else
        exit
    enddoswitch
    return p
end
