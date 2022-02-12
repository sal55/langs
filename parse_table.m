!Table-driven expression parser

function readunit:unit=
!a unit is an expression or statement only; not declarations
    return readfactor(8)
end

function readfactor(int level)unit=
    unit p,q,r
    int opc,opprio,lineno,isassign

    if level<=1 then
	      p:=readterm()
    else
        p:=readfactor(level-1)
    fi

    doswitch lx.symbol                            ! looping switch
    when opsym, assignsym, addrsym, rangesym then
        opc:=lx.subcode
        
        if nextlx.symbol = assignsym then         ! op:= augmented assignment
            lex()
            isassign:=1
            opprio:=jtagpriotable[j_assignx]
            opc:=getoptocode(opc)
        else
            opprio:=jtagpriotable[opc]
        fi
        if opprio<>level then exit fi

        lex()

        case opc
        when j_assign then
            q:=readunit()
        when j_power then
            q:=readfactor(level)
        else
            q:=readfactor(level-1)
        esac
        p:=createunit2(opc,p,q)

    else
        exit
    enddoswitch
    return p
end
