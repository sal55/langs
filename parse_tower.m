!expression parser using a tower of functions
!a 'unit' is an expression or statement
!The code on lines 10 to 28 optimises for expressions with a single term, 
! or assignments with a single term on the rhs

function readunit:unit p=
    unit pt

!   pt:=nil
    pt:=readterm2()

    if jisexpr[pt.tag]=0 then      ! read a statement that never returns a value
        return pt
    fi

    if endsexpr[lx.symbol] then    !lone term with no binary op following
        return pt
    fi

    if lx.symbol=assignsym then
        lex()
        p:=readterm2()
        if endsexpr[lx.symbol] then      ! assignment of the form term := term
            p:=createunit2(j_assign, pt, p)
            return p
        fi
        p:=createunit2(j_assign, pt, readassignment(p))
    else
        p:=readassignment(pt)
    fi

    while lx.symbol=pipesym do
        lex()
        p:=createunit2(j_callfn, readassignment(), p)
    od

    return p
end

function readassignment(unit pt=nil)unit p=
    int pos,opc

    p:=readorterms(pt)

    if (opc:=lx.symbol) in [assignsym, deepcopysym] then
        pos:=lx.pos
        lex()
        if lx.symbol=kemptysym then
            p:=createunit1(j_empty, p)
            lex()
        else
            p:=createunit2(j_assign,p,readassignment(nil))
        fi
        p.pos:=pos
    fi
    return p
end

function readorterms(unit pt=nil)unit p=
    int pos

    p:=readandterms(pt)

    while lx.symbol=orlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.pclop:=korlto
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_orl,p,readandterms())
        p.pclop:=korl
        p.pos:=pos
    od

    return p
end

function readandterms(unit pt=nil)unit p=
    int pos

    p:=readcmpterms(pt)

    while lx.symbol=andlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.pclop:=kandlto
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_andl,p,readcmpterms())
        p.pclop:=kandl
        p.pos:=pos
    od

    return p
end

function readcmpterms(unit pt=nil)unit p=
    int pos,opc,n
    unit ulist,ulistx,q
    var [4]byte genops

    p:=readinterms(pt)

    if lx.symbol not in [eqsym,cmpsym] then
        return p
    fi

    ulist:=ulistx:=p
    p:=createunit1(j_cmpchain,p)
    n:=0                !n counts operand after the first
    clear genops

    doswitch lx.symbol
    when eqsym, cmpsym then
        ++n
        if n>genops.len then serror("cmpchain: Too many items") fi
        genops[n]:=lx.subcode

        pos:=lx.pos
        lex()

        q:=readinterms()
        addlistunit(&ulist,&ulistx,q)
        q.pos:=pos
    else
        exit
    end doswitch

    if n=1 then
        p.tag:=j_cmp
        q:=p.a
        p.pclop:=genops[1]
        p.b:=q.nextunit
        q.nextunit:=nil
        p.hasb:=1
    else
        p.cmpgenop:=genops
    fi

    return p
end

function readinterms(unit pt=nil)unit p=
    int pos,opc

    p:=readrangeterm(pt)

    doswitch lx.symbol
    when insym, notinsym then
        opc:=lx.subcode

        pos:=lx.pos
        lex()

        p:=createunit2(j_bin,p,readrangeterm())
        p.pclop:=opc
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readrangeterm(unit pt=nil)unit p=
    int pos,opc

    p:=readaddterms(pt)

    if lx.symbol=rangesym then
        pos:=lx.pos
        lex()
        p:=createunit2(j_makerange,p,readaddterms())
        p.pos:=pos
    fi

    return p
end

function readaddterms(unit pt=nil)unit p=
    int pos,sym, tag, genop

    p:=readmulterms(pt)

    doswitch sym:=lx.symbol
    when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym, appendsym then
        pos:=lx.pos
        genop:=lx.subcode
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.pclop:=symbolgentoops[sym]
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_bin,p,readmulterms())
        p.pclop:=symbolgenops[sym]
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readmulterms(unit pt=nil)unit p=
    int pos,sym

    p:=readpowerterms(pt)

    doswitch sym:=lx.symbol
    when mulsym, divsym, idivsym, iremsym, shlsym, shrsym then
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.pclop:=symbolgentoops[sym]
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_bin,p,readpowerterms())
        p.pclop:=symbolgenops[sym]
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readpowerterms(unit p=nil)unit=
    int pos

    if p=nil then
        p:=readterm()
    fi

    while lx.symbol=powersym do
        pos:=lx.pos
        lex()
        p:=createunit2(j_bin,p,readpowerterms())
        p.pclop:=kpower
        p.pos:=pos
    od

    return p
end
