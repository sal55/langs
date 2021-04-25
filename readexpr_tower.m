macro readunit = readassignment

function readassignment:unit p=
    int pos,opc

    if lx.symbol=namesym and nexttoken.symbol=assignsym then
        pos:=lx.pos
        p:=createname(lx.symptr)
        lex()
        lex()
        p:=createunit2(j_assign,p, readassignment())
        p.pos:=lx.pos
        return p
    fi

    p:=readorterms()

    if (opc:=lx.symbol)=assignsym then
        pos:=lx.pos
        lex()
        if lx.symbol=kemptysym then
            p:=createunit1(j_empty, p)
            lex()
        else
            p:=createunit2(j_assign,p,readassignment())
        fi
        p.pos:=pos
    fi
    return p
end

function readorterms:unit p=
    int pos

    p:=readandterms()

    while lx.symbol=orlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.genop:=orlto_op
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_orl,p,readandterms())
        p.genop:=orl_op
        p.pos:=pos
    od

    return p
end

function readandterms:unit p=
    int pos

    p:=readcmpterms()

    while lx.symbol=andlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.genop:=andlto_op
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_andl,p,readcmpterms())
        p.genop:=andl_op
        p.pos:=pos
    od

    return p
end

function readcmpterms:unit p=
    int pos,opc,n
    unit ulist,ulistx,q
    [4]byte genops

    p:=readinterms()

    if lx.symbol not in [eqsym,cmpsym] then
        return p
    fi

    ulist:=ulistx:=p
    p:=createunit1(j_cmpchain,p)
    n:=0
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
        p.genop:=genops[1]
        p.b:=q.nextunit
        q.nextunit:=nil
        p.hasb:=1
    else
        p.cmpgenop:=genops
    fi  

    return p
end

function readinterms:unit p=
    int pos,opc

    p:=readrangeterm()

    doswitch lx.symbol
    when insym, notinsym then
        opc:=lx.subcode

        pos:=lx.pos
        lex()

        p:=createunit2(j_bin,p,readrangeterm())
        p.genop:=opc
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readrangeterm:unit p=
    int pos,opc

    p:=readaddterms()

    if lx.symbol=rangesym then
        pos:=lx.pos
        lex()
        p:=createunit2(j_makerange,p,readaddterms())
        p.pos:=pos
    fi

    return p
end

function readaddterms:unit p=
    int pos,sym, tag, genop

    p:=readmulterms()

    doswitch sym:=lx.symbol
    when addsym, subsym, iandsym, iorsym, ixorsym,
            concatsym, appendsym, minsym, maxsym then
        pos:=lx.pos
        genop:=lx.subcode
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.genop:=symbolgentoops[sym]
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_bin,p,readmulterms())
        p.genop:=symbolgenops[sym]
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readmulterms:unit p=
    int pos,sym

    p:=readpowerterms()

    doswitch sym:=lx.symbol
    when mulsym, divsym, idivsym, iremsym, shlsym, shrsym then
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(j_binto,p,readassignment())
            p.genop:=symbolgentoops[sym]
            p.pos:=pos
            exit
        fi

        p:=createunit2(j_bin,p,readpowerterms())
        p.genop:=symbolgenops[sym]
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readpowerterms:unit p=
    int pos

    p:=readterm2()

    while lx.symbol=powersym do
        pos:=lx.pos
        lex()
        p:=createunit2(j_bin,p,readpowerterms())
        p.genop:=power_op
        p.pos:=pos
    od

    return p
end
