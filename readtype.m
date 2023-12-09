! This is a comment
! `docase` is a looping `case` statement
! &-parameters are passed by reference
! I think that in this case:
!   modtype[] contains modifier codes  0/'A'/'F'/'C'/'R' for normal/Array/Function/Const/Pointer(Ref) I think...
!   Each the set of arrays is for the type of one named/unnamed entity.
!   `d` is either an ST reference for a named variable, or stays at nil (NULL) for unnamed ones, eg. for casts and fn params

proc readnamedtype(ref strec owner, &d, []int &modtype, []ref void &modvalue, int &nmodifiers)=
    int length
    [maxtypemods]int fconst
    int nrefs
    unit pdim

    d:=nil
    nrefs:=0

    if lx.symbol=kfnspecsym then
        lex()
    fi

    while lx.symbol=mulsym do           !pointer/qualifier loop
        ++nrefs
        fconst[nrefs]:=0
        lex()
        while lx.symbol=ktypequalsym do
            case lx.subcode
            when const_qual then
                fconst[nrefs]:=1
            when volatile_qual, restrict_qual then
            else
                serror("rnt1")
            esac
            lex()
        od
    od

    case lx.symbol
    when namesym then
        d:=lx.symptr
        lex()
    when lbracksym then
        lex()
        readnamedtype(owner,d,modtype,modvalue,nmodifiers)
        skipsymbol(rbracksym)
    esac

    docase lx.symbol
    when lsqsym then
        lex()
        if lx.symbol=rsqsym then
            length:=0
        else
            pdim:=readassignexpr()
            if pdim.tag=jconst then
                length:=pdim.value
            else
                serror("Can't do VLAs")
            fi
            checksymbol(rsqsym)
        fi
        if length<0 then terror("Negative array dim") fi
        lex()
        modtype[++nmodifiers]:='A'
        modvalue[nmodifiers]:=ref void(length)

    when lbracksym then         !fn params
        lex()
        modtype[++nmodifiers]:='F'
        modvalue[nmodifiers]:=readparams(owner)
    else
        exit
    end docase

!now apply any pointers
    while nrefs do
        if fconst[nrefs] then
            modtype[++nmodifiers]:='C'
        fi
        modtype[++nmodifiers]:='R'
        --nrefs
    od
end
