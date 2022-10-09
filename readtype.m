!Parse a named C type within a declaration like:
!   int a, *b, c[10];

!readtype() is called with the base type is already known, eg. `int` above, and passed as 'm';
! but the identifier is not yet known; for the above, the current token will be 'a' or '*' or 'c';
! it will be called separately for each variable

!It returns the identifier via 'd' and either the same or modified type as the return type
!For function types with parameter lists, those are returned via 'pm' (& means reference param)

function readtype(ref strec owner, &d, int m, ref paramrec &pm)int=
    [maxtypemods]int modtype
    [maxtypemods]ref void modvalue
    ref paramrec pmx
    int nmodifiers
    
    nmodifiers:=0
    pm:=nil

    readnamedtype(owner,d, modtype,modvalue,nmodifiers)

!now apply modifiers to base type:
    for i:=nmodifiers downto 1 do
        case modtype[i]
        when 'A' then                    # array
            m:=createarraymode(m,int(modvalue[i]))
        when 'R' then                    # pointer
            m:=createrefmode(m)
        when 'C' then                    # const
            m:=createconstmode(m)
        when 'F' then                    # function
            if i=1 then             !indicate to caller that this is a normal function
                pm:=modvalue[1]
            else                    !assume fu nction pointer of some sort
                m:=createprocmode(m,modvalue[i])
            fi
        esac
    od

    return m
end

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

!accumulate pointers
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
    when lsqsym then            !array
        lex()
        if lx.symbol=rsqsym then
            length:=0
        else
            pdim:=readassignexpr()
            if pdim.tag=j_const then
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
    enddocase

!now apply any pointers
    while nrefs do
        if fconst[nrefs] then
            modtype[++nmodifiers]:='C'
        fi
        modtype[++nmodifiers]:='R'
        --nrefs
    od
end
