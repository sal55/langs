    function readgeneric:unit=
    # read generic construct; return chosen expr according to type of control expr
    # at '_Generic'
        unit pexpr,pmatch,p
        ref paramrec pm
        int m,t,def,oldingeneric,count
        symbol d

        lex()
        checksymbol(lbracksym)
        lex()
        oldingeneric:=ingeneric   # _Generic may be nested (never tried this)
        ingeneric:=1              # (this flags inhibits type promotions elsewhere, usually 0)
        pexpr:=readassignexpr()		# control expr
        ingeneric:=oldingeneric

        m:=pexpr.mode
        pmatch:=nil
        def:=0
        count:=0

        checksymbol(commasym)

        repeat                      # at comma
            lex()                   # skip comma
            if lx.symbol=kdefaultsym then
                if def then serror("generic/default twice") fi
                def:=1
                if count=0 then t:=-1 else t:=-2 fi
                lex()
            else
                t:=readcasttype(d,0,pm)
            fi
            checksymbol(colonsym)
            lex()
            p:=readassignexpr()

            if (t=-1 or t=m) then
                pmatch:=p
                ++count
            fi
        until lx.symbol<>commasym

        checksymbol(rbracksym)
        lex()
        if not pmatch then serror("Generic: no type match") fi
        if count>1 then serror("Generic: multiple types match") fi

        return pmatch
    end
