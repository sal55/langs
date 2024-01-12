function readdeclspec(ref strec owner,int &linkage)int=
!At first symbol of a declspec, or possible declspec
!read declspec and basetype
!return typecode for basetype, and linkage (static etc)
!if no declspec follows (usually eof) returns 0

    record declrec=
        int32 typeno                !not set, int, float, char, struct, union, enum etc
        byte isconst                !0, or 1 when const used (more than 1 allowed)
        byte isvolatile             !0, or 1 when volatile used
        byte isrestrict
        byte linkage                !0, or static_ss etc; only one allowed
        byte isinline               !1 when inline used
        byte isshort                !1 when short used
        byte islong                 !1 when long used (not short or long long)
        byte isllong                !1 when long long used (islong set to 0)
        byte issigned               !not set, signed
        byte isunsigned             !not set, unsigned
        byte isusertype             !1 if basetype set completely from typedef
                                    !so isshort/long etc or other basetype not allowed
    end
    declrec d
    unit p
    int t,mod,m,fstruct
    ref paramrec pm
    ref strec e

    memset(&d,0,d.bytes)
!clear d
    d.typeno:=tnotset

    fstruct:=mod:=0

    doswitch lx.symbol
    when ktypespecsym then
        switch lx.subcode
        when ts_int, ts_char, ts_float, ts_double, ts_bool, ts_void then
            if d.typeno<>tnotset then
                if fstruct then checksymbol(semisym)
                else goto tserror
                fi
            fi
            d.typeno:=typespectypes[lx.subcode]

        when ts_short then
            if d.isshort or d.islong or d.isllong then goto tserror fi
            d.isshort:=mod:=1
        when ts_long then
            if d.isllong or d.isshort then goto tserror
            elsif d.islong then
                d.islong:=0
                d.isllong:=1
            else
                d.islong:=1
            fi
            mod:=1

        when ts_signed then
            if d.issigned or d.isunsigned then goto tserror fi
            d.issigned:=mod:=1
        when ts_unsigned then
            if d.issigned or d.isunsigned then goto tserror fi
            d.isunsigned:=mod:=1
        else

    tserror:
            serror_s("declspec/ts #",typespecnames[lx.subcode])
        end switch
        lex()

    when ktypequalsym then
        case lx.subcode
        when const_qual then
            d.isconst:=1
        when volatile_qual then d.isvolatile:=1
        when restrict_qual then d.isrestrict:=1
        esac
        lex()

    when klinkagesym then
        if d.linkage then serror("Dual storage spec") fi
        d.linkage:=lx.subcode
        lex()

    when kfnspecsym then
        case lx.subcode
        when inline_fnspec then
            d.isinline:=1
        esac
        lex()
    when kstructsym,kunionsym then
        if d.typeno<>tnotset then serror("struct?") fi
        d.typeno:=readstructdecl(owner)
        d.isusertype:=1
        fstruct:=1

    when kenumsym then
        if d.typeno<>tnotset then serror("enum?") fi
        readenumdecl(owner)
        d.typeno:=ti32          !disregard enum 'type'; just use int
        d.isusertype:=1

    when namesym then           !should resolve to see if a user-type ...
                                ! ... unless a basetype already seen
        if d.typeno=tnotset and (m:=isusertype(owner))<>tnotset then
            if mod then         !unsigned etc without proper base type; assume name is not part o it
                d.typeno:=ti32
                exit
            fi
            d.typeno:=m
            d.isusertype:=1
            lex()
        else
            if d.typeno=tnotset and not mod then
                serror_s("Implicit decls not allowed: #",lx.symptr.name)
            fi

            if d.typeno=tnotset then d.typeno:=ti32 fi
            exit
        fi

    else
        exit
    end doswitch

    t:=(d.typeno<>tnotset|d.typeno|ti32)

    if not d.isusertype then                !otherwise everything should be set up
        case t
        when ti32 then
            if d.isshort then
                t:=(d.isunsigned|tu16|ti16)
            elsif d.islong then
                t:=(d.isunsigned|tu32|ti32)
            elsif d.isllong then
                t:=(d.isunsigned|tu64|ti64)
            elsif d.isunsigned then
                t:=tu32
            fi
        when ti8 then
            if d.isshort or d.islong or d.isllong then serror("char decl?") fi
            t:=(d.isunsigned|tu8|ti8)
        when tr64 then
            if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("dbl decl?") fi

        else
            if mod then serror("declspec/float") fi
        esac
    fi

    if d.isconst then
        t:=createconstmode(t)
    fi

    linkage:=d.linkage
    return t
end
