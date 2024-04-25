!a manual translation of the small pascal compiler from:
! http://pascal.hansotten.com/uploads/pascals/pascal-s%20snepscheut.pas


enumdata []ichar opcodenames =
    (kadd,      $),
    (kneg,      $),
    (kmul,      $),
    (kdivd,     $),
    (kremd,     $),
    (kdiv2,     $),
    (krem2,     $),
    (keqli,     $),
    (kneqi,     $),
    (klssi,     $),
    (kleqi,     $),
    (kgtri,     $),
    (kgeqi,     $),
    (kdupl,     $),
    (kswap,     $),
    (kandb,     $),
    (korb,      $),
    (kload,     $),
    (kstor,     $),
    (khalt,     $),
    (kwri,      $),
    (kwrc,      $),
    (kwrl,      $),
    (krdi,      $),
    (krdc,      $),
    (krdl,      $),
    (keol,      $),

    (kldc,      $),     !instrs from here on have an inline operand
    (kldla,     $),
    (kldl,      $),
    (kldg,      $),
    (kstl,      $),
    (kstg,      $),
    (kmove,     $),
    (kcopy,     $),
    (kaddc,     $),
    (kmulc,     $),
    (kjump,     $),
    (kjumpz,    $),
    (kcall,     $),
    (kadjs,     $),
    (ksets,     $),
    (kexit,     $),
end

record instrrec =
    int op
    int a
end

const codemax = 2000
const memmax = 1048576

[0..codemax]instrrec code
[0..memmax]int m

const imax     = 100    ! {length of identifier table }
const tmax     = 100    ! { length of type table }
const lmax     = 10     ! { maximum level }
const al       = 10     ! { length of identifiers }
const fabs     = 0      ! { standard functions }
const fsqr     = 1
const fodd     = 2
const fchr     = 3
const ford     = 4
const fwrite   = 5
const fwriteln = 6
const fread    = 7
const freadln  = 8
const feoln    = 9

const intip    = 1
const booltip  = 2
const chartip  = 3

enumdata []ichar words =
    (ident,     $),
    (number,    $),
    (string,    $),
    (plus,      $),
    (minus,     $),
    (star,      $),
    (lbrack,    $),
    (rbrack,    $),
    (colon,     $),
    (eql,       $),
    (neq,       $),
    (lss,       $),
    (leq,       $),
    (gtr,       $),
    (geq,       $),
    (lparen,    $),
    (rparen,    $),
    (comma,     $),
    (semicolon, $),
    (period,    $),
    (becomes,   $),
    (beginsym,  "begin"),
    (endsym,    "end"),
    (ifsym,     "if"),
    (thensym,   "then"),
    (elsesym,   "else"),
    (whilesym,  "while"),
    (dosym,     "do"),
    (casesym,   "case"),
    (repeatsym, "repeat"),
    (untilsym,  "until"),
    (forsym,    "for"),
    (tosym,     "to"),
    (downtosym, "downto"),
    (notsym,    "not"),
    (divsym,    "div"),
    (modsym,    "mod"),
    (andsym,    "and"),
    (orsym,     "or"),
    (constsym,  "const"),
    (varsym,    "var"),
    (typesym,   "type"),
    (arraysym,  "array"),
    (ofsym,     "of"),
    (recordsym, "record"),
    (progsym,   "program"),
    (funcsym,   "function"),
    (procsym,   "procedure"),
end

enumdata []ichar idkindnames =
    (konst,     $),
    (varbl,     $),
    (field,     $),
    (tipe,      $),
    (funkt,     $),
end

enumdata []ichar tpkindnames =
    (simple,    $),
    (arrays,    $),
    (records,   $),
end

filehandle inp

int ch           ! { last character read }
int cc           ! { character count }
int ll           ! { line length }
int lineno
[512]char line   ! { present input line }
int sym          ! { last symbol read }
ichar id         ! { last identifier read }
int num          ! { last number read }
[512]char str    ! { last string read }
int slen         ! { length of last string }
int cx           ! { code index }
int lev          ! { procedure nesting level }
int dx           ! { offset in stack }
int labeled      ! { next instruction has label }
[-1..lmax]int namelist

record idrec =
    ichar name
    int link, tip
    int kind
    union
        int val                                             !konst
        struct
            int vlevel, vadr, refpar                        !varbl
        end
        int offset                                          !field
        struct
            int flevel, fadr, lastpar, resultadr, inside    !funkt
        end
    end
end

record typerec =
    int size
    int kind
    union
        struct
            int low, high, elemtip      !arrays
        end
        int fields                      !records
    end
end

[0..imax]idrec itab     ! { identifier table }
[1..tmax]typerec ttab   ! { type table }
int ix, tx       ! { indices in tables }
ref idrec pix               !pointer to itab[ix] set by 'enter'

proc serror(ichar m1, m2="", m3="") =
    println line
    to cc-2 do print " " od
    println "^"
    print "Error: "

    if m3^ then
        fprint m1, m2, m3
    elsif m2^ then
        fprint m1, m2
    else
        print m1
    fi
    print " on line:",lineno
    println
    stop 1
end

proc error(int n)=
    if n>words.len then
        serror(strint(n))
    else
        serror(words[n])
    fi
end

proc myerror(ichar mess)=
    println "Runime error",mess
    stop 1
end

proc getch =
    if cc=ll then
        if myeof(inp) then
            error(100)
        fi

        ll:=cc:=0
        readln @inp
        readstr(line, 'L')
        ll:=strlen(line)
        ++lineno
    fi
    ++cc
    ch:=line[cc]
end

proc getsym =
    int k, s, strend
    [256]char str

    while ch in [' ', '\t'] do getch() od

    switch ch
    when 'a'..'z', 'A'..'Z' then
        k:=0
        repeat
            if k<>al then
                str[++k]:=ch
            fi
            getch()
        until ch not in 'a'..'z' and ch not in 'A'..'Z' and ch not in '0'..'9'
        str[++k]:=0

        id:=pcm_copyheapstring(str)

        sym:=ident
        for s:=beginsym to procsym do
            if eqstring(words[s], id) then
                sym:=s
                exit
            fi
        od

    when '0'..'9' then
        num:=0; sym:=number
        repeat
            num:=num*10 + (ch-'0')
            getch()
        until ch not in '0'..'9'

    when ':' then
        getch()
        if ch='=' then
            getch()
            sym:=becomes
        else
            sym:=colon
        fi

    when '>' then
        getch()
        if ch='=' then
            getch()
            sym:=geq
        else
            sym:=gtr
        fi

    when '<' then
        getch()
        if ch='=' then
            getch()
            sym:=leq
        elsif ch='>' then
            getch()
            sym:=neq
        else
            sym:=lss
        fi

    when '.' then
        getch()
        if ch='.' then
            getch()
            sym:=colon
        else
            sym:=period
        fi

    when '\'' then
        slen:=0; strend:=false; sym:=string

        repeat
            if cc=ll then error(101) fi
            getch()
            if ch='\'' then
                getch()
                if ch='\'' then
                    str[++slen]:=ch
                else
                    strend:=true
                fi
            else
                str[++slen]:=ch
            end
        until strend
        if slen=0 then error(102) fi

    when '+' then
        getch()
        sym:=plus

    when '-' then
        getch()
        sym:=minus

    when '*' then
        getch()
        sym:=minus

    when '(' then
        getch()
        sym:=lparen

    when ')' then
        getch()
        sym:=rparen

    when '[' then
        getch()
        sym:=lbrack

    when ']' then
        getch()
        sym:=rbrack

    when '=' then
        getch()
        sym:=eql

    when ',' then
        getch()
        sym:=comma

    when ';' then
        getch()
        sym:=semicolon

    when '{' then
        repeat
            getch()
        until ch='}'
        getch()
        getsym()

    else
        error(103)
    end switch
end

proc check(int s) =
    if sym<>s then
        serror("Expected '#' not '#'",words[s], words[sym])
    fi
end

proc skip(int s) =
    check(s)
    getsym()
end

proc enter(ichar id, int k, t) =
    int j
    if ix=imax then error(104) fi
    ++ix
    itab[0].name:=id
    j:=namelist[lev]

    while itab[j].name<>id do
        j:=itab[j].link
    end

    if j<>0 then error(105) fi

    pix:=&itab[ix]
    pix.name:=id
    pix.link:=namelist[lev]
    pix.tip:=t
    pix.kind:=k
    namelist[lev]:=ix
end

func position:int =
    int i, j
    itab[0].name:=id
    i:=lev
    repeat
        j:=namelist[i]
        while itab[j].name<>id do
            j:=itab[j].link
        od
        --i
    until (i<-1) or (j<>0)
    if j=0 then error(106) fi
    return j
end

proc gen(instrrec i) =
    ref instrrec p
    int op

    case i.op
    when kdupl, keol, kldc, kldla, kldl, kldg then
        --dx

    when kneg, kdiv2, krem2, kswap, kload, khalt, kwrl, krdl,
            kaddc, kmulc, kjump, kcall, ksets, kexit then

    when kadd, kmul, kdivd, kremd, keqli, kneqi, klssi, kleqi, kgtri,
            kgeqi, kandb, korb, kwrc, krdi, krdc, kstl, kstg, kjumpz then
        ++dx
    when kstor, kwri, kmove then
        dx+:=2

    when kcopy then
        dx:=dx-i.a+1

    when kadjs then
        dx:=dx+i.a

    esac

    if i.op not in [kaddc, kadjs] and i.a=0 or i.op=kmulc and i.a=1 then
        p:=&code[cx-1]

        if labeled then
            code[cx++]:=i
            labeled:=false

        elsif p.op=kldc and i.op=kadd then
            p.op:=kaddc

        elsif p.op=kldc and i.op=kmul then
            p.op:=kmulc

        elsif p.op=kldc and i.op=kneg then
            p.a:=-p.a

        elsif p.op=kldc and p.a=2 and i.op=kdivd then
            p.op:=kdiv2

        elsif p.op=kldc and p.a=2 and i.op=kremd then
            p.op:=krem2

        elsif p.op=kldc and i.op=kstor then
            p.op:=kstg

        elsif p.op=kldc and i.op=kload then
            p.op:=kldg

        elsif p.op=kldla and i.op=kstor then
            p.op:=kstl

        elsif p.op=kldla and i.op=kload then
            p.op:=kldl

        else
            code[cx++]:=i
        fi
    fi
end

proc gen0(int op) =
    instrrec i
    i.op:=op
    gen(i)
end

proc gen1(int op, a) =
    instrrec i
    i.op:=op
    i.a:=a
    gen(i)
end

func codelabel:int =
    labeled:=true
    return cx
end

proc address(int lv, ad) =
    case lv
    when 0 then
        gen1(kldc, ad)

    when lev then
        gen1(kldla, ad-dx)

    else
        gen1(kldl, -dx)
        while lv+1<>lev do
            gen0(kload)
            ++lv
        od
        gen1(kaddc, ad)
    esac
end

proc addressvar(int refx) =
    ref idrec p:=&itab[refx]

    address(p.vlevel, p.vadr)
    if p.refpar then
        gen0(kload)
    fi
end

proc mustbe(int x, y) =
    if x<>y then
        if ttab[x].kind=arrays and ttab[y].kind=arrays and
                ttab[x].low=ttab[y].low and ttab[x].high=ttab[y].high then
            mustbe(ttab[x].elemtip, ttab[y].elemtip)
        else
            error(107)
        fi
    fi
end

proc selector(int &t, &refx) =
    int j, x
    t:=itab[refx].tip
    getsym()

    if sym in [period, lbrack] then
        addressvar(refx)
        refx:=0
        while sym in [period, lbrack] do
            case sym
            when period then
                if ttab[t].kind<>records then error(108) fi
                getsym()
                check(ident)
                j:=ttab[t].fields
                itab[0].name:=id
                while itab[j].name<>id do j:=itab[j].link od

                if j=0 then error(109) fi
                gen1(kaddc, itab[j].offset)
                t:=itab[j].tip
                getsym()

            when lbrack then
                repeat
                    if ttab[t].kind<>arrays then error(110) fi
                    getsym()
                    expression(x)
                    mustbe(intip, x)
                    gen1(kaddc, -ttab[t].low)
                    t:=ttab[t].elemtip
                    gen1(kmulc, ttab[t].size)
                    gen0(kadd)
                until sym<>comma
                skip(rbrack)
            esac
        od
    fi
end

proc varpar(int &t) =
    int j
    check(ident)
    j:=position()
    selector(t, j)
    if j<>0 then addressvar(j) fi
end

proc standfct(int n) =
    int x, l

    case n
    when fabs then
        skip(lparen)
        expression(x)
        mustbe(intip, x)
        gen0(kdupl)
        gen1(kldc, 0)
        gen0(klssi)
        l:=codelabel()
        gen1(kjumpz, 0)
        gen0(kneg)
        code[l].a:=codelabel()
        skip(rparen)

    when fsqr then
        skip(lparen)
        expression(x)
        mustbe(intip, x)
        gen0(kdupl)
        gen0(kmul)
        skip(rparen)

    when fodd then
        skip(lparen)
        expression(x)
        mustbe(intip, x)
        gen0(krem2)
        skip(rparen)
  
    when fchr then
        skip(lparen)
        expression(x)
        mustbe(intip, x)
        skip(rparen)

    when ford then
        skip(lparen)
        expression(x)
        mustbe(chartip, x)
        skip(rparen)

    when fwrite, fwriteln then
        if n=fwrite then check(lparen) fi
        if sym=lparen then
            repeat
                getsym()
                if sym=string then
                    for x:=1 to slen do
                        gen1(kldc, str[x])
                        gen0(kwrc)
                    od
                    getsym()
                else
                    expression(x)
                    if sym=colon then
                        mustbe(intip, x)
                        getsym()
                        expression(x)
                        mustbe(intip,x)
                        gen0(kwri)
                    elsif x=intip then
                        gen1(kldc, 8)
                        gen0(kwri)
                    elsif x=chartip then
                        gen0(kwrc)
                    else
                        error(111)
                    fi
                fi
            until sym<>comma
            skip(rparen)
        fi
        if n=fwriteln then gen0(kwrl) fi

    when fread, freadln then
        if n=fread then check(lparen) fi
        if sym=lparen then
            repeat
                getsym()
                varpar(x)
                if x=intip then
                    gen0(krdi)
                else
                    if x=chartip then
                        gen0(krdc)
                    else
                        error(112)
                    fi
                fi
            until sym<>comma
            skip(rparen)
        fi

        if n=freadln then gen0(krdl) fi

    when feoln then
        gen0(keol)

    esac

end

proc funcall(int i) =
    int d, p, x
    ref idrec pid:=&itab[i]

    getsym()
    if pid.flevel<0 then
        standfct(pid.fadr)
    else
        if pid.tip<>0 then gen1(kldc, 0)    fi
        p:=i
        d:=dx
        if sym=lparen then
            repeat
                getsym()
                if p=pid.lastpar then error(113) fi
                ++p
                if itab[p].refpar then
                    varpar(x)
                else
                    expression(x)
                    if ttab[x].kind<>simple then gen1(kcopy, ttab[x].size) fi
                fi
                mustbe(itab[p].tip, x)
            until sym<>comma
            skip(rparen)
        fi
        if p<>pid.lastpar then error(114) fi
        if pid.flevel<>0 then address(pid.flevel, 0) fi
        gen1(kcall, pid.fadr)
        dx:=d
    fi
end

proc factor(int &t) =
    int i

    case sym
    when ident then
        i:=position()
        t:=itab[i].tip

        case itab[i].kind
        when konst then
            getsym()
            gen1(kldc, itab[i].val)

        when varbl then
            selector(t, i)
            if i<>0 then addressvar(i) fi
            if ttab[t].kind=simple then gen0(kload) fi

        when funkt then
            if t=0 then
                error(115)
            else
                funcall(i)
            fi

        when tipe then
            error(116)

        esac

    when number then
        gen1(kldc, num)
        t:=intip
        getsym()

    when lparen then
        getsym()
        expression(t)
        skip(rparen)

    when notsym then
        getsym()
        factor(t)
        mustbe(booltip, t)
        gen0(kneg)
        gen1(kaddc, 1)

    elsif sym=string and slen=1 then
        gen1(kldc, str[1])
        t:=chartip
        getsym()

    else
        error(117)
    esac
end

proc term(int &x) =
    int y

    factor(x)

    while sym in [andsym, star, divsym, modsym] do
        if sym=andsym then
            mustbe(booltip, x)
        else
            mustbe(intip, x)
        fi

        case sym
        when star then
            getsym()
            factor(y)
            gen0(kmul)

        when divsym then
            getsym()
            factor(y)
            gen0(kdivd)

        when modsym then
            getsym()
            factor(y)
            gen0(kremd)

        when andsym then
            getsym()
            factor(y)
            gen0(kandb)
        esac

        mustbe(x, y)
    od
end

proc simplexpression(int &x) =
    int y
    case sym
    when plus then
        getsym()
        term(x)
        mustbe(intip, x)

    when minus then
        getsym()
        term(x)
        mustbe(intip, x)
        gen0(kneg)
    else
        term(x)
        while sym in [orsym, plus, minus] do
            mustbe((sym=orsym|booltip|intip), x)
            case sym
            when plus then
                getsym()
                term(y)
                gen0(kadd)

            when minus then
                getsym()
                term(y)
                gen0(kneg)
                gen0(kadd)

            when orsym then
                getsym()
                term(y)
                gen0(korb)
            esac
        od
        mustbe(x, y)
    esac
end

proc expression(int &x) =
    int op, y

    simplexpression(x)

    if sym in [eql, neq, lss, leq, gtr, geq] then
        if ttab[x].kind<>simple then error(118) fi
        op:=sym
        getsym()
        simplexpression(y)
        mustbe(x, y)
        case op
        when eql then gen0(keqli)
        when neq then gen0(kneqi)
        when lss then gen0(klssi)
        when leq then gen0(kleqi)
        when gtr then gen0(kgtri)
        when geq then gen0(kgeqi)
        esac
        x:=booltip
    fi
end

proc statement =
    int i, j, t, x
    ref idrec pid

    case sym
    when ident then
        i:=position()
        pid:=&itab[i]

        case pid.kind
        when varbl then
            selector(t, i)
            skip(becomes)
            expression(x)
            mustbe(t, x)
            if i=0 then
                gen0(kswap)
            else
                addressvar(i)
            fi
            if ttab[t].kind=simple then
                gen0(kstor)
            else
                gen1(kmove, ttab[t].size)
            fi

        when funkt then
            if pid.tip=0 then
                funcall(i)
            else
                if not pid.inside then error(119) fi
                getsym()
                skip(becomes)
                expression(x)
                mustbe(pid.tip, x)
                address(pid.flevel+1, pid.resultadr)
                gen0(kstor)
            fi

        else
            error(120)
        esac

    when ifsym then
        getsym()
        expression(t)
        mustbe(booltip, t)
        skip(thensym)
        i:=codelabel()
        gen1(kjumpz, 0)
        statement()
        if sym=elsesym then
            getsym()
            j:=codelabel()
            gen1(kjump, 0)
            code[i].a:=codelabel()
            i:=j
            statement()
        fi

        code[i].a:=codelabel()

    when whilesym then
        getsym()
        i:=codelabel()
        expression(t)
        mustbe(booltip, t)
        skip(dosym)
        j:=codelabel()
        gen1(kjumpz, 0)
        statement()
        gen1(kjump, i)
        code[j].a:=codelabel()

    when repeatsym then
        i:=codelabel()
        repeat
            getsym()
            statement()
        until sym<>semicolon
        skip(untilsym)
        expression(t)
        mustbe(booltip, t)
        gen1(kjumpz, i)

    when beginsym then
        repeat
            getsym()
            statement()
        until sym<>semicolon
        skip(endsym)

    esac
end

proc constant(int &c, &t) =
    int i, s

    if sym=string and slen=1 then
        c:=str[1]
        t:=chartip
    else
        case sym
        when plus then
            getsym()
            s:=1
        when minus then
            getsym()
            s:=-1
        else
            s:=0
        esac

        case sym
        when ident then
            i:=position()
            if itab[i].kind<>konst then error(121) fi
            c:=itab[i].val
            t:=itab[i].tip

        when number then
            c:=num
            t:=intip
        else
            error(122)
        esac

        if s<>0 then
            mustbe(t, intip)
            c:=c*s
        fi
    fi
    getsym()
end

proc constdeclaration =
    ichar a
    int t, c

    a:=id
    getsym()
    skip(eql)
    constant(c, t)
    skip(semicolon)
    enter(a, konst, t)
    itab[ix].val:=c
end

proc arraytyp(int &t) =
    int x
    ref typerec tt:=&ttab[t]

    tt.kind:=arrays
    getsym()
    constant(tt.low, x)
    mustbe(intip, x)
    skip(colon)
    constant(tt.high, x)
    mustbe(intip, x)

    if tt.low>tt.high then error(123) fi

    if sym=comma then
        arraytyp(tt.elemtip)
    else
        skip(rbrack)
        skip(ofsym)
        typ(tt.elemtip)
    fi

    tt.size:=(tt.high-tt.low+1)*ttab[tt.elemtip].size
end

proc typ(int &t) =
    int i, j, sz, ft

    if sym=ident then
        i:=position()
        if itab[i].kind<>tipe then error(124) fi
        t:=itab[i].tip
        getsym()
    else
        if tx=tmax then error(125) fi
        t:=++tx
        if sym=arraysym then
            getsym()
            check(lbrack)
            arraytyp(t)
        else
            skip(recordsym)
            if lev=lmax then error(126) fi
            ++lev
            namelist[lev]:=0
            check(ident)
            sz:=0
            repeat
                enter(id, field, 0)
                i:=ix
                getsym()
                while sym=comma do
                    getsym()
                    check(ident)
                    enter(id, field, 0)
                    getsym()
                od
                j:=ix
                skip(colon)
                typ(ft)
                repeat
                    itab[i].tip:=ft
                    itab[i].offset:=sz
                    sz:=sz+ttab[ft].size
                    ++i
                until i>j
                if sym=semicolon then
                    getsym()
                else
                    check(endsym)
                fi
            until sym<>ident

            ttab[t].size:=sz
            ttab[t].kind:=records
            ttab[t].fields:=namelist[lev]
            ++lev
            skip(endsym)
        fi
    fi
end

proc typedeclaration =
    ichar a
    int t

    a:=id
    getsym()
    skip(eql)
    typ(t)
    skip(semicolon)
    enter(a, tipe, t)
end

proc vardeclaration =
    int p, q, t

    enter(id, varbl, 0)
    p:=ix
    getsym()

    while sym=comma do
        getsym()
        check(ident)
        enter(id, varbl, 0)
        getsym()
    od

    q:=ix
    skip(colon)
    typ(t)
    skip(semicolon)

    ref idrec pid:=&itab[p]
    repeat
        pid.vlevel:=lev
        dx:=dx-ttab[t].size
        pid.tip:=t
        pid.vadr:=dx
        pid.refpar:=false
        ++p
    until p>q
end

proc paramlist(int &p, &ps) =
    int r, t
    if sym=varsym then
        r:=true
        getsym()
    else
        r:=false
    fi

    check(ident)
    p:=ix
    enter(id, varbl, 0)
    getsym()

    while sym=comma do
        getsym()
        check(ident)
        enter(id, varbl, 0)
        getsym()
    od

    skip(colon)
    check(ident)
    typ(t)

    while p<ix do
        ++p
        itab[p].tip:=t
        itab[p].refpar:=r
        if r then
            ++ps
        else
            ps+:=ttab[t].size
        fi
    od
end

proc funcdeclaration(int isf) =
    int f, p, ps, odx
    ref idrec pid

    getsym()
    check(ident)
    enter(id, funkt, 0)
    getsym()
    f:=ix
    itab[f].flevel:=lev
    itab[f].fadr:=codelabel()
    gen1(kjump, 0)

    if lev=lmax then error(127) fi
    ++lev
    namelist[lev]:=0
    ps:=1
    odx:=dx
    if sym=lparen then
        repeat
            getsym()
            paramlist(p, ps)
        until sym<>semicolon
        skip(rparen)
    fi

    if lev>1 then
        dx:=-1
    else
        dx:=0
    fi
    itab[f].resultadr:=ps
    p:=f

    while p<ix do
        ++p
        pid:=&itab[p]
        if pid.refpar then
            --ps
        else
            ps:=ps-ttab[pid.tip].size
            pid.vlevel:=lev
            pid.vadr:=ps
        fi
    od

    if isf then
        skip(colon)
        check(ident)
        typ(itab[f].tip)
        if ttab[itab[f].tip].kind<>simple then error(128) fi
    fi

    skip(semicolon)
    itab[f].lastpar:=ix
    itab[f].inside:=true
    block(itab[f].fadr)
    itab[f].inside:=false
    gen1(kexit, itab[f].resultadr-dx)
    --lev
    dx:=odx
    skip(semicolon)
end

proc block(int l) =
    int d, odx, oix

    odx:=dx
    oix:=ix

    if sym=constsym then
        getsym()
        check(ident)
        repeat constdeclaration() until sym<>ident
    end

    if sym=typesym then
        getsym()
        check(ident)
        repeat typedeclaration() until sym<>ident
    end

    if sym=varsym then
        getsym()
        check(ident)
        repeat vardeclaration() until sym<>ident
    end

    while sym in [funcsym, procsym] do funcdeclaration(sym=funcsym) od

    if l+1=codelabel() then
        --cx
    else
        code[l].a:=codelabel()
    fi

    if lev=0 then
        gen1(ksets, dx)
    else
        d:=dx-odx
        dx:=odx
        gen1(kadjs, d)
    fi

    statement()

    if lev<>0 then
        gen1(kadjs, odx-dx)
        ix:=oix
    fi
end

proc listcode =
    for i:=0 to cx-1 do
        print i,":   ", opcodenames[code[i].op]+1
        if code[i].op>=kldc then
            print "\t",code[i].a
        fi
        println
    od
end

proc interpret =
    int pc, sp, j, k, n, h, c
    instrrec i

    pc:=0
    h:=false

    repeat
        i:=code[pc]
        ++pc

        case i.op
        when kadd  then
            m[sp+1]:=m[sp+1]+m[sp]
            ++sp

        when kneg  then
            m[sp]:=-m[sp]

        when kmul  then
            m[sp+1]:=m[sp+1]*m[sp]
            ++sp

        when kdivd then
            m[sp+1]:=m[sp+1] / m[sp]
            ++sp

        when kremd then
            m[sp+1]:=m[sp+1] rem m[sp]
            ++sp

        when kdiv2 then
            m[sp]:=m[sp] % 2

        when krem2 then
            m[sp]:=m[sp] rem 2

        when keqli then
            m[sp+1]:=m[sp+1]=m[sp]
            ++sp

        when kneqi then
            m[sp+1]:=m[sp+1]<>m[sp]
            ++sp

        when klssi then
            m[sp+1]:=m[sp+1]<m[sp]
            ++sp

        when kleqi then
            m[sp+1]:=m[sp+1]<=m[sp]
            ++sp

        when kgtri then
            m[sp+1]:=m[sp+1]>m[sp]
            ++sp

        when kgeqi then
            m[sp+1]:=m[sp+1]>=m[sp]
            ++sp

        when kdupl then
            --sp
            m[sp]:=m[sp+1]

        when kswap then
            swap(m[sp], m[sp+1])

        when kandb then
            if m[sp]=0 then m[sp+1]:=0 fi
            ++sp

        when korb  then
            if m[sp]=1 then m[sp+1]:=1 fi
            ++sp

        when kload then
            m[sp]:=m[m[sp]]

        when kstor then
            m[m[sp]]:=m[sp+1]
            sp+:=2

        when khalt then
            h:=true

        when kwri  then
            print m[sp+1]
            sp+:=2

        when kwrc  then
            print char(m[sp])
            ++sp

        when kwrl  then
            println

        when krdi  then
            read m[m[sp]]
            ++sp

        when krdc  then
            m[m[sp]]:=c
            ++sp

        when krdl  then
            myerror("RDL")

        when keol  then
            --sp
            myerror("eol")
!           m[sp]:=eoln(input)

        when kldc  then
            --sp
            m[sp]:=i.a
        when kldla then
            --sp
            m[sp]:=sp+1+i.a

        when kldl  then
            --sp
            m[sp]:=m[sp+1+i.a]

        when kldg  then
            --sp
            m[sp]:=m[i.a]

        when kstl  then
            m[sp+i.a]:=m[sp]
            ++sp

        when kstg  then
            m[i.a]:=m[sp]
            ++sp

        when kmove then
            k:=m[sp]
            j:=m[sp+1]
            sp+:=2
            n:=i.a
            repeat --n; m[k+n]:=m[j+n] until n=0

        when kcopy then
            j:=m[sp]
            n:=i.a
            sp:=sp-n+1
            repeat --n; m[sp+n]:=m[j+n] until n=0

        when kaddc then
            m[sp]:=m[sp]+i.a

        when kmulc then
            m[sp]:=m[sp]*i.a

        when kjump then
            pc:=i.a

        when kjumpz then
            if m[sp]=0 then pc:=i.a fi
            ++sp

        when kcall then
            --sp
            m[sp]:=pc
            pc:=i.a

        when kadjs then
            sp:=sp+i.a

        when ksets then
            sp:=i.a

        when kexit then
            pc:=m[sp]
            sp+:=i.a
        esac
    until h
end

proc init =
    ttab[intip].size:=1; ttab[intip].kind:=simple
    ttab[chartip].size:=1; ttab[chartip].kind:=simple
    ttab[booltip].size:=1; ttab[booltip].kind:=simple

    tx:=3; namelist[-1]:=0; lev:=-1; ix:=0

    enter("false", konst, booltip); pix.val:=false
    enter("true", konst, booltip); pix.val:=true
    enter("maxint", konst, intip);   pix.val:=32767
    enter("integer", tipe, intip)
    enter("char", tipe, chartip)
    enter("boolean", tipe, booltip)

    enter("abs", funkt, intip)
    pix.flevel:=-1; pix.fadr:=fabs; pix.inside:=false

    enter("sqr", funkt, intip)
    pix.flevel:=-1; pix.fadr:=fsqr; pix.inside:=false

    enter("odd", funkt, booltip)
    pix.flevel:=-1; pix.fadr:=fodd; pix.inside:=false

    enter("chr", funkt, chartip)
    pix.flevel:=-1; pix.fadr:=fchr; pix.inside:=false

    enter("ord", funkt, intip)
    pix.flevel:=-1; pix.fadr:=ford; pix.inside:=false

    enter("write", funkt, 0)
    pix.flevel:=-1; pix.fadr:=fwrite

    enter("writeln", funkt, 0)
    pix.flevel:=-1; pix.fadr:=fwriteln

    enter("read", funkt, 0)
    pix.flevel:=-1; pix.fadr:=fread

    enter("readln", funkt, 0)
    pix.flevel:=-1; pix.fadr:=freadln

    enter("eoln", funkt, booltip)
    pix.flevel:=-1; pix.fadr:=feoln; pix.inside:=false

    namelist[0]:=0
end

proc compile(ichar filename) =
    inp:=fopen(filename, "rb")
    if inp=nil then abortprogram("Can't open file") fi

    lev:=0
    cc:=0
    ll:=0
    lineno:=0
    getch()
    getsym()
    labeled:=true; cx:=0; dx:=memmax+1
    skip(progsym); skip(ident); check(lparen)

    repeat
        getsym()
        check(ident)
        if not eqstring(id,"input") and not eqstring(id,"output") then error(129) fi
        getsym()
    until sym<>comma

    skip(rparen)
    skip(semicolon)
    gen1(kjump, 0)
    block(0)
    gen0(khalt)
    check(period)

    fclose(inp)

    listcode()
end

proc main =
    init()
    compile("test.pas")
    interpret()
end
 
