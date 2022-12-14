global var hashtable

global var lxsymptr, lxsymbol, lxsubcode, lxvalue, lxsptr
global var lxlineno, lxstart

const cr  = 13
const lf  = 10
const tab =  9
const etx =  0

global proc lex=

    doswitch c:=lxsptr^
    when 'a'..'z', '$', '_' then
        lxvalue::=chr(c)
doname:
        doswitch c:=(++lxsptr)^
        when 'A'..'Z' then
            lxvalue +:= c+32
        when 'a'..'z', '0'..'9', '$', '_', '.' then
            lxvalue +:= c
        else
            exit
        end doswitch

        lxsymbol:=namesym

        lookuplex(lxvalue)
        if lxsymptr.ksymbol then
            lxsymbol:=lxsymptr.ksymbol
            lxsubcode:=lxsymptr.subcode
        fi

        if lxsymbol=namedconstsym then
            lxsymbol:=intconstsym
            lxvalue:=lxsymptr.offset
        fi

        return

    when 'A'..'Z' then
        lxvalue::=chr(c+32)
        goto doname

    when '0'..'9' then
        ++lxsptr
        case lxsptr^
        when 'x','X' then
            ++lxsptr
            case c
            when '0' then
                readnumber(16)
            when '2' then
                readnumber(2)
            else
                lxerror("Bad base")
            esac
        else
            --lxsptr
            readnumber(10)
        esac
        return

    when '!', ';' then
        lxsymbol:=eolsym
        doswitch c:=(++lxsptr)^
        when lf then
            ++lxlineno
            ++lxsptr
            exit
        when etx then
            exit
        end doswitch

        return

    when ' ', tab, cr then
        ++lxsptr

    when lf then
        ++lxsptr
        ++lxlineno
        lxsymbol:=eolsym
        return

    when etx then
        lxsymbol:=eofsym
        return

    when '"', '\'' then
        readstring()
        return

    when '[' then
        ++lxsptr
        lxsymbol:=lsqsym
        return

    when ']' then
        ++lxsptr
        lxsymbol:=rsqsym
        return

    when ',' then
        ++lxsptr
        lxsymbol:=commasym
        return

    when ':' then
        ++lxsptr
        lxsymbol:=colonsym
        return

    when '+' then
        ++lxsptr
        lxsymbol:=addsym
        return

    when '-' then
        ++lxsptr
        lxsymbol:=subsym
        return

    when '*' then
        ++lxsptr
        lxsymbol:=mulsym
        return

    when '/' then
        ++lxsptr
        lxsymbol:=divsym
        return

    when '=' then
        ++lxsptr
        lxsymbol:=eqsym
        return

    when '(',')' then
        lxerror("Use square brackets not ()")

    else
        lxsymbol:=errorsym
        lxvalue:=chr(c)
        return
    end doswitch

end

global proc initlex=
    lxsubcode:=0
    lxsymbol:=errorsym

    lxlineno:=0

    inithashtable()
end

proc inithashtable=
    hashtable:=[:]

    forall i,name in mclnames do
        addreservedword(rightstr(name, -2), opcodesym, i)
    od

    forall i,name in regnames do
        addreservedword(name, regsyms[i], i)
    od

    forall i,name in condnames do
        addreservedword(name, condcodesym, i)
    od

    forall i,name in syscallnames do
        addreservedword(rightstr(name,-4), syscallsym, i)
    od
end

proc addreservedword(name, symbol, subcode)=

    if lookuplex(name) then
        lxerror("DUPL NAME:"+name)
    fi

    lxsymptr.symbol:=0
    lxsymptr.ksymbol:=symbol
    lxsymptr.subcode:=subcode
end

func lookuplex(name)=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)

    lxsymptr:=hashtable{name, nil}

    if lxsymptr<>nil then           !found
        return 1
    fi

    lxsymptr:=new(strec)
    lxsymptr.name:=name
    lxsymptr.symbol:=namesym
    lxsymptr.ksymbol:=0
    lxsymptr.offset:=-1

    hashtable{name}:=lxsymptr

    return 0
end

global proc lexstart(source)=
    lxstart:=lxsptr:=&source
    lxlineno:=1
end

proc readnumber(base)=
    lxvalue:=0

    do
        switch c:=lxsptr++^
        when '_','\'' then
            next
        when '0'..'9' THEN
            d:=c-'0'
        when 'A'..'F' then
            d:=c-('A'-10)
        when 'a'..'f' then
            d:=c-('a'-10)
        else
            --lxsptr
            exit
        end

        if d>=base then
            if c not in ['e','E'] then
                lxerror("Bad digit")
            else        
                --lxsptr
            fi
            exit
        fi

        lxvalue:=lxvalue*word(base)+word(d)
    od

    lxsymbol:=intconstsym
end

proc readstring=

    termchar:=lxsptr++^

    lxsymbol:=(termchar='"'|stringconstsym|charconstsym)
    lxvalue::=""

    do
        switch c:=lxsptr++^
        when '\\' then          !escape char
            c:=lxsptr^
            if c in 'A'..'Z' then c+:=' ' fi

            ++lxsptr
            switch c
            when 'c','r' then
                c:=cr
            when 'l','n' then
                c:=lf
            when 't' then
                c:=tab
            when 'a' then
                c:=7
            when 'b' then
                c:=8
            when '"' then
                c:='"'
            when '\\' then
                c:='\\'
            when '\'' then
                c:='\''
            when 'w' then
                lxvalue+:=chr(cr)
                c:=lf
            else
                c:='?'
            end
        when '"','\'' then
            if c=termchar then
                if lxsptr^=c then
                    ++lxsptr
                else
                    exit
                fi
            fi
        when cr,lf,etx then
            --lxsptr
            lxerror("String not terminated")
        endswitch
        lxvalue+:=c
    od

    if lxsymbol=charconstsym then
        if lxvalue.len>1 then lxerror("Can't do 'ABC'") fi
        lxvalue:=asc(lxvalue)
        lxsymbol:=intconstsym
    fi
end

GLOBAL PROC PS(MESS)=
    CPL MESS,,":",SYMBOLNAMES[LXSYMBOL], LXVALUE, (LXSYMPTR|LXSYMPTR.NAME|"")
END
