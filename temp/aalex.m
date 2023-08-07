!x64 Assembler Tokeniser Module

const cr  = 13
const lf  = 10

global int lxsymbol     !* main symbol kind
global int lxsubcode    !* for some symbols, which specific keyword

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
int lxhashvalue

global ref byte lxsptr
ref byte lxstart
global ref strec lxsymptr

global proc lex=
    int i, c, d, hsum, length
    ref byte pstart

    lxsubcode:=0

    doswitch c:=lxsptr++^
    when 'a'..'z','$','_','.' then
        pstart:=lxsptr-1
        hsum:=c
    doname:

        doswitch c:=lxsptr++^
        when 'a'..'z','0'..'9','_','$','.' then
            hsum:=hsum<<4-hsum+c
        when 'A'..'Z' then
            (lxsptr-1)^:=c+32
            hsum:=hsum<<4-hsum+c+' '
        else
            --lxsptr
            exit
        end

        lxlength:=lxsptr-pstart
        lxhashvalue:=hsum<<5 -hsum

        if lookuplex(cast(pstart),lxlength) then
            if lxsymptr.ksymbol then            !keywords take priority here
                lxsymbol:=lxsymptr.ksymbol
                lxsubcode:=lxsymptr.subcode
            else
                lxsymbol:=lxsymptr.symbol
            fi
        else
            lxsymbol:=namesym
        fi

        return

    when 'A'..'Z' then
        pstart:=lxsptr-1
        hsum:=pstart^:=c+32
        goto doname

    when '0'..'9' then
        readnumber(c)
        return

    when '`' then
        pstart:=lxsptr      !point to start of name in source buffer
        hsum:=0

        doswitch c:=lxsptr^
        when 'A'..'Z','a'..'z','0'..'9','_','$','.' then
            ++lxsptr
            hsum:=hsum<<4-hsum+c
        else
            exit
        end

        lxsymbol:=namesym
        if pstart=lxsptr then
            lxerror("NULL ` name")
        fi
        lxlength:=lxsptr-pstart
        lxhashvalue:=hsum<<5-hsum

        if lookuplex(cast(pstart),lxlength) then
            lxsymbol:=lxsymptr.symbol           !can't be a keyword
            if lxsymbol=0 then                  !assume was a keyword; use as name
                lxsymbol:=lxsymptr.symbol:=namesym
            fi
        fi
        return

    when '!',';','#' then           !comment to eol

        while lxsptr++^ not in [0, lf] do od

        if (lxsptr-1)^=0 then --lxsptr fi
!
        ++lxlineno

        lxsymbol:=eolsym
        return

    when ',' then
        lxsymbol:=commasym
        return

    when ':' then
        if lxsptr^=':' then
            lxsymbol:=dcolonsym
            ++lxsptr
        else
            lxsymbol:=colonsym
        fi
        return

    when '[' then
        lxsymbol:=lsqsym
        return

    when ']' then
        lxsymbol:=rsqsym
        return

    when '+' then
        lxsymbol:=addsym
        return

    when '-' then
        lxsymbol:=subsym
        return

    when '*' then
        lxsymbol:=mulsym
        return

    when '=' then
        lxsymbol:=eqsym
        return

    when '\'' then
        pstart:=lxsptr

        do
            switch lxsptr++^
            when '\'' then
                exit
            when cr,lf then
                lxerror("String not terminated")
            end switch
        od
        length:=lxsptr-pstart-1
        lxvalue:=0
        for i:=length downto 1 do
            lxvalue:=lxvalue<<8+(pstart+i-1)^
        od
        lxsymbol:=intconstsym
        return

    when '"' then
        pstart:=lxsptr

        do
            switch lxsptr++^
            when '"' then
                lxsvalue:=cast(pstart)
                lxlength:=lxsptr-pstart-1
                (lxsvalue+lxlength)^:=0
                lxsymbol:=stringconstsym
                return
            when cr,lf,0 then
                lxerror("String not terminated")
            end switch
        od

    when ' ',9 then

    when cr then            !lf expected to follow

    when lf then
        ++lxlineno
        lxsymbol:=eolsym
        return

    when 0 then
        lxsymbol:=eofsym
        --lxsptr
        return
    else
        lxsymbol:=errorsym
        lxvalue:=c
        return

    end doswitch
end

global proc initlex=
    lxsubcode:=0
    lxsymbol:=errorsym
    lxlineno:=0

    inithashtable()
end

proc readreal(ref[]char s,int slen, intlen,exponseen)=
    int i,fractlen,expon,exponsign,c,digs

    if intlen=0 or intlen=slen then
        fractlen:=0
    else
        fractlen:=slen-intlen
    fi

    expon:=exponsign:=0

    if exponseen then
        case c:=lxsptr++^
        when '+' then
        when '-' then
            exponsign:=1
        else
            --lxsptr
        esac

        digs:=0
        doswitch c:=lxsptr++^
        when '0'..'9' then
            expon:=expon*10+c-'0'
            ++digs
        else
            --lxsptr
            exit
        end
        if digs=0 then
            lxerror("Exponent error")
        fi
        if exponsign then expon:=-expon fi
    fi

    expon:=expon-fractlen

    lxxvalue:=0.0

    for i:=1 to slen do
        c:=s^[i]
        lxxvalue:=lxxvalue*10.0+(c-'0')
    od

    if expon>0 then
        to expon do
            lxxvalue:=lxxvalue*10.0
        od
    elsif expon<0 then
        to -expon do
            lxxvalue:=lxxvalue/10.0
        od
    fi

    lxsymbol:=realconstsym
end

proc readnumber(int c)=
    [256]char str
    int i,d,intlen,slen

    d:=lxsptr^
    case d
    when 'x','X' then           !other base
        case c
        when '0' then           !hex
            ++lxsptr
            readhex()
            return
        when '2' then           !binary
            ++lxsptr
            readbinary()
            return
        else
            cpl c
            lxerror("Base not supported")
        esac
    esac

    str[1]:=c
    slen:=1
    intlen:=0

    doswitch c:=lxsptr++^
    when '0'..'9' then
        str[++slen]:=c
    when '_','\'','`' then
    when '.' then
        intlen:=slen
    when 'e','E' then
        readreal(&str,slen,intlen,1)
        return
    else
        --lxsptr
        exit
    end

    if intlen then
        readreal(&str,slen,intlen,0)
        return
    fi

    if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
        lxerror("Overflow in 64-bit value")
    fi

    lxsymbol:=intconstsym

    lxvalue:=0
    for i:=1 to slen do
        lxvalue:=lxvalue*10+str[i]-'0'
    od
end

proc readbinary=
    int ndigs

    ndigs:=0
    lxvalue:=0
    doswitch lxsptr++^
    when '0' then
        lxvalue:=lxvalue*2
        ++ndigs
    when '1' then
        lxvalue:=lxvalue*2+1
        ++ndigs
    when '2'..'9' then
        lxerror("Bad binary digit")
    when '_','\'','`' then
    else
        --lxsptr
        exit
    end

    if ndigs=0 then
        lxerror("No bin digits")
    elsif ndigs>64 then
        lxerror("Overflow in binary number")
    fi
    lxsymbol:=intconstsym
end

proc readhex=
    int ndigs,c

    ndigs:=0
    lxvalue:=0
    doswitch c:=lxsptr++^
    when '0'..'9' then
        lxvalue:=lxvalue*16+c-'0'
        ++ndigs
    when 'A'..'F' then
        lxvalue:=lxvalue*16+(c-'A'+10)
        ++ndigs
    when 'a'..'f' then
        lxvalue:=lxvalue*16+(c-'a'+10)
        ++ndigs
    when '_','\'','`' then
    else
        --lxsptr
        exit
    end

    if ndigs=0 then
        lxerror("No hex digits")
    elsif ndigs>16 then
        lxerror("Overflow in hex number")
    fi
    lxsymbol:=intconstsym
end

proc inithashtable=
    for i to mclnames.len do
        addreservedword(mclnames[i]+2,kopcodesym,i)
    od

    for i to dregnames.len do
        addreservedword(dregnames[i],kregsym,regindices[i])
        lxsymptr.regsize:=regsizes[i]
    od

    for i to xregnames.len do
        addreservedword(xregnames[i],kxregsym,i)
    od

    for i to fregnames.len do
        addreservedword(fregnames[i],kfregsym,i)
    od

    for i to mregnames.len do
        addreservedword(mregnames[i],kmregsym,i)
    od

    for i to jmpccnames.len do
        addreservedword(jmpccnames[i],kjmpccsym,jmpcccodes[i])
    od

    for i to setccnames.len do
        addreservedword(setccnames[i],ksetccsym,setcccodes[i])
    od

    for i to cmovccnames.len do
        addreservedword(cmovccnames[i],kmovccsym,cmovcccodes[i])
    od

    for i to prefixnames.len do
        addreservedword(prefixnames[i],kprefixsym,prefixsizes[i])
    od

    for i to segmentnames.len do
        addreservedword(segmentnames[i],ksegnamesym,i)
    od

    addreservedword("aframe",kregsym,r14); lxsymptr.regsize:=4
    addreservedword("dframe",kregsym,r14); lxsymptr.regsize:=8
    addreservedword("astack",kregsym,r15); lxsymptr.regsize:=4
    addreservedword("dstack",kregsym,r15); lxsymptr.regsize:=8
    addreservedword("dprog",kregsym,r8); lxsymptr.regsize:=8
    addreservedword("dsptr",kregsym,r9); lxsymptr.regsize:=8

    addreservedword("importlib",kimportlibsym,0)
    addreservedword("importdll",kimportdllsym,0)
end

proc addreservedword(ichar name,int symbol,subcode)=
    lxhashvalue:=gethashvalue(name)
    if lookuplex(name,0) then
        println =name
        lxerror("DUPL NAME")
    fi

    lxsymptr.symbol:=0
    lxsymptr.ksymbol:=symbol
    lxsymptr.subcode:=subcode
end

function lookuplex(ichar name,int length=0)int=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
    ref strec e

    int j,wrapped,insource,firstj

    insource:=length
    if length=0 then
        length:=strlen(name)
    fi

    firstj:=j:=(lxhashvalue iand hstmask)       !j=initial hash index

    wrapped:=0

    do
        lxsymptr:=lexhashtable[j]
        if lxsymptr=nil then                !unused entry, not found
            exit
        fi

        if lxsymptr.namelen=length and memcmp(lxsymptr.name,name,length)=0 then         !match
            return 1
        fi

        if ++j>hstsize then     !wraparound
            if wrapped then
                println "???????HASHTABLE FULL",hstsize,lxlineno
                stop 1
            fi
            wrapped:=1
            j:=1
        fi
    od

    if insource then
        name:=makestring(name,length)
    fi

    if lxsymptr=nil then
        lxsymptr:=pcm_allocz(strec.bytes)
        lexhashtable[j]:=lxsymptr
    fi

    lxsymptr.name:=name
    lxsymptr.namelen:=length
    lxsymptr.symbol:=namesym
    lxsymptr.ksymbol:=0
    lxsymptr.htindex:=j
    lxsymptr.htfirstindex:=firstj
    lxsymptr.moduleno:=currmoduleno
    return 0
end

global proc initsourcefile(ichar source)=
    lxstart:=lxsptr:=cast(source)
    lxlineno:=1
end

global function addnamestr(ichar name)ref strec=
    lxhashvalue:=gethashvalue(name)
    lookuplex(pcm_copyheapstring(name),0)
    return lxsymptr
end

global proc lxerror(ichar m)=           !LXERROR

    fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno

    stop 1
end

global function gethashvalue(ichar s)int=
    int c,hsum

    if s^=0 then return 0 fi

    hsum:=s++^
    while c:=s++^; c do
        hsum:=hsum<<4-hsum+c
    od
    return hsum<<5-hsum
end


function makestring(ichar p,int length)ref char s=
    s:=pcm_alloc(length+1)
    memcpy(s,p,length)
    (s+length)^:=0
    return s
end
