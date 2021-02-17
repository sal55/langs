!(Simple lexer written in my M systems language. This was a direct port of mlex.q,
! and only took 10-15 minutes to modify as the languages share syntax and other features.)

import clib
import mlib

tabledata() []ichar symbolnames=
    (errorsym,          $),
    (dotsym,            $),
    (commasym,          $),
    (semisym,           $),
    (colonsym,          $),
    (assignsym,         $),
    (addsym,            $),
    (subsym,            $),
    (mulsym,            $),
    (divsym,            $),
    (idivsym,           $),

    (eqsym,             $),
    (nesym,             $),
    (ltsym,             $),
    (lesym,             $),
    (gesym,             $),
    (gtsym,             $),
    (shlsym,            $),
    (shrsym,            $),
    (powersym,          $),

    (lbracksym,         $),
    (rbracksym,         $),
    (lsqsym,            $),
    (rsqsym,            $),
    (lcurlysym,         $),
    (rcurlysym,         $),
    (addrsym,           $),
    (ptrsym,            $),
    (ellipsissym,       $),
    (rangesym,          $),
    (barsym,            $),
    (questionsym,       $),
    (atsym,             $),

    (eolsym,            $),
    (eofsym,            $),
    (hashsym,           $),
    (incrsym,           $),
    (decrsym,           $),
    (namesym,           $),

    (intconstsym,       $),
    (charconstsym,      $),
    (stringconstsym,    $),
end

int lxlineno
int lxhash
ref char lxsptr

int lxsymbol
int lxvalue
ichar lxsvalue

const cr    = 13
const lf    = 10
const etx   = 26

proc start=
    ichar infile
    ref char psource
    int nchars, nlines, ntokens, t
    real tsecs

    if nsysparams>=2 then
        infile:=sysparams[2]
    else
        infile:="input"
    fi

    psource:=cast(readfile(infile))

    if psource=nil then
        println "Can't open",infile
        stop
    fi

    nchars:=nlines:=ntokens:=0

    lxsptr:=psource
    lxlineno:=1
    t:=clock()

    repeat
        readtoken()
        ++ntokens
!       printsymbol()
    until lxsymbol=eofsym

    t:=clock()-t
    tsecs:=t/1000.0

    nlines+:=lxlineno
    nchars+:=strlen(psource)

    println
    println "Lines:",nlines
    println "Tokens:",ntokens
    println "Chars:",nchars-1

    println

    println nlines/tsecs,"Lines per second"
    println ntokens/tsecs,"Tokens per second"
    println real(nchars)/tsecs,"Chars per second"

    println
    println tsecs," Seconds"
    println
end

proc readtoken=
    [256]char str
    ichar s
    int c

    doswitch c:=lxsptr^
    when 'a'..'z','$','_' then
        s:=&.str
        s++^:=c
doname::
        doswitch c:=(++lxsptr)^
        when 'A'..'Z' then
            s++^:=c+' '
        when 'a'..'z','0'..'9','_','$' then
            s++^:=c
        else
            exit
        end doswitch

        if c='"' and str[1] in ['f','F'] then
            ++lxsptr
            readrawstring()
            return
        fi

        s^:=0
        lxsvalue:=pcm_copyheapstring(&.str)

        lxsymbol:=namesym
        return

    when 'A'..'Z' then
        s:=&.str
        s++^:=c
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

    when '!' then           !comment to eol
        lxsymbol:=eolsym
docomment::
        doswitch c:=(++lxsptr)^
        when 13 then
        when 10,0 then
            ++lxlineno
            ++lxsptr
            exit
        end

        return

    when '#' then       !(can be used as docstring to eol)
        ++lxsptr
        lxsymbol:=hashsym
        goto docomment

    when '{' then
        ++lxsptr
        lxsymbol:=lcurlysym
        return

    when '}' then
        ++lxsptr
        lxsymbol:=rcurlysym
        return

    when '.' then
        ++lxsptr
        if lxsptr^='.' then
            ++lxsptr
            if lxsptr^='.' then
                ++lxsptr
                lxsymbol:=ellipsissym
            else
                lxsymbol:=rangesym
            fi
        else
            lxsymbol:=dotsym
        fi
        return

    when ',' then
        ++lxsptr
        lxsymbol:=commasym
        return

    when ';' then
        ++lxsptr
        lxsymbol:=semisym
        return

    when ':' then
        ++lxsptr
        if lxsptr^='=' then
            ++lxsptr
            lxsymbol:=assignsym
        else
            lxsymbol:=colonsym
        fi
        return

    when '(' then
        ++lxsptr
        lxsymbol:=lbracksym
        return

    when ')' then
        ++lxsptr
        lxsymbol:=rbracksym
        return

    when '[' then
        ++lxsptr
        lxsymbol:=lsqsym
        return

    when ']' then
        ++lxsptr
        lxsymbol:=rsqsym
        return

    when '|' then
        ++lxsptr
        lxsymbol:=barsym
        return

    when '^' then
        ++lxsptr
        lxsymbol:=ptrsym
        return

    when '@' then
        ++lxsptr
        lxsymbol:=atsym
        return

    when '+' then
        ++lxsptr
        if lxsptr^='+' then
            ++lxsptr
            lxsymbol:=incrsym
        else
            lxsymbol:=addsym
        fi
        return

    when '-' then
        ++lxsptr
        if lxsptr^='-' then
            ++lxsptr
            lxsymbol:=decrsym
        else
            lxsymbol:=subsym
        fi
        return

    when '*' then
        ++lxsptr
        if lxsptr^='*' then
            ++lxsptr
            lxsymbol:=powersym
        else
            lxsymbol:=mulsym
        fi
        return
        return

    when '/' then
        ++lxsptr
        lxsymbol:=divsym
        return

    when '%' then
        ++lxsptr
        lxsymbol:=idivsym
        return

    when '=' then
        ++lxsptr
        lxsymbol:=eqsym
        return

    when '<' then
        switch (++lxsptr)^
        when '=' then
            ++lxsptr
            lxsymbol:=lesym
        when '>' then
            ++lxsptr
            lxsymbol:=nesym
        when '<' then
            ++lxsptr
            lxsymbol:=shlsym
        else
            lxsymbol:=ltsym
        endswitch
        return

    when '>' then
        switch (++lxsptr)^
        when '=' then
            ++lxsptr
            lxsymbol:=gesym
        when '>' then
            ++lxsptr
            lxsymbol:=shrsym
        else
            lxsymbol:=gtsym
        endswitch
        return

    when '&' then
        ++lxsptr
        lxsymbol:=addrsym
        return

    when '\'' then
        ++lxsptr
        readstring('\'')
        return

    when '"' then
        ++lxsptr
        readstring('"')
        return

    when '?' then
        ++lxsptr
        lxsymbol:=questionsym
        return

    when '\\' then
        ++lxsptr

    when ' ','\t' then
        ++lxsptr

    when cr then
        ++lxsptr

    when lf then
        ++lxlineno
        ++lxsptr
        lxsymbol:=eolsym
        return

    when etx,0 then
        lxsymbol:=eofsym
        return

    else
        lxerror("Unknown token")

!    end doswitch
    end
end

proc printsymbol=
    print lxlineno,symbolnames[lxsymbol],"  "

    case lxsymbol
    when namesym,stringconstsym, charconstsym then
        print lxsvalue
    when intconstsym then
        print lxvalue
    esac
    println
end

proc readnumber(int base)=
    int c,d
    lxvalue:=word(0)

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
        lxvalue:=lxvalue*base+d
    od

    lxsymbol:=intconstsym
end

proc readstring(int termchar)=
    [8192]char str
    ichar s
    int c

    lxsymbol:=(termchar='"'|stringconstsym|charconstsym)

    s:=&.str

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
                c:=9
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
                s++^:=cr
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
        when cr,lf,etx,0 then
            --lxsptr
            lxerror("String not terminated")
        endswitch
        s++^:=c
    od

    s^:=0
    lxsvalue:=pcm_copyheapstring(&.str)

end

proc readrawstring=
    [8192]char str
    ichar s
    int c
    lxsymbol:=stringconstsym

    s:=&.str

    do
        switch c:=lxsptr++^
        when '"' then
            exit
        when cr,lf,etx,0 then
            --lxsptr
            lxerror("String not terminated")
        endswitch
        s++^:=c
    od
    s^:=0
    lxsvalue:=pcm_copyheapstring(&.str)
end

proc lxerror(ichar mess)=
    println "Error on line",lxlineno,":",mess
    println
    println
    stop 1
end
