!Tokeniser Module

import clib
import mlib

import* pci_core
import* pci_read

global tabledata() []ichar symbolnames=
    (errorsym,          $),     ! Lex error
    (commasym,          $),     ! ","
    (colonsym,          $),     ! ":"
    (dcolonsym,         $),     ! "::"
    (lsqsym,            $),     ! [
    (rsqsym,            $),     ! ]

    (addsym,            $),     ! +
    (subsym,            $),     ! -
    (mulsym,            $),     ! *
    (addrsym,           $),     ! &

    (eqsym,             $),     ! =

    (eolsym,            $),     ! End of line
    (eofsym,            $),     ! Eof seen

!   (hashsym,           $),     ! #
    (labelsym,          $),     ! #123

    (intconstsym,       $),     ! 123 64 bits signed
    (realconstsym,      $),     ! 123.4 64 bits
    (stringconstsym,    $),     ! "ABC"
    (int128constsym,    $),     ! 123 128 bits unsigned

    (namesym,           $),     ! raw name

    (kopcodesym,        $),     ! push etc
    (typesym,           $),     ! i32 etc

    (kdummysym,         $)      !
end

macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10

global const hstsize=65536*4
global const hstmask=hstsize-1

global [0:hstsize]psymbol lexhashtable
int nsymbols

!the following returned by updated by lexreadtoken()

global int lxfileno=0   !*@ current source file number
global int lxlineno=0   !*@ current source line number

global int nsourcefiles=0   !no. of linear file names

global int lxsymbol     !* main symbol kind
global int lxsubcode    !* for some symbols, which specific keyword

global int64 lxvalue
global word128 lxuvalue128
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
global byte lxtruename
int lxhashvalue

global ref byte lxsptr      !@ points to next char in source
ref byte lxstart            !@ start of source code for this file
global psymbol lxsymptr !set by lookuplex()

![0..255]char alphamap
![0..255]char digitmap
[0..255]char commentmap

global proc lex=
!lowest level lex() function, reads names, numbers etc but does no lookups or translations
!returns results in lx-vars. Current source pointer should be in lxsptr
int i, c, d, hsum, length
ref byte pstart

    lxsubcode:=0

    doswitch c:=lxsptr++^
    when 'a'..'z','$','_','.' then
        pstart:=lxsptr-1        !point to start of name in source buffer
    doname::
        hsum:=pstart^

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
        lxtruename:=0

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
        c:=pstart^:=pstart^+32
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
        lxtruename:=1
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

    when '!',';' then           !comment to eol

        while commentmap[lxsptr++^] do od

        if (lxsptr-1)^=0 then --lxsptr fi
!
        ++lxlineno

        lxsymbol:=eolsym
        return

    when '#' then               !label
        lxvalue:=0
        doswitch c:=lxsptr++^
        when '0'..'9' then
            lxvalue:=lxvalue*10+c-'0'
        else
            --lxsptr
            exit
        end
    
        if lxvalue=0 then lxerror("Bad label") fi
        if labelnooffset=0 then
            maxuserlabel max:=lxvalue
        else
            lxvalue+:=labelnooffset
        fi
        lxsymbol:=labelsym
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

    when '-' then
        c:=lxsptr++^
        if c not in '0'..'9' then lxerror("Bad no") fi
        readnumber(c,-1)
        return

    when '*' then
        lxsymbol:=mulsym
        return

    when '&' then
        lxsymbol:=addrsym
        return

    when '"' then
        readstring()
        return

    when ' ',9 then

    when cr then            !lf expected to follow
    when lf then
        ++lxlineno
        lxsymbol:=eolsym
        return

    when 0,etx then
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
    static byte done=0

    return when done

    lxsubcode:=0
    lxsymbol:=errorsym

    lxlineno:=0

    for i:=0 to 255 do
!       switch i
!       when 'A'..'Z','a'..'z','$','_','0'..'9' then
!           alphamap[i]:=1
!       end
!       switch i
!       when '0'..'9' then
!           digitmap[i]:=1
!       end
        commentmap[i]:=1
    od

    commentmap[0]:=0
    commentmap[lf]:=0

    inithashtable()

    done:=1
end

proc readreal(ichar s,int slen)=
    int c

    c:=lxsptr^
    lxsptr^:=0
    lxxvalue:=strtod(s,nil)
    lxsptr^:=c

    lxsymbol:=realconstsym
end

proc readnumber(int c, signx=1)=
!A digit c 0..9 has just been read. Numeric formats are::
!1234
!0x1234
!2x1101
!Nx....     possible
    [256]char str
    int i,d,intlen,slen,isfloat,sepseen
    ichar s

    d:=lxsptr^
    case d
    when 'x','X' then           !other base
        case c
        when '0' then           !hex
            ++lxsptr
            readhex()
            lxvalue*:=signx
            return
        else
            cpl c
            lxerror("Base not supported")
        esac
    esac

!assume decimal
    str[1]:=c
    s:=lxsptr-1             !start of number in source
    slen:=1
    isfloat:=sepseen:=0

    doswitch c:=lxsptr++^
    when '0'..'9' then
        str[++slen]:=c
    when '_','\'' then
        sepseen:=1
    when '.' then
        if isfloat then lxerror("float?") fi
        isfloat:=1
        intlen:=slen
    when 'e','E' then
        if isfloat=2 then lxerror("float?") fi
        isfloat:=2
    when '-','+' then
        if isfloat<>2 then lxerror("float?") fi
        isfloat:=3
    else
        --lxsptr
        exit
    end

    if isfloat then
        if sepseen then lxerror("seps in float?") fi
        readreal(s,lxsptr-s)
        lxxvalue*:=signx
        return
    fi

    if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
        if slen>39 or 
                (slen=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
            lxerror("Overflows 128 bits")
        fi
        stringtonumber128(str,slen,10)
        return
    fi

    lxsymbol:=intconstsym

    lxvalue:=0
    for i:=1 to slen do
        lxvalue:=lxvalue*10+str[i]-'0'
    od
    lxvalue*:=signx
end

proc readhex=
!positioned at start of hex seq; 0 chars read yet
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
    elsif ndigs>32 then
        lxerror("Overflow in hex number")
    elsif ndigs>16 then
        lxerror("hex/128 bits not ready")
!       stringtonumber128(str,slen,16)
        return
    fi
    lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
    PRINT CAPTION,LXSPTR,":"
    PRINTSYMBOL()
end

global proc printsymbol(filehandle dev=nil)=
    [256]char str

    strcpy(&.str,symbolnames[lxsymbol])
    str[strlen(&.str)-2]:=0

    print @dev,&.str
    to 14-strlen(&.str) do print @dev," " od

    case lxsymbol
    when namesym then
        print @dev,lxsymptr.name

    when intconstsym then
        print @dev, lxvalue
    when realconstsym then
        print @dev, lxxvalue
    when stringconstsym then
        print @dev,"""",,lxsvalue,,""""!,,"end"
    when errorsym then
        print @dev,lxvalue
    when kopcodesym then
        print @dev,pclnames[lxsubcode]+1

    when typesym then
        print @dev,pstdnames[lxsubcode]

    else
        print @dev,symbolnames[lxsymbol]
        if lxsubcode then
            print " ",,lxsubcode
        fi

    end

    println @dev,$,lxlineno
end

proc inithashtable=
!initialise hash table from kwddata
    for i in pclnames.bounds do
        addreservedword(pclnames[i]+1,kopcodesym,i)
    od

    for i in pstdnames.bounds do
        addreservedword(pstdnames[i],typesym,i)
    od

    addreservedword("proc",kopcodesym,kprocdef)
    addreservedword("function",kopcodesym,kprocdef)
    addreservedword("end",kopcodesym,kendproc)
    addreservedword("endext",kopcodesym,kendextproc)

end

proc addreservedword(ichar name,int symbol,subcode)=
    lxhashvalue:=gethashvalue(name)
    if lookuplex(name,0) then
        cpl =name
        lxerror("DUPL NAME")
    fi

    lxsymptr.symbol:=0
    lxsymptr.ksymbol:=symbol
    lxsymptr.subcode:=subcode
end

global proc printhashtable(filehandle devx,ichar caption)=
    psymbol r
    int count,i

    println @devx,caption,":"
    count:=0
    for i:=0 to lexhashtable.upb do
        r:=lexhashtable[i]
        if R AND r.name then
            count+:=1
        fi
    od
    println @devx,count," items in table",hstsize
end

function lookuplex(ichar name,int length=0)int=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
    psymbol e
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

        if ++j>=hstsize then        !wraparound
            if wrapped then
                println "???????HASHTABLE FULL",hstsize,lxlineno
                stop 1
            fi
            wrapped:=1
            j:=1
        fi
    od

!name not found
    if insource then
        name:=pcm_copyheapstringn(name,length)
    fi

    if ++nsymbols>((hstsize*7)/8) then
        lxerror("Hashtable getting full")
    fi

    if lxsymptr=nil then
        lxsymptr:=pcm_allocz(pstrec.bytes)
        lexhashtable[j]:=lxsymptr
    fi

    lxsymptr.name:=name
    lxsymptr.namelen:=length
    lxsymptr.symbol:=namesym
    lxsymptr.ksymbol:=0
    return 0
end

global proc startlex(ichar source)=
    initlex()
    lxstart:=lxsptr:=cast(source)
    lxlineno:=1
    lxsymbol:=errorsym
end

global function addnamestr(ichar name)psymbol=
!add a new name to the symbol table
!return symptr to new (or existing) generic name
    lxhashvalue:=gethashvalue(name)
    lookuplex(pcm_copyheapstring(name),0)
    return lxsymptr
end

global proc lxerror(ichar m)=           !LXERROR

    fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno
    stop 1
end

global function gethashvalue(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes S is lower-case, as conversion not done
    int c,hsum

    if s^=0 then return 0 fi

    hsum:=s++^

    do
        c:=s++^
        exit when c=0
        hsum:=hsum<<4-hsum+c
    od
    return hsum<<5-hsum
end

proc readstring=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter

    var ichar dest
    var int c,d
    var [8]char str

    lxsymbol:=stringconstsym

    lxsvalue:=lxsptr

    dest:=lxsptr                !form string into same buffer

    do
        switch c:=lxsptr++^
        when '\\' then          !escape char
            c:=lxsptr^
            if c>='A'  and c<='Z' then c+:=' ' fi
            ++lxsptr
            switch c
            when 'a' then           !bell ('alert')
                c:=7
            when 'b' then           !backspace
                c:=8
            when 'c','r' then       !carriage return
                    c:=cr
            when 'e' then           !end-of-text
                c:=26
            when 'f' then           !formfeed
                c:=12
            when 'l','n' then       !linefeed, or linux/c-style newline
                c:=lf
            when 's' then           !eScape
                c:=27
            when 't' then           !tab
                c:=9
!           when 'u' then           !reserved for unicode, like \x but with 4 hex digits
            when 'v' then           !vertical tab
                c:=11
            when 'w' then           !windows-style cr-lf
                dest++^:=cr
                c:=lf
            when 'x' then   !2-digit hex code follows
                c:=0
                to 2 do
                    case d:=lxsptr++^
                    when 'A','B','C','D','E','F' then
                        c:=c*16+d-'A'+10
                    when 'a','b','c','d','e','f' then
                        c:=c*16+d-'a'+10
                    when '0','1','2','3','4','5','6','7','8','9' then
                        c:=c*16+d-'0'
                    else
                        lxerror("Bad \\x code")
                    esac
                od
            when 'y' then           !CCI/SM backwards tab
                c:=16
            when 'z','0' then       !null (not fully supported in code)
                c:=0
            when '"','Q' then       !embedded double quote
                c:='"'
            when '\\' then
                c:='\\'
            when '\'' then          !embedded single quote
                c:='\''
            else
                println "<",,char(c),,">"
                lxerror("Unknown string escape")
            end
        when '"' then       !possible terminators
            if lxsptr^=c then       !repeated, assume embedded term char
                ++lxsptr
            else            !was end of string
                exit
            fi
        when cr,lf,0 then
            lxerror("String not terminated")
        endswitch

        dest++^:=c
    od
    lxlength:=dest-lxsvalue
    (lxsvalue+lxlength)^:=0     !overwrites final " or earlier
end

proc stringtonumber128(ichar s, int length,base)=
    word128 aa
    int c,d
    aa:=0
    to length do
        aa:=aa*base
        c:=(s++)^

        if c>='a' then
            d:=c-'a'+10
        elsif c>='A' then
            d:=c-'A'+10
        else
            d:=c-'0'
        fi

        aa:=aa+d
    od

    lxuvalue128:=aa
    lxsymbol:=int128constsym
end

