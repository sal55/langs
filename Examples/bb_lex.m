!M5 compiler lexer

macro hashc(hsum,c)=hsum<<4-hsum+c
macro hashw(hsum)=(hsum<<5-hsum)

const maxstackdepth=20
array [maxstackdepth]ref char lxstart_stack
array [maxstackdepth]ref char lxsource_stack
array [maxstackdepth]ref char lxsptr_stack
array [maxstackdepth]int lxfileno_stack
array [maxstackdepth]tokenrec lxnextlx_stack
array [maxstackdepth]byte lximport_stack
global int sourcelevel=0
global int lximport
!global int nincludestack

const cr    = 13
const lf    = 10
const tab   = 9

ref char lxsource
ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix          !for real nos

int lxfileno
global const hstsize    = 65536
global const hstmask    = hstsize-1

global [0:hstsize]symbol hashtable
symbol hashtablelast

global int astringlength

ichar u64maxstr="18446744073709551615"

global proc lex=
    int lena,lenb
    ref char p

    lx:=nextlx              !grab that already read basic token
    lx.sourceoffset:=lxstart-lxsource

reenter::
    lexreadtoken()

    switch nextlx.symbol
    when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,
            kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
            krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
            ktabledatasym,kassemsym,kifsym then

        if lx.symbol=kendsym then
            if lx.subcode then lxerror("end if if?") fi
            lx.subcode:=nextlx.symbol
            reenter
        fi

    when eolsym then
        if lx.symbol in [commasym, lsqsym, lbracksym] then !ignore eol
            reenter
        elsif symboloptypes[lx.symbol]=bin_op and not assemmode and 
            lx.symbol not in [maxsym, minsym] then
            reenter
        fi
        nextlx.symbol:=semisym
        nextlx.subcode:=1

    when stringconstsym then
        if lx.symbol=stringconstsym then
            lena:=strlen(lx.svalue)
            lenb:=strlen(nextlx.svalue)
            p:=pcm_alloc(lena+lenb+1)
            memcpy(p,lx.svalue,lena)
            memcpy(p+lena,nextlx.svalue,lenb)
            (p+lena+lenb)^:=0
            lx.svalue:=p
        fi
    when ksourcedirsym then
        if not dolexdirective(nextlx.subcode) then      !skip symbol
            reenter
        fi

    when namesym then
        case nextlx.subcode
        when unitnamesym then
            case lx.symbol
            when intconstsym then
                case nextlx.symptr.index
                when million_unit then lx.value *:= 1 million
                when billion_unit then lx.value *:= 1 billion
                when thousand_unit then lx.value *:= 1 thousand
                when kilo_unit then lx.value *:= 1024
                when mega_unit then lx.value *:= 1048576
                when giga_unit then lx.value *:= (1048576*1024)
                else
                    lxerror("Can't do this unit index")
                esac
                lx.subcode:=setinttype(lx.value)
                reenter
            when realconstsym then
                lxerror("Unit suffix after float not implem")
            else
                nextlx.symbol:=namesym
            esac
        when kheadersym then
            if not headermode then
                nextlx.symbol:=namesym
            else
                nextlx.symbol:=kheadersym
                nextlx.subcode:=nextlx.symptr.index

            fi
        else
            nextlx.symbol:=namesym
        esac

    when rawxnamesym then
        nextlx.symbol:=namesym

    when insym then
        if lx.symbol=notlsym then
            lx.symbol:=notinsym
            lx.subcode:=knotin
            reenter
        fi
    when eqsym then
        if lx.symbol=notlsym then
            lx.symbol:=cmpsym
            lx.subcode:=kne
            reenter
        fi
    end switch

    nextlx.pos :=nextlx.pos ior lxfileno<<24
end

global proc lexreadtoken=
!read next token into nextlx
    int c,hsum,commentseen,hashindex,length
    ref char pstart,pnext,p,ss,lxsvalue

    nextlx.subcode:=0

    doswitch lxstart:=lxsptr; lxsptr++^
    when 'a'..'z','_','$',0x80..0xEE, 0xF0..0xFF then
        lxsvalue:=lxsptr-1
    doname::
        hsum:=lxsvalue^

        doswitch c:=lxsptr++^
        when 'a'..'z','0'..'9','_','$',0x80..0xEE, 0xF0..0xFF then
            hsum:=hashc(hsum,c)
        when 'A'..'Z' then
            (lxsptr-1)^:=c+' '
            hsum:=hashc(hsum,c+' ')
        when '"' then
            --lxsptr
            if lxsvalue+1=ref char(lxsptr) then
                case lxsvalue^
                when  'F','f','R','r' then 
                    readrawstring()
                    return
                when  'A','a','Z','z' then 
                    readarraystring(lxsvalue^)
                    return
                esac
            fi
            exit
        else
            --lxsptr
            exit
        end doswitch

        lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))
        return

    when 'A'..'Z' then
        lxsvalue:=lxsptr-1
        lxsvalue^+:=32
        goto doname

    when '0'..'9' then
        lxstart:=lxsptr-1
        case lxsptr^
        when ')',cr,',',' ' then        !assume single digit decimal
            nextlx.symbol:=intconstsym
            nextlx.subcode:=tint
            nextlx.value:=lxstart^-'0'
        when 'x','X' then
            case lxstart^
            when '0' then       !0x
                ++lxsptr
                readhex()
            when '2' then
                ++lxsptr
                readbin()
            else
                lxerror("Bad base")
            esac
        else
            --lxsptr
            readdec()
        esac
        return

    when '!' then           !comment to eol
docomment::
        doswitch c:=lxsptr++^
        when 13 then
            ++lxsptr
            exit
        when 10 then
            exit
        when 0 then
            --lxsptr
            exit
        end
!       ++nextlx.pos
        nextlx.symbol:=eolsym
        return

    when '#' then           !docstring to eol
!       if lxsptr^<>'#' then
            docomment
!       fi

        ++lxsptr
        lxsvalue:=cast(lxsptr)

        doswitch c:=lxsptr++^
        when 13 then
            ++lxsptr
            exit
        when 10 then
            exit
        when 0 then
            --lxsptr
            exit
        end

        length:=lxsptr-cast(lxsvalue,ref char)
        nextlx.symbol:=docstringsym
        nextlx.svalue:=pcm_copyheapstringn(lxsvalue,length)
        return

    when '\\' then          !line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
        commentseen:=0
        doswitch lxsptr++^          !read until end of this line
        when cr then
!           ++nextlx.pos
            ++lxsptr                !skip lf
            exit
        when lf then
!           ++nextlx.pos
            exit
        when 0 then
            nextlx.symbol:=eofsym
            --lxsptr
            return
        when ' ',tab then
        when '!' then
            commentseen:=1
        else
            if not commentseen then
                lxerror("\\ not followed by eol")
            fi
    enddoswitch
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

        doswitch lxsptr++^
        when cr then
!           ++nextlx.pos
            ++lxsptr                !skip lf
        when lf then
!           ++nextlx.pos
        when ' ',tab then
        else
            --lxsptr
            exit
        enddoswitch

    when '{' then
        nextlx.symbol:=lcurlysym
        return

    when '}' then
        nextlx.symbol:=rcurlysym
        return

    when '.' then
        switch lxsptr^
        when '.' then               !.. or ...
            ++lxsptr
            if lxsptr^='.' then
                ++lxsptr
                nextlx.symbol:=ellipsissym
            else
                nextlx.symbol:=rangesym
                nextlx.subcode:=j_makerange     !helps treat as opsym which all have k-code as subcode
            fi
            return
        when '0'..'9' then          !real const: deal with this after the switch
            --lxsptr
LXERROR(".123 not done")
!           readrealnumber(nil,0,10)
            return
        else
            nextlx.symbol:=dotsym
            return
        endswitch

    when ',' then
        nextlx.symbol:=commasym
        return

    when ';' then
        nextlx.symbol:=semisym
        return

    when ':' then
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=assignsym
            nextlx.subcode:=j_assign        !helps treat as opsym which all have k-code as subcode
        when ':' then
            ++lxsptr
            case lxsptr^
            when '=' then
                ++lxsptr
                nextlx.symbol:=deepcopysym
!               nextlx.subcode:=j_deepcopy
            else
                nextlx.symbol:=dcolonsym
            esac
        else
            nextlx.symbol:=colonsym
        endswitch
        return

    when '(' then
        nextlx.symbol:=lbracksym
        return

    when ')' then
        nextlx.symbol:=rbracksym
        return

    when '[' then
        nextlx.symbol:=lsqsym
        return

    when ']' then
        nextlx.symbol:=rsqsym
        return

    when '|' then
        if lxsptr^='|' then
            ++lxsptr
            nextlx.symbol:=dbarsym
        else
            nextlx.symbol:=barsym
        fi
        return

    when '^' then
        nextlx.symbol:=ptrsym
        return

    when '@' then
        if lxsptr^='@' then
            ++lxsptr
            nextlx.symbol:=datsym
        else
            nextlx.symbol:=atsym
        fi
        return

    when '?' then
        nextlx.symbol:=questionsym
        return

    when '~' then
        nextlx.symbol:=curlsym
        return

    when '+' then
        nextlx.symbol:=addsym
        if lxsptr^='+' then
            ++lxsptr
            nextlx.symbol:=incrsym
            nextlx.subcode:=kincr
            return
        fi
        return

    when '-' then
        nextlx.symbol:=subsym
        case lxsptr^
        when '-' then
            ++lxsptr
            nextlx.symbol:=incrsym
            nextlx.subcode:=kdecr
            return
        when '>' then
            ++lxsptr
            nextlx.symbol:=pipesym
            return
        esac
        return

    when '*' then
        if lxsptr^='*' then
            ++lxsptr
            nextlx.symbol:=powersym
        else
            nextlx.symbol:=mulsym
        fi
        return

    when '/' then
        nextlx.symbol:=divsym
        return

    when '%' then
        nextlx.symbol:=idivsym
        return

    when '=' then
        case lxsptr^
        when '>' then
            nextlx.symbol:=sendtosym
            ++lxsptr
        when '=' then
            ++lxsptr
!           if lxsptr^='=' then
!               ++lxsptr
!               nextlx.symbol:=ssmarkersym
!           else
                nextlx.symbol:=samesym
!           fi
        else
            nextlx.symbol:=eqsym
            nextlx.subcode:=keq
        esac
        return

    when '<' then
        nextlx.symbol:=cmpsym
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.subcode:=kle
        when '>' then
            ++lxsptr
            nextlx.subcode:=kne
        when '<' then
            ++lxsptr
            nextlx.symbol:=shlsym
        else
            nextlx.subcode:=klt
        endswitch
        return

    when '>' then
        nextlx.symbol:=cmpsym
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=cmpsym
            nextlx.subcode:=kge
        when '>' then
            ++lxsptr
            nextlx.symbol:=shrsym
        else
            nextlx.symbol:=cmpsym
            nextlx.subcode:=kgt
        endswitch
        return

    when '&' then
        case lxsptr^
!           when '&' then
!           ++lxsptr
!           nextlx.symbol:=opsym
!           nextlx.subcode:=j_andand
        when '.' then
            ++lxsptr
            nextlx.symbol:=anddotsym
            nextlx.subcode:=0
        else
            nextlx.symbol:=addrsym
            nextlx.subcode:=j_addrof
        esac
        return

    when '\'' then
        lxreadstring('\'')
        return

    when '"' then
        lxreadstring('"')
        return

    when '`' then
        readrawxname()
        return

    when ' ',tab then

    when cr then
        ++lxsptr                !skip lf
!       ++nextlx.pos
        nextlx.symbol:=eolsym
        return
    when lf then            !only lfs not preceded by cr
!       ++nextlx.pos
        nextlx.symbol:=eolsym
        return

    when 0 then
        if sourcelevel then
!           --nincludestack
            unstacksource()
RETURN
        else
            nextlx.symbol:=eofsym
            --lxsptr
            return
        fi

    when 0xEF then          !BOM
        lxsptr+:=2

    else
        nextlx.symbol:=errorsym
        return

    end doswitch

end

global proc printsymbol(ref tokenrec lp)=
    tokenrec l
    l:=lp^

    printf("%-18s",symbolnames[l.symbol])

    switch l.symbol
    when namesym then
        printstrn(l.symptr.name,l.symptr.namelen)

        if l.subcode then
            fprint " [#]",symbolnames[l.subcode]
        fi

    when intconstsym then
        case l.subcode
        when tint then print l.value,"int"
        when tword then print l.uvalue,"word"
        else print l.value
        esac

    when realconstsym then
        print l.xvalue

    when stringconstsym then
        print """"
        printstr(l.svalue)
        print """",strlen(l.svalue)
    when charconstsym then
        print "'"
        printstr(l.svalue)
        print "'"
    when decimalconstsym then
        printstr(l.svalue)
        print "L"
    when assignsym,addrsym,ptrsym,deepcopysym,rangesym,
        andlsym,orlsym,eqsym,cmpsym,addsym,subsym,
        mulsym,divsym,idivsym,iremsym,iandsym,iorsym,ixorsym,shlsym,shrsym,
        minsym,maxsym,powersym,samesym then
        print symbolnames[l.symbol]
    elsif l.subcode then
        fprint "SUBCODE:",l.subcode
!   fprint "#",symbolnames[l.subcode]
    end

    print $,=lx.fileno
    println

end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
    inithashtable()
end

global proc printstrn(ichar s, int length)=
    if length then
        print length:"v",s:".*"
    fi
end

proc readrawstring=
!positioned at " of F"
!read raw string
    ichar dest
    int c

    nextlx.symbol:=stringconstsym
    nextlx.svalue:=++lxsptr

    dest:=lxsptr                !form string into same buffer

    doswitch c:=lxsptr++^
    when '"' then
        if lxsptr^='"' then     !repeated, assume embedded term char
            dest++^:='"'
            ++lxsptr
        else            !was end of string
            (lxsptr-1)^:=0
            exit
        fi
    when cr,lf,0 then
        lxerror("Raw string not terminated")
        --lxsptr
        exit
    else
        dest++^:=c
    enddoswitch
end

proc lookup(ref char name, int length, hashindex0)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
    int wrapped, hashindex,INDEX,n
    symbol d
    int j

    j:=hashindex0 iand hstmask

    d:=hashtable[j]
    wrapped:=0

    do
        if d=nil then exit fi

        if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then   !match
            nextlx.symptr:=d
            nextlx.symbol:=d.symbol
            nextlx.subcode:=d.subcode
            return
        fi

        if ++j>=hstsize then
            if wrapped then
                abortprogram("HASHTABLE FULL")
            fi
            wrapped:=1
            j:=0
        fi
        d:=hashtable[j]
    od

!exit when not found; new name will go in entry pointed to by lxsymptr

    d:=pcm_allocz(strec.bytes)
    hashtable[j]:=d

    d.name:=pcm_copyheapstringn(name,length)
    d.namelen:=length
    d.symbol:=namesym

    nextlx.symptr:=d
    nextlx.symbol:=d.symbol
!   nextlx.subcode:=d.subcode
end

function lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
    int j, wrapped, hashvalue

    j:=gethashvaluez(name) iand hstmask

    lx.symptr:=hashtable[j]
    wrapped:=0

    do
        if lx.symptr=nil then
            exit
        elsif eqstring(lx.symptr.name,name) then    !match
            println "Lex dupl name: sub"
            stop 1 
!           lxerror("sys dupl name?")
        fi

        if ++j>=hstsize then
            if wrapped then
                abortprogram("SYS:HASHTABLE FULL")
            fi
            wrapped:=1
            j:=0
        fi
        lx.symptr:=hashtable[j]
    od

!exit when not found; new name will go in entry pointed to by lxsymptr
    lx.symptr:=pcm_allocz(strec.bytes)
    hashtable[j]:=lx.symptr

    lx.symptr.name:=name                !assume can be shared (stored in a table)
    lx.symptr.namelen:=strlen(name)
    lx.symptr.symbol:=namesym           !usually replaced with actual symbol details

    return 0
end

function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
    int c,hsum

    if s^=0 then return 0 fi

    hsum:=s++^

    do
        c:=s++^
        exit when c=0
        hsum:=hashc(hsum,c)
    od
    return hashw(hsum)
end

proc inithashtable=
!populate hashtable with standard symbols
    int i
    memset(&hashtable,0,hashtable.bytes)
    hashtablelast:=&hashtable[hstsize-1]

    for i:=1 to stnames.len do
        lookupsys(stnames[i])

        lx.symptr.symbol:=stsymbols[i]

        case stsymbols[i]
        when unitnamesym, kheadersym then
            lx.symptr.index:=stsubcodes[i]
            lx.symptr.subcode:=stsymbols[i]
            lx.symptr.symbol:=namesym       !masquerades as normal identifier
        else
            lx.symptr.subcode:=stsubcodes[i]
        esac
    od
end

global proc printhashtable=
    println "Hashtable:"

!   for i:=0 to hstsize-1 do
!       if hashtable[i] then
!       fi
!   od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
    lookupsys(name)

    lx.symptr.symbol:=namesym
    lx.symptr.subcode:=symbol
    lx.symptr.index:=subcode

    lx.symptr.regsize:=regsize
end

function dolexdirective(int index)int=
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
    ref strec symptr
    ref char p
    ichar file
    int i,lastsymbol,cond,fileno,length
    array [256]char str

    case index
    when includedir then
        lexreadtoken()
        if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
        file:=nextlx.svalue
        convlcstring(file)
        file:=addext(file,langext)      !add in extension if not present; assume same as source

        fileno:=getsupportfile(file, path:sourcefilepaths[lxfileno], issupport:1)
!       ++nincludestack
        lexreadtoken()
        stacksource(fileno)
        return 0

    else
        cpl sourcedirnames[index]
        lxerror("Directive not implemented")
    esac
    return 0
end

global proc startlex(int fileno)=
!start processing one of the file in sourcefile tables as source code
!assume it is a complete header or module

    lxsource:=lxsptr:=sourcefiletext[fileno]

    nextlx.pos:=0
    lxfileno:=fileno

    nextlx.symbol:=semisym
    nextlx.subcode:=0
end

global function addnamestr(ichar name)ref strec=
    tokenrec oldlx
    ref strec symptr

    oldlx:=nextlx
    lookup(name,strlen(name), gethashvaluez(name))
    symptr:=nextlx.symptr
    nextlx:=oldlx

    return symptr
end

global proc ps(ichar caption)=
    print "PS:",caption,,": "
    printsymbol(&lx)
end

global proc psnext(ichar caption)=
    print caption,,": "
    printsymbol(&nextlx)
end

global proc psx(ichar caption)=
    print caption,,": "
    printsymbol(&lx)
    print " "
    printsymbol(&nextlx)
end

global proc stacksource(int fileno, isimport=0)=
!introduce new source level for macros or include files
!not used for main source

!CPL "STACKSOURCE", SOURCELEVEL
    if sourcelevel>=maxstackdepth then
        lxerror("Include file/macro overflow")
    fi
    ++sourcelevel
    lxstart_stack[sourcelevel]:=lxstart
    lxsource_stack[sourcelevel]:=lxsource
    lxsptr_stack[sourcelevel]:=lxsptr
    lxfileno_stack[sourcelevel]:=lxfileno
    lxnextlx_stack[sourcelevel]:=nextlx
    lximport_stack[sourcelevel]:=lximport
    lximport:=isimport

    lxsource:=lxsptr:=sourcefiletext[fileno]

    nextlx.pos:=0
    lxfileno:=fileno

    nextlx.symbol:=semisym
    nextlx.subcode:=0
end

global proc unstacksource=
    if sourcelevel>0 then           !check that some source is stacked
        lxstart:=lxstart_stack[sourcelevel]
        lxsource:=lxsource_stack[sourcelevel]
        lxsptr:=lxsptr_stack[sourcelevel]
        nextlx:=lxnextlx_stack[sourcelevel]
        lxfileno:=lxfileno_stack[sourcelevel]
        lximport:=lximport_stack[sourcelevel]

        --sourcelevel
    fi
end

proc readarraystring(int prefix)=
    ++lxsptr
    lxreadstring('"')
    nextlx.symbol:=astringconstsym
    nextlx.subcode:=toupper(prefix)
    astringlength:=strlen(nextlx.svalue)
end

function setinttype(word64 a)int=
    if a<=u64(0x7FFF'FFFF'FFFF'FFFF) then
        return ti64
    else
        return tu64
    fi
end

proc readrawxname=
    int c,hsum,length

    nextlx.svalue:=lxsptr
    hsum:=0

    doswitch c:=lxsptr++^
    when 'A'..'Z','a'..'z','0'..'9','_','$' then
        hsum:=hashc(hsum,c)
    else
        --lxsptr
        exit
    end doswitch

    length:=lxsptr-nextlx.svalue

    if length=0 then
        lxerror("Bad ` name")
    fi
    lookup(nextlx.svalue,length, hashw(hsum))
    nextlx.symbol:=rawxnamesym

    return
end

proc lxerror_s(ichar mess,s)=
    lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

    ichar s,t
    int c, d, length, hasescape
    array [8]char str

    if termchar='"' then
        nextlx.symbol:=stringconstsym
    else
        nextlx.symbol:=charconstsym
        nextlx.subcode:=tint
    fi

    s:=lxsptr

!do a first pass that terminates length of final string
    length:=0
    hasescape:=0

    doswitch c:=lxsptr++^
    when '\\' then          !escape char
        c:=lxsptr^
        if c in 'A'..'Z' then c+:=' ' fi
        ++lxsptr
        hasescape:=1

        switch c
        when 'a','b','c','e','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
            ++length
        when 'w' then
            ++length
        when 'x' then   !2-digit hex code follows
            lxsptr+:=2
            ++length
        else
            lxerror("Bad str escape")
        endswitch
    when '"','\'' then      !possible terminators
        if c=termchar then      !terminator char
            if lxsptr^=c then       !repeated, assume embedded term char
                hasescape:=1
                ++lxsptr
                ++length
            else            !was end of string
                exit
            fi
        else
            ++length
        fi
    when cr,lf,0 then
        lxerror("String not terminated")
    else
        ++length
    end doswitch

    if length=0 then
        nextlx.svalue:=""
        return
    elsif not hasescape then
        nextlx.svalue:=pcm_copyheapstringn(s,length)
        return
    fi

!need to copy string to dest and expand the escape codes

    nextlx.svalue:=t:=pcm_alloc(length+1)

    do
        switch c:=s++^
        when '\\' then          !escape char
            c:=s^
            if c>='A'  and c<='Z' then c+:=' ' fi
            ++s
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
                t++^:=cr
                c:=lf
            when 'x' then   !2-digit hex code follows
                c:=0
                to 2 do
                    case d:=s++^
!                   switch d:=s++^
                    when 'A','B','C','D','E','F' then
                        c:=c*16+d-'A'+10
                    when 'a','b','c','d','e','f' then
                        c:=c*16+d-'a'+10
                    when '0','1','2','3','4','5','6','7','8','9' then
                        c:=c*16+d-'0'
                    else
                        lxerror("Bad \\x code")
                    end
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
                str[1]:=c; str[2]:=0
                lxerror_s("Unknown string escape: \\%s",&.str)
            end
        when '"','\'' then      !possible terminators
            if c=termchar then      !terminator char
                if s^=c then        !repeated, assume embedded term char
                    ++s
                else            !was end of string
                    exit
                fi
            fi
        when cr,lf,0 then
            lxerror("String not terminated")
        endswitch

        t++^:=c
    od

    t^:=0
end

proc readdec=
    int c
    ref char dest, destend, pstart
    int islong, length
    array [1024]byte str
    word a

    islong:=0

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'9' then
            a:=a*10+c-'0'
            dest++^:=c
        when 'e','E' then
            lxsptr:=pstart
            readreal()
            return
        when '.' then
            if lxsptr^<>'.' then
                lxsptr:=pstart
                readreal()
                return
            fi
            --lxsptr
            exit

        when '_','\'' then
        when 'l','L' then
            dest^:=0
LXERROR("MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,10)
            return

        when 'b','B' then
            length:=dest-&.str
            if length>64 then lxerror("bin overflow") fi
            dest:=&.str
            a:=0
            to length do
                if dest^>='2' then lxerror("bad bin digit") fi
                a:=a*2+dest++^-'0'
            od
            finish

        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("Numlit too long") fi
    end
    length:=dest-&.str

    if length>20 or length=20 and strncmp(&.str,u64maxstr,20) then
LXERROR("2:MAKEDECIMAL NOT READY")
!       makedecimal(&.str,length,10)
        return
!       lxerror("u64 overflow")
    fi

finish::
    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readhex=
    int c
    ref char dest, destend, pstart
    int length
    array [1024]byte str
    word a

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'9' then
            a:=a*16+c-'0'
            dest++^:=c

        when 'A'..'F' then
            dest++^:=c
            a:=a*16+c-'A'+10
        when 'a'..'f' then
            dest++^:=c-32
            a:=a*16+c-'a'+10

        when '_','\'' then
        when 'l','L' then
            dest^:=0
LXERROR("3:MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,16)
            return

        when '.' then
            --lxsptr
            exit

        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("Numlit too long") fi
    end
    length:=dest-&.str

    if length>16 then
LXERROR("4:MAKEDECIMAL NOT READY")
!       makedecimal(&.str,length,16)
        return
!       lxerror("u64 overflow")
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readbin=
    int c
    ref char dest, destend, pstart
    int length
    array [1024]byte str
    word a

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'1' then
            a:=a*2+c-'0'
            dest++^:=c

        when '_','\'' then
        when 'l','L' then
            dest^:=0
LXERROR("5:MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,2)
            return

        when '2'..'9' then
            lxerror("bin bad digit")
        when '.' then
            --lxsptr
            exit

        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("bin overflow") fi
    end
    length:=dest-&.str

    if length>64 then
LXERROR("6:MAKEDECIMAL NOT READY")
!       makedecimal(&.str,length,2)
        return
!       lxerror("u64 overflow")
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

    var int c,n,negexpon,dotseen,length, fractlen, expon, expseen
    var real x
    array [1024]char str
    ichar dest, destend, pexpon

    dest:=&.str
    destend:=dest+str.len-100
    length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

    do
        switch c:=lxsptr++^
        when '0'..'9' then
            dest++^:=c
            ++length
            if dotseen then ++fractlen fi
        when '.' then
            if dotseen then --lxsptr; exit fi
            dotseen:=1
            dest++^:=c


        when 'e','E' then
            if expseen then lxerror("double expon") fi
            expseen:=1
            dest++^:=c
            while lxsptr^=' ' do ++lxsptr od
            if lxsptr^ in ['+','-'] then
                if lxsptr^='-' then negexpon:=1 fi
                dest++^:=lxsptr++^
            fi

            expon:=0
            doswitch c:=lxsptr++^
            when '0'..'9' then
                expon:=expon*10+c-'0'
                dest++^:=c
                if dest>=destend then lxerror("expon?") fi

            when '_','\'' then
            when 'l','L' then
                dest^:=0
LXERROR("7:MAKEDECIMAL NOT READY")
!               makedecimal(&.str,dest-&.str,10)
                return
            else
                --lxsptr
                exit all
            end doswitch

        when '_','\'' then

        when 'l','L' then
LXERROR("8:MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,10)
            return
        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("r64lit too long") fi
    end
    dest^:=0

!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
   if negexpon then expon:=-expon fi
    expon-:=fractlen
    x:=0.0

    for i:=1 to length+dotseen do       !digits already range-checked
        c:=str[i]
        if c<>'.' then
            x:=x*10.0+c-'0'
        fi
    od

    if expon>=0 then
        to expon do
            x*:=10.0
        od
    else
        to -expon do
            x/:=10.0
        od
    fi

    nextlx.xvalue:=x
!------------------------------------------------------------
! Best way to covert: more accurate representation, but slower
!------------------------------------------------------------
!   nextlx.xvalue:=strtod(str,nil)
!------------------------------------------------------------

    nextlx.symbol:=realconstsym
    nextlx.subcode:=treal
end
