! (C tokeniser module)

import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_headers
import cc_lib

var ref tokenrec tkptr=nil    !! for tokenrec details, see https://github.com/sal55/langs/blob/master/tokenrec.m

var int dowhitespace=0

var int NINCLUDES

record stackinforec =
    var ref char startptr
    var ref char sptr
    var int32 lineno
    var int32 fileno
end

const maxmacroargs=200
var tokenrec normaltkx
var ref tokenrec normaltk = &normaltkx          !indicates use lexm to get tokens
var int noexpand=0                      !inhibit macro expansion for 'defined'

const maxnesting=20
var [maxnesting]stackinforec lx_stack
var int lx_stackindex
var int ifcondlevel=0                   !counts #if levels
var [maxnesting]ichar headerpathlist    !remember path at each level
var [300]char headerpath                !as set up by getsource()

const cr    = 13
const lf    = 10
const tab   = 9

var ref char lxstart
var ref char lxsptr
var int lxhashvalue
var ref char lxsvalue

var [0..255]char alphamap
var [0..255]char digitmap
var [0..255]char commentmap
var [0..255]char linecommentmap
var [0..255]char spacemap

var ref strbuffer destcopy
!const int maxpastedtokens=17000
const int maxpastedtokens=87000
var [maxpastedtokens]ichar pastedtokenlist
var int npastedtokens=0
var int isincludefile=0             !whether readng include file name

var int firstsymbol=1
var ref byte reallxsptr

GLOBAL var int nhstsymbols
var int hstthreshold                !limit above which new hst should be generated

global proc lex_preprocess_only(ichar infile,int showtokens, NN, toconsole=0)=
    var ref char psource
    var int ntokens,nlines,fileno,size
    var int LENGTH
    var int64 nchars,t,hashtot,symtot
    var real tsecs
    static var strbuffer sbuffer
    static var ref strbuffer dest=&sbuffer
    var filehandle f
    var [300]char outfile
    VAR ICHAR SS

    dowhitespace:=1
    fileno:=loadsourcefile(infile,infile)

    strcpy(&.outfile,changeext(infile,"i"))

    psource:=cast(sourcefiletext[fileno])
    size:=sourcefilesizes[fileno]

    nlines:=ntokens:=0
    hashtot:=symtot:=0
    t:=os_clock()

    destcopy:=dest
    gs_init(dest)

    NALLLINES:=0

!println "Preprocessing",infile

    lxsptr:=psource
    lxstart:=lxsptr
    nextlx.lineno:=1
    setfileno(1)
    ifcondlevel:=0

    stacksourcefile("bcc.h",1)

    nextlx.symbol:=eolsym

    repeat
        lexm()
        ++ntokens

        if showtokens then
            emittoken(&nextlx,dest)
        fi

    until nextlx.symbol=eofsym

    if ifcondlevel then
        lxerror("#endif missing")
    fi

    nlines+:=NALLLINES

    if showtokens then
        if toconsole then
            gs_println(dest,nil)
        else
            f:=fopen(&.outfile,"wb")
            gs_println(dest,f)
            fclose(f)
        fi
    fi
end

global proc lexreadtoken=
!read next token into nextlx
    var int c,csum,hsum,dodir
    var ref char p,ss
    var ichar searchstr

    nextlx.subcodex:=0

    doswitch lxsptr++^
    when 'A'..'Z','a'..'z','$','_' then
        lxsvalue:=lxsptr-1
        hsum:=lxsvalue^

        while alphamap[c:=lxsptr++^] do
            hsum:=hsum<<4-hsum+c
        od
        --lxsptr
        nextlx.symbol:=namesym
        nextlx.length:=lxsptr-lxsvalue
        case c
        when '\'', '"' then
            if nextlx.length=1 then
                case lxsvalue^
                when 'l','L','u','U' then
                    ++lxsptr
                    lxreadstring(c,1)
                    return
                esac
            fi
        esac

        lxhashvalue:=hsum<<5-hsum

        ss:=pcm_alloc(nextlx.length+1)      !zero-term in lex(), as headers may need to be
        memcpy(ss,lxsvalue,nextlx.length)   !re-tokenised
        (ss+nextlx.length)^:=0
        lxsvalue:=ss

        lookup()                        !clash, so do normal lookup to set lxsymptr
        return

    when '1'..'9' then                  !can only be decimal
        case lxsptr^
        when ' ',')',cr,',',';' then        !assume single digit decimal
            nextlx.symbol:=intconstsym
            nextlx.subcode:=tsint
            nextlx.value:=(lxsptr-1)^-'0'
            nextlx.length:=1

            setnumberoffset(lxsptr-1-lxstart)
        else
            readdecimal(lxsptr-1)               !note: can also be real const;
        esac
        return

    when '0' then                   !0, hex, binary or octal
        switch lxsptr^
        when 'x','X' then
            ++lxsptr
            readhex(lxsptr-2)
            return
        when 'b','B' then
            ++lxsptr
            readbinary(lxsptr-2)
            return
        when '.' then
            readrealnumber(lxsptr-1,lxsptr-1,1,10)
            return
        when 'u','U','l','L' then
            readdecimal(lxsptr-1)               !note: can also be real const;
            return
        when ',', ')', ']', '}', ';', ' ',':',cr,lf,'&','=','?' then    !assume just zero
            nextlx.symbol:=intconstsym
            nextlx.subcode:=tsint
            nextlx.value:=0
            nextlx.length:=1
            setnumberoffset(lxsptr-1-lxstart)
            return
        else

            readoctal(lxsptr-1)
            return
        endswitch                   !else assume just zero  

    when '#' then           !
        if nextlx.symbol=eolsym then
            nextlx.symbol:=lexhashsym

            return

        elsif lxsptr^='#' then
            ++lxsptr
            nextlx.symbol:=hashhashsym
            return
        else
            nextlx.symbol:=hashsym
            return
        fi

    when '\\' then          !line continuation
        docase lxsptr^
        when cr,lf then
            exit
        when ' ',tab then
            ++lxsptr
        else
            nextlx.symbol:=backslashsym
            return
        end docase

        (lxsptr-1)^:=' '    !convert \ to space
        ++nextlx.lineno
    ++NALLLINES
        case lxsptr^
        when cr then
            ++lxsptr            !point to lf
            lxsptr++^:=' '      !set lf to space (so that '#' processing works
        when lf then
            lxsptr++^:=' '
        else
!       lxerror("\\ not followed by newline")   
        esac

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
                --lxsptr
                nextlx.symbol:=dotsym
                return
            fi
            return
        when '0'..'9' then          !real const: deal with this after the switch
            --lxsptr
            readrealnumber(lxsptr,lxsptr,0,10)
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
        nextlx.symbol:=colonsym
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
        case lxsptr^
        when '|' then
            ++lxsptr
            nextlx.symbol:=orlsym
        when '=' then
            ++lxsptr
            nextlx.symbol:=iortosym
        else
            nextlx.symbol:=iorsym
        esac
        return

    when '^' then
        if lxsptr^='=' then
            ++lxsptr
            nextlx.symbol:=ixortosym
        else
            nextlx.symbol:=ixorsym
        fi
        return

    when '?' then
        nextlx.symbol:=questionsym
        return

    when '~' then
        nextlx.symbol:=inotsym
        return

    when '+' then
        case lxsptr^
        when '+' then
            ++lxsptr
            nextlx.symbol:=incrsym
        when '=' then
            ++lxsptr
            nextlx.symbol:=addtosym
        else
            nextlx.symbol:=addsym
        esac
        return

    when '-' then
        case lxsptr^
        when '-' then
            ++lxsptr
            nextlx.symbol:=decrsym
        when '>' then
            ++lxsptr
            nextlx.symbol:=idotsym
        when '=' then
            ++lxsptr
            nextlx.symbol:=subtosym
        else
            nextlx.symbol:=subsym
        esac
        return

    when '*' then
        if lxsptr^='=' then
            ++lxsptr
            nextlx.symbol:=multosym
        else
            nextlx.symbol:=mulsym
        fi
        return

    when '/' then
        case lxsptr^
        when '/' then                   !comment to 
            readlinecomment()
            nextlx.symbol:=eolsym
            nextlx.length:=0
            return
        when '*' then
            readblockcomment()
        when '=' then
            ++lxsptr
            nextlx.symbol:=divtosym
            return
        else
            nextlx.symbol:=divsym
            return
        esac

    when '%' then
        if lxsptr^='=' then
            ++lxsptr
            nextlx.symbol:=remtosym
        else
            nextlx.symbol:=remsym
        fi
        return

    when '=' then
        case lxsptr^
        when '=' then
            nextlx.symbol:=eqsym
            ++lxsptr
        else
            nextlx.symbol:=assignsym
        esac
        return

    when '<' then
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=lesym
        when '<' then
            if (++lxsptr)^='=' then
                ++lxsptr
                nextlx.symbol:=shltosym
            else
                nextlx.symbol:=shlsym
            fi
        else
            nextlx.symbol:=ltsym
        endswitch
        return

    when '>' then
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=gesym
        when '>' then
            if (++lxsptr)^='=' then
                ++lxsptr
                nextlx.symbol:=shrtosym
            else
                nextlx.symbol:=shrsym
            fi
        else
            nextlx.symbol:=gtsym
        endswitch
        return

    when '&' then
        case lxsptr^
        when '&' then
            ++lxsptr
            nextlx.symbol:=andlsym
        when '=' then
            ++lxsptr
            nextlx.symbol:=iandtosym
        else
            nextlx.symbol:=iandsym
        esac
        return

    when '\'' then
        lxreadstring('\'',0)
        return

    when '"' then
        lxreadstring('"',0)
        return

    when ' ',tab then

    when lf then
    ++NALLLINES
        ++nextlx.lineno
        nextlx.symbol:=eolsym
        nextlx.length:=0
        if dowhitespace then
            nextlx.svalue:=cast(lxsptr)
            doswitch (lxsptr++)^
            when ' ',tab then
            else
                --lxsptr
                exit
            end
!       while spacemap[(++lxsptr)^] do od
            nextlx.length:=lxsptr-nextlx.svalue
        fi
        return
    when cr then                !ignore; always expect lf to follow

    when '!' then
        case lxsptr^
        when '=' then
            nextlx.symbol:=nesym
            ++lxsptr
        else
            nextlx.symbol:=notlsym
        esac
        return

    when '@' then
!PRINTLN "@ SEEN",nextlx.lineno,sourcefilenames[nextlx.fileno],lx_stackindex

    when 0 then
    doeof::
        --lxsptr
        if lx_stackindex then
            unstacksourcefile()
            nextlx.symbol:=eolsym
        else
            nextlx.symbol:=eofsym
        fi
        return

    when 12 then
    else
        PRINTLN "ERROR CHAR",(lxsptr-1)^,lx_stackindex
        lxerror("ERROR CHAR")
        nextlx.symbol:=errorsym
        return

    end doswitch

end

proc readrealnumber(ref char pstart,intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, (or to "." if there was no prefix, then intlen=0)
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
    var ref char fractstart
    var int fractlen,expon,i,c,badexpon,n,adj
    var real basex,x,expbase,f,y,y2,g
    var int64 aa,cc,pref
    const maxrealdigits=500
    var [maxrealdigits+12]char realstr
    var ref char rs
    var [32]char expstr
    var word64 xx1,xx2

    if base<>10 then
        old_readrealnumber(pstart,intstart,intlen,base)
        return
    fi

    fractstart:=nil
    fractlen:=0
    expon:=0

    if lxsptr^='.' then     !read
        fractstart:=++lxsptr
        fractlen:=scannumber(base)-fractstart
    fi
    badexpon:=0

    case lxsptr^
    when 'e','E' then
        if base<>16 then
            ++lxsptr
            expon:=readexponent(badexpon)
        fi
    when 'p','P' then
        if base=16 then
            ++lxsptr
            expon:=readexponent(badexpon)
        fi
    esac

    if badexpon then
        --lxsptr
        readalphanumeric(pstart)
        return
    fi

    case lxsptr^
    when 'f','F','l','L' then
        ++lxsptr
    else
        if alphamap[lxsptr^] then
            readalphanumeric(pstart)
            return
        fi
    esac

    if base=16 then
        realstr[1]:='0'
        realstr[2]:='x'
        rs:=&realstr[3]
        pref:=2
    else
        rs:=&realstr[1]
        pref:=0
    fi

    if intlen+fractlen>maxrealdigits then
        lxerror("Real too long")
    fi
    if intlen then
        memcpy(rs,intstart,intlen)
    fi
    if fractlen then
        memcpy(rs+intlen,fractstart,fractlen)
    fi

    expbase:=basex:=base

    if base=10 then
        expon-:=fractlen
    else
        expon-:=fractlen*4              !each hex digit is 4 binary bits
        expbase:=2.0
    fi

    realstr[pref+intlen+fractlen+1]:=0

    print @&.expstr,(base=10|"e"|"p"),,expon

    strcat(&.realstr,&.expstr)
    if base<>10 then
        lxerror("Non-base-10 floats temporarily unavailable")
    fi

    x:=strtod(&.realstr,nil)

    nextlx.symbol:=realconstsym
    nextlx.subcode:=tdouble
    nextlx.xvalue:=x

    setnumberoffset(intstart-lxstart)
    nextlx.length:=lxsptr-intstart
end

function readexponent(int &badexpon)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
!exponent is always in base 10
    var ref char numstart
    var int length,neg,c
    var int64 a

    neg:=0
    case lxsptr^
    when '+' then ++lxsptr
    when '-' then ++lxsptr; neg:=1
    esac

    numstart:=lxsptr
    length:=scannumber(10)-numstart

    if length=0 then
        badexpon:=1
        return 0
    fi

    a:=0

    to length do
        c:=numstart++^
        a:=a*10+c-'0'
    od

    return (neg|-a|a)
end

proc lxerror(ichar mess)=
!int i
    PRINTLN "Lex error",mess,"in:",,sourcefilepaths[getfileno()],
     "Line:",nextlx.lineno
    println
    println
    println
    os_getch()
    stop 11
end

global proc printsymbol(ref tokenrec lp)=
    var tokenrec l
    l:=lp^

    printf("%-18s",symbolnames[l.symbol])

    case l.symbol
    when namesym then
        printstrn(l.symptr^.name,l.symptr^.namelen)

    when intconstsym then
        print l.value,," "
        shownumberstr(lp)

    when realconstsym then
        print l.xvalue,," "
        shownumberstr(lp)

    when stringconstsym then
        print """"
        printstrn(l.svalue,l.length)
        print """"
    when charconstsym then
        print "'"
        printstrn(l.svalue,l.length)
        print "'"

    elsif l.subcode then
        print "#",l.subcode
    end

    println
end

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
    var int i

    inithashtable()
    fillhashtable()

    for i:=0 to 255 do
        switch i
        when 'A'..'Z','a'..'z','$','_','0'..'9' then
            alphamap[i]:=1
        end
        switch i
        when '0'..'9' then
            digitmap[i]:=1
        end
        commentmap[i]:=1
        linecommentmap[i]:=1
        spacemap[i]:=0
    od

    commentmap['*']:=0
    commentmap[0]:=0
    commentmap[lf]:=0

    linecommentmap[0]:=0
    linecommentmap['\\']:=0
    linecommentmap[lf]:=0

    spacemap[' ']:=1
    spacemap[tab]:=1

    normaltkx.symbol:=eolsym
    npastedtokens:=0
end

global proc printstrn(ichar s, int length,filehandle f=nil)=
    if length then
        if f=nil then
            print length:"v",,s:".*"
        else
            print @f,length:"v",,s:".*"
        fi
    fi
end

function scannumber(int base)ref char=
    var ref char dest
    var int c

    dest:=lxsptr

    doswitch c:=lxsptr++^
    when '0'..'9' then
        dest++^:=c
        if c>='0'+base then
            lxerror("Digit out of range")
        fi
    when 'A'..'F','a'..'f' then
            if base=16 then
            dest++^:=c
        else
            --lxsptr
            exit
        fi
    when '_','\'','`' then
    else
        --lxsptr
        exit
    end doswitch
    return dest
end

function lookup:int=
    var int j, wrapped,length

    retry::
    j:=lxhashvalue iand hstmask
    wrapped:=0


    do
        nextlx.symptr:=hashtable^[j]
        length:=nextlx.symptr^.namelen

        if not length then
            exit
        fi

        if length=nextlx.length then    !match on length
            if memcmp(nextlx.symptr^.name,lxsvalue,length)=0 then   !match
                return 1
            fi
        fi

        if ++j>=hstsize then
            if wrapped then
                abortprogram("HASHTABLE FULL")
            fi
            wrapped:=1
            j:=0
        fi
    od

!exit when not found; new name will go in entry pointed to by lxsymptr

    if nhstsymbols>=hstthreshold then
        newhashtable()

        lxhashvalue:=gethashvalue(lxsvalue,nextlx.length)
        goto retry
    fi

    nextlx.symptr^.name:=lxsvalue
    nextlx.symptr^.namelen:=nextlx.length
    nextlx.symptr^.symbol:=namesym
    ++nhstsymbols

    return 0
end

GLOBAL function gethashvalue(ichar s,int length=-1)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes s is lower-case, as conversion not done
    var int c,hsum

    if length=-1 then
        length:=strlen(s)
    fi
    hsum:=0

    to length do
        hsum:=hsum<<4-hsum+s++^
    od
    return hsum<<5 -hsum
end

proc inithashtable=
    hashtable:=pcm_alloc(hstsize*(ref void.bytes))
    hstmask:=hstsize-1

    for i:=0 to hstmask do
        hashtable^[i]:=pcm_allocz(strec.bytes)
    od

    nhstsymbols:=0
    hstthreshold:=(6*hstsize)/10

end

proc fillhashtable=
!populate hashtable with standard symbols
    var int i

    for i:=1 to stnames.len do
        lxsvalue:=stnames[i]

!sourcedir names can be converted to user name types, which could have a
!zero appended in lex() as the assumption is they are still in-situ within the source
!But with compilers like gcc, these names are in read-only memory.
!So copy them to the heap.

        if stsymbols[i]=ksourcedirsym then
            lxsvalue:=pcm_copyheapstring(lxsvalue)
        fi
        nextlx.length:=strlen(lxsvalue)
        lxhashvalue:=gethashvalue(lxsvalue,nextlx.length)

        if lookup() then
            println stnames[i]
            abortprogram("Duplicate symbol table entry")
        fi

        nextlx.symptr^.symbol:=stsymbols[i]
        nextlx.symptr^.subcode:=stsubcodes[i]
    od

end

function dolexdirective:int=
!positioned just after '#' which is first symbol on a line
!read pp directive and act on it
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
    var ref strec symptr,d
    var ref char p,pstart,s
    var int i,cond,c,syshdr,dir,length, allowmacros
    var [300]char filename

    pstart:=lxsptr

    dir:=getlexdirective()
    if dir=0 then
        printstrn(pstart,lxsptr-pstart); PRINTLN
        lxerror("Invalid # directive")
    fi

    case dir
    when includedir then
        isincludefile:=1

        while lxsptr^=' ' or lxsptr^=tab do ++lxsptr od
        allowmacros:=lxsptr^ <> '<'

        lexm()
        isincludefile:=0

        if nextlx.symbol=ltsym then
            syshdr:=1
            p:=&.filename

            if allowmacros then

                do
                    lexm()
                    case nextlx.symbol
                    when eofsym, eolsym then
                        lxerror("Bad include file")
                    when gtsym then
                        exit
                    else
                        s:=strtoken(&nextlx,length)
                        memcpy(p,s,length)
                        p+:=length
                    esac
                od
            else
                do
                    c:=lxsptr++^
                    case c
                    when '>' then
                        exit
                    when lf,0 then
                        lxerror("include: > expected")
                    else
                        p++^:=c
                    esac
                od
            fi
            p^:=0

        elsif nextlx.symbol=stringconstsym then
            syshdr:=0
            strcpy(&.filename,nextlx.svalue)
        else
            lxerror("include?")
        fi
        lexm()

    IF FSHOWINCLUDES THEN
        PRINTLN "INCLUDE",&.filename,"FROM",sourcefilepaths[getfileno()],nextlx.lineno,
            =nsourcefiles
    FI
    ++NINCLUDES

        stacksourcefile(&.filename,syshdr)
        if not syshdr then
            addautomodule(&.filename,getfileno())
        fi

    when definedir then
        dodefine()

    when undefdir then
        lexreadtoken()
        if nextlx.symbol<>namesym then
            lxerror("undef: name expected")
        fi
        d:=nextlx.symptr
        if d^.nameid<>macroid then
!       println getstname(nextlx.symptr)
!       lxerror("#undef: can't find macro")
        else
            d^.nameid:=nullid
            d^.symbol:=nextlx.symptr^.oldsymbol
            d^.mparamlist:=nil
            d^.attribs.ax_flmacro:=0
        fi

    when ifdefdir then
        cond:=getifdef()
        goto doif

    when ifndefdir then
        cond:=not getifdef()
        goto doif

    when ifdir then
        cond:=getifexpr()
    doif::

        ++ifcondlevel
        if cond then            !carry on reading code as normal
            return 0
        else
    doskipcode::
            dir:=skipcode()
            case dir
            when elifdir then
                cond:=getifexpr()
                if cond then            !do this
                    return 0
                fi
                goto doskipcode
            when elsedir then           !do following code
            when endifdir then
                --ifcondlevel
            esac
        fi

    when elifdir, elsedir then          !encountered after true block
        if not ifcondlevel then
            lxerror("#if missing/elif/else")
        fi
        repeat
            dir:=skipcode()
        until dir=endifdir
        --ifcondlevel

    when endifdir then
        if not ifcondlevel then
            lxerror("#if missing/endif")
        fi
        --ifcondlevel

    when blankdir then
    when linedir then
        repeat
            lexreadtoken()
        until nextlx.symbol=eolsym
    when errordir then
        lexm()
        print "#ERROR:"; showtoken(&nextlx); println
        lxerror("ABORTING")
        goto dowarning2

    when messagedir then
        lexm()
        print "#MESSAGE"
        if nextlx.symbol=eolsym then
            println " Line",nextlx.lineno+1,sourcefilenames[getfileno()]
        else
            showtoken(&nextlx); println
        fi
        goto dowarning2
    when warningdir,pausedir then
        lexm()
        print "#WARNING:"; showtoken(&nextlx); println
    dowarning2::
        while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
        if dir=pausedir then
            print "Press key..."
            os_getch()
            println
        fi

    when pragmadir then
        dopragmadir()

    when debugondir then
        debug:=1

    when debugoffdir then
        debug:=0

    when showmacrodir then
        lexreadtoken()
        if nextlx.symbol=namesym then
            d:=nextlx.symptr
            print nextlx.lineno, sourcefilenames[getfileno()],":"
            PRINT "SHOW MACRO",getstname(d),":"
            if d^.nameid=macroid then
                showtokens("tokens:",d^.tokenlist)
                println
            else
                PRINTLN "not a macro"
            fi
        else
            println "Not a name"
        fi

    else
    skip::
        println "DIRECTIVE NOT IMPL:",sourcedirnames[dir]
        lxsptr:=pstart
        nextlx.symbol:=lexhashsym
        return 1
        lxerror("Directive not implemented")
    esac
    return 0
END

function getlexdirective:int=
!at '#'; read directive, and return index; 0 on error
    var ref strec d

    lexreadtoken()

    case nextlx.symbol
    when namesym then
    when eolsym then
        return blankdir
    when intconstsym then
        repeat
            lexreadtoken()
        until nextlx.symbol=eolsym or nextlx.symbol=eofsym
        return blankdir
    else
        return 0
    esac

    case nextlx.symptr^.symbol
    when ksourcedirsym then
        return nextlx.symptr^.subcode
    when kifsym then
        return ifdir
    when kelsesym then
        return elsedir
    when eolsym then
        return blankdir
    esac

    d:=nextlx.symptr
    if d^.nameid=macroid then           !could have redefined 'define' etc
        if d^.oldsymbol=ksourcedirsym then
            return d^.subcode
        fi
    fi

    return 0
end

global proc startlex(ichar caption,int fileno)=
!s is a 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it

    ifcondlevel:=0
    lx_stackindex:=0
    noexpand:=0

    normaltk := &normaltkx          !indicates use lexm to get tokens

    lx_stackindex:=0
    ifcondlevel:=0
    firstsymbol:=1
    npastedtokens:=0
    isincludefile:=0
    tkptr:=nil

    lxstart:=lxsptr:=sourcefiletext[fileno]
    setfileno(fileno)
    nextlx.lineno:=1
    nextlx.numberoffset:=0

    nextlx.symbol:=eolsym
    nextlx.subcode:=0
    lex()
end

global proc endlex=
    if ifcondlevel then
        println ifcondlevel
        lxerror("#endif missing")
    fi
end

global proc PS(ichar caption)=
    print caption,,":::"
    printsymbol(&lx)
end

global proc PSNEXT(ichar caption)=
    print caption,,":##"
    printsymbol(&nextlx)
end

global function gethashtablesize:int=
    var int i,n

    n:=0
    for i:=0 to hstmask do
        if hashtable^[i]^.name then
            ++n
        fi
    od

    return n
end

proc readlinecomment=
!positioned at second '/' of '//'

    do
        while linecommentmap[(++lxsptr)^] do od     !skip bulk of characters

        case lxsptr^
        when lf then
            ++lxsptr
            exit
        when 0 then
            exit                    !assume on last line not ending in newline char
        when '\\' then
            ++lxsptr
            case lxsptr^
            when cr then            !skip following lf and loop
    ++NALLLINES
                lxsptr+:=2
                ++nextlx.lineno
            when lf then            !loop
                ++lxsptr
    ++NALLLINES
                ++nextlx.lineno
            esac                    !else ignore and loop
!       lxerror("line comment LINE CONT")
        esac
    od
    ++NALLLINES
    ++nextlx.lineno
end

proc readblockcomment=
!positioned at '*' of '/*'

    do
        while commentmap[(++lxsptr)^] do od     !skip bulk of characters

        case lxsptr^
        when lf then
    ++NALLLINES
            ++nextlx.lineno
        when 0 then
            lxerror("block comment eof")
        when '*' then
            if (lxsptr+1)^='/' then     !found end of comment
                lxsptr+:=2
                exit
            fi
        esac
    od
end

proc readhex(ref char pstart)=
!positioned at first char of hex number, after 0x/0X
    var word64 aa
    var int c,length,leading,ll,usigned
    var ref char p

    aa:=0
    p:=lxsptr
    leading:=1
    ll:=usigned:=0
    length:=0

    doswitch c:=lxsptr++^
    when '1'..'9' then
        leading:=0
        aa:=aa*16+(c-'0')
        ++length
    when '0' then
        if leading then
            ++p         !ignore leading zeros
        else
            ++length
            aa:=aa*16
        fi
    when 'A'..'F' then
        leading:=0
        ++length
        aa:=aa*word(16)+(c-'A'+10)
    when 'a'..'f' then
        leading:=0
        ++length
        aa:=aa*word(16)+(c-'a'+10)
    when '.','P','p' then
        --lxsptr
        readrealnumber(pstart,p,lxsptr-p,16)
        return
    when 'L','l' then
        ++ll
        if ll>2 then lxerror("-LL?") fi
    when 'U','u' then
        if usigned then lxerror("-U?") fi
        usigned:=1
    else
        --lxsptr
        exit
    enddoswitch

    setnumberoffset(pstart-lxstart)
    nextlx.length:=lxsptr-pstart

    if length>16 then
        lxerror("Overflow in hex number")
    fi

    nextlx.symbol:=intconstsym
    if aa>0x7FFF'FFFF'FFFF'FFFF then
        nextlx.subcode:=tullong
    elsif aa>0xFFFF'FFFF then
        nextlx.subcode:=tsllong
    elsif aa>0x7FFF'FFFF then
        nextlx.subcode:=tuint
    else
        nextlx.subcode:=tsint
    fi
    nextlx.value:=aa

    checknumbersuffix()
end

proc readbinary(ref char pstart)=
!positioned at first char of binary number, after 0b/0B
    var word64 aa
    var int c,length,res,leading
    var ref char p

    aa:=0
    p:=lxsptr
    leading:=1

    doswitch c:=lxsptr++^
    when '1' then
        leading:=0
    when '0' then
        if leading then ++p fi                  !ignore leading zeros
    when '2'..'9' then
        lxerror("Binary bad digit")
    when '.' then
        lxerror("Binary fp")

    else
        --lxsptr
        exit
    enddoswitch

    length:=lxsptr-p
    setnumberoffset(pstart-lxstart)
    nextlx.length:=lxsptr-pstart

    if length>64 then
        lxerror("Overflow in binary number")
    fi

    to length do
        aa:=aa*2+p++^-'0'
    od

    nextlx.symbol:=intconstsym
    nextlx.subcode:=tsint
    if aa>=0x7FFF'FFFF then
        nextlx.subcode:=tsllong
    fi
    nextlx.value:=aa

    checknumbersuffix()
end

proc readoctal(ref char pstart)=
!positioned at first char of octal number, after 0 (or at 8 or 9)
    var word64 aa
    var int c,length,res,leading,ll,usigned
    var ref char p

    aa:=0
    p:=lxsptr
    leading:=1
    ll:=usigned:=0
    length:=0

    doswitch c:=lxsptr++^
    when '1'..'7' then
        leading:=0
        ++length
    when '0' then
        if leading then
            ++p             !ignore leading zeros
        else
            ++length
        fi
    when '.' then
!   lxerror("Can't do octal/floats")
        --lxsptr
        readrealnumber(pstart,p,lxsptr-p,10)
        return
    when 'L','l' then
        ++ll
        if ll>2 then lxerror("-LL?") fi
    when 'U','u' then
        if usigned then lxerror("-U?") fi
        usigned:=1
    else
        if alphamap[c] then
    doalpha::
            readalphanumeric(pstart)
            return
        fi
        --lxsptr
        exit
    enddoswitch

    setnumberoffset(pstart-lxstart)
    nextlx.length:=lxsptr-pstart

    if length>22 or length=22 and (res:=cmpstringn(p,"1777777777777777777777",22))>0 then
        lxerror("Overflow in octal number")
    fi

    to length do
        aa:=aa*8+p++^-'0'
    od

    nextlx.symbol:=intconstsym
    nextlx.subcode:=tsint
    if aa>=0x7FFF'FFFF then
        nextlx.subcode:=tsllong
    fi
    nextlx.value:=aa

    checknumbersuffix()
end

proc readdecimal(ref char pstart)=
!positioned at first char of decimal number
!will read integer, unless ends with any of ".eE" than assumed to be real
    var word64 aa
    var int c,length,res,leading
    var byte ll,usigned

    var ref char p

    aa:=0
    ll:=usigned:=0

    p:=--lxsptr

    while digitmap[(++lxsptr)^] do od

    while p^='0' do ++p od
    length:=lxsptr-p

    doswitch c:=lxsptr++^
    when '.','E','e' then
        --lxsptr
        readrealnumber(pstart,p,lxsptr-p,10)
        return
    when 'L','l' then
        ++ll
        if ll>2 then lxerror("-LL?") fi
    when 'U','u' then
        if usigned then lxerror("-U?") fi
        usigned:=1
    else
        if alphamap[c] then
            readalphanumeric(pstart)
            return
        fi
        --lxsptr
        exit
    enddoswitch

    setnumberoffset(pstart-lxstart)
    nextlx.length:=lxsptr-pstart

    if length>20 or length=20 and (res:=cmpstringn(p,"18446744073709551615",20))>0 then
        lxerror("Overflow in decimal number")
    fi

    to length do                !A..Z have been preprocessed so that they carry on from '9'
        aa:=aa*word64(10)+word(p++^-'0')
    od

    nextlx.symbol:=intconstsym
    nextlx.subcode:=tsint

    case ll
    when 0,1 then
        if usigned then
            if aa>=0xFFFF'FFFF then
                nextlx.subcode:=tullong
            else
                nextlx.subcode:=tuint
            fi
        else
            if aa>=0x7FFF'FFFF then
                nextlx.subcode:=tsllong
            fi
        fi
    else
        if usigned then
            nextlx.subcode:=tullong
        else
            nextlx.subcode:=tsllong
        fi
    esac

    nextlx.value:=aa
end

function checknumbersuffix:int=
!return type of the constant
!positioned at terminator character which might be a suffix
    var char c

    doswitch c:=lxsptr++^
    when 'L','l','u','U' then
!   lxerror("Numeric SUFFIX")
    else
        if alphamap[c] then
!*!     lxerror("Bad number suffix")
        fi
        --lxsptr
        exit
    enddoswitch

    return tsint            !don't bother for now
end

proc stacksourcefile(ichar file,int syshdr)=
    var ref char sptr
    var int fileno
    var stackinforec info
    var [500]char fullpath

    fileno:=getsourcefile(file,syshdr)
    if fileno=0 then
        println file,strlen(file)
        lxerror("Can't find include file")
    fi

    if lx_stackindex>=maxnesting then
        lxerror("Too many nested includes")
    fi
    ++lx_stackindex

    fullpath[1]:=0
    if lx_stackindex>1 then
        strcpy(&.fullpath,headerpathlist[lx_stackindex-1])
    fi

    if headerpath[1] then
        strcat(&.fullpath,pcm_copyheapstring(&.headerpath))
    fi

    headerpathlist[lx_stackindex]:=pcm_copyheapstring(&.fullpath)

    info.startptr:=lxstart
    info.sptr:=lxsptr
    info.lineno:=nextlx.lineno
    info.fileno:=getfileno()
    lx_stack[lx_stackindex]:=info

    lxstart:=lxsptr:=sourcefiletext[fileno]
    setfileno(fileno)
    nextlx.lineno:=1
end

proc unstacksourcefile=
!called has checked that stack has >=1 entries
    var ichar path
    var stackinforec info

    path:=headerpathlist[lx_stackindex]
    pcm_free(path,strlen(path))

    info:=lx_stack[lx_stackindex--]
    lxstart:=info.startptr
    lxsptr:=info.sptr
    nextlx.lineno:=info.lineno
    setfileno(info.fileno)
end

function getsourcefile(ichar file,int syshdr)int=
!locate using search dirs; 
!read contents into memory, and return fileno
!returns 0 in case of error (file not found, memory problem)

    static var [300]char filespec
    var [300]char filespec2
    var ichar hdrtext
    var int i

    headerpath[1]:=0

    strcpy(&.filespec,file)
    convlcstring(&.filespec)

!check to see if already loaded
    for i:=1 to nsourcefiles do
        if eqstring(&.filespec,sourcefilenames[i]) then
            return i
        fi
    od

!see if a builtin header
    if dointheaders then
        hdrtext:=findheader(&.filespec)
        if hdrtext then
            return loadbuiltin(&.filespec,hdrtext)
        fi
    fi

    strcpy(&.headerpath,extractpath(file))

    if headerpath[1]='/' or headerpath[2]=':' and headerpath[3]='/' then
        if checkfile(file) then
            return loadsourcefile(file,file)
        fi
        return 0            !don't both looking anywhere else
    fi

    for i:=lx_stackindex downto 1 do
        strcpy(&.filespec,headerpathlist[i])
        strcat(&.filespec,file)


        if checkfile(&.filespec) then
            return loadsourcefile(&.filespec,file)
        fi
    od

    for i to nsearchdirs do
        strcpy(&.filespec,searchdirs[i])
        strcat(&.filespec,file)

        if checkfile(&.filespec) then
            strcpy(&.headerpath,extractpath(&.filespec))
            return loadsourcefile(&.filespec,file)
        fi
    od

    return 0
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
    reenter::

    lx:=nextlx              !grab that already read basic token

    lexm()          !read new token for next time around

    if lx.symbol=namesym and lx_stackindex=0 then
        (lx.symptr^.name+lx.length)^:=0
    fi

    docase nextlx.symbol
    when namesym then
        nextlx.symbol:=nextlx.symptr^.symbol            !convert to reserved word, type, op etc
        if nextlx.symbol=ksourcedirsym then
            nextlx.symbol:=namesym
        fi
        nextlx.subcode:=nextlx.symptr^.subcode

        return

    when eolsym then                                !lose eols
        lexm()
    else
        return  
    enddocase

end

proc shownumberstr(ref tokenrec l,filehandle f=nil)=
    var ref char s

    if getfilenox(l) then
        s:=sourcefiletext[getfilenox(l)]+getnumberoffsetx(l)
    else
        s:=pastedtokenlist[l^.pasteno]
    fi
    printstrn(s,l^.length,f)

end

global function addnamestr(ichar name)ref strec=
!look up arbitrary name and return symptr to generic st entry

    var tokenrec oldlx
    var ref strec symptr

    oldlx:=nextlx
    nextlx.length:=strlen(name)
    lxhashvalue:=gethashvalue(name,nextlx.length)

    lxsvalue:=pcm_alloc(nextlx.length+1)
    memcpy(lxsvalue,name,nextlx.length+1)
    lookup()
    symptr:=nextlx.symptr

    nextlx:=oldlx

    return symptr
end

proc lxreadstring(int termchar,int fwide)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter

    const maxlocalstr=2048
    var [maxlocalstr]char str
    var ref char dest,ws
    var ref word16 wd,wd0
    var int c,d,length

    if termchar='"' then
        nextlx.symbol:=(fwide|wstringconstsym|stringconstsym)
    else
        nextlx.symbol:=charconstsym
    fi

    nextlx.svalue:=lxsptr

    if lx_stackindex=0 or fwide then
        dest:=lxsptr                !form string into same buffer
        ws:=dest                    !for wide only
    else                            !for headers that can be re-read, form string externally
        dest:=&.str
    fi
    length:=0

    do
        switch c:=lxsptr++^
        when '\\' then          !escape char
            if isincludefile then
                c:='/'
                goto normalchar
            fi
            c:=lxsptr++^
    reenter::
            switch c
            when 'a' then           !bell ('alert')
                c:=7
            when 'b' then           !backspace
                c:=8
            when 'f' then
                c:=12
            when 'n' then
                c:=lf
            when 'r' then
                c:=cr
            when 't' then           !tab
                c:=tab
            when 'v' then           !vertical tab
                c:=11
            when 'x' then   !2-digit hex code follows
                c:=0
!           to 2 do
                do
                    switch d:=lxsptr++^
                    when 'A','B','C','D','E','F' then
                        c:=c*16+d-'A'+10
                    when 'a','b','c','d','e','f' then
                        c:=c*16+d-'a'+10
                    when '0','1','2','3','4','5','6','7','8','9' then
                        c:=c*16+d-'0'
                    else
                        --lxsptr
                        exit
                    end
                od
            when '0'..'7' then      !octal sequence
                c-:='0'             !get first digit
                to 2 do             !up to 2 more digits (some compilers will read N digits
                    switch d:=lxsptr++^             !then check for overflow)
                    when '0','1','2','3','4','5','6','7' then
                        c:=c*8+d-'0'
                    else
                        --lxsptr
                        exit
                    end
                od

            when '"' then       !embedded double quote
                c:='"'
            when '\\' then
                c:='\\'
            when '\'' then          !embedded single quote
                c:='\''
            when cr then            !skip
    ++NALLLINES
                ++nextlx.lineno
                if lxsptr^=lf then ++lxsptr fi
                next
            when lf then
                next
            end                     !else use the escaped character itself
        when '"','\'' then      !possible terminators
            if c=termchar then      !terminator char
                exit
            fi
        when lf,0 then
            println =nextlx.lineno
            lxerror("String not terminated")
        endswitch
    normalchar::

        if lx_stackindex=0 then
            dest++^:=c
        elsif ++length<maxlocalstr then
            dest++^:=c
        else
            lxerror("Local str too long")
        fi
    od
    dest^:=0


    if fwide then           !need to put string on heap was will use 16-bit chars
        length:=nextlx.length:=dest-nextlx.svalue

        wd0:=wd:=pcm_alloc(length*2+2)
        to length do
            wd++^:=ws++^
        od
        wd^:=0
        nextlx.svalue:=cast(wd0)

    else
        if lx_stackindex=0 then
            nextlx.length:=dest-nextlx.svalue
        else
            nextlx.length:=length

            nextlx.svalue:=pcm_alloc(length+1)
            memcpy(nextlx.svalue,&.str,length+1)
        fi
    fi
end

proc addlisttoken(ref ref tokenrec ulist,ulistx,ref tokenrec p)=
!add strec p to end of linked list headed by ulist^. ulistx^ is current end of list
    if ulist^=nil then      !first
        ulist^:=ulistx^:=p
    else
        ulistx^^.nexttoken:=p
    fi
    p^.nexttoken:=nil

    ulistx^:=p          !update end-of-list pointer
end

proc addlisttoken_copy(ref ref tokenrec ulist,ulistx,ref tokenrec q)=
!like addlisttoken but add copy of nextlx
!(as will likely be in nextlx)
!add strec p to end of linked list headed by ulist^. ulistx^ is current end of list
    var ref tokenrec p

    p:=alloctoken()

    p^:=q^
    p^.nexttoken:=nil

    if ulist^=nil then      !first
        ulist^:=ulistx^:=p
    else
        ulistx^^.nexttoken:=p
    fi
    p^.nexttoken:=nil

    ulistx^:=p          !update end-of-list pointer
end

proc addlist_nextlx(ref ref tokenrec ulist,ulistx)=
!like addlisttoken but add copy of nextlx

    var ref tokenrec p
    p:=alloctoken()
    p^:=nextlx
    p^.nexttoken:=nil

    if ulist^=nil then      !first
        ulist^:=ulistx^:=p
    else
        ulistx^^.nexttoken:=p
    fi
    p^.nexttoken:=nil

    ulistx^:=p          !update end-of-list pointer
end

proc addlisttoken_seq(ref ref tokenrec ulist,ulistx,ref tokenrec seq)=
    var ref tokenrec tk

    while seq do
        tk:=alloctoken()
        tk^:=seq^

        if ulist^=nil then      !first
            ulist^:=ulistx^:=tk
        else
            ulistx^^.nexttoken:=tk
        fi
        tk^.nexttoken:=nil
        ulistx^:=tk

        seq:=seq^.nexttoken
    od
end

proc addlistmparam(ref ref mparamrec ulist,ulistx,ref mparamrec p)=
    if ulist^=nil then      !first
        ulist^:=ulistx^:=p
    else
        ulistx^^.nextmparam:=p
    fi
    ulistx^:=p          !update end-of-list pointer
end

proc dodefine=
!'define' just seen

    var ref mparamrec stlist,stlistx,p,q
    var ref strec stname, d
    var ref tokenrec tklist,tklistx,tk
    var int nparams,ntokens,paramno

    lexreadtoken()
    if nextlx.symbol<>namesym then
        lxerror("define: name expected")
    fi
    stname:=nextlx.symptr
    stname^.lineno:=nextlx.lineno+int(getfileno())<<24

!if stname^.symbol<>namesym then
        stname^.oldsymbol:=stname^.symbol
!fi
    stname^.symbol:=namesym
    stname^.nameid:=macroid
    nparams:=0

    if lxsptr^='(' then
        ++lxsptr
        stlist:=stlistx:=nil
        stname^.attribs.ax_flmacro:=1

        lexreadtoken()
        do
            case nextlx.symbol
            when namesym then           !next param
                d:=nextlx.symptr
                p:=stlist
                while p do
                    if p^.def=d then
                        lxerror("Dupl macro param")
                    fi
                    p:=p^.nextmparam
                od
                q:=pcm_alloc(mparamrec.bytes)
                q^.def:=d
                q^.nextmparam:=nil
                addlistmparam(&stlist,&stlistx,q)
                ++nparams
                lexreadtoken()
!           (d^.name+d^.namelen)^:=0            !zero-term param name; it might be an identifier
                if nextlx.symbol=commasym then
                    lexreadtoken()
                fi
            when rbracksym then
                exit
            when ellipsissym then                   !I need to create a special symbol name
                d:=addnamestr("__VA_ARGS__")
                stname^.attribs.ax_varparams:=1     !flag macro as having a va/args as last param
                lexreadtoken()
                if nextlx.symbol<>rbracksym then
                    lxerror("')' expected")
                fi

                q:=pcm_alloc(mparamrec.bytes)
                q^.def:=d
                q^.nextmparam:=nil
                addlistmparam(&stlist,&stlistx,q)
                ++nparams
                exit
            else
                lxerror("macro params?")
            esac
        od
        stname^.mparamlist:=stlist
    fi

!Now, loop reading tokens until eol
!Store tokens in list
    tklist:=tklistx:=nil
    ntokens:=0

    do
        lexreadtoken()
        case nextlx.symbol
        when eolsym,eofsym then
            exit
        when namesym then
            p:=stname^.mparamlist
            paramno:=1
            while p do
                if p^.def=nextlx.symptr then
                    nextlx.flags ior:=tk_parammask
                    nextlx.paramno:=paramno
                    exit
                fi
                p:=p^.nextmparam
                ++paramno
            od
            if nextlx.symptr=stname then
                nextlx.flags ior:=tk_macromask
            fi
        esac

        ++ntokens
        tk:=alloctoken()
        tk^:=nextlx
        addlisttoken(&tklist,&tklistx,tk)
    od

    stname^.tokenlist:=tklist
    stname^.attribs.ax_nparams:=nparams
end

proc readalphanumeric(ref char pstart)=
!part-read numeric value starting at pstart is followed by non-numeric chars
!read rest of token starting from lxsptr, and form into a name token
    while alphamap[lxsptr++^] do od
    --lxsptr
    nextlx.svalue:=pstart
    nextlx.symbol:=rawnumbersym
    nextlx.length:=lxsptr-pstart
end

function inmacrostack(ref strec d, ref tokenrec macrostack)int=
!return 1 if d is part of the macrostack
!the macrostack is a linked list of strecs, but conveniently uses a list
!of tokens although it is not really a list of tokens

    while macrostack do
        if macrostack^.symptr=d then return 1 fi
        macrostack:=macrostack^.nexttoken
    od
    return 0
end

proc showtokens(ichar caption,ref tokenrec tk)=
    print caption,,"<"
    while tk do
        showtoken(tk)
        tk:=tk^.nexttoken
    od
    println ">"
end

proc lexa(ref tokenrec &tk)=
    if tk=normaltk then
        lexreadtoken()
        return
    fi
    if tk=nil then
        nextlx.symbol:=eofsym
        return
    fi
    nextlx:=tk^
    tk:=tk^.nexttoken
end

proc lexm=
!wrapper around lexreadtoken that applies macro expansion to names
    var ref strec d
    static var int doreset=0
    var int newlineno

    do
        if tkptr then
            nextlx:=tkptr^
            tkptr:=tkptr^.nexttoken
            if tkptr=nil then

                if nextlx.symbol=namesym and nextlx.symptr^.nameid=macroid and peeklb() then
!fix pp bug: macro expansion ending with fn-macro name, with (...) following
!but at normal lexical level. Pick that up here
                    setfileno(sfileno)
                    nextlx.lineno:=slineno
                    doreset:=0
                    goto TEST1
                fi
                doreset:=1

            fi
            return
        fi

        if doreset then
            setfileno(sfileno)
            nextlx.lineno:=slineno
            doreset:=0
        fi

        if firstsymbol then
            firstsymbol:=0
            dospecialinclude()
        fi  
        lexreadtoken()
    TEST1::

        case nextlx.symbol
        when lexhashsym then

            if dolexdirective() then
                return
            fi
            next
        when namesym then
            d:=nextlx.symptr
            case d^.symbol
            when predefmacrosym then
                sfileno:=getfileno()
                slineno:=nextlx.lineno
                expandpredefmacro(d^.subcode,&nextlx,nextlx.lineno)
                doreset:=1                  !can screw up line/file numbers
                return
            else
                if d^.nameid<>macroid or noexpand then
                    return
                fi
            esac
        else
            return
        esac
!have a macro. Now see whether this should be expanded
        sfileno:=getfileno()
        slineno:=nextlx.lineno
        if d^.attribs.ax_flmacro then       !function-like macro; need to peek for "("
            if not peeklb() then
                return
            fi
            tkptr:=expandfnmacro(d,nil,normaltk,1,newlineno)
            slineno:=newlineno
        else                                        !object-like macro: always expand
            tkptr:=expandobjmacro(d,nil,normaltk,1)
        fi

        if tkptr=nil then doreset:=1 fi         !immediately restore file/lineno

    od
end

function peeklb:int=
!look at lxsptr seqence and return 1 if the next token is a "(" left bracket
!lxsptr is left unchanged whatever the result
!only a simplistic approach is used, eg. 0 or 1 space then a "(" must be next
!In theory, there could be any number and combination of spaces, tabs, newlines,
!comments, strings, #-directives between this point and the next token, or
!it could be inside the next #include or just outside this one.
    if lxsptr^='(' or (lxsptr^=' ' and (lxsptr+1)^='(') then
        return 1
    fi
    return 0
end

function peektk(ref tokenrec tk)int=
!version of peeklb that works on a token list rather than chars
!tk is the current token
    tk:=tk^.nexttoken
    if tk=nil then          !nothing follows
        return 0
    fi
    if tk^.symbol=lbracksym then
        return 1
    fi
    return 0
end

function expandobjmacro(ref strec m,ref tokenrec macrostack, &tksource,
        int frombaselevel)ref tokenrec=
    var ref tokenrec tk,p,repl
    var tokenrec newmacro
    var int iscomplex,useshh,expanded
    var ref strec d

    p:=tk:=m^.tokenlist

    iscomplex:=useshh:=0
    while p do
        if p^.symbol=namesym then
            d:=p^.symptr
            if d^.nameid=macroid or d^.symbol=predefmacrosym then
                iscomplex:=1
                exit
            fi
        elsif p^.symbol=hashhashsym then
            iscomplex:=useshh:=1
            exit
        fi

        p:=p^.nexttoken
    od

    if not iscomplex then
        return tk
    fi

    newmacro.symptr:=m              !add m to macrostack
    newmacro.nexttoken:=macrostack

    if useshh then
        repl:=substituteargs(m,nil,nil,0,nil)
    else
        repl:=m^.tokenlist
    fi

    tk:=scantokenseq(repl,&newmacro,expanded)
    return tk
end

function expandfnmacro(ref strec m, ref tokenrec macrostack, &tksource,
        int frombaselevel, &endlineno)ref tokenrec=
!positioned just before "(" token
!read arguments from source (need to use lexm(), reading from char-sourc or tokenlist)
!store args in special arg lists, and prepare args for expansion
!(for this version, args expanded on demand only)
!get tokenlist for m, do argument substitution, then scan it looking for new
!macros to expand
    var [maxmacroargs]ref tokenrec args,expargs
    var ref tokenrec repl,tk
    var tokenrec newmacro
    var int nargs,i,expanded

    nargs:=readmacrocall(m,&args,tksource)
    if frombaselevel then
        endlineno:=nextlx.lineno
    fi

    for i:=1 to nargs do
        expargs[i]:=nil
!   expargs[i]:=scantokenseq(args[i], macrostack)
    od

    repl:=substituteargs(m,&args,&expargs,nargs,macrostack)

    newmacro.symptr:=m              !add m to macrostack
    newmacro.nexttoken:=macrostack

    repl:=scantokenseq(repl,&newmacro,expanded)
    return repl
end

function scantokenseq(ref tokenrec tk, macrostack,int &expanded)ref tokenrec=
!scan token sequence belonging to:
! The replacelist of an object macro
! The substituted replacement list of a function macro
! An argument of a macro
!scan object macro, but can also be an argument

!d is an object macro that may contains further macro definitions
!scan it, and produce a new tokenlist that contains expanded versions
!of nested macro calls
!macrostack is a list of active nested macro defs. This is stored as
!a linked list of tokenrec records, in reverse order. This is just for
!convenience; the .symptr field is used to refer to the macro st entry

    var ref tokenrec newtk,newtkx   !new list of tokens
    var ref tokenrec expandtk       !token seqence from expanding a macro
    var ref tokenrec oldtk
    var ref strec m
    var tokenrec newmacro
    var int noexpandflag,simple,dummy

    reenter::
    expanded:=0

    newtk:=newtkx:=nil
    noexpandflag:=0

    simple:=1
    oldtk:=tk

    while tk do
        case tk^.symbol
        when namesym then
            if tk^.symptr^.nameid=macroid or tk^.symptr^.symbol=predefmacrosym then
                simple:=0
                exit
            fi
        esac

        if tk=nil then exit fi
        tk:=tk^.nexttoken
    od

    if simple then
        return oldtk
    fi

    tk:=oldtk
    while tk do
        case tk^.symbol
        when namesym then
            m:=tk^.symptr
            if m^.nameid=macroid and not noexpandflag then
!macro detected; check if candidate for expansion
                if tk^.flags iand tk_macrolit or noexpand then
                    goto simpletoken
                fi

                if inmacrostack(m,macrostack) then      !is an active macro name
                    addlisttoken_copy(&newtk,&newtkx,tk)
                    newtkx^.flags ior:= tk_macrolit
                    goto skip

                fi
    simple:=0
                if m^.attribs.ax_flmacro then
                    if not peektk(tk) then goto simpletoken fi
                    lexa(tk)
                    expandtk:=expandfnmacro(m,macrostack,tk,1,dummy)
                    addlisttoken_seq(&newtk,&newtkx,expandtk)
                    expanded:=1
                    next
                else
                    expandtk:=expandobjmacro(m,macrostack,tk,0)
                    expanded:=1
                    addlisttoken_seq(&newtk,&newtkx,expandtk)
                fi
            elsif m^.symbol=kdefinedsym then
                noexpandflag:=1
                goto simpletoken
            elsif m^.symbol=predefmacrosym then
                expandtk:=alloctokenz()
                expandpredefmacro(m^.subcode,expandtk,nextlx.lineno)
                addlisttoken_copy(&newtk,&newtkx,expandtk)
                goto skip2
            else
                noexpandflag:=0
                goto simpletoken
            fi
        else
    simpletoken::
            addlisttoken_copy(&newtk,&newtkx,tk)
        esac

    skip::
        if tk=nil then exit fi
    skip2::
        tk:=tk^.nexttoken
    od

    if expanded then
        tk:=newtk
        goto reenter
    fi

    return newtk
end

function readmacrocall(ref strec d, ref[]ref tokenrec args, ref tokenrec &tksource)int=
!positioned just before "(" of a macro call
!read arguments for the macro, and store into args
!return total number of arguments
!each args^[i] entry is a list of tokenrecs
!Caller has already checked that "(" is next token, and this will be a function macro
!tksource will point to an input stream of tokens, but can also be nil, meaning
!read via lexm from actual source. (tksource can't be nil because it's at the
!end of ...

    var int nparams,lbcount,paramno
    var int nargs,usesvargs,varg
    var ref tokenrec tklist,tklistx         !form list of tokens for argument

    lexa(tksource)

    if nextlx.symbol<>lbracksym then lxerror("rmc: no '('") fi

    nparams:=d^.attribs.ax_nparams
    nargs:=0
    if nparams=0 then               !) must follow
        lexa(tksource)
        if nextlx.symbol<>rbracksym then lxerror("rmc: ')' expected") fi
        return 0                    !no args
    fi

    paramno:=1
    lbcount:=1
    tklist:=tklistx:=nil
    usesvargs:=d^.attribs.ax_varparams          !whether macro contains ... va/args
    varg:=0                                     !whether encountered ... yet in arguments

    do
        if paramno=nparams and usesvargs then varg:=1 fi
        lexa(tksource)

        case nextlx.symbol
        when commasym then
            if lbcount=1 and not varg then
                if tklist=nil then                  !empty list: create place-holder token
                    tklist:=alloctokenz()
                    setfilenox(tklist,getfileno())
                    tklist^.symbol:=placeholdersym
                fi
                args^[paramno]:=tklist              !store this list
                tklist:=tklistx:=nil
                ++paramno
            else
                goto addtoken
            fi

        when eofsym then
            lxerror("EOS in macro call")
        when lbracksym then
            ++lbcount
            goto addtoken
        when rbracksym then
            if lbcount>1 then
                --lbcount
                addlist_nextlx(&tklist,&tklistx)
            else
                if tklist=nil then
                    tklist:=alloctokenz()
                    setfilenox(tklist,getfileno())
                    tklist^.symbol:=placeholdersym
                fi
                args^[paramno]:=tklist              !store this list
                exit
            fi
        else
    addtoken::
            addlist_nextlx(&tklist,&tklistx)
        esac
    od

    if paramno<>nparams then
        if paramno+1=nparams and usesvargs then     !no args for ... part, needs dummy arg
            args^[nparams]:=nil
        else
            lxerror("Wrong # macro params")
        fi
    fi
    return nparams
end

function substituteargs(ref strec m,ref[]ref tokenrec args,expargs, int nargs,
ref tokenrec macrostack)ref tokenrec=
!m is a macro def
!args/expargs are arguments that will replace any parameter names encountered
!in m's replacement list
!returns new replacement list with arguments inserted
    var ref mparamrec params
    var ref tokenrec seq,seqstart,lasttoken
    var ref tokenrec newtk,newtkx,niltk,tkexp
    var tokenrec tk
    var int n,i,expanded

    const maxhashhash=100
    var [maxhashhash]ref tokenrec hhpoints
    var int nhashhash

    params:=m^.mparamlist
    seq:=seqstart:=m^.tokenlist     !input token sequence

    newtk:=newtkx:=nil              !output token sequence
    nhashhash:=0
    lasttoken:=nil

    while seq do
        case seq^.symbol
        when hashsym then
            if nargs then
                seq:=seq^.nexttoken
                if seq=nil then lxerror("# at end") fi
                unless seq^.flags iand tk_parammask then
                    lxerror("# not followed by param")
                end unless
                n:=seq^.paramno

                stringify(args^[n],&tk)

                addlisttoken_copy(&newtk,&newtkx,&tk)
            else
                addlisttoken(&newtk,&newtkx,seq)
                newtkx^.symbol:=lithashsym              !change to #'
            fi
        when hashhashsym then
            if seq=seqstart then lxerror("## at start") fi
            if nhashhash>=maxhashhash then lxerror("Too many ##") fi
            hhpoints[++nhashhash]:=newtkx

        elsif seq^.symbol=namesym and seq^.flags iand tk_parammask and nargs then       !args can be () if no "(...)" followed
            n:=seq^.paramno
            if seq^.nexttoken and seq^.nexttoken^.symbol=hashhashsym or \
               lasttoken and lasttoken^.symbol=hashhashsym then
                addlisttoken_seq(&newtk,&newtkx,args^[n])
            else
                tkexp:=expargs^[n]
                if tkexp=nil then
                    tkexp:=expargs^[n]:=scantokenseq(args^[n],macrostack,expanded)
                fi
                addlisttoken_seq(&newtk,&newtkx,tkexp)
            fi

        else
    doother::
            addlisttoken_copy(&newtk,&newtkx,seq)
        esac

        lasttoken:=seq
        seq:=seq^.nexttoken
    od

    if nhashhash then
        niltk:=nil
        for i:=1 to nhashhash do
            pastetokens(hhpoints[i],(i<nhashhash | hhpoints[i+1]| niltk))
        od
    fi

    return newtk
end

function strtoken(ref tokenrec lp,int &length)ichar=
!convert token to a string
!return pointer to the string *which is likely to be unterminated*
!return length of the string in 'length'
!(not sure yet if -1 is a possible length, meaning the string is zero-terminated)
!display token contents naturally
!note that caller should copy the string involved as no promises can be 
!made to ownership
    var ichar name,s
    var tokenrec l
    l:=lp^

    case l.symbol
    when namesym then
    doname::
        length:=l.symptr^.namelen
        return l.symptr^.name

    when intconstsym,realconstsym then
        length:=l.length


        if getfilenox(&l) then
            return sourcefiletext[getfilenox(&l)]+getnumberoffsetx(&l)
        else
            return pastedtokenlist[l.pasteno]
        fi
    when rawnumbersym then
        length:=l.length
        return l.svalue

    when stringconstsym,wstringconstsym then
        s:=strstring(l.svalue,l.length,length,'"')
        return s

    when charconstsym then
        s:=strstring(l.svalue,l.length,length,'\'')
        return s

    when eolsym then
        if dowhitespace then
            length:=l.length+1
            s:=pcm_alloc(length)
            s^:=10      !'\n'
            memcpy(s+1,l.svalue,l.length)
        else
            length:=1
            return "\n"
        fi
        return s

    when eofsym then
        length:=0
        return ""

    when ktypespecsym, ktypequalsym, klinkagesym, kfnspecsym then
        goto doname

    else
        name:=shortsymbolnames[l.symbol]
        if length:=strlen(name) then
            if name^<>'k' then
                return name
            else
                length:=strlen(symbolnames[l.symbol]+1)
                return symbolnames[l.symbol]+1
            fi
        else
            return ""
        fi
    esac
    return ""
end

function strstring(ichar s,int length,&newlength,quotechar)ichar=
!stringify the string, which means converting control codes to
!escape sequences, and adding optional quotes

    var ichar t,u

    t:=u:=pcm_alloc(length*2+4)
    if quotechar then
        u^:=quotechar
        ++u
    fi
    convertstring(s,u,length)
    newlength:=strlen(t)
    if quotechar then
        (t+newlength)^:=quotechar
        ++newlength
    fi
    return t
end

var int lasttoken=0

global proc emittoken(ref tokenrec lp,ref strbuffer dest,int forcespace=0)=
!display token contents naturally
    var int length
    var ichar s

    if lp^.symbol=eolsym and lasttoken=eolsym then
        return
    fi

    s:=strtoken(lp,length)

    if forcespace or needspace(lasttoken,lp^.symbol) then
        gs_char(dest,' ')
    fi

    gs_strn(dest,s,length)


    lasttoken:=lp^.symbol
end

global proc showtoken(ref tokenrec lp)=
    static var strbuffer buffer
    static var ref strbuffer dest=&buffer

    gs_init(dest)
    
    emittoken(lp,dest)
    
print dest^.length:"v",,dest^.strptr:".*"
end

proc stringify(ref tokenrec seq,dest)=
!stringify single or multiple token sequence, and store result as a single
!string token in dest
    var ref char s
    var int length,addspace
    static var strbuffer buffer
    static var ref strbuffer deststr=&buffer

    dest^.symbol:=stringconstsym
    dest^.nexttoken:=nil

    if seq^.nexttoken=nil then      !single
        s:=strtoken(seq,length)
        dest^.length:=length
        dest^.svalue:=s
        return 
    fi

!now do multiple tokens into one string
    gs_init(deststr)
    lasttoken:=0
    addspace:=0
    while seq do
        emittoken(seq,deststr,forcespace:addspace)
        addspace:=1
        seq:=seq^.nexttoken
    od

    dest^.length:=length
    dest^.svalue:=deststr^.strptr
    dest^.length:=deststr^.length
end

proc pastetokens(ref tokenrec tk, &tknext)=
!tk points into a token sequence
!paste the token at tk with the one at tk^.nexttoken, and replace
!tk with the new composite token; tk^.nexttoken is removed
!tknext is either nil, or refers to the next pair of tokens to be pasted together;
!there is a problem when tk^.nexttoken and tknext coincide, so something needs to
!be done in that case (set tknext to point to tk)

    var ref tokenrec tk2
    var int length1,length2
    var ref char s,t,u
    var tokenrec oldtoken,token
    var ref char oldlxsptr
    var int oldlx_stackindex

    tk2:=tk^.nexttoken
    if tk2=tknext then tknext:=tk fi
    tk^.nexttoken:=tk2^.nexttoken               !lose second token

    if tk^.symbol=placeholdersym then
        if tk2^.symbol=placeholdersym then          !two placeholders; leave only first
        else                                        !ph/token; use second
            tk^:=tk2^                               !also unlinks the tk2 token
        fi
    elsif tk2^.symbol=placeholdersym then           !token/ph; leave only first
    else                        !two normal tokens

        s:=strtoken(tk,length1)
        t:=strtoken(tk2,length2)

        u:=pcm_alloc(length1+length2)
        memcpy(u,s,length1)
        memcpy(u+length1,t,length2)
        (u+length1+length2)^:=0

        if npastedtokens>=maxpastedtokens then
            lxerror("Too many pasted tokens")
        fi
        pastedtokenlist[++npastedtokens]:=u

        oldtoken:=nextlx
        oldlxsptr:=lxsptr
        oldlx_stackindex:=lx_stackindex

        lxsptr:=u
        lx_stackindex:=0

        setfileno(0)
        nextlx.lineno:=0
        lexreadtoken()
        token:=nextlx
        lexreadtoken()

        if nextlx.symbol<>eofsym then
!       lxerror("token-paste error")
        fi

        nextlx:=oldtoken
        lxsptr:=oldlxsptr
        lx_stackindex:=oldlx_stackindex

        token.nexttoken:=tk^.nexttoken
        setfilenox(&token,0)
        token.pasteno:=npastedtokens

    token.flags ior:=tk_pasted
        tk^:=token
    fi
end

function getifexpr:int=
    var int sx
    var int x

    lexm()
    x:=evalcondexpr(sx)

    if nextlx.symbol<>eolsym then
        lxerror("#if:eol expected")
    fi

    return x<>0
end

function evalcondexpr(int &sx)i64=
!Main entry point for pp expressions
!Will do conditional ?: expressions here
!Positioned at first symbol of expression, which is in nextlx (if a macro
!it will have been expanded, and this is the first token of that expansion)
    var i64 x,y,z
    var int sy,sz

    x:=evalorexpr(sx)

    if nextlx.symbol=questionsym then
        lexm()
        y:=evalcondexpr(sy)
        if nextlx.symbol<>colonsym then lxerror(": expected") fi
        lexm()
        z:=evalcondexpr(sz)
        if x then
            sx:=sy
            x:=y
        else
            sx:=sz
            x:=z
        fi
    fi

    return x
end

function evalorexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evalandexpr(sx)
    while nextlx.symbol=orlsym do
        lexm()
        y:=evalandexpr(sy)
        x := (x or y|1|0)
    od

    return x
end

function evalandexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evaliorexpr(sx)
    while nextlx.symbol=andlsym do
        lexm()
        y:=evaliorexpr(sy)
        x := (x and y|1|0)
    od

    return x
end

function evaliorexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evalixorexpr(sx)
    while nextlx.symbol=iorsym do
        lexm()
        x ior:= evalixorexpr(sy)
    od

    return x
end

function evalixorexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evaliandexpr(sx)
    while nextlx.symbol=ixorsym do
        lexm()
        x ixor:= evaliandexpr(sy)
    od

    return x
end

function evaliandexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evaleqexpr(sx)
    while nextlx.symbol=iandsym do
        lexm()
        x iand:= evaleqexpr(sy)
    od

    return x
end

function evaleqexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evalcmpexpr(sx)
    while (opc:=nextlx.symbol)=eqsym or opc=nesym do
        lexm()
        y:=evalcmpexpr(sy)
        case opc
        when eqsym then x := x = y
        when nesym then x := x <> y
        esac
    od

    return x
end

function evalcmpexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evalshiftexpr(sx)
    while (opc:=nextlx.symbol)=ltsym or opc=lesym or opc=gesym or opc=gtsym do
        lexm()
        y:=evalshiftexpr(sy)
        case opc
        when ltsym then x := x < y
        when lesym then x := x <= y
        when gesym then x := x >= y
        when gtsym then x := x > y
        esac
    od

    return x
end

function evalshiftexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evaladdexpr(sx)
    while (opc:=nextlx.symbol)=shlsym or opc=shrsym do
        lexm()
        y:=evaladdexpr(sy)
        case opc
        when shrsym then
            x := x>>y
        when shlsym then
            x := x<<y
        esac
    od

    return x
end

function evaladdexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evalmulexpr(sx)
    while (opc:=nextlx.symbol)=addsym or opc=subsym do
        lexm()
        y:=evalmulexpr(sy)
        case opc
        when addsym then
            x +:= y
        when subsym then
            x -:= y
        esac
    od

    return x
end

function evalmulexpr(int &sx)i64=
    var i64 x,y
    var int sy,opc

    x:=evalunaryexpr(sx)
    while (opc:=nextlx.symbol)=mulsym or opc=divsym or opc=remsym do
        lexm()
        y:=evalunaryexpr(sy)
        if y=0 and opc<>mulsym then lxerror("#if:div by zero") fi
        case opc
        when mulsym then
            x *:= y
        when divsym then
            x := x/y
        when remsym then
            x := x rem y
        esac
    od

    return x
end

function evalunaryexpr(int &sx)i64=
    var i64 x
    var int opc

    case nextlx.symbol
    when addsym, subsym, notlsym, inotsym then
        opc:=nextlx.symbol
        lexm()
        x:=evalunaryexpr(sx)
        case opc
        when addsym then
            return x
        when subsym then
            return -x
        when notlsym then
            return not x
        when inotsym then
            return inot x
        esac
    esac

    return evalterm(sx)
end

function evalterm(int &sx)i64=
    var i64 res
    var int lb

    sx:=1
    case nextlx.symbol
    when namesym then
        case nextlx.symptr^.symbol
        when kdefinedsym then
            noexpand:=1
            lb:=0
            lexm()
            if nextlx.symbol=lbracksym then
                lb:=1;
                lexm()
            fi
            if nextlx.symbol<>namesym then lxerror("defined?") fi
            res:=nextlx.symptr^.nameid=macroid
            lexm()
            if lb then
                if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
                lexm()
            fi
            noexpand:=0
        when ksizeofsym then
            lexm()
            if nextlx.symbol<>lbracksym then lxerror("'(' expected") fi
            lexm()
            if nextlx.symbol<>namesym then lxerror("name expected") fi
            case nextlx.symptr^.symbol
            when ktypespecsym then
                res:=typespecsizes[nextlx.symptr^.subcode]
            else
                lxerror("sizeof2")
            esac
            lexm()
            if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
            lexm()
    
        else
            lexm()
            return 0
        esac
    when intconstsym then
        res:=nextlx.value
        lexm()
    when charconstsym then
        if nextlx.length=0 then
            res:=0
        else
            res:=nextlx.svalue^
        fi
        lexm()
    when lbracksym then
        lexm()
        res:=evalcondexpr(sx)
        if nextlx.symbol<>rbracksym then
            lxerror(") expected")
        fi
        lexm()
    else
    printsymbol(&nextlx)
    printstrn(nextlx.svalue,nextlx.length); PRINTLN
        lxerror("evalterm?")
    esac

    return res
end

function getifdef:int=
!just read ifdef/ifndef
!read following name and return 1 if name is macro, or 0 if not
    var int res
    var ref strec d

    noexpand:=1
    lexreadtoken()
    noexpand:=0
    if nextlx.symbol<>namesym then lxerror("Name expected") fi
    d:=nextlx.symptr
    res:=0
    if d^.nameid=macroid then
        res:=1
    elsif d^.symbol=predefmacrosym then
        res:=1
    fi

    lexreadtoken()
    if nextlx.symbol<>eolsym then lxerror("EOL expected") fi
    return res
end

function skipcode:int=
!skip false branch of #if etc until matching #elif/else/endif
!return dir-code of that closing directive
    var int level,dir
    var ref byte pp

    level:=0                        !count nested #if levels

    do

        fastreadtoken()

        case nextlx.symbol
        when lexhashsym then
            dir:=getlexdirective()
            case dir
            when ifdir, ifdefdir, ifndefdir then
                ++level
            when elifdir, elsedir then
                if level=0 then
                    return dir
                fi
            when endifdir then
                if level=0 then
                    return dir
                fi
                --level
            esac
        when eofsym then
            lxerror("#if:Unexpected eof")
        esac
    od
    return 0
end

proc freetokens(ref tokenrec tk)=
    var ref tokenrec nexttk

    while tk do
        nexttk:=tk^.nexttoken
        tk:=nexttk
    od
end

global proc fastreadtoken=
!read next token into nextlx
    var int c,csum,hsum,commentseen,dodir,j
    var ref char pstart,p
    var ichar ss

    nextlx.subcodex:=0

    doswitch lxsptr++^
    when '#' then           !
        p:=lxsptr-2
        dodir:=0
        while p>=lxstart do
            case p^
            when lf then        !# is first thing on a line
                dodir:=1
                exit
            when tab,' ' then   !might have leading white space
            else
                exit            !assume different hash symbol
            esac
            --p
        od
        if dodir or p<lxstart then
            nextlx.symbol:=lexhashsym
        return

        elsif lxsptr^='#' then
            ++lxsptr
        fi

    when '/' then
        case lxsptr^
        when '/' then                   !comment to 
            readlinecomment()
        when '*' then
            readblockcomment()
        esac
    
    when '\'' then
        lxreadstring('\'',0)

    when '"' then
        lxreadstring('"',0)

    when cr then
    ++NALLLINES
        ++nextlx.lineno
        nextlx.symbol:=eolsym
        nextlx.length:=0
        ++lxsptr                !skip lf
    when lf then            !only lfs not preceded by cr
        ++nextlx.lineno
++NALLLINES
        nextlx.symbol:=eolsym
        nextlx.length:=0

    when 0 then
        --lxsptr
        if lx_stackindex then
            unstacksourcefile()
        else
            nextlx.symbol:=eofsym
            return
        fi
    
    when 12 then
    end doswitch
end

function alloctoken:ref tokenrec=
    var ref tokenrec tk
    tk:=pcm_alloc(tokenrec.bytes)
    return tk
end

function alloctokenz:ref tokenrec=
    var ref tokenrec tk
    tk:=pcm_alloc(tokenrec.bytes)
    tk^.nexttoken:=nil
    return tk
end

proc expandpredefmacro(int pdmcode,ref tokenrec tk,int lineno)=
    var [256]char str
    static var []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    var rsystemtime tm
    var ichar s
    var int fileno

    if noexpand then
        return
    fi

    case pdmcode
    when pdm_date then
        os_getsystime(&tm)

        fprint @&.str, "#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

        tk^.symbol:=stringconstsym
        tk^.svalue:=pcm_copyheapstring(&.str)

    when pdm_time then
        os_getsystime(&tm)

        fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

        tk^.symbol:=stringconstsym
        tk^.svalue:=pcm_copyheapstring(&.str)
    when pdm_file then
        tk^.symbol:=stringconstsym
        fileno:=getfilenox(tk)
        if fileno=0 then fileno:=sfileno fi
        if sfileno then
            tk^.svalue:=sourcefilenames[sfileno]
        else
            tk^.svalue:="(File not available)"
        fi
    when pdm_func then
        tk^.symbol:=stringconstsym
        if currproc then
            tk^.svalue:=currproc^.name
        else
            tk^.svalue:="???"
        fi
    when pdm_line then
        tk^.symbol:=intconstsym
        tk^.value:=lineno
    when pdm_stdc then
        tk^.symbol:=intconstsym
        tk^.value:=1
    when pdm_bcc then
        tk^.symbol:=intconstsym
        tk^.value:=1
    else
        println pdmcode
        lxerror("PDM")
    esac

    if tk^.symbol=stringconstsym then
        tk^.length:=strlen(tk^.svalue)
        tk^.subcode:=trefchar
    else
        tk^.subcode:=tsint
        s:=pcm_alloc(16)
!   sprintf(s,"%lld",tk^.value)
        getstrint(tk.value,s)
        tk^.length:=strlen(s)
        if npastedtokens>=maxpastedtokens then
            lxerror("2:Too many pasted tokens")
        fi
        pastedtokenlist[++npastedtokens]:=s
        setfilenox(tk,0)
        tk^.pasteno:=npastedtokens
    fi
end

proc dopragmadir=
    lexm()
    if nextlx.symbol=namesym then
        if memcmp(nextlx.symptr^.name,"pack",4)=0 then
            lexm()
            if nextlx.symbol<>lbracksym then lxerror("'(' expected") fi
            lexm()
            if nextlx.symbol=intconstsym then
                case nextlx.value
                when 1 then
                    structpadding:=0
                else
                    goto finish
                    lxerror("Only pack(1) or () allowed")
                esac
                lexm()
            elsif nextlx.symbol=rbracksym then
                structpadding:=1
            fi
        elsif memcmp(nextlx.symptr^.name,"$callback",9)=0 then
            callbackflag:=1
        fi
    fi
finish::
    while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
end

function needspace(int a,b)int=
    var ichar aname, bname

    if a=0 then return 0 fi         !first token

    aname:=shortsymbolnames[a]
    bname:=shortsymbolnames[b]

    case bname^
    when 'n','k' then
        case aname^
        when 'n','k' then
            return 1
        esac
    when '-','+' then
        case aname^
        when '-','+' then
            return 1
        esac
    esac

    return 0
end

global proc dospecialinclude=
    ++NINCLUDES
    stacksourcefile("bcc.h",1)
    if dheaderfile then
        stacksourcefile(dheaderfile,1)
    fi
end

proc addautomodule(ichar headername,int fileno)=
    var ichar cfilename
    var ichar headerfile
    var int present

    headerfile:=sourcefilepaths[fileno]

    if not fautomodules then
        return
    fi

    if eqstring(extractext(headerfile),"c") then        !ignore .c files
        return
    fi

    cfilename:=changeext(headerfile,"c")
    if checkfile(cfilename) then
        present:=1
        for i:=1 to nautomodules do
            if eqstring(automodulenames[i],cfilename) then
                present:=0
                exit
            fi
        od
        if present then
            automodulenames[++nautomodules]:=pcm_copyheapstring(cfilename)
        fi
    fi
end

proc setnumberoffset(int offset)=
!store offset into nextlx.numberoffset
!except that top byte is msb of fileno
    nextlx.numberoffset:=(nextlx.numberoffset iand 0xFF000000) ior (offset iand 0xFFFFFF)
end

proc setfileno(int fileno)=
    nextlx.fileno:=fileno iand 255
    nextlx.numberoffset := (nextlx.numberoffset iand 0xFFFFFF) ior((fileno iand 0xFF00)<<16)
end

proc setfilenox(ref tokenrec tk,int fileno)=

    tk^.fileno:=fileno iand 255
    tk^.numberoffset := (tk^.numberoffset iand 0xFFFFFF) ior (fileno iand 0xFF00)<<16
end

function getfileno:int=
    return (nextlx.numberoffset>>24)<<8 ior nextlx.fileno
end

function getfilenox(ref tokenrec tk)int=
    return (tk^.numberoffset>>24)<<8 ior tk^.fileno
end

function getnumberoffsetx(ref tokenrec tk)int=
    return tk^.numberoffset iand 0xFFFFFF
end

global proc freehashtable=
!free the user name entries in the hash table
!leave reserved words etc alone
    var ref strec d,e,f

    for i:=0 to hstmask do
        d:=hashtable^[i]
        if d^.name and d^.symbol=namesym then
            if d^.nameid=macroid then
                freetokens(d^.tokenlist)
            fi
            f:=d^.nextdupl
            while f do
                freestentry(f)
                e:=f^.nextdupl
                pcm_free(f,strec.bytes)
                f:=e
            od
            pcm_clearmem(hashtable^[i],strec.bytes)
        elsif d^.name then
            d^.nextdupl:=nil
        fi
    od
end

proc freestentry(ref strec d)=
end

proc regenlookup(ref strec d)=
    var int j, wrapped,length
    var ref strec e

    j:=gethashvalue(d^.name,d^.namelen) iand hstmask
    wrapped:=0

    do
        e:=hashtable^[j]
        length:=e^.namelen

        if not length then
PCM_FREE(HASHTABLE^[J],STREC.BYTes)
            hashtable^[j]:=d
            ++nhstsymbols
            return
        fi

        if length=d^.namelen then   !match on length
            if memcmp(e^.name,d^.name,length)=0 then    !match
                lxerror("regenhst dupl?")
            fi
        fi

        if ++j>=hstsize then
            if wrapped then
                abortprogram("REGENHST FULL?")
            fi
            wrapped:=1
            j:=0
        fi
    od
end

proc printhashtable(ichar caption)=
    var ref strec d

    println caption,,":"
    for i:=0 to  hstsize-1 do
        d:=hashtable^[i]
        if d^.name then
    IF EQSTRING(D^.NAME,"char") then PRINTLN "********************** CHAR" FI
            println i,":",d^.name
        else
            println i,": ----"
        fi
    IF I REM 30=0 THEN OS_GETCH() FI
    od
    println
end

proc newhashtable=
    var ref[0:]ref strec oldhashtable
    var int oldhstsize
    var ref strec d

!remember old hst
    oldhashtable:=hashtable
    oldhstsize:=hstsize
!generate new, blank hst
    hstsize*:=2
    hstmask:=hstsize-1
    nhstsymbols:=0
    hstthreshold:=(6*hstsize)/10

    hashtable:=pcm_alloc(hstsize*(ref void.bytes))

    for i:=0 to hstmask do
        hashtable^[i]:=pcm_allocz(strec.bytes)
    od

!now, rehash all existing hashentries
    for i:=0 to oldhstsize-1 do
        d:=oldhashtable^[i]
        if d^.name then
            regenlookup(d)
        fi
    od

    pcm_free(oldhashtable,oldhstsize*(ref void.bytes))
end

proc old_readrealnumber(ref char pstart,intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, (or to "." if there was no prefix, then intlen=0)
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
    var ref char fractstart
    var int fractlen,expon,i,c,badexpon
    var real basex,x,expbase
    const maxrealdigits=500
    var [maxrealdigits]char realstr

    fractstart:=nil
    fractlen:=0
    expon:=0

    if lxsptr^='.' then     !read
        fractstart:=++lxsptr
        fractlen:=scannumber(base)-fractstart
    fi
    badexpon:=0

    case lxsptr^
    when 'e','E' then
        if base<>16 then
            ++lxsptr
            expon:=readexponent(badexpon)
        fi
    when 'p','P' then
        if base=16 then
            ++lxsptr
            expon:=readexponent(badexpon)
        fi
    esac

    if badexpon then
        --lxsptr
        readalphanumeric(pstart)
        return
    fi

    case lxsptr^
    when 'f','F' then
        ++lxsptr
    else
        if alphamap[lxsptr^] then
            readalphanumeric(pstart)
            return
        fi
    esac

    if intlen+fractlen>maxrealdigits then
        lxerror("Real too long")
    fi
    if intlen then
        memcpy(&realstr,intstart,intlen)
    fi
    if fractlen then
        memcpy(&realstr[1]+intlen,fractstart,fractlen)
    fi

    expbase:=basex:=base

    if base=10 then
        expon-:=fractlen
    else
        expon-:=fractlen*4              !each hex digit is 4 binary bits
        expbase:=2.0
    fi

    x:=0.0

    for i:=1 to intlen+fractlen do      !digits already range-checked
        c:=realstr[i]
        if c>='0' and c<='9' then
            x:=x*basex+(c-'0')
        elsif c>'a' then
            x:=x*basex+c-'a'+10
        else
            x:=x*basex+c-'A'+10
        fi
    od

    if expon>=0 then
        to expon do
            x*:=expbase
        od
    else
        to -expon do
            x/:=expbase
        od
    fi

    nextlx.symbol:=realconstsym
    nextlx.subcode:=tdouble
    nextlx.xvalue:=x

    setnumberoffset(intstart-lxstart)
    nextlx.length:=lxsptr-intstart
end
