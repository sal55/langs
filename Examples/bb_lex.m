import msys
import mlib
import clib

import bb_decls
import bb_tables
import bb_support
import bb_lib
import bb_pclcommon

macro hashc(hsum,c)=hsum<<4-hsum+c
macro hashw(hsum)=(hsum<<5-hsum)

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]int lxlineno_stack
[maxstackdepth]byte isfile_stack
int sourcelevel=0

const cr    = 13
const lf    = 10
const tab   = 9

!int assemmode

ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix          !for real nos

int lxfileno
global const hstsize    = 32768
global const hstmask    = hstsize-1

global [0:hstsize]strec hashtable
global [0:hstsize]word hashkeys

const inittokensize = 1048576

ref[]tokenrec tokenlist
int tokenlistsize
global ref tokenrec nexttoken

byte prescanmode=0

[]ichar maxnumlist=(
    "",                 !1
    "1111111111111111111111111111111111111111111111111111111111111111",     !2
    "11112220022122120101211020120210210211220",                            !3
    "33333333333333333333333333333333",                                     !4
    "2214220303114400424121122430",                                         !5
    "3520522010102100444244423",                                            !6
    "45012021522523134134601",                                              !7
    "1777777777777777777777",                                               !8
    "145808576354216723756",                                                !9
    "18446744073709551615",                                                 !10
    "335500516A429071284",                                                  !11
    "839365134A2A240713",                                                   !12
    "219505A9511A867B72",                                                   !13
    "8681049ADB03DB171",                                                    !14
    "2C1D56B648C6CD110",                                                    !15
    "FFFFFFFFFFFFFFFF")                                                     !16
[maxnumlist.len]int maxnumlen

global proc lex=
    lx:=nexttoken^
    ++nexttoken
end

global function nextlx:ref tokenrec=
    return nexttoken
end

global proc lexreadtoken=
!read next token into nextlx
int c,hsum,commentseen,hashindex,length
ref char pstart,pnext,p,ss,lxsvalue

lx.subcode:=0

doswitch lxsptr++^
when 'a'..'z','$','_' then
    lxsvalue:=lxsptr-1
doname::
    hsum:=lxsvalue^

    doswitch c:=lxsptr++^
    when 'a'..'z','0'..'9','_','$' then
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

    do_name(lxsvalue, lxsptr-lxsvalue, hashw(hsum))
    return

when 'A'..'Z' then
    lxsvalue:=lxsptr-1
    lxsvalue^+:=32
    goto doname

when '0'..'9' then
    c:=(lxsptr-1)^
    case lxsptr^
    when ' ',')',cr,',','|' then        !assume single digit decimal
!   when ' ',')',cr,',' then        !assume single digit decimal
        lx.symbol:=intconstsym
        lx.subcode:=tint
        lx.value:=c-'0'
    when 'x','X' then
        case c
        when '0' then       !0x
            ++lxsptr
            readnumber(16)
        when '1' then
            lxerror("Bad base")
        else                !other base 2..9
            ++lxsptr
            readnumber(c-'0')
        esac
    elsif c='1' and lxsptr^ in '0'..'6' and (lxsptr+1)^ in ['x','X'] then
        int base:=lxsptr^+(10-'0')
        lxsptr+:=2
        readnumber(base)

    else
        --lxsptr
        readdecimalnumber()
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
    ++lx.pos
    lx.symbol:=eolsym
    return

when '#' then           !docstring to eol
    lxsvalue:=cast(lxsptr)

    doswitch c:=lxsptr++^
    when 13,10,0 then           !leave eol for next symbol
        --lxsptr
        exit
    end

    length:=lxsptr-cast(lxsvalue,ref char)
!   (lxsvalue+lxlength)^:=0
    lx.symbol:=docstringsym
    lx.svalue:=pcm_copyheapstringn(lxsvalue,length)
    return

when '\\' then          !line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
    commentseen:=0
    doswitch lxsptr++^          !read until end of this line
    when cr then
        ++lx.pos
        ++lxsptr                !skip lf
        exit
    when lf then
        ++lx.pos
        exit
    when 0 then
        lx.symbol:=eofsym
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
        ++lx.pos
        ++lxsptr                !skip lf
    when lf then
        ++lx.pos
    when ' ',tab then
    else
        --lxsptr
        exit
    enddoswitch
!   next

when '{' then
    lx.symbol:=lcurlysym
    return

when '}' then
    lx.symbol:=rcurlysym
    return

when '.' then
    switch lxsptr^
    when '.' then               !.. or ...
        ++lxsptr
        if lxsptr^='.' then
            ++lxsptr
            lx.symbol:=ellipsissym
        else
            lx.symbol:=rangesym
            lx.subcode:=j_makerange     !helps treat as opsym which all have k-code as subcode
        fi
        return
    when '0'..'9' then          !real const: deal with this after the switch
        --lxsptr
        readrealnumber(nil,0,10)
        return
    else
!       p:=lxsptr-2
!       if p<lxstart or p^=cr or p^=lf then
!           lx.symbol:=lexdotsym
!       else
            lx.symbol:=dotsym
!       fi
        return
    endswitch

when ',' then
    lx.symbol:=commasym
    return

when ';' then
    lx.symbol:=semisym
    return

when ':' then
    switch lxsptr^
    when '=' then
        ++lxsptr
        lx.symbol:=assignsym
        lx.subcode:=j_assign        !helps treat as opsym which all have k-code as subcode
    when ':' then
        ++lxsptr
        case lxsptr^
        when '=' then
            ++lxsptr
            lx.symbol:=deepcopysym
            lx.subcode:=j_deepcopy
        else
            lx.symbol:=dcolonsym
        esac
    else
        lx.symbol:=colonsym
    endswitch
    return

when '(' then
    lx.symbol:=lbracksym
    return

when ')' then
    lx.symbol:=rbracksym
    return

when '[' then
    lx.symbol:=lsqsym
    return

when ']' then
    lx.symbol:=rsqsym
    return

when '|' then
    if lxsptr^='|' then
        ++lxsptr
        lx.symbol:=dbarsym
    else
        lx.symbol:=barsym
    fi
    return

when '^' then
    lx.symbol:=ptrsym
    return

when '@' then
    if lxsptr^='@' then
        ++lxsptr
        lx.symbol:=datsym
    else
        lx.symbol:=atsym
    fi
    return

when '?' then
    lx.symbol:=questionsym
    return

!when 156 then      !'œ' in ansi font or whatever
!when 'œ' then      !'œ' in ansi font or whatever
!   lx.symbol:=poundsym
!   return
!
when '~' then
    lx.symbol:=curlsym
    return

!when 'ª' then
!   lx.symbol:=gatesym
!   return
!
when '+' then
    lx.symbol:=addsym
    if lxsptr^='+' then
        ++lxsptr
        lx.symbol:=incrsym
        lx.subcode:=incr_op
        return
!   else
!       lx.subcode:=j_add
    fi
    return

when '-' then
    lx.symbol:=subsym
    if lxsptr^='-' then
        ++lxsptr
        lx.symbol:=incrsym
        lx.subcode:=decr_op
        return
!   else
!       lx.subcode:=j_sub
    fi
    return

when '*' then
    if lxsptr^='*' then
        ++lxsptr
        lx.symbol:=powersym
    else
        lx.symbol:=mulsym
    fi
    return

when '/' then
    lx.symbol:=divsym
    return

when '%' then
    lx.symbol:=idivsym
    return

when '=' then
    case lxsptr^
    when '>' then
        lx.symbol:=sendtosym
        ++lxsptr
    when '=' then
        lx.symbol:=samesym
        ++lxsptr
!   when ':' then
!       ++lxsptr
!       if lxsptr^<>'=' then lxerror("=:?") fi
!       ++lxsptr
!       lx.symbol:=dispassignsym
!       lx.subcode:=j_dispassign
!CPL "DISPASSIGN"
    else
        lx.symbol:=eqsym
        lx.subcode:=eq_op
    esac
    return

when '<' then
    lx.symbol:=cmpsym
    switch lxsptr^
    when '=' then
        ++lxsptr
        lx.subcode:=le_op
    when '>' then
        ++lxsptr
        lx.subcode:=ne_op
    when '<' then
        ++lxsptr
        lx.symbol:=shlsym
    else
        lx.subcode:=lt_op
    endswitch
    return

when '>' then
    lx.symbol:=cmpsym
    switch lxsptr^
    when '=' then
        ++lxsptr
        lx.symbol:=cmpsym
        lx.subcode:=ge_op
    when '>' then
        ++lxsptr
        lx.symbol:=shrsym
    else
        lx.symbol:=cmpsym
        lx.subcode:=gt_op
    endswitch
    return

when '&' then
    case lxsptr^
!   when '&' then
!       ++lxsptr
!       lx.symbol:=opsym
!       lx.subcode:=j_andand
    when '.' then
        ++lxsptr
        lx.symbol:=anddotsym
        lx.subcode:=0
    else
        lx.symbol:=addrsym
        lx.subcode:=j_addrof
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
    ++lx.pos
    lx.symbol:=eolsym
    return
when lf then            !only lfs not preceded by cr
    ++lx.pos
    lx.symbol:=eolsym
    return

when 0 then
    if sourcelevel then
        unstacksource()
    else
        lx.symbol:=eofsym
        --lxsptr
        return
    fi

else
    lx.symbol:=errorsym
!   lx.value:=c
    return

end doswitch

end

proc readnumber(int base)=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
ref char pstart,dest
int c
ref char p

dest:=pstart:=lxsptr

if base=10 then
    doswitch c:=lxsptr++^
    when '0'..'9' then
        dest++^:=c
    when '_','\'','`' then
    else
        --lxsptr
        exit
    end doswitch
else
    dest:=scannumber(base)
    c:=lxsptr^
fi

switch c            !terminator character
when '.' then       !possible real number
    if (lxsptr+1)^<>'.' then

        readrealnumber(pstart,dest-pstart,base)
        return
    fi
when 'e','E' then
    if base<15 then
        readrealnumber(pstart,dest-pstart,base)
        return
    fi
when 'p','P' then
    if base>=15 then
        readrealnumber(pstart,dest-pstart,base)
        return
    fi
end switch

stringtonumber(pstart,dest-pstart,base)
end

proc readdecimalnumber=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
ref char pstart,dest
int c,n,base,suffix
ref char p

dest:=pstart:=lxsptr
suffix:=0

doswitch c:=lxsptr++^
when '0'..'9' then
    dest++^:=c
when '_','\'','`' then
else
    --lxsptr
    exit
end doswitch

switch c            !terminator character
when '.' then       !possible real number
    if (lxsptr+1)^<>'.' then

        readrealnumber(pstart,dest-pstart,10)
        return
    fi
when 'e','E' then
    readrealnumber(pstart,dest-pstart,10)
    return
when 'b','B' then
    ++lxsptr
    n:=dest-pstart
    p:=pstart
    to n do
        if p^<'0' or p^>'1' then
            lxerror("1101B: bad digit")
        fi
        ++p
    od
    stringtonumber(pstart,n,2)
    return

end switch

stringtodecimalnumber(pstart,dest-pstart,suffix)
end

proc readrealnumber(ichar intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, or is nil
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in lx.xvalue
ref char fractstart,ss
int fractlen,expon,i,c,n
real basex,x
const maxrealdigits=500
[maxrealdigits]char realstr
[32]char str

fractstart:=nil
fractlen:=0
expon:=0
longsuffix:=0

if lxsptr^='.' then     !read
    fractstart:=++lxsptr
    fractlen:=scannumber(base)-fractstart
fi

case lxsptr^
when 'e','E' then
    if base<15 then
        ++lxsptr
        expon:=readexponent(base)
    fi
when 'p','P' then
    if base>=15 then
        ++lxsptr
        expon:=readexponent(base)
    fi
when 'l','L' then
    if longsuffix then lxerror("LL?") fi
    longsuffix:='L'
    ++lxsptr

esac

if longsuffix='L' then
    ss:=pcm_alloc(intlen+fractlen+16)       !add ".", "e", exponent, 0 terminator
    memcpy(ss,intstart,intlen)
    memcpy(ss+intlen,".",1)
    memcpy(ss+intlen+1,fractstart,fractlen)
    memcpy(ss+intlen+fractlen+1,"e",1)
!   n:=sprintf(&.str,"%lld",expon)
    getstrint(expon,&.str)
    memcpy(ss+intlen+fractlen+2,&.str,strlen(&.str)+1)

    lx.symbol:=decimalconstsym
!   lx.subcode:=tflexdecimal
    lx.svalue:=ss
!   lxlength:=strlen(ss)
    return
fi

if intlen+fractlen>maxrealdigits then
    lxerror("Real too long")
fi
if intlen then
    memcpy(&realstr,intstart,intlen)
fi
if fractlen then
    memcpy(&realstr[1]+intlen,fractstart,fractlen)
fi

if base=10 then
    x:=readrealbest(intlen,fractlen,expon,&.realstr)
else
    basex:=base
    expon-:=fractlen
    x:=0.0
    for i:=1 to intlen+fractlen do      !digits already range-checked
        c:=realstr[i]
        if c>='0' and c<='9' then
            x:=x*basex+c-'0'
        elsif c>'a' then
            x:=x*basex+c-'a'+10
        else
            x:=x*basex+c-'A'+10
        fi
    od

    if expon>=0 then
        to expon do
            x*:=basex
        od
    else
        to -expon do
            x/:=basex
        od
    fi
fi

lx.symbol:=realconstsym
lx.subcode:=treal
lx.xvalue:=x
end

function readrealbest(int intlen,fractlen,expon, ichar realstr)real=
    [32]char expstr

    (realstr+intlen+fractlen)^:=0
    expon-:=fractlen

!   sprintf(&.expstr,"e%lld",int32(expon))
    print @&.expstr,"e",,expon
    strcat(realstr,&.expstr)
    return strtod(realstr,nil)
end

function readexponent(int base)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
ref char numstart,numend
int expon,length,neg

neg:=0
case lxsptr^
when '+' then ++lxsptr
when '-' then ++lxsptr; neg:=1
esac

numstart:=lxsptr
length:=scannumber(base)-numstart

if length=0 then
    lxerror("Bad expon")
fi

stringtonumber(numstart, length, base)
return (neg|-lx.value|lx.value)
end

global proc printsymbol(ref tokenrec lp)=
tokenrec l
l:=lp^

printf("%-18s",symbolnames[l.symbol])

case l.symbol
!when rawnamesym then
!!  printstrn(l.svalue,l.length)
!   printstr(l.svalue)
!!  print " (",,l.hashvalue,,")"
when namesym then
    printstrn(l.symptr^.name,l.symptr^.namelen)

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
    minsym,maxsym,concatsym,powersym,samesym then
    print symbolnames[l.symbol]
elsif l.subcode then
    fprint "SUBCODE:",l.subcode
!   fprint "#",symbolnames[l.subcode]
end

println

end

proc stringtonumber(ichar s, int length, base)=
!convert decimal number s to an i64 value
!s contains only digits
!for hex, then a..f and A..F have been converted to '9'+1 to '9'+6
int64 a
word64 b
int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do       !trim leading zeros
    ++s
    --length
od

lx.symbol:=intconstsym

if length>maxnumlen[base] or 
        (length=maxnumlen[base] and strncmp(s,maxnumlist[base],length)>0) then
    if base<>16 then
        lxerror("longint const")

    else
        if length>32 or 
            (length=32 and strncmp(s,"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0) then
            lxerror("longint const")

        else                        !greater than 64 bits, up to 128 bits

            if length=32 and strncmp(s,"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0 then
                lx.subcode:=tu128
            else
                lx.subcode:=ti128
            fi

            lx.pvalue128:=stringtonumber128(s,length,16)
        fi
    fi
    return
fi

a:=0

if base<=10 then
    to length do
        a:=a*base+s++^-'0'
!       a:=a*10+s++^-'0'
    od
else
    to length do
        c:=s++^
        if c>='a' then
            a:=a*base+c-'a'+10
        elsif c>='A' then
            a:=a*base+c-'A'+10
        else
            a:=a*base+c-'0'
        fi
    od
fi

lx.value:=a

lx.subcode:=setinttype(a)
end

proc stringtodecimalnumber(ichar s, int length,suffix=0)=
int64 a
word64 b
int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do       !trim leading zeros
    ++s
    --length
od

lx.symbol:=intconstsym

if length>20 or 
        (length=20 and strncmp(s,"18446744073709551615",20)>0) or suffix then

    if length>39 or 
        (length=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
        if suffix='W' then
            lxerror("-W overflows 128 bits")
        fi
dolongint::
        lx.symbol:=decimalconstsym
!       lx.subcode:=tflexdecimal
        lx.svalue:=pcm_copyheapstring(s)
!       lxlength:=length
    else                        !greater than 64 bits, up to 128 bits

        if suffix='L' then goto dolongint fi

        if (length=39 and strncmp(s,"170141183460469231731687303715884105727",39)>0) then
            lx.subcode:=tu128
        else
            lx.subcode:=ti128
        fi

        lx.pvalue128:=stringtonumber128(s,length,10)
    fi
    return
fi

a:=0

to length do
    a:=a*10+s++^-'0'
od

lx.value:=a

lx.subcode:=setinttype(a)
end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
int i!,n
static int n

for i to maxnumlist.len do
    maxnumlen[i]:=strlen(maxnumlist[i])
od

inithashtable()
end

proc newtokenlist=
    tokenlist:=pcm_alloc(tokenrec.bytes*inittokensize)
    tokenlistsize:=inittokensize
end

global proc printstrn(ichar s, int length)=
if length then
    print length:"v",s:".*"
fi
end

function scannumber(int base)ref char=
!lxsptr is at possible first digit of number sequence
!scan digits until non-digit
!return pointer to next char after compacted sequence
!sequence can be updated in-place (to close gaps caused by separators)
!start of sequence will be at lxsptr
ref char dest
int c

dest:=lxsptr

doswitch c:=lxsptr++^
when '0'..'9' then
    dest++^:=c
    if c>='0'+base then
        lxerror("Digit out of range")
    fi
when 'A'..'D','F','a'..'d','f' then
!   if base=16 then
    if 11<=base<=16 then        !NEEDS TO CHECK LIMITS FOR BASES 10..15
        dest++^:=c
    else
        --lxsptr
        exit
    fi
when 'E','e' then
    if base<15 then
        --lxsptr
        exit
    else
        dest++^:=c
    fi

when '_','\'','`' then
when 'l','L' then
    longsuffix:='L'
    exit

else
    --lxsptr
    exit
end doswitch
return dest
end

proc readrawstring=
!positioned at " of F"
!read raw string
ichar dest
int c

lx.symbol:=stringconstsym
lx.svalue:=++lxsptr

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
!lxlength:=dest-lxsvalue
end

proc lookup(ref char name, int length, hashindex0)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
    int wrapped, hashindex,INDEX

    hashindex:=hashindex0 iand hstmask

    lx.symptr:=&hashtable[hashindex]
    wrapped:=0

    do
        case lx.symptr^.namelen
        when 0 then
            exit
        when length then
            if memcmp(lx.symptr.name,name,length)=0 then    !match
                lx.symbol:=lx.symptr.symbol
                lx.subcode:=lx.symptr.subcode
                return
            fi
        esac

        ++lx.symptr
        if ++hashindex>=hstsize then
            if wrapped then
                abortprogram("HASHTABLE FULL")
            fi
            wrapped:=1
            lx.symptr:=&hashtable[0]
            hashindex:=0
        fi
    od

!exit when not found; new name will go in entry pointed to by lxsymptr
    lx.symptr.name:=pcm_copyheapstringn(name,length)
    lx.symptr.namelen:=length
    lx.symptr.symbol:=namesym
    lx.symbol:=namesym
end

function lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
int j, wrapped, hashvalue

j:=gethashvaluez(name) iand hstmask

lx.symptr:=&hashtable[j]
wrapped:=0

do
    if lx.symptr^.namelen=0 then
        exit
    elsif eqstring(lx.symptr^.name,name) then   !match
        cpl name
        lxerror("sys dupl name?")
    fi

    ++lx.symptr
    if ++j>=hstsize then
        if wrapped then
            abortprogram("SYS:HASHTABLE FULL")
        fi
        wrapped:=1
        lx.symptr:=&hashtable[0]
        j:=0
    fi
od

!exit when not found; new name will go in entry pointed to by lxsymptr
!++NHASHENTRIES

lx.symptr^.name:=name               !assume can be shared (stored in a table)
lx.symptr^.namelen:=strlen(name)
lx.symptr^.symbol:=namesym          !usually replaced with actual symbol details

return 0
end

function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
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

for i:=1 to stnames.len do
    lookupsys(stnames[i])

    lx.symptr.symbol:=stsymbols[i]

    case stsymbols[i]
    when unitnamesym then
        lx.symptr.index:=stsubcodes[i]
        lx.symptr.subcode:=unitnamesym
        lx.symptr.symbol:=namesym       !masquerades as normal identifier
    else
        lx.symptr.subcode:=stsubcodes[i]
    esac
od
end

GLOBAL proc printhashtable=
    println "Hashtable:"

    for i:=0 to hstsize-1 do
        if hashtable[i].namelen then
            println i,hashtable[i].name,symbolnames[hashtable[i].symbol]
        fi
    od
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
[256]char str

case index
when strincludedir,binincludedir then
    lexreadtoken()
    if lx.symbol<>stringconstsym then
!       if lx.symbol=rawnamesym and eqbytes(lxsvalue,"$filename",9) then
!           file:=sourcefilepaths[lxfileno]
!       else
            lxerror("strincl: string expected")
!       fi
    else
        file:=lx.svalue
    fi

    fileno:=getsupportfile(file)
    lx.svalue:=sourcefiletext[fileno]
    length:=sourcefilesizes[fileno]

    lx.symbol:=(index=strincludedir|stringconstsym|astringconstsym)
    lx.subcode:='A'         !for use when an astring
    (lx.svalue+length)^:=0          !sometimes .length is not used (eg. in newstringobj())
    return 1                        !so get it right. Don't need the etx

when includedir then
    lexreadtoken()
    if lx.symbol<>stringconstsym then lxerror("include: string expected") fi
    file:=lx.svalue
    convlcstring(file)
    file:=addext(file,".m")     !add in extension if not present; assume same as source

!   if fverbose then
!       println "  Include:",file
!   fi
    stacksourcefile(file)
    return 0

when defineunitdir then
    LXERROR("DEFINE UNIT NOT DONE")

when emitcdir then
    lexreadtoken()
    if lx.symbol<>stringconstsym then lxerror("emitc/not str") fi
    lx.symbol:=kemitcsym
    return 1

when cclibdir then
    do
!       if ncclibs>=maxcclibs then lxerror("Too many cc libs") fi
        lexreadtoken()
        case lx.symbol
        when stringconstsym then
            addcclib(lx.svalue)
        when namesym then
            addcclib(lx.symptr.name)
        else
            lxerror("cclib/not str/name")
        esac

        lexreadtoken()
        if lx.symbol<>commasym then exit fi
    od
    return 0


else
    cpl sourcedirnames[index]
    lxerror("Directive not implemented")
esac
return 0
END

proc lexreadline=
!read lex chars until eol
!returns with lxsptr pointing to what follows (crlf, etc)
!caller should remember lxsptr as start of text
!processing of next symbol deals with line counting

doswitch lxsptr^
when cr,lf then
    return
when 0 then
    --lxsptr
    return
else
    ++lxsptr
enddoswitch
END

global proc startlex(ichar caption,int fileno)=
!s is a 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it
!(This lex system doesn't deal with include files so there no nested sourcefiles.
!There are only macro expansions which are dealt with locally.)

lxsptr:=sourcefiletext[fileno]

lxfileno:=fileno
lx.pos:=1

lx.symbol:=semisym
lx.subcode:=0
end

global function convertzstring(ichar s, int length)ichar=
static [300]char str

if length>str.len then
    abortprogram("convertzstr")
fi
memcpy(&.str,s,length)
str[length+1]:=0
return &.str
end

global function addnamestr(ichar name)ref strec=
    tokenrec oldlx
    ref strec symptr

    oldlx:=lx
    lookup(name,strlen(name), gethashvaluez(name))
    symptr:=lx.symptr
    lx:=oldlx

    return symptr
end

global function findname(ichar name)ref strec=
!find arbitrary name in st
!return strec of generic entry, or nil if not found

    lookup(name,strlen(name),gethashvaluez(name))
    return lx.symptr
end

global proc ps(ichar caption)=
!print "PS:",,caption,,":"
print caption,,": "
printsymbol(&lx)
end

global proc showhashtablesize=
int i,n

n:=0
for i:=0 to hstmask do
    if hashtable[i].name then
        ++n
    fi
od
end

function getstrfile(ichar filename,int32 &length)ichar=
!locate module within search paths for strinclude
!return pointer to loaded/in-memory file (or nil on error)
    

ichar file
static [300]char filespec
int i

for i:=nsearchdirs downto 1 do
    strcpy(&.filespec,searchdirs[i])
    strcat(&.filespec,filename)

    if checkfile(&.filespec) then
        file:=cast(readfile(&.filespec))
        length:=rfsize
        return file
    fi
od

return nil
end

proc stacksourcefile(ichar file,int ismainmodule=0)=
int fileno
ichar basefile,sptr,path

fileno:=getsupportfile(file)

stacksource(sourcefiletext[fileno],fileno,1)
end

proc stacksource(ichar sptr,int fileno,isfile)=
!introduce new source level for macros or include files
!not used for main source

if sourcelevel>=maxstackdepth then
    lxerror("Include file/macro overflow")
fi
++sourcelevel
lxstart_stack[sourcelevel]:=lxstart
lxsptr_stack[sourcelevel]:=lxsptr
lxfileno_stack[sourcelevel]:=lxfileno
lxlineno_stack[sourcelevel]:=lx.pos
isfile_stack[sourcelevel]:=isfile

lxstart:=lxsptr:=sptr
lx.pos:=1
lxfileno:=fileno
end

proc unstacksource=
if sourcelevel>0 then           !check that some source is stacked
    lxstart:=lxstart_stack[sourcelevel]
    lxsptr:=lxsptr_stack[sourcelevel]
    lx.pos:=lxlineno_stack[sourcelevel]
    lxfileno:=lxfileno_stack[sourcelevel]
    --sourcelevel
fi
end

proc readarraystring(int prefix)=
++lxsptr
lxreadstring('"')
lx.symbol:=astringconstsym
lx.subcode:=toupper(prefix)
end

function stringtonumber128(ichar s, int length,base)ref int128=
ref int128 aa
int c,d

aa:=pcm_allocz(int128.bytes)

to length do
    aa^:=aa^*base
        c:=s++^

        if c>='a' then
            d:=c-'a'+10
        elsif c>='A' then
            d:=c-'A'+10
        else
            d:=c-'0'
        fi

    aa^:=aa^+d
od

return aa
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

    lx.svalue:=lxsptr
    hsum:=0

    doswitch c:=lxsptr++^
    when 'A'..'Z','a'..'z','0'..'9','_','$' then
        hsum:=hashc(hsum,c)
    else
        --lxsptr
        exit
    end doswitch

    length:=lxsptr-lx.svalue

    if length=0 then
        lxerror("Bad ` name")
    fi
    lookup(lx.svalue,length, hashw(hsum))
    lx.symbol:=rawxnamesym

    return
end

proc lxerror_s(ichar mess,s)=
    lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

    ichar s,t
    int c, d, length, hasescape
    [8]char str

    if termchar='"' then
        lx.symbol:=stringconstsym
    else
        lx.symbol:=charconstsym
        lx.subcode:=tint
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
        when 'a','b','c','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
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
        lx.svalue:=""
        return
    elsif not hasescape then
        lx.svalue:=pcm_copyheapstringn(s,length)
        return
    fi

!need to copy string to dest and expand the escape codes

    lx.svalue:=t:=pcm_alloc(length+1)

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
!       when 'u' then           !reserved for unicode, like \x but with 4 hex digits
            when 'v' then           !vertical tab
                c:=11
            when 'w' then           !windows-style cr-lf
                t++^:=cr
                c:=lf
            when 'x' then   !2-digit hex code follows
                c:=0
                to 2 do
                    case d:=s++^
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
                str[1]:=c; str[2]:=0
!           println c,char(c),=lx.pos
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

proc do_name(ichar s, int length, hashindex)=
    lookup(s,length, hashindex)
end

proc extendtokenlist(int ntokens)=
    ref[]tokenrec oldtokenlist
    int oldtokenlistsize

    oldtokenlistsize:=tokenlistsize
    oldtokenlist:=tokenlist

    tokenlistsize*:=2

    tokenlist:=pcm_alloc(tokenrec.bytes*tokenlistsize)

    memcpy(tokenlist,oldtokenlist,ntokens*tokenrec.bytes)

    pcm_free(oldtokenlist,tokenrec.bytes*oldtokenlistsize)
end

global proc starttkscan(int moduleno)=
    nexttoken:=moduletable[moduleno].tklist
end

global function readtokens_a(int fileno, &ntokens)ref tokenrec=
    ref tokenrec lastlx
    ref char p
    int lena,lenb,lastsymbol

    newtokenlist()

    ntokens:=0
    lastsymbol:=0

    startlex("",fileno)

    repeat
        lexreadtoken()

        switch lx.symbol
        when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,
                kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
                krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
                ktrysym,ktabledatasym,kassemsym,kifsym then

            if lastsymbol=kendsym then
                if lastlx.subcode then lxerror("end if if?") fi
                lastlx.subcode:=lx.symbol
                next
            fi

        when eolsym then
            if lastsymbol in [commasym, lsqsym, lbracksym] then !ignore eol
                next
            elsif symboloptypes[lastsymbol]=bin_op and not assemmode and 
                lastsymbol not in [maxsym, minsym] then
                next
            else
                lx.symbol:=semisym
            fi

        when stringconstsym then
            if lastsymbol=stringconstsym then
                lena:=strlen(lastlx.svalue)
                lenb:=strlen(lx.svalue)
                p:=pcm_alloc(lena+lenb+1)
                memcpy(p,lastlx.svalue,lena)
                memcpy(p+lena,lx.svalue,lenb)
                (p+lena+lenb)^:=0
                lastlx.svalue:=p
                next
            fi
        when ksourcedirsym then
            if not dolexdirective(lx.subcode) then      !skip symbol
                next
            fi

        when namesym then
            if lx.subcode=unitnamesym then
                case lastsymbol
                when intconstsym then
                    if lastlx.subcode in [ti128,tu128] then
                        lxerror("No suffix on i128/u128")
                    fi
                    case lx.symptr^.index
                    when million_unit then lastlx.value *:= 1 million
                    when billion_unit then lastlx.value *:= 1 billion
                    when thousand_unit then lastlx.value *:= 1 thousand
                    when kilo_unit then lastlx.value *:= 1024
                    when mega_unit then lastlx.value *:= 1048576
                    when giga_unit then lastlx.value *:= (1048576*1024)
                    else
                        lxerror("Can't do this unit index")
                    esac
                    lastlx.subcode:=setinttype(lastlx.value)
                    next
                when realconstsym then
                    lxerror("Unit suffix after float not implem")
                esac
            fi
        when machinetypesym then
            case lx.subcode
            when 'I','i' then lx.subcode:=ti64
            when 'W','w' then lx.subcode:=tu64
            esac
            lx.symbol:=stdtypesym

        when rawxnamesym then
            lx.symbol:=namesym

        when insym then
            if lastsymbol=notlsym then
                lastlx.symbol:=notinsym
                lastlx.subcode:=notin_op
                next
            fi
        when eqsym then
            if lastsymbol=notlsym then
                lastlx.symbol:=cmpsym
                lastlx.subcode:=ne_op
                next
            fi
        end switch

SKIP::
        if (ntokens+4) >= tokenlistsize then            !some margin
            extendtokenlist(ntokens)
        fi
        ++ntokens

!       lx.fileno:=lxfileno                 !pop dotslice not working?
        lx.pos :=lx.pos ior lxfileno<<24

        tokenlist[ntokens]:=lx
        lastlx:=&tokenlist[ntokens]
        lastsymbol:=lx.symbol
    until lx.symbol=eofsym

    tokenlist[ntokens+1].symbol:=eofsym                 !end with 2 eofs

    return &tokenlist[1]
end
