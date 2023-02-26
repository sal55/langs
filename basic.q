!'Basic' interpreter

var identstarter = ['A'..'Z', 'a'..'z']
var numericstarter = ['0'..'9']
var identchars = ['A'..'Z', 'a'..'z','0'..'9','_','$']
var numericchars = ['0'..'9']

var puncttable = [
    '(':tklbrack, ')':tkrbrack, '+':tkadd, '-':tksub, '*':tkmul, '/':tkdiv,
        '=':tkeq, ',':tkcomma]

enumdata kwdnames =
    (letkwd,    "let"),
    (printkwd,  "print"),
    (ifkwd,     "if"),
    (forkwd,    "for"),
    (gotokwd,   "goto"),
    (nextkwd,   "next"),
    (thenkwd,   "then"),
    (remkwd,    "rem"),
end

enumdata tokennames, priotable, qoptable =
    (tkvar,     $,      0,      0),
    (tknumber,  $,      0,      0),
    (tkstring,  $,      0,      0),
    (tkeol,     $,      0,      0),

    (tklbrack,  $,      0,      0),
    (tkrbrack,  $,      0,      0),

    (tkadd,     $,      2,      +),
    (tksub,     $,      2,      -),
    (tkmul,     $,      1,      *),
    (tkdiv,     $,      1,      /),

    (tkeq,      $,      3,      =),
    (tkne,      $,      3,      <>),
    (tklt,      $,      3,      <),
    (tkle,      $,      3,      <=),
    (tkge,      $,      3,      >=),
    (tkgt,      $,      3,      >),

    (tkand,     "and",  4,      and),
    (tkor,      "or",   5,      or),

    (tkcomma,   $,      0,      0),

    (tksqr,     "sqr",  0,      sqr),
    (tklen,     "len",  0,      len),

    (tkthen,    "then", 0,      0),

    (tkother,   $,      0,      0),
end

var binops = [tkadd, tksub, tkmul, tkdiv,
                tkeq, tkne, tklt, tkle, tkge, tkgt, tkand, tkor]

var lexstr, lexlen, lexpos
var tk, tkvalue

var vars::=[:]             # empty dict (::= assignments create mutable copies)
record linerec = (var lineno, kwd, source)
var program::=()

var pcindex

sub startlex(s) = (lexlen:=s.len; lexstr:=s+chr(0); lexpos:=1)
sub error(m) = abort("Error:"+m)

proc nexttoken(tkexp=0) =

    tkvalue::=""

    docase c:=lexstr.[lexpos++]
    when identstarter then
        tkvalue::=chr(c)
        while (c:=lexstr.[lexpos++]) in identchars do tkvalue+:=c od
        --lexpos

        tk:=tkvalue inx tokennames
        if not tk.isfound then
            tk:=tkvar
            if vars{tkvalue}.isvoid then
                vars{tkvalue}:=0.0
            fi
        fi
        exit

    when numericstarter then
        tkvalue:=c-'0'
        while (c:=lexstr.[lexpos++]) in numericchars do tkvalue:=tkvalue*10+c-'0' od
        --lexpos
        tk:=tknumber
        exit

    when ' ', '\t' then
    when '"' then
        while (c:=lexstr.[lexpos++]) not in ['"',0] do tkvalue+:=c od
        if c=0 then error("String?") fi
        tk:=tkstring
        exit

    when 0 then --lexpos; tk:=tkeol; exit
    when '<' then tk:=(lexstr.[lexpos]='='|(++lexpos; tkle) | tklt); exit
    when '>' then tk:=(lexstr.[lexpos]='='|(++lexpos; tkge) | tkgt); exit
    elsif tk:=puncttable{c,0} then exit
    else tk:=tkother; exit
    end

    if tkexp then checktoken(tkexp) fi
end

sub checktoken(tkexp)= if tk<>tkexp then error(tokennames[tkexp]+" expected") fi

proc loadprogram(filename)=
    lines:=readtextfile(filename)
    if lines=0 then error("Bad file") fi

    lastn:=0

    for line in lines when leftstr(line)<>"'" and line do
        sreadln(line)
        read n:"i", kwd:"n", s:"L"
        if n<=lastn then error("Line seq") fi
        k:=kwd inx kwdnames
        if not k.isfound then error("Bad kwd:"+kwd) fi
        if k=remkwd then next fi

        program &:= linerec(n, k, s+chr(0))
        lastn:=n
    od
end

proc listprogram=
    for line in program do
        println line.lineno, kwdnames[line.kwd]:"jl6", line.source
    od
end

func readexpr=
    nexttoken()
    readfactor(5)
end

func readfactor(n)=
    x:=(n<=1 | readterm() | readfactor(n-1))

    while tk in binops and priotable[tk]=n do
        opc:=tk
        nexttoken()
        x:=mapss(qoptable[opc], x, readfactor(n-1))
    od
    x
end

func readterm=
    case tk
    when tknumber, tkstring then
        x:=tkvalue
        nexttoken()
    when tkvar then
        x:=vars{tkvalue}
        nexttoken()
    when tklbrack then
        x:=readexpr()
        checktoken(tkrbrack)
        nexttoken()
    when tksub then
        nexttoken()
        x:=-readterm()
    when tksqr then
        nexttoken(tklbrack)
        x:=sqrt(readexpr())
        checktoken(tkrbrack)
        nexttoken()

    else
        error("Readterm?")
    esac

    x
end

func executeline(index)=

    line:=program[index]
    startlex(line.source)

    case line.kwd
    when letkwd then
        nexttoken(tkvar)
        varname:=tkvalue
        nexttoken(tkeq)
        vars{varname}:=readexpr()

    when printkwd then
        repeat
            x:=readexpr()
            print x
        until tk<>tkcomma
        println
    
    when gotokwd then
        nexttoken(tknumber)
        lineno:=tkvalue
        nexttoken()
dogoto::
        for i,l in program do
            if l.lineno=lineno then
                return i
            fi
        else
            error("Bad line:"+tostr(lineno))
        end

    when ifkwd then
        x:=readexpr()
        checktoken(tkthen)
        nexttoken(tknumber)
        lineno:=tkvalue
        nexttoken()
        if x then dogoto fi

    else
        error(kwdnames[line.kwd]+" not ready")
    esac
    if tk<>tkeol then error(kwdnames[line.kwd]+": EOL expected:"+tokennames[tk]) fi

    index+1
end

proc runprogram=
    if not program then error("Empty prog") fi
    pcindex:=1

    repeat
        pcindex:=executeline(pcindex)
    until pcindex>program.len

    println "Stopped"
end

proc main=
    loadprogram("test.bas")
#   listprogram()
    runprogram()
end
