!(Part of BASIC subset lexer)

var identstarter = ['A'..'Z', 'a'..'z']
var numericstarter = ['0'..'9']
var identchars = ['A'..'Z', 'a'..'z','0'..'9','_','$']
var numericchars = ['0'..'9', '.', 'e']

var puncttable = ['(':tklbrack, ')':tkrbrack, '+':tkadd, '-':tksub, '*':tkmul, '/':tkdiv,
        '=':tkeq, ',':tkcomma, ';':tksemi, '%':tkidiv, ':':tkcolon]
var zero=chr(0)

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
    (tkidiv,    $,      1,      %),

    (tkeq,      $,      3,      =),
    (tkne,      $,      3,      <>),
    (tklt,      $,      3,      <),
    (tkle,      $,      3,      <=),
    (tkge,      $,      3,      >=),
    (tkgt,      $,      3,      >),

    (tkand,     "and",  4,      and),
    (tkor,      "or",   5,      or),

    (tkcomma,   $,      0,      0),
    (tksemi,    $,      0,      0),
    (tkcolon,   $,      0,      0),

    (tksqr,     "sqr",  0,      sqrt),
    (tklen,     "len",  0,      len),
    (tkchr,     "chr",  0,      chr),
    (tkasc,     "asc",  0,      asc),

    (tklet,     "let",  0,      0),
    (tkprint,   "print",0,      0),
    (tkif,      "if",   0,      0),
    (tkgoto,    "goto", 0,      0),
    (tkbye,     "bye",  0,      0),

    (tkrem,     "rem",  0,      0),
    (tkthen,    "then", 0,      0),

    (tkother,   $,      0,      0),
end

var binops      = [tkadd, tksub, tkmul, tkdiv,tkidiv, tkeq, tkne, tklt, tkle, tkge, tkgt, tkand, tkor]
var keywords    = [tklet, tkprint, tkif, tkgoto, tkrem]
var builtins    = [tksqr, tklen]

var lexstr, lexlen, lexpos
var tk, tkvalue

proc nexttoken(tkexp=0) =
    tkvalue::=""

    docase c:=lexstr.[lexpos++]
    when identstarter then
        tkvalue::=chr(tolower(c))
        while (c:=lexstr.[lexpos++]) in identchars do tkvalue+:=tolower(c) od
        --lexpos

        tk:=tkvalue inx tokennames
        if not tk.isfound then
            tk:=tkvar
            if vars{tkvalue}.isvoid then
                vars{tkvalue}:=0.0
            fi
        fi
        if tk=tkrem then tk:=tkeol fi
        exit

    when numericstarter then
        s::=chr(c)
        while (c:=lexstr.[lexpos++]) in numericchars do s+:=c od
        tkvalue:=strtoval(s)
        --lexpos
        tk:=tknumber
        exit

    when ' ', '\t' then

    when '"' then
        while (c:=lexstr.[lexpos++]) not in ['"',0] do tkvalue+:=c od
        if c=0 then error("String?") fi
        tk:=tkstring
        exit

    when 0 then
        --lexpos; tk:=tkeol; exit

    when '<' then
        tk:=
            case lexstr.[lexpos]
            when '=' then ++lexpos; tkle
            when '>' then ++lexpos; tkne
            else tklt
            esac
            exit
    when '>' then tk:=(lexstr.[lexpos]='='|(++lexpos; tkge) | tkgt); exit
    when '\'' then
        tk:=tkeol
        exit

    elsif tk:=puncttable{c,0} then
        exit
    else
        tk:=tkother; exit

    end

    if tkexp then checktoken(tkexp) fi
end
