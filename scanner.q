!Simple lexer based on Java version
!Interpreted, dynamic code
!Start reading on start() function on line 80 or so
!Reads source from file "input", accumulates tokens in list and prints them aout at the end
!Only tested on a handful of the main tokens 

import files

enum (
    identifier_tk,
    number_tk,
    string_tk,
    and_tk,
    class_tk,
    else_tk,
    false_tk,
    for_tk,
    fun_tk,
    if_tk,
    nil_tk,
    or_tk,
    print_tk,
    return_tk,
    super_tk,
    this_tk,
    true_tk,
    var_tk,
    while_tk,
    left_paren_tk,
    right_paren_tk,
    left_brace_tk,
    right_brace_tk,
    comma_tk,
    dot_tk,
    minus_tk,
    plus_tk,
    semicolon_tk,
    star_tk,
    slash_tk,
    bang_equal_tk,
    bang_tk,
    equal_equal_tk,
    equal_tk,
    less_equal_tk,
    less_tk,
    greater_equal_tk,
    greater_tk,
    eof_tk)

const cr=13, lf=10

var keywords = [
    "and":      and_tk,
    "class":    class_tk,
    "else":     else_tk,
    "false":    false_tk,
    "for":      for_tk,
    "fun":      for_tk,
    "if":       if_tk,
    "nil":      nil_tk,
    "or":       or_tk,
    "print":    print_tk,
    "return":   return_tk,
    "super":    super_tk,
    "this":     this_tk,
    "true":     true_tk,
    "var":      var_tk,
    "while":    while_tk]

var source
var tokens
var startx
var current
var line

proc start=
    source := readstrfile("input")

    tokens:=()
    startx:=current:=1
    line:=1

    while not isatend() do
        startx:=current
        scantoken()
    od

    tokens append:=(eof_tk, 0, line)

    forall tk in tokens do
        println "<",tk,">"
    od
end

proc scantoken =
    c := advance()

    switch c
    when '(' then addtoken(left_paren_tk)
    when ')' then addtoken(right_paren_tk)
    when '{' then addtoken(left_brace_tk)
    when '}' then addtoken(right_brace_tk)
    when ',' then addtoken(comma_tk)
    when '.' then addtoken(dot_tk)
    when '-' then addtoken(minus_tk)
    when '+' then addtoken(plus_tk)
    when ';' then addtoken(semicolon_tk)
    when '*' then addtoken(star_tk)
    when '!' then addtoken((match('=')|bang_bang_tk|bang_tk))
    when '=' then addtoken((match('=')|equal_equal_tk|equal_tk))
    when '<' then addtoken((match('=')|less_equal_tk|less_tk))
    when '>' then addtoken((match('=')|greater_qqual_tk|greater_tk))
    when '/' then
        if match('/') then
            while peek()<>'\n' and not isatend() do advance() od
        else
            addtoken(slash_tk)
        fi
    when ' ', cr, '\t' then
    when lf then
        ++line
    when '"' then
        stringx()
    else
        if isdigit(c) then
            number()
        elsif isalpha(c) then
            identifier()
        else
            error(line, "Unexpected character.")
        fi

    end switch
end

proc identifier =
    while isalphanumeric(peek()) do
        advance()
    od

    text:=source.[startx..current-1]

    tk:=keywords{text,0}
    if tk=0 then
        tk:=identifier_tk
    fi

    addtoken(tk,text)  # (Java version does not store the text or name of the identifer)
end

proc number =
    while isdigit(peek()) do advance() od

    if peek()='.' and isdigit(peeknext()) then
        advance()
        while isdigit(peek()) do advance() od
    fi

    value:=strtoval(source.[startx..current-1])
    addtoken(number_tk, value)
end

proc stringx =
    while peek()<>'"' and not isatend() do
        if peek()=lf then ++line fi
        advance()
    od

    if isatend() then
        error(line,"Unterminated string")
    fi

    advance()

    value:=source.[startx+1..current-2]
    addtoken(string_tk,value)
end

function advance =
    ++current
    return source.[current-1]
end

proc addtoken(tk, value=0) =
    tokens append:=(tk, value, line)
end

function isatend =
    return current >= source.len
end

function isdigit(c) =
    return c in '0'..'9'
end

function isalpha(c) =
    return c in ['A'..'Z','a'..'z', '_']
end

function isalphanumeric(c) =
    return isalpha(c) or isdigit(c)
end

function peek =
    if isatend() then return 0 fi
    return source.[current]
end

function peeknext =
    if current+1>-source.len then return 0 fi
    return source.[current+1]
end

function match(expected) =
    if isatend() then return 0 fi
    if source.[current] <> expected then
        return 0
    fi
    ++current
    return 1
end

proc error(line, message)=
    println "Error:",message,"on line:",line
    stop 1
end
