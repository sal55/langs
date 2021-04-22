import files
import lex
import run

global tabledata() tokennames, asttags =
    (tk_let,        $,      0),    !$ are the enum names as strings
    (tk_print,      $,      0),
    (tk_equals,     $,      0),
    (tk_plus,       $,      jadd),
    (tk_minus,      $,      jsub),
    (tk_times,      $,      jmul),
    (tk_divide,     $,      jdiv),
    (tk_lbrack,     $,      0),
    (tk_rbrack,     $,      0),
    (tk_number,     $,      jconst),
    (tk_name,       $,      jname),
    (tk_newline,    $,      0),
    (tk_eof,        $,      0),
    (tk_error,      $,      0),
end

global tabledata() asttagnames =
    (jassign,       $),
    (jprint,        $),
    (jconst,        $),
    (jname,         $),
    (jadd,          $),
    (jsub,          $),
    (jmul,          $),
    (jdiv,          $),
    (jneg,          $),
    (jstop,         $),
    (jblock,        $),
end

global record astnode =
    var tag, lhs, rhs
end

global var token, tokenvalue

proc start=
    lines:="let a=b+c*d\n"*500'000
    source:="let b=2\nlet c=3\nlet d=4\n" + lines + "print a"

    ast:=readprogram(source)
    runast(ast)
end

function readprogram(source)=

    lexinit(source)
    lex()

    stmts:=()

    while token<>tk_eof do
        stmts append:=readstatement()
        exit when token=tk_eof
        checktoken(tk_newline)
        lex()
    end

    return makeast(jblock, stmts)
end

function readstatement=

    case token
    when tk_let then
        return readlet()
    when tk_print then
        return readprint()
    else
        serror("Unknown statement")
    esac
    return nil
end

function readlet=
    lex()

    checktoken(tk_name)
    name:=tokenvalue

    lex()
    checktoken(tk_equals)
    lex()
    return makeast(jassign, name, readexpression())
end

function readprint=
    lex()
    return makeast(jprint,readexpression())
end

function readexpression=
    p:=readfactor()

    while token in [tk_plus, tk_minus] do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readfactor())
    od
    return p
end

function readfactor=
    p:=readterm()

    while token in [tk_times, tk_divide] do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readterm())
    od
    return p
end

function readterm=
    case token
    when tk_number then
        p:=makeast(jconst, tokenvalue)
        lex()
    when tk_name then
        p:=makeast(jname, tokenvalue)
        lex()
    when tk_minus then
        lex()
        return makeast(jneg, readterm())
    when tk_lbrack then
        lex()
        p:=readexpression()
        checktoken(rbracksym)
        lex()
    else
        serror("Bad expression")
    esac
    return p
end

proc checktoken(expectedtoken)=
    if token<>expectedtoken then
        serror(tokennames[expectedtoken]+" expected not "+tokennames[token])
    fi
end 

proc serror(message)=
    println "Syntax error:",message
    stop 1
end

function makeast(tag, lhs=0, rhs=0)=
    return astnode(tag, lhs, rhs)
end
