import clib
import sys
import files
import lex
import run

global tabledata() tokennames, asttags =
    (tk_let,        $,      0),
    (tk_print,      $,      0),
    (tk_equals,     $,      0),
    (tk_comma,      $,      0),
    (tk_plus,       $,      jadd),
    (tk_minus,      $,      jsub),
    (tk_times,      $,      jmul),
    (tk_divide,     $,      jdiv),
    (tk_power,      $,      jpower),
    (tk_lbrack,     $,      0),
    (tk_rbrack,     $,      0),
    (tk_const,      $,      jconst),
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
    (jpower,        $),
    (jneg,          $),
    (jstop,         $),
    (jblock,        $),
end

global record astnode =
    var tag, lhs, rhs
end

global const langerror=10

global var token, tokenvalue

proc start=
    do
        print "> "
        readln s:"l"
        if s="" then exit fi
        try
            ast:=readprogram(s)
            runast(ast)
        except langerror then
        end
    od
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
        while token=tk_newline do lex() end
    end

    return makeast(jblock, stmts)
end

function readstatement=

    docase token
    when tk_let then
        return readlet()
    when tk_print then
        return readprint()
    when tk_newline then
        lex()
    else
        serror("Unknown statement:"+tokennames[token])
    end docase
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
    exprs:=()
    do
        exprs append:=readexpression()
        exit when token<>tk_comma
        lex()
    od

    return makeast(jprint,exprs)

!   return makeast(jprint,readexpression())
end

function readexpression=
    p:=readaddterm()

    while token in [tk_plus, tk_minus] do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readaddterm())
    od
    return p
end

function readaddterm=
    p:=readmulterm()

    while token in [tk_times, tk_divide] do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readmulterm())
    od
    return p
end

function readmulterm=
    p:=readterm()

    while token=tk_power do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readterm())
    od
    return p
end

function readterm=
    case token
    when tk_const then
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

global proc serror(message)=
    println "Syntax error:",message
    raise langerror
end

function makeast(tag, lhs=0, rhs=0)=
    return astnode(tag, lhs, rhs)
end
