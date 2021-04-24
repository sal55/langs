import clib
import msys
import mlib
import lex
import run

global tabledata() []ichar tokennames, []int asttags =
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

global tabledata() []ichar asttagnames =
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

global type node = ref astnode
global record astnode =
    int tag
    union
        node lhs
        ichar svalue
        int value
    end

    node rhs
    node nextnode
end


global const langerror=10

global int token
global int tokenvalue
global ichar tokensvalue

proc start=
    node ast
    ichar source

    source:=cast(readfile("prog.m"))
    if source=nil then stop fi

    ast:=readprogram(source)

    runast(ast)

end

function readprogram(ichar source)node =
    node stmtlist:=nil,stmtlistx:=nil, p

    lexinit(source)
    lex()

    while token<>tk_eof do
        p:=readstatement()
        if stmtlist=nil then
            stmtlist:=stmtlistx:=p
        else
            stmtlistx.nextnode:=p
            stmtlistx:=p
        fi

        exit when token=tk_eof
        checktoken(tk_newline)
        lex()
        while token=tk_newline do lex() end
    end

    return makeast(jblock, stmtlist)
end

function readstatement:node=
    docase token
    when tk_let then
        return readlet()
    when tk_print then
        lex()
        return readprint()
    when tk_newline then
        lex()
    when tk_name, tk_lbrack, tk_const, tk_minus then
        return readprint()
    else
        serror("Unknown statement:",tokennames[token])
    end docase
    return nil
end

function readlet:node=
    ichar name

    lex()

    checktoken(tk_name)
    name:=pcm_copyheapstring(tokensvalue)

    lex()
    checktoken(tk_equals)
    lex()
    return makeast(jassign, cast(name), readexpression())
end

function readprint:node=
    node exprlist:=nil, exprlistx:=nil, p

    do
        p:=readexpression()
        if exprlist=nil then
            exprlist:=exprlistx:=p
        else
            exprlistx.nextnode:=p
            exprlistx:=p
        fi
        exit when token<>tk_comma
        lex()
    od

    p:=makeast(jprint,exprlist)
    return p
    return makeast(jprint,exprlist)
end

function readexpression:node=
    return readaddterms()
end

function readaddterms:node p=
    int tag

    p:=readmulterms()

    while token in [tk_plus, tk_minus] do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readmulterms())
    od
    return p
end

function readmulterms:node p=
    int tag
    p:=readpowerterms()

    while token in [tk_times, tk_divide] do
        tag:=asttags[token]
        lex()
        p:=makeast(tag, p, readpowerterms())
    od
    return p
end

function readpowerterms:node p=
    p:=readterm()

    while token=tk_power do
        lex()
        p:=makeast(jpower, p, readpowerterms())
    od
    return p
end

function readterm:node p=
    case token
    when tk_const then
        p:=makeast(jconst, cast(tokenvalue))
        lex()
    when tk_name then
        p:=makeast(jname, cast(pcm_copyheapstring(tokensvalue)))
        lex()
    when tk_minus then
        lex()
        return makeast(jneg, readpowerterms())
    when tk_lbrack then
        lex()
        p:=readexpression()
        checktoken(tk_rbrack)
        lex()
    else
        serror("Bad expression")
    esac
    return p
end

proc checktoken(int expectedtoken)=
    [256]char str
    if token<>expectedtoken then
        fprintln @str,"# expected not", tokennames[expectedtoken],tokennames[token]
        serror(str)
    fi
end 

global proc serror(ichar message,param="")=
    println "Syntax error:",message,param
    stop 1
end

function makeast(int tag, node lhs=nil, rhs=nil)node p=
    p:=pcm_allocz(astnode.bytes)
    p.tag:=tag
    p.lhs:=lhs
    p.rhs:=rhs
    return p
end
