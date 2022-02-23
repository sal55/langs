# Simple expression parser

var token
var priotable = ["+":5, "-":5, "*":3, "/":3, "=":7, "^":1]
var source, sourcepos

function readexpr=
    return readfactor(8)
end

function readfactor(n)=

    if n<=1 then
        p:=readterm()
    else
        p:=readfactor(n-1)
    fi

    while token in "=^+-*/" do
        opcode := token
        prio := priotable{opcode}
        if n<>prio then
            exit
        fi
        lex()

        case opcode
        when "=" then           ! assignments
            q:=readexpr()
        when "^" then           ! power (right to left)
            q:=readfactor(n)
        else                    ! normal binary op
            q:=readfactor(n-1)
        esac
        p:=makeAST(opcode, p,q)
    od
    return p
end

function readterm=
    case token
    when "(" then
        lex()
        p:=readexpr()
        if token<>")" then abort("')' expected") fi
        lex()

    when "-" then
        lex()
        return makeAST("neg",readterm())

    elsif token in "abcdefghijklmnopqrstuvwxyz" then
        name:=token
        lex()
        if token="(" then
            lex()
            q:=readexpr()
            if token<>")" then abort("')' expected") fi
            lex()
            return makeAST("call",name, q)
        else
            return makeAST(name)
        fi

    elsif token in "abcdefghijklmnopqrstuvwxyz0123456789" then
        p:=makeAST(token)
        lex()

    else
        abort("Syntax error")
    end
    return p
end

function makeAST(tag, ?a, ?b)=
    if b.isdef then
        return (tag, a, b)
    elsif a.isdef then
        return (tag, a)
    else
        return tag
    fi
end

proc lex=
    repeat
        if sourcepos>source.len then
            token:="?"
        else
            token:=source[sourcepos++]
        fi
    until token <> " "
end

proc readprogram(s)=
    source:=s
    sourcepos:=1
    lex()
    ast:=readexpr()

    println ast
end

readprogram("a=b+c*d")
