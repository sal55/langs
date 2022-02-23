!Simple expression parser
!Output is an 'AST', here just implemented as nested lists

var token
var priotable = ["+":5, "-":5, "*":3, "/":3, "=":7, "^":1]
var source, sourcepos

function readexpr=
    return readfactor(8)
end

function readfactor(n)=
    if n<=1 then
        p := readterm()
    else
        p := readfactor(n-1)
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
            q := readexpr()
        when "^" then           ! power (right to left)
            q := readfactor(n)
        else                    ! normal binary op
            q := readfactor(n-1)
        esac
        p := (opcode, p,q)
    od
    return p
end

function readterm=
    case token
    when "(" then
        lex()
        p := readexpr()
        if token<>")" then abort("')' expected") fi
        lex()

    when "-" then
        lex()
        p := ("neg",readterm())

    elsif token in "abcdefghijklmnopqrstuvwxyz" then
        name := token
        lex()
        if token="(" then
            lex()
            q := readexpr()
            if token<>")" then abort("')' expected") fi
            lex()
            return ("call",name, q)
        else
            return name
        fi

    elsif token in "abcdefghijklmnopqrstuvwxyz0123456789" then
        p := token
        lex()

    else
        abort("Syntax error")
    end
    return p
end

proc lex=
    repeat
        if sourcepos>source.len then
            token := "?"
        else
            token := source[sourcepos++]
        fi
    until token <> " "
end

proc readprogram(s)=
    source := s
    sourcepos := 1
    lex()
    ast:=readexpr()

    println ast
end

readprogram("a=b+c*d")
