import parser

var variables=[:]

global proc runast(ast)=
    case ast.tag
    when jblock then
        forall a in ast.lhs do
            runast(a)
        od
    when jprint then
        forall x in ast.lhs do
            print evaluate(x),," "
        od
        println
    when jassign then
        variables{ast.lhs}:=evaluate(ast.rhs)
    else
        runerror("Can't execute:"+asttagnames[ast.tag])
    esac
end

function evaluate(ast)=
    case ast.tag
    when jconst then
        return ast.lhs
    when jname then
        x:=variables{ast.lhs}
        if x.isvoid then
            runerror(ast.lhs+" not set")
        fi
        return x
    when jadd then
        return evaluate(ast.lhs)+evaluate(ast.rhs)
    when jsub then
        return evaluate(ast.lhs)-evaluate(ast.rhs)
    when jmul then
        return evaluate(ast.lhs)*evaluate(ast.rhs)
    when jdiv then
        return evaluate(ast.lhs)/evaluate(ast.rhs)
    when jpower then
        return evaluate(ast.lhs)**evaluate(ast.rhs)
    when jneg then
        return -evaluate(ast.lhs)
    else
        runerror("Bad expr")
    esac
    return 0
end

proc runerror(message)=
    println "Run error:",message
    raise langerror
end
