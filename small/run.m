import mlib
import parser

const maxvar=20
[maxvar]ichar varnames
[maxvar]int varvalues
int nvars

global proc runast(node ast)=
    node a
    int varindex

    a:=ast.lhs

    case ast.tag
    when jblock then
        while a do
            runast(a)
            a:=a.nextnode
        od

    when jprint then
        while a do
            print evaluate(a),$
            a:=a.nextnode
        od
        println
    when jassign then
        varindex:=findvar(ast.svalue)       
        if varindex=0 then
            if nvars>=maxvar then runerror("Too many vars") fi
            varnames[++nvars]:=ast.svalue
            varindex:=nvars
        fi
        varvalues[varindex]:=evaluate(ast.rhs)
    else
        runerror("Can't execute:",asttagnames[ast.tag])
    esac
end

function evaluate(node ast)int=
    node a:=ast.lhs, b:=ast.rhs
    int varindex

    case ast.tag
    when jconst then
        return ast.value
    when jname then
        varindex:=findvar(ast.svalue)
        if varindex=0 then
            runerror("Var not set:",ast.svalue)
        fi
        return varvalues[varindex]

    when jadd then
        return evaluate(a)+evaluate(b)
    when jsub then
        return evaluate(a)-evaluate(b)
    when jmul then
        return evaluate(a)*evaluate(b)
    when jdiv then
        return evaluate(a)/evaluate(b)
    when jpower then
        return evaluate(a)**evaluate(b)
    when jneg then
        return -evaluate(a)
    else
        runerror("Bad expr")
    esac
    return 0
end

proc runerror(ichar message, param="")=
    println "Run error:",message,param
    stop 1
end

function findvar(ichar name)int=
    for i to nvars do
        if eqstring(name,varnames[i]) then
            return i
        fi
    od
    return 0
end

