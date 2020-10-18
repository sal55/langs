import clib

int token
ref char lxptr
real tkvalue
enum (add_tk, sub_tk, mul_tk, div_tk, lbrack_tk, rbrack_tk,
         number_tk, sin_tk, end_tk)
['a'..'z']real variables

function evalx(ichar expr)real x=
    lxptr:=expr

    nexttk()
    x:=evalexpr()
    if token<>end_tk then
        error("Bad ending")
    fi
    return x
end

function evalexpr:real=
    return evaladd()
end

function evaladd:real=
    real x,y
    int opc

    x:=evalmul()

    while token in [add_tk, sub_tk] do
        opc:=token
        nexttk()
        y:=evalmul()
        if opc=add_tk then
            x+:=y
        else
            x-:=y
        fi
    od

    return x
end

function evalmul:real=
    real x,y
    int opc

    x:=evalterm()

    while token in [mul_tk, div_tk] do
        opc:=token
        nexttk()
        y:=evalterm()
        if opc=mul_tk then
            x*:=y
        else
            if y=0.0 then error("Divide by zero") fi
            x/:=y
        fi
    od
    return x
end

function evalterm:real=
    real x

    case token
    when sub_tk then
        nexttk()
        return -evalterm()
    when lbrack_tk then
        nexttk()
        x:=evalexpr()
        if token<>rbrack_tk then error("')' expected") fi
        nexttk()
        return x
    when sin_tk then
        nexttk()
        return sin(evalterm())
    when number_tk then
        x:=tkvalue
        nexttk()
        return x
    else
        error("Term")
    esac
    return 0.0
end

proc nexttk=
    int c
    ref char pstart, pend

    switch c:=lxptr++^
    when ' ','\t' then nexttk()
    when '+' then token:=add_tk
    when '-' then token:=sub_tk
    when '*' then token:=mul_tk
    when '/' then token:=div_tk
    when '(' then token:=lbrack_tk
    when ')' then token:=rbrack_tk
    when 'A'..'Z' then
        tkvalue:=variables[c+' ']       
        token:=rbrack_tk

    when 'a'..'z' then
        if c='s' and lxptr^='i' and (lxptr+1)^='n' then
            lxptr+:=2
            token:=sin_tk
        else
            tkvalue:=variables[c]       
            token:=number_tk
        fi

    when '.','0'..'9' then
        pstart:=lxptr-1
        tkvalue:=strtod(pstart,&lxptr)
        token:=number_tk

    when 0 then
        token:=end_tk
    else
        error("Syntax")
    endswitch
end


proc error(ichar mess)=
    println "Error:",mess
    stop 1
end

proc start=
    variables['x']:=0.523
    variables['y']:=34.6
    variables['z']:=99.0

    println =evalx("sin(x)*2+y")

end
