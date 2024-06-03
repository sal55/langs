!Convert .cc input file to .c (generate also .cl .cx headers)

var charset
var exports,locals
var strings
var stack
var nstack
var lineno
var errcount

proc start=
    if ncmdparams>=1 then
        file:=convlc(cmdparams(1))
    else
        file:="test"
    fi

    infile:=addext(file,".cc")
    outfile:=changeext(file,".c")

    println "Translating ",infile," to ", outfile

    f:=openfile(infile)
    if not f then
        messagebox(0,"Can't open file "+infile,"Error",0)
        return
    fi
    g:=createfile(outfile)
    if not g then
        messagebox(0,"Can't create file "+outfile,"Error",0)
        closefile(f)
        return
    fi

    println "Processing file: ",infile
    fnindex:=0
    charset:=['A'..'Z','a'..'z','0'..'9','_','$']
    exports:=locals:=()
    stack:=()
    nstack:=0
    lineno:=0
    errcount:=0

    while not eof(f) do
        readln #f,x:"l"
        ++lineno

        x:=convertline(x)

        println #g,x
    od

    closefile(f)
    closefile(g)

    println "Output is in: ",outfile

    f:=createfile(changeext(outfile,"cl"))
    forall x in locals do
        println #f,x
    od
    closefile(f)

    if nstack then serror("Out of sync at eof") fi

    f:=createfile(changeext(outfile,"cx"))
    forall x in exports do
        println #f,x
    od
    closefile(f)
end

function fixbrace(x)=
    n:=" {" in x
    if not n then n:="{" in x fi
    if n then
        x:=left(x,n-1)+";"
    fi
    return x
end

function push(x)=
    stack[++nstack]:=x
end

function pop=
    if nstack then --nstack fi
end

function tos=
    if nstack<1 then serror("Stack error") fi
    return stack[nstack]
end

function serror(s)=
    println "Error on Line:",lineno,":",s
    if ++errcount=10 then
        println "Aborting: too many errors"
        stop
    fi
end

function replacestr(s,t,u,skiprb)=
!replace first occurence of t in s by u
!return new string
!skiprb=1 to only start searching after ) encountered (this simple version does not
! check for whole words)

    n:=0
    if skiprb=1 then
        m:=")" in s
        if m then
            n:=convlc(t) in convlc(right(s,-m))
            if n then n+:=m fi
        fi
    fi

    if n=0 then n:=convlc(t) in convlc(s) fi
    if not n then return s fi
    return left(s,n-1)+u+right(s,-(n+t.len-1))
end

function replacestr_cs(s,t,u,skiprb)=
!case sensitive version
!replace first occurence of t in s by u
!return new string
!skiprb=1 to only start searching after ) encountered (this simple version does not
! check for whole words)

    n:=0
    if skiprb=1 then
        m:=")" in s
        if m then
            n:=t in right(s,-m)
            if n then n+:=m fi
        fi
    fi

    if n=0 then n:=t in s fi
    if not n then return s fi
    return left(s,n-1)+u+right(s,-(n+t.len-1))
end

function getword(x)=
    s:=""
    foreach c in x do
        if not (asc(c) in charset) then exit fi
        s+:=convlc(c)
    od
    return s
end

function convertline(x)=
    if x="" then return x fi
    if left(x,2)="//" then return x fi
    if left(x)="!" then return "//"+right(x,-1) fi
    n:="    !" in x
    if n then x:=left(x,n)+"//"+right(x,-(n+1)) fi
    if left(x)="#" then return x fi

    if convlc(left(x,12))="global proc " then
        if nstack then serror("endproc1 error") fi
        x:="void "+right(x,-11)
        x:=replacestr(x,"=","{",0)
        y:="extern "+fixbrace(x)
        if not (y in exports) then
            exports&:=y
        fi
        push('F')
        return x
    elsif convlc(left(x,16))="global function " then
        if nstack then serror("endproc2 error") fi
        x:=right(x,-16)
        x:=replacestr(x,"=","{",0)
        y:="extern "+fixbrace(x)
        if not (y in exports) then
            exports&:=y
        fi
        push('F')
        return x
    elsif convlc(left(x,5))="proc " then
        if nstack then serror("endproc3 error") fi
        x:="void "+right(x,-5)
        x:=replacestr(x,"=","{",0)
        y:="static "+fixbrace(x)
        if not (y in locals) then
            locals&:=y
        fi
        push('F')
        return x
    elsif convlc(left(x,9))="function " then
        if nstack then serror("endproc4 error") fi
        x:=right(x,-9)
        x:=replacestr(x,"=","{",0)
        y:="static "+fixbrace(x)
        if not (y in locals) then
            locals&:=y
        fi
        push('F')
        return x
    fi

!fix = and == problems; original will use := and =
    oldc:=x.[1]
    s:=oldc
    for i:=2 to x.len-1 do      !assume = can't be at bol or eol
        c:=x.[i]
        d:=x.[i+1]

        if c=":" and d="=" then
            s+:="="
            ++i
        elsif c="=" and not (oldc in "<>") then
            s+:="=="
        elsif c="<" and d=">" then
            s+:="!="
            ++i
        else
            s+:=c
        fi
        oldc:=c
    od
    x:=s+right(x)

    spaces:=""
    while x.len and left(x,1) in " \t" do
        spaces+:=left(x)
        x:=right(x,-1)
    od
    if x="" then return "" fi

    k:=getword(x)
    klen:=k.len
    xrest:=right(x,-k.len)

    case k
    when "if" then
        push('I')
        n:="then" in x
        if not n then n:="THEN" in x fi
        if not n then serror("If: no then"); return spaces+x fi
        return spaces+left(x,n-1)+"{"+right(x,-(n+3))

    when "elsesw" then
        return spaces+"break; default:"+xres

    when "else" then
        if tos()='I' then
            return spaces+"} else {"+xrest
        else
            return spaces+"break; default:"+xrest
        fi

    when "elsif" then
        if tos()<>'I' then serror("Bad elsif") fi
        x:=xrest
        n:="then" in x
        if not n then n:="THEN" in x fi
        if n then
            x:=left(x,n-1)+"{"+right(x,-(n+3))
        else
            serror("Elsif: no then")
        fi
        return spaces+"} else if"+x

    when "fi" then
        if tos()<>'I' then serror("Bad fi") fi
        pop()
        return spaces+"}"+xrest

    when  "endsw" then
        if tos()<>'S' then serror("Bad endsw") fi
        pop()
        return spaces+"}"+xrest

    when "endif" then
        if tos()<>'I' then serror("Bad fi") fi
        pop()
        return spaces+"}"+xrest

    when "endswitch" then
        if tos()<>'S' then serror("Bad endswitch") fi
        pop()
        return spaces+"}"+xrest

    when "end" then
        pop()
        return spaces+"}"+xrest

    when "endproc" then
        if tos()<>'F' then serror("Bad endproc") fi
        pop()
        return spaces+"}"+xrest
    when "switch" then
        push('S')
        if ("{" in x)=0 then
            n:=")" in x
            if n then x:=left(x,n)+" {"+right(x,-n) fi
        fi
        return spaces+x

    when "when" then
        if tos()<>'S' then serror("Bad when") fi
        n:="then" in x
        if not n then n:="THEN" in x fi
        if not n then return spaces+x fi
        cases:=x.[6..n-2]
        rest:=right(x,-(n+3))
        s:=""
        while "," in cases do
            n:="," in cases
            s+:="case "+left(cases,n-1)+":"
            cases:=right(cases,-n)
        od
        s+:="case "+cases+":"

        return spaces+"break; "+s+" "+rest

    when "for" then
        push('L')
        x:=replacestr(x,"do","{",1)
        return spaces+x

    when "while" then
        push('L')
        x:=replacestr(x,"do","{",1)
        return spaces+x

    when "to" then
        push('L')
        x:=replacestr(x,"do","{",1)
        return spaces+x

    when "od" then
        if tos()<>'L' then serror("Bad od:"+chr(tos())); fi
        pop();
        return spaces+"}"+xrest

    when "cpl" then         ! (debugging println)
        if "," in xrest then        !assume printf
            to 4 do
                xrest:=replacestr_cs(xrest,"%D","%d",0)
                xrest:=replacestr_cs(xrest,"\\N","\\n",0)
                xrest:=replacestr_cs(xrest,"%S","%s",0)
            od
            return spaces+"printf("+xrest+");"
        else
            return spaces+"puts("+xrest+");"
        fi
    esac
    return spaces+x
end
