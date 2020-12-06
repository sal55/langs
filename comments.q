import sys
import files

const input="test.c"
const output="fred.c"

proc start=
    s:=readstrfile(input)
    t:=stripcomments(s)
    writestrfile(output,t)
end

function stripcomments(s)=
    if s="" then return "" fi
    case left(s,2)
    when "//" then
        return "\n"+stripcomments(dropline(drop(s,2)))
    when "/*" then
        return " "+stripcomments(nestedcomment(drop(s,2)))
    esac

    if (a:=left(s)) in "'""" then
        return a+skipquote(a,tail(s))
    else
        return a+stripcomments(tail(s))
    fi
end

function dropline(s)=
    n:=chr(10) in s
    if n=0 then abort("newline missing") fi
    return drop(s,n)
end

function nestedcomment(s)=
    if left(s,2)="*/" then return drop(s,2) fi
    if s="" then
        abort("Unterminated comment")
    fi
    return nestedcomment(tail(s))
end

function skipquote(q,s)=
    if s.len>=2 then
        if s[1]="\\" then
            return s[1..2]+skipquote(q,tail(s))
        fi
    fi
    if s.len>1 then
        return s[1]+(s[1]=q|stripcomments(tail(s))|skipquote(q,tail(s)))
    fi
    abort("Unterminated string")
    return ""
end
