import files

proc start=

    s:=readstrfile("./bible.txt")
    to 100 do
        t:=strrepl(s,"lived","LIVEDFOREVER")
    od

    writestrfile("out",t)
end

function strrepl(s, oldstr, newstr)=

    oldlen:=oldstr.len
    if oldlen=0 then return s fi

    t:=""

    while s do
        n:=oldstr in s

        if n=0 then
            t+:=s
             exit
        fi

        t +:= s[1..n-1]+newstr
        s := s[n+oldlen..$]
    od

    return t
end
