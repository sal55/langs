import clib
import mlib

function strrepl_count(ichar s, oldstr, newstr)int =
    ichar t
    int oldlen:=strlen(oldstr)
    int newlen:=strlen(newstr)
    int count:=0

    if oldlen=0 then return strlen(s) fi

    while t:=strstr(s,oldstr) do
            count+:=(t-s)+newlen
            s:=t+oldlen
    od
    return count+strlen(s)
end

proc strrepl_copy(ichar s, oldstr, newstr,dest) =
    ichar t
    int oldlen:=strlen(oldstr)
    int newlen:=strlen(newstr)
    int n

    if oldlen=0 then strcpy(dest,s); return fi

    while t:=strstr(s,oldstr) do
        n:=t-s
        if n then
            memcpy(dest,s,n)
            dest+:=n
            if newlen then
                memcpy(dest,newstr,newlen)
                dest+:=newlen
            fi
        fi
        s:=t+oldlen
    od
    strcpy(dest,s)
end

function strrepl(ichar s, oldstr, newstr)ichar dest=
    int size

    size:=strrepl_count(s,oldstr,newstr)
    dest:=malloc(size+1)
    strrepl_copy(s,oldstr,newstr,dest)
    return dest
end

proc start=
    int n
    ichar s,t

    s:=cast(readfile("bible.txt"))

    if s=nil then stop fi

    for i:=1 to 100 do
        t:=strrepl(s,"lived","LIVEDFOREVER")
        if i<100 then free(t) fi
    od

    writefile("out",cast(t),strlen(t))
end
