const nn=100
var lower:='a'..'z'

proc main=
    words:=readtextfile("/texts/unsorted")
    short::=long::=new(list, lower, ())

    for w in words do
        long[asc(w)] &:= w
    end

    for c in lower do
        d:=long[c]
        s:=short[c]
        repeat
            w:=d[random(d.bounds)]
            if w not in s then
                s&:=w
            end
        until s.len=nn
    end

    challenge1(long, short)
    challenge2(words)
    challenge3(short)
end

proc challenge1(long, short)=
    println "Letter    Words In   Words Out"
    for c in lower do
        fprintln "#       # #", chr(c), long[c].len:"8", short[c].len:"8"
    od
    println
end

proc challenge2(words)=
    d:=[:]
    dupls::=()
    for w in words do
        if d{w}=1 then
            dupls &:= w
        else
            d{w}:=1
        end
    end

    print "found:  "
    for w in dupls do print w,$ end
    println

    isort(dupls)
    print "output: "
    for w in dupls do print convuc(w,1), $ end
    println
    println
end

proc challenge3(short)=
    data::=()

    for c in lower do
        data &&:=sort(short[c])
    end

    maxlen:=0
    for w in data do
        maxlen max:=w.len
    end
    size:=data.len

    rows:=100
    while cols:=size%rows+1; cols>3 do rows+:=100 end

    for r to rows do
        i:=r
        fprint "#. ", r:"5"
        for c to cols when i<=size do
            print data[i]:tostr(maxlen+1)+"jl"
            i+:=rows
        end
        println
    end
end
