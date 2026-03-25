const nn = 100

ref[]ichar words
int nwords

['a'..'z']ref[]ichar big            !all words of each letter
['a'..'z']int nbig                  !how many of each letter

['a'..'z', nn]ichar small           !random subset of nn words

proc main=
    readwords()

    getsmall()

    challenge1()
    challenge2()
    challenge3()
end

proc readwords =
    [512]char buffer
    filehandle f
    int c

    f:=fopen("/texts/unsorted")
    stop unless f

!first pass counts all words and subsets
    while not myeof(f) do
        readlinen(f, buffer, buffer.len)
        ++nwords
        ++nbig[buffer[1]]
    end

!allocate memory
    words:=malloc(ichar.bytes*nwords)
    for c in 'a'..'z' do
        big[c]:=malloc(ichar.bytes*nbig[c])
        nbig[c]:=0
    end
!
!!second pass reads the data
    rewind(f)
    int n:=0
    while not myeof(f) do
        readlinen(f, buffer, buffer.len)
        words[++n]:=strdup(buffer)
        c:=buffer[1]
        big[c, ++nbig[c]]:=words[n]
    end
    fclose(f)
end

proc challenge1=
    println "Letter    Words In   Words Out"
    for c in 'a'..'z' do
        fprintln "#       # #", mchr(c), nbig[c]:"8", nn:"8"
    od
    println
end

proc getsmall=
    ref[]ichar s, d
    int r, ns
    ichar w

    for c in 'a'..'z' do
        s:=big[c]                   !source
        d:=&small[c]                !dest
        ns:=0

        repeat
            r:=mrandomrange(1, nbig[c])
            w:=s[r]
            for i to ns do
                exit when eqstring(w, d[i])
            else
                d[++ns]:=w
            end
        until ns=nn
    end
end

proc isort(ref[]ichar data, int ll, rr)=
    ichar pivot
    int i:=ll, j:=rr

    pivot:=data[(ll+rr)/2]

    repeat
        while strcmp(pivot, data[i])>0 and i<rr do ++i end
        while strcmp(pivot, data[j])<0 and j>ll do --j end

        if i<=j then
            swap(data[i], data[j])
            ++i
            --j
        end
    until i>j

    if ll<j then isort(data, ll, j) end
    if i<rr then isort(data, i, rr) end
end

proc challenge2=
    [100]ichar dupls
    int ndupls:=0

    isort(words, 1, nwords)

    for i in 2..nwords do
        if eqstring(words[i-1], words[i]) and ndupls<dupls.len then
            dupls[++ndupls]:=words[i]
        end
    end

    print "output: "
    for i to ndupls do
        print toupper(dupls[i]^):"c",,dupls[i]+1,$
    end
    println
    println
end 

proc challenge3=
    const size=nn*26
    [size]ichar data
    int ndata:=0, maxwidth:=0, rows, cols, i
    [16]char fmt

    for c in 'a'..'z' do
        for i to nn do
            data[++ndata]:=small[c, i]
            maxwidth max:= strlen(small[c,i])
        end
    end

    rows:=100
    while cols:=size%rows+1; cols>3 do rows+:=100 end

    print @fmt, "jl",,maxwidth+1

    for r to rows do
        i:=r
        fprint "#. ", r:"5"
        for c to cols when i<=size do
            print data[i]:fmt
            i+:=rows
        end
        println
    end
end
