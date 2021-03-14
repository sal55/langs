!Full example of reddit subthread about currying

record findrec=
    var searchstr
    function find(&self,text)=
        return self.searchstr in text
    end
end

proc start=
    text:="xyz abc ghi"

    findabc:=getspecialfunc("abc")
    findghi:=getspecialfunc("ghi")

    println findabc.find(text)
    println findghi.find(text)
end

function getspecialfunc(searchstr)=
    s:=findrec(searchstr)
    return s
end
