import mlib

proc start =
    if nsysparams>2 then
        fprintln "Usage: # [script]",sysparams[1] 
        stop 64
    elsif nsysparams=2 then
        runfile(sysparams[2])
    else
        runprompt()
    fi
end

proc runfile(ichar path) =
    ichar source:=readfile(path)
    if source = nil then
        println "Can't open",path
        stop 1
    fi
    run(source)
end

proc runprompt =
    ichar line
    do
        print "> "
        line:=readline()
        if line^=0 then exit fi
        run(line)
    od
end

proc run(ichar line)=
    println "Interpret:",line
end
