import files

proc start=

    results:=()

    for i:=1 to 127 do
        results append:=testfile(i)
    od
    println

    npasses:=0
    ncompile_errors:=0
    nmatcherrors:=0

    for i in results do
        name:="fizzbuzz"+tostr(i,"z3")
        case results[i]
        when 1 then
            println name,"passed"
            ++npasses
        when -1 then
            ++ncompile_errors
            println name,"failed"
        else
            ++nmatcherrors
            println name,"failed"
        esac
    od
    println

    println npasses,"Passed out of", results.len
    println ncompile_errors,"Failed to compile"
    println nmatcherrors,"Compiled but gave wrong results"

end

function testfile(n)=
    name:="fizzbuzz"+tostr(n,"z3")
    file:=name+".c"
    exe:=name+".exe"

    writestrfile("output","blah blah")

!   status:=system("bcc "+file)
!   status:=system("tcc "+file)
!   status:=system("gcc "+file+" -o"+exe)
!   status:=system("gcc -std=c11 -O2 "+file+" -o"+exe)
!   status:=system("\\dm\\bin\\dmc "+file)
!   status:=system("\\dm\\bin\\dmc -o "+file)
    status:=system("\\lcc\\bin\\lc64 "+file)

    if status<>0 then
        println "Can't compile",file
        return -1
    fi

    system(exe+" >output")
    status:=system("fc expected.txt output >output2")
    if status<>0 then
        println "Bad output for", exe
        return -2
    fi
    println exe,"passed"

    return 1
end
