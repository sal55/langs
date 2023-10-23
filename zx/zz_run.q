!Z80 Assembler

module zz_decls
module zz_disasm
module zz_lex
module zz_parse
module zz_run
module zz_support
module zz_tables

global var asmsource
global var ram
global var ramstart, ramptr
global var labeltable
global var symbollist
global var ramflags             !'1' means byte is a DB byte

var infile, option

proc main=

    println "ZZ Project"

    if ncmdparams>=1 then
        infile:=addext(cmdparams[1],"za")
        option:="-asm"
        if ncmdparams>=2 then
            option:=convlc(cmdparams[2])
        fi
    else
        println "Usage:"
        println "   qq zz file[.za] [options]       # -run -asm"
        stop
    fi

    initlex()
    initdata()

    asmsource:=loadsource(infile)

    lexstart(asmsource)

    parse()

    case option
    when "-asm" then
        showtables()
    when "-run" then
        run(1)
!       run(1000)
    else
        abort("Bad option:"+option)
    esac

!   showtables()
!   println "Finished"
    println
end

proc initdata=
    ram:=new(array,byte, 0..65535, 0)
    ramflags:=new(array,byte, 0..65535, 0)
    ramstart:=ramptr:=&ram
    labeltable::=()
    symbollist::=()
end
