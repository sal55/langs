!Forth interpreter

    var forthsource
    var lxindex
    var stack
    var stackindex
    var forthpredef
    var forthdict
    var callstack_source
    var callstack_lxindex
    var callindex
    var loophead=0          !0, or start of loop body in source
    var loopindex           !current index
    var loopto              !loop inclusive limit

    const etx=26
    var whitespace=[' ', '\t', 13, 10]

    proc main=

        initforth()

        println "Bart-Forth"
        println "Type bye, quit or exit to stop"

        if ncmdparams then
            runfile(cmdparams[1])
        fi

        do
            print "> "
            readln source:"l"

            if source in ("bye", "exit", "quit", "q", "") then exit fi

            runforth(source)
        od
    end

    proc runfile(file)=
        source:=readstrfile(file)
        if source=0 then
            error("Can't load "+file)
        fi
        runforth(source)
    end

    func fix(source)=
        return source+" "+chr(etx)
    end

    proc error(message)=
        println "Forth error:", message
        stop 1
    end

    proc initforth=
        stack::=()
        stackindex:=0
        forthdict::=[:]
        callstack_source::=()
        callstack_lxindex::=()
        callindex:=0
    end

    proc reset(source)=
        forthsource:=source
        lxindex:=0

        forthpredef:= [
            ".":        do_dot,
            "+":        do_add,
            "-":        do_sub,
            "*":        do_mul,
            "/":        do_div,
            "=":        do_eq,
            "<":        do_lt,
            ">":        do_gt,
            "and":      do_and,
            "or":       do_or,
            "invert":   do_invert,
            "mod":      do_mod,

            "dup":      do_dup,
            "drop":     do_drop,
            "swap":     do_swap,
            "emit":     do_emit,
            "CR":       do_cr,
            "cr":       do_cr,
            ":":        do_define,
            "variable": do_variable,
            "const":    do_const,
            "(":        do_comment_rb,
            "\\":       do_comment_eol,
            "s""":      do_stringconst_qt,
!	        ".""":      do_stringconst_qt,
! 	        ".""":      do_printstringconst,
!  	        ".(":       do_printstringconst,
            "if":       do_if,
            "else":     do_else,
            "then":     do_endif,
            "endif":    do_endif,
            "do":       do_do,
            "i":        do_loopindex,
            "loop":     do_loop,
            "print":    do_print,
            ".":        do_print,
            "$dummy":0]

    end

    proc do_dot=
!   print pop(),," "
    end

    proc do_add=
        push(pop()+pop())
    end

    proc do_sub=
        y:=pop()
        x:=pop()
        push(x-y)
    end

    proc do_mul=
        push(pop()*pop())
    end

    proc do_div=
        y:=pop()
        x:=pop()
        push(x/y)
    end

    proc do_eq=
        y:=pop()
        x:=pop()
        push((x=y|-1|0))
    end

    proc do_lt=
        y:=pop()
        x:=pop()
        push((x<y|-1|0))
    end

    proc do_gt=
        y:=pop()
        x:=pop()
        push((x>y|-1|0))
    end

    proc do_and=
        y:=pop()
        x:=pop()
        push(x iand y)
    end

    proc do_or=
        y:=pop()
        x:=pop()
        push(x ior y)
    end

    proc do_invert=
        push(inot pop())
    end

    proc do_mod=
        y:=pop()
        x:=pop()
        push(x rem y)
    end

    proc do_dup=
        x:=pop()
        push(x)
        push(x)
    end

    proc do_drop=
        x:=pop()
    end

    proc do_swap=
        x:=pop()
        y:=pop()
        push(x)
        push(y)
    end

    proc do_emit=
        print chr(pop())
    end

    proc do_cr=
!   println
    end

    proc do_define=
        name:=nexttoken()
        if forthpredef{name}.isdef or forthdict{name}.isdef or name=";" then
            error("Define: "+name+" already defined")
        fi
        startindex:=lxindex

        do
            endindex:=lxindex
            tk:=nexttoken()
            if tk=";" then exit fi
            if tk="" then error("No "";"" terminator for define") fi
        od

        forthdict{name}:=forthsource[startindex..endindex]+" "+chr(etx)

    end

    proc do_variable=
        error("VAR")
    end

    proc do_const=
        error("CONST")
    end

    proc do_comment_rb=
        repeat
            c:=nextchar()
            if c=etx then error("comment)?") fi
        until c =')'
    end

    proc do_comment_eol=
        repeat
            c:=nextchar()
            if c=etx then error("comment\\?") fi
        until c in [13,10]
    end

    proc do_stringconst_qt=
        s:=""
        do
            c:=nextchar()
            if c=etx then error("string""?") fi
            if c='"' then exit fi
            s+:=c
        od
        push(s)
    end

    proc do_printstringconst=
        s:=""
        do
            c:=nextchar()
            if c=etx then error("printstring)?") fi
    !       if c=')' then exit fi
            if c='"' then exit fi
            s+:=c
        od
    !   print s
    end

    proc do_call(source)=
        ++callindex
        callstack_source[callindex]:=forthsource
        callstack_lxindex[callindex]:=lxindex

        forthsource:=source
        lxindex:=0

    end

    proc do_return=
    !reached end of user function (callindex will be >0)
        forthsource:=callstack_source[callindex]
        lxindex:=callstack_lxindex[callindex]
        --callindex
    end

    proc do_if=
        if not pop() then           !false, skip following stmts
            count:=0
            do
                tk:=nexttoken()
                if tk="if" then
                    ++count
                elsif tk in "else then endif" then
                    if count=0 then
                        exit
                    else
                        --count
                    fi
                fi
            od
        fi
    end

    proc do_else=
	!if this is encountered, then reached end of true branch, and need to skip next
        count:=0
        do
            tk:=nexttoken()
            if tk="if" then
                ++count
            elsif tk in "else then endif" then
                if count=0 then
                    exit
                else
                    --count
                fi
            fi
        od
    end

    proc do_endif=
        !no-op
    end

    proc do_do=
        if loophead then error("nested loop") fi
        loophead:=lxindex
        loopindex:=pop()
        loopto:=pop()-1
    end

    proc do_loopindex=
        push(loopindex)
    end

    proc do_loop=
        if not loophead then error("loop?") fi
        loopindex+:=1
        if loopindex<=loopto then
            lxindex:=loophead
        fi
    end

    proc do_print=
        x:=pop()
        print x,$
    end

    func nextchar=
        c:=forthsource.[++lxindex]
        if c=etx then
            --lxindex
        fi
        return c
    end

    func nexttoken=
        while (c:=nextchar()) in whitespace do od

        if c=etx then return "" fi

        w::=chr(c)
        while (c:=nextchar()) not in whitespace do
            w+:=c
        od
        return w
    end

    proc push(value)=
        stack[++stackindex]:=value
    end

    func pop=
        if stackindex=0 then error("stack underflow") fi
        return stack[stackindex--]
    end

    proc runforth(source)=

        reset(fix(source))
        lxindex:=0

        do
            tk:=nexttoken()
            if tk="" then
                if callindex>0 then
                    do_return()
                    next
                else
                    exit
                fi
            fi

            stdop:=forthpredef{tk}

            if stdop.isdef then
                stdop()
            else
                userfn:=forthdict{tk}
                if userfn.isdef then
                    do_call(userfn)
                else
                    x:=strtoval(tk)
                    if x.isstring then
                        error("Unknown name: "+x)
                    fi
                    push(x)
                fi
            fi
        od

        println
    end
