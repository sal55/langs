import sys
import files

tabledata() opcnames, operands =
    (kpush,         $,  'N'),
    (kpop,          $,  0),
    (kadd,          $,  0),
    (ksub,          $,  0),
    (kincr,         $,  0),
    (kdecr,         $,  0),
    (kmul,          $,  0),
    (kdiv,          $,  0),
    (kjump,         $,  'A'),
    (kje,           $,  'A'),
    (kjne,          $,  'A'),
    (kjgt,          $,  'A'),
    (kjlt,          $,  'A'),
    (kjge,          $,  'A'),
    (kjle,          $,  'A'),
    (kget,          $,  'N'),
    (kset,          $,  'N'),
    (kgetarg,       $,  'N'),
    (ksetarg,       $,  'N'),
    (knoop,         $,  0),
    (kprint,        $,  0),
    (kprintc,       $,  0),
    (kprintstack,   $,  0),
    (kcall,         $,  'A'),
    (kret,          $,  0),
    (kcollapseret,  $,  'N'),
    (kendprogram,   $,  0),
!--
    (kproc,         $,  'A'),
    (klabel,        $,  'A'),
    (kend,          $,  0),
end

var bytecode
var pc
var pcentry
var stack, callstack
var stackptr, callstackptr
var procnames, procoffsets
var labelnames, labeloffsets
var fwdrefnames, fwdrefoffsets
var inproc
var lineno

proc start=
    if ncmdparams>=2 then
        file:=cmdparams[2]
    else
        println "Usage: pc interp file"
        stop
    fi

    sourcelines:=readtextfile(file)

    if sourcelines=0 then
        pcerror("Can't open "+file)
    fi

    compile(sourcelines)

    run()
end

proc pcerror(mess)=
    abort(mess+" on line: "+tostr(lineno))
end

proc fixlabels=
    forall i, name in fwdrefnames do
        offset:=fwdrefoffsets[i]
        if n:=(name in procnames) then
            bytecode[offset]:=procoffsets[n]
        elsif n:=(name in labelnames) then
            bytecode[offset]:=labeloffsets[n]
        else
            pcerror("Can't find label or proc: "+name)
        fi
    od
end

proc compile(sourcelines)=
    procnames:=()
    procoffsets:=()
    labelnames:=()
    labeloffsets:=()
    fwdrefnames:=()
    fwdrefoffsets:=()
    inproc:=0
    pc:=0                   !current generated bytecode or opnd index
    pcentry:=1
    bytecode:=()

    forall i,line in sourcelines do
        lineno:=i
        compileline(line)
    od

    bytecode[++pc]:=kendprogram

end

proc compileline(line)=
    sreadln(line)
    read kwd:"n"
    return when leftstr(kwd,2)="--"
    return when kwd=""

    opc:="k"+kwd in opcnames
    if opc=0 then
        pcerror("Unknown opcode:"+kwd)
    fi

    case operands[opc]
    when 'A' then read opnd:"n"
    when 'N' then read opnd:"i"
    else
        opnd:="-"
    esac

    case opc
    when kproc then
        if opnd in procnames then pcerror("Dupl proc:"+opnd) fi
        if inproc then pcerror("Nested proc:"+opnd) fi
        procnames append:=opnd
        procoffsets append:=pc+1
        pcentry:=0
        inproc:=1
    when klabel then
        if opnd in labelnames then pcerror("Dupl label:"+opnd) fi
        labelnames append:=opnd
        labeloffsets append:=pc+1
    when kend then
        if not inproc then pcerror("End outside proc") fi
        inproc:=0
        pcentry:=pc+1
    else
        bytecode[++pc]:=opc
        case operands[opc]
        when 'N' then
            bytecode[++pc]:=opnd
        when 'A' then
            fwdrefnames append:=opnd
            fwdrefoffsets append:=pc+1
            bytecode[++pc]:=0
        esac
    esac
end

proc push(x)=
    stack[++stackptr]:=x
end

function pop=
    if stackptr<1 then pcerror("Stack underflow") fi
    return stack[stackptr--]
end

function peek=
    if stackptr<1 then pcerror("Stack underflow") fi
    return stack[stackptr]
end

proc pushcall(x)=
    callstack[++callstackptr]:=x
end

function popcall=
    if callstackptr<1 then pcerror("Call stack underflow") fi
    return callstack[--callstackptr]
end

proc run=
    fixlabels()

    pc:=pcentry
    stack:=()
    callstack:=()
    stackptr:=callstackptr:=0

    println "Starting interpreter..."
    doswitch (opc:=bytecode[pc++]; x:=bytecode[pc]; opc)
    when kpush then
        push(x)
        ++pc
    when kpop then
        pop()
    when kadd then
        push(pop()+pop())
!   when ksub then
    when kincr then
        push(pop()+1)
    when kdecr then
        push(pop()-1)
!   when kmul then
!   when kdiv then
!   when kjump then
!   when kje then
!   when kjne then
!   when kjgt then
!   when kjlt then
!   when kjge then
    when kjle then
        ++pc
        if peek()<=0 then pc:=x fi
!   when kget then
!   when kset then
    when kgetarg then
        ++pc
        push(stack[$-x])            ! $ means last index
!   when ksetarg then
!   when knoop then
!   when kprint then
    when kprintc then
        print pop():"c"

!   when kprintstack then
    when kcall then
        fnaddr:=bytecode[pc++]
        pushcall(stackptr)
        pushcall(pc)
        pc:=fnaddr

    when kret then
        pc:=popcall()
        stackptr:=popcall()

!   when kcollapseret then
!   when kendprogram then
    elsif opc<=opcnames.len then
        pcerror("Unimplemented opc:"+opcnames[opc])
    else
        pcerror("Unknown opc:"+tostr(opc))
    end doswitch
end
