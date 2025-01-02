!PCL Interpreter

pstrec emptyst

!const dostackcheck = 1
const dostackcheck = 0

macro getopcode      = pc.opcode
macro getseqno       = pc.seqno
macro steppc         = ++pc
macro getnargs       = pc.nargs
macro getnvars       = pc.nvariadics
macro getnparams     = pc.paramslots
macro getnlocals     = pc.localslots
macro getcond        = pc.condcode
macro getlabel       = labeltable[pc.labelno]
macro getlabel2      = labeltable[(pc+1).labelno]
macro getmode        = pc.mode
macro getmode2       = pc.mode2
macro getsize        = pc.size
macro getscale       = pc.scale
macro getextra       = pc.extra
macro getincr        = pc.stepx
macro getswmin       = pc.minlab
macro getswmax       = pc.maxlab
macro isfloat        = ispfloat(getmode)
macro issigned       = psigned[getmode]


ref[]pcl labeltable

global macro pcerror(a) = pcerrorx(pc, a)
global macro pcerror2(a,b) = pcerrorx(pc, a,b)

export proc pcl_runpcl=
    int stopcode

    loadlibs()

    fixuppcl()

    if entryproc=nil then
        pcerrorx(pcstart,"No 'main' entry point")
    fi

    docmdskip()

    if pverbose then
        println "Run PCL:"
    fi

    stopcode:=dispatch_loop(entryproc.pcaddr, entryproc.nparams=2)

    if pverbose then
        println "Stopped",stopcode
        println
    fi

    stop stopcode
end

global func dispatch_loop(pcl pcentry, int cmain=0)int=
    const stacksize = 70'000
    const callstacksize = 10'000
    const pcmask=15

!HEAP VERSION
    ref[]int stack
        ref [stacksize]real     xstack @stack
        ref [stacksize]word     ustack @stack
        ref [stacksize]ref void pstack @stack

    pcl pc:=pcentry
        ref byte pcb @pc
        u64 pci @pc

    int sp:=0
    int fp:=0
    int     a
        real    x   @a
        real32  sx  @a
        word    u   @a
    ref byte ptr
        ref u8  pu8     @ptr
        ref u16 pu16    @ptr
        ref u32 pu32    @ptr
        ref u64 pu64    @ptr
        ref i8  pi8     @ptr
        ref i16 pi16    @ptr
        ref i32 pi32    @ptr
        ref i64 pi64    @ptr
        ref r32 pr32    @ptr
        pcl newpc       @ptr
        u64 newpci      @ptr

    int     b
        real    y   @b
        real32  sy  @b
        word    v   @b

    ref byte ptrb
    int n
    psymbol d

!STACK VERSION
!   [stacksize]int      stack
!       [stacksize]real     xstack @stack
!       [stacksize]word     ustack @stack
!       [stacksize]ref void pstack @stack
!
    [callstacksize]u32 callstack
    [callstacksize]psymbol callstackst
    int callsp:=0
    [256]char str

    INT MAG, OLDSP

    macro zz = sp
    macro yy = sp-1
    macro xx = sp-2
    macro ww = sp-3

    stack:=pcm_alloc(stacksize*int.bytes)

    if cmain then
        int ncmd:=ncmdparams
        ref[0:]ichar cmd := cmdparams
        ncmd:=ncmd-pcmdskip
        cmd:=cast(ref byte(cmd)+pcmdskip*8)
        pstack[++sp]:=cmd
        stack[++sp]:=ncmd+1
        stack[++sp]:=0
    fi

!   doswitchu getopcode
    doswitch getopcode

    when knop      then
        steppc

    when kload     then
        if pc.opndtype=int_opnd then
            stack[++sp]:=pc.value
        else
            stack[++sp]:=pci_getopnd(pc, &stack[fp])
        fi
        steppc

    when kiload    then
        stack[sp]:=pci_loadptr(pstack[sp], getmode)
        steppc

    when kiloadx   then
        a:=stack[sp--]              !index
        ptr := ref byte(pstack[sp]) + a*getscale + getextra
        stack[sp]:=pci_loadptr(ptr, getmode)
        steppc

    when kstore    then
        case pc.opndtype
        when mem_opnd then

            d:=pc.def
            if d.id=static_id then
                pi64:=d.staddr
            elsif getmode=tpblock and d.id=param_id then
                pi64:=pstack[fp+d.offset]
            else
                pi64:=&stack[fp+d.offset]
            fi
            pci_storeptr(ptr, stack[sp--], getmode, getsize)
        else
            pcusopnd(pc)
        esac
        
        steppc

    when kistore   then
        ptr:=pstack[sp--]
        pci_storeptr(ptr, stack[sp--], getmode, getsize)
        steppc

    when kistorex  then
        a:=stack[sp--]              !index
        ptr := ref byte(pstack[sp--]) + a*getscale + getextra
        pci_storeptr(ptr, stack[sp--], getmode, getsize)
        steppc

    when kdupl, kdouble   then
        a:=stack[sp]
        stack[++sp]:=a
        steppc

    when kswapstk  then
        swap(stack[sp-(pc.x-1)], stack[sp-(pc.y-1)])
        steppc

    when kunload   then
        --sp
        steppc

    when kopnd     then
        unimpl
        steppc

    when ktype     then
        unimpl
        steppc

    when kloadbit  then ! Z' := Y.[Z]
        stack[sp-1]:=stack[sp-1].[stack[sp]]
        --sp
        steppc

    when kloadbf   then ! Z' := X.[Y..Z]
        a:=pci_loadbf(stack[xx], stack[yy], stack[zz])
        sp-:=2
        stack[sp]:=a
        steppc

    when kstorebit then ! Y^.[Z] := X
        ptr:=pstack[yy]
        a:=pci_loadptr(ptr, getmode)
        if stack[xx] then           !set bit
            a.[stack[zz]]:=1
        else
            a.[stack[zz]]:=0
        fi

        pci_storeptr(ptr, a, getmode)
        sp-:=3
        steppc

    when kstorebf  then ! X^.[Y..Z] := W
        ptr:=pstack[xx]
        a:=pci_loadptr(ptr, getmode)
        a:=pci_storebf(a, stack[yy], stack[zz], stack[ww])
        pci_storeptr(ptr, a, getmode)
        sp-:=4
        steppc

    when kcallp, kcallf    then
        d:=pc.def

        if dostackcheck then
            if sp>(stacksize-100) then pcerror("Stack overflow") fi
        fi

        if d.imported then
            n:=getnargs
            sp-:=n-1                !point to first arg
            a:=docalldll(d, nil, cast(&stack[sp]), n, getnvars, getmode)
            if pc.opcode=kcallp then
                --sp
            else
                stack[sp]:=a
            fi
            steppc
        elsif not d.pcaddr then
            pcerror2("Proc not defined:",d.name)
        else
            if dostackcheck then
                callstack[++callsp]:=sp-getnargs+(getopcode=kcallf|1|0)
                callstackst[callsp]:=d
            fi

            stack[++sp]:=int(pc+1) ior getnargs
            pc:=d.pcaddr

        fi

    when kretproc  then

        if dotrace then
            fprintln "# Return: # #", ++seqno, callstackst[callsp].name, getlineno(pc)
        fi

        n:=getnparams
        sp-:=getnlocals
        fp:=stack[sp--]

        if dostackcheck then
            newpc:=pstack[sp--]
            sp-:=newpci iand pcmask
            if callsp<1 then pcerror("retp/call underflow") fi
            oldsp:=callstack[callsp--]
            if sp<>oldsp then
                fprint @str,"RETP/SP mismatch: old=# curr=# ",oldsp, sp
                pcerror(str)
            fi

            pci:=newpci iand inot pcmask

        else
            pc:=pstack[sp--]
            sp-:=pci iand pcmask
            pci iand:=inot pcmask

        fi

    when kretfn    then

        if dotrace then
            fprintln "# Return: # #", ++seqno, callstackst[callsp].name, getlineno(pc)
        fi

        a:=stack[sp]
        n:=getnparams
        sp-:=getnlocals
        fp:=stack[--sp]

        if dostackcheck then
            newpc:=cast(stack[--sp])
            sp-:=newpci iand pcmask
            stack[sp]:=a

            if callsp<1 then pcerror("ref/call underflow") fi
            oldsp:=callstack[callsp--]
            if sp<>oldsp then
                fprint @str,"RETF/SP mismatch: old=# curr=# ", oldsp, sp
                pcerror(str)
            fi
            pci:=newpci iand inot pcmask
        else
            pc:=cast(stack[--sp])
            sp-:=pci iand pcmask
            stack[sp]:=a
            pci iand:=inot pcmask
        fi

    when kicallp   then
        ptr:=pstack[sp]

        if newpc>=pcstart and newpc<=pccurr then        !assume local
icallp:
            if dostackcheck then
                callstack[++callsp]:=SP-1-GETNARGS+(GETOPCODE=KICALLF|1|0)
                CALLSTACKST[CALLSP]:=&EMPTYST
            fi
            stack[sp]:=int(pc+1) ior getnargs
            pc:=newpc
        else    
            n:=getnargs
            --sp
            sp-:=n-1
            docalldll(nil, cast(ptr), cast(&stack[sp]), n, getnvars, getmode)
            --sp
            steppc
        fi

    when kicallf   then
        ptr:=pstack[sp]

        if newpc>=pcstart and newpc<=pccurr then        !assume local
            goto icallp
        else    
            n:=getnargs
            --sp
            sp-:=n-1
            a:=docalldll(nil, cast(ptr), cast(&stack[sp]), n, getnvars, getmode)
            stack[sp]:=a
            steppc
        fi

    when kjump     then
        pc:=getlabel

    when kijump    then
        unimpl
        steppc

    when kjumpcc   then
        if ispfloat(getmode) then
            n:=cmpreal(getcond, xstack[sp-1], xstack[sp])
        elsif psigned[getmode] then
            n:=cmpint(getcond, stack[sp-1], stack[sp])
        else
            n:=cmpword(getcond, stack[sp-1], stack[sp])
        fi

        if pc.popone and not n then
            --sp
        else
            sp-:=2
        fi

        if n then
            pc:=getlabel
        else
            steppc
        fi

    when kjumpt    then
        if stack[sp--] then             !ignore possibility of -0.0
            pc:=getlabel
        else
            steppc
        fi

    when kjumpf    then
        if stack[sp--]=0 then
            pc:=getlabel
        else
            steppc
        fi

    when kjumpret  then
        pc:=getlabel

    when kjumpretm then
        unimpl
        steppc

    when ksetcc    then
        case getmode
        when tpr64 then
            pcerror("setcc/r64")
        when tpr32 then
            pcerror("setcc/r32")
        elsif psigned[getmode] then
            n:=cmpint(getcond, stack[sp-1], stack[sp])
        else
            n:=cmpword(getcond, stack[sp-1], stack[sp])
        esac
        --sp
        stack[sp]:=n

        steppc

    when kstop     then
        return stack[sp--]

    when kto       then
        pi64:=pci_getopndaddr(pc+1, &stack[fp]) 
        --(pi64^)
        if pi64^ then
            pc:=getlabel
        else
            pc+:=2
        fi

    when kforup    then
        ptr:=cast(pci_getopndaddr(pc+1, &stack[fp]))
        n:=pci_getopnd(pc+2, &stack[fp])
        pi64^+:=pc.stepx
        if pi64^ <= n then
            pc:=getlabel
        else
            pc+:=3
        fi

    when kfordown  then
        ptr:=cast(pci_getopndaddr(pc+1, &stack[fp]))
        n:=pci_getopnd(pc+2, &stack[fp])
        pi64^-:=pc.stepx
        if pi64^ >= n then
            pc:=getlabel
        else
            pc+:=3
        fi


    when kiswap    then
        if getmode=tpblock then pcerror("swap/block") fi
        ptr:=pstack[sp--]
        ptrb:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=pci_loadptr(ptrb, getmode)
        pci_storeptr(ptr, b, getmode)
        pci_storeptr(ptrb, a, getmode)
        steppc

    when kswitch, kswitchu   then
        a:=stack[sp--]
        if a in getswmin..getswmax then
            pc:=getlabel+1+a-getswmin               !get to index swlabel op
            pc:=getlabel                    !get dest label from there
        else
            pc:=getlabel2
        fi

    when kswlabel  then
        unimpl
        steppc

    when kendsw    then
        unimpl
        steppc

    when kclear    then
        memset(pstack[sp--], 0, pc.size)
        steppc

    when kassem    then
        unimpl
        steppc

    when kadd      then
        if ispfloat(getmode) then
            xstack[sp-1]+:=xstack[sp]
        else
            stack[sp-1]+:=stack[sp]
        fi
        --sp

        steppc

    when ksub      then
        if ispfloat(getmode) then
            xstack[sp-1]-:=xstack[sp]
        else
            stack[sp-1]-:=stack[sp]
        fi
        --sp

        steppc

    when kmul      then
        if ispfloat(getmode) then
            xstack[sp-1]*:=xstack[sp]
        else
            stack[sp-1]*:=stack[sp]
        fi
        --sp

        steppc

    when kdiv      then
        xstack[sp-1]:=xstack[sp-1]/xstack[sp]
        --sp
        steppc

    when kidiv     then
        if psigned[getmode] then
            stack[sp-1]:=stack[sp-1]/stack[sp]
        else
            ustack[sp-1]:=ustack[sp-1]/ustack[sp]
        fi
        --sp
        steppc

    when kirem     then
        if psigned[getmode] then
            stack[sp-1]:=stack[sp-1] rem stack[sp]
        else
            ustack[sp-1]:=ustack[sp-1] rem ustack[sp]
        fi
        --sp
        steppc

    when kidivrem  then
        unimpl
        steppc

    when kbitand   then
        stack[sp-1] iand:=stack[sp]
        --sp
        steppc

    when kbitor    then
        stack[sp-1] ior:=stack[sp]
        --sp
        steppc

    when kbitxor   then
        stack[sp-1] ixor:=stack[sp]
        --sp
        steppc

    when kshl      then
        ustack[sp-1] <<:= ustack[sp]
        --sp
        steppc

    when kshr      then
        if psigned[getmode] then
            stack[sp-1] >>:= stack[sp]
        else
            ustack[sp-1] >>:= ustack[sp]
        fi  
        --sp
        steppc

    when kmin      then
        if ispfloat(getmode) then
            xstack[sp-1] min:= xstack[sp]
        elsif psigned[getmode] then
            stack[sp-1] min:= stack[sp]
        else
            ustack[sp-1] min:= ustack[sp]
        fi
        --sp
        steppc

    when kmax      then
        if ispfloat(getmode) then
            xstack[sp-1] max:= xstack[sp]
        elsif psigned[getmode] then
            stack[sp-1] max:= stack[sp]
        else
            ustack[sp-1] max:= ustack[sp]
        fi
        --sp
        steppc

    when kaddpx    then
        a:=stack[sp--]              !index
        pstack[sp] := ref byte(pstack[sp]) + a*getscale + getextra
        steppc

    when ksubpx    then
        a:=stack[sp--]              !index
        pstack[sp] := ref byte(pstack[sp]) - a*getscale + getextra
        steppc

    when ksubp     then
        stack[sp-1]:=(stack[sp-1]-stack[sp])/getscale
        --sp
        steppc

    when kneg      then
        if ispfloat(getmode) then
            xstack[sp] := -xstack[sp]
        else
            stack[sp] := -stack[sp]
        fi
        steppc

    when kabs      then
        if ispfloat(getmode) then
            xstack[sp] := abs xstack[sp]
        else
            stack[sp] :=  abs stack[sp]
        fi
        steppc

    when kbitnot   then
        ustack[sp]:=inot ustack[sp]
        steppc

    when knot      then
        stack[sp] := stack[sp] ixor 1
        steppc

    when ktoboolt  then
        stack[sp]:=istrue stack[sp]
        steppc

    when ktoboolf  then
        stack[sp]:=not stack[sp]
        steppc

    when ksqr      then
        if ispfloat(getmode) then
            xstack[sp]*:=xstack[sp]
        else
            stack[sp]:=sqr stack[sp]
        fi

        steppc

    when ksqrt     then
        xstack[sp]:=sqrt(xstack[sp])
        steppc

    when ksin      then
        unimpl
        steppc

    when kcos      then
        unimpl
        steppc

    when ktan      then
        unimpl
        steppc

    when kasin     then
        unimpl
        steppc

    when kacos     then
        unimpl
        steppc

    when katan     then
        unimpl
        steppc

    when klog      then
        unimpl
        steppc

    when klog10    then
        unimpl
        steppc

    when kexp      then
        unimpl
        steppc

    when kround    then
        unimpl
        steppc

    when kfloor    then
        unimpl
        steppc

    when kceil     then
        unimpl
        steppc

    when ksign     then
        unimpl
        steppc

    when katan2    then
        unimpl
        steppc

    when kpower    then
        if ispfloat(getmode) then
            xstack[sp-1]:=xstack[sp-1]**xstack[sp]
        else
            stack[sp-1]:=stack[sp-1]**stack[sp]
        fi
        --sp

        steppc

    when kfmod     then
        unimpl
        steppc

    when kincrto   then
        doincr(pstack[sp--], getincr, getmode)
        steppc

    when kdecrto   then
        doincr(pstack[sp--], -getincr, getmode)
        steppc

    when kincrload then
        ptr:=pstack[sp]
        doincr(ptr, getincr, getmode)
        stack[sp]:=pci_loadptr(ptr, getmode)
        steppc

    when kdecrload then
        ptr:=pstack[sp]
        doincr(ptr, -getincr, getmode)
        stack[sp]:=pci_loadptr(ptr, getmode)
        steppc


    when kloadincr then
        ptr:=pstack[sp]
        stack[sp]:=pci_loadptr(ptr, getmode)
        doincr(ptr, getincr, getmode)
        steppc

    when kloaddecr then
        ptr:=pstack[sp]
        stack[sp]:=pci_loadptr(ptr, getmode)
        doincr(ptr, -getincr, getmode)
        steppc

    when kaddto    then     !Z^ +:= Y
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]

        if ispfloat(getmode) then
            a:=int@(real@(a)+real@(b))
        else
            a+:=b
        fi

        pci_storeptr(ptr, a, getmode)
        steppc

    when ksubto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]

        if ispfloat(getmode) then
            a:=int@(real@(a)-real@(b))
        else
            a-:=b
        fi

        pci_storeptr(ptr, a, getmode)
        steppc

    when kmulto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]

        if ispfloat(getmode) then
            a:=int@(real@(a)*real@(b))
        else
            a*:=b
        fi

        pci_storeptr(ptr, a, getmode)
        steppc

    when kdivto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]

        if ispfloat(getmode) then
            a:=int@(real@(a)/real@(b))
        else
            a:=a/b
        fi

        pci_storeptr(ptr, a, getmode)
        steppc

    when kidivto   then
        unimpl
        steppc

    when kiremto   then
        unimpl
        steppc

    when kbitandto then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]
        a iand:=b
        pci_storeptr(ptr, a, getmode)
        steppc

    when kbitorto  then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]
        a ior:=b
        pci_storeptr(ptr, a, getmode)
        steppc

    when kbitxorto then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]
        a ixor:=b
        pci_storeptr(ptr, a, getmode)
        steppc

    when kshlto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]
        a<<:=b
        pci_storeptr(ptr, a, getmode)
        steppc

    when kshrto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]
        if psigned[getmode] then
            a>>:=b
        else
            u>>:=v
        fi
        pci_storeptr(ptr, a, getmode)
        steppc

    when kminto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]

        if isfloat then
            a:=int@(min(real@(a),real@(b)))
        elsif issigned then
            a min:=b
        else
            u min:=v
        fi

        pci_storeptr(ptr, a, getmode)
        steppc


    when kmaxto    then
        ptr:=pstack[sp--]
        a:=pci_loadptr(ptr, getmode)
        b:=stack[sp--]

        if isfloat then
            a:=int@(max(real@(a),real@(b)))
        elsif issigned then
            a max:=b
        else
            u max:=v
        fi

        pci_storeptr(ptr, a, getmode)
        steppc

    when kaddpxto  then
        pu64:=pstack[sp--]
        pu64^+:=stack[sp--]*getscale
        steppc

    when ksubpxto  then
        pu64:=pstack[sp--]
        pu64^-:=stack[sp--]*getscale
        steppc

    when knegto    then
        unimpl
        steppc

    when kabsto    then
        unimpl
        steppc

    when kbitnotto then
        unimpl
        steppc

    when knotto    then
        unimpl
        steppc

    when ktoboolto then
        unimpl
        steppc

    when ktypepun  then
        steppc

    when kfloat    then
        if psigned[getmode2] then
            xstack[sp]:=stack[sp]
        else
            xstack[sp]:=ustack[sp]
        fi
        steppc

    when kfix      then
        stack[sp]:=xstack[sp]
        steppc

    when ktruncate then
        stack[sp]:=pci_loadptr(cast(&stack[sp]), getmode2)
        steppc

    when kwiden    then
        stack[sp]:=pci_loadptr(cast(&stack[sp]), getmode2)
        steppc

    when kfwiden   then             !no-op: value already widened
        steppc

    when kfnarrow  then             !will be narrowed on store
        steppc

    when kstartmx  then
        steppc

    when kresetmx  then
        steppc

    when kendmx    then
        steppc

    when kproc  then                    !proc entry code
        stack[++sp]:=fp
        fp:=sp

        sp+:=getnlocals
        steppc

    when ktcproc   then
        unimpl
        steppc

    when kendproc  then
        unimpl
        steppc

    when kistatic  then
        unimpl
        steppc

    when kzstatic  then
        unimpl
        steppc

    when kdata     then
        unimpl
        steppc

    when klabel    then
        steppc

    when klabeldef then
        steppc

    when ksetjmp   then
        ptr:=pstack[sp]
        pu64^:=cast(pc+1)               !label of next instr
        (pu64+1)^:=sp
        (pu64+2)^:=fp

        stack[sp]:=0

        steppc

    when klongjmp  then
        a:=stack[sp--]                  !ret value
        ptr:=pstack[sp--]
        pc:=cast(pi64^)
        sp:=(pi64+1)^
        fp:=(pi64+2)^
        stack[++sp]:=a

    when ksetcall  then
        steppc

    when ksetarg   then
        steppc

    when kloadall  then
        unimpl
        steppc

    when kstoresl  then
        unimpl
        steppc

    when kstoresld then
        unimpl
        steppc

    when ksliceupb then
        unimpl
        steppc

    when kslicelen then
        unimpl
        steppc

    when ksliceptr then
        unimpl
        steppc

    when keval     then
        --sp
        steppc

    when kcomment  then
        steppc

    when kendprog  then
        unimpl
        steppc

    else
unimpl:
        println
        fprintln "Unimpl: # at seq: #", pclnames[getopcode], getseqno
        println
        stop 1
    end doswitch
    0
end

proc fixuppcl=
!allocate memory for statics
    pcl p
    psymbol d,e, dproc
    ref byte pdata
    int parambytes, framebytes
    int paramslots, localslots
    u64 a

    labeltable:=pcm_alloc((mlabelno+1)*pcl.bytes)

!do static fixups in two passes, as sometimes the ordering gets mixed up
!first pass allocates space, second deals with initialisation that may include
!references to static data declared later

    p:=pcstart
    while p<=pccurr, ++p do
        case p.opcode
        when kproc, ktcproc then
            p.def.pcaddr:=p
        esac
    od

    p:=pcstart
    while p<=pccurr, ++p do
        case p.opcode
        when kistatic, kzstatic then
            d:=p.def
            d.staddr:=pcm_allocz(p.size)
        esac
    od

    p:=pcstart
    while p<=pccurr, ++p do
        case p.opcode
        when kistatic, kzstatic then
            pdata:=p.def.staddr

        when kdata then
            if p.mode<>tpblock then
                case p.opndtype
                when mem_opnd then
                    PCERRORX(P,"FIX/DATA/MEM")
                when memaddr_opnd then
                    d:=p.def
                    case d.id
                    when static_id then
                        a:=cast(d.staddr)
                    when proc_id then
                        a:=cast(d.pcaddr)
                    when import_id then
                        a:=cast(getdllfnptr(d))
                    else
                        pcerrorx(p,"data &mem")
                    esac
                    if a=0 then
                        pcerrorx(p,"data &mem = nil")
                    fi
                else
                    a:=p.value
                esac

                memcpy(pdata, &a, p.size)
            else
                memcpy(pdata, p.svalue, p.size)
            fi
            pdata+:=p.size

        when kproc then
            dproc:=d:=p.def
            e:=d.nextparam
            parambytes:=0
            while e, e:=e.nextparam do
                parambytes+:=8
                e.offset:=-(parambytes/8+1)
            od

            e:=d.nextlocal
            framebytes:=0
            while e, e:=e.nextlocal do
                e.offset:=framebytes/8+1
                framebytes+:=roundtoblock(E.size,8)
            od

            p.paramslots:=paramslots:=parambytes/8
            p.localslots:=localslots:=framebytes/8

        when klabel then
            labeltable[p.labelno]:=p

        when kretproc, kretfn then
            p.paramslots:=paramslots
            p.localslots:=localslots

        esac
    od

end

func getlineno(pcl pc)int=
    ichar filename, sourceline

    if igetmsourceinfo then
        igetmsourceinfo(pc.pos, &filename, &sourceline)
    else
        0
    fi
end
