!ASM bytecode handlers

!int showasmflag=1
int showasmflag=2
!int showasmflag=0

const kopnda    = 8
const kopndb    = 16
const kopndc    = 24
const kopndd    = 32

const ktag          = varrec.tag
const khasref       = varrec.hasref
const kusertag      = varrec.usertag
const krefelemtag   = refrec.elemtag
const kvarptr       = varrec.varptr
const kobjptr       = varrec.objptr
const kptr          = refrec.ptr
const kvalue        = varrec.value
const kxvalue       = varrec.xvalue
const kretaddr      = returnrec.retaddr
const kframeptr_low = returnrec.frameptr_low
const krange_low    = varrec.range_lower
const krange_high   = varrec.range_upper

const jrefcount     = objrec.refcount
const jmutable      = objrec.flags          !byte flags; bit 0=mutable flag
const jvarptr       = listobjrec.varptr
const jlength32     = listobjrec.length
const jlower32      = listobjrec.lower
const jobjptr2      = listobjrec.objptr2

const gdef          = genfieldrec.def
const gnextdef      = genfieldrec.nextdef
const sowner        = strec.owner
const smode         = strec.mode
const snameid       = strec.nameid
const soffset       = strec.fieldoffset

const varshift      = 4

!offsets from the top of the stack, of x, x/y, or x/y/z operands
!the x operand is first pushed, so has a variable offset
!the a suffix means top of stack; b is next; c is last

const xa        = 0             ! one operand x

const xb        = -varsize      ! two operands x y
const ya        = 0

const xc        = -varsize*2    ! three operands x y z
const yb        = -varsize
const za        = 0


const intpsize  = 8

const intpsize2 = intpsize*2
const intpsize4 = intpsize*4
const intpsize6 = intpsize*6

macro jumpnext   =
    assem
        jmp [Dprog]

!       mov D0,[Dprog]
!       jmp D0
    end

!macro jumpnext   =
!   assem
!       *saveregs
!       call showasmcmd
!       *loadregs
!
!       jmp [Dprog]
!   end
!
macro saveregs   =
    assem
        mov [pcptr],Dprog
        mov [sptr],Dsptr
        mov [frameptr],Dframe
    end

!macro saveregs   =
!   assem
!       mov [pcptr],Dprog
!       mov [sptr],Dsptr
!       mov [frameptr],Dframe
!!      call showasmcmd
!   end

macro loadregs   =
    assem
        mov Dprog,[pcptr]
        mov Dsptr,[sptr]
        mov Dframe,[frameptr]
    end

macro pushvar    = asm add Dsptr, varsize

macro popvar     = asm sub Dsptr, varsize

macro pushvar2   = asm add Dsptr, varsize+varsize
macro popvar2    = asm sub Dsptr, varsize+varsize

macro pushvar3   = asm add Dsptr, varsize+varsize+varsize
macro popvar3    = asm sub Dsptr, varsize+varsize+varsize

macro jumpskip1  =
    assem
        add Dprog,8
        *jumpnext
    end

macro jumpskip2  =
    assem
        add Dprog,16
        *jumpnext
    end

macro jumpskip3  =
    assem
        add Dprog,24
        *jumpnext
    end

macro jumpskip4  =
    assem
        add Dprog,32
        *jumpnext
    end

macro jumpskip5  =
    assem
        add Dprog,40
        *jumpnext
    end

macro jumpskip6  =
    assem
        add Dprog,48
        *jumpnext
    end

macro loadskip1  =
    assem
        mov Dsptr,[sptr]
        mov Dframe,[frameptr]
        mov Dprog,[pcptr]
        add Dprog,8
        *jumpnext
    end

macro loadskip2  =
    assem
        mov Dsptr,[sptr]
        mov Dframe,[frameptr]
        mov Dprog,[pcptr]
        add Dprog,16
        *jumpnext
    end

macro callunshareu_d4    =
    assem
        mov D10,D4
        *saveregs
        call var_unshareu
        *loadregs
    end

macro callunshareu_dsptr =
    assem
        mov D10, Dsptr
        *saveregs
        call var_unshareu
        *loadregs
    end

macro callunshareu =
    assem
        *saveregs
        call var_unshareu
        *loadregs
    end

macro callduplu_dsptr =
    assem
        mov D10, Dsptr
        *saveregs
        call var_duplu
        *loadregs
    end

macro callvarfree =
    assem
        *saveregs
        call var_free
        *loadregs
    end

global var [0..pclnames.upb]ref proc jhandlertable

proc showasmcmd=
    if showasmflag<2 then return fi
    println "showasmcmd",showasmflag,strpcptr(), pcptr, sptr, ttname[sptr.tag]
end

global proc initjhandlers=
    ichar name
    static int handlersdone=0

    if handlersdone then return fi

    for i to $get_nprocs() do
        name:=$get_procname(i)
        if eqbytes(name,"j_",2) then
            for k:=0 to pclnames.upb do
                if eqstring(name+2,pclnames[k]+1) then      !skip "j_" and "k"
                    jhandlertable[k]:=$get_procaddr(i)
                    exit
                fi
            else
                pcerror_s("Unknown j-handler",name)
            od
        fi
    od

    for i in jhandlertable.bounds when jhandlertable[i]=nil do
        jhandlertable[i]:=cast(junimpl)
    od

    handlersdone:=1
end

global function asmavailable:int= {1}

function strpcptr:ichar=
    for i in jhandlertable.bounds do
        if jhandlertable[i]=cast(pcptr^,ref proc) then
            return pclnames[i]
        fi
    od
    return "<not found>"
end

global function disploop_asm:ref int =
    disploop()
    return nil
end

threadedproc disploop=
!Note: this is reentrant

    assem
        push D9
        push D8
        push D7
        push D6

        push D5
        push D4
        push D3
        push Dframe

        sub dstack,40

        *loadregs
        *jumpnext
    end

stoplabel::

    assem
        add dstack, 40

        pop Dframe
        pop D3
        pop D4
        pop D5

        pop D6
        pop D7
        pop D8
        pop D9
    end
end

threadedproc junimpl=
    pcerror("-asm Unimplemented (use -debug to see opcode)")
end

threadedproc j_comment=
    saveregs
    skip(1)
    loadregs
    jumpnext
end

threadedproc j_nop=
    saveregs
    k_nop()
    loadregs
    jumpnext
end

threadedproc j_procentry=
    assem
        mov A3,[Dprog+kopnda]
loop1:
        *pushvar
        mov word32 [Dsptr+ktag],tvoid
!       mov word64 [Dsptr+kvalue],0
        dec A3
        jnz loop1
        *jumpskip2
    end

    saveregs
    k_procentry()
    loadregs
    jumpnext
end

threadedproc j_pushm=
    assem
        mov D4,[Dprog+kopnda]
        *pushvar
        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
    L2:
        *jumpskip2
    end
    saveregs
    k_pushm()
    loadregs
    jumpnext
end

threadedproc j_pushf=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        *pushvar
        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
    L2:
        *jumpskip2
    end

    saveregs
    k_pushf()
    loadregs
    jumpnext
end

threadedproc j_pushmref=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],trefvar
        mov D4,[Dprog+kopnda]
        mov [Dsptr+kvarptr],D4
        *jumpskip2
    end
    saveregs
    k_pushmref()
    loadregs
    jumpnext
end

threadedproc j_pushfref=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],trefvar
        mov D4,[Dprog+kopnda]
        lea D0,[D4+Dframe]
        mov [Dsptr+kvarptr],D0
        *jumpskip2
    end

    saveregs
    k_pushfref()
    loadregs
    jumpnext
end

threadedproc j_popm=
    assem
        mov D4,[Dprog+kopnda]

        cmp byte [D4+khasref],1
        jnz L2

        *callunshareu_d4
        mov D4,[Dprog+kopnda]

L2:
        mov D0,[Dsptr+ktag]
        mov [D4+ktag],D0
        mov D1,[Dsptr+kvalue]
        mov [D4+kvalue],D1

        *popvar
        *jumpskip2
    end

    saveregs
    k_popm()
    loadregs
    jumpnext
end

threadedproc j_storem=
    assem
        cmp byte [Dsptr+khasref],1
        jnz L1
        mov D0,[Dsptr+kobjptr]
        inc word32 [D0+jrefcount]
L1:
        mov D4,[Dprog+kopnda]

        cmp byte [D4+khasref],1
        jnz L2

        *callunshareu_d4
        mov D4,[Dprog+kopnda]

L2:
        mov D0,[Dsptr+ktag]
        mov [D4+ktag],D0
        mov D1,[Dsptr+kvalue]
        mov [D4+kvalue],D1

        *jumpskip2
    end

    saveregs
    k_storem()
    loadregs
    jumpnext
end

threadedproc j_popf=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe

        cmp byte [D4+khasref],1
        jnz L2
        *callunshareu_d4

L2:
        mov D0,[Dsptr+ktag]
        mov [D4+ktag],D0
        mov D1,[Dsptr+kvalue]
        mov [D4+kvalue],D1

        *popvar
        *jumpskip2
    end

    saveregs
    k_popf()
    loadregs
    jumpnext
end

threadedproc j_storef=
    assem
        cmp byte [Dsptr+khasref],1
        jnz L1
        mov D0,[Dsptr+kobjptr]
        inc word32 [D0+jrefcount]
L1:
        mov D4,[Dprog+kopnda]
        add D4,Dframe

        cmp byte [D4+khasref],1
        jnz L2
        *callunshareu_d4

L2:
        mov D0,[Dsptr+ktag]
        mov [D4+ktag],D0
        mov D1,[Dsptr+kvalue]
        mov [D4+kvalue],D1

        *jumpskip2
    end

    saveregs
    k_storef()
    loadregs
    jumpnext
end

threadedproc j_pushci=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],tint
        mov D0,[Dprog+kopnda]
        mov [Dsptr+kvalue],D0
        *jumpskip2
    end

    saveregs
    k_pushci()
    loadregs
    jumpnext
end

threadedproc j_pushcu=
    saveregs
    k_pushcu()
    loadregs
    jumpnext
end

threadedproc j_pushvoid=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],tvoid
        *jumpskip1
    end

    saveregs
    k_pushvoid()
    loadregs
    jumpnext
end

threadedproc j_pushnil=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],trefpack
        mov word32 [Dsptr+krefelemtag],tpvoid
        mov word64 [Dsptr+kptr],0
        *jumpskip1
    end

    saveregs
    k_pushnil()
    loadregs
    jumpnext
end

threadedproc j_pushcr=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],treal
        mov D0,[Dprog+kopnda]
        mov [Dsptr+kvalue],D0
        *jumpskip2
    end

    saveregs
    k_pushcr()
    loadregs
    jumpnext
end

threadedproc j_pushcs=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],tstring ior hasrefmask
        mov D0,[Dprog+kopnda]
        mov [Dsptr+kobjptr],D0
        inc word32 [D0+jrefcount]
        *jumpskip2
    end

    saveregs
    k_pushcs()
    loadregs
    jumpnext
end

threadedproc j_pusht=
    saveregs
    k_pusht()
    loadregs
    jumpnext
end

threadedproc j_pushsymbol=
    saveregs
    k_pushsymbol()
    loadregs
    jumpnext
end

threadedproc j_pushptr=
    assem
        cmp byte [Dsptr+ktag],trefvar
        jnz L1
        mov D4,[Dsptr+kvarptr]

        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1
        and A0,hasrefmask
        jz L12
        inc word32 [D1+jrefcount]
L12:
        *jumpskip1

L1:
        cmp byte [Dsptr+ktag],trefpack
        jnz L2
        mov D4,[Dsptr+kptr]
        movzx A0,word16 [Dsptr+krefelemtag]
        cmp A0,tpi32
        jnz L10
        mov word32 [Dsptr+ktag],tint
        mov A0,[D4]
        movsxd D0,A0
        mov [Dsptr+kvalue],D0
        *jumpskip1
L10:
        cmp A0,tpu8
        jnz L11
        mov word32 [Dsptr+ktag],tint
        movzx A0,byte [D4]
        mov [Dsptr+kvalue],D0
        *jumpskip1
L11:

L2:
L99:
    end
    saveregs
    k_pushptr()
    loadregs
    jumpnext
end

threadedproc j_popptr=
    saveregs
    k_popptr()
    loadregs
    jumpnext
end

threadedproc j_zpopm=
    saveregs
    k_zpopm()
    loadregs
    jumpnext
end

threadedproc j_zpopf=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        mov D0,[Dsptr+ktag]
        mov [D4+ktag],D0
        mov D1,[Dsptr+kvalue]
        mov [D4+kvalue],D1

        *popvar
        *jumpskip2
    end

    saveregs
    k_zpopf()
    loadregs
    jumpnext
end

threadedproc j_dupl=
    assem
        *pushvar
        mov D0,[Dsptr+xb]           !D0 = tag etc
        mov [Dsptr+ya],D0
        mov D1,[Dsptr+xb+kvalue]    !D1 = value/objptr etc
        mov [Dsptr+ya+kvalue],D1

        and A0,hasrefmask
        jz L1
        inc word32 [D1+jrefcount]
L1:

        *jumpskip1
    end

    saveregs
    k_dupl()
    loadregs
    jumpnext
end

threadedproc j_copy=
    static varrec x

    assem
        cmp byte [dsptr+khasref],1
        jnz L1

        mov D0,[Dsptr]
        mov [x],D0
        mov D0,[Dsptr+kvalue]
        mov [x+kvalue],D0

        *callduplu_dsptr
        lea D10,[x]
        *callunshareu

L1:     *jumpskip1
    end

    saveregs
    k_copy()
    loadregs
    jumpnext
end

threadedproc j_swap=
    saveregs
    k_swap()
    loadregs
    jumpnext
end

threadedproc j_convrefpack=
    saveregs
    k_convrefpack()
    loadregs
    jumpnext
end

threadedproc j_jump=
    assem
        mov Dprog,[Dprog+kopnda]
        *jumpnext
    end

    saveregs
    k_jump()
    loadregs
    jumpnext
end

threadedproc j_jumpptr=
    saveregs
    k_jumpptr()
    loadregs
    jumpnext
end

threadedproc j_jumptrue=
    assem
        cmp byte [Dsptr+xa+ktag],tint
        jnz L1

        mov D0,[Dsptr+xa+kvalue]
        and D0,D0
        jz L2
        mov Dprog,[Dprog+kopnda]
        *popvar
        *jumpnext
L2:
        *popvar
        *jumpskip2
L1:
    end
    saveregs
    k_jumptrue()
    loadregs
    jumpnext
end

threadedproc j_jumpfalse=
    assem
        cmp byte [Dsptr+xa+ktag],tint
        jnz L1

        mov D0,[Dsptr+xa+kvalue]
        and D0,D0
        jnz L2
        mov Dprog,[Dprog+kopnda]
        *popvar
        *jumpnext
L2:
        *popvar
        *jumpskip2
L1:
    end

    saveregs
    k_jumpfalse()
    loadregs
    jumpnext
end

threadedproc j_jumpeq=
    assem
        mov B0,[Dsptr+xb+ktag]
        mov B1,[Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        cmp D0,[Dsptr+ya+kvalue]
        jz Ltrue
        *popvar2
        *jumpskip2

Ltrue:
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L1:
        cmp B0,treal
        jnz L3

        movq XMM0,[Dsptr+xb+kvalue]
        comisd XMM0,[Dsptr+ya+kvalue]
        jz Ltrue
        *popvar2
        *jumpskip2

L3:
L99:
    end

    saveregs
    k_jumpeq()
    loadregs
    jumpnext
end

threadedproc j_jumpne=
    assem
        mov B0,[Dsptr+xb+ktag]
        mov B1,[Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        cmp D0,[Dsptr+ya+kvalue]
        jnz Ltrue
        *popvar2
        *jumpskip2

Ltrue:
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L1:
        cmp B0,treal
        jnz L3

        movq XMM0,[Dsptr+xb+kvalue]
        comisd XMM0,[Dsptr+ya+kvalue]
        jnz Ltrue
        *popvar2
        *jumpskip2

L3:
L99:
    end

    saveregs
    k_jumpne()
    loadregs
    jumpnext
end

threadedproc j_jumplt=
    assem
        mov B0,[Dsptr+xb+ktag]
        mov B1,[Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        cmp D0,[Dsptr+ya+kvalue]
        jl Ltrue
        *popvar2
        *jumpskip2

Ltrue:
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L1:
        cmp B0,treal
        jnz L3

        movq XMM0,[Dsptr+xb+kvalue]
        comisd XMM0,[Dsptr+ya+kvalue]
        jb Ltrue
        *popvar2
        *jumpskip2

L3:
L99:
    end

    saveregs
    k_jumplt()
    loadregs
    jumpnext
end

threadedproc j_jumple=
    assem
        mov B0,[Dsptr+xb+ktag]
        mov B1,[Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        cmp D0,[Dsptr+ya+kvalue]
        jle Ltrue
        *popvar2
        *jumpskip2

Ltrue:
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L1:
        cmp B0,treal
        jnz L3

        movq XMM0,[Dsptr+xb+kvalue]
        comisd XMM0,[Dsptr+ya+kvalue]
        jbe Ltrue
        *popvar2
        *jumpskip2

L3:
L99:
    end

    saveregs
    k_jumple()
    loadregs
    jumpnext
end

threadedproc j_jumpge=
    assem
        mov B0,[Dsptr+xb+ktag]
        mov B1,[Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        cmp D0,[Dsptr+ya+kvalue]
        jge Ltrue
        *popvar2
        *jumpskip2

Ltrue:
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L1:
        cmp B0,treal
        jnz L3

        movq XMM0,[Dsptr+xb+kvalue]
        comisd XMM0,[Dsptr+ya+kvalue]
        jae Ltrue
        *popvar2
        *jumpskip2

L3:
L99:
    end

    saveregs
    k_jumpge()
    loadregs
    jumpnext
end

threadedproc j_jumpgt=
    assem
        mov B0,[Dsptr+xb+ktag]
        mov B1,[Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        cmp D0,[Dsptr+ya+kvalue]
        jg Ltrue
        *popvar2
        *jumpskip2

Ltrue:
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L1:
        cmp B0,treal
        jnz L3

        movq XMM0,[Dsptr+xb+kvalue]
        comisd XMM0,[Dsptr+ya+kvalue]
        ja Ltrue
        *popvar2
        *jumpskip2

L3:
L99:
    end

    saveregs
    k_jumpgt()
    loadregs
    jumpnext
end

threadedproc j_jumptesteq=
    assem
        cmp word16 [Dsptr+ya+ktag],tint
        jnz L99
        cmp word16 [Dsptr+xb+ktag],tint
        jnz L99
        mov D0,[Dsptr+ya+kvalue]
        cmp D0,[Dsptr+xb+kvalue]
        jnz L2
!equal, so pop both and jump
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
!not equal: keep x on stack
L2:
        *popvar
        *jumpskip2

L99:
    end

    saveregs
    k_jumptesteq()
    loadregs
    jumpnext
end

threadedproc j_jumptestne=
    assem
        cmp word16 [Dsptr+ya+ktag],tint
        jnz L1
        cmp word16 [Dsptr+xb+ktag],tint
        jnz L1
        mov D0,[Dsptr+ya+kvalue]
        cmp D0,[Dsptr+xb+kvalue]
        jz L2
!not equal, so pop y and jump
        *popvar
        mov Dprog,[Dprog+kopnda]
        *jumpnext
L2:
        *popvar2
        *jumpskip2

L1:
    end

    saveregs
    k_jumptestne()
    loadregs
    jumpnext
end

threadedproc j_switch=
    assem
        cmp word16 [Dsptr+ktag],tint
        jnz L1              !get C deal with errors
        mov D4,[Dsptr+kvalue]       !switch index
        *popvar
        sub D4,[Dprog+kopndb]       !index-lower! now 0-based index
        cmp D4,[Dprog+kopnda]       !index0>=n?
        jae L2              !out of range
!in range
        shl D4,1
        mov Dprog,[Dprog+D4*8+intpsize4]
        *jumpnext
!out of range
L2:
        mov D5,[Dprog+kopnda]
        shl D5,1
        mov Dprog,[Dprog+D5*8+intpsize4]
        *jumpnext

L1:
    end

    saveregs
    k_switch()
    loadregs
    jumpnext
end

threadedproc j_tom=
    assem
        mov D4,[Dprog+kopnda]
        mov D5,[Dprog+kopndb]
        dec word64 [D5+kvalue]
        jz L1
        mov Dprog,D4
        *jumpnext
L1:
        *jumpskip3
    end

    saveregs
    k_tom()
    loadregs
    jumpnext
end

threadedproc j_tof=
    assem
        mov D4,[Dprog+kopnda]
        mov D5,[Dprog+kopndb]
        dec word64 [Dframe+D5+kvalue]
        jz L1
        mov Dprog,D4
        *jumpnext
L1:
        *jumpskip3
    end

    saveregs
    k_tof()
    loadregs
    jumpnext
end

threadedproc j_formci=
    saveregs
    k_formci()
    loadregs
    jumpnext
end

threadedproc j_forfci=
    assem
        mov D4,[Dprog+kopnda]       !label
        mov D0,[Dprog+kopndb]       !a
        inc word64 [Dframe+D0+kvalue]   !++a
        mov D0,[Dframe+D0+kvalue]
        cmp A0,[Dprog+kopndc]
        jg L1
        mov Dprog,D4
        *jumpnext
    L1:
        *jumpskip4
    end

    saveregs
    k_forfci()
    loadregs
    jumpnext
end

threadedproc j_formm=
    saveregs
    k_formm()
    loadregs
    jumpnext
end

threadedproc j_forff=
    assem
        mov D0,[Dprog+kopndb]       !b
        mov D5,[Dprog+kopndc]       !c
        mov D4,[Dprog+kopnda]       !label

        inc word64 [Dframe+D0+kvalue]
        mov D0,[Dframe+D0+kvalue]
        cmp D0,[Dframe+D5+kvalue]

        jg L1
        mov Dprog,D4
        *jumpnext
    L1:
        *jumpskip4
    end

    saveregs
    k_forff()
    loadregs
    jumpnext
end

threadedproc j_fordmci=
    saveregs
    k_fordmci()
    loadregs
    jumpnext
end

threadedproc j_fordfci=
    saveregs
    k_fordfci()
    loadregs
    jumpnext
end

threadedproc j_fordmm=
    saveregs
    k_fordmm()
    loadregs
    jumpnext
end

threadedproc j_fordff=
    saveregs
    k_fordff()
    loadregs
    jumpnext
end

threadedproc j_callproc=
    const countinterval=10
    static int count=countinterval

    assem
        dec word32 [count]
        jz L99

        *pushvar
        mov word32 [Dsptr+ktag],treturn
        lea D0,[Dprog+24]       ! return address
        mov [Dsptr+kretaddr],D0
        mov [Dsptr+kframeptr_low],Aframe
        mov Dframe,Dsptr
        mov [frameptr],Dframe
        mov Dprog,[Dprog+kopnda]
        *jumpnext

L99:
        mov word32 [count],countinterval
    end

    saveregs
    k_callproc()
    loadregs
    jumpnext
end

threadedproc j_callptr=
    saveregs
    k_callptr()
    loadregs
    jumpnext
end

threadedproc j_return0=
    assem
        mov Dprog,[Dsptr+kretaddr]
        mov Aframe,[Dsptr+kframeptr_low]
        *popvar
        *jumpnext
    end

    saveregs
    k_return0()
    loadregs
    jumpnext
end

threadedproc j_return=

    assem
        mov D5,[Dprog+kopnda]       !nargs

        mov Dprog,[Dsptr+kretaddr]
        mov Aframe,[Dsptr+kframeptr_low]
        *popvar

        and D5,D5
        jz L2               !no args
L1:     cmp byte [Dsptr+khasref],1
        jnz L11
        *callunshareu_dsptr

L11:    *popvar
        dec D5
        jnz L1

L2:
        *jumpnext
    end

    saveregs
    k_return()
    loadregs
    jumpnext
end

threadedproc j_popretval=
    assem
        mov D4,[Dprog+kopnda]
        mov D0,[Dsptr+ktag]
        mov [Dframe+D4+ktag],D0
        mov D1,[Dsptr+kvalue]
        mov [Dframe+D4+kvalue],D1
        *popvar
        *jumpskip2
    end

    saveregs
    k_popretval()
    loadregs
    jumpnext
end

threadedproc j_modulecall=
    saveregs
    k_modulecall()
    loadregs
    jumpnext
end

threadedproc j_modulereturn=
    saveregs
    k_modulereturn()
    loadregs
    jumpnext
end

threadedproc j_calldll=
    saveregs
    k_calldll()
    loadregs
    jumpnext
end

threadedproc j_callhost=
    saveregs
    k_callhost()
    loadregs
    jumpnext
end

threadedproc j_unshare=
    assem
        mov D5,[Dprog+kopnda]       !assume > 0

L1:     cmp byte [Dsptr+khasref],1
        jnz L2
        *callunshareu_dsptr
L2:     *popvar
        dec D5
        jnz L1
        *jumpskip2
    end

    saveregs
    k_unshare()
    loadregs
    jumpnext
end

threadedproc j_stop=
    saveregs

    asm jmp disploop.stoplabel
end

threadedproc j_stoprunproc=
    saveregs
    asm jmp disploop.stoplabel
!   k_stoprunproc()
!   loadregs
!   jumpnext
end

threadedproc j_makelist=
    saveregs
    k_makelist()
    loadregs
    jumpnext
end

threadedproc j_makerecord=
    saveregs
    k_makerecord()
    loadregs
    jumpnext
end

threadedproc j_makearray=
    saveregs
    k_makearray()
    loadregs
    jumpnext
end

threadedproc j_makebits=
    saveregs
    k_makebits()
    loadregs
    jumpnext
end

threadedproc j_makestruct=
    saveregs
    k_makestruct()
    loadregs
    jumpnext
end

threadedproc j_makeset=
    saveregs
    k_makeset()
    loadregs
    jumpnext
end

threadedproc j_makerange=
    saveregs
    k_makerange()
    loadregs
    jumpnext
end

threadedproc j_makerangelen=
    saveregs
    k_makerangelen()
    loadregs
    jumpnext
end

threadedproc j_makedict=
    saveregs
    k_makedict()
    loadregs
    jumpnext
end

threadedproc j_makedecimal=
    saveregs
    k_makedecimal()
    loadregs
    jumpnext
end

threadedproc j_incrptr=
    assem
        cmp byte [Dsptr+ktag],trefvar
        jnz L99
        mov D4,[Dsptr+kvarptr]
        mov B0,[D4+ktag]
        cmp B0,tint
        jnz L1
        inc word64 [D4+kvalue]
        *popvar
        *jumpskip1

L1:
!       cmp B0,trefpack
!       jnz L2
!       movzx A3,word16 [Dsptr+krefelemtag]
!       mov D3,[D3*8+ttsize]
!       add [D4+kptr],D3
!       *popvar
!       *jumpskip1

L2:

L99:
    end

    saveregs
    k_incrptr()
    loadregs
    jumpnext
end

threadedproc j_incrtom=
    assem
        mov D4,[Dprog+kopnda]
        cmp byte [D4+ktag],tint
        jnz L1
        inc word64 [D4+kvalue]
        *jumpskip2

    L1:
        cmp byte [D4+ktag],trefpack
        jnz L2
        movzx A0,word16 [D4+krefelemtag]
        mov A0,[D0*8+ttsize]
        add [D4+kvarptr],D0
        *jumpskip2

L2:
    end

    saveregs
    k_incrtom()
    loadregs
    jumpnext
end

threadedproc j_incrtof=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        cmp byte [D4+ktag],tint
        jnz L1
        inc word64 [D4+kvalue]
        *jumpskip2

    L1:
        cmp byte [D4+ktag],trefpack
        jnz L2
        movzx A0,word16 [D4+krefelemtag]
        mov A0,[D0*8+ttsize]
        add [D4+kvarptr],D0
        *jumpskip2

L2:
    end

    saveregs
    k_incrtof()
    loadregs
    jumpnext
end

threadedproc j_loadincr=
    assem
        cmp byte [Dsptr+ktag],trefvar
        jnz L99
        mov D4,[Dsptr+kvarptr]

        mov B0,[D4+ktag]
        cmp B0,tint
        jnz L1
!refvar int
        mov D0,[D4+kvalue]
        inc word64 [D4+kvalue]
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D0
        *jumpskip1

L1:
        cmp B0,trefpack
        jnz L2
        cmp word16 [D4+krefelemtag],tpu8
        jnz L2

!refvar refpack u8
        mov D0,[D4+kptr]
        inc word64 [D4+kptr]

        mov word32 [Dsptr+ktag],trefpack
        mov word16 [Dsptr+krefelemtag],tpu8
        mov [Dsptr+kptr],D0
        *jumpskip1

L2:

L99:
    end
!
    saveregs
    k_loadincr()
    loadregs
    jumpnext
end

threadedproc j_incrload=
    assem
        cmp byte [Dsptr+ktag],trefvar
        jnz L99
        mov D4,[Dsptr+kvarptr]

        mov B0,[D4+ktag]
        cmp B0,tint
        jnz L1
!refvar int
        inc word64 [D4+kvalue]
        mov D0,[D4+kvalue]
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D0
        *jumpskip1

L1:
        cmp B0,trefpack
        jnz L2
        cmp word16 [D4+krefelemtag],tpu8
        jnz L2

!refvar refpack u8
        inc word64 [D4+kptr]
        mov D0,[D4+kptr]
        mov word32 [Dsptr+ktag],trefpack
        mov word16 [Dsptr+krefelemtag],tpu8
        mov [Dsptr+kptr],D0
        *jumpskip1

L2:

L99:
    end

    saveregs
!CPL "J:CAN'T DO INCRLOAD",TTNAME[SPTR.TAG]
    k_incrload()
    loadregs
    jumpnext
end

threadedproc j_decrptr=
    saveregs
    k_decrptr()
    loadregs
    jumpnext
end

threadedproc j_decrtom=
    assem
        mov D4,[Dprog+kopnda]
        cmp byte [D4+ktag],tint
        jnz L1
        dec word64 [D4+kvalue]
        *jumpskip2

    L1:
        cmp byte [D4+ktag],trefpack
        jnz L2
        movzx A0,word16 [D4+krefelemtag]
        mov A0,[D0*8+ttsize]
        sub [D4+kvarptr],D0
        *jumpskip2

L2:
    end

    saveregs
    k_decrtom()
    loadregs
    jumpnext
end

threadedproc j_decrtof=
    assem
        mov D4,[Dprog+kopnda]
        cmp byte [Dframe+D4+ktag],tint
        jnz L1
        dec word64 [Dframe+D4+kvalue]
        *jumpskip2

    L1:
        cmp byte [Dframe+D4+ktag],trefpack
        jnz L2
        movzx A0,word16 [Dframe+D4+krefelemtag]
        mov A0,[D0*8+ttsize]
        sub [Dframe+D4+kvarptr],D0
        *jumpskip2

L2:
    end

    saveregs
    k_decrtof()
    loadregs
    jumpnext
end

threadedproc j_loaddecr=
    saveregs
    k_loaddecr()
    loadregs
    jumpnext
end

threadedproc j_decrload=
    saveregs
    k_decrload()
    loadregs
    jumpnext
end

threadedproc j_incr=
    saveregs
    k_incr()
    loadregs
    jumpnext
end

threadedproc j_decr=
    saveregs
    k_decr()
    loadregs
    jumpnext
end

threadedproc j_neg=
    saveregs
    k_neg()
    loadregs
    jumpnext
end

threadedproc j_abs=
    saveregs
    k_abs()
    loadregs
    jumpnext
end

threadedproc j_notl=
    saveregs
    k_notl()
    loadregs
    jumpnext
end

threadedproc j_inot=
    assem
        cmp byte [Dsptr+ktag],tint
        jnz L1

        not word64 [dsptr+kvalue]
!       mov D0,[dsptr+kvalue]
!       not D0
!       mov [dsptr+kvalue],D0
        *jumpskip1
L1:
    end

    saveregs
    k_inot()
    loadregs
    jumpnext
end

threadedproc j_istruel=
    saveregs
    k_istruel()
    loadregs
    jumpnext
end

threadedproc j_asc=
    saveregs
    k_asc()
    loadregs
    jumpnext
end

threadedproc j_chr=
    assem
        cmp byte [Dsptr+ktag],tint
        jnz L99                     !not int; B deals with the error
        mov D0,[Dsptr+kvalue]
        cmp D0,255
        ja L99                      !not in range
        mov D0,[D0*8+chrtable]
        and D0,D0
        jz L99                      !value not cached; B will fill it in
        mov word32 [Dsptr+ktag],tstring+hasrefmask
        mov [Dsptr+kvalue],D0               !point to object
        inc word32 [D0+jrefcount]
        *jumpskip1
L99:
    end

    saveregs
    k_chr()
    loadregs
    jumpnext
end

threadedproc j_sqrt=
    assem
        cmp word16 [Dsptr+ktag],tint
        jnz L1
        fild word32 [Dsptr+kvalue]
        fsqrt
        mov word32 [Dsptr+ktag],treal
        fstp word64 [Dsptr+kvalue]
        *jumpskip1
L1:
        cmp word16 [Dsptr+ktag],treal
        jnz L2

        movq xmm0,[Dsptr+kvalue]
        sqrtsd xmm0,xmm0
        movq [Dsptr+kvalue],xmm0

        *jumpskip1
L2:
    end

    saveregs
    k_sqrt()
    loadregs
    jumpnext
end

threadedproc j_sqr=
    assem
        cmp word16 [Dsptr+ktag],tint
        jnz L1
        mov D0,[Dsptr+kvalue]
        imul2 D0,D0
        mov [Dsptr+kvalue],D0
        *jumpskip1
L1:
        cmp word16 [Dsptr+ktag],treal
        cmp word16 [Dsptr+ktag],treal
        jnz L2

        movq xmm0,[Dsptr+kvalue]
        mulsd xmm0,xmm0
        movq [Dsptr+kvalue],xmm0

        *jumpskip1
L2:
    end

    saveregs
    k_sqr()
    loadregs
    jumpnext
end

threadedproc j_sin=
    saveregs
    k_sin()
    loadregs
    jumpnext
end

threadedproc j_cos=
    saveregs
    k_cos()
    loadregs
    jumpnext
end

threadedproc j_tan=
    saveregs
    k_tan()
    loadregs
    jumpnext
end

threadedproc j_asin=
    saveregs
    k_asin()
    loadregs
    jumpnext
end

threadedproc j_acos=
    saveregs
    k_acos()
    loadregs
    jumpnext
end

threadedproc j_atan=
    saveregs
    k_atan()
    loadregs
    jumpnext
end

threadedproc j_sign=
    saveregs
    k_sign()
    loadregs
    jumpnext
end

threadedproc j_ln=
    saveregs
    k_ln()
    loadregs
    jumpnext
end

threadedproc j_log=
    saveregs
    k_log()
    loadregs
    jumpnext
end

threadedproc j_lg=
    saveregs
    k_lg()
    loadregs
    jumpnext
end

threadedproc j_exp=
    saveregs
    k_exp()
    loadregs
    jumpnext
end

threadedproc j_round=
    saveregs
    k_round()
    loadregs
    jumpnext
end

threadedproc j_floor=
    saveregs
    k_floor()
    loadregs
    jumpnext
end

threadedproc j_ceil=
    saveregs
    k_ceil()
    loadregs
    jumpnext
end

threadedproc j_fract=
    saveregs
    k_fract()
    loadregs
    jumpnext
end

threadedproc j_fmod=
    saveregs
    k_fmod()
    loadregs
    jumpnext
end

threadedproc j_negto=
    saveregs
    k_negto()
    loadregs
    jumpnext
end

threadedproc j_absto=
    saveregs
    k_absto()
    loadregs
    jumpnext
end

threadedproc j_inotto=
    saveregs
    k_inotto()
    loadregs
    jumpnext
end

threadedproc j_notlto=
    saveregs
    k_notlto()
    loadregs
    jumpnext
end

threadedproc j_len=
    assem
        mov W0, [Dsptr+ktag]
        cmp B0,tlist
        jz L1
        cmp B0,tstring
        jz L1
        cmp B0,tarray
        jnz L99
L1:
        mov D1,[Dsptr+kobjptr]
        movzx D3, word32 [D1+jlength32]

!       and W0,hasrefmask
!       jz L2
        *callunshareu_dsptr
!L2:
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D3
        *jumpskip1
L99:
    end

    saveregs
    k_len()
    loadregs
    jumpnext
end

threadedproc j_lwb=
    saveregs
    k_lwb()
    loadregs
    jumpnext
end

threadedproc j_upb=
    assem
        mov W0, [Dsptr+ktag]
        cmp B0,tlist
        jnz L99
L1:
        mov D1,[Dsptr+kobjptr]
        movzx D3, word32 [D1+jlength32]
        movsx D4, word32 [D1+jlower32]
        lea D3,[D3+D4-1]    

        *callunshareu_dsptr
!L2:
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D3
        *jumpskip1
L99:
    end

    saveregs
    k_upb()
    loadregs
    jumpnext
end

threadedproc j_bounds=
    saveregs
    k_bounds()
    loadregs
    jumpnext
end

threadedproc j_boundsx=
    saveregs
    k_boundsx()
    loadregs
    jumpnext
end

threadedproc j_bitwidth=
    saveregs
    k_bitwidth()
    loadregs
    jumpnext
end

threadedproc j_bytesize=
    saveregs
    k_bytesize()
    loadregs
    jumpnext
end

threadedproc j_type=
    saveregs
    k_type()
    loadregs
    jumpnext
end

threadedproc j_elemtype=
    saveregs
    k_elemtype()
    loadregs
    jumpnext
end

threadedproc j_basetype=
    saveregs
    k_basetype()
    loadregs
    jumpnext
end

threadedproc j_dictitems=
    saveregs
    k_dictitems()
    loadregs
    jumpnext
end

threadedproc j_minvalue=
    saveregs
    k_minvalue()
    loadregs
    jumpnext
end

threadedproc j_maxvalue=
    saveregs
    k_maxvalue()
    loadregs
    jumpnext
end

threadedproc j_isint=
    assem
        mov W0,[Dsptr+ktag]
        cmp B0,tint
        setz B3
        movzx D3,B3
        and W0,hasrefmask
        jz L1
        *callunshareu_dsptr
L1:     mov word32 [Dsptr],tint
        mov [Dsptr+kvalue],D3
        *jumpskip1
    end

    saveregs
    k_isint()
    loadregs
    jumpnext
end

threadedproc j_isreal=
    saveregs
    k_isreal()
    loadregs
    jumpnext
end

threadedproc j_isstring=
    saveregs
    k_isstring()
    loadregs
    jumpnext
end

threadedproc j_isrange=
    saveregs
    k_isrange()
    loadregs
    jumpnext
end

threadedproc j_isnumber=
    saveregs
    k_isnumber()
    loadregs
    jumpnext
end

threadedproc j_islist=
    assem
        mov W0,[Dsptr+ktag]
        cmp B0,tlist
        setz B3
        movzx D3,B3
        and W0,hasrefmask
        jz L1
        *callunshareu_dsptr
L1:     mov word32 [Dsptr],tint
        mov [Dsptr+kvalue],D3
        *jumpskip1
    end

    saveregs
    k_islist()
    loadregs
    jumpnext
end

threadedproc j_isrecord=
    saveregs
    k_isrecord()
    loadregs
    jumpnext
end

threadedproc j_ispointer=
    saveregs
    k_ispointer()
    loadregs
    jumpnext
end

threadedproc j_isarray=
    saveregs
    k_isarray()
    loadregs
    jumpnext
end

threadedproc j_ismutable=
    saveregs
    k_ismutable()
    loadregs
    jumpnext
end

threadedproc j_isset=
    saveregs
    k_isset()
    loadregs
    jumpnext
end

threadedproc j_isvoid=
    saveregs
    k_isvoid()
    loadregs
    jumpnext
end

threadedproc j_isdef=
    assem
        cmp byte [Dsptr+ktag],tvoid
        jnz L1
        mov D3,0
        jmp L2
L1:     mov D3,1
L2:
        cmp byte [Dsptr+khasref],1
        jnz L3
        *callunshareu_dsptr
L3:
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D3
        *jumpskip1
    end

    saveregs
    k_isdef()
    loadregs
    jumpnext
end

threadedproc j_isequal=
    saveregs
    k_isequal()
    loadregs
    jumpnext
end

threadedproc j_convert=
    saveregs
    k_convert()
    loadregs
    jumpnext
end

threadedproc j_typepun=
    saveregs
    k_typepun()
    loadregs
    jumpnext
end

threadedproc j_add=
    assem
        mov B0, [Dsptr+xb+ktag]
        mov B1, [Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99
        cmp B0,tint
        jnz L1
        mov D0,[Dsptr+ya+kvalue]
        add [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
        cmp B0,treal
        jnz L2

        fld word64 [Dsptr+xb+kvalue]
        fld word64 [Dsptr+ya+kvalue]
        fadd
        fstp word64 [Dsptr+xb+kvalue]

        *popvar
        *jumpskip1
L2:


L99:
    end

    saveregs
    k_add()
    loadregs
    jumpnext
end

threadedproc j_sub=
    assem
        mov B0, [Dsptr+xb+ktag]
        mov B1, [Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99

        cmp B0,tint
        jnz L1
        mov D0,[Dsptr+ya+kvalue]
        sub [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
        cmp B0,treal
        jnz L2

        fld word64 [Dsptr+xb+kvalue]
        fld word64 [Dsptr+ya+kvalue]
        fsub
        fstp word64 [Dsptr+xb+kvalue]

        *popvar
        *jumpskip1
L2:
L99:
    end

    saveregs
    k_sub()
    loadregs
    jumpnext
end

threadedproc j_mul=
    assem
        mov B0, [Dsptr+xb+ktag]
        mov B1, [Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99
        cmp B0,tint
        jnz L1

        mov D0,[Dsptr+xb+kvalue]
        imul word64 [Dsptr+ya+kvalue]
        mov [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
        cmp B0, treal
        jnz L2

        fld word64 [Dsptr+xb+kvalue]
        fld word64 [Dsptr+ya+kvalue]
        fmul
        fstp word64 [Dsptr+xb+kvalue]

        *popvar
        *jumpskip1

L2:
L99:
    end

    saveregs
    k_mul()
    loadregs
    jumpnext
end

threadedproc j_div=
    assem
        mov B0, [Dsptr+xb+ktag]
        mov B1, [Dsptr+ya+ktag]
        cmp B0,B1
        jnz L99
    
        cmp B0, treal
        jnz L2

        fld word64 [Dsptr+xb+kvalue]
        fld word64 [Dsptr+ya+kvalue]
        fdiv
        fstp word64 [Dsptr+xb+kvalue]

        *popvar
        *jumpskip1

L2:
L99:
    end

    saveregs
    k_div()
    loadregs
    jumpnext
end

threadedproc j_idiv=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov D0,[Dsptr+xb+kvalue]
        cqo
        mov D1, [Dsptr+ya+kvalue]
        and D1,D1
        jz L1
        idiv D1
        mov [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
    end

    saveregs

    k_idiv()
    loadregs
    jumpnext
end

threadedproc j_irem=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov D0,[Dsptr+xb+kvalue]
        cqo
        idiv word64 [Dsptr+ya+kvalue]
        mov [Dsptr+xb+kvalue],D11
        *popvar
        *jumpskip1
L1:
    end
    saveregs
    k_irem()
    loadregs
    jumpnext
end

threadedproc j_idivrem=
    saveregs
    k_idivrem()
    loadregs
    jumpnext
end

threadedproc j_iand=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov D0,[Dsptr+ya+kvalue]
        and [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
    end

    saveregs
    k_iand()
    loadregs
    jumpnext
end

threadedproc j_ior=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov D0,[Dsptr+ya+kvalue]
        or [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
    end
    saveregs
    k_ior()
    loadregs
    jumpnext
end

threadedproc j_ixor=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov D0,[Dsptr+ya+kvalue]
        xor [Dsptr+xb+kvalue],D0
        *popvar
        *jumpskip1
L1:
    end
    saveregs
    k_ixor()
    loadregs
    jumpnext
end

threadedproc j_shl=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov rdx,Dsptr
        mov cl,[Dsptr+ya+kvalue]
        shl word64 [rdx+xb+kvalue],cl
        mov Dsptr,rdx
        *popvar
        *jumpskip1
L1:
    end

    saveregs
    k_shl()
    loadregs
    jumpnext
end

threadedproc j_shr=
    assem
        cmp byte [Dsptr+xb+ktag],tint
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint
        jnz L1
        mov rdx,Dsptr
        mov cl,[Dsptr+ya+kvalue]
        sar word64 [rdx+xb+kvalue],cl
        mov Dsptr,rdx
        *popvar
        *jumpskip1
L1:
    end
    saveregs
    k_shr()
    loadregs
    jumpnext
end

threadedproc j_in=
    saveregs
    k_in()
    loadregs
    jumpnext
end

threadedproc j_notin=
    saveregs
    k_notin()
    loadregs
    jumpnext
end

threadedproc j_eq=
    saveregs
    k_eq()
    loadregs
    jumpnext
end

threadedproc j_ne=
    saveregs
    k_ne()
    loadregs
    jumpnext
end

threadedproc j_lt=
    saveregs
    k_lt()
    loadregs
    jumpnext
end

threadedproc j_le=
    saveregs
    k_le()
    loadregs
    jumpnext
end

threadedproc j_ge=
    saveregs
    k_ge()
    loadregs
    jumpnext
end

threadedproc j_gt=
    saveregs
    k_gt()
    loadregs
    jumpnext
end

threadedproc j_min=
    saveregs
    k_min()
    loadregs
    jumpnext
end

threadedproc j_max=
    saveregs
    k_max()
    loadregs
    jumpnext
end

threadedproc j_concat=
    saveregs
    k_concat()
    loadregs
    jumpnext
end

threadedproc j_append=
    saveregs
    k_append()
    loadregs
    jumpnext
end

threadedproc j_power=
    saveregs
    k_power()
    loadregs
    jumpnext
end

threadedproc j_atan2=
    saveregs
    k_atan2()
    loadregs
    jumpnext
end

threadedproc j_addto=
    assem
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [Dsptr+xb+ktag],trefvar    !lhs is ref var?
        jnz L99
        cmp byte [Dsptr+ya+ktag],tint       !rhs is int
        jnz L1
        cmp byte [D4+ktag],tint         !lhs is ref var:int?
        jnz L99
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        add D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1

L1:
        cmp byte [Dsptr+ya+ktag],treal      !rhs is real
        jnz L2
        cmp byte [D4+ktag],treal            !lhs is ref var:real?
        jnz L99                             !mixed

        movq xmm0,[D4+kvalue]
        addsd xmm0,[Dsptr+kvalue]
        movq [D4+kvalue],xmm0
        *popvar2
        *jumpskip1
L2:
L99:
    end

    saveregs
    k_addto()
    loadregs
    jumpnext
end

threadedproc j_subto=
    assem
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [Dsptr+xb+ktag],trefvar    !lhs is ref var?
        jnz L99
        cmp byte [Dsptr+ya+ktag],tint       !rhs is int
        jnz L1
        cmp byte [D4+ktag],tint         !lhs is ref var:int?
        jnz L99
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        sub D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1
L1:
        cmp byte [Dsptr+ya+ktag],treal      !rhs is real
        jnz L2
        cmp byte [D4+ktag],treal            !lhs is ref var:real?
        jnz L99                             !mixed

        movq xmm0,[D4+kvalue]
        subsd xmm0,[Dsptr+kvalue]
        movq [D4+kvalue],xmm0
        *popvar2
        *jumpskip1
L2:
L99:
    end

    saveregs
    k_subto()
    loadregs
    jumpnext
end

threadedproc j_multo=
    saveregs
    k_multo()
    loadregs
    jumpnext
end

threadedproc j_divto=
    saveregs
    k_divto()
    loadregs
    jumpnext
end

threadedproc j_idivto=
    saveregs
    k_idivto()
    loadregs
    jumpnext
end

threadedproc j_andlto=
    saveregs
    k_andlto()
    loadregs
    jumpnext
end

threadedproc j_orlto=
    saveregs
    k_orlto()
    loadregs
    jumpnext
end

threadedproc j_iandto=
    assem
        cmp byte [Dsptr+xb+ktag],trefvar    !lhs is ref var?
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       !rhs is int
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         !lhs is ref var:int?
        jnz L1
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        and D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1
L1:
    end

    saveregs
    k_iandto()
    loadregs
    jumpnext
end

threadedproc j_iorto=
    assem
        cmp byte [Dsptr+xb+ktag],trefvar    !lhs is ref var?
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       !rhs is int
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         !lhs is ref var:int?
        jnz L1
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        or D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1
L1:
    end

    saveregs
    k_iorto()
    loadregs
    jumpnext
end

threadedproc j_ixorto=
    assem
        cmp byte [Dsptr+xb+ktag],trefvar    !lhs is ref var?
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       !rhs is int
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         !lhs is ref var:int?
        jnz L1
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        xor D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1
L1:
    end

    saveregs
    k_ixorto()
    loadregs
    jumpnext
end

threadedproc j_shlto=
    assem
        cmp byte [Dsptr+xb+ktag],trefvar    !lhs is ref var?
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       !rhs is int
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         !lhs is ref var:int?
        jnz L1
        mov cl,[Dsptr+kvalue]
!   shl word64 [D4+kvalue],cl
        mov D0,[D4+kvalue]
        shl D0,cl
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1
L1:
    end

    saveregs
    k_shlto()
    loadregs
    jumpnext
end

threadedproc j_shrto=
    saveregs
    k_shrto()
    loadregs
    jumpnext
end

threadedproc j_minto=
    saveregs
    k_minto()
    loadregs
    jumpnext
end

threadedproc j_maxto=
    saveregs
    k_maxto()
    loadregs
    jumpnext
end

threadedproc j_concatto=
    saveregs
    k_concatto()
    loadregs
    jumpnext
end

threadedproc j_appendto=
    saveregs
    k_appendto()
    loadregs
    jumpnext
end

threadedproc j_dot=

    assem
JMP L99
        cmp byte [Dsptr+ktag],trecord
        jnz L99
        movzx D3, word16 [Dsptr+kusertag]       !rectype: actual record type
        mov D5, [Dprog+kopnda]          !index: (genfieldindex)
        and D5,D5
        jz L99                          !'not a field' error?

        mov D4, [D5*8+genfieldtable-8]  !g: pointer to genfieldrec

L1:     and D4,D4
        jz L99                          !no more entries
        mov D0,[D4+gdef]                !d: g.def
        mov D1,[D0+sowner]              !d.owner
        cmp W3,[D1+smode]               !rectype=d.owner.mode
        jz L3                           !found 
        mov D4,[D4+gnextdef]
        jmp L1                          !next genfield
!found possible field in d in D0
L3:     cmp byte [D0+snameid],fieldid
        jnz L99                         !not a field; don't handle that here
!got a regular field; now find the offset in the record
        mov D3,[Dsptr+kobjptr]
        mov D3,[D3+jvarptr]             !point to record fields

        movzx D0,word16 [D0+soffset]
        add D3,D0

        mov D0,[D3]
        mov D1,[D3+kvalue]
        test W0,hasrefmask
        jz L4
        inc word32 [D1+jrefcount]
L4:
        push D0
        push D1
        *callunshareu_dsptr             !unshare the record
        pop D1
        pop D0
        mov [Dsptr],D0
        mov [Dsptr+kvalue],D1
        *jumpskip2
    end
L99::

    saveregs
    k_dot()
    loadregs
    jumpnext
end

threadedproc j_index=
    static varrec v

    assem
!JMP L99
        cmp byte [Dsptr+ya+ktag],tint
        jnz L99

!int index:
        mov D6,[Dsptr+xb+ktag]
        cmp B6,tlist
        jnz L2

!list[int]
        mov D5,[Dsptr+xb+kobjptr]

        mov A4,[Dsptr+ya+kvalue]    !index
        sub A4,[D5+jlower32]        !0-base
        cmp A4,[D5+jlength32]
        jae L99                     !bounds error: let M deal with it

        shl A4,varshift             !index*varsize
        add D4,[D5+jvarptr]         !point to element

        *popvar                     !pop list, stack contains list descriptor

        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0         !replace index by list element

        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1

        and A0,hasrefmask
        jz L11
        inc word32 [D1+jrefcount]
L11:
        dec word32 [D5+jrefcount]   !dec count of original list
        jnz L12
        mov [v+ktag],D6
        mov [v+kobjptr],D5
        lea D10,[v]
        *callvarfree
L12:
        *jumpskip1

L2:
L3:
L99:
    end


    saveregs
    k_index()
    loadregs
    jumpnext
end

threadedproc j_dotindex=
    saveregs
    k_dotindex()
    loadregs
    jumpnext
end

threadedproc j_keyindex=
    saveregs
    k_keyindex()
    loadregs
    jumpnext
end

threadedproc j_dotref=
    saveregs
    k_dotref()
    loadregs
    jumpnext
end

threadedproc j_indexref=
    static varrec v

    assem
!JMP L99
        cmp byte [Dsptr+ya+ktag],tint
        jnz L99

!int index:
        mov D6,[Dsptr+xb+ktag]
        cmp B6,tlist
        jnz L2

!list[int]
        mov D5,[Dsptr+xb+kobjptr]

        mov A4,[Dsptr+ya+kvalue]    !index
        sub A4,[D5+jlower32]        !0-base
        cmp A4,[D5+jlength32]
        jae L99                     !bounds error: let M deal with it

        shl A4,varshift             !index*varsize
        add D4,[D5+jvarptr]         !point to element

        *popvar                     !pop list, stack contains list descriptor

        dec word32 [D5+jrefcount]   !dec count of original list
        jnz L12
        mov D10,Dsptr
        *callvarfree
L12:
        mov word32 [Dsptr+ktag],trefvar
        mov [Dsptr+kobjptr],D4

        *jumpskip1

L2:
L3:
L99:
    end

    saveregs
    k_indexref()
    loadregs
    jumpnext
end

threadedproc j_dotindexref=
    saveregs
    k_dotindexref()
    loadregs
    jumpnext
end

threadedproc j_keyindexref=
    saveregs
    k_keyindexref()
    loadregs
    jumpnext
end

threadedproc j_popdot=
    assem
JMP L99
        cmp byte [Dsptr+ktag],trecord
        jnz L99

        movzx D3, word16 [Dsptr+kusertag]       !rectype: actual record type
        mov D5, [Dprog+kopnda]          !index: (genfieldindex)
        and D5,D5
        jz L99                          !'not a field' error?

        mov D4,[Dsptr+kobjptr]
        mov B0,[D4+jmutable]
        and B0,1
        jz L99                      !not mutable

        mov D4, [D5*8+genfieldtable-8]  !g: pointer to genfieldrec

L1:     and D4,D4
        jz L99                          !no more entries
        mov D0,[D4+gdef]                !d: g.def
        mov D1,[D0+sowner]              !d.owner
        cmp W3,[D1+smode]               !rectype=d.owner.mode
        jz L3                           !found 
        mov D4,[D4+gnextdef]
        jmp L1                          !next genfield
!found possible field in d in D0
L3:     cmp byte [D0+snameid],fieldid
        jnz L99                         !not a field; don't handle that here

!got a regular field; now find the offset in the record
        mov D3,[Dsptr+kobjptr]
        mov D3,[D3+jvarptr]             !point to record fields

        movzx D0,word16 [D0+soffset]
        add D3,D0
!D3 points to dest field; need to unshare first
        cmp byte [D3+khasref],1
        jnz L5
        mov D10,D3
        *callunshareu
L5:
        mov D0,[Dsptr+xb]               !get value next below stack
        mov D1,[Dsptr+xb+kvalue]
        test W0,hasrefmask
        jz L4
        inc word32 [D1+jrefcount]
L4:
        push D0
        push D1
        *callunshareu_dsptr             !unshare the record
        pop D1
        pop D0
        *popvar2                        !lose record, and value that is now in D0/D1
        mov [D3],D0
        mov [D3+kvalue],D1
        *jumpskip2
    end
L99::

    saveregs
    k_popdot()
    loadregs
    jumpnext
end

threadedproc j_popindex=
    static varrec v
    assem
JMP L99

        cmp byte [Dsptr+za+ktag],tint
        jnz L99

!int index:
        mov D6,[Dsptr+yb+ktag]
        cmp B6,tlist
        jnz L99

!list[int]
        mov D5,[Dsptr+yb+kobjptr]
        mov B0,[D5+jmutable]
        and B0,1
        jz L99                      !not mutable

        mov A4,[Dsptr+ya+kvalue]    !index
        sub A4,[D5+jlower32]        !0-base
        cmp A4,[D5+jlength32]
        jae L99                     !bounds error or extend: let M deal with it

        shl A4,varshift             !index*varsize
        add D4,[D5+jvarptr]         !point to element

        mov D0,[Dsptr+xc]           !xfer ref count
        mov D1,[Dsptr+xc+kvalue]
        mov [D4],D0
        mov [D4+kvalue],D1

        dec word32 [D5+jrefcount]   !dec count of original list
        jnz L12
        mov [v+ktag],D6
        mov [v+kobjptr],D5
        lea D10,[v]
        *callvarfree
L12:
        *popvar3
        *jumpskip1

L99:

    end

    saveregs
    k_popindex()
    loadregs
    jumpnext

L34::

end

threadedproc j_popdotindex=
    saveregs
    k_popdotindex()
    loadregs
    jumpnext
end

threadedproc j_popkeyindex=
    saveregs
    k_popkeyindex()
    loadregs
    jumpnext
end

threadedproc j_expand=
    saveregs
    k_expand()
    loadregs
    jumpnext
end

threadedproc j_pushtry=
    saveregs
    k_pushtry()
    loadregs
    jumpnext
end

threadedproc j_raise=
    saveregs
    k_raise()
    loadregs
    jumpnext
end

threadedproc j_maps=
    saveregs
    k_maps()
    loadregs
    jumpnext
end

threadedproc j_mapss=
    saveregs
    k_mapss()
    loadregs
    jumpnext
end

threadedproc j_addsp=
    saveregs
    k_addsp()
    loadregs
    jumpnext
end

threadedproc j_pushff=
    assem
        *pushvar2
        mov D4,[Dprog+kopnda]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D4,[Dprog+kopndb]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        *jumpskip4
    end
end

threadedproc j_pushmm=
    assem
        *pushvar2
        mov D4,[Dprog+kopnda]
        mov D0,[D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D4,[Dprog+kopndb]
        mov D0,[D4+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        *jumpskip4
    end
end

threadedproc j_pushfm=
    assem
        *pushvar2
        mov D4,[Dprog+kopnda]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D4,[Dprog+kopndb]
        mov D0,[D4+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        *jumpskip4
    end
end

threadedproc j_pushmf=
    assem
        *pushvar2
        mov D4,[Dprog+kopnda]
        mov D0,[D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D4,[Dprog+kopndb]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        *jumpskip4
    end
end

threadedproc j_pushfff=
    assem
        *pushvar3
        mov D4,[Dprog+kopnda]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xc+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xc+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D4,[Dprog+kopndb]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+yb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+yb+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        mov D4,[Dprog+kopndc]
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+za+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+za+kvalue],D1
        and A0,hasrefmask
        jz L4
        inc word32 [D1+jrefcount]
L4:
        *jumpskip6
    end
end


threadedproc j_nop2=
    jumpskip2
end

threadedproc j_skip=
    jumpskip1
end

threadedproc j_pushci0=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],tint
        xor D0,D0
        mov [Dsptr+kvalue],D0
        *jumpskip2
    end
end

threadedproc j_moveff=
    assem
        mov D5,[Dprog+kopndb]
        cmp byte [Dframe+D5+khasref],1
        jnz L1
        mov D1,[Dframe+D5+kobjptr]
        inc word32 [D1+jrefcount]       !increment before freeing (in case of a:=a)
    L1:
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        cmp byte [D4+khasref],1
        jnz L2
        *callunshareu_d4
    L2:
        mov D5,[Dprog+kopndb]
        mov D0,[Dframe+D5+ktag]
        mov [D4+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [D4+kvalue],D1

        *jumpskip4
    end
end

threadedproc j_zmoveff=
    assem
        mov D5,[Dprog+kopndb]
        cmp byte [Dframe+D5+khasref],1
        jnz L1
        mov D1,[Dframe+D5+kobjptr]
        inc word32 [D1+jrefcount]       !increment before freeing (in case of a:=a)
    L1:
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        mov D5,[Dprog+kopndb]
        mov D0,[Dframe+D5+ktag]
        mov [D4+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [D4+kvalue],D1

        *jumpskip4
    end
end

threadedproc j_movefm=
    assem
        mov D5,[Dprog+kopndb]
        cmp byte [D5+khasref],1
        jnz L1
        mov D1,[D5+kobjptr]
        inc word32 [D1+jrefcount]
    L1:
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        cmp byte [D4+khasref],1
        jnz L2
        *callunshareu_d4
    L2:
        mov D5,[Dprog+kopndb]
        mov D0,[D5+ktag]
        mov [D4+ktag],D0
        mov D1,[D5+kvalue]
        mov [D4+kvalue],D1

        *jumpskip4
    end
end

threadedproc j_movemf=
    assem
        mov D5,[Dprog+kopndb]
        cmp byte [Dframe+D5+khasref],1
        jnz L1
        mov D1,[Dframe+D5+kobjptr]
        inc word32 [D1+jrefcount]
    L1:
        mov D4,[Dprog+kopnda]
        cmp byte [D4+khasref],1
        jnz L2
        *callunshareu_d4
    L2:
        mov D5,[Dprog+kopndb]
        mov D0,[Dframe+D5+ktag]
        mov [D4+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [D4+kvalue],D1

        *jumpskip4
    end
end

threadedproc j_movemm=
    assem
        mov D5,[Dprog+kopndb]
        cmp byte [D5+khasref],1
        jnz L1
        mov D1,[D5+kobjptr]
        inc word32 [D1+jrefcount]
    L1:
        mov D4,[Dprog+kopnda]
        cmp byte [D4+khasref],1
        jnz L2
        *callunshareu_d4
    L2:
        mov D5,[Dprog+kopndb]
        mov D0,[D5+ktag]
        mov [D4+ktag],D0
        mov D1,[D5+kvalue]
        mov [D4+kvalue],D1

        *jumpskip4
    end
end

threadedproc j_movefci=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        cmp byte [D4+khasref],1
        jnz L1
        *callunshareu_d4
    L1:
        mov word32 [D4+ktag],tint
        mov D0,[Dprog+kopndb]
        mov [D4+kvalue],D0
        *jumpskip4
    end
end

threadedproc j_zmovefci=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
    L1:
        mov word32 [D4+ktag],tint
        mov D0,[Dprog+kopndb]
        mov [D4+kvalue],D0
        *jumpskip4
    end
end

threadedproc j_movemci=
    assem
        mov D4,[Dprog+kopnda]
        cmp byte [D4+khasref],1
        jnz L1
        *callunshareu_d4
    L1:
        mov word32 [D4+ktag],tint
        mov D0,[Dprog+kopndb]
        mov [D4+kvalue],D0
        *jumpskip4
    end
end

threadedproc j_pushvoid2=
    assem
        *pushvar2
        mov word32 [Dsptr+ya+ktag],tvoid
        mov word32 [Dsptr+xb+ktag],tvoid
        *jumpskip2
    end
end

threadedproc j_pushvoid3=
    assem
        *pushvar3
        mov word32 [Dsptr+za+ktag],tvoid
        mov word32 [Dsptr+yb+ktag],tvoid
        mov word32 [Dsptr+xc+ktag],tvoid
        *jumpskip3
    end
end

threadedproc j_unshare1=
    assem
        cmp byte [Dsptr+khasref],1
        jnz L1
        *callunshareu_dsptr
L1:     *popvar
        *jumpskip2
    end
end

threadedproc j_unshare2=
    assem
        cmp byte [Dsptr+ya+khasref],1
        jnz L1
        *callunshareu_dsptr
L1:
        cmp byte [Dsptr+xb+khasref],1
        jnz L2
        lea D10,[Dsptr+xb]
        *callunshareu
L2:     *popvar2

        *jumpskip2
    end
end

threadedproc j_unshare3=
    assem
        cmp byte [Dsptr+za+khasref],1
        jnz L1
        *callunshareu_dsptr
L1:
        cmp byte [Dsptr+yb+khasref],1
        jnz L2
        lea D10,[Dsptr+yb]
        *callunshareu
L2:
        cmp byte [Dsptr+xc+khasref],1
        jnz L3
        lea D10,[Dsptr+xc]
        *callunshareu
L3:     *popvar3

        *jumpskip2
    end
end

threadedproc j_procentry1=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],tvoid
        *jumpskip2
    end
end

threadedproc j_procentry2=
    assem
        *pushvar2
        mov word32 [Dsptr+ya+ktag],tvoid
        mov word32 [Dsptr+xb+ktag],tvoid
        *jumpskip2
    end
end

threadedproc j_jumpeqfci=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,D5
        jnz Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5

        add Dprog,intpsize4
        jmp j_jumpeq
!       *jumpnext
    end
end

threadedproc j_jumpnefci=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,D5
        jz Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5

        add Dprog,intpsize4
        jmp j_jumpne
!       *jumpnext
    end
end

threadedproc j_jumpltfci=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,D5
        jge Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5

        add Dprog,intpsize4
        jmp j_jumplt
!       *jumpnext
    end
end

threadedproc j_jumplefci=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,D5
        jg Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5

        add Dprog,intpsize4
        jmp j_jumple
!       *jumpnext
    end
end

threadedproc j_jumpgefci=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,D5
        jl Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5

        add Dprog,intpsize4
        jmp j_jumpge
!       *jumpnext
    end
end

threadedproc j_jumpgtfci=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,D5
        jle Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5

        add Dprog,intpsize4
        jmp j_jumpgt
!       *jumpnext
    end
end

threadedproc j_switchf=
    assem
        mov D3,[Dprog+kopnda]
        cmp word16 [D3+Dframe+ktag],tint
        jnz L99                         !get M deal with errors
        mov D4,[D3+Dframe+kvalue]       !switch index
        sub D4,[Dprog+kopndc]           !index-lower! now 0-based index
        cmp D4,[Dprog+kopndb]           !index0>=n?
        jae L2                          !out of range
!in range
        shl D4,1
        mov Dprog,[Dprog+D4*8+intpsize6]
        *jumpnext
!out of range
    L2:
        mov D5,[Dprog+kopndb]
        shl D5,1
        mov Dprog,[Dprog+D5*8+intpsize6]
        *jumpnext

    L99:
    end
    pcerror("jswitchf/not int")
end

threadedproc j_addfci=
    assem
        mov D4,[Dprog+kopnda]
        mov D5,[Dprog+kopndb]
        cmp byte [D4+Dframe+ktag],tint
        jnz L1
        *pushvar
        mov word32 [Dsptr+ktag],tint
        mov D0,[Dframe+D4+kvalue]
        add D0,D5
        mov [Dsptr+kvalue],D0
        *jumpskip5
L1:
        *pushvar2

        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:

        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5
        add Dprog,intpsize4
        jmp j_add
!       *jumpnext
    end
end

threadedproc j_subfci=
    assem
        mov D4,[Dprog+kopnda]
        mov D5,[Dprog+kopndb]
        cmp byte [D4+Dframe+ktag],tint
        jnz L1
        *pushvar
        mov word32 [Dsptr+ktag],tint
        mov D0,[Dframe+D4+kvalue]
        sub D0,D5
        mov [Dsptr+kvalue],D0
        *jumpskip5
L1:
        *pushvar2

        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:

        mov word32 [Dsptr+ya+ktag],tint
        mov [Dsptr+ya+kvalue],D5
        add Dprog,intpsize4
        jmp j_sub
!       *jumpnext
    end
end

threadedproc j_indexff=
    assem
        mov D2,[Dprog+kopnda]
        mov D3,[Dprog+kopndb]
!JMP L99
        cmp byte [D2+Dframe+ktag],tlist
        jnz L99
        cmp byte [D3+Dframe+ktag],tint
        jnz L99

!list[int]
        mov D6,[D2+Dframe+ktag]

        mov D5,[D2+Dframe+kobjptr]
        mov D4,[D3+Dframe+kvalue]       !index
        sub A4,[D5+jlower32]        !0-base
        cmp A4,[D5+jlength32]
        jae L99                 !bounds error: let B deal with it

!jmp L99
        shl A4,varshift             !index*varsize
        add D4,[D5+jvarptr]         !point to element

        *pushvar
        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0         !replace index by list element
        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1
        and A0,hasrefmask
        jz L1
        inc word32 [D1+jrefcount]
L1:
        *jumpskip5

L99:
        *pushvar2
        mov D0,[Dframe+D2+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D2+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L12
        inc word32 [D1+jrefcount]
L12:
        mov D0,[Dframe+D3+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D3+kvalue]
        mov [Dsptr+ya+kvalue],D1
!       and A0,hasrefmask               !not needed for int/range index
!       jz L13
!       inc word32 [D1+jrefcount]
L13:

        add Dprog,intpsize4
        jmp j_index
!       *jumpnext
    end

end

threadedproc j_addff=
    assem
        mov D4,[Dprog+kopnda]
        mov D5,[Dprog+kopndb]
        cmp byte [D4+Dframe+ktag],tint
        jnz L1
        cmp byte [D5+Dframe+ktag],tint
        jnz L1
        *pushvar
        mov word32 [Dsptr+xa+ktag],tint
        mov D0,[Dframe+D4+kvalue]
        add D0,[Dframe+D5+kvalue]
        mov [Dsptr+kvalue],D0
        *jumpskip5

L1:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:

        add Dprog,intpsize4
        jmp j_add
    end
end

threadedproc j_subff=
    assem
        mov D4,[Dprog+kopnda]
        mov D5,[Dprog+kopndb]
        cmp byte [D4+Dframe+ktag],tint
        jnz L1
        cmp byte [D5+Dframe+ktag],tint
        jnz L1
        *pushvar
        mov word32 [Dsptr+xa+ktag],tint
        mov D0,[Dframe+D4+kvalue]
        sub D0,[Dframe+D5+kvalue]
        mov [Dsptr+kvalue],D0
        *jumpskip5

L1:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:

        add Dprog,intpsize4
        jmp j_sub
    end
end

threadedproc j_pushincrptrm =
    assem
        mov D2,[Dprog+kopnda]
        jmp j_pushincrptrf.L0
    end
end

threadedproc j_pushincrptrf =
!do pushf/loadincr/pushptr, optimised for byte-pointers
!used in the rvalue: p++^ 

    assem
        mov D2,[Dprog+kopnda]
        add D2,Dframe
!JMP L99
L0:
        cmp byte [D2+ktag],trefpack
        jnz L99                         !not pointer to packed type
        cmp byte [D2+krefelemtag],tpu8
        jnz L99

!pointer to byte
        mov D5,[D2+kptr]
        inc word64 [D2+kptr]
        movzx A0,byte [D5]

        *pushvar
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D0
        *jumpskip4

L99:
        *pushvar
        mov word32 [Dsptr+ktag],trefvar
        mov [Dsptr+kvarptr],D2
        add Dprog,intpsize2         !point at loadincr (with pushptr next)
        jmp j_loadincr
    end
end

threadedproc j_popincrptrm =
    assem
        mov D2,[Dprog+kopnda]
        jmp j_popincrptrf.L0
    end
end

threadedproc j_popincrptrf =
    assem
        mov D2,[Dprog+kopnda]
        add D2,Dframe
L0:
!JMP L99
        cmp byte [D2+ktag],trefpack
        jnz L99                         !not pointer to packed type
        cmp byte [Dsptr+ktag],tint
        jnz L99                         !let M deal with conversions or errors

        cmp byte [D2+krefelemtag],tpu8
        jnz L2

!pointer to byte
        mov D5,[D2+kptr]
        inc word64 [D2+kptr]

        mov D0,[Dsptr+kvalue]
        mov [D5],B0
        *popvar
        *jumpskip4

L2:
        cmp word16 [D2+krefelemtag],tpi32
        jnz L3

!pointer to int32
        mov D5,[D2+kptr]
        add word64 [D2+kptr],4

        mov D0,[Dsptr+kvalue]
        mov [D5],A0
        *popvar
        *jumpskip4
L3:
L99:
        *pushvar
        mov word32 [Dsptr+ktag],trefvar
        lea D0,[D2]
        mov [Dsptr+kvarptr],D0
        add Dprog,intpsize2         !point at loadincr (with pushptr next)
        jmp j_loadincr
    end

end

threadedproc j_jumpltff=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99
        cmp byte [D5+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,[Dframe+D5+kvalue]
        jge Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        add Dprog,intpsize4
        jmp j_jumplt
    end
end

threadedproc j_jumpleff=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99
        cmp byte [D5+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,[Dframe+D5+kvalue]
        jg Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        add Dprog,intpsize4
        jmp j_jumple
    end
end

threadedproc j_jumpgeff=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99
        cmp byte [D5+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,[Dframe+D5+kvalue]
        jl Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        add Dprog,intpsize4
        jmp j_jumpge
    end
end

threadedproc j_jumpgtff=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99
        cmp byte [D5+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,[Dframe+D5+kvalue]
        jle Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        add Dprog,intpsize4
        jmp j_jumpgt
    end
end

threadedproc j_jumpeqff=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99
        cmp byte [D5+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,[Dframe+D5+kvalue]
        jnz Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        add Dprog,intpsize4
        jmp j_jumpeq
    end
end

threadedproc j_jumpneff=
    assem
        mov D4,[Dprog+kopndb]
        mov D5,[Dprog+kopndc]
        cmp byte [D4+Dframe+ktag],tint
        jnz L99
        cmp byte [D5+Dframe+ktag],tint
        jnz L99

        mov D0,[Dframe+D4+kvalue]
        cmp D0,[Dframe+D5+kvalue]
        jz Lfalse
        mov Dprog,[Dprog+kopnda]
        *jumpnext
Lfalse:
        *jumpskip6
L99:
        *pushvar2
        mov D0,[Dframe+D4+ktag]
        mov [Dsptr+xb+ktag],D0
        mov D1,[Dframe+D4+kvalue]
        mov [Dsptr+xb+kvalue],D1
        and A0,hasrefmask
        jz L2
        inc word32 [D1+jrefcount]
L2:
        mov D0,[Dframe+D5+ktag]
        mov [Dsptr+ya+ktag],D0
        mov D1,[Dframe+D5+kvalue]
        mov [Dsptr+ya+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3:
        add Dprog,intpsize4
        jmp j_jumpne
    end
end

threadedproc j_pushptrf=
    assem
        *pushvar
        mov D2,[Dprog+kopnda]
        add D2,Dframe
!JMP L99
        cmp byte [D2+ktag],trefvar
        jnz L1
        mov D4,[D2+kvarptr]

        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1
        and A0,hasrefmask
        jz L0
        inc word32 [D1+jrefcount]
L0:
        *jumpskip3

L1:
        cmp byte [D2+ktag],trefpack
        jnz L2

        mov D4,[D2+kptr]
        movzx A0,word16 [D2+krefelemtag]

        cmp A0,tpu8
        jnz L10
        mov word32 [Dsptr+ktag],tint
        movzx A0,byte [D4]
        mov [Dsptr+kvalue],D0
        *jumpskip3
L10:
        cmp A0,tpi32
        jnz L11
        mov word32 [Dsptr+ktag],tint
        mov A0,[D4]
        movsxd D0,A0
        mov [Dsptr+kvalue],D0
        *jumpskip3
L11:
L2:
L99:
        mov D0,[D2+ktag]
        mov [Dsptr+xa+ktag],D0
        mov D1,[D2+kvalue]
        mov [Dsptr+xa+kvalue],D1            !assume pointer is not a heap object
        add Dprog, intpsize2
        jmp j_pushptr
    end
end

threadedproc j_lenf=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
!JMP L99
        mov W0, [D4+ktag]
        cmp B0,tlist
        jz L1
        cmp B0,tstring
        jz L1
        cmp B0,tarray
        jnz L99
L1:
        mov D1,[D4+kobjptr]
        movzx D3, word32 [D1+jlength32]

        *pushvar
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D3
        *jumpskip3
L99:
        *pushvar
        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0
        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1
        and A0,hasrefmask
        jz L3
        inc word32 [D1+jrefcount]
L3: 
        add Dprog,intpsize2
        jmp j_len
    end


end

threadedproc j_even=
    saveregs
    k_even()
    loadregs
    jumpnext
end

threadedproc j_odd=
    saveregs
    k_odd()
    loadregs
    jumpnext
end

