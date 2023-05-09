!PCI Interpreter Core

const dostackcheck = 0

macro getvalue          = pcltable[pc].a.value
macro getsvalue         = pcltable[pc].a.svalue
macro getvalue3         = pcltable[pc+2].a.value
macro getxvalue         = pcltable[pc].a.xvalue
macro getxvalue32       = pcltable[pc].a.xvalue32
macro getlabelno        = labeltable[pcltable[pc].a.labelno]
macro getlabelno2       = labeltable[pcltable[pc+1].a.labelno]
macro getstaticaddr     = pcltable[pc].a.def.staticaddr
macro getstaticaddr2    = pcltable[pc+1].a.def.staticaddr
macro getstaticaddr3    = pcltable[pc+2].a.def.staticaddr
macro getframeoffset    = pcltable[pc].a.def.frameoffset
macro getframeoffset2   = pcltable[pc+1].a.def.frameoffset
macro getframeoffset3   = pcltable[pc+2].a.def.frameoffset
macro getprocindex      = pcltable[pc].a.def.pcindex
macro getdef            = pcltable[pc].a.def

macro getswmin          = pcltable[pc].x.swmin
macro getswmax          = pcltable[pc].x.swmax
macro getnargs          = pcltable[pc].x.nargs
macro getnvars          = pcltable[pc].x.nvars
macro getnlocals        = pcltable[pc].x.nlocals
macro getnparams        = pcltable[pc].x.nparams
macro getnretvals       = pcltable[pc].x.nretvalues
macro getstep           = pcltable[pc].x.step
macro getscale          = pcltable[pc].x.scale
macro getoffset         = pcltable[pc].x.poffset
macro getpopone         = pcltable[pc].x.popone
macro getx              = pcltable[pc].x.x
macro getparam1         = -2                !frame offset of first param

macro getmemsize        = pcltable[pc].memsize

macro getopcode         = pcltable[pc].jcode
macro steppc            = ++pc
macro steppc2           = pc+:=2
macro steppc3           = pc+:=3

const magic = 0x1438'B6F2'778E'A388

macro pcerror(mess) = pcerror3(mess,"",pc)
macro pcerror2(mess,mess2) = pcerror3(mess,mess2,pc)

byte debug

global func exec(int pcstart=1, spstart=0)int=
    const stacksize = 70'000
    [stacksize]int      stack
    [stacksize]real     xstack @stack
    [stacksize]word     ustack @stack
    [stacksize]ref void pstack @stack

    int pc:=pcstart
    int sp:=spstart
    int fp:=0
    ref i64 pi64
    int n,x
    int a,b,c
    int opc
    ref u8 pu8      @pi64
    ref u16 pu16    @pi64
    ref u32 pu32    @pi64
    ref u64 pu64    @pi64
    ref i8 pi8      @pi64
    ref i16 pi16    @pi64
    ref i32 pi32    @pi64
    ref r64 pr64    @pi64

    ref i64 pi64b
    int i,j,spr
    real xx @ x
    symbol d

    doswitchu getopcode
    when jnotready then
        println "NO HANDLER"
        unimpl
        steppc

    when jload_mi8 then
        unimpl
        steppc

    when jload_mi16 then
        unimpl
        steppc

    when jload_mi32 then
        unimpl
        steppc

    when jload_mi64 then
        pi64:=cast(getstaticaddr)
        stack[++sp]:=pi64^
        steppc

    when jload_mu8 then
        pu8:=cast(getstaticaddr)
        stack[++sp]:=pu8^
        steppc

    when jload_mu16 then
        pu16:=cast(getstaticaddr)
        stack[++sp]:=pu16^
        steppc

    when jload_mu32 then
        pu32:=cast(getstaticaddr)
        stack[++sp]:=pu32^
        steppc

    when jload_mr64 then
        unimpl
        steppc

    when jload_mr32 then
        unimpl
        steppc

    when jload_mmem then
        pstack[++sp]:=getstaticaddr
        steppc

    when jload_fi8 then
        stack[++sp]:=i8(stack[fp+getframeoffset])
        steppc

    when jload_fi16 then
        stack[++sp]:=i16(stack[fp+getframeoffset])
        steppc

    when jload_fi32 then
        stack[++sp]:=i32(stack[fp+getframeoffset])
        steppc

    when jload_fi64 then
        stack[++sp]:=stack[fp+getframeoffset]
        steppc

    when jload_fu8 then
        stack[++sp]:=u8(stack[fp+getframeoffset])
        steppc

    when jload_fu16 then
        unimpl
        steppc

    when jload_fu32 then
        stack[++sp]:=u32(stack[fp+getframeoffset])
        steppc

    when jload_fr64 then
        unimpl
        steppc

    when jload_fr32 then
        unimpl
        steppc

    when jload_fmem then
        pstack[++sp]:=&stack[fp+getframeoffset]
        steppc

    when jstore_mi8 then
        pi8:=cast(getstaticaddr)
        pi8^:=stack[sp--]
        steppc

    when jstore_mi16 then
        unimpl
        steppc

    when jstore_mi32 then
        unimpl
        steppc

    when jstore_mi64 then
        pi64:=cast(getstaticaddr)
        pi64^:=stack[sp--]
        steppc

    when jstore_mr64 then
        unimpl
        steppc

    when jstore_mr32 then
        unimpl
        steppc

    when jstore_mmem then
        pi8:=cast(getstaticaddr)
        memcpy(pi8, pstack[sp--], getmemsize)
        steppc

    when jstore_fi64, jstore_fi32, jstore_fi16, jstore_fi8 then
        stack[fp+getframeoffset]:=stack[sp--]
        steppc

    when jstore_fr64 then
        unimpl
        steppc

    when jstore_fr32 then
        unimpl
        steppc

    when jstore_fmem then
        pi8:=cast(&stack[fp+getframeoffset])
        memcpy(pi8, pstack[sp--], getmemsize)
        steppc

    when jloadimm_i64, jloadimm_r64 then
        stack[++sp]:=getvalue
        steppc

    when jloadimm_r32 then
        stack[++sp]:=putr32(getxvalue32)
        steppc

    when jloadimm_str then
        unimpl
        steppc

    when jloadref_m then
        stack[++sp]:=cast(getstaticaddr)
        steppc

    when jloadref_f then
        stack[++sp]:=int(&stack[fp+getframeoffset])
        steppc

    when jloadref_lab then
        stack[++sp]:=getlabelno
        steppc

    when jloadref_p then
        stack[++sp]:=getprocindex
        steppc

    when jloadref_ext then
        d:=getdef
        pstack[++sp]:=getdllfnptr(d.libindex)
        steppc

    when jiload_i8 then
        pi8:=pstack[sp]
        stack[sp]:=pi8^
        steppc

    when jiload_i16 then
        pi16:=pstack[sp]
        stack[sp]:=pi16^
        steppc

    when jiload_i32 then
        pi32:=pstack[sp]
        stack[sp]:=pi32^
        steppc

    when jiload_i64 then
        pi64:=pstack[sp]
        stack[sp]:=pi64^
        steppc

    when jiload_u8 then
        pu8:=pstack[sp]
        stack[sp]:=pu8^
        steppc

    when jiload_u16 then
        pu16:=pstack[sp]
        stack[sp]:=pu16^
        steppc

    when jiload_u32 then
        pu32:=pstack[sp]
        stack[sp]:=pu32^
        steppc

    when jiload_r64 then
        unimpl
        steppc

    when jiload_r32 then
        unimpl
        steppc

    when jiload_mem then            !no action needed; can't load block at p^, use p
        steppc

    when jiloadx_i8 then
        x:=stack[sp--]*getscale+getoffset   
        pi8:=pstack[sp]
        stack[sp]:=(pi8+x)^
        steppc

    when jiloadx_i16 then
        x:=stack[sp--]*getscale+getoffset   
        pi16:=pstack[sp]
        stack[sp]:=ref i16(ref byte(pi16)+x)^
        steppc

    when jiloadx_i32 then
        x:=stack[sp--]*getscale+getoffset   
        pi32:=pstack[sp]
        stack[sp]:=ref i32(ref byte(pi32)+x)^
        steppc

    when jiloadx_i64 then
        x:=stack[sp--]*getscale+getoffset   
        pi64:=pstack[sp]
        stack[sp]:=ref i64(ref byte(pi64)+x)^
        steppc

    when jiloadx_u8 then
        x:=stack[sp--]*getscale+getoffset   
        pu8:=pstack[sp]
        stack[sp]:=(pu8+x)^
        steppc

    when jiloadx_u16 then
        x:=stack[sp--]*getscale+getoffset   
        pu16:=pstack[sp]
        stack[sp]:=ref u16(ref byte(pu16)+x)^
        steppc

    when jiloadx_u32 then
        x:=stack[sp--]*getscale+getoffset   
        pu32:=pstack[sp]
        stack[sp]:=ref u32(ref byte(pu32)+x)^
        steppc

    when jiloadx_r64 then
        unimpl
        steppc

    when jiloadx_r32 then
        unimpl
        steppc

    when jiloadx_mem then
        x:=stack[sp--]*getscale+getoffset   
        pu8:=pstack[sp]
        pstack[sp]:=ref byte(pu8)+x
        steppc

    when jistore_i8 then
        pi8:=pstack[sp--]
        pi8^:=stack[sp--]
        steppc

    when jistore_i16 then
        pi16:=pstack[sp--]
        pi16^:=stack[sp--]
        steppc

    when jistore_i32 then
        pi32:=pstack[sp--]
        pi32^:=stack[sp--]
        steppc

    when jistore_i64 then
        pi64:=pstack[sp--]
        pi64^:=stack[sp--]
        steppc

    when jistore_r64 then
        unimpl
        steppc

    when jistore_r32 then
        unimpl
        steppc

    when jistore_mem then
        pi8:=pstack[sp--]
        memcpy(pi8, pstack[sp--], getmemsize)
        steppc

    when jistorex_i8 then
        x:=stack[sp--]*getscale+getoffset   
        pi8:=pstack[sp--]
        (pi8+x)^:=stack[sp--]
        steppc

    when jistorex_i16 then
        x:=stack[sp--]*getscale+getoffset   
        pi16:=pstack[sp--]
        (ref i16(ref byte(pi16)+x)^:=stack[sp--])
        steppc

    when jistorex_i32 then
        x:=stack[sp--]*getscale+getoffset   
        pi32:=pstack[sp--]
        (ref i32(ref byte(pi32)+x)^:=stack[sp--])
        steppc

    when jistorex_i64 then
        x:=stack[sp--]*getscale+getoffset   
        pi64:=pstack[sp--]
        (ref i64(ref byte(pi64)+x)^:=stack[sp--])
        steppc

    when jistorex_r64 then
        unimpl
        steppc

    when jistorex_r32 then
        unimpl
        steppc

    when jistorex_mem then
        x:=stack[sp--]*getscale+getoffset   
        pi8:=pstack[sp--]
        memcpy(pi8+x, pstack[sp--], getmemsize)
        steppc

    when jswapmem_i8 then
        unimpl
        steppc

    when jswapmem_i16 then
        unimpl
        steppc

    when jswapmem_i32 then
        unimpl
        steppc

    when jswapmem_i64 then
        pi64:=pstack[sp--]
        pi64b:=pstack[sp--]
        swap(pi64^, pi64b^)

        steppc

    when jswapmem_mem then
        unimpl
        steppc

    when jclear_i8 then
        unimpl
        steppc

    when jclear_i16 then
        unimpl
        steppc

    when jclear_i32 then
        pi32:=pstack[sp--]
        pi32^:=0
        steppc

    when jclear_i64 then
        pi64:=pstack[sp--]
        pi64^:=0
        steppc

    when jclear_mem then
        pi8:=pstack[sp--]
        memset(pi8, 0, getmemsize)
        steppc

    when jjumpeq_i64 then
        sp-:=2
        x:=stack[sp+1]=stack[sp+2]
        dojumpcond

    when jjumpeq_r64 then
        unimpl

    when jjumpeq_r32 then
        unimpl

    when jjumpeq_mem then
        unimpl

    when jjumpne_i64, jjumpne_r64 then
        sp-:=2
        x:=stack[sp+1]<>stack[sp+2]
dojumpcond::
        if x then pc:=getlabelno else sp+:=getpopone; steppc fi

    when jjumpne_r32 then
        unimpl

    when jjumpne_mem then
        unimpl

    when jjumplt_i64 then
        sp-:=2
        x:=stack[sp+1]<stack[sp+2]
        dojumpcond

    when jjumplt_u64 then
        sp-:=2
        x:=ustack[sp+1]<ustack[sp+2]
        dojumpcond

    when jjumplt_r64 then
        unimpl

    when jjumplt_r32 then
        unimpl

    when jjumple_i64 then
        sp-:=2
        x:=stack[sp+1]<=stack[sp+2]
        dojumpcond

    when jjumple_u64 then
        sp-:=2
        x:=ustack[sp+1]<=ustack[sp+2]
        dojumpcond

    when jjumple_r64 then
        sp-:=2
        x:=xstack[sp+1]<=xstack[sp+2]
        dojumpcond

    when jjumple_r32 then
        unimpl

    when jjumpge_i64 then
        sp-:=2
        x:=stack[sp+1]>=stack[sp+2]
        dojumpcond

    when jjumpge_u64 then
        sp-:=2
        x:=ustack[sp+1]>=ustack[sp+2]
        dojumpcond

    when jjumpge_r64 then
        unimpl

    when jjumpge_r32 then
        unimpl

    when jjumpgt_i64 then
        sp-:=2
        x:=stack[sp+1]>stack[sp+2]
        dojumpcond

    when jjumpgt_u64 then
        sp-:=2
        x:=ustack[sp+1]>ustack[sp+2]
        dojumpcond

    when jjumpgt_r64 then
        unimpl

    when jjumpgt_r32 then
        unimpl

    when jjumpt_i64, jjumpt_r64, jjumpt_r32 then
        if stack[sp--] then pc:=getlabelno else steppc fi

    when jjumpf_i64, jjumpf_r64, jjumpf_r32 then
        if stack[sp--]=0 then pc:=getlabelno else steppc fi

    when jadd_i64 then
        --sp
        stack[sp]+:=stack[sp+1]
        steppc

    when jadd_r64 then
        --sp
        xstack[sp]+:=xstack[sp+1]
        steppc

    when jadd_r32 then
        unimpl
        steppc

    when jsub_i64 then
        --sp
        stack[sp]-:=stack[sp+1]
        steppc

    when jsub_r64 then
        --sp
        xstack[sp]-:=xstack[sp+1]
        steppc

    when jsub_r32 then
        unimpl
        steppc

    when jmul_i64 then
        --sp
        stack[sp]*:=stack[sp+1]
        steppc

    when jmul_r64 then
        --sp
        xstack[sp]*:=xstack[sp+1]
        steppc

    when jmul_r32 then
        unimpl
        steppc

    when jneg_i64 then
        stack[sp]:=-stack[sp]
        steppc

    when jneg_r64 then
        xstack[sp]:=-xstack[sp]
        steppc

    when jneg_r32 then
        unimpl
        steppc

    when jabs_i64 then
        stack[sp]:=abs stack[sp]
        steppc

    when jabs_r64 then
        xstack[sp]:= abs xstack[sp]
        steppc

    when jabs_r32 then
        unimpl
        steppc

    when jsqr_i64 then
        stack[sp]*:=stack[sp]
        steppc

    when jsqr_r64 then
        xstack[sp]*:=xstack[sp]
        steppc

    when jsqr_r32 then
        unimpl
        steppc

    when jsign_i64 then
        unimpl
        steppc

    when jsign_r64 then
        unimpl
        steppc

    when jsign_r32 then
        unimpl
        steppc

    when jnegto_i8 then
        unimpl
        steppc

    when jnegto_i16 then
        unimpl
        steppc

    when jnegto_i32 then
        unimpl
        steppc

    when jnegto_i64 then
        unimpl
        steppc

    when jnegto_r64 then
        unimpl
        steppc

    when jnegto_r32 then
        unimpl
        steppc

    when jabsto_i8 then
        unimpl
        steppc

    when jabsto_i16 then
        unimpl
        steppc

    when jabsto_i32 then
        unimpl
        steppc

    when jabsto_i64 then
        unimpl
        steppc

    when jabsto_r64 then
        unimpl
        steppc

    when jabsto_r32 then
        unimpl
        steppc

    when jdivf_r64 then
        --sp
        xstack[sp]/:=xstack[sp+1]
        steppc

    when jdivf_r32 then
        unimpl
        steppc

    when jdiv_i64 then
        --sp
        stack[sp]:=stack[sp]/stack[sp+1]
        steppc

    when jdiv_u64 then
        --sp
        ustack[sp]:=ustack[sp]/ustack[sp+1]
        steppc

    when jrem_i64 then
        --sp
        stack[sp]:=stack[sp] rem stack[sp+1]
        steppc

    when jrem_u64 then
        --sp
        ustack[sp]:=ustack[sp] rem ustack[sp+1]
        steppc

    when jdivrem_i64 then
        a:=stack[sp-1]/stack[sp]
        b:=stack[sp-1] rem stack[sp]
        stack[sp-1]:=a
        stack[sp]:=b
        steppc

    when jdivrem_u64 then
        unimpl
        steppc

    when jshr_i64 then
        --sp
        stack[sp]:=stack[sp] >> stack[sp+1]
        steppc

    when jshr_u64 then
        --sp
        ustack[sp]:=ustack[sp] >> ustack[sp+1]
        steppc

    when jeq_i64 then
        --sp
        stack[sp]:=stack[sp]=stack[sp+1]
        steppc

    when jeq_r64 then
        unimpl
        steppc

    when jeq_r32 then
        unimpl
        steppc

    when jne_i64 then
        --sp
        stack[sp]:=stack[sp]<>stack[sp+1]
        steppc

    when jne_r64 then
        unimpl
        steppc

    when jne_r32 then
        unimpl
        steppc

    when jmin_i64 then
        --sp
        stack[sp] min:=stack[sp+1]
        steppc

    when jmin_u64 then
        unimpl
        steppc

    when jmin_r64 then
        unimpl
        steppc

    when jmin_r32 then
        unimpl
        steppc

    when jmax_i64 then
        --sp
        stack[sp] max:=stack[sp+1]
        steppc

    when jmax_u64 then
        unimpl
        steppc

    when jmax_r64 then
        unimpl
        steppc

    when jmax_r32 then
        unimpl
        steppc

    when jlt_i64 then
        --sp
        stack[sp]:=stack[sp]<stack[sp+1]
        steppc

    when jlt_u64 then
        --sp
        stack[sp]:=ustack[sp]<=ustack[sp+1]
        steppc

    when jlt_r64 then
        --sp
        stack[sp]:=stack[sp]<stack[sp+1]
        steppc

    when jlt_r32 then
        unimpl
        steppc

    when jle_i64 then
        --sp
        stack[sp]:=stack[sp]<=stack[sp+1]
        steppc

    when jle_u64 then
        --sp
        ustack[sp]:=ustack[sp]<=ustack[sp+1]
        steppc

    when jle_r64 then
        unimpl
        steppc

    when jle_r32 then
        unimpl
        steppc

    when jge_i64 then
        --sp
        stack[sp]:=stack[sp]>=stack[sp+1]
        steppc

    when jge_u64 then
        --sp
        ustack[sp]:=ustack[sp]>=ustack[sp+1]
        steppc

    when jge_r64 then
        unimpl
        steppc

    when jge_r32 then
        unimpl
        steppc

    when jgt_i64 then
        --sp
        stack[sp]:=stack[sp]>stack[sp+1]
        steppc

    when jgt_u64 then
        --sp
        stack[sp]:=ustack[sp]>ustack[sp+1]
        steppc

    when jgt_r64 then
        unimpl
        steppc

    when jgt_r32 then
        unimpl
        steppc

    when jpower_i64 then
        --sp
        stack[sp]:=stack[sp]**stack[sp+1]
        steppc

    when jpower_u64 then
        unimpl
        steppc

    when jpower_r64 then
        --sp
        xstack[sp]:=xstack[sp]**xstack[sp+1]
        steppc

    when jaddto_i8 then
        x:=stack[sp--]
        pi8:=pstack[sp--]
        pi8^+:=x
        steppc

    when jaddto_i16 then
        x:=stack[sp--]
        pi16:=pstack[sp--]
        pi16^+:=x
        steppc

    when jaddto_i32 then
        x:=stack[sp--]
        pi32:=pstack[sp--]
        pi32^+:=x
        steppc

    when jaddto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^+:=x
        steppc

    when jaddto_r64 then
        xx:=stack[sp--]
        pr64:=pstack[sp--]
        pr64^+:=xx
        steppc

    when jaddto_r32 then
        unimpl
        steppc

    when jsubto_i8 then
        unimpl
        steppc

    when jsubto_i16 then
        unimpl
        steppc

    when jsubto_i32 then
        x:=stack[sp--]
        pi32:=pstack[sp--]
        pi32^-:=x
        steppc

    when jsubto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^-:=x
        steppc

    when jsubto_r64 then
        unimpl
        steppc

    when jsubto_r32 then
        unimpl
        steppc

    when jmulto_i8 then
        unimpl
        steppc

    when jmulto_i16 then
        unimpl
        steppc

    when jmulto_i32 then
        unimpl
        steppc

    when jmulto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^*:=x
        steppc

    when jmulto_r64 then
        xx:=xstack[sp--]
        pr64:=pstack[sp--]
        pr64^*:=xx
        steppc

    when jmulto_r32 then
        unimpl
        steppc

    when jdivto_i8 then
        unimpl
        steppc

    when jdivto_i16 then
        unimpl
        steppc

    when jdivto_i32 then
        unimpl
        steppc

    when jdivto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^:=pi64^/x
        steppc

    when jdivto_u8 then
        unimpl
        steppc

    when jdivto_u16 then
        unimpl
        steppc

    when jdivto_u32 then
        unimpl
        steppc

    when jdivto_u64 then
        unimpl
        steppc

    when jdivfto_r64 then
        xx:=xstack[sp--]
        pr64:=pstack[sp--]
        pr64^:=pr64^/xx
        steppc

    when jdivfto_r32 then
        unimpl
        steppc

    when jminto_i8 then
        unimpl
        steppc

    when jminto_i16 then
        unimpl
        steppc

    when jminto_i32 then
        unimpl
        steppc

    when jminto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^ min:=x
        steppc

    when jminto_u8 then
        unimpl
        steppc

    when jminto_u16 then
        unimpl
        steppc

    when jminto_u32 then
        unimpl
        steppc

    when jminto_u64 then
        unimpl
        steppc

    when jminto_r64 then
        unimpl
        steppc

    when jminto_r32 then
        unimpl
        steppc

    when jmaxto_i8 then
        unimpl
        steppc

    when jmaxto_i16 then
        unimpl
        steppc

    when jmaxto_i32 then
        unimpl
        steppc

    when jmaxto_i64 then
        x:=stack[sp--]
        pu64:=pstack[sp--]
        pu64^ max:=x
        steppc

    when jmaxto_u8 then
        unimpl
        steppc

    when jmaxto_u16 then
        unimpl
        steppc

    when jmaxto_u32 then
        unimpl
        steppc

    when jmaxto_u64 then
        unimpl
        steppc

    when jmaxto_r64 then
        unimpl
        steppc

    when jmaxto_r32 then
        unimpl
        steppc

    when jbitnotto_i8 then
        unimpl
        steppc

    when jbitnotto_i16 then
        unimpl
        steppc

    when jbitnotto_i32 then
        unimpl
        steppc

    when jbitnotto_i64 then
        unimpl
        steppc

    when jnotto_i8 then
        unimpl
        steppc

    when jnotto_i16 then
        unimpl
        steppc

    when jnotto_i32 then
        unimpl
        steppc

    when jnotto_i64 then
        unimpl
        steppc

    when jnotnotto_i8 then
        unimpl
        steppc

    when jnotnotto_i16 then
        unimpl
        steppc

    when jnotnotto_i32 then
        unimpl
        steppc

    when jnotnotto_i64 then
        unimpl
        steppc

    when jincrto_i8 then
        pi8:=pstack[sp--]
        pi8^+:=getstep
        steppc

    when jincrto_i16 then
        pi16:=pstack[sp--]
        pi16^+:=getstep
        steppc

    when jincrto_i32 then
        pi32:=pstack[sp--]
        pi32^+:=getstep
        steppc

    when jincrto_i64 then
        pi64:=pstack[sp--]
        pi64^+:=getstep
        steppc

    when jincrload_i8 then
        pi8:=pstack[sp]
        pi8^+:=getstep
        stack[sp]:=pi8^
        steppc

    when jincrload_i16 then
        unimpl
        steppc

    when jincrload_i32 then
        unimpl
        steppc

    when jincrload_i64 then
        pi64:=pstack[sp]
        pi64^+:=getstep
        stack[sp]:=pi64^
        steppc

    when jloadincr_i8 then
        unimpl
        steppc

    when jloadincr_i16 then
        unimpl
        steppc

    when jloadincr_i32 then
        unimpl
        steppc

    when jloadincr_i64 then
        pi64:=pstack[sp]
        stack[sp]:=pi64^
        pi64^+:=getstep
        steppc

    when jdecrto_i8 then
        pi8:=pstack[sp--]
        pi8^-:=getstep
        steppc

    when jdecrto_i16 then
        pi16:=pstack[sp--]
        pi16^-:=getstep
        steppc

    when jdecrto_i32 then
        unimpl
        steppc

    when jdecrto_i64 then
        pi64:=pstack[sp--]
        pi64^-:=getstep
        steppc

    when jdecrload_i8 then
        unimpl
        steppc

    when jdecrload_i16 then
        unimpl
        steppc

    when jdecrload_i32 then
        pi32:=pstack[sp]
        pi32^-:=getstep
        stack[sp]:=pi32^
        steppc

    when jdecrload_i64 then
        pi64:=pstack[sp]
        pi64^-:=getstep
        stack[sp]:=pi64^
        steppc

    when jloaddecr_i8 then
        unimpl
        steppc

    when jloaddecr_i16 then
        unimpl
        steppc

    when jloaddecr_i32 then
        unimpl
        steppc

    when jloaddecr_i64 then
        pi64:=pstack[sp]
        stack[sp]:=pi64^
        pi64^-:=getstep
        steppc

    when jbitandto_i8 then
        unimpl
        steppc

    when jbitandto_i16 then
        unimpl
        steppc

    when jbitandto_i32 then
        unimpl
        steppc

    when jbitandto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^ iand:=x
        steppc

    when jbitorto_i8 then
        x:=stack[sp--]
        pi8:=pstack[sp--]
        pi8^ ior:=x
        steppc

    when jbitorto_i16 then
        unimpl
        steppc

    when jbitorto_i32 then
        unimpl
        steppc

    when jbitorto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^ ior:=x
        steppc

    when jbitxorto_i8 then
        unimpl
        steppc

    when jbitxorto_i16 then
        unimpl
        steppc

    when jbitxorto_i32 then
        unimpl
        steppc

    when jbitxorto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^ ixor:=x
        steppc

    when jshlto_i8 then
        unimpl
        steppc

    when jshlto_i16 then
        unimpl
        steppc

    when jshlto_i32 then
        unimpl
        steppc

    when jshlto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^ <<:=x
        steppc

    when jshrto_i8 then
        unimpl
        steppc

    when jshrto_i16 then
        unimpl
        steppc

    when jshrto_i32 then
        unimpl
        steppc

    when jshrto_i64 then
        x:=stack[sp--]
        pi64:=pstack[sp--]
        pi64^ >>:=x
        steppc

    when jshrto_u8 then
        unimpl
        steppc

    when jshrto_u16 then
        unimpl
        steppc

    when jshrto_u32 then
        unimpl
        steppc

    when jshrto_u64 then
        unimpl
        steppc

    when jforup_mm then
        pi64:=ref i64(getstaticaddr2)
        pi64^ +:=getstep
        if pi64^<=ref i64(getstaticaddr3)^ then
            pc:=getlabelno
        else
            steppc3
        fi

    when jforup_mf then
        unimpl

    when jforup_fm then
        pi64:=&stack[fp+getframeoffset2]
        pi64^ +:=getstep
        if pi64^<=ref i64(getstaticaddr3)^ then
            pc:=getlabelno
        else
            steppc3
        fi

    when jforup_ff then
        pi64:=&stack[fp+getframeoffset2]
        pi64^ +:=getstep
        if pi64^<=stack[fp+getframeoffset3] then
            pc:=getlabelno
        else
            steppc3
        fi

    when jforup_mi64 then
        unimpl

    when jforup_fi64 then
        pi64:=&stack[fp+getframeoffset2]
        pi64^ +:=getstep
        if pi64^<=getvalue3 then
            pc:=getlabelno
        else
            steppc3
        fi

    when jfordown_mm then
        unimpl

    when jfordown_mf then
        unimpl

    when jfordown_fm then
        unimpl

    when jfordown_ff then
        unimpl

    when jfordown_mi64 then
        unimpl

    when jfordown_fi64 then
        pi64:=&stack[fp+getframeoffset2]
        pi64^ -:=getstep
        if pi64^>=getvalue3 then
            pc:=getlabelno
        else
            steppc3
        fi

    when jto_m then
        unimpl

    when jto_f then
        pi64:=&stack[fp+getframeoffset2]
        --(pi64^)
        if pi64^ then
            pc:=getlabelno
        else
            steppc2
        fi

    when jfloat_r64_i64 then
        xstack[sp]:=stack[sp]
        steppc

    when jfloat_r64_u64 then
        xstack[sp]:=ustack[sp]
        steppc

    when jfloat_r32_i64 then
        unimpl
        steppc

    when jfloat_r32_u64 then
        unimpl
        steppc

    when jfix_i64_r64 then
        stack[sp]:=xstack[sp]
        steppc

    when jfix_u64_r64 then
        unimpl
        steppc

    when jfix_i64_r32 then
        unimpl
        steppc

    when jfix_u64_r32 then
        unimpl
        steppc

    when jfwiden then
        xstack[sp]:=getr32(stack[sp])
        steppc

    when jfnarrow then
        stack[sp]:=int@(real32(xstack[sp]))
        steppc

    when jtruncate_i8 then
        stack[sp]:=i8(stack[sp])
        steppc

    when jtruncate_i16 then
        stack[sp]:=i16(stack[sp])
        steppc

    when jtruncate_i32 then
        stack[sp]:=i32(stack[sp])
        steppc

    when jtruncate_u8 then
        stack[sp]:=u8(stack[sp])
        steppc

    when jtruncate_u16 then
        unimpl
        steppc

    when jtruncate_u32 then
        stack[sp]:=u32(stack[sp])
        steppc

    when jwiden_i8 then
        unimpl
        steppc

    when jwiden_i16 then
        unimpl
        steppc

    when jwiden_i32 then
        stack[sp]:=i32(stack[sp])
        steppc

    when jwiden_u8 then
        unimpl
        steppc

    when jwiden_u16 then
        unimpl
        steppc

    when jwiden_u32 then
        stack[sp]:=u32(stack[sp])
        steppc

    when jsqrt then
        xstack[sp]:=sqrt(xstack[sp])
        steppc

    when jsin then
        unimpl
        steppc

    when jcos then
        unimpl
        steppc

    when jtan then
        unimpl
        steppc

    when jasin then
        unimpl
        steppc

    when jacos then
        unimpl
        steppc

    when jatan then
        unimpl
        steppc

    when jlog then
        unimpl
        steppc

    when jlog10 then
        unimpl
        steppc

    when jexp then
        unimpl
        steppc

    when jround then
        unimpl
        steppc

    when jfloor then
        unimpl
        steppc

    when jceil then
        unimpl
        steppc

    when jfract then
        unimpl
        steppc

    when jatan2 then
        unimpl
        steppc

    when jbitand then
        --sp
        stack[sp] iand:=stack[sp+1]
        steppc

    when jbitor then
        --sp
        stack[sp] ior:=stack[sp+1]
        steppc

    when jbitxor then
        --sp
        stack[sp] ixor:=stack[sp+1]
        steppc

    when jshl then
        --sp
        stack[sp] <<:=stack[sp+1]
        steppc

    when jprocent then
        stack[++sp]:=fp
        fp:=sp
        sp+:=getnlocals
        if dostackcheck then
            stack[++sp]:=magic
        fi

        steppc

    when jsubent then
        unimpl
        steppc

    when jaddpxto then
        n:=stack[sp--]*getscale
        pu64:=pstack[sp--]
        pu64^+:=n
        steppc

    when jsubpxto then
        n:=stack[sp--]*getscale
        pu64:=pstack[sp--]
        pu64^-:=n
        steppc

    when jaddptrx then
        x:=stack[sp--]*getscale+getoffset   
        pi64:=pstack[sp]
        stack[sp]:=int(ref byte(pi64)+x)
        steppc

    when jsubptrx then
        x:=stack[sp--]*getscale+getoffset   
        pi64:=pstack[sp]
        stack[sp]:=int(ref byte(pi64)-x)
        steppc

    when jsubptr then
        --sp
        stack[sp]:=(ref byte(pstack[sp])-ref byte(pstack[sp+1]))/getscale
        steppc

    when jbitnot then
        stack[sp]:=inot stack[sp]
        steppc

    when jnot then
        stack[sp]:=not stack[sp]
        steppc

    when jnotnot then
        stack[sp]:=istrue stack[sp]
        steppc

    when jswitch then
        n:=stack[sp--]
        if n in getswmin..getswmax then
            pc:=pc+n-getswmin+2
            pc:=getlabelno
        else
            pc:=getlabelno2
        fi

    when jswlabel then
        unimpl
        steppc

    when jloadbit then
        n:=stack[sp--]
        stack[sp]:=stack[sp]>>n iand 1
        steppc

    when jstorebit then
        n:=stack[sp--]
        pi64:=pstack[sp--]
        x:=stack[sp--]
        pi64^:=(pi64^ iand inot(1<<n)) ior ((x iand 1)<<n)
        steppc

    when jloadbf then
        j:=stack[sp--]
        i:=stack[sp--]
        if i>j then swap(i,j) fi
        stack[sp]:=(stack[sp]>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
        steppc

    when jstorebf then
        j:=stack[sp--]
        i:=stack[sp--]
        if i>j then swap(i,j) fi
        pi64:=pstack[sp--]
        x:=stack[sp--]
        storebit(pi64,i,j,x)
        steppc

    when junload then
        --sp
        steppc

    when jdouble then
        stack[sp+1]:=stack[sp]
        ++sp
        steppc

    when jdupl then
        unimpl
        steppc

    when jswapopnds then
        swap(stack[sp], stack[sp-1])
        steppc

    when jcallp, jcallf then
        if dostackcheck then
            if sp>stacksize-10 then pcerror("Stack overflow") fi
        fi
        stack[++sp]:=pc+1
        pc:=getprocindex

    when jicallp then
        if stack[sp]>1'000'000 then
            pi64:=pstack[sp--]
            n:=getnargs
            sp-:=n-1                !point to first arg
            docalldll(0, pi64, cast(&stack[sp]), n, getnvars, 0)
            --sp
            steppc
        else
            x:=pc+1
            pc:=stack[sp]
            stack[sp]:=x
        fi

    when jicallf then
        if stack[sp]>1'000'000 then
            pi64:=pstack[sp--]
            n:=getnargs
            sp-:=n-1                !point to first arg
            x:=docalldll(0, pi64, cast(&stack[sp]), n, getnvars, 1)
            stack[sp]:=x
            steppc
        else
            x:=pc+1
            pc:=stack[sp]
            stack[sp]:=x
        fi

    when jcalldllp then
        n:=getnargs
        sp-:=n-1                !point to first arg
        docalldll(getdef.importno, nil, cast(&stack[sp]), n, getnvars, 0)
        --sp
        steppc

    when jcalldllf then
        n:=getnargs
        sp-:=n-1

        x:=docalldll(getdef.importno, nil, cast(&stack[sp]), n, getnvars, 1)
        stack[sp]:=x
        steppc

    when jsetret_m then
        pi8:=pstack[fp+getparam1]
        memcpy(pi8, pstack[sp], getmemsize)
        pstack[sp]:=pi8             !return ref to dest block
        steppc

    when jreturn_p then
        if dostackcheck then
            if stack[sp--]<>magic then pcerror("retp: stack error:") fi
        fi

        n:=getnparams
        sp-:=getnlocals
        fp:=stack[sp--]
        pc:=stack[sp--]
        sp-:=n

    when jreturn_f then
        if dostackcheck then
            if stack[sp-1]<>magic then pcerror("retf: stack error:") fi
            stack[sp-1]:=stack[sp]
            --sp
        fi
        x:=stack[sp]
        n:=getnparams
        sp-:=getnlocals
        fp:=stack[--sp]
        pc:=stack[--sp]
        sp-:=n
        stack[sp]:=x

    when jreturn_m then
        a:=getnretvals
        spr:=sp-a+1             !point to base of ret vals in stack
        sp-:=getnlocals+a       !sp points to pushed fp
        n:=getnparams
        fp:=stack[sp--]
        pc:=stack[sp--]
        sp-:=n
        for i to a do           !replace ret/fp on wards by ret values
            stack[++sp]:=stack[spr+i-1]
        od

    when jjump then
        pc:=getlabelno

    when jijump then
        pc:=stack[sp--]

    when jstartmx then
        unimpl
        steppc

    when jresetmx then
        unimpl
        steppc

    when jendmx then
        unimpl
        steppc

    when jstop then
        return stack[sp]
        exit

    when jassem then
        unimpl
        steppc

    when jprinti64 then
        println stack[sp--]
        steppc

    when jprintu64 then
        unimpl
        steppc

    when jprintr64 then
        unimpl
        steppc

    when jprintr32 then
        println getr32(stack[sp--])
        steppc

    when jprintstr then
        println ichar(stack[sp--])
        steppc

    when jprinthex then
        println stack[sp]:"h", stack[sp]
        --sp
        steppc

    when jprintsp then
        println =SP
        steppc

    when jtest then
        steppc
    
    when jopnd then
        steppc

    when jdebug then
        case getx
        when 0,1 then
            debug:=getx
            println "Debug set to", debug
        when 2 then
            println =SP
        esac
        steppc

    else
unimpl::
        println
        fprintln "Unimpl: # (#) on line: #", jcodenames[getopcode],
                 pclnames[pcltable[pc].opcode], pcllines[pc]
        println
        stop 1
    end doswitch
    0
end

global proc runpcl=
    int tt:=clock()
    int stopcode

    if pcmain=0 then
        pcerror3("'main' not present")
    fi
    if undefflag then
        pcerror3("Some names/labels undefined")
    fi
    updatedatarefs()

    loadlibs()
    docmdskip()

!   println "Start:"

    stopcode:=exec(pcmain, 0)

    tt:=clock()-tt
!   println "\nFinished execution",tt,=stopcode
!   println
end
