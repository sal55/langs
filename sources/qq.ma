=== MA 43 ===
=== qq.m 0 0 1/43 ===
    module qqcli
    module qq_arrays
    module qq_bits
    module qq_calldll
    module qq_decimal
    module qq_decls
    module qq_dicts

    module qq_jhandlers

    module qq_khandlers
    module qq_host
    module qq_lex
    module qq_lib
    module qq_lists
    module qq_newmodules
    module qq_names
    module qq_optim
    module qq_packed
    module qq_parse
    module qq_print
    module qq_pclgen
    module qq_pcllib
    module qq_records
    module qq_resolve
    module qq_sets
    module qq_strings
    module qq_syslibs
    module qq_tables
    module qq_show
    module qq_vars
    module qq_mtypes

=== qqcli.m 0 0 2/43 ===


global enumdata []ichar runnames =
    (header_cc,     $),
    (load_cc,       $),
    (parse_cc,      $),
    (names_cc,      $),
    (gencode_cc,    $),
    (fixup_cc,      $),
    (run_cc,        $),
end

global byte fshowpcl1
global byte fshowpcl2
global byte fshowpcl3
global byte fshowast1
global byte fshowast2
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte foptimise
global byte fwriteqa            
global byte fshowmodules

global byte runcode  = run_cc

MACRO SHOWTIME(MESS) = EVAL 0

global ichar sourcestr

global ichar inputfile

global const maxstatic=11000
global [maxstatic]variant statictable
global [maxstatic]symbol staticdefs
global int nstatics

global const maxproc=11000              
global [maxproc]ref int proctable
global [maxproc]symbol procdefs
global int nprocs


int cliruncode=run_cc

enumdata []ichar optionnames=
    (header_sw,     "header"),
    (load_sw,       "load"),
    (parse_sw,      "parse"),
    (names_sw,      "names"),
    (gen_sw,        "gen"),
    (opt_sw,        "opt"),
    (asmopt_sw,     "asmopt"),
    (fixup_sw,      "fixup"),
    (run_sw,        "run"),

    (ast1_sw,       "ast1"),
    (ast2_sw,       "ast2"),
    (pcl1_sw,       "pcl1"),
    (pcl2_sw,       "pcl2"),
    (pcl3_sw,       "pcl3"),
    (st_sw,         "st"),
    (stflat_sw,     "stflat"),
    (types_sw,      "types"),
    (showmodules_sw,"modules"),
    (showall_sw,    "show"),

    (fn_sw,         "fn"),
    (sw_sw,         "sw"),
    (asm_sw,        "asm"),
    (debug_sw,      "debug"),
    (fdebug_sw,     "fdebug"),
    (lab_sw,        "lab"),
    (ext_sw,        "ext"),
    (qa_sw,         "qa"),
    (qas_sw,        "qas"),
    (verbose_sw,    "v"),
    (docs_sw,       "docs"),
    (alldocs_sw,    "alldocs"),
    (string_sw,     "p"),


    (nosys_sw,      "nosys"),
end





int cmdstartindex                   

ref strbuffer pclstr

proc main=
    ichar source
    int i,nnames,t,tstart, stopcode
    unit p
    static []ichar params=("ABC","DEF")


    getinputoptions()

    if fverbose then
        println dispatchnames[dispatchtype]
    fi


    stopcode:=runqprogram(cliruncode,inputfile,cmdparams[cmdstartindex..ncmdparams])

    showlogfile()

    stop stopcode
end

proc initcli=
    remove("AST1")
    remove("AST2")
    remove("PCL1")
    remove("PCL2")
    remove("ST")
    os_initwindows()
end

proc browsefile(ichar filename)=
    array [300]char str

    fprint @str,"\\m\\med.bat ",filename

    os_execwait(str,1,nil)
end

proc showtiming(int ticks, lines)=

end

proc getinputoptions=
    int paramno,pmtype
    ichar name,value
    ichar appstr, appname
    ref function:ichar fnaddr

    fnaddr:=findfunction("getbuiltin_app")

    paramno:=1

    while pmtype:=nextcmdparamnew(paramno,name,value,"q") do
        case pmtype
        when pm_option then
            convlcstring(name)
            for sw to optionnames.len do
                if eqstring(name,optionnames[sw]) then
                    do_option(sw,value)
                    exit
                fi
            else
                println "Unknown option:",name
                stop 99
            od
        when pm_sourcefile then
            if fnaddr then              
                --paramno
                exit
            fi
            inputfile:=pcm_copyheapstring(name)
            exit                
        esac
    od

    if fnaddr then
        appstr:=fnaddr()
LOADERROR("DO BUILT-IN")

    elsif not inputfile then
        println "Q Interpreter I [mm4]"
        println "Usage:"
        println "   ",,sysparams[1],"filename[.q]"
        stop
    fi

    if dispatchtype in [debug_dispatch,fdebug_dispatch] then
        hasbytecodes:=1
    else
        hasbytecodes:=0
    fi

    cmdstartindex:=paramno



    if fwritedocs then
        docsdev:=fopen(changeext(inputfile,"txt"),"wb")
    fi

    if dispatchtype=asm_dispatch and not asmavailable() then
        dispatchtype:=fn_dispatch
    fi

end

proc do_option(int sw, ichar value)=

case sw
when header_sw then cliruncode:=header_cc
when load_sw then cliruncode:=load_cc
when parse_sw then cliruncode:=parse_cc
when names_sw then cliruncode:=names_cc
when gen_sw then cliruncode:=gencode_cc
when fixup_sw then cliruncode:=fixup_cc
when run_sw then cliruncode:=run_cc

when opt_sw then foptimise:=1

when ast1_sw then fshowast1:=1
when ast2_sw then fshowast2:=1
when pcl1_sw then fshowpcl1:=1
when pcl2_sw then fshowpcl2:=1
when pcl3_sw then fshowpcl3:=1
when st_sw then fshowst:=1
when stflat_sw then fshowstflat:=1
when types_sw then fshowtypes:=1
when showmodules_sw then fshowmodules:=1

when showall_sw then
    fshowast1:=1
    fshowast2:=1
    fshowpcl1:=1
    fshowpcl2:=1
    fshowpcl3:=1
    fshowst:=1
    fshowstflat:=1
    fshowtypes:=1
    fshowmodules:=1

when fn_sw then dispatchtype:=fn_dispatch
when asm_sw then dispatchtype:=asm_dispatch
when debug_sw then dispatchtype:=debug_dispatch
when fdebug_sw then dispatchtype:=fdebug_dispatch
when lab_sw then dispatchtype:=lab_dispatch

when asmopt_sw then foptimise:=1; dispatchtype:=asm_dispatch

when ext_sw then usesyslibs:=0
when qa_sw then fwriteqa:=1
when qas_sw then fwriteqa:=2
when verbose_sw then fverbose:=1
when docs_sw then
    fwritedocs:=1
    cliruncode:=names_sw
when alldocs_sw then
    fwritedocs:=2
    cliruncode:=names_sw

when nosys_sw then fnosys:=1
when string_sw then
    sourcestr:=pcm_copyheapstring(value)
    inputfile:="$"

esac

end

global function runqprogram(int run,ichar filename, slice[]ichar qparams)int =
    int t, tstart, stopcode




    inputfile:=filename
    runcode:=run


    initprogram()

    readprojectfile(filename)


    load_program()

T:=CLOCK()
    parse_program()


    rx_program()

    writeqa_program()

    gxpcl_program()
    optimise_program()

    fixup_program()
    stopcode:=runprogram(qparams)

    return stopcode
end

proc load_program=
    ichar qafile

    if runcode<load_cc then return fi


    loadmodules()



end

proc parse_program=
    int m

    return when runcode<parse_cc

    m:=1

    for m to nmodules do
        parsemodule(m)
    od

    fixusertypes()

    if fshowast1 then
        showast("AST1")
    fi

end

proc rx_program=
    return when runcode<names_cc

    tx_typetable()

    for i to nmodules do
        rx_module(i)
    od


    if fshowast2 then
        showast("AST2")
    fi
end

proc gxpcl_program=
    return when runcode<gencode_cc
    for i:=1 to nmodules do
        gencodemodule(i)
    od

    if fshowpcl1 then
        showpcl(1)
    fi
end

proc optimise_program=

    if not foptimise or dispatchtype<>asm_dispatch then
        fshowpcl2:=0
        return
    fi

    for i to nmodules do
        optimise_module(i)
    od
    if fshowpcl2 then
        showpcl(2)
    fi
end

proc fixup_program=
    return when runcode<fixup_cc


    initkhandlers()
    if dispatchtype=asm_dispatch then
        initjhandlers()
    fi

    for i to nmodules do
        fixupmodule(i)
    od

    if fshowpcl3 then
        showpcl(3)
    fi
end

proc writeqa_program=
    array [300]char filename
    array [maxsourcefile]int sflist
    filehandle f
    int offset, nfiles, fileno

    if not fwriteqa then
        return
    fi
    strcpy(filename, changeext(sourcefilespecs[1],"qa"))

    nfiles:=0

    for i to nsourcefiles do
        if sourcefilesys[i] and fwriteqa=1 then     
            next
        fi

        sflist[++nfiles]:=i
    od

    if nfiles=0 then loaderror("QA:no files") fi

    f:=fopen(filename,"wb")
    if not f then loaderror("Can't create qa file #",filename) fi

    println "Writing ",filename
    fprintln @f,"=== QA # ===",nfiles

    for i to nfiles do
        fileno:=sflist[i]

        fprintln @f,"=== # # # #/# ===",
            sourcefilenames[fileno],
            sourcefilesys[fileno],
            sourcefilesupport[fileno],
            i,nfiles

        offset:=getfilepos(f)
        writerandom(f,cast(sourcefiletext[fileno]),offset,sourcefilesizes[fileno])
    od

    println @f,"=== END ==="

    for i to nfiles do
        fprintln @f,"# #",i,sourcefilenames[sflist[i]]
    od

    fclose(f)
    stop
end


proc initprogram=



    lexinit()
    inithostlib()

    os_initwindows()
end

proc fixproc(symbol d)=
    ref int z
    variant p

    if not d.procfixed then
        if nprocs>=maxproc then gerror("Too many procs") fi
        z:=moduletable[d.moduleno].pcstart+d.labelno
        p:=pcm_alloc(varsize)
        proctable[++nprocs]:=z
        procdefs[nprocs]:=d
        d.pcaddress:=z
        d.procfixed:=1
    fi

end

proc fixupmodule(int m)=
    ref int pc,pcstart,z
    int cmd,y
    symbol d
    variant p

    setcmdmap()

    pc := pcstart := moduletable[m].pcstart

    repeat
        cmd:=pc^
        pc^:=cast(cmdmap[cmd])          
        ++pc

        case cmd
        when kprocdef then
            fixproc(cast(pc^))
        esac

        for i to pclnopnds[cmd] do
            switch pclfmt[cmd,i]
            when cproc then
                d:=cast(pc^)
                fixproc(d)
                pc^:=cast(d.pcaddress)
            when cmemory then
                d:=cast(pc^)
                if d.varptr=nil then
                    if nstatics>=maxstatic then gerror("Too many statics") fi
                    p:=pcm_alloc(varsize)
                    statictable[++nstatics]:=p
                    staticdefs[nstatics]:=d
                    d.varptr:=p
                fi
                pc^:=cast(d.varptr)
            when cframe then
                d:=cast(pc^)
IF D=NIL THEN LOADERROR("D NIL") FI
                pc^:=d.index*16
            when cgenfield then
                pc^:=symbol(pc^).genfieldindex
            when cstring then
                pc^:=cast(obj_make_string(cast(pc^),0))
            when clabel then
                y:=int(pcstart+pc^)
                pc^:=y

            end
            ++pc
        od
    until cmd=kendmodule


end

function runprogram(slice[]ichar cmds)int=
    ref proc fnptr
    int cmd,SS
    ref int pcstart

    return 0 when runcode<run_cc

    for i to cmds.len do
        setcmdparam(i,cmds[i])
    od

    sptr:=&varstack[1]
    stacklimit:=&varstack[stacksize-100]
    pcstart:=pcptr:=moduletable[mainmoduleno].pcstart
    pcerrorpos:=0

    disploop()
    return sptr.value
end

proc disploop=

    case dispatchtype
    when fn_dispatch then
        disploop_fn()

    when debug_dispatch then
        disploop_deb(0)

    when fdebug_dispatch then
        disploop_deb(1)

    when asm_dispatch then
        disploop_asm()

    else
        loaderror("Dispatch not supported: ##",dispatchnames[dispatchtype])
    esac
end


proc disploop_fn=
    type fnptr = ref proc

    repeat
        fnptr(pcptr^)^()
    until stopped
end

proc disploop_deb(int fdeb)=
    fdebug:=fdeb    


    repeat
        khandlertable[pcptr^]^()
    until stopped
end

proc setcmdmap=
    for i:=1 to klastpcl do
        case dispatchtype
        when fn_dispatch then
            cmdmap[i]:=khandlertable[i]
        when debug_dispatch, fdebug_dispatch then
            cmdmap[i]:=cast(i)
        when asm_dispatch then
            cmdmap[i]:=jhandlertable[i]
        esac
    od
end

global function runproc_m(ref void amsg)int=
    varrec a,b,dest
    static int rmsg_typeno
    int i,result
    objrec obj


    if rmsg_typeno=0 then
        for i to ntypes do
            if eqstring(ttname[i],"ws_msg64") then
                rmsg_typeno:=i
                exit
            fi
        od
    fi
    if rmsg_typeno=0 then
        abortprogram("mainwndproc: can't find rmsg")
    fi

    memset(&obj,0,objrec.bytes)
    obj.refcount:=99
    obj.ptr:=ref byte(amsg)
    obj.usertag:=rmsg_typeno

    a.tagx:=tstruct ior hasrefmask
    a.objptr:=&obj



++PCLLEVEL
    runproc(pcl_callbackfn,&a,nil,&dest)
--PCLLEVEL
    result:=dest.value

    result:=0           

    return result
end

global proc runproc(ref void fnptr,variant a,b,dest) =

    variant oldsptr
    ref byte oldframeptr
    ref int oldpcptr
    byte oldstopped
    int nparams



    dest.tagx:=tint
    dest.value:=0

    oldstopped:=stopped     
    oldpcptr:=pcptr
    oldsptr:=sptr
    oldframeptr:=frameptr

    (++sptr).tagx:=999              

    if b and b.tag then         
        nparams:=2
        (++sptr)^:=a^
        (++sptr)^:=b^
    elsif a and a.tag then
        nparams:=1
        (++sptr)^:=a^
    else
        nparams:=0
    fi
    (++sptr).tagx:=tretaddr

    sptr.retaddr:=stopseq







    sptr.frameptr_low:=int(frameptr)
    frameptr:=cast(sptr)
    pcptr:=fnptr

    disploop()



    if (sptr-11).tag=tretaddr then      
        dest^:=sptr^
    else                                
    --SPTR
        dest^:=sptr^                    

        if dest.tag=tvoid then      
            dest.tagx:=tint
            dest.value:=0
        fi
    fi

    pcptr:=oldpcptr
    stopped:=oldstopped

    sptr:=oldsptr           
    frameptr:=oldframeptr   
    stopped:=oldstopped
end

=== qq_arrays.m 0 0 3/43 ===
global proc var_empty_array(int tag, elemtype, lower, variant dest)=
    dest.objptr:=obj_newarray(elemtype,lower, 0)
    dest.tagx:=tag ior hasrefmask
end

global proc obj_free_array(object p)=
    if p.length then
        pcm_free(p.ptr, p.alloc64*ttsize[p.elemtag])
    fi

    pcm_free32(p)
end

global proc obj_free_userarray(object p)=
    if p.length then
        pcm_free(p.ptr,ttsize[p.usertag])
    fi

    pcm_free32(p)
end

global proc var_make_array(variant a, dest, int lower, n, axtype, elemtype) =
    object p
    ref byte q
    int m


    if n then
        if elemtype=tvoid then          
            case (a+n-1).tag
            when tint then elemtype:=ti64
            when treal then elemtype:=tr64
            when tword then elemtype:=tu64
            else
                elemtype:=ti64
            esac
            m:=0
        else
            m:=ttlength[axtype]
            if n<m then
                pcerror("Too few elements")
            elsif n>m then
                println =n,=m
                pcerror("Too many elements")
            fi
        fi
    elsif elemtype=tvoid then
        elemtype:=ti64
    fi

    p:=obj_newarray(elemtype,lower,n)
    q:=p.ptr

    to n do
        var_storepacked(q,a,elemtype)
        q+:=ttsize[elemtype]
        ++a
    od

    if axtype=tarray then
        dest.tagx:=tarray ior hasrefmask
    else
        dest.tagx:=tuserarray ior hasrefmask
        p.usertag:=axtype
    fi
    dest.objptr:=p
end

global function obj_newarray(int elemtype, lower,length)object p=

    ref byte q
    int elemsize

    p:=obj_new()
    p.mutable:=1
    if lower in 0..1 then
        p.lower:=lower
    else
        pcerror("Lwb not 0/1")
    fi
    p.length:=length
    p.objtype:=normal_obj
    p.elemtag:=elemtype
    elemsize:=ttsize[elemtype]

    if length then
        p.ptr:=pcm_allocz(length*elemsize)
        p.alloc64:=allocbytes/elemsize
    fi

    return p
end

global function obj_newarray_u(int usertag)object p=

    ref byte q
    int elemsize

    p:=obj_new()
    p.mutable:=1
    p.objtype:=normal_obj
    p.usertag:=usertag
    elemsize:=ttsize[tttarget[usertag]]

    if ttlength[usertag] then
        p.ptr:=pcm_allocz(ttsize[usertag])
        p.alloc64:=allocbytes/elemsize
    fi

    return p
end

global proc var_getix_array(variant a, int index)=
    varrec v
    object p
    int elemtype,length

    v:=a^
    p:=a.objptr

    if v.tag=tuserarray then
        length:=ttlength[p.usertag]
        index-:=ttlower[p.usertag]
        elemtype:=tttarget[p.usertag]
    else
        length:=p.length
        elemtype:=p.elemtag
        index-:=p.lower
    fi

    if u64(index)>=u64(length) then
        pcerror("ax[int] bounds")
    fi

    if elemtype=tu8 then
        a.tagx:=tint
        a.value:=(p.ptr+index)^
    else
        var_loadpacked(p.ptr+index*ttsize[elemtype],elemtype, a)
    fi

end

global proc var_putix_array(variant a, int index, variant x)=
    varrec v
    object p
    int elemtype, length, lower

    v:=a^
    p:=v.objptr

    if v.tag=tuserarray then
        length:=ttlength[p.usertag]
        lower:=ttlower[p.usertag]
        elemtype:=tttarget[p.usertag]
    else
        length:=p.length
        lower:=p.lower
        elemtype:=p.elemtag
    fi

    index-:=lower


    if u64(index)>=u64(length) then
        if index<0 then
            pcerror("lwb")
        elsif index=length then
            if v.tag=tuserarray then
                pcerror("Can't append user type")
            fi
            obj_append_array(p,x)
        else
            pcerror("ax[i]:=x bounds")
        fi
    fi

    if elemtype=tu8 then
        if x.tag<>tint then pcerror("rhs not int") fi
        a.tagx:=tint
        (p.ptr+index)^:=x.value
    else
        var_storepacked(p.ptr+index*ttsize[elemtype],x,elemtype)
    fi
end

global proc var_getixref_array(variant a, int index)=
    varrec v
    object p
    int elemtype, length, lower

    v:=a^
    p:=v.objptr

    if v.tag=tuserarray then
        length:=ttlength[p.usertag]
        lower:=ttlower[p.usertag]
        elemtype:=tttarget[p.usertag]
    else
        length:=p.length
        lower:=p.lower
        elemtype:=p.elemtag
    fi

    index-:=lower

    if u64(index)>=u64(length) then
        if index<0 then
            pcerror("lwb")
        else
            if u64(index)=u64(length) then
PCERROR("PUTIXREF NEEDS IAPPEND")
                p:=a.objptr
            else
                pcerror("ax[i]:=x bounds")
            fi
        fi
    fi

    a.tagx:=trefpack
    a.elemtag:=elemtype
    a.ptr:=p.ptr+index*ttsize[elemtype]
end

proc obj_append_array(object a, variant x)=
    int n
    ref byte q

    if a.objtype<>normal_obj then
        pcerror("Can't extend slice")
    fi

    if not a.mutable then
        pcerror("Not mutable")
    fi

    n:=a.length+1           

    if n>a.alloc64 then     
        obj_resize_array(a,n)
    else
        a.length:=n
    fi

    q:=a.ptr+(n-1)*ttsize[a.elemtag]

    var_storepacked(cast(q), x, a.elemtag)
end

global proc var_appendto_array(variant a, x)=
    obj_append_array(a.objptr,x)
end

global proc obj_resize_array(object p,int n)=
    ref byte q
    int elemsize

    elemsize:=ttsize[p.elemtag]

    if n<=p.alloc64 then
        p.length:=n
    else
        q:=pcm_alloc(n*elemsize)
        if p.length then
            memcpy(q,p.ptr,p.length*elemsize)
            pcm_free(p.ptr,p.alloc64*elemsize)
        fi
        p.ptr:=q
        p.length:=n
        p.alloc64:=allocbytes/elemsize
    fi
end

global proc var_dupl_array(variant a)=
    object p,q
    int elemsize

    p:=a.objptr
    q:=obj_newarray(p.elemtag, p.lower, p.length)
    a.objptr:=q

    if p.length then
        memcpy(q.ptr, p.ptr,
            p.length*ttsize[p.elemtag])
    fi
end

global proc var_dupl_userarray(variant a)=
    object p,q
    int elemsize,length

    p:=a.objptr
    length:=ttlength[p.usertag]
    q:=obj_newarray_u(p.usertag)
    a.objptr:=q


    if length then
        memcpy(q.ptr, p.ptr,ttsize[p.usertag])
    fi
end

global function var_equal_array(variant a,b)int=
    object p:=a.objptr
    object q:=b.objptr
    int length,elemsize:=p.elemtag

    if p.elemtag<>q.elemtag then
        return 0
    fi
    length:=p.length

    if length<>q.length then
        return 0
    fi
    if length=0 then return 1 fi

    return eqbytes(p.ptr, q.ptr, ttsize[p.elemtag]*length)
end

global proc var_concatto_array(variant a,b)=
    ref byte d
    int n,alen,blen,newlen,oldbytes,newbytes,elemsize
    variant v
    object pa,pb

    pa:=a.objptr
    pb:=b.objptr

    if not pa.mutable then
        pcerror("concatarray/not mut")
    fi

    if pa.elemtag<>pb.elemtag then
        pcerror("concat/not compat")
    fi
    elemsize:=ttsize[pa.elemtag]

    alen:=pa.length
    blen:=pb.length

    if alen=0 then                  
        if blen then                
            obj_resize_array(pa,blen)
            d:=pa.ptr
            memcpy(d,pb.ptr,blen*elemsize)
        fi
    elsif blen then                 
        newlen:=alen+blen
        obj_resize_array(pa,newlen)
        d:=pa.ptr+alen*elemsize
        memcpy(d,pb.ptr,blen*elemsize)
    fi
end

global proc var_getslice_array(variant a, int i,j)=
    int alower,elemsize
    object p,q

    p:=a.objptr

    alower:=p.lower
    elemsize:=ttsize[p.elemtag]

    if i<alower or j>p.length+alower-1 or i>j then
        pcerror("array/slice bounds")
    fi

    q:=obj_new()

    q.objtype:=slice_obj
    q.mutable:=p.mutable
    q.lower:=1
    q.ptr:=p.ptr+(i-alower)*elemsize
    q.elemtag:=p.elemtag

    case p.objtype
    when slice_obj then             
        q.objptr2:=p.objptr2        
        obj_shareu(q.objptr2)

    when extslice_obj then
        q.objptr2:=nil
        q.objtype:=extslice_obj
    else
        q.objptr2:=p                
        ++p.refcount
    esac

    q.length:=j-i+1
    a.objptr:=q
end

global proc var_putslice_array(variant a, int i,j, variant x)=
    ref byte r,s
    object p,q
    int length,sublength,elemsize

    if a.tag=tuserarray then
        pcerror("userax/putslice")
    fi
    p:=a.objptr
    if not p.mutable then pcerror("Not mutable") fi
    length:=p.length

    if i<1 or j>p.length or i>j then
        pcerror("array/slice bounds")
    fi

    sublength:=j-i+1

    q:=x.objptr
    if q.length<sublength then
        pcerror("substr too short")
    fi
    if p.elemtag<>q.elemtag then
        pcerror("Not compat")
    fi
    elemsize:=ttsize[p.elemtag]

    r:=p.ptr+(i-1)*elemsize
    s:=q.ptr
    memcpy(r,s,sublength*elemsize)
end

=== qq_bits.m 0 0 4/43 ===
global proc obj_free_bits(object p, int tag)=
    if p.length then
        pcm_free(p.ptr, getbitssize(p.alloc64, p.elemtag))
    fi

    pcm_free32(p)
end

global proc var_make_bits(variant a, dest, int lower, n, bxtype, elemtype) =
    object p
    ref byte q
    int bitwidthx,offset

    p:=obj_newbits(elemtype,lower,n)
    q:=p.ptr

    bitwidthx:=ttbitwidth[elemtype]
    offset:=0

    to n do
        var_storebit(q,offset,a,elemtype,bitwidthx)
        offset+:=bitwidthx
        if offset>=8 then
            ++q
            offset:=0
        fi
        ++a
    od

    dest.tagx:=bxtype ior hasrefmask
    dest.objptr:=p
end

global function obj_newbits(int elemtype, lower,length)object p=

    ref byte q
    int nbits,bitwidthx,nbytes

    p:=obj_new()
    p.mutable:=1
    p.lower16:=lower
    p.length:=length
    p.objtype:=normal_obj
    p.elemtag:=elemtype

    if length then
        nbytes:=getbitssize(length, elemtype)
        p.ptr:=pcm_allocz(nbytes)
        p.alloc64:=allocbytes*(8/ttbitwidth[elemtype])
    fi

    return p
end

global proc var_getix_bits(variant a, int index)=
    object p
    ref byte q
    int elemtype,offset,shift

    p:=a.objptr
    elemtype:=p.elemtag
    index-:=p.lower16

    if u64(index)>=u64(p.length) then
        pcerror("ax[int] bounds")
    fi
    q:=p.ptr
    a.tagx:=tint

    index+:=p.indexoffset

    switch p.elemtag
    when tu1 then
        a.value:=not not ((q+index>>3)^ iand (1<<(index iand 7)))
    when tu2 then
        shift:=(index iand 3)*2
        a.value:=((q+index>>2)^ iand (3<<shift))>>shift
    when tu4 then
        shift:=(index iand 1)*4
        a.value:=((q+index>>1)^ iand (15<<shift))>>shift
    else
        pcustype_t("bitix",p.elemtag)
    end
end

global proc var_putix_bits(variant a, int index, variant x)=
    object p
    ref byte q
    int elemtype, newoffset

    p:=a.objptr
    elemtype:=p.elemtag

    index-:=p.lower16

    if u64(index)>=u64(p.length) then
        if index<0 then
            pcerror("lwb")
        elsif index=p.length then
            obj_append_bits(p,x)
        else
            pcerror("bx[i]:=x bounds")
        fi
    fi

    q:=getindexoffset(p.ptr, p.indexoffset, index, elemtype, newoffset)
    var_storebit(q, newoffset*ttbitwidth[elemtype],x,elemtype,0)
end

global proc var_getixref_bits(variant a, int index)=
    varrec v
    object p
    ref byte q
    int offset, newoffset
    int elemtype

    p:=a.objptr
    elemtype:=p.elemtag
    index-:=p.lower16

    if u64(index)>=u64(p.length) then
        pcerror("&bx[i] bounds")
    fi

    q:=getindexoffset(p.ptr, p.indexoffset, index, elemtype, newoffset)

    a.tagx:=trefbit
    a.elemtag:=elemtype
    a.ptr:=q
    a.bitoffset:=newoffset*ttbitwidth[elemtype]
end

function getindexoffset(ref byte p, int offset, index, t, &newoffset)ref byte=

    index+:=offset

    switch t
    when tu1 then
        p+:=index>>3                
        newoffset:=index iand 7
    when tu2 then
        p+:=index>>2
        newoffset:=index iand 3
    when tu4 then
        index+:=offset>>2
        p+:=index>>1
        newoffset:=index iand 1
    end

    return p
end

proc obj_append_bits(object a, variant x)=
    int n, newoffset, elemtype
    ref byte q

    if a.objtype<>normal_obj then
        pcerror("Can't extend slice")
    fi

    if not a.mutable then
        pcerror("Not mutable")
    fi

    n:=a.length+1           
    elemtype:=a.elemtag

    if n>a.alloc64 then     
        obj_resize_bits(a,n)
    else
        a.length:=n
    fi

    q:=getindexoffset(a.ptr, a.indexoffset, n-a.lower16, elemtype, newoffset)
    var_storebit(q,newoffset*ttbitwidth[elemtype],x,elemtype, 0)
end

global proc var_appendto_bits(variant a, x)=
    obj_append_bits(a.objptr,x)
end

global proc obj_resize_bits(object p,int n)=
    ref byte q
    int newsize,elemtype

    elemtype:=p.elemtag

    if n<=p.alloc64 then
        p.length:=n
    else
        newsize:=getbitssize(n,elemtype)
        q:=pcm_alloc(newsize)
        if p.length then
            memcpy(q,p.ptr, bits_bytesize(p))
            pcm_free(p.ptr, getbitssize(p.alloc64, elemtype))
        fi
        p.ptr:=q
        p.length:=n
        p.alloc64:=allocbytes*(8/ttbitwidth[elemtype])
    fi
end

global proc var_dupl_bits(variant a)=
    object p,q
    int elemsize

    p:=a.objptr

    q:=obj_newbits(p.elemtag, p.lower16, p.length)
    q.indexoffset:=p.indexoffset

    a.objptr:=q

    if p.length then
        memcpy(q.ptr, p.ptr, bits_bytesize(p))
    fi
end

global function var_equal_bits(variant a,b)int=
    object p:=a.objptr
    object q:=b.objptr
    int length,elemsize:=p.elemtag

    if p.elemtag<>q.elemtag then
        return 0
    fi
    length:=p.length

    if length<>q.length then
        return 0
    fi
    if length=0 then return 1 fi

    return eqbytes(p.ptr, q.ptr, bits_bytesize(p))
end

global proc var_concatto_bits(variant a,b)=
    ref byte d
    int n,alen,blen,newlen,oldbytes,newbytes,elemsize
    variant v
    object pa,pb

PCERROR_S("VAR/BITS/NOT READY",$FUNCTION)
    pa:=a.objptr
    pb:=b.objptr

    if not pa.mutable then
        pcerror("concatarray/not mut")
    fi

    if pa.elemtag<>pb.elemtag then
        pcerror("concat/not compat")
    fi
    elemsize:=ttsize[pa.elemtag]

    alen:=pa.length
    blen:=pb.length

    if alen=0 then                  
        if blen then                
            obj_resize_bits(pa,blen)
            d:=pa.ptr
            memcpy(d,pb.ptr,blen*elemsize)
        fi
    elsif blen then                 
        newlen:=alen+blen
        obj_resize_bits(pa,newlen)
        d:=pa.ptr+alen*elemsize
        memcpy(d,pb.ptr,blen*elemsize)
    fi
end

global proc var_getslice_bits(variant a, int i,j)=
    int alower,elemtype,newoffset
    object p,q

    p:=a.objptr

    alower:=p.lower16
    elemtype:=p.elemtag

    if i<alower or j>p.length+alower-1 or i>j then
        pcerror("bits/slice bounds")
    fi

    q:=obj_new()

    q.objtype:=slice_obj
    q.mutable:=p.mutable
    q.lower16:=1
    q.elemtag:=elemtype

    q.ptr:=getindexoffset(p.ptr, p.indexoffset, i-alower, elemtype, newoffset)
    q.indexoffset:=newoffset

    case p.objtype
    when slice_obj then             
        q.objptr2:=p.objptr2        
        obj_shareu(q.objptr2)

    when extslice_obj then
        q.objptr2:=nil
        q.objtype:=extslice_obj
    else
        q.objptr2:=p                
        ++p.refcount
    esac

    q.length:=j-i+1
    a.objptr:=q
end

global proc var_putslice_bits(variant a, int i,j, variant x)=
    ref byte pp,qq
    object p,q
    int length,sublength,elemtype,offsetp,offsetq,bitwidthx
    varrec v

    p:=a.objptr
    if not p.mutable then pcerror("Not mutable") fi
    length:=p.length
    elemtype:=p.elemtag

    if i<1 or j>p.length or i>j then
        pcerror("bits/slice bounds")
    fi

    sublength:=j-i+1

    q:=x.objptr
    if q.length<sublength then
        pcerror("substr too short")
    fi
    if p.elemtag<>q.elemtag then
        pcerror("Not compat")
    fi

    bitwidthx:=ttbitwidth[elemtype]

    pp:=getindexoffset(p.ptr, p.indexoffset, i-p.lower16, elemtype, offsetp)
    qq:=getindexoffset(q.ptr, q.indexoffset, 0, elemtype, offsetq)
    offsetq*:=bitwidthx
    offsetq*:=bitwidthx

    to sublength do
        var_loadbit(qq, offsetq, elemtype,0, &v)
        var_storebit(pp, offsetp, &v, elemtype,0)
        offsetp+:=bitwidthx
        if offsetp>=8 then ++pp; offsetp:=0 fi
        offsetq+:=bitwidthx
        if offsetq>=8 then ++qq; offsetq:=0 fi
    od  
end

global function bits_bytesize(object p)int=

    return getbitssize(p.length, p.elemtag)
end

global function getbitssize(int n, t)int=
    int nbits:=n*ttbitwidth[t]
    return ((nbits-1)/64+1)*8           
end
=== qq_calldll.m 0 0 5/43 ===
global proc calldll(symbol d, variant args, result, int nargs)=
    symbol e
    const maxparams=100
    array [maxparams]int arglist
    int n, retcode, retval,fnindex,libindex
    word dllinst
    ref proc fnaddr
    ichar name


    if nargs>maxparams then pcerror("Too many dll args") fi

    e:=d.deflist
    n:=0

    for i to nargs do
        if e=nil then       
            if d.mvarparams then
                arglist[i]:=vartopacked(args,nil)
                ++args
            else
                pcerror("Too many dll args")
            fi

        else
            arglist[i]:=vartopacked(args,e)
            ++args
            e:=e.nextdef
        fi
    od

    if d.mode in [tr64] then
        retcode:='R'
    else
        retcode:='I'
    fi
    fnaddr:=dllprocaddr[d.index]
    if fnaddr=nil then
        fnaddr:=loaddllfunction(d)
    fi

    retval:=os_calldllfunction(fnaddr, retcode, nargs,&arglist, nil)
    if d.mode then
        packedtovar(retval, d.mode, result)
    fi
end

function vartopacked(variant p, symbol d)word=
    int s:=p.tag, t
    object a

    if d=nil then               
        case s
        when tstring then
            a:=p.objptr
            return word@(convCstring(a.strptr,a.length))
        when tint,treal,tword,trefpack then
            return p.value
        else
            pcerror("Bad variadic param")
        esac
    fi

    t:=d.mode


    switch ttbasetype[t]
    when ti64, tu64, ti32, tu32, ti16, tu16 then
        case s
        when tint, tword, trefpack,trefvar then
            return p.value
        when treal then
            return int(p.xvalue)
        else
error::
            fprintln "'#' should be '#' (param # #)", strmode(s,1), strmode(t),d.name, d.index
            pcerror("DLL: wrong param type")
        esac

    when tr64 then
        case s
        when tint, tword then
            return word@(real(p.value))
        when treal then
            return word@(p.xvalue)
        else
            error
        esac
    when tstringz then
        case s
        when tstring then
            a:=p.objptr
            return word@(convCstring(a.strptr,a.length))
        when trefpack then
            return word@(p.ptr)
        else
            error
        esac
    when trefpack then
        case s
        when trefpack then
            return word(p.ptr)
        else
            error
        esac
    else
        pcmxtypestt("DLL params:",s,t)
    end

    return 0

end

proc packedtovar(word retval, int t, variant dest)=
    int tbase:=ttbasetype[t]
    object a


    switch tbase
    when tvoid then
    when tr64 then
        dest.tagx:=treal
        dest.xvalue:=real@(retval)
    when tr32 then
        PCERROR("dll/r32ret")
    when ti64,tu64 then
        dest.tagx:=tint
        dest.value:=retval
    when ti32 then
        dest.tagx:=tint
        dest.value:=int32(retval)
    when tu32 then
        dest.tagx:=tint
        dest.value:=word32(retval)
    when ti16 then
        dest.tagx:=tint
        dest.value:=int16(retval)
    when tu16 then
        dest.tagx:=tint
        dest.value:=word16(retval)
    when trefpack then
        dest.tagx:=trefpack
        dest.ptr:=cast(retval)
        dest.elemtag:=tttarget[t]
    when tstringz then
        var_make_string(cast(retval), dest)

    else
        pcerror_s("Rettype not supported:",ttname[t])
    endswitch
end

function loaddllfunction(symbol d)ref proc=
    int fnindex,libindex
    word dllinst
    ref proc fnaddr
    ichar name


    fnindex:=d.index
    fnaddr:=dllprocaddr[fnindex]

    if fnaddr=nil then
        libindex:=dlllibindex[fnindex]
        dllinst:=dllinsttable[libindex]

        if dllinst=0 then
            dllinst:=os_getdllinst(dlltable[libindex].name)
            if dllinst=0 then
                pcerror_s("Can't load DLL:",dlltable[libindex].name)
            fi
            dllinsttable[libindex]:=dllinst
        fi

        name:=(d.truename|d.truename|d.name)
        fnaddr:=os_getdllprocaddr(dllinst,name)

        if fnaddr=nil then
            pcerror_s("Can't find DLL func:",name)
        fi
        dllprocaddr[fnindex]:=fnaddr
    fi

    return fnaddr
end
=== qq_decimal.m 0 0 6/43 ===
const digitwidth   = 9
const digitbase = 1000000000
const digitfmt   = "%09d"
const mdigitfmt  = "z9"


const digitmax   = digitbase-1

global type elemtype = int32
global const decelemsize = elemtype.bytes

record constrec =
    int64 value
    object bnvalue
    ref constrec nextconst
end

enumdata [0:]ichar fpnames =
    (zero_type = 0,     $),
    (normal_type,       $),
    (inf_type,          $),
    (nan_type,          $),
end

enumdata =
    nn_types,         
    zz_types,         
    ii_types,         
    xx_types,         

    nz_types,         
    ni_types,         

    zn_types,         
    in_types,         

    zi_types,         
    iz_types          
end

const maxprec      = 10 million

int currprec       = 500/digitwidth

int stblz            

ref constrec constlist=nil      
global int decstrsize
varrec vtemp                    

macro bn_free(x) = obj_free_dec(x)

global proc obj_free_dec(object p)=
    if p.length then
        pcm_free(p.num, p.length*decelemsize)
    fi

    pcm_free32(p)
end

global proc var_dupl_dec(variant a)=

    object p,q
    int size

    q:=a.objptr
    p:=obj_new()
    p.length:=q.length
    p.expon:=q.expon
    p.neg:=q.neg
    p.numtype:=q.numtype

    size:=q.length*decelemsize

    if size then
        p.num:=pcm_alloc(size)
        memcpy(p.num,q.num,size)
    fi

    a.objptr:=p
end

global proc var_empty_dec(variant dest)=
    dest.tagx:=tdecimal+hasrefmask
    dest.objptr:=makebignum(0)
end

global proc var_make_dec_str(ichar s, int length, variant dest)=
    dest.tagx:=tdecimal ior hasrefmask
    dest.objptr:=bn_makestr(s,length)
end

global proc var_make_dec_int(int a, variant dest)=
    dest.tagx:=tdecimal ior hasrefmask
    dest.objptr:=bn_makeint(a)
end


function badnumber:object c=
    c:=makebignum(0)
    c.numtype:=nan_type
    return c
end

function bn_makestr(ichar s, int length=0)object=
    ichar t,u,oldt
    int tlength
    int neg,dpindex,expon,nonzeros,talloc,dpseen
    int leadingzeros, trailingzeros,zerosafterdp
    int d,n,wd,dp,wdp,w,d2,na,nb
    object a

    if length=0 then
        length:=strlen(s)
    fi
    if length<=0 then
        return badnumber()
    fi

    t:=malloc(length+1)
    memcpy(t,s,length)
    (t+length)^:=0
    oldt:=t
    tlength:=length+1
    s:=t

    talloc:=length+1+10     

    neg:=0
    case s^
    when '+' then ++s
    when '-' then neg:=1; ++s
    esac

    t:=u:=pcm_alloc(talloc)   
    dpindex:=-1
    dpseen:=zerosafterdp:=0
    nonzeros:=0
    leadingzeros:=trailingzeros:=0
    expon:=0


    doswitch s^
    when '1'..'9' then
        u++^:=s++^
        trailingzeros:=0
        nonzeros:=1
    when '0' then
        if nonzeros then
           ++trailingzeros
           u++^:=s++^
        else
           ++leadingzeros
           if dpseen then
              ++zerosafterdp
           fi
           ++s
        fi
    when '_', '\'', '`', ' ',13,10 then
        ++s
    when '.' then
        if dpseen or dpindex>=0 then return badnumber() fi
        if nonzeros then
           dpindex:=u-t
        else
           dpseen:=1
        fi
        ++s
    when 0 then
        exit
    when 'e','E' then
        expon:=readexpon(s+1)
        exit
    else
        return badnumber()
    end doswitch

    u^:=0
    length:=u-t            
    if dpindex<0 then
        if dpseen then
           dpindex:=-zerosafterdp
        else
           dpindex:=length
        fi
    fi
    length-:=trailingzeros    
    (t+length)^:=0

    if length=0 then
        return bn_makeint(0)
    fi

    d:=dpindex-1+expon
    n:=length
    dp:=0
    na:=1
    nb:=n-na

    w:=digitwidth

    if d>=0 then
        wd:=d/w
        wdp:=d rem w
    else
        d2:=abs(d+1)
        wd:=-(d2/w+1)
        wdp:=w-1-(d2 rem w)
    fi

    na:=wdp+1
    nb:=max(n-na,0)
    while nb rem w do ++nb od
    length:=nb/w+1
    u:=t+n
    to na+nb-n do
        u++^:='0'
    od
    n:=na+nb
    (t+n)^:=0

    a:=makebignum(length)
    a.neg:=neg
    a.expon:=wd
    u:=t
    a.num[0]:=strvaln(u,na)
    u+:=na
    
    for i:=1 to length-1 do
        a.num[i]:=strvaln(u,w)
        u+:=w
    od

    pcm_free(t,talloc)
    free(oldt)

    return a
end

function readexpon(ichar s)int=
    int neg, expon
    neg:=expon:=0

    case s^
    when '+' then ++s
    when '-' then neg:=1; ++s
    esac

    doswitch s^
    when '0'..'9' then
        expon:=expon*10+(s^-'0')
        ++s
    when '_', '\'', '`', ' ' then
        ++s
    when 0 then
        exit
    else
        pcerror("make expon?")
    end doswitch

    return (neg|-expon|expon)
end

function bn_makeint(int x)object a=
    array [256]char str

    if x=0 then
        a:=makebignum(0)
    elsif x in 0..digitmax then
        a:=makebignum(1)
        a.num[0]:=x
    elsif -x in 0..digitmax then
        a:=makebignum(1)
        a.num[0]:=-x
        a.neg:=1
    else
        print @str,x
        a:=bn_makestr(str)
    fi

    return a
end

global function var_tostr_dec(variant a,int fmt)ichar=
    return obj_tostr_dec(a.objptr,fmt)
end

function obj_tostr_dec(object a,int fmt=0)ichar=
    int expon,upper
    ichar s,t


    t:=nil
    if a=nil then
        t:="<void>"
    else
        case a.numtype
        when zero_type then t:=(fmt='E' or fmt='F'|"0.0"|"0")
        when inf_type then t:=(a.neg|"-Infinity"|"Infinity")
        when nan_type then t:="<NaN>"
        esac
    fi

    if t then
        s:=pcm_alloc(decstrsize:=strlen(t)+1)
        strcpy(s,t)
        return s
    fi

    if fmt=0 or fmt='A' then
        if bn_isint(a) and (a.expon-a.length)*digitwidth<60 then
           fmt:='I'
        elsif abs(a.expon*digitwidth)<60 then
           fmt:='F'
        else
           fmt:='E'
        fi
    fi

    if fmt='E' then
        s:=tostring_scient(a)
    else
        s:=tostring_float(a,fmt)
    fi
    return s
end

function tostring_scient(object a)ichar=
    ichar s,t
    int expon,nchars,n,shift
    int64 x,scale

    nchars:=3

    expon:=a.expon*digitwidth

    x:=a.num[0]
    scale:=1
    shift:=0
    while x>=10 do
        x:=x/10
        scale*:=10
        ++expon
        ++shift
    od

    nchars:=a.length*digitwidth+16   

    s:=t:=pcm_alloc(decstrsize:=nchars)

    if a.neg then
        t++^:='-'
    fi

    print @t,x,,"."
    t+:=strlen(t)

    if shift then
        print @t, shift:"v",,a.num[0]-x*scale:"z*"
        t+:=strlen(t)
    fi

    for i to a.length-1 do
        print @t,a.num[i]:mdigitfmt
        t+:=strlen(t)
    od

    while (t-1)^='0' and (t-2)^<>'.' do
        --t
    od

    print @t,"e",,expon
    t+:=strlen(t)
    t^:=0

    return s
end

function tostring_float(object a,int fmt)ichar=
    int expon,upper,nchars,w,prel,n,showdot
    ichar s,t

    expon:=a.expon
    upper:=a.length-1

    if fmt='I' and bn_isint(a) then
        showdot:=0
    else
        showdot:=1
    fi

    w:=digitwidth
    nchars:=3            
    if expon<0 then
        nchars+:=abs(expon-1)*w
    fi
    nchars+:=a.length*w
    if expon-upper>0 then
        nchars+:=(expon-upper)*w
    fi
    nchars+:=8              

    s:=t:=pcm_alloc(decstrsize:=nchars)
    
    if a.neg then
        t++^:='-'
    fi

    prel:=0
    if expon<0 then
        prel:=1
        t++^:='0'
        t++^:='.'
        to abs(expon)-1 do
           to digitwidth do
              t++^:='0'
           od
        od
    fi

    for i:=0 to upper do
        n:=sprintf(t,(i>0 or prel|digitfmt|"%d"),a.num[i])
        t+:=n
        if expon=i and i<upper and showdot then
           t++^:='.'
        fi
    od

    to expon-upper do
        to digitwidth do
           t++^:='0'
        od
    od
    if expon>=upper and showdot then
        t++^:='.'
        t++^:='0'
    fi

    t^:=0
    return s
end

function strvaln(ref char s,int n)int=    
    int a

    a:=0
    to n do
        if s^<>'_' then
           a:=a*10+s^-'0'
        fi
        ++s
    od
    return a
end

function bn_isint(object a)int =
    return a.length<=a.expon+1
end

global function obj_len_dec(object a)int=

RETURN BN_GETPREC(A)

    if not bn_isint(a) then
        return 0
    fi
    if BN_iszero(a) then
        return 1
    fi

    return strlen(strint(a.num[0]))+a.expon*digitwidth
end

global function bn_iszero(object a)int=
    return a.numtype=zero_type
end

global function var_equal_dec(variant a,b)int=
    return bn_equal(a.objptr, b.objptr)
end

global proc var_add_dec(variant a,b)=
    object dest:=bn_init()

    bn_add(dest, a.objptr,b.objptr)
    a.objptr:=dest
end

global proc var_sub_dec(variant a,b)=
    object dest:=bn_init()

    bn_sub(dest, a.objptr,b.objptr)
    a.objptr:=dest
end

global proc var_mul_dec(variant a,b)=
    object dest:=bn_init()

    bn_mul(dest, a.objptr,b.objptr)
    a.objptr:=dest
end

global proc var_div_dec(variant a,b)=
    object dest:=bn_init()

    bn_div(dest, a.objptr,b.objptr)
    a.objptr:=dest
end

global proc var_idiv_dec(variant a,b)=
    object dest:=bn_init()

    bn_idiv(dest, a.objptr,b.objptr)
    a.objptr:=dest
end

global proc var_irem_dec(variant a,b)=
    object dest:=bn_init()

    bn_irem(dest, a.objptr,b.objptr)
    a.objptr:=dest
end

global proc var_neg_dec(variant a)=

    bn_negto(a.objptr)
end

global proc var_abs_dec(variant a)=

    bn_absto(a.objptr)
end

global function var_compare_dec(variant a,b)int=
    return bn_cmp(a.objptr, b.objptr)
end

function bn_cmp(object a,b)int=
    object d
    int neg

    if bn_equal(a,b) then
        return 0
    fi

    d:=bn_init()
    bn_sub(d,a,b)
    neg:=d.neg
    bn_free(d)
    return (neg|-1|1)
end

function bn_equal(object a,b)int=

    if a.numtype<>normal_type and a.numtype=b.numtype then
        return a.neg=b.neg
    fi

    if a.length<>b.length or 
       a.numtype<>b.numtype or 
       a.neg<>b.neg or 
       a.expon<>b.expon then
        return 0
    fi

    if a.length=0 then return 1 fi

    return eqbytes(a.num,b.num,a.length*decelemsize)
end

function bn_add(object dest,a,b)int=
    int nega,negb

    switch getbintype(a,b)
    when nn_types then
    when zz_types then
        bn_setzero(dest)
        return 1
    when nz_types then
        bn_dupl(dest,a)
        return 1
    when zn_types then
        bn_dupl(dest,b)
        return 1
    else
        bn_setnan(dest)
        return 0
    end switch

    nega:=a.neg
    negb:=b.neg

    if not nega and not negb then      
        bn_addu(dest,a,b)
    elsif nega and negb then           
        bn_addu(dest,a,b)
        bn_negto(dest)
    elsif not nega and negb then        
        bn_subu(dest,a,b)
    else
        bn_subu(dest,b,a)            
    fi

    return 1
end

function bn_sub(object dest,a,b)int=
    int nega,negb

    switch getbintype(a,b)
    when nn_types then
    when zz_types then
        bn_setzero(dest)
        return 1
    when nz_types then
        bn_dupl(dest,a)
        return 1
    when zn_types then
        bn_dupl(dest,b)
        bn_negto(dest)
        return 1
    else
        bn_setnan(dest)
        return 0
    end switch

    nega:=a.neg
    negb:=b.neg

    if not nega and not negb then      
        bn_subu(dest,a,b)
    elsif nega and negb then           
        bn_subu(dest,b,a)
    elsif not nega and negb then        
        bn_addu(dest,a,b)
    else                           
        bn_subu(dest,b,a)
    fi

    return 1
end

proc bn_addu(object dest,a,b)=
    int preca, precb, precc
    int uppera,upperb,upperc, offset, carry,expona,exponb
    int dc
    word j
    ref[0:]elemtype pa,pb
    ref elemtype pax,pbx
    ref elemtype c,c2

    if a.expon<b.expon then    
        swap(a,b)               
    fi

    expona:=a.expon
    exponb:=b.expon
    preca:=a.length
    precb:=b.length

    offset:=expona-exponb         
    uppera:=preca-1
    upperb:=precb-1

    if uppera>(upperb+offset) then  
        upperc:=uppera
    else                        
        upperc:=upperb+offset
    fi
    precc:=upperc+1

    c:=makesmallnum(precc)       
    carry:=0
    pa:=a.num
    pb:=b.num

    for i:=upperc downto 0 do         

        j:=i-offset               
        if i<=uppera and j<=word(upperb) then
           dc:=pa^[i]+pb^[j]+carry
        elsif i<=uppera then
           dc:=pa^[i]+carry
        elsif j<=word(upperb) then
           dc:=pb^[j]+carry
        else
           dc:=carry
        fi

        if dc>=digitbase then
           carry:=1
           (c+i)^:=dc-digitbase
        else
           (c+i)^:=dc
           carry:=0
        fi
    od

    if carry then
        c2:=makesmallnum(precc+1)
        c2^:=carry
        memcpy(c2+1,c,precc*decelemsize)
        freesmall(c,precc)
        c:=c2
        ++precc
    fi

    smalltobig(dest,c,precc,precc)

    dest.expon:=expona+carry
end

proc bn_subu(object dest,a,b)=
    int preca, precb, precc
    int uppera,upperb,upperc, offset, carry, expona
    int da,db,dc, isneg, z, newprec,diff
    word j
    ref[0:]elemtype pa,pb
    ref elemtype c

    isneg:=0
    if a.expon<b.expon then    
        swap(a,b)               
        isneg:=1
    fi

retry::
    expona:=a.expon
    preca:=a.length
    precb:=b.length

    offset:=expona-b.expon      
    uppera:=preca-1
    upperb:=precb-1

    if uppera>(upperb+offset) then  
        upperc:=uppera
    else                        
        upperc:=upperb+offset
    fi
    precc:=upperc+1

    c:=makesmallnum(precc)
    carry:=0
    pa:=a.num
    pb:=b.num

    for i:=upperc downto 0 do         
        j:=i-offset               
        if i<=uppera and j<=word(upperb) then

           diff:=pa^[i]-pb^[j]-carry
        elsif i<=uppera then
           diff:=pa^[i]-carry
        elsif j<=word(upperb) then
           diff:=-pb^[j]-carry
        else
           diff:=-carry
        fi

        if diff<0 then
           carry:=1
           (c+i)^:=diff+digitbase
        else
           (c+i)^:=diff
           carry:=0
        fi
        
    od

    if carry then
        if isneg then         
           pcerror("SUBU/CARRY")
        fi
        swap(a,b)
        isneg:=1
        freesmall(c,precc)
        goto retry
    fi

    smalltobig(dest,c,precc,precc)
    dest.neg:=isneg
    dest.expon:=expona-stblz

end

function makebignum(int length)object=
    object a

    a:=obj_new()
    if length then
        a.num:=pcm_alloc(length*decelemsize)
        a.numtype:=normal_type
    else
        a.num:=nil
        a.numtype:=zero_type
    fi
    a.length:=length

    a.expon:=0
    a.neg:=0

    return a
end

function makesmallnum(int length)ref elemtype=
    return pcm_alloc(length*decelemsize)
end

function smalltobig(object c, ref elemtype a, int length,alloc,offset=0)object =

    ref elemtype p
    int leadingzeros, trailingzeros, nonzeros, newlength

    bn_setzero(c)

    p:=a
    leadingzeros:=trailingzeros:=nonzeros:=0
    to length do
        if p++^ then
           nonzeros:=1
           trailingzeros:=0
        else
           if nonzeros then
              ++trailingzeros
           else
              ++leadingzeros
           fi
        fi
    od

    stblz:=leadingzeros

    if nonzeros then

        newlength:=length-trailingzeros-leadingzeros

        if newlength=length=alloc then       
           c.num:=cast(a)
        else
           c.num:=cast(makesmallnum(newlength))
           memcpy(c.num,a+leadingzeros,newlength*decelemsize)
           freesmall(a+offset,alloc)
        fi
        c.length:=newlength
        c.numtype:=normal_type
        c.expon:=length-1-leadingzeros          
    elsif alloc then                              
        freesmall(a+offset,alloc)
    fi

    return c
end

proc freesmall(ref elemtype p, int length)=
    pcm_free(p,length*decelemsize)
end

global function bn_init()object=
    object a

    a:=makebignum(0)
    return a
end

proc bn_setzero(object a)=
    if a then
        if a.num then
           freesmall(cast(a.num),a.length)
        fi
        a.num:=nil
        a.length:=0
        a.neg:=0
        a.expon:=0
        a.numtype:=zero_type
    fi
end

proc bn_move(object a,b)=

    bn_setzero(a)
    a.bignumdescr:=b.bignumdescr
    clear b.bignumdescr
end

proc bn_dupl(object a,b)=
    object c
    int size

        c:=bn_init()
        c^:=b^
        if c.length then
            c.num:=cast(makesmallnum(size:=c.length))
            memcpy(c.num,b.num, size*decelemsize)
        fi
        bn_move(a,c)
        bn_free(c)

end

proc bn_setinf(object dest) =
    bn_setzero(dest)
    dest.numtype:=inf_type
end

proc bn_setnan(object dest) =
    bn_setzero(dest)
    dest.numtype:=nan_type
end

global proc var_setnan(variant dest) =
    dest.tagx:=tdecimal+hasrefmask
    dest.objptr:=makebignum(0)
    bn_setnan(dest.objptr)
end

global proc var_setinf(variant dest) =
    dest.tagx:=tdecimal+hasrefmask
    dest.objptr:=makebignum(0)
    bn_setinf(dest.objptr)
end

function getbintype(object a,b)int=
    int atype:=a.numtype, btype:=b.numtype

    if atype=nan_type or btype=nan_type then
        return xx_types
    fi

    case atype
    when normal_type then
        case btype
        when normal_type then
           return nn_types
        when zero_type then
           return nz_types
        else
           return ni_types
        esac
    when zero_type then
        case btype
        when normal_type then
           return zn_types
        when zero_type then
           return zz_types
        else
           return zi_types
        esac
    else
        case btype
        when normal_type then
           return in_types
        when zero_type then
           return iz_types
        else
           return ii_types
        esac
    esac

end

proc bn_negto(object a)=
    if not bn_iszero(a) then
        a.neg:=not a.neg
    fi
end

proc bn_absto(object a)=
    a.neg:=0
end

function bn_mul(object dest,a,b)int=
    int neg

    switch getbintype(a,b)
    when nn_types then
    when zz_types,nz_types,zn_types then
        bn_setzero(dest)
        return 1
    else
        bn_setnan(dest)
        return 0
    end switch

    neg:=a.neg<>b.neg
    bn_mulu(dest,a,b)
    if neg then  
        bn_negto(dest)
    fi
    return 1
end

function bn_mulp(object dest,a,b, int prec)int=
    int res:=bn_mul(dest,a,b)
    if res then
        bn_setprec(dest,(prec=0|currprec|prec))
    fi
    return res
end

proc bn_mulu(object dest, a,b) =


    int uppera, upperb, upperc
    int precc,expona,exponb
    int ax,bx,cx        
    int i,cx1, nc2, pd,pr
    i64 p,carry,x
    object d
    ref elemtype c
    i64 pdquot,pdrem

    expona:=a.expon
    exponb:=b.expon
    uppera:=a.length-1
    upperb:=b.length-1

    precc:=uppera+upperb+2
    nc2:=precc


    c:=makesmallnum(nc2)
    memset(c,0,precc*decelemsize)
    cx:=precc-1

    for bx:=upperb downto 0 do
        carry:=0

        cx1:=cx
        for ax:=uppera downto 0 do
            p:=i64((a.num[ax]))*i64((b.num[bx]))+carry

            pd:=p/digitbase


            pr:=p-pd*digitbase
            x:=int64((c+cx1)^)+pr

            if x>digitmax then
                carry := pd+1
                (c+cx1--)^ := x-digitbase
            else
                carry:=pd
                (c+cx1--)^:=x
            fi

        od
        (c+cx1)^:=carry
        --cx              
    od

    smalltobig(dest,c,precc,nc2)
    dest.expon:=expona+exponb+1-stblz

end


function smallmulto(ref elemtype p,q, int plen, m)int=

    ref elemtype pp,qq
    int carry,d

    case m
    when 0 then
        p^:=0
        return 1
    when 1 then
        memcpy(p,q,plen*decelemsize)
        return plen
    esac

    pp:=p+plen-1
    qq:=q+plen-1
    carry:=0

    to plen do
        d:=int64(qq^)*m+carry
        pp^:=d rem digitbase
        carry:=d/digitbase
        --qq
        --pp
    od

    if carry then            
        pp:=p+plen
        to plen do
           pp^:=(pp-1)^
           --pp
        od
        pp^:=carry
        ++plen
    fi

    return plen
end

function bn_div(object dest,a,b,int prec=0)int=
    int neg

    switch getbintype(a,b)
    when nn_types then
    when zn_types then
        bn_setzero(dest)
        return 1
    when zz_types,nz_types then
        bn_setinf(dest)
        return 0
    else
        bn_setnan(dest)
        return 0
    end switch

    neg:=a.neg<>b.neg

    bn_fdivu(dest,a,b,prec)

    if neg then
        bn_negto(dest)
    fi
    return 1
end

function bn_idiv(object dest,a,b)int=
    int neg
    switch getbintype(a,b)
    when nn_types then
    when zn_types then
        bn_setzero(dest)
        return 1
    when zz_types,nz_types then
        bn_setinf(dest)
        return 0
    else
        bn_setnan(dest)
        return 0
    end switch

    neg:=a.neg<>b.neg
    bn_idivu(dest,a,b)
    if neg then
        bn_negto(dest)
    fi
    return 1
end

function bn_idivrem(object dest,rm,a,b)int=
    int nega,negb

    switch getbintype(a,b)
    when nn_types then
    when zn_types then
        bn_setzero(dest)
        bn_setzero(rm)
        return 1
    when zz_types,nz_types then
        bn_setinf(dest)
        bn_setzero(rm)
        return 0
    else
        bn_setnan(dest)
        return 0
    end switch

    nega:=a.neg
    negb:=b.neg
    bn_idivu(dest,a,b,rm)
    if nega<>negb then    
        bn_negto(dest)
    fi
    if nega then bn_negto(rm) fi
    return 1
end

function bn_irem(object dest,a,b)int=
    object rm,d
    int nega

    switch getbintype(a,b)
    when nn_types then
    when zn_types then
        bn_dupl(dest,b)
        return 1
    when zz_types,nz_types then
        bn_setinf(dest)
        bn_setzero(dest)
        return 0
    else
        bn_setnan(dest)
        return 0
    end switch

    nega:=a.neg
    d:=bn_init()
    bn_idivu(d,a,b,dest)
    if nega then bn_negto(dest) fi
    bn_free(d)
    return 1
end

proc bn_idivu(object dest,a,b,rm=nil)=

    ref elemtype c,x,e
    int expona, exponb, badjust, exponc
    int na,nb,nc,nx,ne,nx2,ne2, cx,nupper
    int uppera, upperb, upperc
    int n, k, nexta
    int64 xx,y
    ref elemtype pa,pb

    na:=a.length
    nb:=b.length
    expona:=a.expon
    exponb:=b.expon
    badjust:=exponb+1-nb

    if na>expona+1 or nb>exponb+1 then
        pcerror("idivu:a or b not int")
    fi
    nc:=expona+1

    if expona<exponb then
        bn_setzero(dest)
        if  rm then
           bn_dupl(rm,a)
        fi
        return
    fi

    uppera:=na-1
    upperb:=nb-1
    upperc:=nc-1
    pa:=cast(a.num)
    pb:=cast(b.num)        

    n:=nb               
    x:=makesmallnum(nx2:=n+1)      
    nx:=n                      
    nupper:=nc-badjust

    for i:=0 to upperb do
        if i<=uppera then
           (x+i)^:=(pa+i)^
        else
           (x+i)^:=0
        fi
    od

    c:=makesmallnum(nc)
    cx:=0

    do
        k:=smalldiv(x,pb,nx,nb)

        (c+cx++)^:=k
        if n>=nupper then               
           exit
        fi

        nexta:=(n>uppera|0|(pa+n)^)
        ++n
        if nx=1 and x^=0 then
           x^:=nexta             
        else
           (x+nx)^:=nexta        
           ++nx
        fi
    od

    if rm and exponb<nb then        
        smalltobig(rm,x,nx,nx2)
    else
        freesmall(x,nx2)
    fi

    if cx=1 and c^=0 then
        freesmall(c,nc)
        bn_setzero(dest)
        if rm then
           bn_dupl(rm,a)
        fi
        return
    fi

    if c^=0 and cx>=2 then          
        smalltobig(dest,c+1,cx-1,nc,-1)
    else
        smalltobig(dest,c,cx,nc)
    fi

    if rm and exponb>=nb then         
        object d
        d:=bn_init()
        bn_mulu(d,b,dest)
        bn_subu(rm,a,d)
        bn_free(d)
    fi

end

proc bn_fdivu(object dest,a,b,int precision)=

    ref elemtype c,x,e
    int expona, exponb, badjust, exponc
    int na,nb,nc,nx,ne,nx2,ne2, cx,nupper,nc2
    int uppera, upperb, upperc
    int n, k, nexta
    int64 xx,y
    ref elemtype pa,pb

    na:=a.length
    nb:=b.length
    expona:=a.expon
    exponb:=b.expon

    if precision then
        precision:=((precision-1)/digitwidth+1)     
    else
        precision:=currprec
    fi
    nc:=precision

    uppera:=na-1
    upperb:=nb-1
    upperc:=nc-1
    pa:=cast(a.num)
    pb:=cast(b.num)        

    n:=nb               
    x:=makesmallnum(nx2:=n+1)      
    nx:=n                      

    for i:=0 to upperb do
        if i<=uppera then
           (x+i)^:=(pa+i)^
        else
           (x+i)^:=0
        fi
    od

    c:=makesmallnum(nc2:=nc+1)
    cx:=0

    do
        k:=smalldiv(x,pb,nx,nb)

        (c+cx++)^:=k

        if cx>nc then            
           exit
        fi

        nexta:=(n>uppera|0|(pa+n)^)
        ++n
        if nx=1 and x^=0 then
           x^:=nexta             
        else
           (x+nx)^:=nexta        
           ++nx
        fi
    od

    freesmall(x,nx2)

    if cx=1 and c^=0 then
        freesmall(c,nc2)
        bn_setzero(dest)
        return
    fi

    if c^=0 and cx>=2 then          
        smalltobig(dest,c+1,cx-1,nc2,-1)
        dest.expon:=expona-exponb-1
    else
        smalltobig(dest,c,cx,nc2)
        dest.expon:=expona-exponb
    fi
end

function smalldiv(ref elemtype x, b, int &xlen, nb)int =

    int k,count
    int64 xx,y
    elemtype xi,bi
    ref elemtype e
    int esize,ne,nx

    nx:=xlen
    k:=0
    count:=0
    e:=makesmallnum(esize:=(nb+1))

    do
        if nx<nb then            
           exit
        elsif nx>nb then           
           xx:=int64(x^)*digitbase+int64((x+1)^)
           y:=xx/(b^+1)
        else                           
           if x^>=(b^+1) then
              y:=x^/(b^+1)
           else
              y:=1
              for i:=0 to nb-1 do
                 xi:=(x+i)^
                 bi:=(b+i)^
                 if xi<bi then
                    y:=0
                    exit all
                 elsif xi>bi then
                    exit
                 fi
              od

           fi
        fi
        k+:=y
        if y>1 then
           ne:=smallmulto(e,b,nb,y)
           nx:=smallsubto(x,e,nx,ne)
        elsif y then
           nx:=smallsubto(x,b,nx,nb)
        else
           pcerror("smalldiv:Y=0")
        fi
    od

    freesmall(e,esize)
    xlen:=nx                 
    return k
end

function smallsubto(ref elemtype p,q, int plen, qlen)int=
    ref elemtype pp,qq
    int carry,diff,z

    pp:=p+plen-1
    qq:=q+qlen-1
    carry:=0
    z:=0                 

    to plen do
        if qq>=q then
           diff:=pp^-qq^-carry
           --qq
        else
           diff:=pp^-carry
        fi

        if diff<0 then
           carry:=1
           pp^:=diff+digitbase
        else
           pp^:=diff
           carry:=0
        fi
        if pp^ then
           z:=0
        else
           ++z
        fi
        --pp
    od
    if carry then pcerror("SSUBTO/CARRY?") fi

    if z=plen then --z fi         

    if z then
        plen-:=z
        pp:=p
        qq:=p+z
        to plen do
           pp++^:=qq++^
        od
    fi

    return plen
end

function bn_getprec(object a)int=
    return a.length*digitwidth
end

proc bn_setprec(object a,int prec)=
    int oldlength,newlength
    object c

    if a.numtype<>normal_type then
        return
    fi

    if prec<1 or prec>maxprec then
        return
    fi

    prec:=((prec-1)/digitwidth+1)*digitwidth        

    newlength:=prec/digitwidth                 

    oldlength:=a.length

    if oldlength<=newlength then
        return
    fi

    c:=makebignum(newlength)
    c.neg:=a.neg
    c.expon:=a.expon

    for i:=0 to newlength-1 do
        if i<oldlength then
           c.num[i]:=a.num[i]
        else
           c.num[i]:=0
        fi
    od

    bn_move(a,c)
    bn_free(c)
end

function bn_getglobalprec:int=
    return currprec*digitwidth
end

proc bn_setglobalprec(int prec)=
    currprec:=((prec-1)/digitwidth+1)
end

function bn_makefloat(real64 x)object =
    object a
    array [2048]char str

    sprintf(&.str,"%.15g",x)

    return bn_makestr(&.str)
end

global function dectemp(variant a)variant=
    vtemp.tagx:=tdecimal+hasrefmask

    case a.tag
    when tint then
        vtemp.objptr:=bn_makeint(a.value)
    when treal then
        vtemp.objptr:=bn_makefloat(a.xvalue)
    else
        pcerror("dectemp")
    esac
    a^:=vtemp
    return a
end

global proc freedectemp=
    bn_free(vtemp.objptr)
end

proc bn_ipower(object d, a,int64 n)=
    object e,f

    if n<0 then
        bn_setzero(d)

    elsif n=0 then
        bn_move(d,bn_makeint(1))

    elsif n=1 then
        bn_dupl(d,a)
    elsif (n iand 1)=0 then
        e:=bn_init()
        bn_mulu(e,a,a)
        bn_ipower(d,e,n/2)
        bn_free(e)    

    else           
        e:=bn_init()
        f:=bn_init()
        bn_mulu(e,a,a)
        bn_ipower(f,e,(n-1)/2)
        bn_mulu(d,a,f)
        bn_free(e)
        bn_free(f)

    fi
end

global proc var_power_dec(variant a, int n)=
    object dest:=bn_init()
    bn_ipower(dest,a.objptr,n)
    a.objptr:=dest
end

global function var_convert_dec_int(variant a)int=
    return bn_toint(a.objptr)
end

function bn_toint(object a)int64=
    int64 x
    if not bn_isint(a) then
        pcerror("dec-float->int not ready")
        return 0
    fi
    if bn_iszero(a) then
        return 0
    fi

    x:=0

    for i:=0 to a.length-1 do
        x:=x*digitbase+a.num[i]
    od

    for i:=a.length to a.expon do
        x*:=digitbase
    od

    if a.neg then
        return -x
    else
        return x
    fi
end

=== qq_decls.m 0 0 7/43 ===

global int dispatchtype=asm_dispatch
global int hasbytecodes=1           

global type unit        = ref unitrec
global type object      = ref objrec
global type symbol      = ref strec
global type variant     = ref varrec
global type decelemtype = int32

global macro pr(a,b)    = (a<<16 ior b)

global const hasrefmask = 0x100
global const varsize    = varrec.bytes

global record modulerec=
    ichar   name                
    ichar   path                
    unit    ast                 
    int     parsed              
    ref int pcstart             
    ref int pcend               
    int     pcsize              
    ref i32 pcsrcstart          

    union
        symbol  def                 
        symbol  stmodule
    end
    symbol stsubprog
    symbol stmacro

    symbol  startfn             
    symbol  mainfn              

    int16   fileno              
    int16   issyslib
    int16   moduleno            
    int16   subprogno
end

global record subprogrec =
    ichar name
    symbol stsubprog
    int issyslib        
    ichar path          
    int16 startmodule
    int16 endmodule
    int fileno
end

global record packfieldrec =
    object structobj            
    ichar name
    int32 packmode              
    int32 offset                
    int32 size                  
    int32 length
end

global record procrec =         
    symbol def
    ref procrec nextproc
end


global record userxrec =
    symbol owner
    ref int16 pmode
    ref userxrec nextmode
end

global record strec =

UNION
    STRUCT

    ichar name              
    symbol  owner
    symbol  deflist         
    symbol  deflistx        

    symbol  nextdef         
    symbol  nextdupl        
    symbol  firstdupl       
    symbol  alias           


    union
        u64 a
        ref int pcaddress       
        variant varptr          
        ichar truename          
        symbol atfield          
        int labelno             
    end
    union
        u64 b
        unit code               
        ref symbol topfieldlist     
    end
    union
        u64 c
        int index               
    end
    union
        u64 d
        struct
            int16 nparams       
            int16 nlocals       
        end
        struct
            int16 nfields       
            int16 maxalign      
            int16 fieldoffset
            int16 baseclassindex        
        end
        int genfieldindex       
    end

    word16  subcode
    byte    moduleno
    byte    subprogno
    int16   mode
    int16   hint                
    u16     flags: (isglobal:2, isimport:1, mstatic:1, misfunc:1, mbyref:1,
                            menumx:1,
                            moptional:1,  mvarparams:1, isframe:1,
                            iscaligned:1,initcode:2)
    byte    forindex        

    byte    symbolcode
    byte    nameid          

    byte    mutable         
    byte    namelen         
    byte    procfixed       
    END


END
end

global record lexrec =      
    union
        int64 value             
        real xvalue             
        word64 uvalue           
        ichar svalue            
        ref strec symptr            
    end

    int32 pos: (sourceoffset:24, moduleno:8)

    int16 symbol
    int16 subcode
end

global record uflagsrec =
    array [7]byte   codes
    byte    ulength
end

global record fieldrec =
    ichar name
    int16 recordtype
    int16 fieldtype
    int32 fieldoffset
end

global record qint =    
    word64 lower
    int64  upper
end

global record unitrec =
    union
        struct
            int16 tag
            union
                byte elemtype           
            end
            union
                byte nparams
                byte enumindex
            end
            int32 pos: (sourceoffset:24, moduleno:8)
        end
        ref void word1
    end

    unit nextunit

    union
        struct
            union
                unit a
                symbol def
                symbol labeldef
                int64 value
                word64 uvalue
                real64 xvalue
                ichar svalue
                int64 range_lower
                int64 qlower
                int pclopcode
            end
            union
                unit b
                int64 range_upper
                int64 qupper
                int64 slength
                int16 mode
                [4]byte cmpgenop
                struct
                    int32 length
                    int32 lower
                end
                int64 index     
            end
        end
    end


end

global lexrec nextlx
global lexrec lx
global const targetbits=64

global const maxsearchdirs=10
global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0

global [5]ichar hostdirs
global int nhostdirs

global int qpos
global int pcerrorpos
global ref modulerec pcerrormodule

global const stacksize=70000
global [stacksize]varrec varstack
global variant sptr
global variant stacklimit
global ref byte frameptr

global ref int pcptr

global int stopped

global symbol stprogram         
global symbol stmodule      
global symbol stsubprog
global symbol stcurrmodule      
global symbol stcurrproc        
                                    
global ref modulerec currmodule 

global int debug
global int COUNT


global unit lastast
global filehandle astdev

global int inproc
GLOBAL INT NALLUNITS
GLOBAL INT NPROCUNITS
GLOBAL INT NALLPCL
GLOBAL INT NALLPROCS
GLOBAL INT NMAXPROCUNITS

global [256]char errorline,errorpointer
global int errorlineno
global ref modulerec errormodule
global symbol sterrorproc


global record genfieldrec=
    symbol def
    ref genfieldrec nextdef
end

global const maxgenfield=1000
global [maxgenfield]ref genfieldrec genfieldtable
global int ngenfields

global const maxdlllib=50
global const maxdllproc=2000

global int ndlllibs
global [maxdlllib]symbol dlltable
global [maxdlllib]u64 dllinsttable          

global int ndllprocs
global [maxdllproc]symbol dllproctable
global [maxdllproc]byte dlllibindex             
global [maxdllproc]ref void dllprocaddr         

global int usesyslibs = 1           

global enumdata []ichar dispatchnames=
    (lab_dispatch,      "-lab"),
    (fn_dispatch,       "-fn"),
    (debug_dispatch,    "-debug"),
    (fdebug_dispatch,   "-fdebug"),
    (asm_dispatch,      "-asm"),
end

global const int maxqparam=32
global int nqparams
global [maxqparam]ichar qparamtable

global ichar err_message
global varrec err_var1, err_var2
global ref int err_pcptr

global ref int stopseq      
global ref int raiseseq     

global ref procrec proclist, proclistx
global int nproclist

global ref proc pcl_callbackfn=nil  

global [0..255]object chrtable      

global int fnosys
global int fverbose
global int fwritedocs
global filehandle docsdev           

GLOBAL INT PCLLEVEL=0

global [0:256]int16 baseclasstable
global [0:256]ref strec baseclassdef
global int nbaseclasses


global int lastretindex

global const maxmodule=100
global const maxsubprog=30
global const maxsourcefile=200

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]byte moduletosub
global [0..maxsubprog]subprogrec subprogtable

global [maxsourcefile]ichar sourcefilespecs     
global [maxsourcefile]ichar sourcefilepaths     
global [maxsourcefile]ichar sourcefilenames     
global [maxsourcefile]byte sourcefilesys        
global [maxsourcefile]byte sourcefilesupport    
global [maxsourcefile]ichar sourcefiletext      
global [maxsourcefile]int sourcefilesizes       
global int nsourcefiles
global int nmodules
global int nsubprogs
global int mainmoduleno


global symbol stmglobals                

GLOBAL OBJECT PPP
GLOBAL INT NCALLS
GLOBAL [0:100]INT NARGCOUNTS

GLOBAL INT NMULT
GLOBAL INT NCONSTS
GLOBAL INT NSMALL

global int headermode=0


global record varrec =
    union
        struct
            byte    tag
            byte    hasref
            byte    bitoffset
            union
                byte    bitlength       
                byte    exceptiontype
            end
        end
        word32      tagx
    end
    union
        word32      elemtag
        word32      frameptr_low
        struct
            i16     frameoffset
            i16     nexceptions
        end
    end
    union
        int64       value
        real64      xvalue
        word64      uvalue
        struct
            int32   range_lower         
            int32   range_upper
        end
        object      objptr              
        variant     varptr              
        ref byte    ptr                 
        symbol      def                 
        ref int     retaddr
    end
end

export record objrec =
    word32 refcount
    struct
        byte flags: (lower:1, mutable:1, bittag:2)
        byte objtype
        union
            u16 elemtag
            u16 usertag
            struct
                byte bitoffset
                byte indexoffset
            end
            i16 lower16
        end
    end

    union
        int64       value
        real64      xvalue
        word64      uvalue
        ichar       strptr
        variant     varptr
        ref byte    ptr
        ref[0:]elemtype num
        word64 b
        ref int     retaddr
    end

    union
        int64 length
        int64 lower64
        struct
            word32 rows
            word32 columns
        end
        word64 c
        ref byte frameptr
    end

    union
        int64 alloc64
        object objptr2
        struct
            int16 neg
            int16 numtype
            int32 expon
        end
        struct
            word32 alloc32
            word32 dictitems
        end
        word64 d
    end
    array[24]byte bignumdescr @ b
end
=== qq_dicts.m 0 0 8/43 ===
global proc var_make_dict(variant a, dest, int n) =
    object p
    variant b
    varrec v

    p:=obj_new_dict(n)

    b:=p.varptr
    v.tagx:=tdict+hasrefmask
    v.objptr:=p

    to n do
        adddictitem(&v,a,a+1)
        a+:=2
    od
    p.dictitems:=n

    dest^:=v
end

global function obj_new_dict(int n)object p=
    int m

    m:=max(16,nextpoweroftwo(n*2))      

    p:=obj_newlist(m,1,nil)
    p.dictitems:=0

    return p
end

global proc obj_free_dict(object p,int internal=0)=
    variant q
    varrec v

    q:=p.varptr
    to p.length do
        var_unshare(q)
        ++q
    od
    if p.length then
        pcm_free(p.varptr,p.alloc32*varrec.bytes)
    fi

    if not internal then
        pcm_free32(p)
    fi
end

global proc var_dupl_dict(variant a)=
    object p,q
    variant plist, qlist

    p:=a.objptr
    q:=obj_new_dict(p.dictitems)
    q^:=p^
    q.refcount:=1
    q.mutable:=1

    a.objptr:=q

    if q.length=0 then return fi

    qlist:=q.varptr:=pcm_alloc(p.length*varrec.bytes)
    q.alloc32:=allocbytes/varrec.bytes  
    plist:=p.varptr

    to q.length do
        qlist^:=plist^
        var_dupl(qlist)
        ++qlist
        ++plist
    od
end

global function var_equal_dict(variant x,y)int =
    int xlen,ylen,res
    object px,py
    variant a,b

PCERROR("EQUALDICT")
    return 1
end

global function var_finddictitem(variant vd, variant p,int doins)variant=

    int hash,index,size,keytag,wrapped,limit
    int64 keyvalue
    variant q
    object pa,qa,d

    retry::
    d:=vd.objptr

    size:=d.length/2


    index:=(var_gethashvalue(p) iand (size-1))      





    q:=d.varptr+index*2                         
    wrapped:=0
    keytag:=p.tag
    keyvalue:=p.value                           
    pa:=p.objptr                                

    do
        if q.tag=tvoid then                 
            exit

        elsif q.tag=keytag then
            case keytag
            when tint,treal,tword,trange then
                if q.value=keyvalue then
                    ++q
                    var_share(q)
                    return q
                fi
            when tstring then
                qa:=q.objptr
                if pa.length=qa.length then 
                    if memcmp(pa.strptr,qa.strptr,pa.length)=0 then
                        ++q
                        var_share(q)
                        return q
                    fi
                fi
            esac
        fi

        ++index
        q+:=2
        if index>=size then
            if wrapped then                 
                pcerror("DICT FULL?")
            fi
            wrapped:=1
            index:=0
            q:=d.varptr
        fi
    od

    if doins then
        limit:=size*3/4
        if d.dictitems>=limit then
            expanddict(vd)
            goto retry
        fi
        q^:=p^
        var_share(q)
        ++(d.dictitems)
        return q+1                          
    else
        return nil
    fi
end

proc expanddict(variant vd)=
    int n,m,i,j,k,oldrefcount
    object d,e
    objrec temp
    variant p,q,r
    varrec ev
    static byte inuse


    if inuse then
        pcerror("expanddict?")
    fi
    inuse:=1

    d:=vd.objptr

    n:=d.alloc32            
    m:=n/2                  

    p:=d.varptr                         

    e:=obj_new_dict(m*2)

    var_objtovar(tdict,e,&ev)

    q:=p

    for i:=1 to m do
        if q.tag<>tvoid then
            r:=var_finddictitem(&ev,q,1)
            ++q
            r^:=q++^                    
        else
            q+:=2
        fi
    od

    obj_free_dict(d,1)              
    oldrefcount:=d.refcount
    d^:=e^                          
    pcm_free(e,e^.bytes)
    d.refcount:=oldrefcount

    inuse:=0
end

proc adddictitem(variant d, p, q)=
    object da
    variant r

    da:=d.objptr

    if da.length=0 then             
        pcerror("NULL DICT")
    fi

    r:=var_finddictitem(d,p,1)

    var_share(q)
    var_unshare(r)          
    r^:=q^
end
=== qq_jhandlers.m 0 0 9/43 ===

int showasmflag=2

const kopnda    = 8
const kopndb    = 16
const kopndc    = 24
const kopndd    = 32

const ktag          = varrec.tag
const khasref       = varrec.hasref
const krefelemtag   = varrec.elemtag
const kelemtag      = varrec.elemtag
const kvarptr       = varrec.varptr
const kobjptr       = varrec.objptr
const kptr          = varrec.ptr
const kvalue        = varrec.value
const kxvalue       = varrec.xvalue
const kretaddr      = varrec.retaddr
const kframeptr_low = varrec.frameptr_low
const krange_low    = varrec.range_lower
const krange_high   = varrec.range_upper

const jrefcount     = objrec.refcount
const jmutable      = objrec.flags          
const jusertag      = objrec.usertag
const jvarptr       = objrec.varptr
const jlength       = objrec.length
const jlower16      = objrec.lower16
const jobjptr2      = objrec.objptr2

const gdef          = genfieldrec.def
const gnextdef      = genfieldrec.nextdef
const sowner        = strec.owner
const smode         = strec.mode
const snameid       = strec.nameid
const soffset       = strec.fieldoffset

const varshift      = 4


const xa        = 0             

const xb        = -varsize      
const ya        = 0

const xc        = -varsize*2    
const yb        = -varsize
const za        = 0


const intpsize  = 8

const intpsize2 = intpsize*2
const intpsize4 = intpsize*4
const intpsize6 = intpsize*6

macro jumpnext   =
    assem
        jmp [Dprog]

    end


macro saveregs   =
    assem
        mov [pcptr],Dprog
        mov [sptr],Dsptr
        mov [frameptr],Dframe
    end


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

global array [0..pclnames.upb]ref proc jhandlertable

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
                if eqstring(name+2,pclnames[k]+1) then      
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

global function asmavailable:int = 1

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

    pcptr+:=2

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

threadedproc j_pushenum=
    assem
        *pushvar
        mov word32 [Dsptr+ktag],tenum
        mov D0,[Dprog+kopnda]
        mov [Dsptr+kvalue],D0
        mov A0,[Dprog+kopndb]
        mov [Dsptr+kelemtag],A0
        *jumpskip3
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
        mov word32 [Dsptr+krefelemtag],tvoid
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
        cmp A0,ti32
        jnz L10
        mov word32 [Dsptr+ktag],tint
        mov A0,[D4]
        movsxd D0,A0
        mov [Dsptr+kvalue],D0
        *jumpskip1
L10:
        cmp A0,tu8
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
        mov D0,[Dsptr+xb]           
        mov [Dsptr+ya],D0
        mov D1,[Dsptr+xb+kvalue]    
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
        *popvar2
        mov Dprog,[Dprog+kopnda]
        *jumpnext
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
        jnz L1              
        mov D4,[Dsptr+kvalue]       
        *popvar
        sub D4,[Dprog+kopndb]       
        cmp D4,[Dprog+kopnda]       
        jae L2              
        shl D4,1
        mov Dprog,[Dprog+D4*8+intpsize4]
        *jumpnext
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
        mov D4,[Dprog+kopnda]       
        mov D0,[Dprog+kopndb]       
        inc word64 [Dframe+D0+kvalue]   
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
        mov D0,[Dprog+kopndb]       
        mov D5,[Dprog+kopndc]       
        mov D4,[Dprog+kopnda]       

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
        mov word32 [Dsptr+ktag],tretaddr
        lea D0,[Dprog+24]       
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
        mov D5,[Dprog+kopnda]       

        mov Dprog,[Dsptr+kretaddr]
        mov Aframe,[Dsptr+kframeptr_low]
        *popvar

        and D5,D5
        jz L2               
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
        mov D5,[Dprog+kopnda]       

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
        mov D0,[D4+kvalue]
        inc word64 [D4+kvalue]
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D0
        *jumpskip1

L1:
        cmp B0,trefpack
        jnz L2
        cmp word16 [D4+krefelemtag],tu8
        jnz L2

        mov D0,[D4+kptr]
        inc word64 [D4+kptr]

        mov word32 [Dsptr+ktag],trefpack
        mov word16 [Dsptr+krefelemtag],tu8
        mov [Dsptr+kptr],D0
        *jumpskip1

L2:

L99:
    end
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
        inc word64 [D4+kvalue]
        mov D0,[D4+kvalue]
        mov word32 [Dsptr+ktag],tint
        mov [Dsptr+kvalue],D0
        *jumpskip1

L1:
        cmp B0,trefpack
        jnz L2
        cmp word16 [D4+krefelemtag],tu8
        jnz L2

        inc word64 [D4+kptr]
        mov D0,[D4+kptr]
        mov word32 [Dsptr+ktag],trefpack
        mov word16 [Dsptr+krefelemtag],tu8
        mov [Dsptr+kptr],D0
        *jumpskip1

L2:

L99:
    end

    saveregs
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
        jnz L99                     
        mov D0,[Dsptr+kvalue]
        cmp D0,255
        ja L99                      
        mov D0,[D0*8+chrtable]
        and D0,D0
        jz L99                      
        mov word32 [Dsptr+ktag],tstring+hasrefmask
        mov [Dsptr+kvalue],D0               
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
        mov D3, [D1+jlength]

        *callunshareu_dsptr
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
        mov D3, [D1+jlength]
        movsx D4, word16 [D1+jlower16]
        lea D3,[D3+D4-1]    

        *callunshareu_dsptr
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

threadedproc j_usertype=
    saveregs
    k_usertype()
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
        cmp byte [Dsptr+xb+ktag],trefvar    
        jnz L99
        cmp byte [Dsptr+ya+ktag],tint       
        jnz L1
        cmp byte [D4+ktag],tint         
        jnz L99
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        add D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1

L1:
        cmp byte [Dsptr+ya+ktag],treal      
        jnz L2
        cmp byte [D4+ktag],treal            
        jnz L99                             

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
        cmp byte [Dsptr+xb+ktag],trefvar    
        jnz L99
        cmp byte [Dsptr+ya+ktag],tint       
        jnz L1
        cmp byte [D4+ktag],tint         
        jnz L99
        mov D1,[Dsptr+kvalue]
        mov D0,[D4+kvalue]
        sub D0,D1
        mov [D4+kvalue],D0
        *popvar2
        *jumpskip1
L1:
        cmp byte [Dsptr+ya+ktag],treal      
        jnz L2
        cmp byte [D4+ktag],treal            
        jnz L99                             

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
        cmp byte [Dsptr+xb+ktag],trefvar    
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         
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
        cmp byte [Dsptr+xb+ktag],trefvar    
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         
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
        cmp byte [Dsptr+xb+ktag],trefvar    
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         
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
        cmp byte [Dsptr+xb+ktag],trefvar    
        jnz L1
        cmp byte [Dsptr+ya+ktag],tint       
        jnz L1
        mov D4,[Dsptr+xb+kvarptr]
        cmp byte [D4+ktag],tint         
        jnz L1
        mov cl,[Dsptr+kvalue]
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
        cmp byte [Dsptr+ya+ktag],tint
        jnz L99

        mov D6,[Dsptr+xb+ktag]
        cmp B6,tlist
        jnz L2

        mov D5,[Dsptr+xb+kobjptr]

        mov D4,[Dsptr+ya+kvalue]    
        movsx D3,word16[D5+jlower16]
        sub D4,D3                   
        cmp D4,[D5+jlength]
        jae L99                     

        shl A4,varshift             
        add D4,[D5+jvarptr]         

        *popvar                     

        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0         

        mov D1,[D4+kvalue]
        mov [Dsptr+kvalue],D1

        and A0,hasrefmask
        jz L11
        inc word32 [D1+jrefcount]
L11:
        dec word32 [D5+jrefcount]   
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
        cmp byte [Dsptr+ya+ktag],tint
        jnz L99

        mov D6,[Dsptr+xb+ktag]
        cmp B6,tlist
        jnz L2

        mov D5,[Dsptr+xb+kobjptr]

        mov D4,[Dsptr+ya+kvalue]    
        movsx D3, word16[D5+jlower16]
        sub D4,D3                   
        cmp D4,[D5+jlength]
        jae L99                     

        shl A4,varshift             
        add D4,[D5+jvarptr]         

        *popvar                     

        dec word32 [D5+jrefcount]   
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

        mov D6,[Dsptr+yb+ktag]
        cmp B6,tlist
        jnz L99

        mov D5,[Dsptr+yb+kobjptr]
        mov B0,[D5+jmutable]
        and B0,1
        jz L99                      

        mov D4,[Dsptr+ya+kvalue]    
        movsx D3,word16[D5+jlower16]
        sub D4,D3       
        cmp D4,[D5+jlength]
        jae L99                     

        shl A4,varshift             
        add D4,[D5+jvarptr]         

        mov D0,[Dsptr+xc]           
        mov D1,[Dsptr+xc+kvalue]
        mov [D4],D0
        mov [D4+kvalue],D1

        dec word32 [D5+jrefcount]   
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
        inc word32 [D1+jrefcount]       
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
        inc word32 [D1+jrefcount]       
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
    end
end

threadedproc j_switchf=
    assem
        mov D3,[Dprog+kopnda]
        cmp word16 [D3+Dframe+ktag],tint
        jnz L99                         
        mov D4,[D3+Dframe+kvalue]       
        sub D4,[Dprog+kopndc]           
        cmp D4,[Dprog+kopndb]           
        jae L2                          
        shl D4,1
        mov Dprog,[Dprog+D4*8+intpsize6]
        *jumpnext
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
    end
end

threadedproc j_indexff=
    assem
        mov D2,[Dprog+kopnda]
        mov D3,[Dprog+kopndb]
        cmp byte [D2+Dframe+ktag],tlist
        jnz L99
        cmp byte [D3+Dframe+ktag],tint
        jnz L99

        mov D6,[D2+Dframe+ktag]

        mov D5,[D2+Dframe+kobjptr]
        mov D4,[D3+Dframe+kvalue]       
        movsx D6,word16[D5+jlower16]
        sub D4,D6                       
        cmp D4,[D5+jlength]
        jae L99                 

        shl A4,varshift             
        add D4,[D5+jvarptr]         

        *pushvar
        mov D0,[D4+ktag]
        mov [Dsptr+ktag],D0         
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
L13:

        add Dprog,intpsize4
        jmp j_index
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

    assem
        mov D2,[Dprog+kopnda]
        add D2,Dframe
L0:
        cmp byte [D2+ktag],trefpack
        jnz L99                         
        cmp byte [D2+krefelemtag],tu8
        jnz L99

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
        add Dprog,intpsize2         
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
        cmp byte [D2+ktag],trefpack
        jnz L99                         
        cmp byte [Dsptr+ktag],tint
        jnz L99                         

        cmp byte [D2+krefelemtag],tu8
        jnz L2

        mov D5,[D2+kptr]
        inc word64 [D2+kptr]

        mov D0,[Dsptr+kvalue]
        mov [D5],B0
        *popvar
        *jumpskip4

L2:
        cmp word16 [D2+krefelemtag],ti32
        jnz L3

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
        add Dprog,intpsize2         
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

        cmp A0,tu8
        jnz L10
        mov word32 [Dsptr+ktag],tint
        movzx A0,byte [D4]
        mov [Dsptr+kvalue],D0
        *jumpskip3
L10:
        cmp A0,ti32
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
        mov [Dsptr+xa+kvalue],D1            
        add Dprog, intpsize2
        jmp j_pushptr
    end
end

threadedproc j_lenf=
    assem
        mov D4,[Dprog+kopnda]
        add D4,Dframe
        mov W0, [D4+ktag]
        cmp B0,tlist
        jz L1
        cmp B0,tstring
        jz L1
        cmp B0,tarray
        jnz L99
L1:
        mov D1,[D4+kobjptr]
        mov D3, [D1+jlength]

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

=== qq_khandlers.m 0 0 10/43 ===

global macro getopnda = (pcptr+1)^
global macro getopndb = (pcptr+2)^
global macro getopndc = (pcptr+3)^
global macro getopndd = (pcptr+4)^

global macro skip(n) = pcptr:=pcptr+(n+1)



const doretcheck=0

global [0..pclnames.upb]ref proc khandlertable

global proc initkhandlers=
    ichar name
    static int handlersdone=0

    if handlersdone then return fi

    for i to $get_nprocs() do
        name:=$get_procname(i)
        if eqbytes(name,"k_",2) then
            for k:=0 to pclnames.upb do
                if eqstring(name+2,pclnames[k]+1) then      
                    khandlertable[k]:=$get_procaddr(i)
                    exit
                fi
            else
                pcerror_s("Unknown khandler",name)
            od
        fi
    od

    for i in khandlertable.bounds when khandlertable[i]=nil do
        khandlertable[i]:=cast(kunimpl)
    od

    handlersdone:=1
end

global proc kunimpl=
    if hasbytecodes then
        pcerror_s("Unimplemented:",pclnames[pcptr^])
    else
        pcerror("Unimplemented (use -fdebug to see opcode)")
    fi
end

global proc k_pushci=
    ++sptr
    sptr.tagx:=tint
    sptr.value:=getopnda
    skip(1)
end

global proc k_pushtrue=
    ++sptr
    sptr.tagx:=tbool
    sptr.value:=1
    ++pcptr
end

global proc k_pushfalse=
    ++sptr
    sptr.tagx:=tbool
    sptr.value:=0
    ++pcptr
end

global proc k_pushcu=
    ++sptr
    sptr.tagx:=tword
    sptr.value:=getopnda
    skip(1)
end

global proc k_pushnil=
    ++sptr
    sptr.tagx:=trefpack
    sptr.elemtag:=tvoid
    sptr.ptr:=nil
    ++pcptr
end

global proc k_pushcs=
    ++sptr

    sptr.tagx:=tstring ior hasrefmask
    sptr.objptr:=cast(getopnda)
    ++sptr.objptr.refcount
    skip(1)
end

global proc k_pushcr=
    ++sptr
    sptr.tagx:=treal
    sptr.xvalue:=real@(getopnda)
    skip(1)
end

global proc k_pushenum=
    ++sptr
    sptr.tagx:=tenum
    sptr.elemtag:=getopndb
    sptr.value:=getopnda
    skip(2)
end

global proc k_stop=
    stopped:=1
end

global proc k_stoprunproc=
    stopped:=1
end

global proc k_pushm=
    ++sptr
    sptr^:=variant(getopnda)^
    var_share(sptr)

    skip(1)
end

global proc k_pushf=
    ++sptr
    sptr^:=variant(frameptr+getopnda)^
    var_share(sptr)

    skip(1)
end

global proc k_pushff=
    sptr+:=2
    (sptr-1)^:=variant(frameptr+getopnda)^
    var_share(sptr-1)

    sptr^:=variant(frameptr+getopndb)^
    var_share(sptr)

    skip(3)
end

global proc k_pushmref=
    ++sptr
    sptr.tagx:=trefvar
    sptr.varptr:=variant(getopnda)

    skip(1)
end

global proc k_pushfref=
    ++sptr
    sptr.tagx:=trefvar
    sptr.varptr:=variant(frameptr+getopnda)

    skip(1)
end

global proc k_popm=
    variant p

    p:=variant(getopnda)
    var_unshare(p)
    p^:=sptr^               
    --sptr

    skip(1)
end

global proc k_storem=
    variant p

    p:=variant(getopnda)
    var_share(sptr)
    var_unshare(p)
    p^:=sptr^               

    skip(1)
end

global proc k_zpopm=
    (variant(getopnda))^:=sptr^             

    --sptr
    skip(1)
end

global proc k_popf=
    variant p

    p:=variant(frameptr+getopnda)
    var_unshare(p)
    p^:=sptr^               
    --sptr

    skip(1)
end

global proc k_storef=
    variant p

    p:=variant(frameptr+getopnda)
    var_share(sptr)
    var_unshare(p)
    p^:=sptr^               

    skip(1)
end

global proc k_zpopf=
    variant p

    p:=variant(frameptr+getopnda)
    p^:=sptr^               
    --sptr

    skip(1)
end

global proc k_popretval=
    variant p

    p:=variant(frameptr+getopnda)
    p^:=sptr^               
    --sptr

    skip(1)
end

global proc k_tom=
    variant p

    p:=cast(getopndb)

    --p.value

    if p.value then
        pcptr:=cast(getopnda)
    else
        skip(2)
    fi
end

global proc k_tof=
    variant p

    p:=cast(frameptr+getopndb)

    --p.value

    if p.value then
        pcptr:=cast(getopnda)
    else
        skip(2)
    fi
end

global proc k_add=
    variant y:=sptr--
    varrec x:=sptr^

    var_add(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_sub=
    variant y:=sptr--
    varrec x:=sptr^

    var_sub(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_mul=
    variant y:=sptr--
    varrec x:=sptr^

    var_mul(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_div=
    variant y:=sptr--
    varrec x:=sptr^

    var_div(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_idiv=
    variant y:=sptr--
    varrec x:=sptr^

    var_idiv(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_irem=
    variant y:=sptr--
    varrec x:=sptr^

    var_irem(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_iand=
    variant y:=sptr--
    varrec x:=sptr^

    var_iand(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_ior=
    variant y:=sptr--
    varrec x:=sptr^

    var_ior(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_ixor=
    variant y:=sptr--
    varrec x:=sptr^

    var_ixor(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_shl=
    variant y:=sptr--
    varrec x:=sptr^

    var_shl(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_shr=
    variant y:=sptr--
    varrec x:=sptr^

    var_shr(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_sqr=
    case sptr.tag
    when tint then
        sptr.value:=sqr(sptr.value)
    when treal then
        sptr.xvalue:=sqr(sptr.xvalue)
    else
        pcustype("Sqr",sptr)
    esac

    ++pcptr
end

global proc k_sign=
    case sptr.tag
    when tint then
        sptr.value:=(sptr.value<0|-1|(sptr.value>0|1|0))
    when treal then
        sptr.tag:=tint
        sptr.value:=(sptr.xvalue<0|-1|(sptr.xvalue>0|1|0))
    else
        pcustype("Sign",sptr)
    esac

    ++pcptr
end

global proc k_sqrt= domaths(ksqrt)
global proc k_sin=  domaths(ksin)
global proc k_cos=  domaths(kcos)
global proc k_tan=  domaths(ktan)
global proc k_asin= domaths(kasin)
global proc k_acos= domaths(kacos)
global proc k_atan= domaths(katan)
global proc k_ln=   domaths(kln)
global proc k_log=  domaths(klog)
global proc k_lg=   domaths(klg)
global proc k_exp=  domaths(kexp)
global proc k_round=    domaths(kround)
global proc k_floor=    domaths(kfloor)
global proc k_ceil=     domaths(kceil)
global proc k_fract=    domaths(kfract)

global proc k_neg=
    varrec x:=sptr^

    var_neg(sptr)
    var_unshare(&x)

    ++pcptr
end

global proc k_negto=
    variant px:=sptr--

    if not var_negto(px) then
        var_inplace_unary(px, cast(var_neg))
    end

    ++pcptr
end

global proc k_absto=
    variant px:=sptr--

    if not var_absto(px) then
        var_inplace_unary(px, cast(var_abs))
    end

    ++pcptr
end

global proc k_inotto=
    variant px:=sptr--

    if not var_inotto(px) then
        var_inplace_unary(px, cast(var_inot))
    end

    ++pcptr
end


global proc k_atan2=
    pcerror("ATAN2 NOT READY")
end

global proc k_fmod=
    pcerror("FMOD NOT READY")
end


global proc k_abs=
    varrec x:=sptr^

    var_abs(sptr)
    var_unshare(&x)

    ++pcptr
end

global proc k_inot=
    varrec x:=sptr^

    var_inot(sptr)
    var_unshare(&x)

    ++pcptr
end

global proc k_istruel=
    int res

    res:=var_istruel(sptr)
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res

    ++pcptr
end

global proc k_notl=
    int res

    res:=not var_istruel(sptr)
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res

    ++pcptr
end

global proc k_jumpeq=
    variant y:=sptr--
    variant x:=sptr--

    if var_equal(x,y) then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)
    var_unshare(y)
end

global proc k_jumpne=
    variant x,y

    y:=sptr--
    x:=sptr--

    if not var_equal(x,y) then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)
    var_unshare(y)
end

global proc k_jumplt=
    variant x,y

    y:=sptr
    x:=sptr-1

    sptr-:=2

    if var_compare(x,y)<0 then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)
    var_unshare(y)
end

global proc k_jumple=
    variant x,y

    y:=sptr--
    x:=sptr--

    if var_compare(x,y)<=0 then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)
    var_unshare(y)
end

global proc k_jumpge=
    variant x,y

    y:=sptr
    x:=sptr-1

    sptr-:=2
    if var_compare(x,y)>=0 then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)
    var_unshare(y)
end

global proc k_jumpgt=
    variant x,y

    y:=sptr--
    x:=sptr--

    if var_compare(x,y)>0 then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)
    var_unshare(y)
end

global proc k_jumpfalse=
    variant x:=sptr--

    if not var_istruel(x) then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
    var_unshare(x)

end

global proc k_jumptrue=
    variant x:=sptr--

    if var_istruel(x) then
        pcptr:=cast(getopnda)
    else
        skip(1)
    fi
end

global proc k_incrtom=
    variant p
    p:=variant(getopnda)
    case p.tag
    when tint then
        ++p.value
    when trefvar then
        ++p.varptr
    when trefpack then
        p.ptr+:=ttsize[p.elemtag]
    when treal then
        p.xvalue+:=1

    else
        pcustype("incrtom",p)
    end
    skip(1)
end

global proc k_incrtof=
    variant p

    p:=variant(frameptr+getopnda)
    case p.tag
    when tint,tword then
        ++p.value
    when trefvar then
        ++p.varptr
    when trefpack then
        p.ptr+:=ttsize[p.elemtag]
    when treal then
        p.xvalue+:=1
    else
        pcustype("incrtof",p)
    esac
    skip(1)
end

global proc k_decrtom=
    variant p

    p:=variant(getopnda)
    case p.tag
    when tint,tword then
        --p.value
    when trefvar then
        --p.varptr
    when trefpack then
        p.value-:=ttsize[p.elemtag]
    when treal then
        p.xvalue-:=1
    else
        pcustype("decrtom",p)
    esac
    skip(1)
end

global proc k_decrtof=
    variant p

    p:=variant(frameptr+getopnda)
    case p.tag
    when tint,tword then
        --p.value
    when treal then
        p.xvalue-:=1
    when trefvar then
        --p.varptr
    when trefpack then
        p.ptr-:=ttsize[p.elemtag]
    else
        pcustype("decrtof",p)
    esac
    skip(1)
end

global proc k_incrload=
    varrec v

    v:=sptr^
    k_incrptr()
    var_loadptr(&v,++sptr)
end

global proc k_loadincr=
    varrec v

    v:=sptr^
    var_loadptr(sptr,sptr)
    ++sptr
    sptr^:=v

    k_incrptr()
end

global proc k_decrload=
    varrec v

    v:=sptr^
    k_decrptr()
    var_loadptr(&v,++sptr)
end

global proc k_loaddecr=
    varrec v

    v:=sptr^
    var_loadptr(sptr,sptr)
    ++sptr
    sptr^:=v

    k_decrptr()
end

global proc k_incrptr=
    variant p

    p:=sptr--

    switch p.tag
    when trefvar then           
        p:=p.varptr
        switch p.tag
        when tint,tword then
            ++p.value
        when trefvar then           
            ++p.varptr
        when trefpack then          
            p.ptr+:=ttsize[p.elemtag]
        when treal then
            p.xvalue+:=1
        else
            pcustype("incrptr/refvar",p)
        endswitch
    when trefpack then          
        switch p.elemtag
        when tu8,ti8 then
            ++(p.ptr)^
        when tu16,ti16 then
            ++(p.ptr)^
        else
            pcustype_t("incrptr/ref",p.elemtag)
        endswitch

    else
        pcustype("incrptr",p)
    endswitch
    ++pcptr
end

global proc k_decrptr=
    variant p

    p:=sptr--

    switch p.tag
    when trefvar then           
        p:=p.varptr
        switch p.tag
        when tint,tword then
            --p.value
        when trefvar then           
            --p.varptr
        when trefpack then          
            p.ptr-:=ttsize[p.elemtag]
        when treal then
            p.xvalue-:=1
        else
            pcustype("incrptr/refvar",p)
        endswitch
    when trefpack then          
        switch p.elemtag
        when tu8,ti8 then
            --(p.ptr)^
        when tu16,ti16 then
            --(p.ptr)^
        else
            pcustype_t("incrptr/ref",p.elemtag)
        endswitch

    else
        pcustype("incrptr",p)
    endswitch
    ++pcptr
end

global proc k_pushvoid=
    ++sptr
    sptr.tagx:=tvoid
    ++pcptr
end

global proc k_callproc=
    const countinterval=100
    static int count=countinterval

    if --count=0 then
        count:=countinterval
        os_peek()
    fi

    if sptr>=stacklimit then
        pcerror("Stack Overflow")
    fi

    ++sptr
    sptr.tagx:=tretaddr
    sptr.retaddr := pcptr+3

    sptr.frameptr_low := word(frameptr)
    frameptr:=cast(sptr)

    pcptr:=cast(getopnda)

end

global proc k_callptr=
    symbol d

    if sptr.tag<>tsymbol then
        pcerror("Probably undefined function")
    fi
    d:=sptr.def
    if d.nameid=linkid then
        d:=d.alias
    FI

    if d.nparams<>getopnda then
        pcerror_s("Callptr: wrong # params; need:",strint(d.nparams))
    fi

    sptr.tagx:=tretaddr
    sptr.retaddr := pcptr+3

    sptr.frameptr_low := word(frameptr)
    frameptr:=cast(sptr)

    pcptr:=cast(d.pcaddress)
end

global proc k_procentry =
    to getopnda do
        ++sptr
        sptr.tagx:=tvoid
    od
    skip(1)
end

global proc k_return=
    int nargs

    if doretcheck then
        if sptr.tag<>tretaddr then
            pcerror_s("Not tretaddr:",ttname[sptr.tag])
        fi
    fi
    nargs:=getopnda

    pcptr:=sptr.retaddr

    (ref int32(&frameptr))^:=sptr.frameptr_low

    --sptr

    to nargs do
        var_unshare(sptr)
        --sptr
    od
end

global proc k_return0=
    int nargs

    if doretcheck then
        if sptr.tag<>tretaddr then
            pcerror_s("Not tretaddr:",ttname[sptr.tag])
        fi
    fi

    pcptr:=sptr.retaddr

    (ref int32(&frameptr))^:=sptr.frameptr_low

    --sptr
end

global proc k_unshare=
    to getopnda do
        var_unshare(sptr)
        --sptr
    od
    skip(1)
end

global proc k_unshare1=
    var_unshare(sptr)
    --sptr
    ++pcptr
end

global proc k_formci=
    variant p

    p:=cast(getopndb)

    ++p.value

    if p.value<=getopndc then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_forfci=
    variant p

    p:=cast(frameptr+getopndb)

    ++p.value

    if p.value<=getopndc then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_fordmci=
    variant p

    p:=cast(getopndb)

    --p.value

    if p.value>=getopndc then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_fordfci=
    variant p

    p:=cast(frameptr+getopndb)

    --p.value

    if p.value>=getopndc then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_formm=
    variant p,q

    p:=cast(getopndb)
    q:=cast(getopndc)

    ++p.value

    if p.value<=q.value then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_fordmm=
    variant p,q

    p:=cast(getopndb)
    q:=cast(getopndc)

    --p.value

    if p.value>=q.value then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_forff=
    variant p,q

    p:=cast(frameptr+getopndb)
    q:=cast(frameptr+getopndc)

    ++p.value

    if p.value<=q.value then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_fordff=
    variant p,q

    p:=cast(frameptr+getopndb)
    q:=cast(frameptr+getopndc)

    --p.value

    if p.value>=q.value then
        pcptr:=cast(getopnda)
    else
        skip(3)
    fi
end

global proc k_comment=
    skip(1)
end

global proc k_makelist=
    variant x,y
    int n

    n:=getopnda

    x:=sptr-n+1         
    sptr:=x

    var_make_list(x,sptr,n,getopndb)
    sptr.objptr.mutable:=0

    skip(2)
end

global proc k_makedict=
    variant x
    int n

    n:=getopnda

    x:=sptr-n*2+1           

    var_make_dict(x,x,n)
    sptr:=x

    skip(1)
end

global proc k_makeset=
    variant x
    int n

    n:=getopnda

    x:=sptr-n+1         

    var_make_set(x,x,n)
    sptr:=x
    sptr.objptr.mutable:=0

    skip(1)
end

global proc k_makerecord=
    variant x,y
    int n

    n:=getopnda

    x:=sptr-n+1             

    var_make_record(x,x,n,getopndb)
    sptr:=x
    sptr.objptr.mutable:=0

    skip(2)
end

global proc k_makestruct=
    variant x,y
    int n

    n:=getopnda

    x:=sptr-n+1             

    var_make_struct(x,x,n,getopndb)
    sptr:=x
    sptr.objptr.mutable:=0

    skip(2)
end

global proc k_makearray=
    variant x
    int n

    n:=getopndb

    x:=sptr-n+1             

    var_make_array(x,x,getopnda, n, getopndc, getopndd)
    sptr:=x
    sptr.objptr.mutable:=0

    skip(4)
end

global proc k_makebits=
    variant x
    int n

    n:=getopndb

    x:=sptr-n+1             

    var_make_bits(x,x,getopnda, n, getopndc, getopndd)
    sptr:=x
    sptr.objptr.mutable:=0

    skip(4)
end

global proc k_index=
    variant y,z
    varrec x

    y:=sptr--
    x:=sptr^

    case y.tag
    when tint then
        var_getix(sptr,y.value)
    when trange then
        var_getslice(sptr,y.range_lower,y.range_upper)
    else
        pcmxtypes("Index",&x,y)
    esac

    var_unshare(&x)

    ++pcptr
end

global proc k_popindex=
    variant x,y,z

    z:=sptr--       
    y:=sptr--       
    x:=sptr--       

    case z.tag
    when tint then
        var_putix(y, z.value, x)
        var_unshare(y)
    when trange then
        var_putslice(y, z.range_lower, z.range_upper, x)
        var_unshare(x)
        var_unshare(y)
    else
        pcmxtypes("Popindex",y,z)
    esac


    ++pcptr
end

global proc k_indexref=
    variant y,p
    varrec x

    y:=sptr--
    x:=sptr^

    case y.tag
    when tint then
        var_getixref(sptr, y.value)
    else
        pcmxtypes("Indexref",sptr,y)
    esac

    var_unshare(&x)
    ++pcptr
end

global proc k_keyindex=
    variant d,k,p,def

    def:=sptr--         
    k:=sptr--           
    d:=sptr             

    if d^.tag<>tdict then
        pcustype("dict{}",d)
    fi

    p:=var_finddictitem(d,k,0)
    var_unshare(d)
    var_unshare(k)

    if p then           
        sptr^:=p^
        var_unshare(def)
    else
        sptr^:=def^         
    fi
    ++pcptr
end

global proc k_popkeyindex=
    variant d,k,p,x

    k:=sptr--           
    d:=sptr--           
    x:=sptr--           

    if d.tag<>tdict then
        pcustype("dict{}:=",d)
    fi

    p:=var_finddictitem(d,k,1)

    if p.tag<>tvoid then
        var_unshare(p)
    fi
    p^:=x^

    var_unshare(d)
    var_unshare(k)

    ++pcptr
end

global proc k_keyindexref=
    variant d,k,p,x

    k:=sptr--           
    d:=sptr             

    if d.tag<>tdict then
        pcustype("&dict{}",d)
    fi

    p:=var_finddictitem(d,k,0)
    if p=nil then
        pcerror("&dict{} not found")
    fi
    var_share(p)
    var_unshare(k)
    var_unshare(d)

    sptr.tagx:=trefvar
    sptr.varptr:=p

    ++pcptr
end

global proc k_dot=
    symbol d
    variant p
    ref byte q
    int rectype
    varrec v


    case sptr.tag
    when trecord, tstruct then
    else
        pcustype("dot/not record",sptr)
    esac
    rectype:=sptr.objptr.usertag

    d:=resolvefield(getopnda, rectype)

    case d.nameid
    when fieldid then
        p:=sptr.objptr.varptr+d.fieldoffset/varsize
        var_share(p)
        var_unshare(sptr)
        sptr^:=p^

    when structfieldid then
        var_loadpacked(sptr.objptr.ptr+d.fieldoffset, d.mode, &v, nil)
        var_unshare(sptr)
        sptr^:=v

    when procid then
        sptr.tagx:=tsymbol
        sptr.def:=d

    when linkid then
        sptr.tagx:=tsymbol
        sptr.def:=d.alias

    else
        pcerror_s("DOT: can't do this fieldtype:",namenames[d.nameid])
    esac

    skip(1)
end

global proc k_dotref=
    symbol d
    variant p
    ref byte q
    int rectype

    case sptr.tag
    when trecord, tstruct then
    else
        pcerror("dot/not record")
    esac
    rectype:=sptr.objptr.usertag

    d:=resolvefield(getopnda, rectype)

    case d.nameid
    when fieldid then
        p:=sptr.objptr.varptr+d.fieldoffset/varsize
        var_unshare(sptr)

        sptr.tagx:=trefvar
        sptr.varptr:=P

    when structfieldid then
        q:=sptr.objptr.ptr+d.fieldoffset
        var_unshare(sptr)
        sptr.tagx:=trefpack
        sptr.ptr:=q
        sptr.elemtag:=d.mode

    else
        pcerror_s("DOTREF: can't do this fieldtype:",namenames[d.nameid])
    esac

    skip(1)
end

global proc k_popdot=
    symbol d
    variant p,x,y

    x:=sptr--
    y:=sptr--

    case x.tag
    when trecord, tstruct then
    else
        pcustype("dot/not record",x)
    esac

    d:=resolvefield(getopnda, x.objptr.usertag)

    IF NOT X.HASREF THEN PCERROR("POPDOT") FI

    if not x.objptr.mutable then
        pcerror("Not mutable")
    fi

    case d.nameid
    when fieldid then
        p:=x.objptr.varptr+d.fieldoffset/varsize
        var_unshare(p)
        p^:=y^              
        var_unshare(x)

    when structfieldid then
        var_storepacked(x.objptr.ptr+d.fieldoffset, y, d.mode)
        var_unshare(x)

    else
        pcerror_s("POPDOT: can't do this fieldtype:",namenames[d.nameid])
    esac

    skip(1)
end

global proc k_dotindex=
    variant y,z
    varrec x


    y:=sptr--
    x:=sptr^

    case y.tag
    when tint then
        var_getdotix(sptr,y.value)
    when trange then
        var_getdotslice(sptr,y.range_lower,y.range_upper)
    else
        pcmxtypes("Dotindex",&x,y)
    esac

    var_unshare(&x)

    ++pcptr
end

global proc k_dotindexref=
    variant y,p
    varrec x

    y:=sptr--
    x:=sptr^

    case y.tag
    when tint then
        var_getdotixref(sptr, y.value)
    when trange then
        var_getdotsliceref(sptr, y.range_lower,y.range_upper)
    else
        pcmxtypes("Dotindexref",sptr,y)
    esac

    var_unshare(&x)
    ++pcptr
end

global proc k_popdotindex=
    variant x,y,z,py

    z:=sptr--       
    y:=sptr--       
    x:=sptr--       

    case z.tag
    when tint then
        var_putdotix(y, z.value, x)
        var_unshare(y)
    when trange then
        var_putdotslice(y, z.range_lower, z.range_upper, x)
        var_unshare(x)
        var_unshare(y)
    else
        pcmxtypes("Popdotindex",y,z)
    esac


    ++pcptr
end

global proc k_len=
    variant x:=sptr
    object p:=x.objptr
    int n, t

    case x.tag
    when tlist,trecord,tarray,tdict,tbits then
        n:=p.length
    when tstring then
        n:=p.length
    when trecord, tuserarray, tstruct then
        n:=ttlength[p.usertag]
    when tset then
        n:=p.length
    when trange then
        n:=x.range_upper-x.range_lower+1
    when tdecimal then
        n:=obj_len_dec(p)
    when tenum then
        t:=x.elemtag; goto dotype
        n:=ttlength[x.elemtag]
    when ttype then
        t:=sptr.value
dotype::
        case ttbasetype[t]
        when tenumdef then
            n:=ttlower[t]
        else
            pcustype("t.len",x)
        esac
    else
        pcustype("Len",x)
    esac

    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_upb=
    variant x:=sptr
    object p:=x.objptr
    int n, t

    case x.tag
    when tlist then
        n:=p.length+p.lower16-1
    when tstring, tdict then
        n:=p.length
    when tarray then
        n:=p.length+p.lower-1
    when tbits then
        n:=p.length+p.lower-1
    when trecord, tstruct then
        n:=ttlength[p.usertag]

    when tuserarray then
        t:=p.usertag
        goto dotype

    when tset then
        n:=p.length-1
    when trange then
        n:=x.range_upper
    when tenum then
        t:=x.elemtag
        goto dotype
    when ttype then
        t:=sptr.value
dotype::
        case ttbasetype[t]
        when tenumdef, tpackarray then
            n:=ttlength[t]+ttlower[t]-1
        else
            pcustype("t.upb",x)
        esac

    else
        pcustype("Upb",x)
    esac

    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_lwb=
    variant x:=sptr
    object p:=x.objptr
    int n, t

    case x.tag
    when tlist then
        n:=p.lower16
    when tstring,tdict then
        n:=1
    when tarray, tbits then
        n:=p.lower
    when trecord,tstruct then
        n:=1
    when tuserarray then
        n:=ttlower[p.usertag]
    when tset then
        n:=0
    when trange then
        n:=x.range_lower
    when tenum then
        n:=ttlower[x.elemtag]
    when ttype then
        t:=sptr.value
dotype::
        case ttbasetype[t]
        when tenumdef then
            n:=ttlower[t]
        else
            pcustype("t.lwb",x)
        esac

    else
        pcustype("Lwb",x)
    esac

    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_bounds=
    do_bounds(0)
    ++pcptr
end

global proc k_boundsx=
    do_bounds(1)
    ++pcptr
end

proc do_bounds(int sx)=
    int a,b,m, t
    object p

    m:=sptr.tag
    p:=sptr.objptr

    case m
    when tlist then
        a:=p.lower16
        b:=p.length+a-1
    when tarray, tbits then
        a:=p.lower
        b:=p.length+a-1
    when tstring, tdict then
        a:=1
        b:=p.length
    when trange then
        a:=sptr.range_lower
        b:=sptr.range_upper
    when tstruct,trecord then
        a:=1
        b:=ttlength[p.usertag]
    when tuserarray then
        t:=p.usertag
        goto dotype

    when tset then
        a:=0
        b:=p.length-1
    when tenum then
        t:=sptr.elemtag
        goto dotype

    when ttype then
        t:=sptr.value
dotype::
        case ttbasetype[t]
        when tenumdef then
            a:=ttlower[t]
            b:=ttlength[t]+a-1
        else
            pcustype("t.bounds",sptr)
        esac

    else
        pcustype("Bounds",sptr)
    esac

    if sx then
        var_unshare(sptr)
        sptr.tagx:=tint
        sptr.value:=a
        ++sptr
        sptr.tagx:=tint
        sptr.value:=b

    else
        var_unshare(sptr)
        sptr.tagx:=trange
        sptr.range_lower:=a
        sptr.range_upper:=b
    fi
end


global proc k_dictitems=
    int n

    case sptr.tag
    when tdict then
        n:=sptr.objptr.dictitems
    when tdecimal then
        n:=sptr.objptr.length
    else
        pcustype("Dictitems/digits",sptr)
    esac
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_append=
    variant y:=sptr--
    varrec x:=sptr^

    var_append(sptr,y)
    var_unshare(&x)

    ++pcptr
end

global proc k_concat=
    variant y:=sptr--
    varrec x:=sptr^

    var_concat(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

global proc k_appendto=
    variant px,x,y

    y:=sptr--
    px:=sptr--

    case px.tag
    when trefvar then
        var_appendto(px.varptr,y)
    else
        pcustype("Appendto",px)
    esac
    ++pcptr
end

global proc k_concatto=
    variant px,x,y

    y:=sptr--
    px:=sptr--

    case px.tag
    when trefvar then
        var_concatto(px.varptr,y)
        var_unshare(y)
    else
        pcustype("Concatto",px)
    esac
    ++pcptr
end

global proc k_addto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_addto(px, y) then
        var_inplace(px,y, cast(var_add), cast(var_addmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_subto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_subto(px, y) then
        var_inplace(px,y, cast(var_sub), cast(var_submixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_multo=
    variant y:=sptr--
    variant px:=sptr--

    if not var_multo(px, y) then
        var_inplace(px,y, cast(var_mul), cast(var_mulmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_divto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_divto(px, y) then
        var_inplace(px,y, cast(var_div), cast(var_divmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_idivto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_idivto(px, y) then
        var_inplace(px,y, cast(var_idiv), cast(var_idivmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_iandto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_iandto(px, y) then
        var_inplace(px,y, cast(var_iand), cast(var_iandmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_iorto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_iorto(px, y) then
        var_inplace(px,y, cast(var_ior), cast(var_iormixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_ixorto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_ixorto(px, y) then
        var_inplace(px,y, cast(var_ixor), cast(var_ixormixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_shlto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_shlto(px, y) then
        var_inplace(px,y, cast(var_shl), cast(var_shlmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_shrto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_shrto(px, y) then
        var_inplace(px,y, cast(var_shr), cast(var_shrmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_copy=
    varrec x

    if sptr.hasref then
        x:=sptr^
        var_duplu(sptr)
        var_unshareu(&x)
    fi

    ++pcptr
end

global proc k_dupl=
    ++sptr
    sptr^:=(sptr-1)^
    var_share(sptr)
    ++pcptr
end

global proc k_makerange=
    variant x,y

    y:=sptr--
    x:=sptr

    if x.tag<>tint or y.tag<>tint then
        pcerror("makerange/not int")
    fi

    sptr.tagx:=trange
    sptr.range_upper:=y.value
    sptr.range_lower:=x.value

    ++pcptr
end

global proc k_makerangelen=
    variant x,y

    y:=sptr--
    x:=sptr

    if x.tag<>tint or y.tag<>tint then
        pcerror("makerangelen/not int")
    fi

    sptr.tagx:=trange
    sptr.range_upper:=x.value+y.value-1
    sptr.range_lower:=x.value

    ++pcptr
end

global proc k_makedecimal=
    varrec x
    object p

    x:=sptr^

    if x.tag<>tstring then pcerror("Not str") fi
    p:=x.objptr
    if p.length=0 then pcerror("Null str") fi

    var_make_dec_str(p.strptr, p.length, sptr)

    var_unshare(&x)

    ++pcptr
end

function resolvefield(int index, rectype)symbol d=
    ref genfieldrec g

    if index=0 then pcerror("Not a field") fi

    g:=genfieldtable[index]

    while g do
        d:=g.def
        if d.owner.mode=rectype then return d fi
        g:=g.nextdef
    od

    pcerror_s("Can't resolve field:",d.name)
    return nil
end

global proc k_pushptr=
    variant p

    p:=sptr

    case p.tag
    when trefvar then
        if not p.varptr then pcerror("Nil^") fi
        sptr^:=p.varptr^

    when trefpack then
        if not p.ptr then pcerror("Nil^") fi
        var_loadpacked(p.ptr,p.elemtag, sptr, nil)

    when trefbit then
        var_loadbit(p.ptr, p.bitoffset, p.elemtag, p.bitlength, sptr)


    else
        pcustype("Pushptr",p)
    esac

    var_share(sptr)

    ++pcptr 
end

global proc k_popptr=
    variant p,x,y

    p:=sptr--
    x:=sptr--

    case p.tag
    when trefvar then
        var_unshare(p.varptr)
        p.varptr^:=x^
    when trefpack then
        var_storepacked(p.ptr,x,p.elemtag)
    when trefbit then
        var_storebit(p.ptr, p.bitoffset, x, p.elemtag, p.bitlength)

    else
        pcustype("Popptr",p)
    esac

    ++pcptr 
end
global proc k_islist=
    istype(tlist)
end

global proc k_isarray=
    istype(tarray)
end

global proc k_isstring=
    istype(tstring)
end

global proc k_isrecord=
    istype(trecord)
end

global proc k_swap=
    array [1024]byte tempbuffer
    variant x,y
    varrec v
    int xt,yt,s,t,n
    ref byte p,q
    int a

    x:=sptr--
    y:=sptr--

    if x.tag<>y.tag then
        pcerror("Swap mismatch")
    fi

    case x.tag
    when trefvar then
        v:=x.varptr^
        x.varptr^:=y.varptr^
        y.varptr^:=v

    when trefpack then
        s:=x.elemtag
        t:=y.elemtag
        if s<>t then goto swaperror fi
        n:=ttsize[s]
        case n
        when 1 then
            p:=x.ptr
            q:=y.ptr
            a:=p^
            p^:=q^
            q^:=a
        elsif ttsize[s]<=tempbuffer.bytes then
            memcpy(&tempbuffer,x.ptr,n)
            memcpy(x.ptr,y.ptr,n)
            memcpy(y.ptr,&tempbuffer,n)
        else
            goto swaperror
        esac

    else
swaperror::
        pcmxtypes("Swap",x,y)
    esac

    ++pcptr
end

global proc k_jumptesteq=
    variant x,y
    int xt,yt,res

    y:=sptr--
    x:=sptr
    xt:=x.tag
    yt:=y.tag


    res:=var_equal(x,y)
    var_unshare(y)
    if res then
        var_unshare(x)
        --sptr
        pcptr:=cast(getopnda)
        return
    fi

    skip(1)
end

global proc k_jumptestne=
    variant x,y
    int xt,yt,res

    y:=sptr--
    x:=sptr
    xt:=x.tag
    yt:=y.tag

    if xt<>yt then
        case pr(xt,yt)
        when pr(tint,trange) then
            if x.value in y.range_lower..y.range_upper then
                --sptr
                skip(1)
                return
            fi
        when pr(tint,tset) then
            if var_in_set(x,y) then
                --sptr
                skip(1)
                return
            fi
        esac

        var_unshare(y)
        pcptr:=cast(getopnda)
        return

    fi

    res:=var_equal(x,y)
    var_unshare(y)
    if not res then
        pcptr:=cast(getopnda)
        return
    fi
    var_unshare(x)
    --sptr

    skip(1)
end

global proc k_jump=
    pcptr:=cast(getopnda)
end

global proc k_jumpptr=
    symbol d

    if sptr.tag<>tsymbol then
        pcerror("symbol expected")
    fi
    d:=cast(sptr.def)
    ++sptr
    if d.nameid<>labelid then
        pcerror("label expected")
    fi
    if not d.procfixed then
        d.pcaddress:=moduletable[d.moduleno].pcstart+d.labelno
        d.procfixed:=1
    fi

    pcptr:=d.pcaddress
end

global proc k_incr=
    case sptr.tag
    when tint then
        ++sptr.value
    else
        pcustype("incr",sptr)
    esac

    ++pcptr
end

global proc k_decr=
    case sptr.tag
    when tint then
        --sptr.value
    else
        pcustype("decr",sptr)
    esac

    ++pcptr
end

global proc k_chr=
    array [8]char str

    if sptr.tag=tint then
        var_makechar(sptr.value,sptr)
    else
        pcustype("CHR",sptr)
    fi
    ++pcptr
end

global proc k_asc=
    int c

    case sptr.tag
    when tstring then
        if sptr.objptr.length then
            c:=sptr^.objptr.strptr^
        else
            c:=0
        fi
        var_unshareu(sptr)
        sptr.tagx:=tint
        sptr.value:=c
    else
        pcustype("ASC",sptr)
    esac

    ++pcptr
end

global proc k_pusht=
    ++sptr
    sptr.tagx:=ttype
    sptr.value:=getopnda
    skip(1)
end

global proc k_type=
    int t:=sptr.tag
    var_unshare(sptr)
    sptr.tagx:=ttype
    sptr.value:=t
    ++pcptr
end

global proc k_basetype=
    int t:=sptr.tag

    case t
    when trecord, tstruct, tuserarray then
        t:=ttbasetype[sptr.objptr.usertag]
    when tenum then
        t:=ttbasetype[sptr.elemtag]
    esac

    var_unshare(sptr)
    sptr.tagx:=ttype
    sptr.value:=t
    ++pcptr
end

global proc k_usertype=
    int t:=sptr.tag

    case t
    when trecord, tstruct, tuserarray then
        t:=sptr.objptr.usertag
    when tenum then
        t:=sptr.elemtag
    esac

    var_unshare(sptr)
    sptr.tagx:=ttype
    sptr.value:=t
    ++pcptr
end

global proc k_elemtype=
    int t:=sptr.tag

    case t
    when tarray,tbits then
        t:=sptr.objptr.elemtag
    when trefpack, trefvar, trefbit, tenum then
        t:=sptr.elemtag
    when tset then
        t:=tu1
    when tuserarray then
        t:=tttarget[sptr.objptr.usertag]
    else
        pcustype_t("elemtype",t)
    esac

    var_unshare(sptr)
    sptr.tagx:=ttype
    sptr.value:=t
    ++pcptr
end

global proc k_nop= ++pcptr

global proc k_modulecall=
    symbol d:=cast(getopnda)
    int moduleno:=d.moduleno

    ++sptr
    sptr.tagx:=tretaddr
    sptr.retaddr := pcptr+2

    pcptr:=moduletable[moduleno].pcstart
end

global proc k_modulereturn=
    pcptr:=sptr.retaddr
    --sptr
end

global proc k_maxvalue=
    int64 a

    if sptr.tag=ttype then sptr.tag:=sptr.value fi

    case sptr.tag
    when tu8 then a:=255
    when tu16 then a:=65536
    when tu32 then a:=0xFFFF'FFFF
    when tu64,tword then a:=0xFFFF'FFFF'FFFF'FFFF
    when ti8 then a:=127
    when ti16 then a:=32767
    when ti32 then a:=0x7FFF'FFFF
    when ti64,tint then a:=0x7FFF'FFFF'FFFF'FFFF
    else
        pcustype("MAXVALUE",sptr)
    esac
    sptr.tagx:=tint
    sptr.value:=a

    ++pcptr

end

global proc k_minvalue=
    int64 a

    if sptr.tag=ttype then sptr.tag:=sptr.value fi

    case sptr.tag
    when tword,tu8,tu16,tu32,tu64 then a:=0
    when ti8 then a:=-128
    when ti16 then a:=-32768
    when ti32 then a:=-0x8000'0000
    when tint,ti64 then a:=-0x8000'0000'0000'0000
    else
        pcustype("MINVALUE",sptr)
    esac
    sptr^.tagx:=tint
    sptr^.value:=a

    ++pcptr
end

global proc k_callhost=
    callhostfunction(getopnda)
    skip(1)
end

global proc k_expand=
    variant dest
    int n
    
    n:=getopnda
    dest:=sptr+n-1

    var_expand(sptr,dest,n)
    sptr:=dest

    skip(1)
end

global proc k_pushsymbol=
    symbol d:=cast(getopnda)
    ++sptr
    sptr.tagx:=tsymbol
    sptr.def:=cast(getopnda)

    skip(1)
end

global proc k_eq=
    variant x,y
    int res

    y:=sptr
    x:=--sptr

    res:=var_equal(x,y)
    var_unshare(x)
    var_unshare(y)

    sptr.tagx:=tbool
    sptr.value:=res

    ++pcptr
end

global proc k_ne=
    variant x,y
    int res

    y:=sptr
    x:=--sptr

    res:=not var_equal(x,y)
    var_unshare(x)
    var_unshare(y)

    sptr.tagx:=tbool
    sptr.value:=res

    ++pcptr
end

global proc k_lt= do_cmp(klt)

global proc k_le = do_cmp(kle)
global proc k_ge = do_cmp(kge)
global proc k_gt = do_cmp(kgt)

proc do_cmp(int opc)=
    variant x,y
    int res

    y:=sptr
    x:=--sptr

    res:=var_compare(x,y)
    var_unshare(x)
    var_unshare(y)

    sptr.tagx:=tbool

    case opc
    when klt then sptr.value:=res<0
    when kle then sptr.value:=res<=0
    when kge then sptr.value:=res>=0
    else sptr.value:=res>0
    esac

    ++pcptr
end

global proc k_calldll=
    symbol d:=cast(getopnda)
    int nargs:=getopndb
    variant p
    calldll(d, sptr-nargs+1, sptr-nargs, nargs)

    sptr-:=nargs

    skip(2)
end

global proc k_in=
    variant x,y
    int n

    y:=sptr
    x:=--sptr

    n:=var_in(x,y)
    var_unshare(x)
    var_unshare(y)

    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_notin=
    variant x,y
    int n

    y:=sptr
    x:=--sptr

    n:=not var_in(x,y)
    var_unshare(x)
    var_unshare(y)

    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_convrefpack =
    variant a
    int tag,elemtype
    ref void p
    object pa

    switch sptr.tag
    when trefvar then
        a:=sptr.varptr

        pa:=a.objptr
        switch a.tag
        when tint,tword,trefpack then
            p:=&a.value
            elemtype:=ti64
        when treal then
            p:=&a.value
            elemtype:=tr64
        when tarray then
            p:=pa.ptr
            elemtype:=pa.elemtag
        when tbits then
            sptr.ptr:=pa.ptr
            sptr.bitoffset:=pa.indexoffset*ttbitwidth[pa.elemtag]
            sptr.bitlength:=0
            sptr.tagx:=trefbit
            sptr.elemtag:=pa.elemtag
            ++pcptr
            return

        when tset then
            sptr.ptr:=pa.ptr
            sptr.bitoffset:=0
            sptr.bitlength:=0
            sptr.tagx:=trefbit
            sptr.elemtag:=tu1
            ++pcptr
            return

        when tstring then
            p:=pa.strptr
            elemtype:=tu8
            if p=nil then
                p:=""
            fi
        when tstruct then
            p:=pa.ptr
            elemtype:=pa.usertag
        when tuserarray then
            p:=pa.ptr
            elemtype:=pa.usertag
        when tdecimal then
            p:=pa.num
            elemtype:=ti32

        else
            pcustype("Getrefpack1",a)
        end switch
    when trefpack,trefbit then
        ++pcptr
        return

    else
        pcustype("Getrefpack2",sptr)
    endswitch
done::

    sptr.tagx:=trefpack
    sptr.ptr:=p
    sptr.elemtag:=elemtype

    ++pcptr
end

global proc k_isdef=
    int res:=sptr.tag<>tvoid
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res
    ++pcptr
end

global proc k_isvoid=
    int res:=sptr.tag=tvoid
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res
    ++pcptr
end

global proc k_isint=
    istype(tint, tword)
end

global proc k_isnumber=
    int res

    if sptr.tag in [tint,treal,tdecimal,tword] then
        res:=1
    else
        res:=0
    fi
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res
    ++pcptr
end

global proc k_ismutable=
    int res

    if sptr.hasref then
        res:=sptr.objptr.mutable
    else
        res:=1
    fi
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res
    ++pcptr
end

global proc k_isreal=
    istype(treal)
end

global proc k_isrange=
    istype(trange)
end

global proc k_isset=
    istype(tset)
end

global proc k_ispointer=
    istype(trefvar,trefpack)
end

proc istype(int t1, t2=tvoid)=
    int res, t:=sptr.tag

    res:=t=t1
    if not res and t2 then
        res:=t=t2
    fi
    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=res
    ++pcptr
end

global proc k_convert=
    varrec x
    int t

    t:=getopnda

    if sptr.tag<>t then
        x:=sptr^
        var_convert(&x,t,sptr)
        var_unshare(&x)
    fi

    skip(1)
end

global proc k_switch=
    int index,n,lower

    n:=getopnda
    lower:=getopndb

    case sptr.tag
    when tint,ttype then
    else
        pcerror_s("switch not int",ttname[sptr.tag])
    esac

    index:=sptr.value-lower     

    --sptr

    if u64(index)>=u64(n) then          
        pcptr:=ref int((pcptr+n*2+4)^)
    else                    
        pcptr:=ref int((pcptr+index*2+4)^)  
    fi
end

global proc k_bytesize=
    int n,t,usert
    object p:=sptr.objptr

    t:=sptr.tag

    case t
    when ttype then
        t:=sptr.value
    when tstruct, trecord, tuserarray then
        t:=sptr.objptr.usertag
    esac

    case t
    when tarray then
        n:=p.length*ttsize[p.elemtag]
    when tset then
        n:=getbitssize(p.length,tu1)
    when tstring then
        n:=p.length
    when tbits then
        n:=bits_bytesize(p)
    when tlist then
        n:=p.length*varsize
    when trecord, tstruct, tuserarray then
        n:=ttsize[t]    
    when tdecimal then
        n:=p.length
        if n then
            n:=n*decelemsize
        fi
    else
        n:=ttsize[t]
    esac

    var_unshare(sptr)
    sptr.tagx:=tint
    sptr.value:=n

    ++pcptr
end

global proc k_bitwidth=
    if sptr.tag=ttype then
        sptr.value:=ttbitwidth[sptr.value]
    elsif ttbitwidth[sptr.tag] then
        sptr.value:=ttbitwidth[sptr.tag]
    else
        pcerror("bitwidth")
    fi

    sptr.tagx:=tint

    ++pcptr
end

global proc k_min=
    variant x,y

    y:=sptr--
    x:=sptr

    if var_compare(x,y)<0 then      
        var_unshare(y)
    else
        var_unshare(x)
        sptr^:=y^
    fi

    ++pcptr
end


global proc k_max=
    variant x,y

    y:=sptr--
    x:=sptr

    if var_compare(x,y)>=0 then     
        var_unshare(y)
    else
        var_unshare(x)
        sptr^:=y^
    fi

    ++pcptr
end

global proc k_addsp=
    sptr-:=getopnda
    skip(1)
end

global proc k_pushtry=
    (++sptr)^.tagx:=texception
    sptr.ptr:=ref byte(getopnda)
    sptr.frameoffset:=frameptr-ref byte(sptr)       
    sptr.exceptiontype:=getopndb
    sptr.nexceptions:=getopndc
    skip(3)
end

global proc k_raise=
    if sptr.tag<>tint then
        pcerror("Raise: not Int on stack [not proceeding direct to RAISE]")
    fi
    pcptr:=raiseexception(sptr.value)               
end

global proc k_isequal=
    variant x,y
    int res

    y:=sptr--
    x:=sptr

    if x.hasref and y.hasref and x.objptr=y.objptr then
        res:=1
    else
        res:=0
    fi

    var_unshare(x)
    var_unshare(y)
    sptr.tagx:=tint
    sptr.value:=res

    ++pcptr
end

global proc k_minto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_minto(px, y) then
        var_inplace(px,y, cast(var_min), cast(var_minmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_maxto=
    variant y:=sptr--
    variant px:=sptr--

    if not var_maxto(px, y) then
        var_inplace(px,y, cast(var_max), cast(var_maxmixed))
    end

    var_unshare(y)
    ++pcptr
end

global proc k_power=
    variant y:=sptr--
    varrec x:=sptr^

    var_power(sptr,y)

    var_unshare(&x)
    var_unshare(y)

    ++pcptr
end

proc domaths(int opcode)=
    switch sptr.tag
    when tint then
        sptr.tagx:=treal
        sptr.xvalue:=getmaths(opcode,sptr.value)

    when treal then
        sptr.xvalue:=getmaths(opcode,sptr.xvalue)

    else
        pcustype("Maths:",sptr)
    end switch
    ++pcptr
end

function getmaths(int opcode, real x)real=
    switch opcode
    when ksqrt then return sqrt(x)
    when ksin then return sin(x)
    when kcos then return cos(x)
    when ktan then return tan(x)
    when kasin then return asin(x)
    when kacos then return acos(x)
    when katan then return atan(x)
    when kln then return ln(x)
    when klog then return log(x)
    when kexp then return exp(x)
    when kround then
        if x>=0.0 then
            return floor(x+0.5)
        else
            return ceil(x-0.5)
        fi

    when kfloor then
        return floor(x)
    when kceil then
        x:=ceil(x)
        if x=0.0 then x:=0.0 FI
        return ceil(x)

    else
        pcerror_s("Maths",pclnames[opcode])
    end
    return 0.0
end

global proc k_typepun=
    sptr.tagx:=getopnda
    skip(1)
end

global proc k_andlto=
    variant y:=sptr--
    variant px:=sptr--
    variant x:=px.varptr

    if px.tag<>trefvar or x.tag<>tint then pcerror("andlto") fi

    x.value iand:=var_istruel(y)
    var_unshare(y)

    ++pcptr
end

global proc k_orlto=
    variant y:=sptr--
    variant px:=sptr--
    variant x:=px.varptr

    if px.tag<>trefvar or x.tag<>tint then pcerror("orlto") fi

    x.value ior:=var_istruel(y)
    var_unshare(y)

    ++pcptr
end

global proc k_notlto=
    variant px:=sptr--
    variant x:=px.varptr

    if px.tag<>trefvar or x.tag<>tint then pcerror("notlto") fi

    x.value ixor:=1

    ++pcptr
end

global proc k_pushoperator=
    ++sptr
    sptr.tagx:=toperator
    sptr.value:=getopnda
    skip(1)
end

global proc k_maps=
    k_mapss()
end

global proc k_mapss=
    static [10]int codeseq

    int nargs

    case sptr.tag
    when toperator then
        codeseq[1]:=cast(cmdmap[sptr.value])
        --sptr
        codeseq[2]:=(pcptr+1)^          
        codeseq[3]:=(pcptr+2)^          
    when tsymbol then
        nargs:=(cmdmap[pcptr^]=cmdmap[kmaps] |1|2)

        for i:=0 downto -(nargs+1) do               
            (sptr+i+1)^:=(sptr+i)^
        od
        (sptr-nargs).tagx:=tvoid
        ++sptr
        codeseq[1]:=cast(cmdmap[kcallptr])
        codeseq[2]:=nargs
        codeseq[3]:=0
        codeseq[4]:=(pcptr+1)^          
        codeseq[5]:=(pcptr+2)^          

    else
        pcerror("Apply:no op")
    esac
    pcptr:=&codeseq[1]              
end

global proc k_idivrem=
    PCERROR("IDIVREM")
end

global proc k_odd=
    case sptr.tag
    when tint,tword then
        sptr.value:=sptr.value.odd
    else
        pcustype("Odd",sptr)
    esac
    sptr.tagx:=tint
    ++pcptr
end

global proc k_even=
    case sptr.tag
    when tint,tword then
        sptr.value:=sptr.value.even
    else
        pcustype("Even",sptr)
    esac
    sptr.tagx:=tint
    ++pcptr
end

=== qq_host.m 0 0 11/43 ===
record dimrec=(mut int lbound, upper, length)

type hostproc0=ref proc
type hostproc1=ref proc(variant a)
type hostproc2=ref proc(variant a,b)
type hostproc3=ref proc(variant a,b,c)
type hostproc4=ref proc(variant a,b,c,d)

type hostfn0=ref proc(variant a)
type hostfn1=ref proc(variant a,b)
type hostfn2=ref proc(variant a,b,c)
type hostfn3=ref proc(variant a,b,c,d)
type hostfn4=ref proc(variant a,b,c,d,e)

record overloadrec=
    int optype, optype2
    ref int pchandler
    ref overloadrec nextrec
end

ref overloadrec tostr_list          
ref overloadrec convert_list

const noparamtag=tvoid
const nodefault=-999999

global [0..hostfnnames.upb]ref proc hosttable

global proc callhostfunction(int hostfn) =
    ref proc fnaddr
    int nparams,isfn
    object p

    fnaddr:=hosttable[hostfn]
    nparams:=hostnparams[hostfn]
    isfn:=hostisfn[hostfn]

    if fnaddr=nil then
        pcerror_s("Hostfn not implemented:",hostfnnames[hostfn])
    fi

    if isfn then        

        switch nparams
        when 0 then
            hostfn0(fnaddr)^(sptr)
        when 1 then
            hostfn1(fnaddr)^(sptr,sptr-1)
        when 2 then
            hostfn2(fnaddr)^(sptr,sptr-1,sptr-2)
        when 3 then
            hostfn3(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3)
        when 4 then
            hostfn4(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3,sptr-4)
        else
            pcerror("callhost/fn")
        endswitch
    else                    

        switch nparams
        when 0 then
            hostproc0(fnaddr)^()
        when 1 then
            hostproc1(fnaddr)^(sptr)
        when 2 then
            hostproc2(fnaddr)^(sptr,sptr-1)
        when 3 then
            hostproc3(fnaddr)^(sptr,sptr-1,sptr-2)
        when 4 then
            hostproc4(fnaddr)^(sptr,sptr-1,sptr-2,sptr-3)
        else
            pcerror("callhost/proc")
        endswitch
    fi

    to nparams do
        var_unshare(sptr) when sptr.hasref
        --sptr
    od
end

global proc inithostlib=

    ichar name
    int n:=$get_nprocs()

    for i to n do
        name:=$get_procname(i)
        if eqbytes(name,"pch_",4) then      
            for k:=0 to hostfnnames.upb do
                if eqstring(name+4,hostfnnames[k]+5) then       
                    hosttable[k]:=$get_procaddr(i)
                    exit
                fi
            else
                loaderror("Unknown hostfn",name)
            od
        fi
    od
end

proc pch_leftstr(variant a, b, c, result)=
    int n,length,padchar
    ref char s
    object pa

    padchar:=' '
    case c.tag
    when tvoid then
    when tstring then
        if c.objptr.length=1 then
            padchar:=c.objptr.strptr^
        else
            pcerror("left/padx")
        fi
    when tint then
        padchar:=c.value
    else
        pcerror("left/pad?")
    esac

    case b.tag
    when tvoid then
        n:=1
    when tint then
        n:=b.value
    else
        pcerror("left:bad n")
    esac
    if a.tag<>tstring then
        pcerror("left:not str")
    fi

    pa:=a.objptr
    length:=pa.length
    s:=pa.strptr

    if n=0 then
        var_empty_string(result,1)
        return
    fi

    result.tagx:=tstring ior hasrefmask
    if n>0 then         
        if n<=length then
            leftstring(a,n,result)
        else                
            padstring_right(a,n,padchar,result)
        fi
    else                    
        n:=-n
        if n<length then
            leftstring(a,length-n,result)
        else
            var_empty_string(result,1)
        fi
    fi
end

proc pch_rightstr(variant a, b, c, result)=
    int n,length,padchar
    ref char s
    object pa

    padchar:=' '
    case c.tag
    when tvoid then
    when tstring then
        if c.objptr.length=1 then
            padchar:=c.objptr.strptr^
        else
            pcerror("right/padx")
        fi
    when tint then
        padchar:=c.value
    else
        pcerror("right/pad?")
    esac

    case b.tag
    when tvoid then
        n:=1
    when tint then
        n:=b.value
    else
        pcerror("right:bad n")
    esac

    pa:=a.objptr
    if a.tag<>tstring then
        pcerror("right:not str")
    fi

    length:=pa.length
    s:=pa.strptr

    result.tagx:=tstring ior hasrefmask

    if n=0 then
        var_empty_string(result,1)
        return
    fi

    if n>0 then         
        if n<=length then
            rightstring(a,n,result)
        else                
            padstring_left(a,n,padchar,result)
        fi
    else                    
        n:=-n
        if n<length then
            rightstring(a,length-n,result)
        else
            var_empty_string(result,1)
        fi
    fi
end

proc pch_convlc(variant a, b, result)=
    checkparam(a,tstring)
    result^:=a^
    ++result^.objptr^.refcount
    var_duplu(result)
    var_iconvcase(result,b,0)
end

proc pch_convuc(variant a, b, result)=
    checkparam(a,tstring)
    result^:=a^
    ++result.objptr.refcount
    var_dupl(result) when result.hasref
    var_iconvcase(result,b,1)
end


proc pch_waitkey(variant result)=
    result.tagx:=tint
    result.value:=os_getch()
end

proc pch_execwait(variant a, b, c, result)=
    ref char workdir
    int flag
    object pa

    checkparam(a,tstring)
    pa:=a.objptr

    flag:=checkparam(b,tint,0)

    if c.tag=tvoid then
        workdir:=nil
    else
        checkparam(c,tstring)
        workdir:=convCstring(c.objptr.strptr,c.objptr.length)
    fi
    result.tagx:=tint
    result.value:=os_execwait(convCstring(pa.strptr,pa.length),flag,workdir)
end

proc pch_execcmd(variant a, b, c, result)=
    ref char workdir
    int flag
    object pa

    checkparam(a,tstring)
    pa:=a.objptr

    flag:=checkparam(b,tint,0)

    if c.tag=tvoid then
        workdir:=nil
    else
        checkparam(c,tstring)
        workdir:=convCstring(c.objptr.strptr,c.objptr.length)
    fi
    result.tagx:=tint
    result.value:=os_execcmd(convCstring(pa.strptr,pa.length),flag)
end

proc pch_makestr(variant a, b, result)=
    int n

    switch a.tag
    when trefpack then
    when tint then
    else
        pcerror("makestr")
    endswitch

    n:=var_getintvalue(b)

    RESULT.TAGX:=TSTRING IOR HASREFMASK

    RESULT.OBJPTR:=obj_make_strslicexobj(cast(a.ptr),n)
end

proc pch_makeref(variant a,b,result) =
    ref byte ptr

    switch (ttbasetype[a.tag])
    when trefvar,trefpack,tint then
        ptr:=a.ptr
    when tstring,tarray,tlist,tset then
        ptr:=a.objptr.ptr
    else
        pcerror("makeref")
    endswitch

    result.tagx:=trefpack
    result.ptr:=ptr
    result.elemtag:=var_getintvalue(b)

    case result.elemtag
    when tu1,tu2,tu4 then
        result.tag:=trefbit
        result.bitoffset:=0
        result.bitlength:=0
    esac
end

proc pch_getcmdparam(variant a, result)=

    int n
    ref char s

    if a.tag=noparamtag then        
        result.tagx:=tint
        result.value:=nqparams
        return
    fi

    n:=var_getintvalue(a)
    if n not in 1..nqparams then pcerror("getcmdpm") fi

    var_make_string(qparamtable[n],result)
end

proc pch_clock(variant result)=
    result.tagx:=tint
    result.value:=os_clock()
end

proc pch_ticks(variant result)=
    result.tagx:=tint
    result.value:=os_ticks()
end

proc pch_sleep(variant a)=
    checkparam(a,tint)
    os_sleep(a.value)
end

proc pch_random(variant a, result)=

    int n,x

    result.tagx:=tint           

    if a.tag=trange then
        x:=mrandomrange(a.range_lower, a.range_upper)
    else
        checkparam(a,tint)
        n:=a.value
        if n>1 then                 
            x:=mrandomint(n)
        elsif n=0 then              
            x:=mrandom()
        elsif n=1 then              
            result.tagx:=treal
            result.xvalue:=mrandomreal()
            return
        else
            mseed(-n)
            x:=0
        fi
    fi
    result.value:=x
end

proc pch_system(variant a,result) =     
    checkparam(a,tstring)
    result.tagx:=tint
    result.value:=system(convCstring(a.objptr.strptr,a.objptr.length))
end


proc pch_$getparam(variant a, result)=
    checkparam(a,tint)

    result^:=variant(frameptr-a.value*varsize)^     
    if result.hasref then
        ++result.objptr.refcount
    fi
end


function checkparam(variant p,int tag,defaultx=nodefault)int64=

    case p.tag
    when tvoid then
        if defaultx=nodefault then
            pcerror("Missing host param")
        fi
        return defaultx
    when tag then
        return p.value
    esac

    if tag=tint then
        case p.tag
        when treal then
            return p.xvalue
        esac
    fi

    cpl ttname[p.tag]
    pcerror("Host param wrong type")
    return 0
end

proc leftstring(variant a, int n, variant result)=
    object p


    var_make_stringn(a.objptr.strptr,n,result,1)
end

proc rightstring(variant a, int n, variant result)=
    object p

    var_make_stringn(a.objptr.strptr+(a.objptr.length-n),n,result,1)
end

proc padstring_right(variant a,int n, fillchar, variant result)=
    ref char s
    int length

    length:=a.objptr.length

    var_new_stringn(n,result)
    s:=result.objptr.strptr

    if length then
        memcpy(s,a.objptr.strptr,length)
        s+:=length
    fi
    to n-length do
        s^:=fillchar
        ++s
    od
end

proc padstring_left(variant a,int n, fillchar, variant result)=
    ref char s
    int length,padlen

    length:=a.objptr.length
    padlen:=n-length

    var_make_stringn(nil,n,result,0)

    s:=result.objptr.strptr
    s+:=padlen

    if length then
        memcpy(s,a.objptr.strptr,length)
    fi
    to padlen do
        --s
        s^:=fillchar
    od
end

proc getbounds(variant p,ref dimrec dims,int lower) =
    int n

    if not p then
        pcerror("New: no bounds")
    fi

    switch p.tag
    when noparamtag then
        dims.lbound:=lower
        dims.upper:=0
        dims.length:=0
    when trange then
        dims.lbound:=p.range_lower
        dims.upper:=p.range_upper
        dims.length:=p.range_upper-p.range_lower+1
        if dims.length<0 then
            dims.length:=0
            dims.upper:=dims.lbound-1
        fi
    else
        n:=var_getintvalue(p)
        dims.lbound:=lower
        dims.upper:=dims.length:=n
    endswitch
end

proc pch_new(variant a, b, c, d, result)=
    varrec v
    int i,t,nbytes,ival,nwords,nbits,offset,elemtype,n, usertag
    dimrec dims
    variant qvar
    ref int64 qint
    ref byte qbyte
    ref byte ptr
    object p

    t:=var_getintvalue(a)

    if t<0 or t>ntypes then
        pcustype_t("New:bad type",t)
    fi
    v.tagx:=t ior hasrefmask
    usertag:=0

    switch ttbasetype[t]
    when tstring then
        var_new_string(b,c, result)
        return

    when tlist then
        getbounds(b,&dims,1)
        p:=obj_newlist(dims.length,dims.lbound,c)

        v.objptr:=p

    when tarray then
        elemtype:=var_getintvalue(b)
        getbounds(c,&dims,1)
        if elemtype>=tu1 and elemtype<=tu4 then
            v.tag:=t:=tbits
            goto dobits2
        fi
        p:=obj_newarray(elemtype, dims.lbound, dims.length)

doarray2::
        v.objptr:=p

        if dims.length then
            if d and d.tag<>tvoid then      
                qbyte:=p.ptr
                to dims.length do
                    var_storepacked(qbyte,d,elemtype)
                    qbyte+:=ttsize[elemtype]
                od
            fi
        fi
    when tpackarray then
        usertag:=t
        v.tag:=tuserarray
        elemtype:=tttarget[t]
        dims.length:=ttlength[t]
        dims.lbound:=ttlower[t]
        dims.upper:=dims.length+dims.lbound-1

        d:=b                    
        p:=obj_newarray_u(t)
        goto doarray2
    when tbits then
        elemtype:=var_getintvalue(b)
        if elemtype not in tu1..tu4 then
            pcerror("new: bad bits elem")
        fi
        getbounds(c,&dims,1)
dobits2::               

        p:=obj_newbits(elemtype,dims.lbound,dims.length)
        v.objptr:=p

        if dims.length then
            if d and d.tag<>tvoid then      
                qbyte:=p.ptr

                offset:=0
                to dims.length do
                    var_storebit(qbyte,offset,d,elemtype,0)
                    offset+:=ttbitwidth[elemtype]
                    if offset>=8 then
                        offset:=0
                        ++qbyte
                    fi
                od
            fi
        fi
    when tset then
        getbounds(b,&dims,0)

        if dims.lbound<0 then
            pcerror("new:set:lwb")
        fi
        if dims.lbound<>0 then
            dims.lbound:=0
            dims.length:=dims.upper+1
        fi

        p:=obj_newset(dims.length)
        v.objptr:=p
    when trecorddef then
        p:=obj_new_record(t,b)
        var_fromobj(t,p,&v)
        v.tag:=trecord
        usertag:=t

    when tpackrecord then

        p:=obj_new_struct(t)

        var_objtovar(t,p,&v)
        v.tag:=tstruct
        usertag:=t

        if b and b.tag<>tvoid then
            pcerror("New: struct init")
        fi

    when tint,tword,treal,trefvar then
        v.value:=0
        v.hasref:=0
        if b and b.tag<>tvoid then
            pcerror("NEW(int/value)")
        fi

    when tdict then
        getbounds(b,&dims,1)
        if dims.lbound<>1 then
            pcerror("new:dict:lwb")
        fi
        p:=obj_new_dict(dims.length)
        v.objptr:=p

    when tdecimal then
        var_empty_dec(result)
        return

    else
        pcustype_t("new",t)
    endswitch
finish::

    if usertag then
        v.objptr.usertag:=usertag
    fi

    result^:=v

end

global proc pch_gethostname(variant result) =
    static [256]char name

    strcpy(name,os_gethostname())

    var_make_string(name,result)
end

global proc pch_getprogname(variant result) =
    static [256]char name

    strcpy(name,inputfile)

    var_make_string(name,result)
end

proc pch_$test2(variant a,b, result)=
    RESULT.TAGX:=TVOID
end

proc pch_$test(variant a, result)=
OBJECT P
P:=A.OBJPTR

CPL "$TEST:",=A, =P, =P.VARPTR^.VALUE,=REF VOID(P.VARPTR)
RESULT.TAGX:=TVOID
end

proc pch_$refcount(variant a, result)=
    result.tagx:=tint
    if a.hasref then
        result.value:=a.objptr.refcount
    else
        result.value:=0
    fi
end

proc pch_testkey(variant result)=
    result.tagx:=tint
    result.value:=os_kbhit()
end

proc pch_getos(variant result)=
    var_make_string(os_getos(),result)
end

proc pch_$procsymbols(variant result)=
    object p
    variant q
    ref procrec pp

    p:=obj_newlist(nproclist,1)
    q:=p.varptr
    pp:=proclist

    to nproclist do
        q.tagx:=tsymbol
        q.def:=pp.def
        ++q
        pp:=pp.nextproc
    od

    result.tagx:=tlist ior hasrefmask
    result.objptr:=p
end

proc pch_$symbolowner(variant a, result)=
    checkparam(a,tsymbol)

    result.tagx:=tsymbol
    result.def:=a.def.owner
end

proc pch_$symbolname(variant a, result)=
    checkparam(a,tsymbol)

    var_make_string(a.def.name,result)
end

proc pch_$symboldefs(variant a, result)=
    object p
    variant q
    symbol d,e
    int n

    checkparam(a,tsymbol)

    d:=a.def
    e:=d.deflist
    n:=0
    while e do
        ++n
        e:=e.nextdef
    od

    p:=obj_newlist(n,1)
    q:=p.varptr
    e:=d.deflist

    to n do
        q.tagx:=tsymbol
        q.def:=e
        ++q
        e:=e.nextdef
    od

    result.tagx:=tlist ior hasrefmask
    result.objptr:=p
end

global proc pch_setmesshandler(variant fn)=
    if fn.tag<>tsymbol or fn.def.nameid<>procid then
        pcerror("Not proc ref")
    fi
    pcl_callbackfn:=cast(fn.def.pcaddress)
    os_setmesshandler(&runproc_m)
end

proc pch_$testcallback=
    varrec v

    v.tagx:=tint
    v.value:=123456
    CPL "TEST CALLBACK ????"
end

proc pch_$smallmemtotal(variant result)=
    result.tagx:=tint
    result.value:=smallmemtotal/varsize
end

proc pch_$id(variant a, result)=
    result.tagx:=tint
    result.value:=a.value
end

global proc pch_iswindows(variant result)=
    result.tagx:=tint
    result.value:=os_iswindows()
end

proc pch_$setdebug(variant a)=
    checkparam(a,tint)

    CPL "SETDEBUG................."
    fdebug:=a^.value
end

proc pch_copy(variant a, dest)=
    dest^:=a^
    var_dupl(dest)
end

proc pch_gethash(variant a,result) =        
    result.tagx:=tint
    result.value:=var_gethashvalue(a)
end

proc pch_makeempty(variant a,result)=
    object p
    int t

    t:=ttbasetype[a^.tag]
    if t=ttype then
        t:=a^.value
    fi

    p:=a.objptr

    case t
    when tlist then
        var_empty_list(p.lower16,result)
        return

    when tstring then
        p:=emptystring
        ++p.refcount
    when tarray then
        var_empty_array(t, p.elemtag, p.lower,result)
        return


    else
        pcustype_t("makeempty?",t)
    esac

    result.tagx:=t ior hasrefmask
    result.objptr:=p
end

proc pch_$infinity(variant dest)=
    var_setinf(dest)
end

proc pch_$nan(variant dest)=
    var_setnan(dest)
end

global proc setcmdparam(int index, ichar s)=


    if s=nil then
        nqparams:=index
    elsif index<=maxqparam then
        qparamtable[index]:=pcm_copyheapstring(s)
        nqparams max:=index
    fi

end

=== qq_lex.m 0 0 12/43 ===
const etx   = 26
const cr    = 13
const lf    = 10
const tab   = 9

ref char lxsource       
ref char lxstart        
ref char lxsptr
int lxifcond
int longsuffix          
int lxfileno

const hstsize   = 65536
const hstmask   = hstsize-1

int nextlxlength
global int lxlength

global array [0:hstsize]strec hashtable
symbol hashtablelast

const maxstackdepth=20
array [maxstackdepth]ref char lxstart_stack
array [maxstackdepth]ref char lxsptr_stack
array [maxstackdepth]int lxfileno_stack
array [maxstackdepth]lexrec lxnextlx_stack
global int sourcelevel=0

ichar u64maxstr="18446744073709551615"


global proc lexreadtoken=
int c,csum,hsum,commentseen
ref char pstart,pnext,p,ss

    nextlx.subcode:=0

    doswitch lxstart:=lxsptr; lxsptr++^
    when 'a'..'e','g'..'z','$','_' then
    dolower::
        nextlx.svalue:=lxsptr-1
    doname::
        hsum:=nextlx.svalue^

        doswitch c:=lxsptr++^
        when 'A'..'Z' then
            (lxsptr-1)^:=c+' '
            hsum:=hsum<<4-hsum+c+' '
        when 'a'..'z','0'..'9','_','$' then
            hsum:=hsum<<4-hsum+c
        else
            --lxsptr
            exit
        end doswitch


        lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

        return

    when 'A'..'E','G'..'Z' then
    doupper::
        nextlx.svalue:=lxsptr-1
        nextlx.svalue^+:=32
        goto doname

    when 'f' then
        if lxsptr^<>'"' then
            goto dolower
        fi
        readrawstring()
        return

    when 'F' then
        if lxsptr^<>'"' then
            goto doupper
        fi
        readrawstring()
        return

    when '0'..'9' then
        case lxsptr^
        when ')',cr,',',' ' then        
            nextlx.symbol:=intconstsym
            nextlx.subcode:=tint
            nextlx.value:=lxstart^-'0'
        when 'x','X' then
            case lxstart^
            when '0' then       
                ++lxsptr
                readhex()
            when '2' then
                ++lxsptr
                readbin()
            else
                lxerror("Bad base")
            esac
        else
            --lxsptr
            readdec()
        esac
        return

    when '!' then           
    docomment::

        doswitch c:=lxsptr++^
        when 13 then
            ++lxsptr
            exit
        when 10 then
            exit
        when etx,0 then
            --lxsptr
            exit
        end
        nextlx.symbol:=eolsym
        return

    when '#' then           
        nextlx.svalue:=cast(lxsptr)

        doswitch c:=lxsptr++^
        when 13,10,etx,0 then           
            --lxsptr
            exit
        end

        nextlxlength:=lxsptr-ref char(nextlx.svalue)
        nextlx.symbol:=docstringsym

        return

    when '\\' then          

        commentseen:=0
        doswitch lxsptr++^          
        when cr then
            ++lxsptr                
            exit
        when lf then
            exit
        when etx,0 then
            nextlx.symbol:=eofsym
            --lxsptr
            return
        when ' ',tab then
        when '!' then
            commentseen:=1
        else
            if not commentseen then
                lxerror("\\ not followed by eol")
            fi
    enddoswitch

        doswitch lxsptr++^
        when cr then
            ++lxsptr                
        when lf then
        when ' ',tab then
        else
            --lxsptr
            exit
        enddoswitch

    when '{' then
        nextlx.symbol:=lcurlysym
        return

    when '}' then
        nextlx.symbol:=rcurlysym
        return

    when '.' then
        switch lxsptr^
        when '.' then               
            ++lxsptr
            if lxsptr^='.' then
                ++lxsptr
                nextlx.symbol:=ellipsissym
            else
                nextlx.symbol:=rangesym
                nextlx.subcode:=jmakerange      
            fi
            return
        when '0'..'9' then          
            --lxsptr
            readreal()
            return
        else
            nextlx.symbol:=dotsym
            return
        endswitch

    when ',' then
        nextlx.symbol:=commasym
        return

    when ';' then
        nextlx.symbol:=semisym
        return

    when ':' then
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=assignsym
            nextlx.subcode:=jassign         
        when ':' then
            ++lxsptr
            case lxsptr^
            when '=' then
                ++lxsptr
                nextlx.symbol:=deepcopysym
                nextlx.subcode:=jdeepcopy
            else
                nextlx.symbol:=dcolonsym
            esac
        else
            nextlx.symbol:=colonsym
        endswitch
        return

    when '(' then
        nextlx.symbol:=lbracksym
        return

    when ')' then
        nextlx.symbol:=rbracksym
        return

    when '[' then
        nextlx.symbol:=lsqsym
        return

    when ']' then
        nextlx.symbol:=rsqsym
        return

    when '|' then
        if lxsptr^='|' then
            ++lxsptr
            nextlx.symbol:=dbarsym
        else
            nextlx.symbol:=barsym
        fi
        return

    when '^' then
        nextlx.symbol:=ptrsym
        nextlx.subcode:=jptrto
        return

    when '@' then
        if lxsptr^='@' then
            ++lxsptr
            nextlx.symbol:=datsym
        else
            nextlx.symbol:=atsym
        fi
        return

    when '?' then
        nextlx.symbol:=questionsym
        return

    when '~' then
        nextlx.symbol:=curlsym
        return

    when '+' then
        nextlx.symbol:=addsym
        if lxsptr^='+' then
            ++lxsptr
            nextlx.symbol:=incrsym
            nextlx.subcode:=jincrload
            return
        else
            nextlx.subcode:=jadd
        fi
        return

    when '-' then
        nextlx.symbol:=subsym
        if lxsptr^='-' then
            ++lxsptr
            nextlx.symbol:=incrsym
            nextlx.subcode:=jdecrload
            return
        else
            nextlx.subcode:=jsub
        fi
        return

    when '*' then
        nextlx.symbol:=mulsym
        if lxsptr^='*' then
            ++lxsptr
            nextlx.symbol:=powersym
            nextlx.subcode:=jpower
        else
            nextlx.subcode:=jmul
        fi
        return

    when '/' then
        nextlx.symbol:=divsym
        nextlx.subcode:=jdiv
        return

    when '%' then
        nextlx.symbol:=idivsym
        nextlx.subcode:=jidiv
        return

    when '=' then
        case lxsptr^
        when '>' then
            nextlx.symbol:=sendtosym
            ++lxsptr
        when '=' then
            nextlx.symbol:=isequalsym
            nextlx.subcode:=jisequal

            ++lxsptr
        else
            nextlx.symbol:=eqsym
            nextlx.subcode:=jeq
        esac
        return

    when '<' then
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=lesym
            nextlx.subcode:=jle
        when '>' then
            ++lxsptr
            nextlx.symbol:=nesym
            nextlx.subcode:=jne
        when '<' then
            ++lxsptr
            nextlx.symbol:=shlsym
            nextlx.subcode:=jshl
        else
            nextlx.symbol:=ltsym
            nextlx.subcode:=jlt
        endswitch
        return

    when '>' then
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=gesym
            nextlx.subcode:=jge
        when '>' then
            ++lxsptr
            nextlx.symbol:=shrsym
            nextlx.subcode:=jshr
        else
            nextlx.symbol:=gtsym
            nextlx.subcode:=jgt
        endswitch
        return

    when '&' then
        case lxsptr^
        when '&' then
            ++lxsptr
            nextlx.symbol:=daddrsym
        when '.' then
            ++lxsptr
            nextlx.symbol:=anddotsym
        else
            nextlx.symbol:=addrsym
            nextlx.subcode:=jaddrof
        esac
        return

    when '\'' then
        lxreadstring('\'')
        return

    when '"' then
        lxreadstring('"')
        return

    when '`' then
        readrawxname()
        return

    when ' ',tab then

    when cr then
        ++lxsptr                
        nextlx.symbol:=eolsym
        return
    when lf then            
        nextlx.symbol:=eolsym
        return

    when etx,0 then
        nextlx.symbol:=eofsym
        --lxsptr
        return

    else
        nextlx.symbol:=errorsym
        nextlx.value:=c
        return

    end doswitch

end

proc lxreadstring(int termchar)=

    ichar dest,pstart
    int c,d,length,hasescape
    array [8]char str

    if termchar='"' then
        nextlx.symbol:=stringconstsym
    else
        nextlx.symbol:=charconstsym
        nextlx.subcode:=tint
    fi

    pstart:=lxsptr

    length:=0
    hasescape:=0

    doswitch c:=lxsptr++^
    when '\\' then          
        c:=lxsptr^
        if c in 'A'..'Z' then c+:=' ' fi
        ++lxsptr
        hasescape:=1

        switch c
        when 'a','b','c','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
            ++length
        when 'w' then
            length+:=2
        when 'x' then   
            lxsptr+:=2
            ++length
        else
            lxerror("Bad str escape")
        endswitch
    when '"','\'' then      
        if c=termchar then      
            if lxsptr^=c then       
                hasescape:=1
                ++lxsptr
                ++length
            else            
                exit
            fi
        else
            ++length
        fi
    when cr,lf,0 then
        lxerror("String not terminated")
    else
        ++length
    end doswitch

    nextlxlength:=length

    if length=0 then
        nextlx.svalue:=""
        return
    elsif not hasescape then
        nextlx.svalue:=pcm_copyheapstringn(pstart,length)
        return
    fi

    nextlx.svalue:=dest:=pcm_alloc(length+1)

    do
        switch c:=pstart++^
        when '\\' then          
            c:=pstart^
            if c>='A'  and c<='Z' then c+:=' ' fi
            ++pstart
            switch c
            when 'a' then           
                c:=7
            when 'b' then           
                c:=8
            when 'c','r' then       
                    c:=cr
            when 'e' then           
                c:=26
            when 'f' then           
                c:=12
            when 'l','n' then       
                c:=lf
            when 's' then           
                c:=27
            when 't' then           
                c:=9
            when 'v' then           
                c:=11
            when 'w' then           
                dest++^:=cr
                c:=lf
            when 'x' then   
                c:=0
                to 2 do
                    case d:=pstart++^
                    when 'A','B','C','D','E','F' then
                        c:=c*16+d-'A'+10
                    when 'a','b','c','d','e','f' then
                        c:=c*16+d-'a'+10
                    when '0','1','2','3','4','5','6','7','8','9' then
                        c:=c*16+d-'0'
                    else
                        lxerror("Bad \\x code")
                    esac
                od
            when 'y' then           
                c:=16
            when 'z','0' then       
                c:=0
            when '"','Q' then       
                c:='"'
            when '\\' then
                c:='\\'
            when '\'' then          
                c:='\''
            else
                str[1]:=c; str[2]:=0
                lxerror_s("Unknown string escape: \\%s",&.str)
            end
        when '"','\'' then      
            if c=termchar then      
                if pstart^=c then       
                    ++pstart
                else            
                    exit
                fi
            fi
        when cr,lf,etx,0 then
            lxerror("String not terminated")
        endswitch

        dest++^:=c
    od
    (nextlx.svalue+nextlxlength)^:=0
end

global proc printsymbol(ref lexrec lp)=
    lexrec l
    l:=lp^

    printf("%-18s",symbolnames[l.symbol])

    case l.symbol
    when namesym then

        printstrn(l.symptr^.name,l.symptr^.namelen)
    when intconstsym then
        case l.subcode
        when tint then print l.value,"int"
        when tword then print l.uvalue,"word"
        else print l.value
        esac

    when realconstsym then
        print l.xvalue

    when stringconstsym then
        print """",$
        printstr(l.svalue)
        print $,""""
    when charconstsym then
        print "'",$
        printstr(l.svalue)
        print $,"'"
    when decimalconstsym then
        printstr(l.svalue)
        print "L"
    when assignsym,addrsym,ptrsym,deepcopysym,rangesym then
        print jtagnames[l.subcode]
    elsif l.subcode then
        print "#",l.subcode
    end

    println

end


global proc lexinit=
    int i
    static int n


    memset(&hashtable,0,hashtable.bytes)
    hashtablelast:=&hashtable[hstsize-1]

    inithashtable()
end

global proc printstrn(ichar s, int length)=
    if length then
        printf("%.*s",length,s)
    fi
end

proc printstr(ichar s)=
    print(s)
end

proc readrawstring=
    ichar pstart
    int length

    nextlx.symbol:=stringconstsym

    pstart:=++lxsptr
    length:=0

    doswitch lxsptr++^
    when '"' then
        exit
    when cr,lf,0 then
        lxerror("Raw string not terminated")
        --lxsptr
        exit
    else
        ++length
    enddoswitch

    nextlxlength:=length

    nextlx.svalue:=pcm_copyheapstringn(pstart,length)
end

global function lookup(ichar name, int length, hashindex)int=
    int j,wrapped,n
    symbol d
    ref char s

    d:=&hashtable[hashindex]
    wrapped:=0



    do

        if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then   
            nextlx.symptr:=d
            nextlx.symbol:=d.symbolcode
            nextlx.subcode:=d.subcode
            return 1
        elsif n=0 then
            exit
        fi

        if ++d>hashtablelast then
            if wrapped then
                abortprogram("HASHTABLE FULL")
            fi
            wrapped:=1
            d:=&hashtable[0]
        fi
    od

    d^.name:=pcm_copyheapstringn(name,length)
    d^.namelen:=length
    d^.symbolcode:=namesym

    nextlx.symptr:=d
    nextlx.symbol:=d.symbolcode
    nextlx.subcode:=d.subcode

    return 0
end

global function gethashvaluez(ichar s)int=
    int c,hsum

    if s^=0 then return 0 fi

    hsum:=s++^

    do
        c:=s++^
        exit when c=0
        hsum:=hsum<<4-hsum + c
    od
    return (hsum<<5-hsum) iand hstmask
end

proc inithashtable=
    int i
    ichar name

    for i:=1 to stnames.len do
        addstname(stnames[i], stsymbols[i], stsubcodes[i])
    od

    for i to hostfnnames.upb when not hostinternal[i] do
        name:=hostfnnames[i]+5              
        addstname(name, khostfnsym, i)

    od
end

proc addstname(ichar name, int symbol, subcode)=
    if lookup(name,strlen(name),gethashvaluez(name)) then
        println name
        abortprogram("Dupl ST entry")
    fi

    nextlx.symptr.symbolcode:=symbol
    nextlx.symptr.subcode:=subcode
end

proc lexreadline=

    doswitch lxsptr^
    when cr,lf then
        return
    when etx,0 then
        --lxsptr
        return
    else
        ++lxsptr
    enddoswitch
END

global proc startlex(int fileno, moduleno)=


    if not fwriteqa then
        lxsource:=lxsptr:=sourcefiletext[fileno]
    else
        lxsource:=lxsptr:=pcm_copyheapstring(sourcefiletext[fileno])
    fi
    lxfileno:=fileno


    nextlx.symbol:=semisym
    nextlx.subcode:=0
    nextlx.moduleno:=moduleno
end

global function addnamestr(ichar name)symbol=
    lexrec oldlx
    symbol symptr

    oldlx:=nextlx

    nextlxlength:=strlen(name)
    nextlx.svalue:=pcm_alloc(nextlxlength+1)
    memcpy(nextlx.svalue,name,nextlxlength+1)
    lookup(nextlx.svalue, nextlxlength, gethashvaluez(name))
    symptr:=nextlx.symptr

    nextlx:=oldlx

    return symptr
end

global proc PS1(ichar caption)=
    print caption,,":::"
    printsymbol(&lx)
end

global proc PS2(ichar caption)=
    print " ",,caption,,":##"
    printsymbol(&nextlx)
end

global proc PS(ichar caption)=
    PS1(caption)
end

global proc lex=
    int lineno,n,dir,namelen
    ref char p
    symbol symptr

    lx:=nextlx              

    lxlength:=nextlxlength
    lx.sourceoffset:=lxstart-lxsource

    reenter::

    lexreadtoken()          
    reenter2::

    switch nextlx.symbol
    when unitnamesym then                   
        case lx.symbol
        when intconstsym then
            case nextlx.symptr^.subcode
            when million_unit then lx.value *:= 1 million
            when billion_unit then lx.value *:= 1 billion
            when thousand_unit then lx.value *:= 1 thousand
            else
                lxerror("Can't do this unit index")
            esac
            lx.subcode:=setinttype(lx.value)
            goto reenter
        when realconstsym then
            lxerror("unit symbol after float?")
        else
            nextlx.symbol:=namesym              
        esac

    when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,kforallsym,
            kdosym,ktosym,kprocsym,kfunctionsym,kimportdllsym,kunlesssym,
            krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,
            ktrysym,ktabledatasym,kifsym then

        if lx.symbol=kendsym then
            lx.subcode:=nextlx.symbol           
            goto reenter
        fi
    when sysconstsym then                   
        case nextlx.subcode
        when con_const then
            nextlx.symbol:=intconstsym
            nextlx.value:=0
            nextlx.subcode:=tint
        when pi_const then
            nextlx.symbol:=realconstsym
            nextlx.xvalue:=pi
            nextlx.subcode:=treal
        when tab_const then
            nextlx.symbol:=stringconstsym
            nextlx.svalue:="\t"
            nextlxlength:=1
        else
            lxerror("sysconst?")
        esac

    when eolsym then

        switch lx.symbol
        when commasym, lsqsym, lbracksym, 
             assignsym,semisym then

            lexreadtoken()
            goto reenter2

            goto reenter
        elsif binopset[lx.symbol] and   lx.symbol not in [maxsym, minsym] then
            lexreadtoken()
            goto reenter2

        end switch
        nextlx.symbol:=semisym

    when insym then
        if lx.symbol=notlsym then
            lx.symbol:=notinsym
            lx.subcode:=jnotin
            goto reenter
        fi
    endswitch

end

global proc showhashtablesize=
    int i,n
SYMBOL D

    n:=0
    for i:=0 to hstmask do
        D:=&HASHTABLE[I]
        if D.name then
CPL "HASHTABLE",I,D.NAME, NAMENAMES[D.NAMEID], =D.NEXTDUPL
            ++n
        fi
    od
end

global function checkname(ichar name,int length=0)int=

    if length=0 then
        length:=strlen(name)
    fi
    if nextlxlength=length and memcmp(nextlx.svalue,name,length)=0 then
        return 1
    fi
    return 0
end

function setinttype(word64 a)int=
    if a<u64(0x7FFF'FFFF'FFFF'FFFF) then
        return tint

    else
        return tword
    fi
end

global proc lxerror_s(ichar mess,a)=
    array [256]char str
    fprint @str,mess,a
    lxerror(&.str)
end

global proc stacksource(int fileno)=

    if sourcelevel>=maxstackdepth then
        lxerror("Include file/macro overflow")
    fi
    ++sourcelevel
    lxstart_stack[sourcelevel]:=lxstart
    lxsptr_stack[sourcelevel]:=lxsptr
    lxfileno_stack[sourcelevel]:=lxfileno
    lxnextlx_stack[sourcelevel]:=nextlx

    lxstart:=lxsptr:=sourcefiletext[fileno]

    nextlx.pos:=1
    lxfileno:=fileno

    nextlx.symbol:=semisym
    nextlx.subcode:=0
end

global proc unstacksource=
    if sourcelevel>0 then           
        lxstart:=lxstart_stack[sourcelevel]
        lxsptr:=lxsptr_stack[sourcelevel]
        nextlx:=lxnextlx_stack[sourcelevel]
        lxfileno:=lxfileno_stack[sourcelevel]
        --sourcelevel
    fi
end



proc makedecimal(ichar s, int length,base)=

    if base<>10 then
        LXERROR("MAKEDECIMAL/16/2")
    fi


    nextlx.symbol:=decimalconstsym
    nextlx.subcode:=tdecimal
    nextlx.svalue:=pcm_copyheapstringn(s,length)
    nextlxlength:=length
end

proc readdec=
    int c
    ref char dest, destend, pstart
    int islong, length
    array [1024]byte str
    word a

    islong:=0

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'9' then
            a:=a*10+c-'0'
            dest++^:=c
        when 'e','E' then
            lxsptr:=pstart
            readreal()
            return
        when '.' then
            if lxsptr^<>'.' then
                lxsptr:=pstart
                readreal()
                return
            fi
            --lxsptr
            exit

        when '_','\'' then
        when 'l','L' then
            dest^:=0
            makedecimal(&.str,dest-&.str,10)
            return

        when 'b','B' then
            length:=dest-&.str
            if length>64 then lxerror("bin overflow") fi
            dest:=&.str
            a:=0
            to length do
                if dest^>='2' then lxerror("bad bin digit") fi
                a:=a*2+dest++^-'0'
            od
            finish

        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("Numlit too long") fi
    end
    length:=dest-&.str

    if length>20 or length=20 and strncmp(&.str,u64maxstr,20) then
        makedecimal(&.str,length,10)
        return
    fi

finish::
    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readhex=
    int c
    ref char dest, destend, pstart
    int length
    array [1024]byte str
    word a

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'9' then
            a:=a*16+c-'0'
            dest++^:=c

        when 'A'..'F' then
            dest++^:=c
            a:=a*16+c-'A'+10
        when 'a'..'f' then
            dest++^:=c-32
            a:=a*16+c-'a'+10

        when '_','\'' then
        when 'l','L' then
            dest^:=0
            makedecimal(&.str,dest-&.str,16)
            return

        when '.' then
            --lxsptr
            exit

        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("Numlit too long") fi
    end
    length:=dest-&.str

    if length>16 then
        makedecimal(&.str,length,16)
        return
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readbin=
    int c
    ref char dest, destend, pstart
    int length
    array [1024]byte str
    word a

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'1' then
            a:=a*2+c-'0'
            dest++^:=c

        when '_','\'' then
        when 'l','L' then
            dest^:=0
            makedecimal(&.str,dest-&.str,2)
            return

        when '2'..'9' then
            lxerror("bin bad digit")
        when '.' then
            --lxsptr
            exit

        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("bin overflow") fi
    end
    length:=dest-&.str

    if length>64 then
        makedecimal(&.str,length,2)
        return
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end


proc readreal=

    int c,n,negexpon,dotseen,length, fractlen, expon, expseen
    real x
    array [1024]char str
    ichar dest, destend, pexpon


    dest:=&.str
    destend:=dest+str.len-100
    length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

    do
        switch c:=lxsptr++^
        when '0'..'9' then
            dest++^:=c
            ++length
            if dotseen then ++fractlen fi
        when '.' then
            if dotseen then --lxsptr; exit fi
            dotseen:=1
            dest++^:=c


        when 'e','E' then
            if expseen then lxerror("double expon") fi
            expseen:=1
            dest++^:=c
            while lxsptr^=' ' do ++lxsptr od
            if lxsptr^ in ['+','-'] then
                if lxsptr^='-' then negexpon:=1 fi
                dest++^:=lxsptr++^
            fi

            expon:=0
            doswitch c:=lxsptr++^
            when '0'..'9' then
                expon:=expon*10+c-'0'
                dest++^:=c
                if dest>=destend then lxerror("expon?") fi

            when '_','\'' then
            when 'l','L' then
                dest^:=0
                makedecimal(&.str,dest-&.str,10)
                return
            else
                --lxsptr
                exit all
            end doswitch

        when '_','\'' then

        when 'l','L' then
            makedecimal(&.str,dest-&.str,10)
            return
        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("r64lit too long") fi
    end
    dest^:=0




    if negexpon then expon:=-expon fi
    expon-:=fractlen
    x:=0.0

    for i:=1 to length+dotseen do       
        c:=str[i]
        if c<>'.' then
            x:=x*10.0+c-'0'
        fi
    od

    if expon>=0 then
        to expon do
            x*:=10.0
        od
    else
        to -expon do
            x/:=10.0
        od
    fi

    nextlx.xvalue:=x

    nextlx.symbol:=realconstsym
    nextlx.subcode:=treal
end

proc readrawxname=
    int c,hsum,length

    nextlx.svalue:=lxsptr
    hsum:=0

    doswitch c:=lxsptr++^
    when 'A'..'Z','a'..'z','0'..'9','_','$' then
        hsum:=hsum<<4-hsum+c
    else
        --lxsptr
        exit
    end doswitch

    lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

    return
end
=== qq_lib.m 0 0 13/43 ===
int currlineno
global int nextavindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const maxlocalunits=500
array [maxlocalunits]unitrec unitpool
int nlocalunits

ichar errormess

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

proc start=
end

global proc reportcterror(ichar errortype,mess,int pos, symbol currproc=nil)=
    geterrorinfo(pos,currproc)
    println errortype,"Error:"
    println "    ",,mess
    println

    showerrorsource()
    stopcompiler(errormodule,errorlineno)
end

global proc geterrorinfo(word pos, symbol currproc=nil)=

    ichar stoken, sline, smodule, slineend, s
    int length, column, lineno, offset, soffset, moduleno

    soffset:=pos.[0..23]
    moduleno:=pos.[24..31]



    if soffset=0 and moduleno=0 then
DEFAULT::
        strcpy(errorline,"<no data>")
        strcpy(errorpointer,"???")
        errormodule:=&moduletable[1]
        errorlineno:=1
        return
    fi

    if moduleno=0 then          
        errormodule:=&moduletable[0]

    elsif currproc=nil then

        println "Geterrorinfo: can't find module"
GOTO DEFAULT
        stop 1
    elsif currproc.nameid=procid then
        errormodule:=&moduletable[currproc.owner.moduleno]
        sterrorproc:=currproc
    elsif moduleno then
        errormodule:=&moduletable[moduleno]
    else
        errormodule:=&moduletable[currproc.moduleno]
        sterrorproc:=nil
    fi

    smodule:=sourcefiletext[errormodule.fileno]
    stoken:=smodule+soffset


    sline:=slineend:=stoken
    while sline>smodule and sline^<>10 do --sline od

    if sline^=10 then ++sline fi

    s:=sline
    errorlineno:=1
    while s>smodule do
        if s^=10 then ++errorlineno fi
        --s
    od

    while slineend^ not in [13,10,26,0] do ++slineend od
    length:=slineend-sline
LENGTH MAX:=0
    column:=stoken-sline+1



    length min:=errorline.len-1


    memcpy(&errorline, sline, length)
    errorline[length+1]:=0

    for i to column-1 do
        if errorline[i] not in [9,' '] then
            errorpointer[i]:=' '
        else
            errorpointer[i]:=errorline[i]
        fi
    od


    errorpointer[column]:='^'
    errorpointer[column+1]:=0
end

global function getlineno(ichar source, int offset)int=
    int lineno
    ichar sline, s

    sline:=source+offset

    while sline>source and sline^<>10 do --sline od
    if sline^=10 then ++sline fi

    s:=sline
    lineno:=1
    while s>source do
        if s^=10 then ++lineno fi
        --s
    od

    return lineno
end

proc showerrorsource=

    println "Line:",errorlineno,"in Module",errormodule.name,,".q:"

    if sterrorproc then
        println "In function:",sterrorproc.name
    fi

    println " |",errorline
    println " |",errorpointer
end

global proc stopcompiler(ref modulerec m,int lineno)=
    filehandle f
    f:=fopen("$error.tmp","w")
    println @f,sourcefilespecs[m.fileno],lineno
    fclose(f)
    println
    println

    stop 1
end


global proc gerror(ichar mess,unit p=nil)=
    reportcterror("Code Gen",mess,(p|p.pos|qpos),stcurrproc)
end

global proc gmerror(ichar mess,unit p=nil)=
    reportcterror("MPL Code Gen",mess,(p|p.pos|qpos),stcurrproc)
end

global proc gerror_s(ichar mess, param,unit p=nil)=
    array [300]char str
    print @str, mess, param
    reportcterror("Code Gen",&.str,(p|p.pos|qpos),stcurrproc)
end

global proc gmerror_s(ichar mess, param,unit p=nil)=
    array [300]char str
    print @str, mess, param
    reportcterror("MPL Code Gen",&.str,(p|p.pos|qpos),stcurrproc)
end

global proc serror(ichar mess)=
    reportcterror("Syntax",mess,lx.pos,stcurrproc)
end

global proc serror_s(ichar mess,param)=
    array [300]char str
    strcpy(&.str,mess)
    strcat(&.str," ")
    strcat(&.str,param)
    reportcterror("Syntax",str,lx.pos,stcurrproc)
end

global proc rxerror(ichar mess,unit p=nil)=
    reportcterror("Resolve",mess,(p|p.pos|qpos),stcurrproc)
end

global proc rxerror_s(ichar mess,param, unit p=nil)=
    array [300]char str
    strcpy(&.str,mess)
    strcat(&.str," ")
    strcat(&.str,param)
    rxerror(str,p)
end

global proc lxerror(ichar mess)=
    reportcterror("Lex",mess,lx.pos,stcurrproc)
end

global proc pcerror(ichar mess)=
    errormess:=mess
    getpcerrorpos(pcptr)
    reportpcerror(mess,pcerrorpos,pcerrormodule.def)
end

global proc pcerror_s(ichar mess, param)=
    array [300]char str
    errormess:=mess
    getpcerrorpos(pcptr)
    strcpy(&.str,mess)
    strcat(&.str," ")
    strcat(&.str,param)
    reportpcerror(str,pcerrorpos,pcerrormodule.def)
end

global proc reportpcerror(ichar mess,int pos, symbol currproc=nil)=
    variant s,send
    ref int pc
    int count,elineno
    ref modulerec emodule

    geterrorinfo(pos,currproc)
    emodule:=errormodule        
    elineno:=errorlineno

    println
    println " ":"80p*"
    println "PC Error:"
    println "    ",,mess
    println

    showerrorsource()

    s:=sptr
    send:=&varstack[1]

    count:=0
    while s>=send and count<5 do
        if s.tag=tretaddr then
            pc:=s.retaddr-3     
            getpcerrorpos(pc)
            geterrorinfo(pcerrorpos, pcerrormodule.def)
            println "Called from line",errorlineno,"in",errormodule.name
            ++count
        fi
        --s
    od

    stopcompiler(emodule,elineno)
end

proc getpcerrorpos(ref int pc)=
    int offset
    ref int pcstart
    ref int32 pcsrcstart

    pcerrormodule:=&moduletable[findmodulefrompc(pc)]

    pcstart:=pcerrormodule.pcstart
    pcsrcstart:=pcerrormodule.pcsrcstart

    offset:=pc-pcstart
    pcerrorpos:=(pcsrcstart+offset)^
end

global proc loaderror(ichar mess,mess2="",mess3="")=
    array [512]char str
    if strchr(mess,'#') then
        fprint @str,mess,mess2,mess3
    else
        print @str,mess
    fi

    println "Load Error:",str
    println "Stopping"
    stop 1
end

function findmodulefrompc(ref int pc)int=
    for i to nmodules do
        if pc>=moduletable[i].pcstart and pc<moduletable[i].pcend then
            return i
        fi
    od
    println "Can't find pcptr module",pc
    if errormess then
        fprintln "(#)",errormess
    fi
    stop 1
    return 0
end

global proc prterror(ichar mess)=
    println "Print error:",mess
    os_getch()
    stop 1
end

global proc pcustype(ichar mess, variant x) =
    pcustype_t(mess, x.tag)
end

global proc pcustype_t(ichar mess, int t) =
    array [256]char str

    fprint @str,"Type not supported: # : #",mess, ttname[t]

    getpcerrorpos(pcptr)
    reportpcerror(str,pcerrorpos,pcerrormodule.def)
end

global proc pcmxtypes(ichar mess, variant x,y) =
    array [256]char str

    pcmxtypestt(mess,x.tag,y.tag)
end

global proc pcmxtypestt(ichar mess, int t,u) =
    array [256]char str

    fprint @str, "Types not supported: # : #/#",
            mess,ttname[t],ttname[u]
    getpcerrorpos(pcptr)
    reportpcerror(str,pcerrorpos,pcerrormodule.def)
end

global function allocunitrec:unit p=
    p:=pcm_alloc32()
    p.word1:=p.nextunit:=p.a:=p.b:=nil
    p.nextunit:=p.a:=p.b:=nil
    p.pos:=lx.pos
    return p
end

global function createintunit(int64 a)unit=
    unit u
    u:=allocunitrec()
    u^.tag:=jintconst
    u^.value:=a
    return u
end

global function createboolunit(int64 a)unit=
    unit u
    u:=allocunitrec()
    u^.tag:=jboolconst
    u^.value:=a
    return u
end

global function createwordunit(int64 a)unit=
    unit u
    u:=allocunitrec()
    u^.tag:=jwordconst
    u^.value:=a
    return u
end

global function createrealunit(real64 x)unit=
    unit u
    u:=allocunitrec()
    u^.tag:=jrealconst
    u^.xvalue:=x
    return u
end

global function createstringunit(ichar s, int slength=-1)unit=
    unit u
    if slength=-1 then
        slength:=strlen(s)
    fi

    u:=allocunitrec()
    u^.tag:=jstringconst
    u^.svalue:=pcm_alloc(slength+1)
    if slength then
        memcpy(u^.svalue,s,slength)
    fi
    (u^.svalue+slength)^:=0
    u^.slength:=slength
    return u
end

global function createunit0(int tag)unit=
    unit u
    u:=allocunitrec()
    u.tag:=tag
    return u
end

global function createunit1(int tag, unit p)unit=
    unit u
    u:=allocunitrec()
    u^.tag:=tag
    u^.a:=p
    return u
end

global function createunit2(int tag, unit p,q)unit=
    unit u

    u:=allocunitrec()

    u.tag:=tag
    u.a:=p
    u.b:=q

    return u
end

global function createname(ref strec p)ref unitrec=
    ref unitrec u

    u:=allocunitrec()
    u.tag:=jname
    u.def:=p

    return u
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=

while p do

    if ulist=nil then       
        ulist:=ulistx:=p
    else
        ulistx^.nextunit:=p
    fi
    ulistx:=p           

    p:=p.nextunit
od
end

global function getrangelwbunit(unit p)unit=
    if p.tag=jmakerange then
        return p.a
    else
        return createunit1(jlwb,p)
    fi
end

global function getrangeupbunit(unit p)unit=
    if p.tag=jmakerange then
        return p.b
    else
        return createunit1(jupb,p)
    fi
end

global function createavname:unit=
    symbol p
    array [32]char str
    ichar name

    sprintf(&.str,"av$%d",++nextavindex)

    name:=pcm_copyheapstring(&.str)
    p:=addnamestr(name)

    return createname(p)
end

global function getzstring(ichar s, int length)ichar t=
    if length=0 then
        return ""
    fi
    t:=pcm_alloc(length+1)
    memcpy(t,s,length)
    (t+length)^:=0
    return t
end

global function convCstring(ref char svalue,int length)ref char =

    const strbufflen=2000
    static array [0:strbufflen]char strbuffer1
    static array [0:strbufflen]char strbuffer2
    static array [0:strbufflen]char strbuffer3
    static int strindex=0       
    static array [0:]ref [0:]char table=(
        &strbuffer1,&strbuffer2,&strbuffer3)
    ref[0:]char p

    if length>=strbufflen then
        pcerror("ConvCstring>=2000")
    fi

    if svalue=nil then
        return ""
    fi

    if ++strindex=table.len then
        strindex:=0
    fi
    p:=table[strindex]
    memcpy(p,svalue,length)
    p^[length]:=0
    return cast(p)
end

global function findprocname(ref proc fnptr)ichar=
    ichar name
    int n:=$get_nprocs()

    for i to n do
        if $get_procaddr(i)=fnptr then
            return $get_procname(i)
        fi
    od

    return "?"
end

global function convtostringz(ref char svalue,int length)ref char =

    const strbufflen=2000
    static array [0:strbufflen]char strbuffer1
    static array [0:strbufflen]char strbuffer2
    static array [0:strbufflen]char strbuffer3
    static array [0:strbufflen]char strbuffer4
    static array [0:strbufflen]char strbuffer5
    static array [0:strbufflen]char strbuffer6
    static int strindex=0       
    static array [0:]ref [0:]char table=(
        &strbuffer1,&strbuffer2,&strbuffer3,
        &strbuffer4,&strbuffer5,&strbuffer6)
    ref[0:]char p

    if length>=strbufflen then
        pcerror("Convtostringz>=2000")
    fi

    if svalue=nil then
        return ""
    fi

    if ++strindex=table.len then
        strindex:=0
    fi
    p:=table[strindex]
    memcpy(p,svalue,length)
    p^[length]:=0
    return cast(p)
end

global function strexpr(unit p)ref strbuffer=
    gs_init(exprstr)
    jeval(p)
    return exprstr
end

global function strexpr_s(unit p)ichar=
    if p=nil then return "" fi
    gs_init(exprstr)
    jeval(p)
    return exprstr.strptr
end

proc jeval(unit p)=
    unit q
    array [500]char str


    case p.tag
    when jintconst then
        sprintf(&.str,"%lld",p.value)
        additem(&.str)

    when jrealconst then
        sprintf(&.str,"%f",p.value)
        additem(&.str)

    when jstringconst then
        if p.slength>str.len/2 then
            strcpy(&.str,"LONGSTR)")
        else
            convertstring(p.svalue,&.str)
        fi
        additem("""")
        additem(&.str)
        additem("""")
    when jname then
        additem(p.def.name)

    when jadd, jsub, jmul, jdiv, jidiv, jidivrem, jiand, jior, jixor,
         jshl, jshr, jin, jnotin, jmin, jmax, jmakerange,
         jeq, jne, jlt, jle, jge, jgt, jirem, jpower,
         jappend then

        strcpy(&.str,getopcname(p.tag))
        additem("(")
        jeval(p.a)
        additem(&.str)
        jeval(p.b)
        additem(")")

    when jneg,jabs,jinot,jchr,jasc,jsqrt,jsqr,jsign,jsin,jcos,jtan,jasin,
        jacos,jatan,jln,jlg,jlog,jexp,jround,jfloor,jceil,jfract,jfmod, jlwb,jupb,jlen,jbounds,
        jbitwidth,jbytesize,jisvoid,jisdef,jisint,jisreal,jisstring, jdictitems,
        jisrange,jislist,jisrecord,jisset,jispointer,jminvalue,jmaxvalue,
        jnotl,jistruel, jismutable, jtype, jbasetype, jusertype, jelemtype,
        jismutable, jisnumber, jodd, jeven then

        strcpy(&.str,getopcname(p.tag))
        additem(&.str)
        additem("(")
        jeval(p.a)
        additem(")")

    when jcall then
        jeval(p.a)
        additem("(")

        q:=p.b
        while q do
            jeval(q)
            q:=q.nextunit
            if q then additem(",") fi
        od
        additem(")")

    when jcallhost then
        additem("Host<")
        additem(hostfnnames[p.index]+5)
        additem(">(")

        q:=p.a
        while q do
            jeval(q)
            q:=q.nextunit
            if q then additem(",") fi
        od
        additem(")")

    when jindex,jdotindex then
        jeval(p.a)
        if p.tag=jdotindex then
            additem(".")
        fi
        additem("[")
        jeval(p.b)
        additem("]")

    when jkeyindex then
        jeval(p.a)
        additem("{")
        jeval(p.b)
        additem("}")

    when jdot then
        jeval(p.a)
        additem(".")
        jeval(p.b)

    when jmakelist then
        additem("(")
        q:=p.a
        while q do
            jeval(q)
            q:=q.nextunit
            if q then additem(",") fi
        od
        additem(")")

    when jmakeset,jmakedict then
        additem("[")
        q:=p.a
        while q do
            jeval(q)
            q:=q.nextunit
            if q then additem(",") fi
        od
        additem("]")

    when jassign then
        jeval(p.a)
        additem(":=")
        jeval(p.b)

    when jtypeconst then
        additem(strmode(p.mode))

    when jconvert then

        additem(strmode(p.mode))
        additem("(")
        jeval(p.a)
        additem(")")
    when jkeyvalue then
        jeval(p.a)
        additem(":")
        jeval(p.b)

    when jaddrof then
        additem("&")
        jeval(p.a)

    when jdaddrof then
        additem("&&")
        jeval(p.a)

    when jptr then
        jeval(p.a)
        additem("^")

    when jptrto then
        additem("^")
        jeval(p.a)

    when jnil then
        additem("nil")

    when jsymbol then
        jeval(p.a)
        additem(".$")

    when jcmpchain then
        additem("CMPCHAIN:")
        q:=p.a
        jeval(q)

        for i to 4 do
            q:=q.nextunit
            if p.cmpgenop[i]=0 then exit fi
            additem(jtagnames[p.cmpgenop[i]])
            jeval(q)

        od

    when jmap then
        additem("map(")
        jeval(p.a)
        q:=p.b
        additem(",")
        jeval(q)
        if q:=q.nextunit then
            additem(",")
            jeval(q)
        fi

        additem(")")

    else
        CPL jtagnames[p.tag]
        loaderror("CAN'T DO JEVAL:",jtagnames[p.tag])
    end
end

global proc additem(ichar s)=
    ichar d
    int lastchar,nextchar

    d:=exprstr.strptr

    if exprstr.length then
        lastchar:=(d+exprstr.length-1)^
        nextchar:=s^
        if isalphanum(lastchar) and isalphanum(nextchar) then
            strbuffer_add(exprstr," ")
        fi
    fi
    strbuffer_add(exprstr,s)
end

function isalphanum(int c)int=
    if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
        return 1
    fi
    return 0
end

global function getopcname(int opc)ichar=
    ichar s

    s:=jshortnames[opc]
    if s=nil then
        s:=jtagnames[opc]+1
    fi
    return s
end

global proc convertstring(ichar s, t)=      
int c

    while c:=s++^ do
        switch c
        when '"' then
            t++^:='\\'
            t++^:='"'
        when 10 then
            t++^:='\\'
            t++^:='n'
        when 13 then
            t++^:='\\'
            t++^:='c'
        when 9 then
            t++^:='\\'
            t++^:='t'
        when '\\' then
            t++^:='\\'
            t++^:='\\'
        when 7,8,26,27 then
            t++^:='<'
            t++^:=c/10+'0'
            t++^:=(c rem 10)+'0'
            t++^:='>'
        else
            t++^:=c
        endswitch
    od
    t^:=0
end

global function extractstringz(variant p)ichar=
    object q:=p.objptr
    if p.tag<>tstring then pcerror("estrz?") fi
    if q.length then
        return pcm_copyheapstring(convtostringz(q.strptr,q.length))
    else
        return pcm_copyheapstring("")
    fi
end

global function createavnamex(symbol owner)unit p=

    symbol d
    p:=createavname()
    resolvename(owner,p)
    d:=p.def

    if d.nameid=frameid then
        ++nproclocals
        d.index:=nproclocals
        pproclocals^:=nproclocals
    fi                          

    return p
end

global proc storemode(symbol owner, int m, ref int16 p)=
    ref userxrec q
    p^:=m
    if m>=0 then return fi

    q:=pcm_alloc(userxrec.bytes)
    q.owner:=owner

    IF OWNER=NIL THEN
        SERROR("STOREMODE/OWNER=0")
    FI

    q^.pmode:=p
    q^.nextmode:=userxmodelist
    userxmodelist:=q
end

global function nextpoweroftwo(int x)int=

    if x=0 then return 0 fi

    int a:=1
    while a<x do
        a<<:=1
    od
    return a
end

global function raiseexception(int exceptno)ref int =
    variant stackend,oldsptr

    stackend:=&varstack[1]
    oldsptr:=sptr
    do
        if sptr<=stackend then
            sptr:=oldsptr
            default_exception(exceptno)
        fi
        if sptr.tag=texception and (exceptno=0 or sptr.exceptiontype=exceptno) then
            exit
        fi
        var_unshare(sptr)
        --sptr
    od

    frameptr:=ref byte(sptr)+sptr.frameoffset
    return cast(sptr.ptr)
end

global proc raise_error(int error_no)=

    (++sptr).tagx:=tint
    sptr.value:=error_no

    err_pcptr:=pcptr

    pcptr:=raiseseq
end

proc default_exception(int exceptno)=
    CPL "DEFAULT EXCEPTION HANDLER"

    case exceptno
    when pc_error then
        pcerror("PC/ERROR")
    when user_error then
        pcerror("USER/ERROR")
    when type_error then
        pcptr:=err_pcptr
PCERROR("PCUSTYPEDEF")

    when mixedtype_error then
        pcptr:=err_pcptr
PCERROR("PCMXTYPESDEF")

    when divide_error then
        pcptr:=err_pcptr

        pcerror("EXCEPTION/DIVIDE BY ZERO")

    when stopmodule_error then
    CPL "STOPMODULEERROR"
    when bounds_error then
    CPL "BOUNDSERROR"
    else
        cpl "Exception:",errornames[exceptno]
    esac


    stop 1
end

global function testelem(ref[0:]byte p,int n)int =
    return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =

    p^[n>>3] ior:= bytemasks[n iand 7]
end

global function strstack(variant p)ichar=
    return strint(int(p-&.varstack))
end


global function ispoweroftwo(int64 x)int=
    int64 a
    int n

    a:=1
    n:=0
    to 60 do
        ++n
        a:=a<<1
        if a=x then
            return n
        fi
    od
    return 0
end

global proc deleteunit(unit p,q)=
    unit r:=p.nextunit
    p^:=q^
    p.nextunit:=r
end

global function getenumname(int m, index)ichar=
    symbol e:=ttfields[m]

    while e, e:=e.nextdef do
        if e.nameid=enumid and e.mode=m and e.index=index then
            return e.name
        fi
    od
    return "?"
end
=== qq_lists.m 0 0 14/43 ===

global object emptylist


proc start=
    emptylist:=obj_new()
    emptylist.lower16:=1
    emptylist.objtype:=normal_obj
end

global proc var_empty_list(int lower, variant dest) =
    object p
    dest.objptr:=obj_newlist(0, lower)
    dest.tagx:=tlist ior hasrefmask
end

global proc var_make_list(variant a, dest, int n, lower) =
    object p
    variant b

    p:=obj_newlist(n, lower)

    b:=p.varptr


    if n and a then
        to n do
            b^:=a^              
            ++a
            ++b
        od
    fi                          

    dest.tagx:=tlist ior hasrefmask
    dest.objptr:=p
end

global function obj_newlist(int n, lower, variant defval=nil)object p=
    variant a

    p:=obj_new()
    p.mutable:=1
    if lower not in -32768..32767 then pcerror("List LWB not 16-bit") fi
    p.lower16:=lower
    p.length:=n
    p.objtype:=normal_obj

    if n then
        p.varptr:=a:=pcm_alloc(n*varrec.bytes)
        p.alloc64:=allocbytes/varrec.bytes

        if defval and defval.tag<>tvoid then
            to n do
                var_share(defval)
                a^:=defval^
                ++a
            od
        else
            to n do
                a.tagx:=tvoid
                ++a
            od
        fi
    fi

    return p
end

global proc obj_free_list(object p)=
    variant q
    varrec v

    q:=p.varptr
    to p.length do
        var_unshare(q)
        ++q
    od
    if p.length then
        pcm_free(p.varptr,p.alloc64*varrec.bytes)
    fi

    pcm_free32(p)
end

global proc var_getix_list(variant a, int index)=
    variant p
    object q
    word offset
    int lower

    q:=a.objptr

    lower:=q.lower16

    offset:=index-lower
    if offset>=word(q.length) then
        pcerror("getlist[int] bounds")
    fi

    a^:=(q.varptr+offset)^
    var_share(a)
end

global proc var_getslice_list(variant a, int i,j)=
    varrec v,v2
    int alower
    object p,q

    p:=a.objptr


    alower:=p.lower16

    if i<alower or j>p.length+alower-1 or i>j then
        pcerror("list/slice bounds")
    fi

    q:=obj_new()

    v.objptr:=q
    q.objtype:=slice_obj
    q.mutable:=p.mutable
    q.lower16:=1
    q.varptr:=p.varptr+i-alower

    case p.objtype
    when slice_obj then             
        q.objptr2:=p.objptr2        
        obj_shareu(q.objptr2)

    when extslice_obj then
        q.objptr2:=nil
        q.objtype:=extslice_obj
    else
        q.objptr2:=p                
        ++p.refcount
    esac

    q.length:=j-i+1
    a.objptr:=q
end

global proc var_getixref_list(variant a, int index)=
    variant p
    object q
    word offset
    varrec v

    q:=a.objptr

    offset:=index-q.lower16

    if offset>=word(q.length) then
        if int(offset)<0 then
            pcerror("&list[int] lwb")
        elsif offset=q.length then
            if q.objtype<>normal_obj then pcerror("Can't extend slice/ext") fi
            v.tagx:=tvoid
            obj_append_list(q,&v)
        else
            pcerror("putlist[int] bounds")
        fi
    fi

    p:=q.varptr+offset

    a.tagx:=trefvar
    a.varptr:=p
end

global proc var_putix_list(variant a, int index, variant x)=
    variant dest
    object q
    word offset
    int lower

    q:=a.objptr

    if not q.mutable then pcerror("List not mutable") fi

    offset:=index-q.lower16
    if offset>=word(q.length) then
        if int(offset)<0 then
            pcerror("putlist[int] lwb")
        elsif offset=q.length then
            if q.objtype<>normal_obj then pcerror("Can't extend slice/ext") fi
            obj_append_list(q,x)
            return
        else
            pcerror("putlist[int] bounds")
        fi
    fi

    dest:=q.varptr+offset
    var_unshare(dest)
    dest^:=x^               
end

global proc var_putslice_list(variant a, int i,j, variant x)=
    variant r,s
    object p,q
    int length,sublength

    p:=a.objptr
    if not p.mutable then pcerror("List not mutable") fi
    length:=p.length

    if i<1 or j>p.length or i>j then
        pcerror("list/slice bounds")
    fi
    sublength:=j-i+1

    q:=x.objptr
    if q.length<sublength then
        pcerror("substr too short")
    fi

    r:=p.varptr+i-1
    s:=q.varptr
    to sublength do
        r^:=s^
        var_share(r)
        ++r
        ++s
    od

end

proc obj_append_list(object a, variant x)=
    int n

    if a.objtype<>normal_obj then
        pcerror("Can't extend slice")
    fi

    if not a.mutable then
        pcerror("list/append not mutable")
    fi

    n:=a.length+1           

    if n>a.alloc64 then     
        obj_resize_list(a,n)
    else
        a.length:=n
    fi

    if x then
        (a.varptr+n-1)^:=x^     
    fi

end

global proc obj_resize_list(object p,int n)=
    variant q
    word32 allocated

    if n<=p.alloc64 then
        p.length:=n
    else
        q:=pcm_alloc(n*varrec.bytes)
        allocated:=allocbytes/varrec.bytes
        if p.length then
            memcpy(q,p.varptr,p.length*varsize)
            pcm_free(p.varptr, p.alloc64*varsize)
        fi
        p.varptr:=q
        p.length:=n
        p.alloc64:=allocated
    fi
end

global proc var_appendto_list(variant a, x)=
    obj_append_list(a.objptr,x)
end

global proc var_dupl_list(variant a)=
    object p,q
    variant plist, qlist

    p:=a.objptr
    q:=obj_new()
    q^:=p^
    q.refcount:=1
    q.mutable:=1
    q.objtype:=normal_obj

    a.objptr:=q

    if q.length=0 then return fi

    qlist:=q.varptr:=pcm_alloc(p.length*varrec.bytes)
    q.alloc64:=allocbytes/varrec.bytes  
    plist:=p.varptr

    to q.length do
        qlist^:=plist^
        if qlist.tag=trecord then
            var_share(qlist)
        else
            var_dupl(qlist)
        fi
        ++qlist
        ++plist
    od
end

global proc var_mul_list(variant p, int m)=
    int oldlength, newlength, n
    object q:=p.objptr, r
    variant a,b

    oldlength:=q.length
    newlength:=oldlength*m

    if oldlength=0 then return fi

    if newlength<0 then
        pcerror("list*int <0")
    elsif newlength=0 then
        p.objptr:=obj_newlist(0,q.lower16)
        return
    fi

    r:=obj_newlist(newlength, q.lower16)
    a:=r.varptr
    b:=q.varptr
    n:=0

    to newlength do
        a^:=b^
        var_share(a)
        ++a
        if oldlength>1 then
            ++b
            if ++n=oldlength then
                b:=q.varptr
                n:=0
            fi
        fi
    od

    p.objptr:=r
end

global function var_equal_list(variant x,y)int =
    int xlen,ylen,res
    object px,py
    variant a,b

    px:=x.objptr
    py:=y.objptr

    if px=py then return 1 fi           

    xlen:=px.length
    ylen:=py.length

    if xlen<>ylen then return 0 fi      

    if xlen=0 then return 1 fi          

    a:=px.varptr
    b:=py.varptr

    to xlen do
        if var_equal(a,b)=0 then return 0 fi    
        ++a
        ++b

    od

    return 1
end

global proc var_concatto_list(variant a,b)=
    variant newptr,c,d
    int n,alen,blen,newlen,oldbytes,newbytes
    variant v
    object pa,pb

    pa:=a.objptr

    if not pa.mutable then
        pcerror("concatlist/not mut")
    fi

    pb:=b.objptr

    alen:=pa.length
    blen:=pb.length

    if alen=0 then                  
        if blen then                
            obj_resize_list(pa,blen)
            d:=pa.varptr
            memcpy(d,pb.varptr,blen*varsize)
            to blen do
                var_share(d)
                ++d
            od
        fi
    elsif blen then                 
        newlen:=alen+blen
        obj_resize_list(pa,newlen)
        d:=pa.varptr+alen
        memcpy(d,pb.varptr,blen*varsize)
        to blen do
            var_share(d)
            ++d
        od
    fi
end

global function var_in_list(variant a,b)int =
    int n:=b.objptr.length
    int lowerm1:=b.objptr.lower16-1
    variant x:=b.objptr.varptr

    for i to n do
        if var_equal(a,x)=1 then
            return i+lowerm1
        fi
        ++x
    od
    return lowerm1
end

=== qq_newmodules.m 0 0 15/43 ===

ichar headerpathx   = ""
ichar altpathx      = ""
ichar importpathx   = ""
ichar subprogpath   = ""
int dirpos
int issyslib

array [headervarnames.len]ichar headervars

macro mainmodule=headervars[hv_mainmodule]

byte freadqa

global proc readprojectfile(ichar filename)=
    int fileno,headerdir, dir, oldsyslib
    ichar basefile


    if not checkfile(filename) then
        loaderror("Can't find main module or project: ##",filename)
    fi

    if eqstring(convlcstring(extractext(filename)),"qa") then
        filename:=loadqafile(filename)
    fi


    fileno:=getsupportfile(filename)

    basefile:=extractbasefile(sourcefilenames[fileno])
    moduletable[0].name:=basefile
    moduletable[0].fileno:=fileno

    initheadervars()

    headermode:=1
    headerdir:=0


    stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
    moduletable[0].stmodule:=stprogram
    addfirstsubprogram(basefile,fileno)

    startlex(fileno,0)

    do
        lex()
        skipsemi()

        case lx.symbol
        when kheadersym then
            headerdir:=1
            dir:=lx.subcode
            dirpos:=lx.pos
            lex()
            case dir
            when hdr_module then
                readmoduledir()
                mainmodule:=""
            when hdr_sysmodule then
                oldsyslib:=issyslib
                issyslib:=1
                readmoduledir()
                issyslib:=oldsyslib
                mainmodule:=""
            when hdr_subprog then
                altpathx:=""
                issyslib:=0
                readsubprogram()
            when hdr_syssubprog then
                if importpathx^=0 then
                    importpathx:=headervars[hv_devpath]
                fi
                issyslib:=1
                readsubprogram()
            when hdr_import then
                if lx.symbol=namesym and eqstring(lx.symptr.name,"qlib") then
                    goto $sysimport
                fi
                issyslib:=0
                altpathx:=""
                readimport()
            when hdr_sysimport then
$sysimport::
                if importpathx^=0 then
                    importpathx:=headervars[hv_devpath]
                fi
                issyslib:=1
                altpathx:=""
                readimport()

            when hdr_altpath then
                altpathx:=fixpath(readvar())
            when hdr_importpath then
                importpathx:=fixpath(readvar())
                subprogpath:=(importpathx^|importpathx|headerpathx)

            when hdr_setvar then
                dosetvar()

            when hdr_showvar then
                doshowvar()

            else
                loaderror("Hdr directive not ready:##",headerdirnames[dir])
            esac

            checksymbol(semisym)

        when semisym then
        when eofsym then
            exit

        else
            if sourcelevel then
                setmixedimport()
                unstacksource()
            else
                setmixedprogram(basefile)
                exit
            fi
        esac
    od


    if nmodules=0 then
        loaderror("No modules specified")
    fi

    addsyslib()

FINISH::
end

proc initheadervars=
    for i to headervars.len do
        headervars[i]:=""
    od

    headervars[hv_devpath]:="c:/qx/"
    headervars[hv_mmpath]:=""
    subprogpath:=headerpathx:=headervars[hv_hdrpath]:=pcm_copyheapstring(sourcefilepaths[1])
    headervars[hv_windows]:="1"
    mainmodule:="1"

end

global proc showprojectinfo(filehandle dev)=
    ref modulerec pm
    ref subprogrec ps
    static ichar tab="    "

    println @dev,"Project Structure:"
    println @dev,"---------------------------------------"
    println @dev,"Modules",nmodules
    for i to nmodules do
        pm:=&moduletable[i]

        if i>1 and pm.subprogno<>moduletable[i-1].subprogno then
            println @dev
        fi

        print @dev, tab,i:"2",pm.name:"16jl", "Sys:",pm.issyslib, "Path:",pm.path,
            "Sub:",subprogtable[pm.subprogno].name,"Fileno:",pm.fileno
        if pm.stmacro then
            print @dev," Alias:",pm.stmacro.name
        fi
        print @dev, "START:",pm.startfn
        if i=mainmoduleno then print @dev, "<MAIN>" fi
        println @dev
    od
    println @dev

    println @dev,"Subprograms",nsubprogs
    for i to nsubprogs do
        ps:=&subprogtable[i]
        println @dev, tab,i,ps.name,"Sys:",ps.issyslib, "Path:",ps.path, "Fileno:",ps.fileno
        if ps.startmodule then
            print @dev, tab,tab
            for j:=ps.startmodule to ps.endmodule do
                print @dev, moduletable[j].name,$
            od
            println @dev
        fi
    od
    println @dev

    println @dev,"Sourcefiles",nsourcefiles
    for i to nsourcefiles do
        println @dev, tab,i,sourcefilenames[i]
        if sourcefilepaths[i]^ then println @dev, tab,tab,sourcefilepaths[i] fi
        println @dev, tab,tab,sourcefilespecs[i]
        println @dev, tab,tab,=sourcefilesizes[i]
        println @dev, tab,tab,=sourcefilesys[i]
        println @dev, tab,tab,=sourcefilesupport[i]
    od
    println @dev


    return unless stprogram
    println @dev,"Symboltable:"
    symbol d:=stprogram.deflist
    while d, d:=d.nextdef do
        ichar id
        case d.nameid
        when moduleid then id:="Mod"
        when subprogid then id:="Sub"
        else id:="---"
        esac
        fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
    od


end

proc readmoduledir=
    ichar modulename, modulefilespec
    symbol stalias

    checksymbol(namesym)
    modulename:=modulefilespec:=pcm_copyheapstring(lx.symptr.name)
    convlcstring(modulename)
    stalias:=nil

    lex()
    if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
        lex()
        if lx.symbol=namesym then
            stalias:=lx.symptr
            lex()
        else
            stalias:=addnamestr(readvar())
        fi
    fi

    if checkwhen() then
        addmodule(modulename,stalias)
    fi
end

function checkwhen:int=
    int index

    return 1 when lx.symbol<>kwhensym

    lex()
    checksymbol(kheadervarsym)
    index:=lx.subcode
    lex()

    return eqstring(headervars[index],"1")
end

proc addmodule(ichar modulename, symbol stalias=nil)=
    ref modulerec pm
    ref subprogrec ps

    for i to nmodules do
        if eqstring(moduletable[i].name, modulename) then
            loaderror("Duplicate module name: # (Line:#)",modulename)
        fi
    od

    for i to nsubprogs do
        if eqstring(subprogtable[i].name, modulename) then
            loaderror("Clashing subprog/module name: # (Line:#)",modulename)
        fi
    od

    if nmodules>=maxmodule then
        loaderror("Too many modules",modulename)
    fi
    pm:=&moduletable[++nmodules]
    pm.moduleno:=nmodules

    pm.name:=pcm_copyheapstring(modulename)
    pm.subprogno:=nsubprogs

    pm.stmodule:=stmodule:=createnewmoduledef(stprogram,addnamestr(modulename))

    pm.path:=(altpathx^|altpathx|subprogpath)
    pm.issyslib:=issyslib

    stmodule.moduleno:=nmodules
    moduletosub[nmodules]:=nsubprogs

    ps:=&subprogtable[nsubprogs]

    if ps.startmodule=0 then
        ps.startmodule:=ps.endmodule:=nmodules
    else
        ps.endmodule:=nmodules
    fi

    if stalias then
LOADERROR("MODULE/AS NOT READY")
    fi
end

proc addsubprogram(ichar subprogname,int fileno)=
    ref subprogrec ps

    if nsubprogs>=maxsubprog then
        loaderror("Too many subprograms",subprogname)
    fi

    for i to nsubprogs do
        if eqstring(subprogtable[i].name, subprogname) then
            loaderror("Duplicate subprog name: # (Line:#)",subprogname)
        fi
    od
    ps:=&subprogtable[++nsubprogs]

    ps.name:=pcm_copyheapstring(subprogname)

    subprogpath:=ps.path:=(importpathx^|importpathx|subprogpath)

    stsubprog:=createnewmoduledef(stprogram,addnamestr(subprogname),subprogid)
    stsubprog.subprogno:=nsubprogs
    ps.stsubprog:=stsubprog
    ps.fileno:=fileno
    ps.issyslib:=issyslib
end

proc addfirstsubprogram(ichar progname, int fileno)=
    ref subprogrec ps

    nsubprogs:=1
    ps:=&subprogtable[1]
    ps.name:=pcm_copyheapstring(progname)
    ps.path:=headerpathx

    stsubprog:=createnewmoduledef(stprogram,addnamestr(progname),subprogid)
    stsubprog.subprogno:=1
    ps.stsubprog:=stsubprog
    ps.fileno:=fileno

    mainmoduleno:=1
end

proc readsubprogram=
    ichar subprogname, subprogfilespec

    checksymbol(namesym)
    subprogname:=subprogfilespec:=pcm_copyheapstring(lx.symptr.name)
    convlcstring(subprogname)

    lex()

    if lx.symbol=kwhensym then
        lex()
        lex()
    fi

    addsubprogram(subprogname,0)

end

proc readimport=
    ichar subprogname, path
    int fileno

    checksymbol(namesym)

    subprogname:=pcm_copyheapstring(lx.symptr.name)
    convlcstring(subprogname)

    lex()

    path:=(importpathx^|importpathx|subprogpath)

    fileno:=getsupportfile(subprogname,"q", path)
    addsubprogram(subprogname,fileno)

    stacksource(fileno)
end

function readvar:ichar s=
    case lx.symbol
    when stringconstsym then
        s:=pcm_copyheapstring(lx.svalue)
    when kheadervarsym then
        s:=headervars[lx.subcode]
    else
        loaderror("readvar/bad expr")
        s:="?"
    esac
    lex()
    return s
end

function fixpath(ichar path)ichar=
    array [300]char newpath
    int n:=strlen(path)
    if n=0 then return path fi
    if (path+n-1)^ in ['\\','/'] then
        return path
    fi
    strcpy(newpath,path)
    strcat(newpath,"\\")
    return pcm_copyheapstring(newpath)
end

proc dosetvar=
    int index

    checksymbol(kheadervarsym)
    index:=lx.subcode
    lex()
    checksymbol(eqsym)
    lex()
    headervars[index]:=readvar()
end

proc doshowvar=
    if lx.symbol=stringconstsym then
        println lx.svalue
    else
        checksymbol(kheadervarsym)
        println headervarnames[lx.subcode]+3,"=",headervars[lx.subcode]
    fi
    lex()
end

proc setmixedprogram(ichar basefile)=
    array [100]char name
    int oldns

    print @name,"$",,basefile
    oldns:=nsubprogs
    nsubprogs:=1
    addmodule(name)
    nsubprogs:=oldns

    moduletable[nmodules].fileno:=1
    mainmoduleno:=nmodules
end

proc setmixedimport=
    array [100]char name
    print @name,"$",,subprogtable[nsubprogs].name
    addmodule(name)
    moduletable[nmodules].fileno:=subprogtable[nsubprogs].fileno
end

global proc loadmodules =
    ref modulerec pm

    for i to nmodules do
        pm:=&moduletable[i]
        loadmodule(pm)
    od
end

proc loadmodule(ref modulerec pm)=
    array [300]char filespec

    if pm.fileno then
        return
    fi

    pm.fileno:=getsupportfile(pm.name, "q", pm.path, issyslib:pm.issyslib)
end

proc addsyslib=

    if fnosys then return fi

    for i to nsubprogs do
        if eqstring(subprogtable[i].name,"qlib") then return fi
    od

    issyslib:=1
    importpathx:=headervars[hv_devpath]
    altpathx:=""

    addsubprogram("qlib",0)
    for name in syslibnames do
        addmodule(extractbasefile(name))
    od
end

global function getsupportfile(ichar filename, ext="", path="",
    int issyslib=0, issupport=0)int =

    array [300]char filespec,filespec2
    ichar file
    int fileno

    file:=filename

    if ext^ then
        strcpy(filespec,addext(filename,ext))
        file:=&.filespec
    fi

    if freadqa then
        return loadbundledfile(file,issyslib,issupport)
    fi

    if issyslib and usesyslibs then

        fileno:=findsyslib(file)
        return fileno when fileno
    fi

    if not isabspath(file) then
        strcpy(filespec2,path)
        strcat(filespec2,file)
        file:=&.filespec2
    fi

    if file=nil or not checkfile(file) then
        loaderror("Can't find file: ##",file)
    fi

    fileno:=loadsourcefile(file)
    sourcefilesys[fileno]:=issyslib
    sourcefilesupport[fileno]:=issupport

    return fileno
end

function isabspath(ichar filespec)int=
    ichar path:=extractpath(filespec)
    if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then   
        return 1
    fi
    return 0
end

global function loadsourcefile(ichar filespec)int=
    ichar s,basefilename

    if nsourcefiles>maxsourcefile then
        loaderror("Too many source files")
    fi

    basefilename:=extractfile(filespec)

    ++nsourcefiles
    sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(filespec)
    sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(extractpath(filespec))
    sourcefilenames[nsourcefiles]:=pcm_copyheapstring(basefilename)

    s:=cast(readfile(filespec))         
    if not s then               
        loaderror("LSF can't load ",filespec)
    fi
    sourcefiletext[nsourcefiles]:=s

    sourcefilesizes[nsourcefiles]:=rfsize

    (s+rfsize)^:=0              
    return nsourcefiles
end

function readfileline(ichar s)ichar =
    array [2048]char str
    ichar t:=str
    int n, c

    n:=0
    docase c:=s++^
    when 0 then
        --s
        exit
    when 10 then
        exit
    else
        if n<str.len then
            t++^:=c
        fi
    end docase

    t^:=0

    readln @&.str
    return s
end

function findnextlineheader(ichar s)ichar=

    int c

    docase c:=s++^
    when 0 then
        return nil
    when 10 then
        if s^='=' and (s+1)^='=' and (s+2)^='=' then
            return s+3
        fi
    end docase

    return nil
end

function loadbundledfile(ichar filespec,int issyslib=0,support=0)int fileno=
    ichar file

    file:=extractfile(filespec)

    for i to nsourcefiles do
        if eqstring(file,sourcefilenames[i]) and support=sourcefilesupport[i] then      
            return i
        fi
    od

    fileno:=findsyslib(file)
    if fileno then
        return fileno
    fi


    loaderror("Can't find bundled file: ##",filespec)
    return 0
end

function loadqafile(ichar filespec, ichar builtinstr=nil)ichar=
    ichar s,t
    array [100]char name
    array [300]char newfilespec
    int sys,support

    freadqa:=1

    if filespec then
        s:=cast(readfile(filespec))
        if s=nil then                           
            loaderror("Can't find QA file ##",filespec)
        fi
        strcpy(newfilespec,extractpath(filespec))
    else
        s:=builtinstr
        newfilespec[1]:=0
    fi


    s:=readfileline(s+3)
    readstr(name,'n')
    if not eqstring(name,"qa") then
        loaderror("QA: bad header")
    fi

    --s                 

    if nsourcefiles then
        loaderror("QA/table not empty")
    fi

    s:=findnextlineheader(s)

    do
        if s=nil then
            loaderror("Unexpected EOF in QA file")
            exit
        fi
        s:=readfileline(s)
        readstr(name,'n')
        read sys,support
        if eqstring(name,"end") then
            exit
        fi
        if nsourcefiles>=maxsourcefile then
            loaderror("Too many files in QA")
        fi

        t:=findnextlineheader(s)
        if t=nil then
            loaderror("QA error")
        fi

        ++nsourcefiles
        sourcefilenames[nsourcefiles]:=sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(name)
        sourcefilesizes[nsourcefiles]:=t-s-3
        sourcefiletext[nsourcefiles]:=s
        sourcefilepaths[nsourcefiles]:=""
        sourcefilespecs[nsourcefiles]:=""
        sourcefilesys[nsourcefiles]:=sys
        sourcefilesupport[nsourcefiles]:=support
        s:=t
    od
    for i to nsourcefiles do
        (sourcefiletext[i]+sourcefilesizes[i])^:=0  
    od


    strcat(newfilespec, sourcefilenames[1])
    return pcm_copyheapstring(newfilespec)
end

=== qq_names.m 0 0 16/43 ===
int sdsize, sdoffset
int sdaligned
int sdlevel
int sdmode
int sdnfields
int sdmaxalign
const int maxstructdepth=10
array [maxstructdepth]byte sdunion      
array [maxstructdepth]int sdmaxsize     


proc start=
end

global function addglobalname(ichar name)symbol=
    lexrec oldlx
    symbol d

    oldlx:=nextlx

    lookup(name,strlen(name),gethashvaluez(name))

    d:=nextlx.symptr
    nextlx:=oldlx
    return d
end

function newstrec:symbol=
    symbol p
    p:=pcm_alloc(strec.bytes)
    memset(p,0,strec.bytes)

    return p
end

global function addsymbol(symbol owner,d, int id, isglobal)symbol e=
    symbol f

    e:=newstrec()
    e.name:=d.name
    e.namelen:=d.namelen
    e.owner:=owner
    e.nameid:=id
    e.isframe:=id=frameid or id=paramid

    if currmodule then
        e.moduleno:=currmodule.moduleno
    fi

    e.firstdupl:=d
    e.isglobal:=isglobal

IF OWNER.NAMEID<>PROCID THEN
    e.nextdupl:=d.nextdupl
    d.nextdupl:=e


    if e.nextdupl and e.nextdupl.owner=owner then
        cpl e.name,"in",owner.name
        serror("AS:Duplicate name")
    fi
else
    f:=owner.deflist
    while f do
        if f.firstdupl=e.firstdupl then
            cpl e.name,"in",owner.name
            serror("AS2:Duplicate name")
        fi
        f:=f.nextdef
    od
fi

    if owner.deflist=nil then           
        owner.deflist:=e
    else
        owner.deflistx.nextdef:=e
    fi
    owner.deflistx:=e

    return e
end

global proc addproc(symbol d)=
    ref procrec p

    p:=pcm_allocz(procrec.bytes)
    p.def:=d

    if proclist=nil then
        proclist:=p
    else
        proclistx.nextproc:=p
    fi
    proclistx:=p
    ++nproclist
end


global function createstroot(ichar name)symbol d=
    d:=newstrec()
    d.name:=pcm_copyheapstring(name)
    d.namelen:=strlen(name)
    d.nameid:=programid

    return d
end

global function newusertypex(ref strec d,e=nil)int=
    int i

    if nuserxtypes>=maxuserxtype then
        serror("Too many external user types")
    fi
    ++nuserxtypes
    ttnamedefx[nuserxtypes]:=d
    ttnamedefx2[nuserxtypes]:=e

    ttxmoduleno[nuserxtypes]:=stcurrmodule.moduleno
    ttposx[nuserxtypes]:=lx.pos
    return -nuserxtypes
end

global function resolvedottedname(symbol owner, d)symbol e=

    e:=d.nextdupl
    while e and e.owner<>owner do
        e:=e.nextdupl
    od

    return e
end

global function strmode(int t, expand=0)ichar=
    static [2048]char str

    istrmode(t,&.str,expand)
    return str
end

proc istrmode(int t, ichar dest,int expand=1)=
    static [2048]char str
    symbol d

    if t<0 then
        strcpy(dest,"*")
        strcat(dest,ttnamedefx[-t].name)
        if ttnamedefx2[-t] then
            strcat(dest,".")
            strcat(dest,ttnamedefx2[-t].name)
        fi
        return
    fi

    if t<tlast then
        strcpy(dest,ttname[t])
        return
    fi

    case ttbasetype[t]
    when trefpack then
        strcpy(dest,"ref ")
        istrmode(tttarget[t], dest+strlen(dest),0)
    when tpackarray then
        fprint @dest, "[#..#]",ttlower[t],ttlength[t]+ttlower[t]-1
        istrmode(tttarget[t], dest+strlen(dest),0)

    when tslice then
        strcpy(dest, "slice[]")
        istrmode(tttarget[t], dest+strlen(dest),0)

    when tpackrecord then
        if not expand then goto $else fi
        strcpy(dest,"struct(")
dostruct::
        d:=ttfields[t]
        while d, d:=d.nextdef do
            istrmode(d.mode, dest+strlen(dest),0)
            strcat(dest, " ")
            strcat(dest, d.name)
            if d.nextdef then
                strcat(dest, ", ")
            fi
        od
        strcat(dest,")")
    when trecorddef then
        if not expand then goto $else fi
        strcpy(dest,"record(")
        goto dostruct


    else
$else::
        strcpy(dest,ttname[t])
    esac
end


global proc addgenfield(symbol d)=
    int index
    symbol dgen
    ref genfieldrec g


    dgen:=d.firstdupl
    index:=dgen.genfieldindex


    if index=0 then         
        if ngenfields>=maxgenfield then
            pcerror("Too many genfields")
        fi
        dgen.genfieldindex:=index:=++ngenfields
    fi

    g:=pcm_alloc(genfieldrec.bytes)
    g.def:=d
    g.nextdef:=genfieldtable[index]
    genfieldtable[index]:=g
end

global function addusertype(symbol d)int=

    if ntypes>=maxtype then pcerror("Too many types") fi

    ++ntypes
    d.mode:=ntypes
    ttnamedef[ntypes]:=d
    ttname[ntypes]:=d.name

    return ntypes

end

global function makereftype(int target, symbol owner=nil)int=

    int newtype

    if owner=nil then
        for i:=tlast+1 to ntypes do
            if ttbasetype[i]=trefpack and tttarget[i]=target then
                return i
            fi
        od
    fi

    newtype:=addanontype()
    ttbasetype[newtype]:=trefpack

    storemode(stcurrproc,target,&tttarget[newtype])

    ttsize[newtype]:=8
    ttbitwidth[newtype]:=64
    ttcat[newtype]:=refcat
    return newtype
end

global function makeaxtype(int target, unit plower, plength)int=
    int newtype,length

    newtype:=addanontype()

    ttbasetype[newtype]:=tpackarray
    storemode(stcurrproc, target, &tttarget[newtype])

    ttlower[newtype]:=1
    ttlengthexpr[newtype]:=plength
    ttlowerexpr[newtype]:=plower
    ttcat[newtype]:=blockcat            



    return newtype
end

global function makeslicetype(int target)int=
    int newtype,length

    newtype:=addanontype()



    ttbasetype[newtype]:=tslice
    storemode(stcurrproc, target, &tttarget[newtype])

    ttlower[newtype]:=1
    ttsize[newtype]:=16

CPL "SLICE",=STRMODE(TTBASETYPE[NEWTYPE])

    return newtype
end

global function makestrtype(int m, unit pwidth)int=
    int newtype

    newtype:=addanontype()
    ttbasetype[newtype]:=m
    ttlengthexpr[newtype]:=pwidth
    ttlower[newtype]:=1
    ttowner[newtype]:=stcurrproc
    return newtype
end

global function addanontype:int=
    array [32]char str

    if ntypes>=maxtype then pcerror("Too many types") fi

    ++ntypes
    print @str,"$T",,ntypes

    ttname[ntypes]:=pcm_copyheapstring(str)
    ttowner[ntypes]:=stcurrproc

    return ntypes

end

global proc createusertype(symbol d, int m)=
    storemode(stcurrproc,m,&d.mode)

    if m>tlast and ttnamedef[m]=nil then
        ttnamedef[m]:=d
        ttname[m]:=d.name
        ttowner[m]:=d.owner
        ttcat[m]:=gettypecat(m)
    fi
end

global function roundoffset(int offset, alignment)int=
    int mask

    if alignment=1 then return offset fi
    mask:=alignment-1
    while offset iand mask do ++offset od

    return offset
end

global function getalignment(int m)int=
    int a

    case ttbasetype[m]
    when tpackarray then
        return getalignment(tttarget[m])
    when tpackrecord then
    esac

    a:=ttsize[m]
    case a
    when 1,2,4,8 then
        return a
    esac
    cpl ttname[m],a
    pcerror("Getalign not 1248")

    return 0
end

global proc duplfield(ref strec p,q)=

    if p.code then
        serror("DUPLFIELD")
    fi
    q.atfield:=p.atfield
    q.index:=p.index
    q.fieldoffset:=p.fieldoffset
end

proc writesig(symbol d, filehandle dev)=
    symbol e
    int n
    fprint @dev, "# #(", (d.misfunc|"function"|"proc"), d.name

    e:=d.deflist
    n:=0
    while e, e:=e.nextdef do
        if e.nameid=paramid then
            ++n
            if e.moptional and e.code then
                fprint @dev,"#=#", e.name, strexpr(e.code).strptr
            elsif e.moptional then
                print @dev,"?",,e.name
            else
                print @dev,e.name
            fi

            if n<d.nparams then
                print @dev,", "
            fi
        fi


    od

    fprintln @dev,")    array [#]", d.owner.name

end

global proc writedocs(symbol d, []ichar &docstrings, int ndocstrings)=
    if not docsdev then
        return
    fi

    if fwritedocs=1 and d.moduleno<>1 then return fi

    writesig(d, docsdev)
    for i to ndocstrings do
        fprintln @docsdev, "# #","#",docstrings[i]
    od
    println @docsdev

end

global function createdupldef(symbol owner,symptr, int id)symbol=
    symbol p,q

    p:=newstrec()

    p.name:=symptr.name
    p.namelen:=symptr.namelen
    p.symbolcode:=namesym
    p.owner:=owner
    p.nameid:=id

    p.nextdupl:=symptr.nextdupl
    symptr.nextdupl:=p

    if owner then
        if owner.deflist=nil then           
            owner.deflist:=owner.deflistx:=p
        else
            owner.deflistx.nextdef:=p
            owner.deflistx:=p
        fi
    fi

    return p
end

global function createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
    return createdupldef(owner,symptr,id)
end

=== qq_optim.m 0 0 17/43 ===
ref int pc, pcstart

macro getopnda = (pc+1)^
macro getopndb = (pc+2)^
macro getopndc = (pc+3)^
macro getopndd = (pc+4)^
macro getopnde = (pc+5)^

macro putopnda(x) = (pc+1)^:=x
macro putopndb(x) = (pc+2)^:=x
macro putopndc(x) = (pc+3)^:=x
macro putopndd(x) = (pc+4)^:=x
macro putopnde(x) = (pc+5)^:=x

ref[0:]int labelmap

global proc optimise_module(int n)=
    int cmd, nopnds, size:=0, x
    pc := pcstart := moduletable[n].pcstart


    size:=moduletable[n].pcsize+1
    labelmap:=pcm_allocz(size*int.bytes)

    repeat
        cmd:=pc^
        nopnds:=pclnopnds[cmd]

        for i to nopnds do
            case pclfmt[cmd,i]
            when cnone then
                exit
            when clabel then
                x:=(pc+i)^
                labelmap^[x]:=1     
            esac

        od
        pc+:=nopnds+1
    until cmd in [kzero,kendmodule]

    pc:=pcstart
    repeat
        cmd:=pc^
        optimise_op(cmd)
    until cmd=kendmodule

    if size then pcm_free(labelmap,size) fi
end

proc putnops(int offset,n)=
    to n do
        (pc+offset++)^:=kskip
    od
end

proc optimise_op(int cmd)=
    int skip, offset, secondlab, cmd2, skip2, x,y,z
    int a,b, thisloc, thislab, destcmd, destlab

    skip:=pclnopnds[cmd]+1          
    a:=getopnda
    b:=getopndb
    offset:=pc-pcstart              



    case cmd
    when kunshare then
        case a
        when 1,2,3 then
            pc^:=(a|kunshare1,kunshare2,kunshare3|0)
            putnops(1,1)
            skip:=2
            return
        esac

    when kprocentry then
        case a
        when 1,2 then
            pc^:=(a|kprocentry1,kprocentry2|0)
            putnops(1,1)
            skip:=2
            return
        esac
    when kjump,kjumpeq,kjumpne,kjumplt,kjumple,kjumpge,kjumpgt then
            thisloc:=offset
            thislab:=a
            if cmd=kjump and thisloc+2=thislab then
                pc^:=knop2
                putopnda(0)
                pc+:=skip
                return
            elsif destcmd=kjump then
                destcmd:=(pcstart+thislab)^
                destlab:=(pcstart+thislab+1)^
                putopnda(destlab)
                pc+:=skip
                return
            fi
    esac

    if labelmap[offset+skip] then   
        pc+:=skip                   
        return
    fi

    secondlab:=0
    cmd2:=(pc+skip)^
    skip2:=pclnopnds[cmd2]+1
    if labelmap[offset+skip+skip2] then
        secondlab:=1
    fi
    

    switch cmd
    when kpushf then
        switch b                
        when kpushf then            
            if secondlab then dopushff fi
            switch getopndd
            when kpushf then
                pc^:=kpushfff
                putopndb(getopndc)
                putopndc(getopnde)
                putnops(4,2)
                skip:=6

            when kadd then
                pc^:=kaddff
                putopndb(getopndc)
                putnops(3,1)        
                skip:=5
            when ksub then
                pc^:=ksubff
                putopndb(getopndc)
                putnops(3,1)
                skip:=5

            when kjumpeq then
                pc^:=kjumpeqff
                dojumpxxff
            when kjumpne then
                pc^:=kjumpneff
                dojumpxxff
            when kjumplt then
                pc^:=kjumpltff
dojumpxxff::
                x:=getopnda
                y:=getopndc
                z:=getopnde
                putopnda(z)
                putopndb(x)
                putopndc(y)

                skip:=6
            when kjumple then
                pc^:=kjumpleff
                dojumpxxff
            when kjumpge then
                pc^:=kjumpgeff
                dojumpxxff
            when kjumpgt then
                pc^:=kjumpgtff
                dojumpxxff
            when kindex then
                pc^:=kindexff
                putopndb(getopndc)
                putnops(3,1)        
                skip:=5

            else
dopushff::
                pc^:=kpushff
                putopndb(getopndc)
                putnops(3,1)
                skip:=4
            end
        when kpushm then            
                pc^:=kpushfm
                putopndb(getopndc)
                putnops(3,1)
                skip:=4
        when kpushci then           
            if secondlab then finish fi
            switch getopndd
            when kadd then
                pc^:=kaddfci
                putopndb(getopndc)
                putnops(3,1)        
                skip:=5
            when ksub then
                pc^:=ksubfci
                putopndb(getopndc)
                putnops(3,1)
                skip:=5

            when kjumplt then
                pc^:=kjumpltfci
dojumpxxfci::
                x:=getopnda
                y:=getopndc
                z:=getopnde
                putopnda(z)
                putopndb(x)
                putopndc(y)
                skip:=6
            when kjumple then
                pc^:=kjumplefci
                dojumpxxfci
            when kjumpge then
                pc^:=kjumpgefci
                dojumpxxfci
            when kjumpgt then
                pc^:=kjumpgtfci
                dojumpxxfci
            when kjumpeq then
                pc^:=kjumpeqfci
                dojumpxxfci
            when kjumpne then
                pc^:=kjumpnefci
                dojumpxxfci
            end
        when kpopm then             
            pc^:=kmovemf
            domoveff
        when kpopf then             
            pc^:=kmoveff
domoveff::
            x:=a
            putopnda(getopndc)
            putopndb(x)
            putnops(3,1)
            skip:=4
        when kzpopf then            
            pc^:=kzmoveff
            domoveff

        when kswitch then           
            pc^:=kswitchf
            putopndb(getopndc)
            putopndc(getopndd)
            putnops(4,1)
            skip:=5
    
        when klen then              
            pc^:=klenf
            putnops(2,1)
            skip:=3
        when kpushptr then          
            pc^:=kpushptrf
            putnops(2,1)
            skip:=3
        end
    when kpushm then
        case b
        when kpushm then            
            pc^:=kpushmm
            putopndb(getopndc)
            putnops(3,1)
            skip:=4
        when kpushf then            
            pc^:=kpushmf
            putopndb(getopndc)
            putnops(3,1)
            skip:=4
        when kpopm then             
            pc^:=kmovemm
            domoveff
        when kpopf then             
            pc^:=kmovefm
            domoveff
        esac
    when kpushci then
        case b
        when kpopm then             
            pc^:=kmovemci
            domoveff
        when kpopf then             
            pc^:=kmovefci
            domoveff
        when kzpopf then            
            pc^:=kzmovefci
            domoveff
        elsif a=0 and b not in [kraise,kstop] then
            pc^:=kpushci0
        esac

    when kpushvoid then
        case a
        when kpushvoid then         
            if not secondlab and b=kpushvoid then
                pc^:=kpushvoid3
                putnops(1,2)
                skip:=3
            else
                pc^:=kpushvoid2
                putnops(1,1)
                skip:=2
            fi
        esac
    when kpushfref then
        case b
        when kloadincr then
            if not secondlab then
                case getopndc
                when kpushptr then      
                    pc^:=kpushincrptrf
                    putnops(2,1)        
                    skip:=4
                when kpopptr then       
                    pc^:=kpopincrptrf
                    putnops(2,1)        
                    skip:=4
                esac
            fi
        esac

    when kpushmref then
        case b
        when kloadincr then
            if not secondlab then
                case getopndc
                when kpushptr then      
                    pc^:=kpushincrptrm
                    putnops(2,1)        
                    skip:=4
                when kpopptr then       
                    pc^:=kpopincrptrm
                    putnops(2,1)        
                    skip:=4
                esac
            fi
        esac

    end

finish::
    pc+:=skip
end

=== qq_packed.m 0 0 18/43 ===
global proc var_loadpacked(ref void p,int t,variant dest, object ownerobj=nil) =
    int length
    variant q,r
    ref int pp
    object s
    ref char ss


    switch ttbasetype[t]
    when ti8 then
        dest.tagx:=tint
        dest.value:=ref i8(p)^

    when ti16 then
        dest.tagx:=tint
        dest.value:=ref i16(p)^

    when ti32 then
        dest.tagx:=tint
        dest.value:=ref int32(p)^

    when ti64 then
        dest.tagx:=tint
        dest.value:=ref i64(p)^

    when tu8 then
        dest.tagx:=tint
        dest.value:=ref byte(p)^

    when tu16 then
        dest.tagx:=tint
        dest.value:=ref u16(p)^

    when tu32 then
        dest.tagx:=tint     
        dest.value:=ref u32(p)^

    when tu64 then
        dest.tagx:=tword        
        dest.value:=ref u32(p)^

    when tr64 then
        dest.tagx:=treal
        dest.xvalue:=ref r64(p)^

    when tr32 then
        dest.tagx:=treal
        dest.xvalue:=ref r32(p)^

    when tpackstrc then
        dest.tagx:=tstring ior hasrefmask
        length:=ttlength[t]
        if length>=2 then       
            length:=getfslength(p,length)
        else                
            length:=1
        fi
        s:=obj_make_strslicexobj(p,length)
        dest.objptr:=s

    when tpackstrz then     
        dest.tagx:=tstring ior hasrefmask
        ss:=p
        to ttlength[t] do
            exit when ss^=0
            ++ss
        od

        s:=obj_make_strslicexobj(p,ss-ref char(p))
        dest.objptr:=s

    elsecase ttbasetype[t]
    when trefpack then
        dest.tagx:=trefpack
        dest.ptr:=cast(ref i64(p)^)
        dest.elemtag:=tttarget[t]

    when tpackrecord then
        s:=obj_new()
        s.mutable:=1
        s.ptr:=p
        dest.objptr:=s
        dest.tagx:=tstruct ior hasrefmask
        s.usertag:=t
        if ownerobj then
            s.objtype:=slice_obj
            s.objptr2:=ownerobj
            ++ownerobj.refcount
        else
            s.objtype:=extslice_obj
        fi
    when tpackarray then
        s:=obj_newarray(tttarget[t],ttlower[t],ttlength[t])
        s.mutable:=1
        s.ptr:=p
        dest.objptr:=s
        dest.tagx:=tuserarray ior hasrefmask
        s.usertag:=t
        if ownerobj then
            s.objtype:=slice_obj
            s.objptr2:=ownerobj
            ++ownerobj.refcount
        else
            s.objtype:=extslice_obj
        fi
    else
        pcmxtypestt("loadpacked",ttbasetype[t],t)
    endswitch
end

global proc var_storepacked(ref byte p,variant q,int t) =

    int plength,qlength
    int s,sbase,tbase
    object qa


    s:=sbase:=q.tag     
    tbase:=ttbasetype[t]

    switch sbase
    when tint,tword, trefpack then
        switch tbase
        when ti8,tu8 then
            (ref byte(p)^):=q.value
            return
        when ti16,tu16 then
            (ref u16(p)^):=q.value
            return
        when ti32,tu32 then
            (ref int32(p)^):=q.value
            return
        when ti64,tu64,trefpack then
            (ref i64(p)^):=q.value
            return
        when tr32 then
            (ref r32(p)^):=q.value
            return
        when tr64 then
            (ref r64(p)^):=q.value
            return
        endswitch

    when treal then
        switch tbase
        when ti32,tu32 then
            (ref int32(p)^):=q.xvalue
            return
        when ti64,tu64 then
            (ref int64(p)^):=q.xvalue
            return
        when tr32 then
        (ref r32(p)^):=q.xvalue
            return
        when tr64 then
            (ref r64(p)^):=q.xvalue
            return
        when ti16,tu16 then
            (ref int16(p)^):=q.xvalue
            return
        endswitch

    when tstring then
        qa:=q.objptr
        plength:=ttlength[t]
        qlength:=qa.length
        switch tbase
        when tpackstrc then         
            if t=tbase then         
                if qlength<>1 then
                    pcerror("Str not len 1")
                fi
                (ref char(p)^):=ref char(qa.strptr)^
                return
            fi
            if qlength>plength then     
                qlength:=plength
            fi
            memcpy(p,qa.strptr,qlength)     
            setfslength(cast(p),plength,qlength)
            return

        when tpackstrz then
            if qlength>=plength then            
                memcpy(p,qa.strptr,plength)     
                (ref byte(p)+plength-1)^:=0         

            else
                memcpy(p,qa.strptr,qlength)     
                (ref byte(p)+qlength)^:=0           
            fi

            return

        endswitch

    when tstruct then
        s:=q.objptr.usertag
        if s<>t then
            pcmxtypestt("spack struct",s,t)
        fi
        memcpy(p,q.objptr.ptr,ttsize[t])
        return

    when tuserarray then
        s:=q.objptr.usertag
        if s<>t then                
                pcmxtypestt("spack array",s,t)
        fi
        memcpy(p,q.objptr.ptr,ttsize[t])
        return

    endswitch

    pcmxtypestt("storepacked (source->dest)",s,t)
end

proc setfslength(ref char s,int m,n) =      

    if m=n then     
    elsif n=m-1 then    
        (s+m-1)^:=0
    else            
        (s+m-2)^:=0     
        (s+m-1)^:=n     
    fi
end

global function getfslength(ref char s,int m)int =      
    s+:=m-1         

    if (s-1)^=0 then        
        return s^
    elsif s^=0 then     
        return m-1
    else                
        return m
    fi
end

global proc var_make_struct(variant a, dest, int n, rectype) =
    symbol d
    ref symbol r
    object p
    variant b
    int m
    ref byte q

    p:=obj_new_struct(rectype)

    b:=p.varptr

    m:=ttlength[rectype]
    d:=ttnamedef[rectype]
    r:=d.topfieldlist


    if n<m then
        pcerror("Too few elements")
    elsif n>m then
        println =n,=m
        pcerror("Too many elements")
    fi

    q:=p.ptr

    to n do
        var_storepacked(q, a, r.mode)
        q+:=ttsize[r.mode]
        ++r
        ++a
    od

    dest.tagx:=tstruct ior hasrefmask
    p.usertag:=rectype
    dest.objptr:=p
end

global function obj_new_struct(int m)object p=
    int size

    p:=obj_new()
    p.mutable:=1
    p.usertag:=m

    size:=ttsize[m]
    if size then
        p.ptr:=pcm_allocz(size)
    fi

    return p
end

global proc var_dupl_struct(variant a)=
    object p,q
    int size

    p:=a.objptr
    size:=ttsize[p.usertag]
    q:=obj_new_struct(p.usertag)
    a.objptr:=q

    memcpy(q.ptr, p.ptr, size)
end

global proc obj_free_struct(object p)=
    pcm_free(p.ptr, ttsize[p.usertag])
    pcm_free32(p)
end

global function var_equal_struct(variant x,y)int=

    return eqbytes(x.objptr.ptr, y.objptr.ptr, ttsize[x.tag])
end

global proc var_getix_struct(variant a, int index)=
    symbol d
    ref symbol r
    varrec v
    object p
    int elemtype

    v:=a^
    p:=a.objptr

    if index<1 or index>ttlength[a.tag] then
        pcerror("struct[int] bounds")
    fi

    d:=ttnamedef[p.usertag]
    r:=(d.topfieldlist+index-1)

    var_loadpacked(p.ptr+r.fieldoffset, r.mode, a)

end


=== qq_parse.m 0 0 19/43 ===
symbol stmodule

int intabledata
ichar tabledataname=nil

const maxdollarstack=10
array [maxdollarstack]unit dollarstack      
int ndollar=0


FUNC READUNIT:UNIT=
    READEXPRESSION()
END


const maxdocstring=50
array [maxdocstring]ichar docstrings
int ndocstrings
int currdllindex

const maxlisttype=20
array [maxlisttype]int listtypestack
int nlisttype
int listtype                

global proc parsemodule(int n)=
    unit p

    return when moduletable[n].parsed

    currmodule:=&moduletable[n]
    startlex(currmodule.fileno, n)



    stcurrmodule:=currmodule.def

    lex()
    lex()

    stcurrproc:=stcurrmodule


    p:=readsunit()

    stcurrmodule.code:=moduletable[n].ast:=p

    skipsemi()
    case lx.symbol
    when commasym then
        serror("Comma seq not allowed")
    when eofsym then
    else
        PS("EOF")
        serror("Bad symbol at eof")
    esac
end


function readexpression:unit p=
    p:=readterm2()

    if exprendset[lx.symbol] then return p fi

    if lx.symbol in [assignsym, deepcopysym] then
        return readassignment(p)
    else
        return readorterms(p)
    fi
end

function readassignment(unit p)unit=
    int pos,opc

    if exprendset[lx.symbol] then return p fi

    p:=readorterms(p)

    if lx.symbol in [assignsym,deepcopysym] then
        opc:=lx.subcode
        pos:=lx.pos
        lex()
        p:=createunit2(opc,p,readassignment(readterm2()))
        p.pos:=pos
    fi
    return p
end

function readorterms(unit p)unit =
    int pos

    if exprendset[lx.symbol] then return p fi

    p:=readandterms(p)

    while lx.symbol=orlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jorlto,p,readunit())
            p.pos:=pos
            exit
        fi

        p:=createunit2(jorl,p,readandterms(readterm2()))
        p.pos:=pos
    od

    return p
end

function readandterms(unit p)unit =
    int pos

    p:=readcmpterms(p)

    while lx.symbol=andlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jandlto,p,readunit())
            p.pos:=pos
            exit
        fi

        p:=createunit2(jandl,p,readcmpterms(readterm2()))
        p.pos:=pos
    od

    return p
end

function readcmpterms(unit p)unit =
    int pos,n
    unit px,q
    array [4]byte genops

    p:=readinterms(p)

    if not cmpopset[lx.symbol] then
        return p
    fi

    clear genops
    px:=p
    p:=createunit1(jcmpchain,p)
    n:=0                

    while cmpopset[lx.symbol] do
        ++n
        if n>genops.len then serror("cmpchain: Too many items") fi
        genops[n]:=lx.subcode
    
        pos:=lx.pos
        lex()

        q:=readinterms(readterm2())
        px.nextunit:=q
        px:=q

        q.pos:=pos
    od
    if n=1 then
        p.tag:=genops[1]
        q:=p.a
        p.b:=q.nextunit
        q.nextunit:=nil
    else
        p.cmpgenop:=genops
    fi  

    return p
end

function readinterms(unit p)unit =
    int pos,opc

    p:=readrangeterm(p)

    doswitch lx.symbol
    when insym, notinsym then
        opc:=lx.subcode

        pos:=lx.pos
        lex()

        p:=createunit2(opc,p,readrangeterm(readterm2()))
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readrangeterm(unit p)unit =
    int pos

    p:=readaddterms(p)

    if lx.symbol=rangesym then
        pos:=lx.pos
        lex()
        p:=createunit2(jmakerange,p,readaddterms(readterm2()))
        p.pos:=pos
    fi

    return p
end

function readaddterms(unit p)unit =
    int pos,opc,a,b
    unit q

    p:=readmulterms(p)
    while addopset[lx.symbol] do
        opc:=lx.subcode

        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jtocodes[opc],p,readassignment(readterm2()))
            p.pos:=pos
            exit
        fi

        q:=readmulterms(readterm2())
        p:=createunit2(opc,p,q)
        p.pos:=pos
    od

    return p
end

function readmulterms(unit p)unit =
    int pos,opc,a,b
    unit q

    p:=readpowerterms(p)

    while mulopset[lx.symbol] do
        opc:=lx.subcode
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jtocodes[opc],p,readassignment(readterm2()))
            p.pos:=pos
            exit
        fi

        p:=createunit2(opc,p,readpowerterms(readterm2()))
        p.pos:=pos
    od

    return p
end

function readpowerterms(unit p)unit =
    int pos

    while lx.symbol=powersym do
        pos:=lx.pos
        lex()
        p:=createunit2(jpower,p,readpowerterms(readterm2()))
        p.pos:=pos
    od

    return p
end

function readterm2:unit p=
    int pos

    pos:=lx.pos
    p:=readterm()
    p:=readtermsuffix(p,pos)
    return p
end

function readtermsuffix(unit p, int pos)unit=
    unit q,r
    ref char pbyte
    word64 a
    int opc,oldipl,shift,t,nparams

    doswitch lx.symbol
    when lbracksym then
        lex()
        q:=readslist(1,1,nparams)
        checksymbol(rbracksym)
        lex()
        p:=createunit2(jcall,p,q)
        p:=readcondsuffix(p)

    when ptrsym then
        p:=createunit1(jptr,p)
        lex()

    when lsqsym then
        p:=readindex(p,0)

    when dotsym then
        p:=readdotsuffix(p)

    when lcurlysym then
        p:=readkeyindex(p)

    when colonsym then
        case listtype
        when 'PARAM' then
            lex()
            p:=createunit2(jkeyword,p,readunit())
        when 'DICT' then
            lex()
            p:=createunit2(jkeyvalue,p,readunit())
        else
            exit
        esac

    when incrsym then
        case lx.subcode
        when jincrload then opc:=jloadincr
        when jdecrload then opc:=jloaddecr
        esac
        lex()
        p:=createunit1(opc,p)

    else
        exit
    enddoswitch

    p.pos:=pos

    return p
end

function readterm:unit=
unit p,q,r
ref char pbyte
word64 a
int oldipl,opc,oldinrp,pos,shift,t,nparams,length

array [20]char str
int64 sa@&str

    pos:=lx.pos

    switch lx.symbol
    when namesym then
        p:=createname(lx.symptr)
        p.pos:=lx.pos
        lex()

    when intconstsym then
        case lx.subcode
        when tint then p:=createintunit(lx.value)
        when tword then p:=createwordunit(lx.value)
        else
            serror("Unknown intconst type")
        esac
        lex()

    when realconstsym then
        p:=createrealunit(lx.xvalue)
        lex()

    when stringconstsym then
        p:=createstringunit(lx.svalue)
        lex()

    when decimalconstsym then
        p:=createstringunit(lx.svalue)
        p.tag:=jdecimal
        lex()

    when charconstsym then
        length:=strlen(lx.svalue)
        sa:=0
        if length>8 then
            serror("char const too long")
        fi
        memcpy(&.str,lx.svalue,length)
        p:=createintunit(sa)
        lex()

    when boolconstsym then
        p:=createboolunit(lx.subcode)
        lex()

    when lbracksym then
        p:=readlbrack()

    when stdtypesym then
        if lx.subcode=tvoid then
            lex()
            if lx.symbol=dotsym and nextlx.symbol=ktypesym then
                lex()
                lex()
                p:=createunit0(jtypeconst)
                p.mode:=tvoid
            else
                p:=createunit0(jnull)
            fi
        else
            p:=readcast()

        fi


    when addsym then
        lex()
        p:=readterm2()

    when subsym  then
        lex()
        if lx.symbol=assignsym then
            opc:=jneg
            goto dounary
        fi
        p:=readterm2()
        if p.tag=jintconst then
            p.value:=-p.value
        else
            p:=createunit1(jneg, p)
        fi

    when notlsym, istruelsym, inotsym, abssym, sqrsym, signsym,ascsym, chrsym,
            incrsym, decrsym, mathssym, maths2sym  then
        opc:=lx.subcode
        lex()
        if lx.symbol=assignsym then
dounary::
            lex()
            p:=createunit1(jtocodes[opc],readterm2())
        else
            p:=createunit1(opc, readterm2())
        fi


    when lsqsym then
        p:=readset()

    when minsym, maxsym then
        opc:=lx.subcode
        lexchecksymbol(lbracksym)
        lex()
        p:=readunit()
        checksymbol(commasym)
        lex()
        p:=createunit2(opc,p, readunit())
        checksymbol(rbracksym)
        lex()

    when ksprintsym then
        p:=readsprint()

    when ksreadsym,ksreadlnsym then
        p:=readsread()

    when addrsym,ptrsym then
        opc:=lx.subcode
        lex()
        p:=createunit1(opc,readterm2())
        if p.a.tag=jcall then
            if p.a.b then
                serror("Params not allowed")
            fi
            p.a:=p.a.a          
        fi

    when daddrsym then
        lex()
        p:=createunit1(jdaddrof,readterm2())

    when knewsym then
        lexchecksymbol(lbracksym)
        lex()
        q:=readslist(0,0,nparams)
        checksymbol(rbracksym)
        lex()
        p:=createunit1(jnew,q)
        p.nparams:=nparams

    when compilervarsym then
        p:=readcompilervar()
        lex()

    when dollarsym then
        if intabledata then
            p:=createstringunit(tabledataname,-1)
        else
            if ndollar<=0 then
                serror("[$] No array")
            fi
            p:=createunit1(jupb,dollarstack[ndollar])
        fi
        lex()

    when dotsym,kglobalsym then
        lexchecksymbol(namesym)
        p:=createname(lx.symptr)
        p.pos:=lx.pos
        lex()

    when kmapsym then
        p:=readmap()

    when kclampsym then
        lexchecksymbol(lbracksym)
        lex()
        p:=readunit()
        checksymbol(commasym)
        lex()
        q:=readunit()
        if lx.symbol=rbracksym and q.tag=jmakerange then
            r:=q.b
            q:=q.a
        else
            checksymbol(commasym)
            lex()
            r:=readunit()
            checksymbol(rbracksym)
        fi
        lex()

        q:=createunit2(jmax,p,q)
        p:=createunit2(jmin,q,r)

    when kgotosym then
        p:=readgoto()

    when kifsym then
        p:=readif()

    when kunlesssym then
        p:=readunless()

    when kcasesym,kdocasesym,kswitchsym,kdoswitchsym then
        p:=readswitchcase()

    when kforsym then
        p:=readfor()

    when ktosym then
        p:=readto()

    when kdosym,kdooncesym then
        p:=readdo()

    when kwhilesym then
        p:=readwhile()

    when krepeatsym then
        p:=readrepeat()

    when kloopsym then
        p:=readloopcontrol()

    when kreturnsym then
        p:=readreturn()

    when kstopsym then
        p:=readstop()

    when kprintsym then
        p:=readprint()

    when kreadsym then
        p:=readread()

    when ktrysym then   
        p:=readtry()
    when kraisesym then 
        lex()
        p:=createunit1(jraise,readunit())

    when kswapsym then          
        lexchecksymbol(lbracksym)
        lex()
        p:=readunit()
        checksymbol(commasym)
        lex()
        q:=readunit()
        checksymbol(rbracksym)
        lex()
        p:=createunit2(jswap,p,q)

    when khostfnsym then
        p:=readhostparams(nil,1)

    when knilsym then
        p:=createunit0((lx.subcode=1|jpnil|jnil))
        lex()

    when kstrincludesym then
        lex()
        p:=createunit1(jstrinclude,readterm2())

    when curlsym then           
        p:=readcurlop()

    when andlsym then
        opc:=lx.subcode
        lex()
        p:=createunit1(jmandl, readterm2())

    when kevalsym then
        lex()
        p:=createunit1(jeval,readunit())

    else

error::
        cpl symbolnames[lx.symbol]
        serror("readterm?")
    endswitch

    p.pos:=pos
    return p
end

function readxunit:unit=
    return readsunit()
end

function readsunit(int inwhile=0)unit=
int lineno,m,globalflag,staticflag
unit ulist,ulistx,p,q,r
symbol stname

lineno:=lx.pos
ulist:=ulistx:=nil
globalflag:=local_scope
staticflag:=0

repeat
    while lx.symbol=semisym do
        lex()
    od

    switch lx.symbol
    when kstaticsym then
        lex()
        staticflag:=1
        redo

    when kglobalsym then
        if globalflag then serror("global global?") fi
        globalflag:=lx.subcode
        lex()
        redo

    when kprocsym,kfunctionsym then
        readprocdef(globalflag)
        globalflag:=local_scope

    when kvarsym,kletsym then
dovar::
        q:=readvardef(lx.symbol=kletsym, globalflag,staticflag)
        while q do                              
            r:=q.nextunit                       
            q.nextunit:=nil
            addlistunit(ulist,ulistx,q)         
            q:=r
        od

        globalflag:=staticflag:=local_scope

    when kconstsym then
        if staticflag then serror("static?") fi
        readconstdef(globalflag)
        globalflag:=local_scope

    when ktypesym then
        readtypedef(globalflag)
        globalflag:=local_scope

    when krecordsym, kstructsym then
        readrecorddef(globalflag, nil)
        globalflag:=local_scope

    when ktabledatasym then
        readtabledef(globalflag)
        globalflag:=local_scope

    when docstringsym then
        if ndocstrings>=maxdocstring then
            serror("Too many docstrings for fn")
        fi
        docstrings[++ndocstrings]:=pcm_copyheapstringn(lx.svalue, lxlength)
        lex()

    when kenumsym then      
        readenumtype(globalflag)


    when kimportdllsym then
        readimportdll()


    when kmacrosym then
        readmacrodef(globalflag)
        globalflag:=local_scope

    when eofsym then
        exit

    when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,sendtosym,
            kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
        exit

    when namesym then
        case nextlx.symbol
        when dcolonsym,colonsym then
            p:=createunit1(jlabeldef,createname(addsymbol(stcurrproc, lx.symptr, labelid, 0)))
            lex()
            lx.symbol:=semisym
            addlistunit(ulist,ulistx,p)
        when namesym then
            goto dovar
        else
            goto doexec
        esac
    when kdosym then                
        if inwhile then
            exit
        fi
        goto doexec

    when kheadersym then
        repeat
            lex()
        until lx.symbol=semisym

    when semisym then
    when lsqsym then
        doexec
    elsif istypestarter() and nextlx.symbol<>lbracksym then
        goto dovar

    else                            

doexec::
        p:=readunit()

        if p.tag=jname and lx.symbol=namesym then
            serror("Possibly var/let needed")
        fi
        addlistunit(ulist,ulistx,p)
        if lx.symbol=kdosym then
            exit
        fi

    endswitch

until lx.symbol<>semisym

case lx.symbol
when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,sendtosym,
    kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
    barsym,eofsym then
else
    serror("Readsunit: "";"" expected, or bad unit starter")
esac

if ulist=nil or ulist.nextunit then         
    return createunit1(jblock,ulist)
else
    return ulist                            
fi
end

global proc checksymbol(int symbol)=
    array [100]char str

    if lx.symbol<>symbol then
        fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]
        serror(&.str)
    fi
end

proc checkequals=
    if lx.symbol<>eqsym then
        serror("""="" expected")
    fi
end

function checkbegin(int fbrack)int=
    int closesym

    skipsemi()

    if lx.symbol=lbracksym and fbrack then
        closesym:=rbracksym
        lex()
    elsif lx.symbol=lcurlysym then
        closesym:=rcurlysym
        lex()
    else
        closesym:=kendsym
    fi
    return closesym
end

proc checkbeginend(int closesym,kwd,startline=0)=
    skipsemi()
    if closesym=rbracksym or closesym=rcurlysym then
        checksymbol(closesym)
    else
        checkend(closesym,kwd,startline:startline)
    fi
    lex()
end

global proc skipsemi=
    while lx.symbol=semisym do lex() od
end

function readindex(unit p,int dot)unit=
    unit q,plower,pupper

    lex()

    do
        if ndollar>=maxdollarstack then
            serror("Too many nested a[$]")
        fi
        dollarstack[++ndollar]:=p
        q:=readunit()
        --ndollar

            p:=createunit2((dot|jdotindex|jindex),p,q)
        exit when lx.symbol<>commasym
        lex()
    od
    checksymbol(rsqsym)
    lex()
    return p
end

function readdotsuffix(unit p)unit=
    unit q
    int t

    while lx.symbol=dotsym do
        lex()
        switch lx.symbol
        when lsqsym then
            p:=readindex(p,1)
        when namesym then
            p:=createunit2(jdot,p,createname(lx.symptr))
            lex()
        when propsym then           
doprop::
            p:=createunit1(lx.subcode,p)
            lex()
        when ktypesym then
            p:=createunit1(jtype,p)
            lex()

        when maxsym then
            lx.subcode:=jmaxvalue
            goto doprop

        when minsym then
            lx.subcode:=jminvalue
            lx.symbol:=propsym
            goto doprop
        when dollarsym then
            if p.tag not in [jname,jdot] then
                serror("...name.$ needed")
            fi
            p:=createunit1(jsymbol,p)
            lex()

        else
            serror("Unknown dot suffix")
        endswitch
    od
    return p
end

function readslist(int iscall=0,donulls, &nparams)unit=
    unit ulist,ulistx
    int oldinparamlist

    ulist:=ulistx:=nil
    nparams:=0

    skipsemi()
    if lx.symbol=rbracksym then     
        return ulist
    fi

    pushlisttype((iscall|'PARAM'|0))

    do
        skipsemi()
        case lx.symbol
        when commasym then
            if donulls then
                addlistunit(ulist,ulistx,createunit0(jnull))
            else
                serror("null comma expr not allowed")
            fi
            lex()
        when rbracksym then
            if donulls then
                addlistunit(ulist,ulistx,createunit0(jnull))
            fi
            exit
        else
            addlistunit(ulist,ulistx,readunit())
            ++nparams
            if lx.symbol=commasym then
                lex()
                if lx.symbol=rbracksym then
                    exit
                fi
            else
                skipsemi()
                if lx.symbol=rbracksym then
                    exit
                fi
                serror("SLIST?")
            fi
        esac
    od

    poplisttype()
    return ulist
end

function readcondsuffix(unit p)unit=

    switch lx.symbol
    when kwhensym then
        lex()
        return createunit2(jif,readunit(),createunit1(jblock,p))
    when kunlesssym then
        lex()
        return createunit2(jif, createunit1(jnotl,readunit()),createunit1(jblock,p))
    else
        return p
    endswitch
end

function readkeyindex(unit p)unit=
    unit q

    lex()

    q:=readunit()

    if lx.symbol=commasym then
        lex()
        q.nextunit:=readunit()
    fi
    
    p:=createunit2(jkeyindex,p,q)

    checksymbol(rcurlysym)
    lex()
    return p
end

function readlbrack:unit=

    unit ulist,ulistx, p,q,r
    int oldirp,length,lower,lowerseen,elemtype

    lex()                   
    ulist:=ulistx:=nil
    length:=0
    lower:=1
    lowerseen:=0

    elemtype:=tvoid


    if lx.symbol=stdtypesym and nextlx.symbol=colonsym then
        elemtype:=lx.subcode
        lex()
        lex()
    fi

    if lx.symbol=intconstsym and nextlx.symbol=colonsym then
        lower:=lx.value
        lowerseen:=1
        lex()
        lex()
    fi


    case lx.symbol
    when rbracksym then         
        lex()
        p:=createunit0(jmakelist)
        p.length:=0
        p.lower:=lower
        p.elemtype:=elemtype
        return p
    elsif (binopset[lx.symbol] or unaryopset[lx.symbol] or lx.symbol=propsym) and
            nextlx.symbol=rbracksym then
        p:=createunit0(joperator)
        p.pclopcode:=jpclcodes[lx.subcode]
        lex()
        checksymbol(rbracksym)
        lex()
        return p

    else                    
        p:=readxunit()
    esac

    case lx.symbol
    when rbracksym then         
        lex()
        if lowerseen then
            p:=createunit2(jkeyvalue,createintunit(lower), p)
        fi

        return p

    when commasym then
        length:=1
        if nextlx.symbol=rbracksym then     
            lex()
            lex()
            p:=createunit1(jmakelist,p)
            p.length:=length
            p.lower:=lower
            p.elemtype:=elemtype
            return p
        fi

        ulist:=ulistx:=p
        repeat
            lex()                           
            if lx.symbol=rbracksym then     
                exit
            fi
            if lx.symbol=commasym then
                serror(",, null expr not allowed")
            fi
            addlistunit(ulist,ulistx,readxunit())
            ++length
            skipsemi()                      
        until lx.symbol<>commasym
        checksymbol(rbracksym)
        lex()
        p:=createunit1(jmakelist,ulist)
        p.length:=length
        p.lower:=lower
        p.elemtype:=elemtype
        return p

    when barsym then            
        lex()
        q:=readxunit()
        case lx.symbol
        when barsym then        
            lex()
            r:=readsunit()
            checksymbol(rbracksym)
            lex()
            q.nextunit:=r
            return createunit2(jif,p,q)
        when rbracksym then
            lex()
            return createunit2(jif,p,q)

        esac

        addlistunit(ulist,ulistx,q) 
        checksymbol(commasym)
        if nextlx.symbol<>barsym then       
            repeat
                lex()               
                addlistunit(ulist,ulistx,readxunit())
            until lx.symbol<>commasym
            checksymbol(barsym)
        else
            lex()                   
        fi
        lex()
        r:=readxunit()
        checksymbol(rbracksym)
        lex()
        p.nextunit:=r
        return createunit2(jselect,p,ulist)

    else
        serror("(x ...")
    esac
    return nil
end

function readif:unit=
    int line, kwd, lineno
    unit pthen,pcond, plist,plistx, pelse, p, pelsif

    line:=lx.pos

    kwd:=lx.symbol          

    lex()
    pcond:=readsunit()
    skipsemi()

    checksymbol(kthensym)
    lex()

    pthen:=readsunit()

    case lx.symbol
    when kelsifsym then
        lx.symbol:=kifsym       
        pelse:=readif()

    when kelsesym then      
        lex()
        pelse:=readsunit()
        checkend(kendsym,kwd)
        lex()
    when kelsecasesym,kelseswitchsym then
        lx.symbol:=kwd
    SERROR("ELSECASE NOT READY")
    else
        PELSE:=NIL
        checkend(kendsym,kwd)
        lex()
    esac

    pthen.nextunit:=pelse
    p:=createunit2(jif,pcond,pthen)
    p.pos:=line

    return p
end

proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=
array [100]char str

    if endsym=lx.symbol=rbracksym then
        return
    fi

    if lx.symbol<>kendsym then
        strcpy(&.str,"Bad 'end' ")
error::

        if startline then
            fprint @(&.str+strlen(&.str))," (from line #)",startline
        fi
        serror(&.str)
    fi

    if lx.subcode=0 then                    
        return
    fi

    unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
        strcpy(&.str,"Mismatched 'end'")
        goto error
    end unless
end

function readunless:unit=
    int line
    unit pcond, pthen, pelse, p
    line:=lx.pos
    lex()
    pcond:=readsunit()
    checksymbol(kthensym)
    lex()

    pthen:=readsunit()

    if lx.symbol=kelsesym then
        lex()
        pelse:=readsunit()
    else            
        pelse:=nil
    fi
    checkend(kendsym,kunlesssym)
    lex()
    pthen.nextunit:=pelse
    p:=createunit2(jif,createunit1(jnotl,pcond),pthen)
    p.pos:=line
    return p
end

function readwhile:unit=
    int pos,id
    unit pcond, pa, pb, pc, pbody, p

    pos:=lx.pos
    lex()

    pcond:=readsunit(1)

    checksymbol(kdosym)
    lex()
    pbody:=readsunit()

    checkend(kendsym,kwhilesym,kdosym)
    lex()

    p:=createunit2(jwhile,pcond,pbody)
    p.pos:=pos
    return p
end

function readrepeat:unit=
    int pos
    unit pbody, pcond, p

    pos:=lx.pos
    lex()
    pbody:=readsunit()
    checksymbol(kuntilsym)
    lex()
    pcond:=readunit()
    p:=createunit2(jrepeat,pbody,pcond)
    p.pos:=pos
    return p
end

function readfor:unit=

    int line, opc, down, isforeach
    unit pstep, pvar, pcond, pfrom, pto, pelse, pbody, p, plist,pvar2

    line:=lx.pos
    isforeach:=lx.subcode
    lex()           
    pvar:=readterm2()

    if pvar.tag<>jname then
        serror("For: name expected")
    else
        pvar.def.forindex:=1
    fi

    opc:=jforup
    pstep:=nil
    pcond:=nil
    pvar2:=nil

    if lx.symbol=commasym then          
        lex()
        pvar2:=readterm2()
    fi

    if lx.symbol in [insym,inrevsym] then   
        down:=lx.symbol=inrevsym
        lex()
        plist:=readunit()

        case plist.tag
        when jmakerange then        
            opc:=jforup
            pfrom:=plist.a
            pto:=plist.b
        when jbounds then           
            plist.tag:=jboundsx
            opc:=jforupx
        else
            opc:=jforall
        esac
    else
        if lx.symbol=assignsym then
            lex()
            pfrom:=readunit()
        else
            pfrom:=createintunit(1)
        fi
        checksymbol(ktosym)
        down:=lx.subcode=1
        opc:=jforup
        lex()
        pto:=readunit()

        if lx.symbol=kbysym then
            lex()
            pstep:=readunit()
            if pstep.tag<>jintconst then serror("BY needs int constant") fi
            if pstep.value<0 then 
                serror("Step must be positive")
            elsif pstep.value=0 then
                serror("Zero step")
            fi
            pstep.value:=abs pstep.value
            if pstep.value=1 then       
                pstep:=nil
            fi
        fi
    fi

    if lx.symbol=kwhensym then
        lex()
        pcond:=readunit()
    fi
    checksymbol(kdosym)
    lex()
    pbody:=readsunit()

    if pcond<>nil then
        pbody:=makeblock(createunit2(jif,pcond,pbody))
    fi
    if lx.symbol=kelsesym then
        lex()
        pelse:=readsunit()
        pbody.nextunit:=pelse
    else
        pelse:=nil
    fi
    checkend(kendsym,kforsym,kdosym)
    lex()


    case opc
    when jforall then
        pvar.nextunit:=plist
        plist.nextunit:=pvar2
        p:=createunit2((down|jforallrev|jforall),pvar,pbody)

    when jforupx then
        pvar.nextunit:=plist
        p:=createunit2((down|jfordownx|jforupx),pvar,pbody)
    else
        pvar.nextunit:=pfrom
        pfrom.nextunit:=pto
        pto.nextunit:=pstep
        p:=createunit2((down|jfordown|jforup),pvar,pbody)
    esac

    if isforeach then
        if p.tag=jforall then
            p.tag:=jforeach
        else
            serror("Foreach?")
        fi
    fi

    p.pos:=line

    if pvar2 and opc not in [jforall, jforallrev] then
        serror("for i,j not allowed")
    fi

    return p
end

function readdo:unit=
    unit p
    int line,opc

    line:=lx.pos
    opc:=(lx.symbol=kdosym|jdo|jdoonce)
    lex()
    p:=readsunit()
    checkend(kendsym,kdosym)
    lex()
    p:=createunit1(opc,p)
    p.pos:=line
    return p
end

function readto:unit=
    int line,id
    unit p, pcount, pbody

    line:=lx.pos
    lex()

    pcount:=readunit()

    checksymbol(kdosym)
    lex()
    pbody:=readsunit()
    checkend(kendsym,ktosym,kdosym)
    lex()

    pcount.nextunit:=createavname()

    p:=createunit2(jto,pcount,pbody)
    p.pos:=line
    return p
end

function makeblock(unit p)unit=
    return createunit1(jblock,p)
end

function readvardef(int islet, isglobal=0, isstatic=0)unit=
    int nvars,varid,m, opc
    symbol d
    unit ulist, ulistx, p

    if stcurrproc=stmglobals then isstatic:=1 fi

    m:=readtypespec(lx.symbol=kvarsym)


    if stcurrproc.nameid=procid then
        varid:=(isstatic|staticid|frameid)
    else
        varid:=staticid
    fi
    nvars:=0
    ulist:=ulistx:=nil

    while lx.symbol=namesym do
        ++nvars

        d:=addsymbol(stcurrproc, lx.symptr, varid, isglobal)
        storemode(stcurrproc,m,&d.mode)

        lex()

        case lx.symbol
        when assignsym,deepcopysym then
            opc:=lx.subcode
            if varid=staticid then
                if stcurrproc.nameid=procid then
                    serror("Need '=' for static in proc")
                fi
            fi
            d.initcode:=(lx.symbol=assignsym|2|3)
            lex()
            d.code:=readunit()
            if varid=frameid then
                p:=createunit2(opc,createname(d),d.code)
                addlistunit(ulist, ulistx, p)
            fi

        when eqsym then
            if varid<>staticid then serror("Need ':=' for non-static") fi
            lex()

            d.initcode:=1
            d.code:=readunit()
        else
            if islet then
                serror("let needs :=")
            fi
        esac

        if lx.symbol<>commasym then
            exit
        fi
        lex()
    od

    if nvars=0 then
        serror("No vars declared")
    fi
    return ulist
end

proc readconstdef(int isglobal=0)=
    int nvars
    symbol d

    lex()

    nvars:=0
    while lx.symbol=namesym do
        ++nvars

        d:=addsymbol(stcurrproc, lx.symptr, constid, isglobal)
        lexchecksymbol(eqsym)
        lex()

        d.code:=readunit()

        if lx.symbol<>commasym then
            exit
        fi
        lex()
    od

    if nvars=0 then
        serror("No consts declared")
    fi
end

function readreturn:unit=
    unit p,q,r

    lex()
    if exprstarterset[lx.symbol] then
        q:=readunit()
        p:=createunit1(jreturn,q)
    else
        p:=createunit0(jreturn)
    fi

    return readcondsuffix(p)
end

function readprint:unit=
    int opc, isfprint, fshowname, length
    unit pformat, pdev, printlist,printlistx, p,q
    ref strbuffer expr

    ichar s

    pushlisttype('PRINT')

    opc:=lx.subcode

    case opc
    when jfprint,jfprintln then
        isfprint:=1
    else
        isfprint:=0
    esac

    lex()

    printlist:=printlistx:=nil
    pformat:=pdev:=nil

    if lx.symbol=atsym then
        lex()
        pdev:=readunit()
        if lx.symbol=commasym then lex() else goto finish fi
    fi
    if isfprint then
        pformat:=readunit()
        if lx.symbol=commasym then lex() else goto finish fi
    fi

    if not exprstarterset[lx.symbol] then
        goto finish
    fi

    do
        case lx.symbol
        when commasym then      
            addlistunit(printlist,printlistx, createunit0(jnogap))
        when dollarsym then
            addlistunit(printlist,printlistx, createunit0(jspace))
            lex()
        else

            fshowname:=0
            if lx.symbol=eqsym then
                fshowname:=1
                lex()
            fi

            p:=readunit()
            if lx.symbol=colonsym then
                lex()
                p:=createunit2(jfmtitem,p,readunit())
            fi

            if fshowname then
                expr:=strexpr(p)
                strbuffer_add(expr,"=")
                s:=expr.strptr

                iconvucn(expr.strptr,expr.length)

                addlistunit(printlist,printlistx,q:=createstringunit(s,expr.length))
            fi
            addlistunit(printlist,printlistx,p)
        esac
        if lx.symbol<>commasym then exit fi
        lex()
    od

    finish::
    if opc=jprint and printlist=nil then
        serror("No print items")
    fi
    if opc=jfprint and printlist=nil and pformat=nil then
        serror("No print items")
    fi

    poplisttype()
    if isfprint then
        if pformat=nil then
            serror("No fmt str")
        fi
        if pformat=nil then
            pformat:=makeblock(pformat)
        fi
        pformat.nextunit:=printlist
        return createunit2(opc,pdev,pformat)
        return pformat
    else

        return createunit2(opc,pdev,printlist)
    fi

end

function readread:unit=
    int opc
    unit pformat, pdev, readlist, readlistx, p

    pushlisttype('PRINT')
    opc:=lx.subcode
    lex()

    readlist:=readlistx:=nil
    pformat:=pdev:=nil

    if lx.symbol=atsym then
        if opc=jread then
            serror("@ on read")
        fi
        lex()
        pdev:=readunit()
        if lx.symbol=commasym then lex() else goto finish fi
    fi

    if not exprstarterset[lx.symbol] then
        goto finish
    fi

    do
        p:=readunit()
        if lx.symbol=colonsym then
            lex()
            p:=createunit2(jfmtitem,p,readunit())
        fi
        addlistunit(readlist,readlistx,p)
        if lx.symbol<>commasym then exit fi
        lex()
    od

    finish::
    if opc=jread and readlist=nil then
        serror("No read items")
    fi

    poplisttype()
    return createunit2(opc,pdev,readlist)
end

function readloopcontrol:unit=
    int opc
    unit p

    opc:=lx.subcode
    lex()
    if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
        lex()
        p:=createunit1(opc,createintunit(0))

    elsif exprstarterset[lx.symbol] then
        p:=createunit1(opc,readintunit())
    else
        p:=createunit1(opc,createintunit(1))
    fi
    return readcondsuffix(p)
end

function readintunit:unit p=
    p:=readunit()
    if p.tag<>jintconst then
        serror("int expr needed")
    fi
    return p
end

function readconstint:int=
    unit p

    p:=readunit()
    case p.tag
    when jintconst then
        return p.value
    when jrealconst then
        return p.xvalue
    else
        serror("Can't do complex expr")
    esac
    return 0
end

proc readenumtype(int isglobal=0)=
    int index, n, lower, m
    symbol d, e, nameptr


    lex()
    if lx.symbol<>lbracksym then
        checksymbol(namesym)
        nameptr:=lx.symptr

        lex()
        checkequals()
        lex()
        e:=addsymbol(stcurrproc, nameptr, typeid, isglobal)
        m:=addanontype()
    else
        e:=nil
        m:=tvoid
    fi
    checksymbol(lbracksym)
    lex()
    if lx.symbol=rbracksym then serror("Empty enums") fi

    index:=1
    n:=0

    while lx.symbol=namesym do
        ++n

        d:=addsymbol(stcurrproc, lx.symptr, enumid, isglobal)
        lex()

        case lx.symbol
        when eqsym then
            if n>=1 then serror("'=' only for 1st elem") fi
            lex()
            lower:=index:=readconstint()
        esac

        d.index:=index++
        d.mode:=m


        if lx.symbol<>commasym then
            exit
        fi
        lex()
    od
    checksymbol(rbracksym)
    lex()

    if e=nil then return fi



    e.mode:=m
    ttfields[m]:=stcurrproc.deflist
    ttlength[m]:=n
    ttlower[m]:=lower
    ttbasetype[m]:=tenumdef

    createusertype(e, m)

end

function readswitchcase:unit=
    int pos, kwd, opc, lineno,rangeused, nwhen
    unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

    pos:=lx.pos
    kwd:=lx.symbol          
    opc:=lx.subcode         

    lex()

    skipsemi()
    if lx.symbol=kwhensym then
        if kwd=kswitchsym then
            serror("switch expr missing")
        fi
        pexpr:=CREATEUNIT0(JNONE)
    else
        pexpr:=readsunit()      
    fi

    pwhenlist:=pwhenlistx:=nil
    rangeused:=0
    nwhen:=0

    skipsemi()
    while lx.symbol=kwhensym do 
        pos:=lx.pos
        lex()
        pwhen:=pwhenx:=nil
        do
            p:=readunit()
            ++nwhen
            p.pos:=pos
            if p.tag=jmakerange then rangeused:=1 fi
            addlistunit(pwhen,pwhenx,p)
            if lx.symbol<>commasym then exit fi
            lex()
        od
        if lx.symbol<>kthensym then checksymbol(sendtosym) fi
        lex()
        pthen:=readsunit()
        pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
        pwhenthen.pos:=pos
        addlistunit(pwhenlist,pwhenlistx,pwhenthen)
    od


    case lx.symbol
    when kelsesym then      
        lex()
        pelse:=readsunit()

        checkend(kendsym,kwd)
        lex()
    when kelsifsym then
        lx.symbol:=kwd
        pelse:=makeblock(readif())
    when kelsecasesym, kelseswitchsym then

        lx.symbol:=kwd
        pelse:=readswitchcase()
    else
        PELSE:=NIL
        checkend(kendsym,kwd)
        lex()
    esac

    pexpr.nextunit:=pelse

    p:=createunit2(opc,pexpr,pwhenlist)
    p.pos:=pos
    return p
end

function readgoto:unit=

    if lx.subcode=1 then        
        lexchecksymbol(ktosym)
    fi
    lex()

    return readcondsuffix(createunit1(jgoto,readunit()))
end

function readstop:unit=
    unit p
    int i
    lex()
    if exprstarterset[lx.symbol] then
        p:=createunit1(jstop,readunit())
    else
        p:=createunit1(jstop,createintunit(0))
    fi
    return readcondsuffix(p)
end

function readcast:unit p=
    int t,opc

    t:=lx.subcode
    lex()

    if t=trange and lx.symbol=lbracksym then
        lex()
        p:=readunit()
        if p.tag in [jkeyvalue,jkeyword] then
            p.tag:=jmakerangelen
        elsif p.tag=jmakerange then
        else
            serror("need a..b or a:n")
        fi
        checksymbol(rbracksym)
        lex()
        return p
    fi


    case lx.symbol
    when atsym,lbracksym then

    else                        
        p:=createunit0(jtypeconst)
        p.mode:=t
        return p
    esac

    if lx.symbol=atsym then
        lex()
        opc:=jtypepun
    else
        opc:=jconvert
    fi
    checksymbol(lbracksym)
    p:=readterm()

    p:=createunit1(opc,p)
    storemode(stcurrproc,t,&p.mode)
    return p
end

function readset:unit=
    int length,nkeyvalues,oldinparamlist
    unit p,ulist,ulistx

    lex()                   

    case lx.symbol
    when rsqsym then        
        lex()
        return createunit1(jmakeset,nil)
    when colonsym then
        lexchecksymbol(rsqsym)
        lex()
        return createunit1(jmakedict,nil)
    esac

    pushlisttype('DICT')

    p:=readunit()
    length:=1
    nkeyvalues:=0
    if p.tag=jkeyvalue then ++nkeyvalues fi

    ulist:=ulistx:=p

    while lx.symbol=commasym do
        lex()
        if lx.symbol=rsqsym then exit fi        
        addlistunit(ulist,ulistx,p:=readunit())
        if p.tag=jkeyvalue then ++nkeyvalues fi

        ++length
        skipsemi()                      
    od

    checksymbol(rsqsym)

    lex()
    if nkeyvalues then
        if length>nkeyvalues then serror("dict: mixed elements") fi
        p:=createunit1(jmakedict,ulist)
    else
        p:=createunit1(jmakeset,ulist)
    fi
    p.length:=length
    poplisttype()
    return p
end

global proc readtabledef(int isglobal=0)=
    int i,ncols,nrows,enums,nextenumvalue,startline,closesym, enummode, firstvalue
    int ltype,lower
    unit ulist,ulistx, plower, p
    const maxcols=20
    array [maxcols]symbol varnames
    array [maxcols]unit plist,plistx
    symbol d, nameptr, e

    const maxrows=500

    enums:=lx.subcode                       
    lex()
    e:=nil
    enummode:=tvoid

    if lx.symbol=lbracksym then     
        lex()
        if not enums then           
            enums:=1
        else                        
            checksymbol(namesym)
            nameptr:=lx.symptr
            lex()
            e:=addsymbol(stcurrproc, nameptr, typeid, isglobal)
            enummode:=addanontype()
        fi
        checksymbol(rbracksym)  
        lex()
    fi


    firstvalue:=nextenumvalue:=1
    
    nrows:=0            
    ncols:=0            


    while lx.symbol=namesym do
        if ++ncols>maxcols then
            serror("tabledata/too many columns")
        fi
        varnames[ncols]:=lx.symptr

        lex()
        if lx.symbol=commasym then
            lex()
        else
            exit
        fi
    od

    checkequals()
    lex()                   

    skipsemi()
    startline:=lx.pos
    closesym:=checkbegin(0)

    skipsemi()

    for i:=1 to ncols do
        plist[i]:=plistx[i]:=nil
    od
    ulist:=ulistx:=nil

    intabledata:=1
    do          
        skipsemi()
        checksymbol(lbracksym)
        lex()
        if ++nrows>maxrows then
            serror("tabledata:too many rows")
        fi

        if enums then
            checksymbol(namesym)

            d:=addsymbol(stcurrproc,lx.symptr,enumid, isglobal)
            d.mode:=enummode
            lex()

            case lx.symbol
            when eqsym then
                if nrows>1 then serror("tabledata '=' not 1st") fi
                lex()
                p:=readunit()
                if p.tag=jintconst then
                    firstvalue:=nextenumvalue:=p.value
                else
                    SERROR("TABLEDATA: COMPLEX ENUM VAL")
                fi
            esac

            d.index:=nextenumvalue++

            tabledataname:=d.name

            if ncols then               
                checksymbol(commasym)       
            fi
            lex()
        fi

        for i:=1 to ncols do
            addlistunit(plist[i],plistx[i],readunit())
            if i=ncols then
                checksymbol(rbracksym)
            else
                checksymbol(commasym)
            fi
            lex()
        od

        if lx.symbol<>commasym then exit fi
        lex()                   
        if lx.symbol=closesym then exit fi      
    od

    intabledata:=0

    skipsemi()
    checkbeginend(closesym,ktabledatasym,startline)





    if nrows=0 then serror("No table data") fi

    for i:=1 to ncols do
        d:=addsymbol(stcurrproc,varnames[i],staticid,isglobal)

        p:=d.code:=createunit1(jmakelist,plist[i])
        p.length:=nrows
        p.lower:=firstvalue
    od

    if e=nil then return fi

    e.mode:=enummode
    ttfields[enummode]:=stcurrproc.deflist
    ttlength[enummode]:=nrows
    ttlower[enummode]:=firstvalue
    ttbasetype[enummode]:=tenumdef

    createusertype(e, enummode)
end

function readtry:unit=
    unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
    lex()

    ptry:=readsunit()
    pexceptlist:=pexceptlistx:=nil          

    while lx.symbol=kexceptsym do
        lex()
        exlist:=exlistx:=nil                
        do
            addlistunit(exlist,exlistx,readunit())
            if lx.symbol<>commasym then exit fi
            lex()
        od
        checksymbol(kthensym)
        lex()
        px:=readsunit()
        addlistunit(pexceptlist,pexceptlistx,createunit2(jexcept,exlist,px))
    od
    checkend(kendsym,ktrysym)
    lex()


    return createunit2(jtry,ptry,pexceptlist)
end

function readsprint:unit=
    int opc,isfprint
    unit pformat, pdev, printlist, printlistx, p

    pushlisttype('PRINT')
    opc:=lx.subcode
    lexchecksymbol(lbracksym)
    lex()

    case opc
    when jsfprint then
        isfprint:=1
    else
        isfprint:=0
    esac

    printlist:=printlistx:=nil
    pformat:=pdev:=nil

    if lx.symbol=atsym then
        lex()
        pdev:=readunit()
        if lx.symbol=commasym then lex() else goto finish fi
    fi
    if isfprint then
        pformat:=readunit()
        if lx.symbol=commasym then lex() else goto finish fi
    fi

    if lx.symbol=rbracksym then
        goto finish
    fi

    do
        if lx.symbol=commasym then      
            addlistunit(printlist,printlistx,createunit0(jnogap))
        else
            p:=readunit()
            if lx.symbol=colonsym then
                lex()
                p:=createunit2(jfmtitem,p,readunit())
            fi
            addlistunit(printlist,printlistx,p)
        fi
        if lx.symbol<>commasym then exit fi
        lex()
    od

    checksymbol(rbracksym)

    finish::
    lex()
    if (opc=jprint or opc=jfprint) and printlist=nil then
        serror("No print items")
    fi

    poplisttype()
    if isfprint then
        if pformat=nil then
            serror("No fmt str")
        fi
        pformat.nextunit:=printlist
        return createunit2(opc,pdev,pformat)
    else
        return createunit2(opc,pdev,printlist)
    fi

end

function readsread:unit=
    int opc
    unit pformat,pdev,p, readlist,readlistx

    pushlisttype('PRINT')
    opc:=lx.subcode
    lexchecksymbol(lbracksym)
    lex()

    readlist:=readlistx:=nil
    pformat:=pdev:=nil

    if lx.symbol=atsym then
        if opc=jread then
            serror("@ on read")
        fi
        lex()
        pdev:=readunit()
        if lx.symbol=commasym then lex() else goto finish fi
    fi

    if lx.symbol=rbracksym then
        goto finish
    fi

    do
        p:=readunit()
        if lx.symbol=colonsym then
            lex()
            p:=createunit2(jfmtitem,p,readunit())
        fi
        addlistunit(readlist,readlistx,p)
        if lx.symbol<>commasym then exit fi
        lex()
    od

    checksymbol(rbracksym)

    finish::
    lex()
    if opc=jread and readlist=nil then
        serror("No read items")
    fi

    poplisttype()

    return createunit2(opc,pdev,readlist)
end

proc readimportdll=
    array [256]char str
    symbol stproc,d, stname
    int closesym, startpos, isfunc, isnew

    lexchecksymbol(namesym)
    stname:=lx.symptr

    lexchecksymbol(eqsym)
    lex()


    isnew:=1
    d:=stname.nextdupl
    while d do
        if d.nameid=dllmoduleid then
            stname:=d
            isnew:=0
            exit
        fi
        d:=d.nextdupl
    od

    if isnew then           
        stname:=addsymbol(stprogram,stname,dllmoduleid,0)
        if ndlllibs>=maxdlllib then
            serror("Too many DLL libs")
        fi
        dlltable[++ndlllibs]:=stname
        stname.index:=ndlllibs
    fi

    currdllindex:=stname.index

    closesym:=checkbegin(1)
    startpos:=lx.pos
    do
        skipsemi()

        case lx.symbol
        when kfflangsym then
            lex()
        when kprocsym,kfunctionsym then
            isfunc:=lx.symbol=kfunctionsym
            lex()
            case lx.symbol
            when namesym then
                stproc:=addsymbol(stcurrproc, lx.symptr, dllprocid, 1)

            when stringconstsym then
                strcpy(str,lx.svalue)
                convlcstring(str)
                stproc:=addsymbol(stcurrproc, addglobalname(str), dllprocid, 1)
                stproc.truename:=pcm_copyheapstring(lx.svalue)
            else
                serror("fn name expected")
            esac

            stproc.misfunc:=isfunc
            stproc.isimport:=1

            if ndllprocs>=maxdllproc then
                serror("Too many DLL procs")
            fi
            dllproctable[++ndllprocs]:=stproc
            dlllibindex[ndllprocs]:=currdllindex
            stproc.index:=ndllprocs

            lex()

            if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
                lexchecksymbol(namesym)

                d:=addsymbol(stproc.owner, lx.symptr, aliasid, 1)
                d.alias:=stproc
                lex()
            fi
            readffiparams(stproc)
        else
            exit
        esac
    od  
    checkbeginend(closesym, kimportdllsym, startpos)
end

proc readffiparams(symbol stproc)=
    int pret,ptype

    if lx.symbol=lbracksym then
        lex()
        if lx.symbol=rbracksym then
            lex()
        else
            ptype:=readtypespec()
            if lx.symbol in [commasym,rbracksym] then       
                readtypeparams(stproc,ptype)
            else
                readtypenameparams(stproc,ptype)
            fi
        fi
    fi

    if lx.symbol in [colonsym,sendtosym] then
        if not stproc.misfunc then serror("Return type for proc?") fi
        lex()
    fi

    pret:=tvoid
    if stproc.misfunc then
        if lx.symbol=semisym then serror("Return type missing") fi
        pret:=readtypespec()
    fi

    storemode(stproc.owner,pret, &stproc.mode)
end

proc readtypeparams(symbol stproc, int ptype)=
    array [32]char str
    int nparams
    symbol stname

    nparams:=0

    do
        ++nparams
        print @str,"$",,nparams

        stname:=addsymbol(stproc, addglobalname(str), dllparamid, 0)
        storemode(stproc,ptype,&stname.mode)
        ++stproc.nparams

        if lx.symbol=commasym then
            lex()
            if lx.symbol=ellipsissym then
                stproc.mvarparams:=1
                lex()
                exit
            fi
            ptype:=readtypespec()
        else
            exit
        fi
    od
    checksymbol(rbracksym)
    lex()
end

proc readtypenameparams(symbol stproc, int ptype)=
    symbol stname

    checksymbol(namesym)
    stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
    storemode(stproc,ptype,&stname.mode)
    ++stproc.nparams
    lex()

    do

        if lx.symbol=eqsym then
            lex()
            stname.code:=readunit()
            stname.moptional:=1
        fi

        case lx.symbol
        when commasym then
            lex()
            if lx.symbol=ellipsissym then
                stproc.mvarparams:=1
                lex()
                exit
            fi

            if istypestarter() then         
                ptype:=readtypespec()
            fi
            checksymbol(namesym)
            stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
            storemode(stproc,ptype,&stname.mode)
            ++stproc.nparams
            lex()
        else
            exit
        esac
    od
    checksymbol(rbracksym)
    lex()
end

global proc readrecorddef(int isglobal, symbol d)=
    int kwd, baseclass, m, startline, closesym, caligned
    symbol nameptr

    baseclass:=0
    if d then           
        kwd:=ktypesym
        goto gotname
    fi

    kwd:=lx.symbol


    lexchecksymbol(namesym)
    nameptr:=lx.symptr

    lex()

    if lx.symbol=lbracksym then
        lex()
        baseclass:=readtypespec()
        checksymbol(rbracksym)
        lex()
    fi

    checkequals()
    lex()

    d:=addsymbol(stcurrproc, nameptr, (kwd=krecordsym|recordid|typeid), isglobal)

    if baseclass then
        if baseclass>0 then serror("baseclass?") fi
        if nbaseclasses>=255 then
                serror("Too many base classes")
        fi
        ++nbaseclasses
        storemode(stcurrproc,baseclass,&baseclasstable[nbaseclasses])
        d.baseclassindex:=nbaseclasses
        baseclassdef[nbaseclasses]:=d
    fi

gotname::

    closesym:=checkbegin(1)

    startline:=lx.pos

    if kwd=krecordsym then
        m:=readrecordbody(d)
    else
        caligned:=0
        m:=readstructbody(d,caligned)
    fi

    checkbeginend(closesym,krecordsym,startline)
end

function readrecordbody(symbol owner)int=

    symbol oldstcurrproc, e
    int m

    m:=addanontype()

    oldstcurrproc:=stcurrproc
    stcurrproc:=owner

    doswitch lx.symbol
    when kconstsym then
        readconstdef(0)
    when kvarsym then
        readrecordfields(owner)
    when kfunctionsym,kprocsym then
        readprocdef(0)

    when krecordsym then
        lex()
        serror("CLASS RECORD")
    when ktypesym then
        lex()
        serror("CLASS TYPE")
    when kendsym,rbracksym,rcurlysym then
        exit
    when eofsym then
        serror("Class eof?")
        exit
    when semisym then
        lex()
    elsif istypestarter() and nextlx.symbol<>lbracksym then
        readrecordfields(owner)
    else
            serror("Unknown record field decl")
    enddoswitch

    ttfields[m]:=owner.deflist
    ttlength[m]:=owner.nfields
    ttlower[m]:=1
    ttbasetype[m]:=trecorddef

    createusertype(owner, m)

    e:=owner.deflist
    while e do
        addgenfield(e)
        e:=e.nextdef
    od


    ttsize[m]:=varsize*owner.nfields

    stcurrproc:=oldstcurrproc

    return m
end

proc readrecordfields(symbol owner)=
    int nvars,offset,index,m
    symbol d


    m:=readtypespec(1)

    nvars:=0
    index:=owner.nfields
    offset:=index*varsize

    while lx.symbol=namesym do
        ++nvars

        d:=addsymbol(stcurrproc, lx.symptr, fieldid, 0)
        storemode(owner, m, &d.mode)
        d.atfield:=nil

        lex()

        if lx.symbol=atsym then
            lex()
            d.atfield:=readatfield()
            d.fieldoffset:=d.atfield.fieldoffset
            d.index:=d.atfield.index
        else
            d.fieldoffset:=offset
            offset+:=varsize
            d.index:=++index
        fi

        if lx.symbol<>commasym then
            exit
        fi
        lex()
    od


    if nvars=0 then
        serror("No fields")
    fi
    stcurrproc.nfields+:=nvars
end

function readstructbody(symbol owner, int caligned)int=
    int m, ngroups, nvars, t
    symbol d, e

    m:=addanontype()

    ngroups:=0

    do
        skipsemi()


        case lx.symbol
        when kstructsym then
            ++ngroups
            lex()
            addstructflag(owner,structblockid)

        when kunionsym then
            ++ngroups
            lex()
            addstructflag(owner,unionblockid)

        when kendsym then
            if ngroups then
                --ngroups
                lex()
                addstructflag(owner,endblockid)
            else
                exit
            fi

        when rbracksym then
            exit

        else
            t:=readtypespec(0)

            nvars:=0
            while lx.symbol=namesym do
                ++nvars
                d:=addsymbol(owner, lx.symptr,structfieldid, 0)
                storemode(owner,t,&d.mode)
                lex()

                if lx.symbol<>commasym then
                    exit
                fi
                lexchecksymbol(namesym)
            od
            if nvars=0 then serror("struct decl?") fi
        esac
    od

    ttfields[m]:=owner.deflist
    ttlength[m]:=owner.nfields
    ttlower[m]:=1
    ttcaligned[m]:=caligned
    ttbasetype[m]:=tpackrecord

    createusertype(owner, m)

    e:=owner.deflist
    while e do
        case e.nameid
        when structblockid, unionblockid, endblockid then
        else
            addgenfield(e)
        esac
        e:=e.nextdef
    od

    return m
end

proc addstructflag(symbol owner, int id)=
    static int structseqno
    array [32]char str

    fprint @str,"$$#",++structseqno

    addsymbol(owner, addglobalname(str),id, 0)
end

proc readprocdef(int isglobal)=
    int kwd,startline,closesym,nparams
    unit pcode
    symbol d, oldstcurrproc
    array [256]char str

    kwd:=lx.symbol
    lexchecksymbol(namesym)
    if stcurrproc.nameid=procid then
        serror("Nested proc")
    fi

    oldstcurrproc:=stcurrproc           
    stcurrproc:=d:=addsymbol(stcurrproc,lx.symptr,procid,isglobal)


    addproc(d)

    lex()

    if lx.symbol=ltsym then
        lexchecksymbol(stringconstsym)
        lex()
        case lx.symbol
        when gtsym then
            lex()
        when gesym then
            lx.symbol:=eqsym
        else
            serror(""">"" expected")
        esac
    fi

    readprocsig(d, kwd=kfunctionsym)

    checkequals()
    lex()

    startline:=lx.pos

    closesym:=checkbegin(0)

    d.code:=readsunit()

    if eqstring(d.name,"start") then
        currmodule.startfn:=d
    elsif eqstring(d.name,"main") then
        currmodule.mainfn:=d
    fi

    checkbeginend(closesym,kwd,startline)

    stcurrproc.misfunc:=kwd=kfunctionsym

    if ndocstrings then
        if fwritedocs then
            writedocs(stcurrproc, docstrings, ndocstrings)
        fi
        ndocstrings:=0
    fi

    stcurrproc:=oldstcurrproc
end

function readatfield:symbol=
    symbol p,d

    checksymbol(namesym)
    d:=lx.symptr
    lex()

    p:=stcurrproc.deflist
    while p do
        if eqstring(p.name,d.name) then
            return p
        fi

        p:=p.nextdef
    od
    serror_s("Can't find @ field",d.name)
    return nil
end

function istypestarter:int=
    case lx.symbol
    when stdtypesym, krefsym, kvarsym, kslicesym, lsqsym then
        return 1
    elsif lx.symbol=namesym then
        if nextlx.symbol=namesym then
            return 1
        fi
    esac
    return 0
end

proc readmacrodef(int isglobal)=

    symbol stmacro, stname, owner

    lexchecksymbol(namesym)

    stmacro:=addsymbol(stcurrproc, lx.symptr, macroid, isglobal)
    owner:=stmacro

    lex()

    if lx.symbol=lbracksym then         
        lex()
        if lx.symbol<>rbracksym then
            do
                case lx.symbol
                when namesym then
                    stname:=addsymbol(owner,lx.symptr,macroparamid,0)
                    stname.firstdupl:=lx.symptr

                    lex()
                    if lx.symbol=rbracksym then
                        exit
                    fi
                    checksymbol(commasym)
                    lex()
                else
                    serror("macro def params")
                esac
            od
        fi
        lex()
    fi

    checkequals()
    lex()
    stmacro.code:=readunit()
end

function readhostparams(unit lhs,int isfn)unit=
    int fnindex, nargs
    unit p,q

    fnindex:=lx.subcode
    lexchecksymbol(lbracksym)
    lex()

    pushlisttype('PARAM')

    q:=readslist(1,1,nargs)

    checksymbol(rbracksym)
    lex()

    if lhs then
        lhs^.nextunit:=q
        q:=lhs
    fi

    p:=createunit1(jcallhost,q)
    p^.index:=fnindex

    poplisttype()

    return p
end

proc pushlisttype(int ltype)=
    if nlisttype>=maxlisttype then
        serror("listtype overflow")
    fi
    listtypestack[++nlisttype]:=listtype
    listtype:=ltype
end

proc poplisttype=
IF NLISTTYPE<1 THEN SERROR("POPLT") FI
    listtype:=listtypestack[nlisttype--]
end

function readcompilervar:unit=
    array [100]char str
    rsystemtime tm
    static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

    case lx.subcode
    when jcvlineno then
        return createintunit(getlineno(sourcefiletext[currmodule.fileno], lx.sourceoffset))

    when jcvstrlineno then
        getstrint(getlineno(sourcefiletext[currmodule.fileno], lx.sourceoffset), str)
    when jcvdate then
        os_getsystime(&tm)

        fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"
    when jcvtime then
        os_getsystime(&tm)
        fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

    else
        serror("compiler var not impl")
    esac

    return createstringunit(pcm_copyheapstring(&.str))
end

function readmap:unit=
    ref unitrec p,a

    lexchecksymbol(lbracksym)
    lex()
    p:=readexpression()
    checksymbol(commasym)
    lex()
    a:=readexpression()

    if lx.symbol=commasym then
        lex()
        a.nextunit:=readexpression()
    fi
    checksymbol(rbracksym)
    lex()
    return createunit2(jmap,p,a)
end

function readcurlop:unit p=
    int opc

    lex()
    case lx.symbol
    when namesym then
        for i to pclnames.upb do
            if eqstring(lx.symptr.name,pclnames[i]+1) then
                opc:=i
                exit
            fi
        else
            serror_s("Not a pcl op:",lx.symptr.name)
        od
    when mathssym,  propsym then
        opc:=jpclcodes[lx.subcode]
    else
        serror("readcurlop")
    esac
    lex()
    p:=createunit0(joperator)
    p.pclopcode:=opc
    return p
end

global proc lexchecksymbol(int symbol)=
    lex()
    checksymbol(symbol)
end

proc readtypedef(int isglobal)=
    int ptype
    symbol d

    lexchecksymbol(namesym)
    d:=addsymbol(stcurrproc, lx.symptr, typeid, isglobal)

    lexchecksymbol(eqsym)
    lex()   

    if lx.symbol=krecordsym then
CPL "TYPE=REC"
        lex()
        d.nameid:=recordid
        readrecorddef(isglobal, d)
        return
    fi

    ptype:=readtypespec(owner:d)


    createusertype(d, ptype)

end

function readtypespec(int allowvar=0, symbol owner=nil)int=

    int flags, arraycode, oldipl
    int a,b,t, startline, closesym, caligned
    symbol d
    const maxdim=10
    array [maxdim]unit lowerdims,lengthdims
    int ndims
    unit x,lowerx, upperx, lengthx

    case lx.symbol
    when lsqsym then
dolsq::
        lex()
        ndims:=0
        pushlisttype(0)
        do
            lowerx:=lengthx:=nil
            if lx.symbol=rsqsym or lx.symbol=commasym then      
            else
                x:=readunit()
                if x.tag=jmakerange then            
                    lowerx:=x.a
                    upperx:=x.b
                    if lowerx.tag=jintconst and upperx.tag=jintconst then
                        lengthx:=createintunit(upperx.value-lowerx.value+1)
                    else
                        lengthx:=createunit2(jsub,upperx,lowerx)
                        lengthx:=createunit2(jadd,lengthx,createintunit(1))
                    fi
                else
                    case lx.symbol
                    when rsqsym,commasym then           
                        lengthx:=x
                    when colonsym then              
                        lowerx:=x
                        lex()
                        if not (lx.symbol=commasym or lx.symbol=rsqsym) then
                            lengthx:=readunit()
                        fi
                    esac
                fi
            fi
            lowerdims[++ndims]:=lowerx
            lengthdims[ndims]:=lengthx
            exit when lx.symbol<>commasym
            lex()
        od
        checksymbol(rsqsym)
        lex()
        poplisttype()
        t:=readtypespec()

        for i:=ndims downto 1 do
            t:=makeaxtype(t,lowerdims[i],lengthdims[i])
        od
        return t

    when krefsym then
        lex()

        if lx.symbol=stdtypesym and lx.subcode=tvoid then
            lex()
            return makereftype(tvoid,owner)
        else
            return makereftype(readtypespec(),owner)
        fi

    when namesym then
        d:=lx.symptr
        lex()
        if lx.symbol=dotsym then
            lexchecksymbol(namesym)
            t:=newusertypex(d,lx.symptr)
            lex()
            return t
        else
            return newusertypex(d)
        fi

    when stdtypesym then
        case lx.subcode
        when tpackstrz then             
            lex()
            if lx.symbol=mulsym then
                lex()
                return makestrtype(tpackstrz, readunit())
            else
                return tstringz
            fi

        when tpackstrc then
            lexchecksymbol(mulsym)
            lex()
            return makestrtype(tpackstrc,readunit())

        when tarray then
            lexchecksymbol(lsqsym)
            goto dolsq

        else
            t:=lx.subcode
            lex()
            return t
        esac

    when krecordsym then
        if owner=nil then serror("anon record") fi
        lex()
        closesym:=checkbegin(1)
        startline:=lx.pos
        t:=readrecordbody(owner)

        checkbeginend(closesym,krecordsym,startline)
        return t

    when kstructsym then
        if owner=nil then serror("anon struct") fi
        lex()
        caligned:=0
        if lx.symbol=kcalignedsym then
            caligned:=1
            lex()
        fi

        closesym:=checkbegin(1)
        startline:=lx.pos
        t:=readstructbody(owner,caligned)

        checkbeginend(closesym,kstructsym,startline)
        return t

    when kvarsym then
        if not allowvar then
            serror("var types not allowed")
        fi
        lex()
        if lx.symbol=colonsym then
            lex()
            return readtypespec(0)
        fi
        return tvar
    when kslicesym then
        lexchecksymbol(lsqsym)
        lexchecksymbol(rsqsym)
        lex()
        t:=makeslicetype(readtypespec(0))

    else
        serror("Type expected")
    esac

    return t
end

proc readreturntype(symbol stproc)=
    stproc.mode:=readtypespec(stproc.nameid=procid)

end

proc readprocsig(symbol stproc, int isfunc)=
    int isbyref,isoptional, retmode
    symbol d

    stproc.mode:=tvoid

    if lx.symbol=lbracksym then     
        lex()
        if lx.symbol<>rbracksym then
            readparams(stproc)
        else
            lex()
        fi

        if lx.symbol=colonsym or lx.symbol=sendtosym then
            lex()
            readreturntype(stproc)
        elsif istypestarter() then
            readreturntype(stproc)
        fi
    elsif lx.symbol=colonsym or lx.symbol=sendtosym then
        lex()
        readreturntype(stproc)
    fi

    if isfunc and stproc.mode=tvoid then
        if stproc.nameid=procid then
            stproc.mode:=tvar
        else
            serror("MFunc needs return type")
        fi
    elsif not isfunc and stproc.mode<>tvoid then
        serror("Ret type given to sub/proc")
    fi
end

proc readparams(symbol stproc, int pmode=tvoid)=
    int isbyref:=0,isoptional:=0
    symbol d

    if pmode<>tvoid then                    
        goto gotmode
    fi

    if not istypestarter() then
        readparams_names(stproc)
        return
    fi
    if lx.symbol=rbracksym then
        lex()
        return
    fi

    do                                      
        isbyref:=isoptional:=0

        if istypestarter() then             
            pmode:=readtypespec(stproc.nameid<>dllprocid)
        elsif pmode=tvoid then
            serror("Type expected")
        fi

gotmode::
        case lx.symbol
        when addrsym then
            isbyref:=1
            lex()
        when questionsym then
            isoptional:=1
            lex()
        when ellipsissym then
            stproc.mvarparams:=1
            lexchecksymbol(rbracksym)
            lex()
            return
        esac

        checksymbol(namesym)
        ++stproc.nparams
        d:=addsymbol(stproc, lx.symptr, paramid,0)
        storemode(stproc,pmode,&d.mode)
        lex()

        if lx.symbol=eqsym then
            isoptional:=1
            lex()
            d.code:=readunit()
        fi

        if isbyref and isoptional then serror("Mixed byref/optional") fi

        d.mbyref:=isbyref
        d.moptional:=isoptional

        case lx.symbol
        when commasym then
            lex()
        else
            exit
        esac
    od

    checksymbol(rbracksym)
    lex()

end

proc readparams_names(symbol stproc)=
    int isbyref,isoptional
    symbol d

    isbyref:=isoptional:=0

    do
        if lx.symbol=addrsym then
            ++isbyref
            lex()
        fi
        if lx.symbol=questionsym then
            ++isoptional
            lex()
        fi
        checksymbol(namesym)
        d:=addsymbol(stproc, lx.symptr, paramid,0)
        d.mode:=tvar
        ++stproc.nparams

        lex()

        if lx.symbol=eqsym then
            isoptional:=1
            lex()
            d.code:=readunit()
        fi

        if isbyref and isoptional then serror("Mixed byref/optional") fi

        d.mbyref:=isbyref
        d.moptional:=isoptional

        isbyref:=isoptional:=0

        if lx.symbol=commasym then
            lex()
            if lx.symbol=ellipsissym then
                stproc.mvarparams:=1
                lex()
                exit
            fi
        else
            exit
        fi
    od

    checksymbol(rbracksym)
    lex()
end
=== qq_print.m 0 0 20/43 ===
global  int mindev      
global  int moutdev
global  ref int minchan     
global  filehandle moutchan
global  varrec minvar       
global  varrec moutvar      

global const std_io = 0     
global const file_io    = 1     
global const str_io = 2     
global const wind_io    = 3     
global const istr_io    = 4     

const maxoclevel=6
array [0:maxoclevel]int32           moutdevstack
array [0:maxoclevel]filehandle  moutchanstack
array [0:maxoclevel]varrec      moutvarstack
array [0:maxoclevel]byte            mgapstack
array [0:maxoclevel]ref char        mfmtstrstack
array [0:maxoclevel]ref char        mfmtcurrstack
int noclevels

global record fmtrec=
    byte    minwidth    
    i8  precision   
    byte    base        

    char    quotechar   
    char    padchar     
    char    realfmt     

    char    plus        
    char    sepchar     
    char    lettercase  
    char    justify     
    char    suffix      
    char    usigned     
    char    charmode    
    char    showtype    
    char    newline     

    array [1]byte   spare
end

const maxstrlen=256
const comma=','
const onesixty=1024

global  ichar mfmtstr       
global  ichar mfmtcurr  
global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,(0,))
byte mgapneeded

const minkb_size=1048576        
ref char kb_start           
ref char kb_pos             
ref char kb_lastpos         
int kb_size                 
int kb_linelength           
int kb_length               
                            
int kb_lastlength           
char termchar               
int itemerror               

global filehandle testfilech    

const maxlistdepth=4
int listdepth=0             

array [0:]char digits=a"0123456789ABCDEF"

global proc pch_print(variant p, fmt=nil)=
    varrec v
    variant q
    ref char s
    varrec emptyfmt


    if fmt=nil then
        fmt:=&emptyfmt
        emptyfmt.tagx:=tvoid
    fi

    if mfmtstr=nil then
        if mgapneeded then
            printstr_n(" ",1)
        else
            mgapneeded:=1
        fi
    else
        printnextfmtchars(0)
    fi

    listdepth:=0

    pch_tostr(p,fmt,&v)
    printstr_n(v.objptr.strptr,v.objptr.length)
    var_unshare(&v)
end

global proc pch_print_nf(variant p)=
    pch_print(p, nil)
end

global proc pch_printnogap=
    mgapneeded:=0
end

global proc pch_println=
    if mfmtstr then
        printnextfmtchars(1)
    fi
    mgapneeded:=0
    printstr_n("\n",-1)
end

global proc pch_reread =
    kb_pos:=kb_lastpos
    kb_length:=kb_lastlength
end

global proc pch_rereadln =
    kb_pos:=kb_start
    kb_length:=kb_linelength
end

proc pch_startprint(variant p)=
    object s

    switch ++noclevels
    when 0, 1 then      

    when maxoclevel+1 then      
        prterror("print #x overflow")
    else
        moutdevstack[noclevels-1]:=moutdev
        moutchanstack[noclevels-1]:=cast(moutchan)
        moutvarstack[noclevels-1]:=moutvar
        mfmtstrstack[noclevels-1]:=mfmtstr
        mfmtcurrstack[noclevels-1]:=mfmtcurr
        mgapstack[noclevels-1]:=mgapneeded
    endswitch

    mfmtstr:=nil
    mfmtcurr:=nil

    if p=nil then
        goto doconsole
    fi
    switch p.tag
    when tint then
        switch p.value
        when 0 then
    doconsole::
            moutdev:=std_io
            moutchan:=nil
        
        when 1 then         
            moutdev:=str_io
            moutchan:=nil
            moutvar.tagx:=tstring ior hasrefmask

            s:=obj_new()
            s.mutable:=1
            moutvar.objptr:=s

        when 2 then
            if testfilech=nil then
                prterror("@2: file not open")
            fi
            moutdev:=file_io
            moutchan:=testfilech
        
        else
            moutdev:=file_io
            moutchan:=cast(filehandle(p.value))
        endswitch

    when trefvar then
        p:=p.varptr
        switch p.tag
        when tstring then
            moutdev:=istr_io
            moutchan:=nil
            moutvar.tagx:=trefvar
            moutvar.varptr:=p
        
        else
        PRINTLN ttname[p.tag]
            prterror("Print@^?")
        endswitch

    else
        switch p.tag
        when trecord, tstruct then      
            moutdev:=std_io
        else
        PRINTLN ttname[p.tag]
            prterror("Can't do startprint...")
        endswitch
    endswitch

    mgapneeded:=0
end

global proc pch_startprintcon=
    varrec v

    v.tagx:=tint
    v.value:=0
    pch_startprint(&v)
end

global proc pch_endprint=
    variant p

    if mfmtstr then
        printnextfmtchars(1)
    fi
    switch moutdev
    when istr_io then
        p:=moutvar.varptr
    endswitch

    if mfmtstr<>nil then
        pcm_free(mfmtstr,strlen(mfmtstr)+1)
    fi

    if --noclevels=-1 then
        prterror("resetoc??")
    fi

    if noclevels=0 then
        moutdev:=std_io

    else            
        moutdev:=moutdevstack[noclevels]
        moutchan:=cast(moutchanstack[noclevels])
        moutvar:=moutvarstack[noclevels]
        mgapneeded:=mgapstack[noclevels]
        mfmtstr:=mfmtstrstack[noclevels]
        mfmtcurr:=mfmtcurrstack[noclevels]
    fi
    mgapneeded:=0
end

global proc pch_strstartprint =
    varrec p

    p.tagx:=tint
    p.value:=1
    pch_startprint(&p)      
end

global proc pch_strendprint(variant dest) =
    if mfmtstr then
        printnextfmtchars(1)
    fi
    if moutdev<>str_io then
        prterror("STRENDPRT/NOT STR")
    fi

    dest^:=moutvar                      
    moutvar.tagx:=tvoid

    pch_endprint()
end

global proc pch_printspace=
    mgapneeded:=0
    print " "
end

global proc pch_readln(variant dev) =
    filehandle ch
    int length
    object pdev

    if kb_start=nil then
        kb_start:=pcm_alloc(minkb_size)
        kb_size:=minkb_size
        kb_lastpos:=kb_start
        kb_pos:=kb_start
        kb_length:=0
        kb_lastlength:=0
        kb_linelength:=0
    fi

    switch dev.tag
    when tvoid then
doconsole::
        readlinen(nil,kb_start,kb_size) 
        kb_length:=strlen(kb_start)

    when tint then
        switch dev.value
        when 0 then
            goto doconsole
        when 1 then
            if testfilech=nil then
                prterror("R@2: file not open")
            fi
            ch:=cast(testfilech)

        else
            ch:=filehandle(dev.value)
        endswitch
        pc_readlinen(cast(ch),kb_start,kb_size)         
        kb_length:=strlen(kb_start)

    when tstring then
        pdev:=dev.objptr
        length:=pdev.length
        if length=0 then
            kb_length:=0
            kb_start^:=0
        elsif length>=kb_size then
            prterror("KB overflow")
        else
            kb_length:=length
            memcpy(kb_start,pdev.strptr,length)
        fi
    else
        pcustype("readln@",dev)
    endswitch

    kb_pos:=kb_start
    kb_lastpos:=kb_pos
    kb_linelength:=kb_length
end

proc pc_readlinen(filehandle handlex,ref char buffer,int size) =
    ref char p
    int n,x
    array [0:100]char buff
    byte crseen
    type fntype=ref clang function (ichar,int32,filehandle)int
    int oldpos

    buffer^:=0

    fgets(buffer,size-2,handlex)

    n:=strlen(buffer)
    if n=0 then
        return
    fi

    p:=buffer+n-1       
    crseen:=0
    while (p>=buffer and (p^=13 or p^=10)) do
        if p^=13 or p^=10 then crseen:=1 fi
        p--^ :=0
    od


    if not crseen and (n+4>size) then
        cpl size,n
        abortprogram("line too long")
    fi
end

global proc pch_sread(variant fmt,variant dest) =
    int fmtcode
    char c

    fmtcode:=getreadfmtcode(fmt)
    kb_lastpos:=kb_pos
    kb_lastlength:=kb_length

    switch fmtcode
    when 'I' then
        stepkbpos(readint(kb_pos,kb_length,dest,0))

    when 'R' then
        stepkbpos(readreal(kb_pos,kb_length,dest))

    when 'N' then
        stepkbpos(readname(kb_pos,kb_length,dest))

    when 'S' then
        stepkbpos(readstring(kb_pos,kb_length,dest))

    when 'H' then
        stepkbpos(readhex(kb_pos,kb_length,dest))

    when 'B' then
        stepkbpos(readbin(kb_pos,kb_length,dest))

    when 'A' then
        stepkbpos(readany(kb_pos,kb_length,dest))

    when 'L' then
        if kb_length=0 then
            var_empty_string(dest)
        else
            var_make_stringn(kb_pos,kb_length,dest,1)
            kb_pos+:=kb_length
            kb_length:=0
        fi

    when 'C' then
        if kb_length=0 then
            var_empty_string(dest)
        else
            termchar:=kb_pos^
    dochar::
            dest.tagx:=tint
            dest.value:=termchar
            ++kb_pos
            --kb_length
        fi

    when 'Z' then           
        goto dochar
    when 'E' then
        dest.tagx:=tint
        dest.value:=itemerror
    when 'D' then
        stepkbpos(readint(kb_pos,kb_length,dest,1))

    else
        prterror("SREAD/FMT?")
    endswitch
end

global proc pch_sreadln(variant dev, variant dest) =
    pch_readln(dev)
    var_make_stringn(kb_start,kb_length,dest,mutable:1)
end

function readname(ref char s,int length,variant dest)ref char =
    ref char send
    ref char itemstr
    int itemlength
    send:=readitem(s,length,itemstr,itemlength)
    var_make_stringn(itemstr,itemlength,dest,1)

    iconvlcn(dest.objptr.strptr,dest.objptr.length)
    return send
end

function readstring(ref char s,int length,variant dest)ref char =
    ref char send
    ref char itemstr
    int itemlength
    send:=readitem(s,length,itemstr,itemlength)
    var_make_stringn(itemstr,itemlength,dest,1)
    return send
end

function readint(ref char sold,int length,variant dest, int dodec)ref char =
    ref char p,s                
    ref char send
    ref char itemstr
    int itemlength,numlength

    send:=readitem(sold,length,s,itemlength)

    strtoint(s,itemlength,dest,dodec)

    return send
end

function readhex(ref char sold,int length,variant dest)ref char =
    array [0:maxstrlen]char str     
    ref char p,s            
    byte res
    i64 aa
    int a,t,nalloc
    char c

    if length=0 then
        dest.tagx:=tint
        dest.value:=0
        termchar:=0
        return sold
    fi

    while (length and (sold^=' ' or sold^=9)) do
        ++sold; --length
    od

    if length<=maxstrlen then   
        s:=&.str
        nalloc:=0
    else
        nalloc:=length+1
        s:=pcm_alloc(nalloc)
    fi

    p:=s                
    while (length) do
        c:=toupper(sold^); ++sold; --length
        if c>='0' and c<='9' then
            p^:=c
            ++p
        elsif c>='A' and c<='F' then
            p^:=c
            ++p
        elsif c='_' then
        else
            termchar:=c
            exit
        fi
    od
    p^:=0               
    length:=p-s         

    if length<=16 then
        t:=tint
    else
        t:=tdecimal
    fi
    p:=s
    switch t
    when tint then
        aa:=0
        do
            c:=p^; ++p
            if c=0 then
                exit
            fi
            if c<'A' then           
                aa:=aa*16+c-'0'
            else                
                aa:=aa*16+(c-'A')+10
            fi
        od
        dest.tagx:=tint
        dest.value:=aa
    else
        prterror("Readhex/long")
    endswitch

    if nalloc then
        pcm_free(s,nalloc)
    fi

    return sold
end

function readbin(ref char sold,int length,variant dest)ref char =
    array [0:maxstrlen]char str     
    ref char p,s            
    byte res
    i64 aa
    int a,t,nalloc
    char c

    if length=0 then
        dest.tagx:=tint
        dest.value:=0
        termchar:=0
        return sold
    fi

    while (length and (sold^=' ' or sold^=9)) do
        ++sold; --length
    od

    if length<=maxstrlen then   
        s:=&.str
        nalloc:=0
    else
        nalloc:=length+1
        s:=pcm_alloc(nalloc)
    fi

    p:=s                
    while (length) do
        c:=toupper(sold^); ++sold; --length
        if c>='0' and c<='1' then
            p^:=c
            ++p
        elsif c='_' then
        else
            termchar:=c
            exit
        fi
    od
    p^:=0               
    length:=p-s         

    if length<=64 then
        t:=tint
    else
        t:=tdecimal
    fi

    p:=s
    switch t
    when tint then
        aa:=0
        do
            c:=p^; ++p
            if c=0 then
                exit
            fi
            aa:=aa*2+c-'0'
        od
        dest.tagx:=tint
        dest.value:=aa

    else
        prterror("Readbin/long")
    endswitch

    if nalloc then
        pcm_free(s,nalloc)
    fi
    return sold
end

function readreal(ref char sold,int length,variant dest)ref char =
    array [512]char str     
    real x
    ref char send
    ref char itemstr
    int itemlength,numlength

    send:=readitem(sold,length,itemstr,itemlength)
    strtoreal(itemstr,itemlength,dest)

    return send
end

global function getreadfmtcode(variant p)int =
    char c

    if p=nil or p.tag=tvoid then
        return 'A'
    fi
    if p.tag<>tstring then
    CPL "P=%s",ttname[p.tag]
        prterror("Readfmt?")
    fi
    if p.objptr.length=0 then
        return 'A'
    fi

    c:=toupper(p.objptr.strptr^)

    switch c
    when 'I', 'R', 'N', 'S', 'F', 'T', 'Z', 'C', 'L', 'H','B','A','E','D' then
        return c
    endswitch

    prterror("Readfmt2?")
    return 0
end

proc stepkbpos(ref char s) =
    int newlen

    newlen:=s-kb_pos

    if newlen=0 then        
        return
    fi
    if newlen>=kb_length then   
        kb_pos:=kb_pos+kb_length    
        kb_length:=0
    else
        kb_pos:=kb_pos+newlen
        kb_length-:=newlen
    fi
end

function readany(ref char sold,int length,variant dest)ref char =
    array [0:maxstrlen]char str         
    ref char p,s                
    byte signd,res
    i64 aa
    int digits,expon,other
    int t,nalloc
    char c

    ref char send
    ref char itemstr
    int itemlength,numlength

    itemerror:=0

    send:=readitem(sold,length,s,itemlength)


    p:=s
    digits:=expon:=other:=0

    to itemlength do
        switch p++^
        when '0'..'9','+','-','_' then digits:=1
        when 'E','e','.' then expon:=1
        else other:=1
        end
    od

    dest.tagx:=tint

    if other or itemlength=0 then
        dest.value:='STR'
        var_make_stringn(s,itemlength,dest,1)
    elsif expon then
        strtoreal(s,itemlength,dest)
    else
        strtoint(s,itemlength,dest,0)
    fi

    return send
end

function readitem(ref char s,int length,ref char &itemstr,int &itemlength)ref char =        
    ref char p
    char quotechar, c

    while (length and (s^=' ' or s^=9)) do
        ++s; --length
    od

    itemstr:=s              

    if length=0 then        
        termchar:=0
        itemlength:=0
        return s
    fi

    quotechar:=0            
    if s^='"' then
        quotechar:='"'
        ++s
        --length
    elsif s^='\'' then
        quotechar:='\''
        ++s
        --length
    fi

    p:=itemstr:=s

    while length do
        c:=s++^; --length
        switch c
        when ' ', 9, comma, '=' then        
            if quotechar or p=s then            
                goto normalchar
            fi
            termchar:=c
            exit
        else
    normalchar::
            if c=quotechar then
                if length and s^=quotechar then 
                    p^:=c
                    ++s
                    ++p
                else                    
                    termchar:=s^
                    if termchar=',' or termchar='=' then
                        ++s
                        termchar:=s^
                    fi
                    exit
                fi
            else
                p^:=c
                ++p
            fi
        endswitch
    od

    if length=0 then
        termchar:=0
    fi
    itemlength:=p-itemstr               

    return s
end

proc strtoreal(ichar s,int length,variant dest)=
    array [512]char str     
    real x
    int32 numlength

    dest.tagx:=treal

    if length>=str.bytes or length=0 then       
        dest.xvalue:=0.0
        return
    fi
    memcpy(&.str,s,length)
    str[length+1]:=0

    itemerror:=0

    if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
        if numlength=length then x:=0.0 fi
        itemerror:=1
    fi

    dest.xvalue:=x
end

proc strtoint(ichar s,int length, variant dest, int dodec)=
    array [0:maxstrlen]char str         
    ref char p,q
    byte signd
    i64 aa
    int a,res,cat
    int t,nalloc
    char c

    itemerror:=0

    if length=0 then
        dest.tagx:=tint
        dest.value:=0
        return
    fi

    signd:=0
    if length and s^='-' then
        signd:=1; ++s; --length
    elsif length and s^='+' then
        ++s; --length
    fi

    while s^='0' and length>1 do
        ++s; --length
    od

    p:=q:=s             

    while length do
        c:=q++^
        --length
        if c>='0' and c<='9' then
            p^:=c
            ++p
        else
            if c='_' then
            else
                itemerror:=1
                exit
            fi
        fi
    od
    p^:=0               
    length:=p-s         


    if length<=18 then
        cat:='A'
    elsif length=19 then
        case cmpstring(s,"9223372036854775808")
        when -1 then cat:='A'
        when 0 then cat:='B'
        else cat:='C'
        esac
    elsif length=20 then
        if cmpstring(s,"18446744073709551615")<=0 then
            cat:='C'
        else
            cat:='D'
        fi
    else
        cat:='D'
    fi

    if dodec then cat:='D' fi

    if signd then
        case cat
        when 'B' then cat:='A'      
        when 'C' then cat:='D'      
        esac
    fi


    case cat
    when 'A' then t:=tint
    when 'B','C' then t:=tword
    else t:=tdecimal
    esac

    p:=s
    if t<>tdecimal then
        aa:=0
        do
            c:=p^; ++p
            if c=0 then
                exit
            fi
            aa:=aa*10+(c-'0')
        od
        if signd then
            aa:=-aa
        fi
        dest.tagx:=t
        dest.value:=aa

    else
        var_make_dec_str(s,length,dest)
    fi
end

proc printnextfmtchars(int lastx) =
    char c
    ref char pstart
    int n

    pstart:=mfmtcurr
    n:=0

    do
        c:=mfmtcurr^
        switch c
        when '#' then
            if lastx then
                goto skip
            fi
            ++mfmtcurr
            if n then
                printstr_n(pstart,n)
            fi
            return
        when 0 then

            if n then
                printstr_n(pstart,n)
            elsif not lastx then
                printstr_n("|",1)
            fi
            return
        when '~' then
            if n then
                printstr_n(pstart,n)
                n:=0
            fi
            ++mfmtcurr
            c:=mfmtcurr^
            if c then
                ++mfmtcurr
                printstr_n(&c,1)
            fi
            pstart:=mfmtcurr
        else
skip::
            ++n
            ++mfmtcurr
        endswitch
    od
end

proc pch_setformat(variant p) =
    int n
    ref char s

    if p.tag<>tstring then
        prterror("(str)")
    fi
    if mfmtstr then
        prterror("Setfmt?")
    fi
    n:=p.objptr.length
    mfmtstr:=pcm_alloc(n+1)
    if n then
        memcpy(mfmtstr,p.objptr.strptr,n)
    fi
    s:=mfmtstr+n
    s^:=0

    mfmtcurr:=mfmtstr
end

global function pc_getfmt(variant p,ref fmtrec fmt)ref fmtrec=

if p=nil or p.tag=tvoid then
    return &defaultfmt
else
    if p.tag<>tstring then
        prterror("pc_getfmt/not str?")
    fi
    if p.objptr.strptr=nil then
        return &defaultfmt
    else
        pc_strtofmt(p.objptr.strptr,p.objptr.length,fmt)
        return fmt
    fi
fi
end

global proc pc_strtofmt(ref char s,int slen,ref fmtrec fmt) =

    int c
    byte wset
    int n
    array [0:100]char str

    initfmtcode(fmt)

    memcpy(&.str,s,slen)        
    str[slen]:=0
    s:=&.str

    wset:=0
    while s^ do
        c:=s^
        ++s
        switch c
        when 'B', 'b' then fmt.base:=2
        when 'H', 'h' then fmt.base:=16
        when 'O', 'o' then fmt.base:=8
        when 'X', 'x' then
            c:=s^
            if c then
                switch c
                when '2'..'9' then c:=c-'0'
                when '1' then
                    ++s
                    c:=s^
                    if c in '0'..'6' then
                        c:=c-'0'+10
                    fi
                else
                    c:=10
                end
                fmt.base:=c
                ++s
            fi

        when 'Q', 'q' then fmt.quotechar:='"'
        when '~' then fmt.quotechar:='~'
        when 'J', 'j' then
            fmt.justify:=toupper(s^)
            if s^ then
                ++s
            fi
        when 'A' then fmt.lettercase:='A'
        when 'a' then fmt.lettercase:='a'
        when 'Z', 'z' then fmt.padchar:='0'
        when 'S', 's' then
            fmt.sepchar:=s^
            if s^ then
                ++s
            fi
        when 'P', 'p' then
            fmt.padchar:=s^
            if s^ then
                ++s
            fi
        when 'T', 't' then
            fmt.suffix:=s^
            if s^ then
                ++s
            fi
        when 'U', 'u' then fmt.usigned:='U'
        when 'W', 'w' then fmt.usigned:='U'
        when 'E', 'e' then fmt.realfmt:='e'
        when 'F', 'f' then fmt.realfmt:='f'
        when 'G', 'g' then fmt.realfmt:='g'
        when '.' then
            wset:=1
        when comma,'_' then fmt.sepchar:=c
        when '+' then fmt.plus:='+'
        when 'M', 'm' then fmt.charmode:='M'
        when 'N', 'n' then fmt.newline:=1
        when 'C', 'c' then fmt.charmode:='C'
        when 'Y', 'y' then fmt.showtype:='Y'
        else
            if c>='0' and c<='9' then
                n:=c-'0'
                do
                    c:=s^
                    if s^=0 then
                        exit
                    fi
                    if c>='0' and c<='9' then
                        ++s
                        n:=n*10+c-'0'
                    else
                        exit
                    fi
                od
                if not wset then
                    fmt.minwidth:=min(n,onesixty-1)
                    wset:=1
                else
                    fmt.precision:=min(n,100)
                fi
            fi
        endswitch
    od
end

proc initfmtcode(ref fmtrec f) =
    f^:=defaultfmt
end

function i64mintostr(ref char s,int base,int sep)int =
    array [0:onesixty]char t
    int i,j,k,g,neg

    switch base
    when 10 then
        strcpy(&t[0],"9223372036854775808")
        j:=3
    when 16 then
        strcpy(&t[0],"8000000000000000")
        j:=1
    when 2 then
        strcpy(&t[0],"1000000000000000000000000000000000000000000000000000000000000000")
        j:=7
    else
        strcpy(&t[0],"<mindint>")
    endswitch

    i:=strlen(&t[0])
    s+:=i
    if sep then
        s+:=j
    fi
    s^:=0

    k:=0
    g:=(base=10|3|4)

    while i do
        --s
        s^:=t[i-- -1]
        if sep and i and ++k=g then
            --s
            s^:=sep
            k:=0
        fi
    od
    return strlen(s)
end

function u64tostr(u64 aa,ref char s,word base,int sep)int =
    array [0:onesixty]char t
    int i,j,k,g
    int dummy
    ref char s0

    i:=0
    k:=0
    g:=(base=10|3|4)

    repeat
        t[++i]:=digits[word(aa rem base)]
        aa:=aa/base
        if sep and aa<>0 and ++k=g then
            t[++i]:=sep
            k:=0
        fi
    until aa=0

    j:=i
    s0:=s
    while i do
        s^:=t[i--]
        ++s
    od
    s^:=0

    return j
end

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt,int usigned)int =
    array [0:onesixty]char str              
    int i,j,k,n,w
    static u64 mindint=0x8000'0000'0000'0000

    if fmt.usigned then
        usigned:=1
    fi

    if aa=mindint and not usigned then      

        str[0]:='-'
        n:=i64mintostr(&str[1],fmt.base,fmt.sepchar)+1
    else
        if (not usigned and aa<-0) or fmt.plus then
            if aa<0 then
                aa:=-aa
                str[0]:='-'
            else
                str[0]:='+'
            fi
            n:=u64tostr(aa,&str[1],fmt.base,fmt.sepchar)+1
        else
            n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)
        fi

    fi

    if fmt.suffix then
        str[n]:=fmt.suffix
        str[++n]:=0
    fi

    if fmt.base>10 or fmt.suffix and fmt.lettercase='a' then    
        convlcstring(&.str)
    fi

    return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
    array [0:onesixty]char str              
    int i,j,k,n,w

    n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)

    if fmt.suffix then
        str[n]:=fmt.suffix
        str[++n]:=0
    fi

    if fmt.base>10 or fmt.suffix and fmt.lettercase='a' then    
        convlcstring(&.str)
    fi

    return expandstr(&.str,s,n,fmt)
end

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
    ref char u,v
    array [256]char str
    int w,nheap     

    nheap:=0

    if fmt.quotechar or fmt.lettercase then     
        if n<256 then
            u:=&.str
        else
            nheap:=n+3                  
            u:=pcm_alloc(nheap)
        fi
        if fmt.quotechar then
            v:=u
            v^:=fmt.quotechar
            ++v
            if n then
                strcpy(v,s)
                v+:=n
            fi
            v^:=fmt.quotechar
            ++v
            v^:=0
            n+:=2
        else
            memcpy(u,s,n)
        fi
        switch fmt.lettercase
        when 'a' then   
            convlcstring(u)
        when 'A' then
            convucstring(u)
        endswitch
        s:=u
    fi

    w:=fmt.minwidth
    if w>n then
        n:=expandstr(s,t,n,fmt)
    else
        memcpy(t,s,n)
    fi
    if nheap then
        pcm_free(u,nheap)
    fi
    return n
end

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =

    int i,w,m

    w:=fmt.minwidth
    if w=0 or w<=n then     
        strncpy(t,s,n)
        (t+n)^:=0
        return n
    fi

    if fmt.justify='L' then 
        strncpy(t,s,n)
        t+:=n
        for i:=1 to w-n do
            t^:=fmt.padchar
            ++t
        od
        t^:=0
    elsif fmt.justify='R' then
        if fmt.padchar='0' and fmt.base and (s^='-' or s^='+') then 
            t^:=s^
            ++t
            to w-n do
                t^:=fmt.padchar
                ++t
            od
            strncpy(t,s+1,n-1)
            (t+n-1)^:=0
        else
            to w-n do
                t^:=fmt.padchar
                ++t
            od
            strncpy(t,s,n)
            (t+n)^:=0
        fi

    else                

        m:=(w-n+1)/2
        to m do
            t^:=fmt.padchar
            ++t
        od
        strncpy(t,s,n)
        t+:=n
        to w-n-m do
            t^:=fmt.padchar
            ++t
        od
        t^:=0

    fi
    return w
end

global proc addstring(object p,ref char t,int n=-1) =
    int oldlen,newlen,oldbytes,newbytes
    ref char newptr

    if n=0 or t^=0 then
        return
    fi
    if n<0 then
        n:=strlen(t)
    fi

    oldlen:=p.length

    if p.refcount=0 then    
        if oldlen=0 then        
            memcpy(p.strptr,t,n)
            p.length:=n
        else                
            memcpy(p.strptr+oldlen,t,n)
            p.length:=oldlen+n
        fi
        return
    fi

    if oldlen=0 then        
        p.strptr:=pcm_alloc(n)
        p.length:=n
        p.alloc64:=allocbytes
        memcpy(p.strptr,t,n)

    else                
        newlen:=oldlen+n
        oldbytes:=p.alloc64
        newbytes:=oldlen+n
        if newbytes<=oldbytes then      
            memcpy(p.strptr+oldlen,t,n)
        else                    
            newptr:=pcm_alloc(newbytes)
            memcpy(newptr,p.strptr,oldlen)  
            memcpy(newptr+oldlen,t,n)       
            p.alloc64:=allocbytes
            pcm_free(p.strptr,oldbytes)
            p.strptr:=newptr
        fi
        p.length:=newlen
    fi
end

proc domultichar (ref char p,int n,ref char dest,ref fmtrec fmt) =
    array [0:20]char str
    ref char q
    int i,nchars

    q:=&.str

    nchars:=n

    to n do
        if p^=0 then exit fi
        q^:=p^
        ++q
        ++p
    od
    q^:=0

    expandstr(str,dest,nchars,fmt)
end

proc printstr_n(ref char s,int n) =
    variant  p
    int x
    type fntype= ref clang function (filehandle f, ichar s, int i, ichar t)int

    if n=-1 then        
        n:=strlen(s)
    fi

    if n=0 then
        return
    fi

    switch moutdev
    when std_io then
        printstrn_app(s,n,nil)

    when file_io then
        printstrn_app(s,n,cast(moutchan))

    when str_io then
        addstring(moutvar.objptr,s,n)

    when istr_io then
        p:=moutvar.varptr
        if p.tag<>tstring then
            prterror("prtstrn1")
        fi
        addstring(moutvar.objptr,s,n)

    when wind_io then
        
    endswitch
end

global proc pch_strtoval(variant p,variant fmt,variant dest) =
    int fmtcode,length
    byte oldmutable
    object q
    array [1024]char str
    ref char s:=&.str

    q:=p.objptr

    if q.length<str.len then
        memcpy(s,q.strptr,q.length)
        str[q.length+1]:=0
    else
        pcerror("STRTOVAL/string too long")
    fi

    fmtcode:=getreadfmtcode(fmt)
    if p.tag<>tstring then
        prterror("strval")
    fi
    length:=p.objptr.length

    switch fmtcode
    when 'I' then
        readint(s,length,dest,0)
    when 'D' then
        readint(s,length,dest,1)
    when 'R' then
        readreal(s,length,dest)
    when 'N' then
        readname(s,length,dest)
    when 'S' then
        readstring(s,length,dest)
    when 'H' then
        readhex(s,length,dest)
    when 'B' then
        readbin(s,length,dest)
    when 'A' then
        readany(s,length,dest)
    else
        prterror("strval:fmt?")
    endswitch
end

proc tostr_int(variant p,ref fmtrec fmt,object dest) =
    array [0:onesixty]char str

    switch fmt.charmode
    when 'M' then
        domultichar(ref char(&p.value),8,str,fmt)

    when 'C' then
        str[1]:=p.value
        str[2]:=0

    else
        i64tostrfmt(p.value,str,fmt,0)
    endswitch

    if fmt.showtype then
        addstring(dest,"I:",2)
    fi

    addstring(dest,str,strlen(str))
end

proc tostr_word(variant p, ref fmtrec fmt, object dest) =
    array [0:onesixty]char str

    switch fmt^.charmode
    when 'M' then
        domultichar(ref char(&p.uvalue),8,str,fmt)

    when 'C' then
        str[1]:=p.uvalue
        str[2]:=0

    else
        u64tostrfmt(p.value,str,fmt)
    endswitch

    if fmt.showtype then
        addstring(dest,"W:",2)
    fi

    addstring(dest,str,strlen(str))
end

proc tostr_real(variant p, ref fmtrec fmt, object dest) =
    array [0:onesixty]char str,str2
    array [0:10]char cfmt
    int n

    cfmt[0]:='%'

    if fmt.precision then
        cfmt[1]:='.'
        cfmt[2]:='*'
        cfmt[3]:=fmt.realfmt
        cfmt[4]:=0
        sprintf(str,cfmt,fmt.precision,p.xvalue)
    else
        cfmt[1]:=fmt.realfmt
        cfmt[2]:=0
        sprintf(str,cfmt,p.xvalue)
    fi

    n:=strlen(str)      
    if n<fmt.minwidth then
        expandstr(str,str2,n,fmt)
        strcpy(str,str2)
    fi

    addstring(dest,str,strlen(str))
end

proc tostr_str(variant p, ref fmtrec fmt, object dest) =
    int oldlen,newlen
    ref char s
    array [0:100]char str
    object q

    q:=p.objptr
    oldlen:=q.length
    newlen:=oldlen

    if fmt.quotechar or fmt.minwidth>newlen then
        if fmt.quotechar then
            newlen+:=2
        fi
        if fmt.minwidth>newlen then
            newlen:=fmt.minwidth
        fi
        s:=pcm_alloc(newlen+1)
        strtostrfmt(q.strptr,s,oldlen,fmt)
        addstring(dest,s,newlen)
        pcm_free(s,newlen+1)
    else
        addstring(dest,q.strptr,oldlen)
    fi
end

proc pch_tostr(variant a, b, result)=
    fmtrec fmt
    ref fmtrec ifmt
    object p

    ifmt:=pc_getfmt(b,&fmt)

    p:=obj_new_string(0)

    listdepth:=0

    tostr(a,ifmt,p)

    result.tagx:=tstring ior hasrefmask
    result.objptr:=p
end

proc tostr_list(variant p, ref fmtrec fmt, object dest) =
    variant q
    int i,n
    char c
    object r

    ++listdepth

    r:=p.objptr
    if r.refcount<0 or listdepth>maxlistdepth then
        addstring(dest,"...",3)
        --listdepth
        return
    fi


    r.refcount:=-r.refcount
    q:=r.varptr

        n:=p.objptr.length

    if fmt.newline then
        to n do
            tostr(q,fmt,dest)
            addstring(dest,"\n",-1)
            ++q
        od

    else
        addstring(dest,"(",1)
        for i:=n downto 1 do
            tostr(q,fmt,dest)
            ++q
            if i<>1 then
                addstring(dest,",",1)
            fi
        od
        addstring(dest,")",1)
    fi
    r.refcount:=-r.refcount
    --listdepth
end

proc tostr_range(variant p, ref fmtrec fmt, object dest) =
    array [0:onesixty]char str

    i64tostrfmt(p^.range_lower,str,fmt,0)
    strcat(str,"..")
    addstring(dest,str)
    i64tostrfmt(p^.range_upper,str,fmt,0)
    addstring(dest,str)
end

proc tostr_array(variant p, ref fmtrec fmt, object dest) =
    array [0:onesixty]char str
    ref byte q
    int i,m,elemtype,a,b,lower, length
    varrec v
    object pa
    ref byte ptr

    m:=p.tag
    pa:=p.objptr

    if p.tag=tarray then
        length:=pa.length
        lower:=pa.lower
        elemtype:=pa.elemtag
    else
        length:=ttlength[pa.usertag]
        lower:=ttlower[pa.usertag]
        elemtype:=tttarget[pa.usertag]
    fi

    a:=lower
    b:=length+lower-1

    q:=pa.ptr

    fprint @str,"#[#:#]",ttname[m],lower,ttname[elemtype]
    addstring(dest,str)
    addstring(dest,"A(")

    for i:=a to b do

        var_loadpacked(q,elemtype,&v,nil)
        q+:=ttsize[elemtype]
        tostr(&v,fmt,dest)
        if i<b then
            addstring(dest,",",1)
        fi
    od
    addstring(dest,")",1)
end

proc tostr_bits(variant p, ref fmtrec fmt, object dest) =
    array [0:onesixty]char str
    ref byte q
    int i,m,elemtype,a,b,bitwidthx,offset
    varrec v
    object pa
    ref byte ptr

    m:=p.tag
    pa:=p.objptr

    a:=pa.lower16
    elemtype:=pa.elemtag
    b:=pa.length+a-1
    bitwidthx:=ttbitwidth[elemtype]
    offset:=pa.indexoffset*bitwidthx

    q:=pa.ptr

    fprint @str,"#[#:#]",ttname[m],pa.lower16,ttname[elemtype]
    addstring(dest,str)
    addstring(dest,"A(")

    for i:=a to b do
        var_loadbit(q,offset,elemtype,0,&v)
        offset+:=bitwidthx
        if offset>=8 then
            offset:=0
            ++q
        fi
        tostr(&v,fmt,dest)
        if i<b then
            addstring(dest,",",1)
        fi
    od
    addstring(dest,")",1)
end

proc tostr_struct(variant p, ref fmtrec fmt, object dest) =
    ref byte q
    int i,m,nfields,needcomma
    varrec v
    object pa
    ref byte ptr
    symbol d
    ref symbol r

    pa:=p.objptr
    m:=pa.usertag

    d:=ttnamedef[m]

    r:=d.topfieldlist
    nfields:=ttlength[m]

    needcomma:=0
    addstring(dest,"(")

    for i to nfields do
        var_loadpacked(pa.ptr+r.fieldoffset, r.mode, &v, nil)
        if needcomma then
            addstring(dest,",")
        fi
        needcomma:=1

        tostr(&v,fmt,dest)
        ++r
    od
    addstring(dest,")")
end

proc tostr_set(variant p,ref fmtrec fmt,object dest) =
    array [0:onesixty]char str
    variant q
    int i,j,first
    varrec v
    object s

    if fmt=nil then
        fmt:=&defaultfmt
    fi

    addstring(dest,"[",1)

    s:=p.objptr

    first:=1

    i:=0
    while (i<s.length) do
        if testelem(cast(s.ptr),i) then 
            j:=i+1              
            while (j<s.length and testelem(cast(s.ptr),j)) do
                ++j
            od
            --j             
            if not first then
                addstring(dest,",",1)
            fi
            first:=0
            if i=j then
                v.tagx:=tint
                v.value:=i
            else
                v.tagx:=trange
                v.range_lower:=i
                v.range_upper:=j
            fi
            tostr(&v,fmt,dest)
            i:=j+1
        else
            ++i
        fi
    od
    addstring(dest,"]",1)
end

proc tostr_dict(variant p,ref fmtrec fmt,object dest) =
    array [0:onesixty]char str
    variant q
    int i,length,needcomma:=0
    object pa

    if fmt=nil then
        fmt:=&defaultfmt
    fi
    addstring(dest,"[",-1)

    pa:=p.objptr
    q:=pa.varptr        

    length:=pa.length/2             

    for i:=length downto 1 do
        if q.tag=tvoid then
            q+:=2
            next
        fi
        if needcomma then
            addstring(dest,",",1)
        fi
        needcomma:=1
        tostr(q,fmt,dest)
        q++
        addstring(dest,":",1)
        tostr(q,fmt,dest)
        q++
    od
    addstring(dest,"]",1)
end

proc tostr_decimal(variant p,ref fmtrec fmt,object dest) =
    ref char s

    s:=var_tostr_dec(p,0)
    addstring(dest,s,-1)
    pcm_free(s,decstrsize)
end

proc tostr(variant p, ref fmtrec fmt, object dest) =
    array [1024]char str


    switch p.tag
    when tint then
        tostr_int(p, fmt, dest)
    when tword then
        tostr_word(p, fmt, dest)
    when treal then
        tostr_real(p, fmt, dest)
    when tstring then
        tostr_str(p, fmt, dest)
    when trange then
        tostr_range(p, fmt, dest)
    when tlist then
        tostr_list(p, fmt, dest)
    when trecord then
        tostr_record(p, fmt, dest)
    when tarray,tuserarray then
        tostr_array(p, fmt, dest)
    when tbits then
        tostr_bits(p, fmt, dest)
    when tset then
        tostr_set(p, fmt, dest)

    when tstruct then
        tostr_struct(p, fmt, dest)
    when tdecimal then
        tostr_decimal(p, fmt, dest)
    when tdict then
        tostr_dict(p, fmt, dest)
    when tvoid then
        addstring(dest,"<Void>")
    when trefvar then
            print @str,"Refvar:",,p.varptr
            addstring(dest,str)
            if p.varptr then
                fprint @str," <#>",ttname[p.varptr.tag]
                addstring(dest,str)
            fi

    when trefpack then
        if p.varptr=nil then
            addstring(dest,"nil")
        else
            fprint @str,"Ref #:#",ttname[p.elemtag],p.ptr
            addstring(dest,str)
        fi

    when trefbit then
        fprint @str,"Refbit #:# (#,#)",ttname[p.elemtag],p.ptr,p.bitoffset,p.bitlength
        addstring(dest,str)

    when tsymbol then
        if p.def then
            fprint @str,"<#:""#"">",namenames[p.def.nameid],p.def.name
            addstring(dest,str)
        else
            addstring(dest,"<nil>")
        fi
    when ttype then
        fprint @str,"#",ttname[p.value]
        addstring(dest,str)
    when toperator then
        fprint @str,"(#)", pclnames[p.value]+1
        addstring(dest,str)
        addstring(dest,str)

    when tenum then
        addstring(dest,getenumname(p.elemtag, p.value))

    when tbool then
        addstring(dest,(p.value|"True"|"False"))

    else
        pcustype("Tostr:",p)
    end
end

proc tostr_record(variant p, ref fmtrec fmt, object dest) =
    variant q
    int i,n
    char c
    object r

    r:=p.objptr

    q:=r.varptr

    n:=ttlength[r.usertag]

    if fmt.newline then
        to n do
            tostr(q,fmt,dest)
            addstring(dest,"\n",-1)
            ++q
        od

    else
        addstring(dest,"(",1)
        for i:=n downto 1 do
            tostr(q,fmt,dest)
            ++q
            if i<>1 then
                addstring(dest,",",1)
            fi
        od
        addstring(dest,")",1)
    fi
end

=== qq_pclgen.m 0 0 21/43 ===
const kjumpt = 1
const kjumpf = 0

const maxloopindex=20
array [maxloopindex,4]ref int loopstack
array [maxloopindex]int trylevelstack
global int loopindex=0
int looptrylevel            

const maxswitchrange=512
const maxlocals=300
const maxparams=100

const maxunits=400                  
int trylevel=0
int currfunction=0              

int retindex                        
int retvaloffset                    
int nprocparams                     
global int nproclocals              
global ref int pproclocals          
const retaddrslots = 1              
int procskiplabel

global proc evalunit(unit p,int res=1)=
    unit a,b
    symbol d
    object ss
    int procflag,index

    qpos:=p.pos

    a:=p.a
    b:=p.b


    switch p.tag
    when jintconst      then
        genpc_int(kpushci,p.value)

    when jwordconst      then
        genpc_int(kpushcu,p.uvalue)

    when jrealconst     then
        genpc_real(kpushcr,p.xvalue)

    when jboolconst then
        genpc((p.value|kpushtrue|kpushfalse))

    when jenumconst      then
        genpc_int2(kpushenum, p.value, p.mode)

    when jnone then

    when jstringconst   then
        genpc(kpushcs)
        genopnd_strz(p.svalue)

    when jname          then
        d:=p.def
        case d.nameid
        when frameid then
            genpc_name(kpushf,d)
        when paramid then
            genpc_name(kpushf,d)
            if d.mbyref then
                genpc(kpushptr)
            fi
        when staticid then
            genpc_name(kpushm,d)
        when procid,labelid,dllprocid then
            genpc_name(kpushsymbol,d)
        else
            genpc_name(kpushsymbol,d)
        esac

    when jsymbol        then            
        if a.tag=jname then
            genpc_name(kpushsymbol,a.def)
        else
            gerror(".$ name expected")
        fi

    when jhostname      then
        genpc_name(khostname,p.def)

    when jblock         then
        if a then
            while a and a.nextunit do
                evalunit(a,0)
                a:=a.nextunit
            od
            if a then
                evalunit(a,res)
            fi
        else
        fi

    when jdecimal then
        genpc(kpushcs)
        genopnd_strz(p.svalue)
        genpc(kmakedecimal)

    when jcall          then do_call(p,a,b,res,procflag)
    when jreturn        then do_return(p,a)
    when jcallhost      then do_callhost(p,a,res)

    when jassign        then do_assign(a,b,res)
    when jdeepcopy      then do_assign(a,b,res,1)
    when jto            then do_to(p,a,b)
    when jif            then do_if(p,a,b,b.nextunit, res)
    when jforup,jfordown    then do_for(p,a,b)
    when jforupx,jfordownx  then do_forx(p,a,b)
    when jforall,jforallrev then do_forall(p,a,b)
    when jforeach       then do_forall(p,a,b)
    when jwhile         then do_while(p,a,b)
    when jrepeat        then do_repeat(p,a,b)
    when jgoto          then
        if a.tag=jname and a.def.nameid=labelid then
            d:=a.def
            if d.labelno=0 then
                d.labelno:=createfwdlabel()
            fi
            genpc_lab(kjump,d.labelno)
        else
            evalunit(a)
            genpc(kjumpptr)
        fi

    when jlabeldef      then
        d:=a.def
        if d.labelno=0 then
            d.labelno:=definelabel()
        else
            index:=d.labelno
            definefwdlabel(index)
        fi

    when jredo          then do_exit(p,1)
    when jnext          then do_exit(p,2)
    when jexit          then do_exit(p,3)
    when jdo,jdoonce    then do_do(p,a)
    when jcase,jdocase then do_case(p,a,b,res)
    when jswitch, jdoswitch then do_switch(p,a,b,res)
    when jswap          then
        evalref(a)
        evalref(b)
        genpc(kswap)

    when jselect        then do_select(a,b,res)
    when jprint,jprintln,jsprint    then do_print(p,a,b)
    when jfprint,jfprintln,jsfprint then do_fprint(p,a,b,b.nextunit)
    when jread,jreadln  then do_read(p,a,b)
    when jnew           then do_new(p)

    when jstop          then
        if a then
            evalunit(a)
        else
            genpc_int(kpushci,0)
        fi
        genpc(kstop)

    when jtry           then do_try(p,a,b)
    when jandl          then do_andl(a,b)
    when jorl          then do_orl(a,b)
    when jmakelist then
        do_pushlist(a,p.length)
        genpc_int2(kmakelist,p.length,p.lower)

    when jmakeset then
        do_pushlist(a,p.length)
        genpc_int(kmakeset,p.length)

    when jmakedict then do_makedict(a,p.length)


    when jkeyvalue      then
        evalunit(a)
        evalunit(b)
    when jmap           then do_map(p,a,b)

    when jadd, jsub, jmul, jdiv, jidivrem, jiand, jior, jixor,
         jshl, jshr, jin, jnotin, jmin, jmax, jmakerange, jmakerangelen,
         jeq, jne, jlt, jle, jge, jgt, jpower,
         jconcat, jappend,jisequal then
        evalunit(a)
        evalunit(b)
        genpc(jpclcodes[p.tag])

    when jaddto, jsubto, jmulto, jdivto, jidivto, jiandto, jiorto, jixorto,
         jshlto, jshrto, jminto, jmaxto, jconcatto, jappendto,
         jandlto, jorlto then
        evalref(a)
        evalunit(b)
        genpc(jpclcodes[p.tag])

    when jidiv then
        do_idiv(p,a,b)

    when jirem then
        do_irem(p,a,b)


    when jneg, jabs, jlwb, jupb, jlen, jbounds, jnotl, jinot,jisarray,
         jisint, jisreal, jbytesize, jisdef, jround, jisvoid, jtype, jbitwidth,
         jistruel, jsqr, jsqrt, jislist, jasc,jchr, jisstring, jisset,
         jbasetype, jusertype, jelemtype, jispointer, jisrange, jisrecord,
         jfloor, jceil, jboundsx, jisnumber, jismutable,
         jsin,jcos,jtan, jasin, jacos, jatan, jexp, jln,
         jminvalue, jmaxvalue, jdictitems, jodd, jeven then
        do_unary(a,jpclcodes[p.tag])

    when jnegto, jabsto, jinotto, jnotlto then
        evalref(a)
        genpc(jpclcodes[p.tag])


    when jdot           then
        evalunit(a)
        genpc_name(kdot,b.def)

    when jindex         then do_bin(a,b,kindex)
    when jdotindex      then do_bin(a,b,kdotindex)
    when jkeyindex      then
        evalunit(a)
        evalunit(b)
        if b.nextunit then
            evalunit(b.nextunit)
        else
            genpc(kpushvoid)
        fi
        genpc(kkeyindex)



    when jptr           then do_unary(a,kpushptr)
    when jptrto then
        if a^.tag=jptr then         
            evalunit(a.a)
        else
            evalref(a)
        fi

    when jaddrof        then
        evalref(a)
        genpc(kconvrefpack)
    when jdaddrof        then
        evalref(a)
        genpc(kconvrefpack)

    when jconvert       then
        do_convert(p.mode, a)
    when jtypepun       then
        evalunit(a)
        genpc_int(ktypepun,p.mode)

    when jtypeconst     then
        genpc_int(kpusht,p.mode)
    when joperator      then
        genpc_int(kpushoperator,p.pclopcode)


    when jincrload, jdecrload, jloadincr, jloaddecr then
        do_incr(p,a,res)



    when jnil           then
        genpc(kpushnil)

    when jraise         then do_unary(a,kraise)

    when jdocstring then
    when jnull then
        genpc(kpushvoid)


    else
        gerror_s("UNSUPPORTED TAG:",JTAGNAMES[P.TAG],p)
    endswitch

    case jhasvalue[p.tag]
    when 0 then
        if res then
            gerror_s("Value expected:",jtagnames[p.tag])
        fi
    when 1 then
        if not res then
            if p.tag=jcall and procflag=1 then      
            elsif p.tag in [jincrload,jdecrload,jloadincr,jloaddecr] then
            elsif p.tag=jcallhost and hostisfn[p.index]=0 then
            else
                genpc_int(kunshare,1)
            fi
        fi
    esac                        
end

global proc gencodemodule(int n)=
    symbol d,e
    int lab

    currmodule:=&moduletable[n]
    stcurrproc:=stcurrmodule:=currmodule.def

    resetpcl(sourcefilesizes[moduletable[n].fileno])

    gencomment("Module data init code:")

    if n=1 then
        lab:=createfwdlabel()
        genpc_lab(kjump,lab)
        genpc(kstoprunproc)
        stopseq:=pcllast

        raiseseq:=pcllast+1
        genpc_int(kpushci,0)
        genpc(kraise)
        definefwdlabel(lab)
    fi

    d:=stcurrmodule.deflist
    while d do
        if d.nameid=staticid and d.code then
            evalunit(d.code)
            if d.initcode=3 then
                genpc(kcopy)
            fi
            genpc_name(kzpopm,d)
        elsif d.nameid=procid then
            e:=d.deflist
            while e do
                if e.nameid=staticid and e.code then
                    evalunit(e.code)
                    genpc_name(kzpopm,e)
                fi
                e:=e.nextdef
            od
        fi
        d:=d.nextdef
    od  

    if n=mainmoduleno then





        for i:=nmodules downto 1 when i<>mainmoduleno do
            genpc_name(kmodulecall, moduletable[i].def)

        od
        for i:=nmodules downto 1 when i<>mainmoduleno do
            if moduletable[i].startfn then
                genpc_name(kcallproc, moduletable[i].startfn)
                genopnd_int(0)
            fi
        od

        if currmodule.mainfn then
            genpc_name(kcallproc, currmodule.mainfn)
            genopnd_int(0)
        elsif currmodule.startfn then
            genpc_name(kcallproc, currmodule.startfn)
            genopnd_int(0)
        fi

        evalunit(stcurrmodule.code,0)
        genpc_int(kpushci,0)
        genpc(kstop)
    else

        evalunit(stcurrmodule.code,0)

        genpc(kmodulereturn)
    fi



    gencomment("Procs:")
    d:=stcurrmodule.deflist
    while d do
        switch d.nameid
        when procid then
            do_procdef(d)
        when staticid then
        when recordid then
            e:=d.deflist
            while e, e:=e.nextdef do
                if e.nameid=procid then
                    do_procdef(e)
                fi
            od

        when constid then
        when enumid then
        when labelid then
        when typeid then
        when dllprocid then
        when aliasid then
        when macroid then
        else
            gerror_s("?Module def:",namenames[d.nameid])
        end switch

        d:=d.nextdef
    od  

    genpc(kendmodule)
    genpc(kendmodule)
    genpc(kendmodule)


    moduletable[n].pcstart:=pclstart
    moduletable[n].pcend:=pclend
    moduletable[n].pcsize:=pclend-pclstart
    moduletable[n].pcsrcstart:=pclsrcstart
end

proc do_procdef (symbol p) =
    int nfreevars,nnofreevars
    int isfunc
    symbol oldcurrproc

    oldcurrproc:=stcurrproc         


    stcurrproc:=p

    retindex:=createfwdlabel()
    isfunc:=p.misfunc

    genprocentry(p,nfreevars,nnofreevars)

    if p.code=nil then
        gerror_s("Empty proc body",p.name)
    else
        evalunit(p.code, isfunc)

    fi

    definefwdlabel(retindex)            
    genprocexit(nfreevars,nnofreevars,isfunc)
    genpc(kprocend)

    if pproclocals^=0 then
        p.labelno:=procskiplabel
    fi

    stcurrproc:=oldcurrproc
end

proc genprocentry(symbol p, int &nfreevars,&nnofreevars) =      
    array [200]char str
    int n
    symbol d

    genpc_name(kprocdef, p)

    nprocparams:=nproclocals:=0

    d:=p.deflist
    while d do
        case d.nameid
        when frameid then
            ++nproclocals
            d.index:=nproclocals
        when paramid then
            ++nprocparams
        esac

        d:=d.nextdef
    od

    d:=p.deflist
    n:=nprocparams

    while d, d:=d.nextdef do
        case d.nameid
        when paramid then
            --n
            d.index:=-(n+retaddrslots)
        esac

    od

    retvaloffset:=-(nprocparams+retaddrslots)
    p.labelno:=definelabel()
    genpc_int(kprocentry, nproclocals)
    procskiplabel:=definelabel()

    pproclocals:=pclnext-1


    d:=p.deflist
    while d do
        case d.nameid
        when frameid then
            if d.code then
                evalunit(d.code)
                if d.initcode=3 then
                    genpc(kcopy)
                fi
                genpc_name(kzpopf, d)
            fi
        esac

        d:=d.nextdef
    od
end

proc genprocexit(int nfree,nnofree,isfunc)=     

    if isfunc then
        genpc_int(kpopretval,-(nprocparams+1)*varsize)
    fi
    if nproclocals then
        genpc_int(kunshare,nproclocals)
    fi

    if nprocparams=0 then
        genpc(kreturn0)
    else
        genpc_int(kreturn,nprocparams)
    fi
end

proc evalref(unit p)=
    unit a,b,c
    symbol d
    int lab1,lab2
    a:=p.a
    b:=p.b

    switch p.tag
    when jname then
        d:=p.def
        if d.nameid in [procid,dllprocid] then
            gerror("^ not allowed")
        fi  

        if d.nameid=paramid and d.mbyref then
            genpc_name(kpushf,d)
        else
            genpc_name(kpushmref+d.isframe,d)
        fi

    when jdot then
        evalunit(a)
        genpc_name(kdotref,b.def)
    when jindex then
        evalunit(a)
        evalunit(b)
        genpc(kindexref)


    when jdotindex then
        evalref(a)
        evalunit(b)
        genpc(kdotindexref)

    when jkeyindex then
        evalunit(a)
        evalunit(b)
        if b.nextunit then gerror("Def val not allowed") fi
        genpc(kkeyindexref)

    when jptr then
        evalunit(a)

    when jif then
        lab1:=createfwdlabel()              
        lab2:=createfwdlabel()

        genjumpcond(kjumpf,p.a,lab1)
        evalref(p.b)
        genjumpl(lab2)
        definefwdlabel(lab1)
        evalref(p.b.nextunit)
        definefwdlabel(lab2)
    else
            gerror_s("evalref",jtagnames[p.tag])
    end switch
end

proc genjumpcond(int opc,unit p,int lab)=
    unit q,r,s
    int oldpos, lab2, i


    q:=p.a
    r:=p.b

    switch p.tag
    when jandl then
        case opc
        when kjumpf then
            genjumpcond(kjumpf,q,lab)
            genjumpcond(kjumpf,r,lab)
        when kjumpt then
            lab2:=createfwdlabel()
            genjumpcond(kjumpf,q,lab2)
            genjumpcond(kjumpt,r,lab)
            definefwdlabel(lab2)
        esac

    when jorl then
        case opc
        when kjumpf then
            lab2:=createfwdlabel()
            genjumpcond(kjumpt,q,lab2)
            genjumpcond(kjumpf,r,lab)
            definefwdlabel(lab2)
        when kjumpt then
            genjumpcond(kjumpt,q,lab)
            genjumpcond(kjumpt,r,lab)
        esac

    when jnotl then
        case opc
        when kjumpf then
            genjumpcond(kjumpt,q,lab)
        when kjumpt then
            genjumpcond(kjumpf,q,lab)
        esac

    when jistruel then
        genjumpcond(opc,q,lab)

    when jblock then
        while q and q.nextunit do
            evalunit(q)
            q:=q.nextunit
        od
        genjumpcond(opc,q,lab)

    when jeq,jne,jlt,jle,jge,jgt then
        evalunit(q)
        evalunit(r)
        gcomparejump(opc,p.tag,lab)

    when jcmpchain then
        r:=q.nextunit
        i:=1
        if opc=kjumpf then
            while r do
                evalunit(q)
                evalunit(r)
                gcomparejump(kjumpt,reversecond(p.cmpgenop[i]),lab)
                ++i
                q:=r
                r:=r.nextunit
            od
        
        else
            lab2:=createfwdlabel()
            while r do
                evalunit(q)
                evalunit(r)
                if r.nextunit then
                    gcomparejump(kjumpt,reversecond(p.cmpgenop[i]),lab2)
                else
                    gcomparejump(kjumpt,p.cmpgenop[i],lab)
                fi
                ++i
                q:=r
                r:=r.nextunit
            od
            definefwdlabel(lab2)
        fi
    else
        evalunit(p)
        genpc_lab((opc=kjumpt|kjumptrue|kjumpfalse),lab)
    endswitch
    qpos:=oldpos

end

proc gcomparejump(int jumpopc,int cond, lab)=
    int opc


    if jumpopc=kjumpf then          
        cond:=reversecond(cond)     
    fi

    case cond
    when jeq then opc:=kjumpeq
    when jne then opc:=kjumpne
    when jlt then opc:=kjumplt
    when jle then opc:=kjumple
    when jge then opc:=kjumpge
    when jgt then opc:=kjumpgt
    else
        gerror("GCOMP: no cond")
    esac


    genpc_lab(opc,lab)
end

proc genjumpl(int lab)=
    genpc_lab(kjump,lab)
end

function reversecond(int op)int=

    case op
    when jeq then return jne
    when jne then return jeq
    when jlt then return jge
    when jle then return jgt
    when jge then return jlt
    when jgt then return jle
    esac
    return 0
end

global proc stacklooplabels(ref int a,b,c)=
    if loopindex>=maxloopindex then
        gerror("Too many nested loops")
    fi
    ++loopindex
    loopstack[loopindex,1]:=a
    loopstack[loopindex,2]:=b
    loopstack[loopindex,3]:=c
end

global proc unstacklooplabels=
    --loopindex
end

global function findlooplabel(int k,n)int=
    int i

    if n=0 then         
        i:=1
    else
        i:=loopindex-(n-1)      
    fi

    if i<1 or i>loopindex then
        gerror("Bad loop index")
    fi

    looptrylevel:=trylevelstack[i]
    return loopstack[i,k]^
end

proc do_assign(unit a,b, int res,deepcopy=0)=
    unit q
    int n

    if a.tag=b.tag=jmakelist then
        if deepcopy or res then gerror("mult/ass::=") fi
        do_multassign(a,b)
        return
    fi

    evalunit(b)
    if deepcopy then
        genpc(kcopy)
    fi

    do_store(a,res)
end

proc do_bin(unit a,b, int opc)=
    evalunit(a)
    evalunit(b)
    genpc(opc)
end

proc do_binref(unit a,b, int opc)=
    evalref(a)
    evalunit(b)
    genpc(opc)
end

proc do_unary(unit a, int opc)=
    evalunit(a)
    genpc(opc)
end

proc do_unaryref(unit a, int opc)=
    evalref(a)
    genpc(opc)
end

proc do_pushlist(unit a, int n)=
    while a, a:=a.nextunit do
        evalunit(a)
    od
end

proc do_makedict(unit a, int n)=
    to n do
        if a.tag=jkeyvalue then
            evalunit(a.a)
            evalunit(a.b)
        else
            gerror("dict not key:val")
        fi
        a:=a.nextunit
    od
    genpc_int(kmakedict,n)
end

proc do_call(unit p,a,b,int res, &procflag)=
    int nargs, nsimple, isfunc, kwdindex
    symbol d
    unit c
    array [maxparams]unit arglist

    isfunc:=1
    nargs:=nsimple:=0
    kwdindex:=0
    c:=b

    while c do
        arglist[++nargs]:=c
        if c.tag in [jintconst, jrealconst] then ++nsimple fi
        if c.tag=jkeyword then
            if kwdindex=0 then kwdindex:=nargs fi
        elsif kwdindex then
            gerror("Non-kwd follows kwd arg")
        fi
        c:=c.nextunit
    od

    case a.tag
    when jname then
        d:=a.def
retry::
        case d.nameid
        when procid then
            if d.misfunc then
                genpc(kpushvoid)
                nargs:=pushparams(d, arglist, nargs, kwdindex)
                genpc_name(kcallproc,d)
            else                    
                isfunc:=0
                nargs:=pushparams(d, arglist, nargs, kwdindex)
                genpc_name(kcallproc,d)
            fi
            genopnd_int(nargs)

        when dllprocid then
            if not d.misfunc then
                isfunc:=0
            else
                genpc(kpushvoid)
            fi
            nargs:=pushparams(d, arglist, nargs, kwdindex)
            genpc_name(kcalldll,d)
            genopnd_int(nargs)
        when aliasid then
            d:=d.alias
            goto retry
        when staticid, frameid, paramid then
            goto docallptr
        else
            gerror_s("CAN'T CALL:",namenames[d.nameid])
        esac
    when jdot then
        if kwdindex then docallptr fi       
        genpc(kpushvoid)
        evalref(a.a)                    
        for i to nargs do               
            evalunit(arglist[i])
        od
        evalunit(a)                     
        genpc(kcallptr)
        ++nargs
        genopnd_int(nargs)
        genopnd_int(0)

    else
docallptr::
        if kwdindex then gerror("Kwd params not allowed for fnptr") fi
        genpc(kpushvoid)
        for i to nargs do
            evalunit(arglist[i])
        od
        evalunit(a)
        genpc(kcallptr)
        genopnd_int(nargs)
        genopnd_int(0)
    esac


    if res and not isfunc then
        gerror("Func ret value expected")
    fi

    procflag:=not isfunc


end

function pushparams(symbol d, []unit &arglist, int nargs, kwdindex)int=

    int nparams, extra,n
    array [maxparams]symbol paramlist
    array [maxparams]byte byreflist
    symbol e, p

    nparams:=d.nparams
    e:=d.deflist
    n:=0
    while e do
        ++n
        paramlist[n]:=e
        byreflist[n]:=e.mbyref
        e:=e.nextdef
    od

    if kwdindex then
        pushkwdparams(d, arglist, nargs, kwdindex)
        return d.nparams
    fi

    extra:=0

    if nargs=nparams then
        for i to nargs do
            evalparam(arglist[i],byreflist[i])
        od
        return nargs
    elsif nargs<nparams then    
        for i to nargs do
            evalparam(arglist[i],byreflist[i])
        od

        for i:=nargs+1 to nparams do
            p:=paramlist[i]
            if not p.code and not p.moptional then
                gerror_s("Param not optional:",strint(i))
            fi
            if p.code then
                if byreflist[i] then gerror("byref with default val") fi
                evalunit(p.code)
            else
                genpc(kpushvoid)
            fi
        od
        return nparams
    else                        
        for i to nparams do
            evalparam(arglist[i],byreflist[i])
        od

        if not d.mvarparams then
            gerror("Too many args")
        fi
        for i:=nparams+1 to nargs do
            evalunit(arglist[i])            
        od
        return nargs
    fi
end

proc evalparam(unit a, int byref)=
    if byref then
        evalref(a)
    else
        evalunit(a)
    fi
end


proc pushkwdparams(symbol d, []unit &arglist, int nargs, kwdindex)=
    int nparams, i,j,k
    array [maxparams]symbol paramlist
    array [maxparams]byte byreflist
    array [maxparams]unit keyunits
    unit p,q
    symbol e

    nparams:=d.nparams

    e:=d.deflist
    for i to nparams do
        paramlist[i]:=e
        byreflist[i]:=e.mbyref
        e:=e.nextdef
    od

    if nargs>nparams then
        gerror("Too many args")
    fi

    for i:=kwdindex to nparams do
        keyunits[i]:=nil            
    od

    for i to kwdindex-1 do          
        evalparam(arglist[i],byreflist[i])
    od

    for i:=kwdindex to nargs do
        p:=arglist[i]
        q:=p.a
        if q.tag<>jname then gerror("kwd not a name") fi
        e:=q.def
        k:=0
        for j:=1 to nparams do
            if eqstring(e.name, paramlist[j].name) then
                k:=j
                exit
            fi
        od

        if k=0 then gerror_s("Can't find kwd param:",e.name) fi
        if k<kwdindex then gerror_s("Kwd arg already positional:",e.name) fi
        if keyunits[k] then gerror_s("Repeating kwd arg:",e.name) fi

        keyunits[k]:=p.b
    od

    for i:=kwdindex to nparams do
        if keyunits[i]=nil then
            q:=paramlist[i].code
            if q=nil and not paramlist[i].moptional then
                gerror_s("Param not optional:",strint(i))
            fi
            keyunits[i]:=q          
        fi
    od

    for i:=kwdindex to nparams do
        if keyunits[i] then
            evalparam(keyunits[i],byreflist[i])
        elsif byreflist[i] then
            gerror("byref param not optional")
        else
            genpc(kpushvoid)
        fi
    od
end

proc do_if(unit p,a,b,pelse, int res)=
    int lab1,lab2

    lab1:=createfwdlabel()              

    if pelse or res then lab2:=createfwdlabel() fi  

    genjumpcond(kjumpf,a,lab1)

    evalunit(b,res)

    if pelse or res then
        genjumpl(lab2)
        definefwdlabel(lab1)
        if pelse then
            evalunit(pelse,res)
        else
            genpc(kpushvoid)
        fi
        definefwdlabel(lab2)
    else
        definefwdlabel(lab1)
    fi
end

proc do_do (unit p,a)=
    int lab_abc,lab_d,lab_test
    lab_abc:=definelabel()
    lab_d:=createfwdlabel()

    stacklooplabels(&lab_abc, &lab_abc, &lab_d)

    evalunit(a,0)

    if p.tag=jdo then
        genjumpl(lab_abc)
    fi
    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_exit(unit p,int k) =
    int n,index

    index:=p.a.value
    if index=0 then index:=loopindex fi

    n:=findlooplabel(k,index)
    if n=0 then
CPL "BAD LOOP"
    else
        genjumpl(n)
    fi
end

proc do_to (unit p,pcount,pbody)=
    int lab_b,lab_c,lab_d
    symbol temp
    unit pav

    pav:=pcount.nextunit
    temp:=pav.def

    evalunit(pcount)
    genpc_name(kzpopm+temp.isframe,temp)

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    stacklooplabels(&lab_b,&lab_c,&lab_d)

    if pcount.tag<>jintconst then           
        genpc_name(kpushm+temp.isframe,temp)
        genpc_int(kpushci,0)
        genpc_lab(kjumple,lab_d)

    elsif pcount.value<=0 then      
        genpc_lab(kjump,lab_d)
    fi

    definefwdlabel(lab_b)
    evalunit(pbody,0)
    definefwdlabel(lab_c)

    genpc_lab(ktom+temp.isframe,lab_b)
    genopnd_name(temp)


    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_while(unit p,pcond,pbody) =
    int lab_b,lab_c,lab_d

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    stacklooplabels(&lab_b, &lab_c, &lab_d)

    genjumpl(lab_c)     

    definefwdlabel(lab_b)

    evalunit(pbody,0)

    definefwdlabel(lab_c)

    genjumpcond(kjumpt,pcond,lab_b)
    definefwdlabel(lab_d)
    --loopindex
end

proc do_repeat(unit p,a,b) =
    int lab_b, lab_c, lab_d

    lab_b:=definelabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    stacklooplabels(&lab_b, &lab_c, &lab_d)

    evalunit(a,0)

    definefwdlabel(lab_c)

    unless b.tag=jintconst and b.value=0 then
        genjumpcond(kjumpf,b,lab_b)
    end

    definefwdlabel(lab_d)
    --loopindex
end

proc do_for(unit p,pvar,pbody)=
    unit pfrom, pto, pstep, pelse,plimit,pautovar
    symbol dvar, limitvar
    int lab_b,lab_c,lab_d,lab_e,opc,oldqpos
    int step, fromval, limit, jumpinto

    pfrom:=pvar.nextunit
    pto:=pfrom.nextunit
    pstep:=pto.nextunit
    pautovar:=nil
    if pstep then
        gerror("By N not implem")
    fi

    pelse:=pbody.nextunit


    dvar:=pvar.def

    if pto.tag not in [jintconst,jname] or
         pto.tag=jname and pto.def.isframe<>dvar.isframe then
        pautovar:=createavnamex(stcurrproc)
    fi

    case p.tag
    when jforup then
        step:=1

    when jfordown then
        step:=-1
    esac

    jumpinto:=1         

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    lab_e:=(pelse|createfwdlabel()|lab_d)
    stacklooplabels(&lab_b,&lab_c,&lab_d)

    if pfrom.tag=jintconst then     
        fromval:=pfrom.value
        if pto.tag=jintconst then
            limit:=pto.value
            if (step=-1 and fromval>=limit) or (step=1 and fromval<=limit) then     
                jumpinto:=0
            fi
        fi
        if jumpinto then
            if step<0 then
                ++fromval
            else
                --fromval
            fi
            pfrom.value:=fromval
        fi
        genpc_int(kpushci,pfrom.value)

        genpc_name(kpopm+dvar.isframe,dvar)
    else
        evalunit(pfrom)
        genpc_name(kpopm+dvar.isframe,dvar)

        genpc_name((step<0|kincrtom|kdecrtom)+dvar.isframe,dvar)
    fi

    if pautovar then
        evalunit(pto)
        limitvar:=pautovar.def
        genpc_name(kzpopm+limitvar.isframe,limitvar)
        pto:=pautovar
    else
        limitvar:=pto.def
    fi

    if jumpinto then
        genjumpl(lab_c)         
    fi
    definefwdlabel(lab_b)

    evalunit(pbody,0)               

    definefwdlabel(lab_c)

    if pto.tag=jintconst then
        opc:=(step<0|kfordmci|kformci)+dvar.isframe
    elsif dvar.isframe=limitvar.isframe then
        opc:=(step<0|kfordmm|kformm)+dvar.isframe
    else
        gerror("for:mixed m/f vars")
    fi

    oldqpos:=qpos
    qpos:=p.pos
    genpc_lab(opc,lab_b)
    qpos:=oldqpos
    genopnd_name(dvar)

    if pto.tag=jintconst then
        genopnd_int(pto.value)
    else
        genopnd_name(limitvar)
    fi

    if pelse then
        definefwdlabel(lab_e)
        evalunit(pelse,0)           
    fi

    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_forx(unit p,pvar,pbody)=
    unit pbounds, pelse,plimit,pautovar
    symbol dvar, limitvar
    int lab_b,lab_c,lab_d,lab_e,opc

    pbounds:=pvar.nextunit

    pautovar:=createavnamex(stcurrproc)

    pelse:=pbody.nextunit
    dvar:=pvar.def

    if p.tag=jfordownx then
        gerror("Can't down inrev yet")
    fi

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    lab_e:=(pelse|createfwdlabel()|lab_d)
    stacklooplabels(&lab_b,&lab_c,&lab_d)

    evalunit(pbounds)               
    limitvar:=pautovar.def
    genpc_name(kzpopm+limitvar.isframe,limitvar)

    genpc(kdecr)
    genpc_name(kpopm+dvar.isframe,dvar)     

    genjumpl(lab_c)         
    definefwdlabel(lab_b)

    evalunit(pbody,0)               

    definefwdlabel(lab_c)

    if dvar.isframe=limitvar.isframe then
        genpc_lab(kformm+dvar.isframe,lab_b)
    else
        gerror("forx:mixed m/f")
    fi
    genopnd_name(dvar)
    genopnd_name(limitvar)

    if pelse then
        definefwdlabel(lab_e)
        evalunit(pelse,0)           
    fi

    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_print(unit p,a,b)=
    int issprint
    unit x

    issprint:=p.tag=jsprint

    if issprint then
        callhostfn(host_strstartprint)
    else
        if a then
            evalunit(a)
            callhostfn(host_startprint)
        else
            callhostfn(host_startprintcon)
        fi
    fi

    x:=b

    while x do
        case x.tag
        when jfmtitem then
            evalunit(x.b)
            evalunit(x.a)
            callhostfn(host_print)
        when jnogap then
            callhostfn(host_printnogap)
        when jspace then
            callhostfn(host_printspace)
        else
            evalunit(x)
            callhostfn(host_print_nf)
        esac
        x:=x.nextunit
    od

    if p.tag=jprintln then
        callhostfn(host_println)
    fi
    if issprint then
        genpc(kpushvoid)
        callhostfn(host_strendprint)
    else
        callhostfn(host_endprint)
    fi
end

proc do_fprint (unit p,a,b,c)=
    int issfprint
    unit x

    issfprint:=p.tag=jsfprint

    if issfprint then
        callhostfn(host_strstartprint)
    else
        if a then
            evalunit(a)
            callhostfn(host_startprint)
        else
            callhostfn(host_startprintcon)
        fi
    fi

    evalunit(b)                 
    callhostfn(host_setformat)

    x:=c
    while x do
        case x.tag
        when jfmtitem then
            evalunit(x.b)
            evalunit(x.a)
            callhostfn(host_print)
        when jnogap then
            callhostfn(host_printnogap)
        else
            genpc(kpushvoid)
            evalunit(x)
            callhostfn(host_print)
        esac
        x:=x.nextunit
    od

    if p.tag=jfprintln then
        callhostfn(host_println)
    fi
    if issfprint then
        genpc(kpushvoid)
        callhostfn(host_strendprint)
    else
        callhostfn(host_endprint)
    fi

end

proc do_read (unit p,a,b)=
unit x,xloop

if p.tag=jreadln then
    if a then
        evalunit(a)
        callhostfn(host_readln)
    else
        genpc(kpushvoid)
        callhostfn(host_readln)
    fi
fi

xloop:=b
while xloop do
    x:=xloop
    genpc(kpushvoid)
    if x.tag=jfmtitem then
        evalunit(x.b)
        callhostfn(host_sread)
        x:=x.a
    else
        genpc(kpushvoid)
        callhostfn(host_sread)
    fi
    if x.tag=jname then
        genpc_name(kpopm+x.def.isframe,x.def)
    else
        evalref(x)
        genpc(kpopptr)
    fi
    xloop:=xloop.nextunit
od
end

proc do_forall (unit p,pindex,pbody)=

    int lab_b,lab_c,lab_d,lab_e
    unit ploopvar, plist, pelse, plimitvar, plistvar
    symbol indexvar,limitvar,loopvar, listvar

    plist:=pindex.nextunit
    ploopvar:=plist.nextunit

    if ploopvar=nil then            
        ploopvar:=pindex

        pindex:=createavnamex(stcurrproc)

    fi
    loopvar:=ploopvar.def

    plimitvar:=createavnamex(stcurrproc)

    limitvar:=plimitvar.def
    indexvar:=pindex.def

    if plist.tag<>jname or plist.def.isframe<>loopvar.isframe then          

        plistvar:=createavnamex(stcurrproc)

        listvar:=plistvar.def
        evalunit(plist)
        genpc_name(kzpopm+listvar.isframe,listvar)
    else
        plistvar:=plist
        listvar:=plistvar.def
    fi

    unless indexvar.isframe=loopvar.isframe=listvar.isframe then
        gerror("forall: mixed vars")
    end

    pelse:=pbody.nextunit

    if p.tag=jforallrev then
        gerror("Forall/rev not ready")
    fi

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    lab_e:=(pelse|createfwdlabel()|lab_d)
    stacklooplabels(&lab_b,&lab_c,&lab_d)

    genpc_name(kpushm+listvar.isframe, listvar)         
    genpc(kboundsx)         

    genpc_name(kzpopm+listvar.isframe,limitvar)     
    genpc(kdecr)
    genpc_name(kzpopm+indexvar.isframe,indexvar)        

    genjumpl(lab_c)         

    definefwdlabel(lab_b)

    genpc_name(kpushm+listvar.isframe,listvar)
    evalunit(pindex)

    if p.tag in [jforall,jforallrev] then
        genpc(kindex)
    else
        genpc(kdotindex)
    fi
    genpc_name(kpopm+loopvar.isframe,loopvar)

    evalunit(pbody,0)           

    definefwdlabel(lab_c)

    if indexvar.isframe=limitvar.isframe then
        genpc_lab(kformm+indexvar.isframe,lab_b)
    else
        gerror("forall:mixed m/f")
    fi
    genopnd_name(indexvar)
    genopnd_name(limitvar)

    if pelse then
        definefwdlabel(lab_e)
        evalunit(pelse,0)           
    fi

    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_case (unit p,pindex,pwhenthen,int res) =

    int lab_a,lab_d
    int loopsw,labnextwhen,labstmtstart,fmult
    unit w,wt,pelse

    if pindex.tag=jnone then
        do_case_nc(p,pindex,pwhenthen,res)
        return
    fi

    loopsw:=p.tag=jdocase or p.tag=jdoswitch
    pelse:=pindex.nextunit

    if loopsw then
        lab_a:=definelabel()
        lab_d:=createfwdlabel()
        stacklooplabels(&lab_a,&lab_a,&lab_d)
    else
        lab_d:=createfwdlabel()
    fi

    evalunit(pindex)            

    wt:=pwhenthen
    while wt do
        w:=wt.a
        fmult:=w.nextunit<>nil

        labnextwhen:=createfwdlabel()

        if fmult then
            labstmtstart:=createfwdlabel()
        fi

        while w do
            evalunit(w)
            w:=w.nextunit
            if w then                   
                genpc_lab(kjumptesteq,labstmtstart)
            else
                genpc_lab(kjumptestne,labnextwhen)
            fi
        od
        if fmult then
            definefwdlabel(labstmtstart)
        fi
        evalunit(wt.b,res)

        if not loopsw then
            genjumpl(lab_d)
        else
            genjumpl(lab_a)
        fi
        definefwdlabel(labnextwhen)
        wt:=wt.nextunit
    od

    genpc_int(kunshare,1)

    if pelse then
        evalunit(pelse,res)
    elsif res then
        genpc(kpushvoid)
    fi
    if loopsw then
        genjumpl(lab_a)
        definefwdlabel(lab_d)
        unstacklooplabels()
    else
        definefwdlabel(lab_d)
    fi
end

proc do_case_nc (unit p,pindex,pwhenthen,int res) =

    int lab_a,lab_d
    int labnextwhen,labstmtstart,fmult
    unit w,wt,pelse

    if p.tag<>jcase then gerror("case-nc") fi

    pelse:=pindex.nextunit

    lab_d:=createfwdlabel()

    wt:=pwhenthen
    while wt do
        w:=wt.a
        fmult:=w.nextunit<>nil

        labnextwhen:=createfwdlabel()

        if fmult then
            labstmtstart:=createfwdlabel()
        fi

        while w do
            evalunit(w)
            w:=w.nextunit
            if w then                   
                genpc_lab(kjumptrue,labstmtstart)
            else
                genpc_lab(kjumpfalse,labnextwhen)
            fi
        od
        if fmult then
            definefwdlabel(labstmtstart)
        fi
        evalunit(wt.b,res)

        genjumpl(lab_d)
        definefwdlabel(labnextwhen)
        wt:=wt.nextunit
    od

    if pelse then
        evalunit(pelse,res)
    elsif res then
        genpc(kpushvoid)
    fi

    definefwdlabel(lab_d)
end

proc do_try(unit p,a,b) =
    int labend,labx
    unit ptry,x,pexcept,pexcode

    ++trylevel
    labend:=createfwdlabel()
    ptry:=a
    labx:=createfwdlabel()

    pexcept:=b

    if pexcept=nil then
        gerror("try: no except")
    elsif pexcept.nextunit then
        gerror("Try:multiple except block not implemented")
    fi

    while pexcept do
        pexcode:=pexcept.a
        if pexcode=nil or pexcode.nextunit then
            gerror("Try:multiple except codes not implemented")
        fi
        genpc_lab(kpushtry,labx)
        genopnd_int(getconstvalue(pexcode))
        genopnd_int(1)
        evalunit(ptry,0)
        genjumpl(labend)
        definefwdlabel(labx)
        evalunit(pexcept.b,0)
        definefwdlabel(labend)
        pexcept:=pexcept.nextunit
    od

    genpc_int(kaddsp,1)
    --trylevel
end

function unitstoarray(unit p, ref[]unit plist, int maxunits)int=
    int n

    n:=0
    while p do
        if n>=maxunits then
            gerror("UTA Too many units")
        fi
        plist^[++n]:=p
        p:=p.nextunit
    od
    
    return n
end

proc do_select(unit pindex,pplist,int res)=
    int n,labend,i,lab,elselab
    unit x,pelse

    array [maxswitchrange]unit plist
    array [maxswitchrange+1]int labels

    pelse:=pindex.nextunit

    n:=unitstoarray(pplist,&plist,maxswitchrange)

    if n>maxswitchrange then
        gerror("Selectx too complex")
    fi

    labend:=createfwdlabel()

    evalunit(pindex)
    genpc_int2(kswitch,n,1)

    for i:=1 to n do
        labels[i]:=pclnext-pclstart     
        genpc_lab(kjumplabel,0)
    od
    labels[n+1]:=pclnext-pclstart
    genpc_lab(kjumplabel,0)

    i:=1
    for i:=1 to n do
        x:=plist[i]
        lab:=definelabel()

        (pclstart+labels[i]+1)^:=lab
        evalunit(x,res)

        genjumpl(labend)    
    od

    elselab:=definelabel()

    (pclstart+labels[n+1]+1)^:=elselab

    if pelse then
        evalunit(pelse,res)
    elsif res then
        genpc(kpushvoid)
    fi

    genpc(knop)

    definefwdlabel(labend)
end

proc do_andl(unit x,y)=
    int a,b

    a:=createfwdlabel()
    b:=createfwdlabel()

    genjumpcond(kjumpf,x,a)
    genjumpcond(kjumpf,y,a)

    genpc_int(kpushci,1)
    genjumpl(b)
    definefwdlabel(a)
    genpc_int(kpushci,0)
    genpc(knop)
    definefwdlabel(b)
end

proc do_orl(unit x,y)=
    int a,b
    a:=createfwdlabel()
    b:=createfwdlabel()

    genjumpcond(kjumpt,x,a)
    genjumpcond(kjumpt,y,a)
    genpc_int(kpushci,0)
    genjumpl(b)
    definefwdlabel(a)
    genpc_int(kpushci,1)
    genpc(knop)
    definefwdlabel(b)
end

proc do_incr(unit p,a, int res)=
    symbol d
    if res then
        do_unaryref(a,jpclcodes[p.tag])
    elsif a.tag=jname then
        d:=a.def
        if d.nameid=paramid and d.mbyref then
            goto dounary
        else
            genpc_name((p.tag in [jincrload,jloadincr]|kincrtom|kdecrtom)+a.def.isframe,a.def)
        fi
    else
dounary::
        do_unaryref(a,(p.tag in [jincrload,jloadincr]|kincrptr|kdecrptr))
    fi
end

proc do_new(unit p)=
    int n
    unit q

    n:=p.nparams
    if n<1 or n>3 then gerror("new args") fi

    q:=p.a

    genpc(kpushvoid)
    to n do
        evalunit(q)
        q:=q.nextunit
    od
    to 3-n do
        genpc(kpushvoid)
    od

    callhostfn(host_new)

end

function checkblockreturn(unit p)int=
ref unitrec q,r

if p=nil then return 0 fi
if p.tag<>jblock then gerror("CBR?") fi

q:=p.a
if q=nil then return 0 fi       

while r:=q.nextunit do          
    q:=r
od

case q.tag
when jreturn then           
    return 1

when jif then
    return checkblockreturn(q.b) and checkblockreturn(q.b.nextunit)     
esac
return 0
end

proc do_callhost(unit p, a, int res)=
    int index:=p.index
    int isfunc:=hostisfn[index]
    int nargs, nparams, fparams
    array [10]unit plist
    unit q


    if res and not isfunc then
        gerror("Host proc not a function")
    fi

    if isfunc then
        genpc(kpushvoid)
    fi

    nargs:=0
    q:=a

    while q do
        if nargs>plist.upb then
            gerror("Too many host args")
        fi
        plist[++nargs]:=q

        q:=q.nextunit
    od

    if index=host_allparams and a=nil then
        nparams:=1
    else
        nparams:=nargs
    fi

    if nparams=0 and hostlvset[index] then
        gerror("LV hostfn: needs 1+ params")
    fi
    fparams:=hostnparams[index]
    if nparams>fparams then
        gerror("Hostfn too many params")
    fi

    to fparams-nparams do
        genpc(kpushvoid)
    od

    for i:=nparams downto 1 do
        if i=1 and hostlvset[index] then
            evalref(plist[i])
        elsif i=1 and index=host_allparams and nargs=0 then
            genpc_name(kpushmref,stcurrproc)
        else
            evalunit(plist[i])
        fi
    od  

    callhostfn(index,res)
end

proc callhostfn(int fnindex,calledasfn=0)=

    genpc_int(kcallhost,fnindex)
end

proc genfree(int n)=
    genpc_int(kunshare,n)
end

proc do_return(unit p,a)=
    if a then

        evalunit(a)
    elsif currfunction=2 then
        gerror("function needs return value")
    fi

    genjumpl(retindex)
end

proc do_multassign(unit a,b)=
    unit p,q
    array [100]unit plist
    int n

    p:=a.a
    q:=b.a
    n:=0

    while p do
        if q=nil then gerror("Too few RHS elems") fi
        evalunit(q)
        if n>=plist.len then gerror("Too many elems") fi
        plist[++n]:=p

        p:=p.nextunit
        q:=q.nextunit
    od

    if q then gerror("Too few LHS elems") fi

    for i:=n downto 1 do
        do_store(plist[i])
    od
end

proc do_store(unit a,int res=0)=
    symbol d
    unit p
    array [100]unit plist
    int n

    if res and a.tag<>jname then
        genpc(kdupl)
    fi

    case a.tag
    when jname then
        d:=a.def
        if d.nameid=paramid and d.mbyref then
            if res then genpc(kdupl) fi
            genpc_name(kpushf,d)
            genpc(kpopptr)
        elsif res then
            genpc_name(kstorem+d.isframe,d)
        else
            genpc_name(kpopm+d.isframe,d)
        fi

    when jdot then
        evalunit(a.a)
        genpc_name(kpopdot,a.b.def)

    when jindex then
        do_bin(a.a, a.b, kpopindex)
    when jdotindex then

        evalref(a.a)
        evalunit(a.b)
        genpc(kpopdotindex)
    when jptr then
        evalunit(a.a)
        genpc(kpopptr)

    when jkeyindex then
        do_bin(a.a, a.b, kpopkeyindex)

    when jmakelist then         
        n:=0
        p:=a.a
        while p do
            if n>=plist.len then gerror("Too many elems") fi
            plist[++n]:=p
            p:=p.nextunit
        od
        if n=0 then gerror("Empty lhs list") fi

        genpc_int(kexpand,n)
        for i:=1 to n do
            do_store(plist[i])
        od

    when jif then
        evalref(a)
        genpc(kpopptr)

    else
        gerror_s("Can't store to this unit yet:",jtagnames[a.tag], a)
    esac
end

function getconstvalue(unit p)int =
    if p and p.tag=jintconst then
        return p.value
    fi
    gerror("gcv Not const")
    return 0
end

proc do_convert(int m,unit p)=
    int n,elemmode,i,lowerx,lbound,mbase,nfields
    array [maxunits]unit plist

    mbase:=ttbasetype[m]

    if p.tag<>jmakelist then        

        if mbase=trecorddef then
            p:=createunit2(jmakelist,p,nil)
        else
            evalunit(p)
            genpc_int(kconvert,m)
            return
        fi
    fi

    n:=unitstoarray(p.a,&plist,maxunits)
    if n and plist[1].tag=jkeyvalue then
        case mbase
        when trecord then
            do_makerecordkv(m,n,plist)
        when tstruct then
            do_makestructkv(m,n,plist)
        else
            gerror("key:value not allowed")
        esac
        return
    fi

    for i:=1 to n do        
        evalunit(plist[i])
    od

    case mbase
    when trecorddef, tpackrecord then
        nfields:=ttlength[m]
        if n then
            checkelems(n,nfields,p)
        else                
            to nfields do
                genpc_int(kpushci,0)
            od
            n:=nfields
        fi
        genpc_int2((mbase=trecorddef|kmakerecord|kmakestruct),n,m)

    when tlist then     
        lowerx:=p.lower
        genpc_int2(kmakelist,n,lowerx)

    when tarray then
        genpc_int4(kmakearray,p.lower,n,tarray,p.elemtype)

    when tpackarray then
        elemmode:=tttarget[m]
        lowerx:=ttlower[m]

        checkelems(n,ttlength[m],p)
        genpc_int4(kmakearray,lowerx,n,m,elemmode)

    when tbits then
        if m=tbits then         
            genpc_int4(kmakebits,p.lower,n,tbits,(p.elemtype=tvoid|tu1|p.elemtype))
        else
            gerror("user-define bit array not ready")
        fi

    when tset then
        genpc_int(kmakeset,n)

    else
        gerror_s("Convert list",strmode(mbase))
    esac
end

proc checkelems(int n,length, unit p)=
    if n<length then
        gerror("Too few elements")
    elsif n>n then
        gerror("Too many elements")
    fi
end

proc do_switch (unit p,pindex,pwhenthen, int res) =
    int minlab,maxlab,x,y,i,n
    unit w,wt, pelse

    pelse:=pindex.nextunit
    minlab:=1000000
    maxlab:=-1000000            

    n:=0                
    wt:=pwhenthen

    while wt do
        w:=wt.a
        while w do
            case w.tag
            when jmakerange then
                x:=getconstvalue(w.a)
                y:=getconstvalue(w.b)
dorange::
                for i:=x to y do
                    minlab :=min(minlab,i)
                    maxlab :=max(maxlab,i)
                od
            when jintconst then
                x:=y:=w.value
                goto dorange
            when jtypeconst then
                x:=y:=w.mode
                goto dorange
            else
                gerror_s("Switch when2: not const",strexpr(w)^.strptr)
            esac
            w:=w.nextunit
        od
        wt:=wt.nextunit
    od

    if maxlab-minlab<=maxswitchrange then
        do_simpleswitch(p,pindex,pwhenthen,pelse, minlab,maxlab, res)
        return
    fi

    gerror("COMPLEX SWITCH/NOT COMPLETE")
end

proc do_simpleswitch(unit p,pindex,pwhenthen,pelse, int a,b, res) =

    unit w,wt,q
    int loopsw,n,offset,x,y,x0,i,labstmt,elselab
    array [1..maxswitchrange+1]ref int labels
    int lab_a,lab_b,lab_c,lab_d

    loopsw:=p.tag=jdoswitch

    n:=b-a+1
    offset:=a-1     

    if loopsw then
        lab_a:=definelabel()
        lab_d:=createfwdlabel()
        stacklooplabels(&lab_a,&lab_a,&lab_d)
    else
        lab_d:=createfwdlabel()
    fi
    elselab:=createfwdlabel()

    evalunit(pindex)

    genpc_int2(kswitch,n,a)

    for i:=1 to n do
        genpc_lab(kjumplabel,0)
        labels[i]:=pcllast+1        
    od

    genpc_lab(kjumplabel,0)         
    labels[n+1]:=pcllast+1


    wt:=pwhenthen
    while wt do
        labstmt:=definelabel()
        w:=wt.a
        while w do
            case w.tag
            when jmakerange then
                x0:=getconstvalue(w.a)
                y:=getconstvalue(w.b)

            when jintconst then
                x0:=y:=w.value
            when jtypeconst then
                x0:=y:=w.mode
            esac

            for x:=x0 to y do
                i:=x-offset
                if labels[i]^ then          
                    cpl x,char(x)
                    gerror("Dupl switch value")
                fi
                labels[i]^:=labstmt
            od
            w:=w.nextunit
        od

        evalunit(wt.b, res)

        if not loopsw then
            genjumpl(lab_d)
        else
            genjumpl(lab_a)
        fi
        wt:=wt.nextunit
    od

    definefwdlabel(elselab)
    if pelse then       
        evalunit(pelse,res)
    fi  

    if loopsw then
        genjumpl(lab_a)
        definefwdlabel(lab_d)
        unstacklooplabels()
    else
        definefwdlabel(lab_d)
    fi

    for i:=1 to n do
        if labels[i]^=0 then
            labels[i]^:=elselab
        fi
    od
    labels[n+1]^:=elselab
end

proc do_makestructkv(int m,n, []unit &plist)=
GERROR("STRUCTLV NOT READY")
end

proc do_makerecordkv(int m,nkeyvals, []unit &kvlist)=
    unit p
    array [maxunits]unit plist
    int nfields, index
    symbol d:=ttnamedef[m], e, f, k

    e:=d.deflist
    nfields:=0

    while e,e:=e.nextdef do
        if e.nameid=fieldid and e.atfield=nil then
            ++nfields
            plist[nfields]:=nil
        fi
    od

    for i to nkeyvals do
        k:=kvlist[i].a.def
        p:=kvlist[i].b

        e:=d.deflist
        f:=nil
        while e,e:=e.nextdef do
            if e.nameid=fieldid and e.firstdupl=k then
                f:=e
                exit
            fi
        od

        if not f then
            gerror_s("Can't find field:",k.name)
        fi
        index:=f.index
        if plist[index] then
            gerror_s("Dupl key:",k.name)
        fi
        plist[index]:=p
    od

    for i to nfields do
        if plist[i] then
            evalunit(plist[i])
        else
            genpc_int(kpushci,0)
        fi
    od

    genpc_int2(kmakerecord,nfields,m)
end

proc do_idiv(unit p,a,b)=
    int n

    evalunit(a)
    if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
        genpc_int(kpushci,n)
        genpc(kshr)
    else
        evalunit(b)
        genpc(kidiv)
    fi
end

proc do_irem(unit p,a,b)=
    int n
    word m

    evalunit(a)
    if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
        m:=inot(0xFFFF'FFFF'FFFF'FFFF << n)
        genpc_int(kpushci,M)
        genpc(kiand)
    else
        evalunit(b)
        genpc(kirem)
    fi
end

proc do_map(unit p,popcode,x)=
    evalunit(x)
    if x.nextunit then
        evalunit(x.nextunit)
    fi
    evalunit(popcode)
    genpc((x.nextunit|kmapss|kmaps))

    int lab:=createfwdlabel()
    genpc_lab(kjump,lab)        
    genpc(knop)                 
    definefwdlabel(lab)
end

=== qq_pcllib.m 0 0 22/43 ===
const pclinitalloc=128

global ref int pclstart             
global ref int pclnext              
global ref int pclend               
global ref int pcllast              
global int pclalloc                 

global ref int32 pclsrcstart
global ref int32 pclsrcnext

global int pclcurrlineno            
const pclelemsize=int.bytes
const pclsrcelemsize=int32.bytes


global const labelinitalloc=8192
global ref[]int labeloffsettable
global int labelalloc
global int nextlabelno
int labelflag

global array [0..pclnames.upb]byte pclnopnds

proc start=
    int nn

    for i:=1 to klastpcl do
        nn:=0
        for j:=1 to 4 do
            if pclfmt[i,j]=0 then exit fi
            ++nn
        od
        pclnopnds[i]:=nn
    od

    pcm_init()


    labelalloc:=labelinitalloc
    labeloffsettable:=pcm_alloc(int.bytes*labelalloc)
end

global proc resetpcl(int sourcesize)=
    int pclsize

    qpos:=0
    nextlabelno:=0
    pclcurrlineno:=0


    pclsize:=sourcesize         

    pclalloc:=1024                  
    while pclalloc<pclsize do
        pclalloc<<:=1
    od

    pclstart:=pcm_alloc(pclalloc*pclelemsize)
    pclnext:=pclstart
    pclend:=pclstart+pclalloc-16            
    pcllast:=nil

    pclsrcstart:=pcm_alloc(pclalloc*pclsrcelemsize)
    pclsrcnext:=pclsrcstart


end

global proc genpc(int opc)=

    if opc=0 then
        GERROR("ZERO PCL OP?")
    fi
IF OPC>PCLNAMES.LEN THEN
GERROR("GENPC:BAD OPC")
FI

    if pclnext>=pclend then
        extendpcldata()
    fi

    pclnext^:=opc
    pcllast:=pclnext

    pclsrcnext^:=qpos

    ++pclnext
    ++pclsrcnext
    labelflag:=0
end

global proc genopnd_int(int64 x)=
    pclnext++^:=x
    ++pclsrcnext
end

global proc genopnd_name(ref strec d)=
    pclnext++^:=int@(d)
    ++pclsrcnext
end

global proc genpc_int(int opc, int64 a)=
    genpc(opc)
    pclnext++^:=a
    ++pclsrcnext
end

global proc genpc_int2(int opc, int64 a,b)=
    genpc(opc)
    pclnext++^:=a
    pclnext++^:=b
    pclsrcnext+:=2

end

global proc genpc_int4(int opc, int64 a,b,c,d)=
    genpc(opc)
    pclnext++^:=a
    pclnext++^:=b
    pclnext++^:=c
    pclnext++^:=d
    pclsrcnext+:=4
end

global proc genpc_name(int opc, ref strec d)=
    if pcllast^=kpopf and opc=kpushf and not labelflag and (pcllast+1)^=int64@(d) then
        pcllast^:=kstoref
        return
    fi

    genpc(opc)
    pclnext++^:=int64@(d)
    ++pclsrcnext
end

global proc genopnd_strz(ichar s)=
    pclnext++^:=int64@(s)
    ++pclsrcnext
end

global proc genopnd_str(object s)=
    pclnext++^:=int64@(s)
    ++pclsrcnext
end

global proc genopnd_obj(object p)=
    pclnext++^:=int64@(p)
    ++pclsrcnext
end

global proc genpc_real(int opc, real x)=
    genpc(opc)
    pclnext++^:=int64@(x)
    ++pclsrcnext
end

global proc genpc_lab(int opc, int a)=
    int lastpc
    genpc(opc)
    genopnd_lab(a)
end

global proc genopnd_lab(int a)=
    int lastpc


    if a>=0 then                
        pclnext++^:=a
        ++pclsrcnext
        return
    fi

    a:=-a                   
    lastpc:=labeloffsettable^[a]        
    labeloffsettable^[a]:=pclnext-pclstart
    pclnext++^:=lastpc
    ++pclsrcnext
end

global proc gencomment(ichar s)=
    genpc(kcomment)
    genopnd_strz(pcm_copyheapstring(s))
end

proc convertstring(ichar s, t)=
int c

while c:=s++^ do
    switch c
    when '"' then
        t++^:='\\'
        t++^:='"'
    when 10 then
        t++^:='\\'
        t++^:='n'
    when 13 then
        t++^:='\\'
        t++^:='c'
    when 9 then
        t++^:='\\'
        t++^:='t'
    when '\\' then
        t++^:='\\'
        t++^:='\\'
    when 7,8,26,27 then
        t++^:='<'
        t++^:=c/10+'0'
        t++^:=(c rem 10)+'0'
        t++^:='>'
    else
        t++^:=c
    endswitch
od
t^:=0
end

global function getdottedname(ref strec p)ichar=
static array [256]char str
array [256]char str2
ref strec owner

strcpy(&.str,p^.name)
return &.str
end

proc extendpcldata=
    int newpclalloc
    ref int newpclstart
    ref int32 newpclsrcstart

    newpclalloc:=pclalloc*2

CPL "EXTENDING PCL TABLE TO",=PCLSTART

    newpclstart:=pcm_alloc(pclelemsize*newpclalloc)
    newpclsrcstart:=pcm_alloc(pclsrcelemsize*newpclalloc)

    memcpy(newpclstart,pclstart, (pclnext-pclstart)*pclelemsize)
    memcpy(newpclsrcstart,pclsrcstart, (pclnext-pclstart)*pclsrcelemsize)

    pclnext:=newpclstart+(pclnext-pclstart)
    pclend:=newpclstart+newpclalloc-10
    pcllast:=newpclstart+(pcllast-pclstart)
    pclsrcnext:=newpclsrcstart+(pclsrcnext-pclsrcstart)

    pcm_free(pclstart,pclalloc*pclelemsize)
    pcm_free(pclsrcstart,pclalloc*pclsrcelemsize)

    pclstart:=newpclstart
    pclalloc:=newpclalloc
    pclsrcstart:=newpclsrcstart
end

global proc extendlabeltable=
    int newlabelalloc
    ref[]int newlabeltable

    newlabelalloc:=labelalloc*2

    newlabeltable:=pcm_alloc(int.bytes*newlabelalloc)

    memcpy(newlabeltable,labeloffsettable, labelalloc*int.bytes)

    pcm_free(labeloffsettable,labelalloc*int.bytes)

    labeloffsettable:=newlabeltable
    labelalloc:=newlabelalloc
end

global proc pushint(int a)=
    genpc_int(kpushci,a)
end

global proc pushreal(real x)=
    genpc_int(kpushcr,int@(x))
end

global proc pushstring(ichar s)=
    genpc(kpushcs)
GERROR("PUSHSTRING")
end

global function definelabel:int=
    if nextlabelno>=labelalloc then extendlabeltable() fi
    ++nextlabelno
    labeloffsettable^[nextlabelno]:=pclnext-pclstart
    labelflag:=1
    return pclnext-pclstart
end

global function createfwdlabel:int=
    if nextlabelno>=labelalloc then extendlabeltable() fi
    ++nextlabelno
    labeloffsettable^[nextlabelno]:=0
    return -nextlabelno
end

global proc definefwdlabel(int &lab)=
    int newlab,index,laboffset,pc,nextpc


    index:=lab
    if index>=0 then gerror("deffwdlabel?") fi
    index:=-index

    laboffset:=pclnext-pclstart

    pc:=labeloffsettable^[index]            
    while pc do                     
        nextpc:=(pclstart+pc)^
        (pclstart+pc)^:=laboffset
        pc:=nextpc
    od
    labeloffsettable^[index]:=laboffset

    lab:=laboffset
    labelflag:=1
end

global function isstatic(symbol d)int=
    return d.nameid=staticid
end
=== qq_records.m 0 0 23/43 ===
global proc var_make_record(variant a, dest, int n, rectype) =
    object p
    variant b
    int m

    p:=obj_new_record(rectype,nil)

    b:=p.varptr

    m:=ttlength[rectype]

    if n<m then
        pcerror("Too few elements")
    elsif n>m then
        println =n,=m
        pcerror("Too many elements")
    fi

    to n do
        b^:=a^              
        ++a
        ++b
    od

    dest.tagx:=trecord ior hasrefmask
    p.usertag:=rectype


    dest.objptr:=p
end

global function obj_new_record(int m, variant defval)object p=
    variant a
    int n

    p:=obj_new()
    p.mutable:=1
    n:=ttlength[m]
    p.objtype:=normal_obj

    if n then
        p.varptr:=a:=pcm_alloc(n*varrec.bytes)

        if defval and defval.tag<>tvoid then
            a:=p.varptr
            to n do
                a^:=defval^
                var_share(a)
                ++a
            od
        else
            to n do
                a.tagx:=tint
                a.value:=0
                ++a
            od
        fi
    fi

    return p
end

global proc obj_free_record(object p)=
    variant q

    q:=p.varptr
    to p.length do
        var_unshare(q)
        ++q
    od
    if p.length then
        pcm_free(p.varptr,p.length*varrec.bytes)
    fi

    pcm_free32(p)
end

global proc var_dupl_record(variant a)=
    object p,q
    variant plist, qlist
    int length

    p:=a.objptr
    q:=obj_new()
    q^:=p^
    q.refcount:=1
    q.mutable:=1

    a.objptr:=q
    length:=ttlength[p.usertag]

    if length=0 then return fi

    qlist:=q.varptr:=pcm_alloc(length*varrec.bytes)
    plist:=p.varptr

    to length do
        qlist^:=plist^
        if qlist.tag=trecord then
            var_share(qlist)
        else
            var_dupl(qlist)
        fi
        ++qlist
        ++plist
    od
end

global function var_equal_record(variant x,y)int =
    int xlen,ylen,res
    object px,py
    variant a,b


    px:=x.objptr
    py:=y.objptr
    if px.usertag<>py.usertag then return 0 fi

    if px=py then
        return 1
    fi

    a:=px.varptr
    b:=py.varptr

    to ttlength[px.usertag] do
        if var_equal(a,b)=0 then return 0 fi    
        ++a
        ++b

    od

    return 1
end

global proc var_getix_record(variant a, int index)=
    object q
    word offset

    q:=a.objptr

    offset:=index-1
    if offset>=word(ttlength[q.usertag]) then
        pcerror("record[int] bounds")
    fi

    a^:=(q.varptr+offset)^
    var_share(a)
end

global proc var_putix_record(variant a, int index, variant x)=
    variant dest
    object q
    word offset

    q:=a.objptr

    if not q.mutable then pcerror("Not mutable") fi

    offset:=index-1
    if offset>=word(ttlength[q.usertag]) then
        pcerror("rec[int] bounds")
    fi

    dest:=q.varptr+offset
    var_unshare(dest)
    dest^:=x^               
end

global proc var_getixref_record(variant a, int index, variant dest)=
    variant p
    object q
    word offset

    q:=a.objptr

    offset:=index-1
    if offset>=word(q.length) then
        pcerror("^rec[int] bounds")
    fi

    p:=q.varptr+offset

    dest.tagx:=trefvar
    dest.varptr:=p
end

=== qq_resolve.m 0 0 24/43 ===
int nprocs

int noexpand
int symbolmode
int macrolevels
int allowmodname

const maxmacroparams=50
array [maxmacroparams]symbol macroparams
array [maxmacroparams]symbol macroparamsgen
array [maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

const maxstructfields=100
array [maxstructfields]symbol structfields
int ntopfields, nallfields

global proc rx_module(int n)=
    currmodule:=&moduletable[n]
    stcurrproc:=stcurrmodule:=currmodule.def
    nprocs:=0


    rx_passdef(stprogram, stcurrmodule)

    if nprocs=0 then
        rx_unit(stcurrmodule,currmodule.ast)
    elsif currmodule.ast then
        RX_UNIT(STCURRMODULE,CURRMODULE.AST)
    fi
end

global proc rx_passdef(symbol owner,p)=
    symbol d

    case p.nameid
    when moduleid,dllmoduleid then
        rx_deflist(p,p.deflist)

    when procid then
        ++nprocs
        fixmode(owner,p)
        rx_deflist(p,p.deflist)
        stcurrproc:=p
        rx_unit(p,p.code)
        stcurrproc:=stcurrmodule

    when dllprocid then
        fixmode(owner,p)
        rx_deflist(p,p.deflist)

    when constid,staticid,frameid,paramid then
        fixmode(owner,p)
        if p.code then
            rx_unit(owner,p.code)


        fi
    when typeid,recordid then
        fixmode(owner,p)
        rx_deflist(p,p.deflist)

    esac
end

global proc rx_deflist(symbol owner,p)=
    while p do
        rx_passdef(owner,p)
        p:=p.nextdef
    od
end

global proc rx_unit(symbol owner, unit p)=
    symbol d
    unit a,b
    int n, flags, oldnoexpand,oldsymbolmode, nk

    a:=p.a
    b:=p.b
    qpos:=p.pos


    switch p.tag
    when jname then
        resolvename(owner,p)
        if p.tag=jname and p.def.nameid=macroid and not noexpand then
            ++macrolevels
            expandmacro(p,p,nil)
            rx_unit(owner,p)
            --macrolevels
        fi

    when jkeyword then
        rx_unit(owner,b)        

    when jdot then
        resolvedot(owner,p)

    when jcall then
        if a.tag=jname then         
            oldnoexpand:=noexpand; noexpand:=1
            rx_unit(owner,a)
            noexpand:=oldnoexpand
        else
            rx_unit(owner,a)
        fi

        rx_unitlist(owner,b)

        if a.tag=jtypeconst then

            p.tag:=jconvert
            p.a:=b
            p.b:=nil
            p.mode:=a.mode

            if ttbasetype[a.mode]=tenumdef then
            else
                nk:=0
                p.a:=createunit1(jmakelist,b)
                n:=0
                while b do
                    if b.tag=jkeyword then
                        ++nk
                        b.tag:=jkeyvalue
                    fi
                    ++n
                    b:=b.nextunit
                od
                if nk and nk<>n then
                    rxerror("Mixed key:value")
                fi

                p.a.length:=n
            fi
        elsif a.tag=jname and a.def.nameid=macroid then
            ++macrolevels
            expandmacro(p,a,b)
            rx_unit(owner,p)
            --macrolevels
        fi

    when jadd, jsub, jmul, jdiv, jidiv, jirem, jiand, jior, jixor,
        jshl, jshr, jmakerange then
        rx_unit(owner,a)
        if not b then rxerror("Binop missing opnd") fi
        rx_unit(owner,b)
        evalbinop(p,a,b)

    when jneg, jabs,jlen, jlwb, jupb,jbytesize,jsqrt then
        rx_unit(owner,a)
        evalmonop(p)


    when jforup,jfordown then           
        resolvename(owner,a,tint)
        a:=a.nextunit
        goto doabc

    when jconvert then
        rx_unit(owner,a)

        evalmonop(p)

    when jsymbol then
        oldnoexpand:=noexpand
        oldsymbolmode:=symbolmode
        noexpand:=1
        symbolmode:=1

        rx_unit(owner,a)
        noexpand:=oldnoexpand
        symbolmode:=oldsymbolmode

        case a.tag
        when jname then
        when jtypeconst then
            d:=ttnamedef[a.mode]

            if d then
                a.def:=d
                a.tag:=jname
            else
                rxerror("T.$?")
            fi

        else
printunit(a)
            rxerror(".$ not name")
        esac

    when jstrinclude then
        rx_unit(owner,a)
        if a.tag<>jstringconst then
            rxerror("Not strconst")
        fi
        n:=getsupportfile(a.svalue,
            path:sourcefilepaths[moduletable[p.moduleno].fileno],
            issupport:1)
        a.svalue:=sourcefiletext[n]
        a.slength:=sourcefilesizes[n]
        deleteunit(p,a)

    else
doabc::

        flags:=jflags[p.tag]
        if flags>=1 then rx_unitlist(owner,a) fi
        if flags=2 then rx_unitlist(owner,b) fi
    endswitch
end

proc rx_unitlist(symbol owner, unit p)=
    while p do
        rx_unit(owner,p)
        p:=p.nextunit
    od
end

proc evalmonop(unit p)=
    int a,c
    real x,z


    case p.tag
    when jlen,jupb,jlwb then
    when jbytesize then
        if p.a.tag=jtypeconst then
            c:=ttsize[p.a.mode]
            newint
        fi
    
    elsecase p.a.tag
    when jintconst then
        a:=p.a.value

        switch p.tag
        when jneg then c:=-a
        when jabs then c:=abs(a)
        else
            return
        endswitch

newint::
        makeintconst(p,c)

    when jrealconst then
        x:=p.a.xvalue

        switch p.tag
        when jneg then z:=-x
        when jabs then z:=abs(x)
        else
            return
        endswitch

        makerealconst(p,z)
    else
        return 
    esac
end

proc evalbinop(unit p,lhs,rhs)=
    int a,b,c
    real x,y,z


    case pr(lhs.tag,rhs.tag)
    when pr(jintconst, jintconst) then
        a:=lhs.value
        b:=rhs.value

        switch p.tag
        when jadd then c:=a+b
        when jsub then c:=a-b
        when jmul then c:=a*b
        when jidiv then c:=a/b
        else
            return
        endswitch

        makeintconst(p,c)

    when pr(jrealconst, jrealconst) then
        x:=lhs.xvalue
        y:=rhs.xvalue

        switch p.tag
        when jadd then z:=x+y
        when jsub then z:=x-y
        when jmul then z:=x*y
        when jdiv then z:=x/y
        else
            return
        endswitch

        makerealconst(p,z)
    else
        return 
    esac
end

proc makeintconst(ref unitrec p,int64 value)=
    p.tag:=jintconst
    p.a:=p.b:=nil
    p.value:=value
    p.mode:=tint
end

proc makerealconst(ref unitrec p,real64 xvalue)=
    p.tag:=jrealconst
    p.a:=p.b:=nil
    p.xvalue:=xvalue
    p.mode:=treal
end

global proc resolvename(symbol owner, unit p, int mode=tvoid)=

    symbol d,e
    unit q
    int moduleno

    d:=p.def
    moduleno:=p.moduleno

    if d.nameid<>genericid then         
        return
    fi

    e:=resolvetopname(owner,d,moduleno,allowmodname)

    if not e then

        case owner.nameid
        when procid then            
            e:=p.def:=addsymbol(owner,d,frameid,0)
            e.mode:=tvar
        when moduleid then
            e:=p.def:=addsymbol(owner,d,staticid,0)
            e.mode:=tvar

        else
            rxerror_s("Undefined: #",d.name,p)
        esac
    else
$else::
retry::
        p.def:=e            

        case e.nameid
        when constid then       
IF SYMBOLMODE THEN
    RETURN
FI

            q:=e.code           
            rx_unit(owner,q)
            if q.tag not in [jintconst, jrealconst, jstringconst] then
                rxerror_s("Not const expr: #",jtagnames[q.tag])
            fi

            e.mode:=q.mode
            p.tag:=q.tag
            p.value:=q.value
            p.mode:=q.mode
            p.slength:=q.slength
        when enumid then
IF SYMBOLMODE THEN
    RETURN
FI
            if e.mode=tvoid then            
                p.tag:=jintconst
                p.value:=e.index
                p.mode:=tint
            else                            
                p.tag:=jenumconst
                p.value:=e.index
                p.mode:=e.mode
            fi

        when staticid then      
        when typeid,recordid then
            p.tag:=jtypeconst
            p.mode:=p.def.mode


        when linkid then
            rxerror("FOUND LINK",p)
        esac
    fi
end

global function resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol=

    int extcount,subprogno
    symbol p,q,powner,d,e,extdef,moddef
    array [10]symbol ambiglist

    if owner.nameid=procid then
        q:=owner.deflist
        while q, q:=q.nextdef do
            if q.firstdupl=stnewname then       
                return q
            fi
        od
    fi

    p:=stnewname.nextdupl
    subprogno:=moduletosub[moduleno]

    extcount:=0
    extdef:=moddef:=nil

    while p, p:=p.nextdupl do                       
        powner:=p.owner                             

        switch powner.nameid
        when moduleid then                          
            if powner.moduleno=moduleno then        
                return p
            elsif p.isglobal then   
                if moduletosub[powner.moduleno]=subprogno or        
                     p.isglobal=export_scope or
                     p.isimport then                
                    ++extcount          
                    extdef:=p
                    if extcount<ambiglist.len then
                        ambiglist[extcount]:=extdef
                    fi
                fi
            fi

        when typeid then                    
            if powner=owner or powner=owner.owner then      
                return p                    
            fi

        when programid then                 
            case p.nameid
            when moduleid, subprogid then   
                if allowmod then
                    moddef:=p
                fi
            when macroid then
                return p

            esac

        endswitch
    od


    if extdef then
        if extcount>1 then
            for i:=1 to extcount do
                extdef:=ambiglist[i]
                println i,extdef.owner.name,namenames[extdef.owner.nameid]
            od
            rxerror_s("Ambiguous ext name: #",extdef.name)
        fi
        return extdef
    fi

    return moddef               
end

proc resolvedot(symbol owner,unit p)=
    symbol qdef,rdef,d,newd,e,fielddef
    unit q,r
    int nfields,oldallowmod

    if symbolmode then
        resolvedot_sym(owner, p)
        return
    fi

    q:=p.a          
    r:=p.b          
    rdef:=r.def                         

    oldallowmod:=allowmodname
    allowmodname:=q.tag=jname
    rx_unit(owner,q)
    allowmodname:=oldallowmod

    case q.tag
    when jname then     

        d:=q.def
    when jtypeconst then    
        d:=q.def
        goto dotype
    else                    
        rdef:=r.def
        goto doexprdot
    esac

    switch d.nameid
    when dllmoduleid,moduleid,typeid,procid,dllprocid then  
dotype::

        newd:=finddupl(d, rdef)

        if newd then                    
            switch newd.nameid
            when enumid then            
                p.tag:=jintconst
                p.value:=newd.index
                p.mode:=tint
            when constid then
                q:=newd.code            
                case q.tag
                when jintconst then
                    p.tag:=jintconst
                    p.a:=p.b:=nil
                    p.value:=q.value
                    p.mode:=newd.mode

                else
                    rxerror("Rxdot:const?",p)
                esac
            when typeid then
                p.tag:=jtypeconst
                p.mode:=newd.mode
                p.def:=newd
            when staticid then
                p.tag:=jname
                p.def:=newd

            when procid,dllprocid then
                p.tag:=jname
                p.a:=p.b:=nil
                p.def:=newd
            when macroid then
                if e.nameid=macroid and not noexpand then
                    ++macrolevels
                    expandmacro(p,p,nil)
                    rx_unit(owner,p)
                    --macrolevels
                fi

            else
                cpl namenames[newd.nameid],,".",,newd.name
                rxerror("Rxdot:.name not allowed here",p)
            endswitch

        else
            cpl d.name,,".",,rdef.name
            rxerror("Can't resolve",p)
        fi

    when frameid, staticid, paramid, fieldid, structfieldid then    
doexprdot::
        nfields:=0
        fielddef:=nil
        e:=rdef.nextdupl
        while e do

            case e.nameid
            when fieldid,structfieldid, constid, procid, typeid, staticid, dllprocid then
                ++nfields
                fielddef:=e             
            esac
            e:=e.nextdupl
        od

        case nfields
        when 0 then             
            cpl rdef.name
            rxerror("Can't find field")
        else                    
            if rdef.nameid<>genericid then
                rxerror("Field name not generic")
            fi
        esac

    else
        cpl namenames[d.nameid]
        rxerror("RXDOT:Unknown nameid",p)
    endswitch
end

proc resolvedot_sym(symbol owner,unit p)=
    symbol qdef,rdef,d,newd,e,fielddef
    unit q,r
    int nfields, oldallowmod

    q:=p.a          
    r:=p.b          
    rdef:=r.def                         

    oldallowmod:=allowmodname
    allowmodname:=q.tag=jname
    rx_unit(owner,q)
    allowmodname:=oldallowmod

    case q.tag
    when jname then     

        d:=q.def
    when jtypeconst then    
        d:=q.def
        if symbolmode then
            newd:=finddupl(d, rdef)
            if newd=nil then
                rxerror_s("Can't resolve .",rdef.name)
            fi
            case newd.nameid
            when fieldid,structfieldid then
                CPL "*******FIELD.$"
            else
                rxerror_s(".$ ON type:",namenames[newd.nameid])
            esac

        fi
        goto dotype
    else                    
        rxerror("RXDOTSYM?")
    esac

    switch d.nameid
    when dllmoduleid,moduleid,typeid,procid,dllprocid then  
    dotype::
        newd:=finddupl(d, rdef)

        if newd then                    
            p.tag:=jname
            p.a:=p.b:=nil
            p.def:=newd
        else
            rxerror_s(".$ Can't resolve",d.name)
        fi
    else
        rxerror_s("RX.$: Unknown nameid:",namenames[d.nameid],p)
    endswitch
end

global function finddupl(symbol d, pdupl)symbol=

    if pdupl.nameid<>genericid then     
        return pdupl
    fi
    pdupl:=pdupl.nextdupl

    while pdupl do
        if pdupl.owner=d then

            return pdupl
        fi
        pdupl:=pdupl.nextdupl
    od

    return nil
end

proc expandmacro(unit p, a, b)=
    symbol d,pm
    unit pnew
    int ignoreargs

    if macrolevels>10 then
        rxerror("Too many macro levels (recursive macro?)")
    fi

    d:=a.def


    pm:=d.deflist
    nmacroparams:=0
    while pm do
        if nmacroparams>=maxmacroparams then
            rxerror("macro param overflow")
        fi
        macroparams[++nmacroparams]:=pm
        macroparamsgen[nmacroparams]:=pm.firstdupl      
        pm:=pm.nextdef
    od

    nmacroargs:=0

    while b do
        if nmacroargs>=maxmacroparams then
            rxerror("macro arg overflow")
        fi
        macroargs[++nmacroargs]:=b
        b:=b.nextunit
    od

    if nmacroargs<nmacroparams then
        rxerror("Too few macro args")
    fi

    ignoreargs:=0
    if nmacroargs>0 and nmacroparams=0 then     
        ignoreargs:=1
        nmacroargs:=nmacroparams:=0

    elsif nmacroargs>nmacroparams then
        rxerror("Too many macro args")
    fi

    pnew:=copyunit(d.code)

    if not ignoreargs then              
        replaceunit(p,pnew)
    else                                
        p.a:=pnew                       
    fi
end

function copylistunit(unit p)unit=
    unit q
    unit plist,plistx

    plist:=plistx:=nil
    while p do
        q:=copyunit(p)
        addlistunit(plist,plistx,q)
        p:=p.nextunit
    od
    return plist
end

function copyunit(unit p)unit=
    unit q
    symbol d

    if p=nil then return nil fi


    if p.tag=jname then
        d:=p.def
        for i to nmacroparams do
            if macroparamsgen[i]=d then
                return copyunit(macroargs[i])
                exit
            fi
        od
    fi

    q:=createunit0(p.tag)

    q^:=p^
    q.nextunit:=nil
    if jflags[q.tag] then
        q.a:=copylistunit(q.a)
        if jflags[q.tag]=2 then
            q.b:=copylistunit(q.b)
        fi
    fi
    return q
end

proc replaceunit(unit p,q)=
    unit pnext
    pnext:=p.nextunit
    p^:=q^
    p.nextunit:=pnext
end

proc fixmode(ref strec owner, p)=
    ref strec d,e
    int m

    m:=p^.mode

    if m>=0 then return fi
    m:=-m

    if ttxmap[m] then               
        p^.mode:=ttxmap[m]
        return
    fi

    if ttnamedefx2[m] then
        rxerror("Can't resolve a:b tentative types yet")
    fi

    d:=ttnamedefx[m]

    e:=resolvetopname(owner,d,ttxmoduleno[m],0)

    if e then
        ttxmap[m]:=e^.mode
        p^.mode:=e^.mode

    else
        rxerror_s("Can't resolve tentative type: #",d^.name)
    fi
end

function fixmode2(ref strec owner, int m)int=
    ref strec d,e
    array [256]char str

    if m>=0 then return m fi
    m:=-m

    if ttxmap[m] then               
        return ttxmap[m]
    fi

    if ttnamedefx2[m] then
        rxerror("2:Can't resolve a:b tentative types yet")
    fi

    d:=ttnamedefx[m]

    IF OWNER=NIL THEN
    CPL D^.NAME
    RXERROR("FIXMODE2 OWNER=0")
    FI

    e:=resolvetopname(owner,d,ttxmoduleno[m],0)

    if e then
        ttxmap[m]:=e^.mode
        return e^.mode
    else
        fprint @&.str,"# in module #, line:#",d^.name,moduletable[ttxmoduleno[m]].name

        rxerror_s("2:Can't resolve tentative type: #",&.str)
    fi
    return 0
end

global proc fixusertypes=
    ref userxrec p
    ref int pmode
    int m, rescan,i


    for i:=1 to 2 do
        p:=userxmodelist
        rescan:=0

        while p do
            m:=p^.pmode^
            if m<0 then
                m:=fixmode2(p^.owner,m)
                if m<0 and i=2 and ttxmap[abs m] then
                    m:=ttxmap[abs m]
                fi
                if m<0 then
                    rescan:=1
                else
                    p^.pmode^:=m


    IF TTTARGET[M]=M THEN
        CPL =TTNAME[M]
        RXERROR("RECURSIVE TYPE?")
    FI
                fi
            fi

            p:=p^.nextmode
        od
        if not rescan then exit fi

    od
    if rescan then
        RXERROR("FIXUSERTYPES PHASE ERROR")
    fi

    for i to nbaseclasses do
        dobaseclass(i)
    od
end

global proc tx_typetable=
    for i:=tlast+1 to ntypes do
        converttype(i)
    od
end

function getconstint(symbol owner,unit a, int ownerid=0)int=

    if a=nil then
        rxerror("GETCONSTINT A=NIL")
    fi

    rx_unit(owner, a)

    case a.tag
    when jintconst then
        return a.value
    when jrealconst then
        return a.xvalue
    when jlwb, jupb, jlen then
        goto $else

    else
$else::
        rxerror_s("Getconstint: not int/real",jtagnames[a.tag])
    esac
    return 0
end

global proc converttype(int m)=
    symbol d,f,owner
    int first,a,b,index,length,lower, elemtype, nbits
    const int maxfield=256
    array [maxfield+1]symbol fieldlist
    int oldmodno,pos,ownerid
    int maxalign, nfields, size
    unit plength, plower

    if ttsize[m] then return fi         


    owner:=ttowner[m]
IF OWNER=NIL THEN
CPL =TTNAMEDEF[M],STRMODE(M)

CPL("CONVTYPE OWNER=NIL")
FI

    plower:=ttlowerexpr[m]
    plength:=ttlengthexpr[m]

    case ttbasetype[m]
    when tpackstrc,tpackstrz then
        ttsize[m]:=ttlength[m]:=getconstint(owner,plength)
    when tpackarray then
        if m=tarray then CPL "CT:ARRAY/ARRAY" fi
        if ttowner[m] then
            ownerid:=ttowner[m].nameid
        else
            ownerid:=0
        fi
        if plower then
            ttlower[m]:=getconstint(owner,plower,ownerid)
        else
            ttlower[m]:=1
        fi

        if plength then

            ttlength[m]:=getconstint(owner,plength, ownerid)
        else
            ttlength[m]:=0
        fi
        elemtype:=tttarget[m]

        case elemtype
        when tu1,tu2,tu4 then
            nbits:=ttlength[m]*ttbitwidth[tttarget[m]]
            ttsize[m]:=(nbits-1)/8+1
        else
            converttype(tttarget[m])
            ttsize[m]:=ttlength[m]*ttsize[tttarget[m]]
            case ttsize[m]
            when 8 then
                ttcat[m]:=u64cat
            when 1,2,4 then
                ttcat[m]:=shortcat
            esac
        esac

    when tpackrecord then
        d:=ttnamedef[m]
        f:=d.deflist

        nfields:=0
        while f do
            if nfields>=maxfield then rxerror("Too many fields") fi
            fieldlist[++nfields]:=f
            f:=f.nextdef
        od

        fieldlist[nfields+1]:=nil
        ntopfields:=nallfields:=0
        maxalign:=1
        index:=1

        scanstruct(1, fieldlist, index, size, 0, ttcaligned[m], maxalign, 2)

        if ttcaligned[m] then
            size:=roundoffset(size,maxalign)
            d.maxalign:=maxalign
        else
            d.maxalign:=1
        fi

        ttsize[m]:=size
        ttlower[m]:=1
        ttlength[m]:=ntopfields

        d.topfieldlist:=pcm_alloc(symbol.bytes*ntopfields)
        memcpy(d.topfieldlist,&structfields,symbol.bytes*ntopfields)

    when trecorddef, tenumdef then
    else
        CPL "CAN'T DO:",STRMODE(M),strmode(ttbasetype[m])
    esac
end

proc scanstruct(int smode, []symbol &fields, int &index, &isize, offset,
    calign, &maxalign, countmode)=
    symbol f
    int newoffset, fieldsize, alignment
    int nfields, structmode, ndepth, size

    size:=0

    while f:=fields[index++] do
        case f.nameid
        when structfieldid then
            converttype(f.mode)
            fieldsize:=ttsize[f.mode]

            if calign then
                alignment:=getalignment(f.mode)
                maxalign max:=alignment
                newoffset:=roundoffset(offset, alignment)
                size+:=newoffset-offset
            else
                newoffset:=offset
            fi
            f.fieldoffset:=newoffset
            offset:=newoffset
countfields::
            ++nallfields
            if countmode then
                structfields[++ntopfields]:=f

            fi
        when structblockid then
            scanstruct(1, fields, index, fieldsize, offset, calign, maxalign,countmode)

        when unionblockid then
            scanstruct(0, fields, index, fieldsize, offset, calign, maxalign, (countmode|1|0))

        when endblockid then
            isize:=size
            return
        esac

        if smode then
            offset+:=fieldsize
            size+:=fieldsize
        else
            size:=max(size,fieldsize)
            countmode:=0
        fi

    od

    isize:=size             
end

proc dobaseclass(int baseclassindex)=
    ref strec sttype,d,e,newd
    int baseclass,normalexit

    baseclass:=baseclasstable[baseclassindex]
    sttype:=baseclassdef[baseclassindex]

    d:=ttnamedef[baseclass].deflist
    while d do
        e:=sttype.deflist
        normalexit:=1
        while e do
            if eqstring(d.name,e.name) then
                normalexit:=0
                exit
            fi
            e:=e.nextdef
        od
        if normalexit then
            case d.nameid
            when procid,linkid then
                newd:=addsymbol(sttype,d.firstdupl,linkid,0)
                newd.alias:=d
            else
                newd:=addsymbol(sttype,d.firstdupl,d.nameid,0)
                duplfield(d,newd)
                ++sttype.nfields
                ttlength[sttype.mode]:=sttype.nfields
                newd.index:=sttype.nfields
                newd.fieldoffset:=(newd.index-1)*varsize
            esac
            addgenfield(newd)


        fi
        d:=d.nextdef
    od
end
=== qq_sets.m 0 0 25/43 ===
global proc obj_free_set(object p)=
    if p.length then
        pcm_free(p.ptr, getbitssize(p.alloc64, tu1))
    fi
    pcm_free32(p)
end

global proc var_dupl_set(variant a)=
    object p:=a.objptr
    object q
    int nbytes, nbits:=p.length

    q:=obj_newset(nbits)    

    if nbits then
        memcpy(q.ptr, p.ptr, getbitssize(nbits, tu1))
    fi

    a.objptr:=q
end

global function var_equal_set(variant x,y)int=
    int xbytes:=getsetbytes(x)
    int ybytes:=getsetbytes(y)
    if xbytes<>ybytes then return 0 fi

    return eqbytes(x.objptr.ptr, y.objptr.ptr, xbytes)
end

function getsetbytes(variant x)int=
    int nbits:=x.objptr.length
    if nbits then
        if nbits iand 7 then
            return nbits/8+1
        else
            return nbits/8
        fi
    else
        return 0
    fi
end

global proc var_make_set(variant data, dest, int n) =

    variant q
    ref byte p
    int top,a,b,i,j,t,size
    byte alloc
    object s
    static int count=0

    if n=0 then
        var_emptyset(dest)
        return
    fi

    top:=0
    q:=data

    to n do
        switch q.tag        
        when trange then

            a:=q.range_lower
            b:=q.range_upper

        when tint then
            a:=q.value
            b:=a
        
        else            
            b:=a:=var_getintvalue(q)
        endswitch
        if a<0 or b<0 then
            pcerror("Neg set element")
        fi
        if a>top then
            top:=a
        fi
        if b>top then
            top:=b
        fi
        ++q
    od

    s:=obj_newset(top+1)

    q:=data
    to n do
        switch q.tag
        when trange then
            a:=q.range_lower
            b:=q.range_upper
            if a>b then
                swap(a,b)
            fi

        when tint then
            b:=a:=q.value

        else
            b:=a:=var_getintvalue(q)
        endswitch

        for j:=a to b do
            setelem(cast(s.ptr),j)
        od
        ++q
    od

    var_objtovar(tset,s,dest)
end

global function obj_newset(int length)object p=

    ref byte q
    int nbits,nbytes

    p:=obj_new()
    p.mutable:=1
    p.length:=length

    nbytes := ((length-1)/64+1)*8       

    if length then
        p.ptr := pcm_alloc(nbytes)              
        p.alloc64:=word64(allocbytes)*8
        pcm_clearmem(p.ptr,allocbytes)
    else
        p.ptr:=nil
    fi

    return p
end

global proc var_emptyset(variant dest)=
    var_objtovar(tset,obj_newset(0),dest)
end

global proc var_getix_set(variant a, int index)=
    object p

    p:=a.objptr

    if u64(index)>=u64(p.length) then
        pcerror("set[int] bounds")
    fi

    a.tagx:=tint
    a.value:=not not ((p.ptr+index>>3)^ iand (1<<(index iand 7)))
end

global proc var_putix_set(variant a, int index, variant x)=
    object p
    ref byte q
    int newoffset

    p:=a.objptr
    if not p.mutable then pcerror("Not Mutable") fi

    if u64(index)>=u64(p.length) then
        if index<0 then
            pcerror("lwb")
        else
            pcerror("set[i]:=x bounds")
        fi
    fi

    q:=getoffset(p.ptr, index, newoffset)

    var_storebit(q, newoffset,x,tu1,0)
end

global proc var_getixref_set(variant a, int index)=
    varrec v
    object p
    ref byte q
    int offset, newoffset

    p:=a.objptr
    if not p.mutable then pcerror("Not Mutable") fi

    if u64(index)>=u64(p.length) then
        pcerror("&set[i] bounds")
    fi

    q:=getoffset(p.ptr, index,  newoffset)

    a.tagx:=trefbit
    a.elemtag:=tu1
    a.ptr:=q
    a.bitoffset:=newoffset
end

function getoffset(ref byte p, int index, &newoffset)ref byte=

    p+:=index>>3                
    newoffset:=index iand 7

    return p
end

global function var_in_set(variant a,b)int =
    int i:=a.value,m
    static [0:]byte masks=(1,2,4,8,16,32,64,128)
    object p

    p:=b.objptr

    if u64(i)>=u64(p.length) then
        return 0
    fi

    if  (p.ptr+i>>3)^ iand masks[i iand 7] then
        return 1
    else
        return 0
    fi
end

global proc iresizeset(variant p,int n)=
    object pp

    pp:=p.objptr

    if pp.length>=n then        
        return
    fi

    obj_resize_set(pp,n)
end

global proc obj_resize_set(object p,int n)=
    ref byte q
    int newsize,elemtype

    elemtype:=p.elemtag

    if n<=p.alloc64 then
        p.length:=n
    else
        newsize:=getbitssize(n,tu1)
        q:=pcm_allocz(newsize)
        if p.length then
            memcpy(q,p.ptr, getbitssize(p.length,tu1))
            pcm_free(p.ptr, getbitssize(p.alloc64, tu1))
        fi
        p.ptr:=q
        p.length:=n
        p.alloc64:=allocbytes*8
    fi
end

global proc iorsetbits(ref int p,q,int n)=
    to (n-1)/64+1 do
        p++^ ior:= q++^
    od
end

global proc ixorsetbits(ref int p,q,int n)=
    to (n-1)/64+1 do
        p++^ ixor:= q++^
    od
end

global proc iandsetbits(ref word p,q,int n)=
    to (n-1)/64+1 do
        p++^ iand:= q++^
    od
end

global proc inotsetbits(ref word p,int n)=
    to (n-1)/64+1 do
        p^ :=inot p^
        ++p
    od
end

global proc var_iorto_set(variant x,y) =
    int xlen,ylen
    int n,i
    ref int p
    object px,py
    ref byte pp

    px:=x.objptr
    py:=y.objptr

    xlen:=px.length
    ylen:=py.length

    if ylen=0 then          
    elsif xlen=0 then       
        x^:=y^
        var_dupl_set(x)
    else
        px:=x.objptr

        iresizeset(x,ylen)      

        iorsetbits(cast(px.ptr),cast(py.ptr),ylen)

    fi
end

global proc var_iandto_set(variant x,y) =
    int xlen,ylen
    int n,i
    ref int p
    object px,py
    ref byte pp

    px:=x.objptr
    py:=y.objptr

    xlen:=px.length
    ylen:=py.length

    if ylen=0 then              
        var_emptyset(x)
    elsif xlen=0 then           
    else                        
        px:=x.objptr

        iresizeset(x,ylen)      

        iandsetbits(cast(px.ptr),cast(py.ptr),ylen)
    fi
end

global proc var_ixorto_set(variant x,y) =
    int xlen,ylen
    int n,i
    ref int p
    object px,py
    ref byte pp

    px:=x.objptr
    py:=y.objptr

    xlen:=px.length
    ylen:=py.length

    if ylen=0 then              
        var_emptyset(x)
    elsif xlen=0 then           
        x^:=y^
        var_dupl_set(x)
    else                        
        px:=x.objptr

        iresizeset(x,ylen)      

        ixorsetbits(cast(px.ptr),cast(py.ptr),ylen)
    fi
end

global proc var_inotto_set(variant x) =
    int xlen
    object px,py

    px:=x.objptr

    xlen:=px.length

    if xlen then                
        inotsetbits(cast(px.ptr),xlen)
    fi
end

=== qq_strings.m 0 0 26/43 ===
global object emptystring

proc start=
    emptystring:=obj_new()
    emptystring.refcount:=1
    emptystring.objtype:=normal_obj
end

global proc var_empty_string(variant dest, int mutable=0)=
    dest.tagx:=tstring ior hasrefmask
    if not mutable then
        dest.objptr:=emptystring
        ++emptystring^.refcount
    else
        dest.objptr:=obj_make_stringn(nil, 0,1)
    fi
end

global proc var_make_string(ichar s, variant dest, int mutable=0)=
    dest.tagx:=tstring ior hasrefmask
    dest.objptr:=obj_make_string(s,mutable)
end

global proc var_make_stringn(ichar s, int length, variant dest, int mutable=0)=
    dest.tagx:=tstring ior hasrefmask
    dest.objptr:=obj_make_stringn(s,length,mutable)
end

global function obj_new_string(int n)object p=
    p:=obj_new()
    p.mutable:=1
    p.length:=n
    p.objtype:=normal_obj

    if n then
        p.strptr:=pcm_alloc(n)
        p.alloc64:=allocbytes
    fi

    return p
end

global function obj_make_string(ichar s,int mutable=0)object p=
    int n

    p:=obj_new_string(n:=strlen(s))
    p.mutable:=mutable

    if n then
        memcpy(p.strptr,s,n)
    fi
    return p
end

global function obj_make_stringn(ichar s,int length,mutable=0)object p=

    p:=obj_new_string(length)
    p.mutable:=mutable

    if length then
        if s then
            memcpy(p.strptr,s,length)
        else
            memset(p.strptr,0,length)
        fi

    fi
    return p
end

global proc obj_free_string(object p)=
    variant q

    if p.length then
        pcm_free(p.strptr,p.alloc64)
    fi

    pcm_free32(p)
end

global proc var_dupl_string(variant a)=
    object p,q

    p:=a.objptr
    q:=obj_new_string(p.length)
    a.objptr:=q

    if q.length then
        memcpy(q.strptr,p.strptr,q.length)
    fi
end

global proc var_getix_string(variant a, int index)=
    object q

    q:=a.objptr

    if word(index-1)>=word(q.length) then
        pcerror("getstring[int] bounds")
    fi

    stringslice(a,index,index,a)
end

global proc var_getixref_string(variant a, int index)=
    object q

    q:=a.objptr

    if word(index-1)>=word(q.length) then
        pcerror("getixref[int] bounds")
    fi

    a.tagx:=trefpack
    a.elemtag:=tu8
    a.ptr:=cast(q.strptr+index-1)
end

global proc var_getdotix_string(variant a, int index)=
    object q

    q:=a.objptr

    if word(index-1)>=word(q.length) then
        pcerror("x.[] bounds")
    fi

    a.tagx:=tint
    a.value:=(q.strptr+index-1)^
end

global proc var_getdotixref_string(variant a, int index,variant dest)=
    object q

    q:=a.objptr

    --index

    if word(index)>=word(q.length) then
        pcerror("x.[] bounds")
    fi

    dest.tagx:=trefpack
    dest.elemtag:=tu8
    dest.ptr:=q.strptr+index
end

global proc var_getslice_string(variant a, int i,j)=
    object p:=a.objptr

    if i<1 or j>p.length or i>j then
        pcerror("string/slice bounds")
    fi

    stringslice(a,i,j,a)
end

proc stringslice(variant a, int i,j, variant dest)=
    object p,q

    p:=a.objptr

    q:=obj_new()
    q.mutable:=p.mutable
    q.length:=j-i+1
    q.objtype:=slice_obj

    case p.objtype
    when slice_obj then             
        q.objptr2:=p.objptr2        
        ++q.objptr2.refcount
    when extslice_obj then
        q.objptr2:=nil
        q.objtype:=extslice_obj
    else
        ++p.refcount
        q.objptr2:=p                
    esac
    q.strptr:=p.strptr+i-1

    dest.tagx:=a.tagx
    dest.objptr:=q
end

global proc var_putix_string(variant a, int index, variant x)=
    ichar s
    object p,q
    int length

    p:=a.objptr
    if not p.mutable then pcerror("Str not mutable") fi
    length:=p.length

    if index not in 1..length then
        if index=length+1 then
            var_addto_string(a,x)
            return
        else
            pcerror("putstring[int] bounds")
        fi
    fi

    s:=p.strptr+index-1
    if x.tag<>tstring then
        pcerror("s[i]:= not str")
    fi
    q:=x.objptr
    if q.length=0 then pcerror("s[i]:=""""") fi
    s^:=q.strptr^
end

global proc var_putslice_string(variant a, int i,j, variant x)=
    ichar s
    object p,q
    int length,sublength

    p:=a.objptr
    if not p.mutable then pcerror("Str not mutable") fi
    length:=p.length

    if i<1 or j>p.length or i>j then
        pcerror("string/slice bounds")
    fi
    sublength:=j-i+1

    s:=p.strptr+i-1
    if x.tag<>tstring then
        pcerror("s[i..j]:= not str")
    fi
    q:=x.objptr
    if q.length<sublength then
        pcerror("substr too short")
    fi
    memcpy(s,q.strptr, sublength)
end

global proc var_putdotix_string(variant a, int index, variant x)=
    ichar s
    object p,q
    int length,ch

    if x.tag<>tint then
        pcerror("s.[i]:= not int")
    fi
    ch:=x.value

    p:=a.objptr
    if not p.mutable then pcerror("Str not mutable") fi
    length:=p.length

    if index not in 1..length then
        if index=length+1 then
            var_addto_string_ch(a,ch)
            return
        else
            pcerror("str.[int] bounds")
        fi
    fi

    (p.strptr+index-1)^:=ch
end

global proc obj_resize_string(object p,int n)=
    ichar s
    int oldalloc

    if n<=p.alloc64 then
        p.length:=n
    else
        oldalloc:=p.alloc64
        s:=pcm_alloc(n)
        p.alloc64:=allocbytes
        if p.length then
            memcpy(s,p.strptr,p.length)

            pcm_free(p.strptr, oldalloc)
        fi

        p.strptr:=s
        p.length:=n
    fi
end

global proc var_add_string(variant a,b)=
    object p:=a.objptr
    object q:=b.objptr
    object r

    int alen:=p.length
    int blen:=q.length
    int newlen

    if blen=0 then
        var_shareu(a)
        return
    elsif alen=0 then
        var_make_stringn(q.strptr,blen,a,1)
        return
    fi

    newlen:=alen+blen
    r:=obj_new_string(newlen)
    memcpy(r.strptr, p.strptr, alen)
    memcpy(r.strptr+alen, q.strptr, blen)

    a.objptr:=r
end

global proc var_addto_string(variant a,b)=

    object p:=a.objptr
    object q:=b.objptr


    int alen:=p.length
    int blen:=q.length
    int newlen

    if not p.mutable then
        PCERROR("ADDTOSTR/NOT MUT")
    FI

    if blen=0 then
        return
    elsif alen=0 then           
        var_unshareu(a)
        a^:=b^
        var_duplu(a)
        return
    fi

    newlen:=alen+blen
    obj_resize_string(p,newlen)
    memcpy(p.strptr+alen, q.strptr, blen)
end

global proc var_addto_string_ch(variant a,int ch)=
    object p:=a.objptr
    int alen:=p.length, n
    array [32]char str
    ichar s

    if not p.mutable then
        PCERROR("ADDTOSTR/ch/NOT MUT")
    FI

    obj_resize_string(p,alen+1)
    (p.strptr+alen)^:=ch

end


global function var_equal_string(variant x,y)int =
    int n,res
    object px,py

    px:=x.objptr
    py:=y.objptr
    if px=py then return 1 fi

    n:=px.length

    if n<>py.length then
        0               
    elsif n=0 then
        1               
    else
        eqbytes(px.strptr,py.strptr,n)
    fi
end

global function var_compare_string(variant x,y)int =
    int res
    object px,py

    px:=x.objptr
    py:=y.objptr

    res:=cmpstring_len(px.strptr, py.strptr, px.length, py.length)
    return res
end

function cmpstring_len(ref char s,t,int slen,tlen)int =

    if slen=0 then
        if tlen=0 then
            return 0        
        else
            return -1       
        fi
    elsif tlen=0 then   
        return 1
    else
        if slen=tlen then
            if slen=1 then
                if s^<t^ then return -1
                elsif s^>t^ then return 1
                else
                    return 0
                fi
            fi
            return cmpstringn(s,t,slen)
        else
            return cmpstring(convCstring(s,slen),convCstring(t,tlen))
        fi
    fi
end

global function var_in_string(variant x,y)int =
    int xlen,ylen,result,i,j,k
    ref char sx, sy
    object px,py

    px:=x.objptr
    py:=y.objptr

    xlen:=px.length
    ylen:=py.length

    if xlen=0 or ylen=0 then        
        return 0
    fi

    k:=ylen-xlen
    for i:=0 to k do            
        sx:=px.strptr
        sy:=py.strptr+i
        for j:=1 to xlen do         
            if sx^<>sy^  then
                goto nextpos
            fi
            ++sx; ++sy
        od
        return i+1
nextpos::
    od
    return 0
end

global proc var_iconvcase(variant a,b,int upper)=
    int i,n
    ref char s
    object pa

    pa:=a.objptr

    if b.tag>tvoid then     
        n:=var_getintvalue(b)
    else
        n:=pa.length            
    fi

    if a.tag<>tstring then
        pcerror("convcase/notstr")
    fi

    if n<0 then
        pcerror("CONVCASE N<0")
    fi

    if n=0 then
        return
    fi

    if n>pa.length then
    cpl =n,pa.length
        pcerror("convcase/N?")
    fi
    s:=pa.strptr

    if upper then
        to n do
            s^:=toupper(s^)
            ++s
        od
    else
        to n do
            s^:=tolower(s^)
            ++s
        od
    fi
end

global proc var_makestrslicexobj(ichar s, int length, variant dest)=
    dest.tagx:=tstring ior hasrefmask
    dest.objptr:=obj_make_strslicexobj(s,length)
end

global function obj_make_strslicexobj(ichar s, int length)object=
    object p

    if length=0 then s:=nil fi

    p:=obj_new()
    p.strptr:=s
    p.mutable:=1
    p.length:=length
    p.objtype:=extslice_obj     
    return p
end

function var_asc(variant a)int=
    object p
    if a.tag<>tstring then pcerror("Asc:not str") fi
    p:=a.objptr
    if p.length<1 then pcerror("Asc:empty") fi

    return p.strptr^
end

global proc var_new_string(variant a, b, dest)=
    int length:=var_getintvalue(a)
    int ch

    if length<0 then pcerror("Length<0") fi

    var_make_stringn(nil,length,dest)

    case b.tag
    when tint then
        ch:=b.value
    when tstring then
        ch:=var_asc(b)
    when tvoid then
        ch:=' '
    else
        pcerror("Not int/str")
    esac

    if length then
        memset(dest.objptr.strptr,ch,length)
    fi
end

global proc var_new_stringn(int length, variant dest)=
    if length<0 then pcerror("Length<0") fi

    var_make_stringn(nil,length,dest)
end

global proc var_mul_string(variant a, int m)=

    int i,oldlen,newlen
    ref char newptr,p
    varrec v
    object pa,s

    if m<0 then
        pcerror("neg str mul")

    elsif m=0 then      
        var_empty_string(a)
        return

    elsif m=1 then      
        var_shareu(a)   
        return

    else                
        pa:=a.objptr
        oldlen:=pa.length
        if oldlen then          
            newlen:=oldlen*m

            v.objptr:=obj_new_string(newlen)
            v.tagx:=tstring ior hasrefmask

            p:=v.objptr.strptr
            to m do
                memcpy(p,pa.strptr,oldlen)
                p+:=oldlen
            od
            a^:=v
        else                
            var_empty_string(a)
            return
        fi
    fi
end

global proc var_convert_string_list(variant a, int t, variant dest)=
    object p:=a.objptr
    variant q
    int length:=p.length
    ichar s

    var_make_list(nil, dest, length, 1)
    q:=dest.objptr.varptr
    s:=p.strptr

    to length do
        var_make_stringn(s,1, q,1)
        ++s
        ++q
    od
end

global proc var_expand_string(variant a, dest, int m)=
    variant b,c
    object p
    ref char s
    int n

    p:=a.objptr
    b:=dest
    s:=p.strptr
    n:=1

    to m do
        if n>p.length then
            var_empty_string(dest)
        else
            var_make_stringn(s,1, dest,1)
            ++s
        fi
        ++n
        --+dest
    od
end

global proc var_makechar(int ch,variant dest)=
    varrec v
    array [8]char str
    object p

    if ch not in 0..255 then
        pcerror("chr range")
    fi

    p:=chrtable[ch]
    if p=nil then           
        str[1]:=ch
        str[2]:=0
        var_make_stringn(str,1,&v,0)
                chrtable[ch]:=p:=v.objptr
    fi
    ++p.refcount

    dest.tagx:=tstring ior hasrefmask
    dest.objptr:=p
end

=== qq_syslibs.m 0 0 27/43 ===
const fsyslibs = 1

global tabledata []ichar syslibnames,[]ichar libtext =
    ("sysp.q",          (fsyslibs | strinclude "sysp.q" | "" )),
    ("clibp.q",         (fsyslibs | strinclude "clibp.q" | "" )),
    ("smlib.q",         (fsyslibs | strinclude "smlib.q" | "" )),
    ("winapi.q",        (fsyslibs | strinclude "winapi.q" | "" )),
    ("gxlib.q",         (fsyslibs | strinclude "gxlib.q" | "" )),
    ("bmlib.q",         (fsyslibs | strinclude "bmlib.q" | "" )),
    ("console.q",       (fsyslibs | strinclude "console.q" | "" )),
    ("winconsts.q",     (fsyslibs | strinclude "winconsts.q" | "" )),
    ("wingxlib.q",      (fsyslibs | strinclude "wingxlib.q" | "" )),
    ("winmessages.q",   (fsyslibs | strinclude "winmessages.q" | "" )),
    ("gxmisc.q",        (fsyslibs | strinclude "gxmisc.q" | "" )),
    ("dates.q",         (fsyslibs | strinclude "dates.q" | "" )),
end

array [syslibnames.len]byte syslibfileno

global function findsyslib(ichar filename)int=

    if not fsyslibs then return 0 fi

    filename:=extractfile(filename)     

    for i to syslibnames.len do
        if eqstring(syslibnames[i],filename) then
            if syslibfileno[i] then
                return syslibfileno[i]
            fi

            if nsourcefiles>=maxsourcefile then loaderror("fsl: too many files") fi
            ++nsourcefiles
            sourcefilenames[nsourcefiles]:=pcm_copyheapstring(filename)
            sourcefiletext[nsourcefiles]:=libtext[i]
            sourcefilesizes[nsourcefiles]:=strlen(libtext[i])
            sourcefilepaths[nsourcefiles]:=""
            sourcefilespecs[nsourcefiles]:=""
            sourcefilesys[nsourcefiles]:=1
            sourcefilesupport[nsourcefiles]:=0

            syslibfileno[i]:=nsourcefiles
            return nsourcefiles
        fi
    od

    return 0
end
=== qq_tables.m 0 0 28/43 ===
global enumdata [0:]ichar stdtypenames,
                    [0:]byte stdtypewidths,
                    [0:]byte stdvar,        
                    [0:]byte stdpack,       
                    [0:]byte stdmvar,       
                    [0:]byte stdffi =       
    (tvoid=0,       $,              0,  1,0,0,0),   
    (tvar,          $,              0,  0,0,0,0),   

    (tu1,           "u1",           1,  0,0,0,0),
    (tu2,           "u2",           2,  0,0,0,0),
    (tu4,           "u4",           4,  0,0,0,0),

    (tpackstrc,     "packstrc",     0,  0,1,0,0),   
    (tpackstrz,     "packstrz",     0,  0,1,0,0),   

    (tpackarray,    "packarray",    0,  0,1,1,0),   
    (tpackrecord,   "packrecord",   0,  0,1,1,0),   
    (tslice,        "slice",        0,  0,0,1,0),   

    (tstringz,      "stringz",      64, 0,1,1,1),   
    (trefproc,      "refproc",      64, 0,1,1,1),   

    (trecorddef,    "recdef",        0, 0,0,0,0),   
    (tenumdef,      "enumdef",       0, 0,0,0,0),   

    (ttype,         "type",         64, 1,1,1,1),   
    (toperator,     "operator",     64, 1,0,0,0),   
    (tretaddr,      "retaddr",      0,  1,0,0,0),   
    (texception,    "except",       0,  1,0,0,0),   
    (tsymbol,       "symbol",       64, 1,0,0,0),   
    (trefpack,      "refpack",      64, 1,1,1,1),   

    (ti8,           "i8",           8,  0,1,0,1),
    (ti16,          "i16",          16, 0,1,0,1),
    (ti32,          "i32",          32, 0,1,0,1),
    (tu8,           "u8",           8,  0,1,0,1),
    (tu16,          "u16",          16, 0,1,0,1),
    (tu32,          "u32",          32, 0,1,0,1),
    (tr32,          "r32",          32, 0,1,0,1),

    (ti64,          "int",          64, 1,1,1,1),   
    (tu64,          "word",         64, 1,1,1,1),   
    (tr64,          "real",         64, 1,1,1,1),   

    (tdecimal,      "decimal",      0,  1,0,0,0),
    (tbool,         "bool",         64, 1,1,1,1),   
    (trange,        "range",        64, 1,0,0,0),   
    (tenum,         "enum",         64, 1,1,1,1),   
    (tset,          "set",          0,  1,0,0,0),   
    (tdict,         "dict",         0,  1,0,0,0),   

    (tuserarray,    "userarray",    0,  1,1,0,0),   
    (tbits,         "bits",         0,  1,0,0,0),   

    (tstring,       "string",       0,  1,0,0,0),   
    (tlist,         "list",         0,  1,0,0,0),   
    (tarray,        "array",        0,  1,0,0,0),   

    (trecord,       "record",       0,  1,0,0,0),   
    (tstruct,       "struct",       0,  1,1,0,1),   

    (trefvar,       "refvar",       64, 1,0,0,0),   
    (trefbit,       "refbit",       128,1,0,0,0),   

end

global const tlast=stdtypenames.upb

global const tint=ti64
global const tword=tu64
global const treal=tr64

global const tlastvartag=trefbit

global enumdata [0:]ichar jtagnames,            
                    [0:]ichar jshortnames,      
                    [0:]byte jflags,            
                    [0:]int16 jpclcodes,        
                    [0:]int16 jtocodes,         
                    [0:]byte jhasvalue =        
    (jnone=0,           $,      nil,        0,      0,          0,  0), 
    (jlabeldef,         $,      nil,        1,      0,          0,  0),
    (jassign,           $,      nil,        2,      0,          0,  2),
    (jdeepcopy,         $,      nil,        2,      0,          0,  2),
    (jkeyword,          $,      nil,        2,      0,          0,  1),
    (jkeyvalue,         $,      nil,        2,      0,          0,  1),
    (jdocstring,        $,      nil,        1,      0,          0,  0),
    (joperator,         $,      nil,        0,      0,          0,  1),
    (jblock,            $,      nil,        1,      0,          0,  2),
    (jif,               $,      nil,        2,      0,          0,  2),
    (jselect,           $,      nil,        2,      0,          0,  2),
    (jwhenthen,         $,      nil,        2,      0,          0,  0),
    (jcase,             $,      nil,        2,      0,          0,  2),
    (jdocase,           $,      nil,        2,      0,          0,  0),
    (jswitch,           $,      nil,        2,      0,          0,  2),
    (jdoswitch,         $,      nil,        2,      0,          0,  0),
    (jrecase,           $,      nil,        1,      0,          0,  0),
    (jforup,            $,      nil,        2,      0,          0,  0),
    (jforupx,           $,      nil,        2,      0,          0,  0),
    (jfordown,          $,      nil,        2,      0,          0,  0),
    (jfordownx,         $,      nil,        2,      0,          0,  0),
    (jforall,           $,      nil,        2,      0,          0,  0),
    (jforallrev,        $,      nil,        2,      0,          0,  0),
    (jforeach,          $,      nil,        2,      0,          0,  0),
    (jdo,               $,      nil,        1,      0,          0,  0),
    (jdoonce,           $,      nil,        1,      0,          0,  0),
    (jto,               $,      nil,        2,      0,          0,  0),
    (jwhile,            $,      nil,        2,      0,          0,  0),
    (jrepeat,           $,      nil,        2,      0,          0,  0),
    (jtry,              $,      nil,        2,      0,          0,  0),
    (jexcept,           $,      nil,        2,      0,          0,  0),
    (jraise,            $,      nil,        1,      0,          0,  0),
    (jcall,             $,      nil,        2,      0,          0,  1),
    (jcallhost,         $,      nil,        1,      0,          0,  1),
    (jnil,              $,      nil,        0,      0,          0,  1),
    (jpnil,             $,      nil,        0,      0,          0,  0),
    (jswap,             $,      nil,        2,      0,          0,  0),
    (jgoto,             $,      nil,        1,      0,          0,  0),
    (jstop,             $,      nil,        1,      0,          0,  0),
    (jreturn,           $,      nil,        1,      0,          0,  2),
    (jtypeconst,        $,      nil,        0,      0,          0,  1),
    (jeval,             $,      nil,        1,      0,          0,  0),

    (jconvert,          $,      nil,        1,      0,          0,  1),
    (jtypepun,          $,      nil,        1,      0,          0,  1),
    (jmap,              $,      nil,        2,      kmaps,      0,  1),

    (jcmpchain,         $,      nil,        1,      0,          0,  1),
    (jname,             $,      nil,        0,      0,          0,  1),
    (jsymbol,           $,      nil,        1,      0,          0,  1),
    (jhostname,         $,      nil,        0,      0,          0,  1),
    (jintconst,         $,      nil,        0,      0,          0,  1),
    (jwordconst,        $,      nil,        0,      0,          0,  1),
    (jint128const,      $,      nil,        0,      0,          0,  1),
    (jword128const,     $,      nil,        0,      0,          0,  1),
    (jrealconst,        $,      nil,        0,      0,          0,  1),
    (jboolconst,        $,      nil,        0,      0,          0,  1),
    (jenumconst,        $,      nil,        0,      0,          0,  1),
    (jstringconst,      $,      nil,        0,      0,          0,  1),
    (jstrinclude,       $,      nil,        1,      0,          0,  1),
    (jdot,              $,      nil,        2,      0,          0,  1),
    (jindex,            $,      nil,        2,      0,          0,  1),
    (jdotindex,         $,      nil,        2,      0,          0,  1),
    (jkeyindex,         $,      nil,        2,      0,          0,  1),
    (jredo,             $,      nil,        2,      0,          0,  0),
    (jnext,             $,      nil,        2,      0,          0,  0),
    (jexit,             $,      nil,        2,      0,          0,  0),
    (jptr,              $,      nil,        1,      0,          0,  1),
    (jaddrof,           $,      nil,        1,      0,          0,  1),
    (jptrto,            $,      nil,        1,      0,          0,  1),
    (jdaddrof,          $,      nil,        1,      0,          0,  1),
    (jnull,             $,      nil,        0,      0,          0,  1),
    (jprint,            $,      nil,        2,      0,          0,  0),
    (jprintln,          $,      nil,        2,      0,          0,  0),
    (jfprint,           $,      nil,        2,      0,          0,  0),
    (jfprintln,         $,      nil,        2,      0,          0,  0),
    (jsprint,           $,      nil,        2,      0,          0,  1),
    (jsfprint,          $,      nil,        2,      0,          0,  1),
    (jnogap,            $,      nil,        0,      0,          0,  0),
    (jspace,            $,      nil,        0,      0,          0,  0),
    (jfmtitem,          $,      nil,        2,      0,          0,  0),
    (jread,             $,      nil,        2,      0,          0,  0),
    (jreadln,           $,      nil,        2,      0,          0,  0),
    (jnew,              $,      nil,        2,      0,          0,  1),
    (jdecimal,          $,      nil,        0,      0,          0,  1),
    (jincr,             $,      "++",       1,      0,          0,  1),
    (jdecr,             $,      "--",       1,      0,          0,  1),
    (jincrload,         $,      nil,        1,      kincrload,  0,  1),
    (jdecrload,         $,      nil,        1,      kdecrload,  0,  1),
    (jloadincr,         $,      nil,        1,      kloadincr,  0,  1),
    (jloaddecr,         $,      nil,        1,      kloaddecr,  0,  1),
    (jneg,              $,      "-",        1,      kneg,       jnegto, 1),
    (jabs,              $,      "abs",      1,      kabs,       jabsto, 1),
    (jnotl,             $,      "not",      1,      knotl,      jnotlto,    1),
    (jinot,             $,      "inot",     1,      kinot,      jinotto,    1),
    (jistruel,          $,      "istrue",       1,      kistruel,   0,  1),
    (jasc,              $,      nil,        1,      kasc,       0,  1),
    (jchr,              $,      nil,        1,      kchr,       0,  1),
    (jsqrt,             $,      nil,        1,      ksqrt,      0,  1),
    (jsqr,              $,      nil,        1,      ksqr,       0,  1),
    (jsin,              $,      nil,        1,      ksin,       0,  1),
    (jcos,              $,      nil,        1,      kcos,       0,  1),
    (jtan,              $,      nil,        1,      ktan,       0,  1),
    (jasin,             $,      nil,        1,      kasin,      0,  1),
    (jacos,             $,      nil,        1,      kacos,      0,  1),
    (jatan,             $,      nil,        1,      katan,      0,  1),
    (jln,               $,      nil,        1,      kln,        0,  1),
    (jlog,              $,      nil,        1,      klog,       0,  1),
    (jlg,               $,      nil,        1,      klg,        0,  1),
    (jexp,              $,      nil,        1,      kexp,       0,  1),
    (jround,            $,      nil,        1,      kround,     0,  1),
    (jfloor,            $,      nil,        1,      kfloor,     0,  1),
    (jceil,             $,      nil,        1,      kceil,      0,  1),
    (jfract,            $,      nil,        2,      kfract,     0,  1),
    (jfmod,             $,      nil,        2,      kfmod,      0,  1),
    (jsign,             $,      nil,        1,      ksign,      0,  1),
    (jnegto,            $,      nil,        1,      knegto,     0,  0),
    (jabsto,            $,      nil,        1,      kabsto,     0,  0),
    (jnotlto,           $,      nil,        1,      knotlto,    0,  0),
    (jinotto,           $,      nil,        1,      kinotto,    0,  0),
    (jlen,              $,      nil,        1,      klen,       0,  1),
    (jlwb,              $,      nil,        1,      klwb,       0,  1),
    (jupb,              $,      nil,        1,      kupb,       0,  1),
    (jbounds,           $,      nil,        1,      kbounds,    0,  1),
    (jboundsx,          $,      nil,        1,      kboundsx,   0,  1),
    (jbitwidth,         $,      nil,        1,      kbitwidth,  0,  1),
    (jbytesize,         $,      nil,        1,      kbytesize,  0,  1),
    (jtype,             $,      nil,        1,      ktype,      0,  1),
    (jelemtype,         $,      nil,        1,      kelemtype,  0,  1),
    (jbasetype,         $,      nil,        1,      kbasetype,  0,  1),
    (jusertype,         $,      nil,        1,      kusertype,  0,  1),
    (jdictitems,        $,      nil,        1,      kdictitems, 0,  1),
    (jminvalue,         $,      nil,        1,      kminvalue,  0,  1),
    (jmaxvalue,         $,      nil,        1,      kmaxvalue,  0,  1),
    (jisint,            $,      nil,        1,      kisint,     0,  1),
    (jisreal,           $,      nil,        1,      kisreal,    0,  1),
    (jisstring,         $,      nil,        1,      kisstring,  0,  1),
    (jisrange,          $,      nil,        1,      kisrange,   0,  1),
    (jisnumber,         $,      nil,        1,      kisnumber,  0,  1),
    (jislist,           $,      nil,        1,      kislist,    0,  1),
    (jisrecord,         $,      nil,        1,      kisrecord,  0,  1),
    (jispointer,        $,      nil,        1,      kispointer, 0,  1),
    (jisarray,          $,      nil,        1,      kisarray,   0,  1),
    (jismutable,        $,      nil,        1,      kismutable, 0,  1),
    (jisset,            $,      nil,        1,      kisset,     0,  1),
    (jisvoid,           $,      nil,        1,      kisvoid,    0,  1),
    (jisdef,            $,      nil,        1,      kisdef,     0,  1),
    (jisequal,          $,      nil,        2,      kisequal,   0,  1),
    (jodd,              $,      nil,        2,      kodd,       0,  1),
    (jeven,             $,      nil,        2,      keven,      0,  1),
    (jadd,              $,      "+",        2,      kadd,       jaddto, 1),
    (jsub,              $,      "-",        2,      ksub,       jsubto, 1),
    (jmul,              $,      "*",        2,      kmul,       jmulto, 1),
    (jdiv,              $,      "/",        2,      kdiv,       jdivto, 1),
    (jidiv,             $,      "%",        2,      kidiv,      jidivto,    1),
    (jirem,             $,      "rem",      2,      kirem,      0,  1),
    (jidivrem,          $,      nil,        2,      kidivrem,   0,  1),
    (jiand,             $,      "iand",     2,      kiand,      jiandto,    1),
    (jior,              $,      "ior",      2,      kior,       jiorto, 1),
    (jixor,             $,      "ixor",     2,      kixor,      jixorto,    1),
    (jshl,              $,      "<<",       2,      kshl,       jshlto, 1),
    (jshr,              $,      ">>",       2,      kshr,       jshrto, 1),
    (jin,               $,      "in",       2,      kin,        0,  1),
    (jnotin,            $,      "notin",    2,      knotin,     0,  1),
    (jinrev,            $,      nil,        2,      0,          0,  0),
    (jandl,             $,      "and",      2,      kandl,      jandlto,    1),
    (jmandl,            $,      nil,        2,      kmandl,     0,  1),
    (jorl,              $,      "or",       2,      korl,       jorlto, 1),
    (jeq,               $,      "=",        2,      keq,        0,  1),
    (jne,               $,      "<>",       2,      kne,        0,  1),
    (jlt,               $,      "<",        2,      klt,        0,  1),
    (jle,               $,      "<=",       2,      kle,        0,  1),
    (jge,               $,      ">=",       2,      kge,        0,  1),
    (jgt,               $,      ">",        2,      kgt,        0,  1),
    (jmin,              $,      "min",      2,      kmin,       jminto, 1),
    (jmax,              $,      "max",      2,      kmax,       jmaxto, 1),
    (jconcat,           $,      nil,        2,      kconcat,    jconcatto,  1),
    (jappend,           $,      nil,        2,      kappend,    jappendto,  1),
    (jpower,            $,      "**",       2,      kpower,     0,  1),
    (jatan2,            $,      nil,        2,      katan2,     0,  1),
    (jaddto,            $,      nil,        2,      kaddto,     0,  0),
    (jsubto,            $,      nil,        2,      ksubto,     0,  0),
    (jmulto,            $,      nil,        2,      kmulto,     0,  0),
    (jdivto,            $,      nil,        2,      kdivto,     0,  0),
    (jidivto,           $,      nil,        2,      kidivto,    0,  0),
    (jandlto,           $,      nil,        2,      kandlto,    0,  0),
    (jorlto,            $,      nil,        2,      korlto,     0,  0),
    (jiandto,           $,      nil,        2,      kiandto,    0,  0),
    (jiorto,            $,      nil,        2,      kiorto,     0,  0),
    (jixorto,           $,      nil,        2,      kixorto,    0,  0),
    (jshlto,            $,      nil,        2,      kshlto,     0,  0),
    (jshrto,            $,      nil,        2,      kshrto,     0,  0),
    (jminto,            $,      nil,        2,      kminto,     0,  0),
    (jmaxto,            $,      nil,        2,      kmaxto,     0,  0),
    (jconcatto,         $,      nil,        2,      kconcatto,  0,  0),
    (jappendto,         $,      nil,        2,      kappendto,  0,  0),

    (jmakerange,        $,      nil,        2,      kmakerange, 0,  1),
    (jmakerangelen,     $,      nil,        2,      kmakerangelen,  0,  1),
    (jmakelist,         $,      nil,        1,      kmakelist,  0,  1),
    (jmakeset,          $,      nil,        1,      kmakeset,   0,  1),
    (jmakedict,         $,      nil,        1,      kmakedict,  0,  1),

    (jcvlineno,         $,      nil,        0,      0,  0,  1),
    (jcvstrlineno,      $,      nil,        0,      0,  0,  1),
    (jcvmodulename,     $,      nil,        0,      0,  0,  1),
    (jcvfilename,       $,      nil,        0,      0,  0,  1),
    (jcvfunction,       $,      nil,        0,      0,  0,  1),
    (jcvdate,           $,      nil,        0,      0,  0,  1),
    (jcvtime,           $,      nil,        0,      0,  0,  1),
    (jcvversion,        $,      nil,        0,      0,  0,  1),
    (jcvpclversion,     $,      nil,        0,      0,  0,  1),
end

global enumdata [0:]ichar fflangnames=

    (noff=0,        $), 
    (windowsff,     $), 
    (clangff,       $), 
    (mlangff,       $), 
    (callbackff,    $), 

    (dummyff,       $) 
end

global type qd=[4]byte

global enumdata [0:]ichar opndnames=
                            
    (cnone=0,   $),
    (cmemory,   $),         
    (cframe,    $),         
    (cproc,     $),         
    (cdllproc,  $),         

    (cgenfield, $),         

    (clabel,    $),         
    (cint,      $),         
    (cword,     $),         
    (creal,     $),         
    (crange,    $),         
    (cstring,   $),         
    (cstringz,  $),         
    (ctype,     $),         
    (csymbol,   $),         
    (coperator, $),         

    (clast,     "?")
end

const p = cproc
const m = cmemory
const f = cframe
const l = clabel
const x = cdllproc
const g = cgenfield
const i = cint
const u = cword
const r = creal
const n = crange
const s = cstring
const z = cstringz
const t = ctype
const d = csymbol
const o = coperator



global enumdata  [0:]ichar pclnames, [0:]qd pclfmt =
    (kzero=0,       $,  qd(0,0,0,0)),
    (knop,          $,  qd(0,0,0,0)),       
    (kskip,         $,  qd(0,0,0,0)),       

    (kprocdef,      $,  qd(d,0,0,0)),       
    (kprocentry,    $,  qd(i,0,0,0)),       
    (kprocend,      $,  qd(0,0,0,0)),
    (kendmodule,    $,  qd(0,0,0,0)),       
    (kcomment,      $,  qd(z,0,0,0)),

    (klabeldef,     $,  qd(d,0,0,0)),       

    (kpushm,        $,  qd(m,0,0,0)),       
    (kpushf,        $,  qd(f,0,0,0)),       
    (kpushmref,     $,  qd(m,0,0,0)),       
    (kpushfref,     $,  qd(f,0,0,0)),       
    (kpopm,         $,  qd(m,0,0,0)),       
    (kpopf,         $,  qd(f,0,0,0)),       
    (kstorem,       $,  qd(m,0,0,0)),       
    (kstoref,       $,  qd(f,0,0,0)),       

    (khostname,     $,  qd(d,0,0,0)),       

    (kpushci,       $,  qd(i,0,0,0)),       
    (kpushcu,       $,  qd(i,0,0,0)),       
    (kpushvoid,     $,  qd(0,0,0,0)),       
    (kpushnil,      $,  qd(0,0,0,0)),       
    (kpushcr,       $,  qd(r,0,0,0)),       
    (kpushcn,       $,  qd(n,0,0,0)),       
    (kpushtrue,     $,  qd(0,0,0,0)),       
    (kpushfalse,    $,  qd(0,0,0,0)),       

    (kpushcs,       $,  qd(s,0,0,0)),       
    (kpushenum,     $,  qd(i,t,0,0)),       

    (kpusht,        $,  qd(t,0,0,0)),       
    (kpushsymbol,   $,  qd(d,0,0,0)),       
    (kpushoperator, $,  qd(o,0,0,0)),       

    (kpushptr,      $,  qd(0,0,0,0)),       
    (kpopptr,       $,  qd(0,0,0,0)),       

    (kzpopm,        $,  qd(m,0,0,0)),       
    (kzpopf,        $,  qd(f,0,0,0)),       

    (kdupl,         $,  qd(0,0,0,0)),       
    (kcopy,         $,  qd(0,0,0,0)),       
    (kswap,         $,  qd(0,0,0,0)),       

    (kconvrefpack,  $,  qd(0,0,0,0)),       

    (kjump,         $,  qd(l,0,0,0)),       
    (kjumpptr,      $,  qd(0,0,0,0)),       

    (kjumptrue,     $,  qd(l,0,0,0)),       
    (kjumpfalse,    $,  qd(l,0,0,0)),       

    (kjumpeq,       $,  qd(l,0,0,0)),       
    (kjumpne,       $,  qd(l,0,0,0)),       
    (kjumplt,       $,  qd(l,0,0,0)),       
    (kjumple,       $,  qd(l,0,0,0)),       
    (kjumpge,       $,  qd(l,0,0,0)),       
    (kjumpgt,       $,  qd(l,0,0,0)),       

    (kjumptesteq,   $,  qd(l,0,0,0)),       
    (kjumptestne,   $,  qd(l,0,0,0)),       

    (kjumplabel,    $,  qd(l,0,0,0)),       

    (kswitch,       $,  qd(i,i,0,0)),       

    (ktom,          $,  qd(l,m,0,0)),       
    (ktof,          $,  qd(l,f,0,0)),       

    (kformci,       $,  qd(l,m,i,0)),       
    (kforfci,       $,  qd(l,f,i,0)),       
    (kformm,        $,  qd(l,m,m,0)),       
    (kforff,        $,  qd(l,f,f,0)),       

    (kfordmci,      $,  qd(l,m,i,0)),       
    (kfordfci,      $,  qd(l,f,i,0)),       
    (kfordmm,       $,  qd(l,m,m,0)),       
    (kfordff,       $,  qd(l,f,f,0)),       

    (kcallproc,     $,  qd(p,i,0,0)),       
    (kcallptr,      $,  qd(i,i,0,0)),       
    (kreturn0,      $,  qd(0,0,0,0)),       
    (kreturn,       $,  qd(i,0,0,0)),       
    (kpopretval,    $,  qd(i,0,0,0)),       

    (kmodulecall,   $,  qd(d,0,0,0)),       
    (kmodulereturn, $,  qd(0,0,0,0)),       

    (kcalldll,      $,  qd(x,i,0,0)),       
    (kcallmproc,    $,  qd(x,i,0,0)),       

    (kcallhost,     $,  qd(i,0,0,0)),       
    (kcallmsys,     $,  qd(i,0,0,0)),       

    (kunshare,      $,  qd(i,0,0,0)),       
    (kaddsp,        $,  qd(i,0,0,0)),       

    (kstop,         $,  qd(0,0,0,0)),       
    (kstoprunproc,  $,  qd(0,0,0,0)),       

    (kmakelist,     $,  qd(i,i,0,0)),       
    (kmakerecord,   $,  qd(i,t,0,0)),       
    (kmakearray,    $,  qd(i,i,t,t)),       
    (kmakebits,     $,  qd(i,i,t,t)),       
    (kmakestruct,   $,  qd(i,t,0,0)),       
    (kmakeset,      $,  qd(i,0,0,0)),       
    (kmakerange,    $,  qd(0,0,0,0)),       
    (kmakerangelen, $,  qd(0,0,0,0)),       
    (kmakedict,     $,  qd(i,0,0,0)),       
    (kmakedecimal,  $,  qd(0,0,0,0)),       

    (kincrptr,      $,  qd(0,0,0,0)),       
    (kincrtom,      $,  qd(m,0,0,0)),       
    (kincrtof,      $,  qd(f,0,0,0)),       
    (kloadincr,     $,  qd(0,0,0,0)),       
    (kincrload,     $,  qd(0,0,0,0)),       

    (kdecrptr,      $,  qd(0,0,0,0)),       
    (kdecrtom,      $,  qd(m,0,0,0)),       
    (kdecrtof,      $,  qd(f,0,0,0)),       
    (kloaddecr,     $,  qd(0,0,0,0)),       
    (kdecrload,     $,  qd(0,0,0,0)),       

    (kincr,         $,  qd(0,0,0,0)),       
    (kdecr,         $,  qd(0,0,0,0)),       

    (kneg,          $,  qd(0,0,0,0)),       
    (kabs,          $,  qd(0,0,0,0)),       
    (knotl,         $,  qd(0,0,0,0)),       
    (kinot,         $,  qd(0,0,0,0)),       
    (kistruel,      $,  qd(0,0,0,0)),       
    (kasc,          $,  qd(0,0,0,0)),       
    (kchr,          $,  qd(0,0,0,0)),       

    (ksqrt,         $,  qd(0,0,0,0)),       
    (ksqr,          $,  qd(0,0,0,0)),       
    (kcube,         $,  qd(0,0,0,0)),       
    (ksin,          $,  qd(0,0,0,0)),       
    (kcos,          $,  qd(0,0,0,0)),       
    (ktan,          $,  qd(0,0,0,0)),       
    (kasin,         $,  qd(0,0,0,0)),       
    (kacos,         $,  qd(0,0,0,0)),       
    (katan,         $,  qd(0,0,0,0)),       
    (ksign,         $,  qd(0,0,0,0)),       
    (kln,           $,  qd(0,0,0,0)),       
    (klog,          $,  qd(0,0,0,0)),       
    (klg,           $,  qd(0,0,0,0)),       
    (kexp,          $,  qd(0,0,0,0)),       
    (kround,        $,  qd(0,0,0,0)),       
    (kfloor,        $,  qd(0,0,0,0)),       
    (kceil,         $,  qd(0,0,0,0)),       
    (kfract,        $,  qd(0,0,0,0)),       
    (kfmod,         $,  qd(0,0,0,0)),       

    (knegto,        $,  qd(0,0,0,0)),       
    (kabsto,        $,  qd(0,0,0,0)),       
    (kinotto,       $,  qd(0,0,0,0)),       
    (knotlto,       $,  qd(0,0,0,0)),       

    (klen,          $,  qd(0,0,0,0)),       
    (klwb,          $,  qd(0,0,0,0)),       
    (kupb,          $,  qd(0,0,0,0)),       
    (kbounds,       $,  qd(0,0,0,0)),       
    (kboundsx,      $,  qd(0,0,0,0)),       
    (kbitwidth,     $,  qd(0,0,0,0)),       
    (kbytesize,     $,  qd(0,0,0,0)),       
    (ktype,         $,  qd(0,0,0,0)),       
    (kelemtype,     $,  qd(0,0,0,0)),       
    (kbasetype,     $,  qd(0,0,0,0)),       
    (kusertype,     $,  qd(0,0,0,0)),       
    (kdictitems,    $,  qd(0,0,0,0)),       
    (kminvalue,     $,  qd(0,0,0,0)),       
    (kmaxvalue,     $,  qd(0,0,0,0)),       
    (kisint,        $,  qd(0,0,0,0)),       
    (kisreal,       $,  qd(0,0,0,0)),       
    (kisstring,     $,  qd(0,0,0,0)),       
    (kisrange,      $,  qd(0,0,0,0)),       
    (kisnumber,     $,  qd(0,0,0,0)),       
    (kislist,       $,  qd(0,0,0,0)),       
    (kisrecord,     $,  qd(0,0,0,0)),       
    (kispointer,    $,  qd(0,0,0,0)),       
    (kisarray,      $,  qd(0,0,0,0)),       
    (kismutable,    $,  qd(0,0,0,0)),       
    (kisset,        $,  qd(0,0,0,0)),       
    (kisvoid,       $,  qd(0,0,0,0)),       
    (kisdef,        $,  qd(0,0,0,0)),       
    (kisequal,      $,  qd(0,0,0,0)),       
    (kconvert,      $,  qd(t,0,0,0)),       
    (ktypepun,      $,  qd(t,0,0,0)),       
    (kodd,          $,  qd(0,0,0,0)),       
    (keven,         $,  qd(0,0,0,0)),       

    (kadd,          $,  qd(0,0,0,0)),       
    (ksub,          $,  qd(0,0,0,0)),       
    (kmul,          $,  qd(0,0,0,0)),       
    (kdiv,          $,  qd(0,0,0,0)),       
    (kidiv,         $,  qd(0,0,0,0)),       
    (kirem,         $,  qd(0,0,0,0)),       
    (kidivrem,      $,  qd(0,0,0,0)),       
    (kiand,         $,  qd(0,0,0,0)),       
    (kior,          $,  qd(0,0,0,0)),       
    (kixor,         $,  qd(0,0,0,0)),       
    (kshl,          $,  qd(0,0,0,0)),       
    (kshr,          $,  qd(0,0,0,0)),       
    (kin,           $,  qd(0,0,0,0)),       
    (knotin,        $,  qd(0,0,0,0)),       
    (kandl,         $,  qd(0,0,0,0)),       
    (kmandl,        $,  qd(0,0,0,0)),       
    (korl,          $,  qd(0,0,0,0)),       
    (keq,           $,  qd(0,0,0,0)),       
    (kne,           $,  qd(0,0,0,0)),       
    (klt,           $,  qd(0,0,0,0)),       
    (kle,           $,  qd(0,0,0,0)),       
    (kge,           $,  qd(0,0,0,0)),       
    (kgt,           $,  qd(0,0,0,0)),       
    (kmin,          $,  qd(0,0,0,0)),       
    (kmax,          $,  qd(0,0,0,0)),       
    (kconcat,       $,  qd(0,0,0,0)),       
    (kappend,       $,  qd(0,0,0,0)),       

    (kpower,        $,  qd(0,0,0,0)),       
    (katan2,        $,  qd(0,0,0,0)),       

    (kaddto,        $,  qd(0,0,0,0)),       
    (ksubto,        $,  qd(0,0,0,0)),       
    (kmulto,        $,  qd(0,0,0,0)),       
    (kdivto,        $,  qd(0,0,0,0)),       
    (kidivto,       $,  qd(0,0,0,0)),       

    (kandlto,       $,  qd(0,0,0,0)),       
    (korlto,        $,  qd(0,0,0,0)),       
    (kiandto,       $,  qd(0,0,0,0)),       
    (kiorto,        $,  qd(0,0,0,0)),       
    (kixorto,       $,  qd(0,0,0,0)),       
    (kshlto,        $,  qd(0,0,0,0)),       
    (kshrto,        $,  qd(0,0,0,0)),       
    (kminto,        $,  qd(0,0,0,0)),       
    (kmaxto,        $,  qd(0,0,0,0)),       
    (kconcatto,     $,  qd(0,0,0,0)),       
    (kappendto,     $,  qd(0,0,0,0)),       

    (kdot,          $,  qd(g,0,0,0)),       
    (kindex,        $,  qd(0,0,0,0)),       
    (kdotindex,     $,  qd(0,0,0,0)),       
    (kkeyindex,     $,  qd(0,0,0,0)),       

    (kdotref,       $,  qd(g,0,0,0)),       
    (kindexref,     $,  qd(0,0,0,0)),       
    (kdotindexref,  $,  qd(0,0,0,0)),       
    (kkeyindexref,  $,  qd(0,0,0,0)),       

    (kpopdot,       $,  qd(g,0,0,0)),       
    (kpopindex,     $,  qd(0,0,0,0)),       
    (kpopdotindex,  $,  qd(0,0,0,0)),       
    (kpopkeyindex,  $,  qd(0,0,0,0)),       

    (kexpand,       $,  qd(i,0,0,0)),       

    (kpushtry,      $,  qd(l,i,i,0)),       
    (kraise,        $,  qd(0,0,0,0)),       
    (kmaps,         $,  qd(0,0,0,0)),       
    (kmapss,        $,  qd(0,0,0,0)),       


    (kpushff,       $,  qd(f,f,0,0)),       
    (kpushmm,       $,  qd(m,m,0,0)),       
    (kpushfm,       $,  qd(f,m,0,0)),       
    (kpushmf,       $,  qd(m,f,0,0)),       

    (kmoveff,       $,  qd(f,f,0,0)),       
    (kzmoveff,      $,  qd(f,f,0,0)),       
    (kmovefm,       $,  qd(f,m,0,0)),       
    (kmovemf,       $,  qd(m,f,0,0)),       
    (kmovemm,       $,  qd(m,m,0,0)),       

    (kmovefci,      $,  qd(f,i,0,0)),       
    (kzmovefci,     $,  qd(f,i,0,0)),       
    (kmovemci,      $,  qd(m,i,0,0)),       

    (kpushfff,      $,  qd(f,f,f,0)),       
    (knop2,         $,  qd(i,0,0,0)),       
    (kpushci0,      $,  qd(i,0,0,0)),       
    (kpushvoid2,    $,  qd(0,0,0,0)),       
    (kpushvoid3,    $,  qd(0,0,0,0)),       

    (kunshare1,     $,  qd(0,0,0,0)),       
    (kunshare2,     $,  qd(0,0,0,0)),       
    (kunshare3,     $,  qd(0,0,0,0)),       
    (kprocentry1,   $,  qd(0,0,0,0)),       
    (kprocentry2,   $,  qd(0,0,0,0)),       

    (kjumpeqfci,    $,  qd(l,f,i,0)),       
    (kjumpnefci,    $,  qd(l,f,i,0)),       
    (kjumpltfci,    $,  qd(l,f,i,0)),       
    (kjumplefci,    $,  qd(l,f,i,0)),       
    (kjumpgefci,    $,  qd(l,f,i,0)),       
    (kjumpgtfci,    $,  qd(l,f,i,0)),       

    (kjumpeqff,     $,  qd(l,f,f,0)),       
    (kjumpneff,     $,  qd(l,f,f,0)),       
    (kjumpltff,     $,  qd(l,f,f,0)),       
    (kjumpleff,     $,  qd(l,f,f,0)),       
    (kjumpgeff,     $,  qd(l,f,f,0)),       
    (kjumpgtff,     $,  qd(l,f,f,0)),       

    (kaddfci,       $,  qd(f,i,0,0)),       
    (ksubfci,       $,  qd(f,i,0,0)),       

    (kaddff,        $,  qd(f,f,0,0)),       
    (ksubff,        $,  qd(f,f,0,0)),       
    (kindexff,      $,  qd(f,f,0,0)),       

    (kpushincrptrm, $,  qd(m,0,0,0)),       
    (kpushincrptrf, $,  qd(f,0,0,0)),       
    (kpopincrptrm,  $,  qd(m,0,0,0)),       
    (kpopincrptrf,  $,  qd(f,0,0,0)),       

    (kswitchf,      $,  qd(f,i,i,0)),       
    (klenf,         $,  qd(f,0,0,0)),       
    (kpushptrf,     $,  qd(f,0,0,0)),       

    (klastpcl,      $,  qd(0,0,0,0))
end

global [0..klastpcl]ref void cmdmap         

global enumdata []ichar symbolnames=
    (errorsym,          $),     
    (dotsym,            $),     
    (lexdotsym,         $),     
    (anddotsym,         $),     
    (commasym,          $),     
    (semisym,           $),     
    (colonsym,          $),     
    (dcolonsym,         $),     
    (assignsym,         $),     
    (deepcopysym,       $),     
    (sendtosym,         $),     
    (lbracksym,         $),     
    (rbracksym,         $),     
    (lsqsym,            $),     
    (rsqsym,            $),     
    (lcurlysym,         $),     
    (rcurlysym,         $),     
    (ptrsym,            $),     
    (barsym,            $),     
    (dbarsym,           $),     
    (atsym,             $),     
    (datsym,            $),     
    (questionsym,       $),     
    (addrsym,           $),     
    (daddrsym,          $),     
    (poundsym,          $),     
    (curlsym,           $),     
    (rangesym,          $),     
    (ellipsissym,       $),     
    (hashsym,           $),     

    (addsym,            $),     
    (subsym,            $),     
    (mulsym,            $),     
    (divsym,            $),     
    (idivsym,           $),     
    (iremsym,           $),     
    (idivremsym,        $),     
    (andlsym,           $),     
    (orlsym,            $),     
    (iandsym,           $),     
    (iorsym,            $),     
    (ixorsym,           $),     
    (shlsym,            $),     
    (shrsym,            $),     

    (minsym,            $),     
    (maxsym,            $),     
    (appendsym,         $),     
    (concatsym,         $),     
    (insym,             $),     
    (notinsym,          $),     
    (inrevsym,          $),     
    (powersym,          $),     

    (eqsym,             $),     
    (nesym,             $),     
    (ltsym,             $),     
    (lesym,             $),     
    (gesym,             $),     
    (gtsym,             $),     
    (isequalsym,        $),     

    (notlsym,           $),     
    (inotsym,           $),     
    (istruelsym,        $),     
    (abssym,            $),     
    (sqrsym,            $),     
    (signsym,           $),     
    (ascsym,            $),     
    (chrsym,            $),     

    (mathssym,          $),     
    (maths2sym,         $),     
    (propsym,           $),     

    (incrsym,           $),     
    (decrsym,           $),     

    (eolsym,            $),     
    (eofsym,            $),     
    (rawnamesym,        $),     
    (docstringsym,      $),     
    (intconstsym,       $),     
    (decimalconstsym,   $),     
    (realconstsym,      $),     
    (charconstsym,      $),     
    (wcharconstsym,     $),     
    (stringconstsym,    $),     
    (astringconstsym,   $),     
    (wstringconstsym,   $),     
    (boolconstsym,      $),     

    (unitnamesym,       $),     
    (namesym,           $),     

    (stdtypesym,        $),     
    (koutsym,           $),     
    (kicharsym,         $),     
    (kifsym,            $),     
    (kthensym,          $),     
    (kelsifsym,         $),     
    (kelsesym,          $),     
    (kelsecasesym,      $),     
    (kelseswitchsym,    $),     
    (kelseselectsym,    $),     
    (kendsym,           $),     
    (kunlesssym,        $),     
    (kcasesym,          $),     
    (kdocasesym,        $),     
    (krecasesym,        $),     
    (kwhensym,          $),     
    (kforsym,           $),     
    (kforallsym,        $),     
    (ktosym,            $),     
    (kbysym,            $),     
    (kdosym,            $),     
    (kdooncesym,        $),     
    (kwhilesym,         $),     
    (krepeatsym,        $),     
    (kuntilsym,         $),     
    (kreturnsym,        $),     
    (kstopsym,          $),     
    (kloopsym,          $),     
    (kgotosym,          $),     
    (kswitchsym,        $),     
    (kdoswitchsym,      $),     
    (kprintsym,         $),     
    (ksprintsym,        $),     
    (kreadsym,          $),     
    (ksreadsym,         $),     
    (ksreadlnsym,       $),     
    (knewsym,           $),     
    (kprocsym,          $),     
    (kfunctionsym,      $),     
    (klabelsym,         $),     
    (krecordsym,        $),     
    (kstructsym,        $),     
    (kunionsym,         $),     
    (kheadersym,        $),     
    (kheadervarsym,     $),     
    (kimportdllsym,     $),     
    (ktypesym,          $),     
    (ktypeattrsym,      $),     
    (krefsym,           $),     
    (kvarsym,           $),     
    (kletsym,           $),     
    (kslicesym,         $),     
    (kmacrosym,         $),     
    (kexpandsym,        $),     
    (koperatorsym,      $),     
    (kconstsym,         $),     
    (kenumsym,          $),     
    (kdirectivesym,     $),     
    (kfflangsym,        $),     
    (kglobalsym,        $),     
    (kstaticsym,        $),     
    (kcalignedsym,      $),     

    (ktrysym,           $),     
    (kexceptsym,        $),     
    (kfinallysym,       $),     
    (kraisesym,         $),     
    (kyieldsym,         $),     
    (kextendsym,        $),     
    (kblocksym,         $),     
    (kcastsym,          $),     
    (kptypesym,         $),     
    (compilervarsym,    $),     
    (dollarsym,         $),     
    (kevalsym,          $),     
    (ktabledatasym,     $),     
    (kmapsym,           $),     
    (kclampsym,         $),     
    (kswapsym,          $),     
    (kcondcompsym,      $),     
    (kerrorsym,         $),     
    (sysconstsym,       $),     
    (khostfnsym,        $),     
    (khostsym,          $),     
    (knilsym,           $),     
    (kstrincludesym,    $),     
    (kdummysym,         $)      
end

global enumdata =
    pi_const,
    tab_const,
    con_const,
end

global enumdata =
    thousand_unit,
    million_unit,
    billion_unit
end

global enumdata [0:]ichar parammodenames=
    (var_param=0,       "Var "),
    (in_param,          "In "),
    (out_param,         "Out "),
    (optional_param,    "Opt "),
end

global enumdata []ichar headerdirnames =
    (hdr_module,        $),
    (hdr_import,        $),
    (hdr_subprog,       $),
    (hdr_sysmodule,     $),
    (hdr_sysimport,     $),
    (hdr_syssubprog,    $),
    (hdr_altpath,       $),
    (hdr_importpath,    $),
    (hdr_link,          $),
    (hdr_exportmodule,  $),
    (hdr_file,          $),
    (hdr_runexe,        $),
    (hdr_setvar,        $),
    (hdr_showvar,       $),
end

global enumdata []ichar headervarnames =
    (hv_devpath,        $),
    (hv_mmpath,         $),
    (hv_hdrpath,        $),
    (hv_ctarget,        $),
    (hv_windows,        $),
    (hv_linux,          $),
    (hv_optim,          $),
    (hv_mainmodule,     $),
    (hv_a,              $),
    (hv_b,              $),
    (hv_c,              $),
end

global enumdata [0:]ichar namenames =
    (genericid=0,   $),     
    (programid,     $),     
    (subprogid,     $),     
    (moduleid,      $),     
    (dllmoduleid,   $),     
    (procid,        $),     
    (dllprocid,     $),     
    (recordid,      $),     
    (typeid,        $),     
    (fieldid,       $),     
    (structfieldid, $),     
    (staticid,      $),     
    (frameid,       $),     
    (paramid,       $),     
    (dllparamid,    $),     
    (labelid,       $),     
    (constid,       $),     
    (enumid,        $),     
    (aliasid,       $),     
    (linkid,        $),     
    (macroid,       $),     
    (macroparamid,  $),     
    (structblockid, $),     
    (unionblockid,  $),     
    (endblockid,    $),     
end

global enumdata [0:]ichar objtypenames =
    (normal_obj=0,  $),
    (slice_obj,     $),
    (extslice_obj,  $)
end

global enumdata [0:]ichar scopenames=
    (local_scope=0,     $), 
    (global_scope,      $), 
    (export_scope,      $), 
end

global tabledata []ichar stnames, []int16 stsymbols, []int16 stsubcodes=

    ("if",          kifsym,         0),
    ("then",        kthensym,       0),
    ("elsif",       kelsifsym,      jif),
    ("else",        kelsesym,       0),
    ("elsecase",    kelsecasesym,   jcase),
    ("elseswitch",  kelseswitchsym, jswitch),
    ("case",        kcasesym,       jcase),
    ("docase",      kdocasesym,     jdocase),
    ("recase",      krecasesym,     jrecase),
    ("when",        kwhensym,       0),
    ("for",         kforsym,        0),
    ("forall",      kforsym,        0),
    ("foreach",     kforsym,        1),
    ("to",          ktosym,         0),
    ("downto",      ktosym,         1),
    ("by",          kbysym,         0),
    ("do",          kdosym,         0),
    ("doonce",      kdooncesym,     0),
    ("end",         kendsym,        0),
    ("while",       kwhilesym,      0),
    ("repeat",      krepeatsym,     0),
    ("until",       kuntilsym,      0),
    ("always",      kuntilsym,      0),
    ("return",      kreturnsym,     0),
    ("yield",       kyieldsym,      0),
    ("stop",        kstopsym,       0),
    ("redo",        kloopsym,       jredo),
    ("next",        kloopsym,       jnext),
    ("exit",        kloopsym,       jexit),
    ("goto",        kgotosym,       0),
    ("go",          kgotosym,       1),
    ("switch",      kswitchsym,     jswitch),
    ("doswitch",    kdoswitchsym,   jdoswitch),
    ("tabledata",   ktabledatasym,  0),
    ("enumdata",    ktabledatasym,  1),
    ("clamp",       kclampsym,      0),
    ("mapss",       kmapsym,        0),
    ("eval",        kevalsym,       0),
    ("$windows",    kcondcompsym,   0),
    ("$linux",      kcondcompsym,   0),

    ("print",       kprintsym,      jprint),
    ("println",     kprintsym,      jprintln),
    ("fprint",      kprintsym,      jfprint),
    ("fprintln",    kprintsym,      jfprintln),
    ("sprint",      ksprintsym,     jsprint),
    ("sfprint",     ksprintsym,     jsfprint),

    ("cp",          kprintsym,      jprint),
    ("cpl",         kprintsym,      jprintln),

    ("read",        kreadsym,       jread),
    ("readln",      kreadsym,       jreadln),


    ("cast",        kcastsym,       13),
    ("$type",       kptypesym,      0),

    ("proc",        kprocsym,       0),
    ("procedure",   kprocsym,       0),
    ("sub",         kprocsym,       0),

    ("function",    kfunctionsym,   0),

    ("func",        kfunctionsym,   0),
    ("fun",         kfunctionsym,   0),
    ("method",      kfunctionsym,   0),

    ("type",        ktypesym,       0),
    ("record",      krecordsym,     0),
    ("struct",      kstructsym,     0),
    ("packrecord",  kstructsym,     0),
    ("union",       kunionsym,      0),
    ("ref",         krefsym,        0),
    ("var",         kvarsym,        0),
    ("let",         kletsym,        0),
    ("slice",       kslicesym,      0),

    ("macro",       kmacrosym,      0),
    ("operator",    koperatorsym,   0),

    ("static",      kstaticsym,     0),
    ("$caligned",   kcalignedsym,   0),
    
    ("const",       kconstsym,      0),
    ("enum",        kenumsym,       0),


    ("module",      kheadersym,     hdr_module),
    ("sysmodule",   kheadersym,     hdr_sysmodule),
    ("import",      kheadersym,     hdr_import),
    ("sysimport",   kheadersym,     hdr_sysimport),
    ("subprog",     kheadersym,     hdr_subprog),
    ("syssubprog",  kheadersym,     hdr_syssubprog),
    ("altpath",     kheadersym,     hdr_altpath),
    ("importpath",  kheadersym,     hdr_importpath),
    ("link",        kheadersym,     hdr_link),
    ("exportmodule",kheadersym,     hdr_exportmodule),
    ("runexe",      kheadersym,     hdr_runexe),
    ("setvar",      kheadersym,     hdr_setvar),
    ("showvar",     kheadersym,     hdr_showvar),

    ("importdll",   kimportdllsym,  0),
    ("strinclude",  kstrincludesym, 0),
    ("unless",      kunlesssym,     0),

    ("try",         ktrysym,        0),
    ("except",      kexceptsym,     0),
    ("finally",     kfinallysym,    0),
    ("raise",       kraisesym,      0),
    ("out",         koutsym,        0),

    ("global",      kglobalsym,     global_scope),
    ("export",      kglobalsym,     export_scope),
    ("host",        khostsym,       0),

    ("clang",       kfflangsym,     clangff),
    ("windows",     kfflangsym,     windowsff),
    ("callback",    kfflangsym,     callbackff),

    ("swap",        kswapsym,       0),

    ("void",        stdtypesym,     tvoid),

    ("int",         stdtypesym,     tint),

    ("word",        stdtypesym,     tword),

    ("real",        stdtypesym,     treal),
    ("bool",        stdtypesym,     tbool),

    ("string",      stdtypesym,     tstring),
    ("list",        stdtypesym,     tlist),
    ("array",       stdtypesym,     tarray),
    ("bits",        stdtypesym,     tbits),
    ("set",         stdtypesym,     tset),
    ("dict",        stdtypesym,     tdict),
    ("decimal",     stdtypesym,     tdecimal),
    ("longint",     stdtypesym,     tdecimal),
    ("typetype",    stdtypesym,     ttype),
    ("range",       stdtypesym,     trange),
    ("recordtype",  stdtypesym,     trecord),

    ("cvoid",       stdtypesym,     tvoid),
    ("i8",          stdtypesym,     ti8),
    ("i16",         stdtypesym,     ti16),
    ("i32",         stdtypesym,     ti32),
    ("i64",         stdtypesym,     ti64),

    ("bit",         stdtypesym,     tu1),
    ("u1",          stdtypesym,     tu1),
    ("u2",          stdtypesym,     tu2),
    ("u4",          stdtypesym,     tu4),
    ("byte",        stdtypesym,     tu8),
    ("u8",          stdtypesym,     tu8),
    ("u16",         stdtypesym,     tu16),
    ("u32",         stdtypesym,     tu32),
    ("u64",         stdtypesym,     tu64),

    ("r32",         stdtypesym,     tr32),
    ("r64",         stdtypesym,     tr64),

    ("int8",        stdtypesym,     ti8),
    ("int16",       stdtypesym,     ti16),
    ("int32",       stdtypesym,     ti32),
    ("int64",       stdtypesym,     ti64),

    ("word8",       stdtypesym,     tu8),
    ("word16",      stdtypesym,     tu16),
    ("word32",      stdtypesym,     tu32),
    ("word64",      stdtypesym,     tu64),

    ("real32",      stdtypesym,     tr32),
    ("real64",      stdtypesym,     tr64),

    ("stringc",     stdtypesym,     tpackstrc),
    ("stringz",     stdtypesym,     tpackstrz),
    ("cstring",     stdtypesym,     tpackstrz),
    ("ichar",       stdtypesym,     tstringz),

    ("million",     unitnamesym,    million_unit),
    ("billion",     unitnamesym,    billion_unit),
    ("as",          unitnamesym,    0),

    ("$lineno",     compilervarsym, jcvlineno),
    ("$strlineno",  compilervarsym, jcvstrlineno),
    ("$date",       compilervarsym, jcvdate),
    ("$time",       compilervarsym, jcvtime),
    ("$",           dollarsym,      0),

    ("and",         andlsym,        jandl),
    ("or",          orlsym,         jorl),
    ("iand",        iandsym,        jiand),
    ("ior",         iorsym,         jior),
    ("ixor",        ixorsym,        jixor),
    ("in",          insym,          jin),
    ("notin",       notinsym,       jnotin),
    ("inrev",       inrevsym,       jinrev),
    ("rem",         iremsym,        jirem),
    ("divrem",      idivremsym,     jidivrem),
    ("min",         minsym,         jmin),
    ("max",         maxsym,         jmax),

    ("not",         notlsym,        jnotl),
    ("inot",        inotsym,        jinot),
    ("istrue",      istruelsym,     jistruel),
    ("abs",         abssym,         jabs),
    ("asc",         ascsym,         jasc),
    ("chr",         chrsym,         jchr),
    ("sqrt",        mathssym,       jsqrt),
    ("sqr",         sqrsym,         jsqr),
    ("cos",         mathssym,       jcos),
    ("sin",         mathssym,       jsin),
    ("tan",         mathssym,       jtan),
    ("asin",        mathssym,       jasin),
    ("acos",        mathssym,       jacos),
    ("atan",        mathssym,       jatan),
    ("atan2",       maths2sym,      jatan2),
    ("sign",        signsym,        jsign),
    ("ln",          mathssym,       jln),
    ("log",         mathssym,       jlog),
    ("exp",         mathssym,       jexp),
    ("round",       mathssym,       jround),
    ("floor",       mathssym,       jfloor),
    ("ceil",        mathssym,       jceil),
    ("fract",       mathssym,       jfract),
    ("fmod",        maths2sym,      jfmod),

    ("append",      appendsym,      jappend),
    ("concat",      concatsym,      jconcat),

    ("len",         propsym,        jlen),
    ("lwb",         propsym,        jlwb),
    ("upb",         propsym,        jupb),
    ("bounds",      propsym,        jbounds),
    ("bitwidth",    propsym,        jbitwidth),
    ("bytes",       propsym,        jbytesize),
    ("minvalue",    propsym,        jminvalue),
    ("maxvalue",    propsym,        jmaxvalue),
    ("basetype",    propsym,        jbasetype),
    ("usertype",    propsym,        jusertype),
    ("elemtype",    propsym,        jelemtype),
    ("dictitems",   propsym,        jdictitems),
    ("decdigits",   propsym,        jdictitems),

    ("isvoid",      propsym,        jisvoid),
    ("isdef",       propsym,        jisdef),
    ("defined",     propsym,        jisdef),
    ("isint",       propsym,        jisint),
    ("isreal",      propsym,        jisreal),
    ("islist",      propsym,        jislist),
    ("isstring",    propsym,        jisstring),
    ("isrange",     propsym,        jisrange),
    ("ispointer",   propsym,        jispointer),
    ("isarray",     propsym,        jisarray),
    ("isrecord",    propsym,        jisrecord),
    ("isset",       propsym,        jisset),
    ("isnumber",    propsym,        jisnumber),
    ("ismutable",   propsym,        jismutable),
    ("odd",         propsym,        jodd),
    ("even",        propsym,        jeven),

    ("endif",       kendsym,        kifsym),
    ("fi",          kendsym,        kifsym),
    ("endcase",     kendsym,        kcasesym),
    ("esac",        kendsym,        kcasesym),
    ("enddocase",   kendsym,        kdocasesym),
    ("endswitch",   kendsym,        kswitchsym),
    ("enddoswitch", kendsym,        kdoswitchsym),
    ("endfor",      kendsym,        kforsym),
    ("endforall",   kendsym,        kforallsym),
    ("od",          kendsym,        kdosym),
    ("enddoonce",   kendsym,        kdooncesym),
    ("endproc",     kendsym,        kprocsym),
    ("endfunction", kendsym,        kfunctionsym),
    ("endwhile",    kendsym,        kwhilesym),
    ("endto",       kendsym,        ktosym),
    ("enddo",       kendsym,        kdosym),
    ("endunless",   kendsym,        kunlesssym),
    ("endmoduledll",    kendsym,    kimportdllsym),
    ("endtry",      kendsym,        ktrysym),
    ("endrecord",   kendsym,        krecordsym),
    ("endblock",    kendsym,        kblocksym),

    ("nil",         knilsym,        0),
    ("pnil",        knilsym,        1),
    ("con",         sysconstsym,    con_const),
    ("pi",          sysconstsym,    pi_const),
    ("true",        boolconstsym,   1),
    ("false",       boolconstsym,   0),

    ("$$dummy",     0,              0)
end

global enumdata [0:]ichar hostfnnames, [0:]byte hostnparams, [0:]byte hostisfn,
            [0:]byte hostinternal =
    (host_dummy=0,          $,  0,  0,  1),

    (host_startprint,       $,  1,  0,  1), 
    (host_startprintcon,    $,  0,  0,  1), 
    (host_strstartprint,    $,  0,  0,  1), 
    (host_setformat,        $,  1,  0,  1), 
    (host_endprint,         $,  0,  0,  1), 
    (host_strendprint,      $,  0,  1,  1), 
    (host_print,            $,  2,  0,  1),     
    (host_print_nf,         $,  1,  0,  1),     

    (host_dprint,           $,  2,  0,  1), 
    (host_println,          $,  0,  0,  1), 
    (host_printnogap,       $,  0,  0,  1), 
    (host_printspace,       $,  0,  0,  1), 

    (host_readln,           $,  1,  0,  1), 
    (host_sreadln,          $,  1,  1,  0), 
    (host_sread,            $,  1,  1,  0), 
    (host_rereadln,         $,  0,  0,  0), 
    (host_reread,           $,  0,  0,  0), 

    (host_strtoval,         $,  2,  1,  0), 
    (host_tostr,            $,  2,  1,  0), 

    (host_leftstr,          $,  3,  1,  0),
    (host_rightstr,         $,  3,  1,  0),
    (host_convlc,           $,  2,  1,  0),
    (host_convuc,           $,  2,  1,  0),
    (host_iconvlc,          $,  2,  0,  0),     
    (host_iconvuc,          $,  2,  0,  0),     

    (host_stop,             $,  0,  0,  1), 
    (host_stopx,            $,  1,  0,  1), 
    (host_ismain,           $,  1,  1,  0), 
    (host_waitkey,          $,  0,  1,  0),
    (host_testkey,          $,  0,  1,  0),
    (host_execwait,         $,  3,  1,  0),
    (host_execcmd,          $,  3,  1,  0),
    (host_shellexec,        $,  2,  1,  0),
    (host_system,           $,  1,  1,  0),

    (host_makestr,          $,  2,  1,  0),
    (host_makestrslice,     $,  2,  1,  0),
    (host_makeref,          $,  2,  1,  0),

    (host_new,              $,  4,  1,  0),
    (host_setoverload,      $,  3,  0,  0),

    (host_getcmdparam,      $,  1,  1,  0),
    (host_gethostname,      $,  0,  1,  0),
    (host_getprogname,      $,  0,  1,  0),

    (host_$setpcerror,      $,  1,  0,  0),
    (host_$setdebug,        $,  1,  0,  0),
    (host_$test2,           $,  2,  1,  0),
    (host_$test,            $,  1,  1,  0),
    (host_$refcount,        $,  1,  1,  0),

    (host_ticks,            $,  0,  1,  0),
    (host_clock,            $,  0,  1,  0),
    (host_sleep,            $,  1,  0,  0),
    (host_random,           $,  1,  1,  0),
    (host_gethash,          $,  1,  1,  0),
    (host_getos,            $,  0,  1,  0),
    (host_gethostsize,      $,  0,  1,  0),
    (host_iswindows,        $,  0,  1,  0),
    (host_setmesshandler,   $,  1,  0,  0),
    (host_$setfprintf,      $,  2,  0,  0),

    (host_$getparam,        $,  1,  1,  0),
    (host_makelink,         $,  1,  1,  0),
    (host_allparams,        $,  1,  1,  0),
    (host_makeempty,        $,  1,  1,  0),
    (host_$procsymbols,     $,  0,  1,  0),
    (host_$symbolowner,     $,  1,  1,  0),
    (host_$symbolname,      $,  1,  1,  0),
    (host_$symboldefs,      $,  1,  1,  0),
    (host_$testcallback,    $,  0,  0,  0),
    (host_$smallmemtotal,   $,  0,  1,  0),
    (host_$id,              $,  1,  1,  0),
    (host_copy,             $,  1,  1,  0),
    (host_$nan,             $,  0,  1,  0),
    (host_$infinity,        $,  0,  1,  0),

    (host_last,             $,  0,  0,  1)
end

global enumdata []ichar errornames =
    (pc_error,          $),
    (user_error,        $),
    (type_error,        $),
    (mixedtype_error,   $),
    (divide_error,      $),
    (stopmodule_error,  $),
    (bounds_error,      $)
end

global array []byte D_binopset = (
    andlsym, orlsym, eqsym, nesym, ltsym, lesym, gtsym, gesym, addsym,
    subsym, mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym,
    shlsym, shrsym, minsym, maxsym, concatsym, powersym, isequalsym,
    idivremsym,  maths2sym, appendsym )

global array [0..symbolnames.upb]byte binopset

global array []byte D_unaryopset = (
    notlsym, inotsym, abssym, istruelsym, sqrsym, signsym, ascsym, chrsym,
    mathssym)

global array [0..symbolnames.upb]byte unaryopset

global array []byte D_addopset=(addsym, subsym, iandsym, iorsym, ixorsym,
        concatsym, appendsym, minsym, maxsym)

global array []byte D_cmpopset=(eqsym, nesym, ltsym, lesym, gesym, gtsym, isequalsym)

global array []byte D_mulopset=(mulsym, divsym, idivsym, iremsym, shlsym, shrsym)

global array [0..symbolnames.upb]byte addopset
global array [0..symbolnames.upb]byte cmpopset
global array [0..symbolnames.upb]byte mulopset
global array [0..symbolnames.upb]byte exprendset

global array []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,namesym,
    incrsym,decrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,
    stringconstsym,stdtypesym,kptypesym,kmapsym,
    ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
    kerrorsym,krefsym, kcastsym, anddotsym, ellipsissym,
    knilsym, khostfnsym, kifsym,curlsym,
    krecordsym, kstructsym)

global array [0:symbolnames.len]byte exprstarterset

global const maxtype=250

global [0..maxtype]ichar ttname
global [0..maxtype]symbol ttnamedef
global [0..maxtype]int16 ttbasetype
global [0..maxtype]int16 tttarget

global [0..maxtype]int ttlower
global [0..maxtype]int ttlength

global [0..maxtype]unit ttlowerexpr
global [0..maxtype]unit ttlengthexpr

global [0..maxtype]int ttsize
global [0..maxtype]byte ttbitwidth
global [0..maxtype]symbol ttfields      
global [0..maxtype]byte ttcaligned
global [0..maxtype]symbol ttowner
global [0..maxtype]byte ttcat
global int ntypes

global const int maxuserxtype=5000
global int nuserxtypes
global int userxtypebase            
global ref userxrec userxmodelist   

global [0:maxuserxtype]symbol ttnamedefx
global [0:maxuserxtype]symbol ttnamedefx2
global [0:maxuserxtype]int ttposx
global [0:maxuserxtype]int ttxmap
global [0:maxuserxtype]byte ttxmoduleno

global [0..host_last]byte hostlvset

proc start=
    int i



    for i:=1 to D_binopset.len do
        binopset[D_binopset[i]]:=1
        exprstarterset[D_binopset[i]]:=1
    od

    for i:=1 to D_unaryopset.len do
        unaryopset[D_unaryopset[i]]:=1
        exprstarterset[D_unaryopset[i]]:=1
    od

    for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od

    exprendset[semisym]:=1
    exprendset[commasym]:=1
    exprendset[rsqsym]:=1
    exprendset[rbracksym]:=1
    exprendset[kendsym]:=1
    exprendset[kdosym]:=1
    exprendset[ktosym]:=1

    for i:=1 to D_addopset.len do addopset[D_addopset[i]]:=1 od
    for i:=1 to D_mulopset.len do mulopset[D_mulopset[i]]:=1 od
    for i:=1 to D_cmpopset.len do cmpopset[D_cmpopset[i]]:=1 od

    for i in 0..tlast do
        ttname[i]:=stdtypenames[i]
        ttbasetype[i]:=i
        ttlower[i]:=1
        ttbitwidth[i]:=stdtypewidths[i]
        ttsize[i]:=stdtypewidths[i]/8
        ttcat[i]:=gettypecat(i)

    od

    ntypes:=tlast

    hostlvset[host_iconvlc]:=1
    hostlvset[host_iconvuc]:=1

end

=== qq_show.m 0 0 29/43 ===

ref[0:]int labelmap
int currlineno
symbol currpclproc

strbuffer pclv
global ref strbuffer pcldest = &pclv

const logfile="qq.log"

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=       
    ref unitrec q
    symbol d
    int t,flags
    ichar idname
    int64 a
    real32 x32

    if p=nil then
        return
    fi
        currlineno:=p.pos iand 16777215



    print @dev,p,":"
    print @dev,getprefix(level,prefix,p)

    idname:=jtagnames[p.tag]
    if idname^='j' then ++idname fi
    print @dev,idname,,": "


    case p.tag
    when jname,jhostname then
        d:=p.def
        print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
        if d.truename then
            print @dev," ",d.truename
        fi

    when jintconst then
        print @dev,p.value

    when jboolconst then
        print @dev,(p.value=1|"true"|"false")

    when jenumconst then
        fprint @dev,"# (#:#)",p.value, strmode(p.mode),getenumname(p.mode, p.value)

    when jwordconst then
        print @dev,p.uvalue

    when jint128const then
        print @dev,p.qupper,p.qlower

    when jword128const then
        print @dev,p.qupper,p.qlower

    when jrealconst then
        print @dev,p.xvalue

    when jstringconst then
        fprint @dev,"""#""",p.svalue

    when jdecimal then
        print @dev,p.svalue,,"L"

    when jcmpchain then
        for i to 4 do
            if p.cmpgenop[i]=0 then exit fi
            print @dev,jtagnames[p.cmpgenop[i]],$
        od

    when joperator then
        print @dev, pclnames[p.pclopcode]




    when jmakelist then
        print @dev,p.lower,,":",=p.length,ttname[p.elemtype]


    when jtypeconst,jconvert then
        print @dev,ttname[p.mode]


    when jcallhost then
        print @dev,hostfnnames[p.index]+5

    esac

    println @dev
    flags:=jflags[p.tag]

    if flags>=1 then printunitlist(dev,p.a,level+1,"1") fi
    if flags=2 then printunitlist(dev,p.b,level+1,"2") fi

end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=      
    if p=nil then return fi

    while p do
        printunit(p,level,prefix,dev)
        p:=p.nextunit
    od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=      
    static array [1024]char str
    array [1024]char indentstr
    array [16384]char modestr

    indentstr[1]:=0
    
    if level>20 then level:=10 fi

    to level do
        strcat(&.indentstr,"- ")
    od


    strcpy(&.str,getlineinfok())
    strcat(&.str,&.indentstr)
    strcat(&.str,prefix)
    if prefix^ then
        strcat(&.str," ")
    fi

    return &.str
end

function getlineinfok:ichar=            
    static array [40]char str

    sprintf(&.str,"%04d ",currlineno)
    return &.str
end

proc writepcl(ref int pcstart,pc, ref int32 pclsource, int pass)=
    array [512]char str
    qd fmt
    int cmdcode,a,needcomma,i,offset,labeldone,commentdone,soffset
    ref strec d
    const tabx="!      ----------"


    cmdcode:=pc^

    memcpy(&fmt,&pclfmt[cmdcode],fmt.bytes)
    labeldone:=commentdone:=0

    case cmdcode
    WHEN KSKIP THEN
        RETURN

    when kprocdef then
        currpclproc:=cast((pc+1)^)
        gstr(tabx)
        gstr("Procdef:")
        gstr(currpclproc.name)
        gline()
        return
    when kprocend then
        gstr(tabx)
        gstrln("End")
        return
    esac

    offset:=PC-PCSTART

    soffset:=(pclsource+offset)^


    ++pc

    if currlineno then
        fprint @str,"# #: #: ",pc-1:"8zh", currlineno:"5", pc-pcstart-1:"4"
    else
        fprint @str,"# #: ",pc-1:"8zh",pc-pcstart-1:"4"
    fi

    gstr(&.str)

    offset:=pc-pcstart-1


    if labelmap^[offset] then
        glabeldef(offset)
        gstr(&.str)
    fi

    case cmdcode
    when kprocdef then
        currpclproc:=cast(pc^)
        return

    when kcomment then
        gstr("! ")
        gstrln(cast(pc^))
        return


    when klabeldef then
        gstr(ref strec(pc^).name)
    GSTR(" /")
    GSTR(NAMENAMES[REF STREC(PC^).NAMEID])
        ++PC

        gstrln(":")
        return


    esac



    strcpy(&.str,pclnames[cmdcode]+1)

    a:=1

    gs_leftstr(pcldest," ",7,'-')
    gs_leftstr(pcldest,&.str,10)
    gstr("     ")

    needcomma:=0

    for i:=a to 4 do
        case fmt[i]
        when cnone then
            exit
        else
            if needcomma then gstr(", ") fi

            strcpy(&.str,writepclopnd(fmt[i],pc++^,i,cmdcode, pass, pcstart))
            gstr(&.str)
            needcomma:=1
        esac
    od

    gline()
end

function writepclopnd(int fmt,int64 x,int n,cmdcode, pass,ref int pcstart)ichar=        
    static array [512]char str,str2
    symbol d
    ichar suffix,s
    int slen
    object p

    d:=ref strec(x)

    case fmt
    when cnone then
        return "None"

    when cint,cword then

        case fmt
        when cint then suffix:=""
        when cword then suffix:="u"
        else
            suffix:=""
        esac
        print @str,x,,suffix,$
        if cmdcode=kcallhost then
            strcat(str,hostfnnames[x]+5)
        fi

    when creal then
        strcpy(str,strreal(real@(x)))

    when crange then
        fprint @str,"#..#",x iand 0xFFFF'FFFF,x>>32

    when cstring then
        if pass<=2 then
            s:=cast(x)
            slen:=strlen(s)
            goto dostring
        else
            p:=cast(x)
            if (slen:=p.length)=0 then return """" fi
            s:=p.strptr
            goto dostring
        fi

    when cstringz then

        s:=cast(x)
        slen:=strlen(s)
    dostring::
        if slen>=255 then slen:=255 fi
        memcpy(&.str,s,slen)            
        str[slen+1]:=0
        convertstring(&.str,&.str2)
        fprint @str,"""#""",&.str2

    when cmemory then
        if pass<=2 then
            strcpy(str,d.name)
        else
            d:=nil
            for i to nstatics do
                if statictable[i]=variant(x) then
                    d:=staticdefs[i]
                    exit
                fi
            od

            fprint @str, "[#] (#:#)", x:"h",(d|d.owner.name|"?"),(d|d.name|"?")
        fi

    when cframe then
        if pass<=2 then
            strcpy(str,d.name)
        else
            d:=currpclproc.deflist

            while d do
                if d.nameid in [frameid, paramid] and d.index*16=x then
                    fprint @str,"[#] (#)",x%16,d.name
                    return str
                fi
                d:=d.nextdef
            od
        fi

    when csymbol then
        fprint @str,"[#]",d.name

    when cproc then
        if pass<=2 then
            strcpy(str,d.name)
        else
            d:=nil
            for i to nprocs do
                if proctable[i]=ref int(x) then
                    d:=procdefs[i]
                    exit
                fi
            od

            fprint @str, "[#] (#)", x:"h",(d|d.name|"?")
        fi

    when cdllproc then
        fprint @str,"[DLL:#]",getdottedname(d)

    when cgenfield then
        if pass<=2 then
            d:=symbol(x)
            fprint @str,".#",d.name
        else
            fprint @str,"## (#)","#",x, genfieldtable[x].def.name
        fi
    when ctype then
        fprint @str,"T:# <#>",strmode(x),int(x)

    when clabel then
        if pass<=2 then
            fprint @str,"L#",x
        else
            fprint @str,"&# (L#)",x:"h",ref int(x)-pcstart
        fi

    when coperator then
        fprint @str,"(#)",pclnames[x]

    else
    other::
        fprint @str,"<# #>",fmt,opndnames[fmt]
    esac
    return str
end

global proc writeallpcl(int n, pass)=
    int cmd,i,lastline,line,labno,offset,index,size,nopnds,x,y
    ref int pc,pclcode
    ref int32 pclsource
    ichar name

    currlineno:=0



    if pass=3 and not hasbytecodes then
        gstrln("Can't show PCL; use -debug")
        return
    fi

    gstr("PCL FOR MODULE:")
    gstrln(moduletable[n].name)

    pc:=pclcode:=moduletable[n].pcstart
    pclsource:=moduletable[n].pcsrcstart

    size:=moduletable[n].pcsize
    labelmap:=pcm_allocz((size+1)*int.bytes)

    repeat
        cmd:=pc^
        nopnds:=pclnopnds[cmd]

        for i to nopnds do
            case pclfmt[cmd,i]
            when cnone then
                exit
            when clabel then
                x:=(pc+i)^
                if pass=3 then
                    x:=ref int(x)-pclcode
                fi

                labelmap^[x]:=1     
            esac

        od
        pc+:=pclnopnds[cmd]+1
    until cmd in [kzero,kendmodule]

    pc:=moduletable[n].pcstart

    repeat
        cmd:=pc^


        writepcl(pclcode,pc,pclsource, pass)
        pc+:=pclnopnds[cmd]+1
    until cmd in [kzero,kendmodule]

    gline()
    pcm_free(labelmap,(size+1)*int.bytes)


end

proc gstr(ichar s)=
    gs_str(pcldest,s)
end

proc gstrln(ichar s)=
    gs_strln(pcldest,s)
end

proc gline=
    gs_line(pcldest)
end

proc gstrint(int a)=
    gs_strint(pcldest,a)
end

proc glabeldef(word64 lab)=
    while lab do
        gstr("L")
        gstrint(lab iand 0xFFFF)
        gstr(": ")
        lab>>:=16
    od
    gline()
end

global proc printglobalsymbols(filehandle f=nil)=
    println @f,"PROC Global Symbol Table"
    println @f

    printst(f,stprogram)


end

global proc printst(filehandle f,symbol p,int level=0)=
    ref strec q

    printstrec(f,p,level)

    q:=p.deflist

    while q<>nil do
        printst(f,q,level+1)
        q:=q.nextdef
    od
end

global proc printglobalsymbols_full(filehandle f=nil)=
    println @f,"\nPROC Global All Symbols in Full"
    println @f

    printstfull(f,stprogram)
end

global proc printstfull(filehandle f,symbol p,int level=0)=
    ref strec q

    printstrecfull(f,p)

    q:=p.deflist

    while q<>nil do
        printstfull(f,q,level+1)
        q:=q.nextdef
    od
end

proc printstrec(filehandle f,symbol p,int level)=
    strec dd
    ref byte q
    strbuffer v
    ref strbuffer d:=&v
    int col,offset,n
    const tabstr="    "
    array [256]char str
    ichar s




    offset:=0
    to level do
        print @f,tabstr
        offset+:=4
        col+:=4
    od

    print @f,padstr(p.name,22-offset,"-")
    print @f, padstr(namenames[p.nameid],12,".")

    col:=40
    dd:=p^



    if p.isimport then
        print @f,"Imp "
    elsif p.isglobal then
        print @f,(p.isglobal|"Glob ","Exp "|"Local ")
    fi


    if p.mbyref then
        print@f,"byref "
    fi
    if p.moptional then
        print@f,"opt "
    fi

    if dd.moduleno then
        fprint @f,"Modno:#",dd.moduleno
    fi

    print @f,"=========="

    if p.owner then
        fprint @str,"(#)",p.owner.name
        print @f, padstr(&.str,18,"-")
    else
        print @f, padstr("()",18,"-")
    fi


    case p.nameid
    when fieldid,frameid,paramid,enumid then
        print @f," Ix:",p.index,," "
        if p.nameid=fieldid and p.atfield then
            print @f,"@",p.atfield.name,$
        fi
        print @f," Offset:",p.fieldoffset,," "
    when structfieldid then
        print @f," Offset:",p.fieldoffset,," "
    when recordid then
        print @f," Nfields:",p.nfields,," "
    when procid, dllprocid then
        fprint @f," Nparms:# ",p.nparams


    esac    

    case p.nameid
    when frameid, staticid,constid,macroid,paramid,dllparamid then
        if p.code then
            case p.initcode
            when 3 then s:="::="
            when 2 then s:=":="
            else s:="="
            esac

            print @f, s, strexpr(p.code).strptr,$
        fi
    esac



        fprint @f,"Mode:#",strmode(p.mode)


    PRINT @F," Moduleno:",P.MODULENO

    println @f
    ichar tab:="          "

end

proc printstrecfull(filehandle f,symbol p)=
    symbol q
    ref symbol r
    const tab="\t"
    int n


    fprintln @f,"Name: ""#"" <#>",p.name,namenames[p.nameid]

    if p.owner then
        println @f,tab,=p.owner.name
    fi
    q:=p.deflist
    if q then
        print @f,tab,"P.DEFLIST="
        while q do
            print @f," ",,q.name
            q:=q.nextdef
        od
        println @f
    fi

    println @f,tab,=p.moduleno

    case p.nameid
    when programid then
    when procid then
        println @f,tab,=p.pcaddress
        println @f,tab,"P.MODE=",strmode(p.mode)
        println @f,tab,=p.nparams
        println @f,tab,=p.nlocals
        println @f,tab,=p.isglobal
        println @f,tab,=p.mvarparams
        println @f,tab,=p.labelno
        println @f,tab,=p.procfixed

    when dllprocid then
        println @f,tab,=p.truename
        println @f,tab,"P.MODE=",strmode(p.mode)
        println @f,tab,=p.index
        println @f,tab,=p.nparams
        println @f,tab,=p.mode,strmode(p.mode)
        println @f,tab,=p.isglobal
        println @f,tab,=p.mvarparams

    when frameid then
        println @f,tab,"P.MODE=",strmode(p.mode)
        println @f,tab,=p.index
        println @f,tab,=p.mutable

    when paramid, dllparamid then
        println @f,tab,"P.MODE=",strmode(p.mode)
        println @f,tab,=p.index
        println @f,tab,=p.moptional
        println @f,tab,=p.mbyref
        println @f,tab,=p.mutable
        if p.code then
            println @f,tab,"P.CODE=",STREXPR_S(p.code)
        fi

        if p.nameid=dllparamid then
            println @f,tab,=p.mode,strmode(p.mode)
        fi

    when fieldid,structfieldid then
        println @f,tab,"P.MODE=",strmode(p.mode)
        println @f,tab,=p.firstdupl.genfieldindex
        println @f,tab,=p.fieldoffset

    when labelid then
        println @f,tab,=p.labelno

    when aliasid then
        println @f,tab,"Aliases:",p.alias.name

    when typeid then
        println @f,tab,"P.MODE=",strmode(p.mode)
        n:=ttlength[p.mode]
        r:=p.topfieldlist
        if r then
            for i to n do
                println @f,tab,tab,i,":",R.name
                ++r
            od
        fi

    esac

    println @f

end

global proc printtypetables(filehandle f)=
    symbol d

    println @f,"PROC TYPE TABLES"
    for m:=tlast+1 to ntypes do
        fprintln @f, "#: # ",m:"3",ttname[m]:"jl12"
        d:=ttnamedef[m]

            println @f,"    ST=",d
            println @f,"    Len=",ttlength[m], "Lower",ttlower[m]
            println @f,"    Size=",ttsize[m]
            println @f,"    Basetype=",ttbasetype[m],ttname[ttbasetype[m]]
            println @f,"    Target=",tttarget[m],ttname[tttarget[m]]
            println @f,"    Caligned=",ttcaligned[m]

        d:=ttfields[m]
        if d then
            println @f,"    Fields:"
            while d do
                println @f,"        ",d.name, (d.mode|strmode(d.mode)|"")
                d:=d.nextdef
            od
        fi



    od

end

global function getpclname(ref void fnaddr)ichar=
    return pclnames[getpclcode(fnaddr)]
end

function getpclcode(ref void fnaddr)int=
    for i to pclnames.len do
        if fnaddr=khandlertable[i] then
CPL "FOUND",I,FNADDR, KHANDLERTABLE[I]
            return i
        fi
    od
    return 0
end

global proc showsttree=
    filehandle f
    ref modulerec m
    symbol d
    ref genfieldrec g
    ref procrec p

    return unless fshowst

    f:=fopen("ST","w")
    printglobalsymbols(f)
    printglobalsymbols_full(f)

    println @f
    println @f,"Modules",nmodules
    for i to nmodules do
        m:=&moduletable[i]
        println @f,"    ",,i,,":",m.name,=m.parsed,=m.pcstart,=m.pcsize
    od

    println @f
    println @f,"Source Files",nsourcefiles
    for i to nsourcefiles do
        println @f,"    ",,i,,":",sourcefilenames[i],=sourcefilesys[i],=sourcefilesupport[i]
    od

    println @f
    println @f,"PROC Global GenField Table",ngenfields
    for i to ngenfields do
        g:=genfieldtable[i]
        if g=nil then next fi
        fprintln @f,"   #) #:",i,g.def.name
        while g do
            d:=g.def
            println @f,"      ",d.name, namenames[d.nameid],d.owner.name
            g:=g.nextdef
        od
    od
    println @f


    println @f,"DLL Table", ndlllibs
    for i to ndlllibs do
        println @f, i,":",dlltable[i].name, dllinsttable[i]
    od
    println @f

    println @f,"DLL Proc Table", ndllprocs
    for i to ndllprocs do
        d:=dllproctable[i]
        println @f, i,":",d.name, dlllibindex[i], dllprocaddr[i],(d.mvarparams|"Variadic"|"")
    od
    println @f

    println @f,"All Proc Table",nproclist
    p:=proclist
    while p do
        println @f,"Proc:",p.def.name,p.def.owner.name
        p:=p.nextproc
    od
    println @f


    fclose(f)
end

global proc showtypes=
    filehandle f
    ref modulerec m

    return unless fshowtypes
    return when runcode=run_cc

    f:=fopen("TYPES","w")
    printtypetables(f)

    fclose(f)
end

global proc showpcl(int pass)=
    filehandle f

    return when runcode=run_cc

    gs_init(pcldest)
    gs_str(pcldest,"PROC ALL PCL pass:")
    gs_strint(pcldest,pass)
    gs_line(pcldest)

    for i to nmodules do
        writeallpcl(i,pass)
    od

    f:=fopen((pass|"PCL1","PCL2"|"PCL3"),"w")
    if not f then return fi
    gs_println(pcldest,f)

    fclose(f)
end

global proc showast(ichar file)=
    filehandle f
    symbol d
    int k,i

    return when runcode=run_cc

    f:=fopen(file,"w")
    return unless f

    println @f,"PROC",file,,":"

    for i to nmodules do
        println @f,"MODULE:",moduletable[i].name
        printunit(moduletable[i].ast, dev:f)
        d:=moduletable[i].def.deflist
        while d do
            if d.nameid=procid then
                println @f,"\n---PROC",d.name
                printunit(d.code, dev:f)
            fi
            d:=d.nextdef
        od
        

    od

    printunit(lastast,dev:f)
    println @f


    fclose(f)
end

global proc showlogfile=
    array [256]char str
    filehandle logdev


    if fshowpcl1+fshowpcl2+fshowpcl3+fshowast1+fshowast2+
            fshowst+fshowtypes+fshowmodules+fshowstflat=0 then return fi
    if runcode=run_cc then
        return
    fi

    if fshowst then
        showsttree()
    fi

    if fshowstflat then
        showstflat()
    fi

    if fshowtypes then
        showtypes()
    fi

    logdev:=fopen(logfile,"w")

    if fshowmodules then showprojectinfo(logdev) fi

    if runcode>=fixup_cc and fshowpcl3 then addtolog("PCL3",logdev) fi
    if runcode>=gencode_cc and foptimise and fshowpcl2 then addtolog("PCL2",logdev) fi
    if runcode>=gencode_cc and fshowpcl1 then addtolog("PCL1",logdev) fi
    if runcode>=names_cc and fshowast2 then addtolog("AST2",logdev) fi
    if runcode>=parse_cc and fshowast1 then addtolog("AST1",logdev) fi
    if fshowst then addtolog("ST",logdev) fi
    if fshowstflat then addtolog("STFLAT",logdev) fi
    if fshowtypes then addtolog("TYPES",logdev) fi
    fclose(logdev)

    fprint @&.str,"c:/m/med.bat -w #",logfile

    os_execwait(&.str,1,nil)

end

proc addtolog(ichar filename, filehandle logdest)=
filehandle f
int c

f:=fopen(filename,"rb")
if f=nil then return fi

do
    c:=fgetc(f)
    exit when c=c_eof
    fputc(c,logdest)
od
fclose(f)
end

global proc showstflat=
    filehandle f
    symbol p

    return unless fshowstflat

    f:=fopen("STFLAT","w")

    println @f,"GLOBAL FLAT SYMBOL TABLE:"

    for i:=0 to hashtable.upb-1 do
        p:=cast(&hashtable[i])
        if p.name then
            case p.symbolcode
            when namesym then
                println @f,i,p,":",p.name,symbolnames[p.symbolcode],namenames[p.nameid]
                p:=p.nextdupl
                while p do
                    int sym:=p.symbolcode
                    if sym=0 then sym:=errorsym fi
                    println @f,"    ",p,p.name,symbolnames[sym],namenames[p.nameid],
                        "(From",(p.owner|p.owner.name|"-"),,")"
                    p:=p.nextdupl
                od
            esac
        fi
    od
    fclose(f)
end

=== qq_vars.m 0 0 30/43 ===









global macro var_share(x) = if x.hasref then ++x^.objptr.refcount fi
global macro var_unshare(x) = var_unshareu(x) when x.hasref
global macro var_dupl(x) = var_duplu(x) when x.hasref
global macro var_shareu(x) = ++x^.objptr^.refcount

objrec zeroobj

global proc var_unshareu(variant p)=
    if --p^.objptr^.refcount<=0 then
        var_free(p)
    fi
end


global proc obj_shareu(object p)=
    ++p^.refcount
end

global function void_new:variant p =
    p:=pcm_alloc(varrec.bytes)
    p.tagx:=tvoid
    return p
end

global function obj_new:object p=
    p:=pcm_alloc32()
    p^:=zeroobj
    p.refcount:=1
    return p
end

global function var_getintvalue(variant p)int =
    switch p^.tag
    when tint,ttype,tword then
        return p.value
    when treal then
        return p.xvalue
    else
        pcustype("getintvalue",p)
    endswitch
    return 0
end

global proc var_fromobj(int tag,object p, variant dest)=
    dest.tagx:=tag ior hasrefmask
    dest.objptr:=p
end

global proc var_empty(int tag, variant dest, int param=0)=
    switch tag
    else
        pcustype_t("empty", tag)
    endswitch
end

global proc var_new(int tag, variant dims, param1, param2, dest)=
    switch tag
    else
        pcustype_t("new", tag)
    endswitch
end

global proc var_make(int tag, variant data, dest, int n,  param=0)=
    switch tag
    else
        pcustype_t("make", tag)
    endswitch
end

global proc var_free(variant a)=
    varrec v
    object q:=a.objptr


    case q.objtype
    when normal_obj then
        switch a.tag
        when tlist then
            obj_free_list(q)
        when trecord then
            obj_free_record(q)
        when tstring then
            obj_free_string(q)
        when tarray then
            obj_free_array(q)
        when tuserarray then
            obj_free_userarray(q)
        when tbits then
            obj_free_bits(q,a.tag)
        when tstruct then
            obj_free_struct(q)
        when tdict then
            obj_free_dict(q)
        when tset then
            obj_free_set(q)
        when tdecimal then
            obj_free_dec(q)

        else
            pcustype("free", a)
        endswitch
    when slice_obj then
        v.tagx:=a.tag
        v.objptr:=q.objptr2
        var_unshareu(&v)
        pcm_free32(q)
    else
        pcm_free32(q)
    esac
end

global proc var_duplu(variant a)=
    switch a.tag
    when tstring then
        var_dupl_string(a)
    when tset then
        var_dupl_set(a)
    when tlist then
        var_dupl_list(a)
    when tdict then
        var_dupl_dict(a)
    when tarray then
        var_dupl_array(a)
    when tuserarray then
        var_dupl_userarray(a)
    when tbits then
        var_dupl_bits(a)
    when trecord then
        var_dupl_record(a)
    when tstruct then
        var_dupl_struct(a)
    when tdecimal then
        var_dupl_dec(a)
    else
        pcustype_t("dupl", a.tag)
    endswitch




end

global proc var_neg(variant a)=
    switch a.tag
    when tint, tword then
        a.value:=-a.value
    when treal then
        a.xvalue:=-a.xvalue
    when tdecimal then
        var_dupl_dec(a)
        var_neg_dec(a)
    when tset then
        var_dupl_set(a)
        var_inotto_set(a)
    else
        pcustype_t("neg", a.tag)
    endswitch
end

global proc var_abs(variant a)=
    switch a.tag
    when tint, tword then
        a.value:=abs a.value
    when treal then
        a.xvalue:= abs a.xvalue
    when tdecimal then
        var_dupl_dec(a)
        var_abs_dec(a)
    else
        pcustype_t("abs", a.tag)
    endswitch
end

global proc var_inot(variant a)=
    switch a.tag
    when tint, tword then
        a.value:=inot a.value
    when tset then
        var_dupl_set(a)
        var_inotto_set(a)
    else
        pcustype_t("inot", a.tag)
    endswitch
end

global function var_istruel(variant a)int=
    switch a.tag
    when tint, tword, trefvar, trefpack, trefbit, ttype, tsymbol,tbool then
        return istrue a.value
    when treal then
        return (a.xvalue<>0|1|0)
    when tstring,tlist,tarray,tbits,tuserarray then
        return a.objptr.length<>0
    when tset then
        return a.objptr.length<>0
    when trecord, tstruct then
        return 1
    when tdecimal then
        return not bn_iszero(a.objptr)
    else
        pcustype_t("istruel", a.tag)
    endswitch
    return 0
end

global function var_negto(variant p)int=
    variant a:=p.varptr

    if p.tag<>trefvar then
        return 0
    fi
    switch a.tag
    when tint, tword then
        a.value:=-a.value
    when treal then
        a.xvalue:=-a.xvalue
    when tset then
        var_inotto_set(a)
    else
        pcustype_t("negto", a.tag)
    endswitch
    return 1
end

global function var_absto(variant p)int=
    variant a:=p.varptr

    if p.tag<>trefvar then
        return 0
    fi
    switch a.tag
    when tint, tword then
        a.value:= abs a.value
    when treal then
        a.xvalue:= abs a.xvalue
    else
        pcustype_t("absto", a.tag)
    endswitch
    return 1
end

global function var_inotto(variant p)int=
    variant a:=p.varptr

    if p.tag<>trefvar then
        return 0
    fi
    switch a.tag
    when tint, tword then
        a.value:=inot a.value
    when tset then
        var_inotto_set(a)
    else
        pcustype_t("inotto", a.tag)
    endswitch
    return 1
end

global proc var_add(variant a, b)=
    if a.tag<>b.tag then
        var_addmixed(a,b)
        return
    fi


    switch a.tag
    when tint, tword then
        a.value+:=b.value
    when treal then
        a.xvalue+:=b.xvalue
    when tdecimal then
        var_add_dec(a,b)
    when tstring then
        var_add_string(a,b)
    when tset then
        var_dupl_set(a)
        var_iorto_set(a,b)
    else
        pcustype_t("add", a.tag)
    endswitch
end

global proc var_addmixed(variant a, b)=
    int newtag:=a.tag

    int tt

    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        newtag:=treal
        a.xvalue:=a.value+b.xvalue
    when pr(treal,      tint)     then
        a.xvalue+:=b.value
    when pr(tint,       tword)    then
        a.value+:=b.value
    when pr(tword,      tint)     then
        newtag:=tint
        a.value+:=b.value
    when pr(tint,tdecimal),
         pr(treal,tdecimal) then
        newtag:=tdecimal
        var_add_dec(dectemp(a),b)
        freedectemp()
    when pr(tdecimal,tint),
         pr(tdecimal,treal) then
        var_add_dec(a,dectemp(b))

    when pr(trefpack,   tint) then
        if a.ptr=nil then pcerror("Nil+x") fi
        a.ptr+:=ttsize[a.elemtag]*b.value
    else
        pcmxtypes("Addmixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_sub(variant a, b)=
    ref byte p,q
    int elemsize,x

    if a.tag<>b.tag then
        var_submixed(a,b)
        return
    fi
    switch a.tag
    when tint, tword then
        a.value-:=b.value
    when treal then
        a.xvalue-:=b.xvalue
    when tdecimal then
        var_sub_dec(a,b)
    when trefpack then
        p:=a.ptr
        q:=b.ptr
        case elemsize:=ttsize[a.elemtag]
        when 1 then x:=p-q
        when 2 then x:=(p-q)>>1
        when 4 then x:=(p-q)>>2
        else x:=(p-q)/elemsize
        esac
        a.tagx:=tint
        a.value:=x
    else
        pcustype_t("sub", a.tag)
    endswitch
end

global proc var_submixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        newtag:=treal
        a.xvalue:=a.value-b.xvalue
        
    when pr(treal,      tint)     then
        a.xvalue-:=b.value
    when pr(tint,       tword)    then
        a.value-:=b.value
    when pr(tword,      tint)     then
        newtag:=tint
        a.value-:=b.value
    when pr(tint,tdecimal),
         pr(treal,tdecimal) then
        newtag:=tdecimal
        var_sub_dec(dectemp(a),b)
        freedectemp()
    when pr(tdecimal,tint),
         pr(tdecimal,treal) then
        var_sub_dec(a,dectemp(b))


    when pr(trefpack,   tint) then
        a.ptr-:=ttsize[a.elemtag]*b.value
    else
        pcmxtypes("Submixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_mul(variant a, b)=
    if a.tag<>b.tag then
        var_mulmixed(a,b)
        return
    fi
    switch a.tag
    when tint,tword then
        a.value*:=b.value
    when treal then
        a.xvalue*:=b.xvalue
    when tdecimal then
        var_mul_dec(a,b)
    when tset then
        var_dupl_set(a)
        var_iandto_set(a,b)
    else
        pcustype_t("mul", a.tag)
    endswitch
end

global proc var_mulmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        newtag:=treal
        a.xvalue:=a.value*b.xvalue
        
    when pr(treal,      tint)     then
        a.xvalue*:=b.value
    when pr(tint,       tword)    then
        a.value*:=b.value
    when pr(tword,      tint)     then
        newtag:=tint
        a.value*:=b.value
    when pr(tint,tdecimal),
         pr(treal,tdecimal) then
        newtag:=tdecimal
        var_mul_dec(dectemp(a),b)
        freedectemp()
    when pr(tdecimal,tint),
         pr(tdecimal,treal) then
        var_mul_dec(a,dectemp(b))

    when pr(tstring, tint) then
        var_mul_string(a,b.value)
    when pr(tlist, tint) then
        var_mul_list(a,b.value)
    else
        pcmxtypes("Mulmixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_div(variant a, b)=
    if a.tag<>b.tag then
        var_divmixed(a,b)
        return
    fi
    switch a.tag
    when tint then
        a.tagx:=treal
        a.xvalue:=real(a.value)/b.value
    when treal then
        a.xvalue/:=b.xvalue
    when tdecimal then
        var_div_dec(a,b)
    else
        pcustype_t("div", a.tag)
    endswitch
end

global proc var_divmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        newtag:=treal
        a.xvalue:=a.value/b.xvalue
    when pr(treal,      tint)     then
        a.xvalue/:=b.value
    when pr(tint,       tword)    then
        newtag:=treal
        a.xvalue:=real(a.value)/real(b.value)
    when pr(tword,      tint)     then
        newtag:=treal
        a.xvalue:=real(a.value)/real(b.value)
    else
        pcmxtypes("Divmixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_idiv(variant a, b)=
    if a.tag<>b.tag then
        var_idivmixed(a,b)
        return
    fi
    switch a.tag
    when tint then
        if b.value then
            a.value:=a.value/b.value
        else
            pcerror("Divide by 0")
        fi
    when tdecimal then
        var_idiv_dec(a,b)
    else
        pcustype_t("idiv", a.tag)
    endswitch
end

global proc var_idivmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       tword)    then
        a.value:=a.value%b.uvalue
    when pr(tword,      tint)     then
        newtag:=tint
        a.value:=a.uvalue%b.value
    else
        pcmxtypes("Idivmixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_irem(variant a, b)=
    if a.tag<>b.tag then
        var_iremmixed(a,b)
        return
    fi

    switch a.tag
    when tint then
        a.value:=a.value rem b.value

    when tdecimal then
        var_irem_dec(a,b)
    else
        pcustype_t("irem", a.tag)
    endswitch
end

global proc var_iremmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       tword)    then
        a.value:=a.value rem b.uvalue
    when pr(tword,      tint)     then
        newtag:=tint
        a.value:=a.uvalue rem b.value
    else
        pcmxtypes("Iremmixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_iand(variant a, b)=
    if a.tag<>b.tag then
        var_iandmixed(a,b)
        return
    fi
    switch a.tag
    when tint, tword then
        a.value iand:= b.value
    when tset then
        var_dupl_set(a)
        var_iandto_set(a,b)
    else
        pcustype_t("iand", a.tag)
    endswitch
end

global proc var_iandmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint, tword), pr(tword, tint) then
        newtag:=tint
        a.value iand :=b.value
    else
        pcmxtypes("Iandmixed",a,b)
    esac

    a.tag:=newtag
end


global proc var_ior(variant a, b)=
    if a.tag<>b.tag then
        var_iormixed(a,b)
        return
    fi
    switch a.tag
    when tint, tword then
        a.value ior:=b.value

    when tset then
        var_dupl_set(a)
        var_iorto_set(a,b)
    else
        pcustype_t("ior", a.tag)
    endswitch
end

global proc var_iormixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint, tword), pr(tword, tint) then
        newtag:=tint
        a.value ior :=b.value
    else
        pcmxtypes("Iormixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_ixor(variant a, b)=
    if a.tag<>b.tag then
        var_ixormixed(a,b)
        return
    fi
    switch a.tag
    when tint, tword then
        a.value ixor :=b.value
    when tset then
        var_dupl_set(a)
        var_ixorto_set(a,b)
    else
        pcustype_t("ixor", a.tag)
    endswitch
end

global proc var_ixormixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint, tword), pr(tword, tint) then
        newtag:=tint
        a.value iand :=b.value
    else
        pcmxtypes("Ixormixed",a,b)
    esac

    a.tag:=newtag
end

global proc var_shl(variant a, b)=
    if a.tag<>b.tag then
        var_shlmixed(a,b)
        return
    fi
    switch a.tag
    when tint then
        a.value<<:=b.value

    else
        pcustype_t("shl", a.tag)
    endswitch
end

global proc var_shlmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       tword)    then
        a.value<<:=b.value
    when pr(tword,      tint)     then
        newtag:=tint
        a.uvalue<<:=b.value
    else
        pcmxtypes("shlmixed",a,b)
    esac

    a.tag:=newtag
end


global proc var_shr(variant a, b)=
    if a.tag<>b.tag then
        var_shrmixed(a,b)
        return
    fi
    switch a.tag
    when tint then
        a.value>>:=b.value
    else
        pcustype_t("shr", a.tag)
    endswitch
end

global proc var_shrmixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    else
        pcmxtypes("shrmixed",a,b)
    esac

    a.tag:=newtag
end

global function var_in(variant a, b)int=
    case pr(a.tag,b.tag)
    when pr(tint, tset) then
        return var_in_set(a,b)
    when pr(tint, trange) then
        return (a.value in b.range_lower..b.range_upper|1|0)
    when pr(tstring,tstring) then
        return var_in_string(a,b)
    elsecase b.tag
    when tlist then
        return var_in_list(a,b)
    else
        pcmxtypes("in", a,b)
    esac
    return 0

end

global function var_equal(variant a,b)int=
    if a.tag<>b.tag then
        return var_equalmixed(a,b)
    fi

    switch a.tag
    when tint, tword, trefvar, trefpack, ttype, tsymbol, tbool then
        return a.value=b.value
    when treal then
        return (a.xvalue=b.xvalue|1|0)
    when tdecimal then
        return var_equal_dec(a, b)
    when tstring then
        return var_equal_string(a, b)
    when tset then
        return var_equal_set(a, b)
    when tlist then
        return var_equal_list(a, b)
    when tdict then
        return var_equal_dict(a, b)
    when tarray, tuserarray then
        return var_equal_array(a, b)
    when tbits then
        return var_equal_bits(a, b)
    when trecord then
        return var_equal_record(a, b)
    when tstruct then
        return var_equal_struct(a, b)
    else
        pcustype_t("equal", a.tag)
    endswitch
    return 0

end

global function var_equalmixed(variant a, b)int=
    int result

    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        return (a.value=b.xvalue|1|0)
        
    when pr(treal,      tint)     then
        return (a.xvalue=b.value|1|0)
    when pr(tint,       tword)    then
        return (a.value=b.uvalue|1|0)
    when pr(tword,      tint)     then
        return (a.uvalue=b.value|1|0)
    when pr(tint,tdecimal),
         pr(treal,tdecimal) then
        result:=var_equal_dec(dectemp(a),b)
        freedectemp()
        return result
    when pr(tdecimal,tint),
         pr(tdecimal,treal) then
        return var_equal_dec(a,dectemp(b))

    else
        return 0
    esac

    return 0
end

global function var_compare(variant a,b)int=
    if a.tag<>b.tag then
        return var_comparemixed(a,b)
    fi

    switch a.tag
    when tint, tword ,trefpack then
        return (a.value<b.value|-1|(a.value>b.value|1|0))
    when treal then
        return (a.xvalue<b.xvalue|-1|(a.xvalue>b.xvalue|1|0))
    when tdecimal then
        return var_compare_dec(a,b)
    when tstring then
        return var_compare_string(a,b)
    else
        pcustype_t("compare", a.tag)
    endswitch
    return 0
end

global function var_comparemixed(variant a, b)int=
    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        return (a.value<b.xvalue|-1|(a.value>b.xvalue|1|0))
    when pr(treal,      tint)     then
        return (a.xvalue<b.value|-1|(a.xvalue>b.value|1|0))
    else
        pcmxtypes("comparemixed",a,b)
    esac

    return 0
end


global proc var_concat(variant a,b)=
    if a.tag<>b.tag then
        pcmxtypes("Concat",a,b)
    fi

    switch a.tag
    when tstring then
        var_add_string(a,b)
    when tlist then
        var_dupl_list(a)
        var_concatto_list(a,b)
    when tarray then
        var_dupl_array(a)
        var_concatto_array(a,b)
    else
        pcustype_t("concat", a.tag)
    endswitch
end

global proc var_append(variant a,b)=

    if a.tag<>b.tag then
        case a.tag
        when tlist then dolist
        when tarray then doarray
        when tbits then dobits
        esac
        error
    elseswitch a.tag
    when tstring then
        var_add_string(a,b)
        var_unshareu(b)     
    when tlist then
dolist::
        var_dupl_list(a)
        var_appendto_list(a,b)
    when tarray then
doarray::
        var_dupl_array(a)
        var_appendto_array(a,b)
    when tbits then
dobits::
        var_dupl_bits(a)
        var_appendto_bits(a,b)
    else
error::
        pcustype_t("append", a.tag)
    end
end

global proc var_min(variant a,b)=
    if a.tag<>b.tag then
        var_minmixed(a,b)
        return
    fi
    pcerror("VARMIN")


end

global proc var_minmixed(variant a,b)=
    pcerror("VARMINMIX")
end

global proc var_max(variant a,b)=
    if a.tag<>b.tag then
        var_maxmixed(a,b)
        return
    fi
    pcerror("VARMAX")
end

global proc var_maxmixed(variant a,b)=
    pcerror("VARMAXMIX")
end

global function var_addto(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    newtag:=a.tag

    if a.tag<>b.tag then
        if newtag=tstring and b.tag=tint then
            var_addto_string_ch(a,b.value)
            return 1
        fi
        return 0

    fi

    switch a.tag
    when tint, tword then
        a.value+:=b.value
    when treal then
        a.xvalue+:=b.xvalue
    when tstring then
        var_addto_string(a,b)
    when tset then
        var_iorto_set(a,b)
    else
        return 0
    endswitch

    a.tag:=newtag

    return 1
end

global function var_subto(variant p,b)int=
    return 0
end

global function var_multo(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    newtag:=a.tag

    if a.tag<>b.tag then
        return 0
    fi

    switch a.tag
    when tset then
        var_iandto_set(a,b)
    else
        return 0
    endswitch

    a.tag:=newtag

    return 1
end

global function var_divto(variant p,b)int=
    return 0
end

global function var_idivto(variant p,b)int=
    return 0
end

global function var_iandto(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    newtag:=a.tag

    if a.tag<>b.tag then
        return 0
    fi

    switch a.tag
    when tset then
        var_iandto_set(a,b)
    else
        return 0
    endswitch

    a.tag:=newtag

    return 1
end

global function var_iorto(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    newtag:=a.tag

    if a.tag<>b.tag then
        return 0
    fi

    switch a.tag
    when tset then
        var_iorto_set(a,b)
    else
        return 0
    endswitch

    a.tag:=newtag

    return 1
end

global function var_ixorto(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    newtag:=a.tag

    if a.tag<>b.tag then
        return 0
    fi

    switch a.tag
    when tset then
        var_ixorto_set(a,b)
    else
        return 0
    endswitch

    a.tag:=newtag

    return 1
end

global function var_shlto(variant p,b)int=
    return 0
end

global function var_shrto(variant p,b)int=
    return 0
end

global function var_concatto(variant a,b)int=
    if a.tag<>b.tag then pcerror("concatto/mixed") fi
    switch a.tag
    when tstring then
        var_addto_string(a,b)
    when tlist then
        var_concatto_list(a,b)
    when tarray then
        var_concatto_array(a,b)
    else
        pcustype("concat",a)
    endswitch
    return 1
end

global function var_appendto(variant a,b)int=
    if a.tag<>b.tag then
        case a.tag
        when tlist then dolist
        when tarray then doarray
        when tbits then dobits
        else
            pcerror("appendto/mixed")
        esac
    fi

    switch a.tag
    when tstring then
        var_addto_string(a,b)
        var_unshareu(b)     
    when tlist then
dolist::
        var_appendto_list(a,b)
    when tarray then
doarray::
        var_appendto_array(a,b)
    when tbits then
dobits::
        var_appendto_bits(a,b)
    else
        pcustype("append",a)
        return 0
    endswitch
    return 1
end

global proc var_getix(variant a, int index)=
    switch a.tag
    when tstring then
        var_getix_string(a,index)
    when tlist then
        var_getix_list(a,index)
    when tarray,tuserarray then
        var_getix_array(a,index)
    when tbits then
        var_getix_bits(a,index)
    when tset then
        var_getix_set(a,index)
    when trecord then
        var_getix_record(a,index)
    when trange then
        if index in a.range_lower..a.range_upper then
            a.tagx:=tint
            a.value:=index
        else
            pcerror("range/bounds")
        fi

    else
        pcustype_t("getix", a.tag)
    endswitch
end

global proc var_putix(variant a, int index, variant x)=

    switch a.tag
    when tstring then
        var_putix_string(a,index,x)
        var_unshareu(x)
    when tlist then
        var_putix_list(a,index,x)
    when tarray,tuserarray then
        var_putix_array(a,index,x)
    when tbits then
        var_putix_bits(a,index,x)
    when tset then
        var_putix_set(a,index,x)
    when trecord then
        var_putix_record(a,index,x)
    else
        pcustype_t("putix", a.tag)
    endswitch
end

global proc var_getixref(variant a, int index)=
    switch a.tag
    when tstring then
        var_getixref_string(a,index)
    when tlist then
        var_getixref_list(a,index)
    when tarray,tuserarray then
        var_getixref_array(a,index)
    when tbits then
        var_getixref_bits(a,index)
    when tset then
        var_getixref_set(a,index)
    when trecord then
        var_getixref_record(a,index,a)
    else
        pcustype_t("getixref", a.tag)
    endswitch
end

global proc var_getslice(variant a, int i,j)=
    switch a.tag
    when tstring then
        var_getslice_string(a,i,j)
    when tlist then
        var_getslice_list(a,i,j)
    when tarray,tuserarray then
        var_getslice_array(a,i,j)
    when tbits then
        var_getslice_bits(a,i,j)
    else
        pcustype_t("getslice", a.tag)
    endswitch
end

global proc var_putslice(variant a, int i,j, variant x)=
    if a.tag<>x.tag then
        pcerror("putslice: not compatible")
    fi

    switch a.tag
    when tstring then
        var_putslice_string(a,i,j,x)
    when tlist then
        var_putslice_list(a,i,j,x)
    when tarray,tuserarray then
        var_putslice_array(a,i,j,x)
    when tbits then
        var_putslice_bits(a,i,j,x)
    else
        pcustype_t("putslice", a.tag)
    endswitch
end

global proc var_getdotix(variant a, int index)=
    switch a.tag
    when tint, tword then
        if index not in 0..63 then
            pcerror("int.[int] bounds")
        fi
        a.value:=(a.value>>index) iand 1
    when tstring then
        var_getdotix_string(a,index)
    when trecord then
        var_getix_record(a,index)
    else
        pcustype_t("getdotix", a.tag)
    endswitch
end

global proc var_putdotix(variant p, int index, variant x)=
    variant a

    if p.tag=trefvar then
        a:=p.varptr

        switch a.tag
        when tint, tword then
            if index not in 0..63 then
                pcerror("int.[int]:= bounds")
            fi
            var_storebit(cast(&a.value),index,x,tu1,1)

        when tstring then
            var_putdotix_string(a,index,x)
        when trecord then
            var_putix_record(a,index,x)
        else
            pcustype("putdotix", a)
        endswitch
    else
        pcustype("putdotix",p)
    fi
end

global proc var_getdotixref(variant p, int index)=
    variant a

    if p.tag=trefvar then
        a:=p.varptr

        switch a.tag
        when tint, tword then
            if index not in 0..63 then
                pcerror("&int.[int] bounds")
            fi
            p.ptr:=cast(&a.value)
            p.tagx:=trefbit
            p.elemtag:=tu1
            p.bitoffset:=index
            p.bitlength:=1
        when tstring then
            var_getdotixref_string(a,index,p)
        when trecord then
            var_getixref_record(a,index,p)
        else
            pcustype_t("getdotixref", a.tag)
        endswitch
    else
        pcustype("not refvar",p)
    fi
end

global proc var_getdotslice(variant a, int i,j)=
    switch a.tag
    when tint, tword then

        if i>j then swap(i,j) fi
        if i<0 or j>63 then pcerror("int.[slice] bounds") fi
        a.value:=(a.value>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

    when tstring then
        var_getslice_string(a,i,j)
    else
        pcustype_t("getdotslice", a.tag)
    endswitch
end

global proc var_putdotslice(variant p, int i,j, variant x)=
    variant a

    if p.tag=trefvar then
        a:=p.varptr

        switch a.tag
        when tint,tword then
            if i>j then swap(i,j) fi
            if i<0 or j>63 then pcerror("int.[slice]:= bounds") fi
            var_storebit(cast(&a.value), i,x,tu1,j-i+1)
        when tstring then
            var_putslice_string(a,i,j,x)
        else
            pcustype("putdotslice", a)
        endswitch
    else
        pcustype("not ref",p)
    fi
end

global proc var_getdotsliceref(variant p, int i,j)=
    variant a

    if p.tag=trefvar then
        a:=p.varptr

        switch a.tag
        when tint, tword then
            if i>j then swap(i,j) fi
            if i<0 or j>63 then
                pcerror("&int.[slice] bounds")
            fi
            p.ptr:=cast(&a.value)
            p.tagx:=trefbit
            p.elemtag:=tu1
            p.bitoffset:=i
            p.bitlength:=j-i+1
        else
            pcustype("getdotsliceref", a)
        endswitch
    else
        pcustype("not ref",p)
    fi
end

global proc var_expand(variant a, dest, int m)=
    variant b,c
    object p
    ref char s
    int n


    if m<2 then pcerror("Expand: LHS too few") fi

    switch a.tag
    when tlist then
        p:=a.objptr
        b:=dest
        c:=p.varptr
        n:=1

        to m do
            if n>p.length then
                dest.tagx:=tvoid
            else
                dest^:=c^
                var_share(dest)
                ++c
            fi
            ++n
            --dest
        od

    when trange then            
        dest.tagx:=tint
        dest.value:=a.range_lower
        --dest
        dest.tagx:=tint
        dest.value:=a.range_upper
        to m-2 do
            --dest
            dest.tagx:=tvoid
        od

    when tstring then
        var_expand_string(a, dest, m)

    else
        pcustype("expand",a)
    endswitch
end

global function var_minto(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    if newtag:=a.tag<>b.tag then
        return 0
    fi

    switch a.tag
    when tint then
        a.value min:=b.value
    when tdecimal then
        if var_compare_dec(a,b)>0 then
            var_shareu(b)
            var_unshareu(a)
            a^:=b^
        fi

    when tstring then
        if var_compare_string(a,b)>0 then       
            var_shareu(b)
            var_unshareu(a)
            a^:=b^
        fi

    else
        return 0
    endswitch
    return 1
end

global function var_maxto(variant p,b)int=
    variant a:=p.varptr
    int newtag

    if p.tag<>trefvar then
        return 0
    fi
    if newtag:=a.tag<>b.tag then
        return 0
    fi

    switch a.tag
    when tint then
        a.value max:=b.value
    when tdecimal then
        if var_compare_dec(a,b)<0 then
            var_shareu(b)
            var_unshareu(a)
            a^:=b^
        fi
    when tstring then
        if var_compare_string(a,b)<0 then       
            var_shareu(b)
            var_unshareu(a)
            a^:=b^
        fi

    else
        return 0
    endswitch
    return 1
end

global proc var_inplace(variant px,y, ref proc(variant,variant) fnadd, fnaddmixed)=
    varrec x
    varrec Z

    var_loadptr(px,&x)
Z:=X

    if x.tag=y.tag then
        fnadd^(&x,y)
    else
        fnaddmixed^(&x,y)
    fi

VAR_UNSHARE(&Z)
    var_storeptr(px,&x)
end

global proc var_inplace_unary(variant px, ref proc(variant) fnneg)=
    varrec x


    var_loadptr(px,&x)
    fnneg^(&x)
    var_storeptr(px,&x)
end

global proc var_loadptr(variant x,y)=

    switch x.tag
    when trefvar then
        y^:=(x.varptr)^
        if y.hasref then
            ++y.objptr.refcount
        fi

    when trefpack then
        var_loadpacked(x.ptr,x.elemtag,y,nil)

    when trefbit then
        var_loadbit(x.ptr, x.bitoffset, x.elemtag, x.bitlength, y)

    else
        pcustype("var_loadptr",x)
    endswitch
end

global proc var_storeptr(variant p,q)=
    variant dest
    variant pptr,qptr
    varrec v
    int i,n,etag
    int poffset,qoffset,bitwidthx
    ref byte pp,qq
    int aa,bb

    switch p.tag
    when trefvar then
        dest:=p.varptr
        var_unshare(dest)
        dest^:=q^

    when trefpack then
        var_storepacked(ref byte(p.ptr),q,p.elemtag)

    when trefbit then
        var_storebit(p.ptr,p.bitoffset,q,p.elemtag,p.bitlength)

    else
        pcustype("var_popptr",p)
    endswitch
end

global proc var_loadbit(ref byte p,int shift,t,bitlength,variant dest) =
    ref word pd
    word mask

    dest.tagx:=tint
    switch (t)
    when tu1 then
        if bitlength=0 then
            dest.value:=not not (p^ iand (1<<shift))
        else
            pd:=cast(p)
            mask:=0xFFFF'FFFF'FFFF'FFFE
            case bitlength
            when 1 then
            when 64 then
                mask:=0
            else
                mask<<:=bitlength-1
            esac
            dest.value:=(pd^>>shift) iand (inot mask)
        fi

    when tu2 then
        dest.value:=(p^ iand (3<<shift))>>shift
    when tu4 then
        dest.value:=(p^ iand (15<<shift))>>shift
    else
        pcustype_t("loadbit",t)
    endswitch

end

global proc var_storebit(ref byte p,int shift,variant q,int t,bitlength) =
    ref word pd
    byte bb
    word mask1,mask2,newvalue

    if q.tag not in [tint,tword] then
        pcerror("storebit not int")
    fi

    switch (t)
    when tu1 then
        if bitlength=0 then
            p^:=(p^ iand inot(1<<shift)) ior ((q.value iand 1)<<shift)
        else
            pd:=cast(p)
            mask1:=0xFFFF'FFFF'FFFF'FFFE
            case bitlength
            when 1 then
            when 64 then
                mask1:=0
            else
                mask1<<:=bitlength-1
            esac

            mask1 :=inot mask1


            if shift then
                mask1<<:=shift
            fi

            mask2:=inot mask1
            newvalue:=q.value
            if shift then
                newvalue<<:=shift
            fi
            pd^:=(pd^ iand mask2) ior (newvalue iand mask1)
        fi

    when tu2 then
        p^:=(p^ iand inot(3<<shift)) ior ((q.value iand 3)<<shift)
    when tu4 then
        p^:=(p^ iand inot(15<<shift)) ior ((q.value iand 15)<<shift)
    else
        pcustype_t("storebit",t)
    endswitch
end

global proc var_convert(variant x, int t, variant dest)=
    int s,tbase
    i64 aa
    varrec bn


    dest^:=x^

    s:=x.tag
    if s=t then     
        return                          
    fi
    tbase:=t

    dest.tag:=t         

    switch s
    when tint then
        switch tbase
        when tint then          
        when treal then
            dest.xvalue:=x.value
        when tword then
        when tdecimal then
            var_make_dec_int(sptr.value,dest)
        elsif ttbasetype[t]=tenumdef then
            dest.tag:=tenum
            dest.elemtag:=t

        else
            pcustype_t("conv int=>",t)
        endswitch

    when tword then
        switch tbase
        when tint then
        when tword then
        when treal then
        else
            pcustype_t("conv dint=>",t);
        endswitch

    when treal then
        switch tbase
        when tint then
            dest.value:=x.xvalue
        else
            pcustype_t("conv real=>",t)
        endswitch

    when trefpack,trefvar,trefbit then
        switch tbase
        when tint,tword then
        else
            pcustype_t("conv ptr=>",t)
        endswitch
    when tstring then
        switch tbase
        when tlist then
            var_convert_string_list(x,t,dest)

        when tdecimal then
            var_make_dec_str(x.objptr.strptr, x.objptr.length, dest)
        when tstring then
        else
            pcustype_t("string=>",t)
        endswitch

    when ttype then
        if tbase<>tint then
            pcustype_t("type=>",t)
        fi

    when tdecimal then
        switch (tbase)
        when tint then
            aa:=var_convert_dec_int(x)
            dest.tagx:=tint
            dest.value:=aa

        else
            pcustype_t("decimal=>",t)
        endswitch

    when tenum then
        switch tbase
        when tint then          
            dest.tagx:=tint
        when tenum then
            dest.elemtag:=x.elemtag
        else
            pcustype_t("conv enum=>",t)
        endswitch

    else
        pcmxtypestt("Convert s.t",s,t)
    endswitch

end

global function var_gethashvalue(variant p)int=
    int hsum,csum,c,n,i,result
    ref char s,s0
    object q

    switch p^.tag
    when tstring then
        n:=p.objptr.length
        if not n then return 0 fi
        hsum:=0
        s:=p.objptr.strptr
        to n do
            c:=s++^
            hsum:=(hsum<<4-hsum) +c
        od
        result:=hsum<<5-hsum

        return result iand 0x7FFF'FFFF'FFFF'FFFF        !keep positive

    when tint,tword,treal,trange then
        return p^.value
    when tdecimal then
        q:=p.objptr
        if q.length=0 then
            return 0
        else
            return q.num[0]
        fi
    else
        pcustype("Can't hash:",p)
    endswitch
    return 0
end

global proc var_objtovar(int tag, object p, variant q)=
    q.tagx:=tag ior hasrefmask
    q.objptr:=p
end

global proc var_putdotix_intint(variant a, int index, variant b)=
    word x:=a.value
    word y:=b.value

    if index not in 0..63 then
        pcerror("int.[int]:= bounds")
    fi

    a.value:=x iand inot (1<<index) ior y<<index
end

global proc var_power(variant a, b)=
    if a.tag<>b.tag then
        var_powermixed(a,b)
        return
    fi

    switch a.tag
    when tint then
        a.value:=a.value**b.value
    when treal then
        a.xvalue:=pow(a.xvalue,b.xvalue)
    when tdecimal then
        var_power_dec(a,var_convert_dec_int(b))
    else
        pcustype_t("power", a.tag)
    endswitch
end

global proc var_powermixed(variant a, b)=
    int newtag:=a.tag
    case pr(a.tag,      b.tag)
    when pr(tint,       treal)    then
        newtag:=treal
        a.xvalue:=pow(a.value,b.xvalue)
        
    when pr(treal,      tint)     then
        a.xvalue:=pow(a.xvalue,b.value)

    when pr(tdecimal, tint) then
        var_power_dec(a,b.value)

    else
        pcmxtypes("Powermixed",a,b)
    esac

    a.tag:=newtag
end

=== qq_mtypes.m 0 0 31/43 ===

global enumdata []ichar catnames, []byte catmodes =
    (u64cat,        $,  tu64),          
    (i64cat,        $,  ti64),
    (r32cat,        $,  tr32),
    (r64cat,        $,  tr64),
    (refcat,        $,  0),
    (shortcat,      $,  0),
    (blockcat,      $,  0),
    (voidcat,       $,  0),
end

global const numcat = r64cat        

global function gettypecat(int m)int =
    case ttbasetype[m]
    when ti64 then return i64cat
    when tu64 then return u64cat
    when tr64 then return r64cat
    when tr32 then return r32cat
    when tpackrecord, tpackarray then
        case ttsize[m]
        when 1,2,4,8 then return i64cat
        else return blockcat
        esac
    when ti8, ti16, ti32, tu8, tu16, tu32 then return shortcat
    when trefpack, tstringz then return refcat
    else
        return voidcat
    esac
end

=== sysp.q 0 1 32/43 ===


export type rkey=struct 
    word16  charcode
    byte    keycode
    byte    shift
end

export var ncmdparams
export var cmdparams
export var stclock=0

export const tab="\t"

export var readfilesize

export var infinity=$infinity()
export var nan=$nan()


proc start=


    ncmdparams:=getcmdparam()

    cmdparams:=new(list,1..ncmdparams)

    for i:=1 to ncmdparams do
        cmdparams[i]:=getcmdparam(i)
    od


end

proc main=
    start()

    a:=(10,20,30,40)
    b:=(3,5,7,9)
    cpl =a

    cpl mapv((sqr),a)

end

export proc reporterror(m)=

    println "Error:",m
end

export func splitstring(s,sep)=

    a::=()
    ns:=0
    if s="" or sep="" then return (s,) fi
    do
        n:=sep in s
        if n=0 then
            a[++ns]:=s
            return a
        fi
        t:=leftstr(s,n-1)
        a[++ns]:=t
        s:=rightstr(s,-(n+sep.len-1))
    od
    return ""
end

export func joinstrings(a,sep)=
    if a.upb=0 then return "" fi
    s:=a[1]
    for i:=2 to a.upb do
        s:=s+sep+a[i]
    od
    return s
end

export proc abort(s)=

    println "Abort:",s,"Error"
    waitkey()
    stop 1
end

export func extractpath(fs)=
    l:=fs.len
    for i:=l downto 1 do
        if chr(fs.[i]) in "\\/:" then
            return leftstr(fs,i)
        fi
    od
    return ""
end

export func extractfile(fs)=
    p:=extractpath(fs)
    if p="" then return fs fi
    return rightstr(fs,-p.len)
end

export func extractbasefile(fs)=
    f:=extractfile(fs)
    if f="" then return "" fi
    e:=extractext(f)
    if e.len then
        f:=leftstr(f,-e.len)
    fi
    if rightstr(f)="." then
        f:=leftstr(f,-1)
    fi
    return f
end

export func extractext(fs,period=0)=

    f:=extractfile(fs)
    if f="" then return "" fi
    e:=""
    do
        n:="." in f
        if n then
            e:=rightstr(f,-n)
            if e="" then        
                return (period.defined and period|"."|"")
            fi

            f:=e
        else
            exit
        fi
    od

    return e
end

export func changeext(file,newext,soft=0)=
    ext:=extractext(file)

    p:=extractpath(file)
    bf:=extractbasefile(file)
    ep:=extractext(file,1)

    if soft and ep<>"" then return file fi      

    if newext="" then
        return p+bf
    elsif leftstr(newext)="." then
        return p+bf+newext
    else
        return p+bf+"."+newext
    fi
end

export func addpath(path,file)=
    if leftstr(file) in "/\\." or file.len>=2 and file.[2]=":" then
        return file
    fi
    return path+file
end

export func addext(file,ext)=

    if extractext(file,1)="" then
        return changeext(file,ext)
    fi
    return file
end

export func replacestr (s,a,b)=
    do
        n:=a in s
        if not n then return s fi
        s:=leftstr(s,n-1)+b+rightstr(s,1-n-a.len)
    od
    return ""
end

export func parsecmdparams(cmd)=

const dash="-"

    if cmd.islist then
        blocks:=cmd
    else
        sreadln(cmd)
        blocks::=()
        do
            read a:"s"
            if a="" then exit fi
            blocks append:=a
        od
    fi

    params::=()
    switches::=()

    forall x in blocks do
        n:=dash in x
        if n=1 then     
            switches concat:=splitstring(convlc(rightstr(x,-1)),"/")
        else            
            params append:=x
        fi
    od

    return (params,switches)
end

export proc waitsec(secs)=
    sleep(int(secs*1000))
end

export func cmd_getswitches=

    switches::=()
    for i:=1 to cmdparams.upb do        
        s:=cmdparams[i]
        if leftstr(s) in "-/" then
            switches append:=convlc(rightstr(s,-1))
        fi
    od
    return switches
end

export func cmd_getparams=

    cmds::=()

    for i:=1 to cmdparams.upb do
        pm:=cmdparams[i]
        if leftstr(pm) in "/-" then
            next
        fi
            cmds append:=pm
    od
    return cmds
end

export func starttimer=
    return stclock:=ticks()
end

export func stoptimer=

    d:=ticks()-stclock
    stclock:=ticks()
    return d
end

export func bnfact(n)=

    if n<=2 then
        return longint(n)
    fi

    f:=1L
    g:=2L
    to n-1 do
        f:=f*g
        g:=g+1L

    od
    return f
end

export proc isort(a,?ll,?rr)=
    if ll.isvoid then
        ll:=a.lwb
        rr:=a.upb
    fi

    i:=ll
    j:=rr

    pivot:=a[(ll+rr)%2]

    repeat
        while pivot>a[i] and i<rr do ++i od
        while pivot<a[j] and j>ll do --j od
        if i<=j then
            swap(a[i],a[j])
            ++i
            --j
        fi
    until i>j
    if ll<j then isort(a,ll,j) fi
    if i<rr then isort(a,i,rr) fi
end

export func sort(a)=
    b::=a
    isort(b)
    return b
end

export func pcerror(m)=

    println "Internal error:",m
    a:=b+c
    return 0
end



export proc insert(&a, b, c)=
    n:=a.upb
    a[n+1]:=c
    for i:=n downto b do
        swap(a[i+1],a[i])
    od
end

export proc isort2(a,b,?ll,?rr)=
    if ll.isvoid then
        ll:=a.lwb
        rr:=a.upb
    fi

    i:=ll
    j:=rr

    pivot:=a[(ll+rr)%2]

    repeat
        while pivot>a[i] and i<rr do ++i od
        while pivot<a[j] and j>ll do --j od
        if i<=j then
            swap(a[i],a[j])
            swap(b[i],b[j])
            ++i
            --j
        fi
    until i>j
    if ll<j then isort2(a,b,ll,j) fi
    if i<rr then isort2(a,b,i,rr) fi
end

export func left(a,n=1)=

    if n>=0 then
        return take(a,n)
    else
        return take(a,a.len+n)
    fi
end

export func right(a,n=1)=

    if n>=0 then
        return drop(a,a.len-n)
    else
        return drop(a,-n)
    fi
end

export func reverse(a)=

    if a.len=0 then
        return makeempty(a)
    fi
    b::=a

    if a then
        for i in a.bounds do
            b[a.upb-i+a.lwb]:=a[i]
        od
    fi
    return b
end

export func expandrange(a,step=1)=
    x::=()
    i:=a.lwb
    while i<=a.upb do
        x append:=i
        i+:=step
    od
    return x
end

export func head(a)=

    if a.len then
        return a[a.lwb]
    else
        return makeempty(a)
    fi
end

export func tail(a)=

    case a.len
    when 0,1 then
        return makeempty(a)
    esac
    return a[2..$]
end

export func init(a)=
    case a.len
    when 0,1 then
        return makeempty(a)
    esac
    return a[a.lwb..$-1]
end

export func last(a)=
    if a.len then
        return a[$]
    else
        return makeempty(a)
    fi
end

export func take(a,n)=

    if a.len=0 or n<=0 then
        return makeempty(a)
    fi
    if n>=a.len then
        return a
    fi
    return a[a.lwb..a.lwb+n-1]
end

export func drop(a,n)=

    if a.len=0 or n>=a.len then
        return makeempty(a)
    fi
    if n<=0 then
        return a
    fi
    return a[a.lwb+n..$]
end

export func zip(a,b)=

    n:=min(a.len,b.len)
    c::=()

    (j, k) := (a.lwb, b.lwb)

    to n do
        c append:=a[j++]
        c append:=b[k++]
    od
    return c
end

export func repeatlist(a,n)=

    b:=makeempty(a)
    to n do
        b concat:=a
    od
    return b
end

export func minimum(a)=
    if not a then
        return void
    fi
    x:=head(a)
    forall y in tail(a) do
        x min:=y
    od
    return x
end

export func maximum(a)=
    if not a then
        return void
    fi
    x:=head(a)
    forall y in tail(a) do
        x max:=y
    od
    return x
end

export func sumlist(a)=

    if not a then
        return void
    fi
    x:=head(a)
    forall y in tail(a) do
        x +:=y
    od
    return x
end

export proc delete(&a,?b)=
    n:=a.upb
    if b.isvoid then b:=n fi

    if n=b=1 then
        a::=()
        return
    fi

    if b>n then return fi
    if b<a.lwb then return fi
    for i:=b to n-1 do
        swap(a[i],a[i+1])           
    od

    resize(a,n-1)
end

export proc resize(&a,n)=

    if n<a.lwb then
        a:=makeempty(a)
        return
    fi

    a::=a[a.lwb..n]         
end
 
export func makebits(data,t=bit)=

    a:=new(bits,t,data.bounds)
    for i:=data.lwb to data.upb do
        a[i]:=data[i]
    od
    return a
end

export func makearray(data,t=int64)=

    a:=new(array,t,data.bounds)
    for i:=data.lwb to data.upb do
        a[i]:=data[i]
    od
    return a
end

export func tolist(a)=

    case a.basetype
    when array,string,bits then
        b:=new(list,a.bounds)
        forall i,x in a do
            b[i]:=x
        od
        return b

    when list then
        return a
    else
        pcerror("tolist:"+tostr(a.type))
    esac
    return 0
end

export func toarray(a,?t)=
    case a.basetype
    when list then
        if t.isvoid then
            if a then
                t:=a[a.lwb].type
            else
                t:=int32
            fi
        fi

    when bits then
        if t.isvoid then
            t:=byte
        fi

    when string then
        if t.isvoid then t:=byte fi
        b:=new(array,t,a.len)
        foreach i,x in a do
            b[i]:=x
        od
        return b
    when array then
        if t.isvoid then
            return a
        fi
        u:=e.elemtype
        if t=u then return a fi
    else
        pcerror("toarray:"+tostr(a.type))
    esac
    b:=new(array,t,a.bounds)

    forall i,x in a do
        b[i]:=x
    od
    return b
end

export func tobits(a,t=bit)=

    case a.basetype
    when list,array then

    when bits then
        if a.elemtype=t then
            return a
        fi

    else
        pcerror("tobits:"+tostr(a.type))
    esac
    b:=new(bits,t,a.bounds)
    forall i,x in a do
        b[i]:=x
    od
    return b
end

export func listtostring(a)=
    s:=""
    forall x in a do
        s+:=chr(x)
    od
    return s
end

export func qversion=
    return "4.0"
end

export proc issort(a,?ll,?rr)=

    if ll.isvoid then
        ll:=a.lwb
        rr:=a.upb
    fi

    i:=ll
    j:=rr

    pivot:=a.[(ll+rr)%2]

    repeat
        while pivot>a.[i] and i<rr do ++i od
        while pivot<a.[j] and j>ll do --j od
        if i<=j then
            swap(a.[i],a.[j])
            ++i
            --j
        fi
    until i>j
    if ll<j then issort(a,ll,j) fi
    if i<rr then issort(a,i,rr) fi
end

export func ssort(a)=

    b::=a
    issort(b)
    return b
end

export func maketable(rows, cols, initval=0)=

    row:=new(list,cols,initval)

    table::=new(list,rows)
    if rows.isint then rows:=1..rows fi

    for i in rows do
        table[i]::=row
    od

    return table
end

export func mapv(op,a)=
    b::=makeempty(a)
    for i,x in a do
            b[i]:=mapss(op,x)
    od
    return b
end

export func mapvv(op,a,b)=
    c::=makeempty(a)
    forall i,x in a do
        c[i]:=mapss(op,x,b[i])
    od
    return c
end

export func mapvs(op,a,bs)=
    c::=makeempty(a)
    forall i,x in a do
        c[i]:=mapss(op,x,bs)
    od
    return c
end

export func mapsv(op,as,b)=
    c::=()
    forall i,x in b do
        c[i]:=mapss(op,as,x)
    od
    return c
end

export func openfile(name,option="rb")=
    if not name.isstring or name="" then
        return 0
    fi
    return fopen(name,option)
end

export func createfile(name,options="wb")=
    if not name.isstring or name="" then return 0 fi
    return fopen(name,options)
end

export func closefile(f)=
    return fclose(f)=0
end

export func checkfile(name)=
    file:=fopen(name,"rb")
    if file=0 then return 0 fi
    fclose(file)
    return 1
end

export func eof(f)=
    c:=fgetc(f)
    if c=-1 then return 1 fi

    ungetc(c,f)
    return 0
end

export func getfilesize(f)=
    p:=ftell(f)         
    fseek(f,0,2)        
    size:=ftell(f)      
    fseek(f,p,0)        
    return size
end

export func setfilepos(f,offset)=
    return fseek(f,offset,0)
end

export func getfilepos(f)=
    return ftell(f)
end

export func readrandom(f,mem,offset,size)=
    fseek(f,offset,0)
    return fread(mem,1,size,f)
end

export func writerandom(f,mem,offset,size)=
    fseek(f,offset,0)
    return fwrite(mem,1,size,f)
end

export func readbytes(f,mem,size)=
    return fread(mem,1,size,f)
end

export func writebytes(f,mem,size)=
    return fwrite(mem,1,size,f)
end

export func inbyte(file)=       
    return fgetc(file)
end

export func inword(file)=       
    bb:=fgetc(file)
    return fgetc(file)<<8+bb
end

export func inlong(file)=       
    ww:=inword(file)
    return inword(file)<<16+ww
end

export proc outbyte(file,x)=        
    fputc(x,file)
end

export proc outword(file,x)=        
    outbyte(file,x iand 255)
    outbyte(file,x.[15..8])
end

export proc outlong(file,x)=        
    outword(file,x iand 65535)
    outword(file,x>>16)
end


export func appendfile(a,b)=

f:=openfile(a)
if f=0 then return 0 fi

h:=openfile(b,"ab")
if h=0 then return 0 fi

while not eof(f) do
    readln @f,x:"l"
    println @h,x
od

closefile(f)
closefile(h)
return 1
end

export func readblockfile(filename,doetx=0)=

f:=openfile(filename)
if f=0 then return nil fi

n:=getfilesize(f)
readfilesize:=n

s:=malloc(n+doetx)
if s=0 then abort("Readfile/Malloc fails") fi
sptr:=makeref(s,byte)

readrandom(f,s,0,n)

if doetx then
    (sptr+n)^:=26
fi

closefile(f)
return sptr
end

export func readstrfile(filename,doetx=0)=

f:=openfile(filename)
if f=0 then return 0 fi

n:=getfilesize(f)
readfilesize:=n

ptr:=malloc(n+1+doetx)
if ptr=0 then abort("Readfile/Malloc fails") fi

readrandom(f,ptr,0,n)
if doetx then
    (makeref(ptr,byte)+n)^:=26
fi

closefile(f)

s::=makestr(ptr,n+doetx)

free(ptr)
return s
end

export func writestrfile(filename,s)=

f:=createfile(filename)
if f=0 then return 0 fi

writerandom(f,makeref(s,byte),0,s.len)

return closefile(f)
end

export func readbinfile(filename)=

f:=openfile(filename)
if f=0 then return 0 fi

n:=getfilesize(f)
readfilesize:=n

a:=new(array,byte,n)
readrandom(f,&a,0,n)

closefile(f)
return a
end

export func writebinfile(filename,a)=

f:=createfile(filename)
if f=0 then return 0 fi

writerandom(f,(&a),0,a.len)

closefile(f)
return 1
end

export func writeblockfile(filename,p,length)=

f:=createfile(filename)
if f=0 then return 0 fi

if not writerandom(f,p,0,length) then return 0 fi

closefile(f)
return 1
end

export func erasefile(filename)=
return remove(filename)
end

export func renamefile(oldfilename,newfilename)=
return rename(oldfilename,newfilename)
end

export func readtextfile(file)=
f:=openfile(file)
if not f then
    return 0 
fi

readfilesize:=getfilesize(f)
a::=()

while not eof(f) do
    a append:= sreadln(f)
od
closefile(f)
return a
end

export func writetextfile(file,a)=
f:=createfile(file)
if not f then return 0 fi

for i:=a.lwb to a.upb do
    println @f,a[i]
od
closefile(f)
return 1
end

export func readbinaryfile(filename,t)=

    f:=openfile(filename)
    if f=0 then return () fi

    n:=getfilesize(f)
    readfilesize:=n
    elems:=n%t.bytes

    a:=new(array,t,elems)
    readrandom(f,&a,0,n)

    closefile(f)
    return a
end

export func writebinaryfile(filename,data)=
    return writeblockfile(filename,&data,data.bytes)
end

export func confirm(m,caption="Confirm",default=1)=

    flags:=0x20000+0x20 
    flags ior:=3        

    flags ior:=(default|0,0x100,0x200|0)

    status:=messagebox(nil,m,caption,flags)
    return status=6
end

export func messagebox(a=nil,mess,caption="Caption",d=0)=
    return messageboxa(nil,mess,caption,d)
end

export func dirlist(s,t=1)=


    if t.isvoid then t:=1 fi            

    nfiles:=0
    data::=()
    file:=new(ws_finddata)

    if (hfind:=findfirstfile(s,&file))<>-1 then 
        repeat
            if (file.fileattributes iand 16) then       
                if (t iand 2)=0 then goto skip fi       
            else                        
                if (t iand 1)=0 then goto skip fi
            fi
            ++nfiles
            if (t iand 4) then              
                data[nfiles]:=convlc(file.filename)
            else
                data[nfiles]::=file.filename
            fi
    skip:
        until not findnextfile(hfind,&file)
        findclose(hfind)
    fi
    return data
end

export func setcurrdir(newdir)=
    return setcurrentdirectory(newdir)
end

export func getcurrdir=
    a:=new(array,byte,256)
    n:=getcurrentdirectory(a.len,&a[1])

    if n then
        dir::=makestr(&a[1],n)
    else
        dir:=""
    fi

    if not (rightstr(dir) in "\\/") then dir +:= "\\" fi
    return dir
end

export func createdir(name)=
    return createdirectory(name,0)
end

export func direxists(path)=
    const file_attribute_directory=16
    const invalid_file_attributes=-1

    attrib := getfileattributesa(path)

    return attrib<>invalid_file_attributes and (attrib iand file_attribute_directory)
end

export proc beep1=
    messagebeep(0)
end
=== clibp.q 0 1 33/43 ===
importdll msvcrt=
    clang func      "malloc"        (int64)ref byte
    clang func      realloc     (int64, int32)int64
    clang procedure free        (int64)
    clang procedure memset      (ref byte, int32, int32)
    clang procedure memcpy      (ref byte, ref byte, int32)
    clang func      ftell       (int64)int32
    clang func      fseek       (int64, int32, int32)int32
    clang func      fread       (ref byte, int32, int32, int64)int32
    clang func      fwrite      (ref byte, int32, int32, int64)int32
    clang func      getc        (int64)int32
    clang func      ungetc      (int32, int64)int32
    clang func      fopen       (stringz, stringz)int64
    clang func      fclose      (int64)int32
    clang func      fgets       (ref byte, int32, int64)ref byte
    clang func      remove      (stringz)int32
    clang func      rename      (stringz, stringz)int32
    clang func      getchar     :int32
    clang procedure putchar     (int32)
    clang procedure setbuf      (int64, int64)

    clang func      rand        :int32
    clang procedure srand       (int32)

    clang func      puts        (stringz)int32
    clang func      printf      (stringz, ...)int32

    clang func      sprintf     (stringz, stringz, ...)int32

    clang func      sscanf      (stringz, stringz, ...)int32
    clang func      isalpha     (int32)int32
    clang func      tolower     (int32)int32
    clang func      strlen      (ref byte)int32
    clang func      atoi        (stringz)int32


    clang func      fgetc       (int64)int32
    clang func      fputc       (int32,  int64)int32
    clang func      fprintf     (int64, stringz, ...)int32
    clang func      fputs       (stringz,  int64)int32
    clang func      feof        (int64)int32
    clang func      _getch      :int32

end

global const c_eof     = -1
global const seek_set  = 0
global const seek_curr = 1
global const seek_end  = 2

=== smlib.q 0 1 34/43 ===

export var popuplist::=()
export var focuslist::=()
export var npopups=0
export var message
export var messw
export var wpopup=nil

record blockrec=
    var posx, posy              
                                
    var dimx,dimy               
    var celldimx, celldimy      
    var cellsx, cellsy          
    var gapx, gapy              
    var marginx, marginy        
    var labelwidth              
    var cellposx,cellposy       
    var pitchx,pitchy           
    var blockstyle              
    var name                    
    var dir                     

end

var blocklist::=()
var nblocks=0
var currblock =nil              
var currgroup =nil              
var currpopup =nil              
var cellx, celly                
var slposx,slposy,sldir         

proc start=
end

proc toggletest=

gxloadfont(1,"Arial","I",20)
gxloadfont(2,"COURIER","B",20)

a:=smdefblock("X"*26,6,"CNR")

a:=smdefblock("X"*26,5,"",labeldim:"*"*12)
b:=smdefblock("X"*26,5,"",labeldim:"*"*12)
c:=smdefblock("X"*26,5,"",labeldim:"*"*12)
d:=smdefblock("X"*10,2)

smorder((a,b,c,d))
smcreate("TOGGLES",smmenusize())

wa:=smblock(a)
d1:="One"
d2:="Two"
d3:="Three"
d4:="Four"
d5:="five"
wd::=()

wd[1]:=smeditbox("Edit d1:",^d1,211,enable:1)
wd[2]:=smeditbox("Edit d2:",^d2,212,enable:1)



smblock(d)
wok:=smok(enable:0)
wcancel:=smcancel()

setfocus(wd[1])


do

    m:=gxaskmess(1)
CPL "MESSAGELOOP:",MESSAGENAMES[CURRMESS.MESSAGE],M,currmess.a,=TOGGLE,=SELECT,=abc

    case m
    when mm_pick then
CPL "PICK"
        exit
    when mm_ok then
        exit
    when mm_cancel then
        exit
    when mm_tabkey then
CPL "TABKEY"
    esac
od


smoff()



end

proc main=
CPL "SMLIB MAIN"

toggletest()
stop

sminit()

A:=smdefblock(
    dim:"X"*28,
    cells:(1,10),
    gap:0,
    margin:10,
)
B:=smdefblock(
    dim:"X"*10,
    cells:(1,3)
)

smorder((a,b))
wpopup:=smcreate("TEST MENU",smmenusize())

wa:=smblock(a)
xx:=()
for i to 130 do
    xx append:=tostr(i)
od

wlb:=smlistbox(^xx)
wlb.style.lbchange:=1
WLB.NAME:="FREDDY"

SMBLOCK(B)
SMOK()
SMCMD("HELLO",201)

NN:=0
do

    gxfocus(wlb)

    m:=gxaskmess(1)
CPL "MESSAGELOOP:",MESSAGENAMES[CURRMESS.MESSAGE],M,currmess.a

    case m
    when mm_pick then
CPL "PICK"
        retval:=currmess.a
        exit
    when mm_ok then
        retval:=gxcurrpos(wlb)
        exit
    when mm_cancel then
        retval:=0
        exit
    when mm_lbchange then               
CPL "LBCHANGE"
    when mm_tabkey then
CPL "TABKEY"
    esac
od


smoff()

end


export proc sminit=
blocklist::=()
nblocks:=0

end

export func smdefblock(?dim,cells=1,style="",gap=0,labeldim="",margin=0,dir='V')=

block:=new(blockrec,0)
block.dir:=dir

if dim.isstring then
    block.celldimx:=gxtextwidth(labelfont,dim)+smx*2
    block.celldimy:=chy+smy*2
else
    (block.celldimx,block.celldimy):=dim
fi

if cells.isint then
    block.cellsx:=1
    block.cellsy:=cells
else
    (block.cellsx,block.cellsy):=cells
fi

if gap.isint then
    block.gapx:=gap
    block.gapy:=gap
else
    (block.gapx,block.gapy):=gap
fi

if margin.isint then
    block.marginx:=margin
    block.marginy:=margin
else
    (block.marginx,block.marginy):=margin
fi

if labeldim then
    if labeldim.isstring then
        block.labelwidth:=gxtextwidth(labelfont,labeldim)+smx*2
    else
        block.labelwidth:=labeldim
    fi
    block.celldimx+:=block.labelwidth
fi

block.blockstyle:=readstylestr(style)



bdx:=bdy:=1             

block.pitchx:=block.celldimx+bdx*2+block.gapx       
block.pitchy:=block.celldimy+bdy*2+block.gapy       


block.cellposx:=block.marginx+bdx                   
block.cellposy:=block.marginy+bdy

block.dimx:=block.pitchx*block.cellsx-block.gapx+block.marginx*2
block.dimy:=block.pitchy*block.cellsy-block.gapy+block.marginy*2

blocklist[++nblocks]:=block

return block
end

export func smmenusize(margin=chy)=

forall i,block in blocklist do
    if i=1 then             
        x1:=block.posx
        y1:=block.posy
        x2:=x1+block.dimx-1
        y2:=y1+block.dimy-1
    else
        x1 min:=block.posx
        y1 min:=block.posy
        x2 max:=block.posx+block.dimx-1
        y2 max:=block.posy+block.dimy-1
    fi
od


forall block in blocklist do
    block.posx+:=margin-x1
    block.posy+:=margin-y1
od

return (x2-x1+margin*2+1,y2-y1+margin*2+1)
end

export proc smorder(blocks,dir='D')=

if dir.isstring then
    dir:=asc(convuc(dir))           
fi

bdx:=bdy:=1                     

dx:=chx+bdx
dy:=chy+bdy
firstblock:=1

forall block in blocks do
    if block.isint then         
        dx:=block+bdx*2
        dy:=block+bdy*2
        next
    fi
    if firstblock then
        lastblock:=block
        firstblock:=0
        next
    fi
    case dir
    when 'D' then           
        block.posx:=lastblock.posx
        block.posy:=lastblock.posy+lastblock.dimy+dy
    when 'R' then
        block.posx:=lastblock.posx+lastblock.dimx+dx
        block.posy:=lastblock.posy
    when 'U' then           
        block.posx:=lastblock.posx
        block.posy:=lastblock.posy-block.dimy-dy
    when 'L' then
        block.posx:=lastblock.posx-block.dimx-dx
        block.posy:=lastblock.posy
    esac
    lastblock:=block
od
end

proc showtestmenu(dim)=

wapplic:=gxcreatewindow(dim:dim,caption:"test")

forall block in blocklist do
    gxbutton(pos:(block.posx,block.posy),dim:(block.dimx,block.dimy),caption:block.name,
    owner:wapplic,style:[ss_border:bs_simplew])
od

eventloop()

end

export func smcreate(caption="",?dim,?pos)=


w:=gxcreatewindow(caption:caption, dim:dim, options:[wf_minmax:0],pos:pos)
w.windclass:=popup_class
currpopup:=w

slposx:=chx
slposy:=chy
sldir:=(dim[1]>dim[2]|'H'|'V')

wpopup:=w

oldfocus:=wfocus
if wfocus then
    gxkillfocus()
fi

popuplist[++npopups]:=w
focuslist[npopups]:=oldfocus        

return w
end

export func smblock(block,border=0)=

case border
when 0 then bord:=bs_none
when 1 then bord:=bs_simple
when 2 then bord:=bs_panel
else
    bord:=bs_none
esac

wblock:=gxgroup(pos:(block.posx,block.posy),dim:(block.dimx,block.dimy),
        owner:currpopup, style:[ss_border:bord])
currblock:=block
currgroup:=wblock
cellx:=celly:=1

return wblock
end

export proc smclose=

gxclose(wpopup)

oldfocus:=focuslist[npopups]
--npopups
if npopups then
    wpopup:=popuplist[npopups]
    if oldfocus then
        gxfocus(oldfocus)
    fi
else
    wpopup:=nil
fi
end

export proc smoff=
smclose()
end

proc nextcell=
if currblock.dir='V' then
    ++celly
    if celly>currblock.cellsy then
        celly:=1
        ++cellx
    fi
else                    
    ++cellx
    if cellx>currblock.cellsx then
        cellx:=1
        ++celly
    fi
fi
end

func getsmpos=
return ((cellx-1)*currblock.pitchx+currblock.cellposx,
        (celly-1)*currblock.pitchy+currblock.cellposy)
end

func getsmdim=
return (currblock.celldimx,currblock.celldimy)
end

func getslpos=
return (slposx,slposy)
end

func getsldim(s)=
if s.isint then
    return (s*chx+chx*2,chy+smy*2)
else
    return (gxtextwidth(labelfont,s)+smx*2,chy+smy*2)
fi
end

proc nextslcell(dim)=
if sldir='H' then
    slposx+:=dim[1]+chx
else
    slposy+:=dim[2]+chy
fi
end

export func smcmd(caption,id=0,enable=1)=


if caption.isint then
    case caption
    when 0 then
    when -1 then
    esac
    nextcell()
    return nil
fi

if id=0 then
    return smlabel(caption)
fi

pos:=getsmpos()
dim:=(currblock.celldimx,currblock.celldimy)
ss:=[ss_border:bs_simplew]

w:=gxbutton(pos:pos,dim:dim,caption:caption,id:id,owner:currgroup, 
    style:ss, enable:enable)
nextcell()
return w
end

export func smlabel(caption)=
pos:=getsmpos()
dim:=(currblock.celldimx,currblock.celldimy)

w:=gxlabel(pos:pos,dim:dim,caption:caption,owner:currgroup)
nextcell()
return w
end

export func smarrow(dir,id)=
pos:=getsmpos()
dim:=(currblock.celldimx,currblock.celldimy)

w:=gxarrow(pos:pos,dim:dim,dir:dir,owner:currgroup)
nextcell()
return w
end

export func smtoggle(caption,linkvar,id=0,enable=1)=

w:=gxtoggle(pos:getsmpos(), dim:getsmdim(), caption:caption,
            linkvar:linkvar,id:id,owner:currgroup, enable:enable,
            style:currblock.blockstyle)


nextcell()
return w
end

export func smselect(caption,linkvar,onvalue=1,id=0,enable=1)=

w:=gxselect(pos:getsmpos(), dim:getsmdim(), caption:caption,
            linkvar:linkvar,onvalue:onvalue,
            id:id,owner:currgroup, enable:enable, style:currblock.blockstyle)

nextcell()
return w
end

export func smeditbox(?caption,linkvar,id=0,enable=1,?style)=

pos:=getsmpos()
dim:=getsmdim()

if caption.isdef then
    gxlabel(pos:pos, dim:(currblock.labelwidth-chx,dim[2]), caption:caption,
     owner:currgroup)
    pos[1]+:=currblock.labelwidth
    dim[1]-:=currblock.labelwidth
fi

w:=gxeditbox(pos:pos, dim:dim,
            linkvar:linkvar,
            id:id,owner:currgroup, enable:enable, style:getstyle(style))

nextcell()
return w
end

export func smlistbox(linkvar,id=0,enable=1)=


CPL "SMLB1"
w:=gxlistbox(pos:getsmpos(),
    dim:(currblock.dimx-currblock.marginx*2,currblock.dimy-currblock.marginy*2),
    linkvar:linkvar,
    style:[ss_vscroll:1,
    ss_border:bs_simplew],
    rows:currblock.cellsy,
    pitch:currblock.pitchy,
    id:id,owner:currgroup)
CPL "SMLB2"
return w
end

export func sllabel(caption)=
pos:=getslpos()
dim:=getsldim(caption)

w:=gxlabel(pos:pos,dim:dim,caption:caption,owner:currpopup)
nextslcell(dim)
return w
end

export func slcmd(caption,id=201,enable=1)=

pos:=getslpos()
dim:=getsldim(caption)
ss:=[ss_border:bs_simplew]

w:=gxbutton(pos:pos,dim:dim,caption:caption,id:id,owner:currpopup, 
    style:ss)
nextslcell(dim)
return w
end

export func sleditbox(linkvar,width=30,id=0,enable=1)=
pos:=getslpos()
dim:=getsldim(width)

w:=gxeditbox(pos:pos, dim:dim,
            linkvar:linkvar,
            id:id,owner:currpopup)

nextslcell(dim)
return w
end

export func smok(caption="OK",enable=1)=
return smcmd(caption,mm_ok,enable)
end

export func smcancel(caption="Cancel",enable=1)=
return smcmd(caption,mm_cancel,enable)
end

export func slok(caption="OK")=
return slcmd(caption,mm_ok)
end

export func slcancel(caption="Cancel")=
return slcmd(caption,mm_cancel)
end

export proc smokcancel=
smok()
smcancel()
end

export proc slinit(w)=
currpopup:=w
slposx:=chx
slposy:=0
sldir:=(w.dimx>w.dimy|'H'|'V')
end

export func smerror(mess) =
pcerror("Unimplemented SM/CCI function: "+mess+"\n\n\n")
println
println
println
stop
return 0
end

export proc settab(?a,?b,?c,?d,?e,?f,?g,?h,?i)=
static var oldtabs

if not a.defined then
    gxtabstops(oldtabs)
    return
fi

oldtabs:=gxtabstops()
params::=allparams()

gxtabstops(param)
end

export proc smupdatevalue(w)=
gxupdate(w)
end

export proc setfocus(w,?b)= gxfocus(w) end

export proc askmenu(a)=
message:=gxaskmess(1)
messw:=currmess.wind
end

func readstylestr(s)=
d:=new(dict)
if s="" then return d fi

s:=convuc(s)

foreach c in s do
    case c
    when 'X' then d{ss_marktype}:=check_mark
    when 'M' then d{ss_marktype}:=radio_mark
    when 'I' then d{ss_marktype}:=invert_mark
    when 'R' then d{ss_returnmess}:=1
    when 'N' then d{ss_noupdate}:=1
    esac
od
return d
end

func getstyle(style)=
if style.defined then
    return readstylestr(style)
else
    return currblock.blockstyle
fi

end

export func smmark(a,b,c)= return smerror($function) end
export func smcheck(?a,?b,?c,?d,?e)= return smerror($function) end
export func smmenu(?a)= return smerror($function) end
export func smshowmenu(a,?b,?c,?d)= return smerror($function) end
export func smdown= return smerror($function) end
export func smup= return smerror($function) end
export func smleft= return smerror($function) end
export func smright= return smerror($function) end
export func smedit(a,b,c,?d)= return smerror($function) end
export func smbutton(a,b)= return smerror($function) end
export func smstartmenu(a)= return smerror($function) end
export func smsize(a)= return smerror($function) end


=== winapi.q 0 1 35/43 ===

export type wt_word     = word16
export type wt_bool     = word32
export type wt_dword    = word32
export type wt_wchar    = word16
export type wt_char     = byte
export type wt_ichar    = stringz
export type wt_string   = stringz
export type wt_ptr      = ref byte
export type wt_wndproc  = word64

export type wt_handle   = ref void
export type wt_int      = int32
export type wt_uint     = word32
export type wt_long     = int32
export type wt_wparam   = word64
export type wt_lparam   = word64
export type wt_size     = word64

export type wt_wparam32 = word32
export type wt_lparam32 = word32
export type wt_handle32 = word32
export type wt_ptr32    = word32
export type wt_string32 = word32
export type wt_wndproc32    = word32

export type wt_wparam64 = word64
export type wt_lparam64 = word64
export type wt_handle64 = word64
export type wt_ptr64    = word64
export type wt_string64 = word64
export type wt_wndproc64= word64

export type wt_result   = word64
export type wt_intptr   = word64
export type wt_coord    = word32

export type ws_spoint= struct
    int16 x,y
end

export type ws_srect=struct
    int16 leftx,top, rightx,bottom
end

export type ws_charinfo=struct
    union
        wt_word unicodechar
        wt_char asciichar
    end union
    wt_word     attributes
end

export type ws_palette16=[0..15]int32

export type ws_console=struct
    ws_spoint size,pos
    wt_word attributes
    ws_srect window
    ws_spoint maxwindowsize
end

export type ws_consoleex=struct
    int32 recsize
    ws_spoint size,pos
    wt_word attributes
    ws_srect window
    ws_spoint maxwindowsize
    wt_word wpopup
    int32 fullscreen
    ws_palette16 palette
end

export type ws_keyevent = struct $caligned
    wt_word eventtype
        wt_bool keydown
        wt_word repeatcount
        wt_word virtualkeycode
        wt_word virtualscancode
        union
            wt_word unicodechar
            wt_char asciichar
        end
        wt_dword controlkeystate
end

export type ws_cursor=struct(int32 size,visible)

export var hconsole, hconsolein

export const stdoutputhandle=0xffff_fff5
export const stdinputhandle=0xfffffff6
export const stderrorputhandle=0xfffffff4
export const invalidhandlevalue=0xffffffff

export const maxpathlen=260

type spath=stringz*maxpathlen
type sshort=stringz*14
export type ws_filetime=struct
    int32 ftlow
    int32 fthigh
end

export type ws_finddata=struct
    int32       fileattributes
    ws_filetime creationtime
    ws_filetime lastaccesstime
    ws_filetime lastwritetime
    int32       filesizehigh
    int32       filesizelow
    int32       reserved0
    int32       reserved1
    spath       filename
    sshort      shortfilename
end

export type ws_systemtime = struct
    word16  year
    word16  month
    word16  dayofweek
    word16  day
    word16  hour
    word16  minute
    word16  second
    word16  milliseconds
end

export type ws_msg64 = struct $caligned
    ref void    hwnd
    int32       message
    int64       wparam
    int64       lparam
    int32       time
    int32       ptx
    int32       pty
end

export type ws_point = struct
    int32 x, y
end

export type ws_rect=struct      
    union
        struct
            int32 leftx,top, rightx,bottom
        end
        struct
            union int32 x,x1 end
            union int32 y,y1 end
            int32 x2,y2
        end
    end
end

export type ws_logbrush = struct
    int32 lbstyle
    int32 lbcolour
    int32 lbhatch
end

export type ws_textmetrics = struct
    int32   height
    int32   ascent
    int32   descent
    int32   int32ernalleading
    int32   externalleading
    int32   avecharwidth
    int32   maxcharwidth
    int32   weight
    int32   overhang
    int32   digitizedaspectx
    int32   digitizedaspecty
    byte    firstchar
    byte    lastchar
    byte    defaultchar
    byte    breakchar
    byte    italic
    byte    underlined
    byte    struckout
    byte    pitchandfamily
    byte    charset
end

export type ws_bitmapv5header = struct
    int32   size
    int32   width
    int32   height
    word16  planes
    word16  bitcount
    int32   compression
    int32   sizeimage
    int32   xpelspermeter
    int32   ypelspermeter
    int32   clrused
    int32   clrimportant
    int32   redmask
    int32   greenmask
    int32   bluemask
    int32   alphamask
    int32   cstype
    [1..9]int32 endpoints
    int32   redgamma
    int32   greengamma
    int32   bluegamma
    int32   intent
    int32   profiledata
    int32   profilesize
    int32   reserved
end

export type ws_bitmapfileheader = struct
    wt_word     typex
    wt_dword    size
    wt_word     res1, res2
    wt_dword    offbits
end

export type ws_bitmapinfoheader = struct
    wt_dword    size
    wt_long     width
    wt_long     height
    wt_word     planes
    wt_word     bitcount
    wt_dword    compression
    wt_dword    sizeimage
    wt_long     xpelspermetre
    wt_long     ypelspermetre
    wt_dword    clrused
    wt_dword    clrimportant
end

export type ws_paintstruct = struct
    int64       hdc
    int32       erase
    ws_rect     paintrect
    int32       restore
    int32       incupdate
    [32]byte    rgbreserved
end

export type ws_openfilename32 = struct
    wt_dword        structsize
    wt_handle32     owner
    wt_handle32     instance
    wt_string32     filter
    wt_string32     customfilter
    wt_dword        maxcustfilter
    wt_dword        filterindex
    wt_string32     file
    wt_dword        maxfile
    wt_string32     filetitle
    wt_dword        maxfiletitle
    wt_string32     initialdir
    wt_string32     title
    wt_dword        flags
    wt_word         fileoffset
    wt_word         fileextension
    wt_string32     defext
    wt_lparam32     custdata
    wt_wndproc32    hook
    wt_string32     templatename
    wt_ptr32        reserved1
    wt_dword        reserved2
    wt_dword        flagsex
end

export type ws_openfilename64 = struct $caligned
    wt_dword        structsize
    wt_handle64     owner
    wt_handle64     instance
    wt_string64     filter
    wt_string64     customfilter
    wt_dword        maxcustfilter
    wt_dword        filterindex
    wt_string64     file
    wt_dword        maxfile
    wt_string64     filetitle
    wt_dword        maxfiletitle
    wt_string64     initialdir
    wt_string64     title
    wt_dword        flags
    wt_word         fileoffset
    wt_word         fileextension
    wt_string64     defext
    wt_lparam64     custdata
    wt_wndproc64    hook
    wt_string64     templatename
    wt_ptr64        reserved1
    wt_dword        reserved2
    wt_dword        flagsex
end

importdll kernel32=
    windows func    "GetLastError"                  :wt_dword
    windows func    "GetStdHandle"                  (wt_dword)wt_handle
    windows func    "WriteConsoleA" as writeconsole             (wt_handle,wt_string,wt_dword,wt_ptr,wt_ptr)wt_bool
    windows func    "SetConsoleCursorPosition"      (wt_handle,wt_coord)wt_bool
    windows func    "GetConsoleScreenBufferInfo"    (wt_handle,wt_ptr)wt_bool
    windows func    "SetConsoleMode"                (wt_handle,wt_dword)wt_bool
    windows func    "WriteConsoleOutputA" as writeconsoleoutput         (wt_handle,wt_ptr,wt_coord,wt_coord,wt_ptr)wt_bool

    windows func    "GetConsoleScreenBufferInfoEx"  (wt_handle,wt_ptr)wt_bool
    windows func    "SetConsoleScreenBufferInfoEx"  (wt_handle,wt_ptr)wt_bool

    windows func    "SetConsoleTextAttribute"       (wt_handle,wt_word)wt_bool
    windows func    "SetConsoleTitleA" as setconsoletitle               (wt_string)wt_bool
    windows func    "ReadConsoleInputA" as readconsoleinput         (wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool
    windows func    "PeekConsoleInputA"         (wt_handle,wt_ptr,wt_dword,wt_ptr)wt_bool
    windows func    "FlushConsoleInputBuffer"       (wt_handle)wt_bool
    windows func    "SetConsoleWindowInfo"          (wt_handle,wt_bool,wt_ptr)wt_bool
    windows func    "SetConsoleScreenBufferSize"    (wt_handle,wt_coord)wt_bool
    windows func    "GetConsoleCursorInfo"          (wt_handle,wt_ptr)wt_bool
    windows func    "SetConsoleCursorInfo"          (wt_handle,wt_ptr)wt_bool
    windows func    "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)wt_bool

    windows func    "FindFirstFileA" as findfirstfile       (stringz,ref int32)int32
    windows func    "FindNextFileA"  as findnextfile            (int32,ref int32)int32
    windows func    "FindClose"                 (int32)int32
    windows func    "SetCurrentDirectoryA" as setcurrentdirectory   (stringz)int32
    windows func    "GetCurrentDirectoryA" as getcurrentdirectory   (int32,int32)int32
    windows func    "CreateDirectoryA" as createdirectory       (stringz,int32)int32
    windows func    "GetFileAttributesA"            (stringz)int32
    windows func    "GetModuleHandleA" as getmodulehandle       (wt_string)wt_handle
    windows func    "GetTickCount"                              :wt_dword
    windows func    "GlobalAlloc"                                   (wt_uint,wt_size)wt_handle
    windows func    "GlobalLock"                                    (wt_handle)wt_ptr
    windows func    "GlobalUnlock"                              (wt_handle)wt_bool
    windows func    "GlobalSize"                                    (wt_handle)wt_size

    windows func    "GetSystemTime"(ref byte)int32
    windows func    "Beep"                          (wt_dword, wt_dword)wt_bool
    windows func    "SetConsoleCP"                              (wt_uint)wt_bool
end

importdll user32=
    windows func    "CreateWindowExA" as createwindowex     (wt_dword, wt_string, wt_string, wt_dword, wt_int,wt_int,wt_int,wt_int,
                                                     wt_handle, wt_handle, wt_handle, wt_ptr)wt_handle

    windows func    "GetMessageA" as getmessage             (wt_ptr, wt_handle, wt_uint, wt_uint)wt_bool
    windows func    "TranslateMessage"                      (wt_ptr)wt_bool
    windows func    "DispatchMessageA" as dispatchmessage       (wt_ptr)wt_result
    windows func    "SetTimer"                              (wt_handle,wt_intptr,wt_uint,wt_ptr)wt_intptr
    windows func    "KillTimer"                             (wt_handle,wt_intptr)wt_bool
    windows func    "SystemParametersInfoA"                 (wt_uint,wt_uint,wt_ptr,wt_uint)wt_bool
    windows func    "GetSystemMetrics"                      (wt_int)wt_int
    windows func    "AppendMenuA" as appendmenu             (wt_handle,wt_uint,wt_intptr,wt_string)wt_bool
    windows func    "GetDC"                                 (wt_handle)wt_handle
    windows func    "ReleaseDC"                             (wt_handle,wt_handle)wt_int

    windows func    "SendMessageA" as sendmessage               (wt_handle,wt_uint,wt_wparam,wt_lparam)wt_result
    windows func    "PostMessageA" as postmessage               (wt_handle,wt_uint,wt_wparam,wt_lparam)wt_bool
    windows func    "PeekMessageA" as peekmessage               (wt_ptr,wt_handle,wt_uint,wt_uint,wt_uint)wt_bool
    windows func    "BeginPaint"                                (wt_handle,wt_ptr)wt_handle
    windows func    "EndPaint"                              (wt_handle,wt_ptr)wt_bool
    windows procedure   "PostQuitMessage"                   (wt_int)
    windows func    "LoadIconA" as loadicon                 (wt_handle,wt_string)wt_handle
    windows func    "LoadCursorA" as loadcursor             (wt_handle,wt_string)wt_handle
    windows func    "SetCursor"                             (wt_handle)wt_handle
    windows func    "DrawMenuBar"                               (wt_handle)wt_bool
    windows func    "GetSystemMenu"                         (wt_handle,wt_bool)wt_handle
    windows func    "CreateMenu"                                :wt_handle
    windows func    "CreatePopupMenu"                           :wt_handle
    windows func    "DestroyMenu"                               (wt_handle)wt_bool
    windows func    "CheckMenuItem"                         (wt_handle,wt_uint,wt_uint)wt_dword
    windows func    "EnableMenuItem"                            (wt_handle,wt_uint,wt_uint)wt_bool
    windows func    "GetSubMenu"                                (wt_handle,wt_int)wt_handle
    windows func    "GetMenuItemID"                         (wt_handle,wt_int)wt_uint
    windows func    "GetMenuItemCount"                      (wt_handle)wt_int
    windows func    "InsertMenuA" as insertmenu             (wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool
    windows func    "ModifyMenuA" as modifymenu             (wt_handle,wt_uint,wt_uint,wt_intptr,wt_string)wt_bool
    windows func    "RemoveMenu"                                (wt_handle,wt_uint,wt_uint)wt_bool
    windows func    "DeleteMenu"                                (wt_handle,wt_uint,wt_uint)wt_bool

    windows func    "DestroyWindow"                         (wt_handle)wt_bool
    windows func    "InvalidateRect"                            (wt_handle,wt_ptr,wt_bool)wt_bool
    windows func    "ValidateRect"                          (wt_handle,wt_ptr)wt_bool
    windows func    "ShowWindow"                                (wt_handle,wt_int)wt_bool
    windows func    "GetClassLongA" as getclassint          (wt_handle,wt_int)wt_word
    windows func    "SetClassLongA" as setclasslong         (wt_handle,wt_int,wt_dword)wt_word
    windows func    "SetWindowTextA" as setwindowtext           (wt_handle,wt_string)wt_bool
    windows func    "GetWindowTextA" as getwindowtext           (wt_handle,wt_string,wt_int)wt_int
    windows func    "GetWindowTextLengthA" as getwindowtextlength   (wt_handle)wt_int
    windows func    "GetKeyState"                               (wt_int)wt_word

    windows func    "GetWindowLongA" as getwindowlongptr        (wt_handle,wt_int)int64
    windows func    "SetWindowLongA" as setwindowlongptr        (wt_handle,wt_int,int64)int64

    windows func    "GetClientRect"                         (wt_handle,wt_ptr)wt_bool
    windows func    "ClientToScreen"                            (wt_handle,wt_ptr)wt_bool
    windows func    "ScreenToClient"                            (wt_handle,wt_ptr)wt_bool
    windows func    "GetWindowRect"                         (wt_handle,wt_ptr)wt_bool
    windows func    "GetSysColor" as getsyscolour               (wt_int)wt_dword
    windows func    "GetScrollInfo"                         (wt_handle,wt_int,wt_ptr)wt_bool
    windows func    "GetMenu"                                   (wt_handle)wt_handle
    windows func    "SetMenu"                                   (wt_handle,wt_handle)wt_ptr
    windows func    "TrackPopupMenu"                            (wt_handle,wt_uint,wt_int,wt_int,wt_int,wt_handle,wt_ptr)wt_bool
    windows func    "GetMenuState"                          (wt_handle,wt_uint,wt_uint)wt_uint
    windows func    "MessageBoxA" \
                                (wt_handle a=nil,wt_string message, wt_string caption="Caption", wt_uint b=0)wt_int
    windows func    "OpenClipboard"                         (wt_handle)wt_bool
    windows func    "CloseClipboard"                            :wt_bool
    windows func    "EmptyClipboard"                            :wt_bool
    windows func    "GetClipboardData"                      (wt_uint)wt_handle
    windows func    "SetClipboardData"                      (wt_uint,wt_handle)wt_handle
    windows func    "MessageBeep"                           (wt_uint x=0)wt_bool
end

importdll gdi32=
    windows func    "Rectangle"                             (wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows func    "RoundRect"                             (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows func    "Ellipse"                                   (wt_handle,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows func    "Arc"                                       (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows func    "Chord"                                 (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows func    "Pie"                                       (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int)wt_bool
    windows func    "Polygon"                                   (wt_handle,wt_handle,wt_int)wt_bool
    windows func    "TextOutA" as textout                       (wt_handle,wt_int,wt_int,wt_string,wt_int)wt_bool
    windows func    "TextOutW"                      (wt_handle,wt_int,wt_int,wt_ptr,wt_int)wt_bool
    windows func    "GetStockObject"                            (wt_int)wt_handle
    windows func    "SelectObject"                          (wt_handle,wt_handle)wt_handle
    windows func    "CreateDCA" as createdc                 (wt_string,wt_string,wt_string,wt_ptr)wt_handle
    windows func    "MoveToEx"                      (wt_handle a,wt_int b,wt_int c,wt_ptr d=nil)wt_bool
    windows func    "CreatePen"                             (wt_int,wt_int,wt_dword)wt_handle
    windows func    "CreateSolidBrush"                      (wt_dword)wt_handle
    windows func    "CreateBrushIndirect"                       (wt_ptr)wt_handle
    windows func    "LineTo"                                    (wt_handle,wt_int,wt_int)wt_bool
    windows func    "GetPixel"                              (wt_handle,wt_int,wt_int)wt_dword
    windows func    "SetPixel"                              (wt_handle,wt_int,wt_int,wt_dword)wt_dword
    windows func    "SetGraphicsMode"                           (wt_handle,wt_int)wt_int
    windows func    "CreateFontIndirectA" as createfontindirect (wt_ptr)wt_handle
    windows func    "CreateFontA" as createfont \
            (wt_int height, wt_int width=0, wt_int escapement=0, wt_int orientation=0, wt_int bold=0,
             wt_dword italic=0, wt_dword underline=0, wt_dword strikeout=0, wt_dword charset=0,
             wt_dword outprec=0, wt_dword clipprec=0, wt_dword quality=0, wt_dword pitch=0, wt_string facename)wt_handle
    windows func    "SaveDC"                                    (wt_handle)wt_int
    windows func    "GetTextMetricsA" as gettextmetrics     (wt_handle,wt_ptr)wt_bool
    windows func    "DeleteObject"                          (wt_handle)wt_bool
    windows func    "RestoreDC"                             (wt_handle,wt_int)wt_bool
    windows func    "GetTextExtentPoint32A" as gettextextentpoint32 (wt_handle,wt_string,wt_int,wt_ptr)wt_bool
    windows func    "GetObjectA" as getobject                   (wt_handle,wt_int,wt_ptr)wt_int
    windows func    "CreatePalette"                         (wt_ptr)wt_handle
    windows func    "GetWindowExtEx"                            (wt_handle,wt_ptr)wt_bool
    windows func    "CreateCompatibleBitmap"                    (wt_handle,wt_int,wt_int)wt_handle
    windows func    "SetBitmapBits"                         (wt_handle,wt_dword,wt_ptr)wt_long
    windows func    "SelectPalette"                         (wt_handle,wt_handle,wt_bool)wt_handle
    windows func    "RealizePalette"                            (wt_handle)wt_uint
    windows func    "SetDIBitsToDevice"                     (wt_handle,wt_int,wt_int,wt_dword,wt_dword,wt_int,wt_int,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int
    windows func    "StretchDIBits"                         (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_int,wt_ptr,wt_ptr,wt_uint,wt_dword)wt_int
    windows func    "SetStretchBltMode"                     (wt_handle,wt_int)wt_int
    windows func    "PatBlt"                                    (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_dword)wt_bool
    windows func    "BitBlt"                                    (wt_handle,wt_int,wt_int,wt_int,wt_int,wt_handle,wt_int,wt_int,wt_dword)wt_bool
    windows func    "SetROP2"                                   (wt_handle,wt_int)wt_int
    windows func    "CreateCompatibleDC"                        (wt_handle)wt_handle
    windows func    "DeleteDC"                              (wt_handle)wt_bool
    windows func    "CreateBitmap"                          (wt_int,wt_int,wt_uint,wt_uint,wt_ptr)wt_handle
    windows func    "CreateBitmapIndirect"                  (wt_ptr)wt_handle
    windows func    "CreateDIBitmap"                            (wt_handle,wt_ptr,wt_dword,wt_ptr,wt_ptr,wt_uint)wt_handle
    windows func    "CreateDIBSection"                      (wt_handle,wt_ptr,wt_uint,wt_ptr,wt_handle,wt_dword)wt_handle
    windows func    "StretchBlt"                                (wt_handle,wt_int,wt_int, wt_int,wt_int,wt_handle, wt_int,wt_int,wt_int, wt_int,wt_dword)wt_bool
    windows func    "PlgBlt"                                (wt_handle,wt_ptr,wt_handle, wt_int,wt_int,wt_int,wt_int, wt_handle, wt_int,wt_int)wt_bool
    windows func    "SetTextColor"  as settextcolour            (wt_handle,wt_dword)wt_dword
    windows func    "SetTextAlign"                          (wt_handle,wt_uint)wt_uint
    windows func    "SetTextJustification"                  (wt_handle,wt_int,wt_int)wt_bool
    windows func    "SetBkColor"  as setbkcolour                (wt_handle,wt_dword)wt_dword
    windows func    "SetBkMode"                             (wt_handle,wt_int)wt_int
    windows func    "GetBkColor"  as getbkcolour                (wt_handle)wt_dword
    windows func    "GetBkMode"                             (wt_handle)wt_int
    windows func    "StartDocA" as startdoc                 (wt_handle,wt_ptr)wt_int
    windows func    "StartPage"                             (wt_handle)wt_int
    windows func    "EndPage"                                   (wt_handle)wt_int
    windows func    "EndDoc"                                    (wt_handle)wt_int
    windows func    "AbortDoc"                              (wt_handle)wt_int
    windows func    "GetViewportOrgEx"                      (wt_handle,wt_ptr)wt_bool
    windows func    "GetDIBits"                             (wt_handle,wt_handle,wt_uint,wt_uint,wt_ptr,wt_ptr,wt_uint)wt_int
    windows func    "GetDIBColorTable" as getdibcolourtable (wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint
    windows func    "SetDIBColorTable" as setdibcolourtable (wt_handle,wt_uint,wt_uint,wt_ptr)wt_uint
    windows func    "GetTextAlign"                          (wt_handle)wt_uint
end

importdll comdlg32=
    windows func    "GetOpenFileNameA"                      (wt_ptr)wt_bool
    windows func    "GetSaveFileNameA"                      (wt_ptr)wt_bool
end
=== gxlib.q 0 1 36/43 ===
module sysp
module winconsts
module winapi
module wingxlib
module gxmisc

export var debug=0

export var messhandlertable=9000    
export var actionhandlertable

export var chx,chy      
export var cha,chd      
export const smx=3      
export const smy=4
export var arrowdim
export var markdim
export var buttonheight
export var listrowheight
export const labelfont=1

export var tabstops=(8,)*20

export var wmouse=0
export var wfocus=0
export var wprinter=0
export var lastmousepos=0
export var lastmousewindow=0
export var currmousewindow=0

export var mousepos
export var mousesw
export var quitmess=0
export var dragmode=0
export var lastbuttontime=0

export var buttonstate=0
export var wmessagetable        
export var buttontable      

const maxqueuesize=100
export var messagequeue=()
export var nmessages=()

const dragtol=1

export var copymode=4

var vktomesstable

export record rwindow =

    var windclass                   
    var flags                       
    var style                       
    var name                        

    var owner                       
    var index                       
    var childlist                   

    var frameposx, frameposy        
    var framedimx, framedimy        

    var posx, posy                  
    var dimx, dimy                  

    var gdi                         

    var enable                      
    var id                          
    var text                        
    var linkvar                     
    var gindex                      

    var attrs                       

    var pixelbits                   
    var pixelptr                    
    var pixelbytes                  
    var linebytes                   
    var framebytes
    var paltype                     
end


export enumdata paltypenames =
    (no_pal=0,      $),
    (greyscale_pal, $),
    (tinted_pal,    $),
    (colour_pal,    $),
    (uv_pal,        $),
end

export type rgdistate = struct
    ref void hwnd               
    ref void hdc                
    ref void hwnd2              
    ref void hdc2               
    int64 originalwndproc   
    union
        int64 menuhandle        
        int64 oldbmobj          
    end
    int32 drawmode          
    int32 updated               

    int32 posx,posy         
    int32 pencolour         
    int32 penwidth          

    int32 penstyle          

    int32 xormode               
    int32 brushcolour           
    int32 brushstyle            
    int32 brushpattern      

    int32 font              
end

export enumdata marktypenames =
    (no_mark=0,         $),
    (radio_mark,        $),
    (tick_mark,         $),
    (check_mark,        $),
    (invert_mark,       $),
    (outline_mark,      $),
    (bold_mark,         $),
end

export enumdata hilitetypenames =
    (no_hilite=0,       $),
    (invert_hilite,     $),
    (outline_hilite,    $),
end

export record togglerec=        
    var textoffset
    var onvalue
end

export record scrollbarrec=     
    var limits                      
    var span                        
    var thumbsize                   
    var thumbspan                   
    var thumbpos                    
    var currpos                     
    var dragmode                    
end

export record editboxrec=       
    var currpos                     
    var caretpos                    
    var textpos                     
end

export record listboxrec=
    var rows                        
    var pagepos                     
    var length                      
    var currpos                     
    var pitch, offset               
end

export record rmessage=
    var     wind        
    var     menuwind    
    var     message     
    var     state       
    var     a,b         
    var     x,y         
end


export type stylerec = struct
    byte    border              
    byte    justify             
    byte    vjustify            
    byte    windbgnd            
    byte    textfgnd            
    byte    textbgnd            
    byte    bgndmode            
    byte    textfont            
    byte    textsize            
    byte    textbold            
    byte    textitalic          
    byte    ispassword          
    byte    fieldwidth          
    byte    dir                 
    byte    marktype            
    byte    hilitetype          
    byte    iframe              
    byte    imark               
    byte    hscroll             
    byte    vscroll             
    byte    lbchange            
    byte    returnmess          
    byte    noupdate            
end

export record rpoint = var x,y end
export record rrect  = (var pos,dim)
export record rframe = (var x1,y1,x2,y2)

export record getrec=
    method getbounds(&self)=
        return 0
    end method
    method getitem(&self,n)=
        return 0
    end method
    method getstritem(&self,n)= return "" end method
end

export enumdata stylenames =    
    (ss_border,         $),     
    (ss_justify,        $),     
    (ss_vjustify,       $),     
    (ss_textfgnd,       $),     
    (ss_textbgnd,       $),     
    (ss_bgndmode,       $),     
    (ss_textfont,       $),     
    (ss_textsize,       $),     
    (ss_textbold,       $),     
    (ss_textitalic,     $),     
    (ss_ispassword,     $),     
    (ss_marktype,       $),     
    (ss_hilitetype,     $),     
    (ss_iframe,         $),     
    (ss_windbgnd,       $),     
    (ss_imark,          $),     
    (ss_hscroll,        $),     
    (ss_vscroll,        $),     
    (ss_lbchange,       $),     
    (ss_returnmess,     $),     
    (ss_noupdate,       $),     
end

export enumdata drawmodenames =
                                
    (dm_screen=0,       $),     
    (dm_memory,         $),     
    (dm_screenmemory,   $),     
    (dm_memoryscreen,   $),     
end


export enumdata wfnames =
    (wa_rightclick=0,   $),     
    (wa_middleclick,    $),     
    (wa_leftdbl,        $),     
    (wa_rightdbl,       $),     
    (wa_middledbl,      $),     
    (wa_leftdrag,       $),     
    (wa_rightdrag,      $),     
    (wa_middledrag,     $),
    (wa_autoupdate,     $),     
    (wa_tab,            $),     
    (wa_strvar,         $),     
    (wa_retmess,        $),     
    (wa_retsel,         $),     
    (wa_memory,         $),     
    (wa_maximised,      $),     
    (wa_param1,         $),     
    (wa_param2,         $), 
    (wa_useenter,       $),
    (wa_closed,         $),     

    (wa_$last,          $)
end

const wa_needdbl    = wa_param1 
const wa_editdd     = wa_param2 

export enumdata bsnames, bscat, bswidths=
    (bs_none=0,     $,  0,  ws_rect(0,0,0,0)),          
    (bs_simplew,    $,  'W',    ws_rect(1,1,1,1)),          
    (bs_simple,     $,  'X',    ws_rect(1,1,1,1)),          
    (bs_thick,      $,  'X',    ws_rect(2,2,2,2)),          
    (bs_panel,      $,  'X',    ws_rect(1,1,1,1)),          
    (bs_inset,      $,  'X',    ws_rect(1,1,1,1)),          
    (bs_ownsimple,  $,  'I',    ws_rect(0,0,0,0)),          
    (bs_ownpanel,   $,  'I',    ws_rect(0,0,0,0)),          
    (bs_owninset,   $,  'I',    ws_rect(0,0,0,0)),          
    (bs_testext,    $,  'X',    ws_rect(10,10,10,10)),
    (bs_testint,    $,  'I',    ws_rect(8,8,8,8)),
    (bs_dummy,      $,  0,  ws_rect(0,0,0,0))
end

export enumdata windowclassnames, defaultborderstyles =
    (no_class=0,        $,  bs_none),           
    (window_class,      $,  wbs_resize),        
    (memwindow_class,   $,  wbs_none),          
    (popup_class,       $,  wbs_thick),         
    (float_class,       $,  bs_thick),          
    (bitmap_class,      $,  bs_none),           
    (screen_class,      $,  bs_none),           
    (printer_class,     $,  bs_none),           

    (group_class,       $,  bs_inset),          
    (panel_class,       $,  bs_inset),          
    (button_class,      $,  bs_simplew),        
    (toggle_class,      $,  bs_none),           
    (select_class,      $,  bs_none),           
    (editbox_class,     $,  bs_simplew),        
    (scrollbar_class,   $,  bs_simplew),        
    (listbox_class,     $,  bs_simplew),        
    (dropdown_class,    $,  bs_none),           
    (framebar_class,    $,  bs_panel),          
    (statusbar_class,   $,  bs_panel),          
    (tooltip_class,     $,  bs_simplew),        
    (arrow_class,       $,  bs_ownpanel),       
    (mark_class,        $,  bs_none),           
    (label_class,       $,  bs_none),           
    (dummy_class,       $,  bs_none)
end


export enumdata actionnames=
    (draw_w,        $),
    (update_w,      $),
    (last_w,        $),
end


export enumdata messagenames=

    (mm_null=0,         $),     

    (mm_activate,       $),     
    (mm_close,          $),     
    (mm_sizewindow,     $),     
    (mm_movewindow,     $),     
    (mm_restore,        $),     

    (mm_setcursor,      $),     
    (mm_setfocus,       $),     
    (mm_killfocus,      $),     

    (mm_move,           $),     
    (mm_click,          $),     
    (mm_dblclick,       $),     
    (mm_clickup,        $),     
    (mm_rclick,         $),     
    (mm_rdblclick,      $),     
    (mm_rclickup,       $),     
    (mm_mclick,         $),     
    (mm_mdblclick,      $),     
    (mm_mclickup,       $),     
    (mm_hover,          $),     

    (mm_onwindow,       $),     
    (mm_offwindow,      $),     
    (mm_draw,           $),     
    (mm_update,         $),     

    (mm_startdrag,      $),     
    (mm_rstartdrag,     $),     
    (mm_mstartdrag,     $),     
    (mm_drag,           $),     
    (mm_enddrag,        $),     

    (mm_command,        $),     
    (mm_dblcommand,     $),     

    (mm_rcommand,       $),     
    (mm_rdblcommand,    $),     

    (mm_mcommand,       $),     
    (mm_mdblcommand,    $),     

    (mm_char,           $),     
    (mm_key,            $),     
    (mm_keyup,          $),     

    (mm_sethozpos,      $),     
    (mm_setvertpos,     $),     
    (mm_select,         $),     
    (mm_pick,           $),     
    (mm_wheel,          $),     
    (mm_lbchange,       $),     

    (mm_timer,          $),     

    (mm_cancel,         $),     
    (mm_ok,             $),     
    (mm_help,           $),     
    (mm_cmdline,        $),     

    (mm_leftkey ,       $),     
    (mm_rightkey,       $),     
    (mm_upkey,          $),     
    (mm_downkey,        $),     
    (mm_pageupkey,      $),     
    (mm_pagedownkey,    $),     
    (mm_homekey,        $),     
    (mm_endkey,         $),     
    (mm_tabkey,         $),     
    (mm_bskey,          $),     
    (mm_deletekey,      $),     
    (mm_enterkey,       $),     
    (mm_insertkey,      $),     
    (mm_functionkey,    $),     

    (mm_up,             $),     
    (mm_down,           $),     
    (mm_right,          $),     
    (mm_left,           $),     
    (mm_edit,           $),     
    (mm_edited,         $),     
    (mm_last,           $)
end


export const mm_user    = 200

export const kb_lbutton = 0x1   
export const kb_rbutton = 0x2
export const kb_mbutton = 0x4

export const kb_shift   = 0x8   
export const kb_ctrl    = 0x10
export const kb_alt     = 0x20
export const kb_capslock    = 0x40
export const kb_dblclick    = 0x80  

export const kb_rshift  = 0x100
export const kb_rctrl   = 0x200
export const kb_ralt    = 0x400

export enumdata colournames, colourvalues =
    (black,     $,  0x_00'00'00),
    (red,       $,  0x_00'00'C0),
    (dkred,     $,  0x_00'00'90),
    (red3,      $,  0x_00'00'70),
    (green,     $,  0x_00'C0'00),
    (dkgreen,   $,  0x_00'90'00),
    (green3,    $,  0x_00'70'00),
    
    (blue,      $,  0x_C0'00'00),
    (dkblue,    $,  0x_90'00'00),
    (blue3,     $,  0x_70'00'00),

    (cyan,      $,  0x_c0'c0'00),
    (dkcyan,    $,  0x_90'90'00),
    (cyan3,     $,  0x_70'70'00),

    (magenta,   $,  0x_c0'00'c0),
    (dkmagenta, $,  0x_90'00'90),
    (magenta3,  $,  0x_70'00'70),

    (yellow,    $,  0x_00'C0'C0),
    (dkyellow,  $,  0x_00'90'90),
    (yellow3,   $,  0x_00'70'70),
    (yellow4,   $,  0x_00'50'50),

    (white,     $,  0x_FF'FF'FF),
    (ltgrey,    $,  0x_C0'C0'C0),
    (grey,      $,  0x_90'90'90),
    (dkgrey,    $,  0x_70'70'70),

    (ltorange,  $,  0x_00'A0'FF),
    (orange,    $,  0x_00'60'FF),
    (flesh,     $,  0x_70'85'EE),
    (pink,      $,  0x_9A'32'DB),
    (dkpink,    $,  0x_72'24'A9),
    (brown,     $,  0x_46'43'7D),
    (blue4,     $,  0x_B7'1C'5E),
    (blue5,     $,  0x_6F'3D'0D),
    (olive,     $,  0x_05'A0'88),
    (ltbrown,   $,  0x_00'70'B0),

    (blue6,     $,  0x_9C'63'1C),
    (green4,    $,  0x_12'51'11),
    (purple,    $,  0x_5E'0D'73),
    (blue7,     $,  0x_E6'27'1C),
    (crimson,   $,  0x_15'2A'D3),
    (violet,    $,  0x_54'16'A0),
    (blue8,     $,  0x_86'68'1E),
    (dkorange,  $,  0x_25'6A'D4),
    (green5,    $,  0x_09'46'41),
    (blue9,     $,  0x_65'0A'1D),

    (ltred,     $,  0x_00'00'FF),
    (ltgreen,   $,  0x_00'FF'00),
    (ltblue,    $,  0x_FF'00'00),
    (ltcyan,    $,  0x_FF'FF'00),
    (ltmagenta, $,  0x_FF'00'FF),
    (ltyellow,  $,  0x_00'FF'FF),

    (button_col,    $,  0),     
    (window_col,    $,  0),     
    (text_col,      $,  0),     
end

export const skipmess = 1       
export const thismess = 0       

export var bmbgnd
export var defstyle         
export var currmess

export var wapplic=nil
export var wscreen=nil

var data,ndata
var tabstack,ntab
var breakflag

const k_menu=30000
const kdivide=30001
const kcolumn=30002
const kfilehistory=30003

var caretdrawn=0
var dkcolour=0x000000
var ltcolour=0xFFFFFF
var thumbdragmode=0
var thumbstartpos=0

var dirtomess=['L':mm_left,'R':mm_right,'U':mm_up,'D':mm_down]

proc start=

initdata()
mxinit()
initmenuhandlers()


end

export proc setupgdi(w,hwnd)=           


if w.gdi then
    return          
fi

gdi:=new(rgdistate)
gdi.hwnd:=hwnd


if w.usertype=rwindow then
    gdi.hdc:=getdc(hwnd)
    gdi.drawmode:=dm_screen
else                        
    gdi.hdc:=createcompatibledc(nil)
    gdi.drawmode:=dm_memory
fi

gdi.posx:=gdi.posy:=0
gdi.updated:=0
gdi.font:=0
gdi.pencolour:=getsyscolour(colour_windowtext)
gdi.penwidth:=0
gdi.penstyle:=ps_solid
gdi.xormode:=0
gdi.brushcolour:=0xff'ff'ff
gdi.brushstyle:=bs_solid
gdi.brushpattern:=0
w.gdi:=gdi
end

export const arleft = "<"
export const arright = ">"
export const arup = "^"
export const ardown = "V"

export var allwindows::=()          

export func ctrlpressed = return (currmess.state iand kb_ctrl) end
export func shiftpressed = return (currmess.state iand kb_shift) end

proc initdata=
messagequeue:=new(list,100)
nmessages:=0

colourvalues::=colourvalues

colourvalues[button_col]:=getsyscolour(colour_btnface)

colourvalues[window_col]:=getsyscolour(colour_window)
colourvalues[text_col]:=getsyscolour(colour_windowtext)

defstyle:=new(stylerec)
defstyle.border     := bs_simplew
defstyle.justify    := 'L'
defstyle.vjustify   := 'M'
defstyle.textfgnd   := black
defstyle.marktype   := check_mark
defstyle.hilitetype := no_hilite
defstyle.windbgnd   := button_col
defstyle.imark      := 1

init_handlertables()
d:=gxchardim(labelfont)
chx:=d.x
chy:=d.y

d:=gxchardim(0,1)
cha:=d.x
chd:=d.y
arrowdim:=chy+2
markdim:=arrowdim-2

buttonheight:=chy+smy*2
listrowheight:=chy+smy*2
end

export func gxcreatewindow(?caption,?pos,?dim,?options,owner=nil)=      

hwnd:=wx_createpopup(caption,pos,dim,options,(owner|owner.gdi.hwnd|nil))

w:=newwindow(hwnd,0,no_class,bs_windows)

if wapplic=nil then
    wapplic:=w
fi

W.STYLE:=NEW(STYLEREC)
W.STYLE.BORDER:=0
W.WINDCLASS:=WINDOW_CLASS
W.STYLE.WINDBGND:=WINDOW_COL
W.ENABLE:=1
W.FLAGS.[WA_LEFTDRAG]:=1
W.FLAGS.[WA_LEFTDBL]:=1

setwindowdims_w(w,hwnd)

setupgdi(w,hwnd)
gxfont(w,1)

GXDRAWMODE(W,DM_SCREENMEMORY)
GXCLEAR(W)

return w
end

proc setwindowdims_w(w,hwnd)=           

box:=new(ws_rect)
getwindowrect(hwnd,&box)
w.frameposx:=box.x
w.frameposy:=box.y
w.framedimx:=box.x2-box.x
w.framedimy:=box.y2-box.y

getclientrect(hwnd,&box)

w.dimx:=box.x2-box.x
w.dimy:=box.y2-box.y

pt:=ws_point(0,0)
clienttoscreen(hwnd,&pt)        
w.posx:=pt.x
w.posy:=pt.y
end

proc setwindowdims_c(w,hwnd)=           


box:=new(ws_rect)
getwindowrect(hwnd,&box)            
w.posx:=box.x-w.owner.posx
w.posy:=box.y-w.owner.posy
w.dimx:=box.x2-box.x
w.dimy:=box.y2-box.y

widths:=bswidths[w.style.border]
if bscat[w.style.border]='I' then widths:=ws_rect(0,0,0,0) fi

w.frameposx:=w.posx-widths.x1
w.frameposy:=w.posy-widths.y1
w.framedimx:=w.dimx+widths.x1+widths.x2
w.framedimy:=w.dimy+widths.y1+widths.y2
end

export proc gxclear(w,?colour)=         

gdi:=w.gdi
gdi.updated:=1

gxcolour(w,getrgb(black))
gxstyle(w,0)

if colour.isvoid then
    colour:=getrgb(w.style.windbgnd)
fi

oldpenstyle:=gdi.penstyle
oldbrushstyle:=gdi.brushstyle

gxbrushstyle(w,bs_solid)
gxstyle(w,ps_null)

gxfillrect(w,0,0,w.dimx,w.dimy,colour)
gxbrushstyle(w,oldbrushstyle)
gxstyle(w,oldpenstyle)
end

export func gxstyle(w,?style)=          

gdi:=w.gdi

if style.isdef and gdi.penstyle<>style then
    case style
    when '!' then style:=ps_dashdot
    when ':' then style:=ps_dashdotdot
    when '-' then style:=ps_dot
    when ' ' then style:=ps_null
    when 'D' then style:=ps_alternate
    when '|','S',0 then style:=ps_solid
    when 'F' then style:=ps_insideframe
    esac

    gdi.penstyle:=style
    if style>=10 then style:=ps_dot fi
    deleteobject(selectobject(gdi.hdc,createpen(style,gdi.penwidth,gdi.pencolour)))
    if gdi.drawmode=dm_screenmemory then
        deleteobject(selectobject(gdi.hdc2,createpen(style,gdi.penwidth,gdi.pencolour)))
    fi
fi
return gdi.penstyle
end

export proc gxbrushstyle(w,?style,?pattern)=        

gdi:=w.gdi
brush:=new(ws_logbrush)

if style.isdef then
    if style<>gdi.brushstyle then
        case style
        when 'S' then style:=bs_solid
        when 'H' then style:=bs_hatched
        when ' ' then style:=bs_null
        when 'B' then style:=bs_dibpattern
        esac

        gdi.brushstyle:=style
    fi
    gdi.brushpattern:=0     
fi

if pattern.isdef and pattern<>gdi.brushpattern then
    case pattern
    when '-' then pattern:=hs_horizontal
    when '|' then pattern:=hs_vertical
    when '\\' then pattern:=hs_fdiagonal
    when '/' then pattern:=hs_bdiagonal
    when '+' then pattern:=hs_cross
    when 'x','X' then pattern:=hs_diagcross
    esac
    gdi.brushpattern:=pattern
fi

brush.lbstyle:=gdi.brushstyle
brush.lbcolour:=gdi.brushcolour
brush.lbhatch:=gdi.brushpattern

deleteobject(selectobject(gdi.hdc,createbrushindirect(&brush)))
if gdi.drawmode=dm_screenmemory then
    deleteobject(x:=selectobject(gdi.hdc2,createbrushindirect(&brush)))
fi
end

export func gxbrushcolour(w,?colour)=           

gdi:=w.gdi

if colour.isdef and colour<>gdi.brushcolour then
    gdi.brushcolour:=colour
    brush:=new(ws_logbrush)
    brush.lbstyle:=gdi.brushstyle
    brush.lbcolour:=colour
    brush.lbhatch:=gdi.brushpattern

    deleteobject(selectobject(gdi.hdc,createbrushindirect(&brush)))
    if gdi.drawmode=dm_screenmemory then
        deleteobject(selectobject(gdi.hdc2,createbrushindirect(&brush)))
    fi
fi
return gdi.brushcolour
end

export proc gxfillrect(w,x,y,width,height,?colour,mode=0)=      

gdi:=w.gdi
gdi.updated:=1

oldbrushcolour:=gdi.brushcolour
if colour.isdef then
    gxbrushcolour(w,colour)
fi

oldpenstyle:=gdi.penstyle
if mode=0 then      
    gxstyle(w,ps_null)
fi

if height<0 then y:=y+height+1; height:=-height fi
if width<0 then x:=x+width+1; width:=-width fi

if mode=0 then      
    rectangle(gdi.hdc,x, y,x+width+1,y+height+1)
    if gdi.drawmode=dm_screenmemory then
        rectangle(gdi.hdc2,x,y,x+width+1,y+height+1)
    fi
else            
    rectangle(gdi.hdc,x, y, x+width, y+height)
    if gdi.drawmode=dm_screenmemory then
        rectangle(gdi.hdc2,x,y,x+width,y+height)
    fi
fi
gxstyle(w,oldpenstyle)
gxbrushcolour(w,oldbrushcolour)
end

export func gxcolour(w,?colour)=        

gdi:=w.gdi

if colour.isdef and gdi.pencolour<>colour then
    gdi.pencolour:=colour
    gdi.xormode:=0
    deleteobject(selectobject(gdi.hdc,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
    setrop2(gdi.hdc,r2_copypen)
    if gdi.drawmode=dm_screenmemory then
        deleteobject(selectobject(gdi.hdc2,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
        setrop2(gdi.hdc2,r2_copypen)
    fi

fi

return gdi.pencolour
end

export proc gxsetpen(w,pen)=
gxcolour(w,getrgb(pen))
end

export proc gxline(w,x,y,?x2,?y2)=      

gdi:=w.gdi

if x2.isvoid then       
    x2:=x
    y2:=y

    movetoex(gdi.hdc,gdi.posx, gdi.posy)
    if gdi.drawmode=dm_screenmemory then
        movetoex(gdi.hdc2,gdi.posx, gdi.posy)
    fi
else
    movetoex(gdi.hdc,x, y)
    if gdi.drawmode=dm_screenmemory then
        movetoex(gdi.hdc2,x, y)
    fi
    gdi.posx:=x
    gdi.posy:=y
fi

lineto(gdi.hdc,x2,y2)

if gdi.drawmode=dm_screenmemory then
    lineto(gdi.hdc2,x2,y2)
fi
gdi.posx:=x2
gdi.posy:=y2
end

export func gxwidth(w,width)=
gdi:=w.gdi
if width.isvoid then
    return gdi.penwidth
fi

if gdi.penwidth<>width then
    gdi.penwidth:=width
    deleteobject(selectobject(gdi.hdc,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
    if gdi.drawmode=dm_screenmemory then
        deleteobject(selectobject(gdi.hdc2,createpen(gdi.penstyle,gdi.penwidth,gdi.pencolour)))
    fi

fi
return width
END

export proc gxlinerel(w,dx,dy)=     

gdi:=w.gdi
movetoex(gdi.hdc, gdi.posx, gdi.posy)
if gdi.drawmode=dm_screenmemory then
    movetoex(gdi.hdc2, gdi.posx, gdi.posy)
fi
x:=gdi.posx+dx
y:=gdi.posy+dy
gxline(w,x,y)
gdi.posx:=x
gdi.posy:=y
end

export proc gxmove(w,x2,y2)=        

gdi:=w.gdi

movetoex(gdi.hdc, x2, y2)
if gdi.drawmode=dm_screenmemory then
    movetoex(gdi.hdc2,x2, y2)
fi
gdi.posx:=x2
gdi.posy:=y2
end

export proc gxmoverel(w,dx,dy)=     
gdi:=w.gdi

gdi.posx+:=dx
gdi.posy+:=dy

movetoex(gdi.hdc,gdi.posx, gdi.posy)
if gdi.drawmode=dm_screenmemory then
    movetoex(gdi.hdc2,gdi.posx, gdi.posy)
fi
end

export proc gxrect(w,x,y,width,height)=     

gdi:=w.gdi
gdi.updated:=1
if height<0 then y:=y+height+1; height:=-height fi
if width<0 then x:=x+width+1; width:=-width fi

oldbrushstyle:=gdi.brushstyle
gxbrushstyle(w,bs_hollow)

rectangle(gdi.hdc,x, y, x+width, y+height)
if gdi.drawmode=dm_screenmemory then
    rectangle(gdi.hdc2,x, y, x+width,y+height)
fi
gxbrushstyle(w,oldbrushstyle)
end

export proc gxcircle(w,x,y,r)=      

gdi:=w.gdi
gdi.updated:=1
oldbrushstyle:=gdi.brushstyle
gxbrushstyle(w,bs_hollow)

ellipse(gdi.hdc,x-r, y-r, x+r-1, y+r-1)
if gdi.drawmode=dm_screenmemory then
    ellipse(gdi.hdc2,x-r, y-r, x+r-1, y+r-1)
fi
gxbrushstyle(w,oldbrushstyle)
end

export proc gxfillcircle(w,x,y,r,?colour,mode=0)=       
gdi:=w.gdi

gdi.updated:=1
oldbrushcolour:=gdi.brushcolour
if colour.isdef then
    gxbrushcolour(w,colour)
fi

oldpenstyle:=gdi.penstyle
if mode=0 then      
    gxstyle(w,ps_null)
fi

ellipse(gdi.hdc,x-r, y-r, x+r-1, y+r-1)
if gdi.drawmode=dm_screenmemory then
    ellipse(gdi.hdc2,x-r, y-r, x+r-1, y+r-1)
fi

gxstyle(w,oldpenstyle)
gxbrushcolour(w,oldbrushcolour)
end

export func gxpixel(w,x,y,?colour)=     
w.gdi.updated:=1

if colour.isvoid then       
    res:=getpixel(w.gdi.hdc, x, y)
    if w.gdi.drawmode=dm_screenmemory then
        getpixel(w.gdi.hdc2, x, y)
    fi
    return res
else
    setpixel(w.gdi.hdc,x, y, colour)
    if w.gdi.drawmode=dm_screenmemory then
        setpixel(w.gdi.hdc2,x,y,colour)
    fi
    return colour
fi
end

export func gxcaption(w,?caption)=      

case w.windclass
when window_class,popup_class then

    if caption.isdef then       
        setwindowtext(w.gdi.hwnd,caption)
        return caption
    else
        buffer:=new(array,byte,512)
        n:=getwindowtext(w.gdi.hwnd,int(&buffer),buffer.len)
        if n then
            s:=makestr(&buffer,n)       
                                        
        else
            s:=""
        fi
        return s
    fi
esac

if caption.isdef then       
    w.text:=caption
    gxdraw(w)
fi

return w.text
end

export proc gxtext(w,s,?x,?y)=      


return when s=""

gdi:=w.gdi

if x.isvoid then x:=gdi.posx fi
if y.isvoid then y:=gdi.posy fi
gdi.updated:=1

startpos::=lengths::=()
ngroups:=0

foreach i,c in s do
    if c<32 then
        ++ngroups
        startpos[ngroups]:=i
        lengths[ngroups]:=0
    else
        if ngroups and lengths[ngroups] then        
            ++lengths[ngroups]
        else                                        
            ++ngroups
            startpos[ngroups]:=i
            lengths[ngroups]:=1
        fi
    fi
od

for i,l in lengths do
    pos:=startpos[i]
    if l then
        slicex:=pos..pos+l-1
        textout(gdi.hdc,x, y,s.[slicex],l)

        if gdi.drawmode=dm_screenmemory then
            textout(gdi.hdc2,x,y,s.[slicex],l)
        fi
        x +:= gxtextwidth(w,s.[slicex])

    else                
        case s.[pos]
        when 13 then
            x:=0
        when 10 then
            y+:=20              

        when 9,16 then          
            currx:=x
            x:=0
            for t in tabstops do
                x+:=t*chx               
                if x>currx then exit fi
            od
            while x<=currx do x+:=chx*8 od

            if s.[pos]=16 and i<ngroups and lengths[i+1] then   
                pos:=startpos[i+1]
                x -:= gxtextwidth(w,s.[pos..pos+lengths[i+1]-1])+1
            fi

        esac    
    fi
od

gdi.posx:=x
gdi.posy:=y
end

export func gxtextcolour(w,?colour,?bgndcolour)=        

gdi:=w.gdi

if colour.isdef and colour<>w.style.textfgnd then
    w.style.textfgnd:=colour
    settextcolour(gdi.hdc,getrgb(colour))
    if gdi.drawmode=dm_screenmemory then
        settextcolour(gdi.hdc2,getrgb(colour))
    fi
fi
if bgndcolour.isdef and bgndcolour<>w.style.textbgnd then
    gxbgndcolour(w,bgndcolour)
fi

return w.style.textfgnd
end

export func gxtextwidth(font,?s)=       

if s="" then return 0 fi



if not font.isint then          
    font:=font.gdi.font
fi
if font=0 then font:=1 fi

selectobject(screendc,fonttable[font])
widthheight:=new(ws_point)


gettextextentpoint32(screendc,s,s.len,&widthheight)

return widthheight.x
end

export func gxloadfont(n,facename,?style,height=0,width=0)=     


if n<=0 then return 0 fi

if style.isvoid then style:="" fi

if n<=nglobalfonts and fonttable[n] then            
    igxremovefont(n)
fi

p:=style
bold:=400
italic:=0
underline:=0
strikeout:=0
for c in style do
    case asc(convuc(c))
    when 'B' then bold:=700
    when 'I' then italic:=1
    when 'U' then underline:=1
    when 'S' then strikeout:=1
    esac
od

hfont:=createfont(
    facename:   facename,
    height:     height,
    width:      width,
    bold:       bold,
    italic:     italic,
    underline:  underline,
    charset:    0,
    quality:    2,
    escapement: 0,
    orientation:0)

if hfont=0 then
    hfont:=getstockobject(system_font)
fi

fonttable[n]:=hfont
nglobalfonts:=max(n,nglobalfonts)

selectobject(screendc,fonttable[n])

tm:=new(ws_textmetrics)

gettextmetrics(screendc,&tm)

fontdimtable[n]::=ws_point(tm.avecharwidth, tm.height+tm.externalleading)
fontvdimtable[n]::=ws_point(tm.ascent, tm.descent)

selectobject(screendc,getstockobject(system_font))

return n
end

proc igxremovefont(n)=

unless n in 1..nglobalfonts then return end
if fonttable[n]=0 then return fi    


deleteobject(fonttable[n])      
fonttable[n]:=0
end

export func gxfont(w,font=1)=       

if not w then w:=wapplic fi
if not w then w:=wscreen fi
gdi:=w.gdi

if font.isdef and font<>gdi.font then
    if font not in 1..nglobalfonts then
        abort("Bad font number "+tostr(font))
    fi
    gdi.font:=font
    if fonttable[font]=0 then
        abort("Font not in use "+tostr(font))
    fi

    oldhfont:=selectobject(gdi.hdc,fonttable[font])
    sendmessage(gdi.hwnd,wm_setfont,fonttable[font],0)

    if gdi.drawmode=dm_screenmemory then
        oldhfont:=selectobject(gdi.hdc2,fonttable[font])
        sendmessage(gdi.hwnd2,wm_setfont,fonttable[font],0)
    fi
    if fontdimtable[font].x=0 then      
        gxchardim(font,0)
    fi
fi
return gdi.font
end

func hascontrolchars(s)=        

foreach c in s do
    if c<32 then return 1 fi
od
return 0
end

export func gxchardim(font,vert=0)=         


if not font.isint then
    font:=font.gdi.font
fi
if font=0 then font:=1 fi

if fontdimtable[font].x=0 then      
    selectobject(screendc,fonttable[font])
    tm:=new(ws_textmetrics)
    gettextmetrics(screendc,&tm)

    fontdimtable[font]::=ws_point(tm.avecharwidth, tm.height+tm.externalleading)

    fontvdimtable[font]::=ws_point(tm.ascent, fontvdimtable[font].y:=tm.descent)

    selectobject(screendc,getstockobject(ansi_var_font))
fi

if vert then
    return fontvdimtable[font]
fi

return fontdimtable[font]
END

export func gxbgndcolour(w,?colour)=        
gdi:=w.gdi

if colour.isdef then

    if colour<>w.style.textbgnd then
        w.style.textbgnd:=colour
        setbkcolour(gdi.hdc,getrgb(colour))
        if gdi.drawmode=dm_screenmemory then
            setbkcolour(gdi.hdc2,getrgb(colour))
        fi
    fi
    gxbgndmode(w,(colour<>w.style.windbgnd|1|0))
fi
return w.style.textbgnd
end

export func gxbgndmode(w,?mode)=        

gdi:=w.gdi

if mode.isdef  then
    case mode
    when 1,'y','Y','T' then
        w.style.bgndmode:=opaque
    else
        w.style.bgndmode:=transparent
    esac

    setbkmode(gdi.hdc,mode+1)
    if gdi.drawmode=dm_screenmemory then
        setbkmode(gdi.hdc2,mode+1)
    fi
fi
return w.style.bgndmode
end

export proc gxhighlight(w,x,y,width,height)=        
const dstinvert=0x00550009  
gdi:=w.gdi

gdi.updated:=1
patblt(gdi.hdc, x, y, width,height,dstinvert)
if gdi.drawmode=dm_screenmemory then
    patblt(gdi.hdc2, x,y, width,height,dstinvert)
fi
end

export proc gxbitblt(w,x2,y2,width,height,x,y)=         
gdi:=w.gdi
gdi.updated:=1
bitblt(gdi.hdc, x2, y2, width,height,
            gdi.hdc,x,y,srccopy)

if gdi.drawmode=dm_screenmemory then
    bitblt(gdi.hdc2,x2,y2,width,height,gdi.hdc2,x,y,srccopy)
fi
end

export func gxaskmess(mode=0)=

repeat
    if mxwait_mm_message()=0 then
        return 0
    fi
    if quitmess then return 0 fi
    x:=process_message(currmess)

    if currmess.message=mm_key and currmess.a=27 then
        return 0
    fi

until x=thismess                    

if mode=1 and currmess.message=mm_command then
    return currmess.a
fi

return currmess.message
end

func process_message(mess)=

if mess.wind=nil then
    return thismess
fi

case mess.message
when mm_close then
    return thismess
esac

status:=domessage(mess)

return status
end

export proc docs=

end

func newwindow(hwnd,index,windclass,borderstyle)=

w:=new(rwindow,0)
w.windclass:=windclass
w.index:=index
w.childlist::=()
w.owner:=nil

addwindow(w)
wx_setw(hwnd,w.gindex)
return w
end

export func getrgb(index)=
if index=0 then return 0 fi
return colourvalues[index]
end

func readstyle(owner,windclass,options)=

if options.usertype=stylerec then           
    return options
fi

ss:=new(stylerec)
if options.isvoid then              
    d::=defstyle
    d.border:=defaultborderstyles[windclass]
    return d
fi

ss.border   :=options{ss_border,defaultborderstyles[windclass]}
ss.justify  :=options{ss_justify,defstyle.justify}
ss.vjustify :=options{ss_vjustify,defstyle.vjustify}
ss.textfgnd :=options{ss_textfgnd,defstyle.textfgnd}
ss.textbgnd :=options{ss_textbgnd,defstyle.textbgnd}
ss.bgndmode :=options{ss_bgndmode,defstyle.bgndmode}

ss.iframe   :=options{ss_iframe,0}

ss.hilitetype   :=options{ss_hilitetype,defstyle.hilitetype}
ss.marktype :=options{ss_marktype,(ss.hilitetype|0|defstyle.marktype)}
ss.imark    :=options{ss_imark,defstyle.imark}

if windclass in [toggle_class, select_class,mark_class] and ss.marktype then
    def:=owner.style.windbgnd
else
    def:=defstyle.windbgnd
fi

ss.windbgnd :=options{ss_windbgnd,def}
ss.hscroll  :=options{ss_hscroll,0}
ss.vscroll  :=options{ss_vscroll,0}
ss.lbchange :=options{ss_lbchange,0}
ss.returnmess   :=options{ss_returnmess,0}
ss.noupdate :=options{ss_noupdate,0}

return ss
end

export func gxpanel(owner,pos,dim,?style)=
ss:=readstyle(owner,panel_class,style)

w:=gxcontrol(owner,panel_class,pos,dim,ss)

gxdraw(w)

return w
end

export func gxstatusbar(owner,pos,dim,?style)=

ss:=readstyle(owner,statusbar_class,style)

if ss.iframe=0 then                 
    bs:=ss.border
    if bscat[bs]<>'I' then          
        dim+:=bswidths[bs].y1+bswidths[bs].y2
    fi
    ss.iframe:=1                    
fi

(ecapos,ecadim):=gxclientarea(owner)

if pos.isint then pos:=chr(pos) fi
if convuc(pos) in "T TOP" then          
    pos:=ecapos
    dir:='T'
else                                    
    pos:=(ecapos[1],ecadim[2]-dim+ecapos[2])
    dir:='B'
fi
dim:=(ecadim[1],dim)

ss.dir:=dir

w:=gxcontrol(owner,statusbar_class,pos,dim,ss)

gxdraw(w)

return w
end

export func gxframebar(owner,pos,dim,?style)=

ss:=readstyle(owner,framebar_class,style)
if ss.iframe=0 then                 
    bs:=ss.border
    if bscat[bs]<>'I' then          
        dim+:=bswidths[bs].y1+bswidths[bs].y2
    fi
    ss.iframe:=1                    
fi

(ecapos,ecadim):=gxclientarea(owner)

if pos.ispointer then pos:=chr(pos) fi
if convuc(pos) in "L LEFT" then         
    pos:=ecapos
    dir:='L'
else                                    
    pos:=(ecadim[1]-dim+ecapos[1],ecapos[2])
    dir:='R'
fi
dim:=(dim,ecadim[2])

ss.dir:=dir

w:=gxcontrol(owner, framebar_class,pos,dim,ss)

gxdraw(w)

return w
end

export func gxbutton(owner,pos,dim,caption,?style,id=201,enable=1)=

ss:=readstyle(owner,button_class,style)

w:=gxcontrol(owner,button_class,pos,dim,ss)
w.id:=id

w.text:=caption
w.enable:=enable
gxdraw(w)

return w
end

export func gxlabel(owner,pos,dim,caption,?style)=

ss:=readstyle(owner,label_class,style)

w:=gxcontrol(owner,label_class,pos,dim,ss)

w.text:=caption
gxdraw(w)

return w
end

export func gxgroup(owner,pos,dim,?style)=

ss:=readstyle(owner,group_class,style)

w:=gxcontrol(owner,group_class,pos,dim,ss)

gxdraw(w)

return w
end

func gxcontrol(owner,windclass=button_class,pos,dim,?ss)=

if ss.type=dict or ss.isvoid then
    ss:=readstyle(owner,windclass,ss)
fi
wb:=wbs_none
case ss.border          
when bs_simplew then
    wb:=wbs_simple
esac

if ss.iframe and bscat[ss.border]<>'I' then
    widths:=bswidths[ss.border]
    pos[1]+:=widths.x1
    pos[2]+:=widths.y1
    dim[1]-:=widths.x1+widths.x2
    dim[2]-:=widths.y1+widths.y2
FI

hwnd:=wx_createcontrol(pos:pos,dim:dim,border:wb,owner:owner.gdi.hwnd)

if hwnd=0 then
    abort("Can't create control window")
fi

w:=newwindow(hwnd,0,no_class,ss.border)
w.windclass:=windclass
w.style:=ss
w.owner:=owner
w.enable:=1

setwindowdims_c(w,hwnd)
setupgdi(w,hwnd)

gxdrawmode(w,dm_screenmemory)

gxfont(w,labelfont)

gxtextcolour(w,w.style.textfgnd,w.style.textbgnd)

gxbgndmode(w,w.style.bgndmode)

w.owner.childlist append:=w
w.index:=w.owner.childlist.upb

return w
end

export func gxtoggle(owner,pos,dim,caption="",linkvar,?style,id=201,enable=1)=

(posx,posy):=pos
(dimx,dimy):=dim
textoffset:=0


ss:=readstyle(owner,toggle_class,style)


if ss.marktype then
    if ss.imark=0 then          
        posx-:=markdim
        dimx+:=markdim
        textoffset:=markdim
    fi
fi

w:=gxcontrol(owner,toggle_class,(posx,posy),(dimx,dimy),ss)
w.linkvar:=linkvar
w.id:=id
w.text:=caption
w.attrs:=togglerec(textoffset,1)
w.enable:=enable

if w.style.marktype then
    gxmark(owner:w,pos:(0,(w.dimy-markdim)%2),id:id, style:ss)
fi

gxdraw(w)
return w
end

export func gxselect(owner,pos,dim,caption="",linkvar,onvalue,?style,id=201,enable=1)=

(posx,posy):=pos
(dimx,dimy):=dim
textoffset:=0

ss:=readstyle(owner,select_class,style)

if ss.marktype and ss.imark=0 then          
    posx-:=markdim
    dimx+:=markdim
    textoffset:=markdim
fi

w:=gxcontrol(owner,select_class,(posx,posy),(dimx,dimy),ss)

w.linkvar:=linkvar
w.id:=id
w.text:=caption
w.attrs:=togglerec(textoffset,onvalue)
w.enable:=enable
if w.style.marktype then
    gxmark(owner:w,pos:(0,(w.dimy-markdim)%2),id:id, style:style)
fi

gxdraw(w)
return w
end

export proc showmessage(mess)=
RETURN
CPL MESS.MESSAGE
cp "Message:",leftstr(messagenames[mess.message],20)
cp "A:",,mess.a,"B:",,mess.b
cp " (X:",,mess.x,"Y:",,mess.y,,") Buttons:",mess.state:"b"

cpl "   Window:",mess.wind.name
end

func domessage(mess)=

m:=mess.message
w:=mess.wind

IF W.GDI=0 THEN PCERROR("DOM/GDI=0") FI

case m
when mm_move,mm_setcursor then
    return skipmess
esac



x:=messhandlertable[m,w.windclass](mess,w)
return x
end

proc init_handlertables=

    messhandlertable:=maketable(mm_null..mm_last, no_class..dummy_class, nil)
    actionhandlertable:=maketable(actionnames.bounds, no_class..dummy_class, nil)

    messalltable:=new(list,mm_null..mm_last,0)      
    fnallall:=nil                                   
    fnfixups:=nil

    actionalltable:=new(list,actionnames.bounds,0)
    allprocs:=$procsymbols()

    forall d in allprocs do
        fnptr:=d
        fnname:=$symbolname(d)
        (name,messname,windname):=splitstring(fnname,"_")           

        if fnname="gxhandler_fixups" then
            fnfixups:=fnptr
        elsif leftstr(fnname,5)="mess_" then
            if messname="all" and windname="all" then
                fnallall:=fnptr
            else
                message:=("mm_"+messname) in messagenames
                if not message then
                    ABORT("CAN'T FIND MESSAGE "+messname)
                fi

                if windname="all" then              
                    messalltable[message]:=fnptr
                else
                    messhandlertable[message,WX:=findwindclass(windname)]:=fnptr
                fi
            fi

        elsif leftstr(fnname,8)="do_draw_" or leftstr(fnname,10)="do_update_" then
            action:=messname+"_w" in actionnames
            if not action then
                ABORT("CAN'T FIND ACTION "+MESSNAME)
            fi
            if windname="all" then
                actionalltable[action]:=fnptr
            else
                windclass:=findwindclass(windname)
                actionhandlertable[action,windclass]:=fnptr
            fi
        fi
    od

    if fnfixups then
        fnfixups()
    fi

    for mx:=0 to mm_last do
        for wx:=0 to dummy_class do
            if not messhandlertable[mx,wx] then
                messhandlertable[mx,wx]:=(messalltable[mx]|messalltable[mx]|fnallall)
            fi
        od
    od


    if not fnallall then
        pcerror("Can't find all/all mess handler")
    fi

    for ax:=1 to DRAW_w do
        for wx:=0 to dummy_class do
            if not actionhandlertable[ax,wx] then
                if not actionalltable then
                    pcerror("No DO/ALL handler for:"+actionnames[ax])
                fi
                actionhandlertable[ax,wx]:=actionalltable[ax]
            fi
        od
    od


end

func findwindclass(name)=
    windclass:=name+"_class" in windowclassnames
    if windclass=0 then
        ABORT("CAN'T FIND WINDOW "+windname)
    fi
    return windclass
end

export proc gxdraw(w)=
fnptr:=actionhandlertable[draw_w,w.windclass]

if fnptr then
    fnptr(w)
else
    cpl "NO DRAW HANDLER",windowclassnames[w.windclass],w.name
    waitkey()
    stop
fi
end

export proc gxupdate(w)=
fnptr:=actionhandlertable[update_w,w.windclass]
if fnptr then
    fnptr(w)
else
    gxdraw(w)
fi
end

export proc eventloop=
do
    m:=gxaskmess()

    SHOWMESSAGE(CURRMESS)

    case m
    when 0,mm_cancel then
        return
    esac

od
end

export func gxeditbox(owner,pos,dim,linkvar,?style,id=201,enable=1)=

ss:=readstyle(owner,editbox_class,style)

w:=gxcontrol(owner,editbox_class,pos,dim,ss)

w.linkvar:=linkvar
w.id:=id
w.attrs:=new(editboxrec)
w.attrs.currpos:=linkvar^.len+1
w.enable:=enable
gxdraw(w)
return w
end

export proc gxebchange(w,?linkvar,charpos=-1)=

if linkvar.isdef then

    w.linkvar:=linkvar
fi

if charpos=-1 then
    w.attrs.currpos:=w.linkvar^.len+1
else
    w.attrs.currpos:=charpos
fi
gxupdate(w)
end

export proc gxsetlbdata(w,linkvar,?pos)=
w.linkvar:=linkvar
if pos.isvoid then
    pos:=(linkvar^|1|0)
fi
w.attrs.currpos:=pos

if w.childlist[1] then
    gxsetscrolllimits(ws,getlvbounds(linkvar),w.attrs.rows)
    gxscrollpos(ws,pos)
fi
end

export proc gxsetlbpos(w,pos)=
w.attrs.currpos:=pos

if pos then
    oldpagepos:=w.attrs.pagepos
    if pos<oldpagepos then
        w.attrs.pagepos:=pos
    elsif pos>oldpagepos+w.attrs.rows-1 then
        w.attrs.pagepos:=pos-w.attrs.rows+1
    fi
    if w.attrs.pagepos<>oldpagepos then
        if w.childlist then
            gxscrollpos(w.childlist[1],w.attrs.pagepos)
        fi
        m:=mm_draw
    else
        m:=mm_update
    fi
else
    m:=mm_draw
fi

postmess(w,m)
if w.style.lbchange then
    postmess(w,mm_lbchange,w.attrs.currpos)
fi
end

export proc gxsetlbpage(w,pagepos)=
w.attrs.pagepos:=pagepos

oldpos:=w.attrs.currpos
if oldpos<pagepos then
    w.attrs.currpos:=pagepos
elsif oldpos>=pagepos+w.attrs.rows then
    w.attrs.currpos:=pagepos+w.attrs.rows-1
fi

if w.childlist then
    gxscrollpos(w.childlist[1],pagepos)
fi

postmess(w,mm_draw)
if w.style.lbchange and oldpos<>w.attrs.currpos then
    postmess(w,mm_lbchange,w.attrs.currpos)
fi
end

export func gxlistbox(owner,pos,dim,linkvar,?style,id=201,rows=0,pitch=0,offset=0)=

ss:=readstyle(owner,listbox_class,style)

(dimx,dimy):=dim
if ss_vscroll and ss_imark=0 then           
    dimx+:=arrowdim
fi

w:=gxcontrol(owner,listbox_class,pos,(dimx,dimy),ss)
w.linkvar:=linkvar
w.id:=id
w.attrs:=new(listboxrec)

if pitch=0 then                             
    pitch:=listrowheight
    offset:=0
    rows:=w.dimy%pitch
fi
w.attrs.rows:=rows
w.attrs.pitch:=pitch
w.attrs.offset:=offset

w.attrs.pagepos:=1
w.attrs.currpos:=(getlvbounds(linkvar).len|1|0)

if w.style.vscroll then
    ws:=gxvertscrollbar(owner:w,pos:(w.dimx-arrowdim,0),dim:w.dimy,id:id,style:style)
    gxsetscrolllimits(ws,getlvbounds(linkvar),w.attrs.rows)

    gxscrollpos(ws,getlvbounds(linkvar).lwb)
fi

gxdraw(w)
return w
end

export func gxarrow(owner,pos,?dim,dir,?style,id=201)=

ss:=readstyle(owner,arrow_class,style)
if dim.isvoid then
    dim:=(arrowdim,arrowdim)
fi

w:=gxcontrol(owner,arrow_class,pos,dim,ss)
w.id:=id
if dir.isstring then dir:=asc(dir) fi
case dir                    
when 'N' then dir:='U'
when 'E' then dir:='R'
when 'S' then dir:='D'
when 'W' then dir:='L'
esac

w.style.dir:=dir            
gxdraw(w)

return w
end

export proc gxsetscrolllimits(w,limits,span=0)=

w.attrs.span:=span
if w.style.dir='H' then
    width:=w.dimx
else
    width:=w.dimy
fi
m:=width-arrowdim*2             

if span=0 then                      
    w.attrs.limits:=limits
    w.attrs.currpos:=limits.lwb
    w.attrs.thumbsize:=arrowdim
    enable:=limits.len>1
    w.attrs.thumbsize:=arrowdim*enable
else
    if limits.isrange then
        length:=limits.len
    else
        length:=limits
    fi
    if length<=span then
        enable:=0
        w.attrs.limits:=1..1
        w.attrs.thumbsize:=0
    else
        w.attrs.limits:=1..length-span+1
        enable:=1
        w.attrs.thumbsize:=max(10,int(m*(span/length)))
    fi
fi

w.attrs.currpos:=w.attrs.limits.lwb
w.enable:=enable

w.attrs.thumbspan:=m-w.attrs.thumbsize      
w.attrs.thumbpos:=arrowdim
postmess(w,mm_draw)
end

export func gxscrollpos(w,pos,u=0)=
if pos.isvoid then
    return w.attrs.currpos
fi

w.attrs.currpos:=pos
if pos not in w.attrs.limits then
    pcerror("Bad scroll pos")
fi

tpos:=int(w.attrs.thumbspan*((pos-w.attrs.limits.lwb)/(w.attrs.limits.len-1)))
w.attrs.thumbpos:=arrowdim+tpos

w.childlist[1].enable:=pos>w.attrs.limits.lwb
w.childlist[2].enable:=pos<w.attrs.limits.upb

if u then
    postmess(w,mm_update)
fi
return 0
end

export func gxhozscrollbar(owner,pos,dim,?style,id=201)=

ss:=readstyle(owner,scrollbar_class,style)
width:=arrowdim
if dim.isint then
    dim:=(dim,width)
else
    width:=dim[1]
fi

w:=gxcontrol(owner,scrollbar_class,pos,dim,ss)
w.id:=id
w.style.dir:='H'

w.attrs:=new(scrollbarrec)
w.flags.[wa_leftdrag]:=1

wa:=gxarrow(owner:w, pos:(0,0), dim:(width,width),dir:'L')
wb:=gxarrow(owner:w, pos:(dim[1]-width,0), dim:(width,width),dir:'R')

gxsetscrolllimits(w,1..200,20)
gxscrollpos(w,1)

gxdraw(w)

return w
end

export func gxvertscrollbar(owner,pos,dim,?style,id=201)=
ss:=readstyle(owner,scrollbar_class,style)
width:=arrowdim
if dim.isint then
    dim:=(width,dim)
else
    width:=dim[2]
fi

w:=gxcontrol(owner,scrollbar_class,pos,dim,ss)
w.id:=id
w.style.dir:='V'

w.attrs:=new(scrollbarrec)
w.flags.[wa_leftdrag]:=1

wa:=gxarrow(owner:w, pos:(0,0), dim:(width,width),dir:'U')
wb:=gxarrow(owner:w, pos:(0,dim[2]-width), dim:(width,width),dir:'D')
gxsetscrolllimits(w,100..200,2)

gxscrollpos(w,100)

gxdraw(w)

return w
end

export func gxmark(owner,pos,?dim,?style,id=201)=

ss:=readstyle(owner,mark_class,style)
if dim.isvoid then
    dim:=(markdim,markdim)
fi

w:=gxcontrol(owner,mark_class,pos,dim,style)
w.id:=id
gxdraw(w)

return w
end

export proc gxfocus(w)=
if wfocus==w then
    return
fi

if wfocus then
    domessage(makemess(wfocus,mm_killfocus))
fi
caretdrawn:=0
domessage(makemess(w,mm_setfocus))
end

export proc gxkillfocus=
if wfocus then
    drawcaret(0)
fi
wfocus:=nil
end

export func gxcopy(w,?bm,x=0,y=0,scalex=1.0,scaley=0,sx=0,sy=0,dimx=0,dimy=0)=      

if bm.isvoid then
    bm:=w
    w:=nil
fi
if bm.isvoid then
    return nil
fi

if dimx=0 then dimx:=bm.dimx-sx fi
if dimy=0 then dimy:=bm.dimy-sy fi

if scalex=0 then scalex:=1.0 fi
if scaley=0 then scaley:=scalex fi

if w=nil then       
    w:=gxcreatewindow(caption:"Bitmap "+tostr(bm.pixelbits)+" bit",pos:(500,500),
            dim:(bm.dimx*scalex,bm.dimy*scaley))
CPL "*******SETDM2",DM_SCREENMEMORY
    w.gdi.drawmode:=dm_screenmemory         
fi

gdi:=w.gdi
gdi.updated:=1

mode:=copymode

setstretchbltmode(gdi.hdc,mode)
stretchblt(gdi.hdc, x, y,int(dimx*scalex),int(dimy*scaley),
                                            bm.gdi.hdc,sx,sy,dimx,dimy, srccopy)
if gdi.drawmode=dm_screenmemory then
    setstretchbltmode(gdi.hdc2,mode)
    stretchblt(gdi.hdc2,x,y,int(dimx*scalex),int(dimy*scaley),
                                            bm.gdi.hdc,sx,sy,dimx,dimy, srccopy)
fi
return w
end

export proc gxrestore(w,?r)=

if r.isvoid then
    x1:=y1:=0


    width:=w.dimx
    height:=w.dimy
else
    x1:=r.x1
    y1:=r.x2
    width:=r.x2-x1+1
    height:=r.y2-y1+1
fi

case w.gdi.drawmode
when dm_screen then         
    gxdraw(w)
when dm_screenmemory then
    destdc:=w.gdi.hdc
    sourcedc:=w.gdi.hdc2
when dm_memoryscreen then
    destdc:=w.gdi.hdc2
    sourcedc:=w.gdi.hdc
else
    abort("gxrest/?")
esac

bitblt(destdc,x1,y1, width,height, sourcedc, x1,y1, srccopy)

end

export func gxdrawmode(w,?drawmode)=


olddrawmode:=w.gdi.drawmode
if drawmode.isvoid then
    return olddrawmode
fi

if olddrawmode=drawmode then        
    return drawmode
elsif olddrawmode<>dm_screen then   
    abort("gxdrawmode2")            
fi

memhwnd:=createcompatiblebitmap(screendc,w.dimx,w.dimy)
memhdc:=createcompatibledc(nil)
selectobject(memhdc,memhwnd)

case drawmode
when dm_screenmemory then
    w.gdi.hwnd2:=memhwnd
    w.gdi.hdc2:=memhdc
when dm_memoryscreen then
    w.gdi.hwnd2:=w.gdi.hwnd         
    w.gdi.hdc2:=w.gdi.hdc
    w.gdi.hwnd:=memhwnd
    w.gdi.hdc:=memhdc
else
    abort("gxdrawmode?")
esac

w.gdi.drawmode:=drawmode
return drawmode
end

export proc switchdest(w)=
gdi:=w.gdi

case gdi.drawmode
when dm_screenmemory then
    t:=gdi.hwnd; gdi.hwnd:=gdi.hwnd2; gdi.hwnd2:=t
    t:=gdi.hdc; gdi.hdc:=gdi.hdc2; gdi.hdc2:=t
    gdi.drawmode:=dm_memory
when dm_memory then
    t:=gdi.hwnd; gdi.hwnd:=gdi.hwnd2; gdi.hwnd2:=t
    t:=gdi.hdc; gdi.hdc:=gdi.hdc2; gdi.hdc2:=t
    gdi.drawmode:=dm_screenmemory
esac
end

export proc gxclose(w)=

case w.windclass
when bitmap_class then
else
    if issubwindow(w,wfocus) then
        wfocus:=nil
    fi

    if issubwindow(w,wmouse) then   
        lastmousewindow:=nil
        wmouse:=nil
    fi

    destroywindow(w.gdi.hwnd)
    gxfreewindow(w)
esac
end

proc gxfreewindow(w)=
for wc in w.childlist do
    gxfreewindow(wc)
od

removewindow(w)

w.gdi:=0
w:=0
end

export func gxmsgbox(message,caption="",options="")=

const mb_abortretryignore   = 0x02
const mb_applmodal          = 0x00
const mb_defbutton1         = 0x00
const mb_defbutton2         = 100
const mb_defbutton3         = 200
const mb_defbutton4         = 300
const mb_help               = 4000
const mb_iconasterisk       = 40
const mb_iconerror          = 10
const mb_iconexclamation    = 30
const mb_iconhand           = mb_iconerror
const mb_iconinformation    = mb_iconasterisk
const mb_iconquestion       = 20
const mb_iconstop           = mb_iconhand
const mb_iconwarning        = mb_iconexclamation
const mb_ok                 = 0x00
const mb_okcancel           = 0x01
const mb_retrycancel        = 0x05
const mb_right              = 80000
const mb_setforeground      = 10000
const mb_systemmodal        = 1000
const mb_taskmodal          = 2000
const mb_yesno              = 0x04
const mb_yesnocancel        = 0x03
const mb_topmost            = 0x040000

const idfail    = 0
const idok      = 1
const idcancel  = 2
const idabort   = 3
const idretry   = 4
const idignore  = 5
const idyes     = 6
const idno      = 7

static var rettable=(0:"fail","ok","cancel","abort","retry","ignore","yes","no",
        "","","tryagain","continue")

static var styletable=(
("bari",mb_abortretryignore),
("bo",mb_ok),
("boc",mb_okcancel),
("brc",mb_retrycancel),
("byn",mb_yesno),
("bync",mb_yesnocancel),
("ix",mb_iconexclamation),
("iw",mb_iconwarning),
("ii",mb_iconinformation),
("iq",mb_iconquestion),
("is",mb_iconstop),
("ie",mb_iconerror),
("ih",mb_iconhand),
("d1",mb_defbutton1),
("d2",mb_defbutton2),
("d3",mb_defbutton3),
("d4",mb_defbutton4),
("h",mb_help),
("rj",mb_right),
("sm",mb_systemmodal))

hwnd:=nil

style:=0
optioncodes:=splitstring(options," ")

for opt in optioncodes do
    for i to styletable.len do
        if styletable[i,1]=opt then style ior:=styletable[i,2] fi
    od
od

style ior:=0x10000


x:=messageboxa(hwnd,message,caption,style)
return rettable[x]
END


export proc gxhandler(windclass,mess,fnptr)=

if not windclass.ispointer then
    windclass:=windclass.windclass
fi

messhandlertable[mess,windclass]:=fnptr
end

export func gxaskfile(caption="File",filespec="*.*",deffile="",startdir="")=

save:=0
if caption='*' then
    save:=1
    caption:=rightstr(caption,-1)
fi

filters:=array(filespec+"@@@")      

for i,bb in filters do          
    if bb='@' then filters[i]:=0 fi
od

ofn:=new((iswin32|ws_openfilename32|ws_openfilename64))

ofn.structsize:=ofn.bytes
ofn.owner:=wapplic.gdi.hwnd
ofn.instance:=getmodulehandle(0)
ofn.filter:=int(&filters)
ofn.flags:=ofn_explorer ior ofn_nochangedir ior ofn_hidereadonly 

ofn.initialdir:=getstringz(startdir)

ofn.defext:=getstringz("")

result:=new(array,byte,300)

result[1]:=0
if deffile<>"" then
    memcpy(&result,&deffile,deffile.len)
fi

ofn.file:=int(&result)

ofn.maxfile:=256
ofn.title:=getstringz(caption)

if not (not save | getopenfilenamea(&ofn) | getsavefilenamea(&ofn)) then
    result[1]:=0        
fi

return string(result)
END

export func gxcurrpos(w)=
return w.attrs.currpos
end

export func gxtabstops(?tabs,signed=0)=
if tabs.isdef then
    tabstops::=tabs
    if signed then
        for i,x in tabstops do
            tabstops[i]:=abs(x)
        od
    fi
fi
return tabstops

end

export func getlvbounds(linkvar)=
if linkvar.ispointer and linkvar^.islist then
    return linkvar^.bounds
else
    return linkvar.getbounds()
fi
return 0
end

export func getlvitem(linkvar,n)=
if linkvar.ispointer and linkvar^.islist then
    return linkvar^[n]
else
    PCERROR("GETLVITEM")
fi
return 0
end

export func getlvstritem(linkvar,n)=
if linkvar.ispointer and linkvar^.islist then
    return tostr(linkvar^[n])
else
    return linkvar.getstritem(n)
fi
return 0
end

export proc gxtext16(w,s,n,x=0,y=0)=        
    gdi:=w.gdi

    textoutw(gdi.hdc,x, y,&s,n)
    if gdi.drawmode=dm_screenmemory then
        textoutw(gdi.hdc2,x,y,&s,n)
    fi
end

export func gxenable(w,flag)=
if flag.isdef then
    w.enable:=flag
    gxupdate(w)
fi
return w.enable
end

export func gxclientarea(w)=

aposx:=aposy:=0

adimx:=w.dimx
adimy:=w.dimy

centx:=(aposx+adimx)%2
centy:=(aposy+adimy)%2

for cw in w.childlist do

    (posx,posy):=(cw.frameposx,cw.frameposy)
    (dimx,dimy):=(cw.framedimx,cw.framedimy)

    case cw.style.dir
    when 'B' then               
        if posy<(aposy+adimy) then
            adimy-:=dimy
        fi

    when 'T' then               
        if (posy+dimy)>aposy then       
            aposy+:=(posy+dimy)
            adimy-:=(posy+dimy)
        fi

    when 'R' then               
        if posx<(aposx+adimx) then
            adimx-:=dimx
        fi

    when 'L' then               
        if (posx+dimx)>aposx then       
            aposx+:=(posx+dimx)
            adimx-:=(posx+dimx)
        fi
    else

        if dimx>dimy then           
            if posy>centy then          
                if posy<(aposy+adimy) then
                    adimy-:=dimy
                fi

            else                    
                if (posy+dimy)>aposy then       
                    aposy+:=(posy+dimy)
                    adimy-:=(posy+dimy)
                fi
            fi
        else                    
            if posx>centx then          

                if posx<(aposx+adimx) then
                    adimx-:=dimx
                fi

            else                    

                if (posx+dimx)>aposx then       
                    aposx+:=(posx+dimx)
                    adimx-:=(posx+dimx)
                fi

            fi
        fi
    esac
od

return ((aposx,aposy), (adimx,adimy))
END

export func addwindow(w)=
n:=nil in allwindows
if not n then
    n:=allwindows.len+1
fi

allwindows[n]:=w
w.gindex:=n
return n
end

export proc removewindow(w)=
n:=w in allwindows
if n then
    allwindows[n]:=nil
fi
end

func get_function_name(fnptr)=
n:=$pcldata('PROC',0)
for i:=1 to n do
    data:=$pcldata('PROC',i)
    if fnptr=data[4] then
        return data[1]
    fi
od
return "NOT FOUND "+tostr(fnptr)
end

func process_wmmessage(msg)=
    x:=process_wmmessage2(msg)
    return x

end

func process_wmmessage2(msg)=



hwnd:=msg.hwnd
w:=getwindow(hwnd)

message:=msg.message
wparam:=msg.wparam
lparam:=msg.lparam

case msg.message
when wm_command then
    w:=getwindow(lparam)            
    i:=wparam iand 0xffff           
    j:=wparam>>16               
    m:=mm_command

    if not w then
        w:=wapplic
    fi

    postmess(w,m,i,j,0)

    return 0

when wm_activate then
    if wparam then              
    fi

when wm_syskeydown,wm_syskeyup,wm_keydown,wm_keyup then
    if dokeymessage(hwnd,message,wparam,lparam) then
        return 0
    fi

when wm_char then
    postmess((wfocus|wfocus|w),mm_char,wparam,lparam,0)

when wm_close then
    if w==wapplic then
        postmess(w,mm_close,0,0,0)
        return 0
    else
        postmess(w,mm_cancel,0,0,0)
        return 0
    fi

when wm_timer then
    if not background and not stationary then       
        if gettickcount()-lastxytime>pausetime then
            stationary:=1
        fi
    fi

when wm_destroy then
    if w and wapplic and w==wapplic then
        killtimer(hwnd,1)
        postquitmessage(0)          
        return 0
    else
        return 1
    fi


when wm_mousemove then

    buttonstate:=wparam iand (kb_lbutton ior kb_rbutton ior kb_mbutton)
    mousepos.x:=lparam iand 65535
    mousepos.y:=lparam>>16

domousemove:
    xyvalid:=1              
    setnewmousewindow(w)

    wmouse:=w
    postmess(wmouse,mm_move)

    lastxy::=getscreencoords(wmouse,mousepos)
    lastxytime:=gettickcount()
    stationary:=0


    if buttonstate<>0 and lastmousewindow<>nil then     
        pt:=getscreencoords(lastmousewindow,lastmousepos)
        dx:=lastxy.x-pt.x
        dy:=lastxy.y-pt.y

        if dragmode then        
            postmess(lastmousewindow,mm_drag,dx,dy,-1)          

        else                
            if ((mousesw=1 and lastmousewindow.flags.[wa_leftdrag]<>0) or \
                            (mousesw=2 and lastmousewindow.flags.[wa_rightdrag]<>0) or \
                            (mousesw=3 and lastmousewindow.flags.[wa_middledrag]<>0)) and \
                        (abs(dx)>dragtol or abs(dy)>dragtol) then
                dragmode:=mousesw
                postmess(lastmousewindow,mm_startdrag,dx,dy,-1)     
            fi

        fi
    else
        if dragmode then
            postmess(lastmousewindow,mm_enddrag,dx,dy,-1)   
            dragmode:=0
        fi
    fi

    return 0

when wm_enteridle then      
    idlemode:=1
    return 0

when wm_paint then

    if w<>nil then
        ps:=new(ws_paintstruct)
        rect:=new(ws_rect)
        beginpaint(hwnd,&ps)
        postmess(w,mm_restore,0,0,0)
        endpaint(hwnd,&ps)
        return 0
    fi

when wm_erasebkgnd then

when wm_move then
    if w<>nil then
    fi

when wm_size then
    x:=lparam iand 0xffff
    y:=lparam>>16
    if w<>nil  and (w.dimx<>x or w.dimy<>y) then
        return 0
    fi


when wm_contextmenu then
    sendmess(w,mm_rclick,wparam>>16,wparam iand 0xffff,0)
    return 0

when wm_mousewheel then
    if not wmouse then wmouse:=w fi
    postmess(wmouse,mm_wheel,int(wparam>>16),wparam iand 0xffff,0)
    return 0

when wm_nclbuttondown,wm_nclbuttondblclick then

when wm_activateapp then
    if wparam then
        postmess(w,mm_activate,1,0,0)
    fi

else
btnmessages:
    if message>=wm_lbuttondown and message<=wm_mbuttondblclk then
        buttonmessages(hwnd,message,wparam,lparam)
        return 0
    fi
esac
return 1    
end

export proc mxinit=
wmessagetable := [\
    wm_lbuttondown:     mm_click,
    wm_lbuttonup:       mm_clickup,
    wm_lbuttondblclk:   mm_dblclick,

    wm_rbuttondown:     mm_rclick,
    wm_rbuttonup:       mm_rclickup,
    wm_rbuttondblclk:   mm_rdblclick,

    wm_mbuttondown:     mm_mclick,
    wm_mbuttonup:       mm_mclickup,
    wm_mbuttondblclk:   mm_mdblclick]

buttontable := [\
    wm_lbuttondown:     1,
    wm_lbuttonup:       0,
    wm_lbuttondblclk:   1,

    wm_rbuttondown:     2,
    wm_rbuttonup:       0,
    wm_rbuttondblclk:   2,

    wm_mbuttondown:     3,
    wm_mbuttonup:       0,
    wm_mbuttondblclk:   3]

mousepos:=new(ws_point)

setmesshandler(process_wmmessage)

vktomesstable:=[\
    vkleft:     mm_leftkey,
    vkright:    mm_rightkey,
    vkup:       mm_upkey,
    vkdown:     mm_downkey,
    vkpageup:   mm_pageupkey,
    vkpagedown: mm_pagedownkey,
    vkhome:     mm_homekey,
    vkend:      mm_endkey,
    vktab:      mm_tabkey,
    vkbackspace:    mm_bskey,
    vkdelete:   mm_deletekey,
    vkenter:    mm_enterkey,
    vkinsert:   mm_insertkey,
    vkescape:   mm_cancel
]
end

export func postmess(w,mess,a=0,b=0,c=0)=

if w=nil then w:=wapplic fi
if w=nil then
 return 0 fi

if w.flags.[wa_closed] then

 return 0 fi

if mess>=1000 then
    headx:=1; mess-:=1000
else
    headx:=0
fi

case mess
when mm_sethozpos,mm_setvertpos,mm_draw,mm_restore,mm_update then
    for i:=1 to nmessages do
        m:=messagequeue[i].message
        if m=mess and w==messagequeue[i].wind then              
            messagequeue[i].a:=a
            messagequeue[i].b:=b
            return 0
        elsif mess=mm_draw and m=mm_update then     
            messagequeue[i].message:=mm_draw
            return 0
        fi
    od
esac

if quitmess or nmessages>=maxqueuesize then
    return 0
fi

postmsg(makemess(w,mess,a,b,c))

return 0                    
end

export func postmsg(msg,headx=0)=

if quitmess or nmessages>=maxqueuesize then
    return 0
fi

if msg.wind.flags.[wa_closed] then return 0 fi

if headx then


    ++nmessages
    for i:=nmessages downto 2 do
        messagequeue[i]:=messagequeue[i-1]
    od
    messagequeue[1]:=msg

else
    ++nmessages
    messagequeue[nmessages]:=msg
fi

return 0                    
end

export proc sendmess(w,mess,a=0,b=0,c=0)=

if w=nil then return fi
if w.flags.[wa_closed] then return fi

sendmsg(makemess(w,mess,a,b,c))
end

proc sendmsg(msg)=
if msg.wind.flags.[wa_closed] then return fi
postmsg(msg,1)
end

export func makemess(w,mess,a=0,b=0,state=-1)=

if w=nil then w:=wapplic fi

m:=new(rmessage,0)

m.wind:=w

m.message:=mess
m.a:=a
m.b:=b
m.state:=state

m.x:=mousepos.x
m.y:=mousepos.y

if m.state=-1 then m.state:=getshiftstate() fi

return m
end

func dokeymessage(hwnd,msg,wparam,lparam)=
case msg
when wm_syskeydown then

    if wparam=vkf10 then msg:=wm_keydown; goto dokey fi

when wm_syskeyup then
    if wparam=vkf10 then msg:=wm_keyup; goto dokey fi

when wm_keydown,wm_keyup then
dokey:
    case wparam
    when vkshift,vkctrl,vkalt,vkcapslock then
    else
        w:=wfocus
        if not w then w:=getwindow(hwnd) fi
        postmess(w,(msg=wm_keydown|mm_key|mm_keyup),wparam,lparam,-1)
        return 1
    esac
esac
return 0
end

func getshiftstate=
state:=0

if getkeystate(vklshift) iand 0x8000 then state ior:=kb_shift fi
if getkeystate(vklcontrol) iand 0x8000 then state ior:=kb_ctrl fi
if getkeystate(vklalt) iand 0x8000 then state ior:=kb_alt fi

if getkeystate(vkrshift) iand 0x8000 then state ior:=kb_rshift fi
if getkeystate(vkrcontrol) iand 0x8000 then state ior:=kb_rctrl fi
if getkeystate(vkralt) iand 0x8000 then
    state ior:=kb_ralt
    state iand:=(inot kb_ctrl)          
fi
if getkeystate(vkcapslock) iand 1 then state ior:=kb_capslock fi

return state ior buttonstate
END

proc buttonmessages(hwnd,msg,wp,lp)=

buttonstate:=wp iand (kb_lbutton ior kb_rbutton ior kb_mbutton)

mousepos.x:=lp iand 0xffff
mousepos.y:=int(lp)>>16
wmouse:=getwindow(hwnd)

mousesw:=buttontable{msg}

if mousesw then         
    lastbuttontime:=gettickcount()
    lastmousepos::=mousepos
    lastmousewindow:=wmouse
else
    mousesw:=0

    if dragmode then
        postmess(lastmousewindow,mm_enddrag,0,0,-1)
        dragmode:=0
    fi

    lastbuttontime:=0
    lastmousewindow:=nil
fi

newmess:=wmessagetable{msg}

case newmess
when mm_dblclick then unless wmouse.flags.[wa_leftdbl] then newmess:=mm_click end
when mm_rdblclick then unless wmouse.flags.[wa_rightdbl] then newmess:=mm_click end
esac

postmess(wmouse,newmess,wmouse.id,0,-1)
END

proc setnewmousewindow(w)=
return when not currmousewindow
unless w==currmousewindow then      
    if currmousewindow<>nil then
        postmess(currmousewindow,mm_offwindow,0,0,0)
    fi

    currmousewindow:=w
    postmess(w,mm_onwindow,0,0,0)
end unless
end

proc frame2rect(f,r)=
r^.x:=f^.x
r^.y:=f^.y

r^.dimx:=f^.x2-f^.x1+1
r^.dimy:=f^.y2-f^.y1+1
end

export func mxwait_mm_message=

if quitmess then                
    return 0
fi

windmsg:=new((iswin32|ws_msg32|ws_msg64))

while nmessages<=0 do
    if x:=getmessage(&windmsg,nil,0,0)<>0 then
        w:=windmsg.hwnd

        translatemessage(&windmsg)
        dispatchmessage(&windmsg)
    else
        quitmess:=1
        exit
    fi
od

if not nmessages then           
    return 0
fi  

currmess:=messagequeue[1]
--nmessages

xlatkeyboard()

for i:=1 to nmessages do
    messagequeue[i]:=messagequeue[i+1]
od
return 1
end

proc xlatkeyboard=
m:=currmess.message

if m=mm_key then

    k:=currmess.a
    if k>=vkf1 and k<=vkf12 then
        newmsg:=currmess
        currmess.message:=mm_functionkey
        currmess.a:=k-vkf1+1
    else
        keymess:=vktomesstable{k,0}
        if keymess then
            currmess.message:=keymess
        fi
    fi
fi
end

func getscreencoords(w,pos)=
pt::=pos
if not w then
    PCERROR("GSC/W=0")
fi

clienttoscreen(w.gdi.hwnd,&pt)      
return pt
end

export func getwindow(hwnd)=
if hwnd=0 then
    return nil
fi

index:=wx_getw(hwnd)
if index then
    return allwindows[index]
fi
return nil
end

proc initmenuhandlers=
ltcolour:=getrgb(ltgrey)
dkcolour:=getrgb(dkgrey)
end

proc gxhandler_fixups=
messhandlertable[mm_startdrag,scrollbar_class]:=mess_drag_scrollbar
messhandlertable[mm_enddrag,scrollbar_class]:=mess_drag_scrollbar
messhandlertable[mm_leftkey,scrollbar_class]:=mess_upkey_scrollbar
end

func mess_all_all(mess,w)=
case mess.message
when mm_startdrag,mm_drag,mm_enddrag then
when mm_command then
when mm_ok,mm_cancel then
when mm_click then
    case w.windclass
    when label_class, group_class then
        return skipmess
    esac
when mm_key then
when mm_sethozpos,mm_setvertpos then
when mm_pick,mm_lbchange then
when mm_leftkey,mm_rightkey,mm_upkey,mm_downkey,mm_enterkey,mm_tabkey then
when mm_pageupkey,mm_pagedownkey then
when mm_homekey, mm_endkey then
when mm_functionkey then
when mm_wheel then
else
    return skipmess
esac

return thismess
end

func mess_restore_all(mess,w)=
gxrestore(W)

return skipmess
end

func mess_killfocus_all             <"mx">      (mess,w)=

drawcaret(0)
wfocus:=nil

return skipmess
end

func mess_setfocus_all              <"mx">      (mess,w)=
if wfocus then
    mess_killfocus_all(mess,wfocus)
fi

wfocus:=w
drawcaret(1)
return skipmess
end

func mess_update_all    <"mx">          (mess,w)=
gxupdate(w)
return skipmess
end

func mess_draw_all      <"mx">          (mess,w)=
gxdraw(w)
return skipmess
end

func mess_click_select      <"mx">  (mess,w)=
if w.enable then
    if not w.style.noupdate then
        p:=w.linkvar
        p^:=w.attrs.onvalue
        for wc in w.owner.childlist do
            if wc.windclass=select_class and wc.linkvar=p then
                gxdraw(wc)
            fi
        od
    fi
    if w.style.returnmess then
        postmess(w,mm_command,w.id)
    fi
fi
return skipmess
end

func mess_click_toggle      <"mx">  (mess,w)=
if w.enable then
    if not w.style.noupdate then
        w.linkvar^:=not w.linkvar^
        gxdraw(w)
    fi
    if w.style.returnmess then
        postmess(w,mm_command,w.id)
    fi
fi
return skipmess
end

func mess_click_button      <"mx">  (mess,w)=

if w.enable=0 then
    beep1()
    return skipmess
fi

if w.id in 0..199 then              
    postmess(w,w.id)
else
    postmess(w,mm_command,w.id)
fi
return skipmess
end

func mess_click_editbox     <"mx">  (mess,w)=
if w.enable then
    if not w.style.noupdate then
        unless w==wfocus then
            gxfocus(w)
        end
    fi
    if w.style.returnmess then
        postmess(w,mm_command,w.id)
    fi
fi

return skipmess
end

func mess_click_arrow       <"mx">  (mess,w)=

case w.owner.windclass
when scrollbar_class then
    postmess(w.owner,dirtomess{w.style.dir},w.id,0,-1)
else
    mess.message:=dirtomess{w.style.dir}
    mess.a:=w.id
    return thismess
esac
return skipmess
end

func mess_click_mark        <"mx">  (mess,w)=

case w.owner.windclass
when toggle_class,select_class then
    postmess(w.owner,mess.message,w.id,0,-1)
esac
return skipmess
end

func mess_click_listbox     <"mx">  (mess,w)=
gxfocus(w)

y:=max(w.attrs.offset,mess.y)

pos:=(y-w.attrs.offset)%w.attrs.pitch+w.attrs.pagepos
if pos<=getlvbounds(w.linkvar).len then
    gxsetlbpos(w,pos)
    postmess(w,mm_pick,pos)
fi

return skipmess
end

func mess_click_scrollbar       <"mx">  (mess,w)=
onthumb:=isonthumb(w,(w.style.dir='H'|mess.x|mess.y))
step:=w.attrs.span
a:=w.attrs.currpos

case w.owner.windclass
when listbox_class then
    case onthumb
    when -1 then
        if a>w.attrs.limits.lwb then
            a:=max(a-step,w.attrs.limits.lwb)
            gxsetlbpage(w.owner,a)
        fi
    when 1 then
        if a<w.attrs.limits.upb then
            a:=min(a+step,w.attrs.limits.upb)
            gxsetlbpage(w.owner,a)
        fi
    esac
else
    if not step then step:=10 fi

    case onthumb
    when -1 then
        if a>w.attrs.limits.lwb then
            a:=max(a-step,w.attrs.limits.lwb)
            gxscrollpos(w,a,1)
            postmess(w,mm_sethozpos,a)
        fi
    when 1 then
        if a<w.attrs.limits.upb then
            a:=min(a+step,w.attrs.limits.upb)
            gxscrollpos(w,a,1)
            postmess(w,mm_sethozpos,a)
        fi
    esac
esac
return skipmess
end

func mess_wheel_scrollbar       <"mx listbox">  (mess,w)=
delta:=currmess.a
n:=abs(currmess.a%120)
to n do
    case w.windclass
    when scrollbar_class then
doscroll:
        postmess(w,(delta>0|mm_up|mm_down))
    when listbox_class then
        if w.childlist then
            w:=w.childlist[1]
            goto doscroll
        fi
        postmess(w,(delta>0|mm_upkey|mm_downkey))
    esac
od
return skipmess
end

func mess_up_scrollbar      <"mx">  (mess,w)=
a:=w.attrs.currpos
if a<=w.attrs.limits.lwb then
    return skipmess
fi
case w.owner.windclass
when listbox_class then
    gxsetlbpage(w.owner,a-1)
    return skipmess
else
    --a
    gxscrollpos(w,a,1)
    postmess(w,mm_setvertpos,a)
esac
return skipmess
end

func mess_left_scrollbar        <"mx">  (mess,w)=

case w.owner.windclass
when listbox_class then
    return skipmess
else
    a:=w.attrs.currpos
    if a>w.attrs.limits.lwb then
        --a
        gxscrollpos(w,a,1)
        postmess(w,mm_sethozpos,a)
    fi
esac
return skipmess
end

func mess_right_scrollbar       <"mx">  (mess,w)=

case w.owner.windclass
when listbox_class then
    return skipmess
else
    a:=w.attrs.currpos
    if a<w.attrs.limits.upb then
        ++a
        gxscrollpos(w,a,1)
        postmess(w,mm_sethozpos,a)
    fi

esac
return skipmess
end

func mess_down_scrollbar        <"mx">  (mess,w)=

a:=w.attrs.currpos
if a>=w.attrs.limits.upb then
    return thismess
fi
case w.owner.windclass
when listbox_class then
    gxsetlbpage(w.owner,a+1)
    return skipmess
else
    ++a
    gxscrollpos(w,a,1)
    postmess(w,mm_setvertpos,a)

esac
return skipmess
end

func mess_drag_scrollbar        <"mx">  (mess,w)=
case mess.message
when mm_startdrag then
    if isonthumb(w,(w.style.dir='H'|mess.x|mess.y))=0 then
        thumbdragmode:=1            
        thumbstartpos:=w.attrs.thumbpos-arrowdim        
    else                            
        return skipmess
    fi
when mm_enddrag then
    thumbdragmode:=0
    return skipmess
elsif not thumbdragmode then
    return skipmess
esac

offset:=(w.style.dir='H'|mess.a|mess.b)     
newpos:=thumbstartpos+offset                        

pos:=int(round((newpos/w.attrs.thumbspan)*(w.attrs.limits.len-1)+w.attrs.limits.lwb))
pos:=clamp(pos,w.attrs.limits.lwb,w.attrs.limits.upb)

case w.owner.windclass
when listbox_class then
    gxsetlbpage(w.owner,pos)
else
    gxscrollpos(w,pos,1)
    postmess(w,(w.style.dir='H'|mm_sethozpos|mm_setvertpos),pos)
esac
return skipmess
end

func mess_move_button       <"mx">          (mess,w)=
return skipmess
end

func mess_move_all          <"mx">          (mess,w)=
return skipmess
end

func mess_char_editbox      <"mx">          (mess,w)=
    if mess.a not in 32..255 then
        if wapplic then
            postmess(wapplic,mm_key,mess.a,mess.b,mess.state)
        fi
        return skipmess
    fi
    if not w.enable or w.style.noupdate then return skipmess fi
    s:=w.linkvar^
    n:=w.attrs.currpos
    c:=chr(mess.a)

    if n>s.len then             
        s+:=c
    elsif n=1 then              
        s:=c+s
    else                        
        s:=leftstr(s,n-1)+c+rightstr(s,-(n-1))
    fi

    w.linkvar^:=s
    ++w.attrs.currpos
    gxdraw(w)

    return skipmess
end

func mess_key_editbox       <"mx">          (mess,w)=

postmess(wapplic,mm_key,mess.a,mess.b,mess.state)

return skipmess
end

func mess_leftkey_editbox       <"mx">      (mess,w)=
if ctrlpressed() then
    postmess(wapplic,mm_leftkey,mess.a,mess.b,mess.state)
    return skipmess
fi

if w.attrs.currpos>1 then
    drawcaret(0)
    --w.attrs.currpos
    drawcaret(1)
fi
return skipmess
end

func mess_rightkey_editbox      <"mx">      (mess,w)=
if ctrlpressed() then
    postmess(wapplic,mm_rightkey,mess.a,mess.b,mess.state)
    return skipmess
fi

if w.attrs.currpos<=w.linkvar^.len then
    drawcaret(0)
    ++w.attrs.currpos
    drawcaret(1)
fi
return skipmess
end

func mess_bskey_editbox     <"mx">      (mess,w)=
s:=w.linkvar^
if not s then return skipmess fi
n:=w.attrs.currpos
if n=1 then return skipmess fi

if n>s.len then             
    s:=leftstr(s,-1)
else                        
    s:=leftstr(s,n-2)+rightstr(s,-(n-1))
fi

w.linkvar^:=s
--w.attrs.currpos
gxdraw(w)

return skipmess
end

func mess_deletekey_editbox     <"mx">      (mess,w)=
s:=w.linkvar^
if not s then return skipmess fi
n:=w.attrs.currpos
if n>s.len then return skipmess fi

if n=1 then             
    s:=rightstr(s,-1)
else                        
    s:=leftstr(s,n-1)+rightstr(s,-n)
fi
w.linkvar^:=s
gxdraw(w)

return skipmess
end

func mess_homekey_editbox       <"mx">      (mess,w)=
if ctrlpressed() then
    postmess(wapplic,mm_homekey,mess.a,mess.b,mess.state)
    return skipmess
fi

drawcaret(0)
w.attrs.currpos:=1
drawcaret(1)

return skipmess
end

func mess_homekey_listbox       <"mx">      (mess,w)=
if w.attrs.currpos>1 then
    gxsetlbpos(w,1)
fi

return skipmess
end

func mess_endkey_editbox        <"mx">      (mess,w)=
if ctrlpressed() then
    postmess(wapplic,mm_endkey,mess.a,mess.b,mess.state)
    return skipmess
fi

drawcaret(0)
w.attrs.currpos:=w.linkvar^.len+1
drawcaret(1)

return skipmess
end

func mess_endkey_listbox        <"mx">      (mess,w)=
if w.attrs.currpos<getlvbounds(w.linkvar).len then
    gxsetlbpos(w,getlvbounds(w.linkvar).len)
fi

return skipmess
end

func mess_upkey_listbox     <"mx">      (mess,w)=
if w.attrs.currpos>1 then
    gxsetlbpos(w,w.attrs.currpos-1)
fi

return skipmess
end

func mess_upkey_scrollbar       <"mx">      (mess,w)=

a:=w.attrs.currpos
if a>w.attrs.limits.lwb then
    --a
    gxscrollpos(w,a,1)
    postmess(w,mm_setvertpos,a)
fi
return skipmess
end

func mess_downkey_listbox       <"mx">      (mess,w)=
if w.attrs.currpos<getlvbounds(w.linkvar).len then
    gxsetlbpos(w,w.attrs.currpos+1)
fi

return skipmess
end

func mess_pageupkey_listbox     <"mx">      (mess,w)=
if (a:=w.attrs.currpos)>1 then
    a:=max(a-w.attrs.rows,1)
    gxsetlbpos(w,a)
fi

return skipmess
end

func mess_pagedownkey_listbox       <"mx">      (mess,w)=
if (a:=w.attrs.currpos)<getlvbounds(w.linkvar).len then
    a:=min(a+w.attrs.rows,getlvbounds(w.linkvar).len)
    gxsetlbpos(w,a)
fi

return skipmess
end

func mess_enterkey_listbox      <"mx">      (mess,w)=
if w.attrs.currpos then
    postmess(w,mm_pick,w.attrs.currpos)
fi

return skipmess
end

proc do_draw_all            <"wx">          (w)=
gxclear(w)
drawborder(w)
drawchildborders(w)
end

proc do_draw_button         <"wx label">            (w)=
gxclear(w)

gxtext_just(w,w.text,0,w.enable)

drawborder(w)

end

proc do_draw_label(w)=
do_draw_button(w)
end

proc do_draw_toggle         <"wx">  (w)=

gxclear(w)

VALSTR:=""

turnedon:=istrue w.linkvar^

if w.style.marktype then
    drawmark(w.childlist[1],turnedon,w.enable)

    gxtext_just(w,w.text+valstr,markdim,w.enable)
else
        if turnedon then
            gxclear(w,getrgb(green))
        fi

    gxtext_just(w,w.text+valstr)
fi
end

proc do_draw_select         <"wx">  (w)=
gxclear(w)

turnedon:=w.linkvar^=w.attrs.onvalue

if w.style.marktype then
    drawmark(w.childlist[1],turnedon,w.enable)
    gxtext_just(w,w.text,markdim,w.enable)
else
    case w.style.hilitetype
    when invert_hilite then
        if turnedon then
            gxclear(w,getrgb(white))
        fi
    esac
        gxtext_just(w,w.text)
fi
end

proc do_draw_editbox            <"wx">  (w)=
gxclear(w)

gxtext_just(w,w.linkvar^,enable:w.enable)

unless wfocus==w then           
    return
end

caretdrawn:=0

drawcaret(1)
end

proc do_draw_arrow          <"wx">  (w)=
gxclear(w)

drawborder(w)
drawarrow(w,w.enable)
end

proc do_draw_mark           <"wx">  (w)=

case w.owner.windclass
when toggle_class, select_class then
    return                  
esac

gxclear(w,getrgb(w.owner.style.windbgnd))

drawborder(w)
end

proc do_draw_scrollbar          <"wx">  (w)=
gxclear(w)
drawborder(w)
gxdraw(w.childlist[1])          
gxdraw(w.childlist[2])

if w.attrs.thumbsize then
    if w.style.dir='H' then
        x:=w.attrs.thumbpos
        dx:=w.attrs.thumbsize
        drawthumb(w,x,0,dx,w.dimy)
    else
        y:=w.attrs.thumbpos
        dy:=w.attrs.thumbsize
        drawthumb(w,0,y,w.dimx,dy)
    fi
fi
end

proc do_draw_listbox            <"wx">  (w)=
gxclear(w)
drawborder(w)
if w.childlist then         
    gxdraw(w.childlist[1])
fi

for i:=1 to w.attrs.rows do
    k:=i+w.attrs.pagepos-1
    if k<=getlvbounds(w.linkvar).len then
        drawlbtext(w,i,getlvstritem(w.linkvar,k),0,k=w.attrs.currpos)
    fi
od
end

proc do_update_all              <"wx">  (w)=
gxdraw(w)
end

proc do_update_listbox          <"wx">  (w)=
gxdraw(w)
end

proc drawcaret(x)=

if wfocus=nil then      
    caretdrawn:=0
    return
fi

case wfocus.windclass
when editbox_class then
    if x then           
        if caretdrawn then return fi    
        xpos:=getcaretpos(wfocus.linkvar^,wfocus.attrs.currpos,0)
        wfocus.attrs.caretpos:=xpos         
    else            
        if not caretdrawn then return fi    
        xpos:=wfocus.attrs.caretpos     
    fi

    caretwidth:=2

    gxhighlight(wfocus,xpos+wfocus.attrs.textpos[1],wfocus.attrs.textpos[2]-chd,caretwidth,20)

    caretdrawn:=x
esac
end

func getcaretpos(s,pos,offset)=
if pos=1 then return 0 fi

return wx_gettextwidth(wfocus.gdi.hdc, leftstr(s,pos-1))
end

proc drawborder(w)=

case bscat[w.style.border]
when 0 then                 
    return
when 'W' then               
    return
when 'X' then               
    posx:=w.frameposx
    posy:=w.frameposy
    dimx:=w.framedimx
    dimy:=w.framedimy
    bs:=w.style.border

    bs:=w.style.border
    wo:=w.owner
    case bs
    when bs_simple then         
        gxcolour(wo,0)
        gxrect(wo,posx,posy,dimx,dimy)
    when bs_thick then
    when bs_panel then
        gxcolour(wo,ltcolour)
        gxline(wo,posx+dimx-1,posy, posx,posy)
        gxline(wo,posx,posy+dimy-1)
        gxcolour(wo,dkcolour)
        gxline(wo,posx+dimx-1,posy+dimy-1)
        gxline(wo,posx+dimx-1,posy)
    when bs_inset then
        gxcolour(wo,dkcolour)
        gxline(wo,posx+dimx-1,posy, posx,posy)
        gxline(wo,posx,posy+dimy-1)
        gxcolour(wo,ltcolour)
        gxline(wo,posx+dimx-1,posy+dimy-1)
        gxline(wo,posx+dimx-1,posy)
    when bs_testext then
        gxcolour(wo,0)
        gxrect(wo,posx,posy,dimx,dimy)
        gxrect(wo,posx+9,posy+9,dimx-18,dimy-18)

    esac
when 'I' then               
    posx:=w.frameposx
    posy:=w.frameposy
    dimx:=w.dimx
    dimy:=w.dimy

    case w.style.border
    when bs_ownpanel then
        gxcolour(w,ltcolour)
        gxline(w,w.framedimx-1,0,0,0)
        gxline(w,0,w.framedimy-1)
        gxcolour(w,dkcolour)
        gxline(w,w.framedimx-1,w.framedimy-1)
        gxline(w,w.framedimx-1,0)

    when bs_owninset then
        gxcolour(w,dkcolour)
        gxline(w,w.framedimx-1,0,0,0)
        gxline(w,0,w.framedimy-1)
        gxcolour(w,ltcolour)
        gxline(w,w.framedimx-1,w.framedimy-1)
        gxline(w,w.framedimx-1,0)
    when bs_ownsimple then
        gxcolour(w,0)
        gxrect(w,0,0,w.framedimx,w.framedimy)
    when bs_testint then
        gxcolour(w,0)
        gxrect(w,0,0,dimx,dimy)
        gxrect(w,7,7,dimx-14,dimy-14)
    esac
esac
end

proc drawchildborders(w)=
if not w.childlist then
    return
fi
for wc in w.childlist do
    if wc.style.border in [bs_simple,bs_thick,bs_panel,bs_inset] then
        drawborder(wc)
    fi
od
end

proc drawarrow(w,enable)=
const factor=0.3

gxsetpen(w,(enable|black|dkgrey))

width:=w.dimx
height:=w.dimy

case w.style.dir
when 'D' then
    x:=int(round(width/2)-1)

    wd:=0

    h:=int(round(min(height,width)*factor))
    if h<3 then h:=3 fi
    y:=int((height+h)*0.5)-1

    to h do
        gxline(w,x,y,x+wd,y)
        x-:=1
        y-:=1
        wd+:=2
    od

when 'U' then
    x:=int(round(width/2)-1)
    wd:=0

    h:=int(round(min(height,width)*factor))
    if h<3 then h:=3 fi
    y:=int(round((height-h)*0.5))
    to h do
        gxline(w,x,y,x+wd,y)
        x-:=1
        y+:=1
        wd+:=2
    od

when 'L' then
    y:=height%2

    ht:=0
    wd:=y

    wd:=int(round(min(height,width)*factor))
    if wd<3 then wd:=3 fi
    x:=int(round((width-wd)*0.5)-1)

    to wd do
        gxline(w,x,y,x,y+ht)
        y-:=1
        x+:=1
        ht+:=2
    od

when 'R' then
    y:=height%2
    ht:=0

    wd:=int(round(min(height,width)*factor))
    if wd<3 then wd:=3 fi
    x:=int(round((width+wd)*0.5)-1)

    to wd do
        gxline(w,x,y,x,y+ht)
        y-:=1
        x-:=1
        ht+:=2
    od
esac
end

export proc gxtext_just(w,s,offset=0,enable=1)=
    dimx:=w.dimx
    dimy:=w.dimy
    width:=wx_gettextwidth(w.gdi.hdc, s)
    height:=chy             

    case w.style.justify
    when 'L' then   x:=smx
    when 'R' then   x:=dimx-width-smx
    else
                x:=(dimx-width)%2
    esac

    case w.style.vjustify
    when 'T' then   y:=smy
    when 'B' then   y:=dimy-height-smy
    else
                y:=(dimy-height)%2
    esac

    if not enable then
        oldtextfgnd:=w.style.textfgnd
        gxtextcolour(w,grey)
    fi

    gxtext(w,s,x+offset,y)

    if not enable then
        gxtextcolour(w,oldtextfgnd)
    fi
    if w.windclass=editbox_class then
        w.attrs.textpos:=(x+offset,y)
    fi
end

proc drawthumb(w,x,y,dx,dy)=

gxcolour(w,0)
gxrect(w,x,y,dx,dy)
gxfillrect(w,x+1,y+1,dx-2,dy-2,getrgb(grey))
end

func isonthumb(w,d)=

a:=w.attrs.thumbpos
b:=w.attrs.thumbsize

if d<a then
    return -1
elsif d>(a+b) then
    return 1
else
    return 0
fi
end

proc drawmark(w,turnedon,enable)=

gxclear(w,getrgb(w.owner.style.windbgnd))
gxsetpen(w,(enable|black|red))

width:=w.dimx
height:=w.dimy
x:=y:=1
wd:=width-2
ht:=height-2
gxrect(w,x,y,wd,ht)
if not turnedon then return fi

case w.style.marktype
when radio_mark then

    gxfillrect(w,x+3,y+3,wd-6,ht-6,getrgb(red))

when check_mark then

    gxline(w,x,y,x+wd-1,y+ht-1)
    gxline(w,x+wd-1,y,x,y+ht-1)

when tick_mark then

    gxline(w,x+3,y+ht%2,x+wd%2,y+ht-4)
    gxline(w,x+wd-3,y+2)

esac
end

proc drawlbtext(w,row,text,clr=0,hilite=0)=

x:=0
y:=(row-1)*w.attrs.pitch+w.attrs.offset

if clr or hilite then
    gxfillrect(w,x,y,w.dimx,w.attrs.pitch,(hilite|getrgb(grey)|getrgb(w.style.windbgnd)))
fi

if hilite then
    oldtextcolour:=gxtextcolour(w)
    gxtextcolour(w,white)
fi

gxtext(w,text,x+smx,y+smy)
if hilite then
    gxtextcolour(w,oldtextcolour)
fi
end

func readnextitem(a)=

if a="" then return list(0,0,0,0) fi

level:=1
tabs:=0
options:=""

while asc(a) in [9,' '] do tabs+:=1; a:=rightstr(a,-1) od

if a="" then return list(0,0,0,0) fi

case asc(a)
when '!' then
    return list(0,0,0,0)
esac

if tabs then
    j:=0
    for i:=1 to ntab do
        if tabs=tabstack[i] then j:=i; exit fi
    od

    if j=0 then
        if tabs>tabstack[ntab] then
            ntab+:=1
            tabstack[ntab]:=tabs
        fi
        level:=ntab
    else
        level:=j
        if j<ntab then ntab:=j fi
    fi
fi

if asc(a) in ['0'..'9'] then
    value:=strtoval(a)
    n:=" " in a
    if not n then
        n:=chr(9) in a
    fi
    if n then
        labelx:=rightstr(a,-n)
    else
        labelx:="?"
    fi

else            

    if "=" in a then    
        return (0,0,0,0)
    fi

    value:=k_menu
    labelx:=a
    case convlc(labelx)
    when "hozbreak","divider" then
        value:=kdivide
    when "vertbreak" then
        value:=kcolumn
    when "filehistory" then
        value:=kfilehistory
    else
        if leftstr(labelx)="-" then value:=kdivide fi
    esac
fi

if labelx="" then           
    return list(0,0,0,0)
fi

return (level,value,(labelx),options)
end

func readmenu(m,n,level)=

restartx:
for i:=n to ndata do
    (l,value,labelx,options):=data[i]

    if l<=level then        
        return i
    fi

    flags:=breakflag
    enable:=1
    if rightstr(labelx)="?" then
        enable:=0
        labelx:=leftstr(labelx,-1)
    fi

    if options<>"" then
        if "H" in options then flags+:="h" fi
        if "C" in options then flags+:="c" fi
    fi

    case value
    when kdivide then
        gxaddmb(m,style:"d")
    when kcolumn then
        breakflag:="v"
    when k_menu then        
        newm:=gxcreatemb()
        n:=readmenu(newm,i+1,l)
        gxaddmb(m,labelx,newm,"p"+flags,enable)
        breakflag:=""
        goto restartx
    when kfilehistory then
        nfiles:=8
        gxaddmb(m,"filehistory",1060,breakflag)
    else                
normalcmd:
        gxaddmb(m,labelx,value,flags,enable)
        breakflag:=""
    esac

skip:
od

return ndata+1          
end

func mbreaddata(a)=

tabstack::=(0,)
ntab:=1
data::=()
ndata:=0
breakflag:=""

if a.isstring then      
    a:=readtextfile(a)
    if a=0 then
        a:=("CANTOPENFILE",)
    fi
fi

for i:=1 to a.upb do
    x:=readnextitem(a[i])

    if x[1] then
        ++ndata
        data[ndata]:=x
    fi
od

m:=gxcreatemb()
readmenu(m,1,0)
return m
end

export func gxmenubar(w,?a)=

if a.defined then       
    m:=mbreaddata(a)

    if not w.ispointer then
        while w.owner<>nil do
            w:=w.owner
        od
    fi

    gxsetmb(w,m)
    return 0
else                
    return mbreaddata(w)
fi
end

func gxcreatemb(?s)=

if s.defined and s in "Pp" then
    return createpopupmenu()
else
    return createmenu()
fi
end

proc gxsetmb(w,m)=

hwnd:=w.gdi.hwnd
a:=getmenu(hwnd)
s:=setmenu(hwnd,m)
if a then destroymenu(a) fi
end

func gxaddmb(wm,caption="X",id=0,style="",enable=0)=

if wm.ispointer then                
    hmenu:=wm
    wm:=nil
else
    hmenu:=getmenu(wm.gdi.hwnd)
fi


flags:=mf_string ior mf_unchecked

if not enable then flags ior:=mf_greyed fi

foreach c in convuc(style) do
    case c
    when 0 then exit
    when 'P' then flags ior:=mf_popup
    when 'D' then flags ior:=mf_separator
    when 'B' then flags ior:=mf_menubreak
    when 'V' then flags ior:=mf_menubarbreak
    when 'H' then flags ior:=mf_help
    when 'C' then flags ior:=mf_checked
    esac
od

if appendmenu(hmenu,flags,id,caption) then
    if wm<>nil then drawmenubar(wm.gdi.hwnd) fi
    return hmenu
fi
return 0
end

proc gxshowmb(wm,w,x,y)=
if wm.ispointer then

    if not y.defined then
        x:=w
        y:=x
        w:=nil
        hwnd:=wapplic.gdi.hwnd
    else
        hwnd:=w.gdi.hwnd
    fi

    pos:=ws_point(x,y)

    if w<>nil then
        clienttoscreen(w.gdi.hwnd,&pos)
    fi

    trackpopupmenu(wm,0,pos.x,pos.y,0,hwnd,0)
else
    drawmenubar(wm.gdi.hwnd)
fi
end

func gxenablemb(wm,id,enable)=

if wm.ispointer then                
    hmenu:=wm
else
    hmenu:=getmenu(wm.gdi.hwnd)
fi

if enable.defined then
    return enablemenuitem(hmenu,id,(enable|0|mf_greyed)+mf_bycommand)
else
    return (getmenustate(hmenu,id,mf_bycommand) iand mf_greyed|0|1)
fi
end

func gxcheckmb(wm,id,check)=
if wm.ispointer then                
    hmenu:=wm
else
    hmenu:=getmenu(wm.gdi.hwnd)
fi

if check.defined then
    return checkmenuitem(hmenu,id,(check|mf_checked|mf_unchecked)+mf_bycommand)
else
    return (getmenustate(hmenu,id,mf_bycommand) iand mf_checked|1|0)
fi
end

proc gxclosemb(m)=
destroymenu(m)
end

export func gxconfirm(m)=
x:=gxmsgbox(m,"Confirm","byn")
return x="yes"
end

func issubwindow(w,w2)=
while w2 do
    if w2==w then return 1 fi
    w2:=w2.owner
od
return 0
end

export proc flushmessages=

end
=== bmlib.q 0 1 37/43 ===

VAR DEBUG=0


importdll imglib =
    clang func imgload_rgb      (stringz, ref byte, ref byte, ref byte, int32)ref byte

    clang func imgload_bgr      (stringz, ref int32, ref int32, ref int32, int32)ref byte

    clang procedure     imgload_free        (ref byte)
    clang func imgsave_jpeg_rgb (stringz, ref byte, int32, int32, int32)int32
    clang func imgsave_jpeg_bgr (stringz, ref byte, int32, int32, int32)int32
end

type bmpheader = struct
    ws_bitmapfileheader fh
    ws_bitmapinfoheader bh
end

var shifts=[2:1, 4:2, 8:3, 16:4, 32:5, 64:6]

proc start=
end

proc main=



CPL "TESTING BMMAIN"
FILE:="C:/JPEG/SMALLGIRL.JPG"


BM:=BMLOADJPG(FILE)
IF NOT BM THEN STOP FI

w:=GXCREATEWINDOW(DIM:(960,540),caption:"HI THERE")
gxcopy(w,bm,scalex:5.0, x:100)
eventloop()

end

export func bmcreate(pixelbits,width,height)=

    bminfo:=new(ws_bitmapv5header)
    bminfo.size:=ws_bitmapv5header.bytes
    bminfo.width:=width
    bminfo.height:=-height
    bminfo.planes:=1
    bminfo.bitcount:=pixelbits

    pixelptr:=nil

    if pixelbits not in [8,24,32] then
        abort("bmcreate pixel size not supported:"+tostr(pixelbits))
    fi



    hwnd:=createdibsection(nil,&bminfo,0,&pixelptr,nil,0)


    pixelptr:=makeref(pixelptr,byte)

    if hwnd=0 then
        error:=getlasterror()
        abort("bmcreate:CreateDIB failed:"+tostr(error))
    fi


    bm:=new(rwindow,0)
    bm.windclass:=bitmap_class

    bm.dimx:=width
    bm.dimy:=abs(height)        

    bm.style:=defstyle

    bm.pixelbits:=pixelbits
    bm.pixelptr:=pixelptr


    bm.pixelbytes:=pixelbits%8

    n:=bm.pixelbytes*width

    if (n iand 3)<>0 then   
        n:=(n+4) iand 0xfffc
    fi
    bm.linebytes:=n
    bm.framebytes:=bm.linebytes*bm.dimy

    if pixelbits=8 then
        palette:=new(array,int32,0..255)
        bm.paltype:=greyscale_pal
        colour:=0
        for i:=0 to 255 do
            palette[i]:=colour
            colour+:=0x10101
        od
    fi

    setupgdi(bm,hwnd)

    bm.gdi.hdc:=createcompatibledc(nil)
    bm.gdi.drawmode:=dm_memory
    bm.gdi.oldbmobj:=selectobject(bm.gdi.hdc,hwnd)  
    setstretchbltmode(bm.gdi.hdc,4)         

    bmputpalette(bm,palette)

    return bm
end

export func bmgetpalette(bm)=
    if bm.paltype then
        palette:=new(array,int32,0..256)
        getdibcolortable(bm.gdi.hdc,0,256,&palette)
        palette[256]:=bm.paltype
        reversepalette(palette)
    else
        palette:=()
    fi
    return palette
end

export proc bmputpalette(bm,p,reverse=1)=
    if bm.paltype then
        if reverse then reversepalette(p) fi        
        setdibcolortable(bm.gdi.hdc,0,256,&p)   
        if reverse then reversepalette(p) fi            
        if p.upb=256 then
            bm.paltype:=p[256]
        fi
    fi
end

export func bmcolour(bm,n,?colour)=

    if colour.isdef then        
        colour:=revpixel(colour)
        setdibcolortable(bm.gdi.hdc,n,1,&colour)
        return colour
    else                
        colour:=0
        getdibcolortable(bm.gdi.hdc,n,1,&colour)
        return revpixel(colour)
    fi
end

export proc reversepalette(&p)=
    for i:=0 to 255 do
        p[i]:=revpixel(p[i])
    od
end

export func revpixel(a)=
return (a iand 0x00ff00) ior (a>>16 iand 255) ior ((a iand 255)<<16)
end

export proc bmshow(bm)=
    gxcopy(bm)
end

export proc bmfree(bm)=
    return when bm=nil
    if not deletedc(bm.gdi.hdc) then
        pcerror("ERROR DELETING BM/HDC")
    fi

    if not deleteobject(bm.gdi.hwnd) then
        pcerror("ERROR DELETING DIB")
    fi
end

export func bmdupl(bm)=
    newbm:=bmcreate(bm.pixelbits, bm.dimx, bm.dimy)
    memcpy(newbm.pixelptr, bm.pixelptr, bm.linebytes*bm.dimy)

    bmduplpalette(newbm,bm)

    return newbm
end

export proc bmduplpalette(newbm,bm)=
    if bm.paltype then
        pal:=bmgetpalette(bm)
        bmputpalette(newbm,pal)
        newbm.paltype:=bm.paltype
    fi
end

export func bmduplz(bm)=
    newbm:=bmcreate(bm.pixelbits, bm.dimx, bm.dimy)
    return newbm
end

export func bmgetptr(bm,x,y)=
    return bm.pixelptr+(bm.linebytes*y+x*bm.pixelbytes)
end

export func bmgetrowptr(bm,y)=
    return bm.pixelptr+y*bm.linebytes
end

func getcbbitmap(hwnd)=

    p:=globallock(hwnd)
    hsize:=ws_bitmapinfoheader.bytes
    bm:=nil

    if p then
        p:=makeref(p,ws_bitmapinfoheader)
        pb:=makeref(p,byte)

        bm:=bmcreate(p^.bitcount,p^.width,p^.height)
        offset:=(bm.paltype|1024|0)     

        if offset then
            setdibcolortable(bm.gdi.hdc,0,256,pb+hsize)
        fi

        pb:=pb+hsize+offset
        for y:=0 to bm.dimy-1 do
            q:=bmgetrowptr(bm,bm.dimy-y-1)
            memcpy(q,pb,bm.linebytes)
            pb:=pb+bm.linebytes
        od

    fi

    globalunlock(hwnd)

    return bm
end

export func bmgetclipboard=
    if openclipboard(0)=0 then
        return nil
    fi

    hwnd:=getclipboarddata(cf_dib)

    bm:=nil
    if hwnd then
        bm:=getcbbitmap(hwnd)
    fi

    closeclipboard()

    return bm
end

export func bmputclipboard(bm)=
    if openclipboard(0)=0 then
        return nil
    fi

    emptyclipboard()

    hwnd:=putcbbitmap(bm)
    if hwnd then
        setclipboarddata(cf_dib,hwnd)
    fi

    closeclipboard()
    return 1
end

func putcbbitmap(bm)=
    hsize:=ws_bitmapinfoheader.bytes
    psize:=(bm.paltype|1024|0)
    fsize:=bm.linebytes*bm.dimy

    hmem:=globalalloc(0,hsize+psize+fsize)
    mem:=makeref(globallock(hmem),byte)

    hdr:=new(ws_bitmapinfoheader)
    hdr.size:=hsize
    hdr.width:=bm.dimx
    hdr.height:=bm.dimy
    hdr.bitcount:=bm.pixelbits
    hdr.planes:=1
    hdr.xpelspermetre:=11811
    hdr.ypelspermetre:=11811
    hdr.clrused:=0

    memcpy(mem,&hdr,hsize)

    if psize then
        pal:=bmgetpalette(bm)
        memcpy(mem+hsize,&pal,psize)
    fi

    mem:=mem+hsize+psize
    for y:=0 to bm.dimy-1 do
        p:=bmgetrowptr(bm,bm.dimy-1-y)
        memcpy(mem, p, bm.linebytes)
        mem:=mem+bm.linebytes
    od
    globalunlock(hmem)

    return hmem
end

proc copy24to8(newbm,oldbm)=
    for y:=0 to oldbm.dimy-1 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(oldbm,y)
        to oldbm.dimx do
            p++^:=q^
            q:=q+3
        od
    od
end


export proc bmresetpalette(bm)=
    pal:=new(array,int32,0..256)
    colour:=0
    for i:=0 to 255 do
        pal[i]:=colour
        colour+:=0x010101
    od
    bmputpalette(bm,pal)
    bm.paltype:=greyscale_pal
end

func makescalemap(x)=
    map:=new(list,0..255)
    for i:=0 to 255 do
        map[i]:=clamp(int(round(i*x)),0,255)
    od
    return map
end

func bmunimpl(mess)=
PRINTLN "UNIMPLEMENTED:",MESS
PRINTLN "ABORTING"
STOP
return 0
end


export func bmload(filename)=
    case e:=convlc(extractext(filename))
    when "jpg","jpeg" then
        return bmloadjpg(filename)
    when "bmp" then
        return bmloadbmp(filename)
    when "pgm" then
        return bmloadpgm_p2p5(filename)
    when "ppm" then
        return bmloadppm_p3p6(filename)
    when "pbm" then
        return bmloadpbm_p1p4(filename)
    when "png" then
        return bmloadpng(filename)
    when "" then                
        exts:=("jpg","bmp","pgm","ppm","png")
        for ext in exts do
            bm:=bmload(addext(filename,ext))
            if bm then
                return bm
            fi
        od
        return nil
    else
        println "CAN'T LOAD",E,"IMAGE"
        return nil
    esac
    return nil
end

func bmloadbmp(filename)=
    f:=openfile(filename)
    if not f then return nil fi

    fileheader:=new(bmpheader)

    readrandom(f,&fileheader,0,bmpheader.bytes)
    filedimx:=fileheader.bh.width
    filedimy:=fileheader.bh.height
    invert:=1
    if filedimy<0 then
        filedimy:=abs(filedimy)
        invert:=0
    fi

    if fileheader.fh.typex<>'BM' then
        closefile(f)
        return nil
    fi

    if fileheader.bh.compression<>0 then
        closefile(f)
        return nil
    fi

    bm:=bmcreate(fileheader.bh.bitcount,filedimx,filedimy)
    framebytes:=bm.linebytes*filedimy

    if bm.paltype then
        palette:=new(array,int32,0..255)
        readrandom(f,&palette,bmpheader.bytes,1024)
        bmputpalette(bm,palette,0)
        colour:=0
        for i:=0 to 255 do
            if palette[i]<>colour then
                bm.paltype:=colour_pal
                exit
            fi
            colour+:=0x010101
        od

    fi

    readrandom(f,bm.pixelptr,fileheader.fh.offbits,framebytes)
    closefile(f)


    if invert then
        n:=bm.linebytes
        buffer:=makeref(malloc(n),byte)

        for y:=0 to filedimy%2 do
            p:=bmgetrowptr(bm,y)
            q:=bmgetrowptr(bm,filedimy-1-y)
            memcpy(buffer,p,n)
            memcpy(p,q,n)
            memcpy(q,buffer,n)
        od
        free(buffer)
    fi

    return bm
end

export func bmloadjpg(filename)=
    w:=h:=n:=0

    p:=imgload_bgr(filename,&w,&h,&n,0)

    pixelbits:=n*8

    if p=nil then
        return nil
    fi

    q:=makeref(p,byte)

    bm:=bmcreate(pixelbits,w,h)

    nbytes:=w*h*n
    dest:=makeref(bm.pixelptr,byte)

    to h do
        memcpy(dest,q,w*n)
        dest:=dest+bm.linebytes
        q:=q+w*n
    od

    imgload_free(p)

    return bm
end

func bmloadpbm_p1p4(filename)=
CPL "CAN'T LOAD PBM"
RETURN NIL
end

func bmloadpgm_p2p5(filename)=

CPL "LOAD PGM P25",FILENAME
    f:=openfile(filename,"rb")
    if f=0 then return nil fi

    readln @f, sig:"s"

    case sig
    when "P5" then
        binary:=1
    when "P2" then
        binary:=0
    else
        abort("Can't read pgm")
        return nil
    esac    

    width:=readnextint(f)
    height:=readnextint(f)
    maxpix:=readnextint(f)

    bm:=bmcreate(8,width,height)

    linebytes:=width
    dest:=makeref(bm.pixelptr,byte)

CPL =WIDTH,=HEIGHT, LINEBYTES,=BM
    to height do
        if binary then
            readbytes(f,dest,width)         
        else
            p:=dest
            to linebytes do
                p++^:=readffint(f)
            od
        fi

        dest:=dest+bm.linebytes
    od

    closefile(f)

    return bm
end

func bmloadppm_p3p6(filename)=

    f:=openfile(filename,"rb")
    if f=0 then return nil fi

    readln @f, sig:"s"

    case sig
    when "P6" then
        binary:=1
    when "P3" then
        binary:=0
    when "P5","P2" then
        closefile(f)
        return bmloadpgm_p2p5(filename)
    when "P4","P1" then
        closefile(f)
        return bmloadpbm_p1p4(filename)
    else
        abort("Can't read ppm/P3")
        return nil
    esac    


    width:=readnextint(f)
    height:=readnextint(f)
    maxpix:=readnextint(f)

    bm:=bmcreate(24,width,height)

    linebytes:=width*3
    dest:=makeref(bm.pixelptr,byte)

    to height do
        if binary then
            readbytes(f,dest,linebytes)         
        else
            p:=dest
            to linebytes do
                p++^:=readffint(f)
            od
        fi

        p:=dest                             
        to width do
            swap(p^,(p+2)^)
            p:=p+3
        od

        dest:=dest+bm.linebytes
    od

    closefile(f)

    return bm
end

func readnextint(f)=
    read x
    while not x.isint and not eof(f) do
        readln @f,x
    od
    if not x.isint then return 0 fi
    return x
end

func readffint(f)=
    repeat
        c:=inbyte(f)
    until c in '0'..'9'

    a:=c-'0'
    do
        c:=inbyte(f)
        if c in '0'..'9' then
            a:=a*10+c-'0'
        else
            exit
        fi
    od

    return a
end

func bmloadpng(filename)=
RETURN BMLOADJPG(FILENAME)
end

export func bmsave(filename,bm,binary=1)=
    case e:=convlc(extractext(filename))
    when "jpg","jpeg" then
        return bmsavejpg(filename,bm)
    when "bmp" then
        return bmsavebmp(filename,bm)
    when "ppm" then
        return bmsaveppm_p3p6(filename,bm,binary)
    when "pgm" then
        return bmsavepgm_p2p5(filename,bm,binary)
    when "pbm" then
        return bmsavepbm_p1p4(filename,bm,binary)
    else
        println "CAN'T SAVE",E,"IMAGE"
        return nil
    esac
    return nil
end

func bmsavebmp(filename,bm)=
    w:=bm.dimx
    h:=bm.dimy
    pixelbytes:=bm.pixelbytes
    framebytes:=bm.linebytes*h
    palettebytes:=(pixelbytes=1|1024|0)

    bmfile:=createfile(filename)
    if bmfile=nil then
        return 0
    fi

    fileheader:=new(bmpheader)

    fileheader.fh.typex:='BM'
    fileheader.fh.offbits:=bmpheader.bytes+palettebytes
    fileheader.fh.size:=fileheader.fh.offbits+framebytes
    fileheader.bh.size:=ws_bitmapinfoheader.bytes
    fileheader.bh.width:=bm.dimx
    fileheader.bh.height:=-bm.dimy
    fileheader.bh.bitcount:=bm.pixelbits
    fileheader.bh.planes:=1
    fileheader.bh.xpelspermetre:=11811      
    fileheader.bh.ypelspermetre:=11811
    fileheader.bh.clrused:=0

    writerandom(bmfile,&fileheader,0,bmpheader.bytes)

    if palettebytes then
        palette:=bmgetpalette(bm)
        reversepalette(palette)
        writerandom(bmfile,&palette,bmpheader.bytes,palettebytes)
    fi

    writerandom(bmfile,bm.pixelptr,fileheader.fh.offbits,framebytes)

    return closefile(bmfile)
end

func bmsavejpg(filename,bm)=
    w:=bm.dimx
    h:=bm.dimy
    pixelbytes:=bm.pixelbytes
    linebytes:=bm.linebytes

    p:=q:=malloc(pixelbytes*w*h)

    s:=makeref(bm.pixelptr,byte)

    to h do
        memcpy(q,s,w*pixelbytes)
        q:=q+bm.linebytes
        s:=s+w*pixelbytes
    od

    status:=imgsave_jpeg_bgr(filename,p,w,h,pixelbytes)

    free(p)

    return status
end

func bmsavepbm_p1p4(filename,bm,binary)=
return bmunimpl("bmsaveppm")
end

func bmsavepgm_p2p5(filename,bm,binary)=
return bmunimpl("bmsavepgm")
end

func bmsaveppm_p3p6(filename,bm,binary)=

    width:=bm.dimx
    height:=bm.dimy

    case bm.pixelbits
    when 24 then
    when 8 then
        return bmsavepgm_p2p5(filename,bm,binary)
    else
        return 0
    esac

    f:=createfile(changeext(filename,"ppm"))

    CPL "WRITEPPM",CHANGEEXT(FILENAME,"PPM")

    if not f then return 0 fi

    println @f,(binary|"P6"|"P3")
    println @f,width
    println @f,height
    println @f,"255"
    buffer:=data

    buffer:=malloc(bm.linebytes)
    if buffer=nil then return 0 fi
    buffer:=makeref(buffer,byte)

    linebytes:=width*3          

    for y:=0 to height-1 do
        memcpy(buffer,bmgetrowptr(bm,y),linebytes)
        p:=buffer                   
        to width do
            swap(p^,(p+2)^)
            p:=p+3
        od
        if binary then
            writebytes(f,buffer,linebytes)
        else
            p:=buffer
            to linebytes do
                print @f,p++^,," "
            od
            println @f
        fi
    od
    closefile(f)
    return 1
end

export func bmrotate(bm, angle)=
    case angle
    when 0 then return bmdupl(bm)
    when -90 then return bmrotleft90(bm)
    when +90 then return bmrotright90(bm)
    when 180 then return rot180(bm)
    esac
    return bmunimpl("bmrotate by "+tostr(angle))
end

export func bmrotleft90(bm)=
    case bm.pixelbits
    when 8 then return rotleft90_8(bm)
    when 24 then return rotleft90_24(bm)
    when 32 then return bmunimpl("ROTLEFT90/32")
    esac
    return nil
end

export func bmrotright90(bm)=
    case bm.pixelbits
    when 8 then return rotright90_8(bm)
    when 24 then return rotright90_24(bm)
    when 32 then return bmunimpl("ROTRIGHT90/32")
    esac
    return nil
end

export func rot180(bm)=
    newbm1:=bmfliphoz(bm)
    newbm2:=bmflipvert(newbm1)
    bmfree(newbm1)
    return newbm2
end

func rotleft90_8(bm)=
    w:=bm.dimx
    h:=bm.dimy
    linebytes:=bm.linebytes

    newbm:=bmcreate(8,h,w)

    for y:=0 to w-1 do
        q:=bmgetptr(bm,w-y-1,0)
        p:=bmgetrowptr(newbm,y)

        to h do
            p++^:=q^
            q:=q+linebytes
        od
    od

    bmduplpalette(newbm,bm)
    return newbm
end

func rotright90_8(bm)=
    w:=bm.dimx
    h:=bm.dimy
    linebytes:=bm.linebytes

    newbm:=bmcreate(8,h,w)

    for y:=0 to w-1 do
        q:=bmgetptr(bm,y,h-1)
        p:=bmgetrowptr(newbm,y)

        to h do
            p++^:=q^
            q:=q-linebytes
        od
    od

    bmduplpalette(newbm,bm)
    return newbm
end


func rotleft90_24(bm)=
    w:=bm.dimx
    h:=bm.dimy

    newbm:=bmcreate(24,h,w)

    for y:=0 to w-1 do
        q:=bmgetptr(bm,w-y-1,0)
        p:=bmgetrowptr(newbm,y)

        to h do
            p++^:=q^
            p++^:=(q+1)^
            p++^:=(q+2)^
            q:=q+bm.linebytes
        od
    od

    return newbm
end


func rotright90_24(bm)=
    w:=bm.dimx
    h:=bm.dimy

    newbm:=bmcreate(24,h,w)

    for y:=0 to w-1 do
        q:=bmgetptr(bm,y,h-1)
        p:=bmgetrowptr(newbm,y)

        to h do
            p++^:=q^
            p++^:=(q+1)^
            p++^:=(q+2)^
            q:=q-bm.linebytes
        od
    od

    return newbm
end

func rotate8(bm,angle)=
return bmunimpl("rotate8")
end

export func bmfliphoz(bm)=
    case bm.pixelbytes
    when 1 then return fliphoz8(bm)
    when 3 then return fliphoz24(bm)
    when 4 then return fliphoz32(bm)
    esac
    return nil
end

func fliphoz8(bm)=
    newbm:=bmdupl(bm)

    w:=newbm.dimx
    h:=newbm.dimy
    buffer:=makeref(malloc(bm.linebytes),byte)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        q:=p+w-1
        for x:=0 to w%2 do
            t:=p^
            p^:=q^
            q^:=t
            ++p; --q
        od
    od

    return newbm
end

func fliphoz24(bm)=
    newbm:=bmdupl(bm)

    w:=newbm.dimx
    h:=newbm.dimy
    buffer:=makeref(malloc(bm.linebytes),byte)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        memcpy(buffer,p,bm.linebytes)
        q:=buffer+(w-1)*3

        to w do
            p++^:=q++^
            p++^:=q++^
            p++^:=q^

            q:=q-5
        od
    od

    return newbm
end

func fliphoz32(bm)=
return bmunimpl("fliphoz_32")
end

export func bmflipvert(bm)=
    newbm:=bmdupl(bm)

    w:=newbm.dimx
    h:=newbm.dimy
    n:=bm.linebytes
    buffer:=makeref(malloc(n),byte)

    for y:=0 to h%2 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(newbm,h-1-y)
        memcpy(buffer,p,n)
        memcpy(p,q,n)
        memcpy(q,buffer,n)
    od
    free(buffer)

    return newbm
end

export func bmrepeat(bm,cols,rows)=
    w:=bm.dimx
    h:=bm.dimy
    newbm:=bmcreate(bm.pixelbits, w*cols, h*rows)
    linebytes:=bm.linebytes

    for y:=0 to h-1 do
        s:=bmgetrowptr(bm,y)
        for r:=0 to rows-1 do
            for c:=0 to cols-1 do
                memcpy(bmgetptr(newbm,c*w,r*h+y),s,linebytes)
            od
        od
    od

    if bm.pixelbits=8 then
        bmduplpalette(newbm,bm)
    fi

    return newbm
end

export func bmscale(bm, sx,?sy)=
    if sy.isvoid then sy:=sx fi
    case bm.pixelbits
    when 8 then return scalex8(bm,sx,sy)
    when 24,32 then return scalex24(bm,sx,sy)
    esac
    return nil
end

func scalex8(bm,sx,sy)=
    w:=bm.dimx
    h:=bm.dimy

    neww:=int(round(w*sx))
    newh:=int(round(h*sy))

    newbm:=bmcreate(24, neww,newh)
    return nil when not newbm

    bm24:=bmtorgb(bm,24)

    stretchblt(newbm.gdi.hdc,0,0,neww,newh,bm24.gdi.hdc,0,0,w,h, srccopy)

    if bm.paltype=greyscale_pal then
        newbm8:=bmgetplane(newbm,"R")
    else
        newbm8:=bmtopal(newbm)
    fi
    bmfree(newbm)
    
    return newbm8
end

func scalex24(bm,sx,sy)=
    w:=bm.dimx
    h:=bm.dimy

    neww:=int(round(w*sx))
    newh:=int(round(h*sy))

    if neww<8 or newh<8 then return nil fi

    newbm:=bmcreate(bm.pixelbits, neww,newh)
    if newbm then
        stretchblt(newbm.gdi.hdc,0,0,neww,newh,bm.gdi.hdc,0,0,w,h, srccopy)
    fi

    return newbm
end

func bmscaleupi8(bm,sx,sy)=
return bmunimpl("bmscaleupi8")
end

func bmscaleupi24(bm,sx,sy)=
return bmunimpl("bmscaleupi24")
end

func bmscaleupi32(bm,sx,sy)=
return bmunimpl("bmscaleupi32")
end

func bmscaledowni8(bm,sx,sy)=
return bmunimpl("bmscaledowni8")
end

func bmscaledowni24(bm,sx,sy)=
return bmunimpl("bmscaledowni24")
end

func bmscaledowni32(bm,sx,sy)=
return bmunimpl("bmscaledowni32")
end

export func bmneg(bm)=
    newbm:=bmdupl(bm)

    dx:=newbm.dimx-1
    dy:=newbm.dimy-1
    n:=newbm.linebytes
    do32:=0
    if n rem 4=0 then
        do32:=1
        n:=n%4
    fi

    for y:=0 to dy do
        if do32 then
            p:=makeref(bmgetrowptr(newbm,y),int32)
            to n do
                p++^ := p^ ixor 0xFFFFFFFF
            od
        else
            p:= bmgetrowptr(newbm,y)
            to n do
                p++^ := p^ ixor 255
            od
        fi
    od
    return newbm
end

export func bmmap(bm,map, channels="RGB")=
    if channels="" then channels:="RGB" fi

    case bm.pixelbits
    when 8 then
        return mapall(bm,map)
    when 24 then
        if channels="RGB" then
            return mapall(bm,map)
        fi
        return mapchan_24(bm,map,channels,0)
    when 32 then
        if channels="RGBA" then
            return mapall(bm,map)
        fi
        return mapchan_24(bm,map,channels,1)
    esac

    return nil
end

func mapall(bm,map)=
    newbm:=bmdupl(bm)
    p:=newbm.pixelptr
    to newbm.framebytes do
        p^:=map[p^]
        ++p
    od
    return newbm
end

func mapchan_24(bm,map,channels,alpha=0)=
    dored:="R" in channels
    dogreen:="G" in channels
    doblue:="B" in channels
    doalpha:="A" in channels

    newbm:=bmdupl(bm)

    for y:=0 to newbm.dimy-1 do
        p:=bmgetrowptr(newbm,y)
        to newbm.dimx do
            if doblue then p^:=map[p^] fi
            ++p
            if dogreen then p^:=map[p^] fi
            ++p
            if dored then p^:=map[p^] fi
            ++p
            if alpha then
                if doalpha then p^:=map[p^] fi
                ++p
            fi
        od
    od

    return newbm
end


export func bmbright(bm,dx,channels="RGB")=
    return bmunimpl("bmbright")
end

export func bmcont(bm,x,channels="RGB")=
    return bmunimpl("bmcont")
end

export func bmgamma(bm,x,channels="RGB")=
return bmunimpl("bmgamma")
end

export func bmtogrey(bm,destbits=24)=
    if destbits=0 then destbits:=bm.pixelbits fi
    case bm.pixelbits
    when 8 then
        case destbits
        when 8 then
            return pal8togrey8(bm)
        when 24 then
            cm:=pal8togrey8(bm)
            newbm:=grey8torgb24(cm)
            bmfree(cm)
            return newbm
        esac
    when 24,32 then
        case destbits
        when 8 then
            return rgb24togrey8(bm)
        when 24 then
            cm:=rgb24togrey8(bm)
            newbm:=grey8torgb24(cm)
            bmfree(cm)
            return newbm
        esac
    esac
CPL =BM.PIXELBITS, =DESTBITS
    return bmunimpl("bmtogrey bad combos")

end

func pal8togrey8(bm)=
    w:=bm.dimx
    h:=bm.dimy

    (rmap, gmap, bmap):=getlumtables()

    newbm:=bmcreate(8,w,h)
    pal:=bmgetpalette(bm)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(bm,y)

        to w do
            colour:=pal[q++^]
            r:=colour.[0..7]
            g:=colour.[8..15]
            b:=colour.[16..23]
            p++^:=rmap[r]+gmap[g]+bmap[b]
        od
    od

    return newbm
end

func pal8togrey24(bm)=
return bmunimpl("pal8togrey24")
end

func rgb24togrey8(bm)=
    qincr:=(bm.pixelbits=32)
    w:=bm.dimx
    h:=bm.dimy

    (rmap, gmap, bmap):=getlumtables()

    newbm:=bmcreate(8,w,h)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(bm,y)

        to w do
            b:=q++^; g:=q++^; r:=q++^
            p++^:=rmap[r]+gmap[g]+bmap[b]
            q:=q+qincr
        od
    od

    return newbm
end

func rgb24togrey24(bm)=
return bmunimpl("rgb24togrey24")
end

export func bmtorgb(bm,destbits=24)=
    if destbits=0 then destbits:=24 fi
    case bm.pixelbits
    when destbits then
        return bmdupl(bm)

    when 8 then
        case destbits
        when 8 then
            bmunimpl("8 to 8 bits rgb")
        when 24 then
            if bm.paltype=greyscale_pal then
                return grey8torgb24(bm)
            else
                return paltorgb24(bm)
            fi
        esac
    when 24 then
        if destbits=32 then
            return bmrgb24torgb32(bm)
        fi
    when 32 then
        if destbits=24 then
            return bmrgb32torgb24(bm)
        fi
    esac
CPL =BM.PIXELBITS, =DESTBITS
    return bmunimpl("bmtorgb bad combos")
end

func paltorgb24(bm)=
    w:=bm.dimx
    h:=bm.dimy

    newbm:=bmcreate(24,w,h)
    pal:=bmgetpalette(bm)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(bm,y)

        to w do
            colour:=pal[q++^]
            r:=colour.[0..7]
            g:=colour.[8..15]
            b:=colour.[16..23]

            p++^:=b
            p++^:=g
            p++^:=r
        od
    od

    return newbm
end

func grey8torgb24(bm)=
    w:=bm.dimx
    h:=bm.dimy

    newbm:=bmcreate(24,w,h)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(bm,y)

        to w do
            lum:=q++^
            p++^:=lum
            p++^:=lum
            p++^:=lum
        od
    od

    return newbm
end

export func bmrgb24torgb32(bm)=
    w:=bm.dimx
    h:=bm.dimy
    newbm:=bmcreate(32,w,h)

    for y:=0 to h-1 do
        q:=bmgetrowptr(bm,y)
        p:=bmgetrowptr(newbm,y)
        to w do
            p++^:=q++^
            p++^:=q++^
            p++^:=q++^
            p++^:=128
        od
    od
    return newbm
end

export func bmrgb32torgb24(bm)=
    w:=bm.dimx
    h:=bm.dimy
    newbm:=bmcreate(24,w,h)

    for y:=0 to h-1 do
        q:=bmgetrowptr(bm,y)
        p:=bmgetrowptr(newbm,y)
        to w do
            p++^:=q++^
            p++^:=q++^
            p++^:=q++^
            q++
        od
    od
    return newbm
end

export func bmtopal(bm)=
    if bm.pixelbits=8 then return bmdupl(bm) fi
    qincr:=(bm.pixelbits=32)
    w:=bm.dimx
    h:=bm.dimy

    newbm:=bmcreate(8,w,h)

    pal:=new(array,int32,0..255)

    for r:=0 to 7 do
        for g:=0 to 7 do
            for b:=0 to 3 do
                index:=r<<5+g<<2+b
                pal[index]:=r<<5+g<<13+b<<22
            od
        od
    od
    bmputpalette(newbm,pal)
    bm.paltype:=colour_pal

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)
        q:=bmgetrowptr(bm,y)


        to w do
            b:=q++^; g:=q++^; r:=q++^

            p++^:=r>>5<<5 + g>>5<<2 + b>>6
            q:=q+qincr
        od
    od

    return newbm

end

export func bmsplittorgb(bm,greydest=1)=

    if bm.pixelbits<24 then
        return ()
    fi
    channels:=bm.pixelbytes

    w:=bm.dimx
    h:=bm.dimy

    pal:=new(array,int32,0..255)
    images::=()

    for offset:=channels-1 downto 0 do

        newbm:=bmcreate(8,w,h)

        for y:=0 to h-1 do
            q:=bmgetrowptr(bm,y)+offset
            p:=bmgetrowptr(newbm,y)
            to w do
                p++^:=q^
                q:=q+channels
            od
        od

        if not greydest then
            colour:=0
            incr:=(3-offset|0x00'00'01,0x00'01'00,0x01'00'00|0x01'01'01)
            for i:=0 to 255 do
                pal[i]:=colour
                colour+:=incr
            od
            bmputpalette(newbm,pal)
            newbm.paltype:=tinted_pal
        fi

        images append:=newbm
    od

    if images.len=4 then
        return (images[2],images[3],images[4],images[1])
    else
        return images
    fi
end

export func bmsplittoyuv(bm)=

    needfree:=0
    case bm.pixelbits
    when 24 then
    when 32 then
        bm:=bmtorgb(bm,24)
        needfree:=1
    else
        return nil
    esac

    greybm:=bmtogrey(bm,8)

    w:=bm.dimx
    h:=bm.dimy


    umap:=new(list,-255..255)
    vmap:=new(list,-255..255)
    for i:=-255 to 255 do
        umap[i]:=int(round(0.492*(i)+128))
        vmap[i]:=int(round(0.702*(i)+128))
    od

    ubm:=bmcreate(8,w,h)
    vbm:=bmcreate(8,w,h)
    for c:=1 to 2 do
        if c=1 then
            offset:=0
            map:=umap
        else
            offset:=2
            map:=vmap
        fi

        for yy:=0 to h-1 do
            py:=bmgetrowptr(greybm,yy)
            p:=bmgetrowptr(bm,yy)           

            pu:=bmgetrowptr(ubm,yy)
            pv:=bmgetrowptr(vbm,yy)
            to w do
                y:=py++^
                r:=(p+2)^
                b:=p^
                pu++^:=umap[b-y]
                pv++^:=vmap[r-y]

                p:=p+3
            od
        od
    od

    pal:=new(array,int32,0..256)
    colour:=0
    pal[128]:=0
    for i:=1 to 127 do
        colour+:=0x020202
        pal [i+128]:=colour
        pal [128-i]:=colour
    od
    pal[256]:=uv_pal
    bmputpalette(ubm,pal)
    bmputpalette(vbm,pal)

    if needfree then
        bmfree(bm)
    fi

    return (greybm,ubm,vbm)
end

export func bmgetplane(bm,plane)=

    incr:=bm.pixelbytes
    if plane.len<>1 or bm.pixelbytes<3 then
        return nil
    fi

    case asc(plane)
    when 'R' then offset:=2
    when 'G' then offset:=1
    when 'B' then offset:=0
    when 'A' then offset:=3
    else return nil
    esac

    w:=bm.dimx
    h:=bm.dimy
    newbm:=bmcreate(8,w,h)

    for y:=0 to h-1 do
        q:=bmgetrowptr(bm,y)+offset
        p:=bmgetrowptr(newbm,y)
        to w do
            p++^:=q^
            q:=q+incr
        od
    od

    return newbm
end

export func bmjoinrgb(redbm,greenbm,bluebm,alphabm=nil)=

    w:=redbm.dimx
    h:=redbm.dimy

    newbm:=bmcreate((alphabm|32|24),w,h)

    for y:=0 to h-1 do
        p:=bmgetrowptr(newbm,y)

        r:=bmgetrowptr(redbm,y)
        g:=bmgetrowptr(greenbm,y)
        b:=bmgetrowptr(bluebm,y)

        if alphabm then
            a:=bmgetrowptr(alphabm,y)
            to w do
                p++^:=b++^
                p++^:=g++^
                p++^:=r++^
                p++^:=a++^
            od
        else
            to w do
                p++^:=b++^
                p++^:=g++^
                p++^:=r++^
            od
        fi
    od

    return newbm
end

export func bmjoinyuv(ybm,ubm,vbm)=

    if ybm.pixelbits<>8 then
        return nil
    fi

    w:=ybm.dimx
    h:=ybm.dimy

    v1425map:=new(list,0..255)
    v726map:=new(list,0..255)
    u395map:=new(list,0..255)
    u2032map:=new(list,0..255)

    for i:=0 to 255 do
        v1425map[i]:=int(round(1.425*(i-128)))
        v726map[i]:=int(round(0.726*(i-128)))
        u395map[i]:=int(round(0.395*(i-128)))
        u2032map[i]:=int(round(2.032*(i-128)))
    od

    newbm:=bmcreate(24,w,h)

    for yy:=0 to h-1 do
        p:=bmgetrowptr(newbm,yy)
        qy:=bmgetrowptr(ybm,yy)
        qu:=bmgetrowptr(ubm,yy)
        qv:=bmgetrowptr(vbm,yy)

        FOR X:=0 TO W-1 DO
            y:=qy++^
            r:=y+v1425map[qv^]
            g:=y-u395map[qu^]-v726map[qv^]
            b:=y+u2032map[qu^]
            ++qu
            ++qv
            p++^:=clamp(b,0,255)
            p++^:=clamp(g,0,255)
            p++^:=clamp(r,0,255)
        od
    od
    return newbm
end

export func bmblur(bm,n)=
    case bm.pixelbits
    when 8 then
        return blur8(bm,n)
    when 24 then
        return blur24(bm,n)
    when 32 then
        return blur32(bm,n)
    esac
    return nil
end

func blur8(bm,n)=
    shift:=shifts{n,1}

    newbm:=bmdupl(bm)
    iblurhoz8(newbm,n)

    newbm2:=rotleft90_8(newbm)
    iblurhoz8(newbm2,n)

    newbm3:=rotright90_8(newbm2)
    bmfree(newbm)
    bmfree(newbm2)

    bmduplpalette(newbm3,bm)
    return newbm3
end

func blur24(bm,n)=
    (r,g,b):=bmsplittorgb(bm)

    r2:=bmblur(r,n)
    g2:=bmblur(g,n)
    b2:=bmblur(b,n)

    newbm:=bmjoinrgb(r2,g2,b2)
    bmfree(r2)
    bmfree(g2)
    bmfree(b2)

    return newbm
end

func blur32(bm,n)=
return bmunimpl("blur32")
end

proc iblurhoz8(bm,n)=
    shift:=shifts{n,1}

    w:=bm.dimx
    h:=bm.dimy

    for y:=0 to h-1 do
        p:=bmgetrowptr(bm,y)
        to w-n-1 do
            sum:=0
            q:=p
            to n do
                sum+:=q++^
            od
            p++^:=sum>>shift
        od
    od
end

func blurhoz24(bm,n)=
return bmunimpl("blurhoz24")
end

func blurhoz32(bm,n)=
return bmunimpl("blurhoz32")
end

export func bmsharpen(bm,n=0)=
    case bm.pixelbits
    when 8 then
        return sharpen8(bm,n)
    when 24 then
        return sharpen24(bm,n)
    when 32 then
        return sharpen32(bm,n)
    esac
    return nil
end

export func sharpen8(bm,n)=

    w:=bm.dimx
    h:=bm.dimy

    newbm:=bmdupl(bm)

    for y:=1 to h-2 do
        p:=bmgetptr(newbm,1,y)

        q:=bmgetptr(bm,1,y-1)
        r:=bmgetptr(bm,1,y)
        s:=bmgetptr(bm,1,y+1)

        to w-2 do
            a:=(q-1)^
            b:=q^
            c:=(q+1)^
            d:=(r-1)^
            e:=r^
            f:=(r+1)^
            g:=(s-1)^
            h:=s^
            i:=(s+1)^


            sum:=e*8-a-b-c-d-f-g-h-i
            p^:=clamp(p^+sum%8,0,255)


            ++p
            ++q
            ++r
            ++s
        od
    od

    return newbm

end

export func sharpen24(bm,n)=
    (r,g,b):=bmsplittorgb(bm)

    r2:=bmsharpen(r,n)
    g2:=bmsharpen(g,n)
    b2:=bmsharpen(b,n)

    newbm:=bmjoinrgb(r2,g2,b2)
    bmfree(r2)
    bmfree(g2)
    bmfree(b2)

    return newbm
end

export func sharpen32(bm,n)=
return bmunimpl("bmsharpen32")
end

func getlumtables=
    rmap:=makescalemap(0.299)
    gmap:=makescalemap(0.587)
    bmap:=makescalemap(0.111)
    return (rmap, gmap, bmap)
end
=== console.q 0 1 38/43 ===
import sys
import clib

import winconsts
import winapi

export const vklbutton=1        
export const vkrbutton=2
export const vkmbutton=4        
export const vkbackspace=8
export const vktab=9
export const vkclear=12
export const vkenter=13
export const vkshift=16
export const vkctrl=17
export const vkalt=18
export const vkbreak=19
export const vkcapslock=20
export const vkrctrl=22
export const vkinslock=24
export const vkescape=27
export const vkspace=32
export const vkpageup=33
export const vkpagedown=34
export const vkend=35
export const vkhome=36
export const vkleft=37
export const vkup=38
export const vkright=39
export const vkdown=40
export const vkinsert=45
export const vkdelete=46
export const vkhelp=47
export const vk0='0'
export const vka='A'
export const vkwindows=91
export const vkrightbutton=93
export const vknumpad0=96       
export const vkmul=106
export const vkadd=107
export const vksub=109
export const vkdecimal=110
export const vkdiv=111
export const vkf1=112
export const vkf2=113
export const vkf3=114
export const vkf4=115
export const vkf5=116
export const vkf6=117
export const vkf7=118
export const vkf8=119
export const vkf9=120
export const vkf10=121
export const vkf11=122
export const vkf12=123
export const vklshift=160
export const vkrshift=161
export const vklcontrol=162
export const vkrcontrol=163
export const vklalt=164
export const vkralt=165

export const vkminus=189
export const vkequals=187
export const vklsq=219
export const vkrsq=221
export const vksemi=186
export const vkquote=192
export const vkhash=222
export const vkcomma=188
export const vkperiod=190
export const vkslash=191
export const vkbackslash=220
export const vkbackquote=223

export const con_black=0
export const con_dkblue=1
export const con_dkred=2
export const con_dkmagenta=3
export const con_dkgreen=4
export const con_dkcyan=5
export const con_dkyellow=6
export const con_dkgrey=7
export const con_grey=8
export const con_blue=9
export const con_red=10
export const con_magenta=11
export const con_green=12
export const con_cyan=13
export const con_yellow=14
export const con_white=15


export record winrec =
    var posx,posy
    var cols,rows
    var fgnd,bgnd           

    var columns         
    var itemcols            
    var pagesize            

    var name

    var hdata           
end

export var wconscreen
export var screencols,screenrows

export var chardata         
export var attrdata         

export var defscreenfgnd=con_black
export var defscreenbgnd=con_grey
export var rlkey=0      
export var rlbuffer         

var cmdindex,ncmds
var cmdhistory

export const capsmask  = 0x8        
export const altmask   = 0x4
export const ctrlmask  = 0x2
export const shiftmask = 0x1

export const capsbit=3
export const altbit=2
export const ctrlbit=1
export const shiftbit=0

var keypending=0
var lastkey
var pendkey
export var hconsole, hconsolein
var colourpalette

export var currbgnd=-1,currfgnd=-1

export var screencolour=con_dkred..con_grey

export VAR SUPPRESS=0

VAR ALLCHARS

proc START=
        init()
    end

proc main=
    init()
    settitle("New Title")




a:=rkey(10,20,30)
end

proc keyscreentest=
    (cols,rows):=(screencols, screenrows)
    CPL =COLS,=ROWS

    row:=rows%2
    col:=cols%2
    ch:="X"

    setcolour(6,1)

    do
        setpos(col,row)
        cp ch
        setpos(col,row)
        k:=getkey().keycode
        case k
        when 27 then
            exit
        when vkleft then col:=max(1,col-1)
        when vkright then col:=min(cols,col+1)
        when vkup then row:=max(1,row-1)
        when vkdown then row:=min(rows,row+1)
        esac
    od


end

export func makerspoint(x,y)=
    return y<<16 ior x
end

export proc setpos(col,row)=
    setconsolecursorposition(hconsole,makerspoint(col-1,row-1))
end

export func getpos=
    info:=new(ws_console)
    getconsolescreenbufferinfo(hconsole,&info)
    return (info.pos.x+1,info.pos.y+1)
end

export proc init(cols=100)=

    cmdhistory::=() 
    ncmds:=cmdhistory.upb
    cmdindex:=0


    hconsole:=getstdhandle(-11)
    hconsolein:=getstdhandle(-10)
    lastkey:=new(ws_keyevent)
    lastkey.repeatcount:=0
    pendkey:=new(ws_keyevent)

    setdims(cols,60)

    getdims()


    wconscreen:=makewin((1,1),(screencols,screenrows),defscreencolour)

    colourpalette:=new(ws_palette16)

    setstdpalette()
end

export func setcursor(?visible)=
    cursor:=new(ws_cursor)
    getconsolecursorinfo(hconsole,&cursor)

    if visible.defined then
        cursor.visible:=visible
        setconsolecursorinfo(hconsole,&cursor)
    fi
    return cursor.visible
end

export proc setcolour(fgnd,bgnd)=

    if fgnd=currfgnd and bgnd=currbgnd then
        return
    fi

    currfgnd:=fgnd
    currbgnd:=bgnd

    setconsoletextattribute(hconsole,(bgnd*16+fgnd))
end

export proc settitle(caption)=
    setconsoletitle(caption)
end

export func getkeychar=
    return waitkey()
end

export func getkey2=

    return getchx()

    k:=getchx()         

    key:=new(rkey)          
    key.charcode:=k iand 65535
    key.shift:=k>>24
    key.keycode:=k.[23..16]

    return key
end

export func getkey=
    do
        k:=getkey2()
        case k.keycode
        when vkshift,vkctrl,vkalt,vkcapslock then
        else
            exit
        esac
    od
    return k
end

export func keyready=
    return testkey()
end

export proc wshowtext(w,s,?col,?row)=
    if col.defined then
        showtext(s,w.posx+col-1,w.posy+row-1)
    else
        showtext(s)
    fi
end

export proc showtext(s,?x,?y)=

    if x.defined then
        setpos(x,y)
    fi

    count:=0
    if s then
        if not suppress then
            writeconsole(hconsole,s,s.len,&count,nil)
        fi
    fi
end

proc setwindowsize(cols,rows)=
    r:=new(ws_srect)
    r.leftx:=0
    r.rightx:=cols-1
    r.top:=0
    r.bottom:=rows-1
    if not setconsolewindowinfo(hconsole,1,&r) then
    fi
end

export proc setdims(cols,rows)=

    maxcol:=cols
    maxrow:=rows

    info:=new(ws_console)
    oldscreenattributes:=info.attributes
    oldscreensize:=info.size

    oldcols:=info.window.rightx-info.window.leftx+1
    oldrows:=info.window.bottom-info.window.top+1

    IF OLDSCREENSIZE.X>COLS OR OLDSCREENSIZE.Y>ROWS THEN    
        setwindowsize(oldscreensize.x min cols, oldscreensize.y min rows)
    fi

    if setconsolescreenbuffersize(hconsole,rows<<16+cols)=0 then
    fi

    setwindowsize(cols,rows)

    wscreencols:=cols
    wscreenrows:=rows

    cursor:=new(ws_cursor)
    cursor.size:=10
    cursor.visible:=1
end

export proc setpalette(index,colour)=
    colourpalette[index]:=colour
end

export proc writepalette=
    r:=new(ws_consoleex)
    r.recsize:=ws_consoleex.bytes
    X:=getconsolescreenbufferinfoex(hconsole,&r)

    r.palette:=colourpalette

    R.WINDOW.RIGHTX:=R.WINDOW.RIGHTX+1      
    R.WINDOW.BOTTOM:=R.WINDOW.BOTTOM+1

    X:=setconsolescreenbufferinfoex(hconsole,&r)

end

proc setstdpalette=

    cols:=(
    (0,     0,      0),         
    (0,     0,      128),       
    (128,   0,      0),         
    (128,   0,      128),       
    (0,     128,    0),         
    (0,     128,    128),       
    (128,   128,    0),         
    (128,   128,    128),       
    (192,   192,    192),       
    (0,     0,      192),       
    (192,   0,      0),         
    (192,   0,      192),       
    (0,     192,    0),         
    (0,     192,    192),       
    (192,   192,    0),         
    (255,   255,    255))       

    forall i,c in cols do
        setpalette(i-1,c[3]<<16+c[2]<<8+c[1])
    od
    writepalette()
end

proc getdims=
    info:=new(ws_console)
    getconsolescreenbufferinfo(hconsole,&info)

    screencols:=info.window.rightx-info.window.leftx+1
    screenrows:=info.window.bottom-info.window.top+1
end

export func getchx=
    const rightaltmask  = 1             
    const leftaltmask   = 2
    const leftctrlmask  = 8
    const rightctrlmask = 4
    const shiftmask     = 16
    const capsmask      = 128
    const scrollmask    = 64

    const leftctrlbit   = 3     
    const rightctrlbit  = 2

    if keypending then
        lastkey:=pendkey
        keypending:=0
    else
        if lastkey.repeatcount=0 then
            repeat
                count:=0
                readconsoleinput(hconsolein,&lastkey,1,&count)
            until lastkey.eventtype=1 and lastkey.keydown=1
        fi
    fi

    altdown     := (lastkey.controlkeystate iand (leftaltmask ior rightaltmask)|1|0)
    ctrldown    := (lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask)|1|0)
    shiftdown   := (lastkey.controlkeystate iand shiftmask|1|0)
    capslock    := (lastkey.controlkeystate iand capsmask|1|0)

    lastkey.repeatcount:=lastkey.repeatcount-1

    charcode:=lastkey.asciichar
    keycode:=lastkey.virtualkeycode iand 255

    if altdown and ctrldown and charcode=166 then
        altdown:=ctrldown:=0;
    else
        if altdown or ctrldown then
            charcode:=0;
            if keycode>='A' and keycode<= 'Z' then
                charcode:=keycode-'@'
            fi
        fi
    fi

    keyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown

    keyshift.[4]:=lastkey.controlkeystate.[leftctrlbit]     
    keyshift.[5]:=lastkey.controlkeystate.[rightctrlbit]


    switch charcode
    when 'A'..'Z','a'..'z','0'..'9' then
    when 8,9,13,27,' ','`' then
    when 0 then             
    else
        keycode:=0
    endswitch

    return rkey(charcode,keycode,keyshift)

end

export proc flushkeyboard=
    flushconsoleinputbuffer(hconsolein)
end

export proc w_writeconsolerow(text, attributes, length, row)=
    buffersize:=1<<16+length
    coord:=0

    box:=ws_srect(0,row-1,length-1,row-1)

    buffer:=new(array,ws_charinfo,length)

    for i:=1 to length do
        x:=new(ws_charinfo)
        x.asciichar  := text.[i]
        x.attributes := attributes.[i]
        buffer[i]:=x
    od

    writeconsoleoutputa(hconsole, &buffer,buffersize,coord,&box)
end

export func setclipboard(s)=
    const ghnd=2 + 0x40

    if openclipboard(nil)=0 then
        return 0
    fi

    emptyclipboard()

    if s<>"" then
        h:=globalalloc(ghnd,s.len+1)
        p:=globallock(h)

        memcpy(p,&s,s.len+1)
    globalunlock(h)
        setclipboarddata(cf_text,h)
    fi

    closeclipboard()

    return 1
end

export func getclipboard=

    if openclipboard(nil)=0 then
        return ""
    fi

    htext:=getclipboarddata(cf_text)

    if not htext then
        return ""
    fi

    size:=globalsize(htext)     

    p:=globallock(htext)
    s:=makestr(p,size-1)        

    globalunlock(htext)

    closeclipboard()
    return s
end

export func makewin(pos, dims, ?fgnd,?bgnd,name="Anon")=

    w:=new(winrec)
    w.posx:=pos[1]
    w.posy:=pos[2]
    w.cols:=dims[1]
    w.rows:=dims[2]
    w.columns:=1
    if dims.len>=3 then
        w.columns:=dims[3]
    fi


    w.itemcols:=w.cols%w.columns
    w.pagesize:=w.rows*w.columns
    w.hdata:=nil

    w.fgnd:=fgnd
    w.bgnd:=bgnd
    w.name:=name

    return w
end

export proc clearwin(w)=
    spaces:=" "*w.cols

    setcolour(w.fgnd,w.bgnd)
    for i:=1 to w.rows do
        showtext(spaces,w.posx,w.posy+i-1)
    od
    setpos(w.posx,w.posy)
end

export proc wsetpos(w,col,row)=
    setpos(w.posx+col-1,w.posy+row-1)
end

export proc wshowtext_b(w,s,col,fgnd,bgnd)=

    length:=s.len
    offset:=w.posx-1    

    chardata.[(col+offset)..(col-1+length+offset)]:=s

    attr:=bgnd<<4+fgnd

    attrdata.[(col+offset)..(col-1+length+offset)]:=chr(attr)*length
end

export proc updateconsolerow(row)=
    w_writeconsolerow(chardata,attrdata,screencols,row)
end

export func getkeyname(key)=
    case key.keycode
    when vkleft then name:="left"
    when vkright then name:="right"
    when vkup then name:="up"
    when vkdown then name:="down"
    when vkpageup then name:="pageup"
    when vkpagedown then name:="pagedown"
    when vkhome then name:="home"
    when vkend then name:="end"
    when vkinsert then name:="insert"
    when vkdelete then name:="delete"
    when vktab then name:="tab"
    when vkescape then name:="escape"
    when vkbackspace then name:="backspace"
    when vkenter then name:="enter"
    when vkf1..vkf12 then name:="f"+tostr(key.keycode-vkf1+1)
    when vkspace then name:="space"
    else
        if key.charcode in [1..26] then 
            name:=chr(key.charcode+'a'-1)
        elsif key.charcode in ['!','"','£','$','%','^','&','*','(',')','-','_','+','=','[',']',
        '{','}',':',';','\'','@','~','#','<','>',',','.','/','¬','¦','|','\\','?'] then
            name:=chr(key.charcode)
            key.shift iand:=inot shiftmask      

        elsif key.keycode in ['A'..'Z','0'..'9'] then
            if (key.shift iand (ctrlmask ior altmask))=0 then
                name:=chr(key.charcode)
                key.shift iand:=inot shiftmask
            else
                name:=convlc(chr(key.keycode))
            fi
        elsif key.keycode in (186..223) then
            case key.keycode
            when vkminus then name:="-"
            when vkequals then name:="="
            when vklsq then name:="["
            when vkrsq then name:="]"
            when vksemi then name:=";"
            when vkquote then name:="'"
            when vkhash then name:="#"
            when vkcomma then name:=","
            when vkperiod then name:="."
            when vkslash then name:="/"
            when vkbackslash then name:="\\"
            when vkbackquote then name:="`"
            else
                return "?"
            esac
        else
            return "?"
        fi
    esac

    prefix::="*"
    if key.shift iand shiftmask then prefix+:="s" fi
    if key.shift iand ctrlmask then prefix+:="c" fi
    if key.shift iand altmask then prefix+:="a" fi
    return prefix+name

end

export func keynametokey(name)=
    charcode:=shift:=keycode:=0

    name:=rightstr(name,-1)     

    if name.len=1 then      
        charcode:=asc(name)
        goto simplekey

    else                
        while name.len>1 do
            case leftstr(name)
            when "s" then
                shift ior:=shiftmask
                name:=rightstr(name,-1)
            when "c" then
                shift ior:=ctrlmask
                name:=rightstr(name,-1)
            when "a" then
                shift ior:=altmask
                name:=rightstr(name,-1)
            else
                exit
            esac
        od

        case name
        when "left" then keycode:=vkleft
        when "right" then keycode:=vkright
        when "up" then keycode:=vkup
        when "down" then keycode:=vkdown
        when "pageup" then keycode:=vkpageup
        when "pagedown" then keycode:=vkpagedown
        when "home" then keycode:=vkhome
        when "end" then keycode:=vkend
        when "insert" then keycode:=vkinsert
        when "delete" then keycode:=vkdelete
        when "tab" then keycode:=charcode:=vktab
        when "escape" then keycode:=vkescape
        when "backspace" then keycode:=charcode:=vkbackspace
        when "enter" then keycode:=charcode:=vkenter
        when "space" then keycode:=charcode:=vkspace
        else
            if name.len>=2 and leftstr(name)="f" then   
                keycode:=vkf1+strtoval(rightstr(name,-1))-1
            elsif name.len=1 then               
    simplekey:
                c:=asc(name)
                case c
                when ['A'..'Z'] then
                    keycode:=c
                when ['a'..'z'] then
                    keycode:=c-' '
                when ['0'..'9'] then
                    keycode:=c
                when '-','_' then keycode:=vkminus
                when '=','+' then keycode:=vkequals
                when '[','{' then keycode:=vklsq
                when ']','}' then keycode:=vkrsq
                when ';',':' then keycode:=vksemi
                when '\'','@' then keycode:=vkquote
                when ',','<' then keycode:=vkcomma
                when '.','>' then keycode:=vkperiod
                when '/','?' then keycode:=vkslash
                when '\\','|' then keycode:=vkbackslash
                when '`','¬' then keycode:=vkbackquote
                when '#','~' then keycode:=vkhash
                when '!' then keycode:='1'
                when '"' then keycode:='2'
                when '£' then keycode:='3'
                when '$' then keycode:='4'
                when '%' then keycode:='5'
                when '^' then keycode:='6'
                when '&' then keycode:='7'
                when '*' then keycode:='8'
                when '(' then keycode:='9'
                when ')' then keycode:='0'
                else
                    pcerror("keynametokey")
                end
            fi
        esac
    fi

    if shift iand (altmask ior ctrlmask) then
        charcode:=0
        if keycode in 'A'..'Z' then
            charcode:=keycode-'@'
        fi
    fi

    key:=new(rkey)          
    key.charcode:=charcode
    key.shift:=shift
    key.keycode:=keycode
    return key
end

export proc clearscreen(?bgnd,?fgnd)=

if bgnd.isvoid then bgnd:=defscreenbgnd fi
if fgnd.isvoid then fgnd:=defscreenfgnd fi
setcolour(fgnd,bgnd)

for i:=1 to screenrows do
    setpos(1,i)
    showtext(" "*screencols)
od
setpos(1,1)
end

export func readline(?cmdline,donewline=1)=


    buffer:=""
    nchars:=0

    (startx,starty):=(getpos())

    pos:=0      

    reenter:
    if cmdline.defined and cmdline<>"" then
        buffer:=cmdline
    reenter2:
        pos:=nchars:=buffer.len
    fi

    do
        rlkey:=0            
        setpos(startx,starty)
        print buffer
        setpos(startx+pos,starty)

        key:=getkey()
        keycode:=key.keycode
        keyshift:=key.shift

        case keycode
        when vkpageup,vkpagedown,vkup,vkdown,vkinsert,vkf1..vkf12 then

    dospecial:
        rlbuffer:=buffer
            oldbufferlen:=buffer.len        
            buffer:=getkeyname(key)
            rlkey:=key              
            exit

        when vkleft then
            if buffer="" then goto dospecial fi
            if (keyshift iand 7) then goto dospecial fi

            if pos>0 then
                --pos
            fi

        when vkhome then
            if buffer="" then goto dospecial fi
            if (keyshift iand 7) then goto dospecial fi
            pos:=0

        when vkend then
            if buffer="" then goto dospecial fi
            if (keyshift iand 7) then goto dospecial fi
            pos:=nchars

        when vkright then
            if buffer="" then goto dospecial fi
            if (keyshift iand 7) then goto dospecial fi
            if pos<nchars then
                ++pos
            fi

        when vkenter then

            exit

        when vkbackspace then

            if (keyshift iand 7) then goto dospecial fi
            if nchars then
                setpos(startx,starty)
                print " "*buffer.len

                case pos
                when 0 then         
                when nchars then        
                    buffer:=leftstr(buffer,-1)
                    --nchars
                    --pos
                else                
                    buffer:=leftstr(buffer,pos-1)+rightstr(buffer,-(pos))
                    --nchars
                    --pos
                esac

            fi

        when vkdelete then
            if (keyshift iand 7) then goto dospecial fi
            if nchars and nchars=pos then
                goto delline
            fi
            if nchars=0 then
                goto dospecial
            fi
            if nchars then
                setpos(startx,starty)
                print " "*buffer.len

                case pos
                when nchars then        
                else                
                    buffer:=leftstr(buffer,pos)+rightstr(buffer,-(pos+1))
                    --nchars
                esac

            fi

        when vkescape then
            if nchars=0 then
                goto dospecial
            fi
    delline:
            setpos(startx,starty)
            print " "*buffer.len

            buffer:=""
            nchars:=pos:=0

        when vktab then
            goto normalkey

        else
    normalkey:
            if (key.charcode>=' ' or key.charcode=9) then
                if pos=0 then
                    buffer:=chr(key.charcode)+buffer
                elsif pos=nchars then
                    buffer:=buffer+chr(key.charcode)
                else
                    buffer:=leftstr(buffer,pos)+chr(key.charcode)+rightstr(buffer,-(pos))
                fi
                ++nchars
                ++pos
            else
                GOTO DOSPECIAL
                print "<",keycode,key.charcode,">"
            fi

        esac
    od

    case buffer
    when "*cup","*cdown" then
        if ncmds then
            setpos(startx,starty)
            print " "*oldbufferlen

            if cmdindex=0 then      
                cmdline:=cmdhistory[ncmds]
                cmdindex:=ncmds
                goto reenter
            fi

            if buffer="*cup" and cmdindex>1 then
                --cmdindex
            elsif buffer="*cdown" and cmdindex<ncmds then
                ++cmdindex
            fi
            cmdline:=cmdhistory[cmdindex]
            goto reenter
        fi
        buffer:=""
        goto reenter2
    esac

    if buffer.len>1 and leftstr(buffer)<>"*" then
        if ncmds=0 or cmdhistory[ncmds]<>buffer then
            cmdhistory[++ncmds]:=buffer
        fi
        cmdindex:=0
    fi

    if donewline then println fi

    return sreadln(buffer)
end

export proc wsetcolumns(w,columns)=
    w.columns:=columns
    w.itemcols:=w.cols%w.columns
    w.pagesize:=w.rows*w.columns
end

=== winconsts.q 0 1 39/43 ===

global const driverversion =  0
global const technology =  2
global const horzsize =  4
global const vertsize =  6
global const horzres =  8
global const vertres =  10
global const bitspixel =  12
global const bitplanes =  14
global const numbrushes =  16
global const numpens =  18
global const nummarkers =  20
global const numfonts =  22
global const numcolours =  24
global const pdevicesize =  26
global const curvecaps =  28
global const linecaps =  30
global const polygonalcaps =  32
global const textcaps =  34
global const clipcaps =  36
global const rastercaps =  38
global const aspectx =  40
global const aspecty =  42
global const aspectxy =  44
global const logpixelsx =  88
global const logpixelsy =  90
global const sizepalette =  104
global const numreserved =  106
global const colourres =  108
global const physicalwidth =  110
global const physicalheight =  111
global const physicaloffsetx =  112
global const physicaloffsety =  113
global const scalingfactorx =  114
global const scalingfactory =  115
global const fw_dontcare =  0
global const fw_thin =  100
global const fw_extralight =  200
global const fw_ultralight =  200
global const fw_light =  300
global const fw_normal =  400
global const fw_regular =  400
global const fw_medium =  500
global const fw_semibold =  600
global const fw_demibold =  600
global const fw_bold =  700
global const fw_extrabold =  800
global const fw_ultrabold =  800
global const fw_heavy =  900
global const fw_black =  900
global const cs_vredraw =  1
global const cs_hredraw =  2
global const cs_keycvtwindow =  4
global const cs_dblclks =  8
global const cs_owndc =  32
global const cs_classdc =  64
global const cs_parentdc =  128
global const cs_nokeycvt =  256
global const cs_noclose =  512
global const cs_savebits =  2048
global const cs_bytealignclient =  4096
global const cs_bytealignwindow =  8192
global const cs_publicclass =  16384
global const sw_hide =  0
global const sw_shownormal =  1
global const sw_normal =  1
global const sw_showminimized =  2
global const sw_showmaximized =  3
global const sw_maximize =  3
global const sw_shownoactivate =  4
global const sw_show =  5
global const sw_minimize =  6
global const sw_showminnoactive =  7
global const sw_showna =  8
global const sw_restore =  9
global const sw_showdefault =  10
global const sw_max =  10
global const pm_noremove =  0
global const pm_remove =  1
global const pm_noyield =  2
global const wm_null =  0
global const wm_create =  1
global const wm_destroy =  2
global const wm_move =  3
global const wm_size =  5
global const wm_activate =  6
global const wa_inactive =  0
global const wa_active =  1
global const wa_clickactive =  2
global const wm_setfocus =  7
global const wm_killfocus =  8
global const wm_enable =  10
global const wm_setredraw =  11
global const wm_settext =  12
global const wm_gettext =  13
global const wm_gettextlength =  14
global const wm_paint =  15
global const wm_close =  16
global const wm_queryendsession =  17
global const wm_quit =  18
global const wm_queryopen =  19
global const wm_erasebkgnd =  20
global const wm_syscolourchange =  21
global const wm_endsession =  22
global const wm_showwindow =  24
global const wm_wininichange =  26
global const wm_devmodechange =  27
global const wm_activateapp =  28
global const wm_fontchange =  29
global const wm_timechange =  30
global const wm_cancelmode =  31
global const wm_setcursor =  32
global const wm_mouseactivate =  33
global const wm_childactivate =  34
global const wm_queuesync =  35
global const wm_getminmaxinfo =  36
global const wm_drawitem =  43
global const wm_notify =  78
global const wm_contextmenu =  123
global const wm_geticon =  127
global const wm_seticon =  128
global const wm_nchittest =  132

global const wm_nclbuttondown   = 161
global const wm_nclbuttonup = 162
global const wm_nclbuttondblclick   = 163

global const wm_menurbuttonup   = 290

global const wm_parentnotify =  528
global const wm_dropfiles =  563
global const wm_enteridle =  289
global const wm_user =  1024
global const wm_mdicreate =  544
global const wm_mdidestroy =  545
global const wm_mdiactivate =  546
global const wm_mdirestore =  547
global const wm_mdinext =  548
global const wm_mdimaximize =  549
global const wm_mditile =  550
global const wm_mdicascade =  551
global const wm_mdiiconarange =  552
global const wm_mdigetactive =  553
global const wm_mdisetmenu =  560
global const wm_entersizemove =  561
global const wm_exitsizemove =  562
global const wm_mdirefrshmenu =  564
global const wm_lbuttondblclk =  515
global const wm_rbuttondblclk =  518
global const wm_lbuttondown =  513
global const wm_rbuttondown =  516
global const wm_mbuttondown =  519
global const wm_mousemove =  512
global const wm_lbuttonup =  514
global const wm_rbuttonup =  517
global const wm_mbuttonup =  520
global const wm_mbuttondblclk =  521
global const wm_mousewheel =  522
global const snd_filename =  131072
global const snd_async =  1
global const dt_singleline =  32
global const dt_centre =  1
global const dt_vcentre =  4
global const ws_overlapped =  0
global const ws_popup =  2147483648
global const ws_child =  1073741824
global const ws_minimize =  536870912
global const ws_visible =  268435456
global const ws_disabled =  134217728
global const ws_clipsiblings =  67108864
global const ws_clipchildren =  33554432
global const ws_maximize =  16777216
global const ws_caption =  12582912
global const ws_border =  8388608
global const ws_dlgframe =  4194304
global const ws_hscroll =  1048576
global const ws_vscroll =  2097152
global const ws_sysmenu =  524288
global const ws_thickframe =  262144
global const ws_group =  131072
global const ws_tabstop =  0
global const ws_scrollbars =  3145728
global const ws_minimizebox =  131072
global const ws_maximizebox =  65536
global const ws_tiled =  0
global const ws_iconic =  536870912
global const ws_sizebox =  262144
global const ws_overlappedwindow =  13565952
global const ws_tiledwindow =  13565952
global const ws_popupwindow =  -2138570752
global const ws_childwindow =  1073741824
global const ws_ex_acceptfiles =  16
global const ws_ex_appwindow =  262144
global const ws_ex_clientedge =  512
global const ws_ex_contexthelp =  1024
global const ws_ex_controlparent =  65536
global const ws_ex_dlgmodalframe =  1
global const ws_ex_left =  0
global const ws_ex_leftscrollbar =  16384
global const ws_ex_ltrreading =  0
global const ws_ex_mdichild =  64
global const ws_ex_noparentnotify =  4
global const ws_ex_overlappedwindow =  768
global const ws_ex_palettewindow =  392
global const ws_ex_right =  4096
global const ws_ex_rightscrollbar =  0
global const ws_ex_rtlreading =  8192
global const ws_ex_staticedge =  131072
global const ws_ex_toolwindow =  128
global const ws_ex_topmost =  8
global const ws_ex_transparent =  32
global const ws_ex_windowedge =  256

global const gw_hwndfirst =  0
global const gw_hwndlast =  1
global const gw_hwndnext =  2
global const gw_hwndprev =  3
global const gw_owner =  4
global const gw_child =  5
global const gw_enabledpopup =  6
global const cb_geteditsel =  320
global const cb_limittext =  321
global const cb_seteditsel =  322
global const cb_addstring =  323
global const cb_deletestring =  324
global const cb_dir =  325
global const cb_getcount =  326
global const cb_getcursel =  327
global const cb_getlbtext =  328
global const cb_getlbtextlen =  329
global const cb_insertstring =  330
global const cb_resetcontent =  331
global const cb_findstring =  332
global const cb_findstringexact =  344
global const cb_selectstring =  333
global const cb_setcursel =  334
global const cb_showdropdown =  335
global const cb_getitemdata =  336
global const cb_setitemdata =  337
global const cb_getdroppedcontrolrect =  338
global const cb_setitemheight =  339
global const cb_getitemheight =  340
global const cb_setextendedui =  341
global const cb_getextendedui =  342
global const cb_getdroppedstate =  343
global const cb_setlocale =  345
global const cb_getlocale =  346
global const cb_gettopindex =  347
global const cb_settopindex =  348
global const cb_gethorizontalextent =  349
global const cb_sethorizontalextent =  350
global const cb_getdroppedwidth =  351
global const cb_setdroppedwidth =  352
global const cb_initstorage =  353
global const cb_multipleaddstring =  355
global const bm_click =  245
global const bm_getcheck =  240
global const bm_getimage =  246
global const bm_getstate =  242
global const bm_setcheck =  241
global const bm_setimage =  247
global const bm_setstate =  243
global const bm_setstyle =  244
global const cf_bitmap =  2
global const cf_dib =  8
global const cf_palette =  9
global const cf_enhmetafile =  14
global const cf_metafilepict =  3
global const cf_oemtext =  7
global const cf_text =  1           
global const cf_unicodetext =  13
global const cf_dif =  5
global const cf_dspbitmap =  130
global const cf_dspenhmetafile =  142
global const cf_dspmetafilepict =  131
global const cf_dsptext =  129
global const cf_gdiobjfirst =  768
global const cf_gdiobjlast =  1023
global const cf_hdrop =  15
global const cf_locale =  16
global const cf_ownerdisplay =  128
global const cf_pendata =  10
global const cf_privatefirst =  512
global const cf_privatelast =  767
global const cf_riff =  11
global const cf_sylk =  4
global const cf_wave =  12
global const cf_tiff =  6

global const tcif_text =  1
global const tcif_image =  2
global const tcif_param =  8
global const tcif_rtlreading =  4

global const wm_keydown =  256
global const wm_keyup =  257
global const wm_char =  258
global const wm_syschar =  262
global const wm_sysdeadchar =  263
global const wm_syskeydown =  260
global const wm_syskeyup =  261
global const mf_insert =  0
global const mf_change =  128
global const mf_append =  256
global const mf_delete =  512
global const mf_remove =  4096
global const mf_bycommand =  0
global const mf_byposition =  1024
global const mf_separator =  2048
global const mf_enabled =  0
global const mf_grayed =  1
global const mf_greyed =  1
global const mf_disabled =  2
global const mf_unchecked =  0
global const mf_checked =  8
global const mf_usecheckbitmaps =  512
global const mf_string =  0
global const mf_bitmap =  4
global const mf_ownerdraw =  256
global const mf_popup =  16
global const mf_menubarbreak =  32
global const mf_menubreak =  64
global const mf_unhilite =  0
global const mf_hilite =  128
global const mf_sysmenu =  8192
global const mf_help =  16384
global const mf_mouseselect =  32768


global const wm_command =  273
global const wm_menuselect =  287
global const wm_cut =  768
global const wm_copy =  769
global const wm_paste =  770
global const wm_clear =  771
global const wm_undo =  772
global const em_getsel =  176
global const em_setsel =  177
global const em_scroll =  181
global const em_linescroll =  182
global const em_scrollcaret =  183
global const em_getmodify =  184
global const em_setmodify =  185
global const em_getlinecount =  186
global const em_lineindex =  187
global const em_sethandle =  188
global const em_gethandle =  189
global const em_getthumb =  190
global const em_linelength =  193
global const em_replacesel =  194
global const em_getline =  196
global const em_limittext =  197
global const em_canundo =  198
global const em_undo =  199
global const em_fmtlines =  200
global const em_linefromchar =  201
global const em_settabstops =  203
global const em_setpasswordchar =  204
global const em_emptyundobuffer =  205
global const em_getfirstvisibleline =  206
global const em_setreadonly =  207
global const em_setwordbreakproc =  208
global const em_getwordbreakproc =  209
global const em_getpasswordchar =  210
global const em_setlimittext =  197
global const em_getseltext =  1086
global const em_setcharformat =  1092
global const em_getcharformat =  1082
global const em_settextmode =  1113
global const em_gettextmode =  1114
global const em_gettextex =  1118
global const em_gettextlengthex =  1119
global const tm_plaintext =  1
global const tm_richtext =  2
global const tm_singlelevelundo =  4
global const tm_multilevelundo =  8
global const tm_singlecodepage =  16
global const tm_multicodepage =  32
global const scf_word =  2
global const scf_selection =  1
global const sb_getborders =  1031
global const sb_getparts =  1030
global const sb_getrect =  1034
global const sb_gettextw =  1037
global const sb_gettextlengthw =  1036
global const sb_settextw =  1035
global const sb_gettexta =  1026
global const sb_gettextlengtha =  1027
global const sb_settexta =  1025
global const sb_gettext =  1026
global const sb_gettextlength =  1027
global const sb_settext =  1025
global const sb_setminheight =  1032
global const sb_setparts =  1028
global const sb_simple =  1033
global const wm_setfont =  48
global const wm_getfont =  49
global const gm_advanced =  2
global const transparent =  1
global const opaque =  2
global const mwt_identity =  1
global const cw_usedefault =  0x8000'0000
global const idc_arrow =  32512
global const idc_ibeam =  32513
global const idc_wait =  32514
global const idc_cross =  32515
global const idc_uparrow =  32516
global const idc_sizenwse =  32642
global const idc_sizenesw =  32643
global const idc_sizewe =  32644
global const idc_sizens =  32645
global const idc_sizeall =  32646
global const idc_no =  32648
global const idc_appstarting =  32650
global const idc_help =  32651
global const idi_application =  32512
global const idi_hand =  32513
global const idi_question =  32514
global const idi_exclamation =  32515
global const idi_asterisk =  32516
global const idi_winlogo =  32517
global const idc_size =  32640
global const idc_icon =  32641
global const arrowpointer =  32512
global const ibeampointer =  32513
global const waitpointer =  32514
global const crosspointer =  32515
global const uparrowpointer =  32516
global const sizenwsepointer =  32642
global const sizeneswpointer =  32643
global const sizewepointer =  32644
global const sizenspointer =  32645
global const sizeallpointer =  32646
global const nopointer =  32648
global const appstartingpointer =  32650
global const helpicon =  32651
global const applicationicon =  32512
global const handicon =  32513
global const questionicon =  32514
global const exclamationicon =  32515
global const asteriskicon =  32516
global const winlogoicon =  32517
global const sizepointer =  32640
global const iconicon =  32641
global const sm_cymin =  29
global const sm_cxmin =  28
global const sm_arrange =  56
global const sm_cleanboot =  67
global const sm_cmetrics =  76
global const sm_cmousebuttons =  43
global const sm_cxborder =  5
global const sm_cyborder =  6
global const sm_cxcursor =  13
global const sm_cycursor =  14
global const sm_cxdlgframe =  7
global const sm_cydlgframe =  8
global const sm_cxdoubleclk =  36
global const sm_cydoubleclk =  37
global const sm_cxdrag =  68
global const sm_cydrag =  69
global const sm_cxedge =  45
global const sm_cyedge =  46
global const sm_cxfixedframe =  7
global const sm_cyfixedframe =  8
global const sm_cxframe =  32
global const sm_cyframe =  33
global const sm_cxfullscreen =  16
global const sm_cyfullscreen =  17
global const sm_cxhscroll =  21
global const sm_cyhscroll =  3
global const sm_cxhthumb =  10
global const sm_cxicon =  11
global const sm_cyicon =  12
global const sm_cxiconspacing =  38
global const sm_cyiconspacing =  39
global const sm_cxmaximized =  61
global const sm_cymaximized =  62
global const sm_cxmaxtrack =  59
global const sm_cymaxtrack =  60
global const sm_cxmenucheck =  71
global const sm_cymenucheck =  72
global const sm_cxmenusize =  54
global const sm_cymenusize =  55
global const sm_cxminimized =  57
global const sm_cyminimized =  58
global const sm_cxminspacing =  47
global const sm_cyminspacing =  48
global const sm_cxmintrack =  34
global const sm_cymintrack =  35
global const sm_cxscreen =  0
global const sm_cyscreen =  1
global const sm_cxsize =  30
global const sm_cysize =  31
global const sm_cxsizeframe =  32
global const sm_cysizeframe =  33
global const sm_cxsmicon =  49
global const sm_cysmicon =  50
global const sm_cxsmsize =  52
global const sm_cysmsize =  53
global const sm_cxvscroll =  2
global const sm_cyvscroll =  20
global const sm_cyvthumb =  9
global const sm_cycaption =  4
global const sm_cykanjiwindow =  18
global const sm_cymenu =  15
global const sm_cysmcaption =  51
global const sm_dbcsenabled =  42
global const sm_debug =  22
global const sm_menudropalignment =  40
global const sm_mideastenabled =  74
global const sm_mousepresent =  19
global const sm_mousewheelpresent =  75
global const sm_network =  63
global const sm_penwindows =  41
global const sm_reserved1 =  24
global const sm_reserved2 =  25
global const sm_reserved3 =  26
global const sm_reserved4 =  27
global const sm_secure =  44
global const sm_showsounds =  70
global const sm_slowmachine =  73
global const sm_swapbutton =  23
global const arw_bottomleft =  0
global const arw_bottomright =  1
global const arw_hide =  8
global const arw_topleft =  2
global const arw_topright =  3
global const arw_down =  4
global const arw_left =  0
global const arw_right =  0
global const arw_up =  4
global const white_brush =  0
global const ltgray_brush =  1
global const gray_brush =  2
global const dkgray_brush =  3
global const black_brush =  4
global const null_brush =  5
global const hollow_brush =  5
global const white_pen =  6
global const black_pen =  7
global const null_pen =  8
global const oem_fixed_font =  10
global const ansi_fixed_font =  11
global const ansi_var_font =  12
global const system_font =  13
global const device_default_font =  14
global const default_palette =  15
global const system_fixed_font =  16
global const stock_last =  16


global const wm_hscroll =  276
global const wm_vscroll =  277


global const wm_ctlcolourmsgbox =  306
global const wm_ctlcolouredit =  307
global const wm_ctlcolourlistbox =  308
global const wm_ctlcolourbtn =  309
global const wm_ctlcolourdlg =  310
global const wm_ctlcolourscrollbar =  311
global const wm_ctlcolourstatic =  312
global const wm_timer =  275

global const srccopy =  13369376
global const srcpaint =  15597702
global const srcand =  8913094
global const srcinvert =  6684742
global const srcerase =  4457256

global const notsrccopy =  3342344
global const notsrcerase =  1114278
global const mergecopy =  12583114
global const mergepaint =  12255782
global const patcopy =  15728673
global const patpaint =  16452105
global const patinvert =  5898313
global const dstinvert =  5570569
global const blackness =  66
global const whiteness =  16711778

global const r2_black =  1
global const r2_notmergepen =  2
global const r2_masknotpen =  3
global const r2_notcopypen =  4
global const r2_maskpennot =  5
global const r2_not =  6
global const r2_xorpen =  7
global const r2_notmaskpen =  8
global const r2_maskpen =  9
global const r2_notxorpen =  10
global const r2_nop =  11
global const r2_mergenotpen =  12
global const r2_copypen =  13
global const r2_mergepennot =  14
global const r2_mergepen =  15
global const r2_white =  16
global const r2_last =  16

global const gdi_error =  4294967295
global const hgdi_error =  4294967295
global const clr_invalid =  4278190080
global const clr_default =  4278190080
global const clr_none =  4294967295
global const ofn_readonly =  1
global const ofn_overwriteprompt =  2
global const ofn_hidereadonly =  4
global const ofn_nochangedir =  8
global const ofn_showhelp =  16
global const ofn_enablehook =  32
global const ofn_enabletemplate =  64
global const ofn_enabletemplatehandle =  128
global const ofn_novalidate =  256
global const ofn_allowmultiselect =  512
global const ofn_extensiondifferent =  1024
global const ofn_pathmustexist =  2048
global const ofn_filemustexist =  4096
global const ofn_createprompt =  8192
global const ofn_shareaware =  16384
global const ofn_noreadonlyreturn =  32768
global const ofn_notestfilecreate =  65536
global const ofn_nonetworkbutton =  131072
global const ofn_nolongnames =  262144
global const ofn_explorer =  524288
global const ofn_nodereferencelinks =  1048576
global const ofn_longnames =  2097152
global const ofn_sharefallthrough =  2
global const ofn_sharenowarn =  1
global const ofn_sharewarn =  0
global const dib_rgb_colours =  0
global const dib_pal_colours =  1
global const dib_pal_indices =  2
global const dib_pal_physindices =  2
global const dib_pal_logindices =  4
global const stm_seticon =  368
global const stm_setimage =  370
global const lr_loadfromfile =  16
global const image_bitmap =  0
global const image_icon =  1
global const lr_copydeleteorg =  8
global const lr_copyreturnorg =  4
global const lr_monochrome =  1
global const lr_createdibsection =  8192
global const lr_defaultsize =  64
global const ss_icon =  3
global const ss_bitmap =  14
global const gcl_menuname =  -8
global const gcl_hbrbackground =  -10
global const gcl_hcursor =  -12
global const gcl_hicon =  -14
global const gcl_hmodule =  -16
global const gcl_cbwndextra =  -18
global const gcl_cbclsextra =  -20
global const gcl_wndproc =  -24
global const gcl_style =  -26
global const gcw_atom =  -32
global const colour_scrollbar =  0
global const colour_background =  1
global const colour_desktop =  1
global const colour_activecaption =  2
global const colour_inactivecaption =  3
global const colour_menu =  4
global const colour_window =  5
global const colour_windowframe =  6
global const colour_menutext =  7
global const colour_windowtext =  8
global const colour_captiontext =  9
global const colour_activeborder =  10
global const colour_inactiveborder =  11
global const colour_appworkspace =  12
global const colour_highlight =  13
global const colour_highlighttext =  14
global const colour_btnface =  15
global const colour_3dface =  15
global const colour_btnshadow =  16
global const colour_3dshadow =  16
global const colour_graytext =  17
global const colour_btntext =  18
global const colour_inactivecaptiontext =  19
global const colour_btnhighlight =  20
global const colour_3dhilight =  20
global const colour_3ddkshadow =  21
global const colour_3dlight =  22
global const colour_infotext =  23
global const colour_infobk =  24
global const colour_tooltipbk =  24
global const mk_lbutton =  1
global const mk_rbutton =  2
global const mk_shift =  4
global const mk_control =  8
global const mk_mbutton =  16
global const cbm_createdib =  2
global const cbm_init =  4
global const cc_enablehook =  16
global const cc_enabletemplate =  32
global const cc_enabletemplatehandle =  64
global const cc_fullopen =  2
global const cc_preventfullopen =  4
global const cc_rgbinit =  1
global const cc_showhelp =  8
global const cc_solidcolour =  128
global const cf_screenfonts =  1
global const cf_printerfonts =  2
global const cf_effects =  256
global const size_restored =  0
global const size_minimized =  1
global const size_maximized =  2
global const size_maxshow =  3
global const size_maxhide =  4
global const gwl_userdata =  -21
global const gwl_id =  -12
global const ta_top =  0
global const ta_left =  0
global const ta_noupdatecp =  0
global const ta_updatecp =  1
global const ta_right =  2
global const ta_centre =  6
global const vta_centre =  6
global const ta_bottom =  8
global const ta_baseline =  24
global const vta_baseline =  24
global const ta_rtlreading =  256
global const aligntop =  0
global const alignbottom =  8
global const alignbaseline =  24
global const aligncentre =  6
global const alignleft =  0
global const alignright =  2

global const em_exgetsel =  1076
global const em_exlimittext =  1077
global const em_exlinefromchar =  1078
global const em_exsetsel =  1079
global const em_getparaformat =  1085
global const em_setparaformat =  1095
global const em_streamin =  1097
global const em_streamout =  1098
global const em_gettextrange =  1099
global const em_findtext =  1080
global const em_findtextex =  1103


global const hwnd_top =  0
global const hwnd_bottom =  1
global const hwnd_topmost =  -1
global const hwnd_notopmost =  -2

global const normalwind =  0
global const modalwind =  -1
global const dialogwind =  -2
global const minimize =  2
global const maximize =  3
global const shiftmask =  1
global const controlmask =  2
global const altmask =  4
global const windowcolour =  15
global const ps_geometric =  65536
global const ps_cosmetic =  0
global const ps_alternate =  8
global const ps_solid =  0
global const ps_dash =  1
global const ps_dot =  2
global const ps_dashdot =  3
global const ps_dashdotdot =  4
global const ps_null =  5
global const ps_insideframe =  6
global const ps_userstyle =  7
global const ps_endcap_round =  0
global const ps_endcap_square =  256
global const ps_endcap_flat =  512
global const ps_join_bevel =  4096
global const ps_join_miter =  8192
global const ps_join_round =  0
global const ps_style_mask =  15
global const ps_endcap_mask =  3840
global const ps_type_mask =  983040
global const bs_solid =  0
global const bs_hollow =  1
global const bs_null =  1
global const bs_hatched =  2
global const bs_pattern =  3
global const bs_dibpattern =  5
global const bs_dibpatternpt =  6
global const bs_pattern8x8 =  7
global const bs_dibpattern8x8 =  8
global const hs_horizontal =  0
global const hs_vertical =  1
global const hs_fdiagonal =  2
global const hs_bdiagonal =  3
global const hs_cross =  4
global const hs_diagcross =  5


global const spi_getworkarea =  48

proc start=
end

=== wingxlib.q 0 1 40/43 ===


export var hwapplic=nil
export var hwchild=nil
export var iswin32
export var screendc

export var nglobalfonts=0
export var fonttable::=()           
export var fontdimtable::=()        
export var fontvdimtable::=()       

proc start  =
    initdata()
end

proc initdata=
    iswin32:=(getos()="W32")
    screendc:=getdc(nil)

    fonttable:=(0,)*20
    fontdimtable:=(0,)*20
    fontvdimtable:=(0,)*20

    fonttable[1]:=getstockobject(17)    
    fonttable[2]:=getstockobject(13)    
    fonttable[3]:=getstockobject(16)    
    fonttable[4]:=getstockobject(10)    
    for i:=1 to 4 do
        fontdimtable[i]::=ws_point(0,0)
        fontvdimtable[i]::=ws_point(0,0)
    od
    nglobalfonts:=4
end

func checkoption(optionnames,optionvalues,name,default=-1)=

    n:=name in optionnames
    if not n then return default fi
    return optionvalues[n]
end

global proc wx_waitmess=
    windmsg:=new((iswin32|ws_msg32|ws_msg64))

    do
        if getmessage(&windmsg,nil,0,0)<>0 then
            w:=windmsg.hwnd
            if windmsg.message=wm_keydown and windmsg.wparam=27 then exit fi
            if windmsg.message=wm_timer then CPL "TIMER!!" fi
            translatemessage(&windmsg)
            dispatchmessage(&windmsg)
            if windmsg.message=wm_close then exit fi
        else
            exit
        fi
    od
end

global func wx_getw(hwnd)=
    n:=getwindowlongptr(hwnd, gwl_userdata)
    return n
end

global proc wx_setw(hwnd,index)=
    setwindowlongptr(hwnd, gwl_userdata, index)
end

global func wx_gettextwidth(hdc,s)=
    size:=new(ws_point)
    gettextextentpoint32(hdc,s,s.len,&size)
    return size.x
end

global func wx_createpopup(?caption,?pos,?dim,?options,owner=nil)=
    const gap=40
    const smallestwidth=150

    if options.isvoid then
 options:=[wf_caption:1,wf_border:wbs_resize]
    fi

    posx:=posy:=-1
    dimx:=640
    dimy:=480
    fcentre:=0
    fautopos:=0
    fmax:=fdesktop:=0

    if caption.isvoid then caption:="<No Caption>" fi

    if dim.defined then
        if dim.isstring and dim="max" then
            fmax:=1
        elsif dim.isstring and dim="desktop" then
            fdesktop:=1
        else
            dimx:=dim[1]
            dimy:=dim[2]
        fi
    fi

    if pos.isvoid or pos="cent" then
        fcentre:=1
    elsif pos="auto" then
        fautopos:=1
    elsif pos.defined and not pos.isstring then
        posx:=pos[1]
        posy:=pos[2]
    else                
        abort("gxcw bad pos")
    fi

    bstyle:=bxstyle:=0
    nocap:=0            

    framex:=framey:=0

    case options{wf_border,wbs_resize}
    when wbs_none then      
        nocap:=1
        framex:=0
        framey:=0
    when wbs_simple then        
        nocap:=1
        bstyle:=ws_border
        framex:=1
        framey:=1
    when wbs_thick then     
        bstyle:=ws_dlgframe
        fixedframe:=0
        framex:=getsystemmetrics(sm_cxfixedframe)
        framey:=getsystemmetrics(sm_cyfixedframe)
    when wbs_resize then
        bstyle:=ws_sizebox
        framex:=getsystemmetrics(sm_cxsizeframe)
        framey:=getsystemmetrics(sm_cysizeframe)
    when wbs_sunken,wbs_sunken2 then        
        bstyle:=ws_dlgframe
        bxstyle:=ws_ex_clientedge
        framex:=5
        framey:=5
    when wbs_sunkenrs then
        bstyle:=ws_sizebox
        bxstyle:=ws_ex_clientedge
        framex:=6
        framey:=6
    esac

    capheight:=getsystemmetrics(sm_cycaption)
    mbheight:=getsystemmetrics(sm_cymenu)

    style:=0
    exstyle:=0

    if options{wf_show,1} then
        style ior:=ws_visible
    fi

    mxleft:=framex
    mxright:=framey
    mytop:=framey+capheight
    mybottom:=framey
    showstyle:=sw_shownormal

    hcwmenu:=nil
    if options{wf_menu,0}=1 then
        mytop+:=mbheight
        hcwmenu:=createmenu()
        appendmenu(hcwmenu,0,998,"fred")
    fi

    style ior:=ws_clipchildren

    if nocap or options{wf_caption,1}=0 then
        mytop-:=capheight
        style ior:=ws_popup
    fi

    if options{wf_iframe,0}=0 then
        if not fautopos then
            posx-:=mxleft
            posy-:=mytop
        fi
        dimx+:=mxleft+mxright
        dimy+:=mytop+mybottom
    fi

    if fcentre or options{wf_cent,0}=1 then
        fautopos:=0
        box:=new(ws_rect)
        systemparametersinfoa(spi_getworkarea,0,&box,0)
        posx:=box.rightx%2-dimx%2
        posy:=(box.bottom-box.top)%2-dimy%2+box.top
    fi

    if fmax or options{wf_max,0} then
        showstyle:=sw_maximize
        style ior:=ws_maximize
    fi


    if options{wf_minmax,1}=1 then
        style ior:=(ws_maximizebox ior ws_minimizebox)

    fi

    if options{wf_sysmenu,1}=1 then
        style ior:=ws_sysmenu
    fi

    if fautopos=0 and options{wf_clip,0}=1 then
        box:=new(ws_rect)
        systemparametersinfoa(spi_getworkarea,0,&box,0)

        if posx<box.leftx+gap then posx:=box.leftx+gap fi

        if posy<box.top+gap then posy:=box.top+gap fi
        dimxmin:=dimx max smallestwidth
        if posx+dimxmin>=box.rightx+gap then posx:=box.rightx-gap-dimxmin fi
        if posy+dimy>=box.bottom+gap then posy:=box.bottom-gap-dimy fi
    elsif fautopos then
        posx:=posy:=cw_usedefault
    fi

    if fdesktop or options{wf_desktop,0}=1 then
        box:=new(ws_rect)
        systemparametersinfoa(spi_getworkarea,0,&box,0)
        posx:=box.leftx
        posy:=box.top
        dimx:=box.rightx-box.leftx
        dimy:=box.bottom-box.top
    fi

    if options{wf_toolwind,0}=1 then
        exstyle ior:=ws_ex_toolwindow
    fi

    classname:="pcc001"

    STYLE IOR:=WS_VISIBLE

    style ior:=bstyle
    exstyle ior:=bxstyle

    hwnd:=createwindowex(
        exstyle,
        classname,
        caption,
        style,
        posx,posy,          
        dimx,dimy,
        owner,          
        hcwmenu,            
        nil,    
        nil)            

    if hwnd=nil then
        e:=getlasterror()
        abort("wx:Can't create popup window "+tostr(e))
    fi
    return hwnd
end

global func wx_createcontrol(?pos,?dim,border=wbs_simple,owner)=
    const gap=40
    const smallestwidth=150

    posx:=posy:=0
    dimx:=160
    dimy:=120

    if dim.defined then
        dimx:=dim[1]
        dimy:=dim[2]
    fi

    if pos.defined then
        posx:=pos[1]
        posy:=pos[2]
    fi

    bstyle:=bxstyle:=0

    case border
    when wbs_none then          
    when wbs_simple then        
        bstyle:=ws_border
    else
        pcerror("createcontrol/bad border "+wbsnames[border])
    esac

    style:=0
    exstyle:=0

    style ior:=ws_clipchildren

    classname:="pcc001"

    style ior:=ws_child
    style ior:=ws_visible

    style ior:=bstyle
    exstyle ior:=bxstyle

    hwnd:=createwindowex(
        exstyle,
        classname,
        nil,
        style,
        posx,posy,          
        dimx,dimy,
        owner,              
        nil,                
        nil,
        nil)                

    if hwnd=0 then
        e:=getlasterror()
        abort("wx:Can't create child window "+tostr(e))
    fi

    return hwnd
end

=== winmessages.q 0 1 41/43 ===
global var winmessagenames=[
    (0:"wm_null"),
    (1:"wm_create"),
    (2:"wm_destroy"),
    (3:"wm_move"),
    (4:"pgk_menu"),
    (5:"wm_size"),
    (6:"wm_activate"),
    (7:"wm_setfocus"),
    (8:"wm_killfocus"),
    (9:"cbn_selendok"),
    (10:"wm_enable"),
    (11:"wm_setredraw"),
    (12:"wm_settext"),
    (13:"wm_gettext"),
    (14:"wm_gettextlength"),
    (15:"wm_paint"),
    (16:"wm_close"),
    (17:"wm_queryendsession"),
    (18:"wm_quit"),
    (19:"wm_queryopen"),
    (20:"wm_erasebkgnd"),
    (21:"wm_syscolorchange"),
    (22:"wm_endsession"),
    (24:"wm_showwindow"),
    (26:"wm_wininichange"),
    (27:"wm_devmodechange"),
    (28:"wm_activateapp"),
    (29:"wm_fontchange"),
    (30:"wm_timechange"),
    (31:"wm_cancelmode"),
    (32:"wm_setcursor"),
    (33:"wm_mouseactivate"),
    (34:"wm_childactivate"),
    (35:"wm_queuesync"),
    (36:"wm_getminmaxinfo"),
    (38:"wm_painticon"),
    (39:"wm_iconerasebkgnd"),
    (40:"wm_nextdlgctl"),
    (42:"wm_spoolerstatus"),
    (43:"wm_drawitem"),
    (44:"wm_measureitem"),
    (45:"wm_deleteitem"),
    (46:"wm_vkeytoitem"),
    (47:"wm_chartoitem"),
    (48:"wm_setfont"),
    (49:"wm_getfont"),
    (50:"wm_sethotkey"),
    (51:"wm_gethotkey"),
    (55:"wm_querydragicon"),
    (57:"wm_compareitem"),
    (64:"tbif_size"),
    (65:"wm_compacting"),
    (70:"wm_windowposchanging"),
    (71:"wm_windowposchanged"),
    (72:"wm_power"),
    (74:"wm_copydata"),
    (75:"wm_canceljournal"),
    (78:"wm_notify"),
    (80:"wm_inputlangchangerequest"),
    (81:"wm_inputlangchange"),
    (82:"wm_tcard"),
    (83:"wm_help"),
    (84:"wm_userchanged"),
    (85:"wm_notifyformat"),
    (123:"wm_contextmenu"),
    (124:"wm_stylechanging"),
    (125:"wm_stylechanged"),
    (126:"wm_displaychange"),
    (127:"wm_geticon"),
    (128:"wm_seticon"),
    (129:"wm_nccreate"),
    (130:"wm_ncdestroy"),
    (131:"wm_nccalcsize"),
    (132:"wm_nchittest"),
    (133:"wm_ncpaint"),
    (134:"wm_ncactivate"),
    (135:"wm_getdlgcode"),
    (160:"wm_ncmousemove"),
    (161:"wm_nclbuttondown"),
    (162:"wm_nclbuttonup"),
    (163:"wm_nclbuttondblclk"),
    (164:"wm_ncrbuttondown"),
    (165:"wm_ncrbuttonup"),
    (166:"wm_ncrbuttondblclk"),
    (167:"wm_ncmbuttondown"),
    (168:"wm_ncmbuttonup"),
    (169:"wm_ncmbuttondblclk"),
    (176:"em_getsel"),
    (177:"em_setsel"),
    (178:"em_getrect"),
    (179:"em_setrect"),
    (180:"em_setrectnp"),
    (181:"em_scroll"),
    (182:"em_linescroll"),
    (183:"em_scrollcaret"),
    (184:"em_getmodify"),
    (185:"em_setmodify"),
    (186:"em_getlinecount"),
    (187:"em_lineindex"),
    (188:"em_sethandle"),
    (189:"em_gethandle"),
    (190:"em_getthumb"),
    (193:"em_linelength"),
    (194:"em_replacesel"),
    (196:"em_getline"),
    (197:"em_setlimittext"),
    (198:"em_canundo"),
    (199:"em_undo"),
    (200:"em_fmtlines"),
    (201:"em_linefromchar"),
    (203:"em_settabstops"),
    (204:"em_setpasswordchar"),
    (205:"em_emptyundobuffer"),
    (206:"em_getfirstvisibleline"),
    (207:"em_setreadonly"),
    (208:"em_setwordbreakproc"),
    (209:"em_getwordbreakproc"),
    (210:"em_getpasswordchar"),
    (211:"em_setmargins"),
    (212:"em_getmargins"),
    (213:"em_getlimittext"),
    (214:"em_posfromchar"),
    (215:"em_charfrompos"),
    (224:"sbm_setpos"),
    (225:"sbm_getpos"),
    (226:"sbm_setrange"),
    (227:"sbm_getrange"),
    (228:"sbm_enable_arrows"),
    (230:"sbm_setrangeredraw"),
    (233:"sbm_setscrollinfo"),
    (234:"sbm_getscrollinfo"),
    (240:"bm_getcheck"),
    (241:"bm_setcheck"),
    (242:"bm_getstate"),
    (243:"bm_setstate"),
    (244:"bm_setstyle"),
    (245:"bm_click"),
    (246:"bm_getimage"),
    (247:"bm_setimage"),
    (255:"wm_input"),
    (256:"wm_keydown"),
    (257:"wm_keyup"),
    (258:"wm_char"),
    (259:"wm_deadchar"),
    (260:"wm_syskeydown"),
    (261:"wm_syskeyup"),
    (262:"wm_syschar"),
    (263:"wm_sysdeadchar"),
    (269:"wm_ime_startcomposition"),
    (270:"wm_ime_endcomposition"),
    (271:"wm_ime_composition"),
    (272:"wm_initdialog"),
    (273:"wm_command"),
    (274:"wm_syscommand"),
    (275:"wm_timer"),
    (276:"wm_hscroll"),
    (277:"wm_vscroll"),
    (278:"wm_initmenu"),
    (279:"wm_initmenupopup"),
    (287:"wm_menuselect"),
    (288:"wm_menuchar"),
    (289:"wm_enteridle"),
    (290:"wm_menurbuttonup"),
    (295:"wm_changeuistate"),
    (296:"wm_updateuistate"),
    (297:"wm_queryuistate"),
    (306:"wm_ctlcolormsgbox"),
    (307:"wm_ctlcoloredit"),
    (308:"wm_ctlcolorlistbox"),
    (309:"wm_ctlcolorbtn"),
    (310:"wm_ctlcolordlg"),
    (311:"wm_ctlcolorscrollbar"),
    (312:"wm_ctlcolorstatic"),
    (320:"cb_geteditsel"),
    (321:"cb_limittext"),
    (322:"cb_seteditsel"),
    (323:"cb_addstring"),
    (324:"cbem_deleteitem"),
    (325:"cb_dir"),
    (326:"cb_getcount"),
    (327:"cb_getcursel"),
    (328:"cb_getlbtext"),
    (329:"cb_getlbtextlen"),
    (330:"cb_insertstring"),
    (331:"cb_resetcontent"),
    (332:"cb_findstring"),
    (333:"cb_selectstring"),
    (334:"cb_setcursel"),
    (335:"cb_showdropdown"),
    (336:"cb_getitemdata"),
    (337:"cb_setitemdata"),
    (338:"cb_getdroppedcontrolrect"),
    (339:"cb_setitemheight"),
    (340:"cb_getitemheight"),
    (341:"cb_setextendedui"),
    (342:"cb_getextendedui"),
    (343:"cb_getdroppedstate"),
    (344:"cb_findstringexact"),
    (345:"cb_setlocale"),
    (346:"cb_getlocale"),
    (347:"cb_gettopindex"),
    (348:"cb_settopindex"),
    (349:"cb_gethorizontalextent"),
    (350:"cb_sethorizontalextent"),
    (351:"cb_getdroppedwidth"),
    (352:"cb_setdroppedwidth"),
    (353:"cb_initstorage"),
    (368:"stm_seticon"),
    (369:"stm_geticon"),
    (370:"stm_setimage"),
    (371:"stm_getimage"),
    (384:"lb_addstring"),
    (385:"lb_insertstring"),
    (386:"lb_deletestring"),
    (387:"lb_selitemrangeex"),
    (388:"lb_resetcontent"),
    (389:"lb_setsel"),
    (390:"lb_setcursel"),
    (391:"lb_getsel"),
    (392:"lb_getcursel"),
    (393:"lb_gettext"),
    (394:"lb_gettextlen"),
    (395:"lb_getcount"),
    (396:"lb_selectstring"),
    (397:"lb_dir"),
    (398:"lb_gettopindex"),
    (399:"lb_findstring"),
    (400:"lb_getselcount"),
    (401:"lb_getselitems"),
    (402:"lb_settabstops"),
    (403:"lb_gethorizontalextent"),
    (404:"lb_sethorizontalextent"),
    (405:"lb_setcolumnwidth"),
    (406:"lb_addfile"),
    (407:"lb_settopindex"),
    (408:"lb_getitemrect"),
    (409:"lb_getitemdata"),
    (410:"lb_setitemdata"),
    (411:"lb_selitemrange"),
    (412:"lb_setanchorindex"),
    (413:"lb_getanchorindex"),
    (414:"lb_setcaretindex"),
    (415:"lb_getcaretindex"),
    (416:"lb_setitemheight"),
    (417:"lb_getitemheight"),
    (418:"lb_findstringexact"),
    (421:"lb_setlocale"),
    (422:"lb_getlocale"),
    (423:"lb_setcount"),
    (424:"lb_initstorage"),
    (425:"lb_itemfrompoint"),
    (512:"wm_mousemove"),
    (513:"wm_lbuttondown"),
    (514:"wm_lbuttonup"),
    (515:"wm_lbuttondblclk"),
    (516:"wm_rbuttondown"),
    (517:"wm_rbuttonup"),
    (518:"wm_rbuttondblclk"),
    (519:"wm_mbuttondown"),
    (520:"wm_mbuttonup"),
    (521:"wm_mbuttondblclk"),
    (522:"wm_mousewheel"),
    (523:"wm_xbuttondown"),
    (524:"wm_xbuttonup"),
    (525:"wm_xbuttondblclk"),
    (528:"wm_parentnotify"),
    (529:"wm_entermenuloop"),
    (530:"wm_exitmenuloop"),
    (531:"wm_nextmenu"),
    (532:"wm_sizing"),
    (533:"wm_capturechanged"),
    (534:"wm_moving"),
    (536:"wm_powerbroadcast"),
    (537:"wm_devicechange"),
    (544:"wm_mdicreate"),
    (545:"wm_mdidestroy"),
    (546:"wm_mdiactivate"),
    (547:"wm_mdirestore"),
    (548:"wm_mdinext"),
    (549:"wm_mdimaximize"),
    (550:"wm_mditile"),
    (551:"wm_mdicascade"),
    (552:"wm_mdiiconarrange"),
    (553:"wm_mdigetactive"),
    (560:"wm_mdisetmenu"),
    (561:"wm_entersizemove"),
    (562:"wm_exitsizemove"),
    (563:"wm_dropfiles"),
    (564:"wm_mdirefreshmenu"),
    (641:"wm_ime_setcontext"),
    (642:"wm_ime_notify"),
    (643:"wm_ime_control"),
    (644:"wm_ime_compositionfull"),
    (645:"wm_ime_select"),
    (646:"wm_ime_char"),
    (656:"wm_ime_keydown"),
    (657:"wm_ime_keyup"),
    (673:"wm_mousehover"),
    (675:"wm_mouseleave"),
    (689:"wm_wtssession_change"),
    (768:"wm_cut"),
    (769:"wm_copy"),
    (770:"wm_paste"),
    (771:"wm_clear"),
    (772:"wm_undo"),
    (773:"wm_renderformat"),
    (774:"wm_renderallformats"),
    (775:"wm_destroyclipboard"),
    (776:"wm_drawclipboard"),
    (777:"wm_paintclipboard"),
    (778:"wm_vscrollclipboard"),
    (779:"wm_sizeclipboard"),
    (780:"wm_askcbformatname"),
    (781:"wm_changecbchain"),
    (782:"wm_hscrollclipboard"),
    (783:"wm_querynewpalette"),
    (784:"wm_paletteischanging"),
    (785:"wm_palettechanged"),
    (786:"wm_hotkey"),
    (791:"wm_print"),
    (792:"wm_printclient"),
    (896:"wm_penwinirst"),
    (911:"wm_penwinlast"),
    (1024:"infotipsize"),
    (1025:"cbem_insertitema"),
    (1026:"cbem_setimagelist"),
    (1027:"cbem_getimagelist"),
    (1028:"cbem_getitema"),
    (1029:"cbem_setitema"),
    (1030:"cbem_getcombocontrol"),
    (1031:"cbem_geteditcontrol"),
    (1032:"cbem_setexstyle"),
    (1033:"cbem_getextendedstyle"),
    (1034:"cbem_haseditchanged"),
    (1035:"cbem_insertitemw"),
    (1036:"cbem_setitemw"),
    (1037:"cbem_getitemw"),
    (1038:"cbem_setextendedstyle"),
    (1039:"ttm_getcurrenttoola"),
    (1040:"ttm_windowfrompoint"),
    (1041:"ttm_trackactivate"),
    (1042:"ttm_trackposition"),
    (1043:"ttm_settipbkcolor"),
    (1044:"ttm_settiptextcolor"),
    (1045:"ttm_getdelaytime"),
    (1046:"ttm_gettipbkcolor"),
    (1047:"ttm_gettiptextcolor"),
    (1048:"ttm_setmaxtipwidth"),
    (1049:"ttm_getmaxtipwidth"),
    (1050:"ttm_setmargin"),
    (1051:"ttm_getmargin"),
    (1052:"ttm_pop"),
    (1053:"tb_getitemrect"),
    (1054:"tb_buttonstructsize"),
    (1055:"tb_setbuttonsize"),
    (1056:"tb_setbitmapsize"),
    (1057:"tb_autosize"),
    (1059:"tb_gettooltips"),
    (1060:"tb_settooltips"),
    (1061:"tb_setparent"),
    (1063:"tb_setrows"),
    (1064:"tb_getrows"),
    (1065:"tb_getbitmapflags"),
    (1066:"tb_setcmdid"),
    (1067:"tb_changebitmap"),
    (1068:"tb_getbitmap"),
    (1069:"tb_getbuttontexta"),
    (1070:"tb_replacebitmap"),
    (1071:"tb_setindent"),
    (1072:"tb_setimagelist"),
    (1073:"tb_getimagelist"),
    (1074:"ttm_addtoolw"),
    (1075:"ttm_deltoolw"),
    (1076:"ttm_newtoolrectw"),
    (1077:"ttm_gettoolinfow"),
    (1078:"ttm_settoolinfow"),
    (1079:"ttm_hittestw"),
    (1080:"ttm_gettextw"),
    (1081:"ttm_updatetiptextw"),
    (1082:"ttm_enumtoolsw"),
    (1083:"ttm_getcurrenttoolw"),
    (1084:"tb_setmaxtextrows"),
    (1085:"tb_gettextrows"),
    (1086:"em_getseltext"),
    (1087:"em_hideselection"),
    (1088:"em_pastespecial"),
    (1089:"em_requestresize"),
    (1090:"em_selectiontype"),
    (1091:"tb_insertbuttonw"),
    (1092:"tb_addbuttonsw"),
    (1093:"tb_hittest"),
    (1094:"em_setolecallback"),
    (1095:"em_setparaformat"),
    (1096:"em_settargetdevice"),
    (1097:"em_streamin"),
    (1098:"em_streamout"),
    (1099:"tb_getbuttontextw"),
    (1100:"tb_saverestorew"),
    (1101:"tb_addstringw"),
    (1102:"em_getoptions"),
    (1103:"tb_getinsertmark"),
    (1104:"tb_setinsertmark"),
    (1105:"tb_insertmarkhittest"),
    (1106:"tb_movebutton"),
    (1107:"tb_getmaxsize"),
    (1108:"tb_setextendedstyle"),
    (1109:"tb_getextendedstyle"),
    (1110:"tb_getpadding"),
    (1111:"tb_setpadding"),
    (1112:"tb_setinsertmarkcolor"),
    (1113:"tb_getinsertmarkcolor"),
    (1114:"tb_mapacceleratorw"),
    (1124:"em_setpunctuation"),
    (1125:"wm_choosefont_setlogfont"),
    (1126:"wm_choosefont_setflags"),
    (1127:"udm_setpos"),
    (1128:"udm_getpos"),
    (1129:"udm_setbuddy"),
    (1130:"udm_getbuddy"),
    (1131:"udm_setaccel"),
    (1132:"udm_getaccel"),
    (1133:"udm_setbase"),
    (1134:"udm_getbase"),
    (1135:"psm_settitlea"),
    (1136:"psm_setwizbuttons"),
    (1137:"psm_pressbutton"),
    (1138:"psm_setcurselid"),
    (1139:"psm_setfinishtexta"),
    (1140:"psm_gettabcontrol"),
    (1141:"psm_isdialogmessage"),
    (1142:"psm_getcurrentpagehwnd"),
    (1144:"psm_settitlew"),
    (1145:"psm_setfinishtextw"),
    (1157:"dl_begindrag"),
    (1158:"dl_dragging"),
    (1159:"dl_dropped"),
    (1160:"dl_canceldrag"),
    (1280:"en_errspace"),
    (1281:"en_maxtext"),
    (1537:"en_hscroll"),
    (1538:"en_vscroll"),
    (1792:"en_msgfilter"),
    (1793:"en_requestresize"),
    (1794:"en_selchange"),
    (1795:"en_dropfiles"),
    (1796:"en_protected"),
    (1797:"en_correcttext"),
    (1798:"en_stopnoundo"),
    (1799:"en_imechange"),
    (1800:"en_saveclipboard"),
    (1801:"en_oleopfailed"),
    (4096:"lvm_getbkcolor"),
    (4097:"lvm_setbkcolor"),
    (4098:"lvm_getimagelist"),
    (4099:"lvm_setimagelist"),
    (4100:"lvm_getitemcount"),
    (4101:"lvm_getitema"),
    (4102:"lvm_setitema"),
    (4103:"lvm_insertitema"),
    (4104:"lvm_deleteitem"),
    (4105:"lvm_deleteallitems"),
    (4106:"lvm_getcallbackmask"),
    (4107:"lvm_setcallbackmask"),
    (4108:"lvm_getnextitem"),
    (4109:"lvm_finditema"),
    (4110:"lvm_getitemrect"),
    (4111:"lvm_setitemposition"),
    (4112:"lvm_getitemposition"),
    (4113:"lvm_getstringwidtha"),
    (4114:"lvm_hittest"),
    (4115:"lvm_ensurevisible"),
    (4116:"lvm_scroll"),
    (4117:"lvm_redrawitems"),
    (4118:"lvm_arrange"),
    (4119:"lvm_editlabela"),
    (4120:"lvm_geteditcontrol"),
    (4121:"lvm_getcolumna"),
    (4122:"lvm_setcolumna"),
    (4123:"lvm_insertcolumna"),
    (4124:"lvm_deletecolumn"),
    (4125:"lvm_getcolumnwidth"),
    (4126:"lvm_setcolumnwidth"),
    (4129:"lvm_createdragimage"),
    (4130:"lvm_getviewrect"),
    (4131:"lvm_gettextcolor"),
    (4132:"lvm_settextcolor"),
    (4133:"lvm_gettextbkcolor"),
    (4134:"lvm_settextbkcolor"),
    (4135:"lvm_gettopindex"),
    (4136:"lvm_getcountperpage"),
    (4137:"lvm_getorigin"),
    (4138:"lvm_update"),
    (4139:"lvm_setitemstate"),
    (4140:"lvm_getitemstate"),
    (4141:"lvm_getitemtexta"),
    (4142:"lvm_setitemtexta"),
    (4143:"lvm_setitemcount"),
    (4144:"lvm_sortitems"),
    (4145:"lvm_setitemposition32"),
    (4146:"lvm_getselectedcount"),
    (4147:"lvm_getitemspacing"),
    (4148:"lvm_getisearchstringa"),
    (4171:"lvm_getitemw"),
    (4172:"lvm_setitemw"),
    (4173:"lvm_insertitemw"),
    (4179:"lvm_finditemw"),
    (4183:"lvm_getstringwidthw"),
    (4191:"lvm_getcolumnw"),
    (4192:"lvm_setcolumnw"),
    (4193:"lvm_insertcolumnw"),
    (4211:"lvm_getitemtextw"),
    (4212:"lvm_setitemtextw"),
    (4213:"lvm_getisearchstringw"),
    (4214:"lvm_editlabelw"),
    (4352:"tvm_insertitema"),
    (4353:"tvm_deleteitem"),
    (4354:"tvm_expand"),
    (4356:"tvm_getitemrect"),
    (4357:"tvm_getcount"),
    (4358:"tvm_getindent"),
    (4359:"tvm_setindent"),
    (4360:"tvm_getimagelist"),
    (4361:"tvm_setimagelist"),
    (4362:"tvm_getnextitem"),
    (4363:"tvm_selectitem"),
    (4364:"tvm_getitema"),
    (4365:"tvm_setitema"),
    (4366:"tvm_editlabela"),
    (4367:"tvm_geteditcontrol"),
    (4368:"tvm_getvisiblecount"),
    (4369:"tvm_hittest"),
    (4370:"tvm_createdragimage"),
    (4371:"tvm_sortchildren"),
    (4372:"tvm_ensurevisible"),
    (4373:"tvm_sortchildrencb"),
    (4374:"tvm_endeditlabelnow"),
    (4375:"tvm_getisearchstringa"),
    (4402:"tvm_insertitemw"),
    (4414:"tvm_getitemw"),
    (4415:"tvm_setitemw"),
    (4416:"tvm_getisearchstringw"),
    (4417:"tvm_editlabelw"),
    (4608:"hdm_getitemcount"),
    (4609:"hdm_insertitema"),
    (4610:"hdm_deleteitem"),
    (4611:"hdm_getitema"),
    (4612:"hdm_setitema"),
    (4613:"hdm_layout"),
    (4614:"hdm_hittest"),
    (4618:"hdm_insertitemw"),
    (4619:"hdm_getitemw"),
    (4620:"hdm_setitemw"),
    (4864:"tcm_first"),
    (4866:"tcm_getimagelist"),
    (4867:"tcm_setimagelist"),
    (4868:"tcm_getitemcount"),
    (4869:"tcm_getitema"),
    (4870:"tcm_setitema"),
    (4871:"tcm_insertitema"),
    (4872:"tcm_deleteitem"),
    (4873:"tcm_deleteallitems"),
    (4874:"tcm_getitemrect"),
    (4875:"tcm_getcursel"),
    (4876:"tcm_setcursel"),
    (4877:"tcm_hittest"),
    (4878:"tcm_setitemextra"),
    (4904:"tcm_adjustrect"),
    (4905:"tcm_setitemsize"),
    (4906:"tcm_removeimage"),
    (4907:"tcm_setpadding"),
    (4908:"tcm_getrowcount"),
    (4909:"tcm_gettooltips"),
    (4910:"tcm_settooltips"),
    (4911:"tcm_getcurfocus"),
    (4912:"tcm_setcurfocus"),
    (4924:"tcm_getitemw"),
    (4925:"tcm_setitemw"),
    (4926:"tcm_insertitemw"),
    (5120:"pgm_first"),
    (8192:"ccm_first")]

proc start=
end
=== gxmisc.q 0 1 42/43 ===
export enumdata optionnames =
    (wf_border,     $),     
    (wf_resize,     $),     
    (wf_hscroll,    $),     
    (wf_vscroll,    $),     
    (wf_menu,       $),     
    (wf_caption,    $),     
    (wf_max,        $),     
    (wf_minmax,     $),     
    (wf_sysmenu,    $),     
    (wf_desktop,    $),     
    (wf_clip,       $),     
    (wf_show,       $),     
    (wf_iframe,     $),     
    (wf_cent,       $),     
    (wf_toolwind,   $)      
end

export enumdata wbsnames=
    (wbs_none=0,$),
    (wbs_simple,$),
    (wbs_thick,$),
    (wbs_resize,$),
    (wbs_sunken,$),
    (wbs_sunken2,$),
    (wbs_sunkenrs,$),
    (wbs_dummy,$)
end
=== dates.q 0 1 43/43 ===

export var daynames=("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

export var Monthnames=("January","February","March","April","May","June","July",
        "August","September","October","November","December")

export var days=(31,28,31, 30,31,30, 31,31,30, 31,30,31)

export record rdate=
    var day,month,year
end

export record rdatetime = 
    var day
    var month
    var year
    var hour
    var minute
    var second
    var milliseconds
    var dayofweek
end


export func makedatetime(d,m,y, h=0, minute=0, s=0)=

    d:=rdatetime(d,m,y, h,minute,s,0,0)
    d.dayofweek:=getdow(d)
    return d
end

export proc setdow(&d)=
    d.dayofweek:=getdow(d)
end

export func strdate(d,sep="-")=
    return tostr(d.day)+sep+leftstr(monthnames[d.month],3)+sep+tostr(d.year)
end

export func strtime(d,sep=":")=
    return tostr(d.hour)+sep+tostr(d.minute,"z2")+sep+tostr(d.second,"z2")
end

export func strdow(d,n=0)=
    if n then
        return leftstr(daynames[d.dayofweek],n)
    else
        return daynames[d.dayofweek]
    fi
end

export func strdatetime(d,dsep="-",tsep=":")=
    return strdate(d,dsep)+" "+strtime(d,tsep)
end

export func parsedate(s,defdate)=

    day:=defdate.day
    month:=defdate.month
    year:=defdate.year
    if s.[1]=" " then s:=rightstr(s,-1) fi

    sepset:=[' ', '-', '/', '.']

    seppos:=0
    for i:=1 to s.len do if s.[i] in sepset then seppos:=i; exit fi od

    if not seppos then      
        day:=strtoval(s)
        goto gotday
    fi
    day:=strtoval(leftstr(s,seppos-1))

    s:=rightstr(s,-seppos)      
    seppos:=0
    for i:=1 to s.len do if s.[i] in sepset then seppos:=i; exit fi od

    if seppos then
        monthstr:=leftstr(s,seppos-1)
        yearstr:=rightstr(s,s.len-seppos)
    else
        monthstr:=s
        yearstr:=""
    fi

    if asc(leftstr(monthstr)) in ['0'..'9'] then    
        month:=strtoval(monthstr)
        if month<1 or month>12 then
            return 0
        fi
    else
        month:=0
        for i:=1 to 12 do
            if convlc(leftstr(monthnames[i],3))=convlc(leftstr(monthstr,3)) then
                month:=i
                exit
            fi
        od
        if not month then
            return 0
        fi
    fi

    if yearstr<>"" then
        year:=strtoval(yearstr)
        if year<200 then
            if year in [00..89] then
                year+:=2000
            else
                year+:=1900
            fi
        fi
    fi

gotday:
    dd:=days[month] 
    if leapyear(year) and month=2 then dd+:=1 fi
    if day<1 or day>dd then return 0 fi
    if year<1990 or year>2089 then return 0 fi
    return makedatetime(day,month,year)
end

export func leapyear(y)=
    return (y-1900) rem 4=0
end

export func getdow(d)=
    return ((getday(d)-1) rem 7)+1
end

export func getday(d)=
    day:=0
    for i:=1990 to d.year-1 do
        day+:=(leapyear(i)|366|365)
    od

    for i:=1 to d.month-1 do
        day+:=(i=2|(leapyear(d.year)|29|28)|days[i])
    od
    day+:=d.day
    return day
end

export func getdays(m,y)=
    if leapyear(y) and m=2 then return 29 fi
    return days[m]
end

export func getmonthname(m,?n)=
    if not m.isint then
        m:=m.month
    fi
    m:=monthnames[m]
    if n.defined then m:=leftstr(m,n) fi
    return m
end

export func getdayname(d,?n)=
    if not d.isint then
        d:=getdow(d)
    fi
    d:=daynames[d]
    if n.defined then d:=leftstr(d,n) fi
    return d
end

export func addday(d0,i)=
    d:=d0
    if i>0 then
        to i do
            ++d.day
            if d.day>getdays(d.month,d.year) then
                d.day:=1
                ++d.month
                if d.month>12 then
                    d.month:=1
                    ++d.year
                fi
            fi
        od
    else
        to -i do
            --d.day
            if d.day<1 then
                --d.month
                if d.month<1 then
                    d.month:=12
                    --d.year
                fi
                d.day:=getdays(d.month,d.year)
            fi
        od
    fi

    if d.year<1990 then d:=makedatetime(1,1,1990) fi
    if d.year>2089 then d:=makedatetime(31,12,2089) fi

    dd:=getdays(d.month,d.year)
    if leapyear(d.year) and d.month=2 then dd+:=1 fi
    if d.day<1 then d.day:=1 fi
    if d.day>dd then d.day:=dd fi
    setdow(d)
    return d
end

export func getdatetime=
    tm:=getsystime()

    return rdatetime(tm.day,tm.month,tm.year,
            tm.hour, tm.minute, tm.second, tm.milliseconds,tm.dayofweek)
end

export func getsystime=
    tm:=new(ws_systemtime)
    getsystemtime(&tm)

    if tm.dayofweek=0 then
        tm.dayofweek:=7
    fi

    return tm
end
=== END ===
1 qq.m
2 qqcli.m
3 qq_arrays.m
4 qq_bits.m
5 qq_calldll.m
6 qq_decimal.m
7 qq_decls.m
8 qq_dicts.m
9 qq_jhandlers.m
10 qq_khandlers.m
11 qq_host.m
12 qq_lex.m
13 qq_lib.m
14 qq_lists.m
15 qq_newmodules.m
16 qq_names.m
17 qq_optim.m
18 qq_packed.m
19 qq_parse.m
20 qq_print.m
21 qq_pclgen.m
22 qq_pcllib.m
23 qq_records.m
24 qq_resolve.m
25 qq_sets.m
26 qq_strings.m
27 qq_syslibs.m
28 qq_tables.m
29 qq_show.m
30 qq_vars.m
31 qq_mtypes.m
32 sysp.q
33 clibp.q
34 smlib.q
35 winapi.q
36 gxlib.q
37 bmlib.q
38 console.q
39 winconsts.q
40 wingxlib.q
41 winmessages.q
42 gxmisc.q
43 dates.q
