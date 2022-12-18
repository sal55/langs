=== MA 33 ===
=== mc.m 0 0 1/33 ===
    module mm_cli
    module mm_assem
    module mc_blockc
!
    module mm_decls

    module mm_diags
!   module mm_diags_dummy

    module mm_export
    module mc_genc
    module mm_lex
    module mm_lib
    module mc_libc

    module mm_libsourcesc
!   module mm_libsources_dummy

    module mm_modules
    module mm_name
    module mm_parse
    module mm_support
    module mm_tables
    module mm_type
    module mm_winc

    module mc_decls as md
=== mm_cli.m 0 0 2/33 ===

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

byte fmodinfo
global ichar projectmodule          !nil, or lead header module

enumdata []ichar optionnames=

    (header_sw,     "header"),
    (load_sw,       "load"),
    (fixup_sw,      "fixup"),
    (parse_sw,      "parse"),
    (name_sw,       "name"),
    (type_sw,       "type"),

    (asm_sw,        "asm"),
    (c_sw,          "c"),
    (mcl_sw,        "mcl"),
    (obj_sw,        "obj"),
    (mx_sw,         "mx"),
    (ml_sw,         "ml"),
    (exe_sw,        "exe"),
    (mexe_sw,       "mexe"),
    (run_sw,        "run"),

    (sys_sw,        "sys"),
    (minsys_sw,     "minsys"),
    (nosys_sw,      "nosys"),
    (minos_sw,      "minos"),
    (nofile_sw,     "nofile"),

    (debug_sw,      "debug"),

    (gcc_sw,        "gcc"),
    (tcc_sw,        "tcc"),
    (tc_sw,         "tc"),
    (bcc_sw,        "bcc"),

    (ma_sw,         "ma"),
    (mas_sw,        "mas"),
    (docs_sw,       "docs"),
    (export_sw,     "exp"),
    (lib_sw,        "lib"),

    (opt_sw,        "opt"),
    (opt1_sw,       "opt1"),
    (opt2_sw,       "opt2"),

    (ast1_sw,       "ast1"),
    (ast2_sw,       "ast2"),
    (ast3_sw,       "ast3"),
    (showmx_sw,     "showmx"),
    (showasm_sw,    "showasm"),
    (st_sw,         "st"),
    (pst_sw,        "pst"),
    (stflat_sw,     "stflat"),
    (types_sw,      "types"),
    (overloads_sw,  "overloads"),
    (ss_sw,         "ss"),
    (showmodules_sw,"modules"),
    (shortnames_sw, "shortnames"),
    (modinfo_sw,    "modinfo"),

    (time_sw,       "time"),
    (v_sw,          "v"),
    (vv_sw,         "vv"),
    (quiet_sw,      "q"),
    (help_sw,       "h"),
    (help2_sw,      "help"),
    (ext_sw,        "ext"),
    (out_sw,        "out"),
    (outpath_sw,    "outpath"),
    (unused_sw,     "unused"),
    (set_sw,        "set"),
    (linux_sw,      "linux"),
end

byte fasmexe

global const logfile=langname+"x.log"

ichar outext=""             !for reporting of primary o/p file

int startclock,endclock,rpclock
byte msfile

global ichar inputfile

proc main=
    unit p,q,r
    int m,fileno,ntokens,t

    startclock:=os_clock()

    stepruncount()

    initdata()

    getinputoptions()

RPCLOCK:=CLOCK()
    readprojectfile(inputfile)

    if fverbose>=1 then
        if passlevel=run_pass then
            if not msfile or fverbose>1 then
                println "Compiling",inputfile,"to memory"
            fi
        else
            fprint "M6 Compiling # to #",inputfile:"14jlp-",changeext(outfile,outext),$
    !       print (msyslevel+1|" [No sys]"," [Min sys]" | " [Full sys]")
            println
        fi
    fi

    remove(logfile)

    do_loadmodules()

    do_parse()

    do_name()

    do_type()

    do_writema()
    if fwritema then finish fi

    do_writeexports()

    case passlevel
    when mcl_pass then
        codegen()

    when asm_pass then
        writeasmfile(asmfilename)

    when exe_pass then
        writeexefile(exefilename)

    when lib_pass then
        writelibfile(libfilename)

    when run_pass then
        runlibfile(libfilename)

    esac

finish::
    if fverbose>=2 then
        println "Finished."
    fi

    if debugmode then showlogfile() fi

    if fshowtiming then
        endclock:=os_clock()
        t:=endclock-startclock
        print "Time",t,"ms"
        if t then
!           println ",",int(real(lxalllines)/t),,"K lines per second"
            println ",",int(real(lxalllines)/t),,"K lines per second (",,lxalllines,,")"
        else
            println
        fi
    fi
end

proc do_loadmodules=
    if passlevel<load_pass then return fi
INT TT

    if fmodinfo then
        showmoduleinfo()
        stop
    fi


    loadmodules()
    if fshowtiming then PRINTLN "LOAD",=TT fi

    addspecialtypes()

end

proc do_parse=
    if passlevel<parse_pass then return fi

    if fwritedocs then
        docfile:=fopen(changeext(outfile,"txt"),"w")
    fi

INT TT:=CLOCK()

    for i:=2 to nmodules do
        parsemodule(i)
    od
    parsemodule(1)
TT:=CLOCK()-TT
if fshowtiming then PRINTLN "PARSE",=TT fi

    if docfile then
        fclose(docfile)
    fi

    if not debugmode or passlevel>=fixup_pass then
        fixusertypes()
    fi

    fixstartprocs()

    if debugmode and fshowast1 then showast("AST1") fi
end

proc do_name=
    if passlevel<name_pass then return fi

    rx_typetable()

!   tx_typetable()
!   fixblockparams()

INT TT:=CLOCK()
    for i:=2 to nmodules do
        rx_module(i)
    od
    rx_module(1)
TT:=CLOCK()-TT
if fshowtiming then PRINTLN "NAME",=TT fi

    if debugmode and fshowast2 then showast("AST2") fi
end

proc do_type=
    if passlevel<type_pass then return fi

INT TT:=CLOCK()
    tx_typetable()

    for i:=1 to nmodules do
        tx_module(i)
    od

    tx_allprocs()
    if fshowtiming then PRINTLN "TYPE",CLOCK()-TT fi

    if debugmode and fshowast3 then showast("AST3") fi
end

proc initdata=
    pcm_init()
    lexsetup()
    initassemsymbols()
    init_tt_tables()
    initbblib()

    nilunit:=malloc(16)

    if os_iswindows() then
        fwindows:=1
    else
        flinux:=1
    fi

!   case target
!   when 'X64' then
!       fx64:=1
!       if flinux then loaderror("Linux/x64") fi
!   else
!       loaderror("Bad os/target")
!   esac
end

proc getinputoptions=
    const slash='-'
    int i,j,k
    int paramno,pmtype,sw,ncolons,passfixed
    ichar name,value,filename,ext
    [300]char filespec

    prodmode:=1
    paramno:=1
    ncolons:=0

    if eqstring(extractfile(os_gethostname()),"ms.exe") then
        msfile:=1
        do_option(run_sw, "")
    fi

    while pmtype:=nextcmdparamnew(paramno,name,value,langext) do
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
            if inputfile then
                loaderror("Specify one lead module only")
            fi
            convlcstring(name)
            inputfile:=pcm_copyheapstring(name)

            if passlevel=run_pass then
                cmdskip:=paramno-1+$CMDSKIP
                exit
            fi

        when pm_libfile then
            loaderror("Lib files go in module headers")
        else
            loaderror("Invalid params")
        esac

    od


    if prodmode=debugmode=0 then
        passlevel:=exe_pass
        outext:="exe"
        if flinux then outext:="" fi
        prodmode:=1
    elsif prodmode and passlevel=0 then
        passlevel:=exe_pass
        outext:="exe"
        if flinux then outext:="" fi
    elsif debugmode and passlevel=0 then
        passlevel:=mcl_pass
        outext:="asm"
    fi

    if msyslevel=-1 then
        msyslevel:=(prodmode|2|0)
        msyslevel:=(prodmode|2|0)
    fi

    if inputfile=nil then
        showcaption()
        println "Usage:"
        println "   ",,cmdparams[0],"filename[."+langext+"]     # Compile project to executable"
        println "   ",,cmdparams[0],"-help            # Other options"
        stop

    else
        filename:=inputfile                 !primary file name
!default output
        outfile:=pcm_copyheapstring(filename)
        if fwritema then
            outext:="ma"
        fi

        if destfilename then
            outfile:=destfilename
        elsif destfilepath then
            strcpy(&.filespec,destfilepath)
            strcat(extractfile(&.filespec), outfile)
            outfile:=pcm_copyheapstring(&.filespec) 
        fi
    fi

    asmfilename:=getoutfilename(outfile,(ctarget|"c"|"asm"))
    exefilename:=getoutfilename(outfile,"exe")
    libfilename:=getoutfilename(outfile,(libmode|"ml"|"mx"))

    objfilename:=getoutfilename(outfile,"obj")
    mafilename:=getoutfilename(outfile,langextma)

    strcpy(filespec,changeext(outfile,""))
    strcat(filespec,"_exp")
    expfilename:=getoutfilename(filespec,langext)
end

proc do_option(int sw, ichar value)=
    static byte outused, outpathused

    switch sw
    when header_sw then passlevel:=header_pass
    when load_sw then passlevel:=load_pass
    when parse_sw then passlevel:=parse_pass
    when fixup_sw then passlevel:=fixup_pass
    when name_sw then passlevel:=name_pass
    when type_sw then passlevel:=type_pass
    when asm_sw then passlevel:=asm_pass; outext:="asm"
    when c_sw then
        if not ctarget then loaderror("-c not allowed") fi
         passlevel:=asm_pass; outext:="c"
    when mcl_sw then passlevel:=mcl_pass; outext:="asm"
    when obj_sw then passlevel:=objpass; outext:="obj"
    when exe_sw then passlevel:=exe_pass; outext:="exe"
    when mexe_sw then passlevel:=lib_pass; outext:="exe"; mxstub:=1
    when mx_sw then passlevel:=lib_pass; outext:="mx"
    when ml_sw then passlevel:=lib_pass; outext:="ml"; libmode:=1
    when run_sw then passlevel:=run_pass; outext:="mem";

    when ma_sw then fwritema:=1; outext:=langextma
    when mas_sw then fwritema:=2; outext:=langextma
    when export_sw then fwriteexports:=1
    when docs_sw then fwritedocs:=1
    when lib_sw then libmode:=1

    when sys_sw then msyslevel:=2
    when minsys_sw then msyslevel:=1
    when nosys_sw then msyslevel:=0
    when minos_sw then minos:=1
    when nofile_sw then fnofile:=1

    when opt_sw then foptim:=2
    when opt1_sw then foptim:=1
    when opt2_sw then foptim:=2

    when debug_sw then debugmode:=1; prodmode:=0

    when time_sw then fshowtiming:=1

    when v_sw then fverbose:=2

    when vv_sw then fverbose:=3

    when quiet_sw then fverbose:=0

    when help_sw,help2_sw then showhelp(); stop

    when ext_sw then dointlibs:=0

    when out_sw then
        if outpathused then loaderror("mixed out/path") fi
        destfilename:=pcm_copyheapstring(value)
        outused:=1

    when outpath_sw then
        if outused then loaderror("mixed out/path") fi
        if (value+strlen(value)-1)^ not in ['\\','/'] then
            loaderror("Path needs to end with \\ or /")
        fi
        destfilepath:=pcm_copyheapstring(value)
        outpathused:=1

    when unused_sw then fcheckunusedlocals:=1

    when ast1_sw then fshowast1:=1
    when ast2_sw then fshowast2:=1
    when ast3_sw then fshowast3:=1
    when showmx_sw then fshowmx:=1
    when showasm_sw then fshowasm:=1
    when st_sw then fshowst:=1
    when stflat_sw then fshowstflat:=1
    when types_sw then fshowtypes:=1
    when overloads_sw then fshowoverloads:=1
    when ss_sw then fshowss:=1
    when showmodules_sw then fshowmodules:=1
    when shortnames_sw then fshortnames:=1

    when gcc_sw then ccompiler:=gcc_cc
    when tcc_sw then ccompiler:=tcc_cc
    when tc_sw then ccompiler:=tc_cc
    when bcc_sw then ccompiler:=bcc_cc

    when linux_sw then flinux:=1; fwindows:=0
    when modinfo_sw then fmodinfo:=1

    endswitch

end

proc showcaption=
    println langnameuc,"Compiler [M6]", $date, $time
end

global proc showhelp=
    static ichar helptext=strinclude(langhelpfile)
    println helptext
end

global proc initassemsymbols=
!initialise hash table from kwddata
    [32]char str
    int i

    for i to md.mclnames.len when i<>m_sub do
        addreservedword(md.mclnames[i]+2,asmopcodesym,i)
    od

    for i to md.dregnames.len do
        addreservedword(md.dregnames[i],regsym,md.regindices[i],md.regsizes[i])
    od


    for i to md.xmmregnames.len do
        addreservedword(md.xmmregnames[i],xregsym,i+xr0-1)
    od

    for i to md.fregnames.len do
        addreservedword(md.fregnames[i],fregsym,i)
    od

    for i to md.mregnames.len do
        addreservedword(md.mregnames[i],mregsym,i)
    od

    for i to md.jmpccnames.len do
        addreservedword(md.jmpccnames[i],jmpccsym,md.jmpcccodes[i])
    od

    for i to md.setccnames.len do
        addreservedword(md.setccnames[i],setccsym,md.setcccodes[i])
    od

    for i to md.cmovccnames.len do
        addreservedword(md.cmovccnames[i],movccsym,md.cmovcccodes[i])
    od

!for i to segmentnames.len do
    for i to segmentnames.upb do
        strcpy(&.str,segmentnames[i])
        str[strlen(&.str)-3]:=0
        addreservedword(pcm_copyheapstring(&.str),segnamesym,i)
    od

    static []ichar regnames=("aframe","dframe","astack","dstack","dprog","dsptr")
    static []byte regnos=(r14,r14, r15,r15, r8, r9)
    static []byte sizes=(4,8,4,8,8,8)
    for i to regnames.len do
        addreservedword(regnames[i], regsym, regnos[i], sizes[i])
    od

end

proc do_writeexports=
    [300]char str

    if not fwriteexports and passlevel<>lib_pass then
        return
    fi

    if not libmode then return fi

    writeexports(expfilename,extractbasefile(libfilename))
    if fwriteexports then
        stop
    fi
end

function getoutfilename(ichar file,ext)ichar=
    return pcm_copyheapstring(changeext(file,ext))
end

proc fixstartprocs=
!make sure each module has a start proc
!make sure the lead module has a main proc
    ref modulerec ms
    symbol d
    unit p, q

    for i to nmodules do
        ms:=&moduletable[i]
        if ms.ststart=nil then
            ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
        fi

        if ms.modulecode then
            p:=makeblock(ms.modulecode)
            q:=ms.ststart.code                  !will be a block
            p.nextunit:=q.a
            ms.ststart.code.a:=p
        fi

!add automatic main proc, but only if modulecode exists
        if i=mainmoduleno and ms.stmain=nil and ms.modulecode then
            ms.stmain:=addstartproc(ms.stmodule,"main", export_scope,i)
        fi
    od
end

function addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
    stproc:=getduplnameptr(owner,addnamestr(name),procid)
    stproc.scope:=scope
    stproc.moduleno:=moduleno
    stproc.subprogno:=moduletosub[moduleno]
    stproc.code:=makeblock(nil)
    adddef(owner,stproc)
    addtoproclist(stproc)

    return stproc
end

proc stepruncount=
    int count
    filehandle f:=fopen(langhomedir+"/bcrun.txt","r+")
    return when not f
    readln @f,count
    fseek(f,0,seek_set) !restore position
    println @f,count+1
    fclose(f)   
end

global proc showmoduleinfo=
    const file="$temp"
    filehandle f
    ref modulerec pm

    println "Writing to",file
    f:=fopen(file, "wb")

    if projectmodule then
        println @f,"module",projectmodule
    fi

    for i to nmodules do
        pm:=&moduletable[i]
        fprintln @f,"module ##.m", (pm.path|pm.path|"--"),pm.name
    od

    fclose(f)
end

=== mm_assem.m 0 0 3/33 ===
global function readassemline:unit=
    lex()
    return assembleline(1)
end

global function readassemblock:unit=
!read lines of assembler after < or assem
!fend=1 when terminated by 'end', 0 when terminator is '>'
!return single nassem unit or nsunit containing multiple nassems
    unit ulist,ulistx,u

    ulist:=ulistx:=nil

    do
        lex()           !first symbol on line
        case lx.symbol
        when eofsym then
            serror("EOF: 'End' missing in Assembler code")
        when kendsym then
            checkend(lx.symbol,kassemsym)
            lex()
            exit
        when semisym then       !assume blank line
        else                !assume some asm code follows
            u:=assembleline(0)
            addlistunit(ulist,ulistx,u)
        esac
    od

    return makeblock(ulist)
end

function assembleline(int oneline)unit=
!1st symbol of possible assembler line has been read
!assemble following symbols, end at eol or other separater symbol
!return nassem unit

!const escapesym=atsym
    unit dlist,dlistx,p,pname,q
    ichar name
    int opc,noperands
    ref strec stname

    dlist:=dlistx:=nil

!look at the start of a line first

    if lx.symbol=namesym and nextlx.symbol in [colonsym,dcolonsym] then !normal label
        p:=createunit0(jlabeldef)
        stname:=getduplnameptr(currproc,lx.symptr,labelid)
        p.def:=stname
        adddef(currproc,stname)
        lex()           !skip colon
        if oneline then
            lex()
        fi
        return p

    elsif lx.symbol=mulsym then     !*name  macro invocation
        lexchecksymbol(namesym)
        pname:=createname(lx.symptr)
        pname.pos:=lx.pos

        lex()
        if lx.symbol<>semisym then
            repeat
                addlistunit(dlist,dlistx,readunit())
                if lx.symbol=commasym then
                    lex()
                fi

            until lx.symbol in [semisym,eofsym]
        fi

        return createunit2(jassemmacro,pname,dlist)
    fi

    case lx.symbol
    when andlsym then
        opc:=m_andx
    doop::
        p:=createunit0(jassem)
        p.asmopcode:=opc
        lex()
    when orlsym then
        opc:=m_orx
        goto doop

    when xorlsym then
        opc:=m_xorx
        goto doop

    when notlsym then
        opc:=m_notx
        goto doop
    when kprocsym then
        if lx.subcode=1 then
            opc:=m_sub
            goto doop
        fi
!       recase else
        $else

    elsif lx.symbol=namesym then                !assume opcode

        p:=createunit0(jassem)

        case lx.subcode
        when asmopcodesym then
            p.asmopcode:=lx.symptr.index

        when jmpccsym then
            p.asmopcode:=m_jmpcc
            p.cond:=lx.symptr.index
        when setccsym then
            p.asmopcode:=m_setcc
            p.cond:=lx.symptr.index
        when movccsym then
            p.asmopcode:=m_cmovcc
            p.cond:=lx.symptr.index
        else
    PS("ASM")
            serror("x64 op expected")
        esac

        lex()
    else
$else::
    PS("ASM")
        SERROR("ASM???")
    esac

!any labels and opcodes have been read; now look at any operands
    if lx.symbol not in [semisym,eofsym] then

    noperands:=0

        do
            q:=readassemopnd()

            if ++noperands<=3 then
                p.abc[+noperands]:=q
            else
                serror("Too many asm opnds")
            fi

            if lx.symbol<>commasym then
                exit
            else
                lex()
            fi
        od

    fi

    checksymbol(semisym)

    return p
end

function readassemopnd:unit p =
!positioned at 1st symbol of an assem operand, which is not ; or eol or eof
    int reg,regix,scale,prefixmode
    unit pcode

    case lx.symbol
    when intconstsym,realconstsym then
        return readunit()
    when namesym then
        case lx.symptr.subcode
        when regsym then
            p:=createunit0(jassemreg)
            p.index:=lx.symptr.index
            p.regsize:=lx.symptr.regsize
            lex()
            return p
        when xregsym then
            p:=createunit0(jassemxreg)
            p.index:=lx.symptr.index
            lex()
            return p
        esac
        return readunit()
    when addsym, subsym then
        return readunit()

    when stdtypesym then
        case lx.subcode
        when tu8,tu16,tu32,tu64 then
        else
            serror("Bad prefix")
        esac
        prefixmode:=lx.subcode
        lexchecksymbol(lsqsym)
        goto gotprefix

    when lsqsym then
        prefixmode:=tvoid
gotprefix::
        reg:=regix:=0
        pcode:=nil
        scale:=1

        lex()
        if lx.symbol=namesym and lx.symptr.subcode=regsym then
            reg:=lx.symptr.index
            lex()
        fi

!       if lx.symbol=addsym and nextlx.symbol=namesym and nextlx().symptr.subcode=regsym then
        if lx.symbol=addsym and nextlx.symbol=namesym and nextlx.symptr.subcode=regsym then
            lex()
        fi
        if lx.symbol=namesym and lx.symptr.subcode=regsym then
            regix:=lx.symptr.index
            lex()
        fi

        if lx.symbol=mulsym then
            lexchecksymbol(intconstsym)
            case scale:=lx.value
            when 1,2,4,8 then
            else
                serror("Bad scale")
            esac
            lex()
        fi

        case lx.symbol
        when addsym, subsym, intconstsym, namesym, lbracksym,ksyscallsym then
            pcode:=readunit()
        esac
        checksymbol(rsqsym)
        lex()
        p:=createunit1(jassemmem,pcode)
        if regix=0 and scale>1 then
            regix:=reg
            reg:=0
        fi
        if pcode=nil and reg+regix=0 then serror("Empty []") fi
        p.reg:=reg
        p.regix:=regix
        p.scale:=scale
        p.prefixmode:=prefixmode
        return p

    else
        PS("BAD OPND")
        serror("ASM: Bad operand?")
    esac
    return nil
end

=== mc_blockc.m 0 0 4/33 ===
const maxnestedloops    = 50

[maxnestedloops,4]int loopstack
int loopindex                           !current level of nested loop/switch blocks

global int blocklevel

global proc evalunit(unit p)=
    unit a,b,c
    symbol d

    if p=nil then return fi
    mlineno:=p.pos

    a:=p.a
    b:=p.b
    c:=p.c

    switch p.tag
    when jconst then
        do_const(p)

    when jname then
        d:=p.def
        if d.nameid=labelid then
            if d.index=0 then
                d.index:=++mlabelno
            fi
            ccstr("goto ")
            ccstrsemi(genclabel(d.index,0))
        else
            applymemmode(p)
            dxstr(getfullnamec(d))
        fi

    when jblock then                !assume an exprlist
        do_block(p)

    when jreturn then
        do_return(p,a)

    when jassign then
        do_assign(p,a,b)

!   when jassignms then
!       do_assignms(a,b)

!   when jassignmm      then do_assignmm(a,b)

    when jeval then
        evalunit(a)
        ccstrline(";")

    when jto then
        do_to(a,b,c)

    when jif then
        if isexpr(p) and not p.ifretflag then
            do_ifx(a,b,c)
        else
            do_if(a,b,c)
        fi

    when jforup, jfordown then
        do_forup(p,a,b,c)

    when jforall then
        do_forall(p,a,b,c)
!
!!  when jforallrev     then do_forall(p,a,b,c,1)

    when jwhile then
        do_while(a,b,c)

    when jrepeat then
        do_repeat(a,b)

    when jgoto then
        do_goto(a)

    when jlabeldef then
        d:=p.def
        if d.index=0 then
            d.index:=++mlabelno
        fi
        ccstr("//")
        ccstr(d.name)
        ccstrline(":")
        ccstrline(genclabel(d.index,1))
!
    when jredo, jnext, jexit then
        do_exit(p)

    when jdo then
        do_do(a)

    when jcase,jdocase then
        do_case(p,a,b,c)


    when jswitch,jdoswitch then
        do_switch(p,a,b,c)

!!  when jrecase        then do_recase(p,a)
    when jswap then
        do_swap(a,b)
!
    when jselect then
        do_select(a,b,c)
!
    when jprint,jprintln, jfprint,jfprintln then
        do_print(p,a,b)

    when jread then
        do_read(p,a)

    when jreadln then
        do_readln(a)

    when jstop then
        ccstr("exit(")
        if a then
            evalunit(a)
        else
            dxchar('0')
        fi
        dxchar(')')

!   when jandl then
!       tx:=do_andl(p,a,b, reg)
!
!   when jorl then
!       tx:=do_orl(p,a,b, reg)
!!
!!  when jmakerange     then GENPC_COMMENT("MAKERANGE")
    when jcallfn, jcallproc then
        do_call(p,a,b, p.tag=jcallfn)
!!
!   when jcmp then
!       tx:=do_setcc(p,a,b, reg)
!
!   when jcmpchain then
!       tx:=do_setccchain(p,a, reg)
!!
    when jbin, jandl, jorl, jcmp then
        do_bin(p,a,b)
!
    when jindex then
        do_index(p,a,b)

!   when jslice then
!       tx:=do_slice(p,a,b, reg)
!
    when jdotindex then
        do_supportcall("getdotindex",a,b)

    when jdotslice then
!PRINTUNIT(P)
        do_supportcall("getdotslice",a,b.a, b.b)
!       do_dotslice(a,b.a,b.b)

!
    when jdot then
        applymemmode(p)
        evalunit(a)
        dxchar('.')
        evalunit(b)
!
    when jptr then
        applymemmode(p)
        if a.tag=jaddrof then
            evalunit(a.a)
        else
            dxstr("(*")
            evalunit(a)
            dxchar(')')
        fi

!       tx:=do_ptr(p,a,1,reg)
!
    when jaddrof then
        case a.tag
        when jptr then
            evalunit(a.a)
        when jif then
            do_ifx(a.a,a.b,a.c, 1)
        when jcallfn, jcallproc then
            evalunit(a)
        else
            dxstr("&")
            evalunit(a)
        esac
    when jaddroffirst then
!       ccstr("(u8*)")
        evalunit(a)
!
    when jconvert then
        do_convert(p,a)
!
    when jtypepun then
        do_typepun(p,a)

    when jshorten then
        dxstr("(")
        dxstr(strmodec(p.mode))
        dxstr(")")
        evalunit(a)
!       tx:=loadunit(a,reg)

!   when jtypeconst then
!       tx:=genint(p.value)
!!
    when junary, jnotl then
        do_unary(p,a)
!
!   when jnotl then
!       tx:=do_notl(p,a, reg)
!
    when jistruel then
        dxstr("!!(")
        evalunit(a)
        dxstr(")")
!!
    when jincr          then
        case p.pclop
        when kincr, kincrload then
            ccstr("++(")
            evalunit(a)
            ccstr(")")
        when kdecr, kdecrload then
            ccstr("--(")
            evalunit(a)
            ccstr(")")
        when kloadincr then
            ccstr("(")
            evalunit(a)
            ccstr(")++")
        else
            ccstr("(")
            evalunit(a)
            ccstr(")--")
        esac

    when jbinto then
        do_binto(p,a,b)

    when jsyscall then
        ccstr("msysc$m_")
        ccstr(sysfnnames[p.fnindex]+3)
        ccchar('(')
        if a then
            evalunit(a)
        fi
        ccchar(')')
!
    when jcvlineno      then
        ccint(getlineno(mlineno))

    when jempty then
        dxstr("memset(&(")
        evalunit(a)
        dxstr("),0,")
        dxint(ttsize[a.mode])
        dxchar(')')

    when jemitc then
        ccstr(p.svalue)

    when jinrange then
        do_inrange(a,b.a,b.b)

    when jinset then
        do_inset(a,b.a)

    when jcmpchain then
        do_cmpchain(p)

    when jinfinity then
        ccstr("m$infinity")

    else
        gerror_s("Evalunit: ",jtagnames[p.tag])
    endswitch

end

global proc evalstmt(unit p)=
    cctab(blocklevel)
    evalunit(p)
    if p.tag<>jblock then
        ccstrline(";")
    fi
end

global proc evalunitc(unit p)=
!if a block, turn into a comma-operator sequence
    unit q

    if p.tag<>jblock then
        evalunit(p)
        return
    fi

    dxchar('(')
    q:=p.a
    while q, q:=q.nextunit do
        if q<>p.a then
            dxstr(", ")
        fi
        evalunit(q)
    od

    dxchar(')')
end

proc evalblock(unit p, int braces=1)=
!p is either a block or a non-block
!always treat as a block
    unitrec r
    unit pnext

    if p.tag=jblock then
        do_block(p,braces)
    else
        r.tag:=jblock
        r.a:=p
        pnext:=p.nextunit
        p.nextunit:=nil             !some single blocks are chained to others,
                                    ! but do not form a sequence (eg. forbody/else)
        do_block(&r,braces) 
        p.nextunit:=pnext

    fi
end

global proc do_block(unit p, int braces=1)=
    unit a:=p.a

    ++blocklevel
    if braces then ccstrline("{") fi
    while a, a:=a.nextunit do
        evalstmt(a)
    od
    --blocklevel
    if braces then
        cctab(blocklevel)
        ccstrline("}")
    fi
end

global proc evalblocklab(unit p,int lab1,lab2, unit pincr=nil, int labincr=0, braces=1)=
!p is either a block or a non-block
!always treat as a block
    unitrec r
    unit pnext

    if p.tag=jblock then
        do_blocklab(p,lab1,lab2, pincr, labincr,braces)
    else
        r.tag:=jblock
        r.a:=p
        pnext:=p.nextunit
        p.nextunit:=nil             !some single blocks are chained to others,
                                    ! but do not form a sequence (eg. forbody/else)
        do_blocklab(&r,lab1,lab2, pincr, labincr, braces)   
        p.nextunit:=pnext
    fi
end


global proc do_blocklab(unit p, int lab1,lab2, unit pincr, int labincr, braces=1)=
    unit a:=p.a

    if braces then ccstrline("{") fi

    if lab1 then
        dxlabel(lab1)
    fi

    ++blocklevel
    while a do
        evalstmt(a)
        a:=a.nextunit
    od
    if lab2 then
        dxlabel(lab2)
    fi

    if pincr then
        evalstmt(pincr)
        definefwdlabel(labincr)
    fi

    --blocklevel
    if braces then
        cctab(blocklevel)
        ccstrline("}")
    fi
end

proc do_const(unit p)=
    [256]char str
    int64 a
    word64 u
    int m

    str[1]:=0
    m:=p.mode

    if ttsigned[m] then
        a:=p.value
        case ttbasetype[p.mode]
        when ti8 then print @&.str,"(i8)",,a
        when ti16 then print @&.str,"(i16)",,a
        when ti32 then
            if a=int32.minvalue then
                dxstr("(int32_t)(-2147483647-1)")
            else
                print @&.str,"(int32_t)",,a
            fi
        when ti64 then
            if a=int64.minvalue then
                dxstr("(i64)(-9223372036854775807-1)")
            else
                fprint @&.str,"(i64)",a
            fi
        ELSE
    GERROR("CONST/INT")
        esac
    elsif ttisinteger[m] or m=tbool64 then
        u:=p.uvalue
        case ttbasetype[p.mode]
        when tu8 then fprint @&.str,"(u8)#u",u
        when tu16 then fprint @&.str,"(u16)#u",u
        when tu32 then fprint @&.str,"(u32)#u",u
        when tu64,tbool64 then fprint @&.str,"(u64)#u",u
        when tc8,tc64 then
            if u<32 or u>126 or u='\\' or u='\'' then
                fprint @&.str,"(u64)#u",u
            else
                fprint @&.str,"'#'",u:"c"
            fi
        else
            gerror("const/word")
        esac

    elsif ttisref[m] then
        if p.tag=jconst and p.mode=trefchar and p.isastring then
            if p.svalue then
                cclongstr(p.svalue,p.slength)
                return
            else
                strcpy(&.str,"0")
            fi
        else
            print @&.str,p.value:"H"
        fi

    elsif ttisreal[m] then
        if ttbasetype[p.mode]=tr64 then
            fprint @&.str,"(double)",p.xvalue:".20g"
            if strchr(&.str,'.')=nil then
                strcat(&.str,".")
            fi
        else
            print @&.str,"(float)",,p.xvalue
        fi
    else
        cpl =strmode(m),ttisref[m]
        gerror("doconst")
    fi

    dxstr(&.str)
end

proc do_return(unit P,a)=
    if not a then
        ccstr("return")
    else
IF A.TAG=JCONVERT AND A.A.TAG=JBLOCK THEN
PRINTLN "RETURN CONV BLOCK"
FI
        ccstr("return ")
        evalunit(a)
    fi
end

proc do_assign(unit p,a,b)=

    case a.tag
    when jdotindex then
        evalunit(a.a)
        dxstr(" = ")
        do_supportcall("setdotindex",a.a,a.b,b)

    when jdotslice then
        evalunit(a.a)
        dxstr(" = ")
        do_supportcall("setdotslice",a.a,a.b.a, a.b.b, b)

    elsif ttbasetype[a.mode]=tarray then
        if p.resultflag then gerror("Block assign returning value?") fi
        do_blockcopy(a,b)
    else
        if p.resultflag then dxchar('(') fi
normalassign::
        evalunit(a)
        dxstr(" = ")
        evalunit(b)

        if p.resultflag then dxchar(')') fi
    esac

end

proc do_bin(unit p,a,b)=
    ichar opstr

    case p.pclop
    when kmin,kmax then
        do_max(p,a,b)
        return
    when kpower then
        if ttisinteger[a.mode] then
            do_supportcall("power_i64",b,a)
        else
            domaths2("pow",a,b)
        fi
        return
    esac

    dxchar('(')
    evalunit(a)
    dxchar(' ')

    opstr:=ccopnames[p.pclop]
    if opstr=nil then
        gerror_s("No C binop:",pclnames[p.pclop])
    fi

    dxstr(opstr)
    dxchar(' ')
    evalunit(b)
    dxchar(')')
end

proc do_binto(unit p,a,b)=
    ichar opstr

    case p.pclop
    when kminto,kmaxto then
        do_maxto(p,a,b)
        return
    esac

    evalunit(a)

    opstr:=ccopnames[p.pclop]
    if opstr=nil then
        gerror_s("No C bintoop:",pclnames[p.pclop])
    fi

    dxchar(' ')
    dxstr(opstr)
    dxchar(' ')
    evalunit(b)
end

proc do_unary(unit p,a)=
    ichar opstr

    case p.pclop
    when kabs then
        if ttisreal[a.mode] then
            opstr:="fabs"
        else
            opstr:="m$llabs"
        fi
    when ksqr then
        dxchar('(')
        evalunit(a)
        dxchar('*')
        evalunit(a)
        dxchar(')')
        return
    when ksqrt, ksin, kcos, ktan, kasin, kacos, katan, kexp,
            kfloor, kceil then
        opstr:=pclnames[p.pclop]+1

    when kln then
        opstr:="log"

    when klog then
        opstr:="log10"

    when klen then
        evalunit(a)
        dxstr(".len")
        return
    when ksliceptr then
        dxstr("((")
        dxstr(strmodec(tttarget[a.mode]))
        dxstr("*)")
        evalunit(a)
        ccstr(".ptr)")
        return

    else
        opstr:=ccopnames[p.pclop]
        if opstr=nil then
            gerror_s("No C unaryop:",pclnames[p.pclop])
        fi
    esac

    dxstr(opstr)
    dxchar('(')
    evalunit(a)
    dxchar(')')
end

proc do_convert(unit p,a)=
    [100]char str
    int oldmode:=p.convmode, newmode:=p.mode, mask

    case p.pclop
    when kfloat,kfix, kfwiden, kfnarrow, ksoftconv then

docast::
        dxstr("(")
        dxstr(strmodec(newmode))
        dxstr(")")
        evalunit(a)

    when ktruncate then
        dxchar('(')
        dxstr(strmodec(newmode))
        dxchar(')')
        dxchar('(')
        dxstr(strmodec(oldmode))
        dxchar(')')
        evalunit(a)
    else
error::
        fprint @str,"#-># (#)",strmode(oldmode), strmode(newmode),pclnames[p.pclop]
        gerror_s("Convert ",str)
    esac
end

proc do_if(unit pcond, plist, pelse)=
    unit pcond0:=pcond

    while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
        if pcond=pcond0 then
            ccstr("if (")
        else
            ccstr("else if (",blocklevel)
        fi
        evalunit(pcond)
        ccstr(") ")
        evalblock(plist)
    od

    if pelse then
        ccstr("else ",blocklevel)
        evalblock(pelse)
    fi
end

proc do_ifx(unit pcond, a, b, int addrof=0)=
    unit pcond0:=pcond

    dxchar('(')
    evalunit(pcond)
    dxstr(" ? ")
    if addrof then dxchar('&') fi
    if a.tag=jconvert then a:=a.a fi
    evalunit(a)
    dxstr(" : ")
    if addrof then dxchar('&') fi
    if b.tag=jconvert then b:=b.a fi
    evalunit(b)
    dxchar(')')
end

proc do_call(unit p,a,b, int isfn)=
    unit q

    if a.tag=jname then
        dxstr(getprocname(a.def))
    else
        dxchar('(')
        evalunit(a)
        dxchar(')')
    fi
    dxchar('(')

    q:=b
    while q do
        evalunit(q)
        q:=q^.nextunit
        if q then
            dxchar(',')
        fi
    od

    dxchar(')')
end

proc do_do(unit a)=
    int lab_abc,lab_d


    lab_abc:=definelabel()
    lab_d:=createfwdlabel()

    stacklooplabels(lab_abc, lab_abc, lab_d)
    ccstr("while (1) ")
    evalblock(a)

    definefwdlabel(lab_d)
    unstacklooplabels()
end

function definelabel:int=
    ++mlabelno
    ccstr(genclabel(mlabelno,1))
    ccsendline()
    cctab(blocklevel)

    return mlabelno
end

function createfwdlabel:int=
    return ++mlabelno
end

proc definefwdlabel(int lab)=
    ccstr(genclabel(lab,1))
    ccsendline()
    cctab(blocklevel)
end

proc stacklooplabels(int a,b,c)=
    ++loopindex
    if loopindex>maxnestedloops then
        gerror("Too many nested loops")
    fi

    loopstack[loopindex,1]:=a
    loopstack[loopindex,2]:=b
    loopstack[loopindex,3]:=c
end

proc unstacklooplabels= !UNSTACKLOOPLABELS
    --loopindex
end

function findlooplabel(int k,n)int=
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
    int i

    i:=loopindex-(n-1)      !point to entry
    if i<1 or i>loopindex then gerror("Bad loop index") fi

    return loopstack[i,k]
end

proc do_exit(unit p)=
    int k,n,index

    switch p.tag
    when jredo then k:=1
    when jnext then k:=2
    when jexit then k:=3
    endswitch

    index:=p.index
    if index=0 then index:=loopindex fi

    n:=findlooplabel(k,index)
    if n=0 then
        gerror("Bad exit/loop index",p)
    else
        ccstr("goto ")
        ccstr(genclabel(n,0))
    fi
end

proc do_to(unit a,b,c)=
    int lab_b, lab_c, lab_d

    ccstr(getfullnamec(c.def))

    ccstr(" = ")
    ccstrsemiu(a)

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    stacklooplabels(lab_b,lab_c,lab_d)

    cctab(blocklevel)
    ccstr("while (")
    ccstr(getfullnamec(c.def))
    ccstr("-- > 0) ")

    evalblocklab(b,lab_b,lab_c)         !main body

    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_while(unit a,b, pincr)=
    int lab_b,lab_c,lab_d, lab_incr

    lab_b:=definelabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    if pincr then
        lab_incr:=createfwdlabel()
    else
        lab_incr:=lab_c
    fi

    stacklooplabels(lab_b, lab_c, lab_d)

    ccstr("while (")
    evalunit(a)
    ccstr(") ")

    evalblocklab(b,0,lab_c, pincr, lab_incr)

    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc do_repeat(unit a,b)=
    int lab_ab, lab_c, lab_d

    lab_ab:=definelabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    stacklooplabels(lab_ab, lab_c, lab_d)

    ccstr("do ")

    evalblocklab(a,0,lab_c)

    ccstr("while (!",blocklevel)
    evalunit(b)
    ccstrline(");")

    definefwdlabel(lab_d)
    unstacklooplabels()
end


proc do_forup(unit p, pindex, pfrom, pbody)=
    int lab_b,lab_c,lab_d,down
    unit pto,pstep,pelse,px, ptoinit
    [512]char varstr

!proc do_for(unit p,pindex,pfrom, pbody, int down) =

    down:=p.tag=jfordown

    pto:=pfrom.nextunit
    pstep:=pto.nextunit
    pelse:=pbody.nextunit
    ptoinit:=pindex.nextunit

    if pto.tag=jptr then
        px:=pto.a
        symbol d
        if px.tag=jname and (d:=px.def).nameid=paramid and d.parammode=out_param then
            gerror("Possibly using &param as for-loop limit")
        fi
    fi

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    stacklooplabels(lab_b,lab_c,lab_d)

    if pindex.tag<>jname then gerror("for/name") fi

    if ptoinit then
        evalstmt(ptoinit)
        cctab(blocklevel)
    fi

    strcpy(&.varstr,getfullnamec(pindex.def))
    ccstr("for (")
    ccstr(&.varstr)
    ccchar('=')
    evalunit(pfrom)
    ccchar(';')
    ccstr(&.varstr)
    ccstr((down|">="|"<="))
    evalunit(pto)
    ccchar(';')

    if pstep then
        ccstr(&.varstr)
        ccstr((down|"-="|"+="))
        evalunit(pstep)
    else
        ccstr((down|"--"|"++"))
        ccstr(&.varstr)
    fi

    ccstr(") ")

    evalblocklab(pbody,lab_b,lab_c)             !do loop body

    if pelse then
!       ccsendline()
        cctab(blocklevel)
        evalblock(pelse)
    fi

    definefwdlabel(lab_d)

    unstacklooplabels()
end

proc do_forall(unit p,pindex,plist, pbody) =
!Structure:
!   forall
!       pindex -> plocal -> pfrom -> pto
!       plist -> passign
!       pbody -> [pelse]

    unit plocal, pfrom, pto, pelse, px, plimit, passign
    int lab_b,lab_c,lab_d,lab_e, lab_cmp
    int a,b,stepx, down:=0
    operand ax, bx
    [512]char varstr

    plocal:=pindex.nextunit
    pfrom:=plocal.nextunit
    pto:=pfrom.nextunit
    passign:=plist.nextunit
    pelse:=pbody.nextunit

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    lab_cmp:=createfwdlabel()

    if pelse then
        lab_e:=createfwdlabel()
    else
        lab_e:=lab_d
    fi

    stacklooplabels(lab_b, lab_c, lab_d)

    strcpy(&.varstr,getfullnamec(pindex.def))
    ccstr("for (")
    ccstr(&.varstr)
    ccchar('=')
    evalunit(pfrom)
    ccchar(';')
    ccstr(&.varstr)
    ccstr((down|">="|"<="))
    evalunit(pto)
    ccchar(';')

    ccstr((down|"--"|"++"))
    ccstr(&.varstr)
    ccstrline(") {")

    evalstmt(passign)
!   definefwdlabel(lab_b)

    evalblocklab(pbody,lab_b,lab_c,braces:0)                !do loop body
    ccstr("}",blocklevel)

    if pelse then
        ccsendline()
        evalblock(pelse)
    fi


    definefwdlabel(lab_d)
    unstacklooplabels()
end

proc dxlabel(int lab) =
    ccstr(genclabel(lab,1))
    ccsendline()
end

proc do_print(unit p,a,b) =
    unit q,r,s
    int m,widenop
    ichar fn

    if a then
        if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
        case ttbasetype[tttarget[a.mode]]
        when tvoid then
            do_syscallproc("msysc$m_print_startfile",a)
        when tc8 then
            do_syscallproc("msysc$m_print_startstr",a)
        when tref then
            do_syscallproc("msysc$m_print_startptr",a)
        else
            gerror("@dev?")
        esac
    else
        do_syscallproc("msysc$m_print_startcon")
    fi

    q:=b

    case p.tag
    when jfprint,jfprintln then
        if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
            gerror("string expected")
        fi
        do_syscallproc("msysc$m_print_setfmt",q)
        q:=p.c
    esac

    while q do
        s:=nil
        case q.tag
        when jfmtitem then
            s:=q.b
            r:=q.a
            m:=r.mode
        when jnogap then
            do_syscallproc("msysc$m_print_nogap")
            q:=q.nextunit
            next
        when jspace then
            do_syscallproc("msysc$m_print_space")
            q:=q.nextunit
            next
        else
            s:=nilunit
            r:=q
            m:=q.mode
        esac

        widenop:=0
        case ttbasetype[m]
        when ti64 then
            fn:="msysc$m_print_i64"
        when ti8,ti16,ti32 then
            fn:="msysc$m_print_i64"
        when tu64 then
            fn:="msysc$m_print_u64"
        when tu8,tu16,tu32 then
            fn:="msysc$m_print_u64"
        when tr32 then
            fn:="msysc$m_print_r64"
        when tr64 then
            fn:="msysc$m_print_r64"
        when tref then
            if tttarget[m]=tc8 or ttbasetype[tttarget[m]]=tarray and tttarget[tttarget[m]]=tc8 then
!           if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
                fn:="msysc$m_print_str"
            else
                fn:="msysc$m_print_ptr"
            fi
        when tarray then
            GERROR("PRINTARRAY")
            q:=q.nextunit
        when trecord then
            GERROR("PRINTRECORD")
        when tc8,tc64 then
            fn:="msysc$m_print_c8"

        else
            gerror_s("PRINT/T=",strmode(m))
        esac

        if widenop then
            GERROR("WIDEN/PRINT")
        fi
        do_syscallproc(fn,r,s)
        q:=q.nextunit
    od

    case p.tag
    when jprintln,jfprintln then
        do_syscallproc("msysc$m_print_newline")
    esac
    do_syscallproc("msysc$m_print_end")

end

proc do_read(unit p,a) =
    int m
    m:=p.mode

    if a=nil then
        a:=createconstunit(0,ti64)
    fi

    if ttisinteger[m] then
        do_syscallproc("msysc$m_read_i64",a)
    elsif ttisreal[m] then
        do_syscallproc("msysc$m_read_r64",a)
    else
        GERROR("CAN'T READ THIS ITEM")
    fi
end

proc do_readln(unit a) =
    if a then
        if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
        case ttbasetype[tttarget[a.mode]]
        when tvoid then
            do_syscallproc("msysc$m_read_fileline",a)
        when tu8,tc8 then
            do_syscallproc("msysc$m_read_strline",a)
        else
            gerror("rd@dev?")
        esac
    else
        do_syscallproc("msysc$m_read_conline")
    fi
end

proc do_index(unit p,a,pindex)=
    int lower
    int addr
    ichar astr
    ref char px

    applymemmode(p)

    if ttbasetype[a.mode]=tslice then
        dxstr("((")
        dxstr(strmodec(tttarget[a.mode]))
        dxstr("*)")
        evalunit(a)
        ccstr(".ptr)")
    else
        evalunit(a)
    fi

    dxstr("[(")
    evalunit(pindex)
    dxchar(')')

    lower:=ttlower[a.mode]
    if lower>0 then
        dxchar('-')
        dxint(lower)
    elsif lower<0 then
        dxchar('+')
        dxint(abs lower)
    fi
    dxchar(']')
end

proc do_case(unit p,pindex,pwhenthen,pelse)=
    int loopsw
    int lab_abc,lab_d
    ichar indexstr
    unit w,wt

    loopsw:=p.tag=jdocase

    if loopsw then
        lab_abc:=definelabel()      !start of loop
        lab_d:=createfwdlabel() !end of case/end of loop
        stacklooplabels(lab_abc,lab_abc,lab_d)
    fi

    wt:=pwhenthen

    indexstr:=nil       
    if wt=nil then doelse fi

    if pindex.tag<>jname then
        cctab(blocklevel)
        ccstr("{")
        ccstr(strmodec(pindex.mode))
        ccstr(" $temp = ")
        evalunitc(pindex)
        ccstrline(";")
        indexstr:="$temp"
    fi

    while wt do
        if wt=pwhenthen then
            ccstr("if (")
        else
            ccstr("else if (",blocklevel)
        fi

        w:=wt.a
        while w do
            if w<>wt.a then
                ccstr(" || ")
            fi
            if pindex then
                ccchar('(')
                if indexstr then
                    ccstr(indexstr)
                else
                    evalunit(pindex)
                fi
                ccstr("==")

                evalunit(w)
                ccchar(')')
            else
                evalunit(w)
            fi
            w:=w.nextunit
        od
        ccstr(") ")

        evalblock(wt.b)
        wt:=wt.nextunit
    od

    if pelse then

        ccstr("else ",blocklevel)
doelse::
        evalblock(pelse)
    fi

    if indexstr then
        ccstr("}",blocklevel)
    fi


    if loopsw then
        ccstr("goto ")
        ccstrsemi(genclabel(lab_abc))
        definefwdlabel(lab_d)
        unstacklooplabels()
    fi
end

proc do_switch(unit p,pindex,pwhenthen,pelse)=
    int loopsw
    int lab_a,lab_b,lab_c,lab_d, x,y,i
    unit w,wt

    loopsw:=p.tag=jdoswitch
    if loopsw then
        lab_a:=definelabel()
        lab_d:=createfwdlabel()
        stacklooplabels(lab_a,lab_a,lab_d)
    fi

    ccstr("switch (")
    evalunitc(pindex)
    ccstrline(") {")

!scan when statements, o/p statements
    wt:=pwhenthen

    while wt do
        w:=wt.a
        while w do      !for each when expression
            case w.tag
            when jmakerange then
                x:=w.a.value
                y:=w.b.value
            when jconst then
                x:=y:=w.value
            esac

            for i:=x to y do
                cctab(blocklevel)
                ccstr("case ")
                ccint(i)
                if wt.b=nil and i=y then
                    ccstrline(":;// ")
                else
                    ccstrline(":;")
                fi
            od
            w:=w.nextunit
        od

        ++blocklevel
        cctab(blocklevel)
        evalblock(wt.b)
        cctab(blocklevel)
        ccstrsemi("break")
        --blocklevel
        wt:=wt.nextunit
    od

    if pelse then
        cctab(blocklevel)
        ccstr("default: ")
        evalblock(pelse)
    fi

    cctab(blocklevel)
    ccstrline("} //SW")

    if loopsw then
        ccstr("goto ")
        ccstrsemi(genclabel(lab_a))
        definefwdlabel(lab_d)
        unstacklooplabels()
    fi
end

proc do_goto(unit a)=
    symbol d

    case a.tag
    when jname then
        d:=a.def
        if d.index=0 then
            d.index:=++mlabelno
        fi
        case d.nameid
        when labelid then
            ccstr("goto ")
            ccstrsemi(genclabel(d.index,0))
        else
            gerror_s("Not label/block name: %s",d.name)
        esac
    else
        gerror("Can't do goto with label pointer")
    esac
end

proc do_max(unit p,a,b)=
    if issimplec(a) and issimplec(b) then
        dxchar('(')
        evalunit(a)
        dxchar((p.pclop=kmin|'<'|'>'))
        evalunit(b)
        dxchar('?')
        evalunit(a)
        dxchar(':')
        evalunit(b)
        dxchar(')')
    else
        dxstr("msysc$m_")

        if ttisreal[a.mode] then
            dxchar('f')
        else
            dxchar('i')
        fi
        dxstr((p.pclop=kmin|"min("|"max("))
        evalunit(a)
        dxchar(',')
        evalunit(b)
        dxchar(')')
    fi
end

proc do_maxto(unit p,a,b)=
!a:=(a>b?a:b)

!quick and dirty version, operands may be evaluated up to 3 times

    evalunit(a)
    ccstr("=(")
    evalunit(a)
    ccchar((p.pclop=kminto|'<'|'>'))
    evalunit(b)
    ccchar('?')
    evalunit(a)
    ccchar(':')
    evalunit(b)
    ccstrsemi(")")
end

function isexpr(unit p)int=
    int expr

    expr:=0
    if jisexpr[p.tag] then expr:=1 fi

    case p.tag
    when jif, jswitch, jcase, jselect then
        if p.mode=tvoid then
            expr:=0
        fi
    esac
    return expr
end

proc do_swap(unit a,b)=

    ccstr("{")

    ccstr(strmodec(a.mode,"temp"))

    ccstr(" = ")
    evalunit(a)
    ccstr("; ")

    evalunit(a)
    ccstr(" = ")
    evalunit(b)
    ccstr("; ")

    evalunit(b)
    ccstr(" = temp; }")
end

proc do_inrange(unit a,b,c)=
    dxchar('(')
    evalunit(a)
    dxstr(" >= ")
    evalunit(b)
    dxstr(" && ")
    evalunit(a)
    dxstr(" <= ")
    evalunit(c)
    dxchar(')')
end

proc do_inset(unit a, p)=
    dxchar('(')

    while p, p:=p.nextunit do
        evalunit(a)
        dxstr(" == ")
        evalunit(p)
        if p.nextunit then
            dxstr(" || ")
        fi
    od
    dxchar(')')
end

proc do_cmpchain(unit p)=
    unit q:=p.a, r
    int i:=1

    dxchar('(')
    repeat
        if i>1 then dxstr(" && ") fi
        evalunit(q)
        r:=q.nextunit
        dxstr(ccopnames[p.cmpgenop[i]])
        ++i
        evalunit(r:=q.nextunit)
        q:=r

    until q.nextunit=nil

    dxchar(')')
end

proc do_blockcopy(unit a,b)=
    dxstr("memcpy(&")
    evalunit(a)

    if b.tag=jptr then      !ptr op cancelled out by &
        dxstr(",")
        evalunit(b.a)
    else
        dxstr(",&")
        evalunit(b)
    fi
    dxchar(',')
    dxint(ttsize[a.mode])
    dxchar(')')
end

proc do_select(unit pindex,plist,pdefault)=
    int i
    unit q

    q:=plist
    i:=1

    while q do
        dxchar('(')
        evalunit(pindex)
        dxstr("==")
        dxint(i)
        ++i
        dxchar('?')
        evalunit(q)
        dxchar(':')
        q:=q.nextunit
    od
    evalunit(pdefault)
    to i-1 do
        dxchar(')')
    od
end

proc do_dotindex(unit a, i)=
    dxstr("call m$getdotindex(")
    evalunit(a)
    dxchar(',')
    evalunit(i)
    dxchar(')')
end

proc do_supportcall(ichar fnname, unit a,b=nil,c=nil,d=nil)=

    dxstr("msysc$m_")
    dxstr(fnname)
    dxchar('(')
    evalunit(a)
    if b then
        dxchar(',')
        evalunit(b)
        if c then
            dxchar(',')
            evalunit(c)
            if d then
                dxchar(',')
                evalunit(d)
            fi
        fi
    fi
    dxchar(')')
end

proc applymemmode(unit p)=
    if p.memmode then
        dxchar('(')
        dxstr(strmodec(p.mode))
        dxchar(')')
    fi
end

proc domaths2(ichar name, unit a,b)=
    dxstr(name)
    dxchar('(')
    evalunit(a)
    dxchar(',')
    evalunit(b)
    dxchar(')')
end

proc do_typepun(unit p, a)=
    int amode:=a.mode, pmode:=p.mode

    if a.tag=jconst then
        if ttisinteger[amode]+ttisreal[amode] and 
            ttisinteger[pmode]+ttisreal[pmode] then
            a.mode:=pmode
            do_const(a)
        elsif ttisinteger[amode]+ttisreal[amode] and ttisref[pmode] then
            dxstr("(")
            dxstr(strmodec(pmode))
            dxstr(")")
            do_const(a)
        else
            gerror("@const")
        fi
    elsif a.tag=jname then
        dxstr("*(")
        dxstr(strmodec(pmode))
        dxstr("*)&")
        evalunit(a)
    else
        dxchar('(')
        dxstr(strmodec(pmode))
        dxchar(')')
        if ttisreal[amode] and ttisinteger[pmode] then
            do_supportcall("tp_r64toi64",a)
        elsif ttisinteger[amode] and ttisreal[pmode] then
            do_supportcall("tp_i64tor64",a)
        elsif ttisinteger[amode] and ttisref[pmode] then
            do_supportcall("tp_i64toref",a)
        elsif ttisref[amode] and ttisinteger[pmode] then
            do_supportcall("tp_reftoi64",a)
        else
            cpl strmode(pmode),"@",strmode(amode)
            gerror("Typepun?")
        fi
    fi
end
=== mm_decls.m 0 0 5/33 ===
global const maxmodule=200
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=1000

global type symbol = ref strec

global macro pr(a,b)    = (a<<16 ior b)

global record tokenrec =        !should be 16-byte record
    byte symbol
    byte subcode
    word16 spare
    word32 pos: (sourceoffset:24, fileno:8)

    union
        ref strec symptr        !pointer to symbol table entry for name
        int64 value             !64-bit int
        real xvalue             !64-bit float
        word64 uvalue           !64-bit word
        ichar svalue            !pointer to string or charconst (not terminated)
    end
end

global record overloadrec =
    int32 amode
    int32 bmode
    int32 rmode
    int16 moduleno
    int16 flags
    unit fncode
    ref overloadrec nextoverload
end

global record procrec =
    symbol def
    ref procrec nextproc
end

global record typenamerec=
    symbol owner            !owner of scope where typename was encountered
                            !moduleno required by resolvetypename can be derived from owner
!A/B used as follows
!  nil B            Simple typename B
!  A   B            Dotted pair A.B
!  A   nil          I think represents a typeof(x) where x is a name
    symbol defa
    union
        symbol defb
        symbol def
    end
    ref int32 pmode
end

global record posrec=
    word32 pos: (sourceoffset:24, fileno:8)
end

global record uflagsrec =
    [7]byte codes
    byte    ulength
end

global record strec =
    ichar name
    ref strec owner
    ref strec deflist
    ref strec deflistx
    ref strec nextdef
    ref strec nextdupl
    ref strec firstdupl         !point to generic version

    unit code           !var idata/proc body/taggedunion tag value/etc

    int32 mode
    byte namelen
    byte symbol
    byte nameid
    byte subcode

    union
        int32 index                 !needs to hold pcindex (also indices for all symbols or .bc files)
        int32 labelno               !for mcl anonymous labels; and for proc labels?
    end
    int32 offset

    word32 pos: (sourceoffset:24, fileno:8)
    word16 flags: (
        isstatic:1,
        used:1,
        txdone:1,
        circflag:1,

        islet:1,
        iscallback:1,
        addrof:1,
        noreg:1,

        isequivtarget:1,
        atfield:1,
        atvar:1,

        isfloat:1,
        isimport:1,
        isqproc:1)

    byte moduleno
    byte subprogno

    unit equivvar

    struct              !when a proc
        ichar truename          !for imported name only
        ref strec paramlist

        byte asmused            !1 when proc contains asmcode
        byte dllindex           !for dllproc: which dll in dlltable
        byte fflang             !0 windowsff. etc.

        byte nretvalues         !function: number of return values (0 for proc)
        byte varparams          !0 or 1; variadic params in B and FF
        byte isthreaded         !0 or 1; variadic params in B and FF
        int16 dummy1
    end

    struct              !when a record or record field
        ref strec equivfield
        uflagsrec uflags
        int32 baseclass
        byte bitfieldwidth      !width of bitfield in record
        byte align              !0, 2, 4, 8, 16 or 255 (auto-align)
        byte bitoffset      !0..31 for bitfields in records
        byte equivoffset
    end

    struct              !when a param name
        ref strec nextparam
        byte parammode          !0=var_param, in_param, out_param
        byte optional           !0 or 1 
        byte variadic           !variadic parameter for B code
        byte dummy3             !variadic parameter for B code
    end

    int16 nrefs
    int16 regsize
    int16 maxalign      !for record types (doesn't fit above)

!----------------------------------

    ref fwdrec fwdrefs  !fwd ref chain
    byte reftype        !label pass 2: extern/back/fwd
    byte segment        !label pass 2: code_seg etc or 0

    int32 stindex       !label pass 2: 0, or 1-based index within coff symboltable
    int16 importindex   !genexe: index into import table

    ref strec nextsym
    int16 impindex
    int16 expindex
    byte reg

    byte scope
    byte equals         !for vars/params: 1/2/3 means =/:=/::= used

end

global type unit   = ref unitrec

global record unitrec =
    byte tag                !jcode tag number
    byte simple             !1 means can be evaluated with handful of regs and no calls
    byte ifretflag
    byte spare
    word32 pos: (sourceoffset:24, fileno:8)

    unit nextunit

    union
        struct
            union
                unit    a
                symbol  def
                symbol  labeldef
                int64   value
                word64  uvalue
                real64  xvalue
                ichar   svalue
                int64   range_lower
            end

            union
                unit    b
                int64   range_upper
            end

            unit        c
        end
        array[3]unit abc
    end

    union                       !misc stuff depends on tag
        struct                  !const string
            word32 slength
            byte isastring
        end

        struct                  !name
            byte dottedname     !for jname: 1=resolved from fully qualified dotted seq
            byte avcode         !jname for/autovars: 'I','T','S' = index/to/step autovars
        end

        union                   !asssemreg/xreg/mem
            struct
                byte reg
                byte regix
                byte scale
                byte prefixmode

                byte regsize
                byte cond
                byte spare2,spare3
            end
            word64 reginfo
        end

        union                   !for makelist
            word32 length       !number of elements
            byte makearray      !1 for makelist to create array-var not list-var
        end
        byte addroffirst    !1 for jnameaddr when derived from &.name

        word32 offset           !for jdot
        int32 whenlabel         !label no associated with when expr; for recase
        int32 swapvar           !for j-swap: 1 when swapping var:ref

        struct
            union
                int16 bitopindex    !
                int16 opcindex      !operator nodes
                int16 fnindex       !sf_add_var etc
                int16 condcode      !pcl_eq etc; for jeq etc
                int16 asmopcode     !for jassem
                int16 bfcode
            end
        end
        int32 index
        [4]byte cmpgenop            !cmpchain: up to 8 genops
    end

    int32 mode
    int32 convmode      !convert/typepun: source/target(?) mode (will be widened to give unit mode)
    byte moduleno
    byte subprogno
    byte initlet        !1 for an assignment that initialises a let
    byte isconst        !1 for jconst, and jmakerange with const range
    byte resultflag     !1 when the result of this unit is needed; 0=void or discarded
    byte pclop          !generic operator for jbin, incr etc
    byte istrueconst    !1 for actual "123" etc, not result of reduction
    byte memmode
end

global record fwdrec =
    ref fwdrec nextfwd
    int32 offset
    int16 reltype
    int16 seg
end

global record modulerec =
    ichar name
    symbol stmodule
    symbol stsubprog
    ichar path          !path where module source file resides
    symbol ststart      !nil, or st entry of local start/main
    symbol stmain
    symbol stmacro      !will be turned into a macro
    unit modulecode     !any code outside of a module
    int16 fileno        !sourcefile table index once loaded
    int16 issyslib      !1 if system lib (different search rules)
    int16 subprogno

end

global record subprogrec =
    ichar name
    symbol stsubprog
    int issyslib        !1 if system lib (different search rules)
    ichar path          !path where import module source file resides
    int16 firstmodule
    int fileno
end

global symbol stprogram     !root into the symbol table
global symbol stmodule      !main module
global symbol stsubprog
global symbol stsysmodule   !optional sys module (needed for name resolving)
global symbol alldeflist        !link together all (user) symbols
global int currmoduleno             !used when compiling modules

global tokenrec lx              !provides access to current token data
global tokenrec nextlx          !provides access to next token

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]byte moduletosub               !convert module no to subprog no
global [0..maxsubprog]subprogrec subprogtable

global [0..maxlibfile]ichar libfiles
global [0..maxlibfile]byte libtypes

global [0..maxsourcefile]ichar sourcefilespecs      !full path
global [0..maxsourcefile]ichar sourcefilepaths      !path only
global [0..maxsourcefile]ichar sourcefilenames      !base filename only
global [0..maxsourcefile]byte sourcefilesys         !1 if a system module
global [0..maxsourcefile]byte sourcefilesupport     !1 is a support file:strinclude etc
global [0..maxsourcefile]ichar sourcefiletext       !text
global [0..maxsourcefile]ichar sourcefiledupl       !copy for ma file only
global [0..maxsourcefile]int sourcefilesizes
global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles
global int mainmoduleno

global const int maxtype=6'000

global int ntypes

global [0..maxtype]symbol       ttnamedef
global [0..maxtype]symbol       ttowner         !for ttlowerexpr/rtlengthexpr

global [0..maxtype]int32        ttbasetype      !basetype
export [0..maxtype]ichar        ttname

global [0..maxtype]word32       ttsize
global [0..maxtype]byte         ttsizeset
global [0..maxtype]int32        ttlower         !.lbound (default 1)
global [0..maxtype]int32        ttlength        !elements in array/record/tuple
global [0..maxtype]ref[]int32   ttmult          !ttlength elements in tuple

global [0..maxtype]unit         ttdimexpr       !length, lower:length, or lower..upper

global [0..maxtype]int32        tttarget        !for array/ref types
global [0..maxtype]byte         ttusercat
global [0..maxtype]int32        ttlineno

global [0..maxtype]byte         ttsigned        !is i8 i16 i32 i64
global [0..maxtype]byte         ttisreal        !is r32 r64
global [0..maxtype]byte         ttisinteger     !is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte         ttisshort       !is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte         ttisref         !is a pointer

global [0..maxtype]byte         ttcat           !is a variant
global [0..maxtype]byte         ttisblock       !is a variant

global const int maxtypename=8'000
global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc
global symbol currsubprog

global int debug=0
global int assemmode=0
global int headermode=0

global ref procrec proclist,proclistx           !linked list of all procs
global ref procrec staticlist,staticlistx       !linked list of all static
global ref procrec constlist,constlistx     !linked list of all export consts

global unit nullunit

global int targetbits=64
global int targetsize=8

global [20]ichar docstrings
global int ndocstrings

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1       !1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2     !0/1/2 = none/min/normal
global byte mvarlib=0       !0/1 = none/yes
global byte fvarnames=0     !display of names in asm/mcl
global byte minos=0         !1 enables simpler windows module

global byte freadma         !1 when .ma file is being compiler
global byte fwritema
global byte fwriteexports
global byte fwritedocs

global byte fexe
global byte fobj
global byte fwritelibs
global byte fshowtiming
global byte fshowss
global byte fshowmx
global byte fshowpcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte fshowoverloads
global byte fshowmodules
global byte foptim
global byte fcheckunusedlocals=0

global byte fwindows=1
global byte flinux
global byte fnofile

global byte dointlibs=1

!pcl/mcl-pass are relevant only for x64 target, and not allowed for 
global enumdata []ichar passnames =
    (header_pass,   $),
    (load_pass,     $),
    (parse_pass,    $),
    (fixup_pass,    $),
    (name_pass,     $),
    (type_pass,     $),
    (pcl_pass,      $),
    (mcl_pass,      $),     !all-inclusive up to this point (includes all prev passes)
    (asm_pass,      $),     !only one of these 3 will be done
    (objpass,       $),     !
    (exe_pass,      $),     !
    (lib_pass,      $),     !
    (run_pass,      $),     !will do up to .exe then run the .exe
    (clang_pass,    $),
end

global enumdata []ichar ccnames=
    (gcc_cc,        $),
    (tcc_cc,        $),
    (tc_cc,     $),
    (bcc_cc,        $),
end

!passlevel used for compiler debug only
global int passlevel=0
global int prodmode=0
global int debugmode=0
!global int mainlib=0                   !1 means main app lib (so export $cmdskip)
global int libmode=0                    !1 means eventual ML/LIB target
global int mxstub=0                     !1 to write prog.exe with run.exe+prog.mx
global int ccompiler=gcc_cc

global ichar outfile                    !one of the following two
global ichar destfilename               !nil, or sets outfile
global ichar destfilepath               !nil, or sets path (can't be mixed with destfilename)
global ichar asmfilename                !set from outfile
global ichar pclfilename                !
global ichar exefilename                !
global ichar libfilename                !
global ichar objfilename                !
global ichar mafilename                 !
global ichar expfilename                !

global symbol extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable

global int nunits

GLOBAL INT NSTRECS
!GLOBAL INT NUNITS

global const langnameuc     = "M"
global const langname       = "m"
global const langext        = "m"
global const langextma      = "ma"
global const langextmauc    = "MA"
global const langlibname    = "mlib"
global const langhomedir    = "C:/mx/"
global const langhelpfile   = "mm_help.txt"

GLOBAL INT NREADASSIGN
GLOBAL INT NSIMPLE
GLOBAL INT NLBRACK
GLOBAL INT NINCR
=== mm_diags.m 0 0 6/33 ===
int currlineno
int currfileno

!const fshowsymbols=1
const fshowsymbols=0

global proc printoverloads(filehandle f)=
    ref overloadrec p


    println @f,"OVERLOADS"
    for i to overloadtable.upb do
        p:=overloadtable[i]
        if p then
!           println @f,"Overloads for",jtagnames[i],P
            while p do
                if p.bmode then
                    fprint @f,"operator (#)(#,#)#",
                        jtagnames[i]+2,strmode(p.amode), strmode(p.bmode), strmode(p.rmode)
                else
                    fprint @f,"operator (#)(#)#",
                        jtagnames[i]+2,strmode(p.amode), strmode(p.rmode)
                fi
                if p.fncode then
                    print @f,"=",p.fncode,strexpr(p.fncode).strptr
                fi
                println @f
                p:=p.nextoverload
            od
            println @f
        fi
    od

end

global proc printst(filehandle f,ref strec p,int level=0)=
ref strec q

if p.symbol<>namesym then
    mcerror("PRINTST not name")
fi

printstrec(f,p,level)

q:=p.deflist

while q<>nil do
    printst(f,q,level+1)
    q:=q.nextdef
od
end

proc printstrec(filehandle f,ref strec p,int level)=
strec dd
ref byte q
strbuffer v
ref strbuffer d:=&v
int col,offset,n
const tabstr="    "
[256]char str

gs_init(d)

print @str, p
gs_str(d,str)
gs_str(d," ")

offset:=0
to level do
    gs_str(d,tabstr)
    offset+:=4
od
gs_str(d,":")

gs_leftstr(d,p.name,28-offset,'-')
gs_leftstr(d,namenames[p.nameid],12,'.')

col:=gs_getcol(d)
dd:=p^


gs_str(d,"[")
if p.isimport then
    gs_str(d,"Imp ")
else
    gs_str(d,(p.scope|"Sub ","Prog ", "Exp "|"Mod "))
fi

if dd.isstatic then
    gs_str(d,"Stat")
fi
if dd.fflang then
    gs_strsp(d,fflangnames[dd.fflang])
fi

if dd.nameid=paramid and dd.parammode then
    gs_str(d,parammodenames[dd.parammode])
fi

if dd.align then
    gs_str(d,"@@")
    gs_strint(d,dd.align)
    gs_str(d," maxalign:")
    gs_strint(d,dd.maxalign)
    gs_str(d," ")
fi
if dd.optional then
    gs_str(d,"Opt ")
fi
if dd.varparams then
    gs_str(d,"Var:")
    gs_strint(d,dd.varparams)
    gs_str(d," ")
fi

if dd.moduleno then
    if dd.nameid<>subprogid then
        print @&.str,"Modno#",,dd.moduleno
    else
        print @&.str,"Subno#",,dd.subprogno
    fi
    gs_str(d,&.str)
fi

if dd.used then
    gs_str(d,"U ")
fi

if dd.isthreaded then
    gs_str(d,"Threaded ")
fi


gs_str(d,"]")
gs_padto(d,col+10,'=')

if p.owner then
    fprint @&.str,"(#)",p.owner.name
    gs_leftstr(d,&.str,18,'-')
else
    gs_leftstr(d,"()",18,'-')
fi

case p.mode
when tvoid then
    gs_str(d,"Void ")
else
    GS_STRINT(D,P.MODE)
    GS_STR(D,":")

    gs_str(d,strmode(p.mode))
    gs_str(d," ")
esac

case p.nameid
when fieldid,paramid then
    gs_str(d," Offset:")
    gs_strint(d,p.offset)
    if p.mode=tbitfield then
        gs_str(d," Bitoffset:")
        gs_strint(d,p.bitoffset)
        gs_str(d,":")
        gs_strint(d,p.bitfieldwidth)
    fi

    sprintf(&.str,"%.*s",int(p.uflags.ulength),&p.uflags.codes)
    print @&.str,p.uflags.ulength:"v",ichar(&p.uflags.codes):".*"
    gs_str(d," UFLAGS:")
    gs_str(d,&.str)
    gs_str(d,"-")
    gs_strint(d,p.uflags.ulength)

    if p.code then
        gs_str(d,"/:=")
        gs_strvar(d,strexpr(p.code))
    fi

    if p.nameid=paramid and p.variadic then
        gs_str(d,"...")
    fi
when genfieldid then
    gs_str(d,"Index:")
    gs_strint(d,p.offset)

when procid,genprocid then

    gs_str(d,"Index:")
    gs_strint(d,p.index)

    gs_str(d," Nret:")
    gs_strint(d,p.nretvalues)

when dllprocid then
    gs_str(d,"Index/PCaddr:")
    gs_strint(d,p.index)
    if p.truename then
        gs_str(d," Truename:")
        gs_str(d,p.truename)
    fi

when staticid then
    if p.code then
        gs_str(d,"=")
        gs_strvar(d,strexpr(p.code))
    fi

when frameid then
    if p.code then
        gs_str(d,":=")
        gs_strvar(d,strexpr(p.code))
    fi

when constid then
    gs_str(d,"Const:")
    gs_strvar(d,strexpr(p.code))

when typeid then
    if p.baseclass then
        gs_str(d,"Baseclass:")
        GS_STR(D,"<HAS BASECLASS>")
    fi
when enumid then
    gs_str(d,"Enum:")
    gs_strint(d,p.index)
when dllmoduleid then
    gs_str(d,"DLL#:")
    gs_strint(d,p.dllindex)
esac

if p.atfield then
    gs_str(d," @")
    gs_str(d,p.equivfield.name)
    gs_str(d," +")
    gs_strint(d,p.equivoffset)
fi
if p.atvar then
    gs_strvar(d,strexpr(p.equivvar))
fi

gs_str(d," Lineno: ???")

gs_println(d,f)

case p.nameid
when constid,frameid,staticid,macroid then
    if p.code then
        printunit(p.code,dev:f)
    fi
esac
end

global proc printstflat(filehandle f)=
int i
ref strec p
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
    p:=cast(&hashtable[i])
    if p.name then
        case p.symbol
        when namesym then
            println @f,i,p,":",p.name,symbolnames[p.symbol],namenames[p.nameid]
            p:=p.nextdupl
            while p do
                println @f,"    ",p,p.name,symbolnames[p.symbol],namenames[p.nameid],
                    "(From",(p.owner|p.owner.name|"-"),,")"
                p:=p.nextdupl
            od
        esac
    fi
od
end

global proc printcode(filehandle f,ichar caption)=
ref strec p
ref procrec pp

pp:=proclist

while pp do
    p:=pp.def

    print @f,p.name,,"=",(p.scope|"Sub","Prog","Exp"|"Mod")
    if p.owner.nameid=typeid then
        print @f," in record",p.owner.name
    fi
    println @f
    printunit(p.code,,"1",dev:f)
    println @f
    pp:=pp.nextproc
od
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=
!p is a tagrec
ref unitrec q
ref strec d
int t
ichar idname
int64 a
real32 x32
static int cmpchain=0

if p=nil then
    return
fi

if p.pos then
    currlineno:=getlineno(p.pos)
    currfileno:=p.fileno
fi

print @dev,p,":"
print @dev,getprefix(level,prefix,p)

idname:=jtagnames[p.tag]+1
print @dev,idname,,":"


case p.tag
when jname then
    d:=p.def

    print @dev,d.name,namenames[d.nameid]

    if d.code then
        print @dev," {",,jtagnames[d.code.tag],,"}"
    fi

    print @dev," ",,getdottedname(d)!,q
    print @dev,(p.dottedname|" {Dotted}"|"")

    if p.c then
        print @dev," Lastcall:",p.c
    fi

    if p.addroffirst then
        print @dev," Addroffirst."
    fi

    print @dev," Moduleno:",p.moduleno

    if p.avcode then print @dev," AV:",char(p.avcode) fi

PRINT @DEV,=P.INDEX


when jlabeldef then
    println @dev,p.def.name

when jconst, jemitc then
    t:=p.mode
    a:=p.value
    if t=trefchar then
        if p.slength>256 then
            print @dev,"""",,"(LONGSTR)",""" *",,p.slength
        elsif p.slength then
            print @dev,"""",,p.svalue,,""" *",,p.slength
        else
            print @dev,""""""
        fi

    elsecase ttbasetype[t]
    when ti64,ti32,ti16,ti8 then print @dev,int64(a)
    when tu64,tu32,tu16,tu8 then print @dev,word64(a)
    when tc64,tc8 then print @dev,chr(a)

!   when ti32,ti64,ti8,ti16 then
!       print @dev,p.value
!   when tu32,tu64,tu8,tu16 then
!       print @dev,p.uvalue
    when tr32 then
        x32:=p.xvalue
        print @dev,real64(x32)
    when tr64 then
        print @dev,p.xvalue
    when tref then
        if p.value then
            print @dev,"#",,p.value,P.SLENGTH
        else
            print @dev,"NIL"
        fi
    when tbool then
        print @dev,(p.value|"True"|"False")
    else
        println =typename(t),typename(ttbasetype[t])
        PRINT @DEV,"<PRINTUNIT BAD CONST PROBABLY VOID"
    fi
    print @dev," ",,typename(t)
    if p.isastring then
        print @dev," <isstr>"
    fi

    if p.whenlabel then
        print @dev," *L",,p.whenlabel
    fi

!when jdecimal then
!   print @dev,p.svalue,"Len:",p.slength
!
when jtypeconst then
    print @dev,typename(p.mode),typename(p.value)

!when joperator then
!   print @dev,jtagnames[p.opcode]+2

when jbitfield then
    print @dev,bitfieldnames[p.bfcode]+3

when jconvert,jtypepun then
!   print @dev,pclnames[p.pclop]," Convmode:",strmode(p.convmode)
    print @dev," Convmode:",strmode(p.convmode)

when jmakelist then
    print @dev,"Len:",p.length," Makeax:",p.makearray

when jdot then
    print @dev,"Offset:",p.offset

when jindex, jptr then

when jexit,jredo,jnext then
    print @dev,"#",,p.index

when jsyscall then
    print @dev,sysfnnames[p.fnindex]+3

when jassem then
!   print @dev,mclnames[p.index]+2
!   if p.index in [m_jmpcc, m_setcc, m_cmovcc] then
!       print @dev," ",condnames[p.cond],=P.COND
!   fi

when jassemreg then
!   print @dev,regnames[p.reg],"size:",p.regsize

when jassemxreg then
!   print @dev,xmmregnames[p.reg]

when jassemmem then
!   ichar plus
!   plus:=""
!   if p.prefixmode then print @dev,strmode(p.prefixmode) fi
!   print @dev,"["
!   if p.reg then 
!       print @dev,regnames[p.reg]
!       plus:="+"
!   fi
!   if p.regix then 
!       print @dev,plus,,regnames[p.regix]
!!      plus:="+"
!   fi
!   if p.scale>1 then
!       print @dev,"*",,p.scale
!   fi
!   print @dev,"]"
!
!when junary, jbin, junaryto, jbinto, jcmp then
!   if p.opindex then
!       print @dev,pclnames[p.opindex]
!   else
!       print @dev,pclnames[p.pclop]
!   fi

!when jmakeset then
!   if p.isconst then
!       print @dev,p.range_lower,,"..",,p.range_upper
!   fi
when jcmpchain then
    for i to p.cmpgenop.len do
        if p.cmpgenop[i]=0 then exit fi
        print @dev,pclnames[p.cmpgenop[i]],," "
    od
esac

case p.tag
when jname, jptr, jindex, jdot,jcallproc, jcallfn, jassign then
    if p.memmode=tvoid then
!       print @dev," LVALUE"
    else
        print @dev," WIDEN FROM:",strmode(p.memmode)
    fi
esac

if p.isconst then
    print @dev," Is const"
else
    print @dev," Not const"
fi

case p.tag
when jbin, jbinto, junary, junaryto, jcmp, jincr, jconvert,
    jandl, jorl, jnotl, jistruel then
    if p.pclop then
        fprint @dev," Pcl<#>",pclnames[p.pclop]
    else
        fprint @dev," no-op"
    fi
esac


println @dev

[16]char opndno
for i:=1 to jsubs[p.tag] do
    strcpy(opndno, strint(i))
    printunitlist(dev,p.abc[i],level+1,opndno)
!   printunitlist(dev,p.abc[i],level+1,strint(76))
od
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=
    if p=nil then return fi

    while p do
        printunit(p,level,prefix,dev)
        p:=p.nextunit
    od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=      !GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
static [1024]char str
[1024]char indentstr
[16384]char modestr
ichar isexpr

indentstr[1]:=0
if level>10 then level:=10 fi

to level do
    strcat(&.indentstr,"- ")
od

isexpr:="-"
if jisexpr[p.tag] then isexpr:="x" fi

case p.tag
when jif, jswitch, jcase, jselect then
    if p.mode=tvoid then
        isexpr:="x"
    fi
esac

fprint @&.modestr,"#<#> #:#",isexpr,
    (p.simple|"S"|"-"),
    (p.resultflag|"RES"|"---"),strmode(p.mode)

modestr[256]:=0

strcat(&.modestr,"-----------------------------")
modestr[17]:=' '
modestr[18]:=0


strcpy(&.str,getlineinfok())
strcat(&.str,&.modestr)
strcat(&.str,&.indentstr)
strcat(&.str,prefix)
if prefix^ then
    strcat(&.str," ")
fi

return &.str
end

function getlineinfok:ichar=
static [40]char str

fprint @&.str,"# # ",CURRFILENO:"Z2",currlineno:"z4"
return &.str
end

global proc printmodelist(filehandle f)=
int mbase
static ichar tab="\t"

PRINTLN @F,=NTYPENAMES
FOR I TO NTYPENAMES DO
    PRINTLN @F,I,TYPENAMES[I].DEF.NAME
OD
PRINTLN @F

println @f,"MODELIST",ntypes

for m:=0 to ntypes do
!for m:=tlast to ntypes do
    println @f,m:"4",strmode(m)
    mbase:=ttbasetype[m]

    println @f,tab,"Basetype:",mbase,strmode(mbase)
    println @f,tab,"ttname:",ttname[m]
    println @f,tab,"ttnamedef:",ttnamedef[m],(ttnamedef[m]|ttnamedef[m].name|"-")
    println @f,tab,"Target:",strmode(tttarget[m])
    println @f,tab,"Size:",ttsize[m],"Sizeset",ttsizeset[m]
    fprintln @f,"# Bounds: #..#  Length:#",tab,ttlower[m],ttlower[m]+ttlength[m]-1,ttlength[m]
    if mbase=ttuple then
        print @f,tab,"Mult:"
        for i to ttlength[m] do print @f,strmode(ttmult[m,i]),," " od
        println @f
    fi
    println @f,tab,"Signed:",ttsigned[m]
    println @f,tab,"Isreal:",ttisreal[m]
    println @f,tab,"Isinteger:",ttisinteger[m]
    println @f,tab,"Isshort:",ttisshort[m]
    println @f,tab,"Isref:",ttisref[m]
    println @f,tab,"Isblock:",ttisblock[m]
    println @f,tab,"Cat:",catnames[ttcat[m]]
    println @f
od

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

        print @dev, tab,i:"2",,
            if subprogtable[moduletosub[i]].firstmodule=i then "*" else " " fi,
            pm.name:"16jl", "Sys:",pm.issyslib, "Path:",pm.path,
            "Sub:",subprogtable[pm.subprogno].name,"Fileno:",pm.fileno
        if pm.stmacro then
            print @dev," Alias:",pm.stmacro.name
        fi
        if pm.stmain then
            print @dev,$,pm.stmain.name,":",scopenames[pm.stmain.scope],pm.stmain
        fi
        if pm.ststart then
            print @dev,$,pm.ststart.name,":",scopenames[pm.ststart.scope],pm.ststart
        fi

        println @dev
    od
    println @dev

    println @dev,"Subprograms",nsubprogs
    for i to nsubprogs do
        ps:=&subprogtable[i]
        println @dev, tab,i,ps.name,"Sys:",ps.issyslib, "Path:",ps.path, "Fileno:",ps.fileno

        if ps.firstmodule then
            print @dev, tab,tab
            for j to nmodules when moduletable[j].subprogno=i do
                print @dev, $,moduletable[j].name
                if ps.firstmodule=j then print @dev,"*" fi
            od
            println @dev
        fi
    od
    println @dev

!   println @dev,"Sourcefiles",nsourcefiles
!   for i to nsourcefiles do
!       println @dev, tab,i,sourcefilenames[i]
!       println @dev, tab,tab,sourcefilepaths[i]
!       println @dev, tab,tab,sourcefilespecs[i]
!       println @dev, tab,tab,=strlen(sourcefiletext[i])
!   od
!   println @dev

    println @dev,"Link files",nlibfiles
    for i to nlibfiles do
        println @dev, tab, libfiles[i]:"16jl",(libtypes[i]='D'|"DLL"|"LIB")
    od
    println @dev

!   println @dev,"Header Variables:"
!   for i to headervars.len do
!       fprintln @dev,"\t#: #",headervarnames[i],headervars[i]
!   od
!   println @dev
!   println @dev,"---------------------------------------"

!   println @dev,"Symboltable:"
!   symbol d:=stprogram.deflist
!   while d, d:=d.nextdef do
!       ichar id
!       case d.nameid
!       when moduleid then id:="Mod"
!       when subprogid then id:="Sub"
!       else id:="---"
!       esac
!       fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
!   od
end

global proc showlogfile=
    [256]char str
    filehandle logdev
    int size

    if not debugmode then return fi

    logdev:=fopen(logfile,"w")

    if fshowmodules then showprojectinfo(logdev) fi

    if fshowasm and passlevel>=mcl_pass then
        println @logdev,"PROC",(ctarget|"C-CODE"|"ASSEMBLY")
        addtolog(asmfilename,logdev)
    fi
    if fshowpcl and passlevel>=pcl_pass then
        println @logdev,"PROC PCL"
        addtolog(pclfilename,logdev)
    fi
    if fshowast3 and passlevel>=type_pass then  addtolog("AST3",logdev) fi
    if fshowast2 and passlevel>=name_pass then  addtolog("AST2",logdev) fi
    if fshowast1 and passlevel>=parse_pass then addtolog("AST1",logdev) fi
    if fshowst then                             showsttree("SYMBOL TABLE",logdev) fi
    if fshowstflat then                         showstflat("FLAT SYMBOL TABLE",logdev) fi

    if fshowtypes then                          printmodelist(logdev) fi

    if fshowoverloads then                      printoverloads(logdev) fi

    size:=getfilesize(logdev)
    fclose(logdev)

    if size then
        print @&.str,"c:\\m\\ed.bat -w ",logfile

        if checkfile("mm.m") then
            os_execwait(&.str,1,nil)
        else
            println "Diagnostic outputs written to",logfile
        fi
    fi

!   stop 0
end

proc showstflat(ichar caption,filehandle f)=
    println @f,"PROC",caption
    printstflat(f)
    println @f
end

proc showsttree(ichar caption,filehandle f)=
    println @f,"PROC",caption
    printst(f,stprogram)
    println @f

    println @f, "Proc List:"
    ref procrec pp:=proclist
    while pp do
        symbol d:=pp.def
        fprintln @f,"#  #.# (#) Mod:",d,d.owner.name, d.name:"20jl", namenames[d.nameid],
            d.moduleno
        pp:=pp.nextproc
    od
    println @f,"End\n"

    println @f, "DLL Proc List:"
    for i to ndllproctable do
        d:=dllproctable[i]
        fprintln @f,"#  #.# (#) Mod: # # #",d,d.owner.name, d.name:"20jl", namenames[d.nameid],
            d.moduleno, libfiles[d.dllindex], libtypes[d.dllindex]:"c"
    od
    println @f,"End\n"
end

global proc showast(ichar filename)=
    filehandle f

    f:=fopen(filename,"w")
    return unless f

    println @f,"PROC",filename
    printcode(f,"")
    println @f
    fclose(f)
end

global proc printsymbol(ref tokenrec lp)=
    tokenrec l
    l:=lp^

    printf("%-18s",symbolnames[l.symbol])

    switch l.symbol
    when namesym then
        printstrn(l.symptr.name,l.symptr.namelen)

        if l.subcode then
            fprint " [#]",symbolnames[l.subcode]
        fi

    when intconstsym then
        case l.subcode
        when tint then print l.value,"int"
        when tword then print l.uvalue,"word"
        else print l.value
        esac

    when realconstsym then
        print l.xvalue

    when stringconstsym then
        print """"
        printstr(l.svalue)
        print """",strlen(l.svalue)
    when charconstsym then
        print "'"
        printstr(l.svalue)
        print "'"
    when decimalconstsym then
        printstr(l.svalue)
        print "L"
    when assignsym,addrsym,ptrsym,deepcopysym,rangesym,
        andlsym,orlsym,eqsym,cmpsym,addsym,subsym,
        mulsym,divsym,idivsym,iremsym,iandsym,iorsym,ixorsym,shlsym,shrsym,
        minsym,maxsym,powersym,samesym then
        print symbolnames[l.symbol]
    elsif l.subcode then
        fprint "SUBCODE:",l.subcode
    end

    println

end

=== mm_export.m 0 0 7/33 ===
strbuffer sbuffer
ref strbuffer dest=&sbuffer

const expscope=export_scope

global proc writeexports(ichar outfile, modulename)=
    ref strec d,e
    ref procrec pp
    array [300]char filename
    filehandle f

    println "Writing exports file to",outfile

    gs_init(dest)
!   wxstr("importlib $")
    wxstr("importlib ")
    wxstr(modulename)
    wxstrln(" =")

    for i:=tuser to ntypes do
        d:=ttnamedef[i]
        if d.scope=export_scope and d.name^<>'$' then

            case ttbasetype[i]
            when trecord then
                exportrecord(d)
            else
                wxstr("    type ")
                wxstr(d.name)
                wxstr(" = ")
                wxstr(strmode(d.mode,0))
                wxline()
            esac
        fi
    od

    pp:=staticlist
    while pp, pp:=pp.nextproc do
        d:=pp.def
        if d.scope=export_scope then
            exportstatic(d)
        fi
    od
    if staticlist then wxline() fi

    pp:=constlist
    while pp, pp:=pp.nextproc do
        d:=pp.def
        if d.scope=export_scope then
            exportconst(d)
        fi
    od
    if constlist then wxline() fi

    pp:=proclist
    while pp, pp:=pp.nextproc do
        d:=pp.def
        if d.scope=export_scope then
            exportproc(d)
        fi
    od

    wxstrln("end importlib")

    f:=fopen(outfile,"wb")
    gs_println(dest,f)
    fclose(f)
end

proc exportstatic(ref strec d)=
    wxstr("    var ")
    wxmode(d.mode)
    wxstr(" ")
    wxstr(d.name)
    wxline()
end

proc exportconst(ref strec d)=
    wxstr("    const ")
    wxmode(d.mode)
    wxstr(" ")
    wxstr(d.name)
    wxstr(" = ")
    jevalx(dest,d.code)
    wxline()
end

proc exportproc(ref strec d)=
    ref strec e
    int currmode,needcomma

!   wxstr("    mlang ")
    wxstr("    ")
    wxstr((d.mode=tvoid|"proc "|"func "))
    wxstr(d.name)
    wxstr("(")

    e:=d.deflist
    needcomma:=0
    currmode:=tvoid

    while e do
        if e.nameid=paramid then
            if needcomma then wxstr(",") fi
            if e.parammode<>out_param then
                if e.mode<>currmode then
                    wxmode(e.mode)
                    wxstr(" ")
                    currmode:=e.mode
                fi
            else
                wxmode(tttarget[e.mode])
                wxstr(" &")
                currmode:=tvoid
            fi
            wxstr(e.name)
            if e.code then
                wxstr("=")
                if ttisref[e.mode] and e.code.tag=jconst and e.code.value=0 then
                    wxstr("nil")
                else
                    jevalx(dest,e.code)
                fi
            fi
            needcomma:=1
        fi
        e:=e.nextdef
    od

    wxstr(")")
    if d.mode then
        wxstr(" => ")
        wxmode(d.mode)
    fi
    wxline()
end

proc wxstr(ichar s)=
    gs_str(dest,s)
end

proc wxstrln(ichar s)=
    gs_strln(dest,s)
end

proc wxline=
    gs_line(dest)
end

proc exportrecord(ref strec d)=
    ref strec e
    ref char flags
    int flag,indent
    const tab="    "

    e:=d.deflist

    wxstr("    record ")
    wxstr(d.name)
    wxstr(" = ")
    wxline()

    indent:=2

    while e do
        if e.nameid=fieldid then
            flags:=cast(&e.uflags)
            docase flags^
            when 'S' then
                to indent do wxstr(tab) od
                wxstrln("struct")
                ++indent
                ++flags
            when 'U' then
                to indent do wxstr(tab) od
                wxstrln("union")
                ++indent
                ++flags
            else
                exit
            end docase

            to indent do wxstr(tab) od
            wxmode(e.mode)
            wxstr(" ")
            wxstrln(e.name)

            do
                flag:=flags++^
                case flag
                when '*'  then
                when 'E' then
                    --indent
                    to indent do wxstr(tab) od
                    wxstrln("end")
                else
                    exit
                esac
            od
        fi

        e:=e.nextdef
    od

    wxstrln("    end")
    wxline()
end

proc wxmode(int mode)=
    ichar name
    if mode>=tuser then
        name:=ttnamedef[mode].name
        if name^<>'$' then
            wxstr(name)
            return
        fi
    fi
    wxstr(strmode(mode,0))
end
=== mc_genc.m 0 0 8/33 ===
record stlinkrec =
    symbol def
    ref stlinkrec nextsymbol
end

ref stlinkrec allsymbols, allsymbolsx

int nallprocs,nexports

ichar cfilename

global proc codegen_clang=

    if fverbose>1 then
        println "Generating C code:"
    fi

    cfilename:=asmfilename
    allsymbols:=allsymbolsx:=nil
    scansymbol(stprogram)

    gs_init(dest)
    ccinitline()

!   do_infoheader(cfilename);
    unless msyslevel=0 then
        do_cheader(cfilename);
    end

    do_alltypes();
    do_allprocdecls();

    do_allvars();
    do_allprocdefs();

    cclinecomment("********** End of C Code **********")

end

proc do_infoheader(ichar cfilename)=
int i

ccstrline("/*")
ccstrline("  M to C  Whole Program Translator")
ccstr("  Input:  "); ccstr(inputfile); ccstrline(" plus imported modules")
ccstr("  Output: "); ccstr(cfilename); ccstrline(" (this file, or renamed from that)")
ccstrline("          File represents entire program")

ccstrline("  Target: C 64-bit")

if fwindows then
    ccstrline("  OS:     Windows")
else
    ccstrline("  OS:     Other")
fi

ccstrline("")
ccstrline("  Modules:")
for i:=1 to nmodules do
    ccstr("  Module ")
    ccint(i)
    ccstr(": ")
    ccstrline(sourcefilespecs[moduletable[i].fileno])
od
ccstrline("")

ccstrline("*********** Start of C Code **********/")
ccblank()

end

proc do_cheader(ichar cfilename)=
ccstrline(strinclude "cheader.m")

!ccstrline("#if (UINTPTR_MAX<0xFFFFFFFFFFFFFFFF)")
!ccstrline("    #error ""Need 64-bit target. Try -m64""")
!ccstrline("#endif")

ccsendline()
end

proc do_alltypes=
    ref stlinkrec p
    symbol d
    int i

    cclinecomment("Forward Struct Declarations")

    for i:=tlast to ntypes do
        if ttbasetype[i]=trecord and tttarget[i]=tvoid then
            do_typedef_fwd(ttnamedef[i])
        fi
    od
    ccblank()

    if ntypes>=tlast then
        cclinecomment("Struct Definitions")
    fi

    for i:=tlast to ntypes do
        if ttbasetype[i]=trecord and tttarget[i]=tvoid then
            do_typedef(ttnamedef[i])
        fi
    od

    ccblank()
end

proc scansymbol(symbol d)=
!collect top-level names into 'allsymbols'
!These are names at module level across the program, but also
!including functions in an importdll block.

    symbol e
    addsymbol(d)
    case d.nameid
    when procid, typeid, dllprocid then
        return
    esac

    e:=d.deflist

    while e do
        scansymbol(e)
        e:=e.nextdef
    od

end

proc addsymbol(symbol d)=
    ref stlinkrec p

    p:=pcm_alloc(stlinkrec.bytes)
    p.def:=d
    p.nextsymbol:=nil
    if allsymbols=nil then
        allsymbols:=p
    else
        allsymbolsx.nextsymbol:=p
    fi
    allsymbolsx:=p
end

proc do_typedef_fwd(symbol d)=
    if ttbasetype[d.mode]<>trecord then
        return
    fi
    genrecordfwd(d.mode)
end

proc do_typedef(symbol d)=
if ttbasetype[d.mode]<>trecord then
    return
fi
genrecorddef(d.mode)
end

proc do_allprocdecls=
    ref stlinkrec p
    symbol d
    cclinecomment("PROCDECLS")
    p:=allsymbols
    while p do
        d:=p.def
        case d.nameid
        when procid then
            do_procdecl(d)
        when dllprocid then
            do_procdecl(d)
        esac
        p:=p.nextsymbol
    od
    ccblank()
end

proc do_allvars=
    ref stlinkrec p
    symbol d
    cclinecomment("VARS")
    p:=allsymbols
    while p do
        d:=p.def
        case d.nameid
        when staticid then
            do_vardef(d)
        when dllvarid then
            do_dllvar(d)
        esac
        p:=p.nextsymbol
    od
    ccblank()
end

proc do_allprocdefs=
    ref stlinkrec p
    symbol d
    cclinecomment("PROCDEFS")
    p:=allsymbols
    while p do
        d:=p.def
        if d.nameid=procid and d.code then
            genprocdef(d)
        fi
        p:=p.nextsymbol
    od
    ccblank()
end

proc do_procdecl(symbol d)=
    if moduletable[d.moduleno].stmain=d then
        ccstrline("int main(int, char**, char**);")
        return
    fi

    if d.code=nil then
        ccstr("extern ")
    elsif d.scope=module_scope then
        ccstr("static ")
    fi
    ccstr(strprocsig(d))
    ccstrline(";")
end

proc do_vardef(symbol d)=


    if d.nameid=staticid then
        ccstr("static ")
    fi

    ccstr(strmodec(d.mode,getfullnamec(d)))

    if d.code then
        ccstr(" = ")
        do_initdata(d.code)
    elsif d.name^='_' then
        if eqstring(d.name,"_fnnprocs") then
            writefn_nprocs()
        elsif eqstring(d.name,"_fnaddresses") then
            writefn_addresses()
        elsif eqstring(d.name,"_fnnames") then
            writefn_names()
        fi

    elsif d.atvar then
        gerror("MODULE@")
    fi

    ccstrline(";")
end

proc do_dllvar(symbol d)=
    ccstr("extern ")
    ccstr(strmodec(d.mode,d.name))
    ccstrline(";")
end

proc genprocdef(symbol p)=
    [256]char str
    symbol d
    int ismain,isstart

    if p.mode<>tvoid then
        if not checkblockreturn(p.code) then
            gerror_s("C:Function needs explicit return: ",p.name)
        fi
    fi

    if eqstring(p.name,"start") then
        ccstrline("// START")
    fi

    currproc:=p
    ismain:=isstart:=0
    case p
    when moduletable[p.moduleno].stmain then ismain:=1
    when moduletable[p.moduleno].ststart then isstart:=1
    esac

    if p.scope=module_scope then
        ccstr("static ")
    fi

    if p.iscallback then
        ccstr("CALLBACK ")
    fi

    if ismain then
        do_maininit()
    else
        ccstr(strprocsig(p))
        ccstrline(" {")
        if isstart then
            do_startinit(p)
        fi
    fi

    d:=p.deflist

    while d do

        switch d.nameid
        when staticid,frameid then
            if d.used or d.code then
                genlocalvar(d) 
            fi
        when constid then
!       genconstdef(d)
        when paramid then
        when labelid then
        when typeid then
        when procid then
        when macroid then
        else
            fprint @&.str,"Can't output: # in # : #",d.name,p.name,namenames[d.nameid]
            gerror(&.str)
        endswitch
        d:=d.nextdef
    od

    blocklevel:=0

    do_block(p.code, braces:0)

    if ismain then
        do_mainterm()
    fi
    cctab()
    ccstr("}")

    ccblank()
    ccblank()

end

proc do_maininit=
    symbol d

    ccstrline("int main(int _nargs, char** _args, char** _envstrings) {")
    if msyslevel=1 then
        ccstrline("    mminc$m_init(_nargs, (void*)_args, (void*)_envstrings);")
    else
        ccstrline("    msysc$m_init(_nargs, (void*)_args, (void*)_envstrings);")
    fi
    ccsendline()
    ccstrline("// call main-start() routines...")

    for i:=2 to nsubprogs do
        d:=moduletable[subprogtable[i].firstmodule].ststart
        docallproc(d)
    od
    d:=moduletable[subprogtable[1].firstmodule].ststart
    docallproc(d)

    ccsendline()
end

proc do_startinit(symbol p)=
    symbol d
    int m, s

    m:=p.moduleno
    s:=p.subprogno
    if subprogtable[s].firstmodule=m then       !lead module in subprogram
        for i to nmodules when moduletosub[i]=s and i<>m do
            d:=moduletable[i].ststart
            docallproc(d)
        od
    fi
    ccsendline()
end

proc do_mainterm=
    ccstrline("    return 0;")
end

proc writefn_nprocs=
    ccstr("=")
    ccint(nallprocs)
end

proc writefn_names=
    int i
    symbol d

    ccstrline("= {")
    nallprocs:=0

    for i:=1 to nmodules do
        d:=moduletable[i].stmodule.deflist

        while d do
            if d.nameid=procid then
                ++nallprocs
                ccstr("(byte*)""",1)
                ccstr(d.name)
                ccstrline(""",")
            fi
            d:=d.nextdef
        od
    od
    ccstr("(byte*)""""}")
end

proc writefn_addresses=
    int i
    symbol d

    ccstrline("= {")
    nallprocs:=0

    for i:=1 to nmodules do
        d:=moduletable[i].stmodule.deflist

        while d do
            if d.nameid=procid then
                ++nallprocs
                ccstr("&",1)
                if eqstring(d.name,"main") and d.moduleno=mainmoduleno then
                    ccstr("main")
                else
                    ccstr(getprocname(d))
                fi
                ccstrline(",")
            fi
            d:=d.nextdef
        od
    od
    ccstr("0}")
end

proc genlocalvar(symbol d)=
    ichar prefix

    cctab(1)
    if d.nameid=staticid then
        ccstr("static ",1)
        ccstr(strmodec(d.mode,d.name))
        if d.code then
            ccstr(" = ")
            do_initdata(d.code)
        fi
    else
        ccstr(strmodec(d.mode,d.name),1)
    fi

    if d.code then

    elsif d.atvar then
        gerror_s("LOCAL@",d.name)
    fi

    ccstrline(";")
end

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
    ccstr(getfullnamec(d),1)
    ccstrline("();")
end



=== mm_lex.m 0 0 9/33 ===
macro hashc(hsum,c)=hsum<<4-hsum+c
macro hashw(hsum)=(hsum<<5-hsum)

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsource_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]tokenrec lxnextlx_stack
[maxstackdepth]byte lximport_stack
global int sourcelevel=0
global int lximport
!global int nincludestack

const cr    = 13
const lf    = 10
const tab   = 9

ref char lxsource
ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix          !for real nos

global int lxalllines

int lxfileno
global const hstsize    = 65536
global const hstmask    = hstsize-1

global [0:hstsize]symbol hashtable

global int astringlength

ichar u64maxstr="18446744073709551615"

[0..255]byte alphamap

['a'..'z']symbol shortnames

global proc lexreadtoken=
!read next token into nextlx
    int c,hsum,commentseen,hashindex,length
    ref char pstart,pnext,p,ss,lxsvalue

    nextlx.subcode:=0

    doswitch lxstart:=lxsptr; lxsptr++^
!   when 'a'..'z','_','$',0x80..0xEE, 0xF0..0xFF then
    when 'a'..'z','_','$' then
        lxsvalue:=lxsptr-1
    doname::
        hsum:=lxsvalue^

        if (c:=lxsptr^)='"' then
            case hsum
            when  'F','f','R','r' then 
                readrawstring()
                return
            when  'A','a','Z','z' then 
                readarraystring(lxsvalue^)
                return
            esac
        elsif alphamap[c] then
            docase alphamap[c:=lxsptr^]
            when 1 then
                hsum:=hashc(hsum,c)
                ++lxsptr
            when 2 then
                lxsptr^:=c+' '
                hsum:=hashc(hsum,c+' ')
                ++lxsptr
            else
                exit
            end docase
        else
            symbol d:=shortnames[lxsvalue^]
            if d then
                nextlx.symptr:=d
                nextlx.symbol:=d.symbol
                nextlx.subcode:=d.subcode
                return
            fi
        fi
!
        lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))
!
        return

    when 'A'..'Z' then
        lxsvalue:=lxsptr-1
        lxsvalue^+:=32
        goto doname

    when '0'..'9' then
        lxstart:=lxsptr-1
        case lxsptr^
        when ')',cr,',',' ' then        !assume single digit decimal
            nextlx.symbol:=intconstsym
            nextlx.subcode:=tint
            nextlx.value:=lxstart^-'0'
        when 'x','X' then
            case lxstart^
            when '0' then       !0x
                ++lxsptr
                readhex()
            when '2' then
                ++lxsptr
                readbin()
            when '8' then
                ++lxsptr
                readoct()
            else
                lxerror("Bad base")
            esac
        else
            --lxsptr
            readdec()
        esac
        return

    when '!' then           !comment to eol
docomment::
        doswitch c:=lxsptr++^
        when cr then
            ++lxalllines
            ++lxsptr
            exit
        when lf then
            ++lxalllines
            exit
        when 0 then
            --lxsptr
            exit
        end
!       ++nextlx.pos
        nextlx.symbol:=eolsym
        return

    when '#' then           !docstring to eol
!       if lxsptr^<>'#' then
            docomment
!       fi

        ++lxsptr
        lxsvalue:=cast(lxsptr)

        doswitch c:=lxsptr++^
        when cr then
            ++lxalllines
            ++lxsptr
            exit
        when lf then
            ++lxalllines
            exit
        when 0 then
            --lxsptr
            exit
        end

        length:=lxsptr-cast(lxsvalue,ref char)
        nextlx.symbol:=docstringsym
        nextlx.svalue:=pcm_copyheapstringn(lxsvalue,length)
        return

    when '\\' then          !line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
        commentseen:=0
        doswitch lxsptr++^          !read until end of this line
        when cr then
            ++lxalllines
!           ++nextlx.pos
            ++lxsptr                !skip lf
            exit
        when lf then
            ++lxalllines
!           ++nextlx.pos
            exit
        when 0 then
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
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

        doswitch lxsptr++^
        when cr then
!           ++nextlx.pos
            ++lxalllines
            ++lxsptr                !skip lf
        when lf then
            ++lxalllines
!           ++nextlx.pos
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
        when '.' then               !.. or ...
            ++lxsptr
            if lxsptr^='.' then
                ++lxsptr
                nextlx.symbol:=ellipsissym
            else
                nextlx.symbol:=rangesym
                nextlx.subcode:=jmakerange      !helps treat as opsym which all have k-code as subcode
            fi
            return
        when '0'..'9' then          !real const: deal with this after the switch
            --lxsptr
LXERROR(".123 not done")
!           readrealnumber(nil,0,10)
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
            nextlx.subcode:=jassign     !helps treat as opsym which all have k-code as subcode
        when ':' then
            ++lxsptr
            case lxsptr^
            when '=' then
                ++lxsptr
                nextlx.symbol:=deepcopysym
!               nextlx.subcode:=jdeepcopy
            else
                nextlx.symbol:=dcolonsym
            esac
        else
            nextlx.symbol:=colonsym
        endswitch
        return

    when '(' then
++NLBRACK

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
            nextlx.subcode:=kincr
++NINCR

            return
        fi
        return

    when '-' then
        nextlx.symbol:=subsym
        case lxsptr^
        when '-' then
            ++lxsptr
            nextlx.symbol:=incrsym
++NINCR
            nextlx.subcode:=kdecr
            return
        when '>' then
            ++lxsptr
            nextlx.symbol:=pipesym
            return
        esac
        return

    when '*' then
        if lxsptr^='*' then
            ++lxsptr
            nextlx.symbol:=powersym
        else
            nextlx.symbol:=mulsym
        fi
        return

    when '/' then
        nextlx.symbol:=divsym
        return

    when '%' then
        nextlx.symbol:=idivsym
        return

    when '=' then
        case lxsptr^
        when '>' then
            nextlx.symbol:=sendtosym
            ++lxsptr
        when '=' then
            ++lxsptr
!           if lxsptr^='=' then
!               ++lxsptr
!               nextlx.symbol:=ssmarkersym
!           else
                nextlx.symbol:=samesym
!           fi
        else
            nextlx.symbol:=eqsym
            nextlx.subcode:=keq
        esac
        return

    when '<' then
        nextlx.symbol:=cmpsym
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.subcode:=kle
        when '>' then
            ++lxsptr
            nextlx.subcode:=kne
        when '<' then
            ++lxsptr
            nextlx.symbol:=shlsym
        else
            nextlx.subcode:=klt
        endswitch
        return

    when '>' then
        nextlx.symbol:=cmpsym
        switch lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=cmpsym
            nextlx.subcode:=kge
        when '>' then
            ++lxsptr
            nextlx.symbol:=shrsym
        else
            nextlx.symbol:=cmpsym
            nextlx.subcode:=kgt
        endswitch
        return

    when '&' then
        case lxsptr^
        when '.' then
            ++lxsptr
            nextlx.symbol:=anddotsym
            nextlx.subcode:=0
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
        ++lxsptr                !skip lf
!       ++nextlx.pos
        ++lxalllines
        nextlx.symbol:=eolsym
        return
    when lf then            !only lfs not preceded by cr
!       ++nextlx.pos
        ++lxalllines
        nextlx.symbol:=eolsym
        return

    when 0 then
        if sourcelevel then
!           --nincludestack
            unstacksource()
RETURN
        else
            nextlx.symbol:=eofsym
            --lxsptr
            return
        fi

    when 0xEF then          !BOM
        lxsptr+:=2

    else
        nextlx.symbol:=errorsym
        return

    end doswitch

end

global proc lex=
    int lena,lenb
    ref char p

    lx:=nextlx              !grab that already read basic token
    lx.sourceoffset:=lxstart-lxsource

reenter::
    lexreadtoken()
reenter2::

    switch nextlx.symbol
    when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,
            kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
            krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
            ktabledatasym,kassemsym,kifsym then

        if lx.symbol=kendsym then
            if lx.subcode then lxerror("end if if?") fi
            lx.subcode:=nextlx.symbol
            reenter
        fi

    when eolsym then
        if lx.symbol in [commasym, lsqsym, lbracksym] then !ignore eol
            reenter
        elsif symboloptypes[lx.symbol]=bin_op and not assemmode and 
            lx.symbol not in [maxsym, minsym] then
            reenter
        fi
        nextlx.symbol:=semisym
        nextlx.subcode:=1

    when stringconstsym then
        if lx.symbol=stringconstsym then
            lena:=strlen(lx.svalue)
            lenb:=strlen(nextlx.svalue)
            p:=pcm_alloc(lena+lenb+1)
            memcpy(p,lx.svalue,lena)
            memcpy(p+lena,nextlx.svalue,lenb)
            (p+lena+lenb)^:=0
            lx.svalue:=p
        fi
    when ksourcedirsym then
        if not dolexdirective(nextlx.subcode) then      !skip symbol
            reenter
        fi

    when namesym then
        case nextlx.subcode
        when unitnamesym then
            case lx.symbol
            when intconstsym then
                case nextlx.symptr.index
                when million_unit then lx.value *:= 1 million
                when billion_unit then lx.value *:= 1 billion
                when thousand_unit then lx.value *:= 1 thousand
                when kilo_unit then lx.value *:= 1024
                when mega_unit then lx.value *:= 1048576
                when giga_unit then lx.value *:= (1048576*1024)
                else
                    lxerror("Can't do this unit index")
                esac
                lx.subcode:=setinttype(lx.value)
                reenter
            when realconstsym then
                lxerror("Unit suffix after float not implem")
            else
                nextlx.symbol:=namesym
            esac
        when kheadersym then
            if not headermode then
                nextlx.symbol:=namesym
            else
                nextlx.symbol:=kheadersym
                nextlx.subcode:=nextlx.symptr.index

            fi
        else
            nextlx.symbol:=namesym
        esac
!   when machinetypesym then
!       case nextlx.subcode
!       when 'I','i' then nextlx.subcode:=ti64
!       when 'W','w' then nextlx.subcode:=tu64
!       esac
!       nextlx.symbol:=stdtypesym

    when rawxnamesym then
        nextlx.symbol:=namesym

    when insym then
        if lx.symbol=notlsym then
            lx.symbol:=notinsym
            lx.subcode:=knotin
            reenter
        fi
    when eqsym then
        if lx.symbol=notlsym then
            lx.symbol:=cmpsym
            lx.subcode:=kne
            reenter
        fi
    end switch

    nextlx.pos :=nextlx.pos ior lxfileno<<24
end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
    inithashtable()
end

global proc printstrn(ichar s, int length)=
    if length then
        print length:"v",s:".*"
    fi
end

proc readrawstring=
!positioned at " of F"
!read raw string
    ichar dest
    int c

    nextlx.symbol:=stringconstsym
    nextlx.svalue:=++lxsptr

    dest:=lxsptr                !form string into same buffer

    doswitch c:=lxsptr++^
    when '"' then
        if lxsptr^='"' then     !repeated, assume embedded term char
            dest++^:='"'
            ++lxsptr
        else            !was end of string
            (lxsptr-1)^:=0
            exit
        fi
    when cr,lf,0 then
        lxerror("Raw string not terminated")
        --lxsptr
        exit
    else
        dest++^:=c
    enddoswitch
end

proc lookup(ref char name, int length, hashindex0)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
    int wrapped, hashindex,INDEX,n
    symbol d
    int j

    j:=hashindex0 iand hstmask

    d:=hashtable[j]
    wrapped:=0

    do
        if d=nil then exit fi

        if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then   !match
            nextlx.symptr:=d
            nextlx.symbol:=d.symbol
            nextlx.subcode:=d.subcode
            return
        fi

        if ++j>=hstsize then
            if wrapped then
                abortprogram("HASHTABLE FULL")
            fi
            wrapped:=1
            j:=0
        fi
        d:=hashtable[j]
    od

!exit when not found; new name will go in entry pointed to by lxsymptr

    d:=pcm_allocz(strec.bytes)
    hashtable[j]:=d

    d.name:=pcm_copyheapstringn(name,length)
    d.namelen:=length
    d.symbol:=namesym

    nextlx.symptr:=d
    nextlx.symbol:=d.symbol

    if length=1 and name^ in 'a'..'z' then
        shortnames[name^]:=d
    fi

end

function lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
    int j, wrapped, hashvalue

    j:=gethashvaluez(name) iand hstmask

    lx.symptr:=hashtable[j]
    wrapped:=0

    do
        if lx.symptr=nil then
            exit
        elsif eqstring(lx.symptr.name,name) then    !match
            println "Lex dupl name:",name
            stop 1 
!           lxerror("sys dupl name?")
        fi

        if ++j>=hstsize then
            if wrapped then
                abortprogram("SYS:HASHTABLE FULL")
            fi
            wrapped:=1
            j:=0
        fi
        lx.symptr:=hashtable[j]
    od

!exit when not found; new name will go in entry pointed to by lxsymptr
    lx.symptr:=pcm_allocz(strec.bytes)
    hashtable[j]:=lx.symptr

    lx.symptr.name:=name                !assume can be shared (stored in a table)
    lx.symptr.namelen:=strlen(name)
    lx.symptr.symbol:=namesym           !usually replaced with actual symbol details

    return 0
end

function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
    int c,hsum

    if s^=0 then return 0 fi

    hsum:=s++^

    do
        c:=s++^
        exit when c=0
        hsum:=hashc(hsum,c)
    od
    return hashw(hsum)
end

proc inithashtable=
!populate hashtable with standard symbols
    int i
    memset(&hashtable,0,hashtable.bytes)

    for i:=1 to stnames.len do
        lookupsys(stnames[i])

        lx.symptr.symbol:=stsymbols[i]

        case stsymbols[i]
        when unitnamesym, kheadersym then
            lx.symptr.index:=stsubcodes[i]
            lx.symptr.subcode:=stsymbols[i]
            lx.symptr.symbol:=namesym       !masquerades as normal identifier
        else
            lx.symptr.subcode:=stsubcodes[i]
        esac
    od
end

global proc printhashtable=
    println "Hashtable:"

!   for i:=0 to hstsize-1 do
!       if hashtable[i] then
!       fi
!   od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
    lookupsys(name)

    lx.symptr.symbol:=namesym
    lx.symptr.subcode:=symbol
    lx.symptr.index:=subcode

    lx.symptr.regsize:=regsize
end

function dolexdirective(int index)int=
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
    ref strec symptr
    ref char p
    ichar file
    int i,lastsymbol,cond,fileno,length
    [256]char str

    case index

    when binincludedir then
        lexreadtoken()
        if nextlx.symbol<>stringconstsym then
            lxerror("strincl: string expected")
        else
            file:=nextlx.svalue
        fi

        fileno:=getsupportfile(file)
        nextlx.svalue:=sourcefiletext[fileno]
        astringlength:=length:=sourcefilesizes[fileno]

        nextlx.symbol:=astringconstsym
        nextlx.subcode:='A'         !for use when an astring
        (nextlx.svalue+length)^:=0          !sometimes .length is not used (eg. in newstringobj())
        return 1                        !so get it right. Don't need the etx

    when includedir then
        lexreadtoken()
        if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
        file:=nextlx.svalue
        convlcstring(file)
        file:=addext(file,langext)      !add in extension if not present; assume same as source

        fileno:=getsupportfile(file, path:sourcefilepaths[lxfileno], issupport:1)
!       ++nincludestack
        lexreadtoken()
        stacksource(fileno)
        return 0

    else
        cpl sourcedirnames[index]
        lxerror("Directive not implemented")
    esac
    return 0
end

global proc startlex(int fileno)=
!start processing one of the file in sourcefile tables as source code
!assume it is a complete header or module

    lxsource:=lxsptr:=sourcefiletext[fileno]

    nextlx.pos:=0
    lxfileno:=fileno

    nextlx.symbol:=semisym
    nextlx.subcode:=0
end

proc start=
    for i:=0 to 255 do
        switch i
!       when 'a'..'z','0'..'9','_','$',0x80..0xEE, 0xF0..0xFF then
        when 'a'..'z','0'..'9','_','$' then
            alphamap[i]:=1
        when 'A'..'Z' then
            alphamap[i]:=2
        end switch
    od
end

global function addnamestr(ichar name)ref strec=
    tokenrec oldlx
    ref strec symptr

    oldlx:=nextlx
    lookup(name,strlen(name), gethashvaluez(name))
    symptr:=nextlx.symptr
    nextlx:=oldlx

    return symptr
end

global proc ps(ichar caption)=
    print "PS:",caption,,": "
    printsymbol(&lx)
end

global proc psnext(ichar caption)=
    print caption,,": "
    printsymbol(&nextlx)
end

global proc psx(ichar caption)=
    print caption,,": "
    printsymbol(&lx)
    print " "
    printsymbol(&nextlx)
end

global proc stacksource(int fileno, isimport=0)=
!introduce new source level for macros or include files
!not used for main source

    if sourcelevel>=maxstackdepth then
        lxerror("Include file/macro overflow")
    fi
    ++sourcelevel
    lxstart_stack[sourcelevel]:=lxstart
    lxsource_stack[sourcelevel]:=lxsource
    lxsptr_stack[sourcelevel]:=lxsptr
    lxfileno_stack[sourcelevel]:=lxfileno
    lxnextlx_stack[sourcelevel]:=nextlx
    lximport_stack[sourcelevel]:=lximport
    lximport:=isimport

    lxsource:=lxsptr:=sourcefiletext[fileno]

    nextlx.pos:=0
    lxfileno:=fileno

    nextlx.symbol:=semisym
    nextlx.subcode:=0
end

global proc unstacksource=
    if sourcelevel>0 then           !check that some source is stacked
        lxstart:=lxstart_stack[sourcelevel]
        lxsource:=lxsource_stack[sourcelevel]
        lxsptr:=lxsptr_stack[sourcelevel]
        nextlx:=lxnextlx_stack[sourcelevel]
        lxfileno:=lxfileno_stack[sourcelevel]
        lximport:=lximport_stack[sourcelevel]

        --sourcelevel
    fi
end

proc readarraystring(int prefix)=
    ++lxsptr
    lxreadstring('"')
    nextlx.symbol:=astringconstsym
    nextlx.subcode:=toupper(prefix)
    astringlength:=strlen(nextlx.svalue)
end

function setinttype(word64 a)int=
    if a<=u64(0x7FFF'FFFF'FFFF'FFFF) then
        return ti64
    else
        return tu64
    fi
end

proc readrawxname=
    int c,hsum,length

    nextlx.svalue:=lxsptr
    hsum:=0

    doswitch c:=lxsptr++^
    when 'A'..'Z','a'..'z','0'..'9','_','$' then
        hsum:=hashc(hsum,c)
    else
        --lxsptr
        exit
    end doswitch

    length:=lxsptr-nextlx.svalue

    if length=0 then
        lxerror("Bad ` name")
    fi
    lookup(nextlx.svalue,length, hashw(hsum))
    nextlx.symbol:=rawxnamesym

    return
end

proc lxerror_s(ichar mess,s)=
    lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

    ichar s,t
    int c, d, length, hasescape
    [8]char str

    if termchar='"' then
        nextlx.symbol:=stringconstsym
    else
        nextlx.symbol:=charconstsym
        nextlx.subcode:=tint
    fi

    s:=lxsptr

!do a first pass that terminates length of final string
    length:=0
    hasescape:=0

    doswitch c:=lxsptr++^
    when '\\' then          !escape char
        c:=lxsptr^
        if c in 'A'..'Z' then c+:=' ' fi
        ++lxsptr
        hasescape:=1

        switch c
        when 'a','b','c','e','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
            ++length
        when 'w' then
            ++length
        when 'x' then   !2-digit hex code follows
            lxsptr+:=2
            ++length
        else
            lxerror("Bad str escape")
        endswitch
    when '"','\'' then      !possible terminators
        if c=termchar then      !terminator char
            if lxsptr^=c then       !repeated, assume embedded term char
                hasescape:=1
                ++lxsptr
                ++length
            else            !was end of string
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

    if length=0 then
        nextlx.svalue:=""
        return
    elsif not hasescape then
        nextlx.svalue:=pcm_copyheapstringn(s,length)
        return
    fi

!need to copy string to dest and expand the escape codes

    nextlx.svalue:=t:=pcm_alloc(length+1)

    do
        switch c:=s++^
        when '\\' then          !escape char
            c:=s^
            if c>='A'  and c<='Z' then c+:=' ' fi
            ++s
            switch c
            when 'a' then           !bell ('alert')
                c:=7
            when 'b' then           !backspace
                c:=8
            when 'c','r' then       !carriage return
                    c:=cr
            when 'e' then           !end-of-text
                c:=26
            when 'f' then           !formfeed
                c:=12
            when 'l','n' then       !linefeed, or linux/c-style newline
                c:=lf
            when 's' then           !eScape
                c:=27
            when 't' then           !tab
                c:=9
!           when 'u' then           !reserved for unicode, like \x but with 4 hex digits
            when 'v' then           !vertical tab
                c:=11
            when 'w' then           !windows-style cr-lf
                t++^:=cr
                c:=lf
            when 'x' then   !2-digit hex code follows
                c:=0
                to 2 do
                    case d:=s++^
!                   switch d:=s++^
                    when 'A','B','C','D','E','F' then
                        c:=c*16+d-'A'+10
                    when 'a','b','c','d','e','f' then
                        c:=c*16+d-'a'+10
                    when '0','1','2','3','4','5','6','7','8','9' then
                        c:=c*16+d-'0'
                    else
                        lxerror("Bad \\x code")
                    end
                od
            when 'y' then           !CCI/SM backwards tab
                c:=16
            when 'z','0' then       !null (not fully supported in code)
                c:=0
            when '"','Q' then       !embedded double quote
                c:='"'
            when '\\' then
                c:='\\'
            when '\'' then          !embedded single quote
                c:='\''
            else
                str[1]:=c; str[2]:=0
                lxerror_s("Unknown string escape: \\%s",&.str)
            end
        when '"','\'' then      !possible terminators
            if c=termchar then      !terminator char
                if s^=c then        !repeated, assume embedded term char
                    ++s
                else            !was end of string
                    exit
                fi
            fi
        when cr,lf,0 then
            lxerror("String not terminated")
        endswitch

        t++^:=c
    od

    t^:=0
end

proc readdec=
    int c
    ref char dest, destend, pstart
    int islong, length
    [1024]byte str
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
LXERROR("MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,10)
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
LXERROR("2:MAKEDECIMAL NOT READY")
!       makedecimal(&.str,length,10)
        return
!       lxerror("u64 overflow")
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
    [1024]byte str
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
LXERROR("3:MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,16)
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
LXERROR("4:MAKEDECIMAL NOT READY")
!       makedecimal(&.str,length,16)
        return
!       lxerror("u64 overflow")
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readoct=
    int c
    ref char dest, destend, pstart
    int length
    [1024]byte str
    word a

    pstart:=lxsptr

    dest:=&.str
    destend:=dest+str.len-10
    a:=0

    do
        switch c:=lxsptr++^
        when '0'..'7' then
            a:=a*8+c-'0'
            dest++^:=c
        when '_','\'' then
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

    if length>22 then
        lxerror("oct overflow")
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readbin=
    int c
    ref char dest, destend, pstart
    int length
    [1024]byte str
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
LXERROR("5:MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,2)
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
LXERROR("6:MAKEDECIMAL NOT READY")
!       makedecimal(&.str,length,2)
        return
!       lxerror("u64 overflow")
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

    int c,n,negexpon,dotseen,length, fractlen, expon, expseen
    real x
    [1024]char str
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
LXERROR("7:MAKEDECIMAL NOT READY")
!               makedecimal(&.str,dest-&.str,10)
                return
            else
                --lxsptr
                exit all
            end doswitch

        when '_','\'' then

        when 'l','L' then
LXERROR("8:MAKEDECIMAL NOT READY")
!           makedecimal(&.str,dest-&.str,10)
            return
        else
            --lxsptr
            exit
        end switch

        if dest>=destend then lxerror("r64lit too long") fi
    end
    dest^:=0

!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
    if negexpon then expon:=-expon fi
    expon-:=fractlen
    x:=0.0

    for i:=1 to length+dotseen do       !digits already range-checked
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
!------------------------------------------------------------
! Best way to covert: more accurate representation, but slower
!------------------------------------------------------------
!   nextlx.xvalue:=strtod(str,nil)
!------------------------------------------------------------

    nextlx.symbol:=realconstsym
    nextlx.subcode:=treal
end
=== mm_lib.m 0 0 10/33 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0

!const allcomplex=1         !disable issimple-checking; everything is complex
const allcomplex=0          !normal issimple-checking


strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global ichar framevarname           !normally nil, set to frame var def to display in comment

global macro isnum(m) = (m in tfirstnum..tlastnum)
global macro isnumx(m) = (m in tfirstnum..tlastnum)
global macro isnumf(m) = (m in [tr64, tr32])
global macro isnumi(m) = (m in [ti64, tu64, tc64])

global function newstrec:symbol=
    symbol p
    p:=pcm_alloc(strec.bytes)
    clear p^

    p.pos:=lx.pos
    p.moduleno:=currmoduleno
    p.subprogno:=moduletosub[currmoduleno]
    return p
end

global function getduplnameptr(symbol owner,symptr,int id)symbol=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
    symbol p,q

    p:=newstrec()

    p.name:=symptr.name
    p.namelen:=symptr.namelen
    p.symbol:=namesym
    p.owner:=owner
    p.nameid:=id

!   if id=frameid or id=paramid then
!       p.frame:=1
!   fi

    p.nextdupl:=symptr.nextdupl
    p.firstdupl:=symptr
    symptr.nextdupl:=p

    return p
end

global proc adddef(symbol owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
    symbol q

    if q:=p.nextdupl then
        if q.owner=owner then
            cpl q.name,"in",owner.name
            serror("Duplicate name")
        fi
    fi

    if owner.deflist=nil then           !first def
        owner.deflist:=p
    else
        owner.deflistx.nextdef:=p
    fi

    owner.deflistx:=p
end

global function createname(symbol p)ref unitrec=
    ref unitrec u

    u:=allocunitrec()
    u.tag:=jname
    u.def:=p
    u.simple:=1

    return u
end

global function createunit0(int tag)ref unitrec=
    ref unitrec u

    u:=allocunitrec()
    u.tag:=tag
    return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
    ref unitrec u
    u:=allocunitrec()
    u.tag:=tag
    u.a:=p
    return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
    ref unitrec u

    u:=allocunitrec()

    u.tag:=tag
    u.a:=p
    u.b:=q
    return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
    ref unitrec u

    u:=allocunitrec()
    u.tag:=tag
    u.a:=p
    u.b:=q
    u.c:=r
    return u
end

global proc insertunit(unit p,int tag)=
!wrap extra unit around p, with given tag
!p itself is effectively changed
    unit q,nextunit
    int mode

    q:=allocunitrec()
    q^:=p^
    mode:=q.mode
    nextunit:=q.nextunit
    q.nextunit:=nil

    clear p^

    p.tag:=tag
    p.pos:=q.pos
    p.a:=q
    p.mode:=mode
    p.nextunit:=nextunit
    p.resultflag:=q.resultflag
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
    unit r:=p.nextunit
    p^:=q^
    p.nextunit:=r
end

global function createconstunit(word64 a, int t)ref unitrec=
    ref unitrec u
    u:=allocunitrec()
    u.tag:=jconst
    u.value:=a
    u.mode:=t

    u.isconst:=1
    u.simple:=1
    return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
    ref unitrec u
    u:=allocunitrec()
    u.tag:=jconst
    u.svalue:=s
    u.mode:=trefchar
    u.isastring:=1
    if length=-1 then
        u.slength:=strlen(s)
    else
        u.slength:=length
    fi
    return u
end

global function newtypename(symbol a,b)int=
    if ntypenames>=maxtypename then
        serror("Too many type names")
    fi
    ++ntypenames
    typenames[ntypenames].defa:=a       !leave .owner/.pmode to be filled in
    typenames[ntypenames].defb:=b       !used type's mode is used

    typenamepos[ntypenames].pos:=lx.pos

    return -ntypenames
end

global function createusertype(symbol stname)int=
!create new, named user type
    if ntypes>=maxtype then
    cpl ntypes,stname.name
        serror("Too many types")
    fi

    ++ntypes
    ttname[ntypes]:=stname.name

    ttnamedef[ntypes]:=stname
    ttbasetype[ntypes]:=tvoid
    ttlineno[ntypes]:=lx.pos

    stname.mode:=ntypes

    return ntypes
end

global function createusertypefromstr(ichar name)int=
!create new, named user type
    symbol stname

    stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
    return createusertype(stname)
end

global function getrangelwbunit(ref unitrec p)ref unitrec=
    if p.tag=jmakerange then
        return p.a
    else
        p:=createunit1(junary,p)
        p.pclop:=klwb
        return p
    fi
end

global function getrangeupbunit(ref unitrec p)ref unitrec=
    if p.tag=jmakerange then
        return p.b
    else
        p:=createunit1(junary,p)
        p.pclop:=kupb
        return p
    fi
end

global function createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
    int k,m

    if typedefx=0 then      !anon type
        for k:=tlast to ntypes do
            if ttusercat[k]=0 and ttbasetype[k]=tarray and tttarget[k]=target and
                    sameunit(dimexpr, ttdimexpr[k],owner, ttowner[k]) then
                return k
            fi
        od
        m:=createusertypefromstr(nextautotype())
    else
        m:=typedefx
    fi

    ttbasetype[m]:=tarray
    ttlower[m]:=1
    ttdimexpr[m]:=dimexpr
    storemode(owner,target,tttarget[m])
    ttowner[m]:=owner

    ttcat[m]:=blockcat
    ttisblock[m]:=1

    return m
end

function sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
    symbol d,e

    if p=q then return 1 fi
    if p=nil or q=nil then return 0 fi

    if p.tag<>q.tag then return 0 fi

    case p.tag
    when jconst then
        return p.value=q.value
    when jmakerange,jkeyvalue then
        return sameunit(p.a, q.a) and sameunit(p.b, q.b)
    when jname then
        if p.def=q.def and powner=qowner then
            return 1
        fi
    esac

    return 0

end

global function createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
!lower is lower bound of array
    int atype,k,m

    atype:=tarray

    if typedefx=0 then      !anon type
!   for k:=tlast to ntypes do
!       if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!           ttlower[k]=lower and ttlength[k]=length then
!           return k
!       fi
!   od
        m:=createusertypefromstr(nextautotype())
    else
        m:=typedefx
    fi

    ttbasetype[m]:=atype
    ttlower[m]:=lower
    ttlength[m]:=length
    IF TARGET<0 THEN
        SERROR("CREATEARRAYMODEK/TARGET NOT RESOLVED")
    FI
    ttsize[m]:=length*ttsize[target]

    storemode(owner,target,tttarget[m])
    ttowner[m]:=owner
    ttcat[m]:=blockcat
    ttisblock[m]:=1

    return m
end

global function nextautotype:ichar=
    static [32]char str

    print @&.str,"$T",,++autotypeno
    return &.str
end

global function createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
    int k,m

    if typedefx=0 then      !anon type
!   for k:=tlast to ntypes do
!       if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!           ttlower[k]=lower and ttlength[k]=length then
!           return k
!       fi
!   od
        m:=createusertypefromstr(nextautotype())
    else
        m:=typedefx
    fi

    ttbasetype[m]:=slicetype
    if dimexpr then
        ttdimexpr[m]:=dimexpr
    else
        ttlower[m]:=1
    fi
    storemode(owner,target,tttarget[m])
    ttowner[m]:=owner
    ttcat[m]:=blockcat
    ttisblock[m]:=1

    return m
end

global function createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
    int k,m

    if typedefx=0 then      !anon type
!   for k:=tlast to ntypes do
!       if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!           ttlower[k]=lower and ttlength[k]=length then
!           return k
!       fi
!   od
        m:=createusertypefromstr(nextautotype())
    else
        m:=typedefx
    fi

    ttbasetype[m]:=tslice
    ttlower[m]:=lower
    storemode(owner,target,tttarget[m])
    ttowner[m]:=owner
    ttcat[m]:=blockcat
    ttisblock[m]:=1

    return m
end

global function createrefmode(symbol owner,int target,typedefx=0)int=
    int k,m
!   int a,b

!PRINTLN "CREATEREFMODE",=STRMODE(TARGET)

    if typedefx=0 then      !anon type
!   if typedefx=0 AND TARGET>=0 then        !anon type
        for k:=tlast to ntypes when ttisref[k] do
            if tttarget[k]=target then
                return k
            fi
!           if target<0 and tttarget[k]<0 then
!               a:=-target; b:=-tttarget[k]
!               if typenames[a].defb=typenames[b].defb and
!                   typenamepos[a].fileno=typenamepos[b].fileno then
!!                  return k
!               fi
!           fi
    
        od
        m:=createusertypefromstr(nextautotype())
    else
        m:=typedefx
    fi

    storemode(owner,target,tttarget[m])
    ttbasetype[m]:=tref
    ttsize[m]:=ttsize[tref]
    ttisref[m]:=1
    ttcat[m]:=d64cat

    return m
end

global function createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
    int m, mproc

    mproc:=createusertype(stproc)
    stproc.paramlist:=paramlist

    stproc.mode:=prettype
    ttbasetype[mproc]:=tproc

!don't bother looking for similar proc sig; each one is unique
    if typedefx=0 then      !anon type
        m:=createusertypefromstr(nextautotype())
    else
        m:=typedefx
    fi

    tttarget[m]:=mproc
    ttbasetype[m]:=tref

    ttsize[m]:=ttsize[tref]
    ttisref[m]:=1
    ttcat[m]:=d64cat

    return m
end

global proc copyttvalues(int dest, source)=
    ttsigned[dest]      := ttsigned[source]
    ttisreal[dest]      := ttisreal[source]
    ttisinteger[dest]   := ttisinteger[source]
    ttisshort[dest]     := ttisshort[source]
    ttisref[dest]       := ttisref[source]
    ttcat[dest]         := ttcat[source]
    ttisblock[dest]     := ttisblock[source]
end

global function getdottedname(symbol p)ichar=
!build full dotted name for st item p
    static [256]char str
    [256]char str2
    symbol owner

    strcpy(&.str,p.name)
    owner:=p.owner
    while owner and owner.nameid<>programid do
        strcpy(&.str2,&.str)
        strcpy(&.str,owner.name)
        strcat(&.str,".")
        strcat(&.str,&.str2)
        owner:=owner.owner
    od
    return &.str
end

global function getavname(symbol owner,int id=frameid)symbol=
!create auto-var name and return pointer to st entry
    symbol p
    [32]char str
    ichar name

    if id=frameid and owner.nameid<>procid then
        serror("Auto frame not in proc")
    fi

    if id=frameid then
        print @&.str,"$av_",,++nextavindex
    else
        print @&.str,"$sv_",,++nextsvindex
    fi

    name:=pcm_copyheapstring(&.str)
    addnamestr(name)

    p:=getduplnameptr(owner,addnamestr(name),id)
    p.used:=1

    p.mode:=tint

    adddef(owner,p)
    return p
end

global proc unionstr_clear(ref uflagsrec u)=
    ((ref word64(u))^:=0)       !clear flags and length togetjer
end

global proc unionstr_append(ref uflagsrec u, int c)=
    if u.ulength=(u.codes.len-1) then
        serror("Uflags overflow/a")
    fi
    ++u.ulength
    u.codes[u.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
    int ulen,vlen,i

    ulen:=u.ulength
    vlen:=v.ulength
    if ulen+vlen>u.codes.len then
        serror("Uflags overflow/c")
    fi
    for i:=1 to vlen do
        u.codes[i+ulen]:=v.codes[i]
    od
    u.ulength:=ulen+vlen
end

global function unionstr_last(ref uflagsrec u)int=
    if u.ulength then
        return u.codes[u.ulength]
    fi
    return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
    memcpy(u,v,uflagsrec.bytes)
end

global function createrecordmode(symbol owner,int typedefx)int=
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
    int m

    if typedefx=0 then
        m:=createusertype(owner)
    else
        m:=typedefx
    fi
    ttbasetype[m]:=trecord
    ttusercat[m]:=1
    ttcat[m]:=blockcat
    ttisblock[m]:=1

    return m
end

global function createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
    int m

    if typedefx=0 then
        m:=createusertype(owner)
    else
        m:=typedefx
    fi
    ttbasetype[m]:=ttuple
    ttusercat[m]:=1
    ttlength[m]:=elementslen
    ttmult[m]:=pcm_alloc(elementslen*int32.bytes)
    for i to elementslen do
        storemode(owner,elements[i],ttmult[m,i])
    od

    return m
end

!global function createenummode(symbol owner,int typedefx)int=
!!typedef is nil, or an empty moderec belonging to a user type
!!owner is an strec for the name def::
!! * user-supplied name belonging to the typedef (same as typedef.namedef)
!! * user-supplied optional name from a stand-alone enum typespec
!! * auto-generated name
!   int m
!
!   if typedefx=0 then
!       m:=createusertype(owner)
!   else
!       m:=typedefx
!   fi
!   ttbasetype[m]:=tenum
!   ttusercat[m]:=1
!   ttcat[m]:=d64cat
!
!   return m
!end

global function convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
    int c
    ichar t0:=t

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

    return t-t0
end

global function strexpr(ref unitrec p)ref strbuffer=
    gs_init(exprstr)

    jevalx(exprstr,p)
    return exprstr
end

global proc jevalx(ref strbuffer dest, ref unitrec p)=
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
    unit q,a,b
    [500]char str

    if p=nil then
        return
    fi

    a:=p.a
    b:=p.b

    switch p.tag
    when jconst then

        case ttbasetype[p.mode]
        when ti32,ti64,ti8,ti16 then
            getstrint(p.value,&.str)
        when tu32,tu64,tu8,tu16 then
            strcpy(&.str,strword(p.uvalue))
        when tc8,tc64 then
            str[1]:=p.uvalue
            str[0]:=0

        when treal,tr32 then
            print @&.str,p.xvalue
        when tref then
            if p.mode=trefchar and p.isastring then
                if p.slength>str.len/2 then
                    strcpy(&.str,"LONGSTR)")
                else
                    convertstring(p.svalue,&.str)
                fi
                gs_additem(dest,"""")
                gs_additem(dest,&.str)
                gs_additem(dest,"""")
                return
            else
                print @&.str,ref void(p.value)
            fi
        else
            strcpy(&.STR,"<EVAL/CONST PROBABLY VOID>")
        esac
        gs_additem(dest,&.str)

    when jname then
        gs_additem(dest,p.def.name)

    when jbin,jcmp then

        strcpy(&.str,pclnames[p.pclop])
        gs_additem(dest,"(")
        jevalx(dest,a)
        gs_additem(dest,&.str)
        jevalx(dest,b)
        gs_additem(dest,")")

    when junary, jistruel, jnotl then

        strcpy(&.str,pclnames[p.pclop])
        gs_additem(dest,&.str)
        gs_additem(dest,"(")

        if a.tag=jtypeconst then
            gs_additem(dest,STRMODE(a.value))
        else
            jevalx(dest,a)
        fi
        gs_additem(dest,")")

    when jcallfn,jcallproc then
        jevalx(dest,a)
        gs_additem(dest,"(")

        q:=b
        while q do
            jevalx(dest,q)
            q:=q.nextunit
            if q then gs_additem(dest,",") fi
        od
        gs_additem(dest,")")

    when jindex,jdotindex,jslice,jdotslice then
        jevalx(dest,a)
        if p.tag=jdotindex or p.tag=jdotslice then
            gs_additem(dest,".")
        fi
        gs_additem(dest,"[")
        jevalx(dest,b)
        gs_additem(dest,"]")

    when jdot then
        jevalx(dest,a)
        gs_additem(dest,".")
        jevalx(dest,b)

    when jmakelist then
        gs_additem(dest,"(")

        q:=a
        while q do
            jevalx(dest,q)
            q:=q.nextunit
            if q then gs_additem(dest,",") fi
        od
        gs_additem(dest,")")

    when jmakerange then
        gs_additem(dest,"(")
        jevalx(dest,a)
        gs_additem(dest,"..")
        jevalx(dest,b)
        gs_additem(dest,")")

    when jassign then
        jevalx(dest,a)
        gs_additem(dest,":=")
        jevalx(dest,b)

    when jif then
        gs_additem(dest,"(")
        jevalx(dest,a)
        gs_additem(dest,"|")
        jevalx(dest,b)
        gs_additem(dest,"|")
        jevalx(dest,p.c)
        gs_additem(dest,")")

    when jtypeconst then
        gs_additem(dest,strmode(p.mode))

    when jconvert,jtypepun then

        gs_additem(dest,strmode(p.convmode))
        if p.tag=jtypepun then
            gs_additem(dest,"@")
        fi
        gs_additem(dest,"(")
        jevalx(dest,a)
        gs_additem(dest,")")

    when jshorten then

        gs_additem(dest,"shorten(")
        jevalx(dest,a)
        gs_additem(dest,")")
    when jautocast then

        gs_additem(dest,"cast(")
        jevalx(dest,a)
        gs_additem(dest,")")
    when jkeyvalue then
        jevalx(dest,a)
        gs_additem(dest,":")
        if b then
            jevalx(dest,p.b)
        else
            gs_str(dest,"-")
        fi

    when jptr then
        jevalx(dest,a)
        gs_additem(dest,"^")

    when jclamp then
        gs_additem(dest,"(")
        jevalx(dest,a)
        gs_additem(dest,",")
        jevalx(dest,b)
        gs_additem(dest,",")
        jevalx(dest,p.c)
        gs_additem(dest,")")

    when jblock then
        gs_additem(dest,"<JBLOCK>")

    when jnull then
        gs_str(dest,"<nullunit>")

    when jaddrof then
        gs_additem(dest,"&")
        jevalx(dest,a)
        if b then
            gs_str(dest,"+")
            gs_strint(dest,b.value)
        fi

    when jaddroffirst then
        gs_additem(dest,"&.")
        jevalx(dest,a)

    when jtypestr then
        gs_additem(dest,"TYPESTR(")
        jevalx(dest,a)
        gs_additem(dest,")")

    when jcvlineno, jcvfilename, jcvmodulename then
        gs_str(dest,"$")
        gs_str(dest,jtagnames[p.tag]+2)

    when jbitfield then
        jevalx(dest,a)
        gs_str(dest,".")
        gs_str(dest,bitfieldnames[p.bitopindex])

    when jfmtitem then
        jevalx(dest,a)
        gs_str(dest,":")
        jevalx(dest,b)

    when jtypeof then
        gs_str(dest,"typeof(")
        jevalx(dest,a)
        gs_str(dest,")")

    when jsyscall then
        gs_str(dest,sysfnnames[p.fnindex]+3)
        gs_str(dest,"(")
        if a then jevalx(dest,a) fi
        gs_str(dest,")")
    when jincr then
        gs_str(dest,"incr ")
        jevalx(dest,a)
    when jstrinclude then
        gs_str(dest,"newstrinclude ")
        jevalx(dest,a)

    else
        PRINTLN jtagnames[p.tag]
        gerror("CAN'T DO JEVAL",p)
    end
end

global function strmode(int m,expand=1)ichar=
    static [4096]char str
    istrmode(m,expand,&.str)
    return &.str
end

global function strmode2(int m,expand=1)ichar=
    static [4096]char str
    istrmode(m,expand,&.str)
    return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=
    symbol d,q,e
    int value,needcomma,x,i,target,mbase,n
    strbuffer sxx
    ref strbuffer xx:=&sxx
    ref strbuffer sdim,slength
    [100]char strdim
    ichar prefix
    typenamerec tn

    if m<0 then
        strcpy(dest,"*")
        tn:=typenames[-m]

        if tn.defb=nil then         !assume typeof
            strcat(dest,"typeof(")
            strcat(dest,tn.defa.name)
            strcat(dest,")")
        else
            if tn.defa then
                strcat(dest,tn.defa.name)
                strcat(dest,".")
            fi
            strcat(dest,tn.def.name)
        fi
        return
    fi

    if m<tlast and m<>tref then
        strcpy(dest,typename(m))
        return
    fi

    case mbase:=ttbasetype[m]
    when tref then
        strcpy(dest,"ref ")
        target:=tttarget[m]
        if target>=0 and ttbasetype[target]=trecord then
            strcat(dest,typename(target))
        else
            istrmode(tttarget[m],0,dest+strlen(dest))
        fi

    when tarray then
        if ttdimexpr[m] then
            gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
            fprint @dest,"@[#]",&.strdim
        else
            if ttlength[m] then
                if ttlower[m]=1 then
                    fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
                else
                    fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
                fi
            else
                if ttlower[m]=1 then
                    fprint @dest,"[]"
                else
                    fprint @dest,"[#:]",ttlower[m]
                fi
            fi
        fi
        istrmode(tttarget[m],0,dest+strlen(dest))

    when tslice then
        prefix:=stdnames[mbase]

        if ttdimexpr[m] then
            gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
            fprint @dest,"@#[#:]",prefix,&.strdim
        else
            if ttlower[m]=1 then
                strcpy(dest,prefix)
                strcat(dest,"[]")
            else
                fprint @dest,"#[#:]",prefix,ttlower[m]
            fi
        fi
        istrmode(tttarget[m],0,dest+strlen(dest))

    when trecord then
        if not expand then
            strcpy(dest,typename(m))
            return
        fi
        strcpy(dest,"")
        if expand<>2 then
            strcat(dest,typename(ttbasetype[m]))
        fi
        strcat(dest,"(")
        d:=ttnamedef[m]
        needcomma:=0

        q:=d.deflist

        while q, q:=q.nextdef do
            if needcomma then strcat(dest,",") fi
            needcomma:=1
            istrmode(q.mode,0,dest+strlen(dest))
            strcat(dest," ")
            strcat(dest,q.name)
        od
        strcat(dest,")")

    when tvoid then         !must be a usertype that is not defined (as normal voids checked above)
        strcpy(dest,"void")

    when tuser then
        strcpy(dest,typename(m))
    when tproc then

        d:=ttnamedef[m]

        strcpy(dest,"proc(")
        q:=d.paramlist
        needcomma:=0
        while q<>nil do
            if needcomma then strcat(dest,",") fi
            needcomma:=1
            istrmode(q.mode,0,dest+strlen(dest))
            strcat(dest," ")
            strcat(dest,q.name)
            q:=q.nextdef
        od
        strcat(dest,")")
        if d.mode<>tvoid then
            istrmode(d.mode,0,dest+strlen(dest))
        fi

    when ttuple then
        strcpy(dest,"Tuple(")
        n:=ttlength[m]
        for i to n do
            istrmode(ttmult[m,i],0,dest+strlen(dest))
            if i<n then strcat(dest,",") fi
        od

        strcat(dest,")")

    when tbitfield then
        strcpy(dest,"bitfield")

    elsif ttbasetype[m]<tlast then
        strcpy(dest,"Alias for:")
        istrmode(tttarget[m],0,dest+strlen(dest))

    else
        println typename(m),STRMODE(TTBASETYPE[M])
        mcerror("NEWSTRMODE")
    esac
end

global proc addtoproclist(symbol d)=
    ref procrec pp

    pp:=pcm_alloc(procrec.bytes)

    if proclist=nil then
        proclist:=proclistx:=pp
    else
        proclistx.nextproc:=pp
        proclistx:=pp
    fi
!
    pp.def:=d
end

global proc addstatic(symbol d)=
    ref procrec pp
    pp:=pcm_alloc(procrec.bytes)

    if staticlist=nil then
        staticlist:=staticlistx:=pp
    else
        staticlistx.nextproc:=pp
        staticlistx:=pp
    fi

    pp.def:=d
end

global proc addexpconst(symbol d)=
    ref procrec pp
    pp:=pcm_alloc(procrec.bytes)

    if constlist=nil then
        constlist:=constlistx:=pp
    else
        constlistx.nextproc:=pp
        constlistx:=pp
    fi
    pp.def:=d
end

global function typename(int m)ichar=
    if m>=0 then
        return ttname[m]
    fi
    return typenames[-m].def.name

end

global function allocunitrec:ref unitrec=
    ref unitrec p
    ref int64 q
    int nwords

    ++nunits

    if remainingunits-- then
        p:=unitheapptr
        ++unitheapptr
        p.pos:=lx.pos
        p.moduleno:=currmoduleno
        p.subprogno:=moduletosub[currmoduleno]
        return p
    fi

!need first or new heap
    p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

    memset(p,0,unitheapsize*unitrec.bytes)
    remainingunits:=unitheapsize-1
    ++unitheapptr
    p.pos:=lx.pos

    p.moduleno:=currmoduleno
    p.subprogno:=moduletosub[currmoduleno]
    return p
end

global function createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
    symbol p,q

    p:=newstrec()

    p.name:=symptr.name
    p.namelen:=symptr.namelen
    p.symbol:=namesym
    p.owner:=owner
    p.nameid:=id

    p.nextdupl:=symptr.nextdupl
    symptr.nextdupl:=p

    if owner then
        if owner.deflist=nil then           !first def
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

global function duplunit(unit p,int lineno=0)unit=
    unit q
    if p=nil then return nil fi

    q:=createunit0(p.tag)

    q^:=p^
    q.nextunit:=nil
    for i to jsubs[q.tag] do
        q.abc[i]:=duplunit(q.abc[i])
    od

    return q
end

global function checkblockreturn(unit p)int=
!p is any statement
!check p, or the last statement of a block, or the last statement of any
!nested block, a return, or is a unit yielding some value (as explicit return
!statement not needed)
! return 1/0 for return/not return
    unit e,wt
    int m,res

    if p=nil then return 0 fi

    m:=p.mode

    case p.tag
    when jreturn then           !that's an easy one...
        return 1
    when jstop then
        return 1
    when jif then
        p.ifretflag:=1
        e:=p.b
        while e, e:=e.nextunit do
            if not checkblockreturn(e) then return 0 fi
        od

        return checkblockreturn(p.c)        !all branches must have a return

    when jblock then
        e:=p.a
        if e then
            while e and e.nextunit do
                e:=e.nextunit
            od
            return checkblockreturn(e)
        fi

    when jcase, jswitch, jdocase, jdoswitch then
        p.ifretflag:=1
        wt:=p.b
        while wt do
            if not checkblockreturn(wt.b) then
                return 0
            fi

            wt:=wt.nextunit
        od

        return checkblockreturn(p.c)        !else

    when jassem then                        !assume yes
        return 1
    esac

    if jisexpr[p.tag] and m<>tvoid then
    if ctarget then
        insertunit(p,jreturn)
        p.mode:=m
    fi

        return 1                            !any non-void expr allowed as return value
    else
        return 0
    fi
end

global function isconstunit(unit a)int=
    return a.isconst
end

global proc getownername(symbol d, ichar dest)=
    symbol owner

    owner:=d.owner

    if owner=nil or owner.nameid=programid then return fi
    getownername(owner,dest)
    strcat(dest,owner.name)
    strcat(dest,".")
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
    int a

    case ttbasetype[m]
    when tarray then
        return getalignment(tttarget[m])
    when trecord then
        return 8
    elsif ttisblock[m] then
        return 8
    esac

    a:=ttsize[m]
    case a
    when 1,2,4,8 then
        return a
    when 0 then
        return 8
    esac
    cpl Strmode(m)
    gerror("GETALIGN SIZE NOT 1248")

    return 0
end

global function ispoweroftwo(int64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
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

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
    if ulist=nil then       !first
        ulist:=ulistx:=p
    else
        ulistx.nextunit:=p
    fi
    ulistx:=p           !update end-of-list pointer
end

global function storemode(symbol owner, int m, int32 &pmode)int =
    ref typenamerec r

    if m>=0 then
        pmode:=m
        return m
    fi

    r:=&typenames[-m]

    if r.pmode=nil then
        r.owner:=owner
        pmode:=m
        r.pmode:=&pmode

    IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

        return m
    fi

!Already one instance of this mode; need a new slot
    m:=newtypename(r.defa, r.defb)
    r:=&typenames[-m]

    r.owner:=owner
    pmode:=m
    r.pmode:=&pmode
    return m
end

global function gettypebase(int m)int=
    switch ttbasetype[m]
    when ti8,ti16,ti32 then ti64
!   when tu8,tu16,tu32 then tu64
    when tu8,tu16,tu32 then ti64

    when tr32 then tr64

    when tc8 then tc64
    else
        m
    end switch
end

global proc writegsfile(ichar filename, ref strbuffer d)=
    filehandle f

    f:=fopen(filename,"wb")
    gs_println(d,f)
    fclose(f)
end

global proc addtolog(ichar filename, filehandle logdest)=
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

global function getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
    symbol d
    unit a

    if p.tag<>jcallfn then txerror("multass/need multfn") fi
!   if p.tag not in [jcallfn,jcallproc] then txerror("multass/need multfn") fi
    a:=p.a

    case a.tag
    when jname then
        return a.def
    else
        return ttnamedef[tttarget[a.mode]]
    esac
end

global function getmemmode(unit p)int =
    if p.memmode then
        return p.memmode
    fi
    return p.mode
end

global function getpclmode(int t)int u=
    u:=ttbasetype[t]
    case u
    when tc64 then u:=tu64
    when tc8 then u:=tu8
    when trecord, tarray then
        if not ttisblock[t] then
            case ttsize[t]
            when 8 then u:=tu64
            when 4 then u:=tu32
            when 2 then u:=tu16
            else u:=tu8
            esac
        fi
    esac
    return u
end

global function getfullname(symbol d)ichar=
!create fully qualified name into caller's dest buffer
    static [128]char str
    [16]symbol chain
    int n:=0
    symbol e:=d

    if d.isimport then
        return d.name
    fi

    repeat
        chain[++n]:=e
        e:=e.owner
    until e=nil or e.nameid=programid

    strcpy(str,chain[n].name)
    for i:=n-1 downto 1 do
        strcat(str,".")
        strcat(str,chain[i].name)
    od
!
!   if d.owner and d.owner.nameid<>programid then
!       getfullname(d.owner,dest)
!       strcat(dest,".")
!   fi
!   strcpy(dest,d.name)
    return str
end

global function getbasename(ichar s)ichar t=
    t:=s+strlen(s)-1
    while t>s and (t-1)^<>'.' do
        --t
    od

    return t
end
=== mc_libc.m 0 0 11/33 ===
const dollar="$"

global unit nilunit

global const cbufferlen=4096                !needs to be same size as exprbuffer
[cbufferlen]char clinebuffer
global ref char clineptr, clineend

global proc cccomment(ichar s=nil)=
!comment that is appended to current line
    ccstr("/* ")
    ccstr(s)
    ccstr(" */")
    ccsendline()
end

global proc ccblank=
    ccsendline()
end

global proc cclinecomment(ichar s)=
    cccomment(s)
end

global proc ccchar(int c)=
    clineptr^:=c
    ++clineptr
end

global proc cctab(int level=0)=
    to level*4 do
        clineptr++^:=' '
    od
end

global proc ccstr(ref char s,int level=0)=
    if level then
        cctab(level)
    fi

    while s^ do
        clineptr++^:=s++^
    od
end

global proc ccstrline(ichar cstr)=
    ccstr(cstr)
    ccsendline()
end

global proc ccstrsemi(ichar cstr)=
    ccstr(cstr)
    ccchar(';')
    ccsendline()
end

global proc ccstrsemiu(unit p)=
    evalunit(p)
    ccchar(';')
    ccsendline()
end

global proc ccsendline=
    clineptr^:=0
    gs_strln(dest,&.clinebuffer)
    ccinitline()
end

global proc ccint(int a)=
    ccstr(strint(a))
end

global proc ccinitline=
    clineptr:=&.clinebuffer
    clineend:=clineptr+cbufferlen-1
end

global function strmodec(int m,ichar name="",int addtab=1)ichar=
    static [1024]char str
    ref char oldlineptr

    oldlineptr:=clineptr
    clineptr:=&.str

    strmodec2(m,name,addtab)
    clineptr^:=0

    clineptr:=oldlineptr
    return &.str
end

global proc strmodec2(int m,ichar name="",int addtab=1)=
!convert type p (which is a kunit) into a C-typespec string
!When a name is associated with the type (decl etc), this must be supplied, as a complex
!type will be wrapped around the name
    ichar suffix,sp,spsuffix
    [1024]char str
    [1024]char buffer
    int target

    if name and name^ then
        suffix:=name
        strcpy(&.str," ")
        strcat(&.str,name)
        spsuffix:=&.str
        sp:="  "
        case name^
        when '[','(','*' then
    lab1::
!           sp:=" "
            sp:=""
        else
            if not addtab then goto lab1 fi
        esac
    else
        sp:=suffix:=spsuffix:=""
    fi

    case ttbasetype[m]
    when ti32,tu32,ti64,tu64,tr32,tr64,tvoid then
        ccstr(strmodex(m))
        ccstr(spsuffix)
    when tref then
        case ttbasetype[tttarget[m]]
        when tproc then
            if tttarget[m]=tproc then           !placeholder version
                ccstr("REF PROC")
                ccstr(suffix)
            else
                ccstr(strprocsig(ttnamedef[m],suffix,0))
            fi
        when tlabel then
            ccstr("int *")
            ccstr(spsuffix)
        else
            target:=tttarget[m]
            if ttbasetype[target]=tarray then
    !           sprintf(&.buffer,"(*%s)",suffix)
                fprint @&.buffer,"(*#)",suffix
            else
                fprint @&.buffer,"*##",sp,suffix
            fi
            strmodec2(target,&.buffer)
        esac

    when tarray then
        if ttlength[m] then
    !       sprintf(&.buffer, "%s[%lld]",suffix,ttlength[m])
            fprint @&.buffer, "#[#]",suffix,ttlength[m]
        else
    !       sprintf(&.buffer, "%s[]",suffix)
            print @&.buffer,suffix,,"[]"
        fi
    
        strmodec2(tttarget[m],&.buffer)
    when trecord then
    
        ccstr("struct ")
        ccstr(getfullnamec(ttnamedef[m]))
        ccstr(spsuffix)

    when tslice then
        ccstr("Slice ")
        ccstr(spsuffix)
    else
        ccstr(strmodex(m))
        ccstr(spsuffix)
    esac
end

function strmodex(int m)ichar=
!return friendlier names for standard types
    case ttbasetype[m]
    when ti32 then return "i32"
    when ti64 then return "i64"
    when ti8 then return  "i8"
    when ti16 then return "i16"
    when tu32 then return "u32"
    when tu64 then return "u64"
    when tu8 then return  "byte"
    when tu16 then return "u16"
    when tr32 then return "r32"
    when tr64 then return "r64"
    when tc8 then return "u8"
    when tc64 then return "u64"
    else
        return strmode(m)
    esac
    return ""
end

global function strprocsig(symbol p,ichar name=nil,int showparamnames=1)ichar=
    [512]char paramstr
    [512]char buffer
    symbol pm
    int rettype
    ichar stdcall,scallback

    pm:=p.paramlist         !paramlist
    rettype:=p.mode

    strcpy(&.paramstr,"(")

    if pm then
        while pm do
            strcat(&.paramstr, strmodec(pm.mode,(showparamnames|pm.name|""),0))
            pm:=pm.nextparam
            if pm then
                strcat(&.paramstr,",")
            fi
        od
        if p.varparams then
            if p.paramlist then
                strcat(&.paramstr,",")
            fi
            strcat(&.paramstr,"...")
        fi
        strcat(&.paramstr,")")
    else
        if p.varparams then
            strcat(&.paramstr,"...")
        else
            strcat(&.paramstr,"void)")
        fi
    fi

    stdcall:=""

    if p.fflang=windowsff then
        if targetbits=64 then
    !       stdcall:="__declspec(dllimport) "
        else
            stdcall:="__stdcall "
        fi
    fi

    if p.iscallback then
        scallback:="gcc_callback"
    else
        scallback:=""
    fi

    if name=nil then
        print @&.buffer,stdcall,,getprocname(p),,&.paramstr
    else
        fprint @&.buffer,"#(#*#)#",scallback,stdcall,name,&.paramstr
    fi
    return pcm_copyheapstring(strmodec(rettype,&.buffer,not showparamnames))
end

global function getprocname(symbol d)ichar=
    ichar name

    name:=d.name

    case d.fflang
    when clangff,windowsff then
        if d.truename then return d.truename fi
        return name
    else
!       if eqstring(name,"main") then return "main"
!       elsif eqstring(name,"start") then return "start"
!       else
            return getfullnamec(d)
!       fi
    esac
    return ""
end

global function getfullnamec(symbol d)ichar=
    getfullnamec2(d)
end

global function getfullnamec2(symbol d)ichar=
    static [256]char str
    [256]char str2
    int n

    case d.nameid
    when procid, staticid, typeid, constid then
        case d.owner.nameid
        when moduleid then
            strcpy(&.str,d.owner.name)
            strcat(&.str,dollar)
            strcat(&.str,d.name)
            return &.str
        when procid then
            if d.nameid=typeid then
                strcpy(&.str,d.owner.owner.name)
                strcat(&.str,dollar)
                strcat(&.str,d.owner.name)
                strcat(&.str,dollar)
                strcat(&.str,d.name)
                return &.str
            fi
        esac
    esac

    if d.truename then return d.truename fi
    return d.name
end

global function genclabel(int n,colon=0)ichar=
    static [16]char str
    print @&.str,"L",,n,(colon|":;"|"")
    return &.str
end

global proc genrecorddef(int m)=
!export record or struct
    static int seqno=0
    symbol q
    int indent,index,nref,target
    [8]byte flags
    byte f
    ichar name
    symbol d

    d:=ttnamedef[m]
    name:=d.name

    if d.align then
        ccstrline("#pragma pack(8)")
    fi

    ccstr("struct ")
    ccstr(getfullnamec(d))
    ccstrline(" {")

    indent:=1

    q:=d.deflist

    while q do
        memcpy(&flags,&q.uflags,8)
        index:=1
        flags[8]:=0
    
        case q.nameid
        when fieldid then
            while (f:=flags[index])='S' or f='U' do
                ++index
                cctab(indent)
                ccstrline((f='S'|"struct {"|"union {"))
                ++indent
            od
    
            case ttbasetype[q.mode]
            when tref then
                target:=tttarget[q.mode]
                nref:=1
                while ttbasetype[target]=tref do
                    target:=tttarget[target]
                    ++nref
                od
                if target=m then
                    cctab(indent)
                    ccstr("struct ")
    !               ccstr(name)
                    ccstr(getfullnamec(d))
                    to nref do ccchar('*') od
                    ccchar(' ')
                    ccstrsemi(q.name)
    
                else
                    goto normal
                fi
            when tbitfield then

            else
    normal::
                cctab(indent)
                ccstrsemi(strmodec(q.mode,q.name))
            esac

            if flags[index]='*' then ++index fi
            while flags[index]='E' do
                --indent
                ++index
                cctab(indent)
                ccstrsemi("}")
            od
        else
            gerror("Non-field in struct")
        esac
        q:=q.nextdef
    od

    ccstrsemi("}")
    if d.align then
        ccstrline("#pragma pack(1)")
    fi
    ccblank()
end

global proc genrecordfwd(int m)=
    ccstr("struct ")
    ccstrsemi(getfullnamec(ttnamedef[m]))
end

global proc do_initdata(unit p,int docomma=0,level=0)=
!output initialisation data for a var
!p is the .code value
!dosemi=1 to write a final semicolon and end the line

    if p=nil then return fi

    if p.tag<>jmakelist then
        if p.tag=jconst and p.mode=trefchar and p.isastring then
    
            if p.slength=0 then
                ccstr("(byte*)""""")
            else
                cclongstr(p.svalue,p.slength)
            fi
        else
            evalunit(p)
        fi
        if docomma then
            ccchar(',')
        fi
        return
    fi

!makelist is a special case
    do_makelist(p.a,p.length,docomma,level+1)
end

global proc cclongstr(ref char svalue, int length)=
!svalue is a raw string that needs conversion to C escape codes, so might get longer
    if clineptr+length*2>=clineend then     !might overflow buffer
        ccsendline()
        gs_str(dest,strstringc(svalue,length))
    else
        dxstr(strstringc(svalue,length))
    fi
end

proc do_makelist(unit a, int length, docomma,level=0)=
    int oneperline,n
    unit p,q

    p:=a

    if length<=10 and a.tag<>jmakelist then             !keep on one line
        ccstr("{")
        while p do
            q:=p.nextunit
            do_initdata(p, q<>nil, level)
            p:=q
        od
    else
        ccstrline("{")
        while p do
            q:=p.nextunit
            to level do
                ccstr("    ")
            od
            do_initdata(p, q<>nil, level)
            p:=q
            ccstrline("")
        od
    fi

    ccchar('}')

    if docomma then
        ccchar(',')
    fi
end

global function issimplec(unit p)int=
!return 1 if unit p is simple
!this is used for min/max etc where I need to know if a term has side effects
!This is complex, so just do simplest possible check
    case p.tag
    when jconst, jname then
        return 1
    esac
    return 0
end

global function strstringc(ichar s, int length)ichar=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
    int i, state, c, a,col

    const maxstrlen=512
    static [maxstrlen*2]char str
    ref char dest, t

    if length>maxstrlen then
        dest:=pcm_alloc(length*2)
    else
        dest:=&.str
    fi
    t:=dest

    strcpy(t,"(byte*)")
    t+:=strlen(t)

    t++^:='"'

    while c:=s++^ do
        case c
        when '"' then t++^:='\\'; t++^:='\"'
        when 10 then t++^:='\\'; t++^:='n'
        when 13 then t++^:='\\'; t++^:='r'
        when 9 then t++^:='\\'; t++^:='t'
        when '\\' then t++^:='\\'; t++^:='\\'
        else
            if c<32 then
                t++^:='\\'
                t++^:=c>>6+'0'
                t++^:=(c>>3 iand 7)+'0'
                t++^:=(c iand 7)+'0'
            else
                t++^:=c
            fi
        esac
    od
    t++^:='"'
    t^:=0

    return dest
end

global proc do_syscallproc(ichar fnname,unit a=nil,b=nil)=
    [32]char str

    if msyslevel=1 and strlen(fnname)>=6 and eqbytes(fnname,"msysc$",6) then
        strcpy(str,"mminc")
        strcat(str,fnname+5)
        fnname:=str
    fi

    dxstr(fnname)
    dxstr("(")
    if a then
        evalsysparam(a)
        if b then
            dxstr(",")
            evalsysparam(b)
        fi
    fi
    dxstr(");")
    ccsendline()
    cctab(blocklevel)
end

proc evalsysparam(unit a)=
    case a
    when nil then
    when nilunit then
        dxstr("NULL")
!!  when zerounit then
!!      dxstr("0")
    else
        evalunit(a)
    esac
end

global proc dxstr(ichar str)=
    while str^ do
        clineptr^:=str^
        ++clineptr
        ++str
    od
end

global proc dxchar(int ch)=
    clineptr++^:=ch
end

global proc dxint(int a)=
    dxstr(strint(a))
end

=== mm_libsourcesc.m 0 0 12/33 ===
const fsyslibs = 1

global tabledata []ichar syslibnames, []ichar libtext =
    ("msysc.m",         strinclude "msysc.m"),
    ("mlib.m",          strinclude "mlib.m"),
    ("mclib.m",         strinclude "mclib.m"),
    ("mlinux.m",        strinclude "mlinux.m"),
    ("mwindllc.m",      strinclude "mwindllc.m"),
end

[syslibnames.len]byte syslibfileno

global function findsyslib(ichar filename)int=
!filename must be module name with .q extension
!return sourcefile no

    if not fsyslibs then return 0 fi

    filename:=extractfile(filename)     !remove any path

    for i to syslibnames.len do
        if eqstring(syslibnames[i],filename) then
            if syslibfileno[i] then
                return syslibfileno[i]
            fi

!add to sourcefiles
            if nsourcefiles>=maxsourcefile then loaderror("fsl: too many files") fi
            ++nsourcefiles
            sourcefilenames[nsourcefiles]:=pcm_copyheapstring(filename)

!           sourcefiletext[nsourcefiles]:=libtext[i]

            sourcefiletext[nsourcefiles]:=pcm_copyheapstring(libtext[i])
            if fwritema then
                sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(libtext[i])
            fi
            sourcefilesizes[nsourcefiles]:=strlen(libtext[i])
!           sourcefilelocs[nsourcefiles]:='PACK'
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
=== mm_modules.m 0 0 13/33 ===
ichar headerpathx   = ""
ichar altpathx      = ""
ichar importpathx   = ""
ichar subprogpath   = ""
int dirpos
int issyslib

[headervarnames.len]ichar headervars

macro mainmodule=headervars[hv_mainmodule]

global proc readprojectfile(ichar filename)=
    int fileno,headerdir, dir, oldsyslib, found
    ichar basefile, extension

!   println "READPROJECT",filename

    extension:=convlcstring(extractext(filename))

    found:=checkfile(filename)
    if not found and not eqstring(extension, langextma) then
        filename:=pcm_copyheapstring(changeext(filename,langextma))
        found:=checkfile(filename)
        if found then
            fprintln "(Building #)",filename
            extension:=langextma
        fi
    fi  

    if not found then
        loaderror("Can't find main module or project: ##",filename)
    fi

    if eqstring(extension,langextma) then
        filename:=loadmafile(filename)
    fi

    fileno:=getsupportfile(filename)

    basefile:=extractbasefile(sourcefilenames[fileno])
    projectmodule:=sourcefilespecs[fileno]

    initheadervars()

    headermode:=1
    headerdir:=0

    moduletable[0].name:="PROGRAM"
    moduletable[0].fileno:=0

    stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
    moduletable[0].stmodule:=stprogram
!
    addfirstsubprogram(basefile,fileno)

    startlex(fileno)

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
                if lx.symbol=namesym and eqstring(lx.symptr.name,langlibname) then
!                   recase hdr_sysimport
                    $hdr_sysimport
                fi
                issyslib:=0
                altpathx:=""
                readimport()
            when hdr_minclude then
                readinclude()
            when hdr_sysimport then
$hdr_sysimport::
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

            when hdr_linkdll then
                addlib(readvar(),'D')
            when hdr_linklib then
                addlib(readvar(),'L')
            else
                loaderror("Hdr directive not ready:##",headerdirnames[dir])
            esac

            checksymbol(semisym)

        when semisym then
        when eofsym then
            exit
        else
            if sourcelevel and lximport then
                setmixedimport()
                unstacksource()
            else
                projectmodule:=nil
                setmixedprogram(basefile)
                exit
            fi
        esac
    od


    if nmodules=0 then
        loaderror("No modules specified")
    fi

    addsyslib()

    addlib("msvcrt",'D')
    addlib("user32",'D')
    addlib("gdi32",'D')
    addlib("kernel32",'D')
end

proc initheadervars=
    for i to headervars.len do
        headervars[i]:=""
    od

    headervars[hv_devpath]:=langhomedir
    headervars[hv_mmpath]:=pcm_copyheapstring(extractpath(sysparams[1]))
    subprogpath:=headerpathx:=headervars[hv_hdrpath]:=pcm_copyheapstring(sourcefilepaths[1])
    if fwindows then headervars[hv_windows]:="1" fi
    mainmodule:="1"
    if ctarget then headervars[hv_ctarget]:="1" fi

end

proc readmoduledir=
!at symbol following 'module'
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
!   Add new module with given name (should be on the heap)
    ref modulerec pm
    ref subprogrec ps

    for i to nmodules do
        if eqstring(moduletable[i].name, modulename) then
            loaderror("Duplicate module name: # (Line:#)",modulename,strint(getlineno(dirpos)))
        fi
    od

    for i to nsubprogs do
        if eqstring(subprogtable[i].name, modulename) then
!           loaderror("Clashing subprog/module name: # (Line:#)",modulename,strint(dirline))
            loaderror("Clashing subprog/module name: # (Line:#)",modulename,strint(getlineno(dirpos)))
        fi
    od

    if nmodules>=maxmodule then
        loaderror("Too many modules",modulename)
    fi
    pm:=&moduletable[++nmodules]

    pm.name:=pcm_copyheapstring(modulename)
    pm.subprogno:=nsubprogs

    pm.stmodule:=stmodule:=createnewmoduledef(stprogram,addnamestr(modulename))

    pm.path:=(altpathx^|altpathx|subprogpath)
    pm.issyslib:=issyslib

    stmodule.moduleno:=nmodules
    stmodule.subprogno:=nsubprogs
    moduletosub[nmodules]:=nsubprogs

    ps:=&subprogtable[nsubprogs]

    if ps.firstmodule=0 then
        ps.firstmodule:=nmodules
    fi

    if stalias then

        pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
        adddef(stprogram, pm.stmacro)
        pm.stmacro.paramlist:=nil
        pm.stmacro.code:=createname(stmodule)
    fi
end

proc addsubprogram(ichar subprogname,int fileno)=
!   Add new subprogram with given name (should be on the heap)
    ref subprogrec ps

    if nsubprogs>=maxsubprog then
        loaderror("Too many subprograms",subprogname)
    fi

    for i to nsubprogs do
        if eqstring(subprogtable[i].name, subprogname) then
            loaderror("Duplicate subprog name: # (Line:#)",subprogname,strint(getlineno(dirpos)))
        fi
    od
    ps:=&subprogtable[++nsubprogs]

    ps.name:=pcm_copyheapstring(subprogname)

    subprogpath:=ps.path:=(importpathx^|importpathx|subprogpath)

    stsubprog:=createnewmoduledef(stprogram,addnamestr(subprogname),subprogid)
    stsubprog.subprogno:=nsubprogs
    ps.stsubprog:=stsubprog
!   stsubprog.subprogno:=nsubprogs
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
!   stsubprog.subprogno:=nsubprogs
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

    fileno:=getsupportfile(subprogname,langext, path)
    addsubprogram(subprogname,fileno)

    stacksource(fileno)
end

proc readinclude=
    ichar name, path
    int fileno

    checksymbol(stringconstsym)
    name:=pcm_copyheapstring(lx.svalue)

    lex()

    fileno:=getsupportfile(name,langext, "")
    stacksource(fileno)
end

function readvar:ichar s=
    case lx.symbol
    when stringconstsym then
        s:=pcm_copyheapstring(lx.svalue)
    when kheadervarsym then
        s:=headervars[lx.subcode]
    when namesym then
        s:=lx.symptr.name
    else
        loaderror("readvar/bad expr")
        s:="?"
    esac
    lex()
    return s
end

function fixpath(ichar path)ichar=
    [300]char newpath
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
    [100]char name
    int oldns
!   loaderror("SIMPLE PROG SETUP NOT DONE")

    print @name,"$",,basefile
    oldns:=nsubprogs
    nsubprogs:=1
    addmodule(name)
    nsubprogs:=oldns
    moduletable[nmodules].fileno:=1
    mainmoduleno:=subprogtable[1].firstmodule:=nmodules
end

proc setmixedimport=
    [100]char name
!   loaderror("SIMPLE PROG SETUP NOT DONE")

    print @name,"$",,subprogtable[nsubprogs].name
    addmodule(name)
    moduletable[nmodules].fileno:=subprogtable[nsubprogs].fileno
    subprogtable[nsubprogs].firstmodule:=nmodules
end

global proc loadmodules =
    ref modulerec pm

    for i to nmodules do
        pm:=&moduletable[i]
        loadmodule(pm)
    od
end

proc loadmodule(ref modulerec pm)=
    [300]char filespec
    ichar path

    if pm.fileno then
!       println pm.name,"Already loaded"
        return
    fi

    path:=pm.path
    if path^=0 and pm.issyslib then
        path:=f"c:\mx\"
    fi

    pm.fileno:=getsupportfile(pm.name, langext, path, issyslib:pm.issyslib)
end

proc addsyslib=
!add in syslib if mlib not already included

    if msyslevel=0 then return fi

    for i to nsubprogs do
        if eqstring(subprogtable[i].name,langname+"libx") then return fi
    od

    issyslib:=1
    importpathx:=headervars[hv_devpath]
    altpathx:=""
    if msyslevel=1 then
        addsubprogram("mlibmin",0)
        addmodule((ctarget|"mminc"|"mmin"))

        return
    fi

    addsubprogram("mlibx",0)

    if ctarget then

        addmodule("msysc")
        addmodule("mlib")
        addmodule("mclib")
        if flinux then
            addmodule("mlinux")
            addmodule("mwindllc")
        else
            addmodule("mwindows")
            addmodule("mwindllc")
        fi

    else
        addmodule("msys")
        addmodule("mlib")
        addmodule("mclib")
        addmodule("mwindows")
        addmodule("mwindll")
    fi

end

global proc addlib(ichar libname, int libtype='D')=
    for i to nlibfiles do
        if eqstring(libfiles[i],libname) then return fi
    od
    if nlibfiles>=maxlibfile then
        loaderror("Too many libs")
    fi
    libfiles[++nlibfiles]:=libname
    libtypes[nlibfiles]:=libtype
end

function readfileline(ichar s)ichar =
!s points into a string representing an entire file, which should be 0-terminated
!read the line s until the next eol or eof, into a line buffer
!return a pointer to the next line
    [2048]char str
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
!starting from s, find next sequence of lf followed by ===
!return nil if eof found
!otherwise return pointer to just after the ===
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

function loadmafile(ichar filespec, ichar builtinstr=nil)ichar=
!load ma file from disk
!unless filespec is nil, then direct from builtinstr
!return name of lead module
    ichar s,t
    [100]char name
    [300]char newfilespec
    int sys,support

    freadma:=1

    if filespec then
        s:=cast(readfile(filespec))
        if s=nil then                           !file not found on disk
            loaderror("Can't find MA file ##",filespec)
        fi
        strcpy(newfilespec,extractpath(filespec))
    else
        s:=builtinstr
        newfilespec[1]:=0
    fi

!need to scan file pickuping the file headers, and populating sourctables

    s:=readfileline(s+3)
    readstr(name,'n')
    if not eqstring(name,langextma) then
        loaderror(langextmauc+": bad header")
    fi

    --s                 !point to previous lf

    if nsourcefiles then
        loaderror(langextmauc+"/table not empty")
    fi

    s:=findnextlineheader(s)

    do
        if s=nil then
            loaderror("Unexpected EOF in "+langextmauc+" file")
            exit
        fi
        s:=readfileline(s)

        readstr(name,'n')
        read sys,support

!       println "Found file",name
        if eqstring(name,"end") then
            exit
        fi
        if nsourcefiles>=maxsourcefile then
            loaderror("Too many files in "+langextmauc)
        fi

        t:=findnextlineheader(s)
        if t=nil then
            loaderror("MA error")
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
!
    for i to nsourcefiles do
        (sourcefiletext[i]+sourcefilesizes[i])^:=0  
    od

!finally, set inputfile to the first file found
    strcat(newfilespec, sourcefilenames[1])
    return pcm_copyheapstring(newfilespec)
end

=== mm_name.m 0 0 14/33 ===
symbol currstproc
int allowmodname=0
int noexpand, noassem
int macrolevels

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_typetable=
    symbol d

    for i:=tuser to ntypes do
        if ttbasetype[i]=trecord then
            d:=ttnamedef[i]
            if d.baseclass then
                do_baseclass(d)
            fi
        fi
    od
end

global proc rx_unit(symbol owner, unit p)=
    symbol d
    unit a,b
    int n,oldnoexpand,oldnoassem,oldtag,useparams

    a:=p.a
    b:=p.b
    mlineno:=p.pos

    switch p.tag
    when jname then
        resolvename(owner,p)
        if P.TAG=JNAME AND p.def.nameid=macroid and not noexpand then
            ++macrolevels
            expandmacro(p,p,nil)
            rx_unit(owner,p)
            --macrolevels
        fi

    when jkeyword then
        rx_unit(owner,b)        !do param value only

    when jdot then
!       if b.tag=jname then
!           d:=resolvetopname(owner,b.def,b.moduleno,fmodule:0,fdoambig:0)
!       fi
        resolvedot(owner,p)

    when jcallproc, jcallfn then
        oldtag:=p.tag

        if a.tag=jname then         !can expand possible macro if params not ready
            oldnoexpand:=noexpand; noexpand:=1
            rx_unit(owner,a)
            noexpand:=oldnoexpand
        else
            rx_unit(owner,a)
        fi

        rx_unitlist(owner,b)

        if a.tag=jname then
            d:=a.def
            case d.nameid
            when typeid then        !change to type conversion
                p.tag:=jconvert
                storemode(owner,d.mode,p.convmode)
                p.a:=b
                if b.nextunit then
                    p.a:=createunit1(jmakelist,b)
                    n:=0
                    while b do
                        ++n
                        b:=b.nextunit
                    od
                    p.a.length:=n
                fi
            when macroid then
                ++macrolevels
                if d.deflist then           !macro uses params
                    expandmacro(p,a,b)
                    b:=nil
                    useparams:=0
                else                        !macro has no params
                    expandmacro(p,a,nil)
                    useparams:=1
                fi

                rx_unit(owner,p)
                --macrolevels

                if useparams and p.tag not in [jcallproc, jcallfn] then
                    insertunit(p,oldtag)
                    p.b:=b                  !note b may be nil
                FI

            else
                if d.mode=tvoid then
                    p.tag:=jcallproc
                fi
            esac
        fi

    when jandl, jorl then
        rx_unit(owner,a)
        rx_unit(owner,b)
        if not isbooltag[a.tag] then insertunit(a,jistruel); a.pclop:=kistruel fi
        if not isbooltag[b.tag] then insertunit(b,jistruel); b.pclop:=kistruel fi

    when jistruel then
    doistruel::
        rx_unit(owner,a)

        if isbooltag[a.tag] then
            deleteunit(p,a)
        fi
        goto doabc

    when jnotl then
        rx_unit(owner,a)
        if a.tag=jnotl then
            deleteunit(p,a)
            p.tag:=jistruel
            p.pclop:=kistruel
            a:=p.a
            goto doistruel
        fi
        if not isbooltag[a.tag] then
            insertunit(a,jistruel); a.pclop:=kistruel
            a:=p.a
        fi
        goto doabc

    when jassemmacro then
        resolvename(owner,a)
        if not noexpand then
            ++macrolevels
            oldnoassem:=noassem
            noassem:=1
            expandmacro(p,a,b)
            noassem:=oldnoassem
            rx_unit(owner,p)
            --macrolevels
        fi

    else
doabc::
        for i to jsubs[p.tag] do
            rx_unitlist(owner,p.abc[i])
        od
    endswitch
end

global function rx_module(int n)int=
    modulerec m
    symbol stmodule, d
    int globalflag,status

    currmoduleno:=n

    rx_passdef(stprogram,moduletable[n].stmodule)

    return 1
end

global proc rx_deflist(symbol owner,p)=
    symbol pstart:=p
    while p do
        rx_passdef(owner,p)
        p:=p.nextdef
    od
end

global proc rx_passdef(symbol owner,p)=
    symbol d

    case p.nameid
    when moduleid,dllmoduleid then
        rx_deflist(p,p.deflist)

    when procid then
        rx_deflist(p,p.deflist)
        currstproc:=p
        rx_unit(p,p.code)
        currstproc:=nil

    when dllprocid then
        rx_deflist(p,p.deflist)

    when constid,staticid,frameid,paramid then
        if p.atvar then
            rx_unit(owner,p.equivvar)
        fi
        if p.code then
            rx_unit(owner,p.code)
        fi
    when typeid then
        rx_deflist(p,p.deflist)

    else
    esac
end

proc rx_unitlist(symbol owner, unit p)=
    while p do
        rx_unit(owner,p)
        p:=p.nextunit
    od
end

global function resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol =
!stnewname points to a symrec with generic nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)

!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match

!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

    int extcount, subprogno
    symbol p,q, powner,extdef,moddef
    [10]symbol ambiglist

    if owner.nameid=procid then
        q:=owner.deflist
        while q, q:=q.nextdef do
            if q.firstdupl=stnewname then       !immediate match
                return q
            fi
        od
    fi

    p:=stnewname.nextdupl
    subprogno:=moduletosub[moduleno]

    extcount:=0
    extdef:=moddef:=nil

    while p, p:=p.nextdupl do                       !p is next candidate
        powner:=p.owner                             !the owner of that entry

        switch powner.nameid
        when moduleid then                          !candidate is file-scope item
            if powner.moduleno=moduleno then        !same module
                return p
            elsif p.scope then  !matches an external module

!               if moduletosub[powner.moduleno]=subprogno or        !within same subprog
                if powner.subprogno=subprogno or        !within same subprog
!                    p.scope=export_scope or
                     p.scope=program_scope or
                     p.isimport then                !visible outside subprog
                    ++extcount          !if an ext match is closest, there can only be one
                    extdef:=p
                    if extcount<ambiglist.len then
                        ambiglist[extcount]:=extdef
                    fi
                fi
            fi

        when typeid then                    !only for code inside a record def
            if powner=owner or powner=owner.owner then      !immediate match
                return p                    !looks at 2 nested record levels only
            fi

        when programid then                 !p is a module
            case p.nameid
            when moduleid, subprogid then   !match a module/subprog name
                moddef:=p
            when macroid then
                return p

            esac

        endswitch
    od

    if allowmod and moddef then
        return moddef
    fi

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
    return nil
!   return moddef               !return nil, or any matching module/subprog
end

global proc resolvename(symbol owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

    symbol d,e
    unit q
    int moduleno, mode,islet

    d:=p.def
    moduleno:=p.moduleno

    if d.nameid<>nullid then            !assume already resolved
        return
    fi

    e:=resolvetopname(owner,d,moduleno,allowmodname)

!PRINTLN "RESOLVENAME",=E

    if not e then
        islet:=0
        mode:=tvoid
        case p.avcode
        when 'I', 'T', 'S' then mode:=ti64; islet:=1
        when 'L','A' then mode:=tany
!       elsif owner.nameid=procid and owner.isqproc then
!           mode:=tvariant
        esac

        if mode=tvoid then
ARRAY[300]CHAR STR
STRCPY(STR, D.NAME)
CONVUCSTRING(STR)
!           rxerror_s("pcl:Undefined: `#`",d.name,p)
            rxerror_s("pcl:Undefined: #",STR,p)
        else
            e:=addframevar(owner,d,moduleno,mode)
            e.pos:=p.pos
            e.islet:=islet
        fi
    fi

    e.used:=1

    p.def:=e
end

global function finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

    if pdupl.nameid<>nullid then        !assume already resolved
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

global function finddupl_sub(symbol d, pdupl)symbol=
!version of finddupl where d is a subprog
    int subprogno

    if pdupl.nameid<>nullid then        !assume already resolved
        return pdupl
    fi
    pdupl:=pdupl.nextdupl
    subprogno:=d.subprogno

    while pdupl do
!       if moduletosub[pdupl.owner.moduleno]=subprogno then
        if pdupl.owner.subprogno=subprogno then
            return pdupl
        fi
        pdupl:=pdupl.nextdupl
    od
    return nil
end

proc resolvedot(symbol owner,unit p)=
    unit lhs,rhs
    symbol d,e,t,f
    int m,moduleno,subprogno,oldallowmod

    moduleno:=p.moduleno
    subprogno:=p.subprogno
    lhs:=p.a
    rhs:=p.b
    e:=rhs.def              !p.b will be a name type (could perhaps be stored as p.def)

    oldallowmod:=allowmodname
    allowmodname:=lhs.tag=jname
    rx_unit(owner,lhs)
    allowmodname:=oldallowmod

!   rx_unit(owner,lhs)

    case lhs.tag
    when jname then
        d:=lhs.def
        case d.nameid
        when moduleid, typeid, procid, typeid then

            e:=finddupl(d,e)
            if e then
                if d.nameid=moduleid then
!                   if moduletosub[e.moduleno]<>subprogno then
                    if e.subprogno<>subprogno then
                        if e.scope<program_scope then
                            rxerror_s("Need export to import '#'",e.name)
                        fi
                    elsif e.moduleno<>moduleno then
                        if not e.scope then
                            rxerror_s("Need global to import '#'",e.name)
                        fi
                    fi
                fi
domodule::
                p.tag:=jname            !convert to dot to name
                p.a:=p.b:=nil
                p.def:=e
                case e.nameid
                when enumid then
                when constid then
                esac
            else
                rxerror_s("Can't resolve .#",p.b.def.name,p)
            fi

        when frameid, staticid, paramid then        !.x applied to normal var
            m:=d.mode
            case ttbasetype[m]
            when trecord then
            when tref then
                do
                    m:=tttarget[m]
                    case ttbasetype[m]
                    when trecord then
                        exit
                    when tref then
                    else
                        rxerror("2:Record expected")
                    esac
                od
            else
                rxerror("Record expected")
            esac
            t:=ttnamedef[m]

            e:=finddupl(t,e)
            if e then
                p.b.def:=e
            else
                rxerror_s("Not a field: #",rhs.def.name)
            fi
        when subprogid then
            e:=finddupl_sub(d,e)
            if e then
!               if moduletosub[e.moduleno]<>subprogno then
                if e.subprogno<>subprogno then
                    if e.scope<program_scope then
                        rxerror_s("Need export to import '#'",e.name)
                    fi
                fi
                goto domodule
            else
                rxerror_s("Can't resolve sub.#",p.b.def.name,p)
            fi

        esac

    else
!Can't fully resolve at this time; leave to next pass
        unless e.nextdupl then
            rxerror_s("Not a field: #",e.name)
        end unless
    esac
end

proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
    ref int32 pmode
    symbol a,d,e,f,owner
    int m,moduleno

    pmode:=p.pmode

    m:=-pmode^                  !typename index

    d:=owner:=p.owner
    while d.nameid<>moduleid do d:=d.owner od
    moduleno:=d.moduleno

    a:=p.defa
    d:=p.defb

    if a=nil and d then         !simple type name V
        e:=resolvetopname(owner,d,moduleno,0)

    elsif d=nil and a then      !typeno
        rxerror("Fixmode can't do typeof yet")
    else                        !assume a.d type reference
        e:=resolvetopname(owner,a,moduleno,0)
        if e then
            f:=e.deflist
            e:=nil
            while f do
                if f.nameid=typeid and f.firstdupl=d then
                    e:=f
                    exit
                fi
                f:=f.nextdef
            od

        fi

    fi

    if e and e.nameid=typeid then
        pmode^:=e.mode

    else
        rxerror_s("2:Can't resolve tentative type: #",d.name)
    fi
end

global proc fixusertypes=
    ref typenamerec p
    int npasses,notresolved,m,zerosizes
    symbol d

!PRINTLN "FIXUSERTYPES"

    npasses:=0
    repeat
        ++npasses
        notresolved:=0

        for i to ntypenames do
            p:=&typenames[i]

            if p.pmode^<0 then
                mlineno:=typenamepos[i].pos
                fixmode(p)
                if p.pmode^<0 then
                    ++notresolved
                fi
            fi
        od

        if npasses>5 then
            println "Type phase errors - check these user types:"

            for i to ntypenames do
                p:=&typenames[i]

                if p.pmode^<0 then
                    d:=p.defb
                    if d=nil then d:=p.defa fi
                    println "   ",d.name
                fi
            od

            rxerror("Fixtypes: too many passes (cyclic ref?)")
        fi

    until notresolved=0
end

function addframevar(symbol owner, d, int moduleno, mode)symbol=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
    symbol e
    e:=getduplnameptr(owner,d,frameid)
    storemode(owner,mode,e.mode)
    adddef(owner,e)
    return e
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

!need to quickly check if a name unit is a macroparam

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
    for i to jsubs[q.tag] do
        q.abc[i]:=copylistunit(q.abc[i])
    od

    return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
    unit pnext
    pnext:=p.nextunit
    p^:=q^
    p.nextunit:=pnext
end

proc expandmacro(unit p, a, b)=
!a is a macro name unit, b is a macro parameter list (rx-processed), which
!can be nil
!p is either the call-unit as this may originally have been, or the same as a::
!M => (NAME)
!M(A,B) => (CALL NAME (A,B))
!Need to replace M or M(A,B) with the duplicated AST from a.code.
!When a name appears in the AST which is a local macroparam name, then that is
!replaced by the corresponding argument from B;
!The new code replaces P (either CALL or NAME)
!Supplied args may be more than macro takes (error) or may be less (error,
!or allow default arg values to be specified)
    symbol d,pm
    unit pnew
    int ignoreargs

    if macrolevels>10 then
        rxerror("Too many macro levels (recursive macro?)")
    fi

    d:=a.def

!First step: get list of macro formal parameters

    pm:=d.paramlist
    nmacroparams:=0
    while pm do
        if nmacroparams>=maxmacroparams then
            rxerror("macro param overflow")
        fi
        macroparams[++nmacroparams]:=pm
!       macroparamsgen[nmacroparams]:=pm.nulldef
        macroparamsgen[nmacroparams]:=pm.firstdupl

        pm:=pm.nextparam
    od

!now get macro args into a list
    nmacroargs:=0

    while b do
        if nmacroargs>=maxmacroparams then
            rxerror("macro arg overflow")
        fi
        macroargs[++nmacroargs]:=b
        b:=b.nextunit
    od

    if nmacroargs<nmacroparams then
        PRINTLN =NMACROARGS, NMACROPARAMS
        rxerror("Too few macro args")
    fi

    ignoreargs:=0
    if nmacroargs>0 and nmacroparams=0 then     !ignore extra params
        ignoreargs:=1
        nmacroargs:=nmacroparams:=0

    elsif nmacroargs>nmacroparams then
        rxerror("Too many macro args")
    fi

    pnew:=copyunit(d.code)

    if not ignoreargs then              !normal expansion
        replaceunit(p,pnew)
    else                                !keep call and paramlist; just replace fn name
        p.a:=pnew                       !with expansion
    fi
end

proc duplfield(symbol owner,p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

    if p.code then
        serror("DUPLFIELD")
    fi

!Need to copy whatever are relevant attributes

    q.atfield:=p.atfield
    q.flags:=p.flags

    q.uflags:=p.uflags      !for .uflags
    storemode(owner,p.mode,q.mode)
end

proc do_baseclass(symbol p)=
!p is class name, which has a baseclass, do the copying necessary for
!inheriting fields
    symbol d,e,newd,dbase
    int normalexit

    dbase:=ttnamedef[p.baseclass]
    d:=dbase.deflist

    while d do              !for each element of base class
        e:=p.deflist

        normalexit:=1
        while e do          !for each element of new class
            if eqstring(d.name,e.name) then
                normalexit:=0
                exit
            fi
            e:=e.nextdef
        od
        if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
            case d.nameid
            when procid,linkid then
                newd:=getduplnameptr(p,d,linkid)
                newd.equivfield:=d
            else
                newd:=getduplnameptr(p,d,d.nameid)
                duplfield(p.owner,d,newd)
            esac
            adddef(p,newd)
        fi
        d:=d.nextdef
    od
end
=== mm_parse.m 0 0 15/33 ===
!M Language Parser

int intabledata=0       !1 means reading table data line; $ gives tabledataname
int inreadprint=0
int inparamlist=0
int inrecordbody=0
int inimportmodule=0
int labelseen=0
ichar tabledataname=nil

const maxprocstack=10
[maxprocstack]ref strec procstack
int nprocstack=0

uflagsrec unionstring, unionpend
ref strec unionlastvar=nil
ref strec dretvar           !part of read-proc: nil, or symptr of retval variable

int try_level=0
int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack        !used for a[$]
int ndollar=0
int insiderecord=0
int insidedllimport=0

const maxforloops=10
[maxforloops]ref strec forindexvars
int nforloops

global filehandle docfile

global function parsemodule(int n)int=
    ref modulerec pm
    ref strec owner
    unit p

    initparser()

    pm:=&moduletable[n]
    currmoduleno:=n

    stmodule:=pm.stmodule

    currproc:=stmodule

    stsubprog:=subprogtable[stmodule.moduleno].stsubprog
    currsubprog:=stsubprog

    startlex(pm.fileno)

    owner:=stmodule

    lex()

    pm.modulecode:=readmoduledefs(owner)

    return 1
end

global function readmoduledefs(ref strec owner)unit =
!first symbol has been read
    ref strec dimport,stimport
    int globalflag,i,callbackflag
    unit ulist,ulistx,p
    ichar name

    globalflag:=module_scope
    callbackflag:=0
    ulist:=ulistx:=nil

    do
        switch lx.symbol
        when kglobalsym then
            if globalflag then serror("global global?") fi
            globalflag:=lx.subcode

            if globalflag=export_scope and stmodule.subprogno<>1 then
                globalflag:=program_scope
            fi

            lex()

        when kprocsym,kfunctionsym then !todo
            readprocdef(owner,globalflag,callbackflag)
            callbackflag:=0
            globalflag:=module_scope

        when stdtypesym,krefsym,kicharsym,ktypeofsym,lsqsym,
            kdictsym,kslicesym then
dovar::
            readvardef(owner,globalflag,0,staticid, 0)
            globalflag:=module_scope

        when kmutsym then
            lex()
            readvardef(owner,globalflag,0,staticid,kmutsym)
            globalflag:=module_scope

        when kletsym then
            lex()
            readvardef(owner,globalflag,0,staticid,kletsym)
            globalflag:=module_scope

        when karraysym then
            lexchecksymbol(lsqsym)
            goto dovar

        when kimportmodulesym then
            readimportmodule(owner)

        when ktypesym then
            readtypedef(owner,globalflag)
            globalflag:=module_scope

        when kconstsym then
            readconstdef(owner,globalflag)
            globalflag:=module_scope

        when kclasssym,krecordsym then
            readclassdef(owner,globalflag)
            globalflag:=module_scope

        when ktabledatasym then
            readtabledef(owner,globalflag)
            globalflag:=module_scope

        when docstringsym then
            adddocstring(lx.svalue)
            lex()

        when semisym then
            lex()

        when eofsym then
            exit

        when kfflangsym then
            if lx.subcode=callbackff then
                callbackflag:=callbackff
                lex()
            else
                serror("fflang?")
            fi

        when kmacrosym then
            readmacrodef(owner,globalflag)
            globalflag:=module_scope

        when kheadersym then
            repeat
                lex()
            until lx.symbol=semisym

        when dotsym then
            SERROR("MODULE/DOT")
        when namesym then
            if istypestarter() then
                goto dovar
            fi
            goto doexec

        else
doexec::
            p:=readunit()
            addlistunit(ulist,ulistx,p)
        endswitch
    od

    return ulist
end

proc initparser=

    unless nullunit then
        nullunit:=createunit0(jnull)
    end unless

    try_level:=0
    currproc:=nil
    varattribs:=0

    intabledata:=0      !1 means reading table data line; $ gives tabledataname
    inreadprint:=0
    inparamlist:=0
    inrecordbody:=0
    inimportmodule:=0
    ichar tabledataname:=""
    labelseen:=0

    ndollar:=0
end

global proc skipsemi=
    while lx.symbol=semisym do lex() od
end

global function makeblock(unit p)unit=
    if p and p.tag=jblock then return p fi
    return createunit1(jblock,p)
end

proc checkequals=
!check that "=" is current symbol
    if lx.symbol<>eqsym then
        serror("""="" expected")
    fi
end

function getcurrline:int=
    return lx.pos
end

function checkbegin(int fbrack)int=
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
    int closesym

    skipsemi()

    if lx.symbol=lbracksym and fbrack then
        closesym:=rbracksym
        lex()
    else
        closesym:=kendsym
    fi
    return closesym
end

proc checkbeginend(int closesym,kwd,startline=0)=
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
    skipsemi()
!   if closesym=rbracksym or closesym=rcurlysym then
    if closesym=rbracksym then
        checksymbol(closesym)
    else
        checkend(closesym,kwd,startline:startline)
    fi
    lex()
end

global proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
    [100]char str

!exit pointing to current symbol (to 'end', keyword after 'end', or ')')
    if endsym=lx.symbol=rbracksym then
        return
    fi

    if lx.symbol<>kendsym then
        strcpy(&.str,"Bad 'end' ")
    error::

        if startline then
            fprint @(&.str+strlen(&.str))," (from line #)",startline iand 16777215
        fi
        serror(&.str)
    fi

!'end', seen, but check that lx.subcode, if non-zero, is endkeywords or is in that set
    if lx.subcode=0 then                    !plain end; for now, that always matches
!       serror("'end' by itself no longer valid")
        return
    fi

    unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
        strcpy(&.str,"Mismatched 'end'")
        goto error
    end unless
end

function readvardef(ref strec owner,int scope=0,isstatic=0,varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used,
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

    unit ulist,ulistx, p
    int nvars,m, initcode
    ref strec stname

    ulist:=ulistx:=nil

    if istypestarter() then
        m:=readtypespec(owner)
    elsif k then
        m:=tauto
!       m:=tvariant
    else
        serror("Readvar?")
    fi

    nvars:=0
    while lx.symbol=namesym do

        ++nvars
        stname:=getduplnameptr(owner,lx.symptr,varid)

        stname.scope:=scope

        stname.isstatic:=isstatic
        stname.islet:=(k=kletsym)
        if varid=dllvarid then
            stname.isimport:=1
        fi

        adddef(owner,stname)
        if varid=staticid then
            addstatic(stname)
        fi

        lex()

        if lx.symbol=colonsym then
            if m<>tauto then serror("Mixed var T x:T") fi
            lex()
            m:=readtypespec(owner)
        fi

        storemode(owner,m,stname.mode)

        if lx.symbol in [assignsym,eqsym,deepcopysym] then
!           initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 esac
            case lx.symbol
            when eqsym then initcode:=1
            when assignsym then initcode:=2
            else initcode:=3
            esac

            if lx.symbol<>eqsym then
                if varid=staticid then
                    serror("Non-variants can't use :=")
                    if owner.nameid=procid then
                        serror("Can't use := for statics inside procs")
                    fi
                    
                fi
            else
                if varid=frameid then
                    serror("Need 'static' for '='")
                    addstatic(stname)
                fi
            fi
            lex()
            if lx.symbol=kemptysym then
                lex()
                if varid<>frameid then serror("empty: not frame") fi
                p:=createunit1(jempty,createname(stname))
                addlistunit(ulist,ulistx,p)
            else
                stname.code:=readunit()
                stname.equals:=initcode
                if varid=frameid then
                    p:=createunit2(jassign,createname(stname),stname.code)
                    p.initlet:=1
                    addlistunit(ulist,ulistx,p)
                fi
            fi

        elsif lx.symbol=atsym then
            if k=kletsym then serror("let@") fi
            lex()
            stname.atvar:=1
            stname.equivvar:=readunit()
        elsif k=kletsym then
            serror("let needs :=/=")
        fi

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

proc readconstdef(ref strec owner,int scope=0)=
!at 'const' symbol
    int nconsts,deft,m
    ref strec stname

    lex()

    nconsts:=0

    if istypestarter() then
        deft:=readtypespec(owner)
    else
        deft:=tauto
    fi

    while lx.symbol=namesym do
        stname:=getduplnameptr(owner,lx.symptr,constid)

        lex()

        checkequals()
        lex()
        stname.code:=readconstexpr(1)

        m:=deft

        storemode(owner,m,stname.mode)
        ++nconsts

        stname.scope:=scope

        adddef(owner,stname)
        if scope=export_scope and stname.name^<>'$' then
            addexpconst(stname)
        fi

        if lx.symbol<>commasym then
            exit
        fi
        lex()
    od

    if nconsts=0 then
        serror("No consts declared")
    fi

end

function readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following::
! (x)       simple expression
! ()        list with no elements
! (x,)      list with one element
! (x,x,...)     list
! (x|x|x])      if then else fi
! (x|x,... |x]) select then else end

!return positioned at symbol following closing ")"
!listtag is jmakelist or jmakearray if 'array' was used

    unit ulist,ulistx, p,q,r, plower
    int oldirp,length, usecomma

    lex()                   !first symbol of first expression
    ulist:=ulistx:=nil
    plower:=nil
    length:=0

    if lx.symbol=atsym then         !lwb override
        lex()
        oldirp:=inreadprint
        inreadprint:=1
        plower:=readunit()

        inreadprint:=oldirp
        checksymbol(colonsym)
        lex()

    elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
        plower:=createconstunit(lx.value,lx.subcode)
        plower.istrueconst:=1
        lex()
        lex()

    elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then  !operator constant
        p:=createunit0(joperator)
        p.pclop:=symbolgentoops[lx.symbol]
!PRINTLN =P.PCLOP
        lex()
        lex()
        return p
    elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then  !operator:= constant
        p:=createunit0(joperator)
        p.pclop:=symbolgentoops[lx.symbol]
        lex()           !read :=
        lexchecksymbol(rbracksym)
        lex()
        return p
    fi

!check symbol after "("
    case lx.symbol
    when rbracksym then         !empty list
        lex()
        p:=createunit0(jmakelist)
        p.b:=plower
        p.length:=0
        return p
    else                    !assume normal expression follows
        p:=readunit()
    esac

!check symbol after "(expr"
    case lx.symbol
    when rbracksym then         !simple (x) expression
        lex()

        return p

    when commasym then          !separate by comma or implicit newline
        usecomma:=1
        if nextlx.symbol=rbracksym then     !means one-element list
            lex()
            lex()
            p:=createunit1(jmakelist,p)
            p.length:=1
            p.b:=plower
            return p
        fi
docomma::                       !entry from implicit newline
        length:=1

!must be regular list
        ulist:=ulistx:=p

        if usecomma then
            repeat
                lex()                           !skip comma
                if lx.symbol=rbracksym then     !allow ,) to end list
                    exit
                fi
                if lx.symbol=commasym then
                    serror(",, null expr not allowed")
                fi
                addlistunit(ulist,ulistx,readunit())
                ++length
                skipsemi()
            until lx.symbol<>commasym
        else

            repeat
                skipsemi()
                if lx.symbol=rbracksym then     !allow ,) to end list
                    exit
                fi
                if lx.symbol=commasym then
                    serror(",, null expr not allowed")
                fi
                addlistunit(ulist,ulistx,readunit())
                ++length
            until lx.symbol<>semisym
        fi

        checksymbol(rbracksym)
        lex()
        p:=createunit1(jmakelist,ulist)
        p.length:=length
        p.b:=plower
        return p

    when barsym then            !ifx/selectx expression; p is selector expression
        lex()
        q:=readunit()
        case lx.symbol
        when barsym then        !(a|b|c)
            lex()
            r:=readsunit()
            checksymbol(rbracksym)
            lex()
            return createunit3(jif,fixcond(p),q,r)
        when rbracksym then
            lex()
            return createunit3(jif,fixcond(p),q,nil)

        esac

!assume selectx expression
        addlistunit(ulist,ulistx,q) !start with one-element list
        checksymbol(commasym)
        if nextlx.symbol<>barsym then       !(n|a,| using one-element list; not useful but allow it...
            repeat
                lex()               !skip comma
                addlistunit(ulist,ulistx,readunit())
            until lx.symbol<>commasym
            checksymbol(barsym)
        else
            lex()                   !skip |
        fi
        lex()
        r:=readunit()
        checksymbol(rbracksym)
        lex()
        return createunit3(jselect,p,ulist,r)

    when semisym then
        if lx.subcode=1 then
            usecomma:=0
            goto docomma
        fi
        ulist:=ulistx:=p
        repeat
            skipsemi()
            if lx.symbol=rbracksym then
                exit
            fi
            addlistunit(ulist,ulistx,readunit())
!           skipsemi()                      !allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
        until lx.symbol<>semisym
        checksymbol(rbracksym)
        lex()

        return makeblock(ulist)


    else
        serror("(x ...")
    esac
    return nil
end

proc addlistparam(ref ref strec ulist,ulistx,ref strec p)=
!add unit p to unit structure ulist,^ulistx  which can be null
    if ulist^=nil then      !first
        ulist^:=ulistx^:=p
    else
        ulistx^.nextparam:=p
    fi
    ulistx^:=p          !update end-of-list pointer
end

function readcast:unit=
!also reads standalone type value
!t<>tvoid means already has ty[e
    unit p
    int opc,t

    t:=readtypespec(currproc)

    case lx.symbol
    when rbracksym then
        p:=createunit0(jtypeconst)
        p.mode:=ttype
        p.value:=t
        return p

    when atsym then
        opc:=jtypepun
        lex()
    when dotsym then            !allow T.type, but also just T (followed by . which
                                !might be T.minvalue etc)
        if nextlx.symbol=ktypesym then
            lex()
            p:=createunit0(jtypeconst)
            p.value:=t
            p.mode:=ttype
            lex()
        else                    !leave dot to be processed by caller
            p:=createunit0(jtypeconst)
            p.value:=t
        fi
        return p
    else
        opc:=jconvert
    esac

    checksymbol(lbracksym)
    lex()
    p:=readunit()
    checksymbol(rbracksym)
    lex()

    p:=createunit1(opc,p)
    storemode(currproc,t,p.convmode)
    return p
end

function readopc:unit=
!op sym seen just before a term
    unit p,q,r
    int tag,opc,firstsym

    firstsym:=lx.symbol

    case lx.symbol
    when mathsopsym then
        tag:=junary
        opc:=lx.subcode
    when maths2opsym then
        tag:=jbin
        opc:=lx.subcode
    else
        tag:=junary
        opc:=symbolgenops[firstsym]
    esac

    lex()
    case firstsym
    when addsym then            !ignore +
        return readterm2()
    when subsym then            !convert minus to negate
        opc:=kneg
    when minsym,maxsym,maths2opsym then
        p:=readterm2()

        if p.tag=jmakelist then
            if p.length<>2 then serror("Needs (x,y)") fi
            q:=p.a
            r:=q.nextunit
            q.nextunit:=nil
            p:=createunit2(jbin,q,r)
            p.pclop:=opc
            return p
        else        !assume single operand
            SERROR("READOPC/SINGLE OPND?")
            return createunit1(opc,p)

        fi
    else
        if symboloptypes[firstsym]=bin_op then
            serror("Can't be used as unary op")
        fi

    esac

    if lx.symbol=assignsym then !op:=, not normally allowed inside expressions
        lex()
        tag:=junaryto
        case firstsym
        when subsym then
            opc:=knegto
        else
            opc:=symbolgentoops[firstsym]
            if opc=0 then
                serror("op:= not available")
            fi
        esac
    fi

    p:=createunit1(tag,q:=readterm2())

    p.pclop:=opc

    if q.tag=jmakelist then
        serror("Too many opnds")
    fi

    return p
end

function readsprint:unit=
    int oldinreadprint,opc,isfprint
    unit pformat, pdev, printlist, printlistx, p

    oldinreadprint:=inreadprint
    inreadprint:=1
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
        if lx.symbol=commasym then      !assume extra comma, meaning nogap
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
    inreadprint:=oldinreadprint
    if (opc=jprint or opc=jfprint) and printlist=nil then
        serror("No print items")
    fi

    if isfprint then
        if pformat.tag=jnull then
            serror("No fmt str")
        fi
        return createunit3(opc,pdev,pformat,printlist)
    else
        return createunit2(opc,pdev,printlist)
    fi
end

function readsread:unit=
!Need to check what sread/sreadln actually mean. I think they are actually supposed
!to work an item at a time::
! a:=sread([fmt])
! b:=sreadln([dev]) returns entire input line, but keeps line for subsequent sread/read
    int oldinreadprint,opc
    unit pformat,pdev,p, readlist,readlistx

    oldinreadprint:=inreadprint
    inreadprint:=1
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
    inreadprint:=oldinreadprint
    if opc=jread and readlist=nil then
        serror("No read items")
    fi

    return createunit2(opc,pdev,readlist)
end

function readcompilervar:unit=
    [100]char str
    rsystemtime tm
    static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    unit p
    ref modulerec currmodule:=&moduletable[currmoduleno]

    switch lx.subcode
    when jcvnil then
        p:=createconstunit(0,tref)
        lex()
        return p

    when jcvpi then
!       p:=createconstunit(int64@(3.14159'265358'979'3238'4626'433'832),treal)
        p:=createconstunit(int64@(pi),treal)
        lex()
        return p

    when jcvinfinity then
        if ctarget then
            p:=createunit0(jinfinity)
            p.mode:=tr64
        else
            p:=createconstunit(int64@(infinity),treal)
        fi
        lex()
        return p

    when jcvlineno then

        p:=createunit0(jcvlineno)
        lex()
        return p

    when jcvstrlineno then
        getstrint(getlineno(lx.pos),&.str)

    when jcvmodulename then
        strcpy(str,stmodule.name)

    when jcvfilename then

        strcpy(str,sourcefilepaths[currmodule.fileno])

    when jcvfunction then
        strcpy(&.str,currproc.name)

    when jcvdate then
        os_getsystime(&tm)
        fprint @str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

    when jcvtime then
        os_getsystime(&tm)
        fprint @str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

    when jcvtargetbits then
        lex()
        return createconstunit(targetbits,tint)
    when jcvtargetsize then
        lex()
        return createconstunit(targetsize,tint)
    when jcvtargetcode then
        strcpy(&.str,"wx64")

    when jcvctarget then
        p:=createconstunit(ctarget,ti64)
        lex()
        return p

    when jcvwindows then
        p:=createconstunit(fwindows,ti64)
        lex()
        return p

    when jcvlinux then
        p:=createconstunit(flinux,ti64)
        lex()
        return p

    when jcvversion then
        strcpy(&.str,"Compiler:BX Experimental")

    when jcvtrue,jcvfalse then
        p:=createconstunit(lx.subcode=jcvtrue,tbool64)
        lex()
        return p
    
    else
        serror_s("compiler var not impl: #",jtagnames[lx.subcode])
    end switch
    lex()

    return createstringconstunit(pcm_copyheapstring(&.str),-1)
end

function readcastx:unit=
!explicit cast using syntax::
! cast(expr)
! cast(expr,type)
! cast@(expr,type)
!at 'cast'
    int opc,m
    unit p

    lex()
    opc:=jconvert
    if lx.symbol=atsym then
        opc:=jtypepun
        lex()
    fi
    checksymbol(lbracksym)
    lex()
    m:=tvoid
    p:=readunit()
    if lx.symbol<>commasym then
        if opc=jtypepun then serror("@ type missing") fi
        opc:=jautocast
    else
        lex()
        m:=readtypespec(currproc)
    fi
    checksymbol(rbracksym)
    lex()

    p:=createunit1(opc,p)
    storemode(currproc,m,p.convmode)

    return p
end

global proc checksymbol(int symbol)=
    [100]char str

    if lx.symbol<>symbol then
        fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]
        serror(&.str)
    fi
end

global proc lexchecksymbol(int symbol)=
    lex()
    checksymbol(symbol)
end

global function readtypespec(ref strec owner,int typedefx=0)int=
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either::
! moderec   Used when called from readtypedef. This is then filled in with the
!       details of the new mode, and the new version is returned
! nil       Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using int16 etc
    ref strec d,e
    int t,kwd,fflang,sltype,w
    unit x,pupper,plx
    unit dim,length
    const maxdim=30
    [maxdim]unit dims
    int ndims,i,n,k

    case lx.symbol
    when lsqsym then        !array bounds
arraybounds::
        lex()

        ndims:=0
        inreadprint:=1
        do
            length:=nil             !both bounds unspecified
            if lx.symbol=rsqsym or lx.symbol=commasym then      ![]
                dim:=nil
            else
                dim:=readunit()
                case lx.symbol
                when rsqsym,commasym then           ![n]
                when colonsym then              !a:n
                    lex()
                    if not (lx.symbol=commasym or lx.symbol=rsqsym) then    !lower:length
                        length:=readunit()
                        dim:=createunit2(jkeyvalue,dim,length)
                    else                                                    !lower::
                        dim:=createunit1(jkeyvalue,dim)
                    fi
                esac
            fi
            if ndims>=maxdim then serror("Too many array dims") fi
            dims[++ndims]:=dim
            exit when lx.symbol<>commasym
            lex()
        od
        inreadprint:=0
        checksymbol(rsqsym)
        lex()
        t:=readtypespec(owner)

        for i:=ndims downto 1 do
            t:=createarraymode(owner,t,dims[i],(i=1|typedefx|0))
        od
        return t

    when stdtypesym then
        t:=lx.subcode
        lex()

    when namesym then
        d:=lx.symptr
        lex()

        if lx.symbol=dotsym then
            lexchecksymbol(namesym)
            t:=newtypename(d,lx.symptr)
            lex()
        else
            t:=newtypename(nil,d)
        fi

    when krecordsym,kstructsym then
        serror("Use 'record name =' syntax")

    when kunionsym then
        serror("Top-level union not allowed")

    when krefsym then       !ref T
        fflang:=0
    retry::

        lex()
        if lx.symbol=ktosym then lex() fi

        case lx.symbol
        when kprocsym,kfunctionsym then !function pointer being created
            t:=readrefproc(owner,typedefx,fflang)

        when kfflangsym then
            fflang:=lx.subcode
            goto retry
!*! elsif lx.symbol=namesym and lx.subcode=asmopcodesym and lx.symptr.index=m_label then
!*!     t:=createrefmode(owner,tlabel,typedefx)
        elsif lx.symbol=stdtypesym then
            case lx.subcode
            when tc8 then
                t:=trefchar
                if typedefx then tttarget[typedefx]:=tc8 fi
            else
                goto readtarget
            esac

            lex()
        else                        !assume normal type
    readtarget::
            t:=readtypespec(owner)
            t:=createrefmode(owner,t,typedefx)
        esac

    when kicharsym then
        lex()
        t:=trefchar
        if typedefx then tttarget[typedefx]:=tc8 fi

    when ktypeofsym then
        lexchecksymbol(lbracksym)
        lexchecksymbol(namesym)

        t:=newtypename(cast(lx.symptr),nil)
        lexchecksymbol(rbracksym)
        lex()

    when kslicesym then
        t:=readslicetype(owner,lx.subcode,typedefx)

    when karraysym then
        lexchecksymbol(lsqsym)
        goto arraybounds
    else
        serror("Bad type starter")
    esac

    if typedefx then            !assume a simple alias
        ttbasetype[typedefx]:=ttbasetype[t]
    fi

    return t
end

function readslicetype(ref strec owner, int slicetype, typedefx)int=
!positioned at 'slice'
!dim is nil, or lower-bound expression
    unit plower
    int t

    lexchecksymbol(lsqsym)
    lex()
    if lx.symbol<>rsqsym then
        inreadprint:=1
        plower:=readunit()
        inreadprint:=0
        checksymbol(colonsym)
        lexchecksymbol(rsqsym)
    else
        plower:=nil
    fi
    lex()
    t:=readtypespec(owner,typedefx)

    return createslicemode(owner,slicetype,t,plower,typedefx)
end

function readslist(int iscall=0,donulls)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a function-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a,b,c )
!eg: (a     !
    unit ulist,ulistx
    int oldinparamlist

    ulist:=ulistx:=nil

    skipsemi()
    if lx.symbol=rbracksym then     !empty list
        return ulist
    fi

    oldinparamlist:=inparamlist
    inparamlist:=iscall

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
                addlistunit(ulist,ulistx,nullunit)
            fi
            exit
        else
            addlistunit(ulist,ulistx,readunit())
            if lx.symbol in [commasym,semisym] then
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
    inparamlist:=oldinparamlist

    return ulist
end

function readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is::
![x] or [x,...]         !single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
    unit q,plower,pupper

    lex()

    if not dot then
        case lx.symbol
        when rsqsym then
    fullslice::
            lex()
            plower:=createunit1(junary,duplunit(p))
            plower.pclop:=klwb
            pupper:=createunit1(junary,duplunit(p))
            pupper.pclop:=kupb
            p:=createunit2(jslice, p, createunit2(jmakerange,plower, pupper))
            return p
        when rangesym,colonsym then
            lexchecksymbol(rsqsym)
            goto fullslice
        esac
    fi

    do
        if ndollar>=maxdollarstack then
            serror("Too many nested a[$]")
        fi
        dollarstack[++ndollar]:=p
        q:=readunit()
        --ndollar

        if q.tag=jmakerange then        !convert into a discrete slice
            p:=createunit2((dot|jdotslice|jslice),p,q)
        else
            p:=createunit2((dot|jdotindex|jindex),p,q)
        fi

        exit when lx.symbol<>commasym
        lex()
    od
    checksymbol(rsqsym)
    lex()
    return p
end

function readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
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
            p:=createunit1(junary,p)
            p.pclop:=lx.subcode
            lex()
        when bitfieldsym then
            p:=createunit1(jbitfield,p)
            p.bfcode:=lx.subcode
            lex()
        when ktypesym then          !.type, convert to .gettype
            case p.tag
            when jtypeconst then            !int.type=>int

            else
                p:=createunit1(jtypeof,p)
            esac
            lex()

        when maxsym then
            lx.subcode:=kmaxvalue
            goto doprop

        when minsym then
            lx.subcode:=kminvalue
            goto doprop
        when stdtypesym then
            if p.tag=jtypeconst and lx.subcode=trange then
                q:=createunit2(jmakerange,
                    createunit1(junary,p),
                    createunit1(junary,p))
                q.a.pclop:=kminvalue
                q.b.pclop:=kmaxvalue
            else
                error
            fi
            lex()
            p:=q

        else
    error::
            serror("Unknown dot suffix")
        endswitch
    od
    return p
end

function readconstexpr(int needconst=1)unit=
    return readunit()
end

function readconstint:int=
!read expression that must yield a constant int value *now*; return value
    int64 x

!keep it simple for now
    if lx.symbol=intconstsym then
        x:=lx.value
        lex()
        return x
    elsif lx.symbol=subsym then
        lex()
        if lx.symbol=intconstsym then
            x:=lx.value
            lex()
            return -x
        fi
    fi

!later can only arbitrary expressions, provided they can be evaluated in this pass
    serror("Can't do complex expr")
    return 0
end

proc readprocdef(ref strec procowner,int scope,fflang=0)=
!at 'proc' etc symbol; read proc def or declaration
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
    int kwd,startline,closesym,shortfun
    ref strec stproc,q,stname

    kwd:=lx.symbol
    shortfun:=lx.subcode=1
    nforloops:=0

!PRINTLN =SYMBOLNAMES[KWD],=SHORTFUN
    assemmode:=1
    stproc:=readprocdecl(procowner,scope,fflang)
    assemmode:=0
    checkequals()

    lex()

    startline:=getcurrline()

    if not shortfun then
        closesym:=checkbegin(0)
    fi

    pushproc(stproc)
    nextavindex:=0

    IF DRETVAR THEN
        stname:=getduplnameptr(stproc,dretvar,frameid)
        storemode(procowner,stproc.mode,stname.mode)
        adddef(stproc,stname)
    fi

    addtoproclist(stproc)

    if shortfun then
        stproc.code:=readunit()
        checksymbol(semisym)
        lex()
    else
        stproc.code:=readsunit()
        checkbeginend(closesym,kwd,startline)
    fi

    stproc.code:=makeblock(stproc.code)

    if ndocstrings and docfile and stproc.scope>=program_scope then
        println @docfile,"proc",stproc.name
        for i to ndocstrings do
            println @docfile,docstrings[i]
            pcm_free(docstrings[i],strlen(docstrings[i]+1))
        od
        println @docfile

        ndocstrings:=0
    fi

    popproc()
end

global function readprocdecl(ref strec procowner,int scope,fflang)ref strec=
!at 'proc'  or 'function' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

    int kwd,varparams,try_level, nparams, nretvalues, isthreaded
    [maxtuplesize]int retmodes
!   int prettype@&retmodes
    macro prettype = retmodes[1]

    ichar metadata, truename
    ref strec pequiv, stproc, owner, paramlist,nameptr

    kwd:=lx.symbol              !remember keyword
    isthreaded:=lx.subcode=2

    pequiv:=nil
    metadata:=""
    truename:=nil
    varparams:=0
    try_level:=0

    lex()

    if lx.symbol=stringconstsym then        !assume dll truename
        truename:=pcm_copyheapstring(lx.svalue)
        convlcstring(lx.svalue)
        lx.symptr:=addnamestr(lx.svalue)
    else
        checksymbol(namesym)
    fi

    nameptr:=lx.symptr

    stproc:=getduplnameptr(procowner,nameptr,(insidedllimport|dllprocid|procid))
    if insidedllimport then
        scope:=subprog_scope
        stproc.dllindex:=insidedllimport        !insidedllimport = curr. dll index
    fi
    stproc.isthreaded:=isthreaded

    if truename then
        stproc.truename:=truename
    fi

    adddef(procowner,stproc)
    if stproc.nameid=dllprocid then
        stproc.isimport:=1
    fi

    owner:=stproc
    pushproc(stproc)

    lex()

    paramlist:=nil
    prettype:=tvoid
    nparams:=0
    nretvalues:=0

    nretvalues:=0
    if lx.symbol=lbracksym then     !possible params
        lex()
        if lx.symbol<>rbracksym then
            paramlist:=readparams(procowner,stproc,fflang,varparams,nparams)
            checksymbol(rbracksym)
        fi
        lex()

        if lx.symbol=colonsym or lx.symbol=sendtosym then
            lex()
            nretvalues:=readreturntype(owner,retmodes)
        elsif typestarterset[lx.symbol] or lx.symbol=namesym then
            nretvalues:=readreturntype(owner,retmodes)
        fi
    elsif lx.symbol=colonsym or lx.symbol=sendtosym then
        lex()
        nretvalues:=readreturntype(owner,retmodes)
    fi

    dretvar:=nil
    if nretvalues=1 then
        if lx.symbol=namesym then
            dretvar:=lx.symptr
            lex()
        fi
    fi

    unless nretvalues or (kwd<>kfunctionsym) then       !function: no result given
        serror("Function needs ret type")
    end unless

    if nretvalues and (kwd<>kfunctionsym) then      !proc: result given
        serror("Proc can't return value")
    fi

    stproc.paramlist:=paramlist
    stproc.nretvalues:=nretvalues

    case nretvalues
    when 0 then
        stproc.mode:=tvoid
    when 1 then
        storemode(procowner,retmodes[1],stproc.mode)
    else
        stproc.mode:=createtuplemode(procowner,retmodes,nretvalues,0)
    esac

    if lx.symbol=atsym then         !equivalence
        lexchecksymbol(namesym)
    SERROR("READPROCDEF @")
        lex()
        stproc.atvar:=1
    fi

    stproc.code:=nil

    case fflang
    when clangff,windowsff then
    else            !assume this language
        case procowner.nameid
        when moduleid then
        when dllmoduleid then
            serror("Need FF specifier")
        esac
    esac
    stproc.scope:=scope
    stproc.varparams:=varparams
    stproc.fflang:=fflang

    if procowner=stmodule then
        if stproc.namelen=5 and eqstring(stproc.name,"start") then
            moduletable[stmodule.moduleno].ststart:=stproc
            stproc.scope:=subprog_scope
        elsif stproc.namelen=4 and eqstring(stproc.name,"main") and
            stmodule.moduleno=mainmoduleno then
            moduletable[stmodule.moduleno].stmain:=stproc
!           if stmodule.moduleno=mainmoduleno then
                stproc.scope:=export_scope
!           fi
        fi
    fi

    popproc()

    return stproc
end

function readparams(ref strec procowner,owner,int fflang,&varparams,&nparams)ref strec=
!positioned at first symbol after '('; this is not ')'
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
    ref strec stlist, stlistx, stname
    int parammode, pmode, m, isoptional,types

    stlist:=stlistx:=nil
    pmode:=tvoid
    nparams:=0
    parammode:=var_param
    types:=0

    if fflang=0 then fflang:=mlangff fi

    if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
        types:=1
    fi

    do                                      !expect type of name at start of loop
        parammode:=var_param
        isoptional:=0

        if types or istypestarter() then                !assume new mode
            pmode:=readtypespec(procowner)
gotmode::

            if nparams=0 and lx.symbol in [commasym, rbracksym] then
                do
                    [32]char str
                    ++nparams
                    str[1]:='$'; str[2]:=0
                    strcat(str, strint(nparams))
                    stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
                    adddef(owner,stname)

                    storemode(owner,pmode,stname.mode)
                    stname.parammode:=parammode
                    addlistparam(&stlist,&stlistx,stname)

                    case lx.symbol
                    when rbracksym then
                        exit
                    esac

                    checksymbol(commasym)
                    lex()
                    if lx.symbol=ellipsissym then
                        varparams:=nparams+1
                        lex()
                        exit
                    fi

                    pmode:=readtypespec(procowner)
                od
                return stlist
            fi

!       elsif lx.symbol=kmutsym then
!           pmode:=tvariant
!           lex()
!           goto gotmode
        elsif pmode=tvoid then
            serror("Type expected")
        fi

        case lx.symbol
        when insym then
            parammode:=in_param
            lex()
            if lx.symbol=colonsym then lex() fi
        when koutsym,addrsym then
            parammode:=out_param
            lex()
            if lx.symbol=colonsym then lex() fi
        when questionsym then
            isoptional:=1
            lex()
        when ellipsissym then
            varparams:=1
            lex()
            return stlist
        esac

        checksymbol(namesym)
        ++nparams
        stname:=getduplnameptr(owner,lx.symptr,paramid)
        adddef(owner,stname)
        lex()

        if parammode=out_param then
            m:=createrefmode(procowner,pmode)
        else
            m:=pmode
        fi

        storemode(owner,m,stname.mode)
        stname.parammode:=parammode
        stname.optional:=isoptional
        addlistparam(&stlist,&stlistx,stname)

        case lx.symbol
        when assignsym, eqsym then
            lex()
            stname.code:=readunit()
            stname.equals:=1
            stname.optional:=1
        esac

        case lx.symbol
        when commasym then
            lex()
        when rbracksym then
            exit
        else
            serror("nameparams1")
        esac
    od

return stlist
end

function readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond
    unit q

    switch lx.symbol
    when kwhensym then
        lex()
        return createunit2(jif,fixcond(readunit()),createunit1(jblock,p))
    when kunlesssym then
        lex()
        q:=createunit1(jnotl,fixcond(readunit()))
        q.pclop:=knotl
        return createunit2(jif, q,createunit1(jblock,p))
    else
        return p
    endswitch
end

function readif:unit=
!at 'if'
    int pos1, kwd, pos2
    unit clist,clistx, plist,plistx, pelse, p, pelsif

    pos1:=lx.pos
    kwd:=lx.symbol          !in case coming from elsecase etc

    clist:=clistx:=plist:=plistx:=pelse:=nil

    repeat
        lex()
        addlistunit(clist,clistx, fixcond(readsunit()))

        skipsemi()
        checksymbol(kthensym)
        lex()

        addlistunit(plist,plistx, readsunit())
        skipsemi()

    until lx.symbol<>kelsifsym

    case lx.symbol
    when kelsesym then      !get r=any else stmt or nil
        lex()
        pelse:=readsunit()
        checkend(kendsym,kwd,0)
        lex()
    when kelsecasesym,kelseswitchsym then
        lx.symbol:=kwd
        pelse:=makeblock(readswitchcase())
    else
        checkend(kendsym,kwd,0)
        lex()
    esac

    p:=createunit3(jif,clist, plist,pelse)
    p.pos:=pos1
    return p
end

function readgoto(int gototag=jgoto)unit=
    ref strec d
    unit p

    if lx.subcode=1 then        !go used
        lexchecksymbol(ktosym)
    fi
    lex()

    return readcondsuffix(createunit1(gototag,readunit()))
end

function readunless:unit=
    int pos
    unit pcond, pthen, pelse, p,q
    pos:=lx.pos
    lex()
    pcond:=fixcond(readsunit())
    checksymbol(kthensym)
    lex()

    pthen:=readsunit()

    if lx.symbol=kelsesym then
        lex()
        pelse:=readsunit()
    else            !assume simple if-then
        PELSE:=NIL
    fi
    checkend(kendsym,kunlesssym)
    lex()
    p:=createunit3(jif,q:=createunit1(jnotl,pcond),pthen,pelse)
    q.pclop:=knotl
    p.pos:=pos
    return p
end

function readswitchcase:unit=
    int pos1, kwd, opc, pos2,rangeused, nwhen
    unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

    pos1:=lx.pos
    kwd:=lx.symbol          !remember kcasesym etc

    opc:=lx.subcode         !pick up tag: kcase etc

    lex()

    skipsemi()
    if lx.symbol=kwhensym then
        if kwd=kswitchsym then
            serror("switch expr missing")
        fi
        pexpr:=nil
    else
        pexpr:=readsunit()      !index expression
    fi

    pwhenlist:=pwhenlistx:=nil
    rangeused:=0
    nwhen:=0

    skipsemi()
    while lx.symbol=kwhensym do !read list of when-then pairs
        pos2:=lx.pos
        lex()
        pwhen:=pwhenx:=nil
        do
            p:=readunit()
            ++nwhen
            p.pos:=pos2
            if p.tag=jmakerange then rangeused:=1 fi
            addlistunit(pwhen,pwhenx,p)
            if lx.symbol<>commasym then exit fi
            lex()
        od
        if lx.symbol<>sendtosym then
            checksymbol(kthensym)
        fi
        lex()
        pthen:=readsunit()
        pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
        pwhenthen.pos:=pos2
        addlistunit(pwhenlist,pwhenlistx,pwhenthen)
    od

    if opc=jswitch and not rangeused then
        if nwhen<=8 then
!*!         opc:=jcase
        fi
    fi

    case lx.symbol
    when kelsesym then      !get r=any else stmt or nil
        lex()
        pelse:=readsunit()

        checkend(kendsym,kwd)
        lex()
    when kelsifsym then
        lx.symbol:=kwd
        pelse:=makeblock(readif())
    when kelsecasesym, kelseswitchsym then
        lx.symbol:=kwd
        pelse:=makeblock(readswitchcase())
    else
        PELSE:=NIL
        checkend(kendsym,kwd)
        lex()
    esac

    p:=createunit3(opc,pexpr,pwhenlist,pelse)
    p.pos:=pos1

    return p
end

function readstop:unit=
    unit p
    int i
    lex()
    if exprstarter[lx.symbol] then
        p:=createunit1(jstop,readunit())
    else
        p:=createunit0(jstop)
    fi
    return readcondsuffix(p)
end

function readreturn:unit=
    unit p,q,r

    lex()
    if exprstarter[lx.symbol] then
        q:=readunit()
        p:=createunit1(jreturn,q)
        p.length:=1
    else
        p:=createunit0(jreturn)
        p.length:=0
    fi

    return readcondsuffix(p)
end

function readdo:unit=
    unit p
    int pos

    pos:=lx.pos
    lex()
    p:=readsunit()
    checkend(kendsym,kdosym)
    lex()
    p:=createunit1(jdo,p)
    p.pos:=pos
    return p
end

function readto:unit=
    int pos,id
    unit p, pcount, pbody

    pos:=lx.pos
    lex()

    pcount:=readunit()

    checksymbol(kdosym)
    lex()
    pbody:=readsunit()
    checkend(kendsym,ktosym,kdosym)
    lex()
    id:=frameid
    if currproc.nameid<>procid then id:=staticid fi

    p:=createunit3(jto,pcount,pbody,createname(getavname(currproc,id)))
!p:=createunit2(jto,pcount,pbody)
    p.pos:=pos
    return p
end

function readwhile:unit=
    int pos
    unit pcond, pbody, pincr, p

    pos:=lx.pos
    lex()

    pcond:=fixcond(readsunit(1))
    pincr:=nil

    if lx.symbol=commasym then
        lex()
        pincr:=readsunit(1)
    fi

    checksymbol(kdosym)
    lex()
    pbody:=readsunit()

    if lx.symbol=kstepsym then
        if pincr then serror("Double incr") fi
        lex()
        pincr:=readsunit()
    fi

    checkend(kendsym,kwhilesym,kdosym)
    lex()

    p:=createunit3(jwhile,pcond,pbody,pincr)
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
    pcond:=fixcond(readunit())
    p:=createunit2(jrepeat,pbody,pcond)
    p.pos:=pos

    return p
end

function readloopcontrol:unit=
    int opc
    unit p

    opc:=lx.subcode

    lex()
    if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
        lex()
        p:=createunit1(opc,createconstunit(0,tint))

    elsif exprstarter[lx.symbol] then
        p:=createunit1(opc,readconstexpr(1))
    else
        p:=createunit1(opc,createconstunit(1,tint))
    fi
    return readcondsuffix(p)
end

function readprint:unit=
    int oldinreadprint, opc, isfprint, fshowname, length
    unit pformat, pdev, printlist,printlistx, p,q
    ref strbuffer expr

    ichar s

    oldinreadprint:=inreadprint
    inreadprint:=1
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

    if not exprstarter[lx.symbol] then
        goto finish
    fi

    do
        case lx.symbol
        when commasym then      !assume extra comma, meaning nogap
            addlistunit(printlist,printlistx, createunit0(jnogap))
        when dollarsym then     !assume extra comma, meaning nogap
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

                addlistunit(printlist,printlistx,q:=createstringconstunit(s,expr.length))
            fi
            addlistunit(printlist,printlistx,p)
        esac
        if lx.symbol<>commasym then exit fi
        lex()
    od

    finish::
    inreadprint:=oldinreadprint
    if opc=jprint and printlist=nil then
        serror("No print items")
    fi
    if opc=jfprint and printlist=nil and pformat=nil then
        serror("No print items")
    fi

    if isfprint then
        if pformat=nil then
            serror("No fmt str")
        fi
        return createunit3(opc,pdev,pformat,printlist)
    else
        return createunit2(opc,pdev,printlist)
    fi
end

function readread:unit=
    int oldinreadprint,opc
    unit pformat, pdev, readlist, readlistx, p, pread

    oldinreadprint:=inreadprint
    inreadprint:=1
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
        if lx.symbol=commasym then lex() fi
    fi

    if opc=jreadln then
        addlistunit(readlist,readlistx,createunit1(jreadln,pdev))
    fi

    if not exprstarter[lx.symbol] then
        goto finish
    fi

    do
        p:=readunit()
        if lx.symbol=colonsym then
            lex()
            pformat:=readunit()
        else
            pformat:=nil
        fi

        pread:=createunit1(jread,pformat)

!

        p:=createunit2(jassign,p,pread)

        addlistunit(readlist,readlistx,p)
        if lx.symbol<>commasym then exit fi
        lex()
    od

    finish::
    inreadprint:=oldinreadprint
    if opc=jread and readlist=nil then
        serror("No read items")
    fi

    if readlist.nextunit then
        return createunit1(jblock,readlist)
    else
        return readlist
    fi
end

function readfor:unit=
!on 'for'; syntax is::
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[,var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!   I   loop index, always i64; will be 'i' (declared or not declared) or autovar
!   L   forall local variable; will be 'x' (declared or not declared); type is variable

    int pos, opc, kwd
    unit pindex, plocal             !for index; for index,local
    unit pfrom, pto, pstep, ptoinit !for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
    unit plist, passign             !for INDEX in/inrev LIST (also LIST.BOUNDS)
    unit pcond, pbody, pelse
    unit p
!
    pos:=lx.pos
    lex()                       !skip 'for' kwd

    plocal:=nil
    ptoinit:=nil
    pindex:=readname()

    if nforloops>=maxforloops then
        serror("Too many for-loops")
    fi
    for i to nforloops do
        if forindexvars[i]=pindex.def then
            serror("Re-using nested loop index")
        fi
    od
    forindexvars[++nforloops]:=pindex.def

    if lx.symbol=commasym then
        lex()
        plocal:=readname()
    fi

    opc:=jforup
    pstep:=nil
    pcond:=nil

    if lx.symbol in [insym, inrevsym] then              !assume forall
        if lx.symbol=jinrev then
            opc:=jfordown               !tentative; may be changed to forall
        fi
        lex()

        plist:=readunit()

        if plist.tag=junary and plist.pclop=kbounds then
            pfrom:=getrangelwbunit(plist.a)
            pto:=getrangeupbunit(plist.a)
        elsif plist.tag=jmakerange then
            pfrom:=plist.a
            pto:=plist.b
        else
            opc:=(opc=jforup|jforall|jforallrev)
            pfrom:=getrangelwbunit(duplunit(plist))
            pto:=getrangeupbunit(duplunit(plist))
        fi

    else
        if lx.symbol=assignsym then
            lex()
            pfrom:=readunit()
        else
            pfrom:=createconstunit(1,tint)
        fi
        checksymbol(ktosym)
        opc:=(lx.subcode=1|jfordown|jforup)
        lex()
        pto:=readunit()

        if lx.symbol=kbysym then
            lex()
            pstep:=readconstexpr(0)
            if pstep.tag=jconst then
                if pstep.value=1 then       !by 1
                    pstep:=nil
                fi
            fi
        fi
    fi

    if lx.symbol=kwhensym then
        lex()
        pcond:=fixcond(readunit())
    fi
    checksymbol(kdosym)
    lex()
    pbody:=readsunit()
    pelse:=nil

    if lx.symbol=kelsesym then
        lex()
        pelse:=readsunit()
    fi
    checkend(kendsym,kforsym,kdosym)
    lex()

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

    if pcond<>nil then
        pbody:=makeblock(createunit2(jif,pcond,pbody))
    fi
    pbody.nextunit:=pelse

!forup/down layout
!   a:  pindex
!   b:  pfrom/pto/pstep
!   c:  pbody

!forall/rev layout
!   a:  pindex/plocal/pfrom/pto
!   b:  plist/passign
!   c:  pbody

    case opc
    when jforup, jfordown then
        if plocal then serror("for i,x?") fi
        pindex.avcode:='I'
        if pto.tag not in [jconst, jname] then
            plocal:=createname(getavname(currproc))
            plocal.avcode:='I'
            ptoinit:=createunit2(jassign, plocal, pto)
            pindex.nextunit:=ptoinit
            pto:=plocal
        fi

        pfrom.nextunit:=pto
        pto.nextunit:=pstep
        p:=createunit3(opc, pindex, pfrom, pbody)

    else                                        !assume forall/rev

        if plocal=nil then                      !only for x
            plocal:=pindex
            pindex:=createname(getavname(currproc))
        fi
        pindex.avcode:='I'
        plocal.avcode:='L'
        pindex.nextunit:=plocal
        plocal.nextunit:=pfrom
        pfrom.nextunit:=pto

        passign:=createunit2(jassign,duplunit(plocal),
                    createunit2(jindex,duplunit(plist),duplunit(pindex)))
        plist.nextunit:=passign

        p:=createunit3(opc, pindex, plist, pbody)

    esac

    p.pos:=pos
    --nforloops
    return p
end

function readname:unit p=
    p:=readterm2()
    if p.tag<>jname then serror("Name expected") fi
    return p
end

global proc readtypedef(ref strec owner,int scope=0)=
!at 'type' symbol
    ref strec sttype,stname
    int t,m

    lexchecksymbol(namesym)
    stname:=lx.symptr

    lex()
    checkequals()
    lex()

    sttype:=getduplnameptr(owner,stname,typeid)
    adddef(owner,sttype)
    m:=createusertype(sttype)
    ttusercat[m]:=1

    t:=readtypespec(sttype,m)       !should return filled-in version of m

    sttype.scope:=scope
    storemode(owner,t,sttype.mode)

    if t>=0 then
        if ttisinteger[t]+ttisreal[t] then
            tttarget[m]:=t
        elsif ttisref[t] then
        elsecase ttbasetype[t]
        when tarray then
        when tslice then
!       when tslice,tvector, tflex then
        when trecord then
        else
            tttarget[m]:=t
        fi
    else
        storemode(owner,t,tttarget[m])
    fi

    if t>=0 then
        copyttvalues(m,t)
    else
        ttbasetype[m]:=tpending
    fi
end

global proc readrecordfields(ref strec owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
    int nvars,offset
    ref strec stname,stbitfield

    nvars:=0
    while lx.symbol=namesym do

        stname:=getduplnameptr(owner,lx.symptr,fieldid)
        storemode(owner,m,stname.mode)
        ++nvars

        if unionpend.ulength then
            unionstr_copy(&stname.uflags,&unionpend)
            unionstr_concat(&unionstring,&unionpend)
            unionstr_clear(&unionpend)
        else
            unionstr_clear(&stname.uflags)
        fi
        unionlastvar:=stname            !filled in from outside with 'E' codes

        adddef(owner,stname)

        lex()

        case lx.symbol
        when atsym then
            lex()
            stname.atfield:=1
            stname.equivfield:=readequivfield(owner)
            if lx.symbol=addsym then
                lex()
                offset:=readconstint()
                if offset>stname.equivoffset.max then serror("Offset>255") fi
                stname.equivoffset:=offset
            fi

        when datsym then
            lexchecksymbol(intconstsym)
            case lx.value
            when 1,2,4,8 then
                stname.align:=lx.value
            when 0 then
                stname.align:=255
            else
                serror("@@ bad align")
            esac
            lex()
        when colonsym then              !read bitfields
!format is int : (a:1, b:3, c:2)
            lexchecksymbol(lbracksym)

            repeat
                lexchecksymbol(namesym)
                stbitfield:=getduplnameptr(owner,lx.symptr,fieldid)
                stbitfield.mode:=tbitfield
                adddef(owner,stbitfield)

                stbitfield.atfield:=1
                stbitfield.equivfield:=stname

                lexchecksymbol(colonsym)
                lexchecksymbol(intconstsym)
                stbitfield.bitfieldwidth:=lx.value
                lex()

            until lx.symbol<>commasym
            checksymbol(rbracksym)
            lex()

        esac

        if lx.symbol<>commasym then
            exit
        fi
        lex()
    od

    if nvars=0 then
        serror("No fields declared")
    fi
end

global proc readtabledef(ref strec owner,int scope=0)=
!at 'tabledata' symbol
    int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym
    int ltype
    unit plower
    ref strec stvar,stenum,stgen
    const maxcols=20
    [maxcols]ref strec varnameptrs
    [maxcols]int varlisttypes
    [maxcols]unit plist,plistx
    const maxrows=500
    [maxrows]int enumvalues

    enums:=lx.subcode               ! means enumdata
    lex()
    tabledataname:=nil

    if lx.symbol=lbracksym then     !tabledata(...) read enum type
        if not enums then serror("use 'enumdata'") fi
        enums:=1
        lex()
        checksymbol(rbracksym)
        lex()
    fi

    nextenumvalue:=1
    nrows:=0            !number of data rows appearing
    ncols:=0            !number of data columns (varnames appearing)

!loop reading variable names
    while lx.symbol<>eqsym do
        ltype:=readtypespec(owner)
        checksymbol(namesym)
        if ++ncols>maxcols then
            serror("tabledata/too many columns")
        fi
        varnameptrs[ncols]:=lx.symptr
        varlisttypes[ncols]:=ltype

        lex()
        if lx.symbol=commasym then
            lex()
        else
            exit
        fi
    od

    lex()                   !skip =

    skipsemi()
    startline:=getcurrline()
    closesym:=checkbegin(0)

    skipsemi()
    firstval:=lastval:=0

    for i:=1 to ncols do
        plist[i]:=plistx[i]:=nil
    od

    intabledata:=1
    do          !loop per row
        skipsemi()
        if ncols>0 then
            checksymbol(lbracksym)
            lex()
        fi
        if ++nrows>maxrows then
            serror("tabledata:too many rows")
        fi

        if enums then
            checksymbol(namesym)
            stgen:=lx.symptr                !generic symbol entry
            tabledataname:=stgen.name       !allow to be picked up by $ lx.symbol
            lex()
            if lx.symbol=eqsym then
                if nrows>1 then serror("tabledata '=' not 1st") fi
                lex()
                nextenumvalue:=readconstint()
            fi
            enumvalues[nrows]:=nextenumvalue

            stenum:=getduplnameptr(owner,stgen,constid)
            stenum.mode:=tint
            stenum.code:=createconstunit(nextenumvalue,tint)
            stenum.scope:=scope
            adddef(owner,stenum)
            if scope=export_scope then
                addexpconst(stenum)
            fi

            if nrows=1 then firstval:=nextenumvalue fi
            lastval:=nextenumvalue

            ++nextenumvalue
            if ncols then               !comma always expected
                checksymbol(commasym)       !check it
                lex()
            fi

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
        lex()                   !should be ( for next entry
        if lx.symbol=closesym then exit fi      !allow trailing comma on last entry
    od

    intabledata:=0

    skipsemi()
    checkbeginend(closesym,ktabledatasym,startline)

!Here, I have::

!enum               1 means enum 0th column present, 0 means not
!ncols              0, or number of actual expression columns
!nrows              Number of data rows
!enumtypename           "", or enum user type name to be created for enum names/values

!varnameptrs[1..ncols]      !Names of the list variables ([]strec]
!varlisttypes[1..ncols]     !Type of each list (or 0 if not provided)
!varelemttypes[1..ncols]    !Type of each element (or 0 if not provided)
!plist[1..ncols]        Each entry is a list of expression units for the column

!enumnames[1..nrows]    When enum=1, list of names/values in 0th column
!enumvalues[1..nrows]

    if nrows=0 then serror("No table data") fi

!for each variable, add a vardef initialised to the list
!add the decls for the vars

    for i:=1 to ncols do

        stvar:=getduplnameptr(owner,varnameptrs[i],staticid)
        stvar.code:=createunit1(jmakelist,plist[i])
        stvar.code.length:=nrows

        storemode(owner,varlisttypes[i],stvar.mode)
        stvar.scope:=scope

        adddef(owner,stvar)
        addstatic(stvar)
    od
end

global proc readclassdef(ref strec owner,int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
    int kwd, baseclass, m, startline, closesym, mrec, normalexit,isrecord, align
    ref strec nameptr, sttype, newd, d,e

    kwd:=lx.symbol
    isrecord:=kwd=krecordsym

    lexchecksymbol(namesym)
    nameptr:=lx.symptr

    lex()
    baseclass:=0
    if lx.symbol=lbracksym then
        lex()
        baseclass:=readtypespec(owner)
        checksymbol(rbracksym)
        lex()
    fi

    checkequals()
    lex()

    align:=0
    if lx.symbol=atsym then
        if lx.subcode=0 then
            lex()
            align:=readconstint()
        else
            lex()
        fi
        align:=1
    fi

    sttype:=getduplnameptr(owner,nameptr,typeid)
    adddef(owner,sttype)
    m:=createusertype(sttype)

    mrec:=createrecordmode(owner, m)
    storemode(owner,mrec,sttype.mode)

    storemode(owner,baseclass,sttype.baseclass)
    sttype.align:=align

    closesym:=checkbegin(1)

    startline:=getcurrline()

    readclassbody(sttype,kwd)

    checkbeginend(closesym,kwd,startline)

    sttype.scope:=scope
end

proc readclassbody(ref strec owner,int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
!classkwd=kclasssym or krecordsym
    int kwd,t
    ref strec d

    unionstr_clear(&unionstring)
    unionstr_clear(&unionpend)

    doswitch lx.symbol
    when kconstsym then
        readconstdef(owner,0)
    when kfunctionsym,kprocsym then
        kwd:=lx.symbol

        if owner.isimport then
            readprocdecl(owner,0,0)
        else
            readprocdef(owner,0)
        fi
    when kclasssym,krecordsym then
        readclassdef(owner,0)

    when ktypesym then
        readtypedef(owner)
    when eofsym then
        serror("Class eof?")
        exit
    when semisym then
        lex()

    when ktabledatasym then
        readtabledef(owner,0)

    when kmacrosym then
        readmacrodef(owner,0)

    when kstructsym,kunionsym then
        unionstr_append(&unionpend,(lx.symbol=kstructsym|'S'|'U'))
        unionlastvar:=nil
        lex()
    when kendsym,rbracksym then
        if unionstring.ulength then
            checkend(kendsym,(unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
            lex()
            if unionlastvar=nil or unionpend.ulength then
                serror("Empty union group")
            fi
            case unionstr_last(&unionlastvar.uflags)
            when 'E','*' then
            else
                unionstr_append(&unionlastvar.uflags,'*')
            esac
            unionstr_append(&unionlastvar.uflags,'E')
            unionstring.ulength--
        else
            exit
        fi

    when kmutsym then

        lex()
        if istypestarter() then
    readmut::
            ++insiderecord
            t:=readtypespec(owner)
            --insiderecord
        else
            t:=tauto
        fi
        readrecordfields(owner,t)

    when kletsym then
        serror("Let not allowed")

    else
        if istypestarter() then
            goto readmut
!       serror("record:need var")
        else
            exit
        fi
    enddoswitch
end

proc readimportmodule(ref strec owner)=
!at 'importmodule' symbol
    int isnew,startline,closesym, libtype
    ref strec d,stname,stname0

    if insidedllimport then serror("nested importdll") fi
    libtype:=lx.subcode

    lex()
    if lx.symbol=stringconstsym then
        stname:=addnamestr(lx.svalue)
    else
        checksymbol(namesym)
        stname:=lx.symptr
    fi

    lex()
    if lx.symbol=lbracksym then
        repeat
            lex()
            checksymbol(namesym)
            addlib(lx.symptr.name,libtype)
            lex()
        until lx.symbol<>commasym
        checksymbol(rbracksym)
        lex()
    fi

    checkequals()
    lex()

!stname points to a nullid symbol
!check whether this module already exists

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

    if isnew then           !new
        stname:=getduplnameptr(stmodule,stname,dllmoduleid)
        adddef(stmodule,stname)
        
        addlib(stname.name, libtype)

        stname.dllindex:=nlibfiles
    fi

    startline:=getcurrline()
    closesym:=checkbegin(0)

    insidedllimport:=stname.dllindex

    readimportbody(owner)

    insidedllimport:=0

    checkbeginend(closesym,kimportmodulesym,startline)

end

proc readimportbody(ref strec owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
    int pos,fflang
    symbol d

    pos:=lx.pos

    do
        skipsemi()
        switch lx.symbol
        when kfflangsym then
            fflang:=lx.subcode
            lex()
            case lx.symbol
            when kprocsym,kfunctionsym then
                goto doproc
            esac

        when kprocsym,kfunctionsym then
            fflang:=0
doproc::
            d:=readprocdecl(owner,0,fflang)
            if ndllproctable>=maxdllproc then
                serror("Too many dll procs")
            fi
            dllproctable[++ndllproctable]:=d

        when ktypesym then
            readtypedef(owner,subprog_scope)

        when kconstsym then
            readconstdef(owner,subprog_scope)

        when kclasssym,krecordsym then
            readclassdef(owner,subprog_scope)

        when kmutsym then
            lex()
            readvardef(owner,subprog_scope,0,dllvarid, kmutsym)

        when stdtypesym,namesym,krefsym,kicharsym,ktypeofsym,lsqsym,
            kdictsym,kslicesym then
            readvardef(owner,subprog_scope,0,dllvarid, 0)

        when eofsym then
            exit

        when kendsym then
            exit
        else
            PS("symbol")
            serror("Not allowed in importmodule")
        endswitch
    od
end

function readequivfield(ref strec owner)ref strec=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
    ref strec p,d

    checksymbol(namesym)
    d:=lx.symptr
    lex()

    p:=owner.deflist
    while p do
        if eqstring(p.name,d.name) then
            return p
        fi

        p:=p.nextdef
    od
    cpl d.name
    serror("Can't find @ field")
    return nil
end

function readrefproc(ref strec owner,int typedefx,int fflang)int=
!'ref' was seen, now positioned at 'proc' 'function' or 'method'
!read proc params and any result, return a complete ref proc spec
    int kwd,prettype,m,varparams,nparams
    [4]int retmodes
    ref strec paramlist,stproc
    int rettype2, rettype3, nretvalues
    ichar name

    kwd:=lx.symbol              !remember whether proc or function
    
    lex()

    paramlist:=nil
    prettype:=tvoid
    nretvalues:=0

!need to create suitable holding typename in advance
    name:=nextautotype()
    stproc:=getduplnameptr(stmodule,addnamestr(name),typeid)
    adddef(stmodule,stproc)
    retmodes[1]:=tvoid

    if kwd=kfunctionsym then
        if lx.symbol=lbracksym then     !possible params
            lex()
            if lx.symbol<>rbracksym then
                paramlist:=readparams(owner,stproc,0,varparams,nparams)
                checksymbol(rbracksym)
            fi
            lex()
            if lx.symbol=colonsym or lx.symbol=sendtosym then
                lex()
                nretvalues:=readreturntype(stproc,retmodes)
            elsif typestarterset[lx.symbol] or lx.symbol=namesym then
                nretvalues:=readreturntype(stproc,retmodes)
            fi
        elsif lx.symbol=colonsym or lx.symbol=sendtosym then
            lex()
            nretvalues:=readreturntype(stproc,retmodes)
        fi
        if nretvalues=0 then
            serror("Function needs return type")
        end

        if nretvalues and kwd=kprocsym then     !proc: result given
            serror("Proc can't return value")
        fi
    else                    !proc with no result
        if lx.symbol=lbracksym then     !possible params
            lex()
            if lx.symbol<>rbracksym then
                paramlist:=readparams(owner,stproc,0,varparams,nparams)
                checksymbol(rbracksym)
            fi
            lex()
        fi
        if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
            serror("proc can't have ret value")
        fi
    fi

    m:=createrefprocmode(owner,stproc,paramlist,kwd,prettype,typedefx)

    storemode(owner,retmodes[1],stproc.mode)
    stproc.nretvalues:=nretvalues

    ttnamedef[m]:=stproc
    stproc.fflang:=fflang

    return m
end

proc pushproc(ref strec p)=
    if nprocstack>=maxprocstack then
        serror("Too many nested proc")
    fi
    procstack[++nprocstack]:=currproc
    currproc:=p
end

proc popproc=
    if nprocstack then
        currproc:=procstack[nprocstack--]
    else
        currproc:=stmodule
    fi
end

function makeastring:unit =
!current symbol is an 'astring', like a regular string constant, but intended
!to be a byte-array
!Simplest treatment, if not the most efficient, is to turn that into normal 
!makelist unit
    unit ulist,ulistx, p, pconst
    ref char s
    int length

    ulist:=ulistx:=nil

    s:=lx.svalue
    length:=astringlength
    to astringlength do
        pconst:=createconstunit(s^,ti64)
        addlistunit(ulist,ulistx,pconst)
        ++s
    od

    if lx.subcode='Z' then
        pconst:=createconstunit(0,ti64)
        addlistunit(ulist,ulistx,pconst)
        ++length
    fi

    p:=createunit1(jmakelist,ulist)
    p.length:=length
    return p
end

function readreturntype(ref strec owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of function decl
    int nretvalues

    retmodes[1]:=readtypespec(owner)
    nretvalues:=1
    while lx.symbol=commasym do
        if nretvalues>=maxtuplesize then
            serror("Too many return values")
        fi
        lex()
        retmodes[++nretvalues]:=readtypespec(owner)
    od

    return nretvalues
end

function readset:unit=
!positioned at "["
    int length,nkeyvalues,oldirp
    unit p,ulist,ulistx

    lex()                   !first symbol of first expression

    case lx.symbol
    when rsqsym then        !empty set, same as 0
        lex()
        return createunit1(jmakeset,nil)
    when colonsym then
        lexchecksymbol(rsqsym)
        lex()
        return createunit1(jmakedict,nil)
    esac

    length:=0
    nkeyvalues:=0

    ulist:=ulistx:=nil

    do
        oldirp:=inreadprint
        inreadprint:=0
        p:=readunit()
        inreadprint:=oldirp
        if p.tag=jkeyvalue then ++nkeyvalues fi
        ++length

        addlistunit(ulist,ulistx,p)

        case lx.symbol
        when commasym then
            lex()
            if lx.symbol=rsqsym then exit fi
        when semisym then
            lexchecksymbol(rsqsym)
            exit
        when rsqsym then
            exit
        else
            serror("readset?")
        esac
        skipsemi()                      !allow a,b,c;]
    od
    lex()

    if nkeyvalues then
        if length>nkeyvalues then serror("dict: mixed elements") fi
        p:=createunit1(jmakedict,ulist)
    else
        p:=createunit1(jmakeset,ulist)
    fi
    p.length:=length
    return p
end

function istypestarter:int=
    if typestarterset[lx.symbol] then return 1 fi
    if lx.symbol=namesym then               !name ...
        case nextlx.symbol
        when namesym then                   !name name
            return 1
        when addrsym then
            return 1
        esac
    fi
    return 0
end

global function readunit:unit p=
    unit pt
    int pos

    pt:=nil
    pos:=lx.pos
    pt:=readterm2()

    if jisexpr[pt.tag]=0 then
        return pt
    fi

    if lx.symbol=assignsym then
        lex()
        p:=readterm2()
        p:=createunit2(jassign, pt, readassignment(p))
    else
        p:=readassignment(pt)
        p.pos:=pos
    fi

    while lx.symbol=pipesym do
        lex()
        p:=createunit2(jcallfn, readassignment(), p)
    od

    return p
end

function readassignment(unit pt=nil)unit p=
    int pos,opc
    unit q

    p:=readorterms(pt)

    if (opc:=lx.symbol) in [assignsym, deepcopysym] then
        pos:=lx.pos
        lex()
        if lx.symbol=kemptysym then
            p:=createunit1(jempty, p)
            lex()
        else
            q:=readassignment(nil)
            if opc=deepcopysym then
                q:=createunit1(jcopy,q)
            fi
            p:=createunit2(jassign,p,q)
!               p:=createunit2((opc=assignsym|jassign|jdeepcopy),p,readassignment(nil))
        fi
        p.pos:=pos
    fi

    return p
end

function readorterms(unit pt=nil)unit p=
    int pos

    p:=readandterms(pt)

    while lx.symbol=orlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jbinto,p,readassignment())
            p.pclop:=korlto
            p.pos:=pos
            exit
        fi

        p:=createunit2(jorl,p,readandterms())
        p.pclop:=korl
        p.pos:=pos
    od

    return p
end

function readandterms(unit pt=nil)unit p=
    int pos

    p:=readcmpterms(pt)

    while lx.symbol=andlsym do
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jbinto,p,readassignment())
            p.pclop:=kandlto
            p.pos:=pos
            exit
        fi

        p:=createunit2(jandl,p,readcmpterms())
        p.pclop:=kandl
        p.pos:=pos
    od

    return p
end

function readcmpterms(unit pt=nil)unit p=
    int pos,opc,n
    unit ulist,ulistx,q
    [4]byte genops

    p:=readinterms(pt)

    if lx.symbol not in [eqsym,cmpsym] then
        return p
    fi

    ulist:=ulistx:=p
    p:=createunit1(jcmpchain,p)
    n:=0                !n counts operand after the first
    clear genops

    doswitch lx.symbol
    when eqsym, cmpsym then
        ++n
        if n>genops.len then serror("cmpchain: Too many items") fi
        genops[n]:=lx.subcode

        pos:=lx.pos
        lex()

        q:=readinterms()
        addlistunit(ulist,ulistx,q)
        q.pos:=pos
    else
        exit
    end doswitch

    if n=1 then
        p.tag:=jcmp
        q:=p.a
        p.pclop:=genops[1]
        p.b:=q.nextunit
        q.nextunit:=nil
    else
        p.cmpgenop:=genops
    fi

    return p
end

function readinterms(unit pt=nil)unit p=
    int pos,opc
    p:=readrangeterm(pt)

    doswitch lx.symbol
    when insym, notinsym then
        opc:=lx.subcode

        pos:=lx.pos
        lex()

        p:=createunit2(jbin,p,readrangeterm())
        p.pclop:=opc
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readrangeterm(unit pt=nil)unit p=
    int pos,opc
    p:=readaddterms(pt)

    if lx.symbol=rangesym then
        pos:=lx.pos
        lex()
        p:=createunit2(jmakerange,p,readaddterms())
        p.pos:=pos
    fi

    return p
end

function readaddterms(unit pt=nil)unit p=
    int pos,sym, tag, genop
    p:=readmulterms(pt)

    doswitch sym:=lx.symbol
    when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym then
        pos:=lx.pos
        genop:=lx.subcode
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jbinto,p,readassignment())
            p.pclop:=symbolgentoops[sym]
            p.pos:=pos
            exit
        fi

        p:=createunit2(jbin,p,readmulterms())
        p.pclop:=symbolgenops[sym]
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readmulterms(unit pt=nil)unit p=
    int pos,sym

    p:=readpowerterms(pt)

    doswitch sym:=lx.symbol
    when mulsym, divsym, idivsym, iremsym, shlsym, shrsym, idivremsym then
        pos:=lx.pos
        lex()

        if lx.symbol=assignsym then
            lex()
            p:=createunit2(jbinto,p,readassignment())
            p.pclop:=symbolgentoops[sym]
            p.pos:=pos
            exit
        fi

        p:=createunit2(jbin,p,readpowerterms())
        p.pclop:=symbolgenops[sym]
        p.pos:=pos
    else
        exit
    end doswitch

    return p
end

function readpowerterms(unit p=nil)unit=
    int pos

    if p=nil then
        p:=readterm2()
    fi

    while lx.symbol=powersym do
        pos:=lx.pos
        lex()
        p:=createunit2(jbin,p,readpowerterms())
        p.pclop:=kpower
        p.pos:=pos
    od

    return p
end

function readterm2:unit=
    unit p,q,r
    ref char pbyte
    word64 a
    int oldipl,opc,oldinrp,pos,shift,t

    pos:=lx.pos

    p:=readterm()

    doswitch lx.symbol
    when lbracksym then
        lex()
        oldinrp:=inreadprint
        inreadprint:=0
        q:=readslist(1,1)
        checksymbol(rbracksym)
        lex()
        if p.tag=jsyscall then
            p.a:=q
        else
            p:=createunit2(jcallfn,p,q)
        fi
        inreadprint:=oldinrp
        p:=readcondsuffix(p)

    when ptrsym then
        p:=createunit1(jptr,p)
        lex()

    when lsqsym then
        p:=readindex(p,0)

    when dotsym then
        p:=readdotsuffix(p)

    when colonsym then
        if inreadprint then exit fi
        lex()
        q:=readunit()
        p:=createunit2((inparamlist|jkeyword|jkeyvalue),p,q)

    when incrsym then
        case lx.subcode
        when kincr then opc:=kloadincr
        when kdecr then opc:=kloaddecr
        esac
        lex()
        p:=createunit1(jincr,p)
        p.pclop:=opc

!   when anddotsym then
!       lexchecksymbol(lsqsym)
!       lex()
!       q:=readunit()
!       if q.tag=jmakerange then
!           p:=createunit2(janddotslice,p,q)
!       else
!           p:=createunit2(janddotindex,p,q)
!       fi
!       checksymbol(rsqsym)
!       lex()

    when lcurlysym then
        serror("X{...} not ready")
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
    int oldipl,opc,oldinrp,pos,shift,t,length

    pos:=lx.pos

    switch lx.symbol
    when namesym then
        if nextlx.symbol=atsym then     !type-punning with user type
            p:=readcast()
        else
            p:=createname(lx.symptr)
            p.pos:=lx.pos
            lex()
        fi

    when intconstsym,realconstsym then
        p:=createconstunit(lx.value,lx.subcode)
        p.istrueconst:=1
        lex()

    when stringconstsym then
        p:=createstringconstunit(lx.svalue,-1)
        lex()

    when astringconstsym then
        p:=makeastring()
        lex()

    when decimalconstsym then
        SERROR("DEC CONST")

    when charconstsym then
        length:=strlen(lx.svalue)
        if length>8 then serror("Char const too long") fi
        a:=0
        if length then
            memcpy(&a,lx.svalue,length)
        fi
        p:=createconstunit(a,tc64)
        p.istrueconst:=1
        lex()

    when lbracksym then
        p:=readlbrack()

    when stdtypesym,krefsym,kicharsym,ktypeofsym then
        p:=readcast()

    when addsym, subsym, minsym, maxsym, abssym, inotsym,
        mathsopsym, sqrtsym, sqrsym, maths2opsym,signsym then
        p:=readopc()

    when notlsym then
        if nextlx.symbol=assignsym then
            p:=readopc()
        else
            lex()
            p:=createunit1(jnotl, readterm2())
            p.pclop:=knotl
        fi

    when istruelsym then
        if nextlx.symbol=assignsym then
            p:=readopc()
        else
            lex()
            p:=createunit1(jistruel, readterm2())
            p.pclop:=kistruel
        fi

    when lsqsym then
        p:=readset()

    when incrsym then
        opc:=lx.subcode
        lex()
        p:=createunit1(jincr,readterm2())
        p.pclop:=opc

    when ksprintsym then
        p:=readsprint()

    when ksreadsym,ksreadlnsym then
        p:=readsread()

    when addrsym,daddrsym then
        opc:=lx.subcode
        lex()
        p:=createunit1(opc,readterm2())
        if p.a.tag=jcallfn then
            if p.a.b then
                serror("Params not allowed")
            fi
            p.a:=p.a.a          !lose the call
        fi

!   when ptrsym then
!       lex()
!       p:=createunit1(jaddrvar,readterm2())
!
    when anddotsym then
        lex()
        p:=createunit1(jaddroffirst,readterm2())

    when compilervarsym then
        p:=readcompilervar()

    when kerrorsym then
        p:= createconstunit(lx.subcode,tint)
        lex()

    when dollarsym then
        if intabledata then
            if not tabledataname then serror("$: no enum") fi
            p:=createstringconstunit(tabledataname,-1)
        else
            if ndollar<=0 then
                serror("[$] No array")
            fi
            p:=createunit1(junary,dollarstack[ndollar])
            p.pclop:=kupb
        fi
        lex()

    when kcastsym then
        p:=readcastx()

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

        q:=createunit2(jbin,p,q)
        q.pclop:=kmax
        p:=createunit2(jbin,q,r)
        p.pclop:=kmin

    when kgotosym then
        p:=readgoto()

    when kifsym then
        p:=readif()

    when kunlesssym then
        p:=readunless()

    when kcasesym,kdocasesym,kswitchsym,kdoswitchsym then
        p:=readswitchcase()

    when krecasesym then
        p:=readrecase()

    when kforsym then
        p:=readfor()

    when ktosym then
        p:=readto()

    when kdosym then
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

    when kswapsym then          !swap using function syntax
        lexchecksymbol(lbracksym)
        lex()
        p:=readunit()
        checksymbol(commasym)
        lex()
        q:=readunit()
        checksymbol(rbracksym)
        lex()
        p:=createunit2(jswap,p,q)

    when kevalsym then
        opc:=lx.subcode
        lex()
        p:=createunit1(jeval,readunit())
        p.index:=opc

    when kassemsym then
        currproc.asmused:=1
        assemmode:=1
        if lx.subcode=0 then
            p:=readassemline()
        else
            p:=readassemblock()
        fi
        assemmode:=0

    when ksyscallsym then
        p:=createunit0(jsyscall)
        p.fnindex:=lx.subcode
        lex()

    when kstrincludesym then
        lex()
        p:=createunit1(jstrinclude,readterm2())

    when kemitcsym then
        if not ctarget then serror("emitc?") fi
        lex()
        checksymbol(stringconstsym)
        p:=createstringconstunit(lx.svalue,-1)
        p.tag:=jemitc
        lex()

    when kemptysym then
        lex()
        p:=createunit1(jempty, readterm2())

    when kcopysym then
        lex()
        p:=createunit1(jcopy, readterm2())

    when lcurlysym then
        serror("{...} not ready")

    else
        cpl symbolnames[lx.symbol],=LX.SYMBOL, ISTYPESTARTER()
        serror("readterm?")
    endswitch

    p.pos:=pos
    return p
end

proc readmacrodef(ref strec owner, int scope)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd,varparams,try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!ref strec pequiv, stproc, owner, paramlist,nameptr

    ref strec nameptr,stmacro, paramlist,paramlistx, stname

    lexchecksymbol(namesym)

    nameptr:=lx.symptr
    stmacro:=getduplnameptr(owner,nameptr,macroid)
    adddef(owner,stmacro)

    owner:=stmacro

    lex()

    paramlist:=paramlistx:=nil

    if lx.symbol=lbracksym then         !may have parameters
        lex()
        if lx.symbol<>rbracksym then
            do
                case lx.symbol
                when namesym then
                    stname:=getduplnameptr(owner,lx.symptr,macroparamid)
                    adddef(owner,stname)
                    addlistparam(&paramlist,&paramlistx,stname)

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
        lex()                       !skip )
    fi
    stmacro.paramlist:=paramlist
    stmacro.scope:=scope

    checkequals()
    lex()
    stmacro.code:=readunit()
end

function readrecase:unit=
    lex()
    if lx.symbol=kelsesym then
        lex()
        return createunit0(jrecase)
    else
        return createunit1(jrecase,readunit())
    fi
end

proc adddocstring(ichar s)=
    if ndocstrings>docstrings.len then
        serror("Too many docstrings")
    fi
    docstrings[++ndocstrings]:=pcm_copyheapstringn(s,strlen(s))
end

function fixcond(unit p)unit=
    if p.tag=jblock and p.a=nil then serror("Empty cond") fi
    if not isbooltag[p.tag] then
        insertunit(p, jistruel)
        p.pclop:=kistruel
    fi
    return p
end

function readsunit(int inwhile=0)unit=
    int pos,m,sym,opc
    unit ulist,ulistx,p,q,r
    ref strec stname

    pos:=lx.pos
    ulist:=ulistx:=nil

    repeat
        while lx.symbol=semisym do
            lex()
        od
        switch lx.symbol
        when kstaticsym then
            lex()
            if lx.symbol in [kletsym,kmutsym] then
                opc:=lx.symbol
                lex()
            else
!           opc:=kmutsym
                opc:=0
            fi
            readvardef(currproc,0,1,staticid,opc)

        when kprocsym,kfunctionsym then
            readprocdef(currproc,0)

        when stdtypesym,krefsym,kicharsym,ktypeofsym,kdictsym,kslicesym,lsqsym then
            if nextlx.symbol in [lbracksym, atsym, dotsym] then     !is a cast etc
                goto doexec
            else
                sym:=0
                goto dovar
            fi

        when karraysym then
            lexchecksymbol(lsqsym)
            sym:=0
            goto dovar

        when kmutsym,kletsym then
            sym:=lx.symbol
            lex()
    dovar::
            q:=readvardef(currproc,0,0,frameid,sym)
            while q do                              !initialised decls involve code
                r:=q.nextunit                       !unlink from this block first
                q.nextunit:=nil
                addlistunit(ulist,ulistx,q)     !add one by-one
                q:=r
            od

        when ktypesym then
            readtypedef(currproc,0)

        when kconstsym then
            readconstdef(currproc,0)

        when kclasssym,krecordsym then
            readclassdef(currproc,0)

        when docstringsym then
            adddocstring(lx.svalue)
            lex()

        when kmacrosym then
            readmacrodef(currproc,0)

        when ktabledatasym then
            readtabledef(currproc,0)

        when eofsym then
            cpl currproc.name
            serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
        when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,
                kelsecasesym,kelseswitchsym,kendsym then
            exit
!
        when namesym then
            case nextlx.symbol
            when dcolonsym then
                p:=createunit0(jlabeldef)
                stname:=getduplnameptr(currproc,lx.symptr,labelid)
                adddef(currproc,stname)
                p.def:=stname
!               p.trylevel:=try_level
                lex()
                lx.symbol:=semisym
                addlistunit(ulist,ulistx,p)
            when namesym then
                sym:=kmutsym
                goto dovar
            goto doexec

            else
                goto doexec
            esac
        when kdosym then                !u;u;u;do rather than u;u;u do
            if inwhile then
                exit
            fi
            goto doexec

        when semisym then

        when kstepsym then
            exit

        else                            !assume a statement
    doexec::
            p:=readunit()
    doexec2::
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
    when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,
        kelsecasesym,kelseswitchsym,kendsym,commasym,
        barsym, kstepsym then
    else
        serror("Readsunit: "";"" expected, or bad unit starter")
    esac

    if ulist=nil or ulist.nextunit then
        return createunit1(jblock,ulist)
    else
        return ulist
    fi
end

=== mm_support.m 0 0 16/33 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global function loadsourcefile(ichar filespec)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
    ichar s,basefilename

    if nsourcefiles>maxsourcefile then
        loaderror("Too many source files")
    fi

    basefilename:=extractfile(filespec)

    ++nsourcefiles
    sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(filespec)
    sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(extractpath(filespec))
    sourcefilenames[nsourcefiles]:=pcm_copyheapstring(basefilename)

    s:=cast(readfile(filespec))         !will overallocate by a few bytes

    if not s then               !unexpected error
        loaderror("LSF can't load ",filespec)
    fi
    sourcefiletext[nsourcefiles]:=s

    if fwritema then
        sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(s)
    fi

    sourcefilesizes[nsourcefiles]:=rfsize
    (s+rfsize)^:=0              !replace etx,0 by 0,0 (effectively, just zero)
    return nsourcefiles
end

function loadbundledfile(ichar filespec,int issyslib=0,support=0)int fileno=
!loading bundled file
!Name of header is in 'file'.
    ichar file

    file:=extractfile(filespec)

    for i to nsourcefiles do
        if eqstring(file,sourcefilenames[i]) and support=sourcefilesupport[i] then      !found
            return i
        fi
    od

    fileno:=findsyslib(file)
    if fileno then
        return fileno
    fi

    if not issyslib then
        loaderror("Can't find bundled file: ##",filespec)
    fi

!syslib not found, use normal means to find it
    return 0
end

global proc mcerror(ichar mess)=
    println "MC Error:",mess

    stop 1
end

global proc serror_gen(ichar mess)=

!   println
    showdivider('*')
    println "Syntax Error:"

    showerrorsource(lx.pos, currproc)

    println mess

    stopcompiler(sourcefilespecs[lx.fileno],getlineno(lx.pos))
end

proc showdivider(char64 ch)=
    to 87 do
        print ch
    od
    println
end

proc showerrorsource(int pos, symbol stproc=nil)=
    int fileno:=getfileno(pos), lineoffset
    ichar errorline,s

    fprintln "    Line:     #",getlineno(pos)
    if stproc and stproc.nameid=procid then
        fprintln "    Function: #()", stproc.name
    fi
    fprintln "    Module:   # (#)", sourcefilenames[fileno],sourcefilespecs[fileno]
    showdivider('-')

    s:=errorline:=getsourceline(pos)
    lineoffset:=getsourcepos(pos)-errorline

    to 6 do print " " od
    while s^ not in [10,0] do
        print s++^
    od
    println
    s:=errorline
    to 6 do print " " od
    to lineoffset do
        if s^=9 then print '\t' else print ' ' fi
        ++s
    od
    println "^"
    showdivider('-')
end

global proc stopcompiler(ichar filename,int lineno)=
    filehandle f
    f:=fopen("$error.tmp","w")
    println @f,filename,lineno
    fclose(f)
    println
    println
    stop 1
end

global proc serror(ichar mess)=

    serror_gen(mess)
end

global proc serror_s(ichar mess,a)=
    [256]char str
    fprint @&.str,mess,a
    serror_gen(&.str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
    int pos

    if p then
        pos:=p.pos
    else
        pos:=mlineno
    fi

    showdivider('*')
    case pass
    when 'N' then println "RX Name Error: "
    when 'T' then println "TX Type Error: "
    when 'G' then println "GX Code Gen Error: "
    when 'A' then println "AX Code Gen Error: "
    esac

    showerrorsource(pos, currproc)

    println mess

    stopcompiler(sourcefilespecs[getfileno(pos)],getlineno(pos))
end

global proc rxerror(ichar mess,unit p=nil)=
    error_gen('N',mess,p)
end

global func gerror(ichar mess,unit p=nil)int=
    error_gen('G',mess,p)
    0
end

global proc axerror(ichar mess)=
    PRINTLN =ALINENO
    error_gen('A',mess)
end

global proc txerror(ichar mess,unit p=nil)=
    error_gen('T',mess,p)
end

global proc txerror_s(ichar mess,a,unit p=nil)=
    [256]char str
    fprint @&.str,mess,a
    error_gen('T',&.str,p)
end

global proc txerror_ss(ichar mess,a,b)=
    [256]char str
    fprint @&.str,mess,a,b
    error_gen('T',&.str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
    [256]char str
    fprint @&.str,mess,a
    error_gen('N',&.str,p)
end

global proc gerror_s(ichar mess,s,unit p=nil)=
    [256]char str

    fprint @&.str,mess,s
    error_gen('G',&.str,p)
end

global proc gerror_t(ichar mess, unit p)=
    [256]char str

    fprint @&.str,mess,strmode(p.mode)
    error_gen('G',&.str,p)
end

global proc lxerror_gen(ichar mess)=

    println "On line",getlineno(lx.pos),"in file",sourcefilespecs[lx.fileno]

    println
    println "**** Lex Error:",mess,"****"
    println

    stopcompiler(sourcefilespecs[lx.fileno],getlineno(lx.pos))
end

global proc lxerror(ichar mess)=
    lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
    [512]char str
    if strchr(mess,'#') then
        fprint @str,mess,mess2,mess3
    else
        print @str,mess
    fi

    println "Load Error:",str
    println "Stopping"
    stop 1
end

global proc gs_additem(ref strbuffer dest,ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
    ichar d
    int lastchar,nextchar

    d:=dest^.strptr

    if dest^.length then
        lastchar:=(d+dest^.length-1)^
        nextchar:=s^
        if isalphanum(lastchar) and isalphanum(nextchar) then
            strbuffer_add(dest," ")
        fi
    fi
    strbuffer_add(dest,s)
end

global proc gs_copytostr(ref strbuffer source,ref char s)=
    if source^.length then
        memcpy(s,source^.strptr,source^.length)
        (s+source^.length)^:=0
    else
        s^:=0
    fi
end

global function isalphanum(int c)int=
    if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
        return 1
    fi
    return 0
end

global proc init_tt_tables=
    int i,size,bitsize
    int s,t,u,v

!Initialise type tt-tables from std types first all fields initially zero

    for i:=0 to tlast-1 do

        ttname[i]:=stdnames[i]
        ttbasetype[i]:=i
        bitsize:=stdbits[i]

        switch bitsize
        when 0 then
            size:=0
        when 1,2,4 then
            size:=1
        else
            size:=bitsize/8
        endswitch

        ttsize[i]:=size

        case i
        when ti8,ti16,ti32,ti64 then
            ttsigned[i]:=1
            ttisinteger[i]:=1
        when tu8, tu16, tu32, tu64, tc8, tc64 then
            ttisinteger[i]:=1
        when tr32, tr64 then
            ttisreal[i]:=1
!       when tref, trefchar, trefbit then
        when tref, trefchar then
            ttisref[i]:=1
        esac

        ttisshort[i]:=stdcat[i]=shortcat

        ttlower[i]:=1

        ttcat[i]:=stdcat[i]
        ttisblock[i]:=stdcat[i]=blockcat

    od

    ttbasetype[trefchar]:=tref
    tttarget[trefchar]:=tc8

    ntypes:=tlast-1
end

global proc addspecialtypes=
    trefproc:=createrefmode(nil,tproc,0)
    treflabel:=createrefmode(nil,tlabel,0)
end

global function getsupportfile(ichar filename, ext="", path="",
    int issyslib=0, issupport=0)int =
!filename is a rel/abs/base filespec (rel path, abs path or none)
!syslib=1: look first inside INT list
!syslib=0 or not found in int (or -EXT mode0):
!   fbundled; load from list of bundled files from .ma fle
!   filename has rel path/no path: appl to given path
!   filename has abs path: use as is
!look up filename at that final location only (not multiple places)
!issupport=0 for a support file; helps with bundled files where there may
!be duplicates

    [300]char filespec,filespec2
    ichar file
    int fileno

    file:=filename

if fverbose=3 then
    fprintln "Get file:# (ext:#) (path:#)",filename,ext, path
fi

    if ext^ then
        strcpy(filespec,addext(filename,ext))
        file:=&.filespec
    fi

    if freadma then
        fileno:=loadbundledfile(file,issyslib, issupport)
        return fileno when fileno

    fi
    if issyslib and dointlibs then
        fileno:=findsyslib(file)
if fverbose=3 and fileno then
    fprintln "Found in syslib: #",sourcefilenames[fileno]
fi
        return fileno when fileno
    fi

    if not isabspath(file) then
        strcpy(filespec2,path)
        strcat(filespec2,file)
        file:=&.filespec2
    fi

if fverbose=3 and fileno then
    println "Checkfile:",file
fi
    if file=nil or not checkfile(file) then
        loaderror("Can't find file: # #",filename)
    fi

    fileno:=loadsourcefile(file)
if fverbose=3 and fileno then
    println "Found:",file
fi
    sourcefilesupport[fileno]:=issupport
    sourcefilesys[fileno]:=issyslib
    return fileno
end

function isabspath(ichar filespec)int=
    ichar path:=extractpath(filespec)
    if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then   !absolute path
        return 1
    fi
    return 0
end

global proc initbblib=
    for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global function getfileno(word pos)int fileno=
    fileno:=pos.[24..31]
    if fileno<1 or fileno>nsourcefiles then
RETURN 1
!       abortprogram("No file no")
    fi
    return fileno
end

global function getlineno(word pos)int=
    ichar source := getsourcestart(pos)
    ichar sline:=getsourceline(pos)
    ichar s:=sline
    int lineno:=1

    while s>source do
        if s^=10 then ++lineno fi
        --s
    od

    return lineno
end

function getsourceline(word pos)ichar=
    ichar source := getsourcestart(pos)
    ichar s :=  getsourcepos(pos)

    while s>source and s^<>10 do --s od
    if s^=10 then ++s fi

    return s
end

function getsourcestart(word pos)ichar=
    return sourcefiletext[getfileno(pos)]
end

function getsourcepos(word pos)ichar=
    return sourcefiletext[getfileno(pos)]+pos.[0..23]
end

global proc do_writema=
    [300]char filename
    [maxsourcefile]int sflist
    filehandle f
    int offset, nfiles, fileno

    if not fwritema then
        return
    fi
    strcpy(filename, changeext(sourcefilespecs[1],langextma))

!first build a table of source files to be o/p
    nfiles:=0

    for i to nsourcefiles do
        if sourcefilesys[i] and fwritema=1 then     !no syslibs
            next
        fi

        sflist[++nfiles]:=i
    od

    if nfiles=0 then loaderror(langextmauc+": no files") fi

    f:=fopen(filename,"wb")
    if not f then loaderror("Can't create "+langextmauc+" file #",filename) fi

    println "Writing ",filename
    fprintln @f,"=== "+langextmauc+" # ===",nfiles

    for i to nfiles do
        fileno:=sflist[i]

        fprintln @f,"=== # # # #/# ===",
            sourcefilenames[fileno],
            sourcefilesys[fileno],
            sourcefilesupport[fileno],
            i,nfiles

        offset:=getfilepos(f)
        writerandom(f,cast(sourcefiledupl[fileno]),offset,sourcefilesizes[fileno])
    od

    println @f,"=== END ==="

    for i to nfiles do
        fprintln @f,"# # #",i,sourcefilenames[sflist[i]],
            sourcefilesys[sflist[i]],
            sourcefilesupport[sflist[i]]
    od

    fclose(f)
end

=== mm_tables.m 0 0 17/33 ===
include "mm_types.m"

global enumdata []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
    (sf_init,               $,  0,  0),
    (sf_print_startfile,    $,  0,  0),
    (sf_print_startstr,     $,  0,  0),
    (sf_print_startptr,     $,  0,  0),
    (sf_print_startcon,     $,  0,  0),
    (sf_print_setfmt,       $,  0,  0),
    (sf_print_nogap,        $,  0,  0),
    (sf_print_space,        $,  0,  0),
    (sf_print_i64,          $,  0,  0),
    (sf_print_i64_nf,       $,  0,  0),
    (sf_print_u64,          $,  0,  0),
    (sf_print_r64,          $,  0,  0),
    (sf_print_r32,          $,  0,  0),
    (sf_print_str,          $,  0,  0),
    (sf_print_str_nf,       $,  0,  0),
    (sf_print_strsl,        $,  0,  0),
    (sf_print_ptr,          $,  0,  0),
    (sf_print_ptr_nf,       $,  0,  0),
    (sf_print_c8,           $,  0,  0),
    (sf_print_bool,         $,  0,  0),
!   (sf_print_var,          $,  0,  0),
    (sf_print_newline,      $,  0,  0),
    (sf_print_end,          $,  0,  0),
    (sf_read_i64,           $,  0,  0),
    (sf_read_r64,           $,  0,  0),
    (sf_read_str,           $,  0,  0),
    (sf_read_fileline,      $,  0,  0),
    (sf_read_strline,       $,  0,  0),
    (sf_read_conline,       $,  0,  0),

    (sf_get_nprocs,         $,  0,  1),     !access functions
!   (sf_getnexports,        $,  0,  1),
    (sf_get_procname,       $,  0,  1),
    (sf_get_procaddr,       $,  0,  1),

    (sf_gettttable,         $,  0,  1),
    (sf_getsttable,         $,  0,  1),
    (sf_getfftable,         $,  0,  1),

    (sf_power_i64,          $,  0,  1),
    (sf_unimpl,             $,  0,  1),

end
!
global [sysfnnames.len]symbol sysfnhandlers

global int mlineno


!!---
global enumdata [0:]ichar jtagnames,
                   [0:]byte jsubs, [0:]byte jisexpr =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!   L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!   u means .a/b/c pointing to a single unit
!   u/nil means can be nil

![a=u] means a is a unit/list, or is nil

    (jnone=0,       $,  0,      0), ! For tagname lookups when tag is zero
    (jconst,        $,  0,      3), ! value/etc=value, typeno=type code
    (jnull,         $,  0,      3), ! Place holder unit: means 'param no present' when used where a param is expected
!   (jvoidvar,      $,  0,      3), ! create void variant
    (jname,         $,  0,      3), ! def=nameptr
!   (jnamelv,       $,  0,      3), ! def=nameptr
    (jblock,        $,  1,      0), ! a=L
!   (jdecimal,      $,  0,      3), ! svalue=str, slength
    (jassem,        $,  3,      0), ! svalue=str, slength
    (jassemmacro,   $,  0,      0), !
    (jassemreg,     $,  0,      0), !
    (jassemxreg,    $,  0,      0), !
    (jassemmem,     $,  1,      0), !
    (jstrinclude,   $,  1,      0), !

!Logical Operators

    (jandl,         $,  2,      2), ! A B   This group are for conditional expressions (no result)
    (jorl,          $,  2,      2), ! A B
!   (jxorl,         $,  0,      2), ! A B
    (jnotl,         $,  1,      1), ! a
    (jistruel,      $,  1,      1), ! a

!Expressions and Operators

    (jmakelist,     $,  2,      3), ! a=L, b=[u], length=N; element list/lower bound expr
    (jmakerange,    $,  2,      3), ! A B
    (jmakeset,      $,  1,      3), ! a=L, length=N
    (jmakedict,     $,  1,      3), !
    (jmakeslice,    $,  1,      3), !
    (jreturnmult,   $,  1,      3), !

    (jkeyword,      $,  1,      3), ! def=st entry
    (jkeyvalue,     $,  2,      3), ! A B
    (jassign,       $,  2,      3), ! A B a := x
    (jassignmm,     $,  2,      3), ! A B (a,b,c) := (x,y,z)
    (jassignms,     $,  2,      3), ! A B (a,b,c) := x
    (jassignmdrem,  $,  2,      3), ! A B (a,b) := x divrem y
    (jcopy,         $,  2,      3), ! A B
    (jcallfn,       $,  2,      3), ! A B

    (jcmp,          $,  2,      2), ! A B
    (jcmpchain,     $,  2,      1), ! A B
    (jbin,          $,  2,      2), ! A B
    (junary,        $,  2,      1), ! A B
    (jbinto,        $,  2,      2), ! A B
    (junaryto,      $,  1,      1), ! A B
    (jincr,         $,  1,      3), ! a ++a

    (jinrev,        $,  2,      2), ! A B
    (jinrange,      $,  2,      2), ! A B
    (jinset,        $,  2,      2), ! A B
    (jclamp,        $,  3,      2), ! A B

!   (jstringz,      $,  0,      3), ! A B

    (jindex,        $,  2,      3), ! A B       a[b]
    (jslice,        $,  2,      3), ! A B       a[b]
    (jdot,          $,  2,      3), ! A B opcode    a.b; opcode=0/1/2 used for signalling in rx pass
    (jdotindex,     $,  2,      3), ! A B       a[b]
    (jdotslice,     $,  2,      3), ! A B       a[b]

    (jptr,          $,  1,      3), ! a     a^
    (jaddrof,       $,  1,      3), ! a     &a
    (jaddroffirst,  $,  1,      3), ! a     &a
    (jconvert,      $,  1,      3), ! typeno=T a        T(a)            T
    (jshorten,      $,  1,      3), !
    (jautocast,     $,  1,      3), ! typeno=T a        T(a)            T
    (jtypepun,      $,  1,      3), ! typeno=T a        T@(a)           T
    (jtypeconst,    $,  0,      3), ! typeno=T          typeconst(T)
    (joperator,     $,  0,      3), ! opcode=opc
    (jupper,        $,  1,      3), ! a     $                   T

    (jbitwidth,     $,  1,      1), ! a
    (jbytesize,     $,  1,      1), ! a
    (jtypeof,       $,  1,      3), ! a
    (jtypestr,      $,  0,      1), ! a
!   (jsliceptr,     $,  0,      1), ! a
    (jbitfield,     $,  1,      3), ! a

    (jminvalue,     $,  1,      3), ! a
    (jmaxvalue,     $,  1,      3), ! a

!Translator Variables

    (jcvlineno,     $,  0,      3), !
    (jcvstrlineno,  $,  0,      3), ! 
    (jcvmodulename, $,  0,      3), ! 
    (jcvfilename,   $,  0,      3), ! 
    (jcvfunction,   $,  0,      3), ! 
    (jcvdate,       $,  0,      3), ! 
    (jcvtime,       $,  0,      3), ! 
    (jcvversion,    $,  0,      3), ! 
    (jcvtypename,   $,  0,      3), ! 
    (jcvtargetbits, $,  0,      3), ! 
    (jcvtargetsize, $,  0,      3), ! 
    (jcvtargetcode, $,  0,      3), ! 
    (jcvctarget,    $,  0,      3), ! 
    (jcvwindows,    $,  0,      3), ! 
    (jcvlinux,      $,  0,      3), ! 
    (jcvnil,        $,  0,      3), ! 
    (jcvpi,         $,  0,      3), ! 
    (jcvinfinity,   $,  0,      3), ! 
    (jcvtrue,       $,  0,      3), ! 
    (jcvfalse,      $,  0,      3), ! 

    (jwhenthen,     $,  2,      0), ! a=L b=u
    (jfmtitem,      $,  2,      3), ! A B  x/fmtstr
    (jnogap,        $,  0,      3), ! 
    (jspace,        $,  0,      3), ! 

!Statements

    (jcallproc,     $,  2,      0), ! a=fn b=L, length
    (jreturn,       $,  1,      0), ! a=x/nil
    (jsyscall,      $,  1,      3), ! a=x or nil

    (jto,           $,  3,      0), ! a=N, b=body, c=tempvar/nil, def=name
    (jif,           $,  3,      3), ! condcode a=then b=else
    (jforup,        $,  3,      0), ! 
    (jfordown,      $,  3,      0), !
    (jforall,       $,  3,      0), !
    (jforallrev,    $,  3,      0), !
    (jwhile,        $,  3,      0), ! a=x b=u
    (jrepeat,       $,  2,      0), ! a=u b=x
    (jgoto,         $,  1,      0), ! a=x
    (jlabeldef,     $,  0,      0), ! def=nameptr
    (jredo,         $,  0,      0), ! [a=x]
    (jnext,         $,  0,      0), ! [a=x]
    (jexit,         $,  0,      0), ! [a=x]
    (jdo,           $,  1,      0), ! [a=u
    (jcase,         $,  3,      3), ! a=x b=L [c=else]      L is series of whenthen pairs
    (jdocase,       $,  3,      0), ! a=x b=L [c=else]
    (jswitch,       $,  3,      3), ! a=x b=L [c=else]
    (jdoswitch,     $,  3,      0), ! a=x b=L [c=else]
    (jswap,         $,  2,      0), ! A B
    (jselect,       $,  3,      3), ! Not implemented
    (jrecase,       $,  1,      0), ! Not implemented
!   (jrecaseelse,   $,  0,      0), ! Not implemented

    (jprint,        $,  2,      0), ! [a=dev] b=L
    (jprintln,      $,  2,      0), ! [a=dev] b=L
    (jfprint,       $,  3,      0), ! [a=dev] b=fmtstr c=L
    (jfprintln,     $,  3,      0), ! [a=dev] b=fmtstr c=L
    (jsprint,       $,  2,      0), !         b=L 
    (jsfprint,      $,  2,      0), !         b=L
    (jread,         $,  2,      0), ! [a=dev] b=L
    (jreadln,       $,  2,      0), ! [a=dev] b=L
    (jsread,        $,  2,      0), ! [a=dev] b=L
    (jsreadln,      $,  2,      0), ! [a=dev] b=L
    (jstop,         $,  1,      0), ! [a=x]
    (jeval,         $,  1,      3), ! "
!   (jstack,        $,  1,      0), ! "
!   (junstack,      $,  1,      0), ! "
    (jempty,        $,  1,      1), ! "
    (jemitc,        $,  0,      0), ! "
    (jinfinity,     $,  0,      0), ! "

    (jdummy,        $,  0,      3)
end

global enumdata []ichar bitfieldnames=
    (bf_msb,        $),
    (bf_lsb,        $),
    (bf_msbit,      $),
    (bf_lsbit,      $),
    (bf_msw,        $),
    (bf_lsw,        $),
    (bf_odd,        $),
    (bf_even,       $),
end

global enumdata [0:]ichar optypenames =
    (no_op=0,       $),
    (bin_op,        $),
    (mon_op,        $),
    (prop_op,       $),
end

!!---
global enumdata []ichar symbolnames,
                    []byte symboloptypes,
                    []byte symbolgenops,
                    []byte symbolgentoops,
                    []byte symbolopprios,
                    []byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
    (errorsym,          $,          0,  0,  0,  0,  0),     ! Lex error
    (dotsym,            ".",        0,  0,  0,  0,  0),     ! "."
    (lexdotsym,         $,          0,  0,  0,  0,  0),     ! ".", used at bol to prefix lexical 
    (anddotsym,         "&.",       0,  0,  0,  0,  1),     ! "&."
    (commasym,          ",",        0,  0,  0,  0,  0),     ! ","
    (semisym,           ";",        0,  0,  0,  0,  0),     ! ";"
    (colonsym,          ":",        0,  0,  0,  0,  0),     ! ":"
    (dcolonsym,         "::",       0,  0,  0,  0,  0),     ! "::"
    (assignsym,         ":=",       bin_op, 0,  0,  1,  0),     ! :=
    (deepcopysym,       "::=",      0,  0,  0,  1,  0),     ! ::=
    (sendtosym,         "=>",       0,  0,  0,  0,  0),     ! =>
    (pipesym,           "->",       0,  0,  0,  0,  0),     ! ->
    (lbracksym,         "(",        0,  0,  0,  0,  1),     ! (
    (rbracksym,         ")",        0,  0,  0,  0,  0),     ! )
    (lsqsym,            "[",        0,  0,  0,  0,  1),     ! [
    (rsqsym,            "]",        0,  0,  0,  0,  0),     ! ]
    (lcurlysym,         "{",        0,  0,  0,  0,  0),     ! {
    (rcurlysym,         "}",        0,  0,  0,  0,  0),     ! }
    (ptrsym,            "^",        0,  0,  0,  0,  1),     ! ^
    (barsym,            "|",        0,  0,  0,  0,  0),     ! |
    (dbarsym,           "||",       0,  0,  0,  0,  0),     ! ||
    (atsym,             "@",        0,  0,  0,  0,  0),     ! @
    (datsym,            "@@",       0,  0,  0,  0,  0),     ! @@
    (questionsym,       "?",        0,  0,  0,  0,  0),     ! ?
    (addrsym,           "&",        0,  0,  0,  0,  1),     ! &
    (daddrsym,          "&&",       0,  0,  0,  0,  0),     ! &&
    (curlsym,           "~",        0,  0,  0,  0,  0),     ! ~
    (rangesym,          "..",       bin_op, 0,  0,  5,  0),     ! ..
    (ellipsissym,       "...",      0,  0,  0,  0,  0),     ! ...
    (hashsym,           "#",        0,  0,  0,  0,  0),     ! #

!   (opsym,             $,      0,  0,  0,  0,  0),     ! Any operator or property tag (use sets to distinguish)

    (addsym,            "+",        bin_op,     kadd,       kaddto,     4,  1),
    (subsym,            "-",        bin_op,     ksub,       ksubto,     4,  1),
    (mulsym,            "*",        bin_op,     kmul,       kmulto,     3,  0),
    (divsym,            "/",        bin_op,     kdiv,       kdivto,     3,  0),
    (idivsym,           "%",        bin_op,     kidiv,      kidivto,    3,  0),
    (iremsym,           "rem",      bin_op,     kirem,      kiremto,    3,  0),
    (idivremsym,        "rem",      bin_op,     kidivrem,   0,          3,  0),
    (iandsym,           "iand",     bin_op,     kiand,      kiandto,    4,  0),
    (iorsym,            "ior",      bin_op,     kior,       kiorto,     4,  0),
    (ixorsym,           "ixor",     bin_op,     kixor,      kixorto,    4,  0),
    (shlsym,            "<<",       bin_op,     kshl,       kshlto,     3,  0),
    (shrsym,            ">>",       bin_op,     kshr,       kshrto,     3,  0),
    (minsym,            "min",      bin_op,     kmin,       kminto,     4,  1),
    (maxsym,            "max",      bin_op,     kmax,       kmaxto,     4,  1),
    (andlsym,           "and",      bin_op,     kandl,      kandlto,    7,  0),
    (orlsym,            "or",       bin_op,     korl,       korlto,     8,  0),
    (xorlsym,           "xor",      bin_op,     0,          0,          8,  0),

    (eqsym,             "=",        bin_op,     keq,        0,          6,  1),
    (cmpsym,            "cmp",      bin_op,     0,          0,          6,  1),
!   (appendsym,         "append",   bin_op,     kappend,    kappendto,  4,  0),
!   (concatsym,         "concat",   bin_op,     kconcat,    kconcatto,  4,  0),
    (powersym,          "**",       bin_op,     kpower,     0,          2,  0),
    (samesym,           "==",       bin_op,     ksame,      0,          6,  0),
!   (ssmarkersym,       "===",      0,          0,          0,          0,  0),
    (insym,             "in",       bin_op,     kin,        0,          6,  0),
    (notinsym,          "notin",    bin_op,     knotin,     0,          6,  0),
    (inrevsym,          "inrev",    0,          0,          0,          0,  0),

    (negsym,            "$neg",     mon_op,     kneg,       0,          0,  1),
    (notlsym,           "not",      mon_op,     knotl,      knotlto,    0,  1),
    (istruelsym,        "istrue",   mon_op,     kistruel,   kistruelto, 0,  1),
    (inotsym,           "inot",     mon_op,     kinot,      kinotto,    0,  1),
    (abssym,            "abs",      mon_op,     kabs,       kabsto,     0,  1),
    (signsym,           "sign",     mon_op,     ksign,      0,          0,  1),
    (sqrtsym,           "sqrt",     mon_op,     ksqrt,      0,          0,  1),
    (sqrsym,            "sqr",      mon_op,     ksqr,       0,          0,  1),

    (propsym,               $,      prop_op,    0,          0,          0,  0),
    (mathsopsym,        $,      0,  0,  0,  0,  1),     ! sin etc
    (maths2opsym,       $,      0,  0,  0,  0,  1),     ! atan2 etc

    (bitfieldsym,       $,      0,  0,  0,  0,  0),     ! Special bit selections
    (eolsym,            $,      0,  0,  0,  0,  0),     ! End of line
    (eofsym,            $,      0,  0,  0,  0,  0),     ! Eof seen
    (rawxnamesym,       $,      0,  0,  0,  0,  0),     ! unassigned name, case-sensitive, that is never a reserved word
    (docstringsym,      $,      0,  0,  0,  0,  0),     ! ! #comment used as documentation string
    (incrsym,           $,      0,  0,  0,  0,  1),     ! 1/2 = ++/--; later may add +2 for x++/x--
    (intconstsym,       $,      0,  0,  0,  0,  1),     ! 123 32 bits signed
    (decimalconstsym,   $,      0,  0,  0,  0,  1),     ! 123 or 123.4 decimal
    (realconstsym,      $,      0,  0,  0,  0,  1),     ! 123.4 64 bits
    (charconstsym,      $,      0,  0,  0,  0,  1),     ! 'A' or 'ABCD'
    (wcharconstsym,     $,      0,  0,  0,  0,  1),     ! 'A'W or 'ABCD'W (but don't have a syntax yet)
    (stringconstsym,    $,      0,  0,  0,  0,  1),     ! "ABC"
    (astringconstsym,   $,      0,  0,  0,  0,  1),     ! A"ABC"
    (wstringconstsym,   $,      0,  0,  0,  0,  1),     ! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
    (unitnamesym,       $,      0,  0,  0,  0,  0),     ! 
    (namesym,           $,      0,  0,  0,  0,  1),     ! identifier symbol
    (ksourcedirsym,     $,      0,  0,  0,  0,  0),     ! 
    (kstrincludesym,    $,      0,  0,  0,  0,  1),     ! 
!   (lexmacronamesym,   $,      0,  0,  0,  0,  0),     ! 
    (regsym,            $,      0,  0,  0,  0,  0),     ! x64 registers
    (xregsym,           $,      0,  0,  0,  0,  0),     ! XMM registers
    (fregsym,           $,      0,  0,  0,  0,  0),     ! ST registers
    (mregsym,           $,      0,  0,  0,  0,  0),     ! MMX registers
    (jmpccsym,          $,      0,  0,  0,  0,  0),     ! 
    (setccsym,          $,      0,  0,  0,  0,  0),     ! 
    (movccsym,          $,      0,  0,  0,  0,  0),     ! 
    (segnamesym,        $,      0,  0,  0,  0,  0),     ! 
    (asmopcodesym,      $,      0,  0,  0,  0,  0),     ! MOV etc

    (stdtypesym,        $,      0,  0,  0,  0,  1),     ! INT, CHAR etc
!   (machinetypesym,    $,      0,  0,  0,  0,  1),     ! INTM etc
!   (packtypesym,       $,      0,  0,  0,  0,  0),     ! Byte etc
    (ktypeofsym,        $,      0,  0,  0,  0,  0),     ! TYPEOF
    (ksubrangesym,      $,      0,  0,  0,  0,  0),     ! SUBRANGE
    (koutsym,           $,      0,  0,  0,  0,  0),     ! OUT
    (kicharsym,         $,      0,  0,  0,  0,  1),     ! ICHAR
    (kifsym,            $,      0,  0,  0,  0,  1),     ! 
    (kthensym,          $,      0,  0,  0,  0,  0),     ! 
    (kelsifsym,         $,      0,  0,  0,  0,  0),     ! 
    (kelsesym,          $,      0,  0,  0,  0,  0),     ! 
    (kelsecasesym,      $,      0,  0,  0,  0,  0),     ! 
    (kelseswitchsym,    $,      0,  0,  0,  0,  0),     ! 
    (kelseselectsym,    $,      0,  0,  0,  0,  0),     ! 
    (kendsym,           $,      0,  0,  0,  0,  0),     ! 
    (kunlesssym,        $,      0,  0,  0,  0,  0),     ! 
    (kcasesym,          $,      0,  0,  0,  0,  1),     ! CASE
    (kdocasesym,        $,      0,  0,  0,  0,  0),     ! DOCASE
    (krecasesym,        $,      0,  0,  0,  0,  0),     ! RECASE
    (kwhensym,          $,      0,  0,  0,  0,  0),     ! 
    (kforsym,           $,      0,  0,  0,  0,  0),     ! FOR
    (ktosym,            $,      0,  0,  0,  0,  0),     ! TO/DOWNTO
    (kbysym,            $,      0,  0,  0,  0,  0),     ! 
    (kdosym,            $,      0,  0,  0,  0,  0),     ! 
    (kwhilesym,         $,      0,  0,  0,  0,  0),     ! 
    (krepeatsym,        $,      0,  0,  0,  0,  0),     ! 
    (kuntilsym,         $,      0,  0,  0,  0,  0),     ! 
    (kreturnsym,        $,      0,  0,  0,  0,  0),     ! 
    (kstopsym,          $,      0,  0,  0,  0,  0),     ! 
    (kloopsym,          $,      0,  0,  0,  0,  0),     ! EXIT/NEXT/LOOP/REDO/RESTART
    (kstepsym,          $,      0,  0,  0,  0,  0),     ! STEP
    (kgotosym,          $,      0,  0,  0,  0,  0),     ! GO/GOTO
    (kswitchsym,        $,      0,  0,  0,  0,  0),     ! SWITCH
    (kdoswitchsym,      $,      0,  0,  0,  0,  0),     ! DOSWITCH
    (kprintsym,         $,      0,  0,  0,  0,  0),     ! PRINT/PRINTLN/FPRINT/FPRINTLN
    (ksprintsym,        $,      0,  0,  0,  0,  0),     ! SPRINT/SFPRINT
    (kreadsym,          $,      0,  0,  0,  0,  0),     ! READ/READLN
    (ksreadsym,         $,      0,  0,  0,  0,  0),     ! SREAD
    (ksreadlnsym,       $,      0,  0,  0,  0,  0),     ! SREADLN
    (kprocsym,          $,      0,  0,  0,  0,  0),     ! PROC
    (kfunctionsym,      $,      0,  0,  0,  0,  0),     ! FUNCTION
!   (kmethodsym,        $,      0,  0,  0,  0,  0),     ! METHOD
    (klabelsym,         $,      0,  0,  0,  0,  0),     ! LABEL
    (krecordsym,        $,      0,  0,  0,  0,  0),     ! RECORD
    (kstructsym,        $,      0,  0,  0,  0,  0),     ! STRUCT
    (kunionsym,         $,      0,  0,  0,  0,  0),     ! UNION
!   (kimportsym,        $,      0,  0,  0,  0,  0),     ! IMPORT
    (kimportmodulesym,  $,      0,  0,  0,  0,  0),     ! IMPORTDLL/IMPORTMODULE
!   (kimportpathsym,    $,      0,  0,  0,  0,  0),     ! IMPORTPATH
!   (kmapmodulesym,     $,      0,  0,  0,  0,  0),     ! MAPMODULE
    (ktypesym,          $,      0,  0,  0,  0,  0),     ! TYPE
    (ktypealiassym,     $,      0,  0,  0,  0,  0),     ! TYPEALIAS
    (kextendtypesym,    $,      0,  0,  0,  0,  0),     ! EXTENDTYPE
    (krefsym,           $,      0,  0,  0,  0,  1),     ! REF
    (kmutsym,           $,      0,  0,  0,  0,  0),     ! MUT
    (kletsym,           $,      0,  0,  0,  0,  0),     ! LET
    (kslicesym,         $,      0,  0,  0,  0,  0),     ! SLICE/SLICE2D
    (karraysym,         $,      0,  0,  0,  0,  0),     ! ARRAY
    (kdictsym,          $,      0,  0,  0,  0,  0),     ! DICT
!   (kflexsym,          $,      0,  0,  0,  0,  0),     ! FLEX
    (kmacrosym,         $,      0,  0,  0,  0,  0),     ! MACRO
    (kexpandsym,        $,      0,  0,  0,  0,  0),     ! EXPAND
    (koperatorsym,      $,      0,  0,  0,  0,  0),     ! OPERATOR
    (kconstsym,         $,      0,  0,  0,  0,  0),     ! 
!   (kenumsym,          $,      0,  0,  0,  0,  0),     ! 
    (knewsym,           $,      0,  0,  0,  0,  0),     ! NEW
!   (kdestroysym,       $,      0,  0,  0,  0,  0),     ! DESTROY
    (kclearsym,         $,      0,  0,  0,  0,  0),     ! CLEAR
    (kclasssym,         $,      0,  0,  0,  0,  0),     ! CLASS
    (kheadersym,        $,      0,  0,  0,  0,  0),     ! MODULE
    (kheadervarsym,     $,      0,  0,  0,  0,  0),     ! MODULE
    (kfflangsym,        $,      0,  0,  0,  0,  0),     ! JLANG CLANG WINDOWS HOST
    (kglobalsym,        $,      0,  0,  0,  0,  0),     ! global
    (kstaticsym,        $,      0,  0,  0,  0,  0),     ! STATIC

!   (ktrysym,           $,      0,  0,  0,  0,  0),     ! 
!   (kexceptsym,        $,      0,  0,  0,  0,  0),     ! 
!   (kfinallysym,       $,      0,  0,  0,  0,  0),     ! 
!   (kraisesym,         $,      0,  0,  0,  0,  0),     ! 
!   (kyieldsym,         $,      0,  0,  0,  0,  0),     ! 
    (kcastsym,          $,      0,  0,  0,  0,  1),     ! CAST
    (compilervarsym,    $,      0,  0,  0,  0,  1),     ! $lineno etc
    (dollarsym,         $,      0,  0,  0,  0,  1),     ! to be used for current array upperbound; also tabledata names
    (kevalsym,          $,      0,  0,  0,  0,  0),     ! EVAL
    (ktabledatasym,     $,      0,  0,  0,  0,  0),     ! tabledata
    (kstacksym,         $,      0,  0,  0,  0,  0),     ! STACK/UNSTACK
    (kclampsym,         $,      0,  0,  0,  0,  1),         ! CLAMP
    (kswapsym,          $,      0,  0,  0,  0,  0),     ! SWAP
    (kerrorsym,         $,      0,  0,  0,  0,  0),     ! PC_ERROR etc
!   (sysconstsym,       $,      0,  0,  0,  0,  0),     ! nil, etc
    (kassemsym,         $,      0,  0,  0,  0,  0),     ! ASM/ASSEM
    (ksyscallsym,       $,      0,  0,  0,  0,  1),     ! $get_procname etc
    (kemitcsym,         $,      0,  0,  0,  0,  0),     ! EMITC
    (kemptysym,         $,      0,  0,  0,  0,  0),     ! EMPTY
    (kcopysym,          $,      0,  0,  0,  0,  1),     ! COPY

    (kdummysym,         $,      0,  0,  0,  0,  0),     !
end

!'pcl' names continue to exist for use as operator codes

global enumdata [0:]ichar pclnames, [0:]ichar ccopnames =
    (kzero=0,           $,  nil),

    (kadd,              $,  "+"),
    (ksub,              $,  "-"),
    (kmul,              $,  "*"),
    (kdiv,              $,  "/"),
    (kidiv,             $,  "/"),
    (kirem,             $,  "%"),
    (kidivrem,          $,  nil),
    (kiand,             $,  "&"),
    (kior,              $,  "|"),
    (kixor,             $,  "^"),
    (kshl,              $,  "<<"),
    (kshr,              $,  ">>"),
    (kin,               $,  nil),
    (knotin,            $,  nil),
    (kmin,              $,  nil),
    (kmax,              $,  nil),
    (keq,               $,  "=="),
    (kne,               $,  "!="),
    (klt,               $,  "<"),
    (kle,               $,  "<="),
    (kge,               $,  ">="),
    (kgt,               $,  ">"),
    (ksame,             $,  nil),
    (kandl,             $,  "&&"),
    (korl,              $,  "||"),
    (kaddrefoff,        $,  "+"),
    (ksubrefoff,        $,  "-"),
    (ksubref,           $,  "-"),
!   (kappend,           $,  nil),
!   (kconcat,           $,  nil),

    (kneg,              $,  "-"),
    (kabs,              $,  nil),
    (kinot,             $,  "~"),
    (knotl,             $,  "!"),
    (kistruel,          $,  "!!"),
    (ksqr,              $,  nil),

    (ksqrt,             $,  nil),
    (ksin,              $,  nil),
    (kcos,              $,  nil),
    (ktan,              $,  nil),
    (kasin,             $,  nil),
    (kacos,             $,  nil),
    (katan,             $,  nil),
    (kln,               $,  nil),
    (klog,              $,  nil),
    (kexp,              $,  nil),
    (kround,            $,  nil),
    (kfloor,            $,  nil),
    (kceil,             $,  nil),
    (kfract,            $,  nil),
    (ksign,             $,  nil),
    (katan2,            $,  nil),
    (kpower,            $,  nil),
    (kfmod,             $,  nil),

    (kincr,             $,  nil),
    (kdecr,             $,  nil),
    (kincrload,         $,  nil),
    (kdecrload,         $,  nil),
    (kloadincr,         $,  nil),
    (kloaddecr,         $,  nil),

    (kaddto,            $,  "+="),
    (ksubto,            $,  "-="),
    (kmulto,            $,  "*="),
    (kdivto,            $,  "/="),
    (kidivto,           $,  "/="),
    (kiremto,           $,  "%="),
    (kiandto,           $,  "&="),
    (kiorto,            $,  "|="),
    (kixorto,           $,  "^="),
    (kshlto,            $,  "<<="),
    (kshrto,            $,  ">>="),
    (kminto,            $,  nil),
    (kmaxto,            $,  nil),
    (kandlto,           $,  "&&="),
    (korlto,            $,  "||="),
    (kaddrefoffto,      $,  "+="),
    (ksubrefoffto,      $,  "-="),
!   (kappendto,         $,  nil),
!   (kconcatto,         $,  nil),

    (knegto,            $,  nil),
    (kabsto,            $,  nil),
    (kinotto,           $,  nil),
    (knotlto,           $,  nil),
    (kistruelto,        $,  nil),

    (ktypepun,          $,  nil),

    (ksoftconv,         $,  nil),

    (kfloat,            $,  nil),
    (kfix,              $,  nil),
    (ktruncate,         $,  nil),
    (kfwiden,           $,  nil),
    (kfnarrow,          $,  nil),

    (klen,              $,  nil),
    (klwb,              $,  nil),
    (kupb,              $,  nil),
    (kbounds,           $,  nil),
    (klenstr,           $,  nil),
    (kbitwidth,         $,  nil),
    (kbytesize,         $,  nil),
    (kminvalue,         $,  nil),
    (kmaxvalue,         $,  nil),
    (ktypestr,          $,  nil),
    (kerror,            $,  nil),
    (kharderror,        $,  nil),
    (karraytoslice,     $,  nil),
    (kichartoslice,     $,  nil),
    (ksofttruncshort,   $,  nil),
    (kcharaxtoichar,    $,  nil),
    (ksliceptr,         $,  nil),
end

[]byte complexops = (
    kpower, katan2, ksin, kcos, ktan, kasin, kacos, kln, klog, kexp,
    kround, kfloor, kceil, kfract, kfmod)
global [pclnames.lwb..pclnames.upb]byte complexopset

global enumdata []ichar sourcedirnames =
    (includedir,    $),
!   (strincludedir, $),
    (binincludedir, $),
!   (textincludedir,$),
!   (defineunitdir, $),
!   (emitcdir,      $),
end

global enumdata []ichar headerdirnames =
    (hdr_module,        $),
    (hdr_import,        $),
    (hdr_subprog,       $),
    (hdr_sysmodule,     $),
    (hdr_sysimport,     $),
    (hdr_syssubprog,    $),
    (hdr_minclude,      $),
!   (hdr_alias,         $),
    (hdr_altpath,       $),
    (hdr_importpath,    $),
    (hdr_linkdll,       $),
    (hdr_linklib,       $),
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

global enumdata [0:]ichar fflangnames=
    (noff=0,        $), ! 
    (windowsff,     $), ! 
    (clangff,       $), ! 
    (mlangff,       $), ! 
    (callbackff,    $), ! 
end

global enumdata [0:]ichar scopenames=
    (Module_scope=0,    "Local"), !         !module
    (subprog_scope,     "Global"), !        !inter-subprog
    (program_scope,     "Program"), !       !inter-module
    (export_scope,      "Export"), !        !inter-program
end

global enumdata =
    thousand_unit,
    million_unit,
    billion_unit,
    kilo_unit,
    mega_unit,
    giga_unit
end

global enumdata [0:]ichar parammodenames=
    (var_param=0,       "Var "),
    (in_param,          "In "),
    (out_param,         "Out "),
    (optional_param,    "Opt "),
end

global enumdata [0:]ichar namenames
    (nullid=0,      $),     !Not assigned (sometimes converted to genfieldid)
    (programid,     $),     !Main root
    (subprogid,     $),
    (moduleid,      $),     !Current or imported module
    (dllmoduleid,   $),     !
    (typeid,        $),     !Type name in type, proc or module
    (procid,        $),     !Proc/method/function/op name
    (dllprocid,     $),     !Dll Proc/function name
    (dllvarid,      $),     !Dll variable name
    (genprocid,     $),     !generic proc name
    (constid,       $),     !Named constant in type, proc or module
    (staticid,      $),     !Static in type or proc or module
    (frameid,       $),     !Local var
    (paramid,       $),     !Local param
    (fieldid,       $),     !Field of Record or Class
    (genfieldid,    $),     !Generic Field of Record or Class
    (enumid,        $),     !Enum name, part of enum type only
    (labelid,       $),     !Label name in proc only
    (macroid,       $),     !Name of macro
    (macroparamid,  $),     !Macro formal parameter name
    (linkid,        $),     !Name in class defined in a base class
end

!!---
global tabledata []ichar stnames, []int stsymbols, []int stsubcodes=

    ("if",          kifsym,         jif),
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
    ("to",          ktosym,         0),
    ("downto",      ktosym,         1),
    ("by",          kbysym,         0),
    ("do",          kdosym,         0),
    ("end",         kendsym,        0),
    ("while",       kwhilesym,      0),
    ("repeat",      krepeatsym,     0),
    ("until",       kuntilsym,      0),
    ("always",      kuntilsym,      1),
    ("return",      kreturnsym,     0),
    ("stop",        kstopsym,       0),
    ("redo",        kloopsym,       jredo),
    ("loop",        kloopsym,       jredo),
    ("next",        kloopsym,       jnext),
    ("exit",        kloopsym,       jexit),
    ("$step",       kstepsym,       0),
    ("goto",        kgotosym,       0),
    ("go",          kgotosym,       1),
    ("switch",      kswitchsym,     jswitch),
    ("doswitch",    kdoswitchsym,   jdoswitch),
    ("tabledata",   ktabledatasym,  0),
    ("enumdata",    ktabledatasym,  1),
    ("clamp",       kclampsym,      0),
    ("eval",        kevalsym,       0),

    ("evalloadref", kevalsym,       loadref_op),
    ("evalgetref",  kevalsym,       getref_op),
    ("evalget",     kevalsym,       get_op),
    ("evalload",    kevalsym,       load_op),
!   ("evalstore",   kevalsym,       store_op),
!   ("evalpush",    kevalsym,       push_op),

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
    ("cast",        kcastsym,       jconvert),

    ("function",    kfunctionsym,   0),
    ("func",        kfunctionsym,   0),
    ("procedure",   kprocsym,       0),
    ("proc",        kprocsym,       0),
    ("fun",         kfunctionsym,   1),
    ("sub",         kprocsym,       1),
    ("threadedproc",        kprocsym,       2),
!   ("threadedproc",        kprocsym,       0),

    ("type",        ktypesym,       0),
    ("class",       kclasssym,      0),
    ("record",      krecordsym,     0),
    ("struct",      kstructsym,     0),
    ("union",       kunionsym,      0),
    ("ref",         krefsym,        0),
    ("pointer",     krefsym,        0),
    ("returning",   sendtosym,      0),
    ("mut",         kmutsym,        0),
    ("var",         kmutsym,        0),
    ("let",         kletsym,        0),

    ("include",     ksourcedirsym,  includedir),
    ("strinclude",  kstrincludesym, 0),
    ("bininclude",  ksourcedirsym,  binincludedir),
    ("emitc",       kemitcsym,      0),
    ("macro",       kmacrosym,      0),

    ("assem",       kassemsym,      1),
    ("asm",         kassemsym,      0),

    ("static",      kstaticsym,     0),
    
    ("const",       kconstsym,      0),

    ("$get_nprocs",     ksyscallsym,        sf_get_nprocs),
    ("$getnprocs",      ksyscallsym,        sf_get_nprocs),

    ("$get_procname",   ksyscallsym,        sf_get_procname),
    ("$getprocname",    ksyscallsym,        sf_get_procname),

    ("$get_procaddr",   ksyscallsym,        sf_get_procaddr),
    ("$getprocaddr",    ksyscallsym,        sf_get_procaddr),

    ("$gettttable",     ksyscallsym,        sf_gettttable),
    ("$getsttable",     ksyscallsym,        sf_getsttable),
    ("$getfftable",     ksyscallsym,        sf_getfftable),

    ("importdll",   kimportmodulesym,   'D'),
    ("importlib",   kimportmodulesym,   'L'),
    ("unless",      kunlesssym,         0),

    ("out",         koutsym,        0),

!   ("new",         knewsym,        jnew),
!   ("newvar",      knewsym,        jnewvar),

    ("global",      kglobalsym,     subprog_scope),
    ("export",      kglobalsym,     export_scope),

    ("clang",       kfflangsym,     clangff),
    ("mlang",       kfflangsym,     mlangff),
    ("windows",     kfflangsym,     windowsff),
    ("callback",    kfflangsym,     callbackff),

    ("swap",        kswapsym,       0),

    ("void",        stdtypesym,     tvoid),
    ("int",         stdtypesym,     tint),
    ("word",        stdtypesym,     tword),
    ("real",        stdtypesym,     treal),

    ("ichar",       kicharsym,      0),

    ("int8",        stdtypesym,     ti8),
    ("int16",       stdtypesym,     ti16),
    ("int32",       stdtypesym,     ti32),
    ("int64",       stdtypesym,     ti64),

    ("i8",          stdtypesym,     ti8),
    ("i16",         stdtypesym,     ti16),
    ("i32",         stdtypesym,     ti32),
    ("i64",         stdtypesym,     ti64),

    ("real32",      stdtypesym,     tr32),
    ("real64",      stdtypesym,     tr64),
    ("r32",         stdtypesym,     tr32),
    ("r64",         stdtypesym,     tr64),

    ("float32",     stdtypesym,     tr32),
    ("float64",     stdtypesym,     tr64),

    ("byte",        stdtypesym,     tu8),
    ("u8",          stdtypesym,     tu8),
    ("u16",         stdtypesym,     tu16),
    ("u32",         stdtypesym,     tu32),
    ("u64",         stdtypesym,     tu64),

    ("word8",       stdtypesym,     tu8),
    ("word16",      stdtypesym,     tu16),
    ("word32",      stdtypesym,     tu32),
    ("word64",      stdtypesym,     tu64),

    ("char",        stdtypesym,     tc8),
    ("char64",      stdtypesym,     tc64),

    ("bool64",      stdtypesym,     tbool64),
    ("bool",        stdtypesym,     tbool64),
    ("bool8",       stdtypesym,     tbool8),

    ("range",       stdtypesym,     trange),
    ("auto",        stdtypesym,     tauto),

    ("label",       stdtypesym,     tlabel),

!   ("string",      stdtypesym,     vstring),
!   ("decimal",     stdtypesym,     vdecimal),
!   ("list",        stdtypesym,     vlist),

    ("slice",       kslicesym,      tslice),
    ("array",       karraysym,      0),
!   ("vector",      kslicesym,      vvector),
    ("typeof",      ktypeofsym,         0),

    ("million",     unitnamesym,    million_unit),
    ("billion",     unitnamesym,    billion_unit),
    ("thousand",    unitnamesym,    thousand_unit),

    ("$lineno",     compilervarsym, jcvlineno),
    ("$strlineno",  compilervarsym, jcvstrlineno),
    ("$filename",   compilervarsym, jcvfilename),
    ("$modulename", compilervarsym, jcvmodulename),
    ("$function",   compilervarsym, jcvfunction),
    ("$date",       compilervarsym, jcvdate),
    ("$time",       compilervarsym, jcvtime),
    ("$version",    compilervarsym, jcvversion),
    ("$typename",   compilervarsym, jcvtypename),
    ("$targetbits", compilervarsym, jcvtargetbits),
    ("$targetsize", compilervarsym, jcvtargetsize),
!   ("$targetname", compilervarsym, jcvtargetname),
    ("$targetcode", compilervarsym, jcvtargetcode),
    ("$ctarget",    compilervarsym, jcvctarget),
    ("$windows",    compilervarsym, jcvwindows),
    ("$linux",      compilervarsym, jcvlinux),
    ("nil",         compilervarsym, jcvnil),
    ("pi",          compilervarsym, jcvpi),
    ("true",        compilervarsym, jcvtrue),
    ("false",       compilervarsym, jcvfalse),
    ("infinity",    compilervarsym, jcvinfinity),
    ("$",           dollarsym,      0),

    ("and",         andlsym,        0),
    ("or",          orlsym,         0),
    ("xor",         xorlsym,        0),
    ("iand",        iandsym,        0),
    ("ior",         iorsym,         0),
    ("ixor",        ixorsym,        0),
    ("in",          insym,          kin),
    ("notin",       notinsym,       knotin),
    ("inrev",       inrevsym,       0),
    ("rem",         iremsym,        0),
    ("divrem",      idivremsym,     0),
    ("min",         minsym,         0),
    ("max",         maxsym,         0),

    ("not",         notlsym,        0),
    ("inot",        inotsym,        0),
    ("istrue",      istruelsym,     0),
    ("abs",         abssym,         kabs),
    ("$neg",        negsym,         0),

!   ("asc",         opsym,          jasc),
!   ("tochr",       opsym,          jchr),
    ("sqr",         sqrsym,         0),
    ("sqrt",        sqrtsym,        0),
    ("sign",        signsym,        0),

    ("sin",         mathsopsym,     ksin),
    ("cos",         mathsopsym,     kcos),
    ("tan",         mathsopsym,     ktan),
    ("asin",        mathsopsym,     kasin),
    ("acos",        mathsopsym,     kacos),
    ("atan",        mathsopsym,     katan),
!   ("sign",        mathsopsym,     ksign),
    ("ln",          mathsopsym,     kln),
    ("log",         mathsopsym,     klog),
    ("exp",         mathsopsym,     kexp),
    ("round",       mathsopsym,     kround),
    ("floor",       mathsopsym,     kfloor),
    ("ceil",        mathsopsym,     kceil),
    ("fract",       mathsopsym,     kfract),

    ("atan2",       maths2opsym,    katan2),
    ("fmod",        maths2opsym,    kfmod),

!   ("append",      appendsym,      0),
!   ("concat",      concatsym,      0),
    ("sliceptr",    propsym,        ksliceptr),

    ("len",         propsym,    klen),
    ("lwb",         propsym,    klwb),
    ("upb",         propsym,    kupb),
    ("bounds",      propsym,    kbounds),
!   ("lenstr",      propsym,    klenstr),
    ("bitwidth",    propsym,    kbitwidth),
    ("bytes",       propsym,    kbytesize),
    ("minvalue",    propsym,    kminvalue),
    ("maxvalue",    propsym,    kmaxvalue),
    ("typestr",     propsym,    ktypestr),

    ("msb",         bitfieldsym,    bf_msb),
    ("lsb",         bitfieldsym,    bf_lsb),
    ("msbit",       bitfieldsym,    bf_msbit),
    ("lsbit",       bitfieldsym,    bf_lsbit),
    ("msw",         bitfieldsym,    bf_msw),
    ("lsw",         bitfieldsym,    bf_lsw),
    ("odd",         bitfieldsym,    bf_odd),
    ("even",        bitfieldsym,    bf_even),

    ("endif",       kendsym,    kifsym),
    ("fi",          kendsym,    kifsym),
    ("endcase",     kendsym,    kcasesym),
    ("esac",        kendsym,    kcasesym),
    ("enddocase",   kendsym,    kdocasesym),
    ("endswitch",   kendsym,    kswitchsym),
    ("enddoswitch", kendsym,    kdoswitchsym),
    ("endfor",      kendsym,    kforsym),
    ("od",          kendsym,    kdosym),
    ("endproc",     kendsym,    kprocsym),
    ("endfunction", kendsym,    kfunctionsym),
    ("endwhile",    kendsym,    kwhilesym),
    ("endto",       kendsym,    ktosym),
    ("enddo",       kendsym,    kdosym),
!   ("endunless",   kendsym,    kunlesssym),
!   ("endimportmodule", kendsym,kimportmodulesym),
!   ("endtry",      kendsym,    ktrysym),
    ("endrecord",   kendsym,    krecordsym),
    ("endassem",    kendsym,    kassemsym),

    ("$caligned",   atsym,          1),
    ("empty",       kemptysym,      0),
    ("clear",       kemptysym,      0),
    ("copy",        kcopysym,       0),

    ("module",      kheadersym,     hdr_module),
    ("sysmodule",   kheadersym,     hdr_sysmodule),
    ("import",      kheadersym,     hdr_import),
    ("sysimport",   kheadersym,     hdr_sysimport),
    ("minclude",    kheadersym,     hdr_minclude),
    ("subprog",     kheadersym,     hdr_subprog),
    ("syssubprog",  kheadersym,     hdr_syssubprog),
    ("altpath",     kheadersym,     hdr_altpath),
    ("importpath",  kheadersym,     hdr_importpath),
    ("linkdll",     kheadersym,     hdr_linkdll),
    ("linklib",     kheadersym,     hdr_linklib),
    ("exportmodule",kheadersym,     hdr_exportmodule),
    ("runexe",      kheadersym,     hdr_runexe),
    ("setvar",      kheadersym,     hdr_setvar),
    ("showvar",     kheadersym,     hdr_showvar),

    ("$devpath",    kheadervarsym,  hv_devpath),
    ("$mmpath",     kheadervarsym,  hv_mmpath),
    ("$hdrpath",    kheadervarsym,  hv_hdrpath),
    ("$$ctarget",   kheadervarsym,  hv_ctarget),
    ("$$windows",   kheadervarsym,  hv_windows),
    ("$$linux",     kheadervarsym,  hv_linux),
    ("$optim",      kheadervarsym,  hv_optim),
    ("$mainmodule", kheadervarsym,  hv_mainmodule),
    ("$a",          kheadervarsym,  hv_a),
    ("$b",          kheadervarsym,  hv_b),
    ("$c",          kheadervarsym,  hv_c),

    ("$$dummy",     0,              0)
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
        kicharsym, ktypeofsym, kslicesym, kdictsym, karraysym)

!list of genops that have an int result, used to populate intresult[]
[]byte intresultlist = (
    kin, knotin, klwb, kupb, klen, klenstr, kbitwidth,
    kbytesize)

global [tc64..tr64, tc64..tr64]int16 softconvtable = (
!To: c64        u64         i64         r32         r64              From:
    (ksoftconv, ksoftconv,  ksoftconv,  kfloat,     kfloat),        !c64
    (ksoftconv, ksoftconv,  ksoftconv,  kfloat,     kfloat),        !u64
    (ksoftconv, ksoftconv,  ksoftconv,  kfloat,     kfloat),        !i64
    (kfix,      kfix,       kfix,       ksoftconv,  kfwiden),       !r32
    (kfix,      kfix,       kfix,       kfnarrow,   ksoftconv))     !r64

global [pclnames.lwb..pclnames.upb]byte intresult

!global [tfirstnum..tlastnum, tfirstnum..tlastnum]int64 softconvtable

global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
            kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
            semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag

!flag 1 means is a memory operand where a value can be accessed by an address mode
! without needing to load. (However, it may need loading for reading if short for
! example.)
!flag 2 used for immediate operands that also do not need loading. Again, some
! may need loading anyway, eg. imms that don't fit into i32, or memaddr of locals

global [jtagnames.lwb..jtagnames.upb]byte ismemtag

proc start=
    int genop, s,t, a, specop

!populate intresultlist
    for i in intresultlist.bounds do
        intresult[intresultlist[i]]:=1
    od

    for i to exprendsymbols.len do
        endsexpr[exprendsymbols[i]]:=1
    od

    isbooltag[jcmp]:=1
    isbooltag[jcmpchain]:=1
    isbooltag[jandl]:=1
    isbooltag[jorl]:=1
    isbooltag[jnotl]:=1
    isbooltag[jistruel]:=1
    isbooltag[jinrange]:=1
    isbooltag[jinset]:=1

    ismemtag[jname]:=1
    ismemtag[jindex]:=1
    ismemtag[jdot]:=1
    ismemtag[jptr]:=1
    ismemtag[jaddrof]:=2
    ismemtag[jaddroffirst]:=2
    ismemtag[jconst]:=2
    ismemtag[jdotindex]:=1
    ismemtag[jdotslice]:=1

![]byte complexops = (
!   kpower, katan2, ksin, kcos, ktan, kasin, kacos, kln, klog, kexp
!   kround, kfloor, kceil, kfract, kfmod)
!global [pclnames.lwb..pclnames.upb]byte complexopset
    for i to complexops.len do complexopset[complexops[i]]:=1 od

end

=== mm_type.m 0 0 18/33 ===
const nolv=0
const needlv=1

const maxparams=100
const maxfields=200
int countedfields
int inassem
int inidata

INT DEB

proc tpass(unit p, int t=tany, lv=nolv)=
    symbol d
    unit a,b,c, q
    int oldmlineno,m,nparams,paramtype,restype,amode

    if p=nil then return fi

    oldmlineno:=mlineno

    mlineno:=p.pos

    a:=p.a
    b:=p.b
    c:=p.c

    p.resultflag:=t<>tvoid

    switch p.tag
    when jname then
        tx_name(p,t,lv)
    when jconst then
!       if lv=needlv then txerror("&const") fi

    when jtypeconst then
        p.mode:=ti64

    when jbytesize, jbitwidth then
        tpass(a)
        p.mode:=ti64

    when jbin, jcmp then
        tx_bin(p,a,b)

    when junary then
        tx_unary(p,a)

    when jbinto then
        tx_binto(p,a,b)

    when junaryto then
        tpasslv(a)
        p.mode:=tvoid

    when jassign then
        tx_assign(p,a,b,t)

    when jaddrof then
        if a.tag=jptr then
            deleteunit(p,a)
            deleteunit(p,p.a)
            tpass(p,t)
        else
            tpasslv(a)
            p.mode:=createrefmode(nil,a.mode)
            setsimple(p)
        fi

    when jaddroffirst then
        tx_addroffirst(p,a,t)

    when jif then
        tx_if(p,a,b,c,t,lv)

    when jindex then
        tx_index(p,a,b,t,lv)

    when jptr then
        tx_ptr(p,a,t,lv)

    when jcallproc, jcallfn then
        tx_callproc(p,a,b,t)

    when jdot then
        tx_dot(p,a,b,lv)

    when jandl, jorl then
        tx_andl(p,a,b)

    when jnotl then
        tx_notl(p,a)

    when jistruel then
        tx_istruel(p,a)

    when jconvert then
        tx_convert(p,a,1)

    when jtypepun then
        tx_typepun(p,a)

    when jincr then
        tx_incrto(p,a,t)

    when jmakerange then
        tx_makerange(p,a,b)

    when jswap then
        tx_swap(p,a,b)

    when jselect then
        tx_select(p,a,b,c,t,lv)

    when jswitch, jdoswitch then
        tx_switch(p,a,b,c,t,lv)

    when jcase, jdocase then
        tx_case(p,a,b,c,t,lv)

!   when jdotindex, jdotslice, janddotindex then
    when jdotindex, jdotslice then
        tx_dotindex(p,a,b,lv)

    when jslice then
        tx_slice(p,a,b)

    when jblock then
        tx_block(p,a,t,lv)

    when jeval then
        tpass(a,tany)

    when jdo then
        tpass(a,tvoid)

    when jreturn then
        tx_return(p,a,t)

    when jprint,jprintln,jfprint,jfprintln then

        tx_unitlist(a)
        fixchararray(a)

        do_printlist(b)
        do_printlist(p.c)

    when jforup, jfordown then
        tx_for(a,b,c)

    when jforall, jforallrev then
        tx_forall(a,b,c)

    when jto then
        tpass(a,ti64)
        tpass(b,tvoid)
        tpass(c,ti64)       !when autovar present

    when jautocast then
        tpass(a)
        if t=tany then txerror("cast() needs type") fi
        coerceunit(a,t,1)
        deleteunit(p,a)

    when jmakelist then
        tx_makelist(p,a,t,lv)

    when jstop then
        tpass(a,ti64)

    when jexit,jredo, jnext then
        tx_exit(p,a)

    when jgoto then
        tx_goto(p,a)

    when jlabeldef then

    when jwhile then

        tpass(a)
        if iscondtrue(a) then
            p.tag:=jdo
            p.a:=b
        elsif iscondfalse(a) then
            p.tag:=jnull
        fi
        tpass(b,tvoid)
        tpass(c,tvoid)

    when jrepeat then
        tpass(a,tvoid)
        tpass(b)
        if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") fi

    when jnogap, jspace then

    when jassem then
        if t<>tvoid then
            p.mode:=t
        fi

        inassem:=1
        tx_unitlist(a)
        tx_unitlist(b)
        tx_unitlist(c)
        inassem:=0

    when jassemreg,jassemxreg then
    when jassemmem then
        tpass(a)

    when jtypeof then
        tpass(a)
        if a.tag=jtypeconst then
            p.value:=a.value
        else
            p.value:=a.mode
        fi
        p.tag:=jtypeconst
        p.mode:=ti64

    when jtypestr then
        tpass(a)
        if a.tag=jtypeconst then
            m:=a.value
        else
            tpass(a)
            m:=a.mode
        fi
        p.tag:=jconst
        p.mode:=trefchar
        p.a:=nil
        p.svalue:=pcm_copyheapstring(strmode(m,0))
        p.slength:=strlen(p.svalue)
        p.isastring:=1

    when jfmtitem then
        tpass(a)
        tpass(b)

    when jreadln then
        tpass(a)

    when jread then
        if a then
            tpass(a,tc64)
        fi
        if ttisinteger[t] or ttisreal[t] then
            t:=gettypebase(t)
        fi
        p.mode:=t
    when jrecase then
        if a then
            tpass(a,ti64)
            if a.tag<>jconst then
                txerror("recase must be const")
            fi
        fi

    when jcvlineno then
        p.mode:=ti64
    when jcvfilename,jcvmodulename then
        p.mode:=trefchar

    when jbitfield then
        tx_bitfield(p,a,lv)

    when jsyscall then
        restype:=tvoid
        paramtype:=tvoid
        case p.fnindex
        when sf_get_nprocs then restype:=ti64
        when sf_get_procname then paramtype:=ti64; restype:=trefchar;
        when sf_get_procaddr then paramtype:=ti64; restype:=tref 
        when sf_gettttable, sf_getsttable, sf_getfftable then; restype:=tref
        esac

        if paramtype<>tvoid then
            if a=nil then txerror("sys: arg missing") fi
            tpass(a,paramtype)
            if a.nextunit then txerror("sys: too many args") fi
        elsif a then txerror("sys: too many args")
        fi

        p.mode:=restype

    when jcmpchain then
        tx_cmpchain(p,a)

    when jempty then
        tpasslv(a)

    when jshorten then

    when jstrinclude then
        tx_strinclude(p,a)

    when jmakeslice then
        tx_makeslice(p,a,t)

    when jmakeset then
        tx_makeset(p,a,t)

    when jemitc then
    when jinfinity then
    when joperator then
        p.mode:=ti64
        p.tag:=jconst
        p.value:=p.pclop
    else
        PRINTLN "TXUNIT: CAN'T DO:",jtagnames[p.tag]
    doelse::

        for i to jsubs[p.tag] do
            tx_unitlist(p.abc[i],t)
        od
    endswitch

    tevaluate(p)

    case p.tag
    when jmakelist, jreturn then
    else
        if t<>tany and t<>tvoid and p.mode<>t then      !does not already match
            coerceunit(p,t)         !apply soft conversion
        fi
    esac
!
    IF T=TVOID THEN
        CASE P.TAG
        WHEN JCONST, JBIN, jUNARY, JCMP THEN
!           TXERROR("Eval needed")
        WHEN JNAME THEN
            unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
!               TXERROR("Eval needed2")
            end

        esac
    fi

    mlineno:=oldmlineno
end

global proc tx_allprocs=
    ref procrec pp
    unit pcode

    pp:=proclist
    while pp, pp:=pp.nextproc do
        currproc:=pp.def
        pcode:=currproc.code

        if ttisshort[currproc.mode] then
            mlineno:=currproc.pos
            txerror("proc short ret type")
         fi

IF NOT CTARGET THEN
        symbol d:=currproc.deflist
        while d, d:=d.nextdef do
            if d.nameid=paramid then
                if ttisblock[d.mode] and d.parammode<>out_param then
                    d.parammode:=out_param
                    d.mode:=createrefmode(nil, d.mode)
!               elsif ttisshort[d.mode] then
!                   txerror_s("short param type:",currproc.name)
                fi
            fi
        od
FI
    od

    pp:=proclist
    while pp do
        currproc:=pp.def
        pcode:=currproc.code

        tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

        case ttbasetype[currproc.mode]
        when tvoid then     !PROC
        when ttuple then    !MULT FN
        else                !REGULAR FN
            if not ctarget and pcode.tag<>jreturn then
                insertunit(pcode,jreturn)
                pcode.mode:=currproc.mode
                pcode.resultflag:=1
            fi
        esac

        pp:=pp.nextproc
    od
end

proc tx_block(unit p,a, int t,lv)=
    while a and a.nextunit do
        tpass(a,tvoid)
        a:=a.nextunit
    od
    if a then
!       tx_unitlist(a,t,lv)
        tpass(a,t,lv)
        p.mode:=(t<>tvoid|a.mode|tvoid)
    fi
end

global proc tx_typetable=
    symbol d

    for i:=tuser to ntypes do
        if ttbasetype[i]=trecord then
            tx_passdef(d:=ttnamedef[i])
        fi
        setmodesize(i)
    od
end

proc setmodesize(int m)=
    int size,target

    if ttsize[m] then return fi

    mlineno:=ttlineno[m]
    case ttbasetype[m]
    when tarray then
        setarraysize(m)
    when trecord then
        setrecordsize(m)
    when tvoid,tproc then
    when tslice then
        setslicesize(m)
    when tauto then
        TXERROR("SETMODESIZE/AUTO?")
    when tany then

    when tpending then
        target:=tttarget[m]
        setmodesize(target)

        ttbasetype[m]:=ttbasetype[target]
        ttsize[m]:=ttsize[target]
        ttlower[m]:=ttlower[target]
        ttlength[m]:=ttlength[target]
        ttnamedef[m]:=ttnamedef[target]

    when ttuple then

    else
        if size:=ttsize[ttbasetype[m]] then
            ttsize[m]:=size
            return
        fi
        println "SIZE 0:",strmode(m),=m,=stdnames[ttbasetype[m]]
        println "Can't set mode size"
    esac
end

proc setarraysize(int m)=
    int lower,length,elemsize,target,size
    unit pdim,a,b

    if ttsizeset[m] then return fi

    pdim:=ttdimexpr[m]

    if pdim then
        a:=pdim.a
        b:=pdim.b
        rx_unit(ttowner[m],pdim)

        case pdim.tag
        when jmakerange then
            tpass(a)
            tpass(b)
            lower:=getconstint(a)
            length:=getconstint(b)-lower+1
        when jkeyvalue then
            tpass(a)
            lower:=getconstint(a)
            if b then
                tpass(b)
                length:=getconstint(b)
            else
                length:=0
            fi
        else
            tpass(pdim)
            length:=getconstint(pdim)
            lower:=1
        esac
    else
        lower:=1
        length:=0
    fi

    ttdimexpr[m]:=nil

    ttlower[m]:=lower
    ttlength[m]:=length

    target:=tttarget[m]
    setmodesize(target)
    elemsize:=ttsize[tttarget[m]]
    ttsize[m]:=size:=length*elemsize
    ttsizeset[m]:=1

    checkblocktype(m)
end

proc setslicesize(int m)=
    unit pdim

    if ttsize[m] then return fi

    pdim:=ttdimexpr[m]

    if pdim then
        rx_unit(ttowner[m],pdim)
        tpass(pdim)
        ttlower[m]:=getconstint(pdim)
        ttdimexpr[m]:=nil
    else
        ttlower[m]:=1
    fi

    setmodesize(tttarget[m])
    ttsize[m]:=ttsize[tslice]
end

global function tx_module(int n)int=
    modulerec m
    symbol d
    int globalflag,status

    currmoduleno:=n

    tx_passdef(moduletable[n].stmodule)

    return 1
end

global proc tx_passdef(symbol p)=
    symbol d
    int oldmlineno
    unit q

    if p.txdone then
        return
    fi

    oldmlineno:=mlineno
    mlineno:=p.pos

    d:=p.deflist
    while d do
        tx_passdef(d)
        d:=d.nextdef
    od

    q:=p.code

    case p.nameid
    when procid then
        currproc:=nil
    when constid,enumid then
        tx_namedconst(p)
    when staticid, frameid, paramid then
        tx_namedef(p)
    esac

    p.txdone:=1
    mlineno:=oldmlineno
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
    while p do
        tpass(p,t)
        p:=p.nextunit
    od
end

proc tx_namedef(symbol d)=
    int m,mold,inidataold
    unit dcode,pequiv

    m:=d.mode
    setmodesize(m)

    if d.circflag then
        txerror("Circular reference detected")
    fi
    if d.txdone then return fi
    dcode:=d.code

    d.circflag:=1

    if d.atvar then
        pequiv:=d.equivvar
        if pequiv.tag=jaddrof then deleteunit(pequiv,pequiv.a) fi
        if pequiv.tag<>jname then
            txerror("@name needed")
        fi
        tpass(pequiv)
    fi

    if dcode and d.nameid<>frameid then
        mold:=m
        m:=gettypebase(m)

        if ttbasetype[m]=tslice and dcode.tag=jconst and dcode.mode=trefchar then
            tpass(dcode,trefchar)
        else
            inidataold:=inidata
            inidata:=1
            tpass(dcode,m)
            inidata:=inidataold
        fi
        d.circflag:=0
        d.txdone:=1
        if ttbasetype[m]=tarray and ttlength[m]=0 then
            d.mode:=dcode.mode
        fi

        if mold<>m then
            if ttisinteger[m] and ttisshort[mold] then
                insertunit(d.code,jshorten)
                d.code.mode:=mold
            elsif mold=tr32 then
                d.code.mode:=mold
            fi
        fi

        if d.nameid=staticid then
            checkconstexpr(d.code)
        fi

    elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
        tpass(dcode,m)
        d.mode:=dcode.mode
        d.circflag:=0
        d.txdone:=1

    else
        d.circflag:=0
        d.txdone:=1
    fi
end

global proc tx_namedconst(symbol d)=
    int m

    if d.circflag then
        txerror("Circular const reference detected")
    fi

    unit q
    if d.txdone then return fi
    q:=d.code

    m:=d.mode

    d.circflag:=1
    tpass(q,(m=tauto|tany|m))

    d.circflag:=0
    checkconstexpr(q)
    if m=tauto then
        d.mode:=q.mode
    fi

    d.txdone:=1
end

proc checkconstexpr(unit p)=
!check whether p is const expr
    unit q
    int pmode

    case p.tag
    when jconst, jtypeconst then
        return
    when jmakelist then
        q:=p.a
        while q do
            checkconstexpr(q)
            q:=q.nextunit
        od

    when jconvert then

        if ttbasetype[p.a.mode]=tref then
            if tttarget[p.a.mode]=tvoid then
                p.a.mode:=p.mode
                deleteunit(p,p.a)
            else
                goto cerror
            fi
        fi
cerror::
        IF NOT CTARGET THEN
            error
        FI

    when jshorten then
        checkconstexpr(p.a)

    when jaddrof, jaddroffirst then
        case p.a.tag
        when jname then
        else
            goto error
        esac

    when jname then
        if p.def.nameid=fieldid then return fi
        if p.def.nameid=labelid then return fi
        error
    else
    error::
        println jtagnames[p.tag],STRMODE(P.MODE)
!   PRINTUNIT(P)
        txerror("Getconstexpr: not const")
    esac
end

function getconstint(unit q)int64=
    checkconstexpr(q)

    if ttisinteger[q.mode] or q.tag=jtypeconst then
        return q.value
    elsif ttisreal[q.mode] then
        return q.xvalue
    else
        cpl strmode(q.mode)
        txerror("Getconstint: not int32/64")
    fi
    return 0
end

proc makenewconst(unit p,int64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

    p.tag:=jconst
    p.a:=p.b:=nil
    p.value:=x
    p.isconst:=1
    p.simple:=1
    if t<>tvoid then
        p.mode:=t
    fi
end

proc tx_name(unit p,int t,lv)=
    symbol d
    int oldmlineno
    unit pcode
    oldmlineno:=mlineno

    d:=p.def
    mlineno:=d.pos

    switch d.nameid
    when constid,enumid then            !note: currently, rxpass converts names to constants

        if lv then txerror("&const") fi

        tx_namedconst(d)
        pcode:=d.code

        p.tag:=jconst
        p.def:=nil
        p.a:=nil
        p.c:=nil

        if pcode.tag=jconvert then      !assume c_soft
            p.value:=pcode.a.value

        else
            p.value:=pcode.value
        fi

        p.slength:=pcode.slength
        p.mode:=d.mode
        p.isconst:=1
        p.isastring:=pcode.isastring

    when staticid,frameid,paramid then
        if d.islet and lv then
!           println D.NAME,=LV,D.ISLET
            txerror_s("Can't use 'let' as lvalue: ",d.name)
        fi

        tx_namedef(d)

        if not inassem then
            p.mode:=d.mode
!           if d.parammode=out_param and not ttisblock[tttarget[d.mode]] then
            if d.parammode=out_param then
                insertunit(p, jptr)
                p.mode:=tttarget[d.mode]
            fi
            twiden(p,lv)

        else
            p.mode:=trefchar
        fi

    when procid,dllprocid then

        p.mode:=trefproc    !use generic refproc mode (yields return type of actual proc mode
                !after a call op, or actual refproc in other context. Don't use actual
                !refproc here, to avoid generating thousands of ref proc modes, one
                !for each call, that will never be needed

    when labelid then
        p.mode:=treflabel

    when moduleid then
        txerror_s("Module name can't be used on it's own: #",d.name)

    when fieldid then
        p.tag:=jconst
        p.def:=nil
        p.a:=nil
        p.c:=nil

        p.value:=d.offset

        p.mode:=ti64
        p.isconst:=1


    when typeid then
        p.tag:=jtypeconst
        p.value:=d.mode
        p.mode:=ti64

    when dllvarid then
        if d.code then
            txerror("Can't init dllvar")
        fi
        p.mode:=d.mode

    else
        mlineno:=p.pos
        txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
    endswitch
    mlineno:=oldmlineno

end

proc tx_bin(unit p,a,b)=
!deal with most binary ops
    unit q
    int amode,bmode,abase,bbase,cmode, resmode, relop, simpleset

    tpass(a)
    tpass(b)
    amode:=a.mode
    bmode:=b.mode


    switch p.pclop
    when kadd then
        if dobinnumx(p,a,b) then return fi
        if ttisref[amode] then
            if ttisref[bmode] and a.isastring and b.isastring then
                combinestrings(p)
                return
            fi
            if isnum(bmode) then
                coerceunit(b,ti64)
                p.pclop:=kaddrefoff
                p.mode:=amode
                return
            fi
        fi

    when ksub then
        if dobinnumx(p,a,b) then return fi
        if ttisref[amode] then
            if ttisref[bmode] then
                if comparemodes(amode, bmode) then
                    p.pclop:=ksubref
                    p.mode:=ti64
                    return
                else
                    txerror("ref-ref: not compat")
                fi
            fi
            if isnum(bmode) then
                coerceunit(b,ti64)
                p.pclop:=ksubrefoff
                p.mode:=amode
                return
            fi
        fi

    when keq, kne, klt, kle, kge, kgt then
        if dobinnumx(p,a,b) then
            p.mode:=tbool
            return
        fi
        p.mode:=tbool
        if ttisref[amode] and ttisref[bmode] then
            if not comparemodes(amode, bmode) then
                txerror("Cmp ref/ref not compat")
            fi
            return
        fi
        if p.pclop in [keq, kne] then
            if comparemodes(amode, bmode) then
                return
            fi
        fi

    when kmul then
        if dobinnumx(p,a,b) then return fi

    when kdiv then
        if isnumi(amode) and isnumi(bmode) then p.pclop:=kidiv; goto doidiv fi
        if dobinnumf(p,a,b) then return fi
        if isnum(amode) and isnum(bmode) then
            p.mode:=tr64
            coerceunit(a,tr64)
            coerceunit(b,tr64)
            return
        fi

    when kidiv, kirem, kidivrem, kiand, kior, kixor then
doidiv::
        if dobinnumi(p,a,b) then return fi

    when kmin, kmax then
        if dobinnumx(p,a,b) then return fi

    when kpower then
        if dobinnumx(p,a,b) then return fi

    when kfmod, katan2 then
        if dobinnumf(p,a,b) then return fi

    when kshl, kshr then
        if isnumi(amode) then
            coerceunit(b,ti64)
            p.mode:=amode
            return
        fi

    when kin, knotin then
        doin(p,a,b)
        return

    when kandl, korl then
        p.mode:=tbool
        if amode=bmode=tbool then return fi

    else
        txerror("txbin?")
    end switch

    TXERROR_SS("BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode))
end

proc tx_binto(unit p,a,b)=
    int abase, bbase, amode,bmode, opc

    tpasslv(a)
    tpass(b)

    amode:=a.mode
    bmode:=b.mode

    abase:=ttbasetype[amode]
    bbase:=ttbasetype[bmode]

    if p.pclop=kdivto and ttisinteger[abase] then
        p.pclop:=kidivto
    fi

    p.mode:=tvoid

    case p.pclop
    when kaddto then                !ref+ref not allowed; or ref+int (later refchar+refchar)
        if abase=tref and bbase=tref then
            txerror("to:ref+ref")
        fi
        if abase=tref and bbase<=tlastnum then
            coerceunit(b,ti64)
            p.pclop:=kaddrefoffto
            return
        fi
    when ksubto then                !ref-int
        if abase=tref and bbase<=tlastnum then
            coerceunit(b,ti64)
            p.pclop:=ksubrefoffto
            return
        fi
    when kshlto, kshrto then
        coerceunit(b,ti64)
        return
    esac

    if isnum(abase) and isnum(bbase) then   !num op num
        coerceunit(b,abase)

    elsif ttisshort[abase] and isnum(bbase) then
        coerceunit(b,abase)

    else
        if not comparemodes(amode,bmode) then
            txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
        fi
    fi
end

function getdominantmode(int amode,bmode)int=
    int abase,bbase

    abase:=ttbasetype[amode]
    bbase:=ttbasetype[bmode]

    if isnum(abase) and isnum(bbase) then
        return max(abase,bbase)
    fi
    if not comparemodes(amode, bmode) then
        txerror("Getdom: no dominant mode")
    fi
    return amode
end

proc tx_cmpchain(unit p,a)=
    int u,genop
    unit q,r

    q:=a
    while q do
        tpass(q,tany)

        if q=a then
            u:=q.mode
        else
            u:=getdominantmode(u,q.mode)
        fi

        q:=q.nextunit
    od

    q:=a
    r:=a.nextunit
    while q do
        coerceunit(q,u)
        q:=q.nextunit
    od

    p.mode:=tbool64
end

proc tx_callproc (unit p,a,pargs,int t)=
!deal with both callproc and callfn (perhaps calldll too)
    unit q
    symbol d,e,pm
    [maxparams]symbol paramlist
    [maxparams]unit arglist,newarglist
    int nparams,i,j,k,nargs,m,kwdused,qm, ismproc
    ichar name

    tpass(a)

    nargs:=nparams:=0
    ismproc:=0

    retry::

    case a.tag
    when jname then
        d:=a.def

        if d.nameid in [procid, dllprocid] then
            ismproc:=d.nameid=procid
getparams::
            e:=d.deflist
            while e do
                if e.nameid=paramid then
                    if nparams>=maxparams then txerror("Param overflow") fi
                    paramlist[++nparams]:=e
                fi
                e:=e.nextdef
            od

        else                    !assume fn ptr
            while ttbasetype[a.mode]=tref do
                insertunit(a,jptr)
                a.mode:=tttarget[a.mode]
            od
            goto dorefproc
        fi

    when jif,jselect then
        TXERROR("Can't do ifx/function")

    else
    dorefproc::
        if a.tag=jdot then
            tmethodcall(p,a,pargs)
            a:=p.a
            pargs:=p.b
            goto retry
        fi

        if ttbasetype[a.mode]<>tproc then
            txerror("Function pointer expected")
        fi

        d:=ttnamedef[a.mode]

        if d=nil then txerror("Function expected") fi
        goto getparams
    esac

    q:=pargs
    while q do
        if nargs>=maxparams then txerror("Param overflow") fi
        arglist[++nargs]:=q
        q:=q.nextunit
    od

    p.mode:=d.mode              !type returned by function (will be void for procs)

    if p.mode=tvoid and p.tag=jcallfn then
        p.tag:=jcallproc
    fi

    if p.mode and t<>tvoid then
        twiden(p,nolv)
    fi

    if d.varparams then
        for i to nargs do

            if i<=nparams then
                tpass(arglist[i],paramlist[i].mode)
            else
                tpass(arglist[i])
            fi
        od
        if t=tvoid then
            p.tag:=jcallproc
        fi
        return

    fi

!I have formal params in paramlist, and actual args in arglist
!Now create new set of args in arglist, which maps any keyword parameters,
!while missing args (represented as nullunit) replaced with nil

!Create new set of actual parameters in params(), with optional/default values filled in
!and any conversions applied
    k:=0
    kwdused:=0
    for i to nparams do
        newarglist[i]:=nil
    od

    for i to nargs do
        q:=arglist[i]
        switch q.tag
        when jkeyword then
            name:=q.a.def.name
            for j to nparams do
                if eqstring(paramlist[j].name,name) then
                    exit
                fi
            else
                txerror_s("Can't find kwd param: #",name)
            od

            if newarglist[j] then
                txerror_s("Kwd: # already used or was implicit",name)
            fi
            newarglist[j]:=q.b
            kwdused:=1

        when jnull then         !missing param
            if kwdused then
                txerror("Normal param follows kwd")
            fi
            q:=nil
            goto doregparam
        else
    doregparam::
            if kwdused then
                txerror("Normal param follows kwd")
            fi
            if k>=nparams then
                cpl =k, =nparams
                txerror("Too many params supplied")
            fi
            newarglist[++k]:=q
        endswitch
    od

!scan params, and fill in optional/default params as needed

    for i to nparams do
        q:=newarglist[i]            !will be nil of not supplied
        pm:=paramlist[i]            !formal param (an st entry)
        if q=nil then
            unless pm.optional then
                txerror_s("Param not optional: #",strint(i))
            end
            if pm.code then     !provide default value
                newarglist[i]:=duplunit(pm.code,p.pos)
            else
                newarglist[i]:=createconstunit(0,ti64)
            fi
        fi
    od

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
    unit ulist:=nil, ulistx:=nil

    for i to nparams do
        pm:=paramlist[i]
        q:=newarglist[i]

        if pm.parammode=out_param then
            tpass(q,m:=tttarget[pm.mode],needlv)
            qm:=q.mode

            if not comparemodes(qm,m) then
                txerror_ss("&param: type mismatch",strmode(qm), strmode(m))
            fi

            UNLESS CTARGET AND Q.TAG=JCONVERT THEN
                insertunit(q,jaddrof)
                q.mode:=pm.mode
            ELSE
                Q.TAG:=JADDROF
                q.a.mode:=pm.mode
            END
            setsimple(q)
        else
            tpass(q,pm.mode)
        fi

        addlistunit(ulist, ulistx, q)
        q.nextunit:=nil
    od
    p.b:=ulist

    if t=tvoid then
        p.tag:=jcallproc
    fi
end

proc tx_unary(unit p,a)=
    int opc,size,amode,mbase,tmax,x,xhigh, resmode

    tpass(a)
    amode:=a.mode
    resmode:=amode

    switch p.pclop
    when klwb, kupb, klen, kbounds then
        do_bounds(p,a)
        return

    when kbytesize,kbitwidth then
        size:=ttsize[(a.tag=jtypeconst|a.value|amode)]*(p.pclop=kbytesize|1|8)
        makenewconst(p,size)
        resmode:=ti64

    when kminvalue, kmaxvalue then
        resmode:=ti64
        if a.tag=jtypeconst then
            mbase:=ttbasetype[a.value]
        else
            mbase:=ttbasetype[getmemmode(a)]
        fi

        if p.pclop=kminvalue then
            case mbase
            when ti8 then x:=-128
            when ti16 then x:=-32768
            when ti32 then x:=-2_147_483_648
            when ti64 then x:=int64.minvalue
            when tu8,tu16,tu32,tu64,tc8,tc64 then x:=0
            else
               txerror_s("Can't do minvalue on #",strmode(mbase))
            esac
        else
            case mbase
            when ti8 then x:=127
            when ti16 then x:=32767
            when ti32 then x:=2_147_483_647
            when ti64 then x:=0x7fff'ffff'ffff'ffff
            when tu8,tc8 then x:=255
            when tu16 then x:=65535
            when tu32 then x:=4294967295
            when tu64 then x:=0; --x; resmode:=tu64
            else
                txerror_s("Can't do maxvalue on #",strmode(mbase))
            esac
        fi
        p.tag:=jconst
        p.a:=nil
        p.value:=x
        p.isconst:=1
        p.simple:=1

!   when katan, kln, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos then
    when katan, kln, kexp, ksin,kcos,ktan, kasin, kacos then
        coerceunit(a,tr64)
        resmode:=tr64

    when ksqrt then
        if amode not in [tr32, tr64] then
            coerceunit(a,tr64)
            resmode:=tr64
        fi

    when ktypestr then
        p.tag:=jconst
        if a.tag=jtypeconst then
            amode:=a.value
        else
            amode:=getmemmode(a)
        fi

        p.mode:=trefchar
        p.svalue:=pcm_copyheapstring(strmode(amode))
        p.isastring:=1
        p.length:=strlen(p.svalue)
        return
    when ksliceptr then
        tx_sliceptr(p,a)
        return

    when kinot then
        if not ttisinteger[amode] then txerror("Inot") fi

    when kneg, kabs, ksqr, klog, kfloor, kceil then
        if not ttisinteger[amode] and not ttisreal[amode] then txerror("Neg/Abs?") fi
    else
        TXERROR("TX:UNARY NOT CHECKED")

    endswitch

    p.mode:=resmode
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
    unit pc:=pcond, pl:=plist
    int u

    u:=tvoid
    if t<>tany then u:=t fi

    while pc, (pc:=pc.nextunit; pl:=pl.nextunit) do
        tpass(pc)
        tpass(pl,t,lv)

        if t=tany then
            if u=tvoid then
                u:=pl.mode
            else
                u:=getdominantmode(u,pl.mode)
            fi
        fi
    od

    if t<>tvoid and pelse=nil then
        txerror("else needed")
    fi
    tpass(pelse,t,lv)

    if t=tany then
        u:=getdominantmode(u,pelse.mode)
    fi

    if t<>tvoid then
        pl:=plist
        while pl, pl:=pl.nextunit do
            if t=tany then
                coerceunit(pl,u)
            fi
        od
        if t=tany then
            coerceunit(pelse,u)
        fi
        p.mode:=u
    fi

    if pcond.nextunit=plist.nextunit=nil then
        if iscondtrue(pcond) then       !branch b only
            if not ctarget or plist.tag=jconst then
                deleteunit(p,plist)
            fi
        elsif iscondfalse(pcond) then   !branch c only
            if pelse=nil then
                pelse:=createunit0(jblock)
            fi
            if not ctarget or pelse.tag=jconst then
                deleteunit(p,pelse)
            fi
        fi
    fi

!PRINTLN "SS1"
    setsimple(p)

end

proc tx_incrto(unit p,a,int t)=
    tpasslv(a)

    unless ttisref[a.mode] or ttisinteger[a.mode] then
        txerror("incr not int/ref")
    end

    if t<>tvoid then
        case p.pclop
        when kincr then p.pclop:=kincrload
        when kdecr then p.pclop:=kdecrload
        esac
        p.mode:=gettypebase(a.mode)

    else                !a++ a-- to ++a --a
        case p.pclop
        when kloadincr then p.pclop:=kincr
        when kloaddecr then p.pclop:=kdecr
        esac
        p.mode:=tvoid
    fi

    twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
    unit pto, pstep, plocal, plist, passign
    int u

    pto:=pfrom.nextunit
    pstep:=pto.nextunit
    passign:=pindex.nextunit

    tpass(pindex)

    if pindex.tag<>jname then
        txerror("Loop index not a variable")
    fi
    u:=pindex.mode

    if passign then
        tpass(passign)
        if passign.b.tag in [jconst, jname] then
            pindex.nextunit:=nil
            pto:=pfrom.nextunit:=passign.b
            pto.nextunit:=pstep
            passign.a.def.used:=0
        fi
    fi



    tpass(pfrom,u)
    tpass(pto,u)

    tpass(pstep,u)

    tpass(pbody,tvoid)
    tpass(pbody.nextunit,tvoid) !optional else
end

proc tx_forall(unit pindex,plist,pbody)=
    unit plocal,pfrom,pto,passign
    int u,mlist,elemtype

    plocal:=pindex.nextunit
    pfrom:=plocal.nextunit
    pto:=pfrom.nextunit
    passign:=plist.nextunit

    tpass(pindex,ti64)
    tpass(pfrom,ti64)
    tpass(pto,ti64)

    tpass(plist)
    mlist:=plist.mode

    case ttbasetype[mlist]
    when tarray then
        elemtype:=tttarget[mlist]
    when tslice then
        elemtype:=tttarget[mlist]
    else
        txerror("forall/can't iterate")
    esac

    tpass(plocal)
    if plocal.mode=tany then
        plocal.mode:=elemtype
        plocal.def.mode:=elemtype
    fi

    tpass(passign)

    tpass(pbody,tvoid)
    tpass(pbody.nextunit,tvoid) !optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
    int amode,emode,pmode,tmode,tbasemode

    tpass(a,,lv)
    deref(a,t<>tvoid)
    amode:=a.mode

    tpass(b,ti64)           !index

    if ttbasetype[amode] not in [tarray, tslice] then
        txerror_s("Can't index: #",strmode(amode))
    fi
    p.mode:=tttarget[amode]
    twiden(p,lv)

    setsimple(p)
end

proc tx_makerange(unit p,a,b)=
    int amode,bmode

    tpass(a,ti64)
    tpass(b,ti64)

    amode:=a.mode
    bmode:=b.mode

    coerceunit(a,ti64)
    coerceunit(b,ti64)
    setsimple(p)
    p.mode:=trange
end

proc tx_ptr(unit p,a,int t,lv)=
    symbol d

    tpass(a)

    case ttbasetype[a.mode]
    when tvoid then
        txerror("Deref Void")
    when tref then
        p.mode:=tttarget[a.mode]

    when tslice then
        txerror("Can't deref slice")
    else
        txerror("PTR: need ref T")
    esac

    setsimple(p)

    twiden(p,lv)
end

proc setrecordsize(int m)=
    [maxfields+8]symbol fieldlist
    int i,nfields,indent,nrfields,size,index, maxalign
    symbol d,e
    ref char flags
    const ss='S', ee='E'
    int flag

    if ttsize[m] then return fi

    d:=ttnamedef[m]
    e:=d.deflist
    nfields:=0

    fieldlist[++nfields]:=symbol@(ss)

    while e do
        if e.nameid=fieldid then
            if nfields>=maxfields then
                gerror("srs:too many fields")
            fi

            setmodesize(e.mode)
            flags:=cast(&e.uflags)
            docase flags^
            when 'S', 'U' then
                flag:=flags^
                fieldlist[++nfields]:=symbol@(flag)
                ++flags
            else
                exit
            end docase

            fieldlist[++nfields]:=e

            do
                flag:=flags++^
                case flag
                when '*'  then
                when 'E' then
                    fieldlist[++nfields]:=symbol@(ee)
                else
                    exit
                esac
            od
        fi

        e:=e.nextdef
    od

    fieldlist[++nfields]:=symbol@(ee)
    fieldlist[nfields+1]:=nil           !terminator

    countedfields:=0
    index:=2
    maxalign:=1
    scanrecord('S',&fieldlist,index,size,0, d.align, maxalign)

    if d.align then
        size:=roundoffset(size,maxalign)
        d.maxalign:=maxalign
    else
        d.maxalign:=1
    fi

    ttsize[m]:=size
    ttlength[m]:=countedfields
    ttlower[m]:=1

    checkblocktype(m)
end

proc checkblocktype(int m)=
    case ttsize[m]
    when 1,2,4 then
        ttisblock[m]:=0
        ttcat[m]:=shortcat
    when 8 then
        ttisblock[m]:=0
        ttcat[m]:=d64cat
    esac
end

proc scanrecord(int state,ref[]symbol fields, int &index, &isize, offset, calign, &maxalign)=
    symbol e,f,ea
    int size:=0,fieldsize,bitoffset:=0, alignment, newoffset

    while f:=fields^[index++] do
        case int(f)
        when 'S','U' then
            scanrecord(int(f),fields, index,fieldsize, offset, calign, maxalign)
        when 'E' then           !end of this nested block
            if state='U' then ++countedfields fi
            isize:=size
            return
        else
            if f.mode=tbitfield then
                fieldsize:=0
                ea:=f.equivfield
                f.offset:=ea.offset
                f.bitoffset:=bitoffset
                bitoffset+:=f.bitfieldwidth
                if bitoffset>ttsize[f.equivfield.mode]*8 then
                    txerror("Bit fields overflow type")
                fi

            elsif f.atfield then
                if ctarget then gerror("Field@") fi
                bitoffset:=0
                e:=f.equivfield
                fieldsize:=0
                f.offset:=e.offset+f.equivoffset
            else
                bitoffset:=0
                if state='S' then ++countedfields fi
                fieldsize:=ttsize[f.mode]
                if calign then
                    alignment:=getalignment(f.mode)
                    if alignment>maxalign then maxalign:=alignment fi
                    newoffset:=roundoffset(offset,alignment)
                    size+:=newoffset-offset
                else
                    newoffset:=offset
                fi
                f.offset:=newoffset
                offset:=newoffset
            fi
        esac
        if state='S' then
            offset+:=fieldsize
            size+:=fieldsize
        else
            size:=max(size,fieldsize)
        fi
    od
end

function roundoffset(int offset, alignment)int=
    int mask

    if alignment=1 then return offset fi
    mask:=alignment-1
    while offset iand mask do ++offset od

    return offset
end

proc tx_convert(unit p,a,int hard=0)=
!PRINTLN "CONV",JTAGNAMES[A.TAG]
    case a.tag
    when jmakelist then
        tx_makelist(a,a.a,p.convmode,nolv)
    else
        tpass(a)
        coerceunit(a,p.convmode,hard)
    esac
    deleteunit(p,a)         !get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
    int alength,tlength,elemtype,newt, i, nfields,isconst, m
    unit q,b
    symbol e

    alength:=p.length
    newt:=0
    isconst:=1

    tlength:=ttlength[t]

    if tlength then
        if alength<tlength then
            txerror_ss("Too few elements",strint(alength), strint(tlength))
        elsif alength>tlength then
            txerror_ss("Too many elements",strint(alength), strint(tlength))
        fi
    fi

    case ttbasetype[t]
    when tarray then
        elemtype:=tttarget[t]
        if tlength=0 then
            newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
        else
            newt:=t
        fi
        q:=a
        while q do
            tpass(q,elemtype,lv)

            unless q.tag=jconst then isconst:=0 end
            q:=q.nextunit
        od

        p.mode:=newt

    when trecord then
        e:=ttnamedef[t].deflist
        q:=a
        while q and e do
            if e.nameid=fieldid then 
                while e.mode=tbitfield do
                    e:=e.nextdef
                    if not e then exit fi
                od

                tpass(q,e.mode,lv)

                unless q.tag=jconst then isconst:=0 end
                q:=q.nextunit
            fi

            e:=e.nextdef
        od
        while e and (e.nameid<>fieldid or e.mode=tbitfield) do
            e:=e.nextdef
        od
        if q or e then
            txerror("Can't initialise unions")
        fi
        p.mode:=t

    else
        txerror_s("Unknown makelist type: #",strmode(t))
    esac

    p.isconst:=isconst

    tpass(p.b,ti64)             !lower

    if not inidata and isconst then
        e:=getavname(currproc,staticid)
        e.mode:=t
        addstatic(e)
        q:=createunit0(jnone)
        q^:=p^
        e.code:=q
        p.tag:=jname
        p.def:=e
    fi

end

proc tx_makeslice(unit p,a, int t)=
    if p.length<>2 then txerror("slice:=[a,b]") fi

    p.b:=a.nextunit
    a.nextunit:=nil
    tpass(a)

    if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
    if tttarget[a.mode]<>tvoid then
        if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
            txerror("slice/ptr mismatch")
        fi
    fi

    tpass(p.b,ti64)
    p.mode:=t
    p.tag:=jmakeslice
    p.resultflag:=1

    tpass(p.b,ti64)

end

proc tx_makeset(unit p,a, int t)=
    p.isconst:=1

    if ttbasetype[t]=tslice then
        tx_makeslice(p,a,t)
        return
    fi

    while a, a:=a.nextunit do
        tpass(a)

        if not a.isconst then
            p.isconst:=0
        fi
    od

    p.mode:=tvoid
end

proc tx_dot(unit p,a,b,int lv)=
    int recmode,recbasemode,i,j,newtag,tmode
    unit q,pindex
    symbol d,dequiv

    tpass(a)            !lhs, yeields ref array type
    setsimple(a)

    recmode:=a.mode
    recbasemode:=ttbasetype[recmode]

    while recbasemode=tref do
        tmode:=tttarget[recmode]
        insertunit(a,jptr)
        setsimple(a)
        recmode:=a.mode:=tmode
        recbasemode:=ttbasetype[recmode]
    od

    if ttbasetype[recmode]<>trecord then
        txerror("Bad record type")
    fi

    d:=b.def

    if d.nameid=nullid then         !not resolved; lhs mode wasn't available
        d:=b.def:=resolvefield(d,recmode)
    fi

    if d.mode=tbitfield then
        i:=d.bitoffset
        j:=i+d.bitfieldwidth-1
        dequiv:=d.equivfield
        b.def:=dequiv               !change from bitfield field to containing int
        b.mode:=dequiv.mode
        p.offset:=d.offset

        if i=j then                 !single bit
            pindex:=createconstunit(i,ti64)
            newtag:=jdotindex
        else                        !bit slice
            pindex:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
            pindex.mode:=trange
            pindex.a.resultflag:=1
            pindex.b.resultflag:=1
            pindex.simple:=1
            newtag:=jdotslice
        fi

        p.mode:=b.mode

        twiden(p,lv)

        setsimple(p)

        insertunit(p,newtag)
        p.mode:=tu64
        p.b:=pindex
        p.a.resultflag:=1
        p.b.resultflag:=1
        p.resultflag:=1
        setsimple(p)
        return

    fi

    b.mode:=d.mode
    p.mode:=d.mode

    setsimple(p)

    p.offset:=d.offset
    twiden(p,lv)
end

function resolvefield(symbol d, int m)symbol=
    symbol e,t

    case ttbasetype[m]
    when trecord then
    when tref then
        m:=tttarget[m]
        if ttbasetype[m]<>trecord then
            txerror("3:record expected")
        fi
    else
        txerror("4:record expected")
    esac
    t:=ttnamedef[m]

    e:=finddupl(t,d)
    if not e then
        txerror_s("Not a field: #",d.name)
    fi
    return e
end

proc tx_andl(unit p,a,b)=
    tpass(a,tbool)
    tpass(b,tbool)
!   if not isbooltag[a.tag] then insertunit(a,jistruel); a.pclop:=kistruel fi
!   if not isboolunit(b) then insertunit(b,jistruel); b.pclop:=kistruel fi

    p.mode:=tbool
    setsimple(p)

!   if p.tag=jandl then
!       if afalse or bfalse then        !result is false
!           makenewconst(p,0,tbool)
!       elsif atrue then
!           deleteunit(p,b)
!       elsif btrue then
!           deleteunit(p,a)
!       fi
!   else                                !assume orl
!       if abtrue or btrue then         !result is true
!           makenewconst(p,1,tbool)
!       elsif afalse then
!           deleteunit(p,b)
!       elsif bfalse then
!           deleteunit(p,a)
!       fi
!   fi

end

proc convintconst(unit p,int64 x)=
!convert unit p into int const x
    p.tag:=jconst
    p.mode:=ti64
    p.a:=p.b:=p.c:=nil
    p.value:=x
    p.isconst:=1
end

proc tx_sliceptr(unit p,a)=
    int m,tmode

    m:=a.mode

    case ttbasetype[m]
    when tslice then
    else
        txerror_s("SLICEPTR #",strmode(m))
    esac

!for when ptr is to be pointer to the array
    tmode:=createarraymodek(nil, tttarget[m], ttlower[m],0,0)

!for when ptr is to be pointer to the array element (although either can be
!cast to the other); although try alternate .sliceptr versions too
!tmode:=tttarget[m]

    p.mode:=createrefmode(nil,tmode)
end

proc tx_swap(unit p,a,b)=
    int av, bv

    tpasslv(a)
    tpasslv(b)

    if not comparemodes(a.mode,b.mode) then
        txerror("SWAP: type mismatch")
    fi

    p.mode:=tvoid
end

proc tx_select(unit p,a,b,c, int t,lv)=
    int i,u
    unit q

    tpass(a,ti64)

    q:=b
    while q do
        tpass(q,t,lv)
        if q=b then
            u:=q.mode
        else
            u:=getdominantmode(u,q.mode)
        fi

        q:=q.nextunit
    od

    tpass(c,t,lv)
    u:=getdominantmode(u,c.mode)

    q:=b
    while q do
        coerceunit(q,u)
        q:=q.nextunit
    od

    if t<>tvoid then
        p.mode:=u
    else
        p.mode:=tvoid
    fi
end

proc tx_case(unit p,a,b,c, int t,lv)=
    int amode,u
    unit wt,w

    if p.tag=jdocase and lv then gerror("&docase") fi

    tpass(a)

    if a=nil then
        amode:=tany
    else
        amode:=a.mode
    fi

    if ttisinteger[amode] and ttsize[amode]<8 then
        coerceunit(a,tint)
        amode:=tint
    fi
    u:=tvoid

    wt:=b
    while wt do             !whenthen chain
        w:=wt.a
        while w do              !each expr between when...then
            tpass(w)
            if w.tag=jmakerange then
                unless ttisinteger[amode] then txerror("case: need int index") end
            else
                if amode=tany then
                    if not isbooltag[w.tag] then
                        TXERROR("CASE/BOOL?")
                        insertunit(w,jistruel)
                    fi
                else
                    coerceunit(w,amode)
                fi
            fi
            w:=w.nextunit
        od
        tpass(wt.b,t,lv)            !process block
        if t<>tvoid then
            if u then
                u:=getdominantmode(u,wt.b.mode)
            else
                u:=wt.b.mode
            fi
        fi
        wt:=wt.nextunit
    od

    if c then
        tpass(c,t,lv)
        if t=tany then
            u:=getdominantmode(u,c.mode)
        fi
    elsif t<>tvoid then
        txerror("case needs else")
    fi

    if t<>tvoid then
        p.mode:=u
    else
        p.mode:=tvoid
    fi

end

proc tx_notl(unit p,a)=
    tpass(a)
    p.mode:=tbool
    setsimple(p)
end

proc tx_istruel(unit p,a)=
    int abase

    tpass(a)

    if isbooltag[a.tag] then
        deleteunit(p,a)
        return
    fi

    abase:=ttbasetype[a.mode]
    if abase=tref then abase:=ti64 fi

    p.mode:=tbool
    setsimple(p)
end

proc tx_typepun(unit p,a)=
    int smode
    case a.tag
    when jmakelist then
        TXERROR("TYPEPUN/LIST")
    else
        tpass(a)

        smode:=getmemmode(a)

        if ttsize[smode]<ttsize[p.convmode] then
            txerror("Typepun: sizes must match")
        fi

        p.mode:=gettypebase(p.convmode)
    esac
end

proc tx_exit(unit p,a)=
    if a=nil then return fi
    tpass(a,ti64)
    if a.tag<>jconst then
        txerror("exit/etc not const")
    fi
    p.index:=a.value
    p.a:=nil
end

proc tx_goto(unit p,a)=
    int m

    tpass(a)
    m:=a.mode

    if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
        txerror("goto: not label")
    fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
    [0:2048]byte valueset
    unit wt, w
    int ax,bx,i,u

    if p.tag=jdoswitch and lv then gerror("&doswitch") fi

    tpass(a,ti64)

    memset(&valueset,0,valueset.bytes)
    u:=tvoid

    wt:=b
    while wt do

        w:=wt.a
        while w do
            tpass(w)

            if not isconstunit(w) then txerror("Switch not constant") fi

            case ttbasetype[w.mode]
            when trange then            !assume makerange
                ax:=w.a.value
                bx:=w.b.value
    dorange::
                for i:=ax to bx do
                    if i<valueset.lwb or i>valueset.upb then
                        txerror("switch: value out of range")
                    fi
                    if valueset[i] then
                        cpl i
                        txerror("Duplicate switch value")
                    fi
                    valueset[i]:=1
                od
            else
                coerceunit(w,ti64,0)
                tevaluate(w)
                if w.tag<>jconst then
                    txerror("Switch value: not const int")
                fi
                ax:=bx:=w.value
                goto dorange
            esac
            w:=w.nextunit
        od
        tpass(wt.b,t,lv)

        if t=tany then
            if u then
                u:=getdominantmode(u,wt.b.mode)
            else
                u:=wt.b.mode
            fi
        fi

        wt:=wt.nextunit
    od

    if c then
        tpass(c,t,lv)
        if t=tany then
            u:=getdominantmode(u,c.mode)
        fi
    elsif t<>tvoid then
        txerror("switch needs else")
    fi

    if t<>tvoid then
        w:=b.a
        while w do              !all elseif unots
            if t=tany then
                coerceunit(b.b,u)
            fi
            w.mode:=b.b.mode
            w:=w.nextunit
        od
        if t=tany then
            coerceunit(c,u)
            p.mode:=u
        else
            p.mode:=t
        fi
    else
        p.mode:=tvoid
    fi
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
    int m

    tpass(a)

    m:=a.mode
    if ttbasetype[m]<>tarray then
        txerror("&. ref[] expected")
    fi

    m:=createrefmode(nil,tttarget[m])
    if a.tag=jname then
        a.addroffirst:=1
    fi
    p.mode:=m
end

proc tx_return(unit p,a, int t)=
    int m,nvalues,nret,i
    ref[]int32 pmult
    unit q

    m:=currproc.mode
    nret:=currproc.nretvalues
    pmult:=ttmult[currproc.mode]

    if a=nil then
        if nret then
            txerror("return value(s) missing")
        fi
        return
    elsif nret=0 then
        txerror("Superfluous return value")
    fi

    if a.tag=jmakelist then
        a.tag:=jreturnmult
        if a.length<>nret then
            case ttbasetype[m]
            when trecord, tarray then
                txerror("return constructor not supported")
            else
                txerror("Wrong number of return values")
            esac
        fi
        q:=a.a              !point to list of return values
        for i to nret do
            tpass(q,pmult[i])
            q:=q.nextunit
        od

        deleteunit(p,a)         !don't need return
        p.resultflag:=1
        if t=tvoid then
            p.mode:=tvoid
        else
            p.mode:=ttuple
        fi

    else
        if nret>1 then txerror("RETERROR?") fi
        tpass(a,m)

        if t=tvoid then                 !regular out-of-line return
            p.mode:=tvoid
        else
            deleteunit(p,a)
!           P.MODE:=A.MODE
        fi
    fi

    IF TTISSHORT[P.MODE] THEN TXERROR("SHORT RET TYPE") FI
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
    int pmode
    unit i,j

    tpass(a,,lv)            !lhs

    pmode:=tu64

    if not ttisinteger[a.mode] then
        txerror("a.[i]: not int/str value")
    fi

    tpass(b)            !index

    case ttbasetype[b.mode]
    when trange then
        i:=b.a
        j:=b.b
        if i.tag=j.tag=jconst then
            if i.value>j.value then
                swap(b.a,b.b)
            fi
        fi
    else                    !assume simple index
        coerceunit(b,ti64)
    esac

    p.mode:=pmode
    setsimple(p)

end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

    tpass(a)            !lhs
    tpass(b)            !will be a range

    setsimple(a)

    if a.mode=trefchar then
        p.mode:=createslicemodek(currproc,tc8,1,0)
    else
        deref(a)
        case ttbasetype[a.mode]
        when tarray then
            p.mode:=createslicemodek(currproc,tttarget[a.mode],1, 0)

        when tslice then
            p.mode:=a.mode

        else
            PRINTLN =STRMODE(A.MODE)
            txerror("a[i..j]: not array")
        esac
    fi
end

proc twiden(unit p, int lv)=
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
    int m,u,mbase

    mbase:=ttbasetype[m:=p.mode]

    if mbase=tvoid then return fi       !nothing to widen (error?)
    if lv then return fi                !lv, keep memory mode as dest

    if not ttisshort[mbase] then return fi  !no widening needed
    case p.tag
    when jname, jptr, jindex, jdot then
            p.memmode:=m                !non-void marks this as non-lv too
            p.mode:=gettypebase(m)
    when jcallproc,jcallfn then
        p.memmode:=m
        p.mode:=gettypebase(m)
    else
        PRINTUNIT(P)
        txerror_s("widen? #",jtagnames[p.tag])
    esac
end

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
    unit a,b,prange
    int length

    if tttarget[slicemode]<>tc8 then
        txerror("Not char slice")
    fi
!
    a:=p
    insertunit(p,jslice)


    if p.a.tag=jconst then
    else
        b:=duplunit(p.a)
        insertunit(b,junary)
        prange:=createunit2(jmakerange,createconstunit(1,ti64),b)

        prange.mode:=trange
        p.b:=prange
    fi

    p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
    int i,j,bitsize,topbit
    unit r

    tpass(a,,lv)

    if not ttisinteger[a.mode] and not ttisref[a.mode] then
        txerror("Int/ref needed")
    fi

    bitsize:=ttsize[ttbasetype[a.mode]]*8
    topbit:=bitsize-1

    case p.bfcode
    when bf_lsb then
        i:=0; j:=7

    when bf_msb then
        j:=topbit
        i:=topbit-7

    when bf_lsbit then
        i:=j:=0

    when bf_odd,bf_even then
        if lv then
            txerror("Can't assign")
        fi
        i:=j:=0

    when bf_msbit then
        i:=j:=topbit

    when bf_lsw then
        i:=0
        j:=bitsize/2-1

    when bf_msw then
        i:=bitsize/2
        j:=topbit
    else
        PRINTLN P.BFCODE
        TXERROR("BITFIELD")
    esac

    if i=j then         !single bit
        p.tag:=jdotindex
        p.b:=createconstunit(i,ti64)
        p.resultflag:=1
        p.b.resultflag:=1

        if p.bitopindex=bf_even then
            p.mode:=tu64
            addnotl(p)
        fi

    else
        r:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
        r.a.resultflag:=1
        r.b.resultflag:=1
        r.mode:=trange
        p.tag:=jdotslice
        p.b:=r
    fi

    p.mode:=tu64
end

proc deref(unit a, int needres=1)=
!a is a unit that needs to be dereferenced because it's about to used as:
! a[i]
! a[i..j]
! a.lwb, a.upb, a.len
!Ie in an array context
    int abasemode, tmode

    abasemode:=ttbasetype[a.mode]

    while abasemode=tref do
        tmode:=tttarget[a.mode]

        insertunit(a,jptr)
        setsimple(a)
        a.mode:=tmode

        abasemode:=ttbasetype[a.mode]
    od

end

proc tmethodcall(unit p, pdot, pargs)=
    int mrec
    unit prec, pfield, pfunc
    symbol d,e

    prec:=pdot.a
    pfield:=pdot.b
    mrec:=prec.mode
    d:=pfield.def

    e:=resolvefield(d,mrec)

    if e=nil then
        txerror_s("Can't resolve method:",d.name)
    fi

    pfunc:=createname(e)
    pfunc.mode:=e.mode
    prec.nextunit:=pargs

    p.a:=pfunc
    p.b:=prec
end

proc do_bounds(unit p,a) =
    int m,mbase,opc,lower,upper

    deref(a)

    m:=a.mode
    if a.tag=jtypeconst then m:=a.value fi

    mbase:=ttbasetype[m]
    p.mode:=ti64

    case p.pclop
    when klwb then
        case mbase
        when tarray,tslice then
            convintconst(p,ttlower[m])
            return
        else
error::
            txerror_s("lwb/upb/len?",strmode(m))
        esac

    when kupb then
        case mbase
        when tarray then
            convintconst(p,ttlower[m]+ttlength[m]-1)
        when tslice then
            p.pclop:=kupb
        else
            goto error
        esac

    when klen then
        case mbase
        when tarray then
            convintconst(p,ttlength[m])
        when tslice then
            p.pclop:=klen
        else
            goto error
        esac
    when kbounds then
        p.mode:=trange
        case mbase
        when tarray then
            p.range_lower:=ttlower[m]
            p.range_upper:=p.range_lower+ttlength[m]-1
            p.tag:=jconst
            p.a:=p.b:=p.c:=nil
            p.isconst:=1
            return

        when tslice then
!       when ti32 then
!           convintconst(p,int32.max-int32.min+1)
!           return
        else
            goto error
        esac
    esac
end

proc addnotl(unit p)=
    insertunit(p,jnotl)
    p.mode:=tbool
    p.pclop:=knotl
end

proc tevaluate(unit p)=
    unit a,b,pname
    int offset

    int tag:=p.tag

!PRINTLN "EVAL",JTAGNAMES[P.TAG],=JISEXPR[TAG]

    if jisexpr[tag]=2 then
        tevalbinop(p)
            setsimple(p)
!       fi

    elsif jisexpr[tag]=1 then
        tevalmonop(p)
        if p.tag=junary and not complexopset[p.pclop] then
            setsimple(p)
        fi

    elsecase tag
    when jmakerange then
        a:=p.a
        b:=p.b
        if ttsize[a.mode]<=8 then           !const range only for 32-bits
            tevaluate(a)
            tevaluate(b)
            if a.tag=jconst and b.tag=jconst then
                p.isconst:=a.isconst iand b.isconst
            fi
        fi
!   when jaddrof then
!       a:=p.a
!
!       pname:=addrdotindex(a, offset)
!
!       if pname then
!           deleteunit(a,pname)
!           if p.b=nil then
!               p.b:=createconstunit(offset,ti64)
!           else 
!               p.b.value+:=offset
!           fi
!       fi
    fi

end

function addrdotindex(unit p, int &offset)unit q=
    int axmode

    case p.tag
    when jdot then
        if p.a.tag=jname then
            offset:=p.offset
            return p.a
        else
            q:=addrdotindex(p.a,offset)
            offset+:=p.offset
            return q
        fi
    when jindex then
        axmode:=p.a.mode
        if p.b.tag=jconst then
            if p.a.tag=jname then
                offset:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
                return p.a
            else
                q:=addrdotindex(p.a,offset)
                if q then
                    offset+:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
                fi
                return q
            fi
        else
            return nil
        fi
    else
        return nil
    esac

end

proc tevalbinop(unit p)=
    int64 a,b,c,offset
    real x,y,z
    unit lhs, rhs

    lhs:=p.a
    rhs:=p.b

    unless lhs.tag=rhs.tag=jconst then
        if lhs.tag=jaddrof and rhs.tag=jconst then
            if lhs.a.tag=jname then         !reduce addrof(a)+k => addrof(a,k)
                offset:=rhs.value*ttsize[tttarget[lhs.mode]]
                if lhs.b=nil then
                    lhs.b:=createconstunit(offset,ti64)
                else
                    lhs.b.value+:=offset
                fi
                deleteunit(p,lhs)
            fi
        fi
        return
    end

    if ttisreal[p.mode] then
        x:=p.a.xvalue
        y:=p.b.xvalue
    else
        a:=p.a.value
        b:=p.b.value
    fi

    case p.mode
    when ti64, tu64 then

        switch p.pclop
        when kadd then c:=a+b
        when ksub then c:=a-b
        when kmul then c:=a*b
        when kidiv then c:=a/b
        when kirem then c:=a rem b
        when kshl then c:=a<<b
        when keq then c:=a=b
        when kne then c:=a<>b
        when klt then c:=a<b
        when kle then c:=a<=b
        when kge then c:=a>=b
        when kgt then c:=a>b
        when kandl then c:=a and b
        when korl then c:=a or b
        when kiand then c:=a iand b
        when kior then c:=a ior b
        else
            return
        end

    when tr64,tr32 then

        switch p.pclop
        when kadd then z:=x+y
        when ksub then z:=x-y
        when kmul then z:=x*y
        when kdiv then z:=x/y

        else
            return
        end
    else
        return
    esac
!
    if ttisreal[p.mode] then
        makenewconst(p,int64@(z))
    else
        makenewconst(p,c)
    fi
end

proc tevalmonop(unit p)=
    int64 a,b,c
    real x,z

    if p.tag=jcmpchain then return fi

    unless p.a.tag=jconst then
        return
    end

    a:=p.a.value
    x:=p.a.xvalue

    case p.mode
    when ti64, tu64 then

        switch p.pclop
        when kneg then c:=-a

        when kistruel then c:=istrue a; p.mode:=tbool
        when knotl then c:=not a; p.mode:=tbool
        when kinot then c:=inot a
        when kabs then c:=abs a

        else
            return
        end switch
    when tr64, tr32 then
        switch p.pclop
        when kneg then z:=-x
        when katan then z:=atan(x)
        when ksqrt then z:=sqrt(x)

        else
            return
        end switch

    when tbool then
        case p.pclop
        when kistruel then c:=istrue a; p.mode:=tbool
        when knotl then c:=not a; p.mode:=tbool
        esac
    else
        return
    esac

    if ttisreal[p.mode] then
        makenewconst(p,int64@(z))
    else
        makenewconst(p,c)
    fi
end

function iscondtrue(unit p)int =
    p.tag=jconst and p.value<>0
end

function iscondfalse(unit p)int =
    p.tag=jconst and p.value=0
end

proc fixchararray(unit a)=
!turn []char into ichar at certain points
    if a and ttbasetype[a.mode]=tarray and tttarget[a.mode]=tc8 then
        coerceunit(a,trefchar,0)
    fi
end

proc combinestrings(unit p)=
!p is (add, a, b) where a and b are string constants.
    unit a:=p.a, b:=p.b
    int alen:=a.length
    int blen:=b.length
    int clen:=alen+blen
    ichar s

    if blen=0 then
        deleteunit(p,a)
        return
    elsif alen=0 then
        deleteunit(p,b)
        return
    fi

    s:=pcm_alloc(clen+1)
    memcpy(s,a.svalue,alen)
    memcpy(s+alen,b.svalue,blen)
    (s+clen)^:=0

    deleteunit(p,a)
    p.length:=clen
    p.svalue:=s

end

proc tx_strinclude(unit p,a)=
    int fileno

    tpass(a)
    if a.tag<>jconst or not a.isastring then
        txerror("strincl/not string")
    fi

    fileno:=moduletable[p.moduleno].fileno

    fileno:=getsupportfile(a.svalue,path:sourcefilepaths[fileno],issupport:1)

    a.svalue:=sourcefiletext[fileno]
    a.slength:=sourcefilesizes[fileno]
!
    deleteunit(p,a)
end

proc coerceunit(unit p, int t, hard=0)=
    int opc, s:=p.mode

    if t=tvoid or s=t then return fi
    if s=tvoid then
        txerror("Void expression/return value missing")
    fi

    opc:=getconversionop(s,t, hard)
    applyconversion(p,s,t,opc)
    setsimple(p)
end

function getconversionop(int s, t, hard)int opc=
!return pcl code needed to convert s=>t:
!  0 (or kzero) means none needed (eg s=t)
!  ksoftconv means a no-op conversion, but it may still need a conversion
!   node, that maps to nop, to indicate the new type
!  kerror means no conversion is possible
!Assume neither s or t are void (checked by caller), so that <lastnum means numeric

    int sbase:=ttbasetype[s]
    int tbase:=ttbasetype[t]

    if s=t then return 0 fi

    opc:=kerror
    int starg:=tttarget[s]
    int ttarg:=tttarget[t]

    if s=trefchar then sbase:=trefchar fi
    if t=trefchar then tbase:=trefchar fi

    switch sbase
    when tfirstnum..tlastnum then
        switch tbase
        when tfirstnum..tlastnum then
            opc:=softconvtable[sbase,tbase]
        when tref, trefchar then
            opc:=ksoftconv
checkhard::
            if not hard then opc:=kharderror fi
        when tfirstshort..tlastshort then
            if ttisinteger[sbase] then
                if not hard then                !needed for idata init
                    opc:=ksofttruncshort
                else
                    opc:=ktruncate
                fi
            fi
        when tbool then
            opc:=kistruel
        when ttype then
            opc:=ksoftconv
        end switch

    when tbool then
        if tbase in [ti64, tu64] then
            opc:=ksoftconv
        fi

    when tref then
        case tbase
        when ti64, tu64 then
            opc:=ksoftconv
            checkhard
        when tref then
            if starg=tvoid or ttarg=tvoid then          !at least one is ref void
IF CTARGET AND TTARG=TVOID THEN RETURN 0 FI
                opc:=ksoftconv
            else
checkref::
                opc:=ksoftconv
                if not comparemodes(s,t) then
                    checkhard
                fi
            fi
        when trefchar then
            checkref
        when tbool then
            opc:=kistruel
        end

    when trefchar then
        case tbase
        when ti64,tu64 then
            opc:=ksoftconv
            checkhard
        when tref then
            if comparemodes(s,t) or hard then
                opc:=ksoftconv
            else
                opc:=kharderror
            fi
        when tbool then
            opc:=kistruel
        when tslice then
!           if ttarg not in [tc8, tu8] then
                opc:=kichartoslice
!           fi
        end

    when tarray then
        case tbase
        when tarray then
            if comparemodes(s,t) then
                opc:=ksoftconv
            fi
        when tslice then
            if comparemodes(starg, ttarg) then
                opc:=karraytoslice
            fi

        when trefchar then
            if starg in [tc8, tu8] then
                opc:=kcharaxtoichar
            fi
        esac

    when tslice then
        case tbase
        when tslice then
            if comparemodes(s,t) then
                opc:=ksoftconv
            fi
        when tref then
            if ttarg=tvoid or comparemodes(starg, ttarg) then
                opc:=ksliceptr
            fi

!       when tbool then
        esac

    when ttype then
        if tbase<=tlastnum then
            opc:=ksoftconv
        fi
    else
        return kerror
    end switch

    opc
end

proc applyconversion(unit p, int s,t, opc)=
!deal with conversion op applied to p:
! do nothing
! report error
! insert special node
! attempt compile-time conversion
! insert convert node
! set p's mode etc
    unit q

    case opc
    when kzero then                 !none needed
        return
    when kerror then
        txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

    when kharderror then
        txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

    when ksoftconv then
        if not ctarget then 
            p.mode:=t
            return
        fi
    when ksofttruncshort then
        if tevalconvert(p,s,t,opc) then
            return
        fi
        insertunit(p,jshorten)
        p.mode:=t           !don't use the short target mode
        return

    when karraytoslice then
        insertunit(p,jslice)
        p.mode:=t
        return
    when kichartoslice then
        tstringslice(p,t)
        return

    when kcharaxtoichar then
        insertunit(p,jaddroffirst)
        p.mode:=trefchar
        return
    esac

    if tevalconvert(p,s,t,opc) then     !try and apply it directly
        return
    fi

!have to add an explict conversion node
    if ctarget and p.tag=jblock then
        p.mode:=t
        q:=p.a
        while q.nextunit do     !find last
            q:=q.nextunit
        od
        p:=q
        dorest


    else
dorest::
        insertunit(p, jconvert)
        p.pclop:=opc

        p.convmode:=s
        p.resultflag:=1

        if ttisshort[t] then
            p.convmode:=t
            t:=gettypebase(t)
        fi

        p.mode:=t
    fi
end


proc checkmodes(int s,t)=
    if not comparemodes(s,t) then
        txerror_ss("Type-compare error: # <-> #",strmode(s), strmode2(t))
    fi
end

function comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ref s/ref t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
    int sbase, tbase, starg, ttarg
    symbol d,e

    if s=t then return 1 fi

    sbase:=ttbasetype[s]
    tbase:=ttbasetype[t]
    starg:=tttarget[s]
    ttarg:=tttarget[t]


    if sbase=tbase then
        case sbase
        when tref then
            if starg=tvoid or ttarg=tvoid then
                return 1
            fi
            return comparemodes(starg,ttarg)

        when tarray then
            if not comparemodes(starg, ttarg) then return 0 fi
            if ttlength[s]=ttlength[t] or ttlength[s]=0 or ttlength[t]=0 then
                return 1
            fi
        when tslice then
            return comparemodes(starg, ttarg)

        when tproc then
            d:=ttnamedef[s]
            e:=ttnamedef[t]
            if d and e then
                if not comparemodes(d.mode,e.mode) then return 0 fi
                if d.paramlist=nil and e.paramlist=nil then return 1 fi
            fi
        esac

    elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
        return 1
    else
!else needs complex param/result-matching
!...
    fi
    return 0
end

function tevalconvert(unit p,int s,t,opc)int=
!conversion op opc to convert from s to t is about to be applied to be
!try and do that at compile time to avoid adding a runtime conversion
!return 1 if it could apply it, 0 if it couldn't
!caller should have already evaluated p to reduce constants etc
    real x,z
    int a,c,sbase,tbase

    if p.tag<>jconst then
        setsimple(p)
        return 0
    fi
    a:=p.value
    x:=p.xvalue

    case pr(s,    t)
    when pr(ti64, tr64), pr(ti64, tr32) then
        z:=a

    when pr(tr64, ti64) then
        c:=x

    when pr(tr64, tr32) then
        z:=real32(x)

!   when pr(ti64, tu64), pr(tu64, ti64), pr(tc64, ti64) then
!       c:=p.value
    when pr(ti64, tu8) then
        c:=byte(a)
    when pr(ti64, ti16) then
        c:=i16(a)

    else
        if ttisinteger[s] and ttisinteger[t] and ttsize[s]=ttsize[t] then
            c:=a
        else
            sbase:=ttbasetype[s]
            tbase:=ttbasetype[t]
!           IF CTARGET THEN RETURN 0 FI
            if sbase=tbase then return 1 fi
            return 0
        fi
    esac

    if ttisreal[t] then
        makenewconst(p,int64@(z),t)

    else
        makenewconst(p,c,t)
    fi

    return 1
end

proc tx_assign(unit p,a,b,int t)=
    int m,mm,needres:=t<>tvoid
    symbol d

    case a.tag
    when jmakelist then
        if b.tag=jmakelist then
            if needres then txerror("Mult assign has no result") fi
            tx_assignmultmult(p,a,b)
        else
            tx_assignmultscalar(p,a,b,t)
        fi
        return
    when jdotindex, jdotslice then
        tx_dotindex(a,a.a,a.b,needlv)
        tpass(b,a.mode)
        p.mode:=ti64
        return
    esac

    if a.tag=jname and a.def.islet and p.initlet then
        tpass(a)
    else
        tpasslv(a)
    fi
    m:=a.mode

    a.resultflag:=needres

    if ttbasetype[m]=tslice and b.tag in [jmakeset,jmakelist] then

        tx_makeslice(b,b.a,m)
        p.mode:=m

    elsif ttisshort[m] and needres then
        p.memmode:=m
        p.mode:=gettypebase(m)
        tpass(b,p.mode)

    else
        if b.pclop in [kidiv, kirem] then       !CAN'T JUST OVERRIDE MODE
            tpass(b)
        elsif b.tag=jread then
            tpass(b,m)
        else
            mm:=m
            if ttisshort[m] then
                mm:=gettypebase(m)
            fi
            case b.tag
            when jautocast then
                tpass(b,mm)
            when jmakelist then
                tpass(b,m)
            else
                tpass(b)
            esac


            if ttbasetype[b.mode]=ttuple then
                d:=getprocretmodes(b)
                coerceunit(a,ttmult[d.mode,1])
                p.mode:=a.mode
            else
                coerceunit(b,mm)
                p.mode:=mm
            fi
        fi
    fi

    setsimple(p)

end

proc tx_assignmultmult(unit pp,a,b)=
!mult:=mult
    unit p,q,lhs,rhs

    pp.tag:=jassignmm

    if a.length<>b.length then
        txerror("Mult assign: count mismatch")
    fi
    if a.length=0 then
        txerror("Invalid assignment")
    fi
    rhs:=b.a
    lhs:=a.a

    p:=lhs
    while p, p:=p.nextunit do
        tpasslv(p)
    od

    p:=lhs

    q:=rhs
    while q, (p:=p.nextunit; q:=q.nextunit) do
        tpass(q,p.mode)
    od
end

proc tx_assignmultscalar(unit pp,a,b,int t)=
!assign 'scalar' to mult LHS, but it might be a tuple type or be an expandable one
    unit p,q, alist:=a.a
    int nretmodes,i, alength:=a.length
    ref[]int32 pmult
    symbol d                !point to def containing return mode info

    nretmodes:=0
    pp.tag:=jassignms

    tpass(b,tany)

    case ttbasetype[b.mode]
    when ttuple then
        d:=getprocretmodes(b)
        nretmodes:=d.nretvalues

        if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") fi

        if alength>nretmodes then
            txerror("mult ass/mult returns don't agree in number")
        fi
        if nretmodes<=1 then
            txerror("mult ass rhs needs fn yielding 2+ values")
        fi

        p:=alist
        pmult:=ttmult[d.mode]
        i:=1

        while p, p:=p.nextunit do
            tpasslv(p,pmult[i++])
        od
    when tslice then
        if alength<>2 then txerror("(a,b):=slice") fi
        tpasslv(alist,createrefmode(nil, tttarget[b.mode]))
        tpasslv(alist.nextunit,ti64)

    when trange then
PRINTLN "MULT:=RANGE"
    when trecord then
PRINTLN "MULT:=RECORD"

    elsif b.tag=jbin and b.pclop=kidivrem then
PRINTLN "MULT:=DIVREM"
        if alength<>2 then txerror("(a,b):=divrem") fi
        tpasslv(alist,ti64)
        tpasslv(alist.nextunit,ti64)
        pp.tag:=jassignmdrem

    else
        txerror_s("Can't expand to mult values:",strmode(b.mode))
    esac

    pp.mode:=t
end

proc tpasslv(unit p, int t=tany)=
!process p as lvalue, but require it to be of type t
!however no conversion is done (not allowed); only a compare is done
    tpass(p,,needlv)
    if t not in [tany, tvoid] then
        if not comparemodes(p.mode, t) then
            txerror_ss("PassLV type mismatch: #:=#",strmode(p.mode), strmode2(t))
        fi
    fi
end

function dobinnumx(unit p,a,b)int=
!Try and apply this to binary operands:
!   NUMX    NUMX    DOM
!a and b have already been processed, but not coerced to any type yet

    int amode:=a.mode, bmode:=b.mode, cmode

    if isnum(amode) and isnum(bmode) then
        p.mode:=cmode:=max(amode, bmode)
        coerceunit(a,cmode)
        coerceunit(b,cmode)
        return 1
    fi
    return 0
end

function dobinnumf(unit p,a,b)int=
!Try and apply this to binary operands:
!   NUMF    NUMF    DOM
    int amode:=a.mode, bmode:=b.mode, cmode

    if isnumf(amode) and isnumf(bmode) then
        p.mode:=cmode:=max(amode, bmode)
        coerceunit(a,cmode)
        coerceunit(b,cmode)
        return 1
    fi
    return 0
end

function dobinnumi(unit p,a,b)int=
!Try and apply this to binary operands:
!   NUMI    NUMI    DOM
    int amode:=a.mode, bmode:=b.mode, cmode

    if isnumi(amode) and isnumi(bmode) then
        p.mode:=cmode:=max(amode, bmode)
        coerceunit(a,cmode)
        coerceunit(b,cmode)
        return 1
    fi
    return 0
end

function doin(unit p,a,b)int=
    int simpleset
    unit q

    simpleset:=1
    if b.tag=jmakeset then
        q:=b.a
        while q, q:=q.nextunit do
            if not ttisinteger[q.mode] then
                simpleset:=0
                exit
            fi
        od
    fi

    if isnum(a.mode) and b.tag in [jmakerange, jmakeset] and simpleset then
        p.tag:=(b.tag=jmakerange|jinrange|jinset)
    else
        txerror("doin")
    fi
    p.mode:=tbool
    if p.pclop=knotin then
        addnotl(p)
    fi
    return 1
end

proc setsimple(unit p)=
    unit a
    case p.tag
!   when jcmp, jcallfn, jcallproc then
    when jcallfn, jcallproc then
        return
    when jbin, junary then
        if complexopset[p.pclop] then
            return
        fi
    esac

    for i to jsubs[p.tag] do
        a:=p.abc[i]
        if a and not a.simple then return fi
    od
    p.simple:=1
end

proc do_printlist(unit b)=
    unit c
    while b do
        if b.tag=jfmtitem then
            tpass(c:=b.a)
            tpass(b.b,trefchar)
        else
            tpass(c:=b)
        fi
        fixchararray(c)
        b:=b.nextunit
    od
end
=== mm_winc.m 0 0 19/33 ===
global int cmdskip

global const ctarget=1
global const exetarget=0

global byte fshortnames

!global proc backend=
!end

global proc codegen=
PRINTLN "CODEGEN/CLANG"

LOADERROR("CODEGEN_CLANG NOT READY")
end

global function runlibfile(ichar filename)int=
LOADERROR("RUNLIBFILE(C)")
    return 1
end

global function writeexefile(ichar exename, int gendll=0)int=
    writeasmfile(asmfilename)

    if os_iswindows() then
        do_link_win(asmfilename,exename,"exe",ccompiler, foptim)
    else
        do_link_lin(asmfilename,exename,"exe",ccompiler, foptim)
    fi

    return 1
end

global function writelibfile(ichar filename)int=
LOADERROR("WRITELIBFILE(C)")
    return 1
end

global function writeasmfile(ichar filename)int=
    codegen_clang()

    writegsfile(filename,dest)
    gs_free(dest)

    return 1
end

global proc do_link_win(ichar cfile,exefile,linkoption,int ccompiler,optimise)=
    [256]char str,str2
    int status
    int doobj

    doobj:=eqstring(linkoption,"obj")

    case ccompiler
    when gcc_cc then
        fprint @str,"gcc -m64 # # -o# # -s ",
            (doobj|"-c"|""),(optimise|"-O3"|""),exefile, cfile
    when tcc_cc then
        fprint @str,f"tcc # -o# # # -luser32 c:\windows\system32\kernel32.dll -fdollars-in-identifiers",
            (doobj|"-c"|""),exefile, cfile,
            (doobj|""|"c:\\windows\\system32\\user32.dll")
    when tc_cc then
        strcpy(str2,cfile)
        str2[strlen(cfile)-1]:=0
        fprint @str,f"call tc #",&.str2
    when bcc_cc then
        fprint @str,"bcc # -out:# #",
            (doobj|"-c"|""),exefile, cfile
    esac

    println "W:Invoking C compiler:",&.str

    status:=os_execwait(&.str)

    if status=0 then return fi
    println "M compiler: couldn't compile intermediate C"
    stop 0
end

global proc do_link_lin(ichar cfile,exefile,linkoption,int ccompiler,optimise)=
    [256]char str,str2
    [300]char newexefile
    int status
    int doobj

    doobj:=eqstring(linkoption,"obj")

    strcpy(newexefile,changeext(exefile,""))

    case ccompiler
    when gcc_cc then
        fprint @str,"gcc # # -o# # -lm -ldl -s -fno-builtin",
            (doobj|"-c"|""),(optimise|"-O3"|""),newexefile, cfile
    when tcc_cc then
        fprint @str,"tcc # -o# # -lm -ldl -fdollars-in-identifiers",
            (doobj|"-c"|""),newexefile, cfile
    else
        abortprogram("Not available on Linux")
    esac

    println "L:Invoking C compiler:",&.str

    status:=os_execwait(&.str)

    if status=0 then return fi
    println "M compiler: couldn't compile intermediate C"
    stop 0
end

=== mc_decls.m 0 0 20/33 ===
export type operand = ref opndrec

export record opndrec =     !up to 32 bytes
    union
        symbol def      ! named symbol/label
        int64 value     ! immediate value
        real64 xvalue   ! immediate real value, mainly for dq
        ichar svalue    ! immediate string
        int labelno     ! internal label
        int sysfn       ! runtime function code
    end

    u16 misc: (         ! bitfields
        size:4,         ! one of 1 2 4 8
        scale:4,        ! one of 1 2 4 8
        mode:4,         ! R, X, imm, [mem]
        valtype:4)
    byte reg            !0, or main register
    byte regix          !0, or index register
    i32 offset          !additional offset to memory operands
end

export record mclrec =      !32 bytes
    ref mclrec nextmcl
    operand a,b
    byte opcode
    byte cond
    byte c                  !third operand, as u8 immediate for some ops
    byte spare
    u32 seqno

!   [4]byte args @ b        !for callfn

!   int seqno
end

global enumdata [0:]ichar valtypenames =
    (no_val=0,      $),     !no operand
    (intimm_val,    $),     !immediate int
    (realimm_val,   $),     !immediate real (mainly for dq etc)
    (realmem_val,   $),     !indirect real (for movq etc)
    (stringimm_val, $),     !immediate string, for address of string etc
    (comment_val,   $),     !comment string (or for general pseudo op)
    (def_val,       $),     !var/proc name
    (label_val,     $),     !label index
    (name_val,      $),     !immediate string must be output as ah unquoted name
end

export enumdata [0:]ichar opndnames =
    (a_none=0,  $),
    (a_reg,     $),     ! Ri
    (a_xreg,    $),     ! xmm register
    (a_imm,     $),     ! d including def name, label etc
    (a_mem,     $),     ! any memory modes: [d], [R], [R*4+R2+d+imm] etc
!   (a_cond,    $),     ! a condition code for jcc/setcc
!   (a_args,    $),     ! vreg arglist (for callfn pseudo op)
!   (a_regvar,  $),     ! Reg holding a regvar
!   (a_xregvar, $),     ! Reg holding a regvar
end

export enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

    (m_procstart,       $,      0,      0),     !
    (m_procend,         $,      0,      0),     !
    (m_programend,      $,      0,      0),     !
    (m_comment,         $,      0,      0),     !
    (m_blank,           $,      0,      0),     !
    (m_deleted,         $,      0,      0),     !
    (m_labelname,       $,      0,      0),     !
    (m_define,          $,      0,      0),     !
    (m_definereg,       $,      0,      0),     !
    (m_evalx,           $,      0,      0),     !

    (m_labelx,          $,      1,      0),     !
    (m_nop,             $,      0,      0x90),      !

    (m_mov,             $,      2,      0),     !
    (m_push,            $,      1,      0),     !
    (m_pop,             $,      1,      0),     !
    (m_lea,             $,      2,      0),     !
    (m_cmovcc,          $,      2,      0),     !

    (m_movd,            $,      2,      0),     !
    (m_movq,            $,      2,      0),     !

    (m_movsx,           $,      2,      0),     !
    (m_movzx,           $,      2,      0),     !
    (m_movsxd,          $,      2,      0),     !

    (m_call,            $,      1,      0xE8),      !
    (m_ret,             $,      0,      0xC3),  !
    (m_leave,           $,      0,      0xC9),  !
    (m_retn,            $,      1,      0),     !

    (m_jmp,             $,      1,      0xE9),  !
    (m_jmpcc,           $,      2,      0),     !
    (m_xchg,            $,      2,      0),     !

    (m_add,             $,      2,      0),     !
    (m_sub,             $,      2,      5),     !
    (m_adc,             $,      2,      2),     !
    (m_sbb,             $,      2,      3),     !
    (m_imul,            $,      1,      5),     !
    (m_mul,             $,      1,      4),     !
    (m_imul2,           $,      2,      0),     !
    (m_imul3,           $,      3,      0),     !

    (m_idiv,            $,      1,      7),     !
    (m_div,             $,      1,      6),     !

    (m_andx,            $,      2,      0x04),  !
    (m_orx,             $,      2,      0x01),  !
    (m_xorx,            $,      2,      0x06),  !
    (m_test,            $,      2,      0),     !

    (m_cmp,             $,      2,      0x07),  !

    (m_shl,             $,      2,      0x04),  !
    (m_sar,             $,      2,      0x07),  !
    (m_shr,             $,      2,      0x05),  !
    (m_rol,             $,      2,      0x00),  !
    (m_ror,             $,      2,      0x01),  !
    (m_rcl,             $,      2,      0x02),  !
    (m_rcr,             $,      2,      0x03),  !

    (m_neg,             $,      1,      3),     !
    (m_notx,            $,      1,      2),     !

    (m_inc,             $,      1,      0),     !
    (m_dec,             $,      1,      1),     !

    (m_cbw,             $,      0,      0), !
    (m_cwd,             $,      0,      0), !
    (m_cdq,             $,      0,      0),     !
    (m_cqo,             $,      0,      0),     !
    (m_setcc,           $,      2,      0),     !

    (m_bsf,             $,      2,      0xBC),  !
    (m_bsr,             $,      2,      0xBD),  !

    (m_sqrtsd,          $,      2,      0x51),  !
    (m_sqrtss,          $,      2,      0x51),  !
    (m_addss,           $,      2,      0x58),  !
    (m_subss,           $,      2,      0x5C),  !
    (m_mulss,           $,      2,      0x59),  !
    (m_divss,           $,      2,      0x5E),  !

    (m_addsd,           $,      2,      0x58),  !
    (m_subsd,           $,      2,      0x5C),  !
    (m_mulsd,           $,      2,      0x59),  !
    (m_divsd,           $,      2,      0x5E),  !

    (m_comiss,          $,      2,      0),     !
    (m_comisd,          $,      2,      0),     !
    (m_xorpd,           $,      2,      0x57),  !
    (m_xorps,           $,      2,      0x57),  !
    (m_andpd,           $,      2,      0x54),  !
    (m_andps,           $,      2,      0x54),  !
    (m_pxor,            $,      2,      0xEF),  !
    (m_pand,            $,      2,      0xDB),  !
    (m_cvtss2si,        $,      2,      0),     !
    (m_cvtsd2si,        $,      2,      0),     !
    (m_cvttss2si,       $,      2,      0),     !
    (m_cvttsd2si,       $,      2,      0),     !

    (m_cvtsi2ss,        $,      2,      0),     !
    (m_cvtsi2sd,        $,      2,      0),     !

    (m_cvtsd2ss,        $,      2,      0),     !
    (m_cvtss2sd,        $,      2,      0),     !

    (m_movdqa,          $,      2,      0x66),  !
    (m_movdqu,          $,      2,      0xF3),  !

    (m_pcmpistri,       $,      3,      0x63),  !
    (m_pcmpistrm,       $,      3,      0x62),  !

    (m_fld,             $,      1,      0),     !
    (m_fst,             $,      1,      2),     !
    (m_fstp,            $,      1,      3),     !

    (m_fild,            $,      1,      0),     !
    (m_fist,            $,      1,      2),     !
    (m_fistp,           $,      1,      3),     !

    (m_fadd,            $,      0,      0xC1),  !
    (m_fsub,            $,      0,      0xE9),  !
    (m_fmul,            $,      0,      0xC9),  !
    (m_fdiv,            $,      0,      0xF9),  !
    (m_fsqrt,           $,      0,      0xFA),  !
    (m_fsin,            $,      0,      0xFE),  !
    (m_fcos,            $,      0,      0xFF),  !
    (m_fsincos,         $,      0,      0xFB),  !
    (m_fptan,           $,      0,      0xF2),  !
    (m_fpatan,          $,      0,      0xF3),  !
    (m_fabs,            $,      0,      0xE1),  !
    (m_fchs,            $,      0,      0xE0),  !

    (m_minss,           $,      2,      0x5D),  !
    (m_maxss,           $,      2,      0x5F),  !
    (m_minsd,           $,      2,      0x5D),  !
    (m_maxsd,           $,      2,      0x5F),  !

    (m_db,              $,      1,      0),     !
    (m_dw,              $,      1,      0),     !
    (m_dd,              $,      1,      0),     !
    (m_dq,              $,      1,      0),     !
    (m_ddoffset,        $,      1,      0),     !

    (m_segment,         $,      1,      0),     !
    (m_isegment,        $,      0,      0),     !
    (m_zsegment,        $,      0,      0),     !
    (m_csegment,        $,      0,      0),     !

    (m_align,           $,      1,      0),     !
    (m_resb,            $,      1,      1),     !
    (m_resw,            $,      1,      2),     !
    (m_resd,            $,      1,      4),     !
    (m_resq,            $,      1,      8),     !

    (m_xlat,            $,      0,      0xD7),  !
    (m_loopnz,          $,      1,      0xE0),  !
    (m_loopz,           $,      1,      0xE1),  !
    (m_loopcx,          $,      1,      0xE2),  !
    (m_jecxz,           $,      1,      0xE3),  !
    (m_jrcxz,           $,      1,      0xE3),  !

    (m_cmpsb,           $,      0,      0),     !
    (m_cmpsw,           $,      0,      0),     !
    (m_cmpsd,           $,      0,      0),     !
    (m_cmpsq,           $,      0,      0),     !

    (m_rdtsc,           $,      0,      0x31),  !
    (m_popcnt,          $,      2,      0),     !

    (m_finit,           $,      0,      0),     !

    (m_fldz,            $,      0,      0xEE),  !
    (m_fld1,            $,      0,      0xE8),  !
    (m_fldpi,           $,      0,      0xEB),  !
    (m_fld2t,           $,      0,      0xE9),  !
    (m_fld2e,           $,      0,      0xEA),  !
    (m_fldlg2,          $,      0,      0xEC),  !
    (m_fldln2,          $,      0,      0xED),  !

    (m_cpuid,           $,      0,      0),     !

    (m_halt,            $,      0,      0xF4),  !
end

export enumdata [0:]ichar regnames, [0:]byte regcodes =
    (rnone=0,   $,  0),         !
    (r0,        $,  0),         !d0 rax
    (r1,        $,  10),        !d1 r10
    (r2,        $,  11),        !d2 r11
    (r3,        $,  7),         !d3 rdi
    (r4,        $,  3),         !d4 rbx
    (r5,        $,  6),         !d5 rsi
    (r6,        $,  12),        !d6 r12
    (r7,        $,  13),        !d7 r13
    (r8,        $,  14),        !d8 r14
    (r9,        $,  15),        !d9 r15
    (r10,       $,  1),         !d10 rcx
    (r11,       $,  2),         !d11 rdx
    (r12,       $,  8),         !d12 r8
    (r13,       $,  9),         !d13 r9
    (r14,       $,  5),         !d14 rbp
    (r15,       $,  4),         !d15 rsp

    (r16,       $,  4),         !b0h ah
    (r17,       $,  7),         !b1h bh
    (r18,       $,  5),         !b10h ch
    (r19,       $,  6),         !b11h dh

    (xr0,       $,  0),
    (xr1,       $,  1),
    (xr2,       $,  2),
    (xr3,       $,  3),
    (xr4,       $,  4),
    (xr5,       $,  5),
    (xr6,       $,  6),
    (xr7,       $,  7),
    (xr8,       $,  8),
    (xr9,       $,  9),
    (xr10,      $,  10),
    (xr11,      $,  11),
    (xr12,      $,  12),
    (xr13,      $,  13),
    (xr14,      $,  14),
    (xr15,      $,  15)
end

export const rframe = r14
export const rstack = r15

global enumdata [0:]ichar condnames, [0:]ichar asmcondnames,
        [0:]int asmrevcond =

    (ov_cond=0, "ov",   "o",        nov_cond),
    (nov_cond,  "nov",  "no",       ov_cond),

    (ltu_cond,  "ltu",  "b",        geu_cond),
    (geu_cond,  "geu",  "ae",       ltu_cond),

    (eq_cond,   "eq",   "z",        ne_cond),
    (ne_cond,   "ne",   "nz",       eq_cond),

    (leu_cond,  "leu",  "be",       gtu_cond),
    (gtu_cond,  "gtu",  "a",        leu_cond),

    (s_cond,    "s",    "s",        ns_cond),
    (ns_cond,   "ns",   "ns",       s_cond),

    (p_cond,    "p",    "p",        np_cond),
    (np_cond,   "np",   "np",       p_cond),

    (lt_cond,   "lt",   "l",        ge_cond),
    (ge_cond,   "ge",   "ge",       lt_cond),

    (le_cond,   "le",   "le",       gt_cond),
    (gt_cond,   "gt",   "g",        le_cond),

    (flt_cond,  "flt",  "b",        fge_cond),      !special floating point codes
    (fge_cond,  "fge",  "ae",       flt_cond),
    (fle_cond,  "fle",  "be",       fgt_cond),
    (fgt_cond,  "fgt",  "a",        fle_cond)
end

global const z_cond = eq_cond
global const nz_cond = ne_cond

!I use my own register designations Dn, An, Wn, Bn (8,4,2,1 bytes),
!which have a more sensible order than the official ones.
!The mapping is shown against Dn. Some (not all) of the official register
!names are used too

!Regindex is the ordinal value used to represent the register: 1..16
!This table is intended for initialising the global symbol table

export tabledata []ichar dregnames, []byte regsizes, []byte regindices =
    ("d0",      8,  r0),        !rax    d0..d9 are for general use
    ("d1",      8,  r1),        !r10    d0..d2 are volatile in ABI
    ("d2",      8,  r2),        !r11

    ("d3",      8,  r3),        !rdi    d3..d9 are preserved across funcs in ABI
    ("d4",      8,  r4),        !rbx
    ("d5",      8,  r5),        !rsi
    ("d6",      8,  r6),        !r12
    ("d7",      8,  r7),        !r13
    ("d8",      8,  r8),        !r14
    ("d9",      8,  r9),        !r15

    ("d10",     8,  r10),       !rcx    d10..d13 are win64 ABI register passing regs
    ("d11",     8,  r11),       !rdx    ..
    ("d12",     8,  r12),       !r8     ..
    ("d13",     8,  r13),       !r9     ..

    ("d14",     8,  r14),       !rbp    frame pointer
    ("d15",     8,  r15),       !rsp    stack pointer

    ("a0",      4,  r0),
    ("a1",      4,  r1),
    ("a2",      4,  r2),
    ("a3",      4,  r3),
    ("a4",      4,  r4),
    ("a5",      4,  r5),
    ("a6",      4,  r6),
    ("a7",      4,  r7),
    ("a8",      4,  r8),
    ("a9",      4,  r9),
    ("a10",     4,  r10),
    ("a11",     4,  r11),
    ("a12",     4,  r12),
    ("a13",     4,  r13),
    ("a14",     4,  r14),
    ("a15",     4,  r15),

    ("w0",      2,  r0),
    ("w1",      2,  r1),
    ("w2",      2,  r2),
    ("w3",      2,  r3),
    ("w4",      2,  r4),
    ("w5",      2,  r5),
    ("w6",      2,  r6),
    ("w7",      2,  r7),
    ("w8",      2,  r8),
    ("w9",      2,  r9),
    ("w10",     2,  r10),
    ("w11",     2,  r11),
    ("w12",     2,  r12),
    ("w13",     2,  r13),
    ("w14",     2,  r14),
    ("w15",     2,  r15),


    ("b0",      1,  r0),
    ("b1",      1,  r1),
    ("b2",      1,  r2),
    ("b3",      1,  r3),
    ("b4",      1,  r4),
    ("b5",      1,  r5),
    ("b6",      1,  r6),
    ("b7",      1,  r7),
    ("b8",      1,  r8),
    ("b9",      1,  r9),
    ("b10",     1,  r10),
    ("b11",     1,  r11),
    ("b12",     1,  r12),
    ("b13",     1,  r13),
    ("b14",     1,  r14),
    ("b15",     1,  r15),
    ("b16",     1,  r16),
    ("b17",     1,  r17),
    ("b18",     1,  r18),
    ("b19",     1,  r19),

    ("rax",     8,  r0),
    ("rbx",     8,  r4),
    ("rcx",     8,  r10),
    ("rdx",     8,  r11),
    ("rsi",     8,  r5),
    ("rdi",     8,  r3),
    ("rbp",     8,  r14),
    ("rsp",     8,  r15),
    ("r8",      8,  r12),
    ("r9",      8,  r13),
    ("r10",     8,  r1),
    ("r11",     8,  r2),
    ("r12",     8,  r6),
    ("r13",     8,  r7),
    ("r14",     8,  r8),
    ("r15",     8,  r9),

    ("eax",     4,  r0),
    ("ebx",     4,  r4),
    ("ecx",     4,  r10),
    ("edx",     4,  r11),
    ("esi",     4,  r5),
    ("edi",     4,  r3),
    ("ebp",     4,  r14),
    ("esp",     4,  r15),
    ("r8d",     4,  r12),
    ("r9d",     4,  r13),
    ("r10d",    4,  r1),
    ("r11d",    4,  r2),
    ("r12d",    4,  r6),
    ("r13d",    4,  r7),
    ("r14d",    4,  r8),
    ("r15d",    4,  r9),

    ("ax",      2,  r0),
    ("bx",      2,  r4),
    ("cx",      2,  r10),
    ("dx",      2,  r11),
    ("si",      2,  r5),
    ("di",      2,  r3),
    ("bp",      2,  r14),
    ("sp",      2,  r15),
    ("r8w",     2,  r12),
    ("r9w",     2,  r13),
    ("r10w",    2,  r1),
    ("r11w",    2,  r2),
    ("r12w",    2,  r6),
    ("r13w",    2,  r7),
    ("r14w",    2,  r8),
    ("r15w",    2,  r9),


    ("al",      1,  r0),
    ("bl",      1,  r4),
    ("cl",      1,  r10),
    ("dl",      1,  r11),

    ("ah",      1,  r16),
    ("bh",      1,  r17),
    ("ch",      1,  r18),
    ("dh",      1,  r19),

    ("sil",     1,  r5),
    ("dil",     1,  r3),
    ("bpl",     1,  r14),
    ("spl",     1,  r15),

    ("r8b",     1,  r12),
    ("r9b",     1,  r13),
    ("r10b",    1,  r1),
    ("r11b",    1,  r2),
    ("r12b",    1,  r6),
    ("r13b",    1,  r7),
    ("r14b",    1,  r8),
    ("r15b",    1,  r9),

end

export []ichar xmmregnames = (
    "xmm0",
    "xmm1",
    "xmm2",
    "xmm3",
    "xmm4",
    "xmm5",
    "xmm6",
    "xmm7",
    "xmm8",
    "xmm9",
    "xmm10",
    "xmm11",
    "xmm12",
    "xmm13",
    "xmm14",
    "xmm15")

export []ichar fregnames = (
    "st0",
    "st1",
    "st2",
    "st3",
    "st4",
    "st5",
    "st6",
    "st7")

export []ichar mregnames = (
    "mmx0",
    "mmx1",
    "mmx2",
    "mmx3",
    "mmx4",
    "mmx5",
    "mmx6",
    "mmx7")

!global enumdata [0:]ichar condnames =
!
!   (ov_cond    = 0,    "o"),
!   (nov_cond   = 1,    "no"),
!
!   (ltu_cond   = 2,    "b"),
!   (geu_cond   = 3,    "ae"),
!
!   (eq_cond    = 4,    "z"),
!   (ne_cond    = 5,    "nz"),
!
!   (leu_cond   = 6,    "be"),
!   (gtu_cond   = 7,    "a"),
!
!   (s_cond     = 8,    "s"),
!   (ns_cond    = 9,    "ns"),
!
!   (p_cond     = 10,   "p"),
!   (np_cond    = 11,   "np"),
!
!   (lt_cond    = 12,   "l"),
!   (ge_cond    = 13,   "ge"),
!
!   (le_cond    = 14,   "le"),
!   (gt_cond    = 15,   "g"),
!end

export tabledata []ichar jmpccnames, []byte jmpcccodes =
    ("jo",      ov_cond),
    ("jno",     nov_cond),
    ("jb",      ltu_cond),
    ("jae",     geu_cond),
    ("jz",      eq_cond),
    ("jnz",     ne_cond),
    ("jbe",     leu_cond),
    ("ja",      gtu_cond),
    ("js",      s_cond),
    ("jns",     ns_cond),
    ("jp",      p_cond),
    ("jnp",     np_cond),
    ("jl",      lt_cond),
    ("jge",     ge_cond),
    ("jle",     le_cond),
    ("jg",      gt_cond),
    ("jc",      ltu_cond),
    ("jnc",     geu_cond),
end


export tabledata []ichar setccnames, []byte setcccodes =
    ("seto",    ov_cond),
    ("setno",   nov_cond),
    ("setb",    ltu_cond),
    ("setae",   geu_cond),
    ("setz",    eq_cond),
    ("setnz",   ne_cond),
    ("setbe",   leu_cond),
    ("seta",    gtu_cond),
    ("sets",    s_cond),
    ("setns",   ns_cond),
    ("setp",    p_cond),
    ("setnp",   np_cond),
    ("setl",    lt_cond),
    ("setge",   ge_cond),
    ("setle",   le_cond),
    ("setg",    gt_cond),
end

export tabledata []ichar cmovccnames, []byte cmovcccodes =
    ("cmovo",   ov_cond),
    ("cmovno",  nov_cond),
    ("cmovb",   ltu_cond),
    ("cmovae",  geu_cond),
    ("cmovz",   eq_cond),
    ("cmovnz",  ne_cond),
    ("cmovbe",  leu_cond),
    ("cmova",   gtu_cond),
    ("cmovs",   s_cond),
    ("cmovns",  ns_cond),
    ("cmovp",   p_cond),
    ("cmovnp",  np_cond),
    ("cmovl",   lt_cond),
    ("cmovge",  ge_cond),
    ("cmovle",  le_cond),
    ("cmovg",   gt_cond),
end

!export enumdata [0:]ichar segmentnames =
!   (no_seg=0,      $),
!   (code_seg,      $),
!   (idata_seg,     $),
!   (zdata_seg,     $),
!   (rodata_seg,    $),
!   (impdata_seg,   $),
!end

export enumdata [0:]ichar reftypenames =    !use during pass2
    (extern_ref=0,      $),     !is external
    (fwd_ref,           $),     !not yet reached
    (back_ref,          $),     !has been reached
end

global int mlabelno
!global byte foptimise

!global const maxoperands=200
!
!global [maxoperands+10]pclstackrec pclopndstack
!global int noperands               !number of pcl operands, including wide
global int mstackdepth              !hw stack size (pcl operands, + extra for wide, + padding)

global int retindex
!
!global record pclstackrec =
!   byte loc            !loc code (stack, reg, xreg usually)
!   byte reg            !reg or xreg when in register
!   byte cat            !voidcat, or x32/x64/wide/block/var cat
!!  byte float          !1 when a float operand: means xreg not reg
!!  byte wide           !1 when wide: uses 2 stack slots, or (reg, reg+1)
!!  byte isvar
!!  byte reg2           !high reg of wide
!   byte ilabel         !1 to load contents of label type, not its address
!   [4]byte spare
!!  int32 offset
!   union
!       u64 value       !immediate value
!       r64 xvalue      !
!       r32 xvalue32    !
!       ichar svalue
!       symbol def      !for memaddr
!       struct
!           int32 labno     !for labels
!           int32 offset
!       end
!   end
!end
!
!!note: the pclstack grows downwards, so that pclstack[1] varies: gets nearer
!!to the start as the stack grows. pclstack[2] etc is above that, a moving window
!!of operands near the top of the stack
!global ref[]pclstackrec pclstack
!global pclstackrec pclstackzero
!
!!Where any active operand is located:
!
!global enumdata [0:]ichar locnames =
!   (no_loc=0,      $),         ! not set
!   (reg_loc,       $),         ! in a d64 register
!   (xreg_loc,      $),         ! in an x64
!!  (stack_loc,     $),         ! on the hardware stack (must be ordered properly)
!   (immd64_loc,    $),         ! d64 immediate value
!!  (immvar_loc,    $),         ! d64 immediate value as var const
!!  (immx64var_loc, $),         ! x64 immediate value as var const
!   (immx64_loc,    $),         ! x64 immediate value
!   (immx32_loc,    $),         ! x32 immediate value
!   (memaddr_loc,   $),         ! immediate addr
!   (label_loc, $),             ! immediate label
!   (string_loc,    $),         ! immediate string
!   (regvar_loc,    $),         ! 
!   (xregvar_loc,   $),         ! 
!   (mem_loc,       $),         ! memory location
!end

global const regmax=r9              !can use r0 to regmax inclusive; only those regs
global const xregmax=xr15

!global int regtop                  !current highest reg used; 0 means no reg used
!global int xregtop
!
!global int stackworkregs           !no. of regs used as work ones
!global int nworkregs               !no. of param regs used as work ones


global u64 regset                   !in use flags for dregs/xregs
global int nregs, nxregs

global u64 isregvar

!These vars give info on the resources used by a proc

!global [r0..r15]byte allregmap     !all regs used
!global [r0..r15]byte allxregmap        !all xregs used

global int inf_proccalls
global int inf_proclocals
global int inf_procxlocals

global int inf_leafproc
global int inf_highreg
global int inf_highxreg
global int inf_maxargs
export int inf_assem

global int inf_r10used      !these may be set in pass2 when occupied by params
global int inf_r11used
global int inf_r13used

global [16]int dsaveregs
global [16]int xsaveregs
global int ndsaveregs   !set in procentry; at one or both will be zero
!global int ndsavepush
global int nxsaveregs
global int dsaveoffset
global int xsaveoffset
global int needstackframe
global int framebytes
global int parambytes
global int needshadow32     !has value 0, 32 or 40, the actual spaced needed

global int dspillbytes, xspillbytes, alignbytes, localbytes, shadowbytes

global byte noxorclear      !1 to suppress xor optimisation

global const wd = 4
global const xc = 3
global const yb = 2
global const za = 1

global const xb = 2
global const ya = 1

global const xa = 1

!global enumdata [0:]ichar xregnames =
!   (xnone=0,   $),
!   (xr0,       $),
!   (xr1,       $),
!   (xr2,       $),
!   (xr3,       $),
!   (xr4,       $),
!   (xr5,       $),
!   (xr6,       $),
!   (xr7,       $),
!   (xr8,       $),
!   (xr9,       $),
!   (xr10,      $),
!   (xr11,      $),
!   (xr12,      $),
!   (xr13,      $),
!   (xr14,      $),
!   (xr15,      $)
!end
!
!global pcl procdefpcl
global symbol procdef

!global const maxcalldepth=16
!global [maxcalldepth]int callshadow            !1 is fresh 32-byte shadow space needed
!global [maxcalldepth]int callslots         !N, 8-byte stack slots to be popped after call
!global [maxcalldepth]byte callpopargs      !How many pcl operands to be popped after call
!global [maxcalldepth]byte callhighparams   !1 if this or outer call used 5+ params
!global [maxcalldepth]byte callnvars            !param no. from which variadics start
global int ncalldepth

global const maxparams=32
global const maxlocals=256

!these are reset at each procdef
global [maxparams]symbol paramdefs
global [maxlocals]symbol localdefs
global int nparams, nlocals
global int retmode
!global ref strec procdef
global int passno
global int sa_nargs

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(xr0,xr1,xr2,xr3,xr4,xr5)

!global int frameoffset,paramoffset
global int paramoffset

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

!global int retindex
global int stackaligned
global const initial_stackalignment = 1

global const rtos=rnone         !means stack operand

!global const rcx=r10
!global const rdx=r11
!global const r14=rframe
!global const r15=rstack

export ref mclrec mccode, mccodex       !genmc adds to this linked list

global int currsegment=0        !

global int currzdataalign=0
global int curridataalign=0

!global int framebytes          !local stackframe size
!global int parambytes
global int frameoffset
global int isthreadedproc
global int iscallbackproc

global int structretoffset          !0, or offset of R9 copy within struct
global ref mclrec stacksetinstr     !caller of any fn: instr that sets sp
global int currblocksize            !0, or set to largest block ret value
!global ref mclrec allmclcode
global ichar allasmstr
global int allasmstrlen

global operand dstackopnd
global operand distackopnd
global operand dframeopnd

global operand zero_opnd=nil
!global unit zero_unit

global [r0..r15,1..8]operand regtable

!global const maxsmallint=32
!global [0..maxsmallint]operand smallinttable

global [-128..64]operand frameregtable

global record constrec =
    union
        int value
        real xvalue
        ichar svalue
    end
    ref constrec nextconst
    int labelno
end

global ref constrec cstringlist
global ref constrec vstringlist
global ref constrec creallist
global ref constrec creal32list

!global const initstringsize    = 1024
!global const initrealsize      = 16
!global const initvintsize      = 16
!
!global ref []ichar stringtable
!!global ref []int32    stringlentable
!global ref []int32   stringlabtable
!global ref []real  realtable
!global ref []int32 reallabtable
!global ref []int   vinttable
!global ref []int32 vintlabtable
!
!global int stringtablesize
!global int realtablesize
!global int vinttablesize
!
!global int nstrings=0
!global int nreals=0
!global int nvints=0

!global strbuffer sbuffer
!global ref strbuffer dest=&sbuffer
global int destlinestart
global symbol currasmproc
global int noregvar             !1 to inhibit strreg showing regvar names

!global int mseqno

!global [rtsnames.len]int rtsproclabels     !non-zero means rtsfn has been used

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

global record relocrec =            !informal version
    ref relocrec nextreloc
    int reloctype
    int offset
    int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
global record dbuffer =
    ref byte pstart
    union
        ref byte pcurr
        ref word16 pcurr16
        ref word32 pcurr32
        ref word64 pcurr64
    end
    ref byte pend
    int alloc
end

global int ss_zdatalen
global ref dbuffer ss_zdata         !used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

!const max_ss_symbols=32768             !exported to coff
global const init_ss_symbols=32768              !exported to coff
!global const init_ss_symbols=16384
global ref []symbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

!global ref stlipstrec globalimportlist     !all global vars and imports across all moduls

global ref[]symbol labeldeftable

global int alineno

export enumdata []ichar segmentnames =
    (code_seg,      "code"),
    (idata_seg,     "idata"),
    (zdata_seg,     "zdata"),
    (rodata_seg,    "rodata"),
    (impdata_seg,   $),
end

global enumdata [0:]ichar loadnames =
    (load_op = 0,   $),
    (get_op,        $),
    (loadref_op,    $),
    (getref_op,     $),
end 

GLOBAL INT NALLMCL

global const maxblocktemps=50
global [maxblocktemps]symbol blockdefs
global int nblocktemps
global symbol blockretname
=== msysc.m 1 0 21/33 ===
!MSYS version for C target

[]ref void _fnaddresses
[]ichar _fnnames
int _fnnprocs
int _fnnexports

global record procinforec=
    word16      fnindex
    byte        rettype
    byte        nparams
    [12]byte    paramlist
end

!for print/read routines
!------------------------------------------
record fmtrec=  ! (default)
    byte    minwidth    ! n (0)   min field width (0 if not used or don't care)
    i8      precision   ! .n (0)   number of decimals/significant figures/max width
    byte    base        ! B,H or Xn (10)  2 to 16

    char    quotechar   ! Qc (0)   0 or '"' or c
    char    padchar     ! Pc, Z (' ')
    char    realfmt     ! E,F,G ('f') 'e' or 'f' or 'g'

    char    plus        ! (0)   0 or '+'
    char    sepchar     ! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
    char    lettercase  ! A,a ('A') 'A' or 'a'
    char    justify     ! JL, JR, JC ('R') 'L' or 'R' or 'C'?
    char    suffix      ! Tc (0)   0 or 'B' or 'H' or c
    char    usigned     ! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
    char    charmode    ! C,D (0)  0 or 'C' or 'D'  o/p int as int or single char or double/multi-char
    char    heapmode    ! M (0)  'M' for str-functions, return ptr tp heap string
    char    param       ! Use int value for <fmtparam>
    byte    spare
end

int fmtparam            !as set with :'V'

enumdata =
    std_io,
    file_io,
    str_io
end

const comma = ','

global int needgap          = 0
int outdev          = std_io
filehandle outchan  = nil
ref char fmtstr     = nil

const maxiostack=10
[maxiostack]filehandle  outchan_stack
[maxiostack]int         outdev_stack
[maxiostack]ref char    fmtstr_stack
[maxiostack]byte        needgap_stack

[maxiostack]ref char    ptr_stack       !this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

const smallstrlen=256

!Read buffer vars
const rd_buffersize = 16384 !total capacity of line buffer

export ref char rd_buffer       ! point to start of read buffer
int rd_length           ! length of this line (as read by readln)
ref char rd_pos         ! current position it's up to (next read starts here)
ref char rd_lastpos     ! set by sread() just before reading used for reread()
int termchar            ! terminator char set by readxxx()
int itemerror           !   set by some read functions, eg for reals

export int $cmdskip         !0 unless set by READMCX/etc

const maxparam=128
export int nsysparams
export int ncmdparams
export int nenvstrings
export [maxparam]ichar sysparams
export ref[0:]ichar cmdparams
export ref[]ichar envstrings


!------------------------------------------

word64 mask63   = 0x7FFF'FFFF'FFFF'FFFF
real offset64   = 9223372036854775808.0     ! 2**63 as r64
real offset32   = 9223372036854775808.0     ! 2**63 as r32

export proc m_init(int nargs, ref[]ichar args, envstrings)=
    nsysparams:=nargs

    if nsysparams>maxparam then
        printf("Too many params\n")
        stop 1
    fi

    for i:=1 to nargs do
        sysparams[i]:=args[i]
    od

!assume nsysparams is >=1, since first is always the program name
    ncmdparams:=nsysparams-($cmdskip+1)
    cmdparams:=cast(&sysparams[$cmdskip+1])

    int j:=1
    nenvstrings:=0
    while envstrings[j] do
        ++nenvstrings
        ++j
    od

end


global function m_getdotindex(word64 a,int i)int=
    return (a iand (1<<i))>>i
end

global func m_setdotindex(word64 a, int i,x)word64=
    ref word32 a32

!see comments on setdotslice
    (a iand inot (1<<i)) ior (word64(x)<<i)
end

global function m_getdotslice(word64 a,int i,j)int=
    if i>=j then
        return (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))
    else
        return (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
    fi
end

global func m_setdotslice(word64 a, int i,j,word64 x)word64=
!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
    int w
    word64 mask64
    word mask
    ref word32 a32

    if i>j then println "SETDOTSLICE?"; stop 52 fi

!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing
!to bytes beyond the 32-bit value
!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too

    mask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i           !shifted field of w 1s
    (a iand inot mask64) ior x<<i
end

global function m_get_nprocs:int=
    return _fnnprocs
end

global function m_get_nexports:int=
    return _fnnexports
end

global function m_get_procname(int n)ref char=
    return _fnnames[n]
end

global function m_get_procaddr(int n)ref void=
    return _fnaddresses[n]
end

global function m_get_procexport(int n)ref void=
    nil
!   return &_fnexports[n]
end

proc pushio=
    if niostack>=maxiostack then
        printf("Too many io levels\n")
        stop 53
    fi
    ++niostack
    outchan_stack[niostack] := outchan
    outdev_stack[niostack]  := outdev
    fmtstr_stack[niostack]  := fmtstr
    needgap_stack[niostack] := needgap
    needgap:=0
    fmtstr:=nil
    outchan:=nil
end

global proc m_print_startfile(ref void dev)=
    pushio()
    outchan:=cast(dev)
    if dev then
        outdev:=file_io
    else
        outdev:=std_io
    fi
end

global proc m_print_startstr(ref char s)=
    ref ref char p
    pushio()

    ptr_stack[niostack]:=s
    p:=&ptr_stack[niostack]

    outchan:=cast(p)
    outdev:=str_io
end

global proc m_print_startptr(ref ref char p)=
    pushio()

    outchan:=cast(p)
    outdev:=str_io
end

global proc m_print_startcon=
    pushio()
    outdev:=std_io
end

global proc m_print_setfmt(ref char format)=
    fmtstr:=format
end

global proc m_print_end=
    needgap:=0
    nextfmtchars(1)
    if niostack=0 then return fi
    outchan := outchan_stack[niostack]
    outdev  := outdev_stack[niostack]
    fmtstr  := fmtstr_stack[niostack]
    needgap := needgap_stack[niostack]
    --niostack
end

global proc m_print_ptr(ref void a,ichar fmtstyle=nil)=
    nextfmtchars()

    printstr(strword(u64(a),"z8h"))
    needgap:=1
end

global proc m_print_i64(int64 a,ichar fmtstyle=nil)=
    [40]char s
    fmtrec fmt
    int n

    nextfmtchars()

    if fmtstyle=nil then
        if a>=0 then
            n:=u64tostr(a,&.s,10,0)
        else
            s[1]:='-'
            n:=u64tostr(-a,&s[2],10,0)+1
        fi
        printstr_n(&.s,n)

    else
        strtofmt(fmtstyle,-1,&fmt)
        if fmt.param='V' then
            fmtparam:=a
            needgap:=0
        else
            tostr_i64(a,&fmt)
        fi
    fi
    needgap:=1
end

global proc m_print_u64(word64 a,ichar fmtstyle=nil)=
    fmtrec fmt

    nextfmtchars()
    if fmtstyle=nil then
        printstr(strword(a))
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_u64(a,&fmt)
    fi
    needgap:=1
end

global proc m_print_r64(real x,ichar fmtstyle=nil)=
    [360]char s
    fmtrec fmt

    nextfmtchars()
    if fmtstyle=nil then
        sprintf(&.s,"%f",x)
        printstr(&.s)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_r64(x,&fmt)
    fi

    needgap:=1
end

global proc m_print_r32(real32 x,ichar fmtstyle=nil)=
    m_print_r64(x,fmtstyle)
end

global proc m_print_c8(int64 a,ichar fmtstyle=nil)=
    [40]char s
    fmtrec fmt
    int n

    nextfmtchars()

    s[1]:=a
    s[2]:=0
    printstr(&.s)
    needgap:=1
end

global proc m_print_str(ichar s, fmtstyle=nil)=
    nextfmtchars()
    fmtrec fmt
    if fmtstyle=nil then
        printstr(s)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_str(s,&fmt)
    fi
    needgap:=1
end

global proc m_print_newline=
    needgap:=0
    nextfmtchars(1)
    printstr("\w")
end

global proc m_print_nogap=
    needgap:=0
end

global proc m_print_space=
    needgap:=0
    printstr(" ")
end

export proc printstr(ichar s)=
    int n
    ref ref char p
    
    case outdev
    when std_io then
        printf("%s",s)
    when file_io then
        fprintf(outchan,"%s",s)
    when str_io then
        p:=cast(outchan)
        strcpy(p^,s)
        p^+:=strlen(s)
    esac
end

global proc printstr_n(ichar s,int n=-1)=
    [smallstrlen]char str
    ref ref char p

    case n
    when -1 then n:=strlen(s)       !assume zero-terminated
    when 0 then return
    esac

    case outdev
    when str_io then
        p:=cast(outchan)
        memcpy(p^,s,n)
        p^+:=n
        p^^:=0
    when file_io then
        s:=makezstring(s,n,&.str)
        fprintf(outchan,"%s",s)
        freezstring(s,n)

    when std_io then
        s:=makezstring(s,n,&.str)
        printf("%s",s)
        freezstring(s,n)
!       printf("%.*s",int32(n),s)
    esac
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
    if length then
        if f=nil then
            emitc "printf((u8*)""%.*s"",(i32)length,s);"
        else
            emitc "fprintf(f,(u8*)""%.*s"",(i32)length,s);"
        fi
    fi
end

function makezstring(ichar s,int n,ichar local)ichar=
    ichar t
    if n<smallstrlen then
        memcpy(local,s,n)
        (local+n)^:=0
        return local
    else
        t:=pcm_alloc(n+1)
        memcpy(t,s,n)
        (t+n)^:=0
        return t
    fi
end

proc freezstring(ichar t,int n)=
    if n>=smallstrlen then
        pcm_free(t,n+1)
    fi
end

proc printchar(int ch)=
    ref ref char p
    case outdev
    when std_io then
        emitc "printf((u8*)""%c"",(int)ch)"
    when file_io then
        emitc "fprintf(msysc$outchan,(u8*)""%c"",(int)ch)"
    when str_io then
        p:=cast(outchan)
        p^^:=ch
        p^+:=1
        p^^:=0
    esac
end

global proc nextfmtchars(int lastx=0)=
    char c
    ref char pstart
    int n

    if not fmtstr then          !format not in use
        if needgap then
            printchar(' ')
!       printstr_n(" ",1)
        fi
        needgap:=0
        return
    fi

    pstart:=fmtstr
    n:=0

    while (1) do
        c:=fmtstr^
        switch c
        when '#' then
            if lastx then
                goto skip
            fi
            ++fmtstr
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
            ++fmtstr
            c:=fmtstr^
            if c then
                ++fmtstr
                printchar(c)
            fi
            pstart:=fmtstr
        else
    skip::
            ++n
            ++fmtstr
        endswitch
    od
end

global proc strtofmt(ref char s,int slen,ref fmtrec fmt) =      !PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n  Width
!.n Max width/precision
!A  Convert to upper when
!a  Convert to lower when
!B  Binary
!C  Show int as single n-bit (unicode) character
!D  Show int as multi-bit (unicode) character
!E,F,G  Specify format for double (corresponds to C format codes)
!F
!G
!H  Hex
!JC Justify centre
!JL Justify left
!JR Justify right
!M  HEAPMODE???
!O  Octal
!Pc Use padding char c
!Q  Add double quotes around string (and deal with embedded quotes)
!'  Add single quotes around string (and deal with embedded quotes)
!Sc Use separator char c between every 3 or 4 digits
!Tc Use terminator char c (typically B or H)
!U  Show ints as unsigned
!V  For ints, don't display: store value as parameter for subsequent '*'
!W  Unsigned
!Xn Use base n (n is hex 0 to F)
!Z  Use "0" padding
!+  Always have + or - in front of integers
!~  Quote char is ~
!*  Same as n but uses parameter set with :'V' on previous int

    char c
    byte wset
    int n
    [0:100]char str

    fmt^:=defaultfmt

    if s=nil then return fi

    if slen=-1 then slen:=strlen(s) fi

    memcpy(&.str,s,slen)        !convert s/slen to zero-terminated string
    str[slen]:=0
    s:=&.str

    wset:=0
    while s^ do
        c:=s^
        ++s
        switch c
        when 'B', 'b' then fmt^.base:=2
        when 'H', 'h' then fmt^.base:=16
        when 'O', 'o' then fmt^.base:=8
        when 'X', 'x' then
            c:=s^
            if c then
                switch c
                when '0'..'9' then c:=c-'0'
                when 'A'..'F' then c:=c-'A'+10
                when 'a'..'f' then c:=c-'a'+10
                else
                    c:=10
                end
                fmt^.base:=c
                ++s
            fi
        when 'Q', 'q' then fmt^.quotechar:='"'
        when '~' then fmt^.quotechar:='~'
        when 'J', 'j' then
            fmt^.justify:=toupper(s^)
            if s^ then
                ++s
            fi
        when 'A' then fmt^.lettercase:='A'
        when 'a' then fmt^.lettercase:='a'
        when 'Z', 'z' then fmt^.padchar:='0'
        when 'S', 's' then
            fmt^.sepchar:=s^
            if s^ then
                ++s
            fi
        when 'P', 'p' then
            fmt^.padchar:=s^
            if s^ then
                ++s
            fi
        when 'T', 't' then
            fmt^.suffix:=s^
            if s^ then
                ++s
            fi
        when 'W', 'w' then fmt^.usigned:='W'
        when 'E', 'e' then fmt^.realfmt:='e'
        when 'F', 'f' then fmt^.realfmt:='f'
        when 'G', 'g' then fmt^.realfmt:='g'
        when '.' then
            wset:=1
        when comma,'_' then fmt^.sepchar:=c
        when '+' then fmt^.plus:='+'
        when 'D', 'd' then fmt^.charmode:='D'
        when 'C', 'c' then fmt^.charmode:='C'
        when 'M', 'm' then fmt^.heapmode:='M'
        when 'V','v' then fmt.param:='V'
        when '*' then
            n:=fmtparam
            goto gotwidth
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
gotwidth::
                if not wset then
                    fmt^.minwidth:=n
                    wset:=1
                else
                    fmt^.precision:=n
                fi
            fi
        endswitch
    od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
    [0:20]char str
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

    return expandstr(&.str,dest,strlen(&.str),fmt)
end

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =     !EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
!result will be zero-terminated, for use in this module

    int i,w,m

!check to see if result is acceptable as it is
    w:=fmt^.minwidth
    if w=0 or w<=n then     ! allow str to be longer than minwidth
        strncpy(t,s,n)
        (t+n)^:=0
        return n
    fi

    if fmt^.justify='L' then    ! left-justify
        strncpy(t,s,n)
        t+:=n
        for i:=1 to w-n do
            t^:=fmt^.padchar
            ++t
        od
        t^:=0
    elsif fmt^.justify='R' then
        if fmt^.padchar='0' and fmt^.base and (s^='-' or s^='+') then ! need to move sign outside 
            t^:=s^
            ++t
            to w-n do
                t^:=fmt^.padchar
                ++t
            od
            strncpy(t,s+1,n-1)
            (t+n-1)^:=0
        else
            to w-n do
                t^:=fmt^.padchar
                ++t
            od
            strncpy(t,s,n)
            (t+n)^:=0
        fi

    else                ! centre-justify?

        m:=(w-n+1)/2
        to m do
            t^:=fmt^.padchar
            ++t
        od
        strncpy(t,s,n)
        t+:=n
        to w-n-m do
            t^:=fmt^.padchar
            ++t
        od
        t^:=0

    fi
    return w
end

function u64tostr(u64 aa,ref char s,word base,int sep)int =     !U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
    [0:onesixty]char t
    u64 dd
    int i,j,k,g
    int dummy
    ref char s0

    i:=0
    k:=0
    g:=(base=10|3|4)

    repeat
        t[++i]:=digits[aa rem base]
        aa:=aa/base

!BUG in separator logic, doesn't work when leading zeros used, eg. printing
!out a full length binary
!so perhaps move this out to expandstr
        ++k
        if sep and aa<>0 and k=g then
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

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)
!returns length of s
    [0:onesixty]char str                ! allow for binary with separators!
    int i,j,k,n,w,usigned
    static u64 mindint=0x8000'0000'0000'0000

    usigned:=0
    if fmt^.usigned then
        usigned:=1
    fi

    if aa=mindint and not usigned then      ! minint

        str[0]:='-'
        n:=i64mintostr(&str[1],fmt^.base,fmt^.sepchar)+1
    else
        if (not usigned and aa<-0) or fmt^.plus then
            if aa<0 then
                aa:=-aa
                str[0]:='-'
            else
                str[0]:='+'
            fi
            n:=u64tostr(aa,&str[1],fmt^.base,fmt^.sepchar)+1
        else
            n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)
        fi
    fi

    if fmt^.suffix then
        str[n]:=fmt^.suffix
        str[++n]:=0
    fi

!str uses upper cases for hex/etc see if lc needed
    if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'    then    ! need lower when
        convlcstring(&.str)
    fi

!at this point, n is the str length including signs and suffix
    return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =     !U64TOSTRFMT
!see i64tostrfmt
    [0:onesixty]char str                ! allow for binary with separators!
    int i,j,k,n,w

    n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

    if fmt^.suffix then
        str[n]:=fmt^.suffix
        str[++n]:=0
    fi

!str uses upper cases for hex/etc see if lc needed
    if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'  then    ! need lower when
        convlcstring(&.str)
    fi

!at this point, n is the str length including signs and suffix
    return expandstr(&.str,s,n,fmt)
end

function i64mintostr(ref char s,int base,int sep)int =      !I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
    [0:onesixty]char t
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

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
!s is a string process according to fmtrec fmt^, and return result in t
!caller should check whether any changes are required to s (now it can just use s), but this
!check is done here anyway (with a simple copy to t)
!n is current length of s
!return length of t
!Three processing stages:
!1 Basic input string s
!2 Additions or mods: quotes, suffix, when conversion
!3 Width adjustment
!1 is detected here, 2 is done here, 3 is done by expandstr
    ref char u,v
    [256]char str
    int w,nheap     ! whether any heap storage is used # bytes allocated

    nheap:=0

    if fmt^.quotechar or fmt^.lettercase then       ! need local copy
        if n<256 then
            u:=&.str
        else
            nheap:=n+3                  ! allow for quotes+terminator
            u:=pcm_alloc(nheap)
        fi
        if fmt^.quotechar then
            v:=u
            v^:=fmt^.quotechar
            ++v
            if n then
                strcpy(v,s)
                v+:=n
            fi
            v^:=fmt^.quotechar
            ++v
            v^:=0
            n+:=2
        else
            memcpy(u,s,n)
        fi
        switch fmt^.lettercase
        when 'a' then   ! need lower when
            convlcstring(u)
        when 'A' then
            convucstring(u)
        endswitch
        s:=u
    fi

    w:=fmt^.minwidth
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

proc tostr_i64(int64 a, ref fmtrec fmt)=
    [360]char str
    int n

    case fmt^.charmode
    when 0 then
        n:=i64tostrfmt(a,&.str,fmt)
    when 'D','d' then
        n:=domultichar(ref char(&a),8,&.str,fmt)

    else                        !assume 'C'
        printchar(a)            !no other formatting allowed
        return
    esac

    printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
    [360]char str
    int n

    case fmt^.charmode
    when 'D','d' then
        n:=domultichar(ref char(&a),8,&.str,fmt)

    when 'C','c' then
        printchar(a)            !no other formatting allowed
        return

    else
        n:=u64tostrfmt(a,&.str,fmt)
    esac

    printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
    [360]char str,str2
    [0:10]char cfmt
    int n

    cfmt[0]:='%'

    if fmt^.precision then
        cfmt[1]:='.'
        cfmt[2]:='*'
        cfmt[3]:=fmt^.realfmt
        cfmt[4]:=0
        sprintf(&.str,&.cfmt,fmt^.precision,x)
    else
        cfmt[1]:=fmt^.realfmt
        cfmt[2]:=0
        sprintf(&.str,&.cfmt,x)
    fi

!at this point, n is the str length including signs and suffix

    n:=strlen(&.str)        ! current length

    if n<fmt^.minwidth then
        n:=expandstr(&.str,&.str2,n,fmt)
        strcpy(&.str,&.str2)
    fi

    printstr_n(&.str,n)
end

proc tostr_str(ref char s, ref fmtrec fmt) =
    int oldlen,newlen,n
    ref char t

!try and work out size of formatted string
    oldlen:=strlen(s)
    newlen:=oldlen

    if fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase then
        if fmt^.quotechar then
            newlen+:=2
        fi
        if fmt^.minwidth>newlen then
            newlen:=fmt^.minwidth
        fi
        t:=pcm_alloc(newlen+1)
        n:=strtostrfmt(s,t,oldlen,fmt)
        printstr_n(t,n)
        pcm_free(t,newlen+1)
    else
        printstr_n(s,oldlen)
    fi
end

function getfmt(ichar fmtstyle)ref fmtrec=
    static fmtrec fmt
    if fmtstyle then
        strtofmt(fmtstyle,-1,&fmt)
        return &fmt
    else
        return &defaultfmt
    fi
end

export function strint(int64 a, ichar fmtstyle=nil)ichar=
    static [100]char str
    ref fmtrec fmt

    m_print_startstr(&.str)
    tostr_i64(a,fmt:=getfmt(fmtstyle))
    m_print_end()
    return getstr(&.str,fmt)
end

export proc getstrint(int64 a, ichar dest)=
    m_print_startstr(dest)
    tostr_i64(a,getfmt(nil))
    m_print_end()
end

export function strword(word64 a, ichar fmtstyle=nil)ichar=
    static [100]char str
    ref fmtrec fmt

    m_print_startstr(&.str)
    tostr_u64(a,fmt:=getfmt(fmtstyle))
    m_print_end()
    return getstr(&.str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
    static [320]char str
    ref fmtrec fmt

    m_print_startstr(&.str)
    tostr_r64(a,fmt:=getfmt(fmtstyle))
    m_print_end()
    return getstr(&.str,fmt)
end

function getstr(ichar s, ref fmtrec fmt)ichar=
    if fmt^.heapmode then
        return pcm_copyheapstring(s)
    else
        return s
    fi
end

proc initreadbuffer=
    if rd_buffer then return fi
    rd_buffer:=pcm_alloc(rd_buffersize)
    rd_buffer^:=0
    rd_pos:=rd_lastpos:=rd_buffer
end

global proc m_read_conline=
    initreadbuffer()
    readlinen(nil,rd_buffer,rd_buffersize)

    rd_length:=strlen(rd_buffer)
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

global proc m_read_fileline(filehandle f)=
    initreadbuffer()
    readlinen(f,rd_buffer,rd_buffersize)

    rd_length:=strlen(rd_buffer)
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

export proc m_read_strline(ichar s)=
    int n

    initreadbuffer()
    n:=strlen(s)

    if n<rd_buffersize then
        strcpy(rd_buffer,s)
    else
        memcpy(rd_buffer,s,rd_buffersize-1)
        (rd_buffer+rd_buffersize-1)^:=0
    fi
    rd_length:=n
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

function readitem(int &itemlength)ref char =
!read next item from rd_buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!NOTE THAT THIS IS DESTRUCTIVE. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
    ref char p,s,itemstr
    char quotechar, c

    unless rd_buffer then 
        initreadbuffer()
    end unless

    s:=rd_pos

!scan string, eliminating leading white space
    while s^=' ' or s^=9 do
        ++s
    od

    itemstr:=s              !assume starts here
    rd_lastpos:=rd_pos:=s

    if s^=0 then            ! No more chars left to read return null string
        termchar:=0
        itemlength:=0
        return s
    fi

    quotechar:=0            ! Allow possible enclosing single or double quotes
    if s^='"' then
        quotechar:='"'
        ++s
    elsif s^='\'' then
        quotechar:='\''
        ++s
    fi

!loop reading characters until separator or end reached
    p:=itemstr:=s

    while s^ do
        c:=s++^
        switch c
        when ' ', 9, comma, '=' then        ! separator
            if quotechar or p=s then            !can be considered part of name if inside quotes, or is only char
                goto normalchar
            fi
            termchar:=c
            exit
        else
    normalchar::
            if c=quotechar then
                if s^=quotechar then    ! embedded quote
                    p^:=c
                    ++s
                    ++p
                else                    ! end of name
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

    if s^=0 then
        termchar:=0
    fi
    itemlength:=p-itemstr               ! actual length of token
    rd_pos:=s

    return itemstr
end

export function strtoint(ichar s,int length=-1, base=10)int64=
!return point to next char after terminator (which can be just off length of string)
    byte signd
    word64 aa
    char c,d

    itemerror:=0

    if length=-1 then
        length:=strlen(s)
    fi
!check for sign
    signd:=0
    if length and s^='-' then
        signd:=1; ++s; --length
    elsif length and s^='+' then
        ++s; --length
    fi

    aa:=0
    while length do
        c:=s++^
        --length
        switch c
        when 'A'..'F' then d:=c-'A'+10
        when 'a'..'f' then d:=c-'a'+10
        when '0'..'9' then d:=c-'0'
        when '_', '\'' then
            next
        else
            itemerror:=1
            exit
        endswitch

        if d>=base then
            itemerror:=1
            exit
        fi
        aa:=aa*base+d
    od

    if signd then
        return -aa
    else
        return aa
    fi
end

global function m_read_i64(int fmt=0)int64=
    ref char s
    int length,c
    int64 aa

    case fmt
    when 'C','c' then
        rd_lastpos:=rd_pos
        if rd_pos^ then
            return rd_pos++^
        else
            return 0
        fi
    when 'T','t' then
        return termchar
    when 'E','e' then
        return itemerror
    esac

    s:=readitem(length)


    case fmt
    when 0,'I','i' then
        return strtoint(s,length)
    when 'B','b' then
        return strtoint(s,length,2)
    when 'H','h' then
        return strtoint(s,length,16)
    esac
    return 0
end

global function m_read_r64(int fmt=0)real=
    [512]char str
    ref char s
    int length
    int32 numlength
    real x

    s:=readitem(length)

    if length=0 or length>=str.len then     !assume not a real
        return 0.0
    fi
    memcpy(&.str,s,length)
    str[length+1]:=0

    itemerror:=0

    if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
        x:=0.0
        itemerror:=1
    fi

    return x
end

export proc m_read_str(ref char dest, int destlen=0,fmt=0)=
    ref char s
    int length,numlength
    real x

    itemerror:=0
    if fmt='L' or fmt='l' then
        s:=rd_pos
        length:=rd_buffer+rd_length-rd_pos

    else
        s:=readitem(length)

        if fmt='N' or fmt='n' then
            iconvlcn(s,length)
        fi
    fi

    if destlen>0 then
        if length>=destlen then
            length:=destlen-1
            itemerror:=1
        fi
    fi
    memcpy(dest,s,length)
    (dest+length)^:=0
end

export proc readstr(ref char dest, int fmt=0,destlen=0)=
    m_read_str(dest,destlen,fmt)
end

export proc rereadln=
    rd_pos:=rd_buffer
    rd_lastpos:=rd_pos
end

export proc reread=
    rd_pos:=rd_lastpos
end

global function valint(ichar s, int fmt=0)int64=
    ref char old_pos, old_lastpos
    int64 aa

    initreadbuffer()
    old_pos:=rd_pos
    old_lastpos:=rd_lastpos

    rd_pos:=s
    aa:=m_read_i64(fmt)
    rd_pos:=old_pos
    rd_lastpos:=old_lastpos
    return aa
end

global function valreal(ichar s)real=
    ref char old_pos, old_lastpos
    real x

    initreadbuffer()
    old_pos:=rd_pos
    old_lastpos:=rd_lastpos

    rd_pos:=s
    x:=m_read_r64()
    rd_pos:=old_pos
    rd_lastpos:=old_lastpos
    return x
end

proc iconvlcn(ref char s,int n) =
    to n do
        s^:=tolower(s^)
        ++s
    od
end

proc iconvucn(ref char s,int n) =
    to n do
        s^:=toupper(s^)
        ++s
    od
end

proc convlcstring(ref char s)=
    while (s^) do
        s^:=tolower(s^)
        ++s
    od
end

proc convucstring(ref char s)=
    while (s^) do
        s^:=toupper(s^)
        ++s
    od
end

global function m_power_i64(int64 n,a)int64=
    if n<0 then
        return 0
    elsif n=0 then
        return 1
    elsif n=1 then
        return a
    elsif (n iand 1)=0 then
    !   return ipower(a*a,n/2)
        return m_power_i64(n/2,sqr a)
    else            !assume odd
        return m_power_i64((n-1)/2,sqr a)*a
    fi
end

global proc m_intoverflow=
    abortprogram("Integer overflow detected")
end

global proc m_dotindex(word i,a)=
!return a.[i] in d0
    ABORTPROGRAM("DOT INDEX")
!   assem
!       mov d0,[a]
!       mov cl,[i]
!       shr d0,cl
!       and d0,1
!   end 
end

global proc m_dotslice(word j,i,a)=
!return a.[i..j] in d0; assumes j>=i
    ABORTPROGRAM("DOT SLICE")
!   assem
!       mov d0,[a]
!       mov rcx,[i]
!       shr d0,cl
!       sub rcx,[j]
!       neg rcx             !j-1
!       mov d2,0xFFFF'FFFF'FFFF'FFFE
!       shl d2,cl
!       not d2
!       and d0,d2
!   end 
end

global proc m_popdotindex(word i,ref word p,word x)=
!p^.[i]:=x
    ABORTPROGRAM("POP DOT INDEX")
!   assem
!       mov d3,[p]
!       mov cl,[i]
!       mov d0,[d3]
!       mov d1,1
!       shl d1,cl           !000001000
!       not d1              !111110111
!       and d0,d1           !clear that bit in dest
!       mov d1,[x]
!       and d1,1
!       shl d1,cl
!       or d0,d1
!       mov [d3],d0
!   end 
end

global proc m_popdotslice(word j,i, ref word p, word x)=
!p^.[i..j]:=x
    ABORTPROGRAM("POP DOT SLICE")
!   assem
!!d3 = p
!!d4 = x, then shifted then masked x
!!d5 = i
!!d6 = clear mask
!
!       mov d3,[p]
!       mov d4,[x]
!       mov d5,[i]
!       mov rcx,d5          !i
!       shl d4,cl           !x<<i
!       mov rcx,[j]
!       sub rcx,d5          !j-i
!       inc rcx             !j-i+1
!       mov d2,0xFFFF'FFFF'FFFF'FFFF
!       shl d2,cl           !...111100000     (assume 5-bit slice)
!       not d2              !...000011111
!       mov rcx,d5          !i
!       shl d2,cl           !...000011111000  (assume i=3)
!       and d4,d2           !mask x (truncate extra bits)
!       mov d0,[d3]
!       not d2              !...111100000111
!       and d0,d2           !clear dest bits
!       or d0,d4            !add in new bits
!       mov [d3],d0
!   end 
end

global function m_imin(int64 a,b)int64=
    return min(a,b)
end

global function m_imax(int64 a,b)int64=
    return max(a,b)
end

global function m_sign(real x)real=
    if x>0.0 then return 1.0
    elsif x<0.0 then return -1.0
    else return 0.0
    fi
end

export function m_tp_i64tor64(i64 a)r64 x=
    memcpy(&x, &a, 8)
    x
end

export function m_tp_r64toi64(r64 x)i64 a=
    memcpy(&a, &x, 8)
    a
end

export function m_tp_reftoi64(ref void p)i64 a=
    memcpy(&a, &p, 8)
    a
end

export function m_tp_i64toref(i64 a)ref void p=
    memcpy(&p, &a, 8)
    p
end
=== mlib.m 1 0 22/33 ===
!const mem_check=1
const mem_check=0

global [0..300]u64 allocupper
global int alloccode                !set by heapalloc
export int allocbytes               !set by heapalloc
export int fdebug=0
export int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
export int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=(mem_check|500000|2)
array [maxmemalloc+1]ref int32 memalloctable
array [maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend          !points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8         !2048
export const int maxblocksize = 2048
export const int $maxblocksizexx = 2048

array [0:maxblocksize+1]byte sizeindextable !convert byte size to block index 1..maxblockindex

const int size16   = 1          !the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

export [0:9]ref word freelist

export record strbuffer =
    ichar strptr
    int32 length
    int32 allocated
end

!export tabledata() [0:]ichar pmnames=
export enumdata [0:]ichar pmnames=
    (pm_end=0,      $),
    (pm_option,     $),
    (pm_sourcefile, $),
    (pm_libfile,    $),
    (pm_colon,      $),
    (pm_extra,      $),
end

[2]word seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)
!array [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

export function pcm_alloc(int n)ref void =
    ref byte p

    if not pcm_setup then
        pcm_init()
    fi

    if n>maxblocksize then          !large block allocation
        alloccode:=pcm_getac(n)
        allocbytes:=allocupper[alloccode]

        p:=allocmem(allocbytes)
        if not p then
            abortprogram("pcm_alloc failure")
        fi

        if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

        return p
    fi

    alloccode:=sizeindextable[n]        !Size code := 0,1,2 etc for 0, 16, 32 etc
    allocbytes:=allocupper[alloccode]
    smallmemtotal+:=allocbytes

    if p:=ref byte(freelist[alloccode]) then        !Items of this block size available
        if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
        freelist[alloccode]:=ref word(int((freelist[alloccode])^))

        return p
    fi

!No items in freelists: allocate new space in this heap block
    p:=pcheapptr                !Create item at start of remaining pool in heap block
    pcheapptr+:=allocbytes          !Shrink remaining pool

    if pcheapptr>=pcheapend then        !Overflows?
        p:=pcm_newblock(allocbytes)     !Create new heap block, and allocate from start of that
        return p
    fi
    if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

    return p
end

export proc pcm_free(ref void p,int n) =
!n can be the actual size requested it does not need to be the allocated size
    int acode

    if n=0 then return fi

    if n>maxblocksize then      !large block
        if mem_check then removefrommemalloc(p,n) fi
        free(p)
        return
    fi

    if p then
        acode:=sizeindextable[n]        !Size code := 0,1,2 etc for 0, 16, 32 etc

        smallmemtotal-:=allocupper[acode]

        if mem_check then removefrommemalloc(p,allocupper[acode]) fi

        cast(p,ref word)^:=word(int(freelist[acode]))
        freelist[acode]:=p
    fi
end

export proc pcm_freeac(ref void p,int alloc) =
    pcm_free(p,allocupper[alloc])
end

!export proc pcm_copymem4(ref void p,q,int n) = !PCM_COPYMEM4
!!copy n bytes of memory from q to p.
!!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!!n can be zero, and need not be a multiple of 4 bytes
!
!   memcpy(p,q,n)
!end

export proc pcm_clearmem(ref void p,int n) =
    memset(p,0,n)
end

export proc pcm_init =
!set up sizeindextable too
    int j,k,k1,k2
    int64 size
    const limit=1<<33

    alloccode:=0
    if pcm_setup then
        return
    fi

    pcm_newblock(0)

    for i to maxblocksize do    !table converts eg. 78 to 4 (4th of 16,32,64,128)
        j:=1
        k:=16
        while i>k do
            k:=k<<1
            ++j
        od
        sizeindextable[i]:=j
    od

    allocupper[1]:=16
    size:=16

    for i:=2 to 27 do
        size*:=2
        allocupper[i]:=size
        if size>=threshold then
                k:=i
            exit
        fi
    od

    for i:=k+1 to allocupper.upb do
        size+:=alloc_step
        if size<limit then
            allocupper[i]:=size
            maxmemory:=size
        else
            maxalloccode:=i-1
            exit
        fi
        
    od
    pcm_setup:=1
end

export function pcm_getac(int size)int =
! convert linear blocksize from 0..approx 2GB to 8-bit allocation code

!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9

    if size<=maxblocksize then
        return sizeindextable[size]     !size 0 to 2KB
    fi

    size:=(size+255)>>8                 !scale by 256

!now same sizetable can be used for 2KB to 512KB (288 to 2KB)

    if size<=maxblocksize then
        return sizeindextable[size]+8
    fi

!sizetable now used for 512KB to 128MB (to 2KB)
    size:=(size+63)>>6                  !scale by 256

    if size<=maxblocksize then
        return sizeindextable[size]+14
    fi

!size>2048, which means it had been over 128MB.
    size:=(size-2048+2047)/2048+22
    return size
end

export function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
    static int totalheapsize
    ref byte p

    totalheapsize+:=pcheapsize
    alloccode:=0
    p:=allocmem(pcheapsize) !can't free this block until appl terminates
    if p=nil then
        abortprogram("Can't alloc pc heap")
    fi

    pcheapptr:=p
    pcheapend:=p+pcheapsize

    if pcheapstart=nil then     !this is first block
        pcheapstart:=p
    fi
    pcheapptr+:=itemsize
    return ref u32(p)
end

export function pcm_round(int n)int =
!for any size n, return actual number of bytes that would be allocated
    static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

    if n>maxblocksize then
        return n
    else
        return allocbytes[sizeindextable[n]]
    fi
end

export function pcm_allocz(int n)ref void =
    ref void p
    p:=pcm_alloc(n)

    memset(p,0,n)
    return p
end

export function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
    ref char q
    int n
    if s=nil then return nil fi

    n:=strlen(s)+1
    q:=pcm_alloc(n)
    memcpy(q,s,n)
    return q
end

export function pcm_copyheapstringn(ref char s,int n)ref char =
    ref char q
    if s=nil then return nil fi

    q:=pcm_alloc(n+1)
    memcpy(q,s,n)
    (q+n)^:=0
    return q
end

export function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
    ref char q
    if length=0 then return nil fi

    q:=pcm_alloc(length)
    memcpy(q,s,length)
    return q
end

proc addtomemalloc(ref int32 ptr,int size)=
!add ptr to allocated table
    int allocated, code

!CPL "***************ADD TO ALLOC:",ptr,size
    for i to maxmemalloc do
        if memalloctable[i]=ptr then
            CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
            stop 2
        fi

        if memalloctable[i]=nil then        !unused entry
            memalloctable[i]:=ptr

            code:=pcm_getac(size)
            allocated:=allocupper[code]

            memallocsize[i]:=allocated
            return
        fi
    od
    CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
    CPL
    stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
!remove ptr to allocated table
    int allocated, code

!CPL "------------------************REMOVE FROM ALLOC:",ptr,size
    code:=pcm_getac(size)
    allocated:=allocupper[code]

    for i to maxmemalloc do
        if memalloctable[i]=ptr then
            if memallocsize[i]<>ALLOCATED then
!               CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
                CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, ROUNDED FREESIZE=",ALLOCATED,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
                abortprogram("MEMSIZE")
            fi
            memalloctable[i]:=nil
            return
        fi
    od
    CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
    CPL 
OS_GETCH()
    abortprogram("MEM")
    stop 4
end

export function allocmem(int n)ref void =
    ref void p

    p:=malloc(n)
    if p then
        return p
    fi
    println n,memtotal
    abortprogram("Alloc mem failure")
    return nil
end

global function reallocmem(ref void p,int n)ref void =
    p:=realloc(p,n)
    return p when p
    println n
    abortprogram("Realloc mem failure")
    return nil
end

export proc abortprogram(ref char s) =
    println s
    print   "ABORTING: Press key..."
!os_getch()
    stop 5
end

export function getfilesize(filehandle handlex)int=
    word32 p,size

    p:=ftell(handlex)       !current position
    fseek(handlex,0,2)      !get to eof
    size:=ftell(handlex)        !size in bytes
    fseek(handlex,p,seek_set)   !restore position
    return size
end

export proc readrandom(filehandle handlex, ref byte mem, int offset, size) =
    int a
    fseek(handlex,offset,seek_set)
    a:=fread(mem,1,size,handlex)            !assign so as to remove gcc warning
end

export function writerandom(filehandle handlex, ref byte mem, int offset,size)int =
    fseek(handlex,offset,seek_set)
    return fwrite(mem,1,size,handlex)
end

export function setfilepos(filehandle file,int offset)int=
    return fseek(file,offset,0)
end

export function getfilepos(filehandle file)int=
    return ftell(file)
end

export function readfile(ref char filename)ref byte =
    filehandle f
    int size
    ref byte m,p

    f:=fopen(filename,"rb")
    if f=nil then
        return nil
    fi
    rfsize:=size:=getfilesize(f)

    m:=pcm_alloc(size+2)        !allow space for etx/zeof etc

    if m=nil then
        return nil
    fi

    readrandom(f,m,0,size)
    p:=m+size           !point to following byte
    (ref u16(p)^:=0)    !add two zero bytes

    fclose(f)
    return m
end

export function writefile(ref char filename,ref byte data,int size)int =
    filehandle f
    int n

    f:=fopen(filename,"wb")
    if f=nil then
        return 0
    fi

    n:=writerandom(f,data,0,size)
    fclose(f)
    return n
end

export function checkfile(ref char file)int=
    filehandle f
    if f:=fopen(file,"rb") then
        fclose(f)
        return 1
    fi
    return 0
end

export proc readlinen(filehandle handlex,ref char buffer,int size) =
!size>2
    int ch
    ref char p
    int n
    array [0:100]char buff
    byte crseen

    if handlex=nil then
        handlex:=filehandle(os_getstdin())
    fi
    if handlex=nil then
        n:=0
        p:=buffer
        do
            ch:=getchar()
            if ch=13 or ch=10 or ch=-1 then
                p^:=0
                return
            fi
            p++^:=ch
            ++n
            if n>=(size-2) then
                p^:=0
                return
            fi
        od
    fi

    buffer^:=0
    if fgets(buffer,size-2,handlex)=nil then
        return
    fi

    n:=strlen(buffer)
    if n=0 then
        return
    fi

    p:=buffer+n-1       !point to last char
    crseen:=0
    while (p>=buffer and (p^=13 or p^=10)) do
        if p^=13 or p^=10 then crseen:=1 fi
        p--^ :=0
    od

!NOTE: this check doesn't work when a line simply doesn't end with cr-lf

    if not crseen and (n+4>size) then
        cpl size,n
        abortprogram("line too long")
    fi
end

export proc iconvlcn(ref char s,int n) =
    to n do
        s^:=tolower(s^)
        ++s
    od
end

export proc iconvucn(ref char s,int n) =
    to n do
        s^:=toupper(s^)
        ++s
    od
end

export function convlcstring(ref char s)ichar s0=
    s0:=s
    while (s^) do
        s^:=tolower(s^)
        ++s
    od
    s0
end

export function convucstring(ref char s)ichar s0=
    s0:=s
    while (s^) do
        s^:=toupper(s^)
        ++s
    od
    s0
end

export function changeext(ref char s,newext)ichar=
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
    static [260]char newfile
    array [32]char newext2
    ref char sext
    int n

    strcpy(&newfile[1],s)

    case newext^
    when 0 then
        newext2[1]:=0
        newext2[2]:=0
    when '.' then
        strcpy(&newext2[1],newext)
    else
        strcpy(&newext2[1],".")
        strcat(&newext2[1],newext)
    esac


    sext:=extractext(s,1)           !include "." when it is only extension

    case sext^
    when 0 then                     !no extension not even "."
        strcat(&newfile[1],&newext2[1])
    when '.' then                       !no extension not even "."
        strcat(&newfile[1],&newext2[2])
    else                            !has extension
        n:=sext-s-2         !n is number of chars before the "."
        strcpy(&newfile[1]+n+1,&newext2[1])
    esac

    return &newfile[1]
end

export function extractext(ref char s,int period=0)ichar=
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
    ref char t,u

    t:=extractfile(s)

    if t^=0 then            !s contains no filename
        return ""
    fi

!t contains filename+ext
    u:=t+strlen(t)-1        !u points to last char of t

    while u>=t do
        if u^='.' then      !start extension found
            if (u+1)^=0 then        !null extension
                return (period|"."|"")
            fi
            return u+1          !return last part of filename as extension exclude the dot
        fi
        --u
    od
    return ""           !no extension seen
end

export function extractpath(ref char s)ichar=
    static [0:260]char str
    ref char t
    int n

    t:=s+strlen(s)-1        !t points to last char

    while (t>=s) do
        switch t^
        when '\\','/',':' then      !path separator or drive letter terminator assume no extension
            n:=t-s+1            !n is number of chars in path, which includes rightmost / or \ or :
            memcpy(&.str,s,n)
            str[n]:=0
            return &.str
        endswitch
        --t
    od
    return ""           !no path found
end

export function extractfile(ref char s)ichar=
    ref char t

    t:=extractpath(s)

    if t^=0 then            !s contains no path
        return s
    fi

    return s+strlen(t)      !point to last part of s that contains the file
    end

export function extractbasefile(ref char s)ichar=
    static [0:100]char str
    ref char f,e
    int n,flen

    f:=extractfile(s)
    flen:=strlen(f)
    if flen=0 then      !s contains no path
        return ""
    fi
    e:=extractext(f,0)

    if e^ then          !not null extension
        n:=flen-strlen(e)-1
        memcpy(&str,f,n)
        str[n]:=0
        return &.str
    fi
    if (f+flen-1)^='.' then
        memcpy(&str,f,flen-1)
        str[flen-1]:=0
        return &.str
    fi
    return f
end

export function addext(ref char s,ref char newext)ichar=
!when filespec has no extension of its own, add newext
    ref char sext

    sext:=extractext(s,1)

    if sext^=0 then                     !no extension not even "."
        return changeext(s,newext)
    fi

    return s                            !has own extension; use that
end

export function pcm_alloc32:ref void =
    ref byte p

    allocbytes:=32
    smallmemtotal+:=32

    if p:=ref byte(freelist[2]) then        !Items of this block size available
        freelist[2]:=ref word(int((freelist[2])^))
        return p
    fi

!No items in freelists: allocate new space in this heap block
    return pcm_alloc(32)
end

export proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

    smallmemtotal-:=32
    if mem_check then removefrommemalloc(p,32) fi

    cast(p,ref word)^:=word(int(freelist[2]))
    freelist[2]:=p
end

export proc pcm_free64(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

    smallmemtotal-:=64
    if mem_check then removefrommemalloc(p,64) fi

    cast(p,ref word)^:=word(int(freelist[3]))
    freelist[3]:=p
end

export function pcm_alloc16:ref void =
    ref byte p
    allocbytes:=16
    smallmemtotal+:=16

    if p:=ref byte(freelist[1]) then        !Items of this block size available
        freelist[1]:=ref word(int((freelist[1])^))
        return p
    fi

!No items in freelists: allocate new space in this heap block
    return pcm_alloc(16)
end

export proc pcm_free16(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

    smallmemtotal-:=16
    if mem_check then removefrommemalloc(p,32) fi

    cast(p,ref word)^:=word(int(freelist[1]))
    freelist[1]:=p
end

export proc outbyte(filehandle f,int x)=
    fwrite(&x,1,1,f)
end

export proc outword16(filehandle f,word x)=
    fwrite(&x,2,1,f)
end

export proc outword32(filehandle f,word x)=
    fwrite(&x,4,1,f)
end

export proc outword64(filehandle f,word64 x)=
    fwrite(&x,8,1,f)
end

export proc outstring(filehandle f, ichar s)=
    fwrite(s,strlen(s)+1,1,f)
end

export proc outblock(filehandle f, ref void p, int n)=
    fwrite(p,n,1,f)
end

export function myeof(filehandle f)int=
    int c

    c:=fgetc(f)
    if c=c_eof then return 1 fi
    ungetc(c,f)
    return 0;
end

export proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
    int newlen,oldlen
    ichar newptr

    IF N=0 THEN CPL "N=0" FI

    if n=-1 then
        n:=strlen(s)
    fi

    oldlen:=dest.length

    if oldlen=0 then                !first string
        dest.strptr:=pcm_alloc(n+1)
        dest.allocated:=allocbytes
        dest.length:=n              !length always excludes terminator
        memcpy(dest.strptr,s,n)
        (dest.strptr+n)^:=0
        return
    fi

    newlen:=oldlen+n
    if newlen+1>dest.allocated then
        newptr:=pcm_alloc(newlen+1)
        memcpy(newptr,dest.strptr,oldlen)
        dest.strptr:=newptr
        dest.allocated:=allocbytes
    fi

    memcpy(dest.strptr+oldlen,s,n)
    (dest.strptr+newlen)^:=0

    dest.length:=newlen
end

export proc gs_init(ref strbuffer dest)=
    pcm_clearmem(dest,strbuffer.bytes)
end

export proc gs_free(ref strbuffer dest)=
    if dest.allocated then
        pcm_free(dest.strptr,dest.allocated)
    fi
end

export proc gs_str(ref strbuffer dest,ichar s)=
    strbuffer_add(dest,s)
end

export proc gs_char(ref strbuffer dest,int c)=
    array [16]char s

    s[1]:=c
    s[2]:=0

    strbuffer_add(dest,&.s,1)
end

export proc gs_strn(ref strbuffer dest,ichar s,int length)=
    strbuffer_add(dest,s,length)
end

export proc gs_strvar(ref strbuffer dest,s)=
    strbuffer_add(dest,s.strptr)
end

export proc gs_strint(ref strbuffer dest,int64 a)=
    strbuffer_add(dest,strint(a))
end

export proc gs_strln(ref strbuffer dest,ichar s)=
    gs_str(dest,s)
    gs_line(dest)
end

export proc gs_strsp(ref strbuffer dest,ichar s)=
    gs_str(dest,s)
    gs_str(dest," ")
end

export proc gs_line(ref strbuffer dest)=
    strbuffer_add(dest,"\w")
end

export function gs_getcol(ref strbuffer dest)int=
    return dest.length
end

export proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
    int col,i,n,slen
    array [2560]char str
    col:=dest.length
    strcpy(&.str,s)
    slen:=strlen(s)
    n:=w-slen
    if n>0 then
        for i:=1 to n do
            str[slen+i]:=padch
        od
        str[slen+n+1]:=0
    fi
    gs_str(dest,&.str)
end

export proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
    gs_leftstr(dest,strint(a),w,padch)
end

export proc gs_padto(ref strbuffer dest,int col, ch=' ')=
    int n
    array [2560]char str

    n:=col-dest.length
    if n<=0 then return fi
    for i:=1 to n do
        str[i]:=ch
    od
    str[n+1]:=0
    gs_str(dest,&.str)
end

export proc gs_println(ref strbuffer dest,filehandle f=nil)=
    if dest.length=0 then return fi
    (dest.strptr+dest.length)^:=0

    if f=nil then
!       println dest.strptr,,"\c"
        println dest.strptr
    else
!       println @f,dest.strptr,,"\c"
        println @f,dest.strptr
    fi
end

export function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=
    static int infile=0
    static ichar filestart=nil
    static ichar fileptr=nil
    static byte colonseen=0
    ref char q
    ichar item,fileext
    ichar rest
    int length
    static [300]char str

    reenter::
    value:=nil
    name:=nil

    if infile then
        if readnextfileitem(fileptr,item)=0 then        !eof
            free(filestart)                             !file allocated via malloc
            infile:=0
            goto reenter
        fi
    else
        if paramno>ncmdparams then
            return pm_end
        fi
        item:=cmdparams[paramno]
        ++paramno

        length:=strlen(item)

        if item^='@' then       !@ file
!           filestart:=fileptr:=cast(readfile(item+1))
!           if filestart=nil then
!               println "Can't open",item
!               stop 7
!           fi
            infile:=1
            goto reenter
        fi

        if item^=':' then
            colonseen:=1
            return pm_colon
        fi
    fi

    value:=nil
    if item^='-' then
        name:=item+(colonseen|0|1)
        q:=strchr(item,':')
        if not q then
            q:=strchr(item,'=')
        fi
        if q then
            value:=q+1
            q^:=0
        fi
        return (colonseen|pm_extra|pm_option)
    fi

    fileext:=extractext(item,0)
    name:=item

    if fileext^=0 then                          !no extension
        strcpy(&.str,name)
        if defext and not colonseen then
            name:=addext(&.str,defext)              !try .c
        fi
!   elsif eqstring(fileext,"dll") then
    elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
        return (colonseen|pm_extra|pm_libfile)
    fi
    return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
    ref char p,pstart,pend
    int n
    static [256]char str

    p:=fileptr

    reenter::
    do
        case p^
        when ' ','\t',13,10 then    !skip white space
            ++p
        when 26,0 then              !eof
            return 0
        else
            exit
        esac
    od

    case p^
    when '!', '#' then          !comment
        ++p
        docase p++^
        when 10 then
            goto reenter
        when 26,0 then
            fileptr:=p-1
            return 0
        else

        enddocase
    esac


    case p^
    when '"' then               !read until closing "
        pstart:=++p
        do
            case p^
            when 0,26 then
                println "Unexpected EOF in @file"
                stop 8
            when '"' then
                pend:=p++
                if p^=',' then ++p fi
                exit
            esac
            ++p
        od
    else
        pstart:=p
        do
            case p^
            when 0,26 then
                pend:=p
                exit
            when ' ','\t',',',13,10 then
                pend:=p++
                exit
            esac
            ++p
        od
    esac

    n:=pend-pstart
    if n>=str.len then
        println "@file item too long"
        stop 9
    fi
    memcpy(&.str,pstart,n)
    str[n+1]:=0
    item:=&.str
    fileptr:=p

    return 1
end

export proc ipadstr(ref char s,int width,ref char padchar=" ")=
    int n

    n:=strlen(s)
    to width-n do
        strcat(s,padchar)
    od
end

export function padstr(ref char s,int width,ref char padchar=" ")ichar=
    static [256]char str

    strcpy(&.str,s)
    ipadstr(&.str,width,padchar)
    return &.str
end

export function chr(int c)ichar=
    static [8]char str

    str[1]:=c
    str[2]:=0
    return &.str
end

export function cmpstring(ichar s,t)int=
    int res
    if (res:=strcmp(s,t))<0 then
        return -1
    elsif res>0 then
        return 1
    else
        return 0
    fi
end

export function cmpstringn(ichar s,t,int n)int=
    int res
    if (res:=strncmp(s,t,n))<0 then
        return -1
    elsif res>0 then
        return 1
    else
        return 0
    fi
end

export function eqstring(ichar s,t)int=
    return strcmp(s,t)=0
end

export function cmpbytes(ref void p,q,int n)int=
    int res
    if (res:=memcmp(p,q,n))<0 then
        return -1
    elsif res>0 then
        return 1
    else
        return 0
    fi
end

export function eqbytes(ref void p,q,int n)int=
    return memcmp(p,q,n)=0
end

export proc mseed(word64 a,b=0)=
    seed[1]:=a
    if b then
        seed[2]:=b
    else
        seed[2] ixor:=a
    fi
end

export function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!   word64 x,y
    int x,y
    x:=seed[1]
    y:=seed[2]
    seed[1]:=y
    x ixor:=(x<<23)
    seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
    return seed[2]+y
end

export function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
    return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

export function mrandomint(int n)int=
!positive random int value from 0 to n-1
    return mrandomp() rem n
end

export function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
    int span
    span:=b-a+1
    if span<=0 then
        return 0
    fi
    return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
    repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
    return x
end

export function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
    return mrandomp()/9223372036854775807
end

export function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

    int a,offset,i,size
    [100]char name
    [300]char exefile
    ref byte packexeptr         !for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
    int packexesize             !byte size
    ref char packfilename
    int packfilesize
    ref byte packfileptr

!macro getfileint(data,offset)=(ref int32(data+offset))^
    macro getfileint(data,offset)=cast(data+offset,ref int32)^

    strcpy(&exefile[1],os_gethostname())
    println "Attempting to open",&.exefile
    packexeptr:=readfile(&exefile[1])

    if not packexeptr then
        cpl "Can't open",&.exefile,packexeptr
        stop
    fi

    packexesize:=rfsize
    cpl "File read OK. Size",packexesize

    a:=getfileint(packexeptr,packexesize-int32.bytes)
    if a<>'PCAK' then
        free(packexeptr)
        packfileptr:=nil
        return nil
    fi

    offset:=getfileint(packexeptr,packexesize-int32.bytes*2)

    packfilename:=cast(packexeptr+offset)
    offset+:=strlen(packfilename)+1
    packfilesize:=getfileint(packexeptr,offset)
    packfileptr:=packexeptr+offset+int32.bytes

    return packfileptr
end

export function readline:ichar=
    readln
    return rd_buffer
end

export function findfunction(ichar name)ref void=
    for i to $getnprocs() do
        if eqstring($getprocname(i),name) then
            return $getprocaddr(i)
        fi
    od
    return nil
end

export function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
    if n iand (align-1)=0 then return n fi
    return n+(align-(n iand (align-1)))
end

=== mclib.m 1 0 23/33 ===
export type filehandle=ref void

importdll $cstd=
    func  malloc        (word64)ref void
    func  realloc   (ref void, word)ref void
    proc free       (ref void)
    proc memset     (ref void, int32, word)
    proc memcpy     (ref void, ref void, word)
    proc memmove        (ref void, ref void, word)
    func  clock     :int32
    func  ftell     (filehandle)int32
    func  fseek     (filehandle, int32, int32)int32
    func  fread     (ref void, word, word, filehandle)word
    func  fwrite        (ref void, word, word, filehandle)word
    func  getc      (filehandle)int32
    func  ungetc        (int32, filehandle)int32
    func  fopen     (ichar a, b="rb")filehandle
    func  fclose        (filehandle)int32
    func  fgets     (ichar, int, filehandle)ichar
    func  remove        (ichar)int32
    func  rename        (ichar, ichar)int32
    func  getchar   :int32
    proc putchar    (int32)
    proc setbuf     (filehandle, ref byte)

    func  strlen        (ichar)int
    func  strcpy        (ichar, ichar)ichar
    func  strcmp        (ichar, ichar)int32
    func  strncmp   (ichar, ichar, word)int32
    func  strncpy   (ichar, ichar, word)word
    func  memcmp        (ref void, ref void, word)int32
    func  strcat        (ichar, ichar)ichar
    func  tolower   (int32)int32
    func  toupper   (int32)int32
    func  isalpha   (int32)int32
    func  isupper   (int32)int32
    func  islower   (int32)int32
    func  isalnum   (int32)int32
    func  isspace   (int32)int32
    func  strstr        (ichar, ichar)ichar
    func  atol      (ichar)int
    func  atoi      (ichar)int32
!   func  strtod        (ichar,ref ref char)real64
    func  strtod        (ichar,ref ref char)real64
    func  _strdup   (ichar)ichar

    func  puts      (ichar)int32
    func  printf        (ichar, ...)int32

    func  sprintf   (ichar, ichar, ...)int32

    func  sscanf        (ichar, ichar, ...)int32
    func  scanf     (ichar, ...)int32

    func  rand      :int32
    proc srand      (word32)
    func  system        (ichar)int32

    func  fgetc     (filehandle)int32
    func  fputc     (int32,  filehandle)int32
    func  fprintf   (filehandle, ichar, ...)int32
    func  fputs     (ichar,  filehandle)int32
    func  feof      (filehandle)int32
    func  getch     :int32
    func  _getch        :int32
    func  kbhit     :int32
    func  _mkdir        (ichar)int32
    func  mkdir     (ichar)int32
    func  strchr        (ichar,int32)ichar

    func  _setmode  (int32,int32)int32

    proc _exit      (int32)
    proc "exit"     (int32)
!   proc `exit      (int32)
    func  pow       (real,real)real

    func  `sin      (real)real
    func  `cos      (real)real
    func  `tan      (real)real
    func  `asin     (real)real
    func  `acos     (real)real
    func  `atan         (real)real
    func  `log      (real)real
    func  `log10        (real)real
    func  `exp      (real)real
    func  `floor        (real)real
    func  `ceil     (real)real

    func  `llabs    (i64)i64

    proc  qsort     (ref void, word64, word64, ref proc)
    proc  sleep     (word32)

end

export macro strdup=_strdup

importdll $cstdextra=
    func  __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof      =-1
export const seek_set   = 0
export const seek_curr  = 1
export const seek_end   = 2
=== mlinux.m 1 0 24/33 ===
!import clib
!import mlib
!
!importlib cstd=
!   clang proc     sleep    (word32)
!end

record termios =
    int32 c_iflag
    int32 c_oflag
    int32 c_cflag
    int32 c_lflag
    char c_line
    [32]char c_cc               !at offset 17
    [3]byte filler
    int32 c_ispeed              !at offset 52
    int32 c_ospeed
end

importlib dlstuff=
    clang function dlopen       (ichar, int32)ref void
    clang function dlsym        (ref void, ichar)ref void
    clang function tcgetattr    (int32, ref termios) int32
    clang function tcsetattr    (int32, int32, ref termios) int32
    clang function gettimeofday (ref timeval, ref void) int32
    clang function gmtime_r     (ref i64, ref tm_rec) ref void
end
 
record timeval =
    i64 tv_sec
    i64 tv_usec
end

record tm_rec =
    int32 tm_sec
    int32 tm_min
    int32 tm_hour
    int32 tm_mday

    int32 tm_mon
    int32 tm_year
    int32 tm_wday
    int32 tm_yday
    int32 tm_isdst
    [20]byte padding
end

!this record is used by some apps, so these fields must be present
export record rsystemtime =
    int32 year
    int32 month
    int32 dayofweek
    int32 day
    int32 hour
    int32 minute
    int32 second
    int milliseconds
end

int init_flag=0


export proc os_init=
    init_flag:=1
end

export function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
    return system(cmdline)
end

export function os_execcmd(ichar cmdline, int newconsole)int =
    return system(cmdline)
end

export function os_getch:int=
    const ICANON  = 8x000002
    const ECHO    = 8x000010
    const TCSANOW = 0

    termios old,new
    char ch

    tcgetattr(0,&old)
    new:=old
    new.c_lflag iand:=inot ICANON
    new.c_lflag iand:=inot ECHO
    tcsetattr(0,TCSANOW,&new)

    ch:=getchar()

    tcsetattr(0,TCSANOW,&old)

    return ch
end

export function os_kbhit:int=
    abortprogram("kbhit")
    return 0
end

export proc os_flushkeys=
    abortprogram("flushkeys")
end

export function os_getconsolein:ref void=
    return nil
end

export function os_getconsoleout:ref void=
    return nil
end

export function os_proginstance:ref void=
    abortprogram("PROGINST")
    return nil
end

export function os_getdllinst(ichar name)u64=
    const RTLD_LAZY=1
    ref void h

    h:=dlopen(name,RTLD_LAZY)

    if h=nil then
        if strcmp(name,"msvcrt")=0 then         !might be linux
            h:=dlopen("libc.so.6",RTLD_LAZY);
        fi
    fi

    return cast(h)
end

export function os_getdllprocaddr(int hlib,ichar name)ref void=
    ref void fnaddr

    if hlib=0 then
        return nil
    fi

    fnaddr:=dlsym(cast(int(hlib)), name)
    return fnaddr
end

export proc os_initwindows=
end

export function os_getchx:int=
    abortprogram("getchx")
    return 0
end

export function os_getos=>ichar=
    if $targetbits=32 then
        return "L32"
    else
        return "L64"
    fi
end

export function os_gethostsize=>int=
    return $targetbits
end

export function os_iswindows:int=
    return 0
end

export function os_shellexec(ichar opc, file)int=
    abortprogram("SHELL EXEC")
    return 0
end

export proc  os_sleep(int a)=
    sleep(a)
end

export function os_getstdin:filehandle =
    return nil
end

export function os_getstdout:filehandle =
    return nil
!   return fopen("con","wb")
end

export function os_gethostname:ichar=
!   abortprogram("gethostname")
    return ""
end

export function os_getmpath:ichar=
!   abortprogram("getmpath")
    return ""
end

export proc os_exitprocess(int x)=
    stop
!   _exit(0)
!   ExitProcess(x)
end

export function os_clock:int64=
    if os_iswindows() then
        return clock()
    else
        return clock()/1000
    fi
end

export function os_ticks:int64=
    return clock()
end

export function os_getclockspersec:int64=
    return (os_iswindows()|1000|1000'000)
end

export proc os_setmesshandler(ref void addr)=
    abortprogram("SETMESSHANDLER")
!   wndproc_callbackfn:=addr
end

export function os_hpcounter:int64=
    return 1
end

export function os_hpfrequency:int64=
    return 1
end

export function os_filelastwritetime(ichar filename)int64=
    return 0
end

export proc os_getsystime(ref rsystemtime tm)=
    timeval tv
    tm_rec tmr


    gettimeofday(&tv, nil)
    gmtime_r(&tv.tv_sec, &tmr)

    tm.year := tmr.tm_year + 1900
    tm.month := tmr.tm_mon + 1
    tm.dayofweek := tmr.tm_wday + 1
    tm.day := tmr.tm_mday
    tm.hour := tmr.tm_hour
    tm.minute := tmr.tm_min
    tm.second := tmr.tm_sec
    tm.milliseconds := tv.tv_usec/1000
tm.month:=1         !avoid crashing the M compiler
end

export proc os_peek=
end

export function  os_allocexecmem(int n)ref byte=
    abortprogram("No allocexec")
    nil
end

=== mwindllc.m 1 0 25/33 ===
type dll0_int=ref clang function:int
type dll1_int=ref clang function(int)int
type dll2_int=ref clang function(int,int)int
type dll3_int=ref clang function(int,int,int)int
type dll4_int=ref clang function(int,int,int,int)int
type dll5_int=ref clang function(int,int,int,int,int)int
type dll6_int=ref clang function(int,int,int,int,int,int)int
type dll9_int=ref clang function(int,int,int,int, int,int,int,int, int)int
type dll10_int=ref clang function(int,int,int,int, int,int,int,int, int,int)int
type dll11_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int)int
type dll12_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int,int)int
type dll14_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int,int, int,int)int

type dll0_r64=ref clang function:r64
type dll1_r64=ref clang function(int)r64
type dll2_r64=ref clang function(int,int)r64

type dll0_r64x=ref clang function:r64
type dll1_r64x=ref clang function(real)r64
type dll2_r64x=ref clang function(real,real)r64

type m_dll0_int=ref function:int
type m_dll1_int=ref function(int)int
type m_dll2_int=ref function(int,int)int
type m_dll3_int=ref function(int,int,int)int
type m_dll4_int=ref function(int,int,int,int)int
type m_dll5_int=ref function(int,int,int,int,int)int
type m_dll12_int=ref function(int,int,int,int, int,int,int,int, int,int,int,int)int

type m_dll0_r64=ref function:r64
type m_dll1_r64=ref function(int)r64
type m_dll2_r64=ref function(int,int)r64


export function os_calldllfunction(ref proc fnaddr,
        int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
!retcode is 'R' or 'I'
!each argcodes element is 'R' or 'I' too
!The x64 version can work with any combination.
!Here, for C, only some combinations are dealt with:
! I result, params all I (not all param counts)
! R result, params all I (not all param counts)
!Mixed params, for arbitrary return type, not handled (not really detected either)

    word64 a
    real64 x
    int oddstack, nextra, pushedbytes

    if retcode='I' then
        return calldll_cint(fnaddr,args,nargs)
    else
        return calldll_creal(fnaddr,args,nargs)
    fi
end 

global function os_pushargs(ref[]word64 args, int nargs, nextra,
                    ref proc fnaddr, int isfloat)word64=
    word64 a
    real64 x
!ABORTPROGRAM("PUSHARGS/C NOT READY")

    return os_calldllfunction(fnaddr, (isfloat|0|'I'), nargs, cast(args), nil)


!   return a
end

function calldll_cint (ref proc fnaddr,ref[]i64 params,int nparams)i64=
switch nparams
when 0 then
    return dll0_int(fnaddr)^()
when 1 then
    return dll1_int(fnaddr)^(params^[1])
when 2 then
    return dll2_int(fnaddr)^(params^[1],params^[2])
when 3 then
    return dll3_int(fnaddr)^(params^[1],params^[2],params^[3])
when 4 then
    return dll4_int(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4])
when 5 then
    return dll5_int(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4], params^[5])
when 6 then
    return dll6_int(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4], params^[5],params^[6])
when 9 then 
    return (dll9_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4], params^[5],params^[6],
                params^[7],params^[8],params^[9])
when 10 then 
    return (dll10_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10])
when 11 then 
    return (dll11_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11])

when 12 then 
    return (dll12_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11],params^[12])

when 14 then 
    return (dll14_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11],params^[12],
                params^[13],params^[14])

else
    cpl nparams
    println "calldll/c/int unsupported # of params", nparams
    stop 1
endswitch
return 0
end

function calldll_creal (ref proc fnaddr,ref[]i64 params,int nparams)i64=
real64 x

switch nparams
when 0 then
    return dll0_r64(fnaddr)^()
when 1 then
    os_dummycall(params^[1],params^[2],params^[3],params^[4])
    x:=dll1_r64(fnaddr)^(params^[1])
when 2 then
    x:=dll2_r64(fnaddr)^(params^[1],params^[2])
else
    println "calldll/c/real too many params"
    stop 1
endswitch
return int64@(x)
end


global proc os_dummycall(r64 a,b,c,d)=
end
=== mm_types.m 0 1 26/33 ===
global enumdata  [0:]ichar stdnames,
        [0:]byte stdbits,
        [0:]byte stdcat =
!    type          name       bits    cat
    (tvoid=0,     "void",        0,   voidcat),

    (tc64,        "c64",        64,   d64cat),
    (tu64,        "u64",        64,   d64cat),
    (ti64,        "i64",        64,   d64cat),
    (tr32,        "r32",        32,   x32cat),
    (tr64,        "r64",        64,   x64cat),

    (tbool64,     "bool64",     64,   d64cat),
    (tref,        "ref",        64,   d64cat),

    (trecord,     "rec",         0,   blockcat),
    (trange,      "range",     128,   blockcat),

    (tarray,      "array",       0,   blockcat),
    (tslice,      "slice",     128,   blockcat),

    (tc8,         "c8",          8,   shortcat),
    (tbool8,      "b8",          8,   shortcat),
    (ti8,         "i8",          8,   shortcat),
    (ti16,        "i16",        16,   shortcat),
    (ti32,        "i32",        32,   shortcat),
    (tu8,         "u8",          8,   shortcat),
    (tu16,        "u16",        16,   shortcat),
    (tu32,        "u32",        32,   shortcat),

    (trefchar,    "ichar",      64,   d64cat),

    (tauto,       "auto",        0,   voidcat),
    (tany,        "any",         0,   voidcat),
    (tproc,       "proc",        0,   voidcat),
    (tlabel,      "label",       0,   voidcat),
    (ttype,       "type",       64,   voidcat),
    (tbitfield,   "bitfl",       8,   voidcat),
    (ttuple,      "tuple",       0,   voidcat),
    (tpending,    "pend",        0,   voidcat),

    (tlast,       "last ",       0,   voidcat),
end

global enumdata [0:]ichar catnames =
    (voidcat=0,     $),         ! Not set

    (d64cat,        $),         ! Any 64-bit value other than x64, including pointers
    (x32cat,        $),         ! 32-bit float
    (x64cat,        $),         ! 64-bit float when can't be treated as d64

    (shortcat,      $),         ! 8/16/32-bit types, maybe zero/sign-extended to d64
!    (bitcat,        $),
    (blockcat,      $),         ! 64-bit pointer to block data
end

global const tuser  = tlast

global const tint   = ti64
global const tword  = tu64
global const treal  = tr64
global const tbool  = tbool64

global const tfirstnum  = tc64
global const tlastnum   = tr64

global const tfirstshort    = tc8
global const tlastshort     = tu32

global const maxtuplesize = 4

global int trefproc
global int treflabel
=== mm_help.txt 0 1 27/33 ===
M Compiler Generating x64 native code - Windows Version

Whole-program compiler builds entire program from the lead module
into a executable file.

    mm main              # Create main.exe from lead module main.m
    mm main.m            # Same (.m extension is default)
    mm -c main           # Create single-file main.asm intermediate ASM

Options:

    -exe                 # Generate .exe executable file (default)
    -dll                 # Generate .dll library and .exp file
    -pcl                 # Generate intermediate PCL file only
    -asm                 # Generate intermediate ASM file only

    -opt                 # Apply simple optimiser

    -out:file            # Name of output file 

    -ma                  # Create .ma file combining source/support files
    -docs                # Create .txt with docstrings of exported files (not finished)
    -run                 # For -exe mode only: run resulting executable

    @file                # Read options from file

Example:

     mm -run prog : abc def

Any parameters for the new program must follow " : " (spaces needed).
=== msysc.m 0 1 28/33 ===
!MSYS version for C target

[]ref void _fnaddresses
[]ichar _fnnames
int _fnnprocs
int _fnnexports

global record procinforec=
    word16      fnindex
    byte        rettype
    byte        nparams
    [12]byte    paramlist
end

!for print/read routines
!------------------------------------------
record fmtrec=  ! (default)
    byte    minwidth    ! n (0)   min field width (0 if not used or don't care)
    i8      precision   ! .n (0)   number of decimals/significant figures/max width
    byte    base        ! B,H or Xn (10)  2 to 16

    char    quotechar   ! Qc (0)   0 or '"' or c
    char    padchar     ! Pc, Z (' ')
    char    realfmt     ! E,F,G ('f') 'e' or 'f' or 'g'

    char    plus        ! (0)   0 or '+'
    char    sepchar     ! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
    char    lettercase  ! A,a ('A') 'A' or 'a'
    char    justify     ! JL, JR, JC ('R') 'L' or 'R' or 'C'?
    char    suffix      ! Tc (0)   0 or 'B' or 'H' or c
    char    usigned     ! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
    char    charmode    ! C,D (0)  0 or 'C' or 'D'  o/p int as int or single char or double/multi-char
    char    heapmode    ! M (0)  'M' for str-functions, return ptr tp heap string
    char    param       ! Use int value for <fmtparam>
    byte    spare
end

int fmtparam            !as set with :'V'

enumdata =
    std_io,
    file_io,
    str_io
end

const comma = ','

global int needgap          = 0
int outdev          = std_io
filehandle outchan  = nil
ref char fmtstr     = nil

const maxiostack=10
[maxiostack]filehandle  outchan_stack
[maxiostack]int         outdev_stack
[maxiostack]ref char    fmtstr_stack
[maxiostack]byte        needgap_stack

[maxiostack]ref char    ptr_stack       !this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

const smallstrlen=256

!Read buffer vars
const rd_buffersize = 16384 !total capacity of line buffer

export ref char rd_buffer       ! point to start of read buffer
int rd_length           ! length of this line (as read by readln)
ref char rd_pos         ! current position it's up to (next read starts here)
ref char rd_lastpos     ! set by sread() just before reading used for reread()
int termchar            ! terminator char set by readxxx()
int itemerror           !   set by some read functions, eg for reals

export int $cmdskip         !0 unless set by READMCX/etc

const maxparam=128
export int nsysparams
export int ncmdparams
export int nenvstrings
export [maxparam]ichar sysparams
export ref[0:]ichar cmdparams
export ref[]ichar envstrings


!------------------------------------------

word64 mask63   = 0x7FFF'FFFF'FFFF'FFFF
real offset64   = 9223372036854775808.0     ! 2**63 as r64
real offset32   = 9223372036854775808.0     ! 2**63 as r32

export proc m_init(int nargs, ref[]ichar args, envstrings)=
    nsysparams:=nargs

    if nsysparams>maxparam then
        printf("Too many params\n")
        stop 1
    fi

    for i:=1 to nargs do
        sysparams[i]:=args[i]
    od

!assume nsysparams is >=1, since first is always the program name
    ncmdparams:=nsysparams-($cmdskip+1)
    cmdparams:=cast(&sysparams[$cmdskip+1])

    int j:=1
    nenvstrings:=0
    while envstrings[j] do
        ++nenvstrings
        ++j
    od

end


global function m_getdotindex(word64 a,int i)int=
    return (a iand (1<<i))>>i
end

global func m_setdotindex(word64 a, int i,x)word64=
    ref word32 a32

!see comments on setdotslice
    (a iand inot (1<<i)) ior (word64(x)<<i)
end

global function m_getdotslice(word64 a,int i,j)int=
    if i>=j then
        return (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))
    else
        return (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
    fi
end

global func m_setdotslice(word64 a, int i,j,word64 x)word64=
!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
    int w
    word64 mask64
    word mask
    ref word32 a32

    if i>j then println "SETDOTSLICE?"; stop 52 fi

!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing
!to bytes beyond the 32-bit value
!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too

    mask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i           !shifted field of w 1s
    (a iand inot mask64) ior x<<i
end

global function m_get_nprocs:int=
    return _fnnprocs
end

global function m_get_nexports:int=
    return _fnnexports
end

global function m_get_procname(int n)ref char=
    return _fnnames[n]
end

global function m_get_procaddr(int n)ref void=
    return _fnaddresses[n]
end

global function m_get_procexport(int n)ref void=
    nil
!   return &_fnexports[n]
end

proc pushio=
    if niostack>=maxiostack then
        printf("Too many io levels\n")
        stop 53
    fi
    ++niostack
    outchan_stack[niostack] := outchan
    outdev_stack[niostack]  := outdev
    fmtstr_stack[niostack]  := fmtstr
    needgap_stack[niostack] := needgap
    needgap:=0
    fmtstr:=nil
    outchan:=nil
end

global proc m_print_startfile(ref void dev)=
    pushio()
    outchan:=cast(dev)
    if dev then
        outdev:=file_io
    else
        outdev:=std_io
    fi
end

global proc m_print_startstr(ref char s)=
    ref ref char p
    pushio()

    ptr_stack[niostack]:=s
    p:=&ptr_stack[niostack]

    outchan:=cast(p)
    outdev:=str_io
end

global proc m_print_startptr(ref ref char p)=
    pushio()

    outchan:=cast(p)
    outdev:=str_io
end

global proc m_print_startcon=
    pushio()
    outdev:=std_io
end

global proc m_print_setfmt(ref char format)=
    fmtstr:=format
end

global proc m_print_end=
    needgap:=0
    nextfmtchars(1)
    if niostack=0 then return fi
    outchan := outchan_stack[niostack]
    outdev  := outdev_stack[niostack]
    fmtstr  := fmtstr_stack[niostack]
    needgap := needgap_stack[niostack]
    --niostack
end

global proc m_print_ptr(ref void a,ichar fmtstyle=nil)=
    nextfmtchars()

    printstr(strword(u64(a),"z8h"))
    needgap:=1
end

global proc m_print_i64(int64 a,ichar fmtstyle=nil)=
    [40]char s
    fmtrec fmt
    int n

    nextfmtchars()

    if fmtstyle=nil then
        if a>=0 then
            n:=u64tostr(a,&.s,10,0)
        else
            s[1]:='-'
            n:=u64tostr(-a,&s[2],10,0)+1
        fi
        printstr_n(&.s,n)

    else
        strtofmt(fmtstyle,-1,&fmt)
        if fmt.param='V' then
            fmtparam:=a
            needgap:=0
        else
            tostr_i64(a,&fmt)
        fi
    fi
    needgap:=1
end

global proc m_print_u64(word64 a,ichar fmtstyle=nil)=
    fmtrec fmt

    nextfmtchars()
    if fmtstyle=nil then
        printstr(strword(a))
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_u64(a,&fmt)
    fi
    needgap:=1
end

global proc m_print_r64(real x,ichar fmtstyle=nil)=
    [360]char s
    fmtrec fmt

    nextfmtchars()
    if fmtstyle=nil then
        sprintf(&.s,"%f",x)
        printstr(&.s)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_r64(x,&fmt)
    fi

    needgap:=1
end

global proc m_print_r32(real32 x,ichar fmtstyle=nil)=
    m_print_r64(x,fmtstyle)
end

global proc m_print_c8(int64 a,ichar fmtstyle=nil)=
    [40]char s
    fmtrec fmt
    int n

    nextfmtchars()

    s[1]:=a
    s[2]:=0
    printstr(&.s)
    needgap:=1
end

global proc m_print_str(ichar s, fmtstyle=nil)=
    nextfmtchars()
    fmtrec fmt
    if fmtstyle=nil then
        printstr(s)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_str(s,&fmt)
    fi
    needgap:=1
end

global proc m_print_newline=
    needgap:=0
    nextfmtchars(1)
    printstr("\w")
end

global proc m_print_nogap=
    needgap:=0
end

global proc m_print_space=
    needgap:=0
    printstr(" ")
end

export proc printstr(ichar s)=
    int n
    ref ref char p
    
    case outdev
    when std_io then
        printf("%s",s)
    when file_io then
        fprintf(outchan,"%s",s)
    when str_io then
        p:=cast(outchan)
        strcpy(p^,s)
        p^+:=strlen(s)
    esac
end

global proc printstr_n(ichar s,int n=-1)=
    [smallstrlen]char str
    ref ref char p

    case n
    when -1 then n:=strlen(s)       !assume zero-terminated
    when 0 then return
    esac

    case outdev
    when str_io then
        p:=cast(outchan)
        memcpy(p^,s,n)
        p^+:=n
        p^^:=0
    when file_io then
        s:=makezstring(s,n,&.str)
        fprintf(outchan,"%s",s)
        freezstring(s,n)

    when std_io then
        s:=makezstring(s,n,&.str)
        printf("%s",s)
        freezstring(s,n)
!       printf("%.*s",int32(n),s)
    esac
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
    if length then
        if f=nil then
            emitc "printf((u8*)""%.*s"",(i32)length,s);"
        else
            emitc "fprintf(f,(u8*)""%.*s"",(i32)length,s);"
        fi
    fi
end

function makezstring(ichar s,int n,ichar local)ichar=
    ichar t
    if n<smallstrlen then
        memcpy(local,s,n)
        (local+n)^:=0
        return local
    else
        t:=pcm_alloc(n+1)
        memcpy(t,s,n)
        (t+n)^:=0
        return t
    fi
end

proc freezstring(ichar t,int n)=
    if n>=smallstrlen then
        pcm_free(t,n+1)
    fi
end

proc printchar(int ch)=
    ref ref char p
    case outdev
    when std_io then
        emitc "printf((u8*)""%c"",(int)ch)"
    when file_io then
        emitc "fprintf(msysc$outchan,(u8*)""%c"",(int)ch)"
    when str_io then
        p:=cast(outchan)
        p^^:=ch
        p^+:=1
        p^^:=0
    esac
end

global proc nextfmtchars(int lastx=0)=
    char c
    ref char pstart
    int n

    if not fmtstr then          !format not in use
        if needgap then
            printchar(' ')
!       printstr_n(" ",1)
        fi
        needgap:=0
        return
    fi

    pstart:=fmtstr
    n:=0

    while (1) do
        c:=fmtstr^
        switch c
        when '#' then
            if lastx then
                goto skip
            fi
            ++fmtstr
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
            ++fmtstr
            c:=fmtstr^
            if c then
                ++fmtstr
                printchar(c)
            fi
            pstart:=fmtstr
        else
    skip::
            ++n
            ++fmtstr
        endswitch
    od
end

global proc strtofmt(ref char s,int slen,ref fmtrec fmt) =      !PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n  Width
!.n Max width/precision
!A  Convert to upper when
!a  Convert to lower when
!B  Binary
!C  Show int as single n-bit (unicode) character
!D  Show int as multi-bit (unicode) character
!E,F,G  Specify format for double (corresponds to C format codes)
!F
!G
!H  Hex
!JC Justify centre
!JL Justify left
!JR Justify right
!M  HEAPMODE???
!O  Octal
!Pc Use padding char c
!Q  Add double quotes around string (and deal with embedded quotes)
!'  Add single quotes around string (and deal with embedded quotes)
!Sc Use separator char c between every 3 or 4 digits
!Tc Use terminator char c (typically B or H)
!U  Show ints as unsigned
!V  For ints, don't display: store value as parameter for subsequent '*'
!W  Unsigned
!Xn Use base n (n is hex 0 to F)
!Z  Use "0" padding
!+  Always have + or - in front of integers
!~  Quote char is ~
!*  Same as n but uses parameter set with :'V' on previous int

    char c
    byte wset
    int n
    [0:100]char str

    fmt^:=defaultfmt

    if s=nil then return fi

    if slen=-1 then slen:=strlen(s) fi

    memcpy(&.str,s,slen)        !convert s/slen to zero-terminated string
    str[slen]:=0
    s:=&.str

    wset:=0
    while s^ do
        c:=s^
        ++s
        switch c
        when 'B', 'b' then fmt^.base:=2
        when 'H', 'h' then fmt^.base:=16
        when 'O', 'o' then fmt^.base:=8
        when 'X', 'x' then
            c:=s^
            if c then
                switch c
                when '0'..'9' then c:=c-'0'
                when 'A'..'F' then c:=c-'A'+10
                when 'a'..'f' then c:=c-'a'+10
                else
                    c:=10
                end
                fmt^.base:=c
                ++s
            fi
        when 'Q', 'q' then fmt^.quotechar:='"'
        when '~' then fmt^.quotechar:='~'
        when 'J', 'j' then
            fmt^.justify:=toupper(s^)
            if s^ then
                ++s
            fi
        when 'A' then fmt^.lettercase:='A'
        when 'a' then fmt^.lettercase:='a'
        when 'Z', 'z' then fmt^.padchar:='0'
        when 'S', 's' then
            fmt^.sepchar:=s^
            if s^ then
                ++s
            fi
        when 'P', 'p' then
            fmt^.padchar:=s^
            if s^ then
                ++s
            fi
        when 'T', 't' then
            fmt^.suffix:=s^
            if s^ then
                ++s
            fi
        when 'W', 'w' then fmt^.usigned:='W'
        when 'E', 'e' then fmt^.realfmt:='e'
        when 'F', 'f' then fmt^.realfmt:='f'
        when 'G', 'g' then fmt^.realfmt:='g'
        when '.' then
            wset:=1
        when comma,'_' then fmt^.sepchar:=c
        when '+' then fmt^.plus:='+'
        when 'D', 'd' then fmt^.charmode:='D'
        when 'C', 'c' then fmt^.charmode:='C'
        when 'M', 'm' then fmt^.heapmode:='M'
        when 'V','v' then fmt.param:='V'
        when '*' then
            n:=fmtparam
            goto gotwidth
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
gotwidth::
                if not wset then
                    fmt^.minwidth:=n
                    wset:=1
                else
                    fmt^.precision:=n
                fi
            fi
        endswitch
    od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
    [0:20]char str
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

    return expandstr(&.str,dest,strlen(&.str),fmt)
end

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =     !EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
!result will be zero-terminated, for use in this module

    int i,w,m

!check to see if result is acceptable as it is
    w:=fmt^.minwidth
    if w=0 or w<=n then     ! allow str to be longer than minwidth
        strncpy(t,s,n)
        (t+n)^:=0
        return n
    fi

    if fmt^.justify='L' then    ! left-justify
        strncpy(t,s,n)
        t+:=n
        for i:=1 to w-n do
            t^:=fmt^.padchar
            ++t
        od
        t^:=0
    elsif fmt^.justify='R' then
        if fmt^.padchar='0' and fmt^.base and (s^='-' or s^='+') then ! need to move sign outside 
            t^:=s^
            ++t
            to w-n do
                t^:=fmt^.padchar
                ++t
            od
            strncpy(t,s+1,n-1)
            (t+n-1)^:=0
        else
            to w-n do
                t^:=fmt^.padchar
                ++t
            od
            strncpy(t,s,n)
            (t+n)^:=0
        fi

    else                ! centre-justify?

        m:=(w-n+1)/2
        to m do
            t^:=fmt^.padchar
            ++t
        od
        strncpy(t,s,n)
        t+:=n
        to w-n-m do
            t^:=fmt^.padchar
            ++t
        od
        t^:=0

    fi
    return w
end

function u64tostr(u64 aa,ref char s,word base,int sep)int =     !U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
    [0:onesixty]char t
    u64 dd
    int i,j,k,g
    int dummy
    ref char s0

    i:=0
    k:=0
    g:=(base=10|3|4)

    repeat
        t[++i]:=digits[aa rem base]
        aa:=aa/base

!BUG in separator logic, doesn't work when leading zeros used, eg. printing
!out a full length binary
!so perhaps move this out to expandstr
        ++k
        if sep and aa<>0 and k=g then
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

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)
!returns length of s
    [0:onesixty]char str                ! allow for binary with separators!
    int i,j,k,n,w,usigned
    static u64 mindint=0x8000'0000'0000'0000

    usigned:=0
    if fmt^.usigned then
        usigned:=1
    fi

    if aa=mindint and not usigned then      ! minint

        str[0]:='-'
        n:=i64mintostr(&str[1],fmt^.base,fmt^.sepchar)+1
    else
        if (not usigned and aa<-0) or fmt^.plus then
            if aa<0 then
                aa:=-aa
                str[0]:='-'
            else
                str[0]:='+'
            fi
            n:=u64tostr(aa,&str[1],fmt^.base,fmt^.sepchar)+1
        else
            n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)
        fi
    fi

    if fmt^.suffix then
        str[n]:=fmt^.suffix
        str[++n]:=0
    fi

!str uses upper cases for hex/etc see if lc needed
    if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'    then    ! need lower when
        convlcstring(&.str)
    fi

!at this point, n is the str length including signs and suffix
    return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =     !U64TOSTRFMT
!see i64tostrfmt
    [0:onesixty]char str                ! allow for binary with separators!
    int i,j,k,n,w

    n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

    if fmt^.suffix then
        str[n]:=fmt^.suffix
        str[++n]:=0
    fi

!str uses upper cases for hex/etc see if lc needed
    if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'  then    ! need lower when
        convlcstring(&.str)
    fi

!at this point, n is the str length including signs and suffix
    return expandstr(&.str,s,n,fmt)
end

function i64mintostr(ref char s,int base,int sep)int =      !I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
    [0:onesixty]char t
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

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
!s is a string process according to fmtrec fmt^, and return result in t
!caller should check whether any changes are required to s (now it can just use s), but this
!check is done here anyway (with a simple copy to t)
!n is current length of s
!return length of t
!Three processing stages:
!1 Basic input string s
!2 Additions or mods: quotes, suffix, when conversion
!3 Width adjustment
!1 is detected here, 2 is done here, 3 is done by expandstr
    ref char u,v
    [256]char str
    int w,nheap     ! whether any heap storage is used # bytes allocated

    nheap:=0

    if fmt^.quotechar or fmt^.lettercase then       ! need local copy
        if n<256 then
            u:=&.str
        else
            nheap:=n+3                  ! allow for quotes+terminator
            u:=pcm_alloc(nheap)
        fi
        if fmt^.quotechar then
            v:=u
            v^:=fmt^.quotechar
            ++v
            if n then
                strcpy(v,s)
                v+:=n
            fi
            v^:=fmt^.quotechar
            ++v
            v^:=0
            n+:=2
        else
            memcpy(u,s,n)
        fi
        switch fmt^.lettercase
        when 'a' then   ! need lower when
            convlcstring(u)
        when 'A' then
            convucstring(u)
        endswitch
        s:=u
    fi

    w:=fmt^.minwidth
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

proc tostr_i64(int64 a, ref fmtrec fmt)=
    [360]char str
    int n

    case fmt^.charmode
    when 0 then
        n:=i64tostrfmt(a,&.str,fmt)
    when 'D','d' then
        n:=domultichar(ref char(&a),8,&.str,fmt)

    else                        !assume 'C'
        printchar(a)            !no other formatting allowed
        return
    esac

    printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
    [360]char str
    int n

    case fmt^.charmode
    when 'D','d' then
        n:=domultichar(ref char(&a),8,&.str,fmt)

    when 'C','c' then
        printchar(a)            !no other formatting allowed
        return

    else
        n:=u64tostrfmt(a,&.str,fmt)
    esac

    printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
    [360]char str,str2
    [0:10]char cfmt
    int n

    cfmt[0]:='%'

    if fmt^.precision then
        cfmt[1]:='.'
        cfmt[2]:='*'
        cfmt[3]:=fmt^.realfmt
        cfmt[4]:=0
        sprintf(&.str,&.cfmt,fmt^.precision,x)
    else
        cfmt[1]:=fmt^.realfmt
        cfmt[2]:=0
        sprintf(&.str,&.cfmt,x)
    fi

!at this point, n is the str length including signs and suffix

    n:=strlen(&.str)        ! current length

    if n<fmt^.minwidth then
        n:=expandstr(&.str,&.str2,n,fmt)
        strcpy(&.str,&.str2)
    fi

    printstr_n(&.str,n)
end

proc tostr_str(ref char s, ref fmtrec fmt) =
    int oldlen,newlen,n
    ref char t

!try and work out size of formatted string
    oldlen:=strlen(s)
    newlen:=oldlen

    if fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase then
        if fmt^.quotechar then
            newlen+:=2
        fi
        if fmt^.minwidth>newlen then
            newlen:=fmt^.minwidth
        fi
        t:=pcm_alloc(newlen+1)
        n:=strtostrfmt(s,t,oldlen,fmt)
        printstr_n(t,n)
        pcm_free(t,newlen+1)
    else
        printstr_n(s,oldlen)
    fi
end

function getfmt(ichar fmtstyle)ref fmtrec=
    static fmtrec fmt
    if fmtstyle then
        strtofmt(fmtstyle,-1,&fmt)
        return &fmt
    else
        return &defaultfmt
    fi
end

export function strint(int64 a, ichar fmtstyle=nil)ichar=
    static [100]char str
    ref fmtrec fmt

    m_print_startstr(&.str)
    tostr_i64(a,fmt:=getfmt(fmtstyle))
    m_print_end()
    return getstr(&.str,fmt)
end

export proc getstrint(int64 a, ichar dest)=
    m_print_startstr(dest)
    tostr_i64(a,getfmt(nil))
    m_print_end()
end

export function strword(word64 a, ichar fmtstyle=nil)ichar=
    static [100]char str
    ref fmtrec fmt

    m_print_startstr(&.str)
    tostr_u64(a,fmt:=getfmt(fmtstyle))
    m_print_end()
    return getstr(&.str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
    static [320]char str
    ref fmtrec fmt

    m_print_startstr(&.str)
    tostr_r64(a,fmt:=getfmt(fmtstyle))
    m_print_end()
    return getstr(&.str,fmt)
end

function getstr(ichar s, ref fmtrec fmt)ichar=
    if fmt^.heapmode then
        return pcm_copyheapstring(s)
    else
        return s
    fi
end

proc initreadbuffer=
    if rd_buffer then return fi
    rd_buffer:=pcm_alloc(rd_buffersize)
    rd_buffer^:=0
    rd_pos:=rd_lastpos:=rd_buffer
end

global proc m_read_conline=
    initreadbuffer()
    readlinen(nil,rd_buffer,rd_buffersize)

    rd_length:=strlen(rd_buffer)
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

global proc m_read_fileline(filehandle f)=
    initreadbuffer()
    readlinen(f,rd_buffer,rd_buffersize)

    rd_length:=strlen(rd_buffer)
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

export proc m_read_strline(ichar s)=
    int n

    initreadbuffer()
    n:=strlen(s)

    if n<rd_buffersize then
        strcpy(rd_buffer,s)
    else
        memcpy(rd_buffer,s,rd_buffersize-1)
        (rd_buffer+rd_buffersize-1)^:=0
    fi
    rd_length:=n
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

function readitem(int &itemlength)ref char =
!read next item from rd_buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!NOTE THAT THIS IS DESTRUCTIVE. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
    ref char p,s,itemstr
    char quotechar, c

    unless rd_buffer then 
        initreadbuffer()
    end unless

    s:=rd_pos

!scan string, eliminating leading white space
    while s^=' ' or s^=9 do
        ++s
    od

    itemstr:=s              !assume starts here
    rd_lastpos:=rd_pos:=s

    if s^=0 then            ! No more chars left to read return null string
        termchar:=0
        itemlength:=0
        return s
    fi

    quotechar:=0            ! Allow possible enclosing single or double quotes
    if s^='"' then
        quotechar:='"'
        ++s
    elsif s^='\'' then
        quotechar:='\''
        ++s
    fi

!loop reading characters until separator or end reached
    p:=itemstr:=s

    while s^ do
        c:=s++^
        switch c
        when ' ', 9, comma, '=' then        ! separator
            if quotechar or p=s then            !can be considered part of name if inside quotes, or is only char
                goto normalchar
            fi
            termchar:=c
            exit
        else
    normalchar::
            if c=quotechar then
                if s^=quotechar then    ! embedded quote
                    p^:=c
                    ++s
                    ++p
                else                    ! end of name
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

    if s^=0 then
        termchar:=0
    fi
    itemlength:=p-itemstr               ! actual length of token
    rd_pos:=s

    return itemstr
end

export function strtoint(ichar s,int length=-1, base=10)int64=
!return point to next char after terminator (which can be just off length of string)
    byte signd
    word64 aa
    char c,d

    itemerror:=0

    if length=-1 then
        length:=strlen(s)
    fi
!check for sign
    signd:=0
    if length and s^='-' then
        signd:=1; ++s; --length
    elsif length and s^='+' then
        ++s; --length
    fi

    aa:=0
    while length do
        c:=s++^
        --length
        switch c
        when 'A'..'F' then d:=c-'A'+10
        when 'a'..'f' then d:=c-'a'+10
        when '0'..'9' then d:=c-'0'
        when '_', '\'' then
            next
        else
            itemerror:=1
            exit
        endswitch

        if d>=base then
            itemerror:=1
            exit
        fi
        aa:=aa*base+d
    od

    if signd then
        return -aa
    else
        return aa
    fi
end

global function m_read_i64(int fmt=0)int64=
    ref char s
    int length,c
    int64 aa

    case fmt
    when 'C','c' then
        rd_lastpos:=rd_pos
        if rd_pos^ then
            return rd_pos++^
        else
            return 0
        fi
    when 'T','t' then
        return termchar
    when 'E','e' then
        return itemerror
    esac

    s:=readitem(length)


    case fmt
    when 0,'I','i' then
        return strtoint(s,length)
    when 'B','b' then
        return strtoint(s,length,2)
    when 'H','h' then
        return strtoint(s,length,16)
    esac
    return 0
end

global function m_read_r64(int fmt=0)real=
    [512]char str
    ref char s
    int length
    int32 numlength
    real x

    s:=readitem(length)

    if length=0 or length>=str.len then     !assume not a real
        return 0.0
    fi
    memcpy(&.str,s,length)
    str[length+1]:=0

    itemerror:=0

    if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
        x:=0.0
        itemerror:=1
    fi

    return x
end

export proc m_read_str(ref char dest, int destlen=0,fmt=0)=
    ref char s
    int length,numlength
    real x

    itemerror:=0
    if fmt='L' or fmt='l' then
        s:=rd_pos
        length:=rd_buffer+rd_length-rd_pos

    else
        s:=readitem(length)

        if fmt='N' or fmt='n' then
            iconvlcn(s,length)
        fi
    fi

    if destlen>0 then
        if length>=destlen then
            length:=destlen-1
            itemerror:=1
        fi
    fi
    memcpy(dest,s,length)
    (dest+length)^:=0
end

export proc readstr(ref char dest, int fmt=0,destlen=0)=
    m_read_str(dest,destlen,fmt)
end

export proc rereadln=
    rd_pos:=rd_buffer
    rd_lastpos:=rd_pos
end

export proc reread=
    rd_pos:=rd_lastpos
end

global function valint(ichar s, int fmt=0)int64=
    ref char old_pos, old_lastpos
    int64 aa

    initreadbuffer()
    old_pos:=rd_pos
    old_lastpos:=rd_lastpos

    rd_pos:=s
    aa:=m_read_i64(fmt)
    rd_pos:=old_pos
    rd_lastpos:=old_lastpos
    return aa
end

global function valreal(ichar s)real=
    ref char old_pos, old_lastpos
    real x

    initreadbuffer()
    old_pos:=rd_pos
    old_lastpos:=rd_lastpos

    rd_pos:=s
    x:=m_read_r64()
    rd_pos:=old_pos
    rd_lastpos:=old_lastpos
    return x
end

proc iconvlcn(ref char s,int n) =
    to n do
        s^:=tolower(s^)
        ++s
    od
end

proc iconvucn(ref char s,int n) =
    to n do
        s^:=toupper(s^)
        ++s
    od
end

proc convlcstring(ref char s)=
    while (s^) do
        s^:=tolower(s^)
        ++s
    od
end

proc convucstring(ref char s)=
    while (s^) do
        s^:=toupper(s^)
        ++s
    od
end

global function m_power_i64(int64 n,a)int64=
    if n<0 then
        return 0
    elsif n=0 then
        return 1
    elsif n=1 then
        return a
    elsif (n iand 1)=0 then
    !   return ipower(a*a,n/2)
        return m_power_i64(n/2,sqr a)
    else            !assume odd
        return m_power_i64((n-1)/2,sqr a)*a
    fi
end

global proc m_intoverflow=
    abortprogram("Integer overflow detected")
end

global proc m_dotindex(word i,a)=
!return a.[i] in d0
    ABORTPROGRAM("DOT INDEX")
!   assem
!       mov d0,[a]
!       mov cl,[i]
!       shr d0,cl
!       and d0,1
!   end 
end

global proc m_dotslice(word j,i,a)=
!return a.[i..j] in d0; assumes j>=i
    ABORTPROGRAM("DOT SLICE")
!   assem
!       mov d0,[a]
!       mov rcx,[i]
!       shr d0,cl
!       sub rcx,[j]
!       neg rcx             !j-1
!       mov d2,0xFFFF'FFFF'FFFF'FFFE
!       shl d2,cl
!       not d2
!       and d0,d2
!   end 
end

global proc m_popdotindex(word i,ref word p,word x)=
!p^.[i]:=x
    ABORTPROGRAM("POP DOT INDEX")
!   assem
!       mov d3,[p]
!       mov cl,[i]
!       mov d0,[d3]
!       mov d1,1
!       shl d1,cl           !000001000
!       not d1              !111110111
!       and d0,d1           !clear that bit in dest
!       mov d1,[x]
!       and d1,1
!       shl d1,cl
!       or d0,d1
!       mov [d3],d0
!   end 
end

global proc m_popdotslice(word j,i, ref word p, word x)=
!p^.[i..j]:=x
    ABORTPROGRAM("POP DOT SLICE")
!   assem
!!d3 = p
!!d4 = x, then shifted then masked x
!!d5 = i
!!d6 = clear mask
!
!       mov d3,[p]
!       mov d4,[x]
!       mov d5,[i]
!       mov rcx,d5          !i
!       shl d4,cl           !x<<i
!       mov rcx,[j]
!       sub rcx,d5          !j-i
!       inc rcx             !j-i+1
!       mov d2,0xFFFF'FFFF'FFFF'FFFF
!       shl d2,cl           !...111100000     (assume 5-bit slice)
!       not d2              !...000011111
!       mov rcx,d5          !i
!       shl d2,cl           !...000011111000  (assume i=3)
!       and d4,d2           !mask x (truncate extra bits)
!       mov d0,[d3]
!       not d2              !...111100000111
!       and d0,d2           !clear dest bits
!       or d0,d4            !add in new bits
!       mov [d3],d0
!   end 
end

global function m_imin(int64 a,b)int64=
    return min(a,b)
end

global function m_imax(int64 a,b)int64=
    return max(a,b)
end

global function m_sign(real x)real=
    if x>0.0 then return 1.0
    elsif x<0.0 then return -1.0
    else return 0.0
    fi
end

export function m_tp_i64tor64(i64 a)r64 x=
    memcpy(&x, &a, 8)
    x
end

export function m_tp_r64toi64(r64 x)i64 a=
    memcpy(&a, &x, 8)
    a
end

export function m_tp_reftoi64(ref void p)i64 a=
    memcpy(&a, &p, 8)
    a
end

export function m_tp_i64toref(i64 a)ref void p=
    memcpy(&p, &a, 8)
    p
end
=== mlib.m 0 1 29/33 ===
!const mem_check=1
const mem_check=0

global [0..300]u64 allocupper
global int alloccode                !set by heapalloc
export int allocbytes               !set by heapalloc
export int fdebug=0
export int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
export int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=(mem_check|500000|2)
array [maxmemalloc+1]ref int32 memalloctable
array [maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend          !points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8         !2048
export const int maxblocksize = 2048
export const int $maxblocksizexx = 2048

array [0:maxblocksize+1]byte sizeindextable !convert byte size to block index 1..maxblockindex

const int size16   = 1          !the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

export [0:9]ref word freelist

export record strbuffer =
    ichar strptr
    int32 length
    int32 allocated
end

!export tabledata() [0:]ichar pmnames=
export enumdata [0:]ichar pmnames=
    (pm_end=0,      $),
    (pm_option,     $),
    (pm_sourcefile, $),
    (pm_libfile,    $),
    (pm_colon,      $),
    (pm_extra,      $),
end

[2]word seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)
!array [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

export function pcm_alloc(int n)ref void =
    ref byte p

    if not pcm_setup then
        pcm_init()
    fi

    if n>maxblocksize then          !large block allocation
        alloccode:=pcm_getac(n)
        allocbytes:=allocupper[alloccode]

        p:=allocmem(allocbytes)
        if not p then
            abortprogram("pcm_alloc failure")
        fi

        if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

        return p
    fi

    alloccode:=sizeindextable[n]        !Size code := 0,1,2 etc for 0, 16, 32 etc
    allocbytes:=allocupper[alloccode]
    smallmemtotal+:=allocbytes

    if p:=ref byte(freelist[alloccode]) then        !Items of this block size available
        if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
        freelist[alloccode]:=ref word(int((freelist[alloccode])^))

        return p
    fi

!No items in freelists: allocate new space in this heap block
    p:=pcheapptr                !Create item at start of remaining pool in heap block
    pcheapptr+:=allocbytes          !Shrink remaining pool

    if pcheapptr>=pcheapend then        !Overflows?
        p:=pcm_newblock(allocbytes)     !Create new heap block, and allocate from start of that
        return p
    fi
    if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

    return p
end

export proc pcm_free(ref void p,int n) =
!n can be the actual size requested it does not need to be the allocated size
    int acode

    if n=0 then return fi

    if n>maxblocksize then      !large block
        if mem_check then removefrommemalloc(p,n) fi
        free(p)
        return
    fi

    if p then
        acode:=sizeindextable[n]        !Size code := 0,1,2 etc for 0, 16, 32 etc

        smallmemtotal-:=allocupper[acode]

        if mem_check then removefrommemalloc(p,allocupper[acode]) fi

        cast(p,ref word)^:=word(int(freelist[acode]))
        freelist[acode]:=p
    fi
end

export proc pcm_freeac(ref void p,int alloc) =
    pcm_free(p,allocupper[alloc])
end

!export proc pcm_copymem4(ref void p,q,int n) = !PCM_COPYMEM4
!!copy n bytes of memory from q to p.
!!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!!n can be zero, and need not be a multiple of 4 bytes
!
!   memcpy(p,q,n)
!end

export proc pcm_clearmem(ref void p,int n) =
    memset(p,0,n)
end

export proc pcm_init =
!set up sizeindextable too
    int j,k,k1,k2
    int64 size
    const limit=1<<33

    alloccode:=0
    if pcm_setup then
        return
    fi

    pcm_newblock(0)

    for i to maxblocksize do    !table converts eg. 78 to 4 (4th of 16,32,64,128)
        j:=1
        k:=16
        while i>k do
            k:=k<<1
            ++j
        od
        sizeindextable[i]:=j
    od

    allocupper[1]:=16
    size:=16

    for i:=2 to 27 do
        size*:=2
        allocupper[i]:=size
        if size>=threshold then
                k:=i
            exit
        fi
    od

    for i:=k+1 to allocupper.upb do
        size+:=alloc_step
        if size<limit then
            allocupper[i]:=size
            maxmemory:=size
        else
            maxalloccode:=i-1
            exit
        fi
        
    od
    pcm_setup:=1
end

export function pcm_getac(int size)int =
! convert linear blocksize from 0..approx 2GB to 8-bit allocation code

!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9

    if size<=maxblocksize then
        return sizeindextable[size]     !size 0 to 2KB
    fi

    size:=(size+255)>>8                 !scale by 256

!now same sizetable can be used for 2KB to 512KB (288 to 2KB)

    if size<=maxblocksize then
        return sizeindextable[size]+8
    fi

!sizetable now used for 512KB to 128MB (to 2KB)
    size:=(size+63)>>6                  !scale by 256

    if size<=maxblocksize then
        return sizeindextable[size]+14
    fi

!size>2048, which means it had been over 128MB.
    size:=(size-2048+2047)/2048+22
    return size
end

export function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
    static int totalheapsize
    ref byte p

    totalheapsize+:=pcheapsize
    alloccode:=0
    p:=allocmem(pcheapsize) !can't free this block until appl terminates
    if p=nil then
        abortprogram("Can't alloc pc heap")
    fi

    pcheapptr:=p
    pcheapend:=p+pcheapsize

    if pcheapstart=nil then     !this is first block
        pcheapstart:=p
    fi
    pcheapptr+:=itemsize
    return ref u32(p)
end

export function pcm_round(int n)int =
!for any size n, return actual number of bytes that would be allocated
    static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

    if n>maxblocksize then
        return n
    else
        return allocbytes[sizeindextable[n]]
    fi
end

export function pcm_allocz(int n)ref void =
    ref void p
    p:=pcm_alloc(n)

    memset(p,0,n)
    return p
end

export function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
    ref char q
    int n
    if s=nil then return nil fi

    n:=strlen(s)+1
    q:=pcm_alloc(n)
    memcpy(q,s,n)
    return q
end

export function pcm_copyheapstringn(ref char s,int n)ref char =
    ref char q
    if s=nil then return nil fi

    q:=pcm_alloc(n+1)
    memcpy(q,s,n)
    (q+n)^:=0
    return q
end

export function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
    ref char q
    if length=0 then return nil fi

    q:=pcm_alloc(length)
    memcpy(q,s,length)
    return q
end

proc addtomemalloc(ref int32 ptr,int size)=
!add ptr to allocated table
    int allocated, code

!CPL "***************ADD TO ALLOC:",ptr,size
    for i to maxmemalloc do
        if memalloctable[i]=ptr then
            CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
            stop 2
        fi

        if memalloctable[i]=nil then        !unused entry
            memalloctable[i]:=ptr

            code:=pcm_getac(size)
            allocated:=allocupper[code]

            memallocsize[i]:=allocated
            return
        fi
    od
    CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
    CPL
    stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
!remove ptr to allocated table
    int allocated, code

!CPL "------------------************REMOVE FROM ALLOC:",ptr,size
    code:=pcm_getac(size)
    allocated:=allocupper[code]

    for i to maxmemalloc do
        if memalloctable[i]=ptr then
            if memallocsize[i]<>ALLOCATED then
!               CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
                CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, ROUNDED FREESIZE=",ALLOCATED,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
                abortprogram("MEMSIZE")
            fi
            memalloctable[i]:=nil
            return
        fi
    od
    CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
    CPL 
OS_GETCH()
    abortprogram("MEM")
    stop 4
end

export function allocmem(int n)ref void =
    ref void p

    p:=malloc(n)
    if p then
        return p
    fi
    println n,memtotal
    abortprogram("Alloc mem failure")
    return nil
end

global function reallocmem(ref void p,int n)ref void =
    p:=realloc(p,n)
    return p when p
    println n
    abortprogram("Realloc mem failure")
    return nil
end

export proc abortprogram(ref char s) =
    println s
    print   "ABORTING: Press key..."
!os_getch()
    stop 5
end

export function getfilesize(filehandle handlex)int=
    word32 p,size

    p:=ftell(handlex)       !current position
    fseek(handlex,0,2)      !get to eof
    size:=ftell(handlex)        !size in bytes
    fseek(handlex,p,seek_set)   !restore position
    return size
end

export proc readrandom(filehandle handlex, ref byte mem, int offset, size) =
    int a
    fseek(handlex,offset,seek_set)
    a:=fread(mem,1,size,handlex)            !assign so as to remove gcc warning
end

export function writerandom(filehandle handlex, ref byte mem, int offset,size)int =
    fseek(handlex,offset,seek_set)
    return fwrite(mem,1,size,handlex)
end

export function setfilepos(filehandle file,int offset)int=
    return fseek(file,offset,0)
end

export function getfilepos(filehandle file)int=
    return ftell(file)
end

export function readfile(ref char filename)ref byte =
    filehandle f
    int size
    ref byte m,p

    f:=fopen(filename,"rb")
    if f=nil then
        return nil
    fi
    rfsize:=size:=getfilesize(f)

    m:=pcm_alloc(size+2)        !allow space for etx/zeof etc

    if m=nil then
        return nil
    fi

    readrandom(f,m,0,size)
    p:=m+size           !point to following byte
    (ref u16(p)^:=0)    !add two zero bytes

    fclose(f)
    return m
end

export function writefile(ref char filename,ref byte data,int size)int =
    filehandle f
    int n

    f:=fopen(filename,"wb")
    if f=nil then
        return 0
    fi

    n:=writerandom(f,data,0,size)
    fclose(f)
    return n
end

export function checkfile(ref char file)int=
    filehandle f
    if f:=fopen(file,"rb") then
        fclose(f)
        return 1
    fi
    return 0
end

export proc readlinen(filehandle handlex,ref char buffer,int size) =
!size>2
    int ch
    ref char p
    int n
    array [0:100]char buff
    byte crseen

    if handlex=nil then
        handlex:=filehandle(os_getstdin())
    fi
    if handlex=nil then
        n:=0
        p:=buffer
        do
            ch:=getchar()
            if ch=13 or ch=10 or ch=-1 then
                p^:=0
                return
            fi
            p++^:=ch
            ++n
            if n>=(size-2) then
                p^:=0
                return
            fi
        od
    fi

    buffer^:=0
    if fgets(buffer,size-2,handlex)=nil then
        return
    fi

    n:=strlen(buffer)
    if n=0 then
        return
    fi

    p:=buffer+n-1       !point to last char
    crseen:=0
    while (p>=buffer and (p^=13 or p^=10)) do
        if p^=13 or p^=10 then crseen:=1 fi
        p--^ :=0
    od

!NOTE: this check doesn't work when a line simply doesn't end with cr-lf

    if not crseen and (n+4>size) then
        cpl size,n
        abortprogram("line too long")
    fi
end

export proc iconvlcn(ref char s,int n) =
    to n do
        s^:=tolower(s^)
        ++s
    od
end

export proc iconvucn(ref char s,int n) =
    to n do
        s^:=toupper(s^)
        ++s
    od
end

export function convlcstring(ref char s)ichar s0=
    s0:=s
    while (s^) do
        s^:=tolower(s^)
        ++s
    od
    s0
end

export function convucstring(ref char s)ichar s0=
    s0:=s
    while (s^) do
        s^:=toupper(s^)
        ++s
    od
    s0
end

export function changeext(ref char s,newext)ichar=
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
    static [260]char newfile
    array [32]char newext2
    ref char sext
    int n

    strcpy(&newfile[1],s)

    case newext^
    when 0 then
        newext2[1]:=0
        newext2[2]:=0
    when '.' then
        strcpy(&newext2[1],newext)
    else
        strcpy(&newext2[1],".")
        strcat(&newext2[1],newext)
    esac


    sext:=extractext(s,1)           !include "." when it is only extension

    case sext^
    when 0 then                     !no extension not even "."
        strcat(&newfile[1],&newext2[1])
    when '.' then                       !no extension not even "."
        strcat(&newfile[1],&newext2[2])
    else                            !has extension
        n:=sext-s-2         !n is number of chars before the "."
        strcpy(&newfile[1]+n+1,&newext2[1])
    esac

    return &newfile[1]
end

export function extractext(ref char s,int period=0)ichar=
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
    ref char t,u

    t:=extractfile(s)

    if t^=0 then            !s contains no filename
        return ""
    fi

!t contains filename+ext
    u:=t+strlen(t)-1        !u points to last char of t

    while u>=t do
        if u^='.' then      !start extension found
            if (u+1)^=0 then        !null extension
                return (period|"."|"")
            fi
            return u+1          !return last part of filename as extension exclude the dot
        fi
        --u
    od
    return ""           !no extension seen
end

export function extractpath(ref char s)ichar=
    static [0:260]char str
    ref char t
    int n

    t:=s+strlen(s)-1        !t points to last char

    while (t>=s) do
        switch t^
        when '\\','/',':' then      !path separator or drive letter terminator assume no extension
            n:=t-s+1            !n is number of chars in path, which includes rightmost / or \ or :
            memcpy(&.str,s,n)
            str[n]:=0
            return &.str
        endswitch
        --t
    od
    return ""           !no path found
end

export function extractfile(ref char s)ichar=
    ref char t

    t:=extractpath(s)

    if t^=0 then            !s contains no path
        return s
    fi

    return s+strlen(t)      !point to last part of s that contains the file
    end

export function extractbasefile(ref char s)ichar=
    static [0:100]char str
    ref char f,e
    int n,flen

    f:=extractfile(s)
    flen:=strlen(f)
    if flen=0 then      !s contains no path
        return ""
    fi
    e:=extractext(f,0)

    if e^ then          !not null extension
        n:=flen-strlen(e)-1
        memcpy(&str,f,n)
        str[n]:=0
        return &.str
    fi
    if (f+flen-1)^='.' then
        memcpy(&str,f,flen-1)
        str[flen-1]:=0
        return &.str
    fi
    return f
end

export function addext(ref char s,ref char newext)ichar=
!when filespec has no extension of its own, add newext
    ref char sext

    sext:=extractext(s,1)

    if sext^=0 then                     !no extension not even "."
        return changeext(s,newext)
    fi

    return s                            !has own extension; use that
end

export function pcm_alloc32:ref void =
    ref byte p

    allocbytes:=32
    smallmemtotal+:=32

    if p:=ref byte(freelist[2]) then        !Items of this block size available
        freelist[2]:=ref word(int((freelist[2])^))
        return p
    fi

!No items in freelists: allocate new space in this heap block
    return pcm_alloc(32)
end

export proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

    smallmemtotal-:=32
    if mem_check then removefrommemalloc(p,32) fi

    cast(p,ref word)^:=word(int(freelist[2]))
    freelist[2]:=p
end

export proc pcm_free64(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

    smallmemtotal-:=64
    if mem_check then removefrommemalloc(p,64) fi

    cast(p,ref word)^:=word(int(freelist[3]))
    freelist[3]:=p
end

export function pcm_alloc16:ref void =
    ref byte p
    allocbytes:=16
    smallmemtotal+:=16

    if p:=ref byte(freelist[1]) then        !Items of this block size available
        freelist[1]:=ref word(int((freelist[1])^))
        return p
    fi

!No items in freelists: allocate new space in this heap block
    return pcm_alloc(16)
end

export proc pcm_free16(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

    smallmemtotal-:=16
    if mem_check then removefrommemalloc(p,32) fi

    cast(p,ref word)^:=word(int(freelist[1]))
    freelist[1]:=p
end

export proc outbyte(filehandle f,int x)=
    fwrite(&x,1,1,f)
end

export proc outword16(filehandle f,word x)=
    fwrite(&x,2,1,f)
end

export proc outword32(filehandle f,word x)=
    fwrite(&x,4,1,f)
end

export proc outword64(filehandle f,word64 x)=
    fwrite(&x,8,1,f)
end

export proc outstring(filehandle f, ichar s)=
    fwrite(s,strlen(s)+1,1,f)
end

export proc outblock(filehandle f, ref void p, int n)=
    fwrite(p,n,1,f)
end

export function myeof(filehandle f)int=
    int c

    c:=fgetc(f)
    if c=c_eof then return 1 fi
    ungetc(c,f)
    return 0;
end

export proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
    int newlen,oldlen
    ichar newptr

    IF N=0 THEN CPL "N=0" FI

    if n=-1 then
        n:=strlen(s)
    fi

    oldlen:=dest.length

    if oldlen=0 then                !first string
        dest.strptr:=pcm_alloc(n+1)
        dest.allocated:=allocbytes
        dest.length:=n              !length always excludes terminator
        memcpy(dest.strptr,s,n)
        (dest.strptr+n)^:=0
        return
    fi

    newlen:=oldlen+n
    if newlen+1>dest.allocated then
        newptr:=pcm_alloc(newlen+1)
        memcpy(newptr,dest.strptr,oldlen)
        dest.strptr:=newptr
        dest.allocated:=allocbytes
    fi

    memcpy(dest.strptr+oldlen,s,n)
    (dest.strptr+newlen)^:=0

    dest.length:=newlen
end

export proc gs_init(ref strbuffer dest)=
    pcm_clearmem(dest,strbuffer.bytes)
end

export proc gs_free(ref strbuffer dest)=
    if dest.allocated then
        pcm_free(dest.strptr,dest.allocated)
    fi
end

export proc gs_str(ref strbuffer dest,ichar s)=
    strbuffer_add(dest,s)
end

export proc gs_char(ref strbuffer dest,int c)=
    array [16]char s

    s[1]:=c
    s[2]:=0

    strbuffer_add(dest,&.s,1)
end

export proc gs_strn(ref strbuffer dest,ichar s,int length)=
    strbuffer_add(dest,s,length)
end

export proc gs_strvar(ref strbuffer dest,s)=
    strbuffer_add(dest,s.strptr)
end

export proc gs_strint(ref strbuffer dest,int64 a)=
    strbuffer_add(dest,strint(a))
end

export proc gs_strln(ref strbuffer dest,ichar s)=
    gs_str(dest,s)
    gs_line(dest)
end

export proc gs_strsp(ref strbuffer dest,ichar s)=
    gs_str(dest,s)
    gs_str(dest," ")
end

export proc gs_line(ref strbuffer dest)=
    strbuffer_add(dest,"\w")
end

export function gs_getcol(ref strbuffer dest)int=
    return dest.length
end

export proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
    int col,i,n,slen
    array [2560]char str
    col:=dest.length
    strcpy(&.str,s)
    slen:=strlen(s)
    n:=w-slen
    if n>0 then
        for i:=1 to n do
            str[slen+i]:=padch
        od
        str[slen+n+1]:=0
    fi
    gs_str(dest,&.str)
end

export proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
    gs_leftstr(dest,strint(a),w,padch)
end

export proc gs_padto(ref strbuffer dest,int col, ch=' ')=
    int n
    array [2560]char str

    n:=col-dest.length
    if n<=0 then return fi
    for i:=1 to n do
        str[i]:=ch
    od
    str[n+1]:=0
    gs_str(dest,&.str)
end

export proc gs_println(ref strbuffer dest,filehandle f=nil)=
    if dest.length=0 then return fi
    (dest.strptr+dest.length)^:=0

    if f=nil then
!       println dest.strptr,,"\c"
        println dest.strptr
    else
!       println @f,dest.strptr,,"\c"
        println @f,dest.strptr
    fi
end

export function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=
    static int infile=0
    static ichar filestart=nil
    static ichar fileptr=nil
    static byte colonseen=0
    ref char q
    ichar item,fileext
    ichar rest
    int length
    static [300]char str

    reenter::
    value:=nil
    name:=nil

    if infile then
        if readnextfileitem(fileptr,item)=0 then        !eof
            free(filestart)                             !file allocated via malloc
            infile:=0
            goto reenter
        fi
    else
        if paramno>ncmdparams then
            return pm_end
        fi
        item:=cmdparams[paramno]
        ++paramno

        length:=strlen(item)

        if item^='@' then       !@ file
!           filestart:=fileptr:=cast(readfile(item+1))
!           if filestart=nil then
!               println "Can't open",item
!               stop 7
!           fi
            infile:=1
            goto reenter
        fi

        if item^=':' then
            colonseen:=1
            return pm_colon
        fi
    fi

    value:=nil
    if item^='-' then
        name:=item+(colonseen|0|1)
        q:=strchr(item,':')
        if not q then
            q:=strchr(item,'=')
        fi
        if q then
            value:=q+1
            q^:=0
        fi
        return (colonseen|pm_extra|pm_option)
    fi

    fileext:=extractext(item,0)
    name:=item

    if fileext^=0 then                          !no extension
        strcpy(&.str,name)
        if defext and not colonseen then
            name:=addext(&.str,defext)              !try .c
        fi
!   elsif eqstring(fileext,"dll") then
    elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
        return (colonseen|pm_extra|pm_libfile)
    fi
    return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
    ref char p,pstart,pend
    int n
    static [256]char str

    p:=fileptr

    reenter::
    do
        case p^
        when ' ','\t',13,10 then    !skip white space
            ++p
        when 26,0 then              !eof
            return 0
        else
            exit
        esac
    od

    case p^
    when '!', '#' then          !comment
        ++p
        docase p++^
        when 10 then
            goto reenter
        when 26,0 then
            fileptr:=p-1
            return 0
        else

        enddocase
    esac


    case p^
    when '"' then               !read until closing "
        pstart:=++p
        do
            case p^
            when 0,26 then
                println "Unexpected EOF in @file"
                stop 8
            when '"' then
                pend:=p++
                if p^=',' then ++p fi
                exit
            esac
            ++p
        od
    else
        pstart:=p
        do
            case p^
            when 0,26 then
                pend:=p
                exit
            when ' ','\t',',',13,10 then
                pend:=p++
                exit
            esac
            ++p
        od
    esac

    n:=pend-pstart
    if n>=str.len then
        println "@file item too long"
        stop 9
    fi
    memcpy(&.str,pstart,n)
    str[n+1]:=0
    item:=&.str
    fileptr:=p

    return 1
end

export proc ipadstr(ref char s,int width,ref char padchar=" ")=
    int n

    n:=strlen(s)
    to width-n do
        strcat(s,padchar)
    od
end

export function padstr(ref char s,int width,ref char padchar=" ")ichar=
    static [256]char str

    strcpy(&.str,s)
    ipadstr(&.str,width,padchar)
    return &.str
end

export function chr(int c)ichar=
    static [8]char str

    str[1]:=c
    str[2]:=0
    return &.str
end

export function cmpstring(ichar s,t)int=
    int res
    if (res:=strcmp(s,t))<0 then
        return -1
    elsif res>0 then
        return 1
    else
        return 0
    fi
end

export function cmpstringn(ichar s,t,int n)int=
    int res
    if (res:=strncmp(s,t,n))<0 then
        return -1
    elsif res>0 then
        return 1
    else
        return 0
    fi
end

export function eqstring(ichar s,t)int=
    return strcmp(s,t)=0
end

export function cmpbytes(ref void p,q,int n)int=
    int res
    if (res:=memcmp(p,q,n))<0 then
        return -1
    elsif res>0 then
        return 1
    else
        return 0
    fi
end

export function eqbytes(ref void p,q,int n)int=
    return memcmp(p,q,n)=0
end

export proc mseed(word64 a,b=0)=
    seed[1]:=a
    if b then
        seed[2]:=b
    else
        seed[2] ixor:=a
    fi
end

export function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!   word64 x,y
    int x,y
    x:=seed[1]
    y:=seed[2]
    seed[1]:=y
    x ixor:=(x<<23)
    seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
    return seed[2]+y
end

export function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
    return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

export function mrandomint(int n)int=
!positive random int value from 0 to n-1
    return mrandomp() rem n
end

export function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
    int span
    span:=b-a+1
    if span<=0 then
        return 0
    fi
    return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
    repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
    return x
end

export function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
    return mrandomp()/9223372036854775807
end

export function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

    int a,offset,i,size
    [100]char name
    [300]char exefile
    ref byte packexeptr         !for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
    int packexesize             !byte size
    ref char packfilename
    int packfilesize
    ref byte packfileptr

!macro getfileint(data,offset)=(ref int32(data+offset))^
    macro getfileint(data,offset)=cast(data+offset,ref int32)^

    strcpy(&exefile[1],os_gethostname())
    println "Attempting to open",&.exefile
    packexeptr:=readfile(&exefile[1])

    if not packexeptr then
        cpl "Can't open",&.exefile,packexeptr
        stop
    fi

    packexesize:=rfsize
    cpl "File read OK. Size",packexesize

    a:=getfileint(packexeptr,packexesize-int32.bytes)
    if a<>'PCAK' then
        free(packexeptr)
        packfileptr:=nil
        return nil
    fi

    offset:=getfileint(packexeptr,packexesize-int32.bytes*2)

    packfilename:=cast(packexeptr+offset)
    offset+:=strlen(packfilename)+1
    packfilesize:=getfileint(packexeptr,offset)
    packfileptr:=packexeptr+offset+int32.bytes

    return packfileptr
end

export function readline:ichar=
    readln
    return rd_buffer
end

export function findfunction(ichar name)ref void=
    for i to $getnprocs() do
        if eqstring($getprocname(i),name) then
            return $getprocaddr(i)
        fi
    od
    return nil
end

export function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
    if n iand (align-1)=0 then return n fi
    return n+(align-(n iand (align-1)))
end

=== mclib.m 0 1 30/33 ===
export type filehandle=ref void

importdll $cstd=
    func  malloc        (word64)ref void
    func  realloc   (ref void, word)ref void
    proc free       (ref void)
    proc memset     (ref void, int32, word)
    proc memcpy     (ref void, ref void, word)
    proc memmove        (ref void, ref void, word)
    func  clock     :int32
    func  ftell     (filehandle)int32
    func  fseek     (filehandle, int32, int32)int32
    func  fread     (ref void, word, word, filehandle)word
    func  fwrite        (ref void, word, word, filehandle)word
    func  getc      (filehandle)int32
    func  ungetc        (int32, filehandle)int32
    func  fopen     (ichar a, b="rb")filehandle
    func  fclose        (filehandle)int32
    func  fgets     (ichar, int, filehandle)ichar
    func  remove        (ichar)int32
    func  rename        (ichar, ichar)int32
    func  getchar   :int32
    proc putchar    (int32)
    proc setbuf     (filehandle, ref byte)

    func  strlen        (ichar)int
    func  strcpy        (ichar, ichar)ichar
    func  strcmp        (ichar, ichar)int32
    func  strncmp   (ichar, ichar, word)int32
    func  strncpy   (ichar, ichar, word)word
    func  memcmp        (ref void, ref void, word)int32
    func  strcat        (ichar, ichar)ichar
    func  tolower   (int32)int32
    func  toupper   (int32)int32
    func  isalpha   (int32)int32
    func  isupper   (int32)int32
    func  islower   (int32)int32
    func  isalnum   (int32)int32
    func  isspace   (int32)int32
    func  strstr        (ichar, ichar)ichar
    func  atol      (ichar)int
    func  atoi      (ichar)int32
!   func  strtod        (ichar,ref ref char)real64
    func  strtod        (ichar,ref ref char)real64
    func  _strdup   (ichar)ichar

    func  puts      (ichar)int32
    func  printf        (ichar, ...)int32

    func  sprintf   (ichar, ichar, ...)int32

    func  sscanf        (ichar, ichar, ...)int32
    func  scanf     (ichar, ...)int32

    func  rand      :int32
    proc srand      (word32)
    func  system        (ichar)int32

    func  fgetc     (filehandle)int32
    func  fputc     (int32,  filehandle)int32
    func  fprintf   (filehandle, ichar, ...)int32
    func  fputs     (ichar,  filehandle)int32
    func  feof      (filehandle)int32
    func  getch     :int32
    func  _getch        :int32
    func  kbhit     :int32
    func  _mkdir        (ichar)int32
    func  mkdir     (ichar)int32
    func  strchr        (ichar,int32)ichar

    func  _setmode  (int32,int32)int32

    proc _exit      (int32)
    proc "exit"     (int32)
!   proc `exit      (int32)
    func  pow       (real,real)real

    func  `sin      (real)real
    func  `cos      (real)real
    func  `tan      (real)real
    func  `asin     (real)real
    func  `acos     (real)real
    func  `atan         (real)real
    func  `log      (real)real
    func  `log10        (real)real
    func  `exp      (real)real
    func  `floor        (real)real
    func  `ceil     (real)real

    func  `llabs    (i64)i64

    proc  qsort     (ref void, word64, word64, ref proc)
    proc  sleep     (word32)

end

export macro strdup=_strdup

importdll $cstdextra=
    func  __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof      =-1
export const seek_set   = 0
export const seek_curr  = 1
export const seek_end   = 2
=== mlinux.m 0 1 31/33 ===
!import clib
!import mlib
!
!importlib cstd=
!   clang proc     sleep    (word32)
!end

record termios =
    int32 c_iflag
    int32 c_oflag
    int32 c_cflag
    int32 c_lflag
    char c_line
    [32]char c_cc               !at offset 17
    [3]byte filler
    int32 c_ispeed              !at offset 52
    int32 c_ospeed
end

importlib dlstuff=
    clang function dlopen       (ichar, int32)ref void
    clang function dlsym        (ref void, ichar)ref void
    clang function tcgetattr    (int32, ref termios) int32
    clang function tcsetattr    (int32, int32, ref termios) int32
    clang function gettimeofday (ref timeval, ref void) int32
    clang function gmtime_r     (ref i64, ref tm_rec) ref void
end
 
record timeval =
    i64 tv_sec
    i64 tv_usec
end

record tm_rec =
    int32 tm_sec
    int32 tm_min
    int32 tm_hour
    int32 tm_mday

    int32 tm_mon
    int32 tm_year
    int32 tm_wday
    int32 tm_yday
    int32 tm_isdst
    [20]byte padding
end

!this record is used by some apps, so these fields must be present
export record rsystemtime =
    int32 year
    int32 month
    int32 dayofweek
    int32 day
    int32 hour
    int32 minute
    int32 second
    int milliseconds
end

int init_flag=0


export proc os_init=
    init_flag:=1
end

export function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
    return system(cmdline)
end

export function os_execcmd(ichar cmdline, int newconsole)int =
    return system(cmdline)
end

export function os_getch:int=
    const ICANON  = 8x000002
    const ECHO    = 8x000010
    const TCSANOW = 0

    termios old,new
    char ch

    tcgetattr(0,&old)
    new:=old
    new.c_lflag iand:=inot ICANON
    new.c_lflag iand:=inot ECHO
    tcsetattr(0,TCSANOW,&new)

    ch:=getchar()

    tcsetattr(0,TCSANOW,&old)

    return ch
end

export function os_kbhit:int=
    abortprogram("kbhit")
    return 0
end

export proc os_flushkeys=
    abortprogram("flushkeys")
end

export function os_getconsolein:ref void=
    return nil
end

export function os_getconsoleout:ref void=
    return nil
end

export function os_proginstance:ref void=
    abortprogram("PROGINST")
    return nil
end

export function os_getdllinst(ichar name)u64=
    const RTLD_LAZY=1
    ref void h

    h:=dlopen(name,RTLD_LAZY)

    if h=nil then
        if strcmp(name,"msvcrt")=0 then         !might be linux
            h:=dlopen("libc.so.6",RTLD_LAZY);
        fi
    fi

    return cast(h)
end

export function os_getdllprocaddr(int hlib,ichar name)ref void=
    ref void fnaddr

    if hlib=0 then
        return nil
    fi

    fnaddr:=dlsym(cast(int(hlib)), name)
    return fnaddr
end

export proc os_initwindows=
end

export function os_getchx:int=
    abortprogram("getchx")
    return 0
end

export function os_getos=>ichar=
    if $targetbits=32 then
        return "L32"
    else
        return "L64"
    fi
end

export function os_gethostsize=>int=
    return $targetbits
end

export function os_iswindows:int=
    return 0
end

export function os_shellexec(ichar opc, file)int=
    abortprogram("SHELL EXEC")
    return 0
end

export proc  os_sleep(int a)=
    sleep(a)
end

export function os_getstdin:filehandle =
    return nil
end

export function os_getstdout:filehandle =
    return nil
!   return fopen("con","wb")
end

export function os_gethostname:ichar=
!   abortprogram("gethostname")
    return ""
end

export function os_getmpath:ichar=
!   abortprogram("getmpath")
    return ""
end

export proc os_exitprocess(int x)=
    stop
!   _exit(0)
!   ExitProcess(x)
end

export function os_clock:int64=
    if os_iswindows() then
        return clock()
    else
        return clock()/1000
    fi
end

export function os_ticks:int64=
    return clock()
end

export function os_getclockspersec:int64=
    return (os_iswindows()|1000|1000'000)
end

export proc os_setmesshandler(ref void addr)=
    abortprogram("SETMESSHANDLER")
!   wndproc_callbackfn:=addr
end

export function os_hpcounter:int64=
    return 1
end

export function os_hpfrequency:int64=
    return 1
end

export function os_filelastwritetime(ichar filename)int64=
    return 0
end

export proc os_getsystime(ref rsystemtime tm)=
    timeval tv
    tm_rec tmr


    gettimeofday(&tv, nil)
    gmtime_r(&tv.tv_sec, &tmr)

    tm.year := tmr.tm_year + 1900
    tm.month := tmr.tm_mon + 1
    tm.dayofweek := tmr.tm_wday + 1
    tm.day := tmr.tm_mday
    tm.hour := tmr.tm_hour
    tm.minute := tmr.tm_min
    tm.second := tmr.tm_sec
    tm.milliseconds := tv.tv_usec/1000
tm.month:=1         !avoid crashing the M compiler
end

export proc os_peek=
end

export function  os_allocexecmem(int n)ref byte=
    abortprogram("No allocexec")
    nil
end

=== mwindllc.m 0 1 32/33 ===
type dll0_int=ref clang function:int
type dll1_int=ref clang function(int)int
type dll2_int=ref clang function(int,int)int
type dll3_int=ref clang function(int,int,int)int
type dll4_int=ref clang function(int,int,int,int)int
type dll5_int=ref clang function(int,int,int,int,int)int
type dll6_int=ref clang function(int,int,int,int,int,int)int
type dll9_int=ref clang function(int,int,int,int, int,int,int,int, int)int
type dll10_int=ref clang function(int,int,int,int, int,int,int,int, int,int)int
type dll11_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int)int
type dll12_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int,int)int
type dll14_int=ref clang function(int,int,int,int, int,int,int,int, int,int,int,int, int,int)int

type dll0_r64=ref clang function:r64
type dll1_r64=ref clang function(int)r64
type dll2_r64=ref clang function(int,int)r64

type dll0_r64x=ref clang function:r64
type dll1_r64x=ref clang function(real)r64
type dll2_r64x=ref clang function(real,real)r64

type m_dll0_int=ref function:int
type m_dll1_int=ref function(int)int
type m_dll2_int=ref function(int,int)int
type m_dll3_int=ref function(int,int,int)int
type m_dll4_int=ref function(int,int,int,int)int
type m_dll5_int=ref function(int,int,int,int,int)int
type m_dll12_int=ref function(int,int,int,int, int,int,int,int, int,int,int,int)int

type m_dll0_r64=ref function:r64
type m_dll1_r64=ref function(int)r64
type m_dll2_r64=ref function(int,int)r64


export function os_calldllfunction(ref proc fnaddr,
        int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
!retcode is 'R' or 'I'
!each argcodes element is 'R' or 'I' too
!The x64 version can work with any combination.
!Here, for C, only some combinations are dealt with:
! I result, params all I (not all param counts)
! R result, params all I (not all param counts)
!Mixed params, for arbitrary return type, not handled (not really detected either)

    word64 a
    real64 x
    int oddstack, nextra, pushedbytes

    if retcode='I' then
        return calldll_cint(fnaddr,args,nargs)
    else
        return calldll_creal(fnaddr,args,nargs)
    fi
end 

global function os_pushargs(ref[]word64 args, int nargs, nextra,
                    ref proc fnaddr, int isfloat)word64=
    word64 a
    real64 x
!ABORTPROGRAM("PUSHARGS/C NOT READY")

    return os_calldllfunction(fnaddr, (isfloat|0|'I'), nargs, cast(args), nil)


!   return a
end

function calldll_cint (ref proc fnaddr,ref[]i64 params,int nparams)i64=
switch nparams
when 0 then
    return dll0_int(fnaddr)^()
when 1 then
    return dll1_int(fnaddr)^(params^[1])
when 2 then
    return dll2_int(fnaddr)^(params^[1],params^[2])
when 3 then
    return dll3_int(fnaddr)^(params^[1],params^[2],params^[3])
when 4 then
    return dll4_int(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4])
when 5 then
    return dll5_int(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4], params^[5])
when 6 then
    return dll6_int(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4], params^[5],params^[6])
when 9 then 
    return (dll9_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4], params^[5],params^[6],
                params^[7],params^[8],params^[9])
when 10 then 
    return (dll10_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10])
when 11 then 
    return (dll11_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11])

when 12 then 
    return (dll12_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11],params^[12])

when 14 then 
    return (dll14_int(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11],params^[12],
                params^[13],params^[14])

else
    cpl nparams
    println "calldll/c/int unsupported # of params", nparams
    stop 1
endswitch
return 0
end

function calldll_creal (ref proc fnaddr,ref[]i64 params,int nparams)i64=
real64 x

switch nparams
when 0 then
    return dll0_r64(fnaddr)^()
when 1 then
    os_dummycall(params^[1],params^[2],params^[3],params^[4])
    x:=dll1_r64(fnaddr)^(params^[1])
when 2 then
    x:=dll2_r64(fnaddr)^(params^[1],params^[2])
else
    println "calldll/c/real too many params"
    stop 1
endswitch
return int64@(x)
end


global proc os_dummycall(r64 a,b,c,d)=
end
=== cheader.m 0 1 33/33 ===

#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"

//#include <stdio.h>
//#include <stdlib.h>
//#include <ctype.h>
//#include <string.h>
//#include <math.h>
//#include <time.h>
//#include <sys/types.h>
//#include <sys/stat.h>

//#pragma pack(1)

typedef signed char         i8;
typedef short              i16;
typedef int                i32;
typedef long long          i64;

typedef unsigned char       u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef unsigned char     byte;

typedef float              r32;
typedef double             r64;

extern r64 sqrt(r64);
extern r64 fabs(r64);

typedef struct {void* ptr; u64 length;} Slice;

#define NULL ((void*)0)

#ifndef CALLBACK
#define CALLBACK
#endif

i64 m$llabs(i64 a){ return (a>=0?a:-a);}
#define m$infinity (1.0/0.0)
=== END ===
1 mc.m 0|0
2 mm_cli.m 0|0
3 mm_assem.m 0|0
4 mc_blockc.m 0|0
5 mm_decls.m 0|0
6 mm_diags.m 0|0
7 mm_export.m 0|0
8 mc_genc.m 0|0
9 mm_lex.m 0|0
10 mm_lib.m 0|0
11 mc_libc.m 0|0
12 mm_libsourcesc.m 0|0
13 mm_modules.m 0|0
14 mm_name.m 0|0
15 mm_parse.m 0|0
16 mm_support.m 0|0
17 mm_tables.m 0|0
18 mm_type.m 0|0
19 mm_winc.m 0|0
20 mc_decls.m 0|0
21 msysc.m 1|0
22 mlib.m 1|0
23 mclib.m 1|0
24 mlinux.m 1|0
25 mwindllc.m 1|0
26 mm_types.m 0|1
27 mm_help.txt 0|1
28 msysc.m 0|1
29 mlib.m 0|1
30 mclib.m 0|1
31 mlinux.m 0|1
32 mwindllc.m 0|1
33 cheader.m 0|1
