=== MA 33 ===
=== mm.m 0 0 1/33 ===
    module mmcli
    module mm_assem
    module mc_blockmcl
    module mm_decls

    module mm_diags_dummy

    module mm_export
    module mm_lex
    module mm_lib

    module mm_libsources

    module mm_modules
    module mm_name
    module mm_parse
    module mm_support
    module mm_tables
    module mm_type


    module mc_genmcl
    module mc_genss
    module mc_libmcl
    module mc_decls as md
    module mc_objdecls
    module mc_writeexe


    module mx_decls
    module mx_run
    module mx_lib
    module mx_write
=== mmcli.m 0 0 2/33 ===

macro SHOW(m) = eval 0

enumdata []ichar optionnames=

    (header_sw,     "header"),
    (load_sw,       "load"),
    (fixup_sw,      "fixup"),
    (parse_sw,      "parse"),
    (name_sw,       "name"),
    (type_sw,       "type"),

    (asm_sw,        "asm"),
    (asm2_sw,       "c"),
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
end

byte fasmexe

INT ABC,DEF

global const logfile=langname+"x.log"

ichar outext=""             

int startclock,endclock
int cmdskip
byte msfile

ichar inputfile

proc main=




    stepruncount()
    start_common('W','X64')
end

global proc start_common(int os, target)=
    unit p,q,r
    int m,fileno,ntokens,t

    startclock:=os_clock()

    initdata(os,target)

    getinputoptions()

    readprojectfile(inputfile)

    if fverbose>=1 then
        if passlevel=run_pass then
            if not msfile or fverbose>1 then
                println "Compiling",inputfile,"to memory"
            fi
        else
            fprint "M6 Compiling # to #",inputfile:"14jlp-",changeext(outfile,outext),$
    
            println
        fi
    fi

    remove(logfile)

    do_loadmodules()


    do_parse()

    do_name()

    do_type()

    do_writema()

    do_writeexports()

    case passlevel
    when mcl_pass then
        codegen_mcl()
    when asm_pass then
        writeasmfile(asmfilename)

    when exe_pass then
        writeexefile(exefilename)

    when lib_pass then
        writelibfile(libfilename)

    when run_pass then
        runlibfile(libfilename)

    esac





    if fverbose>=2 then
        println "Finished."
    fi

    if debugmode then showlogfile() fi

    if fshowtiming then
        endclock:=os_clock()
        t:=endclock-startclock
        print "Time",t,"ms"
        if t then
            println ",",int(real(lxalllines)/t),,"K lines per second"
        else
            println
        fi
    fi
end

proc do_loadmodules=
    if passlevel<load_pass then return fi

    loadmodules()

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


INT TT:=CLOCK()
    for i:=2 to nmodules do
        rx_module(i)
    od
    rx_module(1)
TT:=CLOCK()-TT

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

    if debugmode and fshowast3 then showast("AST3") fi
end

proc initdata(int os, target)=
    pcm_init()
    lexsetup()
    initassemsymbols()
    init_tt_tables()
    initbblib()

    if os='W' then
        fwindows:=1
    else
        flinux:=1
    fi

    case target
    when 'X64' then
        fx64:=1
        if flinux then loaderror("Linux/x64") fi
    else
        loaderror("Bad os/target")
    esac
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
        prodmode:=1
    elsif prodmode and passlevel=0 then
        passlevel:=exe_pass
        outext:="exe"
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
        filename:=inputfile                 
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

    asmfilename:=getoutfilename(outfile,"asm")
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
            q:=ms.ststart.code                  
            p.nextunit:=q.a
            ms.ststart.code.a:=p
        fi

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
    fseek(f,0,seek_set) 
    println @f,count+1
    fclose(f)   
end

global function runlibfile(ichar filename)int=
    ref librec plib
    codegen_mcl()

    genss()
    plib:=writememlib(filename)

    loadmemmcu(plib)
    fixuplib(plib)

    if fshowmx then
        LOADERROR("SHOWMX missing")
    else
        runprogram(plib, cmdskip)
    fi
    return 1
end

global function writeexefile(ichar filename, int gendll=0)int=

    codegen_mcl()

    genss()

    initsectiontable()

    genexe(nil, filename, gendll)

    writeexe(filename, gendll)

    return 1
end

global function writelibfile(ichar filename)int=
    codegen_mcl()
    genss()

    writemcx(filename)

    return 1
end

global function writeasmfile(ichar filename)int=
    codegen_mcl()

    ref strbuffer asmstr
    asmstr:=getmclstr()
    writegsfile(filename,asmstr)
    gs_free(asmstr)

    return 1
end
=== mm_assem.m 0 0 3/33 ===
global function readassemline:unit=
    lex()
    return assembleline(1)
end

global function readassemblock:unit=
    unit ulist,ulistx,u

    ulist:=ulistx:=nil

    do
        lex()           
        case lx.symbol
        when eofsym then
            serror("EOF: 'End' missing in Assembler code")
        when kendsym then
            checkend(lx.symbol,kassemsym)
            lex()
            exit
        when semisym then       
        else                
            u:=assembleline(0)
            addlistunit(ulist,ulistx,u)
        esac
    od

    return makeblock(ulist)
end

function assembleline(int oneline)unit=

    unit dlist,dlistx,p,pname,q
    ichar name
    int opc,noperands
    ref strec stname

    dlist:=dlistx:=nil


    if lx.symbol=namesym and nextlx.symbol in [colonsym,dcolonsym] then 
        p:=createunit0(jlabeldef)
        stname:=getduplnameptr(currproc,lx.symptr,labelid)
        p.def:=stname
        adddef(currproc,stname)
        lex()           
        if oneline then
            lex()
        fi
        return p

    elsif lx.symbol=mulsym then     
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
        $else

    elsif lx.symbol=namesym then                

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

=== mc_blockmcl.m 0 0 4/33 ===
const kjumpt = 1        
const kjumpf = 0

const dodotchains=0

const maxnestedloops    = 50

const maxparams=100

const maxswitchrange=500
const maxcases=maxswitchrange

const maxcasedepth=20
[maxcasedepth]unit casestmt
[maxcasedepth]int caseelse
int casedepth

ref[]int sw_labeltable          
ref[]int sw_valuetable
int sw_lower
int sw_ncases                   
byte sw_defaultseen             
int sw_defaultlabel
int sw_breaklabel

int maxreg=0

global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)

global macro loadunitx(p, isref, reg) = (isref|loadref(p, reg)|loadunit(p, reg))

global func loadunit(unit p, int reg=rnone)operand tx=
    unit a,b,c
    symbol d
    ref[]int32 pmult
    operand ux,ax
    int regs:=regset, oldreg

    if p=nil then return nil fi
    mlineno:=p.pos
    tx:=nil

    a:=p.a
    b:=p.b
    c:=p.c


    switch p.tag
    when jconst then
        tx:=do_const(p, 1, reg)

    when jname then
        tx:=do_name(p,1, reg)

    when jblock then
        while a, a:=a.nextunit do
            tx:=loadunit(a, (a.nextunit|rnone|reg))
            if tx and a.nextunit then
                popregs(regs)
            fi
        od

    when jreturn then
        tx:=do_return(a)

    when jreturnmult then
        tx:=do_returnmult(a)
        regs:=regset

    when jassign then
        tx:=do_assign(p,a,b)

    when jassignms then
        do_assignms(a,b)

    when jto then
        do_to(p,a,b)

    when jif then
        tx:=do_if(p,a,b,c,0, reg)

    when jforup, jfordown then
        do_for(p,a,b,c, p.tag=jfordown)
    when jforall then
        do_forall(p,a,b,c,0)

    when jwhile then
        do_while(p,a,b,c)
    when jrepeat then
        do_repeat(p,a,b)
    when jgoto then
        do_goto(a)

    when jlabeldef then
        do_labeldef(p)

    when jredo then
        do_exit(p,1)

    when jnext then
        do_exit(p,2)

    when jexit then
        do_exit(p,3)

    when jdo then
        do_do(a)
    when jcase then
        tx:=do_case(p,a,b,c, loopsw:0, isref:0, reg:reg)
    when jdocase then
        do_case(p,a,b,c, loopsw:1, isref:0)

    when jswitch then
         tx:=do_switch(p,a,b,c, loopsw:0, isref:0, reg:reg)
    when jdoswitch then
         do_switch(p,a,b,c, loopsw:1, isref:0)

    when jswap then
        do_swap(a,b)

    when jselect then
        tx:=do_select(p,a,b,c,0, reg)

    when jprint,jprintln, jfprint,jfprintln then
        do_print(p,a,b)

    when jread then
        tx:=do_read(p,a)

    when jreadln then
        do_readln(a)

    when jstop then
        do_stop(a)
    when jeval          then
        case p.index
        when load_op then
 tx:=loadunit(a)

        when get_op then tx:=evalunit(a)
        when loadref_op then tx:=loadref(a,r0)
        when getref_op then tx:=loadref(a)
        esac

        genmc(m_evalx, tx)
        GENCOMMENT("")
        popregs(regs)
        tx:=nil

    when jandl then
        tx:=do_andl(p,a,b, reg)

    when jorl then
        tx:=do_orl(p,a,b, reg)
    when jcallfn, jcallproc then
        tx:=do_callproc(p,a,b, p.tag=jcallfn)
    when jcmp then
        tx:=do_setcc(p,a,b, reg)

    when jcmpchain then
        tx:=do_setccchain(p,a, reg)
    when jbin then
        tx:=do_bin(p,a,b,reg)

    when jindex then
        tx:=do_index(a,b,1,reg)

    when jslice then
        tx:=do_slice(p,a,b, reg)
    when jdotindex then
        tx:=do_dotindex(p,a,b, reg)

    when jdotslice then
        tx:=do_dotslice(p,a,b, reg)

    when jdot then
        tx:=do_dot(p,1,reg)

    when jptr then
        tx:=do_ptr(p,a,1,reg)

    when jaddrof, jaddroffirst then
        if b then GERROR("ADDROF OFFSET?") fi
        tx:=loadref(a,reg)
    when jconvert then
        tx:=do_convert(p,a)

    when jtypepun then
        tx:=do_typepun(p,a, reg)

    when jshorten then
        tx:=loadunit(a,reg)

    when jtypeconst then
        tx:=genint(p.value)
    when junary then
        tx:=do_unary(p,a, reg)

    when jnotl then
        tx:=do_notl(p,a, reg)

    when jistruel then
        tx:=do_istruel(p,a, reg)
    when jincr          then
        if p.pclop in [kincr, kdecr] then
            do_incr(p,a)
        else
            tx:=do_incrload(p,a, reg)
        fi

    when jbinto then
        do_binto(p,a,b)

    when jsyscall then
        tx:=do_syscall(p,a, reg)

    when jassem         then
        domcl_assem(p)
        if p.mode then
            if ttisreal[p.mode] then
                tx:=genxreg(r0)
            else
                tx:=genreg(r0)
            fi
        fi

    when jcvlineno      then
        tx:=genint(getlineno(mlineno))
    when jempty then
        do_clear(p,a)
else
        gerror_s("Loadunit: ",jtagnames[p.tag])
        return nil
    endswitch


    if p.resultflag then

        if tx=nil then gerror("1:Result expected") fi

        if reg and tx.reg<>reg then
            oldreg:=tx.reg
            if reg>=xr0 and oldreg<xr0 or reg<xr0 and oldreg>=xr0 then
                GERROR("LU/MOVE: MIXED REGS")
            fi

            ax:=genreg(reg)
            if reg>=xr0 then
                genmc(m_movq, ax, tx)       
            else
                genmc(m_mov, ax, tx)
            fi
            tx:=ax
        fi

        if ttbasetype[p.mode]<>ttuple then
            popregs(regs,tx)
        fi

    else
        if tx then
            case p.tag
            when jassign, jcallproc, jsyscall then
            else
                popregs(regs)
            esac
        else
            popregs(regs)
        fi
        return nil
    fi


    return tx
end

global func evalunit(unit p, int access=0)operand tx=

INT ISWRITE:=0
    int regs, doload
    unit a:=p.a, b:=p.b

    if not ismemtag[p.tag] then         
        if iswrite then
            gerror("Evalunit: can't write")
        fi
        return loadunit(p)
    fi

    regs:=regset
    doload:=0

    if access='A' and p.memmode and ttisshort[p.memmode] then doload:=1 fi

    switch p.tag
    when jconst then
        tx:=do_const(p,doload)

    when jname then
        tx:=do_name(p,doload)

    when jeval then
        tx:=evalunit(a)
        genmc(m_evalx, tx)
        popregs(regs)
        tx:=nil

    when jindex then
        tx:=do_index(a,b, doload)

    when jdotindex, jdotslice then
        return loadunit(p)

    when jdot then
        tx:=do_dot(p,doload)

    when jptr then
        tx:=do_ptr(p,a,doload)


    when jaddrof then
        if b then GERROR("ADDROF OFFSET?") fi
        tx:=loadref(a)
    else
        gerror_s("Evalunit: ",jtagnames[p.tag])
        return nil
    endswitch

    if p.resultflag then
        if tx=nil then gerror("2:Result expected") fi
    else
        if tx then
            popregs(regs, tx)
        fi
        return nil
    fi

    return tx
end

func loadref(unit p, int reg=0)operand px=
    int regs:=regset



    case p.tag
    when jif then
        px:=do_if(p,p.a,p.b,p.c,reg)


    when jcallfn then
        if ttisblock[p.mode] then
            px:=loadunit(p, reg)
            popregs(regs, px)
            return px

        fi
        error

    when jslice then
        px:=loadunit(p)

    elsif not ismemtag[p.tag] then
error::
        gerror_s("Loadref? ",jtagnames[p.tag])
    else
        px:=evalunit(p,'W')             
    esac

    px:=loadmemaddr(px,reg)

    popregs(regs, px)
    px
end

global proc pushunit(unit p)=
    int regs:=regset
    operand ax

    ax:=loadunit(p)

    if ttisreal[p.mode] then
        pushstack(8)
        genmc(m_movq, distackopnd, ax)
    else
        genmc(m_push,ax)
    fi

    ++mstackdepth

    popregs(regs)
end

func poptoreg(int m, reg=rnone)operand ax=

    if ttisreal[m] then
        ax:=genxreg(reg)
        genmc((m=tr64|m_movq|m_movd), ax, distackopnd)
        popstack(8)
    else
        ax:=genreg(reg)
        genmc(m_pop,ax)
    fi

    --mstackdepth

    ax
end

global func poptoarg(int m, argno, variadic=0)int=
    operand ax

    if ttisreal[m] then
        ax:=genxreg(argno+(xr0-1))
        genmc((m=tr64|m_movq|m_movd), ax, distackopnd)
        popstack(8)
        if argno>=variadic then         
            genmc(m_movq, genreg(argno+r10-1), genxreg(argno+xr0-1))
        fi
    else
        ax:=genreg(argno+(r10-1))
        genmc(m_pop,ax)
    fi

    --mstackdepth

    ax.reg
end

global proc loadarg(unit p, int argno, variadic=0) =

    --argno                         


    if ttisreal[p.mode] then
        loadunit(p, argno+xr0)
        if variadic and argno+1>=variadic then          
            genmc(m_movq, genreg(argno+r10), genxreg(argno+xr0))
        fi
    else
        loadunit(p, argno+r10)
    fi
end

func loadbin(unit a,b, int reg=rnone, isrev=0, loadb=1)operand, operand=
    operand ax, bx

    if not loadb and b.memmode and ttcat[b.memmode]=shortcat then
        loadb:=1
    fi

    if b.simple then                
        ax:=loadunit(a, reg)
        bx:=(loadb|loadunit(b)|evalunit(b))


    elsif a.simple and isrev then           
        ax:=loadunit(b, reg)
        bx:=(loadb|loadunit(a)|evalunit(a))
    else                            
        pushunit(a)
        bx:=(loadb|loadunit(b)|evalunit(b))
        ax:=poptoreg(a.mode)

    fi

    return (ax, bx)
end

func loadbinto(unit a,b, int loadb=1)operand, operand=
    operand ax, bx

    if b.simple then                
        ax:=evalunit(a,'W')
        bx:=(loadb|loadunit(b)|evalunit(b,'A'))

    elsif a.simple then
        bx:=(loadb|loadunit(b)|evalunit(b,'A'))
        ax:=evalunit(a,'W')
    else                            
        pushunit(b)
        ax:=evalunit(a,'W')
        bx:=poptoreg(b.mode)

    fi

    return (ax, bx)
end


func do_const(unit p, int load=1, reg=rnone)operand ax =
    int mode:=p.mode
    operand px

    if ttisinteger[mode] or mode=tbool64 then
        if not load and p.value in i32.min..i32.max then        
            return genint(p.value)
        fi
loadint::
        genmc(m_mov, ax:=genreg(reg), genint(p.value))

    elsif ttisreal[mode] then
        px:=genrealmem(p.xvalue, mode)
        return px when not load

        genmc((mode=tr64|m_movq|m_movd), ax:=genxreg(reg), px)
        return ax

    elsif ttisref[mode] then
        if p.isastring then
            genmc(m_mov, ax:=genreg(reg), genstring(p.svalue))
        else
            loadint
        fi
    else

        gerror("do_const")
    fi

    ax
end

func do_name(unit p, int load=1,reg=rnone)operand tx=
    symbol d:=p.def
    operand px
    int cat

    case d.nameid
    when procid,dllprocid then
        if load then
            genmc(m_mov, tx:=genreg(reg), genmemaddr(d))
            return tx
        else
            return genmemaddr(d)
        fi

    when labelid then
        if d.index=0 then
            d.index:=++mlabelno
        fi
        if p.resultflag then        
            genmc(m_mov, tx:=genreg(reg), genlabel(d.index))
            return tx
        else
            genjumpl(d.index)
        fi
        p.resultflag:=0
        p.mode:=tvoid
    else
        px:=genmem(d)

        cat:=ttcat[d.mode]
        if not load then return px fi


        tx:=(ttisreal[d.mode]|genxreg(reg,px.size)|genreg(reg))

        case cat
        when d64cat,x64cat then
            genmc(getopndmov(tx), tx, px)
        when x32cat then
            genmc(getopndmov(tx), tx, px)
        when shortcat then
            genmc((ttsigned[d.mode]|m_movsx|m_movzx), tx, px)
        when blockcat then
            genmc(m_lea, tx, px)

        esac
        return tx
    esac
    return nil
end

proc genjumpl(int lab)=
    genmc(m_jmp, genlabel(lab))
end

func do_assign(unit p,a,b)operand tx=
    operand px, ax, bx
    unit c
    symbol d
    int offset, regs

    regs:=regset
    tx:=nil

    if b.tag=jmakelist then
        if not p.resultflag then
GERROR("ASSIGNBLOCK")
            return nil
        fi
    fi

    if b.tag=jmakeslice and a.tag=jname then
        ax:=loadunit(b.a)
        bx:=loadunit(b.b)

        d:=a.def
        genmc(m_mov, px:=genmem(d), ax)
        genmc(m_mov, applyoffset(px,8), bx)
        popregs(regs)
        if p.resultflag then
            genmc(m_lea, tx:=genreg(), genmem(d))
        fi
        return tx
    fi

    case a.tag
    when jdotindex then
        tx:=do_storedotindex(a,b,p.resultflag)
        return tx
    when jdotslice then
        tx:=do_storedotslice(a,b, p.resultflag)
        return tx
    esac



    a.resultflag:=1

    (px, bx):=loadbinto(a,b, loadb:1)

    tx:=storemem(px, bx, getmemmode(a), p.resultflag)

    if p.resultflag then
        popregs(regs, tx)
        return tx
    else
        popregs(regs)
        return nil
    fi
end

func loadmem(int load, operand ax, int reg, mode)operand tx=

    return ax when not load

    if ax.mode in [a_reg, a_xreg] then
        if ax.reg<>reg then
CPL "*************LOADMEM/ALREADY IN REG"
            GENCOMMENT("LOADMEM: MOVE REG NEEDED")

        fi
        return ax
    fi

    if ttisreal[mode] then
        tx:=genxreg(reg)
    else
        tx:=genreg(reg)
    fi


    case ttcat[mode]
    when d64cat then
        genmc(m_mov, tx, ax)
    when x64cat then
        genmc(m_movq, tx, ax)
    when x32cat then
        genmc(m_movd, tx, ax)
    when shortcat then
        genmc((ttsigned[mode]|m_movsx|m_movzx), tx, changeopndsize(ax,ttsize[mode]))
    when blockcat then
        if ax.mode=a_mem and ax.reg and ax.regix=0 and ax.valtype=0 and ax.offset=0 then
            return genreg(ax.reg)
        fi
        genmc(m_lea, tx, ax)
    esac

    return tx
end

func loadmemaddr(operand ax, int reg=rnone)operand tx=

    if ax.mode in [a_reg, a_xreg] and reg in r0..r13 then
        if ax.reg<>reg then
            GENCOMMENT("LOADMEMADDR: MOVE REG NEEDED")
        fi
        return ax
    fi

    tx:=genreg(reg)
    genmc((ax.mode=a_imm|m_mov|m_lea), tx, ax)
    return tx
end

func storemem(operand ax, bx, int mode, needres)operand=
    int regs:=regset

    case ttcat[mode]
    when d64cat then
        genmc(m_mov, ax, bx)
    when x64cat then
        genmc(m_movq, ax, bx)
    when x32cat then
        genmc(m_movd, ax, bx)
    when shortcat then
        genmc(m_mov, ax, changeopndsize(bx,ax.size))    
    when blockcat then
        copyblock(ax, bx, ttsize[mode])
    esac

    if not needres then
        bx:=nil
    fi
    popregs(regs, bx)
    bx
end

func do_bin(unit p, a, b, int reg=rnone)operand =
    [100]char str
    operand ax, bx:=nil, rx, tx
    int cat:=ttcat[p.mode], opc, offset, size, regs, scale, n
    int target:=tttarget[a.mode], rev

    regs:=regset
    rev:=0

    switch p.pclop
    when kadd then
        rev:=1
        case cat
        when d64cat then opc:=m_add
        when x64cat then opc:=m_addsd
        when x32cat then opc:=m_addss
        else
            error
        esac    
doadd::
        (ax, bx):=loadbin(a,b, isrev:rev, loadb:0, reg:reg)

        genmc(opc, ax, bx)

    when ksub then
        case cat
        when d64cat then opc:=m_sub
        when x64cat then opc:=m_subsd
        when x32cat then opc:=m_subss
        else
            error
        esac    

        doadd

    when kmul then
        rev:=1
        case cat
        when d64cat then opc:=m_imul2
        when x64cat then opc:=m_mulsd
        when x32cat then opc:=m_mulss
        else
            error
        esac    

        doadd

    when kdiv then
        case cat
        when d64cat then doidiv
        when x64cat then opc:=m_divsd
        when x32cat then opc:=m_divss
        else
            error
        esac    

        doadd

    when kiand then
        if cat<>d64cat then error fi
        rev:=1
        opc:=m_andx
        doadd

    when kior then
        if cat<>d64cat then error fi
        rev:=1
        opc:=m_orx
        doadd

    when kixor then
        if cat<>d64cat then error fi
        rev:=1
        opc:=m_xorx
        doadd

    when kmin then
        case cat
        when x64cat then opc:=m_minsd
        when x32cat then opc:=m_minss
        when d64cat then
            (ax, bx):=loadbin(a,b, reg:reg)
            genmc(m_cmp, ax, bx)
            genmc_cond(m_cmovcc, (ttsigned[a.mode]|gt_cond|gtu_cond), ax, bx)
            finish
        esac    
        doadd

    when kmax then
        case cat
        when x64cat then opc:=m_maxsd
        when x32cat then opc:=m_maxss
        when d64cat then
            (ax, bx):=loadbin(a,b, reg:reg)
            genmc(m_cmp, ax, bx)
            genmc_cond(m_cmovcc, (ttsigned[a.mode]|lt_cond|ltu_cond), ax, bx)
            finish
        esac    
        doadd

    when kidiv then
doidiv::
        ax:=do_divrem(a,b, ttsigned[a.mode], isdiv:1)

    when kirem then
        ax:=do_divrem(a,b, ttsigned[a.mode], isdiv:0)

    when kaddrefoff then
        size:=ttsize[target]
        if b.tag=jconst then                
            ax:=loadunit(a)
            tx:=genindex(areg:ax.reg, offset:b.value*size)
        else
            (ax, bx):=loadbin(a,b, loadb:1, reg:reg)
            scale:=scaleindex(bx, size)
            tx:=genindex(areg:ax.reg, ireg:bx.reg, scale:scale)
        fi
        genmc(m_lea, ax:=genreg(), tx)

    when ksubrefoff then
        size:=ttsize[target]
        if b.tag=jconst then                
            ax:=loadunit(a)
            tx:=genindex(areg:ax.reg, offset:-b.value*size)
        else
            (ax, bx):=loadbin(a,b, loadb:1, reg:reg)
            mulimm(bx, size)
            genmc(m_sub, ax, bx)
            popregs(regs)
            return ax
        fi
        genmc(m_lea, ax:=genreg(), tx)

    when ksubref then
        size:=ttsize[target]
        ax:=loadunit(a)
        bx:=loadunit(b)
        genmc(m_sub, ax, bx)
        if size>1 then
            n:=ispoweroftwo(size)
            if n then
                genmc(m_shr, ax, genint(n))
            else
                gerror("ref-ref not 2^n")
            fi
        fi

    when kshl then
        ax:=do_shift(a, b, m_shl)

    when kshr then
        ax:=do_shift(a, b, (ttsigned[a.mode]|m_sar|m_shr))

    when kpower then
        if ttsigned[a.mode] then
            ax:=gensysfn(sf_power_i64, a,b)
        else
            ax:=do_maths2(a,b, reg, "pow*")
        fi
    else
error::
        fprint @str,"Bin:#:#",pclnames[p.pclop],strmode(p.mode)
        gerror(str)
    end

finish::
    popregs(regs, ax)
    ax
end

func do_unary(unit p, a, int reg=rnone)operand =
    [100]char str
    operand ax, bx:=nil, rx, tx, lx
    int cat:=ttcat[p.mode], opc, regs

    regs:=regset

    switch p.pclop
    when kneg then
        ax:=loadunit(a, reg)
        case cat
        when d64cat then
            genmc(m_neg, ax)
        when x64cat then
            if not labneg64 then labneg64:=createfwdlabel() fi
            genmc(m_xorpd, ax, genlabelmem(labneg64))

        when x32cat then
            if not labneg32 then labneg32:=createfwdlabel() fi
            genmc(m_xorps, ax, genlabelmem(labneg32))
        else
            error
        esac    

    when kinot then
        ax:=loadunit(a, reg)
        genmc(m_notx, ax)

    when kabs then
        ax:=loadunit(a, reg)
        case cat
        when d64cat then
            genmc(m_cmp, ax, genint(0))
            genmc_cond(m_jmpcc, ge_cond, lx:=genlabel(++mlabelno))
            genmc(m_neg,ax)
            genmc(m_labelx, lx)
        when x64cat then
            if not lababs64 then lababs64:=createfwdlabel() fi
            genmc(m_andpd, ax, genlabelmem(lababs64))
        when x32cat then
            if not lababs32 then lababs32:=createfwdlabel() fi
            genmc(m_andps, ax, genlabelmem(lababs32))
        esac

    when ksqr then
        ax:=loadunit(a, reg)

        case cat
        when d64cat then opc:=m_imul2
        when x64cat then opc:=m_mulsd
        when x32cat then opc:=m_mulss
        else
            error
        esac
        genmc(opc, ax, ax)

    when ksqrt then
        ax:=loadunit(a, reg)

        case cat
        when x64cat then opc:=m_sqrtsd
        when x32cat then opc:=m_sqrtss
        else
            error
        esac
        genmc(opc, ax, ax)

    when ksin, kcos, ktan, kasin, kacos, katan, kexp,
        kround, kceil, kfloor then
        strcpy(str, pclnames[p.pclop]+1)
        strcat(str, "*")
domaths::
        ax:=do_maths(a, reg, str)
    when kln then
        strcpy(str, "log*")
        domaths
    when klog then
        strcpy(str, "log10*")
        domaths

    when ksliceptr then
        ax:=loadunit(a,reg)
        bx:=genireg(ax.reg)
        genmc(m_mov, ax, makeopndind(ax))

    when klen then                  
        ax:=loadunit(a,reg)
        bx:=genireg(ax.reg)
        genmc(m_mov, ax, applyoffset(makeopndind(ax),8))

    else
        fprint @str,"Unary:#:#",pclnames[p.pclop],strmode(p.mode)
        gerror(str)
        error
    end

    popregs(regs, ax)
    ax
end

proc do_labeldef(unit p)=
    symbol d
    [256]char str

    d:=p.def
    if d.index=0 then
        d.index:=++mlabelno
    fi

    print @&.str,d.name,,"::"
    gencomment(&.str)
    genmc(m_labelx, genlabel(d.index))
end

proc do_goto(unit a)=
    operand ax
    symbol d

    if a.tag=jname and a.def.nameid=labelid then
        d:=a.def
        if d.index=0 then
            d.index:=++mlabelno
        fi
        genmc(m_jmp, genlabel(d.index))
    else
        int regs:=regset
        ax:=loadunit(a)
        genmc(m_jmp, ax)
        popregs(regs)
    fi
end

proc do_to(unit p,a,b) =
    operand ax,bx,cx
    unit cvar
    int lab_b,lab_c,lab_d,count, regs:=regset

    cvar:=p.c

    a.mode:=ti64

    ax:=loadunit(a)
    genmc(m_mov, cx:=genmem(cvar.def), ax)
    popregs(regs)

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()
    stacklooplabels(lab_b,lab_c,lab_d)

    if a.tag<>jconst then           
        ax:=loadunit(cvar)
        genmc(m_cmp, ax, genint(0))

        genmc_cond(m_jmpcc, le_cond, genlabel(lab_d))
        popregs(regset)

    else
        count:=a.value
        if count<=0 then
            genjumpl(lab_d)
        fi
    fi

    definefwdlabel(lab_b)
    popregs(regs)
    loadunit(b)                     

    definefwdlabel(lab_c)

    genmc(m_dec, cx)
    genmc_cond(m_jmpcc, nz_cond, genlabel(lab_b))

    definefwdlabel(lab_d)
    --loopindex
end

proc docond(int opc,unit p,int lab)=
    genjumpcond(opc,p,lab)
end

proc genjumpcond(int opc,unit p,int lab)=
    unit q,r,s, a,b
    int lab2,i, regs:=regset, regs2, nolab
    operand ax, bx, cx, lx

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
        ax:=loadunit(q)
        if ax.mode=a_xreg then
            genmc((q.mode=tr32|m_movd|m_movq), bx:=genreg(), ax)
            ax:=bx
        fi
    
        genmc(m_andx, ax, ax)
        genmc_cond(m_jmpcc, (opc=kjumpt|nz_cond|z_cond), genlabel(lab))
        popregs(regs)

    when jblock then
        while q and q.nextunit do
            loadunit(q)
            q:=q.nextunit
        od
        genjumpcond(opc,q,lab)

    when jcmp then

        gcomparejump(opc,p.pclop,q,r,lab)

    when jinrange then
        a:=r.a
        b:=r.b
        unless a.simple and b.simple then gerror("inrange/complx") end
        ax:=loadunit(q)                 
        bx:=evalunit(a,'A')
        cx:=evalunit(b,'A')
        lx:=genlabel(lab)

        genmc(m_cmp, ax,bx)
        if opc=kjumpt then
            nolab:=createfwdlabel()
            genmc_cond(m_jmpcc, (ttsigned[a.mode]|lt_cond|ltu_cond),genlabel(nolab))
            genmc(m_cmp, ax, cx)
            genmc_cond(m_jmpcc, (ttsigned[a.mode]|le_cond|leu_cond),lx)
            definefwdlabel(nolab)
        else
            genmc_cond(m_jmpcc, (ttsigned[a.mode]|lt_cond|ltu_cond),lx)
            genmc(m_cmp, ax, cx)
            genmc_cond(m_jmpcc, (ttsigned[a.mode]|gt_cond|gtu_cond),lx)
        fi

    when jinset then
        s:=r.a
        if s=nil then
            gerror("empty set")
        fi

        ax:=loadunit(q)             
        regs2:=regset
        lx:=genlabel(lab)

        if opc=kjumpf then
            lab2:=createfwdlabel()

            while s do
                if not s.simple then gerror("inset/cx2") fi
                bx:=evalunit(s,'A')
                s:=s.nextunit
                genmc(m_cmp, ax, bx)
                popregs(regs2)

                if s then
                    genmc_cond(m_jmpcc, eq_cond, genlabel(lab2))
                else
                    genmc_cond(m_jmpcc, ne_cond, lx)
                fi
            od
            definefwdlabel(lab2)
        else

            while s do
                bx:=evalunit(s,'A')
                s:=s.nextunit
                genmc(m_cmp, ax, bx)

                genmc_cond(m_jmpcc, eq_cond, lx)
            od
        fi

    when jcmpchain then
        r:=q.nextunit
        i:=1
        regs2:=regset
        if opc=kjumpf then
            while r, r:=r.nextunit do
                ax:=loadunit(q)
                bx:=evalunit(r,'A')
                genmc(m_cmp, ax, bx)
                popregs(regs2)
                genmc_cond(m_jmpcc, getmclcond(reversecond(p.cmpgenop[i]),q.mode), genlabel(lab))
                ++i
                q:=r
            od
        
        else
            lab2:=createfwdlabel()
            while r, r:=r.nextunit do
                ax:=loadunit(q)
                bx:=evalunit(r,'A')
                genmc(m_cmp, ax, bx)
                popregs(regs2)
                if r.nextunit then
                    genmc_cond(m_jmpcc, getmclcond(reversecond(p.cmpgenop[i]),q.mode), genlabel(lab2))
                else
                    genmc_cond(m_jmpcc, getmclcond(p.cmpgenop[i],q.mode), genlabel(lab))
                fi
                ++i
                q:=r
            od
            definefwdlabel(lab2)
        fi
    else            
        if p.mode not in [ti64,tu64,tbool64] then gerror_s("jumptrue/not i64:",strmode(p.mode)) fi
        ax:=loadunit(p)
        genmc(m_andx, ax, ax)
        genmc_cond(m_jmpcc, (opc=kjumpt|nz_cond|z_cond), genlabel(lab))
    endswitch

    popregs(regs)

end

proc gcomparejump(int jumpopc,int cond,unit lhs,rhs,int lab)=
    operand ax,bx
    int regs, rev, opc

    if jumpopc=kjumpf then          
        cond:=reversecond(cond)     
    fi

    regs:=regset
    rev:=(cond in [keq, kne] | 1 | 0)

GENCOMMENT("GCOMP")
    (ax, bx):=loadbin(lhs, rhs, isrev:rev, loadb:0)

    case ttcat[lhs.mode]
    when x64cat then opc:=m_comisd
    when x32cat then opc:=m_comiss
    else             opc:=m_cmp
    esac

    genmc(opc, ax, bx)

    popregs(regs)

    genmc_cond(m_jmpcc, getmclcond(cond,lhs.mode), genlabel(lab))
end

global function reversecond(int pclop)int=
    case pclop
    when keq then pclop:=kne
    when kne then pclop:=keq
    when klt then pclop:=kge
    when kle then pclop:=kgt
    when kge then pclop:=klt
    when kgt then pclop:=kle
    esac

    return pclop
end

proc setmultopnd(operand ax, rx) =
    if ax=nil then                      
        return
    fi

    if rx.reg=rnone then                
        rx^:=ax^                        
    elsif ax.reg<>rx.reg then           
        genmc(getopndmov(rx), rx, ax)   
    fi
end

func do_if(unit p, pcond, plist, pelse, int isref, reg=rnone)operand tx =
    int labend,lab2, regs:=regset
    operand rx

    labend:=createfwdlabel()

    rx:=newmclopnd()
    tx:=nil

    while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
        lab2:=createfwdlabel()

        docond(kjumpf,pcond,lab2)
        popregs(regs)

        tx:=loadunitx(plist,isref,reg)
        setmultopnd(tx,rx)
        popregs(regs,tx)

        if pcond.nextunit or pelse then
            genjumpl(labend)
        fi
        definefwdlabel(lab2)
    od

    if pelse then
        popregs(regs)
        tx:=loadunitx(pelse,isref,reg)
        setmultopnd(tx,rx)
        popregs(regs,tx)
    fi
    definefwdlabel(labend)

    rx
end

func do_andl(unit p,a,b, int reg)operand tx =
    operand bx
    int labfalse, labend, regs:=regset

    labfalse:=createfwdlabel()
    labend:=createfwdlabel()

    genjumpcond(kjumpf,a,labfalse)
    genjumpcond(kjumpf,b,labfalse)

    tx:=genreg(reg)
    genmc(m_mov, tx, genint(1))

    genjumpl(labend)

    definefwdlabel(labfalse)
    genmc(m_mov, tx, genint(0))

    definefwdlabel(labend)

    popregs(regs, tx)
    tx
end

func do_orl(unit p,a,b, int reg)operand tx =
    int labtrue, labfalse, labend, regs:=regset

    labtrue:=createfwdlabel()
    labfalse:=createfwdlabel()
    labend:=createfwdlabel()

    genjumpcond(kjumpt,a,labtrue)
    genjumpcond(kjumpf,b,labfalse)

    definefwdlabel(labtrue)

    tx:=genreg(reg)
    genmc(m_mov, tx, genint(1))
    genjumpl(labend)

    definefwdlabel(labfalse)
    genmc(m_mov, tx, genint(0))

    definefwdlabel(labend)

    popregs(regs, tx)
    tx
end

func do_notl(unit p,a, int reg)operand ax =

    if not ttisinteger[a.mode] and a.mode<>tbool64 then gerror("notl/not int") fi
    ax:=loadunit(a, reg)
    genmc(m_xorx, changeopndsize(ax,1), genint(1))
    ax
end

func do_istruel(unit p,a, int reg)operand ax =
    operand bx

    if not ttisinteger[a.mode] and a.mode<>tbool64 then gerror("istruel/not int") fi

    ax:=loadunit(a, reg)
    genmc(m_test, ax,ax)
    genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
    genmc(m_movzx, changeopndsize(ax,4),bx)

    ax
end

func do_ptr(unit p,a, int load=1,reg=rnone)operand tx=
    operand px
    unit poffset
    int regs:=regset, memmode:=getmemmode(p), size, scale, neg, amode

    if a.tag=jbin and a.pclop=kaddrefoff then
        neg:=1
doaddconst::
        amode:=tttarget[a.a.mode]
        px:=loadunit(a.a)
        poffset:=a.b
        size:=ttsize[amode]

        if poffset.tag=jconst then
            px:=genindex(areg:px.reg, offset:poffset.value*size*neg, size:size)
        else
            tx:=loadunit(poffset)
            scale:=scaleindex(tx, size)
            px:=genindex(areg:px.reg, ireg:tx.reg, scale:scale, size:size)
        fi
        px.size:=ttsize[memmode]

        finish
    elsif a.tag=jbin and a.pclop=ksubrefoff and a.b.tag=jconst then
        neg:=-1
        doaddconst

    fi

    px:=loadunit(a,(reg<xr0|reg|rnone))

IF PX.REG=RFRAME THEN GERROR("PTR: LOADINIT gives [DFRAME?]") fi
    px:=genireg(px.reg,ttsize[memmode])
    popregs(regs, px)

finish::
    tx:=getaddrmode(px, nil, ttsize[memmode], 0)

    tx:=loadmem(load, tx, reg, memmode)



    return tx
end

func do_index(unit parray, pindex, int load=1, reg=rnone)operand tx =
    int addoffset, scale, regs, memmode, size
    operand px, ax

    regs:=regset


    memmode:=tttarget[parray.mode]


    px:=evalunit(parray)

IF TTBASETYPE[PARRAY.MODE]=TSLICE THEN
    ax:=genreg()
    genmc(m_mov, ax, changeopndsize(px,8))
    px:=makeopndind(ax)
    popregs(regs,px)
FI



    setopndsize(px, ttsize[memmode])

    tx:=getaddrmode(px, pindex, ttsize[memmode], -ttlower[parray.mode]*ttsize[memmode])
    tx:=loadmem(load, tx, reg, memmode)

    popregs(regs, tx)
    tx
end

proc mulreg(operand rx, int x)=
    int n

    if x>1 then
        if n:=ispoweroftwo(x) then
            genmc(m_shl,rx, genint(n))
        else
            genmc(m_imul2,rx, genint(x))
        fi
    fi
end

func do_dot(unit pdot, int load=1, reg=rnone)operand tx =
    operand px
    int offset, regs:=regset,memmode:=getmemmode(pdot)
    unit a,pname

    px:=evalunit(pdot.a)

    setopndsize(px, ttsize[memmode])

IF PX.MODE<>A_MEM THEN
    GERROR("DOT: PX NOT MEM")
FI

    tx:=getaddrmode(px, nil, ttsize[memmode], pdot.offset)
    tx:=loadmem(load, tx, reg, memmode)
    popregs(regs,tx)
    tx
end

func getaddrmode(operand ax, unit pindex=nil, int size=1, offset=0)operand tx=

    operand ix, bx, rxa, rxb
    int mulfactor:=1, scale:=1

    if pindex and pindex.tag=jbin and pindex.pclop in [kadd, ksub] then
        if pindex.b.tag=jconst then     
            offset+:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)*size
            pindex:=pindex.a
        fi
    fi

    if pindex and pindex.tag=jconst then
        offset+:=pindex.value*size
        pindex:=nil
    fi

    if pindex then
        case size
        when 1,2,4,8 then
            scale:=size
        else                                
            mulfactor:=size
        esac
    fi


    if pindex then                  
        rxa:=rxb:=nil
        if not pindex.simple then       
            if ax.reg in r0..r13 then
                rxa:=genreg(ax.reg)
                genmc(m_push, rxa)
            fi
            if ax.regix then
                rxb:=genreg(ax.regix)
                genmc(m_push, rxb)
            fi
        fi
        ix:=loadunit(pindex)
        if rxb then genmc(m_pop, rxb) fi    
        if rxa then genmc(m_pop, rxa) fi

        if ax.regix=0 then          
addregix::
            ax.regix:=ix.reg
            if mulfactor<>1 then
                mulimm(ix, mulfactor)
            else
                ax.scale:=scale
            fi
            ax.offset+:=offset
        else
            bx:=genreg(getlowreg(ax))
            genmc(m_lea, bx, ax)
            ax:=genireg(Bx.reg)
            addregix

        fi

    else                            
        ax.offset+:=offset
    fi

    return ax
end

proc do_while(unit p,pcond,pbody,pincr) =
    int lab_b,lab_c,lab_d,lab_incr

    lab_b:=createfwdlabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    if pincr then
        lab_incr:=createfwdlabel()
    else
        lab_incr:=lab_c
    fi

    stacklooplabels(lab_b, lab_c, lab_d)

    genjumpl(lab_incr)      

    definefwdlabel(lab_b)

    loadunit(pbody)

    definefwdlabel(lab_c)

    if pincr then
        evalunit(pincr)
        definefwdlabel(lab_incr)
    fi

    docond(kjumpt,pcond,lab_b)
    definefwdlabel(lab_d)
    --loopindex
end

proc do_repeat(unit p,a,b) =
    int lab_ab, lab_c, lab_d

    lab_ab:=definelabel()
    lab_c:=createfwdlabel()
    lab_d:=createfwdlabel()

    stacklooplabels(lab_ab, lab_c, lab_d)

    loadunit(a)

    definefwdlabel(lab_c)

    unless b.tag=jconst and b.value=0 then
        docond(kjumpf,b,lab_ab)
    end

    definefwdlabel(lab_d)
    --loopindex
end

proc do_incr(unit p,a) =
    operand ax
    int opc, step

    ax:=evalunit(a,'W')
    if ax.mode<>a_mem then gerror("incr/not mem") fi

    if ttisref[a.mode] then
        step:=ttsize[tttarget[a.mode]]
    else
        step:=1
    fi

    do_incrstep(ax, step, p.pclop=kincr)
end

proc do_incrstep(operand ax, int step, isincr)=
    if step=1 then
        genmc((isincr|m_inc|m_dec), ax)
    else
        genmc((isincr|m_add|m_sub), ax, genint(step))
    fi
end

func do_incrload(unit p,a, int reg=rnone)operand ax=
    operand px
    int opc, step, regs:=regset

    ax:=genreg(reg)
    px:=evalunit(a,'W')

    if px.mode<>a_mem then gerror("incr/not mem") fi

    if ttisref[a.mode] then
        step:=ttsize[tttarget[a.mode]]
    else
        step:=1
    fi

    if ttcat[a.mode]=d64cat then
        opc:=m_mov
    else
        opc:=(ttsigned[a.mode]|m_movsx|m_movzx)
    fi

    case p.pclop
    when kincrload then
        do_incrstep(px, step, 1)
        genmc(opc, ax, px)  
    when kdecrload then
        do_incrstep(px, step, 0)
        genmc(opc, ax, px)  
    when kloadincr then
        genmc(opc, ax, px)  
        do_incrstep(px, step, 1)
    when kloaddecr then
        genmc(opc, ax, px)  
        do_incrstep(px, step, 0)
    esac

    popregs(regs, ax)
    ax
end

proc do_for(unit p,pindex,pfrom, pbody, int down) =

    unit pto, pstep, pelse, px, plimit, ptoinit, ptemp
    int lab_b,lab_c,lab_d,lab_e, lab_cmp
    int a,b,step, reg:=regset
    operand ax, bx

    pto:=pfrom.nextunit
    pstep:=pto.nextunit
    pelse:=pbody.nextunit
    ptoinit:=pindex.nextunit

    case pto.tag
    when jptr then
        px:=pto.a
        symbol d
        if px.tag=jname and (d:=px.def).nameid=paramid and
             d.parammode=out_param then
            gerror("Possibly using &param as for-loop limit")
        fi
    when jconst, jname then
    else
        if pto.mode=ti64 then
GERROR("FOR/BLOCKTEMP")
        else
            gerror("Complex TO")
        fi
    esac

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

    genmc(m_mov, genmem(pindex.def), loadunit(pfrom))
    popregs(reg)

    if ptoinit then         
        ptoinit.resultflag:=0
GENCOMMENT("PTOINIT:")
        loadunit(ptoinit)
    fi

    if pfrom.tag=jconst and pto.tag=jconst then
        a:=pfrom.value
        b:=pto.value
        if (down and a>=b) or (not down and a<=b) then  
        else                            
            genmc(m_jmp, genlabel(lab_e))
        fi
    else
        genmc(m_jmp, genlabel(lab_cmp))
    fi

    definefwdlabel(lab_b)

    loadunit(pbody)             

    definefwdlabel(lab_c)

    if pstep then
        if pstep.tag<>jconst then
            gerror("for/step non-const not ready")
        fi
        step:=pstep.value
        if step<=0 then
            gerror("Bad for-step")
        fi
    else
        step:=1
    fi

    do_incrstep(evalunit(pindex), step,  not down)

    definefwdlabel(lab_cmp)

    ax:=loadunit(pindex)
    bx:=loadunit(pto)
    genmc(m_cmp, ax, bx)
    genmc_cond(m_jmpcc, (down|ge_cond|le_cond), genlabel(lab_b))

    if pelse then
        definefwdlabel(lab_e)
        loadunit(pelse)
    fi

    definefwdlabel(lab_d)
    --loopindex

    popregs(reg)

end

proc do_forall(unit p,pindex,plist, pbody, int down) =

    unit plocal, pfrom, pto, pelse, px, plimit, passign
    int lab_b,lab_c,lab_d,lab_e, lab_cmp
    int a,b,stepx,regs:=regset
    operand ax, bx

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

    genmc(m_mov, genmem(pindex.def), loadunit(pfrom))
    popregs(regs)

    if pfrom.tag=jconst and pto.tag=jconst then
        a:=pfrom.value
        b:=pto.value
        if (down and a>=b) or (not down and a<=b) then  
        else                            
            genmc(m_jmp, genlabel(lab_e))
        fi
    else
        genmc(m_jmp, genlabel(lab_cmp))
    fi

    definefwdlabel(lab_b)

    passign.resultflag:=0
    loadunit(passign)

    loadunit(pbody)             

    definefwdlabel(lab_c)

    do_incrstep(evalunit(pindex), 1, not down)

    ax:=loadunit(pindex)
    bx:=loadunit(pto)
    genmc(m_cmp, ax, bx)
    genmc_cond(m_jmpcc, (down|ge_cond|le_cond), genlabel(lab_b))

    if pelse then
        definefwdlabel(lab_e)
        loadunit(pelse)
    fi

    definefwdlabel(lab_d)
    --loopindex

    popregs(regs)
end

proc do_print(unit p,a,b) =
    unit q,r,fmt
    int m, fn, needprintend, regs:=regset
    operand ax, bx

    if a then
        needprintend:=1
        if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
        case ttbasetype[tttarget[a.mode]]
        when tvoid then
            gensysproc(sf_print_startfile,a)
        when tc8 then
            gensysproc(sf_print_startstr,a)
        when tref then
            gensysproc(sf_print_startptr,a)
        else
            gerror("@dev?")
        esac
        popregs(regs)
    else
        needprintend:=1
        gensysproc(sf_print_startcon)
    fi

    q:=b

    case p.tag
    when jfprint,jfprintln then
        if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
            gerror("string expected")
        fi
        gensysproc(sf_print_setfmt, q)
        q:=p.c
    esac

    while q do
        case q.tag
        when jfmtitem then
            fmt:=q.b
            r:=q.a
            m:=r.mode
        when jnogap then
            gensysproc(sf_print_nogap)
            q:=q.nextunit
            next
        when jspace then
            gensysproc(sf_print_space)
            q:=q.nextunit
            next
        else
            fmt:=nil
            r:=q
            m:=q.mode
        esac

        switch ttbasetype[m]
        when ti64 then
            fn:=sf_print_i64
            if not fmt then fn:=sf_print_i64_nf fi
        when tu64 then
            fn:=sf_print_u64
        when tr32 then
            fn:=sf_print_r32
        when tr64 then
            fn:=sf_print_r64
        when tref then
            if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
                fn:=sf_print_str
                if not fmt then fn:=sf_print_str_nf fi
            else
                fn:=sf_print_ptr
                if not fmt then fn:=sf_print_ptr_nf fi
            fi
        when tbool then
            fn:=sf_print_bool
        when tarray then
            GERROR("PRINTARRAY")
            q:=q.nextunit
        when trecord then
            GERROR("PRINTRECORD")
        when tslice then
            if tttarget[m]=tc8 then
                fn:=sf_print_strsl
            else
                gerror("PRINTSLICE")
            fi

        when tc64 then
            fn:=sf_print_c8

        else
            gerror_s("PRINT/T=#",strmode(m))
        end switch

        case fn
        when sf_print_i64_nf, sf_print_str_nf, sf_print_ptr_nf then
            gensysproc(fn, r)
        else
            gensysproc(fn, r, (fmt|fmt|createconstunit(0,ti64)))
        esac
        popregs(regs)

        q:=q.nextunit
    od

    case p.tag
    when jprintln,jfprintln then
        gensysproc(sf_print_newline)
    esac
    if needprintend then
        gensysproc(sf_print_end)
    fi
end


func do_callproc(unit p,a,b,int isfn)operand tx =
    [maxparams]unit arglist
    [maxparams]byte argcomplex      
    int nargs, nhighargs, nlowargs, nmult,isptr, nvariadics, blockret, nret, size
    int highcomplex, j, stackbytes, alignbytes, oldstackdepth, regs
    symbol d,dblock
    symbol dtemp
    ref[]int32 pmult
    unit q
    operand cx

    isptr:=0
    ++ncalldepth
    oldstackdepth:=mstackdepth
    regs:=regset

    case a.tag
    when jname then
        d:=a.def

    when jptr then
        d:=ttnamedef[a.mode]
        isptr:=1
    else
        gerror("call/not ptr")
    esac

    nargs:=nhighargs:=nlowargs:=0
    stackbytes:=alignbytes:=0
    nvariadics:=0
    blockret:=0

    if ttisblock[p.mode] then
        blockret:=1

        dblock:=newblocktemp(p.mode)
        q:=createname(dblock)
        q.mode:=p.mode
        arglist[++nargs]:=q
    fi

    q:=b
    while q, q:=q.nextunit do
        if nargs>=maxparams then gerror("maxargs") fi
        arglist[++nargs]:=q
        if d.varparams and nargs>=d.varparams and nargs<=4 and nvariadics=0 then
            nvariadics:=nargs
        fi

    od

    nhighargs:=max(nargs-4, 0)
    nlowargs:=min(nargs, 4)

    if nhighargs.odd ixor mstackdepth.odd then
        alignbytes:=8
        pushstack(8)
        ++mstackdepth
    fi


    for i:=nargs downto 5 do
        pushunit(arglist[i])        
        popregs(regs)
    od

    highcomplex:=0
    for i to nlowargs do
        argcomplex[i]:=j:=(arglist[i].simple|0|2)       
        if j then highcomplex:=i fi
    od

    if highcomplex then argcomplex[highcomplex]:=1 fi   

    for i to nlowargs when argcomplex[i] do
        if i<>highcomplex then
            pushunit(arglist[i])
            popregs(regs)
        fi
    od

    if highcomplex then             
        loadarg(arglist[highcomplex], highcomplex,nvariadics)
        popregs(regs)
    fi

    for i to nlowargs when argcomplex[i]=0 do
        loadarg(arglist[i], i, nvariadics)
        popregs(regs)
    od

    for i:=nlowargs downto 1 when argcomplex[i]=2 do
        poptoarg(arglist[i].mode, i, nvariadics)
    od

    stackbytes:=nhighargs*8+alignbytes

    if mstackdepth then
        stackbytes+:=32
        pushstack(32)
        mstackdepth+:=4
    fi



    if not isptr then
        genmc(m_call, genmemaddr(d))
    else
        genmc(m_call, CX:=loadunit(a.a))


    fi

    if stackbytes then
        popstack(stackbytes)
    fi
    mstackdepth:=oldstackdepth

    --ncalldepth

    if not isfn then return nil fi



    popregs(regs)


    return getretopnd((p.memmode|p.memmode|p.mode))

end

proc do_stop(unit a)=
    symbol d
    int regs:=regset

    if a then
        loadarg(a, 1)
    else
        loadarg(createconstunit(0,ti64), 1)
    fi

    genmc(m_call, genextname("exit*"))
    popregs(regs)
end

proc do_do(unit a) =
    int lab_abc,lab_d

    lab_abc:=definelabel()
    lab_d:=createfwdlabel()

    stacklooplabels(lab_abc, lab_abc, lab_d)

    loadunit(a)

    genjumpl(lab_abc)
    definefwdlabel(lab_d)
    --loopindex
end

proc do_exit(unit p,int k) =
    int n,index

    index:=p.index
    if index=0 then index:=loopindex fi

    n:=findlooplabel(k,index)
    if n=0 then
        gerror("Bad exit/loop index",p)
    else
        genjumpl(n)
    fi
end

proc do_swap(unit a, b)=
    operand ax,bx, px, qx
    int regs:=regset, size

    px:=evalunit(a,'W')
    qx:=evalunit(b,'W')

    ax:=genreg()
    bx:=genreg()

    case ttcat[a.mode]
    when blockcat then
        GERROR("SWAP BLOCK")
    when shortcat then
        size:=ttsize[a.mode]
        genmc(m_movzx, ax, px)      
        genmc(m_movzx, bx, qx)
        genmc(m_mov, qx, changeopndsize(ax,size))
        genmc(m_mov, px, changeopndsize(bx,size))

    else
        genmc(m_mov, ax, px)
        genmc(m_mov, bx, qx)
        genmc(m_mov, qx, ax)
        genmc(m_mov, px, bx)
    esac

    popregs(regs)

end

proc saverdx=
    if inf_r11used then
        genmc(m_push, genreg(r11))
    fi
end

proc restorerdx=
    if inf_r11used then
        genmc(m_pop, genreg(r11))
    fi
end

func do_divrem(unit a,b, int issigned, isdiv)operand =
    int opc, n, shifts, swapreg
    operand ax,bx

    ax:=loadunit(a)

    if isdiv and b.tag=jconst then
        n:=b.value
        case n
        when 0 then
            merror("Divide by zero")
        when 1 then
            return ax
        else
            shifts:=ispoweroftwo(n)
            if shifts then
                genmc((issigned|m_sar|m_shr), ax, genint(shifts))
                return ax
            fi
        esac
    fi 

    bx:=loadunit(b)
    saverdx()
    swapreg:=fixdivopnds(ax,bx)

    if issigned then
        genmc(m_cqo)
        opc:=m_idiv
    else
        genmc(m_xorx, genreg(r11), genreg(r11))
        opc:=m_div
    fi

    genmc(opc, bx)

    if not isdiv then
        genmc(m_xchg, genreg(r0), genreg(r11))
    fi
    restorerdx()

    if swapreg then
        genmc(m_xchg, genreg(r0), genreg(swapreg))
    fi

    ax
end

func fixdivopnds(operand ax, bx)int=
    operand cx
    int rega,regb

    rega:=ax.reg
    regb:=bx.reg

    if rega=r0 then         
        return 0
    fi
    if regb=r0 then         
        genmc(m_xchg,ax, bx)
        return rega
    fi

    cx:=genreg(r0)
    genmc(m_xchg, cx, ax)

    if getregbit(r0)=0 then         
        return 0
    fi

    return rega             
end

func do_shift(unit a, b, int opc)operand=
    operand ax
    ax:=loadunit(a)

    if b.tag=jconst then
        genmc(opc, ax, genint(b.value))
    else
        if inf_r10used then merror("shift:cl in use") fi
        loadarg(b,1)
        genmc(opc,ax, genreg(r10,1))
    fi
    ax
end

func do_convert(unit p,a, int reg=rnone)operand bx=
    operand ax,cx
    [100]char str
    int oldmode:=p.convmode, newmode:=p.mode, regs:=regset
    int lab, lab2, mask

    ax:=bx:=loadunit(a)

    case p.pclop
    when kfloat then

        bx:=genxreg(reg,size:ttsize[newmode])

        case pr(oldmode, newmode)
        when pr(ti64, tr64), pr(tc64, tr64) then
            genmc(m_cvtsi2sd, bx, ax)

        when pr(ti64, tr32) then
            genmc(m_cvtsi2ss, bx, ax)

        when pr(tu64, tr64) then

            lab:=createfwdlabel()
            lab2:=createfwdlabel()
            genmc(m_cmp, ax, genint(0))
            genmc_cond(m_jmpcc, lt_cond, genlabel(lab))
            genmc(m_cvtsi2sd, bx, ax)
            genmc(m_jmp, genlabel(lab2))

            definefwdlabel(lab)
            if not labmask63 then
                labmask63:=++mlabelno
                laboffset64:=++mlabelno
            fi
            genmc(m_andx,ax, genlabelmem(labmask63))
            genmc(m_cvtsi2sd, bx, ax)
            genmc(m_addsd, bx, genlabelmem(laboffset64))
            definefwdlabel(lab2)

        else
            error
        esac

    when kfix then
        bx:=genreg(reg)
        genmc((oldmode=tr64|m_cvttsd2si|m_cvttss2si), bx, ax)

    when kfwiden then
        bx:=changeopndsize(ax,8)
        genmc(m_cvtss2sd, bx, ax)

    when kfnarrow then
        bx:=changeopndsize(ax,4)
        genmc(m_cvtsd2ss, bx, ax)

    when ktruncate then
        case ttsize[oldmode]            
        when 1 then mask:=255
        when 2 then mask:=65535
        when 4 then mask:=0xFFFF'FFFF
        esac

        genmc(m_andx, ax, genint(mask))

        genmc((ttsigned[oldmode]|m_movsx|m_movzx), ax, changeopndsize(ax,ttsize[oldmode]))

    when kistruel then
        case ttcat[p.mode]
        when d64cat then
            genmc(m_test, ax, ax)
            genmc_cond(m_setcc, ne_cond, cx:=changeopndsize(ax,1))
            genmc(m_movzx, changeopndsize(ax,4),cx)
        else
            error
        esac
    else
error::
        fprint @str,"#-># (#)",strmode(oldmode), strmode(newmode),pclnames[p.pclop]
        gerror_s("Convert ",str)
    esac

    popregs(regs,bx)
    bx
end

proc do_clear(unit p, a)=
    int regs:=regset
    operand px

    px:=makeopndind(loadref(a))

    clearblock(px, ttsize[a.mode])
    popregs(regs)
end

func do_case(unit p,pindex,pwhenthen,pelse, int loopsw,isref, reg=rnone)operand tx =
    const maxcase=256
    [maxcase]int labtable
    [maxcase]unit unittable
    int ncases, opc, regs, regs2
    operand ix, bx, rx

    int lab_abc, lab_d, labnextwhen, labstmtstart, labelse
    unit w,wt

    if pindex=nil then
        GERROR("EMPTY CASE NOT DONE")
    fi

    rx:=newmclopnd()
    tx:=nil
    regs:=regset

    if loopsw then
        lab_abc:=definelabel()      
        lab_d:=createfwdlabel() 
        stacklooplabels(lab_abc,lab_abc,lab_d)
    else
        lab_d:=createfwdlabel() 
    fi

    ix:=loadunit(pindex)

    if casedepth>=maxcasedepth then
        gerror("case nested too deeply")
    fi
    casestmt[++casedepth]:=p

    ncases:=0
    wt:=pwhenthen
    while wt, wt:=wt.nextunit do
        w:=wt.a
        if ncases>=maxcase then
            gerror("too many cases")
        fi
        labtable[++ncases]:=createfwdlabel()
        unittable[ncases]:=wt.b

        while w, w:=w.nextunit do
            regs2:=regset
            bx:=evalunit(w,'A')
            genmc(m_cmp, ix, bx)
            popregs(regs2)

            genmc_cond(m_jmpcc, eq_cond, genlabel(w.whenlabel:=labtable[ncases]))
        od
    od
    popregs(regs)

    labelse:=createfwdlabel()
    caseelse[casedepth]:=labelse
    genjumpl(labelse)

    for i:=1 to ncases do
        definefwdlabel(labtable[i])

        tx:=loadunitx(unittable[i],isref,reg)
        setmultopnd(tx,rx)
        popregs(regs)

        if loopsw then
            genjumpl(lab_abc)
        else
            genjumpl(lab_d)
        fi
    od

    definefwdlabel(labelse)

    if pelse then
        tx:=loadunitx(pelse,isref,reg)
        setmultopnd(tx,rx)
    fi

    if loopsw then
        genjumpl(lab_abc)
        definefwdlabel(lab_d)
        --loopindex
    else
        definefwdlabel(lab_d)
    fi

    --casedepth

    popregs(regs, tx)
    tx
end

func do_switch(unit p,pindex,pwhenthen,pelse, int loopsw,isref, reg=rnone)operand tx =
    const maxlabels = 1000
    int minlab,maxlab,n,iscomplex,i
    int lab_a,lab_b,lab_d, labjump, elselab, labstmt,ax,bx, regs
    [0..maxlabels]ref mclrec labels
    unit w,wt
    operand rx,ix

    minlab:=1000000
    maxlab:=-1000000        

    n:=0                    
    iscomplex:=0            

    rx:=newmclopnd()
    tx:=nil
    regs:=regset

    wt:=pwhenthen
    while wt, wt:=wt.nextunit do
        w:=wt.a
        while w, w:=w.nextunit do       
            case w.tag
            when jmakerange then
                ax:=w.a.value
                bx:=w.b.value
    dorange::
                for i:=ax to bx do
                    minlab := min(i,minlab)
                    maxlab := max(i,maxlab)
                od
            when jconst then        
                ax:=bx:=w.value
                goto dorange
            else
                gerror_s("Switch when2: not const: #",strexpr(w).strptr)
            esac
        od
    od

    n:=maxlab-minlab+1
    if n>maxlabels then
        gerror("Switch too big")
    fi

    if loopsw then
        lab_a:=definelabel()
        lab_d:=createfwdlabel()
        stacklooplabels(lab_a,lab_a,lab_d)
    else
        lab_d:=createfwdlabel()
    fi

    labjump:=createfwdlabel()
    elselab:=createfwdlabel()

    ix:=loadunit(pindex)
    if minlab then
        genmc(m_sub, ix, genint(minlab))
    fi
    genmc(m_cmp, ix, genint(maxlab-minlab+1))
    genmc_cond(m_jmpcc,geu_cond, genlabel(elselab))
    genmc(m_jmp, genindex(ireg:ix.reg, scale:8, labno:labjump))
    popregs(regs)

    definefwdlabel(labjump)

    for i:=minlab to maxlab do          
        genmc(m_dq, genlabel(elselab))
        labels[i]:=mccodex
    od



    wt:=pwhenthen
    while wt, wt:=wt.nextunit do
        labstmt:=definelabel()
        w:=wt.a
        while w, w:=w.nextunit do
            case w.tag
            when jmakerange then
                ax:=w.a.value
                bx:=w.b.value
            when jconst then
                    ax:=bx:=int(w.value)
            esac
            for i:=ax to bx do
                labels[i].a.labelno:=labstmt
            od
        od

        tx:=loadunitx(wt.b,isref,reg)
        setmultopnd(tx,rx)
        popregs(regs)

        genjumpl((loopsw|lab_a|lab_d))
    od

    definefwdlabel(elselab)
    if pelse then
        tx:=loadunitx(pelse,isref,reg)
        setmultopnd(tx,rx)
    fi

    if loopsw then
        genjumpl(lab_a)
        definefwdlabel(lab_d)
        --loopindex
    else
        definefwdlabel(lab_d)
    fi

    popregs(regs, tx)
    tx
end

proc do_binto(unit p,a,b)=
    [128]char str
    int cat:=ttcat[a.mode]
    int target:=tttarget[a.mode]
    int regs:=regset
    operand px, ax, bx

    case p.pclop
    when kaddto then
        case cat
        when d64cat,shortcat then
            do_bintomem(a,b, m_add)
        when x64cat then
            do_bintoreg(a,b, m_addsd)
        when x32cat then
            do_bintoreg(a,b, m_addss)
        else
            error
        esac

    when ksubto then
        case cat
        when d64cat,shortcat then
            do_bintomem(a,b, m_sub)
        when x64cat then
            do_bintoreg(a,b, m_subsd)
        when x32cat then
            do_bintoreg(a,b, m_subss)
        else
            error
        esac

    when kmulto then
        case cat
        when d64cat,shortcat then
            do_bintoreg(a,b, m_imul2)
        when x64cat then
            do_bintoreg(a,b, m_mulsd)
        when x32cat then
            do_bintoreg(a,b, m_mulss)
        else
            error
        esac

    when kdivto then
        case cat
        when d64cat,shortcat then
            doidiv
        when x64cat then
            do_bintoreg(a,b, m_divsd)
        when x32cat then
            do_bintoreg(a,b, m_divss)
        else
            error
        esac

    when kidivto then
doidiv::
        GERROR("IDIVTO")

    when kiremto then
        GERROR("IREMTO")

    when kiandto then
        do_bintomem(a,b, m_andx)
    when kiorto then
        do_bintomem(a,b, m_orx)
    when kixorto then
        do_bintomem(a,b, m_xorx)
    when kshlto then
        do_shiftnto(a,b, m_shl)

    when kshrto then
        do_shiftnto(a,b, (ttsigned[a.mode]|m_sar|m_shr))

    when kminto then
        case cat
        when d64cat, shortcat then
            domaxto_int(a, b, (ttsigned[a.mode]|le_cond|leu_cond))
        when x64cat then
            do_bintoreg(a,b, m_minsd)
        when x32cat then
            do_bintoreg(a,b, m_minss)
        else
            error
        esac

    when kmaxto then
        case cat
        when d64cat, shortcat then
            domaxto_int(a, b, (ttsigned[a.mode]|ge_cond|geu_cond))
        when x64cat then
            do_bintoreg(a,b, m_maxsd)
        when x32cat then
            do_bintoreg(a,b, m_maxss)
        else
            error
        esac


    when kaddrefoffto then
        do_addrefoffto(p,a,b, 1)
    when ksubrefoffto then
        do_addrefoffto(p,a,b, 0)

    else
error::
        fprint @str,"Binto:#:#",pclnames[p.pclop],strmode(p.mode)
        gerror(str)
    esac

    popregs(regset)
end

proc do_bintomem(unit a,b, int opc)=
    operand px, bx

    (px, bx):=loadbinto(a,b, loadb:1)

    if bx.size<>px.size then
        bx:=changeopndsize(bx, px.size)
    fi

    genmc(opc, px, bx)
end

proc do_bintoreg(unit a,b, int opc)=
    operand px, ax, bx

    (px, bx):=loadbinto(a,b, loadb:0)

    case ttcat[a.mode]
    when d64cat then
        ax:=genreg()
        genmc(m_mov, ax, px)
        genmc(opc, ax, bx)
        genmc(m_mov, px, ax)
    when x64cat then
        ax:=genxreg()
        genmc(m_movq, ax, px)
        genmc(opc, ax, bx)
        genmc(m_movq, px, ax)
    when x32cat then
        ax:=genxreg()
        genmc(m_movd, ax, px)
        genmc(opc, ax, bx)
        genmc(m_movd, px, ax)
    when shortcat then
        ax:=genreg()
        GERROR("DOBINTOREG/SHORT")
    esac
end

func do_return(unit a)operand tx=
    if a then
        tx:=loadunit(a,(ttisreal[a.mode]|xr0|r0))
    else
        tx:=nil
    fi
    genjumpl(retindex)
    tx
end

func do_returnmult(unit a)operand tx=
    operand ax
    unit q
    int regoffset:=0, regs:=regset


    q:=a
    while q, q:=q.nextunit do
        if not q.simple then
            gerror("Retmult:complex")
        fi
        regs:=regset
        ax:=loadunit(q, (ttisreal[q.mode]|xr0|r0)+regoffset)
        popregs(regs,ax)
        if q=a then tx:=ax fi

        ++regoffset
    od

    genjumpl(retindex)
    tx
end

proc do_addrefoffto(unit p,a,b, int isincr)=
    operand px, bx
    int size:=ttsize[tttarget[a.mode]]

    if b.tag=jconst then
        px:=evalunit(a,'W')
        do_incrstep(px, size*b.value, isincr)
        return
    fi

    (px, bx) := loadbinto(a,b, loadb:1)
    mulreg(bx, size)

    genmc((isincr|m_add|m_sub), px, bx)
end

proc domaxto_int(unit a, b, int cond)=
    operand px, ax, bx, lx
    int lab

    (px, bx) := loadbinto(a,b, loadb:1)

    ax:=genreg()
    genmc(m_mov, ax, px)
    genmc(m_cmp, ax, bx)

    lab:=++mlabelno

    genmc_cond(m_jmpcc, cond, lx:=genlabel(lab))
    genmc(m_mov, px, changeopndsize(bx, px.size))
    genmc(m_labelx, lx)
end

func do_setcc(unit p,a,b, int reg=rnone)operand=
    int cond, isfloat, regs:=regset
    operand ax,bx,cx

    (ax, bx):=loadbin(a, b)
    isfloat:=0
    cond:=getmclcond(p.pclop, a.mode)

    case ttcat[a.mode]
    when d64cat then
        genmc(m_cmp,ax,bx)

    when x32cat then
        isfloat:=1
        genmc(m_comiss,ax,bx)

    when x64cat then
        genmc(m_comisd,ax,bx)
        isfloat:=1

    else
        gerror_t("setcc:",a)
    esac

    if isfloat then
        cx:=genreg(reg,1)
    else
        cx:=changeopndsize(ax,1)
    fi

    genmc_cond(m_setcc,cond, cx)
    genmc(m_movzx, ax:=changeopndsize(cx,8), cx)

    popregs(regs, ax)
    ax
end

func do_read(unit p,a)operand ax=
    int m, regs:=regset, opc

    m:=p.mode

    if a=nil then
        a:=createconstunit(0,ti64)
    fi

    if ttisinteger[m] then
        opc:=sf_read_i64

    elsif ttisreal[m] and ttsize[m]=8 then
        opc:=sf_read_r64

    elsif m=trefchar then
        opc:=sf_read_str

    else
        gerror_t("Read:", p)
    fi

    ax:=gensysfn(opc, a)

    popregs(regs,ax)
    ax
end

proc do_readln(unit a) =
    if a then
        if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

        case ttbasetype[tttarget[a.mode]]
        when tvoid then
            gensysproc(sf_read_fileline, a)
        when tu8,tc8 then
            gensysproc(sf_read_strline, a)
        else
            gerror("rd@dev?")
        esac
    else
        gensysproc(sf_read_conline)
    fi
end

func do_syscall(unit p,a, int reg)operand ax=
    int lab

    if lab_funcnametable=0 then
        lab_funcnametable:=++mlabelno
        lab_funcaddrtable:=++mlabelno
        lab_funcnprocs:=++mlabelno
    fi


    case p.fnindex
    when sf_getnprocs then
        ax:=genreg(reg)
        genmc(m_mov, ax, genlabelmem(lab_funcnprocs))
    when sf_getprocname then
        lab:=lab_funcnametable
dogetproc::
        ax:=loadunit(a, reg)
        genmc(m_mov, ax, genindex(ireg:ax.reg,scale:8,offset:-8,labno:lab))

    when sf_getprocaddr then
        lab:=lab_funcaddrtable
        dogetproc

    else
        gerror_s("syscall:",sysfnnames[p.fnindex])
    esac
    ax
end

func do_dotindex(unit p,a,b, int reg)operand ax =
    operand bx
    int regs:=regset, i

    (ax, bx):=loadbin(a,b, loadb:0, reg:reg)

    if b.tag<>jconst then
        gerror("dotix i not imm")
    fi

    i:=b.value

    if i then
        genmc(m_shr, ax, genint(i))
    fi
    genmc(m_andx, changeopndsize(ax,4), genint(1))

    popregs(regs,ax)

    ax
end

func do_dotslice(unit p,a,b, int reg)operand ax =
    operand bx, mx
    int regs:=regset, i, j
    u64 mask

    unless b.a.tag=b.b.tag=jconst then
        gerror("dotsl not const i..j")
    end

    i:=b.a.value
    j:=b.b.value

    ax:=loadunit(a,reg)

    if i then
        genmc(m_shr, ax, genint(i))
    fi

    mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

    if mask<=word(int32.maxvalue) then          
        genmc(m_andx, ax, genint(mask))
    else
        genmc(m_mov, mx:=genreg(), genint(mask))
        genmc(m_andx, ax, mx)
    fi

    popregs(regs,ax)
    ax
end

func do_typepun(unit p, a, int reg)operand bx=
    [100]char str
    operand ax,ix
    int oldmode:=a.mode, newmode:=p.convmode, regs:=regset, size

    ax:=loadunit(a)
    bx:=ax
    size:=ttsize[newmode]
    IF SIZE<>TTSIZE[OLDMODE] THEN GERROR("TYPEPUN SIZE?") FI

    if ttisreal[oldmode] and not ttisreal[newmode] then     
        bx:=genreg(reg,size)
        if size=4 then
            genmc(m_movd, bx, ax)
            ix:=changeopndsize(bx,8)
            if ttsigned[newmode] then
                genmc(m_movsx, ix, bx)
            fi
            bx:=ix

        else
            genmc(m_movq, bx, ax)
        fi

    elsif not ttisreal[oldmode] and ttisreal[newmode] then
        bx:=genxreg(reg,size)
        genmc((size=4|m_movd|m_movq), bx, ax)
    fi

    popregs(regs, bx)
    bx
end

proc do_shiftnto(unit a,b, int opc)=
    operand px, bx, cx
    int regs:=regset

    px:=evalunit(a,'W')
    if b.tag=jconst then
        genmc(opc, px, genint(b.value))
    else
        cx:=genreg(r10)
        genmc(m_push,cx)
        bx:=loadunit(b,r10)
        genmc(opc, px, changeopndsize(bx,1))
        genmc(m_pop,cx)
    fi
    popregs(regs)
end

func do_select(unit p,a,b,c, int isref, reg)operand tx =
    operand rx, ix
    const maxlabels=256
    [maxlabels]ref mclrec labels
    int labend,labjump,n,i,elselab,labstmt
    int regs
    unit q

    rx:=newmclopnd()
    tx:=nil
    regs:=regset

    q:=b
    n:=0
    while q, q:=q.nextunit do
        if n>=maxlabels then gerror("selectx: too many labels") fi
        ++n
    od

    labend:=createfwdlabel()
    labjump:=createfwdlabel()
    elselab:=createfwdlabel()

    ix:=loadunit(a)
    genmc(m_dec, ix)
    genmc(m_cmp, ix, genint(n))
    genmc_cond(m_jmpcc, geu_cond, genlabel(elselab))
    genmc(m_jmp, genindex(ireg:ix.reg, scale:8, labno:labjump))
    popregs(regs)

    definefwdlabel(labjump)

    for i to n do           
        genmc(m_dq, genlabel(elselab))
        labels[i]:=mccodex
    od

    q:=b
    i:=0
    while q, q:=q.nextunit do
        labstmt:=definelabel()
        ++i
        labels[i].a.labelno:=labstmt

        tx:=loadunitx(q,isref,reg)
        setmultopnd(tx,rx)
        popregs(regs)

        genjumpl(labend)
    od

    definefwdlabel(elselab)

    tx:=loadunitx(c,isref,reg)
    setmultopnd(tx,rx)

    definefwdlabel(labend)

    popregs(regs, tx)
    tx
end

func do_setccchain(unit p,q, int reg)operand tx =
    operand ax, bx
    int lab1,lab2,i, regs, regs2
    unit r

    lab1:=createfwdlabel()
    lab2:=createfwdlabel()

    r:=q.nextunit
    i:=1

    regs:=regset
    tx:=genreg(reg)
    regs2:=regs

    while r, r:=r.nextunit do
        ax:=loadunit(q)
        bx:=evalunit(r,'A')
        genmc(m_cmp, ax, bx)    
        popregs(regs2)

        genmc_cond(m_jmpcc, getmclcond(reversecond(p.cmpgenop[i]),q.mode), genlabel(lab1))
        ++i
        q:=r
    od

    genmc(m_mov, tx, genint(1))
    genmc(m_jmp, genlabel(lab2))

    definefwdlabel(lab1)
    genmc(m_mov, tx, genint(0))
    definefwdlabel(lab2)

    popregs(regs, tx)
    tx
end

func do_maths(unit a, int reg, ichar opname)operand ax=
    int regs:=regset

    loadarg(a, 1)

    do_callext(genextname(opname))
    popregs(regs)

    getretopnd(a.mode)
end

func do_maths2(unit a, b, int reg, ichar opname)operand ax=
    int regs:=regset

    if b.simple then
        loadarg(a, 1)
        loadarg(b, 2)
    elsif a.simple then
        loadarg(b, 2)
        loadarg(a, 1)
    else
        gerror("maths2/complex")
    fi

    do_callext(genextname(opname))
    popregs(regs)

    getretopnd(a.mode)
end

proc do_callext(operand px)=
    int pushedbytes:=0

    if mstackdepth then
        pushedbytes:=(mstackdepth.odd|40|32)
        pushstack(pushedbytes)
    fi

    genmc(m_call, px)

    popstack(pushedbytes)
end

func do_storedotindex(unit a,rhs, int resflag)operand=
    operand px, ax, bx
    unit pdot:=a.a, poffset:=a.b
    int i, x, regs:=regset


    unless a.simple and rhs.simple then
        gerror("a.[i]:= not simple")
    end
    if poffset.tag<>jconst then gerror("sdotix") fi
    i:=poffset.value

    ax:=loadunit(pdot)

    genmc(m_andx, ax, genint(inot(1<<i)))

    if rhs.tag=jconst then
        x:=rhs.value
        if x then
            genmc(m_orx, ax, genint(1<<i))
        fi
    else
        bx:=loadunit(rhs)
        if i then
            genmc(m_shl, bx, genint(i))
        fi
        genmc(m_orx, ax, bx)
    fi

    px:=evalunit(pdot,'W')
    genmc(m_mov, px, changeopndsize(ax,px.size))

    if resflag then
        popregs(regs,ax)
        ax
    else
        popregs(regs)
        nil
    fi
end

func do_storedotslice(unit a,rhs, int resflag)operand =
    operand px, ax, bx, mx
    unit pdot:=a.a, pslice:=a.b
    int i, j, x, regs:=regset
    u64 mask


    unless a.simple and rhs.simple then
        gerror("a.[i..j]:= not simple")
    end

    unless pslice.a.tag=pslice.b.tag=jconst then
        gerror("dotsl not const i..j")
    end

    i:=pslice.a.value
    j:=pslice.b.value



    ax:=loadunit(pdot)
    bx:=loadunit(rhs)

    mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)
    genmc(m_mov, mx:=genreg(), genint(mask))
    if i then
        genmc(m_shl, bx, genint(i))
    fi
    genmc(m_andx, ax, mx)
    genmc(m_orx, ax, bx)

    px:=evalunit(pdot,'W')
    genmc(m_mov, px, changeopndsize(ax,px.size))


    if resflag then
        popregs(regs,ax)
        ax
    else
        popregs(regs)
        nil
    fi
end

func do_slice(unit p,a,b, int reg)operand ax=
    operand px, bx
    unit ra, rb
    symbol d
    int regs:=regset

    ax:=nil


    if b=nil then
        GERROR("WHOLE SLICE")

    else
        ra:=b.a
        rb:=b.b
        if ra.tag=rb.tag=jconst then
            genmc(m_mov, bx:=genreg(), genint(rb.value-ra.value+1))
        else
            (bx,ax):=loadbin(rb,ra)
            genmc(m_sub, bx, ax)
            genmc(m_inc, bx)
        fi

        if not a.simple then gerror("slice/complex") fi
        px:=do_index(a, ra, load:0)
        genmc(m_lea, ax:=genreg(), px)

        d:=newblocktemp(p.mode)
        genmc(m_mov, px:=genmem(d), ax)
        genmc(m_mov, applyoffset(px,8), bx)
        popregs(regs)
        if p.resultflag then
            genmc(m_lea, ax:=genreg(), genmem(d))
        else
            ax:=nil
        fi

        popregs(regs,ax)

    fi
    return ax
end

proc do_assignms(unit a,b)=
    unit p
    int nlhs,nrhs,i, regs:=regset
    symbol d
    operand px

    nlhs:=a.length

    case b.tag
    when jcallfn then
        loadunit(b)

        if b.a.tag<>jname then
            gerror("multassign from fn: not simple fn")
        fi
        d:=b.a.def
        nrhs:=d.nretvalues

        a:=a.a                  
    elsif ttbasetype[b.mode]=tslice then
GERROR("DECONSTR SLICE NOT READY")
    else
        gerror("(a,b):=x; var only")
    esac

    i:=0
    while a, a:=a.nextunit do
        if not a.simple then gerror("ass/ms/complex") fi
        ++i
        px:=evalunit(a,'W')
        storemem(px, genreg(multregs[i]), a.mode, 0)
    od

    popregs(regs)
end

=== mm_decls.m 0 0 5/33 ===
global const maxmodule=200
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=1000

global type symbol = ref strec

global macro pr(a,b)    = (a<<16 ior b)

global record tokenrec =        
    byte symbol
    byte subcode
    word16 spare
    word32 pos: (sourceoffset:24, fileno:8)

    union
        ref strec symptr        
        int64 value             
        real xvalue             
        word64 uvalue           
        ichar svalue            
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
    symbol owner            
                            
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
    ref strec firstdupl         

    unit code           

    int32 mode
    byte namelen
    byte symbol
    byte nameid
    byte subcode

    union
        int32 index                 
        int32 labelno               
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

    struct              
        ichar truename          
        ref strec paramlist

        byte asmused            
        byte dllindex           
        byte fflang             

        byte nretvalues         
        byte varparams          
        byte isthreaded         
        int16 dummy1
    end

    struct              
        ref strec equivfield
        uflagsrec uflags
        int32 baseclass
        byte bitfieldwidth      
        byte align              
        byte bitoffset      
        byte equivoffset
    end

    struct              
        ref strec nextparam
        byte parammode          
        byte optional           
        byte variadic           
        byte dummy3             
    end

    int16 nrefs
    int16 regsize
    int16 maxalign      


    ref fwdrec fwdrefs  
    byte reftype        
    byte segment        

    int32 stindex       
    int16 importindex   

    ref strec nextsym
    int16 impindex
    int16 expindex
    byte reg

    byte scope
    byte equals         

end

global type unit   = ref unitrec

global record unitrec =
    byte tag                
    byte simple             
    array[2]byte spare
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

    union                       
        struct                  
            word32 slength
            byte isastring
        end

        struct                  
            byte dottedname     
            byte avcode         
        end

        union                   
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

        union                   
            word32 length       
            byte makearray      
        end
        byte addroffirst    

        word32 offset           
        int32 whenlabel         
        int32 swapvar           

        struct
            union
                int16 bitopindex    
                int16 opcindex      
                int16 fnindex       
                int16 condcode      
                int16 asmopcode     
                int16 bfcode
            end
        end
        int32 index
        [4]byte cmpgenop            
    end

    int32 mode
    int32 convmode      
    byte moduleno
    byte subprogno
    byte initlet        
    byte isconst        
    byte resultflag     
    byte pclop          
    byte istrueconst    
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
    ichar path          
    symbol ststart      
    symbol stmain
    symbol stmacro      
    unit modulecode     
    int16 fileno        
    int16 issyslib      
    int16 subprogno

end

global record subprogrec =
    ichar name
    symbol stsubprog
    int issyslib        
    ichar path          
    int16 firstmodule
    int fileno
end


global symbol stprogram     
global symbol stmodule      
global symbol stsubprog
global symbol stsysmodule   
global symbol alldeflist        
global int currmoduleno             

global tokenrec lx              
global tokenrec nextlx          

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]byte moduletosub               
global [0..maxsubprog]subprogrec subprogtable

global [0..maxlibfile]ichar libfiles
global [0..maxlibfile]byte libtypes

global [0..maxsourcefile]ichar sourcefilespecs      
global [0..maxsourcefile]ichar sourcefilepaths      
global [0..maxsourcefile]ichar sourcefilenames      
global [0..maxsourcefile]byte sourcefilesys         
global [0..maxsourcefile]byte sourcefilesupport     
global [0..maxsourcefile]ichar sourcefiletext       
global [0..maxsourcefile]ichar sourcefiledupl       
global [0..maxsourcefile]int sourcefilesizes
global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles
global int mainmoduleno


global const int maxtype=6'000

global int ntypes

global [0..maxtype]symbol       ttnamedef
global [0..maxtype]symbol       ttowner         

global [0..maxtype]int32        ttbasetype      
export [0..maxtype]ichar        ttname

global [0..maxtype]word32       ttsize
global [0..maxtype]byte         ttsizeset
global [0..maxtype]int32        ttlower         
global [0..maxtype]int32        ttlength        
global [0..maxtype]ref[]int32   ttmult          

global [0..maxtype]unit         ttdimexpr       

global [0..maxtype]int32        tttarget        
global [0..maxtype]byte         ttusercat
global [0..maxtype]int32        ttlineno

global [0..maxtype]byte         ttsigned        
global [0..maxtype]byte         ttisreal        
global [0..maxtype]byte         ttisinteger     
global [0..maxtype]byte         ttisshort       
global [0..maxtype]byte         ttisref         

global [0..maxtype]byte         ttcat           
global [0..maxtype]byte         ttisblock       

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

global ref procrec proclist,proclistx           
global ref procrec staticlist,staticlistx       
global ref procrec constlist,constlistx     


global unit nullunit, voidvarunit

global int targetbits=64
global int targetsize=8

global [20]ichar docstrings
global int ndocstrings

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1       

global byte msyslevel=2     
global byte mvarlib=0       
global byte fvarnames=0     
global byte minos=0         

global byte freadma         
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
global byte fwindows
global byte flinux
global byte fx64
global byte fssonly
global byte fnofile

global byte dointlibs=1

global enumdata []ichar passnames =
    (header_pass,   $),
    (load_pass,     $),
    (parse_pass,    $),
    (fixup_pass,    $),
    (name_pass,     $),
    (type_pass,     $),
    (pcl_pass,      $),
    (mcl_pass,      $),     
    (asm_pass,      $),     
    (objpass,       $),     
    (exe_pass,      $),     
    (lib_pass,      $),     
    (run_pass,      $),     
    (clang_pass,    $),
end

global int passlevel=0
global int prodmode=0
global int debugmode=0
global int libmode=0                    
global int mxstub=0                     

global ichar outfile                    
global ichar destfilename               
global ichar destfilepath               
global ichar asmfilename                
global ichar pclfilename                
global ichar exefilename                
global ichar libfilename                
global ichar objfilename                
global ichar mafilename                 
global ichar expfilename                

global symbol extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable

global int nunits

GLOBAL INT NSTRECS


global const langnameuc     = "M"
global const langname       = "m"
global const langext        = "m"
global const langextma      = "ma"
global const langextmauc    = "MA"
global const langlibname    = "mlib"
global const langhomedir    = "C:/mx/"
global const langhelpfile   = "mm_help.txt"

=== mm_diags_dummy.m 0 0 6/33 ===
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_lib

int currlineno



global proc printmodelist(filehandle f)=        
end

global proc printst(filehandle f,ref strec p,int level=0)=  
end

proc printstrec(filehandle f,ref strec p,int level)=        
end

global proc printstflat(filehandle f)=
end

global proc printcode(filehandle f,ichar caption)=
end

global proc printmodulecode(filehandle f,ref strec m)=
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=       
    PRINTLN @DEV,"<printunit>"
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=      
end

global proc printoverloads(filehandle f)=
end

global proc showprojectinfo(filehandle dev)=
end

global proc showlogfile=
CPL "NO DIAGS MODULE"
end

global proc showast(ichar filename)=
end


global proc showopndstack=
end


global function stropndstack(int indent=0)ichar=
nil
end

global proc printsymbol(ref tokenrec lp)=
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

    wxstr("    mlang ")
    wxstr((d.mode=tvoid|"proc     "|"function "))
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
=== mm_lex.m 0 0 8/33 ===
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

const cr    = 13
const lf    = 10
const tab   = 9

ref char lxsource
ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix          

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
    int c,hsum,commentseen,hashindex,length
    ref char pstart,pnext,p,ss,lxsvalue


    nextlx.subcode:=0

    doswitch lxstart:=lxsptr; lxsptr++^
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

        lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))
        return

    when 'A'..'Z' then
        lxsvalue:=lxsptr-1
        lxsvalue^+:=32
        goto doname

    when '0'..'9' then
        lxstart:=lxsptr-1
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
        nextlx.symbol:=eolsym
        return

    when '#' then           
            docomment

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

    when '\\' then          

        commentseen:=0
        doswitch lxsptr++^          
        when cr then
            ++lxalllines
            ++lxsptr                
            exit
        when lf then
            ++lxalllines
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

        doswitch lxsptr++^
        when cr then
            ++lxalllines
            ++lxsptr                
        when lf then
            ++lxalllines
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
LXERROR(".123 not done")
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
            return
        fi
        return

    when '-' then
        nextlx.symbol:=subsym
        case lxsptr^
        when '-' then
            ++lxsptr
            nextlx.symbol:=incrsym
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
                nextlx.symbol:=samesym
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
        ++lxsptr                
        ++lxalllines
        nextlx.symbol:=eolsym
        return
    when lf then            
        ++lxalllines
        nextlx.symbol:=eolsym
        return

    when 0 then
        if sourcelevel then
            unstacksource()
RETURN
        else
            nextlx.symbol:=eofsym
            --lxsptr
            return
        fi

    when 0xEF then          
        lxsptr+:=2

    else
        nextlx.symbol:=errorsym
        return

    end doswitch

end

global proc lex=
    int lena,lenb
    ref char p

    lx:=nextlx              
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
        if lx.symbol in [commasym, lsqsym, lbracksym] then 
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
        if not dolexdirective(nextlx.subcode) then      
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
    inithashtable()
end

global proc printstrn(ichar s, int length)=
    if length then
        print length:"v",s:".*"
    fi
end

proc readrawstring=
    ichar dest
    int c

    nextlx.symbol:=stringconstsym
    nextlx.svalue:=++lxsptr

    dest:=lxsptr                

    doswitch c:=lxsptr++^
    when '"' then
        if lxsptr^='"' then     
            dest++^:='"'
            ++lxsptr
        else            
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
    int wrapped, hashindex,INDEX,n
    symbol d
    int j

    j:=hashindex0 iand hstmask

    d:=hashtable[j]
    wrapped:=0

    do
        if d=nil then exit fi

        if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then   
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
    int j, wrapped, hashvalue

    j:=gethashvaluez(name) iand hstmask

    lx.symptr:=hashtable[j]
    wrapped:=0

    do
        if lx.symptr=nil then
            exit
        elsif eqstring(lx.symptr.name,name) then    
            println "Lex dupl name:",name
            stop 1 
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

    lx.symptr:=pcm_allocz(strec.bytes)
    hashtable[j]:=lx.symptr

    lx.symptr.name:=name                
    lx.symptr.namelen:=strlen(name)
    lx.symptr.symbol:=namesym           

    return 0
end

function gethashvaluez(ichar s)int=
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
    int i
    memset(&hashtable,0,hashtable.bytes)

    for i:=1 to stnames.len do
        lookupsys(stnames[i])

        lx.symptr.symbol:=stsymbols[i]

        case stsymbols[i]
        when unitnamesym, kheadersym then
            lx.symptr.index:=stsubcodes[i]
            lx.symptr.subcode:=stsymbols[i]
            lx.symptr.symbol:=namesym       
        else
            lx.symptr.subcode:=stsubcodes[i]
        esac
    od

end

global proc printhashtable=
    println "Hashtable:"

end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
    lookupsys(name)

    lx.symptr.symbol:=namesym
    lx.symptr.subcode:=symbol
    lx.symptr.index:=subcode

    lx.symptr.regsize:=regsize
end

function dolexdirective(int index)int=
    ref strec symptr
    ref char p
    ichar file
    int i,lastsymbol,cond,fileno,length
    [256]char str

    case index
    when includedir then
        lexreadtoken()
        if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
        file:=nextlx.svalue
        convlcstring(file)
        file:=addext(file,langext)      

        fileno:=getsupportfile(file, path:sourcefilepaths[lxfileno], issupport:1)
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

    lxsource:=lxsptr:=sourcefiletext[fileno]

    nextlx.pos:=0
    lxfileno:=fileno

    nextlx.symbol:=semisym
    nextlx.subcode:=0
end

proc start=
    for i:=0 to 255 do
        switch i
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
    if sourcelevel>0 then           
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

    length:=0
    hasescape:=0

    doswitch c:=lxsptr++^
    when '\\' then          
        c:=lxsptr^
        if c in 'A'..'Z' then c+:=' ' fi
        ++lxsptr
        hasescape:=1

        switch c
        when 'a','b','c','e','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
            ++length
        when 'w' then
            ++length
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

    if length=0 then
        nextlx.svalue:=""
        return
    elsif not hasescape then
        nextlx.svalue:=pcm_copyheapstringn(s,length)
        return
    fi


    nextlx.svalue:=t:=pcm_alloc(length+1)

    do
        switch c:=s++^
        when '\\' then          
            c:=s^
            if c>='A'  and c<='Z' then c+:=' ' fi
            ++s
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
                t++^:=cr
                c:=lf
            when 'x' then   
                c:=0
                to 2 do
                    case d:=s++^
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
                if s^=c then        
                    ++s
                else            
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
        return
    fi

    nextlx.symbol:=intconstsym
    nextlx.subcode:=setinttype(a)
    nextlx.value:=a
end

proc readreal=

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
                return
            else
                --lxsptr
                exit all
            end doswitch

        when '_','\'' then

        when 'l','L' then
LXERROR("8:MAKEDECIMAL NOT READY")
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
=== mm_lib.m 0 0 9/33 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0
const allcomplex=0          


strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global ichar framevarname           

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
    symbol p,q

    p:=newstrec()

    p.name:=symptr.name
    p.namelen:=symptr.namelen
    p.symbol:=namesym
    p.owner:=owner
    p.nameid:=id


    p.nextdupl:=symptr.nextdupl
    p.firstdupl:=symptr
    symptr.nextdupl:=p

    return p
end

global proc adddef(symbol owner,p)=
    symbol q

    if q:=p.nextdupl then
        if q.owner=owner then
            cpl q.name,"in",owner.name
            serror("Duplicate name")
        fi
    fi

    if owner.deflist=nil then           
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
    typenames[ntypenames].defa:=a       
    typenames[ntypenames].defb:=b       

    typenamepos[ntypenames].pos:=lx.pos

    return -ntypenames
end

global function createusertype(symbol stname)int=
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
    int k,m

    if typedefx=0 then      
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
    int atype,k,m

    atype:=tarray

    if typedefx=0 then      
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
    int k,m

    if typedefx=0 then      
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
    int k,m

    if typedefx=0 then      
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


    if typedefx=0 then      
        for k:=tlast to ntypes when ttisref[k] do
            if tttarget[k]=target then
                return k
            fi
    
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
    int m, mproc

    mproc:=createusertype(stproc)
    stproc.paramlist:=paramlist

    stproc.mode:=prettype
    ttbasetype[mproc]:=tproc

    if typedefx=0 then      
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
    symbol p
    [32]char str
    ichar name

    if id=frameid and owner.nameid<>procid then
        serror("Auto frame not in proc")
    fi

    if id=frameid then
        print @&.str,"av_",,++nextavindex
    else
        print @&.str,"sv_",,++nextsvindex
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
    ((ref word64(u))^:=0)       
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


global function convertstring(ichar s, t)int=
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
        CPL jtagnames[p.tag]
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

        if tn.defb=nil then         
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

    when tvoid then         
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
    unit e,wt
    int m,res

    if p=nil then return 0 fi

    m:=p.mode

    case p.tag
    when jreturn then           
        return 1
    when jstop then
        return 1
    when jif then
        e:=p.b
        while e, e:=e.nextunit do
            if not checkblockreturn(e) then return 0 fi
        od

        return checkblockreturn(p.c)        

    when jblock then
        e:=p.a
        if e then
            while e and e.nextunit do
                e:=e.nextunit
            od
            return checkblockreturn(e)
        fi

    when jcase, jswitch, jdocase, jdoswitch then
        wt:=p.b
        while wt do
            if not checkblockreturn(wt.b) then
                return 0
            fi

            wt:=wt.nextunit
        od

        return checkblockreturn(p.c)        

    when jassem then                        
        return 1
    esac

    if jisexpr[p.tag] and m<>tvoid then
        return 1                            
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
    if ulist=nil then       
        ulist:=ulistx:=p
    else
        ulistx.nextunit:=p
    fi
    ulistx:=p           
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
    when tu8,tu16,tu32 then ti64

    when tr32 then tr64

    when tc8 then tc64
    else
        m
    end switch
end

global proc writegsfile(ichar filename, ref strbuffer d)=
    filehandle f

    f:=fopen(filename,"w")
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
    symbol d
    unit a

    if p.tag<>jcallfn then txerror("multass/need multfn") fi
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
    return str
end

global function roundtoblock(int n,align)int=
    if n iand (align-1)=0 then return n fi
    return n+(align-(n iand (align-1)))
end

global function getbasename(ichar s)ichar t=
    t:=s+strlen(s)-1
    while t>s and (t-1)^<>'.' do
        --t
    od

    return t
end


=== mm_libsources.m 0 0 10/33 ===
const fsyslibs = 1

global tabledata []ichar syslibnames, []ichar libtext =
    ("msys.m",          strinclude "msys.m"),
    ("mlib.m",          strinclude "mlib.m"),
    ("mclib.m",         strinclude "mclib.m"),
    ("mwindows.m",      strinclude "mwindows.m"),
    ("mwindll.m",       strinclude "mwindll.m"),
end

[syslibnames.len]byte syslibfileno

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
            if fwritema then
                sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(libtext[i])
            fi
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
=== mm_modules.m 0 0 11/33 ===

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


    initheadervars()

    headermode:=1
    headerdir:=0

    moduletable[0].name:="PROGRAM"
    moduletable[0].fileno:=0

    stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
    moduletable[0].stmodule:=stprogram
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
    headervars[hv_windows]:="1"
    mainmodule:="1"

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
            loaderror("Duplicate module name: # (Line:#)",modulename,strint(getlineno(dirpos)))
        fi
    od

    for i to nsubprogs do
        if eqstring(subprogtable[i].name, modulename) then
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
        return
    fi


    path:=pm.path
    if path^=0 and pm.issyslib then
        path:=f"c:\mx\"
    fi

    pm.fileno:=getsupportfile(pm.name, langext, path, issyslib:pm.issyslib)
end

proc addsyslib=

    if msyslevel=0 then return fi

    for i to nsubprogs do
        if eqstring(subprogtable[i].name,langname+"libx") then return fi
    od

    issyslib:=1
    importpathx:=headervars[hv_devpath]
    altpathx:=""
    if msyslevel=1 then
        addsubprogram(langname+"libtemp",0)
        addmodule(langname+"systemp")

        return
    fi

    addsubprogram(langname+"libx",0)
    addmodule(langname+"sys")
    addmodule(langname+"lib")

    addmodule(langname+"clib")
    addmodule(langname+"windows")
    addmodule(langname+"windll")


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
    ichar s,t
    [100]char name
    [300]char newfilespec
    int sys,support

    freadma:=1

    if filespec then
        s:=cast(readfile(filespec))
        if s=nil then                           
            loaderror("Can't find MA file ##",filespec)
        fi
        strcpy(newfilespec,extractpath(filespec))
    else
        s:=builtinstr
        newfilespec[1]:=0
    fi


    s:=readfileline(s+3)
    readstr(name,'n')
    if not eqstring(name,langextma) then
        loaderror(langextmauc+": bad header")
    fi

    --s                 

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
    for i to nsourcefiles do
        (sourcefiletext[i]+sourcefilesizes[i])^:=0  
    od

    strcat(newfilespec, sourcefilenames[1])
    return pcm_copyheapstring(newfilespec)
end

=== mm_name.m 0 0 12/33 ===
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
        rx_unit(owner,b)        

    when jdot then
        resolvedot(owner,p)

    when jcallproc, jcallfn then
        oldtag:=p.tag

        if a.tag=jname then         
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
            when typeid then        
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
                if d.deflist then           
                    expandmacro(p,a,b)
                    b:=nil
                    useparams:=0
                else                        
                    expandmacro(p,a,nil)
                    useparams:=1
                fi

                rx_unit(owner,p)
                --macrolevels

                if useparams and p.tag not in [jcallproc, jcallfn] then
                    insertunit(p,oldtag)
                    p.b:=b                  
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



    int extcount, subprogno
    symbol p,q, powner,extdef,moddef
    [10]symbol ambiglist


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
            elsif p.scope then  

                if powner.subprogno=subprogno or        
                     p.scope=program_scope or
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
end

global proc resolvename(symbol owner, unit p)=

    symbol d,e
    unit q
    int moduleno, mode,islet

    d:=p.def
    moduleno:=p.moduleno

    if d.nameid<>nullid then            
        return
    fi

    e:=resolvetopname(owner,d,moduleno,allowmodname)


    if not e then
        islet:=0
        mode:=tvoid
        case p.avcode
        when 'I', 'T', 'S' then mode:=ti64; islet:=1
        when 'L','A' then mode:=tany
        esac

        if mode=tvoid then
ARRAY[300]CHAR STR
STRCPY(STR, D.NAME)
CONVUCSTRING(STR)
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

    if pdupl.nameid<>nullid then        
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
    int subprogno

    if pdupl.nameid<>nullid then        
        return pdupl
    fi
    pdupl:=pdupl.nextdupl
    subprogno:=d.subprogno

    while pdupl do
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
    e:=rhs.def              

    oldallowmod:=allowmodname
    allowmodname:=lhs.tag=jname
    rx_unit(owner,lhs)
    allowmodname:=oldallowmod


    case lhs.tag
    when jname then
        d:=lhs.def
        case d.nameid
        when moduleid, typeid, procid, typeid then

            e:=finddupl(d,e)
            if e then
                if d.nameid=moduleid then
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
                p.tag:=jname            
                p.a:=p.b:=nil
                p.def:=e
                case e.nameid
                when enumid then
                when constid then
                esac
            else
                rxerror_s("Can't resolve .#",p.b.def.name,p)
            fi

        when frameid, staticid, paramid then        
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
        unless e.nextdupl then
            rxerror_s("Not a field: #",e.name)
        end unless
    esac
end

proc fixmode(ref typenamerec p)=
    ref int32 pmode
    symbol a,d,e,f,owner
    int m,moduleno

    pmode:=p.pmode

    m:=-pmode^                  

    d:=owner:=p.owner
    while d.nameid<>moduleid do d:=d.owner od
    moduleno:=d.moduleno

    a:=p.defa
    d:=p.defb

    if a=nil and d then         
        e:=resolvetopname(owner,d,moduleno,0)

    elsif d=nil and a then      
        rxerror("Fixmode can't do typeof yet")
    else                        
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
    unit pnext
    pnext:=p.nextunit
    p^:=q^
    p.nextunit:=pnext
end

proc expandmacro(unit p, a, b)=
    symbol d,pm
    unit pnew
    int ignoreargs

    if macrolevels>10 then
        rxerror("Too many macro levels (recursive macro?)")
    fi

    d:=a.def



    pm:=d.paramlist
    nmacroparams:=0
    while pm do
        if nmacroparams>=maxmacroparams then
            rxerror("macro param overflow")
        fi
        macroparams[++nmacroparams]:=pm
        macroparamsgen[nmacroparams]:=pm.firstdupl

        pm:=pm.nextparam
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
        PRINTLN =NMACROARGS, NMACROPARAMS
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

proc duplfield(symbol owner,p,q)=

    if p.code then
        serror("DUPLFIELD")
    fi


    q.atfield:=p.atfield
    q.flags:=p.flags

    q.uflags:=p.uflags      
    storemode(owner,p.mode,q.mode)
end

proc do_baseclass(symbol p)=
    symbol d,e,newd,dbase
    int normalexit

    dbase:=ttnamedef[p.baseclass]
    d:=dbase.deflist

    while d do              
        e:=p.deflist

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
=== mm_parse.m 0 0 13/33 ===

int intabledata=0       
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
ref strec dretvar           

int try_level=0
int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack        
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

        when kprocsym,kfunctionsym then 
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

    unless voidvarunit then
        voidvarunit:=createunit0(jvoidvar)
    end unless

    try_level:=0
    currproc:=nil
    varattribs:=0

    intabledata:=0      
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
    if lx.symbol<>eqsym then
        serror("""="" expected")
    fi
end

function getcurrline:int=
    return lx.pos
end

function checkbegin(int fbrack)int=
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
    skipsemi()
    if closesym=rbracksym then
        checksymbol(closesym)
    else
        checkend(closesym,kwd,startline:startline)
    fi
    lex()
end

global proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=
    [100]char str

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

    if lx.subcode=0 then                    
        return
    fi

    unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
        strcpy(&.str,"Mismatched 'end'")
        goto error
    end unless
end

function readvardef(ref strec owner,int scope=0,isstatic=0,varid=staticid, k)unit=


    unit ulist,ulistx, p
    int nvars,m, initcode
    ref strec stname

    ulist:=ulistx:=nil

    if istypestarter() then
        m:=readtypespec(owner)
    elsif k then
        m:=tauto
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
            initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 esac
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


    unit ulist,ulistx, p,q,r, plower
    int oldirp,length, usecomma

    lex()                   
    ulist:=ulistx:=nil
    plower:=nil
    length:=0

    if lx.symbol=atsym then         
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

    elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then  
        p:=createunit0(joperator)
        p.opcindex:=lx.subcode
        lex()
        lex()
        return p
    elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then  
        p:=createunit0(joperator)
        p.pclop:=symbolgentoops[lx.symbol]
        lex()           
        lexchecksymbol(rbracksym)
        lex()
        return p
    fi

    case lx.symbol
    when rbracksym then         
        lex()
        p:=createunit0(jmakelist)
        p.b:=plower
        p.length:=0
        return p
    else                    
        p:=readunit()
    esac

    case lx.symbol
    when rbracksym then         
        lex()

        return p

    when commasym then          
        usecomma:=1
        if nextlx.symbol=rbracksym then     
            lex()
            lex()
            p:=createunit1(jmakelist,p)
            p.length:=1
            p.b:=plower
            return p
        fi
docomma::                       
        length:=1

        ulist:=ulistx:=p

        if usecomma then
            repeat
                lex()                           
                if lx.symbol=rbracksym then     
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
                if lx.symbol=rbracksym then     
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

    when barsym then            
        lex()
        q:=readunit()
        case lx.symbol
        when barsym then        
            lex()
            r:=readsunit()
            checksymbol(rbracksym)
            lex()
            return createunit3(jif,fixcond(p),q,r)
        when rbracksym then
            lex()
            return createunit3(jif,fixcond(p),q,nil)

        esac

        addlistunit(ulist,ulistx,q) 
        checksymbol(commasym)
        if nextlx.symbol<>barsym then       
            repeat
                lex()               
                addlistunit(ulist,ulistx,readunit())
            until lx.symbol<>commasym
            checksymbol(barsym)
        else
            lex()                   
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
    if ulist^=nil then      
        ulist^:=ulistx^:=p
    else
        ulistx^.nextparam:=p
    fi
    ulistx^:=p          
end

function readcast:unit=
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
    when dotsym then            
                                
        if nextlx.symbol=ktypesym then
            lex()
            p:=createunit0(jtypeconst)
            p.value:=t
            p.mode:=ttype
            lex()
        else                    
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
    when addsym then            
        return readterm2()
    when subsym then            
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
        else        
            SERROR("READOPC/SINGLE OPND?")
            return createunit1(opc,p)

        fi
    else
        if symboloptypes[firstsym]=bin_op then
            serror("Can't be used as unary op")
        fi

    esac

    if lx.symbol=assignsym then 
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
        p:=createconstunit(int64@(pi),treal)
        lex()
        return p

    when jcvinfinity then
        p:=createconstunit(int64@(infinity),treal)
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
        fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

    when jcvtime then
        os_getsystime(&tm)
        fprint @&.str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

    when jcvtargetbits then
        lex()
        return createconstunit(targetbits,tint)
    when jcvtargetsize then
        lex()
        return createconstunit(targetsize,tint)
    when jcvtargetcode then
        strcpy(&.str,"wx64")

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

    ref strec d,e
    int t,kwd,fflang,sltype,w
    unit x,pupper,plx
    unit dim,length
    const maxdim=30
    [maxdim]unit dims
    int ndims,i,n,k

    case lx.symbol
    when lsqsym then        
arraybounds::
        lex()

        ndims:=0
        inreadprint:=1
        do
            length:=nil             
            if lx.symbol=rsqsym or lx.symbol=commasym then      
                dim:=nil
            else
                dim:=readunit()
                case lx.symbol
                when rsqsym,commasym then           
                when colonsym then              
                    lex()
                    if not (lx.symbol=commasym or lx.symbol=rsqsym) then    
                        length:=readunit()
                        dim:=createunit2(jkeyvalue,dim,length)
                    else                                                    
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

    when krefsym then       
        fflang:=0
    retry::

        lex()
        if lx.symbol=ktosym then lex() fi

        case lx.symbol
        when kprocsym,kfunctionsym then 
            t:=readrefproc(owner,typedefx,fflang)

        when kfflangsym then
            fflang:=lx.subcode
            goto retry
        elsif lx.symbol=stdtypesym then
            case lx.subcode
            when tc8 then
                t:=trefchar
                if typedefx then tttarget[typedefx]:=tc8 fi
            else
                goto readtarget
            esac

            lex()
        else                        
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

    if typedefx then            
        ttbasetype[typedefx]:=ttbasetype[t]
    fi

    return t
end

function readslicetype(ref strec owner, int slicetype, typedefx)int=
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
    unit ulist,ulistx
    int oldinparamlist

    ulist:=ulistx:=nil

    skipsemi()
    if lx.symbol=rbracksym then     
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

        if q.tag=jmakerange then        
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
        when ktypesym then          
            case p.tag
            when jtypeconst then            

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
    int64 x

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

    serror("Can't do complex expr")
    return 0
end

proc readprocdef(ref strec procowner,int scope,fflang=0)=
    int kwd,startline,closesym
    ref strec stproc,q,stname

    kwd:=lx.symbol
    nforloops:=0

    assemmode:=1
    stproc:=readprocdecl(procowner,scope,fflang)
    assemmode:=0
    checkequals()

    lex()

    startline:=getcurrline()

    if lx.symbol=semisym then
        closesym:=checkbegin(0)
    else
        closesym:=0
    fi

    pushproc(stproc)
    nextavindex:=0

    IF DRETVAR THEN
        stname:=getduplnameptr(stproc,dretvar,frameid)
        storemode(procowner,stproc.mode,stname.mode)
        adddef(stproc,stname)
    fi

    addtoproclist(stproc)

    if closesym then
        stproc.code:=readsunit()
        checkbeginend(closesym,kwd,startline)
    else
        stproc.code:=readunit()
        checksymbol(semisym)
        lex()
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

    int kwd,varparams,try_level, nparams, nretvalues, isthreaded
    [maxtuplesize]int retmodes
    int prettype@&retmodes

    ichar metadata, truename
    ref strec pequiv, stproc, owner, paramlist,nameptr

    kwd:=lx.symbol              
    isthreaded:=lx.subcode=2

    pequiv:=nil
    metadata:=""
    truename:=nil
    varparams:=0
    try_level:=0

    lex()

    if lx.symbol=stringconstsym then        
        truename:=pcm_copyheapstring(lx.svalue)
        convlcstring(lx.svalue)
        lx.symptr:=addnamestr(lx.svalue)
    else
        checksymbol(namesym)
    fi

    nameptr:=lx.symptr

    stproc:=getduplnameptr(procowner,nameptr,(insidedllimport|dllprocid|procid))
    if insidedllimport then scope:=subprog_scope fi
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
    if lx.symbol=lbracksym then     
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

    unless nretvalues or (kwd<>kfunctionsym) then       
        serror("Function needs ret type")
    end unless

    if nretvalues and (kwd<>kfunctionsym) then      
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

    if lx.symbol=atsym then         
        lexchecksymbol(namesym)
    SERROR("READPROCDEF @")
        lex()
        stproc.atvar:=1
    fi

    stproc.code:=nil

    case fflang
    when clangff,windowsff then
    else            
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
        elsif stproc.namelen=4 and eqstring(stproc.name,"main") then
            moduletable[stmodule.moduleno].stmain:=stproc
            if stmodule.moduleno=mainmoduleno then
                stproc.scope:=export_scope
            fi
        fi
    fi

    popproc()

    return stproc
end

function readparams(ref strec procowner,owner,int fflang,&varparams,&nparams)ref strec=         
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

    do                                      
        parammode:=var_param
        isoptional:=0

        if types or istypestarter() then                
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
    int pos1, kwd, pos2
    unit clist,clistx, plist,plistx, pelse, p, pelsif

    pos1:=lx.pos
    kwd:=lx.symbol          

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
    when kelsesym then      
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

    if lx.subcode=1 then        
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
    else            
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
    kwd:=lx.symbol          

    opc:=lx.subcode         

    lex()

    skipsemi()
    if lx.symbol=kwhensym then
        if kwd=kswitchsym then
            serror("switch expr missing")
        fi
        pexpr:=nil
    else
        pexpr:=readsunit()      
    fi

    pwhenlist:=pwhenlistx:=nil
    rangeused:=0
    nwhen:=0

    skipsemi()
    while lx.symbol=kwhensym do 
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
        fi
    fi

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

    return createunit1(jblock,readlist)
end

function readfor:unit=


    int pos, opc, kwd
    unit pindex, plocal             
    unit pfrom, pto, pstep, ptoinit 
    unit plist, passign             
    unit pcond, pbody, pelse
    unit p
    pos:=lx.pos
    lex()                       

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

    if lx.symbol in [insym, inrevsym] then              
        if lx.symbol=jinrev then
            opc:=jfordown               
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
                if pstep.value=1 then       
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


    if pcond<>nil then
        pbody:=makeblock(createunit2(jif,pcond,pbody))
    fi
    pbody.nextunit:=pelse



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

    else                                        

        if plocal=nil then                      
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

    t:=readtypespec(sttype,m)       

    sttype.scope:=scope
    storemode(owner,t,sttype.mode)

    if t>=0 then
        if ttisinteger[t]+ttisreal[t] then
            tttarget[m]:=t
        elsif ttisref[t] then
        elsecase ttbasetype[t]
        when tarray then
        when tslice then
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
        unionlastvar:=stname            

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
        when colonsym then              
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

    enums:=lx.subcode               
    lex()
    tabledataname:=nil

    if lx.symbol=lbracksym then     
        if not enums then serror("use 'enumdata'") fi
        enums:=1
        lex()
        checksymbol(rbracksym)
        lex()
    fi

    nextenumvalue:=1
    nrows:=0            
    ncols:=0            

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

    lex()                   

    skipsemi()
    startline:=getcurrline()
    closesym:=checkbegin(0)

    skipsemi()
    firstval:=lastval:=0

    for i:=1 to ncols do
        plist[i]:=plistx[i]:=nil
    od

    intabledata:=1
    do          
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
            stgen:=lx.symptr                
            tabledataname:=stgen.name       
            lex()
            if lx.symbol=eqsym then
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
            if ncols then               
                checksymbol(commasym)       
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
        lex()                   
        if lx.symbol=closesym then exit fi      
    od

    intabledata:=0

    skipsemi()
    checkbeginend(closesym,ktabledatasym,startline)





    if nrows=0 then serror("No table data") fi


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
        else
            exit
        fi
    enddoswitch
end

proc readimportmodule(ref strec owner)=
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
    checkequals()
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
        stname:=getduplnameptr(stmodule,stname,dllmoduleid)
        adddef(stmodule,stname)
        
        addlib(stname.name, libtype)

    stname.dllindex:=nlibfiles
    fi

    startline:=getcurrline()
    closesym:=checkbegin(0)

    insidedllimport:=1

    readimportbody(owner)

    insidedllimport:=0

    checkbeginend(closesym,kimportmodulesym,startline)

end

proc readimportbody(ref strec owner)=
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
    int kwd,prettype,m,varparams,nparams
    [4]int retmodes
    ref strec paramlist,stproc
    int rettype2, rettype3, nretvalues
    ichar name

    kwd:=lx.symbol              
    
    lex()

    paramlist:=nil
    prettype:=tvoid
    nretvalues:=0

    name:=nextautotype()
    stproc:=getduplnameptr(stmodule,addnamestr(name),typeid)
    adddef(stmodule,stproc)
    retmodes[1]:=tvoid

    if kwd=kfunctionsym then
        if lx.symbol=lbracksym then     
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

        if nretvalues and kwd=kprocsym then     
            serror("Proc can't return value")
        fi
    else                    
        if lx.symbol=lbracksym then     
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
    int length,nkeyvalues,oldirp
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
        skipsemi()                      
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
    if lx.symbol=namesym then               
        case nextlx.symbol
        when namesym then                   
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

    if endsexpr[lx.symbol] then
        return pt
    fi

    if lx.symbol=assignsym then
        lex()
        p:=readterm2()
        if endsexpr[lx.symbol] then
            p:=createunit2(jassign, pt, p)
            p.pos:=pos
            return p
        fi
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
    n:=0                
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
        if nextlx.symbol=atsym then     
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
            p.a:=p.a.a          
        fi

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

    ref strec nameptr,stmacro, paramlist,paramlistx, stname

    lexchecksymbol(namesym)

    nameptr:=lx.symptr
    stmacro:=getduplnameptr(owner,nameptr,macroid)
    adddef(owner,stmacro)

    owner:=stmacro

    lex()

    paramlist:=paramlistx:=nil

    if lx.symbol=lbracksym then         
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
        lex()                       
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
                opc:=0
            fi
            readvardef(currproc,0,1,staticid,opc)

        when kprocsym,kfunctionsym then
            readprocdef(currproc,0)

        when stdtypesym,krefsym,kicharsym,ktypeofsym,kdictsym,kslicesym,lsqsym then
            if nextlx.symbol in [lbracksym, atsym, dotsym] then     
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
            while q do                              
                r:=q.nextunit                       
                q.nextunit:=nil
                addlistunit(ulist,ulistx,q)     
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

        when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,
                kelsecasesym,kelseswitchsym,kendsym then
            exit
        when namesym then
            case nextlx.symbol
            when dcolonsym then
                p:=createunit0(jlabeldef)
                stname:=getduplnameptr(currproc,lx.symptr,labelid)
                adddef(currproc,stname)
                p.def:=stname
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
        when kdosym then                
            if inwhile then
                exit
            fi
            goto doexec

        when semisym then

        when kstepsym then
            exit

        else                            
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

=== mm_support.m 0 0 14/33 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

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

    if fwritema then
        sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(s)
    fi

    sourcefilesizes[nsourcefiles]:=rfsize
    (s+rfsize)^:=0              
    return nsourcefiles
end

global function loadbuiltin(ichar shortfile, text)int=
    ichar s

    if nsourcefiles>maxsourcefile then
        loaderror("Too many source files")
    fi
    ++nsourcefiles

    sourcefilepaths[nsourcefiles]:=""
    sourcefilespecs[nsourcefiles]:=sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)
    sourcefilesys[nsourcefiles]:=1

    sourcefiletext[nsourcefiles]:=pcm_copyheapstring(text)
    if fwritema then
        sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(text)
    fi

    sourcefilesizes[nsourcefiles]:=strlen(text)
    return nsourcefiles
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

    if not issyslib then
        loaderror("Can't find bundled file: ##",filespec)
    fi

    return 0
end

global proc mcerror(ichar mess)=
    println "MC Error:",mess

    stop 1
end

global proc serror_gen(ichar mess)=

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
    CPL =ALINENO
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
    if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then   
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

    nfiles:=0

    for i to nsourcefiles do
        if sourcefilesys[i] and fwritema=1 then     
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
        fprintln @f,"# #",i,sourcefilenames[sflist[i]]
    od

    fclose(f)
    stop
end

=== mm_tables.m 0 0 15/33 ===
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
    (sf_print_newline,      $,  0,  0),
    (sf_print_end,          $,  0,  0),
    (sf_read_i64,           $,  0,  0),
    (sf_read_r64,           $,  0,  0),
    (sf_read_str,           $,  0,  0),
    (sf_read_fileline,      $,  0,  0),
    (sf_read_strline,       $,  0,  0),
    (sf_read_conline,       $,  0,  0),

    (sf_getnprocs,          $,  0,  1),     
    (sf_getprocname,        $,  0,  1),
    (sf_getprocaddr,        $,  0,  1),

    (sf_gettttable,         $,  0,  1),
    (sf_getsttable,         $,  0,  1),
    (sf_getfftable,         $,  0,  1),

    (sf_power_i64,          $,  0,  1),
    (sf_unimpl,             $,  0,  1),

end
global [sysfnnames.len]symbol sysfnhandlers


global int mlineno


global enumdata [0:]ichar jtagnames,
                   [0:]byte jsubs, [0:]byte jisexpr =



    (jnone=0,       $,  0,      0), 
    (jconst,        $,  0,      3), 
    (jnull,         $,  0,      3), 
    (jvoidvar,      $,  0,      3), 
    (jname,         $,  0,      3), 
    (jnamelv,       $,  0,      3), 
    (jblock,        $,  1,      0), 
    (jdecimal,      $,  0,      3), 
    (jassem,        $,  3,      0), 
    (jassemmacro,   $,  0,      0), 
    (jassemreg,     $,  0,      0), 
    (jassemxreg,    $,  0,      0), 
    (jassemmem,     $,  1,      0), 
    (jstrinclude,   $,  1,      0), 


    (jandl,         $,  2,      2), 
    (jorl,          $,  2,      2), 
    (jnotl,         $,  1,      1), 
    (jistruel,      $,  1,      1), 


    (jmakelist,     $,  2,      3), 
    (jmakerange,    $,  2,      3), 
    (jmakeset,      $,  1,      3), 
    (jmakedict,     $,  1,      3), 
    (jmakeslice,    $,  1,      3), 
    (jreturnmult,   $,  1,      3), 

    (jkeyword,      $,  1,      3), 
    (jkeyvalue,     $,  2,      3), 
    (jassign,       $,  2,      3), 
    (jassignmm,     $,  2,      3), 
    (jassignms,     $,  2,      3), 
    (jassignmdrem,  $,  2,      3), 
    (jcopy,         $,  2,      3), 
    (jcallfn,       $,  2,      3), 

    (jcmp,          $,  2,      2), 
    (jcmpchain,     $,  2,      1), 
    (jbin,          $,  2,      2), 
    (junary,        $,  2,      1), 
    (jbinto,        $,  2,      2), 
    (junaryto,      $,  1,      1), 
    (jincr,         $,  1,      3), 

    (jinrev,        $,  2,      2), 
    (jinrange,      $,  2,      2), 
    (jinset,        $,  2,      2), 
    (jclamp,        $,  3,      2), 

    (jstringz,      $,  0,      3), 

    (jindex,        $,  2,      3), 
    (jslice,        $,  2,      3), 
    (jdot,          $,  2,      3), 
    (jdotindex,     $,  2,      3), 
    (jdotslice,     $,  2,      3), 

    (jptr,          $,  1,      3), 
    (jaddrof,       $,  1,      3), 
    (jaddroffirst,  $,  1,      3), 
    (jconvert,      $,  1,      3), 
    (jshorten,      $,  1,      3), 
    (jautocast,     $,  1,      3), 
    (jtypepun,      $,  1,      3), 
    (jtypeconst,    $,  0,      3), 
    (joperator,     $,  0,      3), 
    (jupper,        $,  1,      3), 

    (jbitwidth,     $,  1,      1), 
    (jbytesize,     $,  1,      1), 
    (jtypeof,       $,  1,      3), 
    (jtypestr,      $,  0,      1), 
    (jbitfield,     $,  1,      3), 

    (jminvalue,     $,  1,      3), 
    (jmaxvalue,     $,  1,      3), 


    (jcvlineno,     $,  0,      3), 
    (jcvstrlineno,  $,  0,      3), 
    (jcvmodulename, $,  0,      3), 
    (jcvfilename,   $,  0,      3), 
    (jcvfunction,   $,  0,      3), 
    (jcvdate,       $,  0,      3), 
    (jcvtime,       $,  0,      3), 
    (jcvversion,    $,  0,      3), 
    (jcvtypename,   $,  0,      3), 
    (jcvtargetbits, $,  0,      3), 
    (jcvtargetsize, $,  0,      3), 
    (jcvtargetcode, $,  0,      3), 
    (jcvnil,        $,  0,      3), 
    (jcvpi,         $,  0,      3), 
    (jcvinfinity,   $,  0,      3), 
    (jcvtrue,       $,  0,      3), 
    (jcvfalse,      $,  0,      3), 

    (jwhenthen,     $,  2,      0), 
    (jfmtitem,      $,  2,      3), 
    (jnogap,        $,  0,      3), 
    (jspace,        $,  0,      3), 


    (jcallproc,     $,  2,      0), 
    (jreturn,       $,  1,      0), 
    (jsyscall,      $,  1,      3), 

    (jto,           $,  3,      0), 
    (jif,           $,  3,      3), 
    (jforup,        $,  3,      0), 
    (jfordown,      $,  3,      0), 
    (jforall,       $,  3,      0), 
    (jforallrev,    $,  3,      0), 
    (jwhile,        $,  3,      0), 
    (jrepeat,       $,  2,      0), 
    (jgoto,         $,  1,      0), 
    (jlabeldef,     $,  0,      0), 
    (jredo,         $,  0,      0), 
    (jnext,         $,  0,      0), 
    (jexit,         $,  0,      0), 
    (jdo,           $,  1,      0), 
    (jcase,         $,  3,      3), 
    (jdocase,       $,  3,      0), 
    (jswitch,       $,  3,      3), 
    (jdoswitch,     $,  3,      0), 
    (jswap,         $,  2,      0), 
    (jselect,       $,  3,      3), 
    (jrecase,       $,  1,      0), 

    (jprint,        $,  2,      0), 
    (jprintln,      $,  2,      0), 
    (jfprint,       $,  3,      0), 
    (jfprintln,     $,  3,      0), 
    (jsprint,       $,  2,      0), 
    (jsfprint,      $,  2,      0), 
    (jread,         $,  2,      0), 
    (jreadln,       $,  2,      0), 
    (jsread,        $,  2,      0), 
    (jsreadln,      $,  2,      0), 
    (jstop,         $,  1,      0), 
    (jeval,         $,  1,      3), 
    (jstack,        $,  1,      0), 
    (junstack,      $,  1,      0), 
    (jempty,        $,  1,      1), 

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

global enumdata []ichar symbolnames,
                    []byte symboloptypes,
                    []byte symbolgenops,
                    []byte symbolgentoops,
                    []byte symbolopprios,
                    []byte exprstarter =
    (errorsym,          $,          0,  0,  0,  0,  0),     
    (dotsym,            ".",        0,  0,  0,  0,  0),     
    (lexdotsym,         $,          0,  0,  0,  0,  0),     
    (anddotsym,         "&.",       0,  0,  0,  0,  1),     
    (commasym,          ",",        0,  0,  0,  0,  0),     
    (semisym,           ";",        0,  0,  0,  0,  0),     
    (colonsym,          ":",        0,  0,  0,  0,  0),     
    (dcolonsym,         "::",       0,  0,  0,  0,  0),     
    (assignsym,         ":=",       bin_op, 0,  0,  1,  0),     
    (deepcopysym,       "::=",      0,  0,  0,  1,  0),     
    (sendtosym,         "=>",       0,  0,  0,  0,  0),     
    (pipesym,           "->",       0,  0,  0,  0,  0),     
    (lbracksym,         "(",        0,  0,  0,  0,  1),     
    (rbracksym,         ")",        0,  0,  0,  0,  0),     
    (lsqsym,            "[",        0,  0,  0,  0,  1),     
    (rsqsym,            "]",        0,  0,  0,  0,  0),     
    (lcurlysym,         "{",        0,  0,  0,  0,  0),     
    (rcurlysym,         "}",        0,  0,  0,  0,  0),     
    (ptrsym,            "^",        0,  0,  0,  0,  1),     
    (barsym,            "|",        0,  0,  0,  0,  0),     
    (dbarsym,           "||",       0,  0,  0,  0,  0),     
    (atsym,             "@",        0,  0,  0,  0,  0),     
    (datsym,            "@@",       0,  0,  0,  0,  0),     
    (questionsym,       "?",        0,  0,  0,  0,  0),     
    (addrsym,           "&",        0,  0,  0,  0,  1),     
    (daddrsym,          "&&",       0,  0,  0,  0,  0),     
    (curlsym,           "~",        0,  0,  0,  0,  0),     
    (rangesym,          "..",       bin_op, 0,  0,  5,  0),     
    (ellipsissym,       "...",      0,  0,  0,  0,  0),     
    (hashsym,           "#",        0,  0,  0,  0,  0),     


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
    (powersym,          "**",       bin_op,     kpower,     0,          2,  0),
    (samesym,           "==",       bin_op,     ksame,      0,          6,  0),
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
    (mathsopsym,        $,      0,  0,  0,  0,  1),     
    (maths2opsym,       $,      0,  0,  0,  0,  1),     

    (bitfieldsym,       $,      0,  0,  0,  0,  0),     
    (eolsym,            $,      0,  0,  0,  0,  0),     
    (eofsym,            $,      0,  0,  0,  0,  0),     
    (rawxnamesym,       $,      0,  0,  0,  0,  0),     
    (docstringsym,      $,      0,  0,  0,  0,  0),     
    (incrsym,           $,      0,  0,  0,  0,  1),     
    (intconstsym,       $,      0,  0,  0,  0,  1),     
    (decimalconstsym,   $,      0,  0,  0,  0,  1),     
    (realconstsym,      $,      0,  0,  0,  0,  1),     
    (charconstsym,      $,      0,  0,  0,  0,  1),     
    (wcharconstsym,     $,      0,  0,  0,  0,  1),     
    (stringconstsym,    $,      0,  0,  0,  0,  1),     
    (astringconstsym,   $,      0,  0,  0,  0,  1),     
    (wstringconstsym,   $,      0,  0,  0,  0,  1),     

    (unitnamesym,       $,      0,  0,  0,  0,  0),     
    (namesym,           $,      0,  0,  0,  0,  1),     
    (ksourcedirsym,     $,      0,  0,  0,  0,  0),     
    (kstrincludesym,    $,      0,  0,  0,  0,  1),     
    (regsym,            $,      0,  0,  0,  0,  0),     
    (xregsym,           $,      0,  0,  0,  0,  0),     
    (fregsym,           $,      0,  0,  0,  0,  0),     
    (mregsym,           $,      0,  0,  0,  0,  0),     
    (jmpccsym,          $,      0,  0,  0,  0,  0),     
    (setccsym,          $,      0,  0,  0,  0,  0),     
    (movccsym,          $,      0,  0,  0,  0,  0),     
    (segnamesym,        $,      0,  0,  0,  0,  0),     
    (asmopcodesym,      $,      0,  0,  0,  0,  0),     

    (stdtypesym,        $,      0,  0,  0,  0,  1),     
    (ktypeofsym,        $,      0,  0,  0,  0,  0),     
    (ksubrangesym,      $,      0,  0,  0,  0,  0),     
    (koutsym,           $,      0,  0,  0,  0,  0),     
    (kicharsym,         $,      0,  0,  0,  0,  1),     
    (kifsym,            $,      0,  0,  0,  0,  1),     
    (kthensym,          $,      0,  0,  0,  0,  0),     
    (kelsifsym,         $,      0,  0,  0,  0,  0),     
    (kelsesym,          $,      0,  0,  0,  0,  0),     
    (kelsecasesym,      $,      0,  0,  0,  0,  0),     
    (kelseswitchsym,    $,      0,  0,  0,  0,  0),     
    (kelseselectsym,    $,      0,  0,  0,  0,  0),     
    (kendsym,           $,      0,  0,  0,  0,  0),     
    (kunlesssym,        $,      0,  0,  0,  0,  0),     
    (kcasesym,          $,      0,  0,  0,  0,  1),     
    (kdocasesym,        $,      0,  0,  0,  0,  0),     
    (krecasesym,        $,      0,  0,  0,  0,  0),     
    (kwhensym,          $,      0,  0,  0,  0,  0),     
    (kforsym,           $,      0,  0,  0,  0,  0),     
    (ktosym,            $,      0,  0,  0,  0,  0),     
    (kbysym,            $,      0,  0,  0,  0,  0),     
    (kdosym,            $,      0,  0,  0,  0,  0),     
    (kwhilesym,         $,      0,  0,  0,  0,  0),     
    (krepeatsym,        $,      0,  0,  0,  0,  0),     
    (kuntilsym,         $,      0,  0,  0,  0,  0),     
    (kreturnsym,        $,      0,  0,  0,  0,  0),     
    (kstopsym,          $,      0,  0,  0,  0,  0),     
    (kloopsym,          $,      0,  0,  0,  0,  0),     
    (kstepsym,          $,      0,  0,  0,  0,  0),     
    (kgotosym,          $,      0,  0,  0,  0,  0),     
    (kswitchsym,        $,      0,  0,  0,  0,  0),     
    (kdoswitchsym,      $,      0,  0,  0,  0,  0),     
    (kprintsym,         $,      0,  0,  0,  0,  0),     
    (ksprintsym,        $,      0,  0,  0,  0,  0),     
    (kreadsym,          $,      0,  0,  0,  0,  0),     
    (ksreadsym,         $,      0,  0,  0,  0,  0),     
    (ksreadlnsym,       $,      0,  0,  0,  0,  0),     
    (kprocsym,          $,      0,  0,  0,  0,  0),     
    (kfunctionsym,      $,      0,  0,  0,  0,  0),     
    (klabelsym,         $,      0,  0,  0,  0,  0),     
    (krecordsym,        $,      0,  0,  0,  0,  0),     
    (kstructsym,        $,      0,  0,  0,  0,  0),     
    (kunionsym,         $,      0,  0,  0,  0,  0),     
    (kimportmodulesym,  $,      0,  0,  0,  0,  0),     
    (ktypesym,          $,      0,  0,  0,  0,  0),     
    (ktypealiassym,     $,      0,  0,  0,  0,  0),     
    (kextendtypesym,    $,      0,  0,  0,  0,  0),     
    (krefsym,           $,      0,  0,  0,  0,  1),     
    (kmutsym,           $,      0,  0,  0,  0,  0),     
    (kletsym,           $,      0,  0,  0,  0,  0),     
    (kslicesym,         $,      0,  0,  0,  0,  0),     
    (karraysym,         $,      0,  0,  0,  0,  0),     
    (kdictsym,          $,      0,  0,  0,  0,  0),     
    (kmacrosym,         $,      0,  0,  0,  0,  0),     
    (kexpandsym,        $,      0,  0,  0,  0,  0),     
    (koperatorsym,      $,      0,  0,  0,  0,  0),     
    (kconstsym,         $,      0,  0,  0,  0,  0),     
    (knewsym,           $,      0,  0,  0,  0,  0),     
    (kclearsym,         $,      0,  0,  0,  0,  0),     
    (kclasssym,         $,      0,  0,  0,  0,  0),     
    (kheadersym,        $,      0,  0,  0,  0,  0),     
    (kheadervarsym,     $,      0,  0,  0,  0,  0),     
    (kfflangsym,        $,      0,  0,  0,  0,  0),     
    (kglobalsym,        $,      0,  0,  0,  0,  0),     
    (kstaticsym,        $,      0,  0,  0,  0,  0),     

    (kcastsym,          $,      0,  0,  0,  0,  1),     
    (compilervarsym,    $,      0,  0,  0,  0,  1),     
    (dollarsym,         $,      0,  0,  0,  0,  1),     
    (kevalsym,          $,      0,  0,  0,  0,  0),     
    (ktabledatasym,     $,      0,  0,  0,  0,  0),     
    (kstacksym,         $,      0,  0,  0,  0,  0),     
    (kclampsym,         $,      0,  0,  0,  0,  1),         
    (kswapsym,          $,      0,  0,  0,  0,  0),     
    (kerrorsym,         $,      0,  0,  0,  0,  0),     
    (kassemsym,         $,      0,  0,  0,  0,  0),     
    (ksyscallsym,       $,      0,  0,  0,  0,  1),     
    (kemptysym,         $,      0,  0,  0,  0,  0),     
    (kcopysym,          $,      0,  0,  0,  0,  1),     

    (kdummysym,         $,      0,  0,  0,  0,  0),     
end


global enumdata [0:]ichar pclnames =
    (kzero=0,           $),

    (kadd,              $),
    (ksub,              $),
    (kmul,              $),
    (kdiv,              $),
    (kidiv,             $),
    (kirem,             $),
    (kidivrem,          $),
    (kiand,             $),
    (kior,              $),
    (kixor,             $),
    (kshl,              $),
    (kshr,              $),
    (kin,               $),
    (knotin,            $),
    (kmin,              $),
    (kmax,              $),
    (keq,               $),
    (kne,               $),
    (klt,               $),
    (kle,               $),
    (kge,               $),
    (kgt,               $),
    (ksame,             $),
    (kandl,             $),
    (korl,              $),
    (kaddrefoff,        $),
    (ksubrefoff,        $),
    (ksubref,           $),

    (kneg,              $),
    (kabs,              $),
    (kinot,             $),
    (knotl,             $),
    (kistruel,          $),
    (ksqr,              $),

    (ksqrt,             $),
    (ksin,              $),
    (kcos,              $),
    (ktan,              $),
    (kasin,             $),
    (kacos,             $),
    (katan,             $),
    (kln,               $),
    (klog,              $),
    (kexp,              $),
    (kround,            $),
    (kfloor,            $),
    (kceil,             $),
    (kfract,            $),
    (ksign,             $),
    (katan2,            $),
    (kpower,            $),
    (kfmod,             $),

    (kincr,             $),
    (kdecr,             $),
    (kincrload,         $),
    (kdecrload,         $),
    (kloadincr,         $),
    (kloaddecr,         $),

    (kaddto,            $),
    (ksubto,            $),
    (kmulto,            $),
    (kdivto,            $),
    (kidivto,           $),
    (kiremto,           $),
    (kiandto,           $),
    (kiorto,            $),
    (kixorto,           $),
    (kshlto,            $),
    (kshrto,            $),
    (kminto,            $),
    (kmaxto,            $),
    (kandlto,           $),
    (korlto,            $),
    (kaddrefoffto,      $),
    (ksubrefoffto,      $),

    (knegto,            $),
    (kabsto,            $),
    (kinotto,           $),
    (knotlto,           $),
    (kistruelto,        $),

    (ktypepun,          $),

    (ksoftconv,         $),

    (kfloat,            $),
    (kfix,              $),
    (ktruncate,         $),
    (kfwiden,           $),
    (kfnarrow,          $),

    (klen,              $),
    (klwb,              $),
    (kupb,              $),
    (kbounds,           $),
    (klenstr,           $),
    (kbitwidth,         $),
    (kbytesize,         $),
    (kminvalue,         $),
    (kmaxvalue,         $),
    (ktypestr,          $),
    (kerror,            $),
    (kharderror,        $),
    (karraytoslice,     $),
    (kichartoslice,     $),
    (ksofttruncshort,   $),
    (kcharaxtoichar,    $),
    (ksliceptr,         $),
end

[]byte complexops = (
    kpower, katan2, ksin, kcos, ktan, kasin, kacos, kln, klog, kexp,
    kround, kfloor, kceil, kfract, kfmod)
global [pclnames.lwb..pclnames.upb]byte complexopset

global enumdata []ichar sourcedirnames =
    (includedir,    $),
    (binincludedir, $),
end

global enumdata []ichar headerdirnames =
    (hdr_module,        $),
    (hdr_import,        $),
    (hdr_subprog,       $),
    (hdr_sysmodule,     $),
    (hdr_sysimport,     $),
    (hdr_syssubprog,    $),
    (hdr_minclude,      $),
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
    (noff=0,        $), 
    (windowsff,     $), 
    (clangff,       $), 
    (mlangff,       $), 
    (callbackff,    $), 
end

global enumdata [0:]ichar scopenames=
    (Module_scope=0,    "Local"), 
    (subprog_scope,     "Global"), 
    (program_scope,     "Program"), 
    (export_scope,      "Export"), 
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
    (nullid=0,      $),     
    (programid,     $),     
    (subprogid,     $),
    (moduleid,      $),     
    (dllmoduleid,   $),     
    (typeid,        $),     
    (procid,        $),     
    (dllprocid,     $),     
    (dllvarid,      $),     
    (genprocid,     $),     
    (constid,       $),     
    (staticid,      $),     
    (frameid,       $),     
    (paramid,       $),     
    (fieldid,       $),     
    (genfieldid,    $),     
    (enumid,        $),     
    (labelid,       $),     
    (macroid,       $),     
    (macroparamid,  $),     
    (linkid,        $),     
end


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
    ("sub",         kprocsym,       1),
    ("threadedproc",        kprocsym,       2),

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
    ("macro",       kmacrosym,      0),

    ("assem",       kassemsym,      1),
    ("asm",         kassemsym,      0),

    ("static",      kstaticsym,     0),
    
    ("const",       kconstsym,      0),

    ("$get_nprocs",     ksyscallsym,        sf_getnprocs),
    ("$getnprocs",      ksyscallsym,        sf_getnprocs),

    ("$get_procname",   ksyscallsym,        sf_getprocname),
    ("$getprocname",    ksyscallsym,        sf_getprocname),

    ("$get_procaddr",   ksyscallsym,        sf_getprocaddr),
    ("$getprocaddr",    ksyscallsym,        sf_getprocaddr),

    ("$gettttable",     ksyscallsym,        sf_gettttable),
    ("$getsttable",     ksyscallsym,        sf_getsttable),
    ("$getfftable",     ksyscallsym,        sf_getfftable),

    ("importdll",   kimportmodulesym,   'D'),
    ("importlib",   kimportmodulesym,   'L'),
    ("unless",      kunlesssym,         0),

    ("out",         koutsym,        0),


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


    ("slice",       kslicesym,      tslice),
    ("array",       karraysym,      0),
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
    ("$targetcode", compilervarsym, jcvtargetcode),
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

    ("sqr",         sqrsym,         0),
    ("sqrt",        sqrtsym,        0),
    ("sign",        signsym,        0),

    ("sin",         mathsopsym,     ksin),
    ("cos",         mathsopsym,     kcos),
    ("tan",         mathsopsym,     ktan),
    ("asin",        mathsopsym,     kasin),
    ("acos",        mathsopsym,     kacos),
    ("atan",        mathsopsym,     katan),
    ("ln",          mathsopsym,     kln),
    ("log",         mathsopsym,     klog),
    ("exp",         mathsopsym,     kexp),
    ("round",       mathsopsym,     kround),
    ("floor",       mathsopsym,     kfloor),
    ("ceil",        mathsopsym,     kceil),
    ("fract",       mathsopsym,     kfract),

    ("atan2",       maths2opsym,    katan2),
    ("fmod",        maths2opsym,    kfmod),

    ("sliceptr",    propsym,        ksliceptr),

    ("len",         propsym,    klen),
    ("lwb",         propsym,    klwb),
    ("upb",         propsym,    kupb),
    ("bounds",      propsym,    kbounds),
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
    ("$ctarget",    kheadervarsym,  hv_ctarget),
    ("$windows",    kheadervarsym,  hv_windows),
    ("$linux",      kheadervarsym,  hv_linux),
    ("$optim",      kheadervarsym,  hv_optim),
    ("$mainmodule", kheadervarsym,  hv_mainmodule),
    ("$a",          kheadervarsym,  hv_a),
    ("$b",          kheadervarsym,  hv_b),
    ("$c",          kheadervarsym,  hv_c),

    ("$$dummy",     0,              0)
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
        kicharsym, ktypeofsym, kslicesym, kdictsym, karraysym)

[]byte intresultlist = (
    kin, knotin, klwb, kupb, klen, klenstr, kbitwidth,
    kbytesize)

global [tc64..tr64, tc64..tr64]int16 softconvtable = (
    (ksoftconv, ksoftconv,  ksoftconv,  kfloat,     kfloat),        
    (ksoftconv, ksoftconv,  ksoftconv,  kfloat,     kfloat),        
    (ksoftconv, ksoftconv,  ksoftconv,  kfloat,     kfloat),        
    (kfix,      kfix,       kfix,       ksoftconv,  kfwiden),       
    (kfix,      kfix,       kfix,       kfnarrow,   ksoftconv))     

global [pclnames.lwb..pclnames.upb]byte intresult


global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
            kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
            semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag


global [jtagnames.lwb..jtagnames.upb]byte ismemtag

proc start=
    int genop, s,t, a, specop

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

    for i to complexops.len do complexopset[complexops[i]]:=1 od

end

=== mm_type.m 0 0 16/33 ===
const nolv=0
const needlv=1

const maxparams=100
const maxfields=200
int countedfields
int inassem
int inidata

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
    when jconst, jdecimal then

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
        tx_unitlist(p.c)

    when jforup, jfordown then
        tx_for(a,b,c)

    when jforall, jforallrev then
        tx_forall(a,b,c)

    when jto then
        tpass(a,ti64)
        tpass(b,tvoid)
        tpass(c,ti64)       

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

        tpass(a,tbool)
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
        when sf_getnprocs then restype:=ti64
        when sf_getprocname then paramtype:=ti64; restype:=trefchar;
        when sf_getprocaddr then paramtype:=ti64; restype:=tref 
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

    else
        CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
    doelse::

        for i to jsubs[p.tag] do
            tx_unitlist(p.abc[i],t)
        od
    endswitch

    tevaluate(p)

    case p.tag
    when jmakelist, jreturn then
    else
        if t<>tany and t<>tvoid and p.mode<>t then      
            coerceunit(p,t)         
        fi
    esac
    IF T=TVOID THEN
        CASE P.TAG
        WHEN JCONST, JBIN, jUNARY, JCMP THEN
        WHEN JNAME THEN
            unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
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

        symbol d:=currproc.deflist
        while d, d:=d.nextdef do
            if d.nameid=paramid then
                if ttisblock[d.mode] and d.parammode<>out_param then
                    d.parammode:=out_param
                    d.mode:=createrefmode(nil, d.mode)
                fi
            fi
        od
    od

    pp:=proclist
    while pp do
        currproc:=pp.def
        pcode:=currproc.code

        tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

        case ttbasetype[currproc.mode]
        when tvoid then     
        when ttuple then    
        else                
            if pcode.tag<>jreturn then
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
                goto error
            fi
        fi
        goto error

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
    PRINTUNIT(P)
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
    when constid,enumid then            

        if lv then txerror("&const") fi

        tx_namedconst(d)
        pcode:=d.code

        p.tag:=jconst
        p.def:=nil
        p.a:=nil
        p.c:=nil

        if pcode.tag=jconvert then      
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
            txerror_s("Can't use 'let' as lvalue: ",d.name)
        fi

        tx_namedef(d)

        if not inassem then
            p.mode:=d.mode
            if d.parammode=out_param then
                insertunit(p, jptr)
                p.mode:=tttarget[d.mode]
            fi
            twiden(p,lv)

        else
            p.mode:=trefchar
        fi

    when procid,dllprocid then

        p.mode:=trefproc    
                
                
                

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

cpl pclnames[p.pclop]
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
    when kaddto then                
        if abase=tref and bbase=tref then
            txerror("to:ref+ref")
        fi
        if abase=tref and bbase<=tlastnum then
            coerceunit(b,ti64)
            p.pclop:=kaddrefoffto
            return
        fi
    when ksubto then                
        if abase=tref and bbase<=tlastnum then
            coerceunit(b,ti64)
            p.pclop:=ksubrefoffto
            return
        fi
    when kshlto, kshrto then
        coerceunit(b,ti64)
        return
    esac

    if isnum(abase) and isnum(bbase) then   
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

        else                    
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

    p.mode:=d.mode              

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

        when jnull then         
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


    for i to nparams do
        q:=newarglist[i]            
        pm:=paramlist[i]            
        if q=nil then
            unless pm.optional then
                txerror_s("Param not optional: #",strint(i))
            end
            if pm.code then     
                newarglist[i]:=duplunit(pm.code,p.pos)
            else
                newarglist[i]:=createconstunit(0,ti64)
            fi
        fi
    od

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

            insertunit(q,jaddrof)
            setsimple(q)
            q.mode:=pm.mode
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

    when katan, kln, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos then
        coerceunit(a,tr64)
        resmode:=tr64

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
        if iscondtrue(pcond) then       
            deleteunit(p,plist)
        elsif iscondfalse(pcond) then   
            if pelse=nil then
                pelse:=createunit0(jblock)
            fi
            deleteunit(p,pelse)
        fi
    fi

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

    else                
        case p.pclop
        when kloadincr then p.pclop:=kincr
        when kloaddecr then p.pclop:=kdecr
        esac
        p.mode:=tvoid
    fi

    twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
    unit pto, pstep, plocal, plist
    int u

    pto:=pfrom.nextunit
    pstep:=pto.nextunit

    tpass(pindex)
    if pindex.tag<>jname then
        txerror("Loop index not a variable")
    fi
    u:=pindex.mode
    tpass(pindex.nextunit)

    tpass(pfrom,u)
    tpass(pto,u)


    tpass(pstep,u)

    tpass(pbody,tvoid)
    tpass(pbody.nextunit,tvoid) 
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
    tpass(pbody.nextunit,tvoid) 
end

proc tx_index(unit p,a,b,int t,lv) =
    int amode,emode,pmode,tmode,tbasemode

    tpass(a,,lv)
    deref(a,t<>tvoid)
    amode:=a.mode

    tpass(b,ti64)           

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
    fieldlist[nfields+1]:=nil           

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
        when 'E' then           
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
    case a.tag
    when jmakelist then
        tx_makelist(a,a.a,p.convmode,nolv)
    else
        tpass(a)
        coerceunit(a,p.convmode,hard)
    esac
    deleteunit(p,a)         
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

    tpass(p.b,ti64)             

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

    tpass(a)            
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

    if d.nameid=nullid then         
        d:=b.def:=resolvefield(d,recmode)
    fi

    if d.mode=tbitfield then
        i:=d.bitoffset
        j:=i+d.bitfieldwidth-1
        dequiv:=d.equivfield
        b.def:=dequiv               
        b.mode:=dequiv.mode
        p.offset:=d.offset

        if i=j then                 
            pindex:=createconstunit(i,ti64)
            newtag:=jdotindex
        else                        
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

    p.mode:=tbool
    setsimple(p)


end

proc convintconst(unit p,int64 x)=
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

    tmode:=createarraymodek(nil, tttarget[m], ttlower[m],0,0)


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
    while wt do             
        w:=wt.a
        while w do              
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
        tpass(wt.b,t,lv)            
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
            when trange then            
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
        while w do              
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
        q:=a.a              
        for i to nret do
            tpass(q,pmult[i])
            q:=q.nextunit
        od

        deleteunit(p,a)         
        p.resultflag:=1
        if t=tvoid then
            p.mode:=tvoid
        else
            p.mode:=ttuple
        fi

    else
        if nret>1 then txerror("RETERROR?") fi
        tpass(a,m)

        if t=tvoid then                 
            p.mode:=tvoid
        else
            deleteunit(p,a)
        fi
    fi

    IF TTISSHORT[P.MODE] THEN TXERROR("SHORT RET TYPE") FI
end

proc tx_dotindex(unit p,a,b,int lv) =
    int pmode
    unit i,j

    tpass(a,,lv)            

    pmode:=tu64

    if not ttisinteger[a.mode] then
        txerror("a.[i]: not int/str value")
    fi

    tpass(b)            

    case ttbasetype[b.mode]
    when trange then
        i:=b.a
        j:=b.b
        if i.tag=j.tag=jconst then
            if i.value>j.value then
                swap(b.a,b.b)
            fi
        fi
    else                    
        coerceunit(b,ti64)
    esac

    p.mode:=pmode
    setsimple(p)

end

proc tx_slice(unit p,a,b) =

    tpass(a)            
    tpass(b)            

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
            CPL =STRMODE(A.MODE)
            txerror("a[i..j]: not array")
        esac
    fi
end

proc twiden(unit p, int lv)=
    int m,u,mbase

    mbase:=ttbasetype[m:=p.mode]

    if mbase=tvoid then return fi       
    if lv then return fi                

    if not ttisshort[mbase] then return fi  
    case p.tag
    when jname, jptr, jindex, jdot then
            p.memmode:=m                
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
    unit a,b,prange
    int length

    if tttarget[slicemode]<>tc8 then
        txerror("Not char slice")
    fi
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
        CPL P.BFCODE
        TXERROR("BITFIELD")
    esac

    if i=j then         
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
CPL "HERE"
        p.b:=r
    fi

    p.mode:=tu64
end

proc deref(unit a, int needres=1)=
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


    if jisexpr[tag]=2 then
        tevalbinop(p)
            setsimple(p)

    elsif jisexpr[tag]=1 then
        tevalmonop(p)
        if p.tag=junary and not complexopset[p.pclop] then
            setsimple(p)
        fi

    elsecase tag
    when jmakerange then
        a:=p.a
        b:=p.b
        if ttsize[a.mode]<=8 then           
            tevaluate(a)
            tevaluate(b)
            if a.tag=jconst and b.tag=jconst then
                p.isconst:=a.isconst iand b.isconst
            fi
        fi
    fi

end

function addrdotindex(unit p, int &offset)unit q=
    int axmode

CPL "ADDRDOTIX",STRMODE(P.MODE)
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
            if lhs.a.tag=jname then         
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
    if a and ttbasetype[a.mode]=tarray and tttarget[a.mode]=tc8 then
        coerceunit(a,trefchar,0)
    fi
end

proc combinestrings(unit p)=
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
                if not hard then                
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
            if starg=tvoid or ttarg=tvoid then          
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
            if ttarg not in [tc8, tu8] then
                opc:=kichartoslice
            fi
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

    case opc
    when kzero then                 
        return
    when kerror then
        txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

    when kharderror then
        txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

    when ksoftconv then
        p.mode:=t
        return
    when ksofttruncshort then
        if tevalconvert(p,s,t,opc) then
            return
        fi
        insertunit(p,jshorten)
        p.mode:=t           
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

    if tevalconvert(p,s,t,opc) then     
        return
    fi

    insertunit(p, jconvert)
    p.pclop:=opc

    p.convmode:=s
    p.resultflag:=1

    if ttisshort[t] then
        p.convmode:=t
        t:=gettypebase(t)
    fi

    p.mode:=t
end

proc checkmodes(int s,t)=
    if not comparemodes(s,t) then
        txerror_ss("Type-compare error: # <-> #",strmode(s), strmode2(t))
    fi
end

function comparemodes(int s,t)int=
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
    fi
    return 0
end

function tevalconvert(unit p,int s,t,opc)int=
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
        if b.pclop in [kidiv, kirem] then       
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
    unit p,q, alist:=a.a
    int nretmodes,i, alength:=a.length
    ref[]int32 pmult
    symbol d                

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
CPL "MULT:=SLICE",A.LENGTH
        if alength<>2 then txerror("(a,b):=slice") fi
        tpasslv(alist,createrefmode(nil, tttarget[b.mode]))
        tpasslv(alist.nextunit,ti64)

    when trange then
CPL "MULT:=RANGE"
    when trecord then
CPL "MULT:=RECORD"

    elsif b.tag=jbin and b.pclop=kidivrem then
CPL "MULT:=DIVREM"
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
    tpass(p,,needlv)
    if t not in [tany, tvoid] then
        if not comparemodes(p.mode, t) then
            txerror_ss("PassLV type mismatch: #:=#",strmode(p.mode), strmode2(t))
        fi
    fi
end

function dobinnumx(unit p,a,b)int=

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
    when jcallfn, jcallproc then
        return
    when jbin then
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
=== mc_genmcl.m 0 0 17/33 ===
global symbol blockretname
global ref mclrec mclpushstack          
global ref mclrec mcldefine             

macro divider = gencomment("------------------------")
global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)

global function codegen_mcl:int=
    ref procrec pp
    symbol d, e

    mclinit()


    pp:=staticlist
    while pp do
        d:=pp.def
        dostaticvar(d)
        pp:=pp.nextproc
    od

    gencomment("")

    pp:=proclist
    while pp do
        d:=pp.def
        genprocdef(currproc:=d)
        pp:=pp.nextproc
    od

    genabsneg()
    genstringtable()
    genrealtable()

    genfunctable()

    genmc(m_programend)

    if (debugmode and fshowasm) then
    fi

    return 1
0
end

proc genprocdef (symbol p) =
    ref modulerec ms
    symbol d
    operand rx

    procdef:=p

    setsegment('C')

    genmc(m_procstart,genmemaddr(procdef))
    genmc(m_labelname,genmemaddr(procdef))

    regset:=0
    nregs:=nxregs:=0
    mstackdepth:=ncalldepth:=0
    nblocktemps:=0

    ms:=&moduletable[p.moduleno]
    if p=ms.stmain then
        genmaindef(p)
        return
    elsif p=ms.ststart then
        genstartdef(p)
        return
    fi
    genlocals(p)
    retindex:=createfwdlabel()
    divider()
    rx:=loadunit(p.code)
    divider()
    definefwdlabel(retindex)
    genreturn()
    checkreturn(p)
    genblocktemps()
    gencomment(showregset("REGS:"))
    [100]char str
    print @str,"Stack depth:",mstackdepth
    gencomment(str)

    genmc(m_procend)
    gencomment("")
end

proc dostaticvar(symbol d)=
    unit p
    int align

    if d.isimport then return fi
    if d.scope = program_scope and d.name^='$' then
        if eqstring(d.name,"$cmdskip") then
            d.scope:=export_scope               
        fi
    fi

    align:=getalignment(d.mode)

    if d.atvar=1 then
        return
    elsif d.code then
        setsegment('I',align)
        genmc(m_labelname,genmemaddr(d))
        genidata(d.code)
    else
        setsegment('Z',align)
        genmc(m_labelname,genmemaddr(d))
        genmc(m_resb,genint(ttsize[d.mode]))
    fi

end

proc genidata(unit p,int doterm=1, am='A',offset=0)=
    int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion,tbase
    unit q,a,b
    symbol d
    real32 sx

    t:=p.mode
    mlineno:=p.pos
    tbase:=ttbasetype[t]

    case p.tag
    when jconst then
        if ttisref[p.mode] then
            if p.mode=trefchar then
                if p.svalue then
                    genmc(m_dq, genstring(p.svalue))
                else
                    genmc(m_dq, genint(0))
                fi
            else
                genmc(m_dq, genint(p.value))
            fi
        elsif ttisreal[p.mode] then
            case ttsize[p.mode]
            when 4 then
                genmc(m_dd, genrealimm(p.xvalue,4))
            when 8 then
                genmc(m_dq, genrealimm(p.xvalue))
            else
                gerror_s("IDATA/REAL:",strmode(p.mode),p)
            esac

        else                        
            case ttsize[getmemmode_m(p)]
            when 1 then
                genmc(m_db, genint(p.value))
            when 2 then
                genmc(m_dw, genint(p.value))
            when 4 then
                genmc(m_dd, genint(p.value))
            when 8 then
                genmc(m_dq, genint(p.value))
            else
                gerror_s("IDATA/INT:",strmode(p.mode),p)
            esac

        fi

    when jmakelist then
        q:=p.a
        while q do
            genidata(q)
            q:=q.nextunit
        od

    when jname then
        d:=p.def
        case d.nameid
        when staticid,procid,dllprocid then
            genmc((am='P' or ttsize[p.mode]=8|m_dq|m_dd), genmemaddr(d))
        when labelid then
            if d.index=0 then d.index:=++mlabelno fi
            genmc(m_dq, genlabel(d.index))
        else
            gerror("Idata &frameXXX")
        esac
        return
    when jconvert then
        genidata(p.a)
    when jshorten then
        a:=p.a
        case ttsize[p.mode]
        when 1 then
            genmc(m_db, genint(a.value))
        when 2 then
            genmc(m_dw, genint(a.value))
        when 4 then
            genmc(m_dd, genint(a.value))
        else
            gerror_s("IDATA/SHORTEN:",strmode(p.mode),p)
        esac

    when jaddrof,jaddroffirst then
        genidata(p.a,am:'P',offset:(p.b|p.b.value|0))
    else
        gerror_s("IDATA: ",jtagnames[p.tag],p)

    esac
end

proc checkreturn(symbol p)=
    if p.mode<>tvoid then
        if not checkblockreturn(p.code) then
            gerror_s("Function needs explicit return: ",p.name)
        fi
    fi
end

proc genreturn=
    operand ax, bx

    if ttisblock[procdef.mode] then
GENCOMMENT("COPY BLOCK RETURN")

        genmc(m_mov, ax:=genreg(), genmem(blockretname))
        genmc(m_push, ax)
        bx:=genreg(r0)
        copyblock(ax, bx, ttsize[blockretname.mode], savedest:0)
        genmc(m_pop, genreg(r0))

    fi

    if procdef.isthreaded then
        genmc(m_ret)                
        return
    fi

GENCOMMENT("RESTORE REGS ETC")


    popstack(framebytes)
    genmc(m_pop, dframeopnd)
    genmc(m_ret)

end

proc genlocals(symbol p)=
    symbol d:=p.deflist, e, b

    frameoffset:=paramoffset:=0
    nparams:=0
    mclpushstack:=mcldefine:=nil

    if p.isthreaded then
        e:=d
        while d, d:=d.nextdef do
            if d.nameid in [paramid, frameid] then
                gerror("param/locals in threaded proc")
            fi
        od
        return
    fi

    if ttisblock[p.mode] then
        b:=getduplnameptr(p,addnamestr("$1x"),paramid)
        b.mode:=p.mode
        blockretname:=b
        paramdefs[++nparams]:=b
        b.offset:=paramoffset+16        
        genmc(m_define, genname(getdispname(b)), genint(b.offset))
        paramoffset+:=8
    fi

    while d do
        mlineno:=d.pos
        case d.nameid
        when frameid then
            if not d.atvar then
                frameoffset-:=roundsizetg(ttsize[d.mode])
                d.offset:=frameoffset
                genmc(m_define, genname(getdispname(d)), genint(frameoffset))
            fi
        when paramid then
            paramdefs[++nparams]:=d
            d.offset:=paramoffset+16        
            genmc(m_define, genname(getdispname(d)), genint(d.offset))
            paramoffset+:=8
        esac
        d:=d.nextdef
    od
    mcldefine:=mccodex

    framebytes:=-frameoffset
    if framebytes iand 8 then
        framebytes+:=8
    fi
    framebytes+:=32             

    genmc(m_push, dframeopnd)
    genmc(m_mov, dframeopnd, dstackopnd)

    pushstack(framebytes)
    mclpushstack:=mccodex

    spillparams()

end

proc spillparams=
    symbol d
    operand ax
    int offset:=16, regoffset:=0

    regoffset:=0

    for i to nparams do
        if regoffset>3 then exit fi
        d:=paramdefs[i]
        if not d.reg then
            ax:=genindex(areg:rframe, size:8, offset:offset)
            case ttbasetype[d.mode]
            when tr64 then
                genmc(m_movq, ax, genxreg(regoffset+xr0))
            when tr32 then
                genmc(m_movd, changeopndsize(ax,4), genxreg(regoffset+xr0))
            else
                genmc(m_mov, ax, genreg(regoffset+r10))
            esac
        elsif d.reg then            
            MERROR("PARAM SPILL/REG")
        fi
        offset+:=8
        ++regoffset
    od

end

proc genblocktemps=


    return when nblocktemps=0

GENCOMMENT("<GENBLOCKTEMPS>",STRINT(NBLOCKTEMPS))

end

proc docallproc(symbol d)=
    genmc(m_call, genmemaddr(d))
end

proc genmaindef(symbol p)=
    ref modulerec ms
    symbol d
    int m

    genlocals(p)


    retindex:=createfwdlabel()

    for i:=2 to nsubprogs do
        d:=moduletable[subprogtable[i].firstmodule].ststart
        docallproc(d)
    od
    d:=moduletable[subprogtable[1].firstmodule].ststart
    docallproc(d)


    divider()
    evalunit(p.code)
    divider()

    definefwdlabel(retindex)

    genreturn()
    checkreturn(p)

    genblocktemps()

    gencomment(showregset("REGS:"))
    [100]char str
    print @str,"Stack depth:",mstackdepth
    gencomment(str)

    genmc(m_procend)
    gencomment("")
end

proc genstartdef(symbol p)=
    ref modulerec ms
    symbol d, e
    int lead:=0, m,s

    m:=p.moduleno
    s:=p.subprogno
    if subprogtable[s].firstmodule=m then
        lead:=1
    fi

    genlocals(p)


    retindex:=createfwdlabel()

    if lead then

        for i to nmodules when moduletosub[i]=s and i<>m do
            d:=moduletable[i].ststart
            docallproc(d)
        od
    fi

    divider()
    evalunit(p.code)
    divider()

    definefwdlabel(retindex)

    genreturn()
    checkreturn(p)

    genblocktemps()

    genmc(m_procend)
    gencomment("")
end

global function newblocktemp(int m)symbol=
    [16]char str
    symbol d
    int size

    ++nblocktemps
    fprint @str,"$T#",nblocktemps
    d:=getduplnameptr(currproc,addnamestr(str),frameid)
    d.mode:=m


    setmclentry(mcldefine)

    size:=roundtoblock(ttsize[d.mode],16)
    frameoffset-:=size
    framebytes+:=size
    d.offset:=frameoffset
    genmc(m_define, genname(getdispname(d)), genint(frameoffset))

    mcldefine:=resetmclentry()
    resetmclentry()

    mclpushstack.b.value:=framebytes


    d
end

proc setmclentry(ref mclrec p)=

    static ref mclrec oldmccodex, nextmcl

    oldmccodex:=mccodex
    mccodex:=p
    nextmcl:=p.nextmcl
    mcldefine.nextmcl:=nil
end

func resetmclentry:ref mclrec pnew =
    mccodex.nextmcl:=setmclentry.nextmcl
    pnew:=mccodex
    mccodex:=setmclentry.oldmccodex
    pnew
end
=== mc_genss.m 0 0 18/33 ===
const wmask = 2x1000                
const rmask = 2x0100                
const xmask = 2x0010                
const bmask = 2x0001                

int rex
int sizeoverride                    
int addroverride                    
int f2override                      
int f3override                      

int currseg=0
ref dbuffer currdata                
ref relocrec currrelocs
int nrelocs

int instrno=2

REF MCLREC CURRMCL

global proc genss=
    int index
    ref mclrec m

    initlib(mlabelno)

    ss_zdatalen:=0
    ss_zdata:=buffercreate()
    ss_idata:=buffercreate()
    ss_code:=buffercreate()

    ss_idatarelocs:=nil
    ss_coderelocs:=nil
    ss_nsymbols:=0

    switchseg(code_seg)

    alineno:=9999
    fixregvar()

    m:=mccode
    index:=0

    while m do
        doinstr(m,++index)
        m:=m.nextmcl
    od

    switchseg(0)                    

    if bufferlength(ss_zdata) then
        axerror("Zdata contains code or data")
    fi
end

proc doinstr(ref mclrec m,int index)=
    operand a,b
    symbol d,e
    int x,offset,shortjmp,n

    buffercheck(currdata)

    rex:=sizeoverride:=addroverride:=f2override:=f3override:=0

    a:=m.a
    b:=m.b

    ++instrno
    alineno:=m.seqno



    CURRMCL:=M

    switch m.opcode
    when m_procstart then
        CURRASMPROC:=M.A.DEF
    when m_procend then
    when m_define then

    when m_definereg then
    when m_deleted then
    when m_programend then

    when m_labelname then
        case a.valtype
        when stringimm_val then
        when def_val then
            d:=a.def
            d.reftype:=back_ref
            d.segment:=currseg
            d.offset:=getcurrdatalen(6)

            if d.scope=export_scope then
                getstindex(d)
            fi

            dofwdrefs(d)
        esac

    when m_labelx then
        d:=labeldeftable[a.labelno]

        d.reftype:=back_ref
        d.segment:=currseg
        d.offset:=getcurrdatalen(6)
        dofwdrefs(d)

    when m_call then
        do_call(a)

    when m_jmp then
        do_jmp(a,m)

    when m_jmpcc then
        d:=getdef(a,1)
        offset:=getrel32(d,getcurrdatalen(7)+1)
        if offset<0 then            
            if offset<-126 then
                genbyte(0x0F)
                genbyte(0x80+m.cond)
                gendword(offset-4)
            else
                genbyte(0x70+m.cond)
                genbyte(offset)
            fi
        else
            shortjmp:=checkshortjump(m,d)
            if not shortjmp then
                genbyte(0x0F)
                genbyte(0x80+m.cond)
                genrel32(a)
            else
                genbyte(0x70+m.cond)
                genrel8(a)
            fi
        fi

    when m_db then
        genopnd(a,1)
    when m_dw then
        genopnd(a,2)
    when m_dd then
        genopnd(a,4)
    when m_dq then
        genopnd(a,8)

    when m_ddoffset then
        genrel32(a)

    when m_segment then
        switchseg(a.value)

    when m_csegment then
        switchseg(code_seg)
    when m_isegment then
        switchseg(idata_seg)
    when m_zsegment then
        switchseg(zdata_seg)

    when m_nop, m_halt then
        genbyte(mclcodes[m.opcode])

    when m_cbw then
        genbyte(0x66)
        genbyte(0x98)

    when m_cwd then
        genbyte(0x66)
        genbyte(0x99)

    when m_cdq then
        genbyte(0x99)

    when m_cqo then
        genbyte(0x48)
        genbyte(0x99)

    when m_ret then
        genbyte(0xC3)

    when m_retn then
        if a.mode<>a_imm then axerror("retn?") fi
        genbyte(0xC2)
        genword(a.value)

    when m_push then
        do_push(a)

    when m_pop then
        do_pop(a)

    when m_inc, m_dec then
        do_inc(a,mclcodes[m.opcode])

    when m_neg, m_notx, m_mul, m_imul, m_div, m_idiv then
        do_neg(a,mclcodes[m.opcode])

    when m_add, m_sub, m_andx, m_orx, m_xorx, m_adc, m_sbb, m_cmp then
        do_arith(a,b, mclcodes[m.opcode])

    when m_mov then
        do_mov(a,b)
    when m_lea then
        do_lea(a,b)

    when m_movsx then
        do_movsx(a,b,0xBE)

    when m_movzx then
        do_movsx(a,b,0xB6)

    when m_movsxd then
        do_movsxd(a,b)

    when m_xchg then
        do_exch(a,b)

    when m_imul2 then
        do_imul2(a,b)


    when m_resb, m_resw, m_resd, m_resq then
        if a.mode=a_imm then
            n:=a.value*mclcodes[m.opcode]
            case currseg
            when code_seg then
                to n do genbyte(0x90) od
            when idata_seg then
                to n do genbyte(0) od
            else

                ss_zdatalen+:=n
            esac
        else
            axerror("resb?")
        fi

    when m_align then
        if a.mode=a_imm then
            x:=a.value
            if x<1 or x>16384 then axerror("align2") fi
            if currseg<>zdata_seg then
                while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) od
            else
                while ss_zdatalen rem x do  ++ss_zdatalen od
            fi
        else
            axerror("align?")
        fi

    when m_shl,m_shr,m_sar,m_rol,m_ror,m_rcl,m_rcr then
        do_shift(a,b,mclcodes[m.opcode])

    when m_test then
        do_test(a,b)

    when m_loopcx, m_loopz, m_loopnz then
        do_loop(a,mclcodes[m.opcode])

    when m_jecxz then
        do_jcxz(a,4)

    when m_jrcxz then
        do_jcxz(a,8)

    when m_xlat then
        genbyte(0xD7)

    when m_setcc then
        do_setcc(m.cond,a)

    when m_movd then
        do_movxmm(a,b,4)

    when m_movq then
        do_movxmm(a,b,8)

    when m_addss, m_subss, m_mulss, m_divss, m_sqrtss, m_minss, m_maxss then
        do_arithxmm(a,b,0xF3,mclcodes[m.opcode])

    when m_addsd, m_subsd, m_mulsd, m_divsd, m_sqrtsd, m_minsd, m_maxsd then
        do_arithxmm(a,b,0xF2,mclcodes[m.opcode])

    when m_andps,m_xorps then
        do_logicxmm(a,b,mclcodes[m.opcode],4)

    when m_andpd,m_xorpd,m_pand,m_pxor then
        do_logicxmm(a,b,mclcodes[m.opcode],8)

    when m_comiss then
        do_arithxmm(a,b,0,0x2F)

    when m_comisd then
        do_arithxmm(a,b,0x66,0x2F)

    when m_cvtss2sd then
        do_convertfloat(a,b,0xF3)

    when m_cvtsd2ss then
        do_convertfloat(a,b,0xF2)

    when m_cvtss2si then
        do_fix(a,b,0xF3,0x2D)

    when m_cvtsd2si then
        do_fix(a,b,0xF2,0x2D)

    when m_cvttss2si then
        do_fix(a,b,0xF3,0x2C)

    when m_cvttsd2si then
        do_fix(a,b,0xF2,0x2C)

    when m_cvtsi2ss then
        do_float(a,b,0xF3)

    when m_cvtsi2sd then
        do_float(a,b,0xF2)

    when m_cmovcc then
        do_cmovcc(m.cond, a,b)

    when m_fsqrt,m_fsin,m_fcos,m_fsincos,m_fptan, m_fpatan,m_fabs,m_fchs then
        genbyte(0xD9)
        genbyte(mclcodes[m.opcode])

    when m_fld, m_fst, m_fstp then
        do_fmem(a,1,mclcodes[m.opcode])

    when m_fild, m_fist, m_fistp then
        do_fmem(a,0,mclcodes[m.opcode])

    when m_fadd, m_fsub, m_fmul, m_fdiv then
        genbyte(0xDE)
        genbyte(mclcodes[m.opcode])

    when m_cmpsb then
        genbyte(0xA6)

    when m_cmpsw then
        genbyte(0x66)
        genbyte(0xA7)
    when m_cmpsd then
        genbyte(0xA7)
    when m_cmpsq then
        genbyte(0x48)
        genbyte(0xA7)

    when m_rdtsc then       
        genbyte(0x0F)
        genbyte(mclcodes[m.opcode])

    when m_movdqa, m_movdqu then
        do_movdqx(a,b,mclcodes[m.opcode])

    when m_finit then
        genbyte(0xDB)
        genbyte(0xE3)

    when m_fldz, m_fld1, m_fldpi, m_fld2t, m_fld2e, m_fldlg2, m_fldln2 then
        genbyte(0xD9)
        genbyte(mclcodes[m.opcode])

    when m_popcnt then
        do_popcnt(a,b)

    when m_bsf, m_bsr then
        do_bsf(a,b,mclcodes[m.opcode])

    when m_cpuid then
        genbyte(0x0F)
        genbyte(0xA2)

    when m_comment then
    when m_blank then
    else
        println "*** Can't do opcode",mclnames[m.opcode],"line",alineno,=M.OPCODE,=M_HALT
    CPL
    CPL
    AXERROR("STOPPING")
    endswitch

end


macro genbyte(x) = currdata.pcurr++^:=x

proc genword(int x)=
    addword(currdata,x)
end


proc gendword(int x)=
    adddword(currdata,x)
end


proc genqword(int64 x)=
    addqword(currdata,x)
end

proc genopnd(operand a,int size=0)=
    ref char s
    int64 x
    int length

    if size=0 then size:=a.size fi

    case a.valtype
    when stringimm_val then
        s:=a.svalue
        length:=strlen(s)
        if length>100 then
            buffercheck(currdata,max(1024,length+1))
        fi
        while s^ do
            genbyte(s++^)
        od
        return
    WHEN NAME_VAL THEN
        PRINTLN "GENSS/NAME OPND"
    esac

    if getdef(a) and size<=2 then
        axerror("8/16-BIT RELOC")
    fi

    case size
    when 1 then
        genbyte(a.value)
    when 2 then
        genword(a.value)
    when 4 then
        case a.valtype
        when intimm_val then
            gendword(a.value)
        when realimm_val then
            real32 x32
            x32:=a.xvalue
            gendword(int32@(x32))
        when realmem_val then
            CPL "       OPND/REALMEM4"
        when stringimm_val then
            CPL "       OPND/STRINGIMM4"
        when def_val,label_val then
            genabs32(a)
        when name_val then
            CPL "       OPND/NAME4"
        else
            cpl valtypenames[a.valtype]
            axerror("OPND/4/VALTYPE?")
        esac

    when 8 then
        case a.valtype
        when intimm_val then
            genqword(a.value)
        when realimm_val then
            genqword(int64@(a.xvalue))
        when realmem_val then
            CPL "       OPND/REALMEM8",ALINENO
        when stringimm_val then
            CPL "       OPND/STRINGIMM8"
        when def_val,label_val then
            genabs64(a)
        when name_val then
            CPL "       OPND/NAME8"
        else
            CPL "HERE"
            cpl valtypenames[a.valtype]
            axerror("OPND/8/VALTYPE?")
        esac

    esac
end

proc addrelocitem(int reloctype, symbol d)=
    ref relocrec r
    int stindex, adjust

    stindex:=getstindex(d)

    adjust:=4
    if reloctype=addr64_rel then adjust:=8 fi

    r:=pcm_alloc(relocrec.bytes)
    r.nextreloc:=currrelocs
    r.reloctype:=reloctype
    r.offset:=getcurrdatalen(1)-adjust
    r.stindex:=stindex

    ++nrelocs
    currrelocs:=r
end

function getstindex(symbol d)int=
    if d.stindex=0 then
        if ss_nsymbols>=ss_symboltablesize then
            extendsymboltable()
        fi
        d.stindex:=++ss_nsymbols
        ss_symboltable[d.stindex]:=d

        if d.segment=0 then
            if d.isimport then
                d.segment:=code_seg
            fi
        fi

    fi
    return d.stindex
end

proc genrel32(operand a)=
    symbol d

    d:=getdef(a)

    if d=nil then               
        gendword(a.value)
        return
    fi

    case d.reftype
    when back_ref then
        if d.segment<>currseg then
            axerror("Rel label across segments")            
        fi
        gendword(d.offset-(getcurrdatalen(2)+4)+a.offset)
    when fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel32_rel)
        gendword(a.offset)
    else                                
        gendword(a.offset)      
        addrelocitem(rel32_rel,d)
    esac
end

function getdef(operand a,int dneeded=0)symbol =
    symbol d

    if a.mode in [a_mem,a_imm] then
        case a.valtype
        when label_val then
            return labeldeftable[a.labelno]
        when def_val then
            d:=a.def
            if d.reftype=0 then
                if not d.isimport then
                    d.reftype:=fwd_ref
                fi
            fi

            return d
        esac
    fi
    if dneeded then             
        println opndnames[a.mode],valtypenames[a.valtype]
        axerror("getdef/no def")
    fi
    return nil
end

proc genabs32(operand a)=
    symbol d

    d:=getdef(a,1)

    case d.reftype
    when back_ref then

        gendword(d.offset+a.offset)
        addrelocitem(addr32_rel,d)

    when fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
        if d.nameid in [frameid, paramid] then
            gendword(d.offset+a.offset)
        else
            gendword(a.offset)
            addrelocitem(addr32_rel,d)
        fi

    else                                
        gendword(a.offset)                  
        addrelocitem(addr32_rel,d)
    esac
end

proc genabs64(operand a)=
    symbol d

    d:=getdef(a,1)

    case d.reftype
    when back_ref then
        genqword(d.offset+a.offset)
        addrelocitem(addr64_rel,d)

    when fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
        if d.nameid in [frameid, paramid] then
            genqword(d.offset+a.offset)
        else
            genqword(a.offset)
            addrelocitem(addr64_rel,d)
        fi

    else                                
        genqword(a.offset)              
        addrelocitem(addr64_rel,d)
    esac
end

function getrel32(symbol d,int offset)int=
    if d.reftype=back_ref then                  
        if d.segment<>currseg then
            axerror("Rel label across segments2")
        fi
        return d.offset-(offset+1)
    else
        return int32.maxvalue
    fi
end

proc dofwdrefs(symbol d)=
    ref fwdrec f
    int offset, seg
    ref byte p8
    ref int32 p32
    ref int64 p64
    ref dbuffer data

    if d.fwdrefs=nil then return fi
    f:=d.fwdrefs

    while f do
        offset:=f.offset

        case f.reltype
        when rel32_rel then
            p32:=bufferelemptr(currdata,offset)
            p32^:=d.offset-offset-4

        when addr32_rel,addr64_rel then
            case f.seg
            when code_seg then data:=ss_code
            when zdata_seg then axerror("Fwd ref in zdata")
            when idata_seg then data:=ss_idata
            esac

            p32:=bufferelemptr(data,offset)
            if f.reltype=addr32_rel then
                p32^:=p32^+d.offset
            else
                p64:=cast(p32)
                p64^:=p64^+d.offset
            fi
        when rel8_rel then
            p8:=bufferelemptr(currdata,offset)
            p8^:=d.offset-offset-1
        else
            CPL RELOCNAMES[F.RELTYPE],D.NAME
            AXERROR("DOFWDREFS/CAN'T DO RELTYPE")
        esac

        f:=f.nextfwd
    od
end

proc genrex=
    if sizeoverride then
        genbyte(0x66)
    fi
    if addroverride then
        genbyte(0x67)
    fi
    if rex then
        if rex<0x40 then
            genbyte(0x40+rex)
        else
            genbyte(rex)
        fi
    fi
end

function isbytesized(int64 x)int=
    return -128<=x<=127
end

function isdwordsized(int64 x)int=
    return int32.minvalue<=x<=int32.maxvalue
end

proc do_push(operand a)=
    int code,am

    case a.mode
    when a_reg then
        if a.size<>8 then axerror("pushreg not 64-bit") fi
        code:=regcodes[a.reg]
        if code>=8 then
            rex :=bmask
            code iand:=7
        fi
        genrex()
        genbyte(0x50+code)

    when a_imm then
        if getdef(a) then
            genbyte(0x68)
            genopnd(a,4)
        elsif isbytesized(a.value) then
            genbyte(0x6A)
            genbyte(a.value)
        elsif isdwordsized(a.value) then
            genbyte(0x68)
            gendword(a.value)
        else
            axerror("push imm value too large")
        fi

    when a_mem then
        if a.size<>8 then axerror("push not 64-bit") fi
        am:=genrm(a,6)
        genrex()
        genbyte(0xFF)
        genamode(a,am)
    else
        axerror("push opnd?")
    esac
end

proc do_pop(operand a)=
    int code, am

    case a.mode
    when a_reg then
        if a.size<>8 then axerror("popreg not 64-bit") fi
        code:=regcodes[a.reg]
        if code>=8 then
            rex :=bmask
            code iand:=7
        fi
        genrex()
        genbyte(0x58+code)

    when a_mem then
        if a.size<>8 then axerror("pop not 64-bit") fi
        am:=genrm(a,0)
        genrex()
        genbyte(0x8F)
        genamode(a,am)
    else
        axerror("pop opnd?")
    esac
end

proc do_inc(operand a,int code)=
    int opc, am

    opc:=(a.size=1|0xFE|0xFF)

    case a.mode
    when a_reg, a_mem then
        am:=genrm(a,code)
        checkhighreg(a)
        setopsize(a)
        genrex()
        genbyte(opc)
        genamode(a,am)

    else
        axerror("inc/opnd?")
    esac
end

proc do_neg(operand a,int code)=
int opc, am

    opc:=(a.size=1|0xF6|0xF7)

    case a.mode
    when a_reg, a_mem then
        am:=genrm(a,code)
        checkhighreg(a)
        setopsize(a)
        genrex()
        genbyte(opc)
        genamode(a,am)

    else
        axerror("neg/div/etc opnd?")
    esac
end

proc genamode(operand a,int am)=
    int sib,mode,dispsize,offset
    symbol d

    sib:=am>>16

    mode:=(am>>8)iand 255
    dispsize:=am iand 255

    genbyte(mode)           

    if sib>=0 then      
        genbyte(sib)
    fi

    case dispsize           
    when 0 then
    when 1 then
        getdispsize(a,offset)
        genbyte(offset)
    when 4 then

        case a.mode
        when a_mem then

            case a.valtype
            when def_val, label_val then
                genabs32(a)
            when no_val then
                getdispsize(a,offset)
                gendword(offset)
            else
                axerror("genam/3")
            esac
        else
            CPL OPNDNAMES[A.MODE]
            axerror("GENAMODE/MODE?")
        esac
    else
        axerror("genamode size 2/8")
    esac
end

proc setopsize(operand a)=
    case a.size
    when 1 then         
    when 2 then         
        sizeoverride:=1
    when 8 then         
        rex ior:=wmask
    when 4 then         
    else
        axerror("Operand size not set")
    esac
end


function getdispsize(operand a, int &offset)int=
    symbol d

    d:=getdef(a)
    offset:=a.offset

    if d then
        if d.nameid in [frameid, paramid] then
            offset+:=d.offset
        else
            return 4
        fi
    fi

    if offset then
        return (isbytesized(offset)|1|4)
    else
        return 0
    fi
end

function genrm(operand a,int opc)int=
    static []int scaletable6=( 0, 1<<6, 0, 2<<6, 0, 0, 0, 3<<6)
    int mode, rm, scale, dispsize, sib, index, base
    int reg, regix, code, offset

    mode:=rm:=0             
    scale:=0                
    dispsize:=0
    sib:=-1


    case a.mode
    when a_reg then         
        code:=getregcodeb(a.reg)
        return sib<<16 + 3<<14 + opc<<11 + code<<8 + dispsize

    when a_mem then

    when a_xreg then
        code:=getregcodebx(a.reg)
        return sib<<16 + 3<<14 + opc<<11 + code<<8 + dispsize

    else
        axerror("genrm not mem")
    esac

    reg:=a.reg
    regix:=a.regix

    if reg=regix=0 then                     
        mode:=0
        rm:=4
        scale:=1
        index:=4
        base:=5
        dispsize:=4

    elsif a.scale<=1 and regix=0 then           
        dispsize:=getdispsize(a,offset)
        if dispsize then
            mode:=(dispsize=1|1|2)
        fi

        rm:=regcodes[reg]

        if rm<>4 and rm<>12 then
            base:=rm
            if (rm=5 or rm=13) and dispsize=0 then
                mode:=1; dispsize:=1
            fi
            index:=0
        else
            index:=4                
            base:=rm
            scale:=1                

        fi
    elsif regix and reg=0 then
        dispsize:=4
        mode:=0
        rm:=4
        scale:=(a.scale|a.scale|1)
        base:=5
        index:=regcodes[regix]
        if regix=rstack then axerror("Scaled rstack?") fi

    else                                        
        dispsize:=getdispsize(a,offset)
        if dispsize then
            mode:=(dispsize=1|1|2)
        fi
        rm:=4

        scale:=(a.scale|a.scale|1)
        if reg=0 then
            base:=5
        else
            if reg in [rframe,r7] and dispsize=0 then
                mode:=1; dispsize:=1
            fi
            base:=regcodes[reg]
        fi

        if regix=0 then
            index:=4
        else
            index:=regcodes[regix]

            if not reg then
                dispsize:=4
            fi

            if regix=rstack and scale>1 then axerror("Can't scale rstack") fi
        fi

    fi

    if index>=8 then rex ior:= xmask; index iand:=7 fi
    if base>=8  then rex ior:= bmask; base  iand:=7 fi

    if scale then
        sib:=scaletable6[scale] + index<<3 + base
    fi
    rm iand:=7

    return sib<<16 + mode<<14 + opc<<11 + rm<<8 + dispsize
end

function makeam(int m,s,d)int=
    return s<<16+m<<8+d
end

function makemodrm(int mode,opc,rm)int=
    return mode<<6+opc<<3+rm
end

proc do_arith(operand a,b,int code)=
    int am, regcode, opc, dispsize
    int64 x

    case a.mode
    when a_reg then
        case b.mode
        when a_reg, a_mem then
            regcode:=getregcoder(a.reg)
            am:=genrm(b,regcode)
            checkhighreg(a)
            checkhighreg(b)
            setopsize(a)
            opc:=code<<3 ior (a.size=1|0x02|0x03)
            genrex()
            genbyte(opc)
            genamode(b,am)

        when a_imm then
    doregimm::
            if getdef(b) then
                if code<0 or code>7 then axerror("non-add arith/label") fi
                if a.size<4 then axerror("add imm/size") fi
                am:=genrm(a,code)
                setopsize(a)
                genrex()
                genbyte(0x81)
                genamode(a,am)
                genopnd(b,4)
                return

            fi

            x:=b.value
            dispsize:=1
            if a.size=1 then
                opc:=0x80
            elsif -128<=x<=127 then
                opc:=0x83
            else
                unless -0x8000'0000 <= x <= 0xFFFF'FFFF then axerror("3:exceeding word32 value") end
                opc:=0x81
                dispsize:=(a.size=2|2|4)
            fi

            am:=genrm(a,code)
            checkhighreg(a)
            setopsize(a)
            genrex()
            genbyte(opc)
            genamode(a,am)
            case dispsize
            when 1 then genbyte(x)
            when 2 then genword(x)
            when 4 then gendword(x)
            esac

        else
            axerror("ADD reg,???")
        esac

    when a_mem then
        case b.mode
        when a_reg then
            regcode:=getregcoder(b.reg)
            am:=genrm(a,regcode)
            checkhighreg(b)
            setopsize(b)
            opc:=code<<3 ior (b.size=1|0x00|0x01)
            genrex()
            genbyte(opc)
            genamode(a,am)

        when a_imm then
            go to doregimm
        else
            axerror("ADD mem,???")
        esac

    else
        CPL OPNDNAMES[A.MODE]
        axerror("Can't add to this opnd")
    esac
end

proc do_mov(operand a,b)=
    int regcode, am
    int64 value

    case a.mode
    when a_reg then
        case b.mode
        when a_reg, a_mem then
            if a.size<>b.size and b.size then
                axerror("1:Opnd size mismatch")
            fi

            checkhighreg(a)
            checkhighreg(b)
            regcode:=getregcoder(a.reg)
            am:=genrm(b,regcode)

            setopsize(a)

            genrex()
            genbyte((a.size=1|0x8A|0x8B))
            genamode(b,am)

        when a_imm then
            value:=b.value
            regcode:=getregcodeb(a.reg)
            if getdef(b) and a.size<=2 then axerror("mov imm?") fi
            case a.size
            when 1 then
                checkhighreg(a)
                case a.reg
                when r5,r3,r14,r15 then
                    rex ior:=0x40
                esac
                unless -128<=value<=255 then axerror("exceeding byte value") end
                genrex()
                genbyte(0xB0+regcode)
                genbyte(value)

            when 2 then
                unless -32768<=value<=65535 then axerror("exceeding word16 value") end
                genbyte(0x66)
                genrex()
                genbyte(0xB8+regcode)
                genword(value)
            when 4 then
                if getdef(b) then
                    genrex()
                    genbyte(0xB8+regcode)
                    genopnd(b,4)
                else
                    unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
                        CPL value,ref void(value)
                        axerror("1:exceeding word32 value")
                    end
doreg32::
                    genrex()
                    genbyte(0xB8+regcode)
                    gendword(value)
                fi

            else                            
                if getdef(b) then
                    rex ior:=wmask
                    genrex()
                    genbyte(0xB8+regcode)
                    genopnd(b,8)
                else
                    if value>=0 and value<=0xFFFF'FFFF then
                        goto doreg32            
                    fi
                    rex ior:=wmask
                    genrex()
                    genbyte(0xB8+regcode)
                    genqword(value)
                fi

            esac

        else
            axerror("MOV REG/??")
        esac
    when a_mem then
        case b.mode
        when a_reg then
            if a.size<>b.size and a.size then
CPL A.SIZE, B.SIZE
                axerror("2:Opnd size mismatch")
            fi
            regcode:=getregcoder(b.reg)
            checkhighreg(b)
            am:=genrm(a,regcode)
            setopsize(b)
            genrex()
            genbyte((b.size=1|0x88|0x89))
            genamode(a,am)

        when a_imm then
            value:=b.value
            am:=genrm(a,0)
            if getdef(b) and a.size<=2 then axerror("mov imm?") fi

            if a.size=0 then a.size:=1 fi

            case a.size
            when 0,1 then
                unless -128<=value<=255 then axerror("exceeding byte value") end

                setopsize(a)
                genrex()
                genbyte(0xC6)
                genamode(a,am)
                genbyte(value)

            when 2 then
                unless -32768<=value<=65535 then axerror("exceeding word16 value") end
                setopsize(a)
                genrex()
                genbyte(0xC7)
                genamode(a,am)
                genword(value)
            when 4,8 then
                if not getdef(b) then
                    unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding word32 value") end
                fi
                setopsize(a)
                genrex()
                genbyte(0xC7)
                genamode(a,am)
                genopnd(b,4)
            esac

        else
            CPL OPNDNAMES[A.MODE]
            CPL OPNDNAMES[B.MODE]
            axerror("MOV MEM/?")
        esac
    else
        axerror("MOV ?/..")
    esac
end

function getregcoder(int reg)int=
    int regcode

    regcode:=regcodes[reg]
    if regcode>=8 then
        regcode-:=8
        rex ior:=rmask
    fi
    return regcode
end

function getregcodeb(int reg)int=
    int regcode

    regcode:=regcodes[reg]
    if regcode>=8 then
        regcode-:=8
        rex ior:=bmask
    fi
    return regcode
end

function getregcodebx(int reg)int=

    int regcode

    regcode:=regcodes[reg]
    if regcode>=8 then
        regcode-:=8
        rex ior:=bmask
    fi
    return regcode
end

function getregcoderx(int reg)int=
    int regcode

    regcode:=regcodes[reg]
    if regcode>=8 then
        regcode-:=8
        rex ior:=rmask
    fi
    return regcode
end


proc do_lea(operand a,b)=
    int regcode, am

    unless a.mode=a_reg and b.mode=a_mem then
        axerror("LEA not reg/mem")
    end

    if a.size<4 then axerror("LEA size error") fi
    regcode:=getregcoder(a.reg)

    am:=genrm(b,regcode)
    setopsize(a)
    genrex()
    genbyte(0x8D)
    genamode(b,am)
end

proc do_movsx(operand a,b,int opc)=
    int am, regcode

    if a.mode<>a_reg then axerror("movsx not reg") fi

    if a.size=8 and b.size=4 then
        if opc=0xBE then
            do_movsxd(a,b)
        else                        
            a:=regtable[a.reg,4]
            do_mov(a,b)
        fi
        return
    fi

    if a.size=1 or a.size<=b.size then axerror("movsx size error") fi

    if opc=0xB6 and b.size=4 then axerror("movsx 4=>8 bytes?") fi

    case b.mode
    when a_reg then
    when a_mem then
        if b.size=0 then axerror("movsx need size prefix") fi
        if b.size=8 then axerror("movsx size 8") fi
    else
        axerror("movsx not reg/mem")
    esac

    regcode:=getregcoder(a.reg)

    am:=genrm(b,regcode)
    setopsize(a)
    checkhighreg(b)
    genrex()
    genbyte(0x0F)
    genbyte((b.size=1|opc|opc+1))
    genamode(b,am)
end

proc checkhighreg(operand a)=
    if a.mode=a_reg then
        case a.reg
        when r5,r3,r14,r15 then
            rex ior:=0x40
        esac
    fi
end

proc do_exch(operand a,b)=
    int regcode, am

    if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then      
        if a.reg<>r0 then               
            swap(a,b)
        fi
        if a.size<>b.size then axerror("exch size") fi

        setopsize(a)
        regcode:=getregcodeb(b.reg)
        genrex()
        genbyte(0x90+regcode)
        return
    fi

    if a.mode=a_mem then swap(a,b) fi

    unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then axerror("exch opnds") end
    if b.size=0 and b.mode=a_mem then b.size:=a.size fi
    if a.size<>b.size then axerror("exch size") fi

    if a.size=1 then
        checkhighreg(a)
        checkhighreg(b)
    fi

    regcode:=getregcoder(a.reg)

    am:=genrm(b,regcode)
    setopsize(a)
    genrex()
    genbyte((a.size=1|0x86|0x87))
    genamode(b,am)
end

proc do_movsxd(operand a,b)=
    int regcode, am

    if b.mode=a_mem and b.size=0 then b.size:=4 fi

    if a.size<>8 or b.size>4 then axerror("movsxd size") fi

    if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
        axerror("movsxd opnds")
    fi

    regcode:=getregcoder(a.reg)
    am:=genrm(b,regcode)

    setopsize(a)
    genrex()
    genbyte(0x63)
    genamode(b,am)
end

proc do_imul2(operand a,b)=
    int regcode, am, opc
    int64 value

    if a.mode<>a_reg then
        axerror("imul2 opnds")
    fi
    if b.size=0 then b.size:=a.size fi
    if a.size=1 then axerror("imul2 byte") fi

    case b.mode
    when a_reg,a_mem then
        if a.size<>b.size then
 axerror("imul2 size") fi
        regcode:=getregcoder(a.reg)
        am:=genrm(b,regcode)

        setopsize(a)
        genrex()
        genbyte(0x0F)
        genbyte(0xAF)
        genamode(b,am)

    when a_imm then                     
        if getdef(b) then axerror("mul/label") fi
        value:=b.value
        regcode:=getregcoder(a.reg)     
        regcode:=getregcodeb(a.reg)
        opc:=0xC0+regcode<<3+regcode
        setopsize(a)
        genrex()

        if -128<=value<=127 then
            genbyte(0x6B)
            genbyte(opc)
            genbyte(value)
        elsif a.size=2 then
            genbyte(0x69)
            genbyte(opc)
            genword(value)
        else
            genbyte(0x69)
            genbyte(opc)
            gendword(value)
        fi
    else
        axerror("imul2 opnds")
    esac
end

proc do_shift(operand a,b,int opc)=
    int am, w

    if a.mode<>a_reg and a.mode<>a_mem then axerror("shift opnds1?") fi

    am:=genrm(a,opc)
    checkhighreg(a)
    setopsize(a)
    genrex()
    w:=(a.size=1|0|1)

    case b.mode
    when a_imm then
        if getdef(b) then axerror("shift/label") fi
        if b.value=1 then
            genbyte(0xD0+w)
            genamode(a,am)
        else
            genbyte(0xC0+w)
            genamode(a,am)
            genbyte(b.value)
        fi
    when a_reg then
        if b.reg<>r10 or b.size<>1 then axerror("cl or b10 needed") fi
        genbyte(0xD2+w)
        genamode(a,am)

    else
        axerror("shift opnds2?")
    esac
end

proc do_test(operand a,b)=
    int64 value
    int opc, am, regcode

    if a.mode=a_reg and a.reg=r0 and b.mode=a_imm then
        value:=b.value
        case a.size
        when 1 then
            genbyte(0xA8)
            genbyte(value)
        when 2 then
            genbyte(0x66)
            genbyte(0xA9)
            genword(value)
        when 4 then
            genbyte(0xA9)
            gendword(value)
        else
            genbyte(0x48)
            genbyte(0xA9)
            gendword(value)
        esac

    elsif (a.mode=a_reg or a.mode=a_mem) and b.mode=a_imm then
        opc:=(a.size=1|0xF6|0xF7)
        value:=b.value

        am:=genrm(a,0)
        checkhighreg(a)
        setopsize(a)
        genrex()
        genbyte(opc)
        genamode(a,am)
        case a.size
        when 1 then
            genbyte(value)
        when 2 then
            genword(value)
        else
            gendword(value)
        esac

    elsif a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then
    doregmem::
        regcode:=getregcoder(a.reg)
        am:=genrm(b,regcode)
        checkhighreg(a)
        checkhighreg(b)
        setopsize(a)
        genrex()
        genbyte((a.size=1|0x84|0x85))
        genamode(b,am)

    elsif a.mode=a_mem and b.mode=a_reg then
        swap(a,b)
        goto doregmem
    else
        axerror("test opnds")
    fi

end

proc do_loop(operand a,int opc)=
    int offset

    offset:=getrel32(getdef(a,1),getcurrdatalen(9)+1)
    if offset<0 then            
        if offset<-126 then
            axerror("loop jmp out of range")
        fi
        genbyte(opc)
        genbyte(offset)
    else
        axerror("Can't do loopxx fwd jump")
    fi
end

proc do_jcxz(operand a,int opsize)=
    int offset

    offset:=getrel32(getdef(a,1),getcurrdatalen(10)+1)
    if offset<0 then            
        if offset<-126 then
            axerror("jcxz jmp out of range")
        fi
        if opsize=4 then genbyte(0x67) fi
        genbyte(0xE3)
        genbyte(offset)
    else
        axerror("Can't do jcxz fwd jump")
    fi
end

proc do_setcc(int cond, operand a)=
    int am

    if (a.mode<>a_reg and a.reg<>a_mem) or a.size>1 then axerror("setcc opnd/size") fi

    am:=genrm(a,0)
    checkhighreg(a)
    genrex()
    genrex()
    genbyte(0x0F)
    genbyte(0x90+cond)
    genamode(a,am)
end

proc do_movxmm(operand a,b,int size)=
    int am, regcode, regcode1, regcode2

    case a.mode
    when a_reg then
        case b.mode
        when a_xreg then
            if a.size<>size then axerror("1:movdq size") fi

            regcode:=getregcoderx(b.reg)
            am:=genrm(a,regcode)
            setopsize(a)
            genbyte(0x66)
            genrex()
            genbyte(0x0F)
            genbyte(0x7E)
            genamode(b,am)

        else
            axerror("movdq reg,?")
        esac
    when a_xreg then
        case b.mode
        when a_reg then
            if b.size<>size then axerror("3:movdq size") fi
            regcode:=getregcoderx(a.reg)
            am:=genrm(b,regcode)
            setopsize(b)
            genbyte(0x66)
            genrex()
            genbyte(0x0F)
            genbyte(0x6E)
            genamode(a,am)

        when a_xreg then
            regcode1:=getregcoderx(a.reg)
            regcode2:=getregcodebx(b.reg)
            genbyte(0xF3)
            genrex()
            genbyte(0x0F)
            genbyte(0x7E)
            genbyte(0xC0+regcode1<<3+regcode2)

        when a_mem then
            if b.size and b.size<>size then axerror("4:movdq size") fi
            regcode:=getregcoderx(a.reg)
            am:=genrm(b,regcode)
            if size=4 then
                genbyte(0x66)
                genrex()
                genbyte(0x0F)
                genbyte(0x6E)
            else
                genbyte(0xF3)
                genrex()
                genbyte(0x0F)
                genbyte(0x7E)
            fi
            genamode(b,am)

        else
            axerror("movdq xreg,?")
        esac
    when a_mem then
        case b.mode
        when a_xreg then
            if a.size and a.size<>size then axerror("5:movdq size") fi
            regcode:=getregcoderx(b.reg)
            am:=genrm(a,regcode)
            if size=4 then
                genbyte(0x66)
                genrex()
                genbyte(0x0F)
                genbyte(0x7E)
            else
                genbyte(0x66)
                genrex()
                genbyte(0x0F)
                genbyte(0xD6)
            fi
            genamode(a,am)

        else
            axerror("movdq mem,?")
        esac
    else
        axerror("movdq opnds")
    esac

end

proc do_arithxmm(operand a,b,int prefix,opc)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        axerror("arithxmm opnds")
    fi

    if b.mode=a_xreg then
        regcode:=getregcoderx(a.reg)
        am:=genrm(b,regcode)
        if prefix then genbyte(prefix) fi
        genrex()
        genbyte(0x0F)
        genbyte(opc)
        genamode(a,am)
    else
        regcode:=getregcoderx(a.reg)
        am:=genrm(b,regcode)
        if prefix then genbyte(prefix) fi
        genrex()
        genbyte(0x0F)
        genbyte(opc)
        genamode(b,am)
    fi
end

proc do_logicxmm(operand a,b,int opc,size)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        axerror("logicxmm opnds")
    fi

    if size=8 then
        genbyte(0x66)
    fi

    if b.mode=a_xreg then
        regcode:=getregcoderx(a.reg)
        am:=genrm(b,regcode)
        genrex()
        genbyte(0x0F)
        genbyte(opc)
        genamode(b,am)
    else
        regcode:=getregcoderx(a.reg)
        am:=genrm(b,regcode)
        genrex()
        genbyte(0x0F)
        genbyte(opc)
        genamode(b,am)
    fi
end

proc do_convertfloat(operand a,b,int prefix)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        axerror("convertfloat opnds")
    fi

    genbyte(prefix)

    if a.mode=a_xreg then
        regcode:=getregcodeRx(a.reg)
        am:=genrm(b,regcode)
        genrex()
        genbyte(0x0F)
        genbyte(0x5A)
        genamode(b,am)
    else
        regcode:=getregcoderx(b.reg)
        am:=genrm(a,regcode)
        genrex()
        genbyte(0x0F)
        genbyte(0x5A)
        genamode(b,am)
    fi
end

proc do_fix(operand a,b,int prefix,opc)=
    int am, regcode

    if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
        axerror("fix opnds")
    fi

    genbyte(prefix)

    if b.mode=a_xreg then
        regcode:=getregcoder(a.reg)
        am:=genrm(b,regcode)
        setopsize(a)
    else
        regcode:=getregcoder(a.reg)
        am:=genrm(b,regcode)
        setopsize(a)
    fi

    genrex()
    genbyte(0x0F)
    genbyte(opc)
    genamode(b,am)
end

proc do_float(operand a,b,int prefix)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
        axerror("float opnds")
    fi

    if b.mode=a_mem then
        if b.size=0 then b.size:=4 fi
        if b.size<>4 and b.size<>8 then axerror("float size") fi
    fi

    genbyte(prefix)

    regcode:=getregcoderx(a.reg)
    am:=genrm(b,regcode)
    setopsize(b)
    genrex()
    genbyte(0x0F)
    genbyte(0x2A)
    genamode(b,am)
end

proc do_call(operand a)=
    int am, regcode
    case a.mode
    when a_imm then
        genbyte(0xE8)
        genrel32(a)
    else                
        case a.size
        when 0 then a.size:=8
        when 1,2,4 then
            axerror("call[]size")
        esac
        am:=genrm(a,2)
        setopsize(a)
        genrex()
        genbyte(0xFF)
        genamode(a,am)

    esac
end

proc do_jmp(operand a,ref mclrec m)=
    int am, regcode, offset, shortjmp
    symbol d

    case a.mode
    when a_imm then             
        case a.valtype
        when label_val,def_val then
            d:=getdef(a,1)
            offset:=getrel32(d,getcurrdatalen(11)+1)+a.offset
            if offset<0 and offset>-126 then
                genbyte(0xEB)
                genbyte(offset)
            else
                shortjmp:=0
                if offset>0 then                
                    shortjmp:=checkshortjump(m,d)
                fi

                if not shortjmp then
                    genbyte(0xE9)
                    genrel32(a)
                else
                    genbyte(0xEB)
                    genrel8(a)
                fi
            fi
        else
            CPL VALTYPENAMES[A.VALTYPE]
            AXERROR("JMP/IMM NOT LABELNO")
        esac
    else                
        case a.size
        when 0 then a.size:=8
        when 1,2,4 then
            axerror("jmp[]size")
        esac
        am:=genrm(a,4)
        setopsize(a)
        genrex()
        genbyte(0xFF)
        genamode(a,am)
    esac
end

function getcurrdatalen(int id)int=

    if currseg=zdata_seg then
        return ss_zdatalen
    fi
    return bufferlength(currdata)
end

proc do_cmovcc(int cond, operand a,b)=
    int am, regcode
    if a.size<>b.size and b.size then
        axerror("3:Opnd size mismatch")
    fi
    if a.size=1 then axerror("cmov/byte") fi
    regcode:=getregcoder(a.reg)
    am:=genrm(b,regcode)

    setopsize(a)
    genrex()
    genbyte(0x0F)
    genbyte(0x40+cond)
    genamode(b,am)
end

proc do_fmem(operand a, int freal, code)=
    int am, regcode, mf

    if a.mode<>a_mem then
        axerror("fmem/not mem")
    fi

    if freal then
        case a.size
        when 4 then mf:=0
        when 8 then mf:=2
        when 16 then
            mf:=1
            case code
            when 0 then code:=5
            when 3 then code:=7
            else
                axerror("r80 not allowed")
            esac
        else
            CPL "SIZE=",A.SIZE
            axerror("fmem size")
        esac
    else
        case a.size
        when 2 then mf:=3
        when 4 then mf:=1
        when 8 then
            mf:=3
            case code
            when 0 then code:=5
            when 3 then code:=7
            else
                axerror("fst i64?")
            esac
        else
            axerror("fmem int size")
        esac
    fi

    am:=genrm(a,code)
    genrex()
    genbyte(0xD9+mf<<1)
    genamode(a,am)
end

proc genrel8(operand a)=
    symbol d

    d:=getdef(a,1)

    if d.reftype=fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
        genbyte(0)
    else                                
        axerror("genrel8")
    fi
end

function checkshortjump(ref mclrec m,symbol d)int=
    int n

    n:=0
    m:=m.nextmcl
    while m and n<=8 do
        case m.opcode
        when m_labelx then
            if m.a.labelno=d.labelno then
                return 1
            fi
        when m_comment, m_blank, m_deleted then
        else
            ++n
        esac
        m:=m.nextmcl
    od

    return 0
end

function addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
    ref fwdrec q

    q:=pcm_alloc(fwdrec.bytes)
    q.nextfwd:=p
    q.offset:=offset
    q.reltype:=reltype
    q.seg:=seg
    return q
end

proc switchseg(int newseg)=
    if newseg=currseg then return fi

    case currseg                        
    when code_seg then
        ss_coderelocs:=currrelocs
        ss_ncoderelocs:=nrelocs
    when idata_seg then
        ss_idatarelocs:=currrelocs
        ss_nidatarelocs:=nrelocs
    esac

    currseg:=newseg

    case currseg
    when code_seg then
        currdata:=ss_code
        currrelocs:=ss_coderelocs
        nrelocs:=ss_ncoderelocs
    when idata_seg then
        currdata:=ss_idata
        currrelocs:=ss_idatarelocs
        nrelocs:=ss_nidatarelocs
    when zdata_seg then
        currdata:=ss_zdata
    esac                            

end

proc do_movdqx(operand a,b, int opc)=
int am,regcode

case a.mode
when a_xreg then
    case b.mode
    when a_xreg then
        regcode:=getregcodebx(b.reg)
        am:=genrm(a,regcode)
        genbyte(opc)
        genrex()
        genbyte(0x0F)
        genbyte(0x6F)
        genamode(a,am)

    when a_mem then
        regcode:=getregcoderx(a.reg)
        am:=genrm(b,regcode)
        genbyte(opc)
        genrex()
        genbyte(0x0F)
        genbyte(0x6F)
        genamode(b,am)

    else
        axerror("movdqx?")
    esac
when a_mem then
    case b.mode
    when a_xreg then
        regcode:=getregcoderx(b.reg)
        am:=genrm(a,regcode)
        genbyte(opc)
        genrex()
        genbyte(0x0F)
        genbyte(0x7F)
        genamode(a,am)

    else
        axerror("movdqx")
    esac
else
    axerror("movdqx")
esac

end

proc do_popcnt(operand a,b)=
    int am, regcode

    if b.mode=a_mem then
        if b.size=0 then b.size:=8 fi
    fi

    genbyte(0xF3)

    regcode:=getregcodebx(a.reg)
    am:=genrm(b,regcode)
    setopsize(a)
    genrex()
    genbyte(0x0F)
    genbyte(0xB8)
    genamode(b,am)
end

proc do_bsf(operand a,b, int opc)=
    int am, regcode

    if b.mode=a_mem then
        if b.size=0 then b.size:=8 fi
    fi
    if a.size<>b.size then axerror("bsf size") fi

    regcode:=getregcodebx(a.reg)
    am:=genrm(b,regcode)
    setopsize(a)
    genrex()
    genbyte(0x0F)
    genbyte(opc)
    genamode(b,am)
end

proc extendsymboltable=
    ref[]symbol oldsymboltable
    int oldsymboltablesize

    oldsymboltablesize:=ss_symboltablesize
    oldsymboltable:=ss_symboltable

    ss_symboltablesize*:=2

    ss_symboltable:=pcm_alloc(ref void.bytes*ss_symboltablesize)

    for i:=1 to ss_nsymbols do
        ss_symboltable[i]:=oldsymboltable[i]
    od

    pcm_free(oldsymboltable,ref void.bytes*oldsymboltablesize)
end

proc fixregvar=
    ref mclrec m
    m:=mccode

end

global proc initlib(int nlabels)=
    [256]char str

    ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
    ss_symboltablesize:=init_ss_symbols
    ss_nsymbols:=0

    labeldeftable:=pcm_alloc(nlabels*ref void.bytes)
    for i to nlabels do
        labeldeftable[i]:=pcm_allocz(strec.bytes)
        labeldeftable[i].labelno:=i
        fprint @&.str,"(L#)",i
        labeldeftable[i].name:=pcm_copyheapstring(&.str)
        labeldeftable[i].reftype:=fwd_ref
    od
end

global function buffercreate(int size=1024)ref dbuffer=
    ref dbuffer a

    a:=pcm_alloc(dbuffer.bytes)

    a.alloc:=size
    a.pstart:=a.pcurr:=pcm_alloc(a.alloc)
    a.pend:=a.pstart+a.alloc
    return a
end

proc bufferexpand(ref dbuffer a)=
    int newalloc,usedbytes
    ref byte p

    newalloc:=a.alloc*2
    usedbytes:=a.pcurr-a.pstart

    if usedbytes>a.alloc then
        axerror("dbuffer error")
        stop
    fi

    p:=pcm_alloc(newalloc)
    memcpy(p,a.pstart,usedbytes)
    a.pstart:=p
    a.pcurr:=p+usedbytes
    a.alloc:=newalloc
    a.pend:=p+newalloc
end

global proc buffercheck(ref dbuffer a,int n=1024)=
    while a.pend-a.pcurr<n do
        bufferexpand(a)
    od
end

global function bufferlength(ref dbuffer a)int=
    return a.pcurr-a.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
    return a.pstart+offset
end

global proc addword(ref dbuffer a, int x)=
    a.pcurr16^:=x
    ++(a.pcurr16)
end

global proc adddword(ref dbuffer a, int x)=
    a.pcurr32^:=x
    ++(a.pcurr32)
end

global proc addqword(ref dbuffer a, int64 x)=
    a.pcurr64^:=x
    ++(a.pcurr64)
end

=== mc_libmcl.m 0 0 19/33 ===
const fasmformat=1

const fuseregtable=1

const targetsize=8

global int fshowmsource=0
global int fshortnames

const maxnestedloops    = 50
global [maxnestedloops,4]int loopstack
global int loopindex                            

global [16]int multregmap               
global int nmultregs

int mclseqno

global macro isframex(d) = (d.nameid in [frameid, paramid])

global proc mclinit=
    operand a
    int r,s

    for r:=r0 to r15 do
        regtable[r,1]:=genreg0(r,1)
        regtable[r,2]:=genreg0(r,2)
        regtable[r,4]:=genreg0(r,4)
        regtable[r,8]:=genreg0(r,8)
    od

    zero_opnd:=genint(0)

    for i in frameregtable.bounds do
        a:=newmclopnd()
        a.mode:=a_mem
        a.reg:=rframe
        a.size:=8
        a.offset:=i
        frameregtable[i]:=a
    end

    dframeopnd:=genreg(rframe,8)
    dstackopnd:=genreg(rstack,8)
    distackopnd:=genireg(rstack,8)

    initmcdest()

    setsegment('C')

    lab_funcnametable:=0
    lab_funcaddrtable:=0

end

global proc initmcdest=
    mccode:=mccodex:=nil
end

export proc genmc(int opcode, operand a=nil,b=nil)=
    ref mclrec m, oldm
    int labno

++NALLMCL

    m:=pcm_allocz(mclrec.bytes)
    m.opcode:=opcode
    m.seqno:=++mclseqno

    m.a:=a
    m.b:=b

IF OPCODE=M_IMUL2 AND A.SIZE=8 AND B.SIZE<8 THEN
CPL "IMUL2/MIXED SIZE"
FI

    case opcode
    when m_call then
        ++inf_proccalls

    when m_lea then
        if b and b.valtype=def_val then
            b.def.addrof:=1
        fi
    when m_labelx then
        labno:=a.labelno

    esac

    if mccode then
        mccodex.nextmcl:=m
        mccodex:=m
    else
        mccode:=mccodex:=m
    fi
end

global proc genmc_cond(int opcode, cond, operand a=nil,b=nil)=
    genmc(opcode,a,b)
    mccodex.cond:=cond
end

global proc genmc_str(int opcode,ichar s)=
    genmc(opcode,genstring(s))
end

global func newmclopnd:operand a=
    a:=pcm_allocz(opndrec.bytes)
    return a
end

global func duplopnd(operand a)operand=
    operand b
    b:=pcm_alloc(opndrec.bytes)
    b^:=a^
    return b
end

export func genxreg(int xreg=rnone,size=8)operand=
    operand a

    a:=newmclopnd()

    a.mode:=a_xreg
    if xreg=rnone then
        a.reg:=getnextxreg()
    else
        a.reg:=xreg
        setregbit(xreg)
    fi
    a.size:=size
    return a
end

export func genindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, symbol def=nil)operand=
    operand a
    a:=newmclopnd()

    a.mode:=a_mem
    a.reg:=areg

    a.regix:=ireg
    a.scale:=scale
    a.size:=size

    a.offset:=offset

    if labno then
        a.value:=labno
        a.valtype:=label_val
    elsif def then
        a.def:=def
        ++def.nrefs
        a.valtype:=def_val
        if isframex(def) then
            a.reg:=rframe
        fi
    fi

    return a
end

global func getmclstr:ref strbuffer=
    symbol d,e
    ref mclrec m
    [32]char str2,str3
    int i

    gs_init(dest)

    for i to nlibfiles when libfiles[i]^<>'$' do
        asmstr("          ")
        asmstr((libtypes[i]='D'|"importdll "|"importlib "))
        asmstr(libfiles[i])
        gs_line(dest)
    od

    m:=mccode
    i:=1
    while m do
        writemcl(i,m)
        ++i
        m:=m.nextmcl
    od

    return dest
end

global proc gencomment(ichar s,t=nil)=
    [300]char str

    if s=nil or s^=0 then
        genmc(m_blank)
    elsif t then
        strcpy(str,s)
        strcat(str," ")
        strcat(str,t)
        genmc(m_comment,gencommentstring(str))
    else
        genmc(m_comment,gencommentstring(s))
    fi
end

global func genstring(ichar s,int length=-1)operand=
    operand a
    return genlabel(getstringindex(s))
end

global func gencommentstring(ichar s,int length=-1)operand=
    operand a
    a:=newmclopnd()
    a.mode:=a_imm
    if length<0 then
        length:=strlen(s)
    fi
    a.svalue:=pcm_alloc(length+1)
    memcpy(a.svalue,s,length)
    (a.svalue+length)^:=0

    a.valtype:=stringimm_val
    a.size:=8
    return a
end

global func genname(ichar s)operand=
    [64]char str
    operand a

    a:=newmclopnd()
    a.mode:=a_imm
    a.svalue:=pcm_copyheapstring(s)
    a.valtype:=name_val
    a.size:=8

    return a
end


proc writemcl(int index,ref mclrec mcl)=

    case mcl.opcode
    when m_deleted then
    else
        strmcl(mcl)
        gs_line(dest)
    esac
end

global proc strmcl(ref mclrec mcl)=
    static [512]char str
    [128]char opcname
    operand a,b
    int opcode,cond,sizepref
    ichar s,comment
    symbol d

    opcode:=mcl.opcode



    cond:=mcl.cond
    a:=mcl.a
    b:=mcl.b
    comment:=nil

    case opcode
    when m_procstart then
        asmstr(";Proc ")
        asmstr(a.def.name)
        currasmproc:=a.def

        return

    when m_procend then
        asmstr(";End ")
        currasmproc:=nil

        return

    when m_blank then
        return
    when m_comment then
        asmchar(';')
        asmstr(a.svalue)
        GOTO DOCOMMENTS
        return
    when m_deleted then
        asmstr("; <deleted>")
        GOTO DOCOMMENTS
        return

    when m_labelname then               
        d:=a.def
        case a.valtype
        when def_val then
            asmstr(getdispname(d))
        when stringimm_val then
            asmstr(a.svalue)
            return
        else
            merror("strmcl/lab")
        esac

        asmstr(":")

        if d.scope=export_scope then
            asmstr("\n")
            asmstr(d.name)
            asmstr("::")
        fi
        return

    when m_labelx then
        fprint @&.str,"L#:",a.value
        asmstr(&.str)
        return

    when m_define then
        asmstr("          ")
        asmstr(a.svalue)
        asmstr(" = ")
        asmopnd(b)
        return

    when m_definereg then
        asmstr("          ")
        asmstr(a.svalue)
        asmstr(" = ")

        case b.mode
        when a_reg then
            asmstr(getregname(b.reg, b.size))
        else
            asmstr(getxregname(b.reg, b.size))
        esac
        return

    when m_programend then
        asmstr(";         programend")
        return

    esac

    case opcode
    when m_jmpcc then
        print @&.opcname,"j",,asmcondnames[cond]

    when m_setcc then
        print @&.opcname,"set",,asmcondnames[cond]

    when m_cmovcc then
        print @&.opcname,"cmov",,asmcondnames[cond]

    when m_call then
        strcpy(&.opcname,"call")
    when m_andx then
        strcpy(&.opcname,"and")
    when m_orx then
        strcpy(&.opcname,"or")
    when m_xorx then
        strcpy(&.opcname,"xor")
    when m_notx then
        strcpy(&.opcname,"not")

    ELSIF OPCODE>M_HALT THEN
        STRCPY(&.OPCNAME,STRINT(OPCODE))

    else
        strcpy(&.opcname,mclnames[opcode]+2)
    esac
    ipadstr(&.opcname,10," ")

    if not fasmformat then
        if a and b then
            fprint @&.str,"  #/#",a.size,b.size
        elsif a then
            fprint @&.str,"  #",a.size
        else
            strcpy(&.str,"  ")
        fi
    else
        strcpy(&.str,"  ")
    fi

    ipadstr(&.str,10)

    strcat(&.str,&.opcname)

    asmstr(&.str)

    if a and b then     
        sizepref:=needsizeprefix(opcode,a,b)
        asmopnd(a,sizepref)
        asmstr(",   ")
        asmopnd(b,sizepref)


    elsif a and a.mode then                             
        if opcode=m_call then
            asmopnd(a,0)
        else
            asmopnd(a,1)
        fi
    fi


DOCOMMENTS::
end

global proc asmopnd(operand a,int sizeprefix=0,debug=0)=
    asmstr(stropnd(a,sizeprefix,debug))
end

global proc setsegment(int seg,align=1)=
    int opc,oldalign

    if seg<>currsegment then
        case seg
        when 'I' then opc:=m_isegment
        when 'Z' then opc:=m_zsegment
        when 'C' then opc:=m_csegment
        when 'R' then MERROR("CAN'T DO RODATA SEG")
        else
            MERROR("BAD SEG CODE")
        esac
        if mccodex and mccodex.opcode in [m_isegment,m_zsegment,m_csegment] then
            mccodex.opcode:=opc
        else
            genmc(opc)
        fi

        currsegment:=seg
    fi

    if align>1 then
        if mccodex.opcode=m_align then
            oldalign:=mccodex.a.value
            if oldalign>=align then return fi
        fi
        genmc(m_align,genint(align))
    fi
end

global func getsizeprefix(int size,enable=0)ichar=
    if not enable then return "" fi
    case size
    when 1 then return "byte "
    when 2 then return "word16 "
    when 4 then return "word32 "
    when 8 then return "word64 "
    esac
    return ""
end

global func needsizeprefix(int opcode,operand a,b)int=
    case opcode
    when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
        return 1

    when m_cvtss2si,m_cvtsd2si, m_cvttss2si,m_cvttsd2si then
        return 1
    when m_shl, m_shr, m_sar then
        if a.mode=a_mem then return 1 fi
        return 0
    esac

    if a.mode=a_reg or a.mode=a_xreg or b.mode=a_reg or b.mode=a_xreg then
        return 0
    fi
    return 1
end

global func changeopndsize(operand a,int size)operand=
    operand b

    if a.size<>size then
            b:=duplopnd(a)
            b.size:=size
        return b
    fi
    return a
end

global func makeopndind(operand a,int size=0)operand=
    operand b

    if a.mode<>a_reg then
        merror("makeopndind")
    fi

    return genireg(a.reg,size)
end

global func applyoffset(operand a,int offset,int size=0)operand=
    operand b

    if offset=0 and size=0 then
        return a
    fi
    b:=duplopnd(a)
    b.offset:=b.offset+offset
    if size then
        b.size:=size
    fi

    return b
end

export func genint(int64 x,int size=8)operand a=
    a:=newmclopnd()
    a.mode:=a_imm

    a.value:=x
    a.valtype:=intimm_val
    a.size:=size

    return a
end

global func genrealmem(real64 x,int mode=tr64)operand a=
    a:=newmclopnd()
    a.mode:=a_mem
    if mode=tr64 then
        a.value:=getrealindex(x)
        a.size:=8
    else
        a.value:=getreal32index(x)
        a.size:=4
    fi
    a.valtype:=label_val
    return a
end

global func genrealimm(real64 x,int mode=tr64)operand a=
    a:=newmclopnd()
    a.mode:=a_imm
    a.xvalue:=x
    a.valtype:=realimm_val
    a.size:=ttsize[mode]
    return a
end

export func genlabel(int x=0)operand a=
    a:=newmclopnd()
    a.mode:=a_imm
    if x=0 then x:=++mlabelno fi
    a.value:=x
    a.valtype:=label_val
    return a
end

global func genlabelmem(int x)operand a=

    a:=genlabel(x)
    a.mode:=a_mem
    return a
end

global func genregvar(symbol d)operand a=
    a:=genreg(d.reg,8)
    setbit(isregvar,d.reg,1)

    return a
end

global func genxregvar(symbol d)operand a=
    a:=genxreg(d.reg)
    setbit(isregvar,d.reg,1)

    return a
end

global func genmem(symbol d)operand a=
    int reg, size

    if d.reg then
        if ttisreal[d.mode] then
            return genxregvar(d)
        else
            return genregvar(d)
        fi
    fi

    reg:=rnone
    if isframex(d) then

        reg:=rframe
    fi

    if d.atvar and d.equivvar then
        d:=d.equivvar.def
    fi

    a:=newmclopnd()
    a.mode:=a_mem
    a.reg:=reg
    a.def:=d
    ++d.nrefs
    a.valtype:=def_val

    setopndsize(a,ttsize[d.mode])

    return a
end

export func genmemaddr(symbol d)operand=
    operand a

    if d.atvar and d.equivvar then
        d:=d.equivvar.def
    fi

    d.addrof:=1
    ++d.nrefs

    a:=newmclopnd()
    a.mode:=a_imm

    a.def:=d
    ++d.nrefs
    a.valtype:=def_val
    a.size:=8

    return a
end

global func genreg(int reg=rnone,size=8)operand=
    return genreg0(reg,size)
end

global func genreg0(int reg,size=8)operand a=

    if reg>=xr0 then
        return genxreg(reg,size)
    fi


    a:=newmclopnd()
    a.mode:=a_reg
    if reg=rnone then
        a.reg:=getnextreg()
    else
        a.reg:=reg
        setregbit(reg)
    fi
    a.size:=size
    return a
end

global func genireg(int reg,size=8,offset=0)operand=
    operand a

    a:=newmclopnd()
    a.mode:=a_mem
    a.reg:=reg
    setregbit(reg)
    setopndsize(a,size)
    a.offset:=offset
    return a
end

global func roundsizetg(int size)int=
    while size iand (targetsize-1) do ++size od
    return size
end

global func getregname(int reg,size=8)ichar=
    static [1..17]ichar prefix=("B","W","","A","","","","D","","","","","","","","Q","N")
    static [32]char str
    [16]char str2
    ichar rs

    if reg>=xr0 then
        return getxregname(reg, size)
    fi

    case reg
    when rnone then return "-"
    when rframe then rs:="frame"
    when rstack then rs:="stack"
    else
        getstrint(reg-r0,&.str2)
        rs:=&.str2
    esac

    if size>8 then size:=8 fi

    print @&.str,prefix[size],,rs
    return &.str
end

global func getxregname(int reg,size=8)ichar=
    static [32]char str

    if fasmformat then
        print @&.str,"XMM",,reg-xr0
    else
        print @&.str,(size=8|"DX"|"SX"),,reg-xr0
    fi
    return &.str
end

global func sameoperand(operand a,b)int=
    return memcmp(a,b,opndrec.bytes)=0
end

global func sameregopnd(operand a,b)int=
    unless a.mode=b.mode=a_reg then return 0 end
    return a.reg=b.reg
end

global func getstringindex(ichar s)int=
    if s=nil then           
        kk0used:=++mlabelno
        return kk0used
    fi

    if cstringlist and eqstring(cstringlist.svalue,s) then
        return cstringlist.labelno
    fi

    return addconst(cstringlist, cast(s))
end

global func addconst(ref constrec &clist, int value)int=
    ref constrec p
    p:=pcm_allocz(constrec.bytes)
    p.value:=value
    p.labelno:=++mlabelno
    p.nextconst:=clist
    clist:=p
    return mlabelno
end

global func getrealindex(real x)int=
    return addconst(creallist,cast@(x,int))
end

global func getreal32index(real x)int=
    return addconst(creal32list,cast@(x,int))
end

proc asmstr(ichar s)=
    gs_str(dest,s)
end

proc asmchar(int c)=
    gs_char(dest,c)
end

global func getdispname(symbol d)ichar=
    static [256]char str
    ichar name

    if fshortnames then
        return d.name
    fi

    name:=getfullname(d)

    if d.reg then
        fprint @str,"#.#",(d.isfloat|"X"|"R"), name
        return str
    fi


    if d.truename and d.isimport then
        strcpy(str,"`")
        strcat(str,d.truename)
        strcat(str,"*")
    elsif d.isimport then
        strcpy(str,name)
        strcat(str,"*")
    else        
        return name
    fi

    return str

end 

export proc merror(ichar mess,ichar param="")=
    fprintln "MCL Error: # (#) on Line: # in #",mess,param,
        getlineno(mlineno), sourcefilenames[getfileno(mlineno)]
    stopcompiler(sourcefilespecs[getfileno(mlineno)],getlineno(mlineno))
end

export proc merrort(ichar mess,int t)=
    [300]char str
    fprint @str, "MCL Type not supported for (#)",mess
    merror(str, ttname[t])
end


global proc genstringtable=
    ref constrec p

    return unless cstringlist

    gencomment("String Table")

    setsegment('I',8)

    if kk0used then
        genmc(m_labelx,genlabel(kk0used))
        gendb(0)
    fi

    p:=cstringlist
    while p, p:=p.nextconst do
        genmc(m_labelx,genlabel(p.labelno))
        genstringimm(p.svalue,1)
    od
end

global proc genstringimm(ichar s, int doterm)=
    int i, c, seqlen, length
    ref char seq

    length:=strlen(s)
    if length=0 then
        gendb(0)
        return
    fi

    seqlen:=0

    to length do
        c:=s++^
        if c<32 or c>=127 or c='\"' then
            if seqlen then
                gendbstring(seq, seqlen)
                seqlen:=0
            fi
            gendb(c)
        else
            if seqlen=0 then
                seqlen:=1
                seq:=s-1
            else
                ++seqlen
            fi
        fi
    od
    if seqlen then
        gendbstring(seq,seqlen)
    fi
    if doterm then
        gendb(0)
    fi
end

proc gendb(int a)=
    genmc(m_db,genint(a))
end

proc gendbstring(ichar s, int length)=
    genmc(m_db,gencommentstring(s,length))
end

proc gendq(int a)=
    genmc(m_dq,genint(a))
end

global proc genrealtable=
    ref constrec p

    return unless creallist or creal32list

    gencomment("Real Table")
    setsegment('I',8)
    p:=creallist
    while p, p:=p.nextconst do
        genmc(m_labelx,genlabel(p.labelno))
        const inf=1.0/0.0
        if p.xvalue=(inf) then
            genmc(m_dq, genint(u64@(p.xvalue)))
        else
            genmc(m_dq, genrealimm(p.xvalue,8))
        fi
    od

    gencomment("Real32 Table")
    p:=creal32list
    while p, p:=p.nextconst do
        genmc(m_labelx,genlabel(p.labelno))
        if p.xvalue=(inf) then
            genmc(m_dd, genint(u32@(real32(p.xvalue))))
        else
            genmc(m_dd, genrealimm(p.xvalue,4))
        fi

    od
end

global proc genabsneg=
    setsegment('I',16)

    if lababs32 then
        gencomment("lababs32")
        genmc(m_labelx,genlabel(lababs32))
        gendq(0x7FFF'FFFF'7FFF'FFFF)
        gendq(0x7FFF'FFFF'7FFF'FFFF)
    fi
    if lababs64 then
        gencomment("lababs64")
        genmc(m_labelx,genlabel(lababs64))
        gendq(0x7FFF'FFFF'FFFF'FFFF)
        gendq(0x7FFF'FFFF'FFFF'FFFF)
    fi

    if labneg32 then
        gencomment("labneg32")
        genmc(m_labelx,genlabel(labneg32))
        gendq(0x8000'0000'8000'0000)
        gendq(0x8000'0000'8000'0000)
    fi
    if labneg64 then
        gencomment("labneg64")
        genmc(m_labelx,genlabel(labneg64))
        gendq(0x8000'0000'0000'0000)
        gendq(0x8000'0000'0000'0000)
    fi

    if labzero then
        gencomment("labzero")
        genmc(m_labelx,genlabel(labzero))
        gendq(0)
    fi

    if labmask63 then
        gencomment("mask63/offset64")
        genmc(m_labelx,genlabel(labmask63))
        gendq(0x7FFF'FFFF'FFFF'FFFF)
        genmc(m_labelx,genlabel(laboffset64))
        gendq(0x43E0'0000'0000'0000)
    fi
end

global func createfwdlabel:int =
    return ++mlabelno
end

global proc definefwdlabel(int lab) =
    genmc(m_labelx,genlabel(lab))
end

global func definelabel:int =
    genmc(m_labelx,genlabel(++mlabelno))
    return mlabelno
end

global func stropnd(operand a,int sizeprefix=0,debug=0)ichar=
    static [512]char str
    [128]char str2
    ichar plus,t
    int offset,tc


    str[1]:=0

    case a.mode
    when a_reg, a_xreg then
        return strreg(a.reg, a.size)

    when a_imm then
        strcpy(str,strvalue(a))

    when a_mem then
        case a.valtype
        when intimm_val then
            strcpy(str,strint(a.value))
        when realimm_val then
            strcpy(str,strreal(a.xvalue))
        when realmem_val then
            fprint @str,"M#",a.xvalue
        esac

        strcat(str,getsizeprefix(a.size,sizeprefix))
        strcat(str,"[")

        plus:=""
        if a.reg then
            strcat(str,strreg(a.reg,8))
            plus:="+"
        fi
        if a.regix then
            strcat(str,plus)
            strcat(str,strreg(a.regix,8))
            plus:="+"
            if a.scale>1 then
                strcat(str,"*")
                strcat(str,strint(a.scale))
            fi
        fi

        if a.valtype in [def_val,label_val] then
            if plus^='+' then
                strcat(str,plus)
            fi
            strcat(str,strvalue(a))
        elsif offset:=a.offset then
            print @str2,offset:"+"
            strcat(str,str2)
        fi
        strcat(str,"]")




    else
        println "BAD OPND",A.MODE
        return "<BAD OPND>"
    esac

    return str
end

func strreg(int reg, size=8)ichar=
    symbol d

    d:=checkregvar(reg,0)

    if size=8 and d then
        return getdispname(d)
    else
        getregname(reg,size)
    fi
end

func checkregvar(int reg, isfloat)symbol d=
RETURN NIL
end

global func strvalue(operand a)ichar=
    static [512]char str
    [128]char str2
    symbol def
    int64 value,offset,length
    ichar ss

    def:=a.def
    value:=a.value

    strcpy(&.str,"")

    case a.valtype
    when def_val then
        strcat(&.str,getdispname(def))

    addoffset::
        if offset:=a.offset then
            print @&.str2,(offset>0|"+"|""),,offset
            strcat(&.str,&.str2)
        fi

    when intimm_val then
        strcat(&.str,strint(value))

    when realimm_val then
        print @&.str,a.xvalue:"20.20"

    when realmem_val then
        strcat(&.str,"M")
        strcat(&.str,strreal(a.xvalue))

    when stringimm_val then
        strcat(&.str,"""")
        strcat(&.str,a.svalue)
        strcat(&.str,"""")

    when name_val then
        strcat(&.str,a.svalue)

    when label_val then
        strcat(&.str,"L")
        strcat(&.str,strint(a.labelno))
        goto addoffset

    esac

    return &.str

end

func makesimpleaddr(operand ax)operand=
    int reg

    if ax.reg and ax.regix=0 and ax.valtype=no_val then     
        return ax
    fi

    reg:=getnextreg()

    genmc(m_lea, genreg(reg), ax)
    return genireg(reg)
end

func makeblockaddr(operand ax)operand=
    if ax.mode=a_reg then
        return makeopndind(ax)
    fi

    return makesimpleaddr(ax)
end

global proc clearblock(operand ax, int n)=

    operand rx, rcount
    int nwords,lab,oddbytes,offset,workreg, countreg

    ax:=makeblockaddr(ax)

    oddbytes:=n rem 8       
    n-:=oddbytes            
    nwords:=n/8             

    rx:=genreg(workreg:=getnextreg())       
    genmc(m_xorx,rx,rx)

    offset:=0

    if 1<=nwords<=8 then        
        ax:=changeopndsize(ax,targetsize)

        to nwords do
            genmc(m_mov,applyoffset(ax,offset),rx)
            offset+:=8
        od

    elsif nwords<>0 then        


        if nwords iand 3 then       

            rcount:=genreg(countreg:=getnextreg())  
            lab:=++mlabelno

            ax:=makesimpleaddr(ax)

            genmc(m_mov,rcount,genint(nwords))
            genmc(m_labelx,genlabel(lab))
            genmc(m_mov,ax,rx)

            genmc(m_add,genreg(ax.reg),genint(targetsize))

            genmc(m_dec,rcount)
            genmc_cond(m_jmpcc,ne_cond,genlabel(lab))

            offset:=0
            freereg(countreg)
        else
            rcount:=genreg(countreg:=getnextreg())  
            lab:=++mlabelno

            ax:=makesimpleaddr(ax)
            genmc(m_mov,rcount,genint(nwords/4))
            genmc(m_labelx,genlabel(lab))

            for i to 4 do
                genmc(m_mov,applyoffset(ax,offset),rx)
                offset+:=8
            od

            genmc(m_add,genreg(ax.reg),genint(targetsize*4))

            genmc(m_dec,rcount)
            genmc_cond(m_jmpcc,ne_cond,genlabel(lab))

            offset:=0
            freereg(countreg)
        fi
    fi

    if oddbytes then
        n:=oddbytes                     

        if n>=4 then
            rx:=changeopndsize(rx,4)
            genmc(m_mov,applyoffset(ax,offset,4),rx)
            n-:=4
            offset+:=4
        fi
        if n>=2 then
            rx:=changeopndsize(rx,2)
            genmc(m_mov,applyoffset(ax,offset,2),rx)
            n-:=2
            offset+:=2
        fi
        if n=1 then
            rx:=changeopndsize(rx,1)
            genmc(m_mov,applyoffset(ax,offset,1),rx)
        fi
    fi

    freereg(workreg)
end

global proc copyblock(operand ax,bx, int n, savedest=1)=

    operand rx, rcount
    int nwords,lab,oddbytes,offset,workreg, countreg, axreg, regs

GENCOMMENT("COPY BLOCK")
    ax:=makeblockaddr(ax)
    bx:=makeblockaddr(bx)
GENCOMMENT(STROPND(AX))
GENCOMMENT(STROPND(BX))

    regs:=regset

    oddbytes:=n rem 8       
    n-:=oddbytes            
    nwords:=n/8             

    rx:=genreg()            

    offset:=0

    if 1<=nwords<=4 then        
        ax:=changeopndsize(ax,targetsize)
        bx:=changeopndsize(bx,targetsize)

        to nwords do
            genmc(m_mov,rx,applyoffset(bx,offset))
            genmc(m_mov,applyoffset(ax,offset),rx)
            offset+:=8
        od

    elsif nwords<>0 then        
        rcount:=genreg(countreg:=getnextreg())  
        lab:=++mlabelno
        if savedest then
            axreg:=ax.reg
            genmc(m_push, genreg(axreg))
        fi

        ax:=makesimpleaddr(ax)
        bx:=makesimpleaddr(bx)
        ax.size:=bx.size:=8

        genmc(m_mov,rcount,genint(nwords))
        genmc(m_labelx,genlabel(lab))
        genmc(m_mov,rx,bx)
        genmc(m_mov,ax,rx)

        genmc(m_add,genreg(ax.reg),genint(targetsize))
        genmc(m_add,genreg(bx.reg),genint(targetsize))

        genmc(m_dec,rcount)
        genmc_cond(m_jmpcc,ne_cond,genlabel(lab))
        if savedest then
            genmc(m_pop, genreg(axreg))
        fi

        offset:=0
        freereg(countreg)
    fi

    if oddbytes then
        n:=oddbytes                     

        if n>=4 then
            rx:=changeopndsize(rx,4)
            genmc(m_mov,rx,applyoffset(bx,offset,4))
            genmc(m_mov,applyoffset(ax,offset,4),rx)
            n-:=4
            offset+:=4
        fi
        if n>=2 then
            rx:=changeopndsize(rx,2)
            genmc(m_mov,rx,applyoffset(bx,offset,2))
            genmc(m_mov,applyoffset(ax,offset,2),rx)
            n-:=2
            offset+:=2
        fi
        if n=1 then
            rx:=changeopndsize(rx,1)
            genmc(m_mov,rx,applyoffset(bx,offset,1))
            genmc(m_mov,applyoffset(ax,offset,1),rx)
        fi
    fi

    popregs(regs)
end

global proc genfunctable=
    [256]char str
    ichar s,t
    ref procrec pp
    int firststringlab,nextlab,nprocs


    if lab_funcaddrtable=0 then return fi
    gencomment("Function Table")
    nprocs:=0
    setsegment('C',16)
    genmc(m_labelx, genlabel(lab_funcaddrtable))

    pp:=proclist
    while pp, pp:=pp.nextproc do
        genmc(m_dq, genmemaddr(pp.def))
        ++nprocs
    od

    firststringlab:=0
    genmc(m_labelx, genlabel(lab_funcnametable))

    pp:=proclist
    while pp, pp:=pp.nextproc do
        if firststringlab=0 then
            firststringlab:=nextlab:=++mlabelno
        else
            nextlab:=++mlabelno
        fi

        genmc(m_dq, genlabel(nextlab))
    od

    nextlab:=firststringlab
    pp:=proclist
    while pp, pp:=pp.nextproc do
        genmc(m_labelx, genlabel(nextlab))
        s:=pp.def.name
        t:=s

        while s^ do
            if s^='.' then
                t:=s+1
            fi
            ++s
        od
        genstringimm(t,1)
        ++nextlab
    od

    genmc(m_labelx, genlabel(lab_funcnprocs))
    genmc(m_dq, genint(nprocs))
end

global func genextname(ichar s)operand=
    [64]char str
    symbol d

    strcpy(&.str,s)
    str[strlen(s)]:=0

    d:=pcm_allocz(strec.bytes)

    d.name:=pcm_copyheapstring(&.str)
    d.isimport:=1

    return genmemaddr(d)
end


global proc domcl_assem(unit pcode)=
    return when not pcode or pcode.tag<>jassem

    inf_assem:=1

    genmc(pcode.asmopcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
    mccodex.cond:=pcode.cond

    case pcode.asmopcode
    when m_pcmpistri,m_pcmpistrm then
        if pcode.c=nil or pcode.c.tag<>jconst then gerror("pcmpistr/no imm") fi
        mccodex.c:=pcode.c.value

    esac

end

func genasmopnd(unit p)operand ax=
    symbol d
    int offset,labno
    unit a              
    unit x,y
    symbol e

    if p=nil then return nil fi

    case p.tag
    when jassemreg then
        ax:=genreg(p.reg,p.regsize)

    when jconst then
        ax:=genint(p.value)

    when jassemmem then
        a:=p.a
        d:=nil
        offset:=labno:=0

        if a then
            case a.tag
            when jconst then
                offset:=a.value
            when jname then
                d:=a.def
                if d.nameid=labelid then
                    labno:=d.labelno
                    d:=nil
                fi
            when jbin then
                x:=a.a
                y:=a.b
                if x.tag=jname and y.tag=jconst then
                    d:=x.def
                    if d.nameid=labelid then
                        labno:=d.labelno
                        d:=nil
                    fi
                else
                    goto error
                fi
                offset:=(a.pclop in [kadd,kaddrefoff]|y.value|-y.value)
            when junary then
                if a.pclop<>kneg then merror("assume/unary") fi
                unless a.a.tag=jconst then gerror("-name") end
                offset:=-a.a.value
            when jsyscall then
MERROR("ASSEM/SYSFN?")

            else
error::
                cpl jtagnames[a.tag]
                gerror("Can't do memexpr")
            esac
        fi
        ax:=genindex(areg:p.reg, ireg:p.regix, scale:p.scale, size:ttsize[p.prefixmode],
            offset:offset, labno:labno, def:d)

    when jname then
        d:=p.def
        if d.nameid=labelid then
            if d.labelno=0 then
                d.labelno:=++mlabelno
            fi

            labno:=d.labelno

            ax:=genlabel(labno)
        else
            ax:=genmemaddr(d)
        fi

    when jassemxreg then
        ax:=genxreg(p.reg)
    when jbin then              
        x:=p.a
        y:=p.b
        if x.tag=jname and y.tag=jconst then
            d:=x.def
            offset:=(p.pclop in [kadd,kaddrefoff]|y.value|-y.value)
            if d.nameid=labelid then
                labno:=d.labelno
                ax:=genlabel(labno)
            else
                ax:=genmemaddr(d)
            fi
            ax.offset:=offset
        else
            gerror("ax:imm/add")
        fi
    else
        cpl jtagnames[p.tag]
        gerror("genasmopnd?")
    esac

    return ax

end

global func getnextreg:int=

    int reg


    for r:=r0 to regmax do
        if getregbit(r)=0 then
            setregbit(r)
            inf_highreg max:=r
            ++nregs
            return r
        fi
    od

    merror("GNR: no more regs")
    0
end

global func getnextxreg:int=
    int reg,firstreg

    for r:=xr4 to xregmax do
        if getregbit(r)=0 then
            setregbit(r)
            inf_highxreg max:=r
            ++nxregs
            return r
        fi
    od

    merror("GNXR: no more regs")
    0
end

global macro freereg(r) =
    (clrregbit( r); ++nregs)

global proc pushstack(int n)=
    if n then
        genmc(m_sub,dstackopnd,genint(n))
    fi
end

global proc popstack(int n)=
    if n then
        genmc(m_add,dstackopnd,genint(n))
    fi
end


global func showregset(ichar caption)ichar=
    static [300]char str

    print @str,caption:"8JL",$
    for i in r0..xr15 do
        if i in [r14,r15] or i in r16..r19 then
            next
        fi

        strcat(str, strint(getregbit(i)))
        strcat(str, " ")

        if i in [r2,r9,r13, xr3,xr5] then
            strcat(str, " ")
            if i=r13 then
                strcat(str, "// ")
            fi

        fi
    od
    str
end

global proc stacklooplabels(int a,b,c)=
    ++loopindex
    if loopindex>maxnestedloops then
        gerror("Too many nested loops")
    fi

    loopstack[loopindex,1]:=a
    loopstack[loopindex,2]:=b
    loopstack[loopindex,3]:=c

end

global func findlooplabel(int k,n)int=
    int i

    i:=loopindex-(n-1)      
    if i<1 or i>loopindex then gerror("Bad loop index") fi
    return loopstack[i,k]
end

global func getopndmov(operand ax)int =
    case ax.mode
    when a_xreg then
        return (ax.size=4|m_movd|m_movq)
    when a_reg then
        return m_mov
    else
        gerror("getopndmov?")
    esac
end

global func getregopnd(unit a)operand =

    if ttisreal[a.mode] then
        genxreg(getnextxreg())
    else
        genreg(getnextreg())
    fi
end

global func getmclcond(int cond, mode)int=
    static [0..5]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
    static [0..5]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

    if ttsigned[mode] then
        scondcodes[cond-keq]
    else
        ucondcodes[cond-keq]
    fi
end

global proc popregs(u64 regs, operand ax=nil)=
    regset:=regs
    if ax then
        if ax.reg then setregbit(ax.reg) fi
        if ax.regix then setregbit(ax.regix) fi
    fi
end

global func getlowreg(operand ax)int reg=
    reg:=0

    if ax.reg and ax.reg<>rframe then
        reg:=ax.reg

    fi
    if ax.regix and ax.regix<>rframe then
        if reg then
            reg:=min(reg, ax.regix)
        else
            reg:=ax.regix
        fi
    fi
    if reg then return reg fi
    getnextreg()
end

global proc mulimm(operand ax, int n)=
    int shifts,m

    case n
    when 0 then
        genmc(m_xorx, ax,ax)
        return
    when 1 then
        return
    when -1 then
        genmc(m_neg, ax)
        return
    esac

    shifts:=0
    m:=n

    while m.even do
        m>>:=1
        ++shifts
    od

    if shifts then
        genmc(m_shl, ax, genint(shifts))
    fi

    case m
    when 1 then
        return
    when 3, 5, 9 then
        genmc(m_lea, ax, genindex(areg: ax.reg, ireg:ax.reg, scale:m-1))
    else                        
        if shifts then
            mccodex.opcode:=m_imul2
            mccodex.b:=genint(n)
        else
            genmc(m_imul2, ax, genint(n))
        fi
    esac

end

global function scaleindex(operand ax, int scale)int=
    int n
    if scale in [1,2,4,8] then return scale fi
    mulimm(ax,scale)
    return 1
end

global func gensysfn(int fnindex, unit a=nil,b=nil,c=nil)operand=
    gensysproc(fnindex, a,b,c, 1)
end

global func gensysproc(int fnindex, unit a=nil,b=nil,c=nil, int asfunc=0)operand=
    [maxparams]unit arglist
    [maxparams]byte argcomplex      
    int nargs, highcomplex, j, stackbytes, oldstackdepth
    symbol d

    ++ncalldepth
    oldstackdepth:=mstackdepth

    arglist[1]:=a
    arglist[2]:=b
    arglist[3]:=c
    nargs:=0
    for i to 3 do
        if arglist[i] then ++nargs fi
    od

    stackbytes:=0

    if mstackdepth.odd then

        stackbytes:=8
        pushstack(8)
        ++mstackdepth
    fi

    highcomplex:=0
    for i to nargs do
        argcomplex[i]:=j:=(arglist[i].simple|0|2)       
        if j then highcomplex:=i fi
    od

    if highcomplex then argcomplex[highcomplex]:=1 fi   

    for i to nargs when argcomplex[i] do
        if i<>highcomplex then
            pushunit(arglist[i])
        fi
    od

    if highcomplex then             
        loadarg(arglist[highcomplex], highcomplex)
    fi

    for i to nargs when argcomplex[i]=0 do
        loadarg(arglist[i], i)
    od

    for i:=nargs downto 1 when argcomplex[i]=2 do
        poptoarg(arglist[i].mode, i)
    od


    if mstackdepth then
        stackbytes+:=32
        pushstack(32)
        mstackdepth+:=4
    fi

    d:=getsysfnhandler(fnindex)
    if d then
        genmc(m_call, genmemaddr(d))
    else
        genmc(m_call, genname(sysfnnames[fnindex]+3))
    fi

    if stackbytes then
        popstack(stackbytes)
    fi
    mstackdepth:=oldstackdepth

    --ncalldepth

    if not asfunc then return nil fi

    return genreg(r0)           
end

global function getsysfnhandler(int fn)symbol p=
    [300]char str
    int report

    if sysfnhandlers[fn] then
        return sysfnhandlers[fn]
    fi

    strcpy(str,"m$")
    strcat(str,sysfnnames[fn]+3)    

    ref procrec pp:=proclist
    while pp, pp:=pp.nextproc do
        if eqstring(pp.def.name, str) then
            sysfnhandlers[fn]:=pp.def
            return pp.def
        fi
    od

    report:=passlevel>asm_pass

    if report then
        println "Sysfn not found:",&.str
    fi
    if fn<>sf_unimpl then
        p:=getsysfnhandler(sf_unimpl)
        if p=nil and report then
            gerror("No m$unimpl")
        fi
        return p
    fi

    return nil
end

proc setbit(u64 &a, int n, x)=
    if x then
        a ior:= 1<<n
    else
        a iand := inot (1<<n)
    fi
end

func getbit(u64 a, int n)int=
    return (a>>n) iand 1
end

global proc setregbit(int n)=
    regset ior:= 1<<n
end

global proc clrregbit(int n)=
    regset iand := inot (1<<n)
end

global func getregbit(int n)int=
    return (regset>>n) iand 1
end

global proc setxregbit(int n)=
    regset ior:= 1<<(n+32)
end

global proc clrxregbit(int n)=
    regset iand := inot (1<<(n+32))
end

global func getxregbit(int n)int=
    return (regset>>(n+32)) iand 1
end

global proc setopndsize(operand ax, int size)=
    case size
    when 1,2,4,8 then
        ax.size:=size
    when 0 then
        ax.size:=8
    else
        ax.size:=0
    esac
end

global func getretopnd(int mode)operand ax=
    int reg, m
    ref[]int32 pmult
    [10]operand axmult

    if ttisreal[mode] then
        if getregbit(xr0) then gerror("ret:xr0 in use") fi
        genxreg(xr0,ttsize[mode])

    elsif ttbasetype[mode]=ttuple then
        nmultregs:=ttlength[mode]
        pmult:=ttmult[mode]

        for i to nmultregs do
            m:=pmult[i]
            if ttisreal[m] then
                reg:=xr0+i-1
            else
                reg:=r0+i-1
            fi
            if getregbit(xr0) then gerror("retmult:reg in use") fi
            axmult[i]:=genreg(reg,size:ttsize[mode])
            multregs[i]:=reg
        od
        return axmult[1]

    else
        if getregbit(r0) then
            GENCOMMENT("RET/R0 IN USE")
            cpl("RET/R0 IN USE"),CURRPROC.NAME
            
         fi

        ax:=genreg(r0)
        if ttcat[mode]=shortcat then
            genmc((ttsigned[mode]|m_movsx|m_movzx), ax, genreg(r0,ttsize[mode]))
        fi
        genreg(r0)
    fi
end

=== mc_decls.m 0 0 20/33 ===
export type operand = ref opndrec

export record opndrec =     
    union
        symbol def      
        int64 value     
        real64 xvalue   
        ichar svalue    
        int labelno     
        int sysfn       
    end

    u16 misc: (         
        size:4,         
        scale:4,        
        mode:4,         
        valtype:4)
    byte reg            
    byte regix          
    i32 offset          
end

export record mclrec =      
    ref mclrec nextmcl
    operand a,b
    byte opcode
    byte cond
    byte c                  
    byte spare
    u32 seqno


end

global enumdata [0:]ichar valtypenames =
    (no_val=0,      $),     
    (intimm_val,    $),     
    (realimm_val,   $),     
    (realmem_val,   $),     
    (stringimm_val, $),     
    (comment_val,   $),     
    (def_val,       $),     
    (label_val,     $),     
    (name_val,      $),     
end

export enumdata [0:]ichar opndnames =
    (a_none=0,  $),
    (a_reg,     $),     
    (a_xreg,    $),     
    (a_imm,     $),     
    (a_mem,     $),     
end

export enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

    (m_procstart,       $,      0,      0),     
    (m_procend,         $,      0,      0),     
    (m_programend,      $,      0,      0),     
    (m_comment,         $,      0,      0),     
    (m_blank,           $,      0,      0),     
    (m_deleted,         $,      0,      0),     
    (m_labelname,       $,      0,      0),     
    (m_define,          $,      0,      0),     
    (m_definereg,       $,      0,      0),     
    (m_evalx,           $,      0,      0),     

    (m_labelx,          $,      1,      0),     
    (m_nop,             $,      0,      0x90),      

    (m_mov,             $,      2,      0),     
    (m_push,            $,      1,      0),     
    (m_pop,             $,      1,      0),     
    (m_lea,             $,      2,      0),     
    (m_cmovcc,          $,      2,      0),     

    (m_movd,            $,      2,      0),     
    (m_movq,            $,      2,      0),     

    (m_movsx,           $,      2,      0),     
    (m_movzx,           $,      2,      0),     
    (m_movsxd,          $,      2,      0),     

    (m_call,            $,      1,      0xE8),      
    (m_ret,             $,      0,      0xC3),  
    (m_leave,           $,      0,      0xC9),  
    (m_retn,            $,      1,      0),     

    (m_jmp,             $,      1,      0xE9),  
    (m_jmpcc,           $,      2,      0),     
    (m_xchg,            $,      2,      0),     

    (m_add,             $,      2,      0),     
    (m_sub,             $,      2,      5),     
    (m_adc,             $,      2,      2),     
    (m_sbb,             $,      2,      3),     
    (m_imul,            $,      1,      5),     
    (m_mul,             $,      1,      4),     
    (m_imul2,           $,      2,      0),     
    (m_imul3,           $,      3,      0),     

    (m_idiv,            $,      1,      7),     
    (m_div,             $,      1,      6),     

    (m_andx,            $,      2,      0x04),  
    (m_orx,             $,      2,      0x01),  
    (m_xorx,            $,      2,      0x06),  
    (m_test,            $,      2,      0),     

    (m_cmp,             $,      2,      0x07),  

    (m_shl,             $,      2,      0x04),  
    (m_sar,             $,      2,      0x07),  
    (m_shr,             $,      2,      0x05),  
    (m_rol,             $,      2,      0x00),  
    (m_ror,             $,      2,      0x01),  
    (m_rcl,             $,      2,      0x02),  
    (m_rcr,             $,      2,      0x03),  

    (m_neg,             $,      1,      3),     
    (m_notx,            $,      1,      2),     

    (m_inc,             $,      1,      0),     
    (m_dec,             $,      1,      1),     

    (m_cbw,             $,      0,      0), 
    (m_cwd,             $,      0,      0), 
    (m_cdq,             $,      0,      0),     
    (m_cqo,             $,      0,      0),     
    (m_setcc,           $,      2,      0),     

    (m_bsf,             $,      2,      0xBC),  
    (m_bsr,             $,      2,      0xBD),  

    (m_sqrtsd,          $,      2,      0x51),  
    (m_sqrtss,          $,      2,      0x51),  
    (m_addss,           $,      2,      0x58),  
    (m_subss,           $,      2,      0x5C),  
    (m_mulss,           $,      2,      0x59),  
    (m_divss,           $,      2,      0x5E),  

    (m_addsd,           $,      2,      0x58),  
    (m_subsd,           $,      2,      0x5C),  
    (m_mulsd,           $,      2,      0x59),  
    (m_divsd,           $,      2,      0x5E),  

    (m_comiss,          $,      2,      0),     
    (m_comisd,          $,      2,      0),     
    (m_xorpd,           $,      2,      0x57),  
    (m_xorps,           $,      2,      0x57),  
    (m_andpd,           $,      2,      0x54),  
    (m_andps,           $,      2,      0x54),  
    (m_pxor,            $,      2,      0xEF),  
    (m_pand,            $,      2,      0xDB),  
    (m_cvtss2si,        $,      2,      0),     
    (m_cvtsd2si,        $,      2,      0),     
    (m_cvttss2si,       $,      2,      0),     
    (m_cvttsd2si,       $,      2,      0),     

    (m_cvtsi2ss,        $,      2,      0),     
    (m_cvtsi2sd,        $,      2,      0),     

    (m_cvtsd2ss,        $,      2,      0),     
    (m_cvtss2sd,        $,      2,      0),     

    (m_movdqa,          $,      2,      0x66),  
    (m_movdqu,          $,      2,      0xF3),  

    (m_pcmpistri,       $,      3,      0x63),  
    (m_pcmpistrm,       $,      3,      0x62),  

    (m_fld,             $,      1,      0),     
    (m_fst,             $,      1,      2),     
    (m_fstp,            $,      1,      3),     

    (m_fild,            $,      1,      0),     
    (m_fist,            $,      1,      2),     
    (m_fistp,           $,      1,      3),     

    (m_fadd,            $,      0,      0xC1),  
    (m_fsub,            $,      0,      0xE9),  
    (m_fmul,            $,      0,      0xC9),  
    (m_fdiv,            $,      0,      0xF9),  
    (m_fsqrt,           $,      0,      0xFA),  
    (m_fsin,            $,      0,      0xFE),  
    (m_fcos,            $,      0,      0xFF),  
    (m_fsincos,         $,      0,      0xFB),  
    (m_fptan,           $,      0,      0xF2),  
    (m_fpatan,          $,      0,      0xF3),  
    (m_fabs,            $,      0,      0xE1),  
    (m_fchs,            $,      0,      0xE0),  

    (m_minss,           $,      2,      0x5D),  
    (m_maxss,           $,      2,      0x5F),  
    (m_minsd,           $,      2,      0x5D),  
    (m_maxsd,           $,      2,      0x5F),  

    (m_db,              $,      1,      0),     
    (m_dw,              $,      1,      0),     
    (m_dd,              $,      1,      0),     
    (m_dq,              $,      1,      0),     
    (m_ddoffset,        $,      1,      0),     

    (m_segment,         $,      1,      0),     
    (m_isegment,        $,      0,      0),     
    (m_zsegment,        $,      0,      0),     
    (m_csegment,        $,      0,      0),     

    (m_align,           $,      1,      0),     
    (m_resb,            $,      1,      1),     
    (m_resw,            $,      1,      2),     
    (m_resd,            $,      1,      4),     
    (m_resq,            $,      1,      8),     

    (m_xlat,            $,      0,      0xD7),  
    (m_loopnz,          $,      1,      0xE0),  
    (m_loopz,           $,      1,      0xE1),  
    (m_loopcx,          $,      1,      0xE2),  
    (m_jecxz,           $,      1,      0xE3),  
    (m_jrcxz,           $,      1,      0xE3),  

    (m_cmpsb,           $,      0,      0),     
    (m_cmpsw,           $,      0,      0),     
    (m_cmpsd,           $,      0,      0),     
    (m_cmpsq,           $,      0,      0),     

    (m_rdtsc,           $,      0,      0x31),  
    (m_popcnt,          $,      2,      0),     

    (m_finit,           $,      0,      0),     

    (m_fldz,            $,      0,      0xEE),  
    (m_fld1,            $,      0,      0xE8),  
    (m_fldpi,           $,      0,      0xEB),  
    (m_fld2t,           $,      0,      0xE9),  
    (m_fld2e,           $,      0,      0xEA),  
    (m_fldlg2,          $,      0,      0xEC),  
    (m_fldln2,          $,      0,      0xED),  

    (m_cpuid,           $,      0,      0),     

    (m_halt,            $,      0,      0xF4),  
end

export enumdata [0:]ichar regnames, [0:]byte regcodes =
    (rnone=0,   $,  0),         
    (r0,        $,  0),         
    (r1,        $,  10),        
    (r2,        $,  11),        
    (r3,        $,  7),         
    (r4,        $,  3),         
    (r5,        $,  6),         
    (r6,        $,  12),        
    (r7,        $,  13),        
    (r8,        $,  14),        
    (r9,        $,  15),        
    (r10,       $,  1),         
    (r11,       $,  2),         
    (r12,       $,  8),         
    (r13,       $,  9),         
    (r14,       $,  5),         
    (r15,       $,  4),         

    (r16,       $,  4),         
    (r17,       $,  7),         
    (r18,       $,  5),         
    (r19,       $,  6),         

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

    (ov_cond=0,     "ov",   "o",        nov_cond),
    (nov_cond=1,    "nov",  "no",       ov_cond),

    (ltu_cond=2,    "ltu",  "b",        geu_cond),
    (geu_cond=3,    "geu",  "ae",       ltu_cond),

    (eq_cond=4,     "eq",   "z",        ne_cond),
    (ne_cond=5,     "ne",   "nz",       eq_cond),

    (leu_cond=6,    "leu",  "be",       gtu_cond),
    (gtu_cond=7,    "gtu",  "a",        leu_cond),

    (s_cond=8,      "s",    "s",        ns_cond),
    (ns_cond=9,     "ns",   "ns",       s_cond),

    (p_cond=10,     "p",    "p",        np_cond),
    (np_cond=11,    "np",   "np",       p_cond),

    (lt_cond=12,    "lt",   "l",        ge_cond),
    (ge_cond=13,    "ge",   "ge",       lt_cond),

    (le_cond=14,    "le",   "le",       gt_cond),
    (gt_cond=15,    "gt",   "g",        le_cond),

    (flt_cond=16,   "flt",  "b",        fge_cond),      
    (fge_cond=17,   "fge",  "ae",       flt_cond),
    (fle_cond=18,   "fle",  "be",       fgt_cond),
    (fgt_cond=19,   "fgt",  "a",        fle_cond)
end

global const z_cond = eq_cond
global const nz_cond = ne_cond



export tabledata []ichar dregnames, []byte regsizes, []byte regindices =
    ("d0",      8,  r0),        
    ("d1",      8,  r1),        
    ("d2",      8,  r2),        

    ("d3",      8,  r3),        
    ("d4",      8,  r4),        
    ("d5",      8,  r5),        
    ("d6",      8,  r6),        
    ("d7",      8,  r7),        
    ("d8",      8,  r8),        
    ("d9",      8,  r9),        

    ("d10",     8,  r10),       
    ("d11",     8,  r11),       
    ("d12",     8,  r12),       
    ("d13",     8,  r13),       

    ("d14",     8,  r14),       
    ("d15",     8,  r15),       

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


export enumdata [0:]ichar reftypenames =    
    (extern_ref=0,      $),     
    (fwd_ref,           $),     
    (back_ref,          $),     
end

global int mlabelno

global int mstackdepth              

global int retindex

global const regmax=r9              
global const xregmax=xr15



global u64 regset                   
global int nregs, nxregs

global u64 isregvar



global int inf_proccalls
global int inf_proclocals
global int inf_procxlocals

global int inf_leafproc
global int inf_highreg
global int inf_highxreg
global int inf_maxargs
export int inf_assem

global int inf_r10used      
global int inf_r11used
global int inf_r13used

global [16]int dsaveregs
global [16]int xsaveregs
global int ndsaveregs   
global int nxsaveregs
global int dsaveoffset
global int xsaveoffset
global int needstackframe
global int framebytes
global int parambytes
global int needshadow32     

global int dspillbytes, xspillbytes, alignbytes, localbytes, shadowbytes

global byte noxorclear      

global const wd = 4
global const xc = 3
global const yb = 2
global const za = 1

global const xb = 2
global const ya = 1

global const xa = 1

global symbol procdef

global int ncalldepth

global const maxparams=32
global const maxlocals=256

global [maxparams]symbol paramdefs
global [maxlocals]symbol localdefs
global int nparams, nlocals
global int retmode
global int passno
global int sa_nargs

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(xr0,xr1,xr2,xr3,xr4,xr5)

global int paramoffset

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

global int stackaligned
global const initial_stackalignment = 1

global const rtos=rnone         


export ref mclrec mccode, mccodex       

global int currsegment=0        

global int currzdataalign=0
global int curridataalign=0

global int frameoffset
global int isthreadedproc
global int iscallbackproc

global int structretoffset          
global ref mclrec stacksetinstr     
global int currblocksize            
global ichar allasmstr
global int allasmstrlen

global operand dstackopnd
global operand distackopnd
global operand dframeopnd

global operand zero_opnd=nil

global [r0..r15,1..8]operand regtable


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


global int destlinestart
global symbol currasmproc
global int noregvar             



global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

global record relocrec =            
    ref relocrec nextreloc
    int reloctype
    int offset
    int stindex
end

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
global ref dbuffer ss_zdata         
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

global const init_ss_symbols=32768              
global ref []symbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize


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
=== mc_objdecls.m 0 0 21/33 ===
global record imagefileheader =
    word16  machine
    word16  nsections
    word32  timedatestamp
    word32  symtaboffset
    word32  nsymbols
    word16  optheadersize
    word16  characteristics
end

global record imagedir =
    word32  virtualaddr
    word32  size
end

global record optionalheader =          
    word16  magic
    byte     majorlv
    byte     minorlv
    word32 codesize
    word32 idatasize
    word32 zdatasize
    word32 entrypoint
    word32 codebase
    word64  imagebase
    word32 sectionalignment
    word32 filealignment
    word16  majorosv
    word16  minorosv
    word16  majorimagev
    word16  minorimagev
    word16  majorssv
    word16  minorssv
    word32 win32version
    word32 imagesize
    word32 headerssize
    word32 checksum
    word16  subsystem
    word16  dllcharacteristics
    word64   stackreserve
    word64   stackcommit
    word64   heapreserve
    word64   heapcommit
    word32 loaderflags
    word32 rvadims
    imagedir exporttable
    imagedir importtable
    imagedir resourcetable
    imagedir exceptiontable
    imagedir certtable
    imagedir basereloctable
    imagedir debug
    imagedir architecture
    imagedir globalptr
    imagedir tlstable
    imagedir loadconfigtable
    imagedir boundimport
    imagedir iat
    imagedir delayimportdescr
    imagedir clrheader
    imagedir reserved
end

global record imagesectionheader =
    [8]char name
    union
        word32  physical_address
        word32  virtual_size
    end
    word32  virtual_address
    word32  rawdata_size
    word32  rawdata_offset
    word32  relocations_ptr
    word32  linenos_offset
    word16  nrelocs
    word16  nlinenos
    word32  characteristics
end

global record imagesymbol =
    union
        [8]char shortname
        struct
            word32  shortx
            word32  longx
        end
        word64 longname
    end
    word32  value
    int16   sectionno
    word16  symtype
    byte    storageclass
    byte    nauxsymbols
end

global record importdirrec =
    word32  implookuprva
    word32  timedatestamp
    word32  fwdchain
    word32  namerva
    word32  impaddressrva
end

global record coffrelocrec =
    int32   virtualaddr
    int32   stindex
    int16   reloctype
end

global enumdata [0:]ichar relocnames =
    (abs_rel = 0,   $),
    (addr64_rel,    $),
    (addr32_rel,    $),
    (addr32nb_rel,  $),
    (rel32_rel,     $),
    (rel321_rel,    $),
    (rel8_rel,      $),             
end

global record auxsectionrec = 
    int32 length
    int16 nrelocs
    int16 nlines
    int32 checksum
    int16 sectionno
    int32 dummy
end

global record sectionrec =
    union
        ref dbuffer data        
        ref byte bytedata       
    end
    ichar name                  
    int segtype                 
    int rawsize                 
    int rawoffset               
    int virtsize                
    int virtoffset              
    ref relocrec relocs         
    int nrelocs                 
end

global record importrec =               
    symbol def              
    int libno                   
    ichar name                  
    int hintnameoffset          
    int iatoffset               
    int thunkoffset             
end

global record exportrec =       
    symbol def              
    ichar name                  
end

global record dllrec =                  
    ichar name                  
    int nprocs                  
    int nametableoffset         
    int addrtableoffset         
    int dllnameoffset           
    int dllextraoffset          
end

global record exportdirrec =
    word32 exportflags
    word32 timedatestamp
    word16 majorversion
    word16 minorversion
    word32 namerva
    word32 ordinalbase
    word32 naddrtable
    word32 nnamepointers
    word32 expaddressrva
    word32 namepointerrva
    word32 ordtablerva
end
=== mc_writeexe.m 0 0 22/33 ===

[maxlibfile]int64 libinsttable
[maxlibfile]ichar libinstnames
[maxlibfile]int libnotable          

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
    ref basereloc nextitem
    word32 address              
    int32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]int32 blockcounts
[maxbaseblock]int32 blockbytes
[maxbaseblock]byte blockpadding
int nbaseblocks
int basetablesize


const filealign = 512
const sectionalign = 4096
const exe_imagebase = 0x40'0000

const dll_imagebase = 0x1000'0000

global int imagebase

int imagesize
int filesize
ref[]int64 thunktable               
int fileiatoffset
int fileiatsize
symbol stentrypoint             
symbol stentrypoint2
symbol stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir              

global const maximports = 3000
global [0..maximports]importrec importtable
global int nimports

global const maxexports = 1000
global [maxexports]exportrec exporttable
global int nexports
ichar dllfilename
int isdll

const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

ref byte datastart
ref byte dataptr
ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset             

int blockdirvirtaddr
int blockdirvirtsize
int blockdiroffset

global proc writeexe(ichar outfile,int dodll)=
    imagefileheader header
    optionalheader optheader
    int offset,i
    int64 aa

    dllfilename:=extractfile(outfile)
    isdll:=dodll

    datastart:=dataptr:=pcm_allocz(filesize)

    writedosstub()
    writepesig()
    writefileheader()
    writeoptheader()
    for i to nsections do
        writesectionheader(&sectiontable[i])
    od
    writepadding(sectiontable[1].rawoffset)
    for i to nsections do
        writesectiondata(&sectiontable[i])
    od



    if writefile(outfile,datastart,dataptr-datastart)=0 then
        println "Error writing exe file (possibly still running)"
        stop 1
    fi
end

global proc genexe(ichar entrypoint, outfile, int dodll)=

    dllfilename:=extractfile(outfile)
    isdll:=dodll

    imagebase:=(isdll|dll_imagebase|exe_imagebase)

    userentrypoint:=entrypoint
    loadlibs()
    scanst()                

    getoffsets()

    relocdata(&sectiontable[csect])
    relocdata(&sectiontable[dsect])
end

proc loadlibs=
    int i
    int64 hinst
    ichar file
    [300]char filename


    for i to nlibfiles when libfiles[i]^<>'$' do
        if libtypes[i]='L' then
CPL =LIBFILES[I],LIBTYPES[I]:"c"
            axerror("Can't use LIB files with EXE")
        fi
        strcpy(&.filename,libfiles[i])
        hinst:=os_getdllinst(&.filename)
        if hinst=0 then
            cpl "File:",&.filename
            axerror("Can't load search lib")
        fi
        libinsttable[i]:=hinst
        libinstnames[i]:=pcm_copyheapstring(&.filename)
    od
end

global proc initsectiontable=

    sectiontable[csect].name:=".text"
    sectiontable[csect].segtype:=code_seg
    sectiontable[csect].data:=ss_code
    sectiontable[csect].virtsize:=bufferlength(ss_code)


    if bufferlength(ss_idata)=0 then
        addqword (ss_idata,0)
    fi

    sectiontable[dsect].name:=".data"
    sectiontable[dsect].segtype:=idata_seg
    sectiontable[dsect].data:=ss_idata

    sectiontable[dsect].virtsize:=bufferlength(ss_idata)
    sectiontable[dsect].rawsize:=roundtoblock(sectiontable[dsect].virtsize,filealign)
    sectiontable[dsect].nrelocs:=ss_nidatarelocs
    sectiontable[dsect].relocs:=ss_idatarelocs

    if ss_zdatalen=0 then
        ss_zdatalen:=16
    fi

    sectiontable[zsect].name:=".bss"
    sectiontable[zsect].segtype:=zdata_seg
    sectiontable[zsect].virtsize:=ss_zdatalen


    sectiontable[csect].rawsize:=roundtoblock(sectiontable[csect].virtsize,filealign)
    sectiontable[csect].nrelocs:=ss_ncoderelocs
    sectiontable[csect].relocs:=ss_coderelocs

    sectiontable[isect].name:=".idata"
    sectiontable[isect].segtype:=impdata_seg
    sectiontable[isect].virtsize:=0
    sectiontable[isect].rawsize:=0

    nsections:=4
end

function extractlibname(ichar name, int &libno,moduleno)ichar=
    ref char s,name2
    [256]char str
    [256]char str2
    int i

    name2:=nil

    reenter::
    s:=name
    libno:=0

    while s^ do
        if s^='.' then          
            memcpy(&.str,name,s-name)
            str[s-name+1]:=0
            strcat(&.str,".dll")

            for i:=1 to ndlls do
                if eqstring(&.str,dlltable[i].name) then
                    libno:=i
                    ++dlltable[libno].nprocs
                    return (name2|name2|s+1)
                fi
            od
            if ndlls>=maxlibs then axerror("Too many libs") fi
            libno:=++ndlls

            dlltable[libno].name:=pcm_copyheapstring(&.str)
            dlltable[libno].nprocs:=1
            return (name2|name2|s+1)
        fi

        ++s
    od

    int n

    for i:=1 to nlibfiles when libinsttable[i] do
        if os_getdllprocaddr(libinsttable[i],name) then
            n:=i
            exit                
        fi
    else
CPL NAME
        axerror("Can't find external function")
    od

    if libno:=libnotable[n] then            
        ++dlltable[libno].nprocs
        return name
    fi

    strcpy(&.str,libfiles[n])
    strcat(&.str,".dll")
    if ndlls>=maxlibs then axerror("2:Too many libs") fi
    libno:=++ndlls

    dlltable[libno].name:=pcm_copyheapstring(&.str)
    dlltable[libno].nprocs:=1
    libnotable[n]:=libno

    return name
end

proc scanst=

    int i,libno
    symbol d
    ichar name, libname, dname, basename

    for i:=1 to ss_nsymbols do
        d:=ss_symboltable[i]
        dname:=(d.truename|d.truename|d.name)
        if d.isimport then
            if nimports>=maximports then axerror("genexe: Too many imports") fi
            ++nimports
            name:=extractlibname(dname,libno,1)
            importtable[nimports].libno:=libno          
            importtable[nimports].name:=name                
            importtable[nimports].def:=d

            d.importindex:=nimports
        elsif d.scope=export_scope then
            basename:=getbasename(dname)
            if userentrypoint then
                if eqstring(basename,userentrypoint) then
                    stentrypoint:=d
                fi
            else
                if eqstring(basename,"main") and not isdll then
                    stentrypoint:=d
                fi
            fi

            if nexports>=maxexports then axerror("gendll: Too many exports") fi
            ++nexports

            exporttable[nexports].def:=d
            exporttable[nexports].name:=getbasename(dname)
        fi
    od
end

proc relocdata(ref sectionrec s)=
    ref sectionrec u
    ref relocrec r
    ref byte p
    ref word32 p32
    ref word64 p64
    symbol d
    int offset,index,thunkoffset,iatoffset

    p:=bufferelemptr(s.data,0)
    r:=s.relocs

    while r do
        d:=ss_symboltable[r.stindex]
        index:=d.importindex                
        thunkoffset:=importtable[index].thunkoffset

        case r.reloctype
        when rel32_rel then
            if not d.isimport then
                axerror("rel32/not imported")
            fi
            (ref word32(p+r.offset)^:=thunkoffset-r.offset-4)
        when addr32_rel, addr64_rel then                
            if d.isimport then
                (ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
            else
                u:=nil
                case d.segment
                when zdata_seg then u:=&sectiontable[zsect]
                when idata_seg then u:=&sectiontable[dsect]
                when code_seg then u:=&sectiontable[csect]
                else
                    CPL D.NAME,D.SEGMENT
                    AXERROR("RELOCDATA/SEG?")
                esac
                    p32:=cast(p+r.offset)
                    if r.reloctype=addr32_rel then
                        p32^:=p32^+u.virtoffset+imagebase
                    else
                        p64:=cast(P32)
                        p64^:=p64^+u.virtoffset+imagebase
                    fi
            fi
        else
            cpl relocnames[r.reloctype]
            axerror("Can't do this rel type")
        esac

        r:=r.nextreloc
    od

end

proc getbaserelocs(ref sectionrec s)=
    ref sectionrec u
    ref relocrec r
    ref byte p
    symbol d
    int index

    p:=bufferelemptr(s.data,0)
    r:=s.relocs

    while r do
        d:=ss_symboltable[r.stindex]

        case r.reloctype
        when addr32_rel, addr64_rel then                
            if d.isimport then
            else
                case d.segment
                when zdata_seg then u:=&sectiontable[zsect]
                when idata_seg then u:=&sectiontable[dsect]
                when code_seg then u:=&sectiontable[csect]
                esac

                newbasereloc(u.virtoffset+r.offset, r.reloctype)

            fi
        esac

        r:=r.nextreloc
    od

end

proc writerecordx(ref void r, int length)=
    memcpy(dataptr,r,length)
    dataptr+:=length
end

proc writedosstub=
    static []byte stubdata = (
    0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 
    0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 
    0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 
    0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD, 
    0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68, 
    0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 
    0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F, 
    0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E, 
    0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20, 
    0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 
    0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)

    writerecordx(&stubdata,stubdata.bytes)
end

proc writepesig=
    dataptr++^:='P'
    dataptr++^:='E'
    dataptr++^:=0
    dataptr++^:=0
end

proc writepadding(int offset)=
    dataptr:=datastart+offset           
end

proc writefileheader=
    imagefileheader header

    clear header

    header.machine:=0x8664
    header.nsections:=nsections
    header.optheadersize:=optionalheader.bytes
    header.characteristics:=0x22F
    if isdll then
        header.characteristics:=0x22E ior 0x2000
    fi


    writerecordx(&header,header.bytes)
end

proc writeoptheader=
    optionalheader header

    clear header

    header.magic:=0x20B
    header.majorlv:=1
    header.minorlv:=0
    header.codesize:=sectiontable[csect].rawsize
    header.idatasize:=sectiontable[dsect].rawsize+sectiontable[isect].rawsize
    header.zdatasize:=roundtoblock(sectiontable[zsect].virtsize,filealign)
    
    if stentrypoint=nil then
        stentrypoint:=stentrypoint2
    fi

    if stentrypoint=nil then
        if userentrypoint then
            cpl userentrypoint
            axerror("User entry point not found")
        else
            if not isdll then
                axerror("Entry point not found: main")
            fi
        fi
    else
        header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint.offset
    fi

    header.codebase:=sectionalign
    header.imagebase:=imagebase
    header.sectionalignment:=sectionalign
    header.filealignment:=filealign
    header.majorosv:=4
    header.minorosv:=0
    header.majorssv:=5
    header.minorssv:=2
    header.imagesize:=imagesize
    header.headerssize:=sectiontable[1].rawoffset
    header.subsystem:=3

    header.stackreserve:=4194304
    header.stackcommit:=2097152

    header.heapreserve:=1048576
    header.heapcommit:=4096
    header.rvadims:=16

    header.importtable.virtualaddr:=sectiontable[isect].virtoffset
    header.importtable.size:=sectiontable[isect].virtsize-exportdirvirtsize-blockdirvirtsize

    if isdll then
        header.dllcharacteristics:=0x40     
        header.exporttable.virtualaddr:=exportdirvirtaddr
        header.exporttable.size:=exportdirvirtsize

        header.basereloctable.virtualaddr:=blockdirvirtaddr
        header.basereloctable.size:=blockdirvirtsize
    fi

    header.iat.virtualaddr:=fileiatoffset
    header.iat.size:=fileiatsize

    writerecordx(&header,header.bytes)

end

proc writesectionheader(ref sectionrec s)=
    imagesectionheader sheader

    clear sheader

    strcpy(&sheader.name[1],s.name)
    sheader.virtual_size:=s.virtsize
    sheader.virtual_address:=s.virtoffset
    sheader.rawdata_offset:=s.rawoffset
    sheader.rawdata_size:=s.rawsize

    int64 aa
    case s.segtype
    when zdata_seg then
        aa:=0xC050'0080
        sheader.characteristics:=aa
    when idata_seg then
        aa:=0xC050'0040
        sheader.characteristics:=aa
    when code_seg then
        aa:=0x6050'0020
        sheader.characteristics:=aa
    when impdata_seg then
        aa:=0xC030'0040
        sheader.characteristics:=aa
    esac
    writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(ref sectionrec s)=
    case s.segtype
    when impdata_seg then
        writerecordx(s.bytedata,s.virtsize)     
        if s.rawsize>s.virtsize then
            dataptr+:=(s.rawsize-s.virtsize)
        fi

    when zdata_seg then                 
    else
        writerecordx(bufferelemptr(s.data,0),s.rawsize)
    esac
end

proc writeexporttable(ref byte pstart)=
    const maxexports=2000
    [maxexports]int sortindex
    ref exportdirrec phdr := cast(pstart)
    ref word32 paddrtable
    ref word32 pnametable
    ref word16 pordtable
    ref char pdllname
    ref char pnames
    int addrtableoffset
    int nametableoffset
    int ordtableoffset
    int dllnameoffset
    int namesoffset
    int virtoffset
    int sectionno
    symbol d
    ichar basename

    phdr.timedatestamp:=0x5f89f4f8

    phdr.ordinalbase:=1
    phdr.naddrtable:=nexports
    phdr.nnamepointers:=nexports

    addrtableoffset:=exportdirrec.bytes
    nametableoffset:=addrtableoffset+nexports*4
    ordtableoffset:=nametableoffset+nexports*4
    dllnameoffset:=ordtableoffset+nexports*2
    namesoffset:=dllnameoffset+strlen(dllfilename)+1

    virtoffset:=sectiontable[isect].virtoffset+exportdiroffset

    paddrtable:=cast(pstart+addrtableoffset)
    pnametable:=cast(pstart+nametableoffset)
    pordtable:=cast(pstart+ordtableoffset)
    pdllname:=cast(pstart+dllnameoffset)
    pnames:=cast(pstart+namesoffset)

    phdr.namerva:=dllnameoffset+virtoffset
    phdr.expaddressrva:=addrtableoffset+virtoffset
    phdr.namepointerrva:=nametableoffset+virtoffset
    phdr.ordtablerva:=ordtableoffset+virtoffset

    strcpy(pdllname,dllfilename)

    if nexports>maxexports then
        axerror("Too many exports - can't sort")
    fi

    sortexports(sortindex)

    for i to nexports do
        d:=exporttable[sortindex[i]].def
        basename:=exporttable[sortindex[i]].name
        sectionno:=getsectionno(d.segment)

        strcpy(pnames,basename)
        pnametable^:=namesoffset+virtoffset
        ++pnametable
        namesoffset+:=strlen(basename)+1
        pnames+:=strlen(basename)+1

        paddrtable^:=d.offset+sectiontable[sectionno].virtoffset
        ++paddrtable
        pordtable^:=i-1
        ++pordtable
    od
end

function getexporttablesize:int=
    int size

    size:=exportdirrec.bytes
    size+:=nexports*4           
    size+:=nexports*4           
    size+:=nexports*2           

CPL =DLLFILENAME
    size+:=strlen(dllfilename)+1
    for i to nexports do
        size+:=strlen(exporttable[i].def.name)+1
    od

    return size
end

proc newbasereloc(int addr, reltype)=
    ref basereloc p

    p:=pcm_allocz(basereloc.bytes)
    p.address:=addr
    p.reloctype:=reltype

    p.nextitem:=basereloclist

    basereloclist:=p
    ++nbaserelocs
    maxrelocaddr max:=addr

end

proc scanbaserelocs=
    int baseaddr,addr,nextblock
    ref basereloc p

    baseaddr:=0x1000
    nbaseblocks:=0

    repeat
        nextblock:=baseaddr+0x1000
        if nbaseblocks>=maxbaseblock then axerror("Too many blocks") fi
        ++nbaseblocks
        blockbases[nbaseblocks]:=baseaddr
        blockcounts[nbaseblocks]:=0


        p:=basereloclist
        while p do
            addr:=p.address
            if addr>=baseaddr and addr<nextblock then
                ++blockcounts[nbaseblocks]
            fi

            p:=p.nextitem
        od

        baseaddr:=nextblock
    until baseaddr>maxrelocaddr

    for i to nbaseblocks when blockcounts[i] do
        if blockcounts[i].odd then
            ++blockcounts[i]
            ++blockpadding[i]
        fi
        blockbytes[i]:=blockcounts[i]*2+8
        basetablesize+:=blockbytes[i]
    od
end

proc writebasereloctable(ref byte pstart)=
    
    ref word32 p32
    ref word16 p16
    int baseaddr,addr,nextblock
    ref basereloc q

    p32:=cast(pstart)

    for i to nbaseblocks when blockcounts[i] do
        p32^:=blockbases[i]
        ++p32
        p32^:=blockbytes[i]
        ++p32
        p16:=cast(p32)

        q:=basereloclist
        baseaddr:=blockbases[i]
        nextblock:=baseaddr+4096

        while q do
            addr:=q.address
            if addr>=baseaddr and addr<nextblock then
                p16^:=addr-baseaddr+(q.reloctype=addr32_rel|3|10)<<12
                ++p16
            fi
            q:=q.nextitem
        od
        if blockpadding[i] then p16++^:=0 fi

        p32:=cast(p16)

    od
end

proc sortexports([]int &sortindex)=
    symbol d,e

    for i to nexports do
        sortindex[i]:=i
    od

    int swapped

    repeat
        swapped:=0
        for i:=1 to nexports-1 do

            d:=exporttable[sortindex[i]].def
            e:=exporttable[sortindex[i+1]].def

            if strcmp(getbasename(d.name), getbasename(e.name))>0 then

                swapped:=1
                swap(sortindex[i], sortindex[i+1])
            fi
        od
    until not swapped

end

function getsectionno(int segment)int=
    case segment
    when zdata_seg then zsect
    when idata_seg then dsect
    when code_seg then csect
    else axerror("GSN"); 0
    esac
end

proc getoffsets=
    int fileoffset, imageoffset,i,diroffset,impdirno,hinttableoffset,j,n
    int codesize,length,thunkoffset,offset,dirstartoffset

    fileoffset:=128+4+imagefileheader.bytes+optionalheader.bytes    
    fileoffset+:=imagesectionheader.bytes*nsections

    fileoffset:=roundtoblock(fileoffset,filealign)
    imageoffset:=4096

    ref byte pcode
    codesize:=sectiontable[csect].virtsize
    pcode:=bufferelemptr(ss_code,codesize)
    while codesize iand 7 do pcode++^:=0x90; ++codesize od
    thunkoffset:=codesize
    codesize+:=nimports*8

    sectiontable[csect].virtsize:=codesize
    sectiontable[csect].rawsize:=roundtoblock(codesize,filealign)

    buffercheck(ss_code, codesize-thunkoffset+16)       

    for i:=1 to nsections do
        if sectiontable[i].segtype<>zdata_seg then
            sectiontable[i].rawoffset:=fileoffset
        fi
        if sectiontable[i].segtype<>zdata_seg then
            fileoffset:=roundtoblock(fileoffset+sectiontable[i].virtsize,filealign)
        fi
        sectiontable[i].virtoffset:=imageoffset

        if sectiontable[i].segtype=impdata_seg then
            diroffset:=imageoffset
            impdirno:=i
        fi

        imageoffset:=roundtoblock(imageoffset+sectiontable[i].virtsize,sectionalign)
    od

    if isdll then
        getbaserelocs(&sectiontable[csect])
        getbaserelocs(&sectiontable[dsect])
    fi


    diroffset+:=(ndlls+1)*importdirrec.bytes            


    for i to ndlls do
        dlltable[i].nametableoffset:=diroffset              
        diroffset+:=(dlltable[i].nprocs+1)*8
    od
    fileiatoffset:=diroffset
    for i to ndlls do
        dlltable[i].addrtableoffset:=diroffset              
        diroffset+:=(dlltable[i].nprocs+1)*8
    od
    fileiatsize:=diroffset-fileiatoffset

    hinttableoffset:=diroffset
    for i to nimports do
        length:=strlen(importtable[i].name)+3
        if length iand 1 then ++length fi       
        importtable[i].hintnameoffset:=diroffset
        diroffset+:=length
    od


    diroffset:=roundtoblock(diroffset,4)

    for i to ndlls do
        length:=strlen(dlltable[i].name)+1
        if length iand 1 then ++length fi       
        dlltable[i].dllextraoffset:=diroffset
        diroffset+:=dlltable[i].nprocs*4        
        dlltable[i].dllnameoffset:=diroffset
        diroffset+:=length
    od

    dirstartoffset:=sectiontable[impdirno].virtoffset

    if isdll then
        exportdirvirtaddr:=diroffset
        exportdiroffset:=diroffset-dirstartoffset
        exportdirvirtsize:=getexporttablesize()

        diroffset+:=exportdirvirtsize

        scanbaserelocs()

        blockdirvirtaddr:=diroffset
        blockdiroffset:=diroffset-dirstartoffset
        blockdirvirtsize:=basetablesize
        diroffset+:=blockdirvirtsize
    fi

    offset:=diroffset-dirstartoffset


    sectiontable[impdirno].virtsize:=offset
    sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
    filesize:=roundtoblock(fileoffset+offset,filealign)

    imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

    ref byte pimpdir

    pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

    ref importdirrec pdir
    ref int64 paddr,pname
    int iatoffset
    pdir:=cast(pimpdir)

    for i:=1 to ndlls do
        pdir.implookuprva:=dlltable[i].nametableoffset
        pdir.impaddressrva:=dlltable[i].addrtableoffset
        pdir.namerva:=dlltable[i].dllnameoffset
        ++pdir

        iatoffset:=dlltable[i].addrtableoffset
        paddr:=cast(pimpdir+iatoffset-dirstartoffset)
        pname:=cast(pimpdir+dlltable[i].nametableoffset-dirstartoffset)
        for j to nimports when importtable[j].libno=i do
            pname^:=paddr^:=importtable[j].hintnameoffset
            importtable[j].iatoffset:=iatoffset
            iatoffset+:=8
            ++pname
            ++paddr
        od
    od

    ref byte phint
    ref word32 pextra

    for i to nimports do
        phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
        phint+:=2                   
        strcpy(cast(phint),importtable[i].name)
    od
    int xxx
    xxx:=dirstartoffset
    for i to ndlls do
        pextra:=cast(pimpdir+dlltable[i].dllextraoffset-dirstartoffset)
        for j to dlltable[i].nprocs do
            pextra^:=xxx
            ++pextra
        od
        xxx+:=importdirrec.bytes
        phint:=pimpdir+dlltable[i].dllnameoffset-dirstartoffset
        strcpy(cast(phint),dlltable[i].name)
    od

    if isdll then
        writeexporttable(ref byte(pimpdir)+exportdiroffset)
        writebasereloctable(ref byte(pimpdir)+blockdiroffset)
    fi

    ref byte thunkptr,codebase
    int thunkaddr
    thunkptr:=bufferelemptr(ss_code,thunkoffset)
    codebase:=bufferelemptr(ss_code,0)


    for i to nimports do
        importtable[i].thunkoffset:=thunkptr-codebase

        thunkptr++^:=0xFF
        thunkptr++^:=0x25


        thunkaddr:=importtable[i].iatoffset-(sectiontable[csect].virtoffset+
            importtable[i].thunkoffset+6)

        (ref int32(thunkptr)^:=thunkaddr)
        thunkptr+:=4
        thunkptr++^:=0x90
        thunkptr++^:=0x90
    od
end
=== mx_decls.m 0 0 23/33 ===



global const mcxsig = 'MCX\e'

global enumdata [0:]ichar mcxdirnames =
    (pad_dir = 0,       $),     
    (version_dir,       $),     
    (code_dir,          $),     
    (idata_dir,         $),     
    (zdata_dir,         $),     
    (reloc_dir,         $),     
    (dlls_dir,          $),     
    (libs_dir,          $),     
    (importsymbols_dir, $),     
    (exportsymbols_dir, $),     
    (exportsegs_dir,    $),     
    (exportoffsets_dir, $),     
    (entry_dir,         $),     
    (end_dir,           $),     
end



global enumdata [0:]ichar mcxrelocnames =
    (no_rel = 0,        $),

    (locabs32_rel,  "locabs32"),        
    (locabs64_rel,  "locabs64"),        

    (impabs32_rel,  "impabs32"),        
    (impabs64_rel,  "impabs64"),        

    (imprel32_rel,  "imprel32"),        
end




global record librec=

    ichar version

    int codesize            
    int idatasize           
    int zdatasize           

    int nrelocs             
    int ndlllibs            
    int nlibs               
    int nimports            
    int nexports            

    ref byte codeptr        
    ref byte idataptr       

    ref[]mcxreloc   reloctable      
    ref[]ichar      dllnames        
    ref[]ichar      libnames        
    ref[]ichar      importnames     
    ref[]ichar      exports         
    ref[]byte       exportsegs      
    ref[]u64        exportoffsets   

    u64 entryoffset                 
                                    


    ref byte zdataptr               
    int codexsize                   
    ref[]u64        exportaddr      
    ref[]int16      importxreftable 

    ichar           filespec        
    ichar           libname         
    ref byte        entryaddr       
    int             libno           
end


global record mcxreloc =
    u32     offset          
    union
        u16     stindex         
        byte    targetsegment   
    end
    byte    segment         
    byte    reloctype       
end

global const maxdlls =      20
global const maxlibs =      20
global const maxsymbols =   3000


global [maxdlls]ichar       dllnametable
global [maxdlls]u64         dllinsttable
global int ndlllibs


global [maxlibs]ichar       libnametable
global [maxlibs]ref librec  libtable
global [maxlibs]byte        librelocated        
global [maxlibs]byte        libinitdone         
global int nlibs


global [maxsymbols]ichar    symbolnametable 
global [maxsymbols]byte     symboldefined   
global [maxsymbols]ref void symboladdress   
global [maxsymbols]int16    symbollibindex  
global [maxsymbols]byte     symboldllindex  
global int nsymbols

global int nsymimports=0, nsymexports=0
=== mx_run.m 0 0 24/33 ===

global function writememlib(ichar filename)ref librec plib=
    int n, k
    librec lib

    clear lib


    ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

    roundsegment(ss_code,8,0x90)
    roundsegment(ss_idata,8,0)

    lib.version:="0.1234"

    lib.filespec:=filename
    lib.libname:=pcm_copyheapstring(extractbasefile(filename))
    lib.libno:=1

    countsymbols()
    writerelocs(&lib)

    lib.zdatasize:=ss_zdatalen
    lib.codesize:=bufferlength(ss_code)
    lib.idatasize:=bufferlength(ss_idata)

    lib.codeptr:=bufferelemptr(ss_code,0)
    lib.idataptr:=bufferelemptr(ss_idata,0)

    int ndlls:=0, nlibs:=0
    for i to nlibfiles when libfiles[i]^<>'$' do
        if libtypes[i]='D' then ++ndlls else ++nlibs fi
    od

    lib.ndlllibs:=ndlls
    lib.nlibs:=nlibs

    lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
    lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

    k:=0
    for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
        lib.dllnames[++k]:=libfiles[i]
    od

    k:=0
    for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='L' do
        lib.libnames[++k]:=libfiles[i]
    od

    addsymbols(&lib)
    plib:=pcm_alloc(librec.bytes)
    memcpy(plib, &lib, librec.bytes)    

    return plib
end

proc roundsegment(ref dbuffer p, int align, value)=
    int length:=bufferlength(p)
    int newlength:=roundtoblock(length, align)

    buffercheck(p, align)

    to newlength-length do
        p.pcurr++^:=value
    od
end

proc writerelocs(ref librec lib)=
    ref relocrec oldr
    mcxreloc newr
    int n, k
    symbol d
    ref u64 baseptr64
    ref u32 baseptr32@baseptr64

    lib.nrelocs:=ss_nidatarelocs+ss_ncoderelocs
    lib.reloctable:=pcm_alloc(lib.nrelocs*mcxreloc.bytes)

    k:=0

    for i in code_seg..idata_seg do
        oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

        while oldr, oldr:=oldr.nextreloc do
            clear newr

            newr.offset:=oldr.offset
            newr.segment:=(i=code_seg|idata_seg|code_seg)

            d:=ss_symboltable[oldr.stindex]

            case oldr.reloctype
            when rel32_rel then
                if d.isimport then
                    newr.stindex:=d.impindex
                    newr.reloctype:=imprel32_rel
                else
                    axerror("rel32/rel not imported")
                fi
            when addr32_rel, addr64_rel then
                if d.isimport then
                    newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
                    newr.stindex:=d.impindex
                else
                    if oldr.reloctype=addr32_rel then
                        newr.reloctype:=locabs32_rel
                    else
                        newr.reloctype:=locabs64_rel
                    fi
                    newr.targetsegment:=d.segment
                fi
            else
                axerror("reloc?")
            esac

            lib.reloctable[++k]:=newr

        od
    od
end

proc addsymbols(ref librec lib)=
    symbol d, stentry:=nil
    u64 epoffset:=-1
    int n, k
    ichar name


    lib.nimports:=nsymimports
    lib.nexports:=nsymexports
    lib.importnames:=pcm_alloc(nsymimports*ichar.bytes)
    lib.exports:=pcm_alloc(nsymexports*ichar.bytes)
    lib.exportsegs:=pcm_alloc(nsymexports)
    lib.exportoffsets:=pcm_alloc(nsymexports*u64.bytes)

    k:=0
    for i to ss_nsymbols when ss_symboltable[i].impindex do
        d:=ss_symboltable[i]
        lib.importnames[++k]:=(d.truename|d.truename|d.name)
    od

    k:=0
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            if eqstring(d.name, "main") then
                stentry:=d
            fi
            lib.exports[++k]:=d.name
            lib.exportsegs[k]:=d.segment
            lib.exportoffsets[k]:=d.offset
        fi
    od

    if stentry then
        lib.entryoffset:=stentry.offset
    else
        lib.entryoffset:=-1
    fi
end

=== mx_lib.m 0 0 25/33 ===
global function readlibfile(ichar filespec, ref byte p)ref librec plib=

    librec lib
    u64 sig
    int dir,n,tablesize
    ref byte q

    clear lib

    sig:=readu32(p)
    if sig<>mcxsig then
        println "Bad sig - not MCX file"
        stop 1
    fi

    lib.filespec:=pcm_copyheapstring(filespec)
    lib.libname:=pcm_copyheapstring(extractbasefile(filespec))

    doswitch dir:=readbyte(p)
    when version_dir then
        lib.version:=readstring(p)

    when zdata_dir then
        lib.zdatasize:=readu32(p)

    when idata_dir then
        lib.idatasize:=n:=readu32(p)
        lib.idataptr:=pcm_alloc(n)
        memcpy(lib.idataptr, p, n)  
        p+:=n

    when code_dir then
        lib.codesize:=n:=readu32(p)
        lib.codeptr:=p              
        p+:=n

    when dlls_dir then
        lib.ndlllibs:=n:=readu32(p)
        lib.dllnames:=pcm_alloc(ichar.bytes*n)
        for i to n do
            lib.dllnames[i]:=readstring(p)
        od

    when libs_dir then
        lib.nlibs:=n:=readu32(p)
        lib.libnames:=pcm_alloc(ichar.bytes*n)
        for i to n do
            lib.libnames[i]:=readstring(p)
        od
    when importsymbols_dir then
        lib.nimports:=n:=readu32(p)
        lib.importnames:=pcm_alloc(ichar.bytes*n)
        for i to n do
            lib.importnames[i]:=readstring(p)
        od

    when exportsymbols_dir then
        lib.nexports:=n:=readu32(p)
        lib.exports:=pcm_alloc(ichar.bytes*n)
        for i to n do
            lib.exports[i]:=readstring(p)
        od

    when exportsegs_dir then
        n:=readu32(p)
        lib.exportsegs:=pcm_alloc(n)
        for i to n do
            lib.exportsegs[i]:=readbyte(p)
        od

    when exportoffsets_dir then
        n:=readu32(p)
        lib.exportoffsets:=pcm_alloc(u64.bytes*n)
        for i to n do
            lib.exportoffsets[i]:=readu32(p)
        od

    when reloc_dir then
        lib.nrelocs:=n:=readu32(p)
        n:=lib.nrelocs*mcxreloc.bytes
        lib.reloctable:=pcm_alloc(n)
        memcpy(lib.reloctable, p, n)
        p+:=n

    when entry_dir then
        lib.entryoffset:=readu32(p)

    when end_dir then
        exit

    when pad_dir then

    else
        println "Unknown directive:",mcxdirnames[dir]
        stop
    end doswitch

    plib:=pcm_alloc(librec.bytes)
    memcpy(plib, &lib, librec.bytes)    

    return plib
end

function readbyte(ref byte &p)int=
    return p++^
end

function readu32(ref byte &p)u64 x=
    x:=ref u32(p)^
    p+:=4
    x
end

function readstring(ref byte &p)ichar s=
    s:=pcm_copyheapstring(p)

    while (++p)^ do od
    ++p

    return s
end

global proc alloclibdata(ref librec lib)=
    int tablesize, n
    ref byte p

    lib.zdataptr:=pcm_allocz(lib.zdatasize)

    tablesize:=lib.nimports*16          
    n:=lib.codesize

    p:=os_allocexecmem(n+tablesize)     
    if p=nil then
        error("Can't alloc code memory")
    fi
    memcpy(p, lib.codeptr, n)

    memset(p+n, 0, tablesize)

    lib.codeptr:=p
    lib.codexsize:=tablesize

    lib.exportaddr:=pcm_alloc(u64.bytes*lib.nexports)
    lib.importxreftable:=pcm_alloc(i16.bytes*lib.nimports)

    if lib.entryoffset<>0xFFFF'FFFF then
        lib.entryaddr:=lib.codeptr+lib.entryoffset
    fi
end

global proc error(ichar mess, param="")=
    if param^ then
        fprintln mess,param
    else
        println mess
    fi
    println "Aborting"
    stop 1
end

global proc loadmemmcu(ref librec lib)=

    int newlib
    ichar name:=lib.libname

    checknew(name,lib.filespec)

    newlib:=mxaddlib(name)
    libtable[newlib]:=lib

    loadimports(lib)
end

global proc checknew(ichar name, filename)=
    if findlib(name) then
        error("Lib already exists:",filename)
    fi
end

global function findlib(ichar name)int n=

    for i to nlibs do
        if eqstring(name,libnametable[i]) then return i fi
    od
    return 0
end

global function mxaddlib(ichar name)int n=
    if nlibs>=maxlibs then 
        error("Too many libs")
    fi

    libnametable[++nlibs]:=name
    return nlibs
end

global proc fixuplib(ref librec lib)=



    loaddlls()              
    checksymbols()          
    dorelocations()         
end

proc loaddlls=
    u64 inst

    for i to ndlllibs when not dllinsttable[i] do
        inst:=os_getdllinst(dllnametable[i])
        if inst=0 then
            error("Can't find DLL: #", dllnametable[i])
        fi
        dllinsttable[i]:=inst
    od
end

function finddllsymbol(ichar name, int &dllindex)ref void p=

    dllindex:=0
    for i to ndlllibs do
        p:=os_getdllprocaddr(dllinsttable[i], name)
        if p then
            dllindex:=i
            return p
        fi
    od

    return nil
end

proc checksymbols=
    int dllindex,undef:=0
    ref void p

    for i to nsymbols when not symboldefined[i] do
        p:=finddllsymbol(symbolnametable[i], dllindex)
        if p then
            symboladdress[i]:=p
            symboldllindex[i]:=dllindex
            symboldefined[i]:=1
        else
            println "Undef",symbolnametable[i]
            ++undef
        fi
    od

    if undef then
    fi
end

proc dorelocations=
    for i to nlibs when not librelocated[i] do
        reloclib(libtable[i])
    od
end

proc reloclib(ref librec lib)=
    int index, targetoffset
    ichar name
    ref byte p
    ref byte q
    ref u64 qaddr       
    mcxreloc r

    p:=lib.codeptr+lib.codesize
    qaddr:=cast(p+lib.nimports*u64.bytes)

    for i to lib.nimports do
        name:=lib.importnames[i]
        p++^:=0x48
        p++^:=0xFF
        p++^:=0x24
        p++^:=0x25
        (ref u32(p)^:=cast(qaddr))
        p+:=4

        index:=lib.importxreftable[i]
        qaddr++^:=cast(symboladdress[index])

    od

    for i to lib.nrelocs do
        r:=lib.reloctable[i]
        case r.segment
        when code_seg then p:=lib.codeptr+r.offset
        when idata_seg then p:=lib.idataptr+r.offset
        when zdata_seg then p:=lib.zdataptr+r.offset
        esac

        case r.reloctype
        when locabs32_rel then
            targetoffset:=ref u32(p)^
            case r.targetsegment
            when code_seg then
                (ref u32(p)^ := cast(lib.codeptr+targetoffset))
            when idata_seg then
                (ref u32(p)^ := cast(lib.idataptr+targetoffset))
            when zdata_seg then
                (ref u32(p)^ := cast(lib.zdataptr+targetoffset))
            esac

        when locabs64_rel then
            targetoffset:=ref u32(p)^
            case r.targetsegment
            when code_seg then
                (ref u64(p)^ := cast(lib.codeptr+targetoffset))
            when idata_seg then
                (ref u64(p)^ := cast(lib.idataptr+targetoffset))
            when zdata_seg then
                (ref u64(p)^ := cast(lib.zdataptr+targetoffset))
            esac

        when impabs64_rel then

            index:=lib.importxreftable[r.stindex]           
            (ref u64(p)^+:=cast(symboladdress[index],u64))

        when impabs32_rel then
            index:=lib.importxreftable[r.stindex]           
            (ref u32(p)^+:=cast(symboladdress[index],u64))

        when imprel32_rel then
            if r.segment<>code_seg then error("imprel32?") fi
            index:=r.stindex                                
            q:=lib.codeptr+lib.codesize+(index-1)*8

            (ref u32(p)^ := q-(p+4))    
        esac

    od

    librelocated[lib.libno]:=1

end

global proc loadimports(ref librec plib)=

    ref librec qlib
    ichar name

    for i to plib.nlibs do
        dosublib(plib.libnames[i])
    od

    alloclibdata(plib)
    dosymbols(plib)
end

proc dosublib(ichar name)=
    ref librec qlib
    int n:=findlib(name)

    if not n then                                   
        n:=mxaddlib(name)
        println "Loading sublib", name
        qlib:=loadlibfile(addext(name,"ml"),n)      
        loadimports(qlib)                       
    fi
end

global function loadlibfile(ichar filename, int libno)ref librec plib=
    ref byte p

    p:=readmxfile(filename)
    if p=nil then
        error("Can't find #",filename)
    fi

    plib:=readlibfile(filename,p)
    plib.libno:=libno
    libtable[libno]:=plib   
end

proc dosymbols(ref librec lib)=

    int ix, libx, dllx
    ref byte baseaddr


    for i to lib.ndlllibs do
        adddll(lib.dllnames[i])
    od

    for i to lib.nimports do
        ix:=addsymbol(lib.importnames[i])
        lib.importxreftable[i]:=ix
    od

    for i to lib.nexports do
        ix:=addsymbol(lib.exports[i])
        if symboldefined[ix] then
            CPL "Dupl symbol:",lib.exports[i]
            NEXT
        fi
        symboldefined[ix]:=1

        case lib.exportsegs[i]
        when code_seg then baseaddr:=lib.codeptr
        when idata_seg then baseaddr:=lib.idataptr
        when zdata_seg then baseaddr:=lib.zdataptr
        else baseaddr:=nil
        esac

        symboladdress[ix]:=cast(baseaddr+lib.exportoffsets[i])
        symbollibindex[ix]:=lib.libno

    od
end

function readmxfile(ichar filename)ref byte p=

    p:=readfile(filename)
    return nil when p=nil
    (p+rfsize)^:=end_dir        

    return p
end

proc adddll(ichar name)=
    for i to ndlllibs do
        if eqstring(name,dllnametable[i]) then return fi
    od

    if ndlllibs>=maxdlls then 
        error("Too many DLLs")
    fi

    dllnametable[++ndlllibs]:=name
end

function addsymbol(ichar name)int=
    for i to nsymbols do
        if eqstring(name,symbolnametable[i]) then return i fi
    od

    if nsymbols>=maxsymbols then 
        error("Too many Imports")
    fi

    symbolnametable[++nsymbols]:=name
    return nsymbols
end

proc setspecialglobals(int cmdskip)=


    for i to nsymbols when symbolnametable[i]^='$' do
        if eqstring(symbolnametable[i],"$cmdskip") then
            (ref byte(symboladdress[i])^:=cmdskip)
        fi
    od
end

global proc runprogram(ref librec lib, int cmdskip=0)=
    ref proc fnptr
    int libno:=lib.libno


    for i to nlibs when i<>libno and not libinitdone[i] do
        calllibinit(libtable[i])
    od

    if lib.entryaddr=nil then
        error("No entry point found")
    fi

    setspecialglobals(cmdskip)


    fnptr:=cast(lib.entryaddr)

    fnptr()

    libinitdone[libno]:=1

end

global proc calllibinit(ref librec lib)=
    ref proc fnptr
    int libno:=lib.libno



    if lib.entryaddr then
        fnptr:=cast(lib.entryaddr)
        fnptr()
    fi
    libinitdone[lib.libno]:=1
end

=== mx_write.m 0 0 26/33 ===


ref dbuffer dest

symbol entrypoint

global proc writemcx(ichar filename)=
    int n

    ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

    roundsegment(ss_code,8,0x90)
    roundsegment(ss_idata,8,0)

    dest:=buffercreate()

    genword32(mcxsig)

    genbyte(version_dir)
    genstring("0.1234")

    countsymbols()
    writerelocs()

    genbyte(zdata_dir)
    genword32(ss_zdatalen)

    genbyte(code_dir)
    genword32(n:=bufferlength(ss_code))
    genblock(bufferelemptr(ss_code,0), n)

    genbyte(idata_dir)
    genword32(n:=bufferlength(ss_idata))

    genblock(bufferelemptr(ss_idata,0), n)

    int ndlls:=0, nlibs:=0
    for i to nlibfiles when libfiles[i]^<>'$' do
        if libtypes[i]='D' then ++ndlls else ++nlibs fi
    od

    genbyte(dlls_dir)
    genword32(ndlls)
    for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
        genstring(libfiles[i])
    od

    genbyte(libs_dir)
    genword32(nlibs)
    for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='L' do
        genstring(libfiles[i])
    od

    writesymbols()

    genbyte(end_dir)

CPL "WRITE MX FILE",FILENAME, =DEST.PSTART,DEST.PCURR-DEST.PSTART

    writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

global proc writerelocs=
    ref relocrec oldr
    mcxreloc newr
    int n,count
    symbol d
    ref u64 baseptr64
    ref u32 baseptr32@baseptr64

    genbyte(reloc_dir)
    genword32(n:=ss_nidatarelocs+ss_ncoderelocs)

    count:=0

    for i in code_seg..idata_seg do
        oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

        while oldr, oldr:=oldr.nextreloc do
            ++count
            clear newr

            newr.offset:=oldr.offset
            newr.segment:=(i=code_seg|idata_seg|code_seg)

            d:=ss_symboltable[oldr.stindex]

            case oldr.reloctype
            when rel32_rel then
                if d.isimport then
                    newr.stindex:=d.impindex
                    newr.reloctype:=imprel32_rel
                else
                    axerror("rel32/rel not imported")
                fi
            when addr32_rel, addr64_rel then
                if d.isimport then
                    newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
                    newr.stindex:=d.impindex
                else
                    if oldr.reloctype=addr32_rel then
                        newr.reloctype:=locabs32_rel
                    else
                        newr.reloctype:=locabs64_rel
                    fi
                    newr.targetsegment:=d.segment
                fi
            else
                axerror("reloc?")
            esac

            genblock(&newr, newr.bytes)

        od
    od
end

global proc countsymbols=
    symbol d
    for i:=1 to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.scope=export_scope then d.expindex:=++nsymexports fi
        if d.isimport then d.impindex:=++nsymimports fi
    od
end

proc writesymbols=
    symbol d
    int n
    ichar name

    genbyte(importsymbols_dir)
    genword32(nsymimports)

    for i to ss_nsymbols when ss_symboltable[i].impindex do
        d:=ss_symboltable[i]
        genstring((d.truename|d.truename|d.name))
    od

    genbyte(exportsymbols_dir)
    genword32(nsymexports)

    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            if eqstring(d.name, "main") then
                entrypoint:=d
            fi
            genstring(d.name)
        fi
    od

    genbyte(exportsegs_dir)
    genword32(nsymexports)
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            genbyte(d.segment)
        fi
    od

    genbyte(exportoffsets_dir)
    genword32(nsymexports)
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            genword32(d.offset)
        fi
    od

    genbyte(entry_dir)      
    if entrypoint then
CPL "ENTRYPT",ENTRYPOINT.OFFSET
        genword32(entrypoint.offset)
    else
        genword32(0xFFFF'FFFF)
    fi
end

proc roundsegment(ref dbuffer p, int align, value)=
    int length:=bufferlength(p)
    int newlength:=roundtoblock(length, align)

    buffercheck(p, align)

    to newlength-length do
        p.pcurr++^:=value
    od
end

proc genbyte(int x)=
    buffercheck(dest,1)
    dest.pcurr++^:=x
end

proc genword32(int x)=
    buffercheck(dest,4)
    dest.pcurr32++^:=x
end

proc genstring(ichar s)=
    genblock(s, strlen(s)+1)
end

proc genblock(ref void p, int length)=
    buffercheck(dest,length)
    memcpy(dest.pcurr, p, length)
    dest.pcurr+:=length
end
=== mm_types.m 0 1 27/33 ===
global enumdata  [0:]ichar stdnames,
        [0:]byte stdbits,
        [0:]byte stdcat =
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
    (voidcat=0,     $),         

    (d64cat,        $),         
    (x32cat,        $),         
    (x64cat,        $),         

    (shortcat,      $),         
    (blockcat,      $),         
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
=== mm_help.txt 0 1 28/33 ===
M Compiler Generating x64 native code - Windows Version

Whole-program compiler builds entire program from the lead module
into a executable file.

    mm main              
    mm main.m            
    mm -c main           

Options:

    -exe                 
    -dll                 
    -pcl                 
    -asm                 

    -opt                 

    -out:file            

    -ma                  
    -docs                
    -run                 

    @file                

Example:

     mm -run prog : abc def

Any parameters for the new program must follow " : " (spaces needed).
=== msys.m 0 1 29/33 ===
global record procinforec=
    word16      fnindex
    byte        rettype
    byte        nparams
    [12]byte    paramlist
end

record fmtrec=  
    byte    minwidth    
    i8      precision   
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
    char    heapmode    
    char    param       
    byte    spare
end

int fmtparam            

enumdata =
    std_io,file_io,str_io
end

const comma = ','

export int $cmdskip         

export int needgap          = 0
int outdev          = std_io
filehandle outchan  = nil
ref char fmtstr     = nil

const maxiostack=10
array [maxiostack]filehandle    outchan_stack
array [maxiostack]int           outdev_stack
array [maxiostack]ref char  fmtstr_stack
array [maxiostack]byte      needgap_stack

array [maxiostack]ref char  ptr_stack       
int niostack=0

array [0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

const rd_buffersize = 16384 

export ref char rd_buffer       
export int rd_length            
ref char rd_pos         
ref char rd_lastpos     
int termchar            
int itemerror           

array [4096]char printbuffer
ichar printptr
int printlen


const maxparam=128
export int nsysparams
export int ncmdparams
export int nenvstrings
export [maxparam]ichar sysparams
export ref[0:]ichar cmdparams
export ref[]ichar envstrings

const maxcallback=8
array [0..maxcallback,8]word64 callbackstack
int ncallbacks=0

word64 mask63   = 0x7FFF'FFFF'FFFF'FFFF
real offset64   = 9223372036854775808.0     
real offset32   = 9223372036854775808.0     

proc start=
    int32 nargs
    int nargs64
    ref[]ichar args
    static [128]byte startupinfo            
    int res
    

    res:=__getmainargs(&nargs,cast(&args),cast(&envstrings),0,cast(&startupinfo))
    
    nsysparams:=nargs
    
    if nsysparams>maxparam then
        printf("Too many params\n")
        stop 50
    fi

    nargs64:=nargs          
    for i:=1 to nargs64 do
        sysparams[i]:=args[i]
    od
    
    ncmdparams:=nsysparams-($cmdskip+1)
    cmdparams:=cast(&sysparams[$cmdskip+1])

    int j:=1
    nenvstrings:=0
    while envstrings[j] do
        ++nenvstrings
        ++j
    od


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

export proc m$print_startfile(ref void dev)=
    pushio()
    outchan:=cast(dev)
    if dev then
        outdev:=file_io
    else
        outdev:=std_io
    fi
    resetprintbuffer()
end

export proc m$print_startstr(ref char s)=
    ref ref char p
    pushio()

    ptr_stack[niostack]:=s
    p:=&ptr_stack[niostack]

    outchan:=cast(p)
    outdev:=str_io
end

export proc m$print_startptr(ref ref char p)=
    pushio()

    outchan:=cast(p)
    outdev:=str_io
end

export proc m$print_startcon=
    pushio()
    outdev:=std_io
    resetprintbuffer()
end

export proc m$print_setfmt(ref char format)=
    fmtstr:=format
end

export proc m$print_end=
    needgap:=0
    nextfmtchars(1)
    if niostack=1 and outdev in [std_io,file_io] then
        dumpprintbuffer()
    fi

    if niostack=0 then return fi
    outchan := outchan_stack[niostack]
    outdev  := outdev_stack[niostack]
    fmtstr  := fmtstr_stack[niostack]
    needgap := needgap_stack[niostack]


    --niostack
end

export proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
    array [20]char s

    if fmtstyle=nil then
        fmtstyle:="z8H"
    fi
    m$print_u64(a,fmtstyle)
end

export proc m$print_ptr_nf(u64 a)=
    m$print_ptr(a)
end

export proc m$print_i64(int64 a,ichar fmtstyle=nil)=
    array [40]char s
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

export proc m$print_i64_nf(int64 a)=
    m$print_i64(a)
end

export proc m$print_bool(int64 a, ichar fmtstyle=nil)=
    if a then
        m$print_str("True",fmtstyle)
    else
        m$print_str("False",fmtstyle)
    fi
end

export proc m$print_u64(word64 a,ichar fmtstyle=nil)=
    array [40]char s
    fmtrec fmt

    nextfmtchars()
    if fmtstyle=nil then
        sprintf(&.s,"%llu",a)
        printstr(&.s)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_u64(a,&fmt)
    fi
    needgap:=1
end

export proc m$print_r64(real x,ichar fmtstyle=nil)=
    array [360]char s
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

export proc m$print_r32(real32 x,ichar fmtstyle=nil)=
    m$print_r64(x,fmtstyle)
end

global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
    array [40]char s
    fmtrec fmt
    int n

    nextfmtchars()

    s[1]:=a
    s[2]:=0
    printstr(&.s)
    needgap:=1
end

export proc m$print_str(ichar s, fmtstyle=nil)=
    nextfmtchars()

    if s=nil then
        printstr("<null>")
        return
    fi

    fmtrec fmt
    if fmtstyle=nil then
        printstr(s)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_str(s,-1,&fmt)
    fi
    needgap:=1
end

export proc m$print_strn(ichar s, int length, ichar fmtstyle=nil)=
    nextfmtchars()

    if s=nil then
        printstr("<null>")
        return
    fi

    fmtrec fmt
    if fmtstyle=nil then
        printstr_n(s,length)
    else
        strtofmt(fmtstyle,-1,&fmt)
        tostr_str(s,length,&fmt)
    fi
    needgap:=1
end

export proc m$print_str_nf(ichar s)=
    m$print_str(s)
end

export proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
    nextfmtchars()
    fmtrec fmt
        abortprogram("FORMATED PRINT SLICE NOT READY")
    needgap:=1
end

export proc m$print_newline=
    needgap:=0
    nextfmtchars(1)
    printstr("\w")
end

export proc m$print_nogap=
    needgap:=0
end

export proc m$print_space=
    needgap:=0
    printstr(" ")
end

export proc printstr(ichar s)=
    printstr_n(s,strlen(s))
end

export proc printstr_n(ichar s,int n)=
    ref ref char p

    return when n=0

    if niostack=1 and outdev in [std_io,file_io] then
        addtobuffer(s,n)
    else
        dumpstr(s,n)
    fi
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
    if f=nil then
        printf("%.*s",length,s)
    else
        fprintf(f,"%.*s",length,s)
    fi
fi
end

proc printchar(int ch)=
    ref ref char p
    array [4]char str

    str[1]:=ch
    str[0]:=ch
    printstr_n(str,1)
end

global proc nextfmtchars(int lastx=0)=
    char c
    ref char pstart
    int n
    if not fmtstr then          
        if needgap then
            printchar(' ')
        fi
        needgap:=0
        return
    fi

    pstart:=fmtstr
    n:=0

    do
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

proc strtofmt(ref char s,int slen,ref fmtrec fmt) =     

    int c, base
    byte wset
    int n
    array [0:100]char str

    fmt^:=defaultfmt

    if s=nil then return fi

    if slen=-1 then slen:=strlen(s) fi

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
            base:=0
            do
                c:=s^
                if c in '0'..'9' then
                    base:=base*10+c-'0'
                    ++s
                else
                    exit
                fi
            od
            if base in 2..16 then
                fmt.base:=base
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
        when 'W', 'w' then fmt.usigned:='W'
        when 'E', 'e' then fmt.realfmt:='e'
        when 'F', 'f' then fmt.realfmt:='f'
        when 'G', 'g' then fmt.realfmt:='g'
        when '.' then
            wset:=1
        when comma,'_' then fmt.sepchar:=c
        when '+' then fmt.plus:='+'
        when 'D', 'd' then fmt.charmode:='D'
        when 'C', 'c' then fmt.charmode:='C'
        when 'M', 'm' then fmt.heapmode:='M'
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
                    fmt.minwidth:=n
                    wset:=1
                else
                    fmt.precision:=n
                fi
            fi
        endswitch
    od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
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

    return expandstr(&.str,dest,strlen(&.str),fmt)
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

function u64tostr(u64 aa,ref char s,word base,int sep)int =     
    array [0:onesixty]char t
    u64 dd
    int i,j,k,g
    int cc
    int dummy
    ref char s0

    i:=0
    k:=0
    g:=(base=10|3|4)

    repeat
        if base=10 then
            assem
                mov     rcx, [aa]
                mov     rax, rcx
                mov     rdx, 7378697629483820647
                imul    rdx
                mov     rax, rdx
                mov     rdx, rcx
                sar     rdx, 63
                sar     rax, 2
                sub     rax, rdx
                lea     rdx, [rax+rax*4]
                add     rdx, rdx
                sub     rcx, rdx
                mov     [dd], rcx
                mov     [aa], rax
            end
        else
            dd:=aa rem base
            aa:=aa/base
        fi

        t[++i]:=digits[dd]

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
    array [0:onesixty]char str              
    int i,j,k,n,w,usigned
    const i64 mindint=0x8000'0000'0000'0000



    usigned:=0
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

    if (fmt.base>10 or fmt.suffix) and fmt.lettercase='a'   then    
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
    fi

    return expandstr(&.str,s,n,fmt)
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

proc tostr_i64(int64 a, ref fmtrec fmt)=
    array [360]char str
    int n

    case fmt.charmode
    when 0 then
        n:=i64tostrfmt(a,&.str,fmt)
    when 'D','d' then
        n:=domultichar(ref char(&a),8,&.str,fmt)

    else                        
        printchar(a)            
        return
    esac

    printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
    array [360]char str
    int n

    case fmt.charmode
    when 'D','d' then
        n:=domultichar(ref char(&a),8,&.str,fmt)

    when 'C','c' then
        printchar(a)            
        return

    else
        n:=u64tostrfmt(a,&.str,fmt)
    esac

    printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
    array [360]char str,str2
    array [0:10]char cfmt
    int n

    cfmt[0]:='%'

    if fmt.precision then
        cfmt[1]:='.'
        cfmt[2]:='*'
        cfmt[3]:=fmt.realfmt
        cfmt[4]:=0
        sprintf(&.str,&.cfmt,fmt.precision,x)
    else
        cfmt[1]:=fmt.realfmt
        cfmt[2]:=0
        sprintf(&.str,&.cfmt,x)
    fi


    n:=strlen(&.str)        

    if n<fmt.minwidth then
        n:=expandstr(&.str,&.str2,n,fmt)
        strcpy(&.str,&.str2)
    fi

    printstr_n(&.str,n)
end

proc tostr_str(ref char s, int oldlen, ref fmtrec fmt) =
    int newlen,n
    ref char t

    if oldlen=-1 then
        oldlen:=strlen(s)
    fi
    newlen:=oldlen

    if fmt.quotechar or fmt.minwidth>newlen or fmt.lettercase or fmt.precision then
        if fmt.quotechar then
            newlen+:=2
        fi
        if fmt.minwidth>newlen then
            newlen:=fmt.minwidth
        fi
        t:=pcm_alloc(newlen+1)
        n:=strtostrfmt(s,t,oldlen,fmt)
        if fmt.precision then
            n min:=fmt.precision
        fi

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

    m$print_startstr(&.str)
    tostr_i64(a,fmt:=getfmt(fmtstyle))
    m$print_end()
    return getstr(&.str,fmt)
end

export proc getstrint(int64 a, ichar dest)=
    m$print_startstr(dest)
    tostr_i64(a,getfmt(nil))
    m$print_end()
end

export function strword(word64 a, ichar fmtstyle=nil)ichar=
    static [100]char str
    ref fmtrec fmt

    m$print_startstr(&.str)
    tostr_u64(a,fmt:=getfmt(fmtstyle))
    m$print_end()
    return getstr(&.str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
    static [320]char str
    ref fmtrec fmt

    m$print_startstr(&.str)
    tostr_r64(a,fmt:=getfmt(fmtstyle))
    m$print_end()
    return getstr(&.str,fmt)
end

export function getstr(ichar s, ref fmtrec fmt)ichar=
    if fmt.heapmode then
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

global proc m$read_conline=
    initreadbuffer()

    readlinen(nil,rd_buffer,rd_buffersize)

    rd_length:=strlen(rd_buffer)
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

global proc m$read_fileline(filehandle f)=
    ichar p
    initreadbuffer()

    if f=filehandle(1) then
ABORTPROGRAM("READ CMDLINE")
        return
    fi

    readlinen(f,rd_buffer,rd_buffersize)

    rd_length:=strlen(rd_buffer)
    rd_pos:=rd_buffer
    rd_lastpos:=nil
end

global proc m$read_strline(ichar s)=
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
    ref char p,s,itemstr
    char quotechar, c

    unless rd_buffer then 
        initreadbuffer()
    end unless

    s:=rd_pos

    while s^=' ' or s^=9 do
        ++s
    od

    itemstr:=s              
    rd_lastpos:=rd_pos:=s

    if s^=0 then            
        termchar:=0
        itemlength:=0
        return s
    fi

    quotechar:=0            
    if s^='"' then
        quotechar:='"'
        ++s
    elsif s^='\'' then
        quotechar:='\''
        ++s
    fi

    p:=itemstr:=s

    while s^ do
        c:=s++^
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
                if s^=quotechar then    
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

    if s^=0 then
        termchar:=0
    fi
    itemlength:=p-itemstr               
    rd_pos:=s

    return itemstr
end

export function strtoint(ichar s,int length=-1, word base=10)int64=
    byte signd
    word64 aa
    word c,d

    itemerror:=0

    if length=-1 then
        length:=strlen(s)
    fi
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

global function m$read_i64(int fmt=0)int64=
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

global function m$read_r64(int fmt=0)real=
    array [512]char str
    ref char s
    int length
    int32 numlength
    real x

    s:=readitem(length)

    if length=0 or length>=str.len then     
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

global proc m$read_str(ref char dest, int destlen=0,fmt=0)=
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
    m$read_str(dest,destlen,fmt)
end

export proc rereadln=
    rd_pos:=rd_buffer
    rd_lastpos:=rd_pos
end

export proc reread=
    rd_pos:=rd_lastpos
end

export function valint(ichar s, int fmt=0)int64=
    ref char old_pos, old_lastpos
    int64 aa

    initreadbuffer()
    old_pos:=rd_pos
    old_lastpos:=rd_lastpos

    rd_pos:=s
    aa:=m$read_i64(fmt)
    rd_pos:=old_pos
    rd_lastpos:=old_lastpos
    return aa
end

export function valreal(ichar s)real=
    ref char old_pos, old_lastpos
    real x

    initreadbuffer()
    old_pos:=rd_pos
    old_lastpos:=rd_lastpos

    rd_pos:=s
    x:=m$read_r64()
    rd_pos:=old_pos
    rd_lastpos:=old_lastpos
    return x
end

proc mclunimpl(ichar mess)=
    printf("MCL-UNIMPL: %s\n",mess)
    stop 1
end

proc dumpstr(ichar s, int n, fbuffer=0)=

    ref ref char p

    return when n=0

    if outdev=str_io then
        p:=cast(outchan)
        memcpy(p^,s,n)
        p^+:=n
        p^^:=0
        return
    fi

    if fbuffer and n>=2 and outdev=std_io then
        --printptr              
        if printptr^=10 then
            if (printptr-1)^=13 then        
                (printptr-1)^:=0
            else                            
                printptr^:=0
            fi
            puts(printbuffer)
            return
        fi
    fi

    case outdev
    when std_io then
        printf("%.*s",n,s)
    when file_io then
        fprintf(outchan,"%.*s",n,s)
    esac
end

proc dumpprintbuffer=
    if printlen then
        dumpstr(&.printbuffer,printlen,1)
    fi

    resetprintbuffer()
end

proc resetprintbuffer=
    printptr:=&.printbuffer
    printlen:=0
end

proc addtobuffer(ichar s, int n)=
    if printlen+n>=(printbuffer.len-8) then
        dumpprintbuffer()
    fi

    if n<printbuffer.len then
        memcpy(printptr,s,n)
        printptr+:=n
        printlen+:=n
        return
    fi

    dumpstr(s, n)           
end

global function m$power_i64(int64 a,n)int64=
    if n<0 then
        return 0
    elsif n=0 then
        return 1
    elsif n=1 then
        return a
    elsif (n iand 1)=0 then
        return m$power_i64(sqr a,n/2)
    else            
        return m$power_i64(sqr a,(n-1)/2)*a
    fi
end

export function vector_dupl(ref void p, int size)ref void=
    CPL "VECTOR_DUPL",P,SIZE
    p
end
=== mlib.m 0 1 30/33 ===
import clib

const mem_check=0

global [0..300]u64 allocupper
global int alloccode                
export int allocbytes               
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

const int maxmemalloc=(mem_check|500000|2)
array [maxmemalloc+1]ref int32 memalloctable
array [maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend          
ref byte pcheapptr

const int maxblockindex = 8         
export const int maxblocksize = 2048
export const int $maxblocksizexx = 2048

array [0:maxblocksize+1]byte sizeindextable 

const int size16   = 1          
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

export enumdata [0:]ichar pmnames=
    (pm_end=0,      $),
    (pm_option,     $),
    (pm_sourcefile, $),
    (pm_libfile,    $),
    (pm_colon,      $),
    (pm_extra,      $),
end

array [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

export function pcm_alloc(int n)ref void =
    ref byte p

    if not pcm_setup then
        pcm_init()
    fi

    if n>maxblocksize then          

        alloccode:=pcm_getac(n)
        allocbytes:=allocupper[alloccode]

        p:=allocmem(allocbytes)
        if not p then
            abortprogram("pcm_alloc failure")
        fi

        if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

        return p
    fi

    alloccode:=sizeindextable[n]        
    allocbytes:=allocupper[alloccode]
    smallmemtotal+:=allocbytes

    if p:=ref byte(freelist[alloccode]) then        
        if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
        freelist[alloccode]:=ref word(int((freelist[alloccode])^))

        return p
    fi

    p:=pcheapptr                
    pcheapptr+:=allocbytes          

    if pcheapptr>=pcheapend then        
        p:=pcm_newblock(allocbytes)     
        return p
    fi
    if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

    return p
end

export proc pcm_free(ref void p,int n) =
    int acode

    if n=0 then return fi

    if n>maxblocksize then      
        if mem_check then removefrommemalloc(p,n) fi

        free(p)
        return
    fi

    if p then
        acode:=sizeindextable[n]        

        smallmemtotal-:=allocupper[acode]

        if mem_check then removefrommemalloc(p,allocupper[acode]) fi

        cast(p,ref word)^:=word(int(freelist[acode]))
        freelist[acode]:=p
    fi
end


export proc pcm_freeac(ref void p,int alloc) =
    pcm_free(p,allocupper[alloc])
end


export proc pcm_clearmem(ref void p,int n) =
    memset(p,0,n)
end

export proc pcm_init =
    int j,k,k1,k2
    int64 size
    const limit=1<<33

    alloccode:=0
    if pcm_setup then
        return
    fi

    pcm_newblock(0)


    for i to maxblocksize do    
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


    if size<=maxblocksize then
        return sizeindextable[size]     
    fi

    size:=(size+255)>>8                 


    if size<=maxblocksize then
        return sizeindextable[size]+8
    fi

    size:=(size+63)>>6                  

    if size<=maxblocksize then
        return sizeindextable[size]+14
    fi

    size:=(size-2048+2047)/2048+22
    return size
end

export function pcm_newblock(int itemsize)ref void=
    static int totalheapsize
    ref byte p

    totalheapsize+:=pcheapsize
    alloccode:=0
    p:=allocmem(pcheapsize) 
    if p=nil then
        abortprogram("Can't alloc pc heap")
    fi

    pcheapptr:=p
    pcheapend:=p+pcheapsize

    if pcheapstart=nil then     
        pcheapstart:=p
    fi
    pcheapptr+:=itemsize
    return ref u32(p)
end

export function pcm_round(int n)int =
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
    ref char q
    if length=0 then return nil fi

    q:=pcm_alloc(length)
    memcpy(q,s,length)
    return q
end

proc addtomemalloc(ref int32 ptr,int size)=
    int allocated, code

    for i to maxmemalloc do
        if memalloctable[i]=ptr then
            CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
            stop 2
        fi

        if memalloctable[i]=nil then        
            memalloctable[i]:=ptr

            code:=pcm_getac(size)
            allocated:=allocupper[code]

            memallocsize[i]:=allocated
            return
        fi
    od
    CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
    stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
    int allocated, code

    code:=pcm_getac(size)
    allocated:=allocupper[code]

    for i to maxmemalloc do
        if memalloctable[i]=ptr then
            if memallocsize[i]<>ALLOCATED then
                CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, ROUNDED FREESIZE=",ALLOCATED,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
                abortprogram("MEMSIZE")
            fi
            memalloctable[i]:=nil
            return
        fi
    od
    CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
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
    stop 5
end

export function getfilesize(filehandle handlex)int=
    word32 p,size

    p:=ftell(handlex)       
    fseek(handlex,0,2)      
    size:=ftell(handlex)        
    fseek(handlex,p,seek_set)   
    return size
end

export proc readrandom(filehandle handlex, ref byte mem, int offset, size) =
    int a
    fseek(handlex,offset,seek_set)
    a:=fread(mem,1,size,handlex)            
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

    m:=malloc(size+2)       

    if m=nil then
        return nil
    fi

    readrandom(f,m,0,size)
    p:=m+size           
    (ref u16(p)^:=0)    

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


    sext:=extractext(s,1)           

    case sext^
    when 0 then                     
        strcat(&newfile[1],&newext2[1])
    when '.' then                       
        strcat(&newfile[1],&newext2[2])
    else                            
        n:=sext-s-2         
        strcpy(&newfile[1]+n+1,&newext2[1])
    esac

    return &newfile[1]
end

export function extractext(ref char s,int period=0)ichar=
    ref char t,u

    t:=extractfile(s)

    if t^=0 then            
        return ""
    fi

    u:=t+strlen(t)-1        

    while u>=t do
        if u^='.' then      
            if (u+1)^=0 then        
                return (period|"."|"")
            fi
            return u+1          
        fi
        --u
    od
    return ""           
end

export function extractpath(ref char s)ichar=
    static [0:260]char str
    ref char t
    int n

    t:=s+strlen(s)-1        

    while (t>=s) do
        switch t^
        when '\\','/',':' then      
            n:=t-s+1            
            memcpy(&.str,s,n)
            str[n]:=0
            return &.str
        endswitch
        --t
    od
    return ""           
end

export function extractfile(ref char s)ichar=
    ref char t

    t:=extractpath(s)

    if t^=0 then            
        return s
    fi

    return s+strlen(t)      
    end

export function extractbasefile(ref char s)ichar=
    static [0:100]char str
    ref char f,e
    int n,flen

    f:=extractfile(s)
    flen:=strlen(f)
    if flen=0 then      
        return ""
    fi
    e:=extractext(f,0)

    if e^ then          
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
    ref char sext

    sext:=extractext(s,1)

    if sext^=0 then                     
        return changeext(s,newext)
    fi

    return s                            
end



export function pcm_alloc32:ref void =
    ref byte p


    allocbytes:=32
    smallmemtotal+:=32

    if p:=ref byte(freelist[2]) then        
        freelist[2]:=ref word(int((freelist[2])^))
        return p
    fi

    return pcm_alloc(32)
end

export proc pcm_free32(ref void p) =


    smallmemtotal-:=32
    if mem_check then removefrommemalloc(p,32) fi

    cast(p,ref word)^:=word(int(freelist[2]))
    freelist[2]:=p
end

export function pcm_alloc64:ref void =
    ref byte p
    allocbytes:=64
    smallmemtotal+:=64

    if p:=ref byte(freelist[3]) then        
        freelist[3]:=ref word(int((freelist[3])^))
        return p
    fi

    return pcm_alloc(64)

end

export proc pcm_free64(ref void p) =

    smallmemtotal-:=64
    if mem_check then removefrommemalloc(p,64) fi

    cast(p,ref word)^:=word(int(freelist[3]))
    freelist[3]:=p
end

export function pcm_alloc16:ref void =
    ref byte p
    allocbytes:=16
    smallmemtotal+:=16

    if p:=ref byte(freelist[1]) then        
        freelist[1]:=ref word(int((freelist[1])^))
        return p
    fi

    return pcm_alloc(16)
end

export proc pcm_free16(ref void p) =

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

    if oldlen=0 then                
        dest.strptr:=pcm_alloc(n+1)
        dest.allocated:=allocbytes
        dest.length:=n              
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
        println dest.strptr,,"\c"
    else
        println @f,dest.strptr,,"\c"
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
        if readnextfileitem(fileptr,item)=0 then        
            free(filestart)                             
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

        if item^='@' then       
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

    if fileext^=0 then                          
        strcpy(&.str,name)
        if defext and not colonseen then
            name:=addext(&.str,defext)              
        fi
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
        when ' ','\t',13,10 then    
            ++p
        when 26,0 then              
            return 0
        else
            exit
        esac
    od

    case p^
    when '!', '#' then          
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
    when '"' then               
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
    int x,y
    x:=seed[1]
    y:=seed[2]
    seed[1]:=y
    x ixor:=(x<<23)
    seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
    return seed[2]+y
end

export function mrandomp:int =
    return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

export function mrandomint(int n)int=
    return mrandomp() rem n
end

export function mrandomrange(int a,b)int=
    int span
    span:=b-a+1
    if span<=0 then
        return 0
    fi
    return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
    repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
    return x
end

export function mrandomreal1:real=
    return mrandomp()/9223372036854775807
end

export function checkpackfile:ref byte=

    int a,offset,i,size
    array [100]char name
    array [300]char exefile
    ref byte packexeptr         
    int packexesize             
    ref char packfilename
    int packfilesize
    ref byte packfileptr

    macro getfileint(data,offset)=cast(data+offset,ref int32)^

    strcpy(&exefile[1],os_gethostname())
    println "Attempting to open",&exefile
    packexeptr:=readfile(&exefile[1])

    if not packexeptr then
        cpl "Can't open",&exefile,&packexeptr
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

=== mclib.m 0 1 31/33 ===
export type filehandle=ref void

importdll $cstd=
    function  malloc        (word64)ref void
    function  realloc   (ref void, word)ref void
    proc free       (ref void)
    proc memset     (ref void, int32, word)
    proc memcpy     (ref void, ref void, word)
    proc memmove        (ref void, ref void, word)
    function  clock     :int32
    function  ftell     (filehandle)int32
    function  fseek     (filehandle, int32, int32)int32
    function  fread     (ref void, word, word, filehandle)word
    function  fwrite        (ref void, word, word, filehandle)word
    function  getc      (filehandle)int32
    function  ungetc        (int32, filehandle)int32
    function  fopen     (ichar a, b="rb")filehandle
    function  fclose        (filehandle)int32
    function  fgets     (ichar, int, filehandle)ichar
    function  remove        (ichar)int32
    function  rename        (ichar, ichar)int32
    function  getchar   :int32
    proc putchar    (int32)
    proc setbuf     (filehandle, ref byte)

    function  strlen        (ichar)int
    function  strcpy        (ichar, ichar)ichar
    function  strcmp        (ichar, ichar)int32
    function  strncmp   (ichar, ichar, word)int32
    function  strncpy   (ichar, ichar, word)word
    function  memcmp        (ref void, ref void, word)int32
    function  strcat        (ichar, ichar)ichar
    function  tolower   (int32)int32
    function  toupper   (int32)int32
    function  isalpha   (int32)int32
    function  isupper   (int32)int32
    function  islower   (int32)int32
    function  isalnum   (int32)int32
    function  isspace   (int32)int32
    function  strstr        (ichar, ichar)ichar
    function  atol      (ichar)int
    function  atoi      (ichar)int32
    function  strtod        (ichar,ref ref char)real64
    function  _strdup   (ichar)ichar

    function  puts      (ichar)int32
    function  printf        (ichar, ...)int32

    function  sprintf   (ichar, ichar, ...)int32

    function  sscanf        (ichar, ichar, ...)int32
    function  scanf     (ichar, ...)int32

    function  rand      :int32
    proc srand      (word32)
    function  system        (ichar)int32

    function  fgetc     (filehandle)int32
    function  fputc     (int32,  filehandle)int32
    function  fprintf   (filehandle, ichar, ...)int32
    function  fputs     (ichar,  filehandle)int32
    function  feof      (filehandle)int32
    function  getch     :int32
    function  _getch        :int32
    function  kbhit     :int32
    function  _mkdir        (ichar)int32
    function  mkdir     (ichar)int32
    function  strchr        (ichar,int32)ichar

    function  _setmode  (int32,int32)int32

    proc _exit      (int32)
    proc "exit"     (int32)
    function  pow       (real,real)real

    function  `sin      (real)real
    function  `cos      (real)real
    function  `tan      (real)real
    function  `asin     (real)real
    function  `acos     (real)real
    function  `atan         (real)real
    function  `log      (real)real
    function  `log10        (real)real
    function  `exp      (real)real
    function  `floor        (real)real
    function  `ceil     (real)real

    proc  qsort     (ref void, word64, word64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
    function  __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof      =-1
export const seek_set   = 0
export const seek_curr  = 1
export const seek_end   = 2
=== mwindows.m 0 1 32/33 ===
module mclib

const wm_destroy=2

type wt_word    = word16
type wt_wordpm  = word32
type wt_bool    = word32
type wt_dword   = word32
type wt_wchar   = word16
type wt_wcharpm = word32
type wt_char    = byte
type wt_ichar   = ref char
type wt_ptr     = ref void
type wt_wndproc = ref proc
type wt_handle  = ref void
type wt_int     = int32
type wt_uint    = word32
type wt_long    = int32
type wt_wparam  = word
type wt_lparam  = word
type wt_point   = rpoint

export record rsystemtime =
    wt_word year
    wt_word month
    wt_word dayofweek
    wt_word day
    wt_word hour
    wt_word minute
    wt_word second
    wt_word milliseconds
end

importdll $windowsdlls=
    windows function  "GetStdHandle"(wt_dword)wt_handle
    windows function  "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
    windows function  "SetConsoleCtrlHandler"(wt_wndproc,int)int
    windows function  "SetConsoleMode"(wt_handle,wt_dword)int
    windows function  "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
                        wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
    windows function  "GetLastError":wt_dword
    windows function  "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
    windows function  "GetExitCodeProcess"(wt_handle,wt_ptr)int
    windows function  "CloseHandle"(wt_handle)int
    windows function  "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
    windows function  "FlushConsoleInputBuffer"(wt_handle)int
    windows function  "LoadLibraryA"(wt_ichar)wt_handle
    windows function  "GetProcAddress"(wt_handle,wt_ichar)ref void
    windows function  "LoadCursorA"(wt_handle,wt_ichar)wt_handle
    windows function  "RegisterClassExA"(wt_ptr)wt_wordpm
    windows function  "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)int
    windows function  "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
    windows procedure "Sleep"(wt_dword)
    windows function  "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

    windows procedure "ExitProcess"(wt_uint)
    windows proc     "PostQuitMessage"(wt_int)

    windows proc     "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

    windows function  "QueryPerformanceCounter"(ref int64)wt_bool
    windows function  "QueryPerformanceFrequency"(ref int64)wt_bool

    windows function  "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
    windows function  "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

    windows procedure "GetSystemTime"(ref rsystemtime)
    windows procedure "GetLocalTime"(ref rsystemtime)

    windows function  "GetTickCount64":u64
    windows function  "PeekMessageA"        (ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

    windows function  "GetCommandLineA":ichar

    windows function  "VirtualAlloc" (ref void, wt_dword, wt_dword, wt_dword)ref void
    windows function  "VirtualProtect" (ref void, wt_dword, wt_dword, ref wt_dword)wt_bool

end

record input_record = $caligned
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

record rspoint=(int16 x,y)

record rsrect=
    int16 leftx,top,rightx,bottom
end

global record rpoint =
    wt_long x,y
end

record rconsole=
    rspoint size,pos
    word16 attributes
    rsrect window
    rspoint maxwindowsize
end

record rstartupinfo =
    wt_dword    size
    word32 dummy1
    wt_ichar    reserved
    wt_ichar    desktop
    wt_ichar    title
    wt_dword    x
    wt_dword    y
    wt_dword    xsize
    wt_dword    ysize
    wt_dword    xcountchars
    wt_dword    ycountchars
    wt_dword    fillattribute
    wt_dword    flags
    wt_word     showwindow
    wt_word     reserved2
    word32 dummy2
    wt_ptr      reserved4
    wt_handle   stdinput
    wt_handle   stdoutput
    wt_handle   stderror
end

record rprocess_information =
    wt_handle process
    wt_handle thread
    wt_dword processid
    wt_dword threadid
end

record rwndclassex =
    wt_uint     size
    wt_uint     style
    wt_wndproc  wndproc
    wt_int      clsextra
    wt_int      wndextra
    wt_handle   instance
    wt_handle   icon
    wt_handle   cursor
    wt_handle   background
    wt_ichar    menuname
    wt_ichar    classname
    wt_handle   iconsm
end

global record rmsg =
    wt_handle   hwnd
    wt_uint     message
    word32      dummy1
    wt_wparam   wParam
    wt_lparam   lParam
    wt_dword    time
    word32      dummy2
    wt_point    pt
end

const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

const MEM_COMMIT                = 4096
const MEM_RESERVE               = 8192
const PAGE_EXECUTE              = 16
const PAGE_EXECUTE_READ         = 32
const PAGE_EXECUTE_READWRITE    = 64
const PAGE_NOACCESS             = 1


wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending          

ref function (ref void)int wndproc_callbackfn=nil   

int init_flag=0

export proc os_init=
    int i,count
    rconsole info

    hconsole:=GetStdHandle(u32(-11))
    hconsolein:=GetStdHandle(u32(-10))

    lastkey.repeatcount:=0
    keypending:=0

    SetConsoleCtrlHandler(nil,1)

    SetConsoleMode(hconsole,1 ior 2)

    init_flag:=1

end

export function  os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
    wt_dword exitcode
    int status
    int cflags:=0

    rstartupinfo si
    rprocess_information xpi

    clear si
    clear xpi

    switch newconsole
    when 0 then cflags := NORMAL_PRIORITY_CLASS
    when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
    when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
    endswitch

    si.size := rstartupinfo.bytes

    status:=CreateProcessA(
        nil,
        cmdline,
        nil,

        nil,
        1,
        cflags,

        nil,
        nil,
        &si,
        &xpi )

    if status=0 then        
        status:=GetLastError()
        printf("Winexec error: %lld\n",status)
        return -1
    fi

    WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
    GetExitCodeProcess(xpi.process,&exitcode)

    CloseHandle(xpi.process)
    CloseHandle(xpi.thread)

    return exitcode
end

export function  os_execcmd(ichar cmdline, int newconsole=0)int =
    wt_dword exitcode
    int i,j,k

    rstartupinfo si
    rprocess_information xpi

    clear si
    clear xpi

    si.size := rstartupinfo.bytes

    CreateProcessA( nil,
        cmdline,
        nil,
        nil,
        1,
        NORMAL_PRIORITY_CLASS ior (newconsole|CREATE_NEW_CONSOLE|0),
        nil,
        nil,
        &si,
        &xpi )

    CloseHandle(xpi.process)
    CloseHandle(xpi.thread)

    return 1
end

export function  os_getch:int=
    int k

    k:=os_getchx() iand 255

    return k
end

export function  os_kbhit:int=
    wt_dword count

    unless init_flag then os_init() end

    GetNumberOfConsoleInputEvents(hconsolein,&count)
    return count>1
end

export function  os_getdllinst(ichar name)u64=
    wt_handle hinst

    hinst:=LoadLibraryA(name)
    return cast(hinst)
end

export function  os_getdllprocaddr(int hinst,ichar name)ref void=
    return GetProcAddress(cast(hinst),name)
end

export proc os_initwindows=
    os_init()
    os_gxregisterclass("pcc001")
end

export proc os_gxregisterclass(ichar classname)=
    const idcarrow=32512
    rwndclassex r
    static byte registered

    if registered then
        return
    fi

    clear r

    r.size:=r.bytes
    r.style:=8 ior 32       
    r.wndproc:=cast(&mainwndproc)
    r.instance:=nil

    r.icon:=nil     
    r.cursor:=LoadCursorA(nil,ref void(idcarrow))       
    r.background:=cast(15+1)                    
    r.menuname:=nil
    r.classname:=classname
    r.iconsm:=nil   

    if RegisterClassExA(&r)=0 then
        printf("Regclass error: %lld %lld\n",classname,GetLastError())
        stop 1
    end
    registered:=1
end

global callback function  mainwndproc (
        wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)int=
    rmsg m
    int i,result
    int l
    static int count=0

    m.hwnd:=hwnd
    m.message:=message
    m.wParam:=wParam
    m.lParam:=lParam
    m.pt.x:=0
    m.pt.y:=0
    
    if (wndproc_callbackfn) then
        result:=(wndproc_callbackfn^)(&m)
    else
        result:=0
    fi

    if m.message=wm_destroy then
        return 0
    fi

    if not result then
        return DefWindowProcA(hwnd,message,wParam,lParam)
    else
        return 0
    fi
end

export proc os_setmesshandler(ref void addr)=
    wndproc_callbackfn:=addr
end

export function  os_getchx:int=
    const rightaltmask  = 1
    const leftaltmask   = 2
    const leftctrlmask  = 8
    const rightctrlmask = 4
    const shiftmask     = 16
    const capsmask      = 128
    const scrollmask    = 64
    int count
    int charcode,keyshift,keycode
    int altdown,ctrldown,shiftdown,capslock

    unless init_flag then os_init() end

    if keypending then
        lastkey:=pendkey
        keypending:=0
    else
        if lastkey.repeatcount=0 then
            repeat
                count:=0
                ReadConsoleInputA(hconsolein,&lastkey,1,&count)
            until (lastkey.eventtype=1 and lastkey.keydown=1)
        fi
    fi


    altdown     := ((lastkey.controlkeystate iand (leftaltmask ior rightaltmask))|1|0)
    ctrldown    := ((lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask))|1|0)
    shiftdown   := ((lastkey.controlkeystate iand shiftmask)|1|0)
    capslock    := ((lastkey.controlkeystate iand capsmask)|1|0)

    --lastkey.repeatcount       

    charcode:=lastkey.asciichar
    keycode:=lastkey.virtualkeycode iand 255

    if charcode<0 then
        if charcode<-128 then
            charcode:=0
        else
            charcode+:=256
        fi
    fi


    if altdown and ctrldown and charcode=166 then
        altdown:=ctrldown:=0
    else
        if altdown or ctrldown then
            charcode:=0
            if keycode>='A' and keycode<= 'Z' then
                charcode:=keycode-'@'
            fi
        fi
    fi

    keyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown

    return keyshift<<24 ior keycode<<16 ior charcode
end

export function  os_getos=>ichar=
    if $targetbits=32 then
        return "W32"
    else
        return "W64"
    fi
end

export function  os_gethostsize=>int=
    return $targetbits
end

export function  os_shellexec(ichar opc, file)int=
    return system(file)
end

export proc  os_sleep(int a)=
    Sleep(a)
end

export function  os_getstdin:filehandle =
    return fopen("con","rb")
end

export function  os_getstdout:filehandle =
    return fopen("con","wb")
end

export function  os_gethostname:ichar=
    static [300]char name
    static int n

    GetModuleFileNameA(nil,&.name,name.bytes)
    return &.name
end

export function  os_getmpath:ichar=
    return F"C:\m\"
end


export function  os_clock:int64=
    return clock()
end

export function  os_ticks:int64=
    return GetTickCount64()
end

export function  os_iswindows:int=
    return 1
end

export proc os_getsystime(ref rsystemtime tm)=
    GetLocalTime(tm)
end

export proc os_peek=
    int ticks
    static int lastticks
    array [100]byte m
    ticks:=GetTickCount64()
    if ticks-lastticks>=1000 then
        lastticks:=ticks
        PeekMessageA(&m,nil,0,0,0)
    fi
end

export function  os_allocexecmem(int n)ref byte=
    ref byte p
    u32 oldprot
    int status

    p := VirtualAlloc(nil, n, MEM_RESERVE ior MEM_COMMIT, PAGE_NOACCESS);
    if p = nil then return nil fi

    status := VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot);
    if status = 0 then return nil fi

    return p
end

proc start=
end

=== mwindll.m 0 1 33/33 ===
export function os_calldllfunction(ref proc fnaddr,
        int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
    word64 a
    real64 x
    int nextra, pushedbytes

    nextra:=0

    if nargs<4 then
        nextra:=4-nargs         
    elsif nargs.odd then        
        nextra:=1
    fi

    pushedbytes:=(nextra+nargs)*8

    to nextra do
        asm push 0
    od

    for i:=nargs downto 1 do
        a:=args^[i]                 
        asm push word64 [a]
    od


    assem
        mov D10,[Dstack]
        movq XMM0,[Dstack]
        mov D11,[Dstack+8]
        movq XMM1,[Dstack+8]
        mov D12,[Dstack+16]
        movq XMM2,[Dstack+16]
        mov D13,[Dstack+24]
        movq XMM3,[Dstack+24]
    end

    if retcode='I' then
        a:=((ref function:int64(fnaddr))^())
        asm add Dstack,[pushedbytes]
        return a
    else
        x:=((ref function:real64(fnaddr))^())
        asm add Dstack,[pushedbytes]
        return word64@(x)
    fi
end 

global function os_pushargs(ref[]word64 args, int nargs, nextra,
                    ref proc fnaddr, int isfloat)word64=
    word64 a
    real64 x

    to nextra do
        asm push 0
    end

    for i to nargs do
        a:=args[i]
        asm push word64 [a]
    od

    if isfloat then
        x:=((ref function:real64(fnaddr))^())
        a:=int64@(x)
    else
        a:=((ref function:int64(fnaddr))^())
    fi

    return a
end
=== END ===
1 mm.m
2 mmcli.m
3 mm_assem.m
4 mc_blockmcl.m
5 mm_decls.m
6 mm_diags_dummy.m
7 mm_export.m
8 mm_lex.m
9 mm_lib.m
10 mm_libsources.m
11 mm_modules.m
12 mm_name.m
13 mm_parse.m
14 mm_support.m
15 mm_tables.m
16 mm_type.m
17 mc_genmcl.m
18 mc_genss.m
19 mc_libmcl.m
20 mc_decls.m
21 mc_objdecls.m
22 mc_writeexe.m
23 mx_decls.m
24 mx_run.m
25 mx_lib.m
26 mx_write.m
27 mm_types.m
28 mm_help.txt
29 msys.m
30 mlib.m
31 mclib.m
32 mwindows.m
33 mwindll.m
