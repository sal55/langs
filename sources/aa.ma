=== MA 16 ===
=== aa.m 0 0 1/16 ===

module aacli
module aa_mcxdecls
module aa_decls
module aa_tables
module aa_objdecls
module aa_lex
module aa_parse
module aa_showss
module aa_writeobj
module aa_writeexe
module aa_writess
module aa_disasm
module aa_genss
module aa_lib

=== aacli.m 0 0 2/16 ===

import* aa_common

global int logdest=0            

byte fshowmcl
byte fshowss
byte fshowss2
byte fshowsx
byte fshowtiming

global enumdata []ichar optionnames=
    (lex_sw,        "lex"),     
    (parse_sw,      "parse"),
    (gen_sw,        "gen"),
    (obj_sw,        "obj"),
    (dll_sw,        "dll"),
    (exe_sw,        "exe"),
    (lib_sw,        "lib"),
    (run_sw,        "run"),

    (mcl_sw,        "mcl"),
    (ss_sw,         "ss"),
    (ss2_sw,        "ss2"),
    (sx_sw,         "sx"),
    (time_sw,       "time"),
    (s_sw,          "s"),
    (d_sw,          "d"),
    (v_sw,          "v"),
    (q_sw,          "q"),
    (help_sw,       "help"),
    (out_sw,        "out"),
    (main_sw,       "main"),
    (start_sw,      "start"),
end

global int axlevel = exe_sw

const logfile = "mx.log"

ichar inputfile
ichar outputfile

proc main=
    ichar ext
    ref strbuffer ss
    int ntokens,t,i,U,j


    T:=CLOCK()
    initall()

    getinputoptions()

    inputfile:=moduletable[1].filename


    initlogfile()

    if axlevel=lex_sw then
        if nmodules>1 then loaderror("lex test/multi files") fi
        lextest(inputfile)
    else
        if outputfile=nil then
            outputfile:=inputfile
        fi
        ext:=
            case axlevel
            when dll_sw then "dll"
            when obj_sw then "obj"
            when lib_sw, run_sw then "lib"
            else
                "exe"
            esac
            outputfile:=pcm_copyheapstring(changeext(outputfile,ext))

        if not fquiet then
                println "Assembling",inputfile,"to",outputfile
        fi

        if fverbose then
            showcaption()
            println
        fi
        loadsourcefiles()
INT TT:=CLOCK()
        parsemodules()
TT:=CLOCK()-TT
CPL =TT

        genss()
        case axlevel
        when obj_sw then
            if fshowss or fshowsx then
                initsectiontable()                  
                ss:=writessdata(0)
                gs_println(ss,logdev)
            fi

            writess(outputfile)
        when exe_sw, dll_sw then
            initsectiontable()
            if fshowss then
                ss:=writessdata(0)
                gs_println(ss,logdev)
            fi

            genexe(nil,outputfile, axlevel=dll_sw)
            if fshowsx then
                ss:=writessdata(1)
                gs_println(ss,logdev)
            fi

            writeexe(outputfile, axlevel=dll_sw)

        when lib_sw, run_sw then
            writemcx(outputfile)
            if fshowss2 then
                ss:=showssdata()
                gs_println(ss,logdev)
            fi

        esac

        if fshowmcl then
            ss:=writemclblock()
            gs_println(ss,logdev)
        fi
    fi

    if fshowtiming then
        T:=CLOCK()-T
        CPL "Time",T
    fi

    closelogfile()
    stop 0
end

proc loadsourcefiles=
    int i
    ichar source

    for i to nmodules do
        source:=cast(readfile(moduletable[i].filename))
        if source=nil then
            loaderror_s("Can't load file: %s",moduletable[i].filename)
        fi
        moduletable[i].source:=source
    od
end

proc parsemodules=
    int i
    ichar source

    for i to nmodules do
        currmoduleno:=i
        modulenamelist:=nil
        readmodule(i)

        checkundefined()
        if nundefined then
            println "Couldn't assemble - press key"
            stop 1
        fi

        scanglobals()           
        if fshowsx then
        fi
        if i<>nmodules then
            resethashtable()
        fi
    od

if fshowsx then
fi

ref mclrec m

m:=mccode

while m do
    fixopnd(m.a)
    fixopnd(m.b)
    m:=m.nextmcl
od

end

proc fixopnd(ref opndrec a)=
    ref strec d
    if a=nil then return fi
    if a.labeldef then
        d:=a.labeldef
        if d.basedef then
            a.labeldef:=d.basedef
        fi
    fi
end

proc initlogfile=
    case logdest
    when 2 then
        remove(logfile)
        logdev:=cast(fopen(logfile,"w"))
    when 0,1 then
        logdev:=nil
    esac

end

proc closelogfile=          
    [512]char str

    if logdest=2 then
        fclose(logdev)

        print @&.str,f"\m\ed.bat",logfile

CPL "CALL EXECWAIT",STR
        os_execwait(&.str,1,nil)
    fi
    end

proc initall=
    pcm_init()
    initlex()
    initlib()
end

proc lextest(ichar file)=
    loadsourcefiles()
    initsourcefile(moduletable[1].source)

    lxsymbol:=eolsym
    while lxsymbol<>eofsym do
        lex()
    od
end

proc getinputoptions=
    const slash='-'
    int i,j,k
    int paramno,pmtype,sw
    ichar name,value,ext

    paramno:=1

    while pmtype:=nextcmdparamnew(paramno,name,value,".asm") do
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
                stop 1
            od
        when pm_sourcefile then
            addmodule(name)
        when pm_libfile then
            addsearchlib(convlcstring(name))
        esac
    od

    if nmodules=0 and nsearchlibs=0 then
        showcaption()
        println
        println "Usage:"
        println "   ",,cmdparams[0],"filename[.asm]           # Assemble filename.asm to filename.exe"
        println "   ",,cmdparams[0],"-help                    # Show other options"
        stop 1
    fi

    if fshowss or fshowsx or fshowss2 or fshowmcl then
        if logdest=0 then logdest:=2 fi
    fi

    addsearchlib("msvcrt")
    addsearchlib("gdi32")
    addsearchlib("user32")
    addsearchlib("kernel32")

    if nmodules=0 then
        loaderror("No input files specified")
    fi
end

proc do_option(int sw, ichar value)=

    case sw
    when lex_sw, parse_sw, gen_sw, obj_sw, exe_sw, dll_sw, lib_sw, run_sw then
        axlevel:=sw
    when mcl_sw then
        fshowmcl:=1
    when ss_sw then
        fshowss:=1
    when ss2_sw then
        fshowss2:=1
    when sx_sw then
        fshowsx:=1
    when time_sw then
        fshowtiming:=1
    when s_sw then
        logdest:=1
    when d_sw then
        logdest:=2
    when v_sw then
        fverbose:=1
    when q_sw then
        fquiet:=1
    when help_sw then
        showhelp()
    when out_sw then
        outputfile:=pcm_copyheapstring(value)
    when main_sw then
    when start_sw then
    esac

end

proc showhelp=
    println
    println strinclude "aa_help.txt"
    stop 1
end

proc showcaption=
    print "AA Assembler/Linker",$date
end

proc loaderror(ichar mess)=
    println "Error:",mess
    stop 1
end

proc loaderror_s(ichar mess,s)=
    [256]char str
    sprintf(&.str,mess,s)
    loaderror(&.str)
end

proc addmodule(ichar name)=
    if nmodules>=maxmodules then
        loaderror("Too many modules")
    fi
    ++nmodules
    moduletable[nmodules].filename:=pcm_copyheapstring(name)
    moduletable[nmodules].name:=pcm_copyheapstring(extractfile(name))
    moduletable[nmodules].source:="<empty>"

end

proc addsearchlib(ichar name)=
    [300]char str

    if eqstring(extractext(name),"mcx") then
        addimportlib(name)
        return
    fi

    name:=changeext(name,"")

    for i to nsearchlibs do
        if eqstring(searchlibs[i],name) then return fi
    od

    if nsearchlibs>=maxsearchlibs then
        loaderror("Too many DLLs")
    fi
    ++nsearchlibs
    searchlibs[nsearchlibs]:=pcm_copyheapstring(name)
end

proc addimportlib(ichar name)=
    [300]char str

    name:=changeext(name,"")

    for i to nimportlibs do
        if eqstring(importlibs[i],name) then return fi
    od

    if nimportlibs>=maximportlibs then
        loaderror("Too many LIBs")
    fi
    ++nimportlibs
    importlibs[nimportlibs]:=pcm_copyheapstring(name)
end

proc showmodules=
    int i

    println "Modules:",nmodules
    for i:=1 to nmodules do
        println "  ",i,,":",
            padstr(moduletable[i].name,13),
            padstr(moduletable[i].filename,25),
            strlen(moduletable[i].source)
    od
    println
    println "DLL Libs:",nsearchlibs
    for i:=1 to nsearchlibs do
        println "  ",i,,":",searchlibs[i]
    od
    println
    println "Import Libs:",nimportlibs
    for i:=1 to nimportlibs do
        println "  ",i,,":",importlibs[i]
    od
    println
end

function getemptyst(ref strec d)ref strec=
    ref strec dnew

    if d.ksymbol then                   
        dnew:=pcm_allocz(strec.bytes)
        dnew.name:=d.name
        dnew.namelen:=d.namelen
        dnew.ksymbol:=d.ksymbol
        dnew.subcode:=d.subcode
        dnew.regsize:=d.regsize
        return dnew
    fi
    return nil
end

function findduplname(ref strec d)ref strec=

    ref strec e
    if d.basedef then
        return d.basedef
    fi

    e:=dupltable[d.htfirstindex]

    while e do
        if d.namelen=e.namelen and memcmp(d.name,e.name,d.namelen)=0 then
            d.basedef:=e
            return e
        fi
        e:=e.nextdupl
    od
    return nil
end

proc adddupl(ref strec d)=

    d.nextdupl:=dupltable[d.htfirstindex]
    dupltable[d.htfirstindex]:=d
end

proc scanglobals=

    ref strec d,e

    d:=modulenamelist

    while d do
        case d.symbol
        when importedsym then
            e:=findduplname(d)
            if e then
                case e.symbol
                when importedsym then           
                when exportedsym then
                    d.symbol:=exportedsym       
                    d.reftype:=e.reftype:=fwd_ref
                esac
            else
                addimport(d)
                adddupl(d)
            fi
        when exportedsym then
            e:=findduplname(d)
            if e then
                case e.symbol
                when importedsym then
                    e.symbol:=exportedsym       
                    d.reftype:=e.reftype:=fwd_ref
                when exportedsym then           
                    CPL MODULETABLE[D.MODULENO].NAME,D.NAME,D.HTINDEX
                    CPL MODULETABLE[E.MODULENO].NAME,E.NAME,E.HTINDEX
                    serror_s("Multiply-defined global: %s",d.name)
                esac
            else
                e:=d
                addimport(d)
                adddupl(d)
            fi
        esac

        d:=d.nextdef
    od
end

proc resethashtable=

    ref strec d,e

    d:=modulenamelist

    while d do
        lexhashtable[d.htindex]:=getemptyst(d)
        d:=d.nextdef
    od

    modulenamelist:=nil

end
=== aa_mcxdecls.m 0 0 3/16 ===

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


export enumdata []ichar segmentnames =
    (code_seg,      "code"),
    (idata_seg,     "idata"),
    (zdata_seg,     "zdata"),
    (rodata_seg,    "rodata"),
    (impdata_seg,   $),
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
global [maxlibs]byte        libdefined      
global int nlibs


global [maxsymbols]ichar    symbolnametable 
global [maxsymbols]byte     symboldefined   
global [maxsymbols]ref void symboladdress   
global [maxsymbols]int16    symbollibindex  
global [maxsymbols]byte     symboldllindex  
global int nsymbols

=== aa_decls.m 0 0 4/16 ===

global const compilerversion="2018.1.22"




global type symbol = ref strec

global record fwdrec =
    ref fwdrec nextfwd
    int32 offset
    int16 reltype
    int16 seg
end

global record opndrec = 
    ref strec labeldef  
    union
        int64 value     
        real64 xvalue   
        ref char svalue
    end
    byte mode       
    byte size       
    byte reg        
    byte regix      

    byte scale      
    byte addrsize   
    byte valtype    
    byte spare2
end

global record strec =
    ichar name          
    ref fwdrec fwdrefs  
        ref opndrec expr    
            int32 offset        
            int32 stindex       
            int32 importindex   

    byte symbol         
    byte ksymbol        
    byte subcode        
    byte regsize        

    byte scope          
    byte reftype        
    byte segment        
    byte namelen

    ref strec basedef       
    ref strec nextdef       
    ref strec nextdupl      

    int32 moduleno
    word32 htindex              
    word32 htfirstindex         

    word32 impindex         
    word32 expindex

    [40]BYTE SPARE
end

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

global record modulerec =
    ichar filename
    ichar name
    ichar source
end

global record stlistrec =
    ref strec def
    ref stlistrec nextitem
end

global int lxfileno=0   
global int lxlineno=0   

global int nsourcefiles=0   

global const maxmodules=200
global const maxsearchlibs=30
global const maximportlibs=30
global [maxmodules]modulerec moduletable
export [maxsearchlibs]ichar searchlibs
export [maximportlibs]ichar importlibs
global int nmodules
export int nsearchlibs
export int nimportlibs

global const hstsize=65536

global const hstmask=hstsize-1
global [0:hstsize]ref strec lexhashtable
global [0:hstsize]ref strec dupltable       

global ref void logdev      

global int fverbose=0       
global int fquiet=0

global int LINECOUNT=0

global int nundefined=0
global int alineno=0

global int ss_zdatalen
global ref dbuffer ss_zdata         
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

global const init_ss_symbols=16384
global ref []ref strec ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref stlistrec globalimportlist       

global ref strec modulenamelist         
global int currmoduleno

GLOBAL INT NMCLASM
GLOBAL INT NMCLOPNDSASM
=== aa_tables.m 0 0 5/16 ===

global enumdata []ichar symbolnames=
    (errorsym,          $),     
    (commasym,          $),     
    (colonsym,          $),     
    (dcolonsym,         $),     
    (lsqsym,            $),     
    (rsqsym,            $),     

    (addsym,            $),     
    (subsym,            $),     
    (mulsym,            $),     

    (eqsym,             $),     

    (eolsym,            $),     
    (eofsym,            $),     

    (hashsym,           $),     

    (intconstsym,       $),     
    (realconstsym,      $),     
    (stringconstsym,    $),     

    (namesym,           $),     
    (namedconstsym,     $),     
    (fwdlocalsym,       $),     
    (localsym,          $),     
    (importedsym,       $),     
    (exportedsym,       $),     

    (kopcodesym,        $),     
    (kregsym,           $),     
    (kxregsym,          $),     
    (kfregsym,          $),     
    (kmregsym,          $),     
    (kjmpccsym,         $),     
    (ksetccsym,         $),     
    (kmovccsym,         $),     
    (kprefixsym,        $),     
    (ksegnamesym,       $),     
    (kimportlibsym,     $),     
    (kimportdllsym,     $),     

    (kdummysym,         $)      
end

global enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

    (m_comment,         $,      0,      0),     
    (m_blank,           $,      0,      0),     
    (m_end,             $,      0,      0),     

    (m_labelx,          $,      1,      0),     
    (m_nop,             $,      0,      0x90),      
    (m_param,           $,      1,      0),     
    (m_assem,           $,      1,      0),     
    (m_proc,            $,      1,      0),     

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
    (m_retn,            $,      1,      0),     
    (m_leave,           $,      0,      0xC9),      

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

    (m_and,             $,      2,      0x04),  
    (m_or,              $,      2,      0x01),  
    (m_xor,             $,      2,      0x06),  
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
    (m_not,             $,      1,      2),     

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

    (m_halt,            $,      0,      0xF4),  
end

global enumdata [0:]ichar regnames, [0:]byte regcodes =
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
end

global const rframe = r14
global const rstack = r15




global tabledata []ichar dregnames, []byte regsizes, []byte regindices =
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

global []ichar xregnames = (
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

global []ichar fregnames = (
    "st0",
    "st1",
    "st2",
    "st3",
    "st4",
    "st5",
    "st6",
    "st7")

global []ichar mregnames = (
    "mmx0",
    "mmx1",
    "mmx2",
    "mmx3",
    "mmx4",
    "mmx5",
    "mmx6",
    "mmx7")

global enumdata [0:]ichar condnames =

    (ov_cond    = 0,    "o"),
    (nov_cond   = 1,    "no"),

    (ltu_cond   = 2,    "b"),
    (geu_cond   = 3,    "ae"),

    (eq_cond    = 4,    "z"),
    (ne_cond    = 5,    "nz"),

    (leu_cond   = 6,    "be"),
    (gtu_cond   = 7,    "a"),

    (s_cond     = 8,    "s"),
    (ns_cond    = 9,    "ns"),

    (p_cond     = 10,   "p"),
    (np_cond    = 11,   "np"),

    (lt_cond    = 12,   "l"),
    (ge_cond    = 13,   "ge"),

    (le_cond    = 14,   "le"),
    (gt_cond    = 15,   "g"),
end

global tabledata []ichar jmpccnames, []byte jmpcccodes =
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

global tabledata []ichar setccnames, []byte setcccodes =
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
    ("setc",    ltu_cond),
    ("setnc",   geu_cond),
end

global tabledata []ichar cmovccnames, []byte cmovcccodes =
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
    ("cmovc",   ltu_cond),
    ("cmovnc",  geu_cond),
end

global tabledata []ichar prefixnames, []byte prefixsizes =
    ("byte",    1),     
    ("word",    2),
    ("word16",  2),
    ("word32",  4),
    ("dword",   4),
    ("word64",  8),
    ("qword",   8),
    ("tword",   10),
    ("word80",  10),
    ("word128", 16)
end

global enumdata [0:]ichar reftypenames =    
    (extern_ref=0,      $),     
    (fwd_ref,           $),     
    (back_ref,          $),     
end

=== aa_objdecls.m 0 0 6/16 ===
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
    (rel32_rel,     $),
    (rel8_rel,      $),             
end

global enumdata [0:]ichar coffscopenames =
    (cofflocal_scope=0, $),
    (export_scope,      $),
    (import_scope,      $),
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
    ref strec def               
    int libno                   
    ichar name                  
    int hintnameoffset          
    int iatoffset               
    int thunkoffset             
end

global record exportrec =       
    ref strec def               
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

=== aa_lex.m 0 0 7/16 ===
macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10


global int lxsymbol     
global int lxsubcode    

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
int lxhashvalue

global ref byte lxsptr      
ref byte lxstart        
global ref strec lxsymptr       

[0..255]char alphamap
[0..255]char digitmap
[0..255]char commentmap

global proc lex=
    int i, c, d, hsum, length
    ref byte pstart

    lxsubcode:=0


    doswitch c:=lxsptr++^
    when 'a'..'z','$','_','.' then
        pstart:=lxsptr-1        
        hsum:=c
    doname::

        doswitch c:=lxsptr++^
        when 'a'..'z','0'..'9','_','$','.' then
            hsum:=hsum<<4-hsum+c
        when 'A'..'Z' then
            (lxsptr-1)^:=c+32
            hsum:=hsum<<4-hsum+c+' '
        else
            --lxsptr
            exit
        end

        lxlength:=lxsptr-pstart
        lxhashvalue:=hsum<<5 -hsum

        if lookuplex(cast(pstart),lxlength) then
            if lxsymptr.ksymbol then            
                lxsymbol:=lxsymptr.ksymbol
                lxsubcode:=lxsymptr.subcode
            else
                lxsymbol:=lxsymptr.symbol
            fi
        else
            lxsymbol:=namesym
        fi

        return

    when 'A'..'Z' then
        pstart:=lxsptr-1
        hsum:=pstart^:=c+32
        goto doname

    when '0'..'9' then
        readnumber(c)
        return

    when '`' then
        pstart:=lxsptr      
        hsum:=0

        doswitch c:=lxsptr^
        when 'A'..'Z','a'..'z','0'..'9','_','$','.' then
            ++lxsptr
            hsum:=hsum<<4-hsum+c
        else
            exit
        end

        lxsymbol:=namesym
        if pstart=lxsptr then
            lxerror("NULL ` name")
        fi
        lxlength:=lxsptr-pstart
        lxhashvalue:=hsum<<5-hsum

        if lookuplex(cast(pstart),lxlength) then
            lxsymbol:=lxsymptr.symbol           
            if lxsymbol=0 then                  
                lxsymbol:=lxsymptr.symbol:=namesym
            fi
        fi
        return

    when '!',';','#' then           

        while commentmap[lxsptr++^] do od

        if (lxsptr-1)^=0 then --lxsptr fi
        ++lxlineno

        lxsymbol:=eolsym
        return

    when ',' then
        lxsymbol:=commasym
        return

    when ':' then
        if lxsptr^=':' then
            lxsymbol:=dcolonsym
            ++lxsptr
        else
            lxsymbol:=colonsym
        fi
        return

    when '[' then
        lxsymbol:=lsqsym
        return

    when ']' then
        lxsymbol:=rsqsym
        return

    when '+' then
        lxsymbol:=addsym
        return

    when '-' then
        lxsymbol:=subsym
        return

    when '*' then
        lxsymbol:=mulsym
        return

    when '=' then
        lxsymbol:=eqsym
        return

    when '\'' then
        pstart:=lxsptr

        do
            switch lxsptr++^
            when '\'' then
                exit
            when cr,lf then
                lxerror("String not terminated")
            endswitch
        od
        length:=lxsptr-pstart-1
        lxvalue:=0
        for i:=length downto 1 do
            lxvalue:=lxvalue<<8+(pstart+i-1)^
        od
        lxsymbol:=intconstsym
        return

    when '"' then
        pstart:=lxsptr

        do
            switch lxsptr++^
            when '"' then
                lxsvalue:=cast(pstart)
                lxlength:=lxsptr-pstart-1
                (lxsvalue+lxlength)^:=0
                lxsymbol:=stringconstsym
                return
            when cr,lf,etx,0 then
                lxerror("String not terminated")
            endswitch
        od

    when ' ',9 then

    when cr then            

    when lf then
        ++lxlineno
        lxsymbol:=eolsym
        return

    when 0,etx then
        lxsymbol:=eofsym
        --lxsptr
        return
    else
        lxsymbol:=errorsym
        lxvalue:=c
        return

    end doswitch
end

global proc initlex=
    lxsubcode:=0
    lxsymbol:=errorsym

    lxlineno:=0

    int i
    for i:=0 to 255 do
        switch i
        when 'A'..'Z','a'..'z','$','_','0'..'9' then
            alphamap[i]:=1
        end
        switch i
        when '0'..'9' then
            digitmap[i]:=1
        end
        commentmap[i]:=1
    od

    commentmap[0]:=0
    commentmap[lf]:=0

    inithashtable()
end

proc readreal(ref[]char s,int slen, intlen,exponseen)=
    int i,fractlen,expon,exponsign,c,digs
    int64 x

    if intlen=0 or intlen=slen then
        fractlen:=0
    else
        fractlen:=slen-intlen
    fi

    expon:=0
    exponsign:=0

    if exponseen then
        case c:=lxsptr++^
        when '+' then
        when '-' then
            exponsign:=1
        else
            --lxsptr
        esac

        digs:=0
        doswitch c:=lxsptr++^
        when '0'..'9' then
            expon:=expon*10+c-'0'
            ++digs
        else
            --lxsptr
            exit
        end
        if digs=0 then
            lxerror("Exponent error")
        fi
        if exponsign then expon:=-expon fi
    fi

    expon:=expon-fractlen

    lxxvalue:=0.0

    for i:=1 to slen do
        c:=s^[i]
        lxxvalue:=lxxvalue*10.0+(c-'0')
    od

    if expon>0 then
        to expon do
            lxxvalue:=lxxvalue*10.0
        od
    elsif expon<0 then
        to -expon do
            lxxvalue:=lxxvalue/10.0
        od
    fi

    lxsymbol:=realconstsym
end

proc readnumber(int c)=
    [256]char str
    int i,d,intlen,slen

    d:=lxsptr^
    case d
    when 'x','X' then           
        case c
        when '0' then           
            ++lxsptr
            readhex()
            return
        when '2' then           
            ++lxsptr
            readbinary()
            return
        else
            cpl c
            lxerror("Base not supported")
        esac
    esac

    str[1]:=c
    slen:=1
    intlen:=0

    doswitch c:=lxsptr++^
    when '0'..'9' then
        str[++slen]:=c
    when '_','\'','`' then
    when '.' then
        intlen:=slen
    when 'e','E' then
        readreal(&str,slen,intlen,1)
        return
    else
        --lxsptr
        exit
    end

    if intlen then
        readreal(&str,slen,intlen,0)
        return
    fi

    if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
        lxerror("Overflow in 64-bit value")
    fi

    lxsymbol:=intconstsym

    lxvalue:=0
    for i:=1 to slen do
        lxvalue:=lxvalue*10+str[i]-'0'
    od
end

proc readbinary=
    int ndigs

    ndigs:=0
    lxvalue:=0
    doswitch lxsptr++^
    when '0' then
        lxvalue:=lxvalue*2
        ++ndigs
    when '1' then
        lxvalue:=lxvalue*2+1
        ++ndigs
    when '2'..'9' then
        lxerror("Bad binary digit")
    when '_','\'','`' then
    else
        --lxsptr
        exit
    end

    if ndigs=0 then
        lxerror("No bin digits")
    elsif ndigs>64 then
        lxerror("Overflow in binary number")
    fi
    lxsymbol:=intconstsym
end

proc readhex=
    int ndigs,c

    ndigs:=0
    lxvalue:=0
    doswitch c:=lxsptr++^
    when '0'..'9' then
        lxvalue:=lxvalue*16+c-'0'
        ++ndigs
    when 'A'..'F' then
        lxvalue:=lxvalue*16+(c-'A'+10)
        ++ndigs
    when 'a'..'f' then
        lxvalue:=lxvalue*16+(c-'a'+10)
        ++ndigs
    when '_','\'','`' then
    else
        --lxsptr
        exit
    end

    if ndigs=0 then
        lxerror("No hex digits")
    elsif ndigs>16 then
        lxerror("Overflow in hex number")
    fi
    lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
    PRINT CAPTION,":"
    PRINTSYMBOL()
end

global proc printsymbol(filehandle dev=nil)=
    [256]char str

    strcpy(&.str,symbolnames[lxsymbol])
    str[strlen(&.str)-2]:=0

    print @dev,&.str
    to 14-strlen(&.str) do print @dev," " od

    case lxsymbol
    when namesym then

        print @dev,lxsymptr.name

    when intconstsym then
        print @dev, lxvalue
    when realconstsym then
        print @dev, lxxvalue
    when stringconstsym then
        print @dev,"""",,lxsvalue,,""""
    when errorsym then
        print @dev,lxvalue
    else
        print @dev,symbolnames[lxsymbol]
        if lxsubcode then
            print " ",,lxsubcode
        fi

    end

    println @dev
end

proc clearhashtable=
end

proc inithashtable=
    [32]char str
    int i

    if hstsize>65536 then
    fi

    clearhashtable()

    for i to mclnames.len do
        addreservedword(mclnames[i]+2,kopcodesym,i)
    od

    for i to dregnames.len do
        addreservedword(dregnames[i],kregsym,regindices[i])
        lxsymptr.regsize:=regsizes[i]
    od

    for i to xregnames.len do
        addreservedword(xregnames[i],kxregsym,i)
    od

    for i to fregnames.len do
        addreservedword(fregnames[i],kfregsym,i)
    od

    for i to mregnames.len do
        addreservedword(mregnames[i],kmregsym,i)
    od

    for i to jmpccnames.len do
        addreservedword(jmpccnames[i],kjmpccsym,jmpcccodes[i])
    od

    for i to setccnames.len do
        addreservedword(setccnames[i],ksetccsym,setcccodes[i])
    od

    for i to cmovccnames.len do
        addreservedword(cmovccnames[i],kmovccsym,cmovcccodes[i])
    od

    for i to prefixnames.len do
        addreservedword(prefixnames[i],kprefixsym,prefixsizes[i])
    od

    for i to segmentnames.len do
        addreservedword(segmentnames[i],ksegnamesym,i)
    od

    addreservedword("aframe",kregsym,r14); lxsymptr.regsize:=4
    addreservedword("dframe",kregsym,r14); lxsymptr.regsize:=8
    addreservedword("astack",kregsym,r15); lxsymptr.regsize:=4
    addreservedword("dstack",kregsym,r15); lxsymptr.regsize:=8
    addreservedword("dprog",kregsym,r8); lxsymptr.regsize:=8
    addreservedword("dsptr",kregsym,r9); lxsymptr.regsize:=8

    addreservedword("importlib",kimportlibsym,0)
    addreservedword("importdll",kimportdllsym,0)
end

proc addreservedword(ichar name,int symbol,subcode)=
    lxhashvalue:=gethashvalue(name)
    if lookuplex(name,0) then
        cpl =name
        lxerror("DUPL NAME")
    fi

    lxsymptr.symbol:=0
    lxsymptr.ksymbol:=symbol
    lxsymptr.subcode:=subcode
end

global proc printhashtable(filehandle devx,ichar caption)=
    ref strec r
    int count,i

    println @devx,caption,":"
    count:=0
    for i:=0 to lexhashtable.upb do
        r:=lexhashtable[i]
        if R AND r.name then
            count+:=1

        fi
    od
    println @devx,count," items in table",hstsize
end

function lookuplex(ichar name,int length=0)int=
    ref strec e

    int j,wrapped,insource,firstj

    insource:=length
    if length=0 then
        length:=strlen(name)
    fi

    firstj:=j:=(lxhashvalue iand hstmask)       

    wrapped:=0

    do
        lxsymptr:=lexhashtable[j]
        if lxsymptr=nil then                
            exit
        fi

        if lxsymptr.namelen=length and memcmp(lxsymptr.name,name,length)=0 then         
            return 1
        fi

        if ++j>hstsize then     
            if wrapped then
                println "???????HASHTABLE FULL",hstsize,lxlineno
                stop 1
            fi
            wrapped:=1
            j:=1
        fi
    od

    if insource then
        name:=makestring(name,length)
    fi

    if lxsymptr=nil then
        lxsymptr:=pcm_allocz(strec.bytes)
        lexhashtable[j]:=lxsymptr
    fi

    lxsymptr.name:=name
    lxsymptr.namelen:=length
    lxsymptr.symbol:=namesym
    lxsymptr.ksymbol:=0
    lxsymptr.htindex:=j
    lxsymptr.htfirstindex:=firstj
    lxsymptr.moduleno:=currmoduleno
    return 0
end

global proc initsourcefile(ichar source)=
    lxstart:=lxsptr:=cast(source)
    lxlineno:=1
end

global function addnamestr(ichar name)ref strec=
    lxhashvalue:=gethashvalue(name)
    lookuplex(pcm_copyheapstring(name),0)
    return lxsymptr
end

global proc lxerror(ichar m)=           

    fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno

    stop 1
end

global function gethashvalue(ichar s)int=
    int c,hsum

    if s^=0 then return 0 fi

    hsum:=s++^

    do
        c:=s++^
        exit when c=0
        hsum:=hsum<<4-hsum+c
    od
    return hsum<<5-hsum
end

global proc skiptoeol=
    repeat
        lex()
    until lxsymbol=eolsym or lxsymbol=eofsym
END

function makestring(ichar p,int length)ref char=
    ref char s

    s:=pcm_alloc(length+1)
    memcpy(s,p,length)
    (s+length)^:=0
    return s
end
=== aa_parse.m 0 0 8/16 ===
ref strec exprlabeldef
int64 exprvalue
int exprtype

global proc readmodule(int moduleno)=
    ref strec symptr
    int sym

    initsourcefile(moduletable[moduleno].source)

    lxsymbol:=eolsym

    genmc(m_segment,genint(code_seg))

    while lxsymbol=eolsym do

        lex()

        switch lxsymbol
        when kopcodesym then
            readinstr()

        when namesym then
            symptr:=lxsymptr
            lex()
            sym:=lxsymbol
            case sym
            when eqsym then
                lex()
                case lxsymbol
                when kregsym then
                    createregalias(symptr,lxsymptr.subcode, lxsymptr.regsize)
                    lex()
                when kxregsym then
                    createxregalias(symptr,lxsymptr.subcode)
                    lex()
                else
                    createnamedconst(symptr,readexpression())
                esac

            when colonsym,dcolonsym then
                createlabel(symptr,(sym=colonsym|localsym|exportedsym))
                genmc(m_labelx, genlab(symptr))
                symptr.reftype:=fwd_ref
                lxsymbol:=eolsym
                redo
            else
                println symptr.name
                serror("colon expected after label")
            esac

        when fwdlocalsym then
            symptr:=lxsymptr
            lex()
            case lxsymbol
            when eqsym then
                serror_s("Redefining label as const: %s",symptr.name)
            when colonsym,dcolonsym then
                symptr.fwdrefs:=nil
                genmc(m_labelx, genlab(symptr))
                symptr.symbol:=(lxsymbol=colonsym|localsym|exportedsym)
                symptr.reftype:=fwd_ref
                lxsymbol:=eolsym
                redo
            else
                serror("Instruction expected")
            esac

        when importedsym then
            serror_s("Defining imported name: %s",symptr.name)
        when localsym, exportedsym then
            serror("Redefining symbol")
        when namedconstsym then
            serror_s("2:Const redefined: %s",symptr.name)

        when kjmpccsym then
            readcondinstr(m_jmpcc)

        when ksetccsym then
            readcondinstr(m_setcc)

        when kmovccsym then
            readcondinstr(m_cmovcc)

        when eolsym then            
        when eofsym then
            return
        when kimportlibsym then
            lex()
            checksymbol(namesym)
            if nimportlibs>=maximportlibs then serror("Too many import libs") fi
            for i to nimportlibs do
                if eqstring(importlibs[i],lxsymptr.name) then   
                    exit
                fi
            else
                importlibs[++nimportlibs]:=lxsymptr.name
            od
            lex()

        when kimportdllsym then
            lex()
            checksymbol(namesym)
            if nsearchlibs>=maxsearchlibs then serror("Too many DLLs") fi
            for i to nsearchlibs do
                if eqstring(searchlibs[i],lxsymptr.name) then   
                    exit
                fi
            else
                searchlibs[++nsearchlibs]:=lxsymptr.name
            od
            lex()

        else
            println "Unknown symbol (possibly redefining regvar):",symbolnames[lxsymbol]
        end switch
    od
    serror("EOL expected")
end

global proc checkundefined=
    int i
    ref strec d

    d:=modulenamelist
    while d do
        if d.symbol=fwdlocalsym then
            println "Undefined:",padstr(d.name,20)
            ++nundefined
        fi
        d:=d.nextdef
    od
end

proc checksymbol(int symbol)=
    [265]char str

    if lxsymbol<>symbol then
        fprint @&.str,"# expected not #",symbolnames[symbol],symbolnames[lxsymbol]

        serror(&.str)
    fi
end

proc readinstr=
    int opcode
    ref opndrec a,b,c

    opcode:=lxsubcode

    lex()

    switch opcode
    when m_db, m_dw, m_dd, m_dq, m_ddoffset then
        do
            if lxsymbol=stringconstsym then
                a:=genstrimm(lxsvalue)
                lex()
                genmc(opcode,a)
            else
                a:=readoperand()
                genmc(opcode,a)
            fi
            if lxsymbol=commasym then
                lex()
            else
                exit
            fi
        od
    when m_segment then
        checksymbol(ksegnamesym)
        genmc(m_segment,genint(lxsubcode))
        lex()

    when m_isegment then
        genmc(m_segment, genint(idata_seg))
    when m_zsegment then
        genmc(m_segment, genint(zdata_seg))
    when m_csegment then
        genmc(m_segment, genint(code_seg))

    when m_imul3 then
        a:=readoperand()
        checksymbol(commasym)
        lex()
        b:=readoperand()
        checksymbol(commasym)
        lex()
        c:=readoperand()
        SERROR("IMUL3 CAN'T DO 3 OPNDS")

    when m_pcmpistri,m_pcmpistrm then
        a:=readoperand()
        checksymbol(commasym)
        lex()
        b:=readoperand()
        checksymbol(commasym)
        lex()
        c:=readoperand()
        if c.mode<>a_imm then serror("pcmpistr/not int") fi
        genmc(opcode,a,b)
        mccodex.c:=c.value

    when m_proc then
        while lxsymbol<>eolsym do lex() od
    else
        a:=b:=nil
        if lxsymbol<>eolsym then
            a:=readoperand()
            if lxsymbol=commasym then
                lex()
                b:=readoperand()
            fi
        fi 

        genmc(opcode,a,b)
    end

end

proc readcondinstr(int opc)=
    ref opndrec a,b

    a:=genint(lxsubcode)
    lex()
    b:=readoperand()

    if lxsymbol=commasym and opc=m_cmovcc then      
        genmc(m_param,b)                            

        lex()
        b:=readoperand()
    fi

    genmc(opc,a,b)
end

function readoperand:ref opndrec=
    ref opndrec p
    int size

    switch lxsymbol
    when kregsym then
        p:=regtable[lxsubcode, lxsymptr.regsize]
        lex()
        return p
    when lsqsym then
        lex()
        return readaddrmode(0)
    when kxregsym then
        p:=genxreg(lxsubcode)
        lex()
        return p
    when kprefixsym then
        size:=lxsubcode
        lex()
        checksymbol(lsqsym)
        lex()
        return readaddrmode(size)

    else
        return readexpression()
    end
    return nil
end

function readexpression:ref opndrec=
    ref strec labelx
    int64 valuex
    int typex

    readterm()

    docase lxsymbol
    when addsym then
        labelx:=exprlabeldef
        valuex:=exprvalue
        typex:=exprtype
        lex()
        readterm()
        if exprlabeldef then serror("+label?") fi
        exprlabeldef:=labelx
        if typex or exprtype then serror("add real") fi
        exprvalue+:=valuex

    when subsym then
        labelx:=exprlabeldef
        valuex:=exprvalue
        typex:=exprtype
        lex()
        readterm()
        if exprlabeldef then serror("+label?") fi
        exprlabeldef:=labelx
        if typex or exprtype then serror("sub real") fi
        exprvalue:=valuex-exprvalue
    when mulsym then
        labelx:=exprlabeldef
        valuex:=exprvalue
        typex:=exprtype
        lex()
        readterm()
        if exprlabeldef then serror("+label?") fi
        exprlabeldef:=labelx
        if typex or exprtype then serror("add real") fi
        exprvalue*:=valuex

    else
        exit
    end

    return genimm_expr(exprlabeldef,exprvalue,exprtype)
end

proc readterm=
    ref strec symptr
    real x
    exprlabeldef:=nil
    exprvalue:=0
    exprtype:=0

    switch lxsymbol
    when fwdlocalsym, localsym, exportedsym then
        exprlabeldef:=lxsymptr
        lex()
        if lxsymbol=mulsym then     
            serror("* applied to non-extern label or applied inconsistently")
        fi

    when importedsym then
        exprlabeldef:=lxsymptr
        lex()
        if lxsymbol<>mulsym then        
CPL LXSYMPTR.NAME
            serror("* missing or applied inconsistently")
        fi
        lex()

    when namedconstsym then
        exprlabeldef:=lxsymptr.expr.labeldef
        exprvalue:=lxsymptr.expr.value
        exprtype:=lxsymptr.expr.valtype

        lex()
    when namesym then
        symptr:=lxsymptr
        exprlabeldef:=symptr
        lex()
        if lxsymbol=mulsym then     
            createlabel(symptr,importedsym)
            lex()
        else
            createlabel(symptr,fwdlocalsym)
        fi

    when intconstsym then
        exprvalue:=lxvalue
        lex()
    when realconstsym then
        exprvalue:=int64@(lxxvalue)
        exprtype:='R'
        lex()

    when subsym then
        lex()
        readterm()
        if not exprlabeldef then
            if not exprtype then
                exprvalue:=-exprvalue
            else
                x:=-(real@(exprvalue))
                exprvalue:=int64@(x)
            fi
        else
            serror("neg/label")
        fi
    when addsym then
        lex()
        readterm()

    else
        serror("READTERM")
    end
end

proc readreg(int &reg,&regsize,&scale)=

    reg:=lxsubcode
    regsize:=lxsymptr.regsize
    lex()
    if lxsymbol=mulsym then
        lex()
        checksymbol(intconstsym)
        case lxvalue
        when 1,2,4,8 then
        else
            serror("*n must be 1,2,4,8")
        esac
        scale:=lxvalue
        lex()
    else
        scale:=0
    fi
end

function readaddrmode(int size)ref opndrec=
    int reg,regsize,scale,regix, addrsize, regixsize, scaleix
    ref opndrec x
    ref opndrec p

    reg:=regix:=0
    regsize:=regixsize:=0
    scale:=scaleix:=0
    x:=nil

    if lxsymbol=kregsym then
        readreg(reg,regsize,scale)
        case lxsymbol
        when addsym then
            lex()
            if lxsymbol=kregsym then
                readreg(regix,regixsize,scaleix)

                case lxsymbol
                when addsym,subsym then
                    x:=readexpression()
                esac

            else
                x:=readexpression()
            fi
        when subsym then
            x:=readexpression()
        esac
    else
        x:=readexpression()
    fi

    if scale and scaleix then serror("Two *N scales") fi
    checksymbol(rsqsym)
    lex()

    if scale and not scaleix then
        swap(reg,regix)
        swap(regsize,regixsize)
        swap(scale,scaleix)
    fi
    if scaleix=0 then scaleix:=1 fi

    if regsize and regixsize and regsize<>regixsize then serror("Addr reg size mismatch") fi

    p:=genindex(areg:reg, ireg:regix, scale:scaleix, x:x, size:size,
        addrsize:(regsize=4 or regixsize=4|4|8))
    return p
end
=== aa_showss.m 0 0 9/16 ===
import* aa_common

int nsymimports=0, nsymexports=0


global proc writemcx(ichar filename)=
    filehandle f
    int n

    CPL "WRITEMCX",FILENAME
    ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

    roundsegment(ss_code,8,0x90)
    roundsegment(ss_idata,8,0)

    f:=fopen(filename, "wb")
    outword32(f, mcxsig)

    outbyte(f, version_dir)
    outstring(f,"0.1234")

    scansymbols()
    writerelocs(f)

    outbyte(f, zdata_dir)
    outword32(f,ss_zdatalen)

    outbyte(f, code_dir)
    outword32(f, n:=bufferlength(ss_code))
    outblock(f, bufferelemptr(ss_code,0), n)

    outbyte(f, idata_dir)
    outword32(f, n:=bufferlength(ss_idata))

    outblock(f, bufferelemptr(ss_idata,0), n)

    outbyte(f, dlls_dir)
    outword32(f, nsearchlibs)
    for i to nsearchlibs do
        outstring(f, searchlibs[i])
    od

    outbyte(f, libs_dir)
    outword32(f, nimportlibs)
    for i to nimportlibs do
        outstring(f, importlibs[i])
    od

    writesymbols(f)


    outbyte(f,end_dir)

    fclose(f)

end

global function showssdata:ref strbuffer=
    gs_init(dest)
    gs_strln(dest,"AFTER GENSS")

    showsections()

    gs_line(dest)

    showsectionrelocs2("Idata",ss_idatarelocs,ss_nidatarelocs)
    showsectionrelocs2("Code",ss_coderelocs,ss_ncoderelocs)

    gs_str(dest,"proc Section Zdata: ")
    gs_strint(dest,ss_zdatalen)
    gs_line(dest)

    showsectiondata(ss_idata)
    showsectioncode(ss_code)

    showsymboltable2()
    showimporttable()
    gs_strln(dest,"END OF GENSS")

    gs_line(dest)
    return dest
end

proc showsectiondata(ref dbuffer d)=
int i,k,length,bb
    [128]char str,str2
    ref byte p

    gs_str(dest,"proc Section ")
    gs_str(dest,"Idata:")
    gs_str(dest," Size:")
    length:=bufferlength(d)
    gs_strint(dest, length)
    gs_line(dest)
    gs_line(dest)

    k:=0
    p:=bufferelemptr(d,0)

    str[1]:=0

    ref byte baseaddr:=nil

    print @&.str2,baseaddr:"Z8H",,": "

    gs_str(dest,&.str2)

    for i:=1 to length do
        bb:=p++^
        print @&.str2,bb:"z2H",," "
        gs_str(dest,&.str2)

        if 32<=bb<=127 then
            str2[1]:=bb
            str2[2]:=0
            strcat(&.str,&.str2)
        else
            strcat(&.str,".")
        fi
        if ++k=16 or i=length then
            if k<16 then
                to 16-k do
                    gs_str(dest,"   ")
                    strcat(&.str," ")
                od
            fi
            gs_str(dest,"   [")
            gs_str(dest,&.str)
            gs_strln(dest,"]")
            k:=0
            str[1]:=0
            baseaddr+:=16
            print @&.str2,baseaddr:"z8h",,": "
            gs_str(dest,&.str2)
        fi
    od
    if k=0 then
        gs_line(dest)
    fi

    gs_line(dest)
    if k then gs_line(dest) fi
end

proc showsectioncode(ref dbuffer d)=
    ref byte codeptr,codeend,codestart
    int length,offset
    ichar s
    [16]char str

    gs_strln(dest, "proc Section Code")

    length:=bufferlength(d)
    codestart:=codeptr:=bufferelemptr(d,0)
    codeend:=codeptr+length

    ref byte baseaddr:=nil

    while codeptr<codeend do
        offset:=codeptr-codestart
        s:=decodeinstr(codeptr,baseaddr+offset)
        exit when s=nil

        print @&.str,offset:"4",," "
        gs_str(dest,&.str)

        gs_strln(dest,s)
    od

    gs_line(dest)
end

proc gs_value(ichar caption, int64 value)=
    [256]char str

    strcpy(&.str,caption)
    strcat(&.str,":")
    ipadstr(&.str,20)
    gs_str(dest,&.str)

    fprint @&.str,"0x# #",value:"H",value
    gs_strln(dest,&.str)
end

proc showsymboltable2=
    [300]char str
    symbol d

    gs_strln(dest,"Proc Symbol Table")
    int i
    for i:=1 to ss_nsymbols do
        d:=ss_symboltable[i]

        fprint @str,"#: # # #", i, d.name:"20jl", symbolnames[d.symbol]:"15jl",
                    segmentnames[d.segment]

        gs_strln(dest,str)

    od
    gs_line(dest)
end

proc showimporttable=
    [256]char str
    dllrec d
    importrec p


    gs_strln(dest,"Proc Dll List")
    for i:=1 to nsearchlibs do
        gs_strint(dest,i)
        gs_str(dest,": ")
        gs_str(dest,searchlibs[i])
        gs_line(dest)
    od
    gs_line(dest)

    gs_strln(dest,"Proc Lib List")
    for i:=1 to nimportlibs do
        gs_strint(dest,i)
        gs_str(dest,": ")
        gs_str(dest,importlibs[i])
        gs_line(dest)
    od
    gs_line(dest)
    gs_strln(dest,"Proc Import List")

    for i:=1 to nimports do
        p:=importtable[i]

        gs_strint(dest,i)
        gs_str(dest,": ")
        if p.libno then
            strcpy(&.str,p.name)
            ipadstr(&.str,16)
            gs_str(dest,&.str)
            gs_str(dest," (")
            gs_str(dest,dlltable[p.libno].name)
            gs_strln(dest,")")

            gs_value("  IAT Offset        ",p.iatoffset)
            gs_value("  Thunk Offset      ",p.thunkoffset)
            gs_value("  Hint/Name Offset  ",p.hintnameoffset)

        else
            strcpy(&.str,p.name)
            ipadstr(&.str,20)
            gs_str(dest,&.str)
            gs_strln(dest," (---)")
        fi
    od
    gs_line(dest)
end

proc showsections=
    gs_strln(dest,"proc Sections")
    gs_line(dest)

    gs_str(dest,"Section Zdata: Size:")
    gs_strint(dest, ss_zdatalen)

    gs_str(dest,"Section Idata: Size:")
    gs_strint(dest, bufferlength(ss_idata))

    gs_str(dest,"Section Code: Size:")
    gs_strint(dest, bufferlength(ss_code))
end

proc writerelocs(filehandle f)=
    ref relocrec oldr
    mcxreloc newr
    int n,count
    symbol d
    ref u64 baseptr64
    ref u32 baseptr32@baseptr64

    outbyte(f, reloc_dir)
    outword32(f, n:=ss_nidatarelocs+ss_ncoderelocs)

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
                case d.symbol
                when importedsym then
                    newr.stindex:=d.impindex
                    newr.reloctype:=imprel32_rel
                else
                    gerror("rel32/rel not imported")
                esac
            when addr32_rel, addr64_rel then
                case d.symbol
                when importedsym then
                    newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
                    newr.stindex:=d.impindex

                else


                    if oldr.reloctype=addr32_rel then
                        newr.reloctype:=locabs32_rel
                    else
                        newr.reloctype:=locabs64_rel
                    fi
                    newr.targetsegment:=d.segment
                esac
            else
                gerror("reloc?")
            esac

            outblock(f, &newr, newr.bytes)

        od
    od

end

proc scansymbols=
    symbol d
    for i:=1 to ss_nsymbols do
        d:=ss_symboltable[i]
        case d.symbol
        when exportedsym then d.expindex:=++nsymexports
        when importedsym then d.impindex:=++nsymimports
        esac
    od
end

proc writesymbols(filehandle f)=
    symbol d
    u64 epoffset:=-1
    int n

    outbyte(f, importsymbols_dir)
    outword32(f, nsymimports)

    for i to ss_nsymbols when ss_symboltable[i].impindex do
        outstring(f,ss_symboltable[i].name)
    od

    outbyte(f, exportsymbols_dir)
    outword32(f, nsymexports)

    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            if epoffset=-1 and (eqstring(d.name,"start") or eqstring(d.name,"main")) then
                epoffset:=d.offset
            fi
            outstring(f,d.name)
        fi
    od

    outbyte(f, exportsegs_dir)
    outword32(f, nsymexports)
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            outbyte(f,d.segment)
        fi
    od

    outbyte(f, exportoffsets_dir)
    outword32(f, nsymexports)
    for i to ss_nsymbols do
        d:=ss_symboltable[i]
        if d.expindex then
            outword32(f,d.offset)
        fi
    od

    outbyte(f,entry_dir)        
    outword32(f,epoffset)
end

proc roundsegment(ref dbuffer p, int align, value)=
    int length:=bufferlength(p)
    int newlength:=roundtoblock(length, align)

    buffercheck(p, align)

    to newlength-length do
        p.pcurr++^:=value
    od
end

proc showsectionrelocs2(ichar caption,ref relocrec relocs, int nrelocs)=
    ref relocrec r

    gs_str(dest,"proc Section Relocs: ")
    gs_str(dest,caption)
    gs_str(dest," ")
    gs_strint(dest,nrelocs)
    gs_line(dest)

    r:=relocs

    while r do

        gs_str(dest,"Reloc: ")
        gs_str(dest,relocnames[r.reloctype])
        gs_str(dest," Offset: ")
        gs_strint(dest,r.offset)
        gs_str(dest," ST Index: ")
        gs_strint(dest,r.stindex)
        gs_str(dest," ")
        gs_str(dest,ss_symboltable^[r.stindex].name)
        gs_line(dest)

        r:=r.nextreloc
    od
    gs_line(dest)
end

=== aa_writeobj.m 0 0 10/16 ===

int symtaboffset

ref byte datastart
ref byte dataptr

[0..10'000]imagesymbol symboltable

int nsymbols

int stoffset=0              

const maxstring=5000
[maxstring]ichar stringtable
[maxstring]int stringlengths
int nextstringoffset=0
int nstrings=0

export proc writess(ichar outfile)=
    writecoff(outfile)
end

proc writerecord(ref void r, int length)=
    memcpy(dataptr,r,length)
    dataptr+:=length
end

proc writerelocs(ref relocrec r,int nrelocs)=
    static coffrelocrec s
    ref strec d

    return when nrelocs=0

    while r do
        case r^.reloctype
        when addr32_rel, addr64_rel then        
            d:=ss_symboltable^[r^.stindex]

            case d^.segment
            when zdata_seg then s.stindex:=2
            when idata_seg then s.stindex:=4
            when code_seg then s.stindex:=6
            when 0 then                         
                s.stindex:=r^.stindex+stoffset
            else
                gerror("wrelocs/bad seg")
            esac

        else
            s.stindex:=r^.stindex+stoffset
        esac

        s.reloctype:=r^.reloctype
        s.virtualaddr:=r^.offset


        memcpy(dataptr,&s,s.bytes)
        dataptr+:=s.bytes

        r:=r^.nextreloc
    od
end

proc writedata(ref dbuffer data)=
    memcpy(dataptr, bufferelemptr(data,0), bufferlength(data))
    dataptr+:=bufferlength(data)
end

proc writesymboltable=
    int i
    for i:=1 to nsymbols do
        writerecord(&symboltable[i],imagesymbol.bytes)
    od
end

proc writestringtable=
    ref int32 p
    int i,n

    p:=cast(dataptr)
    p^:=nextstringoffset
    dataptr+:=4

    for i to nstrings do
        n:=stringlengths[i]+1
        memcpy(dataptr,stringtable[i],n)
        dataptr+:=n
    od
end

function makesymbol(ichar name,int namelen=0, value=0, sectionno=0,symtype=0,storage=0,naux=0)ref imagesymbol=
    static imagesymbol r
    int length

    if namelen=0 then namelen:=strlen(name) fi

    if namelen<8 then
        strcpy(&r.shortname[1],name)
    elsif namelen=8 then
        memcpy(&r.shortname[1],name,namelen)
    else
        r.shortx:=0
        r.longx:=addstringentry(name,namelen)
    fi
    r.value:=value
    r.sectionno:=sectionno
    r.symtype:=symtype
    r.storageclass:=storage
    r.nauxsymbols:=naux
    return &r
end

proc addsymbol(ref imagesymbol r)=
    if nsymbols>=symboltable.len then
        gerror("as:Too many symbols")
    fi
    memcpy(&symboltable[++nsymbols],r,r^.bytes)
end

proc initsymboltable(ichar filename)=
    nsymbols:=0

    addsymbol(makesymbol(".file",storage:103, sectionno:-2,naux:1))
    addsymbol(strtoaux(filename))

    addsymbol(makesymbol(".bss", storage:3, sectionno:1, naux:1))
    addsymbol(cast(sectiontoaux(nil, 0)))

    addsymbol(makesymbol(".data", storage:3, sectionno:2, naux:1))
    addsymbol(cast(sectiontoaux(ss_idata, ss_nidatarelocs)))

    addsymbol(makesymbol(".text", storage:3, sectionno:3, naux:1))
    addsymbol(cast(sectiontoaux(ss_code, ss_ncoderelocs)))
end

function strtoaux(ref char s)ref imagesymbol=
    static imagesymbol r
    ref byte p:=cast(&r)
    int n

    clear p^

    n:=0
    while s^<>0 and n<r.bytes do
        p++^:=s++^
        ++n
    od

    return &r
end

function sectiontoaux(ref dbuffer data, int nrelocs)ref auxsectionrec=
    static auxsectionrec r

    clear r

    if data=nil then            
        r.length:=ss_zdatalen
    else
        r.length:=bufferlength(data)

    fi
    r.nrelocs:=nrelocs
    return &r
end

function addstringentry(ichar s, int length)int=
    int offset

    offset:=nextstringoffset
    if nstrings>maxstring then
        gerror("W:too many strings")
    fi
    stringtable[++nstrings]:=s
    stringlengths[nstrings]:=length

    nextstringoffset+:=length+1

    return offset
end

proc convertsymboltable=
    ref strec s
    ichar name
    int i,sect, scope

    stoffset:=nsymbols-1

    nstrings:=0
    nextstringoffset:=4

    for i to ss_nsymbols do
        s:=ss_symboltable^[i]

        name:=s^.name

        case s^.segment
        when zdata_seg then sect:=1
        when idata_seg then sect:=2
        when code_seg then sect:=3
        else sect:=0
        esac

        case s^.symbol
        when fwdlocalsym,localsym then
            scope:=3
        when importedsym,exportedsym then
            scope:=2
        else
            scope:=0
        esac

        addsymbol(makesymbol(s^.name,s^.namelen,sectionno:sect, storage:scope, value:s^.offset))

    od
end

proc writecoff(ichar outfile)=
    imagefileheader header
    imagesectionheader zsection, isection, csection
    int offset
    int64 aa


    clear header
    clear zsection
    clear isection
    clear csection

    header.machine:=0x8664
    header.nsections:=3

    strcpy(&zsection.name[1],".bss")
    zsection.rawdata_size:=ss_zdatalen

    zsection.characteristics:=0xC040'0080

    if ss_nidatarelocs>=65536 or ss_ncoderelocs>=65536 then
        gerror("Too many relocs (exceeds 16-bit field)")
    fi

    strcpy(&isection.name[1],".data")
    isection.rawdata_size:=bufferlength(ss_idata)
    isection.nrelocs:=ss_nidatarelocs

    isection.characteristics:=0xC050'0040

    strcpy(&csection.name[1],".text")
    csection.rawdata_size:=bufferlength(ss_code)
    csection.nrelocs:=ss_ncoderelocs

    csection.characteristics:=0x6050'0020

    initsymboltable(outfile)

    convertsymboltable()

    offset:=imagefileheader.bytes

    offset+:=imagesectionheader.bytes*3

    if isection.nrelocs then
        isection.relocations_ptr:=offset
        offset+:=isection.nrelocs*coffrelocrec.bytes
    fi

    if csection.nrelocs then
        csection.relocations_ptr:=offset
        offset+:=csection.nrelocs*coffrelocrec.bytes
    fi

    isection.rawdata_offset:=offset
    offset+:=isection.rawdata_size

    csection.rawdata_offset:=offset
    offset+:=csection.rawdata_size


    header.symtaboffset:=offset
    offset+:=nsymbols*imagesymbol.bytes
    header.nsymbols:=nsymbols

    offset+:=nextstringoffset

    datastart:=dataptr:=malloc(offset)

    writerecord(&header,header.bytes)
    writerecord(&zsection,zsection.bytes)

    writerecord(&isection,isection.bytes)
    writerecord(&csection,csection.bytes)
    writerelocs(ss_idatarelocs,ss_nidatarelocs)
    writerelocs(ss_coderelocs,ss_ncoderelocs)

    writedata(ss_idata)
    writedata(ss_code)

    writesymboltable()
    writestringtable()

    if fverbose then
        println "Writing file:",outfile
    fi
    writefile(outfile,datastart,dataptr-datastart)

end

=== aa_writeexe.m 0 0 11/16 ===

[maxsearchlibs]int64 libinsttable
[maxsearchlibs]ichar libinstnames
[maxsearchlibs]int libnotable           

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
const dll_imagebase = 0x6624'0000
global word imagebase

int imagesize
int filesize
ref[]int64 thunktable               
int fileiatoffset
int fileiatsize
ref strec stentrypoint              
ref strec stentrypoint2
ref strec stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir              

global const maximports = 3000
global [maximports]importrec importtable
global int nimports

global const maxexports = 12000
global [maxexports]exportrec exporttable
global int nexports
ichar dllfilename
int isdll

global const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

global ref byte datastart
global ref byte dataptr
global ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset             

int blockdirvirtaddr
int blockdirvirtsize
int blockdiroffset


export proc writeexe(ichar outfile, int dodll)=
    imagefileheader header
    optionalheader optheader
    int offset,i
    int64 aa

    dllfilename:=outfile
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


    if fverbose then
        CPL "Writing file:",outfile
    fi

    if writefile(outfile,datastart,dataptr-datastart)=0 then
        println "Error writing exe file (possibly still running)"
        stop 1
    fi
end

export proc genexe(ichar entrypoint, outfile, int dodll)=

    dllfilename:=outfile
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

    for i to nsearchlibs do
        strcpy(&.filename,searchlibs[i])
        strcat(&.filename,".dll")
        hinst:=os_getdllinst(&.filename)
        if hinst=0 then
            cpl searchlibs[i]
            cpl &.FILENAME
            gerror("Can't load search lib")
        fi
        libinsttable[i]:=hinst
        libinstnames[i]:=pcm_copyheapstring(&.filename)
    od
end

export proc initsectiontable=

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
            if ndlls>=maxlibs then gerror("Too many libs") fi
            libno:=++ndlls

            dlltable[libno].name:=pcm_copyheapstring(&.str)
            dlltable[libno].nprocs:=1
            return (name2|name2|s+1)
        fi

        ++s
    od

    int n

    for i:=1 to nsearchlibs do
        if os_getdllprocaddr(libinsttable[i],name) then
            n:=i
            exit                
        fi
    else
        println name,moduletable[moduleno].name
        gerror("Can't find external function")
    od

    if libno:=libnotable[n] then            
        ++dlltable[libno].nprocs
        return name
    fi

    strcpy(&.str,searchlibs[n])
    strcat(&.str,".dll")
    if ndlls>=maxlibs then gerror("2:Too many libs") fi
    libno:=++ndlls

    dlltable[libno].name:=pcm_copyheapstring(&.str)
    dlltable[libno].nprocs:=1
    libnotable[n]:=libno

    return name
end

proc scanst=

    int i,libno
    ref strec d
    ichar name, libname

    for i:=1 to ss_nsymbols do

        d:=ss_symboltable^[i]
        case d.symbol
        when importedsym then
            if nimports>=maximports then gerror("genexe: Too many imports") fi
            ++nimports

            name:=extractlibname(d.name,libno,d.moduleno)

            importtable[nimports].libno:=libno          
            importtable[nimports].name:=name                
            importtable[nimports].def:=d

            d.importindex:=nimports
        when exportedsym then
            if userentrypoint then
                if eqstring(d.name,userentrypoint) then
                    stentrypoint:=d
                fi
            else
                if eqstring(d.name,"main") and not isdll then
                    stentrypoint:=d
                elsif eqstring(d.name,"start") and not isdll then
                    stentrypoint2:=d
                elsif eqstring(d.name,"dllmain") and isdll then
                    stentrypoint:=d
                fi
            fi

            if stentrypoint and stentrypoint.segment<>code_seg then
                gerror("Entry point not in code seg")
            fi


            if nexports>=maxexports then gerror("gendll: Too many exports") fi
            ++nexports

            exporttable[nexports].def:=d
            exporttable[nexports].name:=d.name

        esac
    od
end

proc relocdata(ref sectionrec s)=
    ref sectionrec u
    ref relocrec r
    ref byte p
    ref word32 p32
    ref word64 p64
    ref strec d
    word thunkoffset
    int offset,index,iatoffset

    p:=bufferelemptr(s.data,0)
    r:=s.relocs

    while r do
        d:=ss_symboltable^[r.stindex]
        index:=d.importindex                
        thunkoffset:=importtable[index].thunkoffset

        case r.reloctype
        when rel32_rel then
            if d.symbol<>importedsym then
                gerror("rel32/not imported")
            fi
            (ref word32(p+r.offset)^:=int(thunkoffset)-r.offset-4)

        when addr32_rel, addr64_rel then                
            if d.symbol=importedsym then

                (ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
            else
                case d.segment
                when zdata_seg then u:=&sectiontable[zsect]
                when idata_seg then u:=&sectiontable[dsect]
                when code_seg then u:=&sectiontable[csect]
                esac

                p32:=cast(p+r.offset)
                IF R.RELOCTYPE=ADDR32_REL THEN
                    p32^:=p32^+u.virtoffset+imagebase
                ELSE
                    P64:=cast(P32)
                    p64^:=p64^+u.virtoffset+imagebase
                FI
            fi
        else
            cpl relocnames[r.reloctype]
            gerror("Can't do this rel type")
        esac

        r:=r.nextreloc
    od

end

proc getbaserelocs(ref sectionrec s)=
    ref sectionrec u
    ref relocrec r
    ref byte p
    ref strec d
    int index

    p:=bufferelemptr(s.data,0)
    r:=s.relocs

    while r do
        d:=ss_symboltable^[r.stindex]

        case r.reloctype
        when addr32_rel, addr64_rel then                
            if d.symbol=importedsym then
CPL "BASERELOC/SKIP IMPORT",D.NAME
            else
CPL "ADD BASERELOC",D.NAME
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

    memset(&header,0,header.bytes)

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

    memset(&header,0,header.bytes)

    header.magic:=0x20B
    header.majorlv:=1
    header.minorlv:=0
    header.codesize:=sectiontable[csect].rawsize
    header.idatasize:=sectiontable[dsect].rawsize+sectiontable[isect].rawsize
    header.zdatasize:=roundtoblock(sectiontable[zsect].virtsize,filealign)

    if stentrypoint=nil then
        stentrypoint:=stentrypoint2
        if stentrypoint=nil then
            stentrypoint:=stentrypoint3
            if stentrypoint then
                println "Using tertiary 'WinMain' entry point"
            fi
        fi
    fi
    if stentrypoint=nil then
        if userentrypoint then
            cpl userentrypoint
            gerror("User entry point not found")
        else
            if not isdll then
                gerror("Entry point not found: main or start")
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

    memset(&sheader,0,sheader.bytes)

    strcpy(&sheader.name[1],s.name)
    sheader.virtual_size:=s.virtsize
    sheader.virtual_address:=s.virtoffset
    sheader.rawdata_offset:=s.rawoffset
    sheader.rawdata_size:=s.rawsize

    int64 aa
    case s.segtype
    when zdata_seg then
        sheader.characteristics:=0xC050'0080
    when idata_seg then
        sheader.characteristics:=0xC050'0040
    when code_seg then
        sheader.characteristics:=0x6050'0020
    when impdata_seg then
        sheader.characteristics:=0x4030'0040
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
        if length.odd then ++length fi      
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
        thunkptr++^:=0x48
        thunkptr++^:=0xFF
        thunkptr++^:=0x24
        thunkptr++^:=0x25
        thunkaddr:=imagebase+importtable[i].iatoffset
        (ref int32(thunkptr)^:=thunkaddr)

        thunkptr+:=4
    od
end

function getsectionno(int segment)int=
    case segment
    when zdata_seg then zsect
    when idata_seg then dsect
    when code_seg then csect
    else gerror("GSN"); 0
    esac
end

proc writeexporttable(ref byte pstart)=
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
    ref strec d

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
        gerror("Too many exports - can't sort")
    fi

    sortexports(sortindex)

    for i to nexports do
        d:=exporttable[sortindex[i]].def
        sectionno:=getsectionno(d.segment)

        strcpy(pnames,d.name)
        pnametable^:=namesoffset+virtoffset
        ++pnametable
        namesoffset+:=strlen(d.name)+1
        pnames+:=strlen(d.name)+1

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
        if nbaseblocks>=maxbaseblock then gerror("Too many blocks") fi
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
        if blockcounts[i] iand 1 then
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
    ref strec d,e
    for i to nexports do
        sortindex[i]:=i
    od

    int swapped

    repeat
        swapped:=0
        for i:=1 to nexports-1 do

            d:=exporttable[sortindex[i]].def
            e:=exporttable[sortindex[i+1]].def

            if strcmp(d.name, e.name)>0 then
                swapped:=1
                swap(sortindex[i], sortindex[i+1])
            fi
        od
    until not swapped

end
=== aa_writess.m 0 0 12/16 ===
import* aa_common

global function writessdata(int fexe)ref strbuffer=
    gs_init(dest)
    showssdata(fexe)

    gs_line(dest)
    return dest
end

proc showssdata(int fexe)=
gs_strln(dest,(fexe|"EXE FORMAT"|"AFTER GENSS"))

    showsections()

    gs_line(dest)

    showsectionrelocs2("Idata",ss_idatarelocs,ss_nidatarelocs)
    showsectionrelocs2("Code",ss_coderelocs,ss_ncoderelocs)

    gs_str(dest,"proc Section Zdata: ")
    gs_strint(dest,ss_zdatalen)
    gs_line(dest)

    showsectiondata(&sectiontable[dsect])
    showsectioncode(&sectiontable[csect])
    if fexe then
        showsectiondata(&sectiontable[isect])
    fi

    showsymboltable2()
    showimporttable()
    gs_strln(dest,"END OF GENSS")

end

proc showsectiondata(ref sectionrec d)=
int i,k,length,bb
    [128]char str,str2
    ref byte p

    gs_str(dest,"proc Section ")
    gs_str(dest,d.name)
    gs_str(dest," Size:")
    gs_strint(dest,d.virtsize)
    gs_line(dest)
    gs_line(dest)

    k:=0
    if d.segtype<>impdata_seg then
        p:=bufferelemptr(d.data,0)
    else
        p:=d.bytedata
    fi
    length:=d.virtsize

    str[1]:=0

    ref byte baseaddr:=cast(imagebase+d.virtoffset)

    print @&.str2,baseaddr:"Z8H",,": "

    gs_str(dest,&.str2)

    for i:=1 to length do
        bb:=p++^
        print @&.str2,bb:"z2H",," "
        gs_str(dest,&.str2)

        if 32<=bb<=127 then
            str2[1]:=bb
            str2[2]:=0
            strcat(&.str,&.str2)
        else
            strcat(&.str,".")
        fi
        if ++k=16 or i=length then
            if k<16 then
                to 16-k do
                    gs_str(dest,"   ")
                    strcat(&.str," ")
                od
            fi
            gs_str(dest,"   [")
            gs_str(dest,&.str)
            gs_strln(dest,"]")
            k:=0
            str[1]:=0
            baseaddr+:=16
            print @&.str2,baseaddr:"z8h",,": "
            gs_str(dest,&.str2)
        fi
    od
    if k=0 then
        gs_line(dest)
    fi

    gs_line(dest)
    if k then gs_line(dest) fi
end

proc showsectioncode(ref sectionrec p)=
ref byte codeptr,codeend,codestart
    int length,offset
    ichar s
    [16]char str

    gs_strln(dest, "proc Section Code")

    length:=p.virtsize
    codestart:=codeptr:=bufferelemptr(p.data,0)
    codeend:=codeptr+length

    ref byte baseaddr:=cast(imagebase+p.virtoffset)

    while codeptr<codeend do
        offset:=codeptr-codestart
        s:=decodeinstr(codeptr,baseaddr+offset)
        exit when s=nil

        print @&.str,offset:"4",," "
        gs_str(dest,&.str)

        gs_strln(dest,s)
    od

    gs_line(dest)
end

proc showsectionrelocs2(ichar caption,ref relocrec relocs, int nrelocs)=
    ref relocrec r

    gs_str(dest,"proc Section Relocs: ")
    gs_str(dest,caption)
    gs_str(dest," ")
    gs_strint(dest,nrelocs)
    gs_line(dest)

    r:=relocs

    while r do

        gs_str(dest,"Reloc: ")
        gs_str(dest,relocnames[r.reloctype])
        gs_str(dest," Offset: ")
        gs_strint(dest,r.offset)
        gs_str(dest," ST Index: ")
        gs_strint(dest,r.stindex)
        gs_str(dest," ")
        gs_str(dest,ss_symboltable^[r.stindex].name)
        gs_line(dest)

        r:=r.nextreloc
    od
    gs_line(dest)

end

proc gs_value(ichar caption, int64 value)=
    [256]char str

    strcpy(&.str,caption)
    strcat(&.str,":")
    ipadstr(&.str,20)
    gs_str(dest,&.str)

    fprint @&.str,"0x# #",value:"H",value
    gs_strln(dest,&.str)
end

proc showsymboltable2=

    gs_strln(dest,"Proc Symbol Table")
    int i
    for i:=1 to ss_nsymbols do
        gs_strint(dest,i)
        gs_str(dest,": ")
        gs_strln(dest,ss_symboltable^[i].name)
    od
    gs_line(dest)
end

proc showimporttable=
    [256]char str
    dllrec d
    importrec p


    gs_strln(dest,"Proc Dll List")
    int i
    for i:=1 to ndlls do
        gs_strint(dest,i)
        gs_str(dest,": ")
        gs_str(dest,dlltable[i].name)
        gs_str(dest," ")
        gs_strint(dest,dlltable[i].nprocs)
        gs_line(dest)
        gs_value("      Name Table Offset",dlltable[i].nametableoffset)
        gs_value("      Addr Table Offset",dlltable[i].addrtableoffset)
        gs_value("      DLL Name Offset  ",dlltable[i].dllnameoffset)
    od
    gs_line(dest)
    gs_strln(dest,"Proc Import List")

    for i:=1 to nimports do
        p:=importtable[i]

        gs_strint(dest,i)
        gs_str(dest,": ")
        if p.libno then
            strcpy(&.str,p.name)
            ipadstr(&.str,16)
            gs_str(dest,&.str)
            gs_str(dest," (")
            gs_str(dest,dlltable[p.libno].name)
            gs_strln(dest,")")

            gs_value("  IAT Offset        ",p.iatoffset)
            gs_value("  Thunk Offset      ",p.thunkoffset)
            gs_value("  Hint/Name Offset  ",p.hintnameoffset)

        else
            strcpy(&.str,p.name)
            ipadstr(&.str,20)
            gs_str(dest,&.str)
            gs_strln(dest," (---)")
        fi
    od
    gs_line(dest)
end

proc showsections=
    sectionrec s
    int i

    gs_strln(dest,"proc Section Headersxxx")
    gs_line(dest)

    for i:=1 to nsections do
        s:=sectiontable[i]

        gs_str(dest,"Section ")
        gs_strint(dest,i)
        gs_str(dest,": ")
        gs_str(dest,s.name)
        gs_str(dest,"  (")
        gs_str(dest,segmentnames[s.segtype])
        gs_strln(dest,")")

        gs_value("    Raw Offset",s.rawoffset)
        gs_value("    Raw Size",s.rawsize)
        gs_value("    Virtual Offset",s.virtoffset)
        gs_value("    Virtual Size",s.virtsize)
        gs_value("    Nrelocs",s.nrelocs)
        gs_value("    Data",int(s.data))
        gs_line(dest)

    od
end

=== aa_disasm.m 0 0 13/16 ===

const showmregs=1

const halt=0xF4

int nmodules
int xfchsmask_pd

enumdata [0:]ichar opnames =
    (add_op=0,  "add"),
    (or_op,     "or"),
    (adc_op,    "adc"),
    (sbb_op,    "sbb"),
    (and_op,    "and"),
    (sub_op,    "sub"),
    (xor_op,    "xor"),
    (cmp_op,    "cmp")
end

[0:]ichar condnames = 
("o", "no", "b","ae","z","nz","be","a","s","ns","p","np",
 "l","ge","le","g")

enumdata []ichar addrmodenames=     
    (amreg,         $),             
    (ammem,         $),             
    (amrel,         $)              
end

const wmask = 2x1000
const rmask = 2x0100
const xmask = 2x0010
const bmask = 2x0001

const rstack=5                      
const rframe=6

int rex

int addrmode                        
int rmreg                           
int rmopc                           
int basereg                         
int indexreg                        
int scale                           
int opsize                          
int offset
int offsetsize                      
int sizeoverride                    
int addroverride                    
int f2override                      
int f3override                      

[256]char deststr
ichar destptr

ref byte codeptr

global function decodeinstr(ref byte &cptr,baseaddr=nil)ichar=
    int n,w
    int opc,reg,op,xxx,oldopsize,dispsize
    ref byte pstart
    static [256]char str
    [128]char str2
    const maxinstrlen=14
    ichar s

    deststr[1]:=0

    pstart:=codeptr:=cptr

    rex:=0
    opsize:=1
    f2override:=f3override:=sizeoverride:=addroverride:=0
    basereg:=indexreg:=offset:=0

    retry::                     

    switch opc:=codeptr++^
    when 0x00,0x1, 0x08,0x9, 0x10,0x11, 0x18,0x19,
                        0x20,0x21, 0x28,0x29, 0x30,0x31, 0x38,0x39 then 
        op:=opc>>3
        decodeaddr(opc iand 1)
        getsilx(basereg)
        getsil(rmreg)
        genstr(opnames[op])
        printaddrmode()
        genstr(", ")
        genstr(strreg(rmreg,opsize))

    when 0x02,0x3, 0x0A,0xB, 0x12,0x13, 0x1A,0x1B,
                        0x22,0x23, 0x2A,0x2B, 0x32,0x33, 0x3A,0x3B then 
        op:=opc>>3
        decodeaddr(opc iand 1)
        genstr(opnames[op])
        genstr(" ")
        getsil(rmreg)
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0x04,0x5, 0x0C,0xD, 0x14,0x15, 0x1C,0x1D,
                        0x24,0x25, 0x2C,0x2D, 0x34,0x35, 0x3C,0x3D then 
        genstr(opnames[opc>>3])
        genstr(" ")
        if opc iand 1 then
            opsize:=4
            if sizeoverride then opsize:=2 fi
            if rex iand wmask then opsize:=8 fi
        fi
        genstr(strreg(1,opsize))
        genstr(", ")
        genintd(readimm())

    when 0x0F then
        decodetwobyteinstr()

    when 0x40 .. 0x4F then
        rex:=opc

        goto retry

    when 0x50 .. 0x57 then
        reg:=getreg(opc iand 7,rex iand bmask)
        genstr("push ")
        genstr(strreg(reg,8))

    when 0x58 .. 0x5F then
        reg:=getreg(opc iand 7,rex iand bmask)
        genstr("pop ")
        genstr(strreg(reg,8))

    when 0x63 then
        decodeaddr(1)
        genstr("movsxd ")
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        opsize:=4
        printaddrmode()

    when 0x66 then
        sizeoverride:=1
        goto retry

    when 0x67 then
        addroverride:=1
        goto retry

    when 0x68 then
        genstr("push ")
        genintd(readint32())

    when 0x6A then
        genstr("push ")
        genintd(readsbyte())

    when 0x69, 0x6B then
        decodeaddr(1)
        if basereg<>rmreg then
            genstr("imul3")
            genstr(" ")
            genstr(strreg(rmreg,opsize))
            genstr(", ")
        else
            genstr("imul2")
        fi
        printaddrmode()
        genstr(", ")
        opsize:=(opc iand 2|1|opsize)
        genintd(readimm())

    when 0x70..0x7F then
        genstr("j")
        genstr(condnames[opc iand 15])
        genstr(" ")
        genintd(readsbyte())

    when 0x80..0x83 then            
        decodeaddr(opc iand 1)
        genstr(opnames[rmopc])
        getsilx(basereg)
        printaddrmode()
        genstr(", ")
        if opc<>0x83 then
            genintd(readimm())
        else
            genintd(readsbyte())
        fi

    when 0x84, 0x85 then            
        decodeaddr(opc iand 1)
        getsilx(basereg)
        getsil(rmreg)
        genstr("test ")
        printaddrmode()
        genstr(", ")
        genstr(strreg(rmreg,opsize))

    when 0x86,0x87 then             
        decodeaddr(opc iand 1)
        genstr("exch2 ")
        getsilx(basereg)
        getsil(rmreg)
        genstr(strreg(rmreg,opsize))
        genstr(",")
        printaddrmode()

    when 0x88, 0x89 then            
        decodeaddr(opc iand 1)
        genstr("mov")
        getsilx(basereg)
        getsil(rmreg)

        printaddrmode()
        genstr(", ")
        genstr(strreg(rmreg,opsize))

    when 0x8A, 0x8B then            
        decodeaddr(opc iand 1)
        genstr("mov ")
        getsilx(basereg)
        getsil(rmreg)
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0x8D then
        decodeaddr(1)
        genstr("lea ")
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0x8F then
        decodeaddr(1)
        opsize:=1
        genstr("pop")
        printaddrmode()

    when 0x90 then
        if rex then goto doexch fi
        genstr("nop")

    when 0x91..0x97 then            
    doexch::
        reg:=(opc iand 7)+1
        if rex iand bmask then reg+:=8 fi
        opsize:=(sizeoverride|2|4)
        if rex iand wmask then opsize:=8 fi
        genstr("xchg ")
        genstr(strreg(1,opsize))
        genstr(", ")
        genstr(strreg(reg,opsize))

    when 0x98 then
        if sizeoverride then
            genstr("cbw")
        else
            genstr("cbw???")
        fi
    when 0x99 then
        if sizeoverride then
            genstr("cwd")
        elsif rex iand wmask then
            genstr("cqo")
        else
            genstr("cdq")
        fi
    when 0x9B then genstr("wait")

    when 0x9C then genstr("pushf")
    when 0x9D then genstr("popf")
    when 0x9E then genstr("sahf")
    when 0x9F then genstr("lahf")

    when 0xA4..0xA7, 0xAA..0xAF then
        genstr((opc>>1 iand 7|"?","movs","cmps","?","stos","lods","scas"|"?"))
        if opc iand 1=0 then
            genstr("b")
        else
            if rex iand wmask then
                genstr("q")
            elsif sizeoverride then
                genstr("w")
            else
                genstr("d")
            fi
        fi

    when 0xA8, 0xA9 then                
        genstr("test ")
        if opc iand 1 then
            opsize:=(sizeoverride |2|4)
            if rex iand wmask then opsize:=8 fi
        fi
        genstr(strreg(1,opsize))
        genstr(", ")
        genintd(readimm())

    when 0xB0..0xBF then            
        reg:=(opc iand 7)+1
        if rex iand bmask then reg+:=8 fi
        if (opc iand 2x1000) then
            opsize:=(sizeoverride |2|4)
            if rex iand wmask then opsize:=8 fi
        fi
        genstr("mov ")
        getsil(reg)

        genstr(strreg(reg,opsize))
        genstr(", ")
        genintd(readimm8())

    when 0xC0, 0xC1, 0xD0..0xD3 then
        decodeaddr(opc iand 1)
        getsilx(basereg)
        genstr((rmopc+1|"rol","ror","rcl","rcr","shl","shr","?","sar"|"?"))
        printaddrmode()
        if opc<=0xC1 then
            genstr(", ")
            genintd(readbyte())
        else
            genstr((opc iand 2|", cl"|", 1"))
        fi

    when 0xC2 then
        genstr("retn ")
        genintd(readword16())

    when 0xC3 then
        genstr("ret")

    when 0xC6,0xC7 then
        decodeaddr(opc iand 1)
        genstr("mov")
        printaddrmode()
        genstr(", ")
        genintd(readimm())

    when 0xD7 then genstr("xlat")

    when 0xD8..0xDF then
        decode8087(opc iand 7)

    when 0xE0 then genstr("loopnz "); genintd(readsbyte())
    when 0xE1 then genstr("loopz "); genintd(readsbyte())
    when 0xE2 then genstr("loop "); genintd(readsbyte())

    when 0xE3 then
        if addroverride then
            genstr("jecxz ")
        else
            genstr("jrcxz ")
        fi
        genintd(readsbyte())

    when 0xE8 then
        genstr("call ")
        genintd(readint32())

    when 0xE9 then
        genstr("[4] jmp ")
        genintd(readint32())

    when 0xEB then
        genstr("jmp ")
        genintd(readsbyte())

    when 0xF2 then
        if codeptr^<>0x0F and (codeptr^<0x40 and codeptr^>0x4F) then
            genstr("repne")
        else
            f2override:=1
            goto retry
        fi
    when 0xF3 then
        if codeptr^<>0x0F and (codeptr^<0x40 and codeptr^>0x4F) then
            genstr("repe")
        else
            f3override:=1
            goto retry
        fi

    when 0xF4 then

    when 0xF6,0xF7 then
        decodeaddr(opc iand 1)
        getsilx(basereg)
        genstr((rmopc+1|"test","?","not","neg","mul","imul","div","idiv"|"?"))
        printaddrmode()
        if rmopc=0 then
            if opsize=8 then opsize:=4 fi
            genstr(", ")
            genintd(readimm())
        fi

    when 0xFE then
        w:=0
        goto doff

    when 0xFF then          
        w:=1
    doff::
        decodeaddr(w)
        case rmopc
        when 2x_000 then    
            getsilx(basereg)
            genstr("inc")
        when 2x_001 then    
            getsilx(basereg)
            genstr("dec")
        when 2x_010 then    
            opsize:=8
            genstr("icall")
        when 2x_100 then    
            opsize:=8
            genstr("jmp")
        when 2x_110 then    
            opsize:=8
            genstr("push")
        else
            println "FFxx?"
        esac
        printaddrmode()

    else
        genstr("Unknown opcode: ")
    genhex(opc)
    endswitch


        print @&.str,baseaddr:"z6h",,": "

    n:=codeptr-pstart
    to n do
        print @&.str2,int(pstart++^):"z2H",," "

        strcat(&.str,&.str2)
    od
    to maxinstrlen-n do
        strcat(&.str,"-- ")
    od
    strcat(&.str,&.deststr)

    cptr:=codeptr

    return &.str
end

proc decodetwobyteinstr=
    int opc,rhssize,third,imm
    ichar opcstr

    switch opc:=codeptr++^
    when 0x2A then                  
        decodeaddr(1)
        if f3override then
            genstr("cvtsi2ss ")
        else
            genstr("cvtsi2sd ")
        fi
        genstr(strxmm(rmreg))
        genstr(", ")
        printaddrmode(0)
        
    when 0x2C then                  
        decodeaddr(1)
        if f3override then
            genstr("cvttss2si ")
            rhssize:=4
        else
            genstr("cvttsd2si ")
            rhssize:=8
        fi
        if rex iand wmask then
            genstr(strreg(rmreg,8))
        else
            genstr(strreg(rmreg,4))
        fi
        genstr(", ")
        opsize:=rhssize
        printaddrmode(1)

    when 0x2D then                  
        decodeaddr(1)
        if f3override then
            genstr("cvtss2si ")
            rhssize:=4
        else
            genstr("cvtsd2si ")
            rhssize:=8
        fi
        if rex iand wmask then
            genstr(strreg(rmreg,8))
        else
            genstr(strreg(rmreg,4))
        fi
        genstr(", ")
        opsize:=rhssize
        printaddrmode(1)

    when 0x2F then                  
        decodeaddr(1)
        if sizeoverride then
            opsize:=8
            genstr("comisd ")
        else
            opsize:=4
            genstr("comiss ")
        fi
        genstr(strxmm(rmreg))
        genstr(", ")
        printaddrmode(1)

    when 0x3A then                  
        third:=codeptr++^

        case third
        when 0x63 then
            genstr("pcmpistri ")
        when 0x62 then
            genstr("pcmpistrm ")
        else
            genstr("Unknown opcode 2-byte opcode: 0F ")
            genhex(opc)
            return
        esac

        decodeaddr(1)
        genstr(strxmm(rmreg))
        genstr(", ")
        printaddrmode(1)
        genstr(", ")
        imm:=codeptr++^
        genintd(imm)

    when 0x40..0x4F then
        decodeaddr(1)
        genstr("cmov")
        genstr(condnames[opc iand 15])
        genstr(" ")
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0x51 then                  
        decodeaddr(1)
        opsize:=(f3override|4|8)
        genstr((opsize=4|"sqrtss "|"sqrtsd "))
        genstr(strxmm(rmreg))
        genstr(", ")
        printaddrmode(1)

    when 0x54 then                  
        decodeaddr(1)
        genstr((sizeoverride|"andpd "|"andps "))
        genstr(strxmm(rmreg))
        genstr(", ")
        opsize:=(sizeoverride|8|4)
        printaddrmode(1)

    when 0x57 then                  
        decodeaddr(1)
        genstr((sizeoverride|"xorpd "|"xorps "))
        genstr(strxmm(rmreg))
        genstr(", ")
        opsize:=(sizeoverride|8|4)
        printaddrmode(1)

    when 0x58 then                  
        opcstr:="adds"
    doarith::
        genstr(opcstr)
        decodeaddr(1)
        if f2override then
            opsize:=8
            genstr("d ")
        else
            opsize:=4
            genstr("s ")
        fi
        genstr(strxmm(rmreg))
        genstr(", ")
        printaddrmode(1)

    when 0x59 then                  
        opcstr:="muls"
        goto doarith

    when 0x5A then                  
        decodeaddr(1)
        if f3override then
            genstr("cvtss2sd ")
            rhssize:=4
        else
            genstr("cvtsd2ss ")
            rhssize:=8
        fi
        genstr(strxmm(rmreg))
        genstr(", ")
        opsize:=rhssize
        printaddrmode(1)

    when 0x5C then                  
        opcstr:="subs"
        goto doarith

    when 0x5D then
        opcstr:="mins"
        goto doarith

    when 0x5E then                  
        opcstr:="divs"
        goto doarith

    when 0x5F then
        opcstr:="maxs"
        goto doarith


    when 0x6E then                  
        decodeaddr(1)
        opsize:=(rex iand wmask|8|4)
        genstr((opsize=4|"movd "|"movq "))
        if sizeoverride then        
            genstr(strxmm(rmreg))
        else
            genstr(strmmx(rmreg))
        fi
        genstr(", ")
        printaddrmode()

    when 0x6F then                  
        decodeaddr(1)
        opsize:=16
        if sizeoverride then        
            genstr("movdqa ")
        elsif f3override then       
            genstr("movdqu ")
        else
            genstr("No 66/F3 ")
        fi
        genstr(strxmm(rmreg))
        genstr(", ")
        printaddrmode(1)

    when 0x7E then                  
        decodeaddr(1)
        if f3override then
            opsize:=8
            genstr("movq ")
            genstr(strxmm(rmreg))
            genstr(", ")
            printaddrmode(1)
        elsif rex iand wmask then
            opsize:=8
            genstr("movq ")
            printaddrmode()
            genstr(", ")
            genstr(strxmm(rmreg))
        else
            opsize:=4
            genstr("movd ")
            printaddrmode()
            genstr(", ")
            if sizeoverride then        
                genstr(strxmm(rmreg))
            else
                genstr(strmmx(rmreg))
            fi
        fi

    when 0x7F then                  
        decodeaddr(1)
        opsize:=16
        if sizeoverride then        
            genstr("movdqa ")
        elsif f3override then       
            genstr("movdqu ")
        else
            genstr("No 66/F3 ")
        fi
        printaddrmode(1)
        genstr(", ")
        genstr(strxmm(rmreg))

    when 0x80..0x8F then            
        genstr("[long] j")
        genstr(condnames[opc iand 15])
        genstr(" ")
        if sizeoverride then
            genintd(readint16())
        else
            genintd(readint32())
        fi

    when 0x90..0x9F then
        decodeaddr(0)
        genstr("set")
        genstr(condnames[opc iand 15])
        genstr(" ")
        getsilx(basereg)
        printaddrmode()

    when 0xAF then
        decodeaddr(1)
        genstr("imul ")
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0xB6, 0xB7, 0xBE, 0xBF then
        decodeaddr(1)
        genstr((opc<0xBE|"movzx "|"movsx "))
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        opsize:=(opc iand 1|2|1)
        printaddrmode()

    when 0xB8 then
        decodeaddr(1)
        genstr("popcnt ")
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0xBC, 0xBD then
        decodeaddr(1)
        genstr((opc=0xBC|"bsf "|"bsr "))
        genstr(strreg(rmreg,opsize))
        genstr(", ")
        printaddrmode()

    when 0xD6 then
        decodeaddr(1)
        opsize:=8
        genstr("movq ")
        printaddrmode(1)
        genstr(",")
        genstr(strxmm(rmreg))   

    when 0xDB then                  
        decodeaddr(1)
        genstr("pand ")
        genstr(strxmm(rmreg))
        genstr(", ")
        opsize:=8   
        printaddrmode(1)

    when 0xEF then                  
        decodeaddr(1)
        genstr("pxor ")
        genstr(strxmm(rmreg))
        genstr(", ")
        opsize:=8   
        printaddrmode(1)


    else
    error::
        genstr("Unknown opcode 2-byte opcode: 0F ")
    genhex(opc)
    endswitch
end

proc decodeaddr(int w=0)=
    int modrm,xxx,mode,sib,rm

    basereg:=indexreg:=0
    scale:=1
    offset:=0
    if w then
        opsize:=(sizeoverride|2|4)
        if rex iand wmask then opsize:=8 fi
    else
        opsize:=1
    fi

    modrm:=codeptr++^

    mode:=modrm>>6
    xxx:=(modrm>>3) iand 7
    rm:=modrm iand 7

    if mode=3 then      
        basereg:=rm+1
        addrmode:=amreg
    elsif rm<>4 then                
        if mode=0 and rm=5 then     
            offset:=readint32()     
            addrmode:=ammem

        else
            basereg:=rm+1
            addrmode:=ammem
            case mode
            when 1 then
                offset:=readsbyte()
            when 2 then
                offset:=readint32()
            esac
        fi
    else            
        addrmode:=ammem
        sib:=readbyte()
        indexreg:=((sib>>3) iand 7)+1
        basereg:=(sib iand 7)+1
        scale:=(sib>>6+1|1,2,4,8|0)

        if mode=0 and basereg=rframe and indexreg=rstack then   
            indexreg:=basereg:=0
            offset:=readint32()

        elsif mode=0 and basereg=rframe  then   
            basereg:=0
            offset:=readint32()

        elsif mode=0 and indexreg=rstack then   
            indexreg:=0

        else
            case mode
            when 1 then
                offset:=readsbyte()
            when 2 then
                offset:=readint32()
            esac
            if indexreg=rstack then             
                indexreg:=0
            fi
        fi

    fi

    if basereg and rex iand bmask then basereg+:=8 fi
    if indexreg and rex iand xmask then indexreg+:=8 fi

    rmreg:=xxx+1
    if rex iand rmask then rmreg+:=8 fi
    rmopc:=xxx
end

function readbyte:int=
    return codeptr++^
end

function readsbyte:int=
    return (ref int8(codeptr++))^
end

function readword16:word=
    word a
    a:=ref word16(codeptr)^
    codeptr+:=2
    return a
end

function readint16:int=
    int a
    a:=ref int16(codeptr)^
    codeptr+:=2
    return a
end

function readword32:word=
    word a
    a:=ref word32(codeptr)^
    codeptr+:=4
    return a
END

function readint32:int=
    int a
    a:=ref int32(codeptr)^
    codeptr+:=4
    return a
END

function readint64:int64=
    int64 a
    a:=ref int64(codeptr)^
    codeptr+:=8
    return a
END

function getreg(int regcode,upper)int=
    if upper then
        return regcode+8+1
    fi
    return regcode+1
end

global function strreg(int reg,opsize)ichar=
static []ichar regnames8=("al","cl","dl","bl","spl","bpl","sil","dil",
                        "r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b",
                "ah","bh","ch","dh")

static []ichar regnames16=("ax","cx","dx","bx","sp","bp","si","di",
                        "r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w")

static []ichar regnames32=("eax","ecx","edx","ebx","esp","ebp","esi","edi",
                        "r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d")

static []ichar regnames64=("rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
                        "r8","r9","r10","r11","r12","r13","r14","r15")

static []ichar mregnames8=("B0","B10","B11","B4","B15","B14","B5","B3",
                        "B12","B13","B1","B2","B6","B7","B8","B9",
                    "B16","B18","B19","B17")

static []ichar mregnames16=("W0","W10","W11","W4","Wsp","Wbp","W5","W3",
                        "W12","W13","W1","W2","W6","W7","W8","W9")

static []ichar mregnames32=("A0","A10","A11","A4","Astack","Aframe","A5","A3",
                        "A12","A13","A1","A2","A6","A7","A8","A9")

static []ichar mregnames64=("D0","D10","D11","D4","Dstack","Dframe","D5","D3",
                        "D12","D13","D1","D2","D6","D7","D8","D9")

    if reg=0 then return "<>" fi

    if showmregs then
        case opsize
        when 1 then return mregnames8[reg]
        when 2 then return mregnames16[reg]
        when 4 then return mregnames32[reg]
        when 8 then return mregnames64[reg]
        esac
    else
        case opsize
        when 1 then return regnames8[reg]
        when 2 then return regnames16[reg]
        when 4 then return regnames32[reg]
        when 8 then return regnames64[reg]
        esac
    fi
    return ""
end

function strfreg(int freg)ichar=
    static []ichar fregnames=("st0","st1","st2","st3","st4","st5","st6","st7")
    return fregnames[freg]
end

proc printaddrmode(int xmm=0)=
    static [100]char str
    ichar plus
    int addrsize

    genstr(" ")

    case addrmode
    when amreg then
        if xmm then
            genstr(strxmm(basereg))
        else
            getsilx(basereg)
            genstr(strreg(basereg,opsize))
        fi
        return
    esac

    case opsize
    when 1 then genstr("byte ")
    when 2 then genstr("word ")
    when 4 then genstr("dword ")
    when 8 then genstr("qword ")
    when 10 then genstr("tword ")
    when 16 then genstr("oword ")
    else
    CPL "///OPSIZE",opsize
    esac

    genstr("[")
    plus:=""
    addrsize:=(addroverride|4|8)

    if basereg then
        genstr(strreg(basereg,addrsize))
        plus:="+"
    fi
    if indexreg then
        genstr(plus)
        genstr(strreg(indexreg,addrsize))
        if scale>1 then
            genstr("*")
            genintd(scale)
        fi
        plus:="+"
    fi

    if offset or (basereg=0 and indexreg=0) then
        if basereg=0 and indexreg=0 then
            genhex(offset)
        else
            if offset>0 then genstr(plus) fi
            genintd(offset)
        fi
    fi
    genstr("]")
    if addrmode=amrel then genstr("+RIP") fi
end

proc genstr(ichar s)=
    strcat(&.deststr,s)
end

proc genintd(int64 a)=
    genstr(strint(a))
end

proc genhex(int64 a)=
    genstr(strint(a,"h"))
end

function readimm:int=

    case opsize
    when 1 then return readsbyte()
    when 2 then return readint16()
    when 4,8 then return readint32()            
    esac
    return 0
end

function readimm8:int64=
    if opsize<8 then return readimm() fi
    return readint64()
end

function strxmm(int reg)ichar=
    static [32]char str
    print @&.str,"xmm",,reg-1
    return &.str
end

function strmmx(int reg)ichar=
    static [32]char str

    print @&.str,"mmx",,reg-1
    return &.str
end

proc decode8087(int ttt)=
    byte bb
    int longopc,freg,shortopc,code

    bb:=codeptr++^          

    longopc:=ttt<<8+bb      
    freg:=(bb iand 7)+1     


    case longopc
    when 2x'110'1101'1001 then genstr("fcompp")
    when 2x'001'1110'0100 then genstr("ftst")
    when 2x'001'1110'0101 then genstr("fxam")
    when 2x'001'1110'1110 then genstr("fldz")
    when 2x'001'1110'1000 then genstr("fld1")
    when 2x'001'1110'1011 then genstr("fldpi")
    when 2x'001'1110'1001 then genstr("fldl2t")
    when 2x'001'1110'1010 then genstr("fldl2e")
    when 2x'001'1110'1100 then genstr("fldlg2")
    when 2x'001'1110'1101 then genstr("fldln2")

    when 2x'001'1111'1010 then genstr("fsqrt")
    when 2x'001'1111'1110 then genstr("fsin")
    when 2x'001'1111'1111 then genstr("fcos")
    when 2x'001'1111'1011 then genstr("fsincos")
    when 2x'001'1111'1101 then genstr("fscale")
    when 2x'001'1111'1000 then genstr("fprem")
    when 2x'001'1111'1100 then genstr("frndint")
    when 2x'001'1111'0100 then genstr("fxtract")
    when 2x'001'1110'0001 then genstr("fabs")
    when 2x'001'1110'0000 then genstr("fchs")

    when 2x'001'1111'0010 then genstr("fptan")
    when 2x'001'1111'0011 then genstr("fpatan")
    when 2x'001'1111'0000 then genstr("f2xm1")
    when 2x'001'1111'0001 then genstr("fyl2x")
    when 2x'001'1111'1001 then genstr("fyl2xp1")

    when 2x'011'1110'0011 then genstr("finit")
    when 2x'011'1110'0000 then genstr("feni")
    when 2x'011'1110'0001 then genstr("fdisi")

    when 2x'011'1110'0010 then genstr("fclex")

    when 2x'001'1111'0111 then genstr("fincstp")
    when 2x'001'1111'0110 then genstr("fdecstp")
    when 2x'001'1101'0000 then genstr("fnop")

    elsecase longopc iand 2x'111'11111'000          !ignore bottom 3 bits

    when 2x'001'11000'000 then genstr("fld "); genstr(strfreg(freg))
    when 2x'101'11010'000 then genstr("fst "); genstr(strfreg(freg))
    when 2x'101'11011'000 then genstr("fstp "); genstr(strfreg(freg))
    when 2x'001'11001'000 then genstr("fxch "); genstr(strfreg(freg))
    when 2x'000'11010'000 then genstr("fcom "); genstr(strfreg(freg))
    when 2x'000'11011'000 then genstr("fcomp "); genstr(strfreg(freg))
    when 2x'101'11000'000 then genstr("ffree "); genstr(strfreg(freg))

    elsecase longopc iand 2x'001'11111'000          !ignore bottom 3 bits and top 2

    when 2x'000'11000'000 then do87arith("fadd",ttt,freg)

    when 2x'000'11100'000 then do87arith("fsub",ttt,freg)
    when 2x'000'11101'000 then do87arith("fsubr",ttt,freg)

    when 2x'000'11001'000 then do87arith("fmul",ttt,freg)

    when 2x'000'11110'000 then do87arith("fdiv",ttt,freg)
    when 2x'000'11111'000 then do87arith("fdivr",ttt,freg)

    else    
        --codeptr                   
        decodeaddr(0)           
        shortopc:=ttt<<3 + rmopc

        case shortopc               
        when 2x'111'101 then do87mem("fld",4)
        when 2x'011'101 then do87mem("fld",5)
        when 2x'111'100 then do87mem("fldbcd")

        when 2x'111'111 then do87mem("fstp",4)
        when 2x'011'111 then do87mem("fstp",5)
        when 2x'111'110 then do87mem("fstpbcd")

        when 2x'001'101 then do87mem("fldcw")
        when 2x'001'111 then do87mem("fstcw")
        when 2x'101'111 then do87mem("fstsw")

        when 2x'001'110 then do87mem("fstenv")
        when 2x'001'100 then do87mem("fldenv")
        when 2x'101'110 then do87mem("fsave")
        when 2x'101'100 then do87mem("frstor")

        elsecase shortopc iand 2x001'111        !ignore top two bits (mf code)

        when 2x'001'000 then do87mem("fld",ttt>>1)
        when 2x'001'010 then do87mem("fst",ttt>>1)
        when 2x'001'011 then do87mem("fstp",ttt>>1)
        when 2x'000'010 then do87mem("fcom",ttt>>1)
        when 2x'000'011 then do87mem("fcomp",ttt>>1)
        when 2x'000'000 then do87mem("fadd",ttt>>1)
        when 2x'000'100 then do87mem("fsub",ttt>>1)
        when 2x'000'101 then do87mem("fsubr",ttt>>1)
        when 2x'000'001 then do87mem("fmul",ttt>>1)
        when 2x'000'110 then do87mem("fdiv",ttt>>1)
        when 2x'000'111 then do87mem("fdivr",ttt>>1)

        else
            genstr("UNKNOWN x87 OPCODE")
        esac
    esac

end

proc do87arith(ichar opcstr, int ttt,freg)=
    int d, p

    d:=ttt iand 2x100       
    p:=ttt iand 2x010       

    genstr(opcstr)
    if p then
        genstr("p")
    fi
    genstr(" ")

    if d=0 then
        genstr("st0, ")
    genstr(strfreg(freg))
    else
    genstr(strfreg(freg))
        genstr(", st0")
    fi
end

proc do87mem(ichar opcstr,int mf=-1)=
    genstr("f")

    case mf
    when 2x'00 then opsize:=4
    when 2x'01 then genstr("i"); opsize:=4
    when 2x'10 then opsize:=8
    when 2x'11 then genstr("i"); opsize:=2
    when 4 then genstr("i"); opsize:=8
    when 5 then opsize:=10
    esac
    genstr(opcstr+1)

    genstr(" ")
    printaddrmode()
end

proc getsil(int &reg)=
    if opsize=1 and not rex and reg>=5 and reg<=8 then
        reg+:=12                
    fi
end

proc getsilx(int &reg)=
    if addrmode=amreg and opsize=1 and rex=0 and reg>=5 and reg<=8 then
        reg+:=12                
    fi
end
=== aa_genss.m 0 0 14/16 ===
const wmask = 2x1000                
const rmask = 2x0100                
const xmask = 2x0010                
const bmask = 2x0001                

int rex
int sizeoverride                    
int addroverride                    
int f2override                      
int f3override                      

ref opndrec extraparam

int currseg=0
ref dbuffer currdata                
ref relocrec currrelocs
int nrelocs

ref mclrec currmcl

enumdata [0:]ichar opnames =
    (add_op=0,  "add"),
    (or_op,     "or"),
    (adc_op,    "adc"),
    (sbb_op,    "sbb"),
    (and_op,    "and"),
    (sub_op,    "sub"),
    (xor_op,    "xor"),
    (cmp_op,    "cmp")
end


export proc genss=
    int index
    ref mclrec m

    ss_zdatalen:=0
    ss_zdata:=buffercreate()
    ss_idata:=buffercreate()
    ss_code:=buffercreate()
    ss_idatarelocs:=nil
    ss_coderelocs:=nil
    ss_nsymbols:=0

    switchseg(code_seg)

    alineno:=9999
    extraparam:=nil

    m:=mccode
    index:=0

    while m do
        alineno:=m.lineno

        doinstr(m,++index)
        m:=m.nextmcl
    od

    switchseg(0)                    

    if bufferlength(ss_zdata) then
        gerror("Zdata contains code or data")
    fi

end

proc doinstr(ref mclrec m,int index)=
    ref opndrec a,b
    ref strec d,e
    int x,offset,shortjmp,n

    CURRMCL:=M
    buffercheck(currdata)

    rex:=sizeoverride:=addroverride:=f2override:=f3override:=0


    a:=m.a
    b:=m.b

    switch m.opcode
    when m_labelx then
        d:=a.labeldef

        d.reftype:=back_ref
        d.segment:=currseg
        d.offset:=getcurrdatalen(6)

        if d.symbol=exportedsym then
            getstindex(d)
        fi

        dofwdrefs(d)

    when m_call then
        do_call(a)

    when m_jmp then
        do_jmp(a,m)

    when m_jmpcc then
        offset:=getrel32(b.labeldef,getcurrdatalen(7)+1)

        if offset<0 then            
            if offset<-126 then
                genbyte(0x0F)
                genbyte(0x80+a.value)
                gendword(offset-4)
            else
                genbyte(0x70+m.a.value)
                genbyte(offset)
            fi
        else
            shortjmp:=checkshortjump(m,b.labeldef)
            if not shortjmp then
                genbyte(0x0F)
                genbyte(0x80+a.value)
                genrel32(b)
            else
                genbyte(0x70+a.value)
                genrel8(b)
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

    when m_leave then
        genbyte(0xC9)

    when m_retn then
        if a.mode<>a_imm then gerror("retn?") fi
        genbyte(0xC2)
        genword(a.value)

    when m_push then
        do_push(a)

    when m_pop then
        do_pop(a)

    when m_inc, m_dec then
        do_inc(a,mclcodes[m.opcode])

    when m_neg, m_not, m_mul, m_imul, m_div, m_idiv then
        do_neg(a,mclcodes[m.opcode])

    when m_add, m_sub, m_and, m_or, m_xor, m_adc, m_sbb, m_cmp then
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
            buffercheck(currdata,n)
            case currseg
            when code_seg then
                to n do genbyte(0x90) od
            when idata_seg then
                to n do genbyte(0) od
            else
                ss_zdatalen+:=n
            esac
    
        else
            gerror("resb?")
        fi

    when m_align then
        if a.mode=a_imm then
            x:=a.value
            if x<1 or x>16384 then gerror("align2") fi
            if currseg<>zdata_seg then
                while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) od
            else
                while ss_zdatalen rem x do  ++ss_zdatalen od
            fi
        else
            gerror("align?")
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
        do_setcc(a,b)

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

    when m_andpd,m_xorpd, m_pand, m_pxor then
        do_logicxmm(a,b,mclcodes[m.opcode],8)

    when m_pcmpistri,m_pcmpistrm then
        do_pcmpistri(a,b,m.c,mclcodes[m.opcode])

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

    when m_param then
        extraparam:=a

    when m_cmovcc then
        do_cmovcc(a,extraparam,b)

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

    else
        println "*** CAN'T DO OPCODE",mclnames[m.opcode],"line",alineno
    endswitch

end

proc genbyte(int x)=
    currdata.pcurr++^:=x
end

proc genword(int x)=
    addword(currdata,x)
end

proc gendword(int x)=
    adddword(currdata,x)
end

proc genqword(int64 x)=
    addqword(currdata,x)
end

proc genopnd(ref opndrec a,int size=0)=
    ref char s
    int64 x
    int length

    if size=0 then size:=a.size fi

    switch a.mode
    when a_imm,a_mem then
    when a_string then
        s:=a.svalue
        length:=strlen(s)
        if length>100 then
            buffercheck(currdata,max(1024,length+1))
        fi
        while s^ do
            genbyte(s++^)
        od
        return
    else
        gerror("GENOPND/bad opnd")
    endswitch

    if a.labeldef and size<=2 then
        gerror("8/16-BIT RELOC")
    fi

    case size
    when 1 then
        genbyte(a.value)
    when 2 then
        genword(a.value)
    when 4 then
        if a.labeldef then
            genabs32(a)
        else
            if a.valtype then       
                gendword(getr32bits(a.xvalue))
            else
                gendword(a.value)
            fi
        fi
    when 8 then
        if a.labeldef then
            genabs64(a)
        else
            x:=a.value
            if a.valtype then
                genqword(x)

            else
                genqword(x)
            fi
        fi
    esac
end

proc addrelocitem(int reloctype, ref strec d)=
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

function getstindex(ref strec d)int=
    if d.stindex=0 then
        if ss_nsymbols>=ss_symboltablesize then
            extendsymboltable()
        fi
        d.stindex:=++ss_nsymbols
        ss_symboltable^[d.stindex]:=d
    fi
    return d.stindex
end

proc genrel32(ref opndrec a)=
    ref strec d

    d:=a.labeldef

    if d=nil then               
        gendword(a.value)
        return
    fi

    case d.reftype
    when back_ref then
        if d.segment<>currseg then
            gerror("Rel label across segments")         
        fi
        gendword(d.offset-(getcurrdatalen(2)+4))
    when fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel32_rel)
        gendword(0)
    else                                
        gendword(a.value)               
        addrelocitem(rel32_rel,d)
    esac
end

proc genabs32(ref opndrec a)=
    ref strec d

    d:=a.labeldef

    case d.reftype
    when back_ref then
        gendword(d.offset+a.value)
        addrelocitem(addr32_rel,d)

    when fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
        gendword(a.value)
        addrelocitem(addr32_rel,d)

    else                                
        gendword(a.value)
        addrelocitem(addr32_rel,d)
    esac
end

proc genabs64(ref opndrec a)=
    ref strec d

    d:=a.labeldef

    case d.reftype
    when back_ref then
        genqword(d.offset+a.value)
        addrelocitem(addr64_rel,d)

    when fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr32_rel,currseg)
        genqword(a.value)
        addrelocitem(addr64_rel,d)

    else                                
        genqword(a.value)
        addrelocitem(addr64_rel,d)
    esac
end

function getrel32(ref strec d,int offset)int=

    if d.reftype=back_ref then                  
        if d.segment<>currseg then
            gerror("Rel label across segments2")
        fi
        return d.offset-(offset+1)
    else
        return int32.maxvalue
    fi
end

proc dofwdrefs(ref strec d)=
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
            when zdata_seg then gerror("Fwd ref in zdata")
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
    CPL RELOCNAMES[F.RELTYPE]
            GERROR("DOFWDREFS/CAN'T DO RELTYPE")
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

proc do_push(ref opndrec a)=
    int code,am

    case a.mode
    when a_reg then
        if a.size<>8 then gerror("pushreg not 64-bit") fi
        code:=regcodes[a.reg]
        if code>=8 then
            rex :=bmask
            code iand:=7
        fi
        genrex()
        genbyte(0x50+code)

    when a_imm then
        if a.labeldef then
            genbyte(0x68)
            genopnd(a,4)
        elsif isbytesized(a.value) then
            genbyte(0x6A)
            genbyte(a.value)
        elsif isdwordsized(a.value) then
            genbyte(0x68)
            gendword(a.value)
        else
            gerror("push imm value too large")
        fi

    when a_mem then
        if a.size<>8 then gerror("push not 64-bit") fi
        am:=genrm(a,6)
        genrex()
        genbyte(0xFF)
        genamode(a,am)
    else
        gerror("push opnd?")
    esac
end

proc do_pop(ref opndrec a)=
    int code, am

    case a.mode
    when a_reg then
        if a.size<>8 then gerror("popreg not 64-bit") fi
        code:=regcodes[a.reg]
        if code>=8 then
            rex :=bmask
            code iand:=7
        fi
        genrex()
        genbyte(0x58+code)

    when a_mem then
        if a.size<>8 then gerror("pop not 64-bit") fi
        am:=genrm(a,0)
        genrex()
        genbyte(0x8F)
        genamode(a,am)
    else
        gerror("pop opnd?")
    esac
end

proc do_inc(ref opndrec a,int code)=
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
        gerror("inc/opnd?")
    esac
end

proc do_neg(ref opndrec a,int code)=
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
        gerror("neg/div/etc opnd?")
    esac
end

proc genamode(ref opndrec a,int am)=
    int sib,mode,dispsize

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
        genbyte(a.value)
    when 4 then
        if a.labeldef then
            genabs32(a)
        else
            gendword(a.value)
        fi
    else
        gerror("genamode size 2/8")
    esac
end

function makemodrm(int mode,opc,rm)int=
    return mode<<6+opc<<3+rm
end

proc setopsize(ref opndrec a)=
    case a.size
    when 1 then         
    when 2 then         
        sizeoverride:=1
    when 8 then         
        rex ior:=wmask
    when 4 then         
    else
        gerror("Operand size not set")
    esac
end

proc setaddrsize(ref opndrec a)=
    if a.mode=a_mem and a.addrsize=4 then
        addroverride:=1
    fi
end

function getdispsize(ref opndrec a,int mand=1)int=

    if a.labeldef then return 4 fi
    if a.value or mand then
        return (isbytesized(a.value)|1|4)
    else
        return 0
    fi
end

function genrm(ref opndrec a,int opc)int=
    static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
    int mode, rm, scale, dispsize, needsib, sib, index, base
    int reg, regix, code

    mode:=rm:=0             
    scale:=0                
    dispsize:=0
    needsib:=0
    sib:=-1

    if a.mode=a_mem and a.addrsize=4 then
        addroverride:=1
    fi

    case a.mode
    when a_reg then         
        code:=getregcodeb(a.reg)
        return makeam(makemodrm(3,opc,code), sib, dispsize)

    when a_mem then

    when a_xreg then
        code:=getregcodebx(a.reg)
        return makeam(makemodrm(3,opc,code), sib, dispsize)     

    else
        gerror("genrm not mem")
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
        dispsize:=getdispsize(a,0)
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
        if regix=rstack then gerror("Scaled rstack?") fi

    else                                    
        dispsize:=getdispsize(a,0)
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
        fi

        if regix and not reg then
            dispsize:=4
        fi

        if regix=rstack and scale>1 then gerror("Can't scale rstack") fi

    fi

    if index>=8 then rex ior:= xmask; index iand:=7 fi
    if base>=8  then rex ior:= bmask; base  iand:=7 fi

    if scale then
        sib:=scaletable[scale]<<6 + index<<3 + base
    fi
    rm iand:=7

    return makeam(makemodrm(mode:mode,opc:opc,rm:rm), sib, dispsize)
end

proc genrmbyte(int mode,opc,rm)=
    genbyte(mode<<6+opc<<3+rm)
end

function makeam(int m,s,d)int=
    return s<<16+m<<8+d
end

proc do_arith(ref opndrec a,b,int code)=
    int am, regcode, opc, dispsize
    int64 x

    case a.mode
    when a_reg then
        case b.mode
        when a_reg,a_mem then
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
            if b.labeldef then
                if code<0 or code>7 then gerror("non-add arith/label") fi
                if a.size<4 then gerror("add imm/size") fi
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
                unless -0x8000'0000 <= x <= 0xFFFF'FFFF then gerror("3:exceeding word32 value") end
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
            gerror("ADD reg,???")
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
            gerror("ADD mem,???")
        esac

    else
    cpl opnames[code]
        gerror("Can't add to this opnd")
    esac
end

proc do_mov(ref opndrec a,b)=
    int regcode, am
    int64 value

    case a.mode
    when a_reg then
        case b.mode
        when a_reg, a_mem then
            if a.size<>b.size and b.size then
                gerror("Opnd size mismatch")
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
            if b.labeldef and a.size<=2 then gerror("mov imm?") fi
            case a.size
            when 1 then
                checkhighreg(a)
                case a.reg
                when r5,r3,r14,r15 then
                    rex ior:=0x40
                esac
                unless -128<=value<=255 then gerror("exceeding byte value") end
                genrex()
                genbyte(0xB0+regcode)
                genbyte(value)

            when 2 then
                unless -32768<=value<=65535 then gerror("exceeding word16 value") end
                genbyte(0x66)
                genrex()
                genbyte(0xB8+regcode)
                genword(value)
            when 4 then
                if b.labeldef then
                    genrex()
                    genbyte(0xB8+regcode)
                    genopnd(b,4)
                else
                    unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
                        CPL value,ref void(value)
                        gerror("1:exceeding word32 value")
                    end
doreg32::
                    genrex()
                    genbyte(0xB8+regcode)
                    gendword(value)
                fi

            else                            
                if b.labeldef then
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
            gerror("MOV REG/??")
        esac
    when a_mem then
        case b.mode
        when a_reg then
            if a.size<>b.size and a.size then
                gerror("Opnd size mismatch")
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
            if b.labeldef and a.size<=2 then gerror("mov imm?") fi
    
            if a.size=0 then a.size:=1 fi
    
            case a.size
            when 0,1 then
                unless -128<=value<=255 then gerror("exceeding byte value") end
    
                setopsize(a)
                genrex()
                genbyte(0xC6)
                genamode(a,am)
                genbyte(value)
    
            when 2 then
                unless -32768<=value<=65535 then gerror("exceeding word16 value") end
                setopsize(a)
                genrex()
                genbyte(0xC7)
                genamode(a,am)
                genword(value)
            when 4,8 then
                if not b.labeldef then
                    unless -0x8000'0000<=value<=0xFFFF'FFFF then gerror("2:exceeding word32 value") end
                fi
                setopsize(a)
                genrex()
                genbyte(0xC7)
                genamode(a,am)
                genopnd(b,4)
            esac
    
        else
            gerror("MOV MEM/?")
        esac
    else
        gerror("MOV ?/..")
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

    regcode:=reg-1
    if regcode>=8 then
        regcode-:=8
        rex ior:=bmask
    fi
    return regcode
end

function getregcoderx(int reg)int=
    int regcode

    regcode:=reg-1
    if regcode>=8 then
        regcode-:=8
        rex ior:=rmask
    fi
    return regcode
end


proc do_lea(ref opndrec a,b)=
    int regcode, am

    unless a.mode=a_reg and b.mode=a_mem then
        gerror("LEA not reg/mem")
    end

    if a.size<4 then gerror("LEA size error") fi
    regcode:=getregcoder(a.reg)

    am:=genrm(b,regcode)
    setopsize(a)
    genrex()
    genbyte(0x8D)
    genamode(b,am)

end

proc do_movsx(ref opndrec a,b,int opc)=
    int am, regcode

    if a.mode<>a_reg then gerror("movsx not reg") fi

    if a.size=8 and b.size=4 then
        if opc=0xBE then
            do_movsxd(a,b)
        else                        
            a:=regtable[a.reg,4]
            do_mov(a,b)
        fi
        return
    fi

    if a.size=1 or a.size<=b.size then gerror("movsx size error") fi

    if opc=0xB6 and b.size=4 then gerror("movsx 4=>8 bytes?") fi

    case b.mode
    when a_reg then
    when a_mem then
        if b.size=0 then gerror("movsx need size prefix") fi
        if b.size=8 then gerror("movsx size 8") fi
    else
        gerror("movsx not reg/mem")
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

proc checkhighreg(ref opndrec a)=
    if a.mode=a_reg then
        case a.reg
        when r5,r3,r14,r15 then
            rex ior:=0x40
        esac
    fi
end

proc do_exch(ref opndrec a,b)=
    int regcode, am

    if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then      
        if a.reg<>r0 then               
            swap(a,b)
        fi
        if a.size<>b.size then gerror("exch size") fi

        setopsize(a)
        regcode:=getregcodeb(b.reg)
        genrex()
        genbyte(0x90+regcode)
        return
    fi

    if a.mode=a_mem then swap(a,b) fi

    unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then gerror("exch opnds") end
    if b.size=0 and b.mode=a_mem then b.size:=a.size fi
    if a.size<>b.size then gerror("exch size") fi

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

proc do_movsxd(ref opndrec a,b)=
    int regcode, am

    if b.mode=a_mem and b.size=0 then b.size:=4 fi

    if a.size<>8 or b.size>4 then gerror("movsxd size") fi

    if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
        gerror("movsxd opnds")
    fi

    regcode:=getregcoder(a.reg)
    am:=genrm(b,regcode)

    setopsize(a)
    genrex()
    genbyte(0x63)
    genamode(b,am)

end

proc do_imul2(ref opndrec a,b)=
    int regcode, am, opc
    int64 value

    if a.mode<>a_reg then
        gerror("imul2 opnds")
    fi
    if b.size=0 then b.size:=a.size fi
    if a.size=1 then gerror("imul2 byte") fi

    case b.mode
    when a_reg,a_mem then
        if a.size<>b.size then gerror("imul2 size") fi
        regcode:=getregcoder(a.reg)
        am:=genrm(b,regcode)

        setopsize(a)
        genrex()
        genbyte(0x0F)
        genbyte(0xAF)
        genamode(b,am)

    when a_imm then                     
        if b.labeldef then gerror("mul/label") fi
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
        gerror("imul2 opnds")
    esac
end

proc do_imul3(ref opndrec a,b,c)=
    int64 value
    int regcode1, regcode2, opc

    if a.mode<>a_reg or b.mode<>a_reg then
        gerror("imul3 opnds")
    fi
    if a.size=1 then gerror("imul3 byte") fi
    if c.mode<>a_imm then gerror("imul3 not imm") fi

    value:=c.value
    regcode1:=getregcoder(a.reg)
    regcode2:=getregcodeb(b.reg)
    opc:=0xC0+regcode1<<3+regcode2
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
end

proc do_shift(ref opndrec a,b,int opc)=
    int am, w

    if a.mode<>a_reg and a.mode<>a_mem then gerror("shift opnds1?") fi

    am:=genrm(a,opc)
    checkhighreg(a)
    setopsize(a)
    genrex()
    w:=(a.size=1|0|1)

    case b.mode
    when a_imm then
        if b.labeldef then gerror("shift/label") fi
        if b.value=1 then
            genbyte(0xD0+w)
            genamode(a,am)
        else
            genbyte(0xC0+w)
            genamode(a,am)
            genbyte(b.value)
        fi
    when a_reg then
        if b.reg<>r10 or b.size<>1 then gerror("cl or b10 needed") fi
        genbyte(0xD2+w)
        genamode(a,am)

    else
        gerror("shift opnds2?")
    esac
end

proc do_test(ref opndrec a,b)=
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
        gerror("test opnds")
    fi

end

proc do_loop(ref opndrec a,int opc)=
    int offset

    offset:=getrel32(a.labeldef,getcurrdatalen(9)+1)
    if offset<0 then            
        if offset<-126 then
            gerror("loop jmp out of range")
        fi
        genbyte(opc)
        genbyte(offset)
    else
        gerror("Can't do loopxx fwd jump")
    fi
end

proc do_jcxz(ref opndrec a,int opsize)=
    int offset

    offset:=getrel32(a.labeldef,getcurrdatalen(10)+1)
    if offset<0 then            
        if offset<-126 then
            gerror("jcxz jmp out of range")
        fi
        if opsize=4 then genbyte(0x67) fi
        genbyte(0xE3)
        genbyte(offset)
    else
        gerror("Can't do jcxz fwd jump")
    fi
end

proc do_setcc(ref opndrec a,b)=
    int am

    if (b.mode<>a_reg and b.reg<>a_mem) or b.size>1 then gerror("setcc opnd/size") fi

    am:=genrm(b,0)
    checkhighreg(b)
    genrex()
    genbyte(0x0F)
    genbyte(0x90+a.value)
    genamode(b,am)
end

proc do_movxmm(ref opndrec a,b,int size)=
    int am, regcode, regcode1, regcode2

    case a.mode
    when a_reg then
        case b.mode
        when a_xreg then
            if a.size<>size then gerror("1:movdq size") fi
            regcode:=getregcoderx(b.reg)
            am:=genrm(a,regcode)
            setopsize(a)
            genbyte(0x66)
            genrex()
            genbyte(0x0F)
            genbyte(0x7E)
            genamode(b,am)

        else
            gerror("movdq reg,?")
        esac
    when a_xreg then
        case b.mode
        when a_reg then
            if b.size<>size then gerror("3:movdq size") fi
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
            if b.size and b.size<>size then gerror("4:movdq size") fi
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
            gerror("movdq xreg,?")
        esac
    when a_mem then
        case b.mode
        when a_xreg then
            if a.size and a.size<>size then gerror("5:movdq size") fi
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
            gerror("movdq mem,?")
        esac
    else
        gerror("movdq opnds")
    esac

end

proc do_arithxmm(ref opndrec a,b,int prefix,opc)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        gerror("arithxmm opnds")
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

proc do_logicxmm(ref opndrec a,b,int opc,size)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        gerror("logicxmm opnds")
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

proc do_convertfloat(ref opndrec a,b,int prefix)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        gerror("convertfloat opnds")
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

proc do_fix(ref opndrec a,b,int prefix,opc)=
    int am, regcode

    if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
        gerror("fix opnds")
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

proc do_float(ref opndrec a,b,int prefix)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
        gerror("float opnds")
    fi

    if b.mode=a_mem then
        if b.size=0 then b.size:=4 fi
        if b.size<>4 and b.size<>8 then gerror("float size") fi
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

proc do_call(ref opndrec a)=
    int am, regcode
    case a.mode
    when a_imm then
        genbyte(0xE8)
        genrel32(a)
    else                
        case a.size
        when 0 then a.size:=8
        when 1,2,4 then
            gerror("call[]size")
        esac
        am:=genrm(a,2)
        setopsize(a)
        setaddrsize(a)
        genrex()
        genbyte(0xFF)
        genamode(a,am)

    esac
end

proc do_jmp(ref opndrec a,ref mclrec m)=
    int am, regcode, offset, shortjmp

    case a.mode
    when a_imm then
        offset:=getrel32(a.labeldef,getcurrdatalen(11)+1)
        if offset<0 and offset>-126 then
            genbyte(0xEB)
            genbyte(offset)
        else
            shortjmp:=0
            if offset>0 then                
                shortjmp:=checkshortjump(m,a.labeldef)
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
        case a.size
        when 0 then a.size:=8
        when 1,2,4 then
            gerror("jmp[]size")
        esac
        am:=genrm(a,4)
        setopsize(a)
        setaddrsize(a)
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

proc do_cmovcc(ref opndrec c,a,b)=
    int am, regcode
    if a.size<>b.size and b.size then
        gerror("Opnd size mismatch")
    fi
    if a.size=1 then gerror("cmov/byte") fi
    regcode:=getregcoder(a.reg)
    am:=genrm(b,regcode)

    setopsize(a)
    genrex()
    genbyte(0x0F)
    genbyte(0x40+c.value)
    genamode(b,am)
end

proc do_fmem(ref opndrec a, int freal, code)=
    int am, regcode, mf

    if a.mode<>a_mem then
        gerror("fmem/not mem")
    fi

    if freal then
        case a.size
        when 4 then mf:=0
        when 8 then mf:=2
        when 10,16 then
            mf:=1
            case code
            when 0 then code:=5
            when 3 then code:=7
            else
                gerror("r80 not allowed")
            esac
        else
            CPL "SIZE=",A.SIZE
            gerror("fmem size")
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
                gerror("fst i64?")
            esac
        else
            gerror("fmem int size")
        esac
    fi

    am:=genrm(a,code)
    genrex()
    genbyte(0xD9+mf<<1)
    genamode(a,am)
end

function getr32bits(real x)int=
    real32 sx:=x
    return int32@(sx)
end

proc genrel8(ref opndrec a)=
    ref strec d

    d:=a.labeldef

    if d.reftype=fwd_ref then
        d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
        genbyte(0)
    else                                
        gerror("genrel8")
    fi
end

function checkshortjump(ref mclrec m,ref strec d)int=
    int n

    n:=0
    m:=m.nextmcl
    while m and n<=8 do
        ++n
        if m.opcode=m_labelx and m.a.labeldef=d then
            return 1
        fi

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

proc do_movdqx(ref opndrec a,b, int opc)=
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
            gerror("movdqx?")
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
            gerror("movdqx")
        esac
    else
        gerror("movdqx")
    esac

end

proc do_popcnt(ref opndrec a,b)=
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

proc do_bsf(ref opndrec a,b, int opc)=
    int am, regcode

    if b.mode=a_mem then
        if b.size=0 then b.size:=8 fi
    fi
    if a.size<>b.size then gerror("bsf size") fi

    regcode:=getregcodebx(a.reg)
    am:=genrm(b,regcode)
    setopsize(a)
    genrex()
    genbyte(0x0F)
    genbyte(opc)
    genamode(b,am)
end

proc extendsymboltable=
    ref[]ref strec oldsymboltable
    int oldsymboltablesize

    oldsymboltablesize:=ss_symboltablesize
    oldsymboltable:=ss_symboltable

    ss_symboltablesize*:=2
    CPL "EXTENDING SYMBOL TABLE TO",SS_SYMBOLTABLESIZE

    ss_symboltable:=pcm_alloc(ref void.bytes*ss_symboltablesize)

    for i:=1 to ss_nsymbols do
        ss_symboltable^[i]:=oldsymboltable^[i]
    od

    pcm_free(oldsymboltable,ref void.bytes*oldsymboltablesize)
end

proc do_pcmpistri(ref opndrec a,b,int c,opc)=
    int am, regcode

    if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
        gerror("pcmpistrx opnds")
    fi

    genbyte(0x66)

    if b.mode=a_xreg then
        swap(a,b)
        regcode:=getregcoderx(b.reg)
        am:=genrm(a,regcode)
        genrex()
        genbyte(0x0F)
        genbyte(0x3A)
        genbyte(opc)
        genamode(a,am)
    else
        regcode:=getregcoderx(a.reg)
        am:=genrm(b,regcode)
        genrex()
        genbyte(0x0F)
        genbyte(0x3A)
        genbyte(opc)
        genamode(b,am)
    fi

    genbyte(c)

end

=== aa_lib.m 0 0 15/16 ===
const ptrsize=8

fwdrec dummy1

global enumdata [0:]ichar opndnames =
    (a_none=0,  $),
    (a_reg,     $),
    (a_imm,     $),
    (a_mem,     $),     
    (a_cond,    $),     
    (a_xreg,    $),     
    (a_string,  $),     
end


global record mclrec =
    ref mclrec nextmcl
    ref opndrec a,b
    u16 opcode
    u16 c
    u32 lineno
end


global int currsegment=0        

global opndrec dstackopnd
global opndrec dframeopnd

global int labelno=0
global ref opndrec zero_opnd=nil

global ref mclrec mccode, mccodex

strbuffer destv
global ref strbuffer dest=&destv

global [r0..r19, 1..8]ref opndrec regtable

global proc initlib=
    zero_opnd:=genint(0)

    int reg,size

    for reg:=r0 to r15 do
        for size:=1 to 8 do
            case size
            when 1,2,4,8 then
                regtable[reg,size]:=genreg0(reg,size)
            esac
        od
    od
    for reg:=r16 to r19 do
        regtable[reg,1]:=genreg0(reg,1)
    od

    ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
    ss_symboltablesize:=init_ss_symbols
    ss_nsymbols:=0

end

global proc genmc(int opcode,ref opndrec a=nil,b=nil)=  
ref mclrec m
int nopnds

    m:=pcm_alloc(mclrec.bytes)
++NMCLASM

    m.nextmcl:=nil

    if lxsymbol=eolsym then
        m.lineno:=lxlineno-1
    else
        m.lineno:=lxlineno
    fi

    m.opcode:=opcode

    nopnds:=(a=nil|0|(b=nil|1|2))
    if nopnds=2 and opcode in [m_pcmpistri,m_pcmpistrm] then nopnds:=3 fi

    if nopnds<mclnopnds[opcode] then
        serror("Too few operands")
    elsif nopnds>mclnopnds[opcode] then
        serror("Too many operands")
    fi

    m.a:=a
    m.b:=b

    if mccode then
        mccodex.nextmcl:=m
        mccodex:=m
    else
        mccode:=mccodex:=m
    fi
end

global proc genmcstr(int opcode,ichar s)=   

    genmc(opcode,genstrimm(s))
end

function newopnd(int mode)ref opndrec=
ref opndrec a

    ++NMCLOPNDSASM

    a:=pcm_allocz(opndrec.bytes)
    a.mode:=mode
    return a
end

global function genxreg(int xreg)ref opndrec=       
    ref opndrec a

    a:=newopnd(a_xreg)
    a.reg:=xreg
    a.size:=16
    return a
end

global function genindex(int areg=0,ireg=0,scale=1,ref opndrec x=nil,int size=0,addrsize=8)ref opndrec=     
    ref opndrec a

    if x then                           
        a:=x
        x.mode:=a_mem
    else
        a:=newopnd(a_mem)
    fi

    a.reg:=areg
    a.regix:=ireg
    a.scale:=scale
    a.size:=size
    a.addrsize:=addrsize
    return a
end

global function writemclblock:ref strbuffer=        
    int i
    ref mclrec m

    gs_init(dest)

    gs_strln(dest,"MC CODE")

    m:=mccode
    i:=1

    while m do
        writemcl(i,m)
        m:=m.nextmcl
        ++i
    od
    return dest         
end

global proc gencomment(ichar s=nil)=            
    if s=nil then
        genmc(m_blank)
    else
        genmcstr(m_comment,s)
    fi
end

global function genstrimm(ichar s)ref opndrec=          
    ref opndrec a
    a:=newopnd(a_string)
    a.svalue:=s
    return a
end

function getsizetag(int size)ichar=         
    case size
    when 1 then return "b"
    when 2 then return "h"
    when 4 then return "w"
    when 8 then return "d"
    esac
    GERROR("GETSIZETAG?")
    return nil
end

proc writemcl(int index,ref mclrec mcl)=            
    [512]char mclstr
    [512]char str
    ichar semi

    strcpy(&.mclstr,strmcl(mcl))
    if mclstr[1]=0 then return fi

    case mcl.opcode
    when m_comment then
        semi:=";"
    else
        semi:=" "
    esac

    print @&.str,semi:"z3",index:"z4",," "

    gs_str(dest,&.str)
    gs_strln(dest,&.mclstr)
end

global function strmcl(ref mclrec mcl)ichar=            
    static [512]char str
    [128]char str2
    int opcode,sizepref

    opcode:=mcl.opcode

    case opcode
    when m_assem then
        return mcl.a.svalue
    when m_blank then
        return ""
    when m_comment then
            strcpy(&.str,";")
            strcat(&.str,mcl.a.svalue)
            return &.str


    when m_labelx then
        strcpy(&.str,mcl.a.labeldef.name)
        strcat(&.str,":")
        return &.str

    esac

    strcpy(&.str,"      ")

    case opcode
    when m_jmpcc then
        strcat(&.str,"j")
        strcat(&.str,condnames[mcl.a.value])

    when m_setcc then
        strcat(&.str,"set")
        strcat(&.str,condnames[mcl.a.value])
    when m_cmovcc then
        strcat(&.str,"cmov")
    strcat(&.str,condnames[mcl.a.value])
    else
        strcat(&.str,mclnames[opcode]+2)
    esac

    ipadstr(&.str,12)


    if mcl.a and mcl.b then     
        sizepref:=needsizeprefix(mcl.opcode,mcl.a,mcl.b)

        strcat(&.str,stropnd(mcl.a,sizepref))
        strcat(&.str,", ")
        strcat(&.str,stropnd(mcl.b,sizepref))

    elsif mcl.a then                                
        if mcl.opcode=m_call then
            strcat(&.str,stropnd(mcl.a,0))
        else
            strcat(&.str,stropnd(mcl.a,1))
        fi
    fi

    case opcode
    when m_pcmpistri,m_pcmpistrm then
        fprint @&.str2,", #",mcl.c
        strcat(&.str,&.str2)
    esac


    return &.str
end

global function stropnd(ref opndrec a,int sizeprefix=0)ichar=           
    static [256]char str
    ichar plus,s
    int64 value
    ref strec d

    case a.mode
    when a_reg then
        return getregname(a.reg,a.size)
    when a_imm then
        d:=a.labeldef
        value:=a.value
        if d then
            if d.symbol=namedconstsym then
                return inttostr(d.expr.value)
            fi

            s:=GETFULLNAME(d)

            if value then
                if value>0 then
                    strcpy(&.str,s)
                    strcat(&.str,"+")
                    strcat(&.str,inttostr(value))
                else
                    strcpy(&.str,s)
                    strcat(&.str,inttostr(value))
                fi
                return &.str
            else
                strcpy(&.str,s)
                return &.str
            fi
        fi
        if a.valtype=0 then
            return inttostr(value)
        else
            return realtostr(real@(value))
        fi

    when a_mem then
        str[1]:=0
        strcat(&.str,getsizeprefix(a.size,sizeprefix))
        strcat(&.str,"[")
        plus:=""

        if a.reg then
            strcat(&.str,getregname(a.reg,a.addrsize))
            plus:="+"
        fi

        if a.regix then
            strcat(&.str,plus)
            strcat(&.str,getregname(a.regix,a.addrsize))
            plus:="+"
            if a.scale>1 then
                strcat(&.str,"*")
                strcat(&.str,inttostr(a.scale))
            fi
        fi

        if a.labeldef then
            strcat(&.str,plus)
            strcat(&.str,strdef(a.labeldef))
            plus:="+"
        fi

        if a.value>0 then
            strcat(&.str,plus)
            strcat(&.str,inttostr(a.value))
        elsif a.value<0 then
            strcat(&.str,inttostr(a.value))
        fi

        strcat(&.str,"]")
    when a_string then
        if strlen(a.svalue)>=str.len then
            print @&.str,"""<Long string>"""
        else
            print @&.str,"""",,a.svalue,,""""
        fi

    when a_cond then
        return opndnames[a.value]

    when a_xreg then
        return xgetregname(a.reg)

    else
        return "<BAD OPND>"
    esac

    return &.str
end

function strdef(ref strec def)ichar=            
    if def.symbol=namedconstsym then
        return inttostr(def.expr.value)
    fi
    return getfullname(def)
end

global proc setsegment(int seg)=        
    if seg=currsegment then
        return
    fi
    case seg
    when 'D' then genmcstr(m_segment,".data")
    when 'Z' then genmcstr(m_segment,".bss")
    when 'C' then genmcstr(m_segment,".text")
    when 'R' then genmcstr(m_segment,".rodata")
    esac
    currsegment:=seg
    end

    function getsizeprefix(int size,enable=0)ichar=     
    if not enable then return "" fi
    case size
    when 1 then return "byte "
    when 2 then return "word "
    when 4 then return "dword "
    when 8 then return "qword "
    when 0 then return ""
    esac
    return "N:"
end

function needsizeprefix(int opcode,ref opndrec a,b)int=     

    case opcode
    when m_movsx,m_movzx then
        return 1
    when m_cvtsi2ss,m_cvtsi2sd then
        return 1
    esac

    if a.mode=a_reg or a.mode=a_xreg or b.mode=a_reg or b.mode=a_xreg then
        return 0
    fi
    return 1
end

global function genimm_expr(ref strec d, int64 value, int t, size=4)ref opndrec=
    ref opndrec a

    a:=newopnd(a_imm)
    a.size:=size

    a.labeldef:=d
    a.value:=value
    a.valtype:=t

    return a
end

global function genint(int64 x,int size=4)ref opndrec=
    ref opndrec a


    a:=newopnd(a_imm)
    a.size:=size
    a.value:=x

    return a
end

global function genlab(ref strec d,int size=4)ref opndrec=
    ref opndrec a

    a:=newopnd(a_imm)
    a.size:=size
    a.labeldef:=d

    return a
end

global function genmem(ref strec d,int size=4)ref opndrec=
    ref opndrec a

    a:=genlab(d,size)
    a.mode:=a_mem
    return a
end

global function genreg0(int reg,size=4)ref opndrec= 

    ref opndrec a
    a:=newopnd(a_reg)
    a.reg:=reg
    a.size:=size
    return a
end

global function getfullname(ref strec d)ichar=
    static [256]char str
    ichar ms

    ms:=""
    if d.basedef then
        ms:=d.basedef.name
    fi


    fprint @&.str,"<# : ## &:# SYM:## M:#>",
        d.name,"#",d.moduleno,d:"8",
        strlen(symbolnames[d.symbol])-3:"v",symbolnames[d.symbol]:".*", ms

    return &.str
    return d.name
end

global function getregname(int reg,size=4)ichar=
    ichar prefix,rs
    static [32]char str

    case reg
    when rnone then return "-"
    when rframe then rs:="frame"
    when rstack then rs:="stack"

    else
        rs:=inttostr(reg-r0)
    esac

    case size
    when 1 then prefix:="B"
    when 2 then prefix:="W"
    when 4 then prefix:="A"
    else prefix:="D"
    esac

    strcpy(&.str,prefix)
    strcat(&.str,rs)
    return &.str
end

global function xgetregname(int reg)ichar=
    static [16]char str

    print @&.str,"xmm",,reg-r0

    return &.str
end

global proc printst(filehandle f)=
    ref strec r
    int count,i

    r:=modulenamelist
    while r do
        printstrec(f,r)
        r:=r.nextdef
    od

end

global proc printstrec(filehandle f,ref strec d)=
    const w=16

    case d.symbol
    when fwdlocalsym, localsym, exportedsym then
        println @f,"Label:       ",padstr(d.name,w),(d.scope=fwd_ref|"U"|"-"),
            symbolnames[d.symbol],,"\T",,
        padstr((d.segment|segmentnames[d.segment]|"no seg"),12),
            d.offset, d.fwdrefs
    when importedsym then
        println @f,"Label:       ",padstr(d.name,w),"EXTERN"

    when namedconstsym then
        println @f,"Named const: ",padstr(d.name,w),"=",stropnd(d.expr)
    else
        println @f,"??"
    esac
end

global proc adddef(ref strec d)=
    d.nextdef:=modulenamelist
    modulenamelist:=d
end

global proc addimport(ref strec d)=
    ref stlistrec p

    p:=pcm_alloc(stlistrec.bytes)
    p.def:=d
    p.nextitem:=globalimportlist
    globalimportlist:=p
end

global proc createlabel(ref strec symptr,int symbol)=
    symptr.symbol:=symbol
    symptr.stindex:=0
    symptr.moduleno:=currmoduleno
    adddef(symptr)
end

global proc createnamedconst(ref strec symptr,ref opndrec expr)=
    symptr.symbol:=namedconstsym
    symptr.expr:=expr
    adddef(symptr)
end

global proc createregalias(ref strec symptr,int regindex, regsize)=
    symptr.symbol:=kregsym
    symptr.ksymbol:=kregsym
    symptr.subcode:=regindex
    symptr.regsize:=regsize

    adddef(symptr)
end

global proc createxregalias(ref strec symptr,int regindex)=
    symptr.symbol:=kxregsym
    symptr.ksymbol:=kxregsym
    symptr.subcode:=regindex

    adddef(symptr)
end

global proc gerror(ichar mess)=
    println "SS code gen error:",mess
    println "On line:", alineno
    println
    stop 1
end

global proc serror(ichar mess)=
    println "Syntax error: '",,mess,,"' on line",lxlineno,moduletable[currmoduleno].name
    stop 1
end

global proc serror_s(ichar mess, param)=
    [256]char str
    sprintf(&.str,mess, param)
    serror(&.str)
end

function inttostr(int64 a)ichar=
    static [64]char str

    getstrint(a,&.str)
    return &.str
end

function realtostr(real a)ichar=
    static [64]char str
    strcpy(&.str,strreal(a))
    return &.str
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
        println "dbuffer error"
        cpl
        cpl
        cpl
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

global proc addbyte(ref dbuffer a, int x)=
    a.pcurr^:=x
    ++a.pcurr
end

global proc addword(ref dbuffer a, int x)=
    a.pcurr16^:=x
    ++a.pcurr16
end

global proc adddword(ref dbuffer a, int x)=
    a.pcurr32^:=x
    ++a.pcurr32
end

global proc addqword(ref dbuffer a, int64 x)=
    a.pcurr64^:=x
    ++a.pcurr64
end

global proc printmodulesymbols(filehandle f)=
    [256]char str
    ref strec d,e

    println @f,"MODULE SYMBOLS IN",moduletable[currmoduleno].name

    d:=modulenamelist

    while d do
        print @f,"   ",,padstr(d.name,14),padstr(symbolnames[d.symbol],12)



        fprint @f,"|| # # #",d.htfirstindex:"6",d.htindex:"6",d:"8H"

        e:=dupltable[d.htfirstindex]
        if e then
            print @f,"||"
            while e do
                print @f,"(",,e.name,,")"
                e:=e.nextdupl
            od
        fi
        println @f," BASE:",(d.basedef|d.basedef.name|""),d.basedef
        d:=d.nextdef
    od
    println @f
end

global proc printimportsymbols(filehandle f)=
    ref strec d,e
    ref stlistrec p

    println @f,"GLOBAL IMPORT TABLE",globalimportlist

    p:=globalimportlist

    while p do
        d:=p.def
        print @f,"   ",,padstr(d.name,14),padstr(symbolnames[d.symbol],12)
        println @f,=d.offset,reftypenames[d.reftype],ref void(d)
        p:=p.nextitem
    od
    println @f
end

global proc printdupltable(filehandle f)=
    [256]char str
    ref strec d,e
    ref stlistrec p
    int i

    println @f,"DUPL TABLE"

    for i:=0 to dupltable.upb when dupltable[i] do
        d:=dupltable[i]

        print @f,"  ",d.htfirstindex,,":"
        while d do

            fprint @&.str,"(# # (#) #) ",d.htindex:"6",d.name,
                    moduletable[d.moduleno].name,d:"8H"

            d:=d.nextdupl
        od
        println @f
    od
    println @f
end

global function roundtoblock(int n,align)int=
    if n iand (align-1)=0 then return n fi

    return n+(align-(n iand (align-1)))
end

=== aa_help.txt 0 1 16/16 ===
'AA' Assembler-Linker for Win64

Assembles ASM files written in a special syntax to OBJ or EXE or DLL format.

Usage:

    aa prog            Assemble prog.asm to prog.exe
    aa prog -dll       Assemble prog.asm to prog.dll
    aa prog -obj       Assemble prog.asm to prog.obj (needs ext. linker)

    aa a b c           Assemble&link modules a.asm, b.asm, c.asm into a.exe

Options:

    -out:file          Name output file (default is .exe applied to 1st module)
    -exe               Generate executable (default)
    -obj               Generate object file (one .obj file for multiple i/p files)
    file.dll           Include library in list of DLLs to search

    @file              Read options and files from @ file

Can only link to external DLL libraries; not other .o/.obj/.lib/.a files.

DLLs msvcrt.dll, user32.dll, gdi32.dll, user32.dll are automatically included.
=== END ===
1 aa.m
2 aacli.m
3 aa_mcxdecls.m
4 aa_decls.m
5 aa_tables.m
6 aa_objdecls.m
7 aa_lex.m
8 aa_parse.m
9 aa_showss.m
10 aa_writeobj.m
11 aa_writeexe.m
12 aa_writess.m
13 aa_disasm.m
14 aa_genss.m
15 aa_lib.m
16 aa_help.txt
