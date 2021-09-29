import msys
import clib
import mlib
import oslib

import* pci_core
import* pci_read
import pc_win64

mapmodule pci_clang => pci_clangx
import* pci_clang

!mapmodule pci_mcl => pci_mclx
import* pci_mcl

import pc_genss
import pc_objdecls
import pc_writeexe

mapmodule pc_writess => pc_writessx
import pc_writess

const rtsfile="rts.pcl"

int target
!int optimflag

byte fshowc
byte fshowpcl
byte fshowmcl
byte fshowss
byte fshowst
byte fshowstflat
byte fshowtiming
byte fverbose
byte freadpcl           !1=pcl input, 0=pcb
byte freadrts=1         !1=include rts.pcl
byte mcltarget          !true when target is exe/dll/asm
byte foptim             !true when target is exe/dll/asm

ichar destfile          !actual output file
ichar destext
ichar destfilename      !override with -out
ichar destfilepath      !override with -outpath
ichar infile            !copy of inputfiles[1]

tabledata() []ichar targetnames =
    (load_target,   $),
    (pcl_target,    $),
    (pcb_target,    $),
    (clang_target,  $),
    (exe_target,    $),
    (dll_target,    $),
    (asm_target,    $),
    (run_target,    $),
    (runvm_target,  $),
    (runjit_target, $),
end

tabledata() []ichar optionnames=

    (exe_sw,        "exe"),
    (dll_sw,        "dll"),
    (obj_sw,        "obj"),
    (asm_sw,        "asm"),
    (clang_sw,      "clang"),
    (pcl_sw,        "pcl"),
    (pcb_sw,        "pcb"),
    (load_sw,       "load"),
    (runvm_sw,      "runvm"),
    (runjit_sw,     "runjit"),

    (opt_sw,        "opt"),
    (opt1_sw,       "opt1"),
    (opt2_sw,       "opt2"),

    (rts_sw,        "rts"),
    (norts_sw,      "norts"),

    (showpcl_sw,    "showpcl"),
    (showmcl_sw,    "showmcl"),
    (showc_sw,      "showc"),
    (ss_sw,         "showss"),
    (st_sw,         "st"),
    (stflat_sw,     "stflat"),

    (time_sw,       "time"),
    (v_sw,          "v"),
    (quiet_sw,      "q"),
    (help_sw,       "h"),
    (help2_sw,      "help"),
    (out_sw,        "out"),
    (outpath_sw,    "outpath"),
end

ichar progsource
ichar error

const maxinputfiles=20
const maxlibfiles=20

global [0..maxinputfiles]ichar inputfiles
global [0..maxlibfiles]ichar libfiles
global int ninputfiles
global int nlibfiles


proc start=

    getinputoptions()
!
    println "Processing",infile,"to",destfile
!!
    if freadpcl then
        if not pcl_readpclfile(infile,(freadrts|rtsfile|nil)) then
            loaderror(pcl_lasterror(),infile)
        fi
    else
        loaderror("Can't load .pcl files yet")
    fi

    case target
    when load_target then
        println "Done"
    when pcl_target then
        println "Writing to:",destfile
        pcl_writepclfile(destfile)
        showoutputfile(fshowpcl)
    when pcb_target then
        loaderror("PCB writing not ready")
    when clang_target then
!       loaderror("Clang target not ready")
        pcl_writeclangfile(destfile)
!       doclang(destfile)
        showoutputfile(fshowc)
!   when exe_target,dll_target,asm_target then
!       loaderror("x64 target not ready")
    when runvm_target then
        loaderror("RunVM target not ready")
!   when runjit_target then
!       loaderror("RunJIT target not ready")
    when asm_target then
        pcl_writeasmfile(destfile,foptim)
        showoutputfile(fshowmcl)
    when exe_target then
        pcl_writeexefile(destfile,foptim)
        if fshowss then
            pcl_showss("SS",1)
            destfile:="SS"
            showoutputfile(1)
        fi

    when dll_target then
        pcl_writedllfile(destfile,foptim)
!   esac
!   elsif mcltarget then
!       domcl(destfile)
    esac
end

proc showoutputfile(int flag)=
    [300]char str

    if flag then
        println @str,"\\m\\olded.bat -w ",destfile
        os_execwait(str,1,nil)
    fi
end

proc getinputoptions=
    int paramno,pmtype
    ichar name,value,ext
    [300]char filespec

    paramno:=2

    while pmtype:=nextcmdparam(paramno,name,value,"pcl") do

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
            if ninputfiles>=maxinputfiles then
                loaderror("Too many input files")
            fi
            convlcstring(name)
            inputfiles[++ninputfiles]:=pcm_copyheapstring(name)

        when pm_libfile then
            if nlibfiles>=maxlibfiles then
                loaderror("Too many lib files")
            fi
            libfiles[++nlibfiles]:=pcm_copyheapstring(name)
        esac

    od

    if target=0 then
        target:=exe_target
        destext:="exe"
    fi

    if ninputfiles=0 then
        println "Usage:"
        println "   ",,sysparams[1],"filename[.pcl/.pcb]    # Compile to executable"
        println "   ",,sysparams[1],"-help                  # Show other options"
        stop

    elsif ninputfiles=1 then
        infile:=inputfiles[1]               !primary file name

        ext:=extractext(infile)

!default output
        destfile:=pcm_copyheapstring(changeext(infile,destext))

        if destfilename then
            destfile:=pcm_copyheapstring(addext(destfilename,destext))
        elsif destfilepath then
            strcpy(&.filespec,destfilepath)
            strcat(extractfile(&.filespec), destfile)
            destfile:=pcm_copyheapstring(&.filespec)    
        fi

        if eqstring(destfile, inputfiles[1]) then
            loaderror("Overwriting input file:",destfile)
        fi

        freadpcl:=eqstring(convlcstring(extractext(inputfiles[1])),"pcl")

    else
        loaderror("Can't do multiple pcl/pcb modules yet")
    fi

    if target in [exe_target, dll_target, asm_target, runjit_target] then
        mcltarget:=1
    fi

end

proc do_option(int sw, ichar value)=
    static byte outused, outpathused

    switch sw
    when exe_sw then target:=exe_target; destext:="exe"
    when dll_sw then target:=dll_target; destext:="dll"
    when asm_sw then target:=asm_target; destext:="asm"
    when clang_sw then target:=clang_target; destext:="c"
    when pcl_sw then target:=pcl_target; destext:="pcl"
    when pcb_sw then target:=pcb_target; destext:="pcb"
    when runvm_sw then target:=runvm_target; destext:=""
    when runjit_sw then target:=runjit_target; destext:=""
    when load_sw then target:=load_target; destext:=""

    when opt_sw then foptim:=2
    when opt1_sw then foptim:=1
    when opt2_sw then foptim:=2
    when rts_sw then freadrts:=1
    when norts_sw then freadrts:=0

    when time_sw then fshowtiming:=1

    when v_sw then fverbose:=1

    when help_sw,help2_sw then showhelp(); stop

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

    when showpcl_sw then fshowpcl:=1
    when showmcl_sw then fshowmcl:=1
    when showc_sw then fshowc:=1
    when ss_sw then fshowss:=1
    when st_sw then fshowst:=1
    when stflat_sw then fshowstflat:=1
    endswitch
end

proc showhelp=
    println "Help not ready"
    stop
end

global proc loaderror(ichar mess,param="")=
    println "Load error:",mess,param
    stop 1
end

