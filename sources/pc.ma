=== MA 25 ===
=== pc.m 0 0 1/25 ===
	module pccli
	module pc_decls
	module pc_disasm
	module pc_genmcl
	module pc_genss
	module pc_lex
	module pc_libmcl
	module pc_libpcl
	module pc_objdecls
	module pc_optim
	module pc_parse
	module pc_runmx
	module pc_stackmcl
	module pc_tables
	module pc_writeexe
	module pc_writemx
	module pc_writess

	module pci_mcl as md
	module pc_win64
	module rundecls
	module runmx
	module runshow
=== pccli.m 0 0 2/25 ===
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
byte freadpcl			!1=pcl input, 0=pcb
byte freadrts=1			!1=include rts.pcl
byte mcltarget			!true when target is exe/lib/asm
byte foptim				!true when target is exe/lib/asm

ichar destfile			!actual output file
ichar destext
ichar destfilename		!override with -out
ichar destfilepath		!override with -outpath
ichar infile			!copy of inputfiles[1]

tabledata() []ichar targetnames =
	(load_target,	$),
	(pcl_target,	$),
	(pcb_target,	$),
	(clang_target,	$),
	(exe_target,	$),
	(lib_target,	$),
	(asm_target,	$),
	(run_target,	$),
	(runvm_target,	$),
	(runjit_target,	$),
end

tabledata() []ichar optionnames=

	(exe_sw,		"exe"),
	(lib_sw,		"lib"),
	(obj_sw,		"obj"),
	(asm_sw,		"asm"),
	(clang_sw,		"clang"),
	(pcl_sw,		"pcl"),
	(pcb_sw,		"pcb"),
	(load_sw,		"load"),
	(runvm_sw,		"runvm"),
	(runjit_sw,		"runjit"),

	(opt_sw,		"opt"),
	(opt1_sw,		"opt1"),
	(opt2_sw,		"opt2"),

	(rts_sw,		"rts"),
	(norts_sw,		"norts"),

	(showpcl_sw,	"showpcl"),
	(showmcl_sw,	"showmcl"),
	(showc_sw,		"showc"),
	(ss_sw,			"showss"),
	(st_sw,			"st"),
	(stflat_sw,		"stflat"),

	(time_sw,		"time"),
	(v_sw,			"v"),
	(quiet_sw,		"q"),
	(help_sw,		"h"),
	(help2_sw,		"help"),
	(out_sw,		"out"),
	(outpath_sw,	"outpath"),
end

ichar progsource
ichar error

const maxinputfiles=20
const maxlibfiles=20

global [0..maxinputfiles]ichar inputfiles
global [0..maxlibfiles]ichar libfiles
global int ninputfiles
global int nlibfiles


!proc main=
proc main=

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
!		loaderror("Clang target not ready")
		pcl_writeclangfile(destfile)
!		doclang(destfile)
		showoutputfile(fshowc)
!	when exe_target,dll_target,asm_target then
!		loaderror("x64 target not ready")
	when runvm_target then
		loaderror("RunVM target not ready")
!	when runjit_target then
!		loaderror("RunJIT target not ready")
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

	when lib_target then
		pcl_writelibfile(destfile,foptim)
!	esac
!	elsif mcltarget then
!		domcl(destfile)
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

	paramno:=1

	while pmtype:=nextcmdparamnew(paramno,name,value,"pcl") do
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
		println "	",,sysparams[1],"filename[.pcl]      # Compile to executable"
		println "	",,sysparams[1],"-help               # Show other options"
		stop

	elsif ninputfiles=1 then
		infile:=inputfiles[1]				!primary file name

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

	if target in [exe_target, lib_target, asm_target, runjit_target] then
		mcltarget:=1
	fi

end

proc do_option(int sw, ichar value)=
	static byte outused, outpathused

	switch sw
	when exe_sw then target:=exe_target; destext:="exe"
	when lib_sw then target:=lib_target; destext:="lib"
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
	println strinclude "pchelp.txt"
	stop
end

global proc loaderror(ichar mess,param="")=
	println "Load error:",mess,param
	stop 1
end

export proc abc=
end
export proc zyx=
end

export function dummyproc(int a,b)int =
	[4]char str
	int c:=12345678
	str[1]:='B'
	str[2]:='A'
	str[3]:='R'
	str[4]:=0
	puts(&.str)

	a+b
end
=== pc_decls.m 0 0 3/25 ===
export type psymbol = ref pstrec
export type pcl = ref pclrec

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record pstrec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref fwdrec fwdrefs	!fwd ref chain
	pcl pcldef			!points to pcl instruction that defines this name

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	union
		byte subcode		!when used as keyword
		byte rtsindex	!proc names: rts index if it's an rts function
	end
	byte pclop			!pcl op used to define this: klocal etc

	byte scope			!label pass 1: fwd/extern/local/global
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0
	byte namelen

	int32 offset		!label (pass 2): offset of label when encountered
	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	int32 labelno		!assigned global label when a proc/static
	int16 importindex	!genexe: index into import table

	word16 flags:(isdefined:1, isimported:1, addrof:1,
				isexported:1, isfloat:1, noreg:1, atvar:1,
				isthreaded:1, istruename:1, isrts:1)

	int16 nrefs
	int16 impindex
	int16 expindex
	byte reg
	byte spare
	psymbol nextsym
!	[9]byte spare
end

export record pclrec =
	byte opndtype
	byte opcode
	byte flags:(isexported:1, isimported:1)
	byte mode				!t in tables

	int32 size

	union
		struct
			union
				int64 value
				real64 xvalue
				real32 xvalue32
				ichar svalue
				int tempno
				int labelno
				psymbol def
				ref void asmcode
			end
			union						!two 32-bit params used according to opcode
				struct
					int32 x				!common access to these 1/2 extra attribs
					int32 y
				end

				struct					! (x,y) pointer ops
					int32 scale			! scale factor for offset
					int32 extra			! extra constant byte offset, already scaled
				end
				struct					! (x,y) call/etc
					int32 nargs			! number of args
					int32 nvariadics	! 0, or arg # that is first variadic
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end

				int32 oldmode			! (x) u in tables
				int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				int32 truncmode			! (x) convert/truncate: truncated mode
				int32 align
				int32 extvariadics		! (x) for an extproc; 0 or start param of variadics
				int32 nret				! (x) setretmult: no. of return values

			end
		end
	end
	u32 seqno
	u32 spare
end

global int optimflag

global [rtsnames.len]psymbol rtsproctable		!nil, or an rts proc has been defined in pcl code

global const maxpdlllib=50
global int npdllnametable
global [maxpdlllib]ichar pdllnametable

global psymbol pallsymbols				!linked list of all symbols
global byte pstready=1					!1 addsym enabled (disabled during hst init)

!PROC $INIT=
!CPL "PCL DECLS", PSTREC.BYTES
!END
=== pc_disasm.m 0 0 4/25 ===
!const showmregs=1
const showmregs=0

const halt=0xF4

int nmodules
int xfchsmask_pd

tabledata() [0:]ichar opnames =
	(add_op=0,	"add"),
	(or_op,		"or"),
	(adc_op,	"adc"),
	(sbb_op,	"sbb"),
	(and_op,	"and"),
	(sub_op,	"sub"),
	(xor_op,	"xor"),
	(cmp_op,	"cmp")
end

[0:]ichar condnames = 
("o", "no", "b","ae","z","nz","be","a","s","ns","p","np",
 "l","ge","le","g")

tabledata() []ichar addrmodenames=		! rm modes
	(amreg,			$),				! R
	(ammem,			$),				! [R+d]
	(amrel,			$)				! [RIP+d]
end

const wmask = 2x1000
const rmask = 2x0100
const xmask = 2x0010
const bmask = 2x0001

const rstack=5						!1-base register codes
const rframe=6

int rex

int addrmode						!amreg/ammem/amrel
int rmreg							!0, or 1..16; adjusted middle value of modrm byte
int rmopc							!0 to 7; middle value of modrm byte 
int basereg							!0, or 1..16
int indexreg						!0, or 1..16
int scale							!1,2,4
int opsize							!1,2,4,8
int offset
int offsetsize						!1 or 4
int sizeoverride					!32=>16 switch
int addroverride					!32=>16 switch
int f2override						!xmm regs
int f3override						!xmm regs

[256]char deststr
ichar destptr

ref byte codeptr

global function decodeinstr(ref byte &cptr,baseaddr=nil)ichar=
!decode next instruction at codeptr
!return 1 if decoded, with codeptr stepped to start of next instruction
!return 0 when end-of-code seen (nop or 0x90)
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

	retry::						!back here after prefix byte seen

	switch opc:=codeptr++^
	when 0x00,0x1, 0x08,0x9, 0x10,0x11, 0x18,0x19,
						0x20,0x21, 0x28,0x29, 0x30,0x31, 0x38,0x39 then	!arith R/M, R
		op:=opc>>3
		decodeaddr(opc iand 1)
		getsilx(basereg)
		getsil(rmreg)
		genstr(opnames[op])
		printaddrmode()
		genstr(", ")
		genstr(strreg(rmreg,opsize))

	when 0x02,0x3, 0x0A,0xB, 0x12,0x13, 0x1A,0x1B,
						0x22,0x23, 0x2A,0x2B, 0x32,0x33, 0x3A,0x3B then	!arith R,R/M
		op:=opc>>3
		decodeaddr(opc iand 1)
		genstr(opnames[op])
		genstr(" ")
		getsil(rmreg)
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0x04,0x5, 0x0C,0xD, 0x14,0x15, 0x1C,0x1D,
						0x24,0x25, 0x2C,0x2D, 0x34,0x35, 0x3C,0x3D then	!arith rAX,imm
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
!	if rex iand wmask then wopsize:=8 fi

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

	when 0x80..0x83 then			!arith r/m,imm
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

	when 0x84, 0x85 then			!test reg,reg/mem
		decodeaddr(opc iand 1)
		getsilx(basereg)
		getsil(rmreg)
		genstr("test ")
		printaddrmode()
		genstr(", ")
		genstr(strreg(rmreg,opsize))

	when 0x86,0x87 then				!complex excg
		decodeaddr(opc iand 1)
		genstr("exch2 ")
		getsilx(basereg)
		getsil(rmreg)
		genstr(strreg(rmreg,opsize))
		genstr(",")
		printaddrmode()

	when 0x88, 0x89 then			!mov r/m,reg
		decodeaddr(opc iand 1)
		genstr("mov")
		getsilx(basereg)
		getsil(rmreg)

		printaddrmode()
		genstr(", ")
		genstr(strreg(rmreg,opsize))

	when 0x8A, 0x8B then			!mov reg,r/m
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

	when 0x91..0x97 then			!exch eax/reg
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

	when 0xA8, 0xA9 then				!test r0,imm
		genstr("test ")
		if opc iand 1 then
			opsize:=(sizeoverride |2|4)
			if rex iand wmask then opsize:=8 fi
		fi
		genstr(strreg(1,opsize))
		genstr(", ")
		genintd(readimm())

	when 0xB0..0xBF then			!mov reg,imm
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
!	println "	end of code [halt]"
		return nil

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

	when 0xFF then			!various
		w:=1
	doff::
		decodeaddr(w)
		case rmopc
		when 2x_000 then	!inc
			getsilx(basereg)
			genstr("inc")
		when 2x_001 then	!dec
			getsilx(basereg)
			genstr("dec")
		when 2x_010 then	!call
			opsize:=8
			genstr("icall")
		when 2x_100 then	!jmp
			opsize:=8
			genstr("jmp")
		when 2x_110 then	!push
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

!at this point, deststr contains the decoded instruction
!need to put in address, bytes etc

	if baseaddr then
		print @&.str,baseaddr:"z6h",,": "
	else
		print @&.str,pstart:"z6h",,": "
	fi

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
!0F has been decoded
	int opc,rhssize,third,imm
	ichar opcstr

	switch opc:=codeptr++^
	when 0x2A then					!cvtsi2ss/sd XMM, REG/MEM
		decodeaddr(1)
		if f3override then
			genstr("cvtsi2ss ")
		else
			genstr("cvtsi2sd ")
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(0)
		
	when 0x2C then					!cvt2ss/sd2si XMM, REG/MEM
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

	when 0x2D then					!cvt2ss/sd2si XMM, REG/MEM
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

	when 0x2F then					!comiss/comisd XMM, REG/MEM
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

	when 0x3A then					!possible pcmpistri
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

	when 0x51 then					!sqrtss/sd
		decodeaddr(1)
		opsize:=(f3override|4|8)
		genstr((opsize=4|"sqrtss "|"sqrtsd "))
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)

	when 0x54 then					!ANDPD
		decodeaddr(1)
		genstr((sizeoverride|"andpd "|"andps "))
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=(sizeoverride|8|4)
		printaddrmode(1)

	when 0x57 then					!XORPD
		decodeaddr(1)
		genstr((sizeoverride|"xorpd "|"xorps "))
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=(sizeoverride|8|4)
		printaddrmode(1)

	when 0x58 then					!addss/addsd
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

	when 0x59 then					!mulss/mulsd
		opcstr:="muls"
		goto doarith

	when 0x5A then					!cvtss2sd/cvtsd2ss
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

	when 0x5C then					!subss/subsd
		opcstr:="subs"
		goto doarith

	when 0x5D then
		opcstr:="mins"
		goto doarith

	when 0x5E then					!divss/divsd
		opcstr:="divs"
		goto doarith

	when 0x5F then
		opcstr:="maxs"
		goto doarith


	when 0x6E then					!mov X/MM, REG/MEM
		decodeaddr(1)
		opsize:=(rex iand wmask|8|4)
		genstr((opsize=4|"movd "|"movq "))
		if sizeoverride then		!xmm
			genstr(strxmm(rmreg))
		else
			genstr(strmmx(rmreg))
		fi
		genstr(", ")
		printaddrmode()

	when 0x6F then					!movdqa/dqu, X/MEM, X/X
		decodeaddr(1)
		opsize:=16
		if sizeoverride then		!66
			genstr("movdqa ")
		elsif f3override then		!F3
			genstr("movdqu ")
		else
			genstr("No 66/F3 ")
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)

	when 0x7E then					!mov REG/MEM, X/MM
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
			if sizeoverride then		!xmm
				genstr(strxmm(rmreg))
			else
				genstr(strmmx(rmreg))
			fi
		fi

	when 0x7F then					!movdqa/dqu, MEM/X
		decodeaddr(1)
		opsize:=16
		if sizeoverride then		!66
			genstr("movdqa ")
		elsif f3override then		!F3
			genstr("movdqu ")
		else
			genstr("No 66/F3 ")
		fi
		printaddrmode(1)
		genstr(", ")
		genstr(strxmm(rmreg))

	when 0x80..0x8F then			!long rel jumps
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

	when 0xA2 then
		genstr("cpuid")

	when 0xAF then
		decodeaddr(1)
		genstr("imul ")
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0xB6, 0xB7, 0xBE, 0xBF then
		decodeaddr(1)
!	opsize:=4
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

	when 0xDB then					!PAND
		decodeaddr(1)
		genstr("pand ")
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=8	!(sizeoverride|8|4)
		printaddrmode(1)

	when 0xEF then					!PXOR
		decodeaddr(1)
		genstr("pxor ")
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=8	!(sizeoverride|8|4)
		printaddrmode(1)


	else
	error::
		genstr("Unknown opcode 2-byte opcode: 0F ")
    genhex(opc)
	endswitch
end

proc decodeaddr(int w=0)=
!codeptr points to modrm byte, with possible sib and/or disp following
!decode modrm, sib and disp
!store result in amode::
! basereg		0 when not used
! indexreg
! scale			1,2,4,8 factor for indexreg
! offset		0, or any offset or abs address
! addrmode		rm-code
!the function returns the xxx value (middle part of modrm byte)
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

	if mode=3 then		!plain register access
		basereg:=rm+1
		addrmode:=amreg
	elsif rm<>4 then				!not esp; no sib
		if mode=0 and rm=5 then		![ebp] is actually [rip+disp]
			offset:=readint32()		!
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
	else			!sib follows
		addrmode:=ammem
		sib:=readbyte()
		indexreg:=((sib>>3) iand 7)+1
		basereg:=(sib iand 7)+1
		scale:=(sib>>6+1|1,2,4,8|0)

		if mode=0 and basereg=rframe and indexreg=rstack then	!no base/index regs, only d32 disp
			indexreg:=basereg:=0
			offset:=readint32()

		elsif mode=0 and basereg=rframe  then	!no base/index regs, only d32 disp
			basereg:=0
			offset:=readint32()

		elsif mode=0 and indexreg=rstack then	!no index register, only base; no disp
			indexreg:=0

		else
			case mode
			when 1 then
				offset:=readsbyte()
			when 2 then
				offset:=readint32()
			esac
			if indexreg=rstack then				!stack means no index reg
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
!freg is 0-based
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
!	print plus,,offset,"<",ref void(offset),,">"
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
!read signed offset according to opsize

	case opsize
	when 1 then return readsbyte()
	when 2 then return readint16()
	when 4,8 then return readint32()			!64-bit uses 32-bit immediate
	esac
	return 0
end

function readimm8:int64=
!like readimm but can 8 bytes too
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

	bb:=codeptr++^			!following byte

	longopc:=ttt<<8+bb		!bottom 11 bits of 2-bytes opcode
	freg:=(bb iand 7)+1		!where bb specifies a register in bottom 3 bits

!first look at all dedicated opcodes before treating bb as modrm byte

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

	elsecase longopc iand 2x'111'11111'000			!ignore bottom 3 bits

	when 2x'001'11000'000 then genstr("fld "); genstr(strfreg(freg))
	when 2x'101'11010'000 then genstr("fst "); genstr(strfreg(freg))
	when 2x'101'11011'000 then genstr("fstp "); genstr(strfreg(freg))
	when 2x'001'11001'000 then genstr("fxch "); genstr(strfreg(freg))
	when 2x'000'11010'000 then genstr("fcom "); genstr(strfreg(freg))
	when 2x'000'11011'000 then genstr("fcomp "); genstr(strfreg(freg))
	when 2x'101'11000'000 then genstr("ffree "); genstr(strfreg(freg))

	elsecase longopc iand 2x'001'11111'000			!ignore bottom 3 bits and top 2

	when 2x'000'11000'000 then do87arith("fadd",ttt,freg)

	when 2x'000'11100'000 then do87arith("fsub",ttt,freg)
	when 2x'000'11101'000 then do87arith("fsubr",ttt,freg)

	when 2x'000'11001'000 then do87arith("fmul",ttt,freg)

	when 2x'000'11110'000 then do87arith("fdiv",ttt,freg)
	when 2x'000'11111'000 then do87arith("fdivr",ttt,freg)

	else	!finally, have to deal with modrm etc
		--codeptr					!put back modrm byte
		decodeaddr(0)			!code is middle bits
		shortopc:=ttt<<3 + rmopc

		case shortopc				!look at combination of ttt and code (middle bits of modrm)
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

		elsecase shortopc iand 2x001'111		!ignore top two bits (mf code)

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

	d:=ttt iand 2x100		!d=0:  to st0; d<>0: to freg
	p:=ttt iand 2x010		!p<>0: pop after operation

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
!mf has values 0,1,2,4 for type and width, when used; but also 4 for i64
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
!for certain byte-reg combinations, convert regs ah,ch,dh,bh to spl,bpl,sil,dil
	if opsize=1 and not rex and reg>=5 and reg<=8 then
		reg+:=12				!5..8 => 17..20
	fi
end

proc getsilx(int &reg)=
!as getsil but used for basereg, which must have addrmode=amreg
	if addrmode=amreg and opsize=1 and rex=0 and reg>=5 and reg<=8 then
		reg+:=12				!5..8 => 17..20
	fi
end
=== pc_genmcl.m 0 0 5/25 ===
!const fshowpcl=1
const fshowpcl=0
!const fshowopndstack=1
const fshowopndstack=0

!const fshowbothmcl=1
const fshowbothmcl=0

ref mclrec procdefmcl	!points to first mcl instr for proc


pcl currpcl
[0..klast]ref proc(pcl) px_handlertable

[0..5]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[0..5]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

[]int multregs=(r0,r1,r2,r10,r11,r12)
[]int multxregs=(r0,r1,r2,r3,r4,r5)

global proc genmcl(int optim)=

	if mcldone then return fi

	inithandlers()
	optimflag:=optim

	mclinit()

!	for i to nlibfiles when libfiles[i]^<>'$' do
!		pcl_gen((libtypes[i]='D'|kimportdll|kimportlib), pcl_genname(libfiles[i]))
!	od

	currpcl:=pcstart
	mlabelno:=plabelno
	mseqno:=0
	passno:=1

	repeat
		convertpcl(currpcl)
		++currpcl
	until currpcl.opcode=kendprogram

	genabsneg()
	genstringtable()
	genrealtable()
	genrtsproctable()

	genfunctiontable()
	genmc(m_nop)
	genmc(m_nop)

	mcldone:=1
end

proc convertpcl(pcl p)=
	[1256]char str
	ichar ss
	int m

	if fshowpcl  then
		case p.opcode
		when klabel, kcomment, klocal, kprocdef, kprocentry, kthreadedproc,
			kretproc, kendproc, kparam, kextproc, kextparam, kendextproc then
		else
				strcpy(&.str,"                       ")
				strcat(&.str,pclnames[p.opcode])
				mgencomment(&.str)
		esac
	fi

!CPL PCLNAMES[P.OPCODE]

	mseqno:=p.seqno
	px_handlertable[p.opcode]^(p)

	if fshowopndstack then
		case p.opcode
		when klabel, kcomment, klocal, kprocdef, kprocentry, kthreadedproc,
			kretproc, kendproc, kparam, kextproc, kextparam, kendextproc then
		else
			showopndstack()
		esac
	fi
end

proc inithandlers=
	static byte initdone=0
	ichar name
	int n

	if initdone then return fi

	n:=$get_nprocs()

	for i to n do
		name:=$get_procname(i)
		if eqbytes(name,"px_",3) then
			for k in pclnames.bounds do
				if eqstring(pclnames[k]+1,name+3) then
					px_handlertable[k]:=$get_procaddr(i)
					exit
				fi
			else
				gerrorc("Invalid handler name:",name)
			od
		fi
	od

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc unimpl(pcl p)=
!doesn't need a handler, but used as default handler for all opcodes
!for which its pc-handler doesn't exist
	[300]char str

	print @str,"Unimplemented Opcode:",pclnames[p.opcode]
	mgencomment(str)
!	gerrorc("Unimplemented Opcode:",pclnames[p.opcode])
	println "Unimplemented Opcode:",pclnames[p.opcode]
end

global proc gerrorc(ichar mess, param=nil)=
	print "MCL Gen error:",mess
	if param then
		print ":",param
	fi

!	println " on line:",lxlineno
	stop 1
end

proc px_zero(pcl p)=
	unimpl(p)
end

proc px_nop(pcl p)=
	unimpl(p)
end

proc px_stop(pcl p)=
	psymbol d

	loadparam(1,r10)

	d:=pcl_makesymbol("exit")
	d.isimported:=1
	genmc(m_call, mgenmemaddr(d))

	delopnd()
end

proc px_comment(pcl p)=
	mgencomment(p.svalue)
end

proc px_importdll(pcl p)=
!	unimpl(p)
end

proc px_importlib(pcl p)=
!	unimpl(p)
end

proc px_istatic(pcl p)=
	setsegment('I',p.align)
	genmc(m_labelname,mgenmemaddr(p.def))
end

proc px_zstatic(pcl p)=
	psymbol d

	d:=p.def
	setsegment('Z',p.align)
	genmc(m_labelname,mgenmemaddr(d))

	genmc(m_resb,mgenint(p.size))
end

proc px_equiv(pcl p)=
	unimpl(p)
end

proc px_procdef(pcl p)=
	ichar name

	procdefpcl:=currpcl
	procdef:=currpcl.def

	name:=procdef.name
!
!CPL "--------PROCDEF",NAME

	setsegment('C',16)
	if passno=1 then
		mgencomment("DUMMY")
		procdefmcl:=mccodex
	fi

	genmc(m_procstart,mgenmemaddr(procdef))
	genmc(m_labelname,mgenmemaddr(procdef))

	nlocals:=nparams:=0
end

proc px_threadedproc(pcl p)=
	px_procdef(p)
end

proc px_procentry(pcl p)=
	int np, regoffset, offset, dreg, xreg, nregparams, nspill,hasequiv
	mcloperand ax
	psymbol d

!CPL "PROCENTRY",PROCDEF.NAME,=INF_LEAFPROC

	framebytes:=0
	frameoffset:=0
	paramoffset:=0
	needstackframe:=0
	ndsaveregs:=nxsaveregs:=0			!not of if b=non-vol regs to be spilled
	ndsavepush:=0
	nregparams:=nspill:=0
	needshadow48:=0			!duplicate caller's shadow space
	needshadow32:=0			!local shadow space
	hasequiv:=0

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF1",=NDSAVEPUSH,=NDSAVEREGS,=INF_HIGHREG,=INF_ASSEM FI
	if inf_assem then
!CPL "SKIP",INF_ASSEM
 skip fi

	if passno=2 and inf_leafproc then	!no point in pass1 as no info avialable
		dreg:=r10			!next available dreg
		xreg:=r0			!next available xreg

		for i to nparams do
			if i>4 then exit fi
			d:=paramdefs[i]
			case pcat[d.pcldef.mode]
			when d64cat then
				if not d.addrof and not d.noreg and d.nrefs then
					d.reg:=dreg
					isregvar[dreg]:=1
					if dreg=r10 then inf_r10used:=1 fi
					if dreg=r11 then inf_r11used:=1 fi
					if dreg=r13 then inf_r13used:=1 fi
					++nregparams
				fi
			when x64cat then
				if not d.addrof and d.nrefs then
					d.reg:=xreg
					isxregvar[dreg]:=1
					++nregparams
				fi
			esac
			++dreg
			++xreg
		od
	fi
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF2",=NDSAVEPUSH,=NDSAVEREGS,=INF_HIGHREG FI

	if passno=2 then		!no point in pass1 as no info avialable
		dreg:=r9			!next available dreg
		xreg:=r15			!next available xreg
		for i to nlocals do
			d:=localdefs[i]
			case pcat[d.pcldef.mode]
			when d64cat then
				if not d.addrof and not d.noreg and d.nrefs then
					if dreg<=inf_highreg or dreg<r3 then next fi
					dsaveregs[++ndsaveregs]:=dreg
					d.reg:=dreg
					isregvar[dreg]:=1
					--dreg
				fi
			when x64cat then
				if not d.addrof and d.nrefs and not d.noreg then
!				if not d.addrof and d.nrefs then
					if xreg<=inf_highxreg or xreg<r6 then next fi
					xsaveregs[++nxsaveregs]:=xreg
					d.reg:=xreg
					isxregvar[dreg]:=1
					--xreg
				fi
			esac
		od

!see if any params not regvars can use spare nonvol regs
		if not inf_leafproc then
			for i to nparams do
				if i>4 then exit fi
				d:=paramdefs[i]
				case pcat[d.pcldef.mode]
				when d64cat then
					if not d.addrof and d.nrefs and not d.noreg then
						if dreg<=inf_highreg or dreg<r3 then next fi
						dsaveregs[++ndsaveregs]:=dreg
						d.reg:=dreg
						isregvar[dreg]:=1
						--dreg
						++nregparams
					fi
				when x64cat then
					if not d.addrof and d.nrefs and not d.noreg then
						if xreg<=inf_highxreg or xreg<r6 then next fi
						xsaveregs[++nxsaveregs]:=xreg
						d.reg:=xreg
						isxregvar[dreg]:=1
						--xreg
						++nregparams
					fi
				esac
			od
		fi

	fi
skip::
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF3",=NDSAVEPUSH,=NDSAVEREGS FI

	for i to nparams do
		d:=paramdefs[i]

!CPL =D.NAME

		if not d.reg then			!not a regvar
			if i>1 and pcat[d.pcldef.mode]=widecat and paramdefs[i-1]=d then
!CPL "SKIP"
			else
				d.offset:=paramoffset+16
				genmc(m_define, mgenname(getfullname(d)), mgenint(d.offset))
			fi

		elsif pcat[d.pcldef.mode]=d64cat then
			genmc(m_definereg, mgenname(getfullname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getfullname(d)), mgenxreg(d.reg))
		fi
		paramoffset+:=8
	od

	for i:=r3 to inf_highreg do		!add any non-vol regs
		dsaveregs[++ndsaveregs]:=i
	od

	for i:=r6 to inf_highxreg do		!add any non-vol xregs
		xsaveregs[++nxsaveregs]:=i
	od

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF4",=NDSAVEPUSH,=NDSAVEREGS FI

!Decided class of proc entry/exit code:
!(1) Full (always a stack frame, nonvols not pushed)
!(2) Pushed nonvols, uses stack frame
!(3) Pushed nonvols, no stack frame

	if nparams>4 then
		needstackframe:=1
		nspill:=4-nregparams
	else
		ndsavepush:=ndsaveregs
		ndsaveregs:=0
		nspill:=nparams-nregparams
		if nspill then needstackframe:=1 fi
!		if nspill and nthen needstackframe:=1 fi
!		if ndsavepush then
!			if nspill then
!				needstackframe:=1
!				needshadow48:=1
!			fi
!		fi
	fi
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF5",=NDSAVEPUSH,=NDSAVEREGS FI

	for i to nlocals do
		d:=localdefs[i]
		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(d.pcldef.size)
			d.offset:=frameoffset
			genmc(m_define, mgenname(getfullname(d)), mgenint(d.offset))
		elsif pcat[d.pcldef.mode]=d64cat then
			genmc(m_definereg, mgenname(getfullname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getfullname(d)), mgenxreg(d.reg))
		fi
	od

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF6",=NDSAVEPUSH,=NDSAVEREGS FI
	if hasequiv then
		for i to nlocals do
			d:=localdefs[i]

			if d.atvar then
MERROR("D.ATVAR NOT READY")
!				d.offset:=d.equivvar.def.offset
!				if d.reg then merror("@ on reg var") fi
!				genmc(m_define, mgenname(getfullname(d)),
!					 mgenname(getfullname(d.equivvar.def)))
			fi
		od
	fi

	frameoffset-:=ndsaveregs*8			!non-vol reg spill area
	dsaveoffset:=frameoffset
	frameoffset-:=nxsaveregs*8
	xsaveoffset:=frameoffset

	framebytes:=-frameoffset

!CPL =PROCDEF.ISTHREADED
	if (nlocals or nparams) and procdef.isthreaded then
		merror("params/locals in threaded?")
	fi
	if framebytes then needstackframe:=1 fi	!may already be set

	while framebytes iand 15 do ++framebytes od	!multiple of 16

	if needstackframe and ndsavepush.odd then framebytes+:=8 fi	!alignment

	if needstackframe and not inf_leafproc then
		framebytes +:= 32
	fi

	if needstackframe and ndsavepush then needshadow48:=1 fi

!start to generate code
	if not needstackframe and not inf_leafproc and not procdef.isthreaded then
		needshadow32:=(ndsavepush.odd | 32 | 40)
	fi

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF7",=NDSAVEPUSH,=NDSAVEREGS FI
!!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN
!!CPL "............FOUND PROC"
!mgeninfos("***NAME***",PROCDEF.NAME)
!mgeninfo("NPARAMS",nparams)
!mgeninfo("NDSAVE",ndsaveregs)
!mgeninfo("NXSAVE",nxsaveregs)
!mgeninfo("NDPUSH",ndsavepush)
!mgeninfo("NSPILL",nspill)
!mgeninfo("NREGPARAMS",nregparams)
!mgeninfo("SHADOW48",needshadow48)
!mgeninfo("FRAMEBYTES",framebytes)
!mgeninfo("SHADOW32",needshadow32)
!mgeninfo("NEEDFRAME",needstackframe)
!mgeninfo("LEAFPROC",inf_leafproc)
!mgeninfo("ISTHREADED",procdef.isthreaded)
!mgeninfo("ASSEM USED",inf_assem)
!fi

	for i to ndsavepush do
		genmc(m_push, mgenreg(dsaveregs[i]))
	od

	if needshadow48 then			!create new shadow space to spill params
!MGENCOMMENT("NEED SHADOW SPACE")
		pushstack(48)
	fi

	if needstackframe then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		if framebytes then
			pushstack(framebytes)
		fi
	elsif needshadow32 then
!MGENCOMMENT("PUSH SHADOW32")
		pushstack(needshadow32)
	fi

!SAVE D3..D9
	offset:=dsaveoffset
	for i to ndsaveregs do
		genmc(m_mov, mgenindex(areg:rframe, size:8, offset:offset),
			mgenreg(dsaveregs[i]))
		offset+:=8
	od

	offset:=xsaveoffset
	for i to nxsaveregs do
		genmc(m_movq, mgenindex(areg:rframe, size:8, offset:offset),
			mgenxreg(xsaveregs[i]))
		offset+:=8
	od

	offset:=16
	regoffset:=0
	for i to nparams do
		if regoffset>3 then exit fi
		d:=paramdefs[i]
		IF NOT D.REG THEN
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
!			ax:=mgenindex(areg:rframe, size:d.size, offset:offset)
			case pcat[d.pcldef.mode]
			when x64cat then
				genmc(m_movq, ax, mgenxreg(r0+regoffset))
			when x32cat then
				genmc(m_movd, changeopndsize(ax,4), mgenxreg(r0+regoffset))
!			when widecat then
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
!				offset+:=8
!				++regoffset
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
			else
				genmc(m_mov, ax, mgenreg(regoffset+r10))
			esac
		elsif d.reg then			!may use non-vol regs
			case pcat[d.pcldef.mode]
			when x64cat then
				if d.reg<>r0+regoffset then
					genmc(m_movq, mgenxreg(d.reg), mgenxreg(r0+regoffset))
				fi
!			when x32cat then
!				genmc(m_movd, ax, mgenxreg(r0+regoffset))
!			when widecat then
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
!				offset+:=8
!				++regoffset
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
			when d64cat then
				if d.reg<>r10+regoffset then
					genmc(m_mov, mgenreg(d.reg), mgenreg(regoffset+r10))
				fi
			esac

		fi
		offset+:=8
		++regoffset
	od
end

proc px_endproc(pcl p)=
	genmc(m_procend)

!CPL "PROCEND",PROCDEF.NAME,=PASSNO
	if passno=1 then
		resetopnds1()

!CPL "AFTER PASS1",=INF_ASSEM,=OPTIMFLAG
		if inf_assem then
			inf_assem:=0
			resetopnds2()

		elsif optimflag and not inf_assem then
			passno:=2
!CPL "HERE"
			currpcl:=procdefpcl-1

			if not fshowbothmcl then
				mccodex:=procdefmcl
				mccodex.nextmcl:=nil
			fi

		fi
	else
		peephole(procdefmcl)
		resetopnds2()
		passno:=1
	fi
end

proc px_endprogram(pcl p)=
	unimpl(p)
end

proc px_extproc(pcl p)=
!	unimpl(p)
end

proc px_extparam(pcl p)=
!	unimpl(p)
end

proc px_extvariadics(pcl p)=
!	unimpl(p)
end

proc px_endextproc(pcl p)=
!	unimpl(p)
end

proc px_extvar(pcl p)=
!	unimpl(p)
end

proc px_local(pcl p)=
	if nlocals>=maxlocals then merror("Too many locals") fi
	++nlocals
	case pcat[p.mode]
	when d64cat then
		++inf_proclocals
	when x64cat then
		++inf_procxlocals
	esac
	localdefs[nlocals]:=p.def
end

proc px_param(pcl p)=
	if nparams>=maxparams then merror("Too many params") fi
	++nparams
	paramdefs[nparams]:=p.def
	if pcat[p.mode]=widecat then
		++nparams
		paramdefs[nparams]:=p.def
	fi
end

proc px_label(pcl p)=
	genmc(m_label, mgenlabel(p.labelno))
end

proc px_labelname(pcl p)=
	unimpl(p)
end

proc px_push(pcl p)=
	case p.opndtype
	when mem_opnd then
		addmem(p)
	when memaddr_opnd then
		addmemaddr(p.def)
	when int_opnd then
		addint(p.value)
	when real_opnd then
		addreal(p.xvalue)
	when real32_opnd then
		addreal32(p.xvalue32)
	when string_opnd then
		addstring(p.svalue)
	else
		merror("push",opndnames[p.opndtype])
	esac
end

proc px_pop(pcl p)=
	mcloperand ax,bx

	case p.opndtype
	when mem_opnd then
		case pcat[p.mode]
		when d64cat then
!load any defered memtypes
			for i to noperands do
				case pclstack[i].loc
				when stack_loc then		!should not be any more
					exit
				when mem_loc then
					if pclstack[i].def=p.def then
						genopnd_ld(i)
					fi
				esac
			od

			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa))
		when x64cat then
			genmc(m_movq, mgenmem(p.def), genopnd_ld(xa))
		when x32cat then
			genmc(m_movd, mgenmem(p.def), genopnd_ld(xa))
		when widecat then
			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa))
			if p.opcode<>kstore then delopnd() fi
			genmc(m_mov, mgenmemhigh(p.def), genopnd_ld(xa))
			if p.opcode<>kstore then delopnd() fi
			return
		when shortcat then
			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa,p.size))

		when blockcat then
			bx:=genopnd_ind()
			addmemaddr(p.def)
			ax:=genopnd_ind()

			copyblock(ax,bx,p.size)

			delopnd()
		else
!			merrort("POPMEM",p.mode)
			CPL "POPMEM",p.mode
		esac
	else
		merroropnd("POP",p.opndtype)
	esac

	if p.opcode<>kstore then
		delopnd()
	fi
end

proc px_store(pcl p)=
	px_pop(p)
end

proc px_pushnc(pcl p)=
	unimpl(p)
end

proc px_opnd(pcl p)=
	unimpl(p)
end

proc px_type(pcl p)=
	unimpl(p)
end

proc px_pushptroff(pcl p)=
	mcloperand ax,bx,cx,fx
	int m

	m:=p.mode

	cx:=do_addrmode(p)

	if pclstack[2].loc<>reg_loc then
		pclstack[2].fmt:=reg_d64			!reg not needed to load addr, but
		pclstack[2].loc:=reg_loc			!need to prepare it for result
		pclstack[2].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=genopnd(xb)

!here, ax is a suitable dest reg (not used for float dest), cx is the access mode

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when x64cat then
!need to turn ax into a float reg
		addreg_x64()
		swapopnds(1,3)
		fx:=genopnd(xc)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
!need to turn ax into a float reg
		addreg_x32()
		swapopnds(1,3)
		fx:=genopnd(xc)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when shortcat then
		cx.size:=p.size
		genmc((psigned[m]|m_movsx|m_movzx), ax, cx)

	when widecat then
		bx:=genopnd_d64()
		swapopnds(1,2)
		swapopnds(2,3)
		genmc(m_mov, bx, applyoffset(cx,8,8))
		genmc(m_mov, ax, changeopndsize(cx,8))
		delopnd()
		setwideopnd()
		return
	when blockcat then
		genmc(m_lea, ax, cx)

	else
		merrort("pushptroff",m)
	esac	

	delopnd()
end

proc px_popptroff(pcl p)=
	mcloperand ax,bx,cx,px
	int m

	m:=p.mode

	px:=do_addrmode(p)
	cx:=genopnd_ld(xc)

	case pcat[m]
	when d64cat then
		genmc(m_mov, px,cx)

	when x64cat then
		genmc(m_movq, px,cx)

	when x32cat then
		genmc(m_movd, changeopndsize(px,4),cx)

	when shortcat then
		px.size:=p.size
		genmc(m_mov, px,changeopndsize(cx,p.size))

	when widecat then
		genmc(m_mov, changeopndsize(px,8),cx)
		genmc(m_mov, applyoffset(px,8,8),genopnd_ld(xc+1))

	when blockcat then
		copyblock(px,makeopndind(cx),p.size)

	else
		merrort("popptroff ",m)
	esac	

	delopnd()
	delopnd()
	if p.opcode=kpopptroff then
		delopnd()
		if pcat[m]=widecat then
			delopnd()
		fi
	fi
end

proc px_storeptroff(pcl p)=
	px_popptroff(p)
end

proc px_pushptr(pcl p)=
	mcloperand ax,px,cx,fx,bx
	int m

	m:=p.mode
	if isregvaropnd(xa) and pcat[m]<>blockcat then
		cx:=mgenireg(pclstack[1].reg)
		ax:=makeregopnd(xa)
	elsif pclstack[xa].fmt=imm_memaddr then
		cx:=mgenmem(pclstack[1].def)
		ax:=makeregopnd(1)
	else
		ax:=genopnd_ld()
		cx:=makeopndind(ax)
	fi

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when shortcat then
		genmc((psigned[m]|m_movsx|m_movzx), ax, changeopndsize(cx,p.size))

	when x64cat then
		addreg_x64()
		swapopnds(1,2)
		fx:=genopnd(xb)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
		addreg_x32()
		swapopnds(1,2)
		fx:=genopnd(xb)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when widecat then
		bx:=genopnd_d64()
		genmc(m_mov, bx, cx)
		genmc(m_mov, ax, applyoffset(cx,8))
		setwideopnd()

	when blockcat then		!nothing further needed

	else

		MGENCOMMENT("****PUSHPTR")
!		merror("pushptr ",stdnames[m])
	esac	

end

proc px_popptr(pcl p)=
	mcloperand ax,bx,cx,px
	int m

	m:=p.mode
	bx:=genopnd_ld(xb)
	if isregvaropnd(xa) and pcat[m]<>blockcat then
		ax:=mgenireg(pclstack[1].reg)
	else
		ax:=genopnd_ind(ya)
	fi

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax,bx)

	when shortcat then
		genmc(m_mov, changeopndsize(ax,psize[m]),changeopndsize(bx,psize[m]))

	when x64cat then
		genmc(m_movq, ax,bx)

	when x32cat then
		genmc(m_movd, changeopndsize(ax,4),bx)

	when widecat then
		genmc(m_mov, ax,bx)
		genmc(m_mov, applyoffset(ax,8),genopnd_ld(xb+1))

	when blockcat then
		copyblock(ax,makeopndind(bx),p.size)

	else
		merrort("popptr ",m)
	esac	

	delopnd()
	if p.opcode=kpopptr then
		delopnd()
		if pcat[m]=widecat then
			delopnd()
		fi
	fi

end

proc px_storeptr(pcl p)=
	px_popptr(p)
end

proc px_dotindex(pcl p)=
	mcloperand ax
	int i

	if pclstack[1].fmt<>imm_d64 then
		merror("dotix i not imm")
	fi

	ax:=genopnd_ld(xb)
	i:=pclstack[1].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi
	genmc(m_andx, changeopndsize(ax,4), mgenint(1))

	delopnd()
end

proc px_popdotindex(pcl p)=
	mcloperand ax,bx,cx,rx,mx
	int i,size,cxfmt,rhs,axoffset

	if pclstack[3].fmt=imm_d64 then
		rhs:=pclstack[3].value
		cx:=nil
	else
		cx:=genopnd_ld(xc)
	fi

	if pclstack[1].fmt<>imm_d64 then
		merror("popdotix i not imm")
	fi
	i:=pclstack[1].value
	size:=p.size

	axoffset:=xb

	addreg_d64()
	rx:=genopnd()
	addreg_d64()
	mx:=genopnd()

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]))
!	else
		ax:=genopnd_ind(axoffset+2,size:size)
		genmc((size=8|m_mov|m_movzx),rx,ax)
!	fi


!	genmc(m_mov,mx,mgenint(1<<i))
	genmc(m_mov,mx,mgenint(inot(1<<i)))
!	genmc(m_notx,mx)
	genmc(m_andx,rx,mx)

	if cx then
		if i then genmc(m_shl, cx, mgenint(i)) fi
		genmc(m_orx, rx, cx)
	elsif rhs<>0 then
		genmc(m_orx, rx, mgenint(1<<i))
	fi

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]), rx)
!	else
		genmc(m_mov,ax,changeopndsize(rx,size))
!	fi

	delopnd()			!mx
	delopnd()			!rx
	delopnd()			!bx/index
	delopnd()			!addr
	if p.opcode=kpopdotindex then
		delopnd()		!value being stored
	fi
end

proc px_storedotindex(pcl p)=
	px_popdotindex(p)
end

proc px_dotslice(pcl p)=
	mcloperand ax,mx,mx4
	int i,j
	word mask

	if pclstack[yb].fmt<>imm_d64 or pclstack[za].fmt<>imm_d64 then
		merror("dotslice i/j not imm")
	fi

	ax:=genopnd_ld(xc)
	i:=pclstack[yb].value
	j:=pclstack[za].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi

	mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
	if mask<=word(int32.maxvalue) then			!use immediate
		genmc(m_andx, ax, mgenint(mask))
	else
		mx:=makeregopnd(yb)
		genmc(m_mov, mx, mgenint(mask))
		genmc(m_andx, ax, mx)
	fi

	delopnd()
	delopnd()
end

proc px_popdotslice(pcl p)=
	mcloperand ax,rx,mx,mx4,dx
	int i,j,size
	word mask

	if pclstack[yb].fmt<>imm_d64 or pclstack[za].fmt<>imm_d64 then
		merror("popdotslice i/j not imm")
	fi

	dx:=genopnd_ld(wd)

	size:=p.size
	ax:=genopnd_ind(xc,size:size)

	i:=pclstack[yb].value
	j:=pclstack[za].value

	mx:=makeregopnd(yb)
	rx:=makeregopnd(za)

	loadtoreg(rx,ax,p.mode)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

!	mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
	genmc(m_mov, mx, mgenint(mask))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_andx, rx, mx)
	genmc(m_orx, rx, dx)

	storefromreg(ax,rx,size)

	delopnd()			!j
	delopnd()			!i
	delopnd()			!A
	if p.opcode=kpopdotslice then
		delopnd()		!x
	fi

end

proc px_storedotslice(pcl p)=
	px_popdotslice(p)
end

proc px_popstack(pcl p)=
	delopnd()
end

proc px_eval(pcl p)=
	if pclstack[1].wide='L' then
		genopnd_ld(ya)
		genopnd_ld(xb)
		delopnd()
		delopnd()
	else
		genopnd_ld(xa)
		delopnd()
	fi
end

proc px_callproc(pcl p)=
	int nslots, nargs:=callargs[ncalldepth]

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenmemaddr(p.def))

	to nargs do
		poparg()
	od
	popslots(nslots)
end

proc px_callprocptr(pcl p)=
	int nslots, nargs:=callargs[ncalldepth]
	nslots:=do_pushparams(p,1)

	genmc(m_call, genopnd_ld(xa))

	delopnd()			!the ptr

	to nargs do
		poparg()
	od
	popslots(nslots)
end

proc px_retproc(pcl p)=
	int offset

	offset:=dsaveoffset
	for i to ndsaveregs do
		genmc(m_mov, mgenreg(dsaveregs[i]),
			mgenindex(areg:rframe, size:8, offset:offset))
		offset+:=8
	od

	offset:=xsaveoffset
	for i to nxsaveregs do
		genmc(m_movq, mgenxreg(xsaveregs[i]),
			mgenindex(areg:rframe, size:8, offset:offset))
		offset+:=8
	od

!	if framebytes then
!		genmc(m_add, dstackopnd, mgenint(framebytes))
!	fi
!	if framebytes or parambytes then
!		genmc(m_pop, dframeopnd)
!	fi

	if needstackframe then
		if framebytes then
			genmc(m_add, dstackopnd, mgenint(framebytes))
		fi
		genmc(m_pop, dframeopnd)
	elsif needshadow32 then
		popstack(needshadow32)
	fi

	if needshadow48 then
		popstack(48)
	fi
	for i:=ndsavepush downto 1 do
		genmc(m_pop, mgenreg(dsaveregs[i]))
	od

!	if ndsavepush.odd then
!		genmc(m_pop, mgenreg(dsaveregs[1]))
!!		popstack(8)
!	fi
!
	genmc(m_ret)
end

proc px_callfn(pcl p)=
	px_callproc(p)

	dogetretvalue(p)
end

proc px_callfnptr(pcl p)=
	px_callprocptr(p)

	dogetretvalue(p)
end

proc px_retfn(pcl p)=
	px_retproc(p)
end

proc px_jump(pcl p)=
	genmc(m_jmp, mgenlabel(p.labelno))
end

proc px_jumpptr(pcl p)=
	unimpl(p)
end

proc px_jumpeq(pcl p)=
	dojumpcc(p)
end

proc px_jumpne(pcl p)=
	dojumpcc(p)
end

proc px_jumplt(pcl p)=
	dojumpcc(p)
end

proc px_jumple(pcl p)=
	dojumpcc(p)
end

proc px_jumpge(pcl p)=
	dojumpcc(p)
end

proc px_jumpgt(pcl p)=
	dojumpcc(p)
end

proc px_jumptrue(pcl p)=
	dojumptruefalse(p,nz_cond)
end

proc px_jumpfalse(pcl p)=
	dojumptruefalse(p,z_cond)
end

proc px_jumpinrange(pcl p)=
	mcloperand ax,bx,cx,lx,nolx
	int nolab

	ax:=genopnd_ld(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	lx:=mgenlabel(p.labelno)

	genmc(m_cmp, ax,bx)

	nolx:=mgenlabel(nolab:=mcreatefwdlabel())
	genmc_cond(m_jmpcc, (psigned[p.mode]|lt_cond|ltu_cond),nolx)
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, (psigned[p.mode]|le_cond|leu_cond),lx)
	mdefinefwdlabel(nolab)

	delopnd()
	delopnd()
	delopnd()
end

proc px_jumpnotinrange(pcl p)=
	mcloperand ax,bx,cx,lx

	ax:=genopnd_ld(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	lx:=mgenlabel(p.labelno)

	genmc(m_cmp, ax,bx)

	genmc_cond(m_jmpcc, (psigned[p.mode]|lt_cond|ltu_cond),lx)
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, (psigned[p.mode]|gt_cond|gtu_cond),lx)

	delopnd()
	delopnd()
	delopnd()
end

proc px_setjumpeq(pcl p)=
	genmc(m_cmp,genopnd_ld(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
end

proc px_setjumpeqx(pcl p)=
	genmc(m_cmp,genopnd_ld(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc px_setjumpne(pcl p)=
	genmc(m_cmp,genopnd(xb),genopnd(ya))

	genmc_cond(m_jmpcc, ne_cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc px_seteq(pcl p)=
	dosetcc(p)
end

proc px_setne(pcl p)=
	dosetcc(p)
end

proc px_setlt(pcl p)=
	dosetcc(p)
end

proc px_setle(pcl p)=
	dosetcc(p)
end

proc px_setge(pcl p)=
	dosetcc(p)
end

proc px_setgt(pcl p)=
	dosetcc(p)
end

proc px_casejumpeq(pcl p)=
	genmc(m_cmp, genopnd_ld(xb), genopnd(ya))
	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
end

proc px_selecteq(pcl p)=
	doselectcc(p)
end

proc px_selectne(pcl p)=
	doselectcc(p)
end

proc px_selectlt(pcl p)=
	doselectcc(p)
end

proc px_selectle(pcl p)=
	doselectcc(p)
end

proc px_selectge(pcl p)=
	doselectcc(p)
end

proc px_selectgt(pcl p)=
	doselectcc(p)
end

proc px_selecttrue(pcl p)=
	mcloperand ax,bx,cx,dx

	if pcat[p.mode]<>d64cat then merrort("selecttrue",p.mode) fi

	ax:=genopnd_ld(xa)
!	genmc(m_andx, ax,ax)
	genmc(m_test, ax,ax)

	noxorclear:=1
	dx:=genopnd_ld(yb)
	cx:=genopnd_ld(xc)
!	dx:=genopnd_ld(xc)
!	cx:=genopnd_ld(yb)
	noxorclear:=0
	genmc_cond(m_cmovcc, z_cond, cx,dx)

	delopnd()
	delopnd()
end

proc px_to(pcl p)=
	pcl q
	mcloperand ax

	q:=currpcl:=p+1

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.labelno))
end

proc px_forup(pcl p)=
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown(pcl p)=
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_swap(pcl p)=
	mcloperand ax,bx

	mcloperand px:=genopnd_ind(xb,p.size)
	mcloperand qx:=genopnd_ind(ya,p.size)

	ax:=mgenreg(getnextreg(),p.size)
	bx:=mgenreg(getnextreg(),p.size)

	case pcat[p.mode]
	when d64cat,shortcat then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)

	else
		merrort("swap",p.mode)
	esac

	freereg(ax.reg)
	freereg(bx.reg)

	delopnd()
	delopnd()
end

proc px_makeslice(pcl p)=
	setwideopnd()
end

proc px_switch(pcl p)=
	int minlab, maxlab, jumplab, elselab
	mcloperand ax

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=genopnd_ld(xa)
	if minlab<>0 then
		genmc(m_sub,ax,mgenint(minlab))
	fi
	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))
	genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))

	delopnd()

	setsegment('I')
end

proc px_switchlabel(pcl p)=
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endswitch(pcl p)=
	setsegment('C')
end

proc px_clear(pcl p)=
	mcloperand ax

	ax:=genopnd_ind()
	clearblock(ax,p.size)
	delopnd()
end

proc px_csegment(pcl p)=
	unimpl(p)
end

proc px_isegment(pcl p)=
	unimpl(p)
end

proc px_zsegment(pcl p)=
	unimpl(p)
end

proc px_rosegment(pcl p)=
	unimpl(p)
end

proc px_data(pcl p)=
	mcloperand ax
	int opc

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when real_opnd,real32_opnd then
		ax:=mgenrealimm(p.xvalue,p.size)
	when string_opnd then
		 ax:=mgenlabel(getstringindex(p.svalue))

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.size
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
		merror("DATA/not 1248")
	esac

	genmc(opc,ax)
end

proc px_db(pcl p)=
	unimpl(p)
end

proc px_dw(pcl p)=
	unimpl(p)
end

proc px_dd(pcl p)=
	unimpl(p)
end

proc px_dq(pcl p)=
	unimpl(p)
end

proc px_dstring(pcl p)=
	unimpl(p)
end

proc px_dstringz(pcl p)=
	unimpl(p)
end

proc px_reserve(pcl p)=
	unimpl(p)
end

proc px_assem(pcl p)=
!CPL "PCL/GENASM",=HOSTASMHANDLER
	if hostasmhandler then
		hostasmhandler(p.asmcode)
	else
		unimpl(p)
	fi
	if p.mode then
		dogetretvalue(p)
	fi
end

proc px_add(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if pclstack[1].fmt=imm_d64 and pclstack[1].value=1 then
			genmc(m_inc, ax)
		else
			bx:=genopnd(ya)
			genmc(m_add,ax,bx)
		fi
	when x64cat then
		dobin_float(m_addsd)
	when x32cat then
		dobin_float(m_addss)
	else
		merrort("add:",p.mode)
	esac
	delopnd()
end

proc px_sub(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if pclstack[1].fmt=imm_d64 and pclstack[1].value=1 then
			genmc(m_dec, ax)
		else
			bx:=genopnd(ya)
			genmc(m_sub,ax,bx)
		fi
	when x64cat then
		dobin_float(m_subsd)
	when x32cat then
		dobin_float(m_subss)
	else
		merrort("sub:",p.mode)
	esac
	delopnd()
end

proc px_mul(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if  pclstack[1].fmt=imm_d64 then
			mulimm(ax,pclstack[1].value)
			delopnd()
			return
		fi

		bx:=genopnd(ya)
		genmc(m_imul2,ax,bx)

	when x64cat then
		dobin_float(m_mulsd)
	when x32cat then
		dobin_float(m_mulss)
	else
		merrort("mul:",p.mode)
	esac
	delopnd()
end

proc px_div(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when x64cat then
		dobin_float(m_divsd)
	when x32cat then
		dobin_float(m_divss)
!	when widecat then
	else
		merrort("div:",p.mode)
	esac
	delopnd()
end

proc px_idiv(pcl p)=
	case p.mode
	when tpi64 then
		do_divrem(p, issigned:1, isdiv:1)
	when tpu64 then
		do_divrem(p, issigned:0, isdiv:1)
	else
		merrort("idiv:",p.mode)
	esac
end

proc px_irem(pcl p)=
	case p.mode
	when tpi64 then
		do_divrem(p, issigned:1, isdiv:0)
	when tpu64 then
		do_divrem(p, issigned:0, isdiv:0)
	else
		merrort("irem:",p.mode)
	esac
end

proc px_iand(pcl p)=
	dobitwise(p, m_andx)
end

proc px_ior(pcl p)=
	dobitwise(p, m_orx)
end

proc px_ixor(pcl p)=
	dobitwise(p, m_xorx)
end

proc px_shl(pcl p)=
	case pcat[p.mode]
	when d64cat then
		do_shift(p,m_shl)
	else
		merrort("shl:",p.mode)
	esac
end

proc px_shr(pcl p)=
	case p.mode
	when tpi64 then
		do_shift(p,m_sar)
	when tpu64 then
		do_shift(p,m_shr)
	else
		merrort("shr:",p.mode)
	esac
end

proc px_in(pcl p)=
	unimpl(p)
end

proc px_notin(pcl p)=
	unimpl(p)
end

proc px_min(pcl p)=
	case p.mode
	when tpi64 then
		domax_int(gt_cond)
	when tpu64 then
		domax_int(gtu_cond)
	when tpr64 then
		domax_float(m_minsd)
	when tpr32 then
		domax_float(m_minss)
	else
		merrort("min:",p.mode)
	esac
end

proc px_max(pcl p)=
	case p.mode
	when tpi64 then
		domax_int(lt_cond)
	when tpu64 then
		domax_int(ltu_cond)
	when tpr64 then
		domax_float(m_maxsd)
	when tpr32 then
		domax_float(m_maxss)
	else
		merrort("max:",p.mode)
	esac
end

proc px_eq(pcl p)=
	unimpl(p)
end

proc px_ne(pcl p)=
	unimpl(p)
end

proc px_lt(pcl p)=
	unimpl(p)
end

proc px_le(pcl p)=
	unimpl(p)
end

proc px_ge(pcl p)=
	unimpl(p)
end

proc px_gt(pcl p)=
	unimpl(p)
end

proc px_same(pcl p)=
	unimpl(p)
end

proc px_andl(pcl p)=
	unimpl(p)
end

proc px_orl(pcl p)=
	unimpl(p)
end

proc px_addrefoff(pcl p)=
	mcloperand ax,cx

	cx:=do_addrmode(p)

	if pclstack[2].loc<>reg_loc then
		pclstack[2].fmt:=reg_d64			!reg not needed to load addr, but
		pclstack[2].loc:=reg_loc			!need to prepare it for result
		pclstack[2].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=genopnd(xb)

	genmc(m_lea, ax, cx)
	delopnd()
end

proc px_subref(pcl p)=
	mcloperand ax,bx
	int n

	ax:=genopnd_ld(xb)
!	bx:=genopnd_ld(ya)
	bx:=genopnd(ya)
	genmc(m_sub,ax,bx)

	if p.scale>1 then
		n:=ispoweroftwo(p.scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
			MERROR("SUB/REF NOT POWER OF TWO")
		fi
	fi

	delopnd()
end

proc px_subrefoff(pcl p)=
	int scale, extra, offset
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=genopnd_ld(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(m_sub, ax, mgenint(pclstack[1].value*scale+extra))
	else
		bx:=genopnd_ld(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
MERROR("SUBREF/EXTRA")
!			genmc(m_add, ax, mgenint(extra))
		fi
	fi
	delopnd()
end

proc px_neg(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_neg,ax)

	when x64cat then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd,genopnd_ld(xa),mgenlabelmem(labneg64))
	when x32cat then
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps,genopnd_ld(xa),mgenlabelmem(labneg32))

	when widecat then
		mcloperand axlow, axhigh, bxlow, bxhigh
		axlow:=genopnd_ld()
		axhigh:=genopnd_ld(2)

		bxhigh:=genopnd_d64()
		bxlow:=genopnd_d64()

		genmc(m_xorx,bxlow,bxlow)
		genmc(m_xorx,bxhigh,bxhigh)
		genmc(m_sub,bxlow,axlow)
		genmc(m_sbb,bxhigh,axhigh)

		swapopnds(1,3)
		swapopnds(2,4)

		delopnd()
		delopnd()
	else
		merrort("neg",p.mode)
	esac
end

proc px_abs(pcl p)=
	mcloperand ax,lx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_label, lx)

	when x64cat then
		if not lababs64 then lababs64:=mcreatefwdlabel() fi
		genmc(m_andpd,genopnd_ld(xa),mgenlabelmem(lababs64))
	when x32cat then
		if not lababs32 then lababs32:=mcreatefwdlabel() fi
		genmc(m_andps,genopnd_ld(xa),mgenlabelmem(lababs32))
	else
		merrort("abs",p.mode)
	esac
end

proc px_inot(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_notx,ax)

	else
		merrort("inot",p.mode)
	esac
end

proc px_notl(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_xorx,ax,mgenint(1))

	else
		merrort("notl",p.mode)
	esac
end

proc px_istruel(pcl p)=
	mcloperand ax, bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)

	else
		merrort("istruel",p.mode)
	esac
end

proc px_sqr(pcl p)=
	mcloperand ax

	ax:=genopnd_ld(xa)

	case pcat[p.mode]
	when d64cat then
		genmc(m_imul2,ax,ax)
!
	when x64cat then
		genmc(m_mulsd,ax,ax)
	when x32cat then
		genmc(m_mulss,ax,ax)
	else
		merrort("sqr",p.mode)
	esac
end

proc px_sqrt(pcl p)=
	mcloperand ax

	ax:=genopnd_ld(xa)

	case pcat[p.mode]
	when x64cat then
		genmc(m_sqrtsd,ax,ax)
	when x32cat then
		genmc(m_sqrtss,ax,ax)
	else
		merrort("sqrt",p.mode)
	esac
end

proc px_sin(pcl p)=
	domaths(p,"sin*")
end

proc px_cos(pcl p)=
	domaths(p,"cos*")
end

proc px_tan(pcl p)=
	domaths(p,"tan*")
end

proc px_asin(pcl p)=
	domaths(p,"asin*")
end

proc px_acos(pcl p)=
	domaths(p,"acos*")
end

proc px_atan(pcl p)=
	domaths(p,"atan*")
end

proc px_ln(pcl p)=
	domaths(p,"log*")
end

proc px_log(pcl p)=
	domaths(p,"log10*")
end

proc px_exp(pcl p)=
	domaths(p,"exp*")
end

proc px_round(pcl p)=
	domaths(p,"round*")
end

proc px_floor(pcl p)=
	domaths(p,"floor*")
end

proc px_ceil(pcl p)=
	domaths(p,"ceil*")
end

proc px_fract(pcl p)=
	unimpl(p)
end

proc px_sign(pcl p)=
	unimpl(p)
end

proc px_atan2(pcl p)=
	unimpl(p)
end

proc px_power(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then

		swapopnds(1,2)
		do_syscall(rts_power_i64,2,d64cat)

	when x64cat then
!CPL "HERE"
		swapopnds(1,2)
		domaths2(p,"pow*")
!		dobin_float(m_mulsd)
!	when x32cat then
!		dobin_float(m_mulss)
!	when widecat then
!!		do_syscall(rts_mul_i128,4,widecat)
!!		delopnd()
!!		delopnd()
		return
	else
		merrort("power:",p.mode)
	esac
!	delopnd()
end

proc px_fmod(pcl p)=
	unimpl(p)
end

proc px_incr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincr(p,m_inc, m_add)
	else
		merrort("incr", p.mode)
	esac
end

proc px_decr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincr(p,m_dec, m_sub)
	else
		merrort("decr", p.mode)
	esac
end

proc px_incrload(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincrload(p,m_inc, m_add)
	else
		merrort("incrload", p.mode)
	esac
end

proc px_decrload(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincrload(p,m_dec, m_sub)
	else
		merrort("decrload", p.mode)
	esac
end

proc px_loadincr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doloadincr(p,m_inc, m_add)
	else
		merrort("loadincr", p.mode)
	esac
end

proc px_loaddecr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doloadincr(p,m_dec, m_sub)
	else
		merrort("loaddecr", p.mode)
	esac
end

proc px_addto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_add)
	when x64cat then
		dobinto_float(p,m_addsd)
	when x32cat then
		dobinto_float32(p,m_addss)
!	when shortcat then

!	when widecat then
	else
		merrort("addto:",p.mode)
	esac
end

proc px_subto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_sub)
	when x64cat then
		dobinto_float(p,m_subsd)
	when x32cat then
		dobinto_float32(p,m_subss)
!	when widecat then
	else
		merrort("subto:",p.mode)
	esac
end

proc px_multo(pcl p)=
	mcloperand ax,bx,cx

	case pcat[p.mode]
	when d64cat then
		addreg_d64()
		ax:=genopnd_ind(xc)
		bx:=genopnd(yb)
		cx:=genopnd(za)

		genmc(m_mov, cx,ax)

		if  pclstack[2].fmt=imm_d64 then
			mulimm(cx, pclstack[2].value)
		else
			genmc(m_imul2, cx,bx)
		fi
		genmc(m_mov, ax,cx)

		delopnd()
		delopnd()
		delopnd()
	when x64cat then
		dobinto_float(p,m_mulsd)
	when x32cat then
		dobinto_float32(p,m_mulss)
!	when widecat then
	else
		merrort("multo:",p.mode)
	esac
end

proc px_divto(pcl p)=
	mcloperand ax,bx,cx

	case pcat[p.mode]
	when x64cat then
		dobinto_float(p,m_divsd)
	when x32cat then
		dobinto_float32(p,m_divss)
!	when widecat then
	else
		merrort("divto:",p.mode)
	esac
end

proc px_idivto(pcl p)=
	unimpl(p)
end

proc px_iremto(pcl p)=
	unimpl(p)
end

proc px_iandto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_andx)
!	when widecat then
	else
		merrort("iandto:",p.mode)
	esac
end

proc px_iorto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_orx)
!	when widecat then
	else
		merrort("iorto:",p.mode)
	esac
end

proc px_ixorto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_xorx)
!	when widecat then
	else
		merrort("ixorto:",p.mode)
	esac
end

proc px_shlto(pcl p)=
	case p.mode
	when tpi64,tpu64 then
		do_shiftnto(p,m_shl)
	else
		merrort("shlto:",p.mode)
	esac
end

proc px_shrto(pcl p)=
	case p.mode
	when tpi64 then
		do_shiftnto(p,m_sar)
	when tpu64 then
		do_shiftnto(p,m_shr)
	else
		merrort("shrto:",p.mode)
	esac
end

proc px_minto(pcl p)=
	case p.mode
	when tpi64 then
		domaxto_int(le_cond)
	when tpu64 then
		domaxto_int(leu_cond)
	when tpr64 then
		domaxto_r64(leu_cond)
	when tpr32 then
		domaxto_r32(leu_cond)
!	when tpi128 then
	else
		merrort("minto:",p.mode)
	esac
end

proc px_maxto(pcl p)=
	case p.mode
	when tpi64 then
		domaxto_int(ge_cond)
	when tpu64 then
		domaxto_int(geu_cond)
	when tpr64 then
		domaxto_r64(geu_cond)
	when tpr32 then
		domaxto_r32(geu_cond)
!	when tpi128 then
	else
		merrort("maxto:",p.mode)
	esac
end

proc px_andlto(pcl p)=
	unimpl(p)
end

proc px_orlto(pcl p)=
	unimpl(p)
end

proc px_addrefoffto(pcl p)=
	int scale, extra,offset
!
	scale:=p.scale
	extra:=p.extra
	offset:=pclstack[1].value*scale+extra	!in case imm_d64

	mcloperand ax,bx,rx
	int reg,size

	if ismemaddr(xb) then
		ax:=mgenmem(pclstack[2].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)

		genmc(m_mov, rx, ax)

		if pclstack[1].fmt=imm_d64 then
			genmc(m_add,rx,mgenint(offset))
		else
			bx:=genopnd_ld(ya)
			mulimm(bx,scale)
			genmc(m_add,rx,bx)
		fi

		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=genopnd_ind(xb)
		if pclstack[1].fmt=imm_d64 then
			genmc(m_add,ax,mgenint(offset))
		else
			bx:=genopnd_ld(ya)
			mulimm(bx,scale)
			genmc(m_add,ax,bx)
		fi
	fi
	delopnd()
	delopnd()
end

proc px_subrefoffto(pcl p)=
	int scale, extra
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=genopnd_ind(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(m_sub, ax, mgenint(pclstack[1].value*scale+extra))
	else
		bx:=genopnd_ld(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
MERROR("SUBTOREF/EXTRA")
!			genmc(m_sub, ax, mgenint(extra))
		fi
	fi

	delopnd()
	delopnd()
end

proc px_negto(pcl p)=
	unimpl(p)
end

proc px_absto(pcl p)=
	unimpl(p)
end

proc px_inotto(pcl p)=
	unimpl(p)
end

proc px_notlto(pcl p)=
	unimpl(p)
end

proc px_istruelto(pcl p)=
	unimpl(p)
end

proc px_typepun(pcl p)=
	mcloperand ax,bx,cx

	bx:=genopnd_ld(xa)

	case pcat[p.mode]
	when d64cat then
		case pclstack[1].fmt
		when xreg_x64 then
			addreg_d64()
			ax:=genopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(1,2)
			delopnd()
		when reg_d64 then
		else
			goto error
		esac

	when x64cat then
		case pclstack[1].fmt
		when reg_d64 then
			addreg_x64()
			ax:=genopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(1,2)
			delopnd()
		else
			goto error
		esac
	when shortcat then
		case pclstack[1].fmt
		when xreg_x32 then
			addreg_d64()
			ax:=genopnd(xa)
			cx:=changeopndsize(ax,4)
            genmc(m_movd, cx,bx)
			swapopnds(1,2)
			delopnd()

			genmc((psigned[p.mode]|m_movsx|m_movzx),ax,cx)
		when reg_d64 then

		else
			goto error
		esac

	else
error::
CPL =PSTDNAMES[P.MODE]
CPL =FMTNAMES[PCLSTACK[1].FMT]
CPL "TYPEPUN"
!MGENCOMMENT("TYPEPUN")
		MERROR("TYPEPUN")
	esac
end

proc px_softconv(pcl p)=
	unimpl(p)
end

proc px_widen(pcl p)=
	unimpl(p)
end

proc px_narrow(pcl p)=
	unimpl(p)
end

proc px_float(pcl p)=
	mcloperand ax,fx
	int lab,lab2

	ax:=genopnd_ld(xa)

	case p.oldmode
	when tpi64 then
		addreg_x64()
		fx:=genopnd(xa)
		genmc(m_cvtsi2sd, fx, ax)
		swapopnds(1,2)
	when tpu64 then
		addreg_x64()
		fx:=genopnd(xa)

		lab:=mcreatefwdlabel()
		lab2:=mcreatefwdlabel()
		genmc(m_cmp, ax, mgenint(0))
		genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_jmp, mgenlabel(lab2))

		mdefinefwdlabel(lab)
		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		fi
		genmc(m_andx,ax, mgenlabelmem(labmask63))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_addsd, fx, mgenlabelmem(laboffset64))
		mdefinefwdlabel(lab2)
		swapopnds(1,2)

	else
		merrort("float",p.mode)
	esac

	delopnd()
end

proc px_fix(pcl p)=
	mcloperand fx,ax
	int newmode:=p.mode, oldmode:=p.oldmode

	case pcat[newmode]
	when d64cat then
		fx:=genopnd_ld(xa)
		addreg_d64()
		ax:=genopnd(xa)
		genmc((oldmode=tpr64|m_cvttsd2si|m_cvttss2si), ax, fx)
		swapopnds(1,2)
		delopnd()

	else
		merrort("fix->",newmode)
	esac
end

proc px_truncate(pcl p)=
	mcloperand ax
	int mask

	case psize[p.truncmode]
	when 1 then mask:=255
	when 2 then mask:=65535
	when 4 then mask:=0xFFFF'FFFF
	esac

	ax:=genopnd_ld(xa)
	genmc(m_andx, ax, mgenint(mask))

	genmc((psigned[p.truncmode]|m_movsx|m_movzx), ax, changeopndsize(ax,psize[p.truncmode]))
end

proc px_fwiden(pcl p)=
	mcloperand fx
	fx:=genopnd_ld()
	genmc(m_cvtss2sd, fx,fx)
	pclstack[1].fmt:=xreg_x64
end

proc px_fnarrow(pcl p)=
	mcloperand ax:=genopnd_ld(xa)
	genmc(m_cvtsd2ss, ax,ax)
	pclstack[1].fmt:=xreg_x32
end

proc px_softtruncw(pcl p)=
	mcloperand ax,bx,bx2,lx

	ax:=genopnd_ld()
	swapopnds(1,2)
	delopnd()
	unsetwideopnd()
end

proc px_widenw(pcl p)=
	mcloperand ax,bx,bx2,lx

	ax:=genopnd_ld()
	bx:=genopnd_d64()
	bx2:=changeopndsize(bx,4)

	swapopnds(1,2)

	if psigned[p.mode] then
		genmc(m_xorx,bx2,bx2)
		genmc(m_cmp,ax,mgenint(0))
		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_notx, bx)
		genmc(m_label,lx)
	else
		genmc(m_xorx,bx2,bx2)
	fi

	setwideopnd()
end

proc px_len(pcl p)=
	delopnd()
	unsetwideopnd()
end

proc px_lwb(pcl p)=
	unimpl(p)
end

proc px_upb(pcl p)=
	unimpl(p)
end

proc px_bounds(pcl p)=
	unimpl(p)
end

proc px_lenstr(pcl p)=
	unimpl(p)
end

proc px_bitwidth(pcl p)=
	unimpl(p)
end

proc px_bytesize(pcl p)=
	unimpl(p)
end

proc px_bytes(pcl p)=
	unimpl(p)
end

proc px_minvalue(pcl p)=
	unimpl(p)
end

proc px_maxvalue(pcl p)=
	unimpl(p)
end

proc px_typestr(pcl p)=
	unimpl(p)
end

proc px_error(pcl p)=
	unimpl(p)
end

proc px_arraytoslice(pcl p)=
	unimpl(p)
end

proc px_ichartoslice(pcl p)=
	unimpl(p)
end

proc px_softtruncshort(pcl p)=
	unimpl(p)
end

proc px_charaxtoichar(pcl p)=
	unimpl(p)
end

proc px_sliceptr(pcl p)=
	swapopnds(1,2)
	delopnd()
	unsetwideopnd()
end

proc px_startmult(pcl p)=
	pushallopnds()
end

proc px_resetmult(pcl p)=
	if pclstack[1].float then
		MERROR("RESETMULT/XREG")
	else
		movetoreg(r0)
	fi

	if p.opcode=kresetmult then
		delopnd()
	fi
end

proc px_endmult(pcl p)=
	px_resetmult(p)
end

proc px_setret(pcl p)=
	do_setret(r0,r0)

	regset[r0]:=0
	xregset[r0]:=0
end

proc px_setretmult(pcl p)=
	int k,wide

	k:=0

	for i:=1 to p.nret do
		++k
		wide:=pclstack[1].wide
		do_setret(multregs[k],multxregs[k])
!		if wide then
!			++k
!			do_setret(multregs[k],multxregs[k])
!		fi
	od

	for i:=1 to k do
		regset[multregs[i]]:=xregset[multxregs[i]]:=0
	od
end

proc px_setargs(pcl p)=
	int nslots,shadow,align,nargs,opcode,nvars

	if p then
		nargs:=p.nargs
		nvars:=p.nvariadics
		opcode:=p.opcode
	else
		nargs:=sa_nargs		!set via global
		nvars:=0
		opcode:=0
	fi

	nslots:=0			!total slots to be recovered after a call
	shadow:=0			!whether 4-slot shadow space to be created
	align:=0			!whether stack alignment fix needed

	case opcode
	when ksetargs then
		saveallopnds()		!get latest mstackdepth
	else
		saveallopnds(nargs+1)
	esac

	if nargs<=4 then					!no pushed args needed
		if mstackdepth=0 then
		else
			shadow:=1
			align:=mstackdepth.odd
			nslots:=4
		fi
	else								!some pushed params
		shadow:=1
		nslots:=nargs
		align:=(mstackdepth+nslots).odd
	fi

	nslots+:=align
	if align then
		if opcode=ksetargs then		!normal
			pushslots(1)
			align:=0
		fi								!else leave to be stored in callalign
	fi

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi
	++ncalldepth
	IF NCALLDEPTH<1 THEN
		ABORTPROGRAM("CALLDEPTH?")
	FI

	callargs[ncalldepth]:=nargs
	callnvars[ncalldepth]:=nvars
	callslots[ncalldepth]:=nslots
	callshadow[ncalldepth]:=shadow
	callalign[ncalldepth]:=align
end

proc px_duplstack(pcl p)=
	mcloperand ax

	if pcat[p.mode]>=widecat then
		merrort("duplstack",p.mode)
	fi

	duploperand()
end

proc px_swapstack(pcl p)=
	swapopnds(1,p.x+1)		!the x-attr is 0-based; need 1-based
end

proc px_getnprocs(pcl p)=
	dosetfntable()
	addlabel(lab_funcnprocs)
end

proc px_getprocname(pcl p)=
	mcloperand ax

	dosetfntable()
	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcnametable))
	else
		merrort("gpn",p.mode)
	esac
end

proc px_getprocaddr(pcl p)=
	mcloperand ax

	dosetfntable()
	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcaddrtable))
	else
		merrort("gpa",p.mode)
	esac
end

proc px_last(pcl p)=
	unimpl(p)
end

proc dobin_float(int opc)=
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	genmc(opc,ax,bx)
end

function do_pushparams(pcl p, int isptr)int=
!isptr=1 when top pcl operand is the function pointer for indirect calls

	int pushedslots, nparams, nvars, nargs

	if p then
		nargs:=callargs[ncalldepth]
		nvars:=callnvars[ncalldepth]
	else
		nargs:=sa_nargs; isptr:=0
		nvars:=0
	fi

!CPL "DOPUSHPARAMS", =NCALLDEPTH, =NARGS, =CALLSLOTS[NCALLDEPTH],=CALLARGS[NCALLDEPTH]


	if nargs>inf_maxargs and nargs<=4 then inf_maxargs:=nargs fi
	nparams:=nargs

	if nparams>4 then
		pushallopnds(isptr+4+1)
	fi

!low params are 'pushed' after high params
!this allows r13 to be used as a scratch register

	do_pushlowparams(nparams,nvars,isptr)

	if callshadow[ncalldepth] then
		pushslots(callalign[ncalldepth]+4)
	fi

	pushedslots:=callslots[ncalldepth]
	--ncalldepth
	return pushedslots
end

proc do_pushlowparams(int nparams, nvariadics=0, isptr=0)=
!nparams=0 to 4 (if more than 4 in total, then nparams must be 4 here)
!load params to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register

	int ireg, xreg, j

	if nparams=0 then return fi
	nparams min:=4

	for i to nparams do
		j:=i+isptr
		ireg:=r10+i-1
		xreg:=xr0+i-1

		if pclstack[j].float then
			unless nvariadics and i>=nvariadics then ireg:=0 end
		else
			xreg:=0
		fi

		if ireg then loadparam(j,ireg) fi
		if xreg then loadxparam(j,xreg) fi
	od
end

proc do_for(pcl p, int incop, addop, cond)=
	pcl q,r
	mcloperand ax,bx,cx,dx,mx
	int reg

	q:=p+1
	r:=currpcl:=q+1

	mx:=mgenmem(q.def)

	if q.def.reg then
		if p.stepx=1 then
			genmc(incop, mx)
		else
			genmc(addop, mx, mgenint(p.stepx))
		fi
		ax:=mx
	else
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax,mx)
		if p.stepx=1 then
			genmc(incop, ax)
		else
			genmc(addop, ax, mgenint(p.stepx))
		fi
		genmc(m_mov, mx, ax)
	fi

	if r.opndtype=int_opnd then
		bx:=mgenint(r.value)
	else
		bx:=mgenmem(r.def)
	fi

	genmc(m_cmp, ax, bx)
	freereg(ax.reg)
!

	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
end

proc dojumpcc(pcl p)=
	int m:=p.mode, cond
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	case pcat[m]
	when d64cat then
		if psigned[m] then
			cond:=scondcodes[p.opcode-kjumpeq]
		else
			cond:=ucondcodes[p.opcode-kjumpeq]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[p.opcode-kjumpeq]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[p.opcode-kjumpeq]
		genmc(m_comisd,ax,bx)
	else
		merrort("jumpcc:",p.mode)
	esac

	genmc_cond(m_jmpcc,cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc dosetcc(pcl p)=
	int m:=p.mode, cond
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	case pcat[m]
	when d64cat then
		if psigned[m] then
			cond:=scondcodes[p.opcode-kseteq]
		else
			cond:=ucondcodes[p.opcode-kseteq]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[p.opcode-kseteq]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[p.opcode-kseteq]
		genmc(m_comisd,ax,bx)
	else
		merrort("setcc:",p.mode)
	esac

	genmc_cond(m_setcc,cond, bx:=changeopndsize(ax,1))
	genmc(m_movzx, changeopndsize(ax,4), bx)
	delopnd()
end

proc do_setretfloat(int destreg)=
	int currreg
	mcloperand ax,rx

	rx:=mgenxreg(destreg)

	ax:=genopnd_ld(1)
	currreg:=ax.reg

	case pclstack[1].loc
	when xreg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				merror("setretfloat/dest in use")
			else
				genmc(m_movq, rx, ax)
				xregset[destreg]:=1
			fi
		fi
	else
		merror("setretf?")
	esac
	delopnd()		!assume next is a jump to return point

end

proc do_setret(int destreg,destxreg)=
!make sure top-of-stack is in nth register for multi-value return
!for normal returns, n will be 1
!nth value must be in d0/d1/d2, or x0/x1/x2
!Value might not be on the stack
!prior registers not available. Current value will not be in previous
!regs as they will have been moved out

	int currreg
	mcloperand ax,rx

	if pclstack[1].float then
		do_setretfloat(destxreg)
		return
	fi

	rx:=mgenreg(destreg)

	ax:=genopnd_ld(1)
	currreg:=ax.reg

	case pclstack[1].loc
	when reg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				swapopndregs(destreg)
				genmc(m_xchg, rx, ax)
			else
				genmc(m_mov, rx, ax)
			fi
		fi
	else
CPL =LOCNAMES[PCLSTACK[1].LOC]
!CPL =PROCDEF.NAME
		merror("setret?")
	esac
	delopnd()						!assume next is a jump to return point
	regset[destreg]:=1
	mccodex.regend[destreg]:=0			!d0 will not be freed
end

proc dogetretvalue(pcl p)=
	int reg,xreg,i,n
	[10]int cats

	if (p+1).opcode=ktype then
		n:=0
		while (++p).opcode=ktype do
			cats[++n]:=pcat[p.mode]
		od
		currpcl:=p-1

		for i:=n downto 1 do 
			case cats[i]
			when shortcat, widecat then
				merror("Short/wide mulret type")
			esac

			dogetretvalue_n(multregs[i],multxregs[i], cats[i])
		od

	else
!CPL "HERE"
		dogetretvalue_n(r0,r0,pcat[p.mode])
		if pcat[p.mode]=shortcat then
			genmc((psigned[p.mode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.size))
		fi
	fi
end

proc dogetretvalue_n(int reg,xreg,cat)=

	case cat
	when d64cat,shortcat then
		addreg0(reg)
	when x64cat then
		addxreg0(xreg,xreg_x64)
	when x32cat then
		addxreg0(xreg,xreg_x32)
	when widecat then
		addwidereg0(reg)
!	when shortcat then
!		genmc((ttisint[p.mode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.size))
	else
CPL PSTDNAMES[CAT]
		merror("getretval/n?")
	esac
end

proc do_shift(pcl p, int opc)=
	mcloperand ax
	ax:=genopnd_ld(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(opc, ax, mgenint(pclstack[1].value))
	else
		if inf_r10used then merror("shift:cl in use") fi
		loadparam(reg:r10)
		genmc(opc,ax, mgenreg(r10,1))
	fi
	delopnd()
end

proc mulimm(mcloperand ax, int n)=
!multiply operand in ax (a simple reg) by constant n
!will try efficient method if possible, otherwise use normal multiply 
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
		genmc(m_shl, ax, mgenint(shifts))
	fi

	case m
	when 1 then
		return
	when 3, 5, 9 then
		genmc(m_lea, ax, mgenindex(areg: ax.reg, ireg:ax.reg, scale:m-1))
	else						!mul needed anyway; forget the shift
		if shifts then
			mccodex.opcode:=m_imul2
			mccodex.b:=mgenint(n)
		else
			genmc(m_imul2, ax, mgenint(n))
		fi
	esac

end

proc dojumptruefalse(pcl p, int cond)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
	else
		merrort("jumptrue/false",p.mode)
	esac
	delopnd()
end

proc dobitwise(pcl p, int opc)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		bx:=genopnd(ya)
		genmc(opc,ax,bx)
	else
		merrort("bitwise:",p.mode)
	esac
	delopnd()
end

function do_addrmode(pcl p)mcloperand px =
!Top two stack elements are an array (xb) and index (ya)
!Return a operand which provdes the address mode to access the element,
!for either reading or writing
!The address mode will use 0, 1 or 2 registers. The registers may be 1 or 2
!associated with the pcl operands, or may be regvars.
!If for reading, caller will need to make their own arrangements for a dest reg.
!When Xb has to be loaded into a register anyway, then the caller can make use
!of that

	mcloperand ax,bx
	int m, scale, extra,offset, reg,regix
	psymbol d

	scale:=p.scale
	extra:=p.extra
	offset:=pclstack[1].value*scale+extra	!for imm offset

	m:=p.mode

	px:=nil

	if isregvaropnd(xb) then
		if isregvaropnd(ya) then			!regvar/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(areg:pclstack[2].reg,ireg:regix, offset:extra, scale:scale)

		elsif isimm64(ya) then			!regvar/imm
			px:=mgenindex(areg:pclstack[2].reg, offset:offset)
		else							!regvar/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(areg:pclstack[2].reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	elsif ismemaddr(xb) then
		d:=pclstack[2].def
		if isregvaropnd(ya) then			!memaddr/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)

		elsif isimm64(ya) then			!memaddr/imm
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
		ax:=genopnd_ld(xb)
		if isregvaropnd(ya) then			!any/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)
		elsif isimm64(ya) then			!any/imm
			px:=mgenindex(areg:ax.reg, offset:offset)
		else							!any/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)

		fi
	fi

!CPL "DAM",PX.SIZE,P.SIZE
	if px.size=0 then px.size:=p.size fi
	return px
end

function scaleregvar(int reg, &scale, n)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mcloperand ax

	if scale in [1,2,4,8] then return reg fi

	regix:=getnextreg()
	ax:=mgenreg(regix)
	genmc(m_mov,ax, mgenreg(reg))

	pclstack[n].fmt:=reg_d64
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=regix
	mulimm(ax,scale)
!	genmc(m_imul2, ax, mgenint(scale))
	scale:=1

	return regix
end

function scaleindex(mcloperand ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi
	mulimm(ax,scale)
!
!	n:=ispoweroftwo(scale)
!	if n then
!		genmc(m_shl, ax, mgenint(n))
!	else
!		genmc(m_imul2, ax, mgenint(scale))
!	fi
	return 1
end

function makeregopnd(int n)mcloperand ax=
!turn given pcl operand, which does not occupy a register,
!make it into register operand. Note that other characteristics, such
!as value/def for imm/mem/memaddr, are not affected
!offset = xa, yb etc

	pclstack[n].fmt:=reg_d64
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=getnextreg()

	return genopnd(n)
end

proc dobinto_int(pcl p, int opc)=
	mcloperand ax,bx,rx
	int reg,size

	size:=p.size

	if size=8 and ismemaddr(xb) then
		ax:=mgenmem(pclstack[2].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)
		genmc(m_mov, rx, ax)
		bx:=genopnd(ya)
		genmc(opc,rx,bx)
		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=genopnd_ind(xb,size:size)
		bx:=genopnd_ld(ya,size)

		genmc(opc,ax,bx)
	fi
	delopnd()
	delopnd()
end

proc dobinto_float(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x64()
	ax:=genopnd_ind(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	genmc(m_movq, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movq, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

proc dobinto_float32(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x32()
	ax:=genopnd_ind(xc,4)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	genmc(m_movd, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movd, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

proc doselectcc(pcl p)=
	mcloperand ax,bx, cx,dx
	int cond

	if pcat[p.mode]<>d64cat then merrort("selectcc",p.mode) fi

	ax:=genopnd_ld(xb)
!	bx:=genopnd_ld(ya)
	bx:=genopnd(ya)
	genmc(m_cmp,ax,bx)

	noxorclear:=1
	dx:=genopnd_ld(xc)
	cx:=genopnd_ld(wd)
	noxorclear:=0
	if psigned[p.mode] then
		cond:=scondcodes[p.opcode-kselecteq]
	else
		cond:=ucondcodes[p.opcode-kselecteq]
	fi
	genmc_cond(m_cmovcc, reversemcond(cond), cx,dx)
	delopnd()
	delopnd()
	delopnd()
end

function reversemcond(int cond)int=

	case cond
	when z_cond then return nz_cond
	when nz_cond then return nz_cond

	when lt_cond then return ge_cond
	when le_cond then return gt_cond
	when ge_cond then return lt_cond
	when gt_cond then return le_cond

	when ltu_cond then return geu_cond
	when leu_cond then return gtu_cond
	when geu_cond then return ltu_cond
	when gtu_cond then return geu_cond
	esac
	return 0
end

proc do_divrem(pcl p, int issigned, isdiv)=
	int opc, n, shifts

	loadopnd(2)

	if isdiv and pclstack[1].fmt=imm_d64 then
		n:=pclstack[1].value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			delopnd()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), genopnd(xb), mgenint(shifts))
				delopnd()
				return
			fi
		esac
	fi 

	loadopnd(1)
	saverdx()
	fixdivopnds()

	if issigned then
		genmc(m_cqo)
		opc:=m_idiv
	else
		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, genopnd(ya,p.size))

	if not isdiv then
		genmc(m_xchg,mgenreg(r0),mgenreg(r11))
	fi
	restorerdx()

	delopnd()

end

proc fixdivopnds=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	int regx,regy,zop

	regx:=pclstack[2].reg
	regy:=pclstack[1].reg

	if regx=r0 then			!regy will be OK
		return
	fi
	if regy=r0 then			!need to swap then
		genmc(m_xchg,genopnd(xb),genopnd(ya))
		swapopnds(1,2)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg,mgenreg(r0),genopnd(xb))
		regset[regx]:=0
		pclstack[2].reg:=r0
		regset[r0]:=1
		return
	fi

!need to move current occupier of r0
!	for zop:=1 to noperands do
	for zop:=noperands downto 1 do
		if pclstack[zop].loc=reg_loc and pclstack[zop].reg=r0 then exit fi
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg,mgenreg(r0),genopnd(xb))	
	swap(pclstack[2].reg,pclstack[zop].reg)		!switch registers

end

proc saverdx=
	if inf_r11used then
		genmc(m_push, mgenreg(r11))
	fi
end

proc restorerdx=
	if inf_r11used then
		genmc(m_pop, mgenreg(r11))
	fi
end

proc doincr(pcl p, int incrop, addop)=
	if p.stepx=1 then
		if ismemaddr(xa) then
			genmc(incrop, mgenmem(pclstack[1].def))
		else
			genmc(incrop, genopnd_ind(xa))
		fi
	else
		if ismemaddr(xa) then
			genmc(addop, mgenmem(pclstack[1].def), mgenint(p.stepx))
		else
			genmc(addop, genopnd_ind(xa), mgenint(p.stepx))
		fi
	fi
	delopnd()
end

proc doincrload(pcl p, int incrop, addop)=
	mcloperand ax, mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclstack[1].def)
		ax:=makeregopnd(xa)
	else
		mx:=genopnd_ind(xa,p.size)
		ax:=genopnd(xa)
	fi

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	if pcat[p.mode]=shortcat then
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi
end

proc doloadincr(pcl p, int incrop, addop)=
	mcloperand ax,mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclstack[1].def)
	else
		mx:=genopnd_ind(xa,p.size)
	fi

	addreg_d64()
	ax:=genopnd()

	if pcat[p.mode]=shortcat then
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi

	if p.stepx=1 then
		genmc(incrop,mx)
	else
		genmc(addop,mx, mgenint(p.stepx))
	fi

	swapopnds(1,2)
	delopnd()
end

proc do_syscall(int fnindex, nargs, retcat)=
!retcat = 0, d64cat, x64cat, x32cat, widecat

	int nslots

	sa_nargs:=nargs
	px_setargs(nil)

	nslots:=do_pushparams(nil,0)

	genmc(m_call, mgenlabel(getrtsproclabel(fnindex)))

!	mccodex.a.mode:=a_mem
	mccodex.a.size:=8

	to sa_nargs do
		poparg()
	od
	popslots(nslots)

	getretvalue_bycat(retcat)
end

proc getretvalue_bycat(int cat)=
	case cat
	when 0 then
		return
	when d64cat then
		addreg0(r0)
	when x64cat then
		addxreg0(r0,xreg_x64)
	when x32cat then
		addxreg0(r0,xreg_x32)
	when widecat then
		addwidereg0(r0)
	else
		merror("getval bycat")
	esac
end

proc px_pushlabel(pcl p)=
	if p.opndtype<>label_opnd then merror("pushlabel") fi
!CPL "ADD LABEL..."
	addlabeladdr(p.labelno)
end

proc do_shiftnto(pcl p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mcloperand px

	px:=genopnd_ind(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(opc, px, mgenint(pclstack[1].value))
	else
		if inf_r10used then merror("shiftto:cl in use") fi
		loadparam(1,r10)
		genmc(opc, px, mgenreg(r10,1))
	fi

	delopnd()
	delopnd()
end

proc domax_float(int opc)=
	mcloperand ax,bx
	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)
	genmc(opc,ax,bx)
	delopnd()
end

proc domax_int(int cond)=
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd_ld(ya)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	delopnd()
end

proc domaxto_r64(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)
	addreg_x64()
	ax:=genopnd(xa)

	genmc(m_movq, ax, px)

	genmc(m_comisd, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movq, px,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_r32(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)
	addreg_x32()
	ax:=genopnd(xa)

	genmc(m_movd, ax, px)

	genmc(m_comiss, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movd, px,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_int(int cond)=
	mcloperand ax,bx,lx
	int lab

	ax:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, ax,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
end

proc dosetfntable=
	if lab_funcnametable=0 then
		lab_funcnametable:=++mlabelno
		lab_funcaddrtable:=++mlabelno
		lab_funcnprocs:=++mlabelno
	fi
end

proc loadtoreg(mcloperand rx, ax, int m)=
	if pcat[m]=d64cat then
		genmc(m_mov, rx, ax)
	elsif psigned[m] then
		genmc(m_movsx, rx, ax)
	else
		genmc(m_movzx, rx, ax)
	fi
end

proc storefromreg(mcloperand ax, rx, int size)=
	genmc(m_mov, ax, changeopndsize(rx,size))
end

proc domaths(pcl p, ichar opname)=
	int nslots
	p.nargs:=1

	px_setargs(p)

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenextname(opname))

	poparg()

	popslots(nslots)

	dogetretvalue(p)
end

proc domaths2(pcl p, ichar opname)=
	int nslots
	p.nargs:=2

	px_setargs(p)

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenextname(opname))

	poparg()
	poparg()

	popslots(nslots)

	dogetretvalue(p)
end
=== pc_genss.m 0 0 6/25 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

int rex
int sizeoverride					!32=>16 switch
int addroverride					!32=>16 switch
int f2override						!xmm regs
int f3override						!xmm regs

mcloperand extraparam

int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
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
	extraparam:=nil

	fixregvar()

	m:=mccode
	index:=0

	while m do
		doinstr(m,++index)
		m:=m.nextmcl
	od

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	fi
end

proc doinstr(ref mclrec m,int index)=
	mcloperand a,b
	psymbol d,e
	int x,offset,shortjmp,n

	buffercheck(currdata)

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=0

	a:=m.a
	b:=m.b

	++instrno
	alineno:=instrno

	CURRMCL:=M

	switch m.opcode
	when m_procstart then
		CURRASMPROC:=M.A.DEF
	when m_procend then
	when m_define then

	when m_definereg then
	when m_deleted then

	when m_labelname then
		case a.valtype
		when stringimm_val then
		when def_val then
			d:=a.def
			d.reftype:=back_ref
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)

			if d.isexported then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_label then
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
		if offset<0 then			!backjump
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
!		if x not in 1..16384 then axerror("align2") fi
			if x<1 or x>16384 then axerror("align2") fi
			if currseg<>zdata_seg then
				while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) od
			else
				while ss_zdatalen rem x do	++ss_zdatalen od
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

	when m_param then
		extraparam:=a

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

	when m_rdtsc then		!single opcodes that need a 0x0F prefix
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

proc genopnd(mcloperand a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
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
			CPL "		OPND/REALMEM4"
		when stringimm_val then
			CPL "		OPND/STRINGIMM4"
		when def_val,label_val then
			genabs32(a)
		when name_val then
			CPL "		OPND/NAME4"
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
			CPL "		OPND/REALMEM8",ALINENO
		when stringimm_val then
			CPL "		OPND/STRINGIMM8"
		when def_val,label_val then
			genabs64(a)
		when name_val then
			CPL "		OPND/NAME8"
		else
			CPL "HERE"
			cpl valtypenames[a.valtype]
			axerror("OPND/8/VALTYPE?")
		esac

	esac
end

proc addrelocitem(int reloctype, psymbol d)=
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

function getstindex(psymbol d)int=
!retrieve existing obj st index, or create new one
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols
		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.isimported then
				d.segment:=code_seg
			fi
		fi

	fi
	return d.stindex
end

proc genrel32(mcloperand a)=
!used by call/longjmp/ddoffset
	psymbol d

	d:=getdef(a)

	if d=nil then				!constant
		gendword(a.value)
		return
	fi

	case d.reftype
	when back_ref then
		if d.segment<>currseg then
			axerror("Rel label across segments")			!might be Ok if treated as external?
		fi
		gendword(d.offset-(getcurrdatalen(2)+4)+a.offset)
	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel32_rel)
		gendword(a.offset)
	else								!external symbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

function getdef(mcloperand a,int dneeded=0)psymbol =
	psymbol d

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if not d.isimported then
					d.reftype:=fwd_ref
				fi
			fi

			return d
		esac
	fi
	if dneeded then				!must return a non-nil value
		println opndnames_ma[a.mode],valtypenames[a.valtype]
		axerror("getdef/no def")
	fi
	return nil
end

proc genabs32(mcloperand a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then

		gendword(d.offset+a.offset)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
!CPL "NEW1",D.FWDREFS, D.NAME,D
		if d.pcldef and d.pcldef.opcode in [klocal, kparam] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		fi

	else								!external symbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(mcloperand a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then
		genqword(d.offset+a.offset)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
		if d.pcldef and d.pcldef.opcode in [klocal, kparam] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		fi

	else								!external symbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	esac
end

function getrel32(psymbol d,int offset)int=
!get rel difference between offset in this segment, and label d
	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			axerror("Rel label across segments2")
		fi
		return d.offset-(offset+1)
	else
		return int32.maxvalue
	fi
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
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

proc do_push(mcloperand a)=
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

proc do_pop(mcloperand a)=
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

proc do_inc(mcloperand a,int code)=
!inc/dec
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

proc do_neg(mcloperand a,int code)=
!neg/not/mul/imul/div/idiv
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

proc genamode(mcloperand a,int am)=
	int sib,mode,dispsize,offset
	psymbol d

	sib:=am>>16

	mode:=(am>>8)iand 255
	dispsize:=am iand 255

	genbyte(mode)			!modrm byte

	if sib>=0 then		!sib byte
		genbyte(sib)
	fi

	case dispsize			!disp bytes
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
			CPL OPNDNAMES_MA[A.MODE]
			axerror("GENAMODE/MODE?")
		esac
	else
		axerror("genamode size 2/8")
	esac
end

function makemodrm(int mode,opc,rm)int=
	return mode<<6+opc<<3+rm
end

proc setopsize(mcloperand a)=
	case a.size
	when 1 then			!assume set via specific opcodes
	when 2 then			!override default 4 bytes
		sizeoverride:=1
	when 8 then			!override default 4 bytes
	    rex ior:=wmask
	when 4 then			!assume 4 bytes is default
	else
		axerror("Operand size not set")
	esac
end

proc setaddrsize(mcloperand a)=
	if a.mode=a_mem and a.addrsize=4 then
		addroverride:=1
	fi
end

function getdispsize(mcloperand a, int &offset)int=
!look at imm/mem displacement, and return 0,1 or 4
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	psymbol d

	d:=getdef(a)
	offset:=a.offset

	if d then
		if d.pcldef and d.pcldef.opcode in [klocal, kparam] then
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

function genrm(mcloperand a,int opc)int=
!work out modrm, and possible sib and address offset sequence from
!operand a (a_mem) and middle bits x (0..7) of the modrm byte
!returns: (modrm, sib, dispsize)
! sib = -1 means no sib byte
! dispsize is 0 (no disp), 1 (8-bit), or 4 (32-bit)
!will also set rex bits as needed
!!                         0  1  2  3  4  5  6  7
!static var scaletable=(0: 0, 0, 1, 0, 2, 0, 0, 3)
!                       1  2  3  4  5  6  7  8
	static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
	int mode, rm, scale, dispsize, sib, index, base
	int reg, regix, code, offset

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used
	dispsize:=0
	sib:=-1

	if a.mode=a_mem and a.addrsize=4 then
		addroverride:=1
	fi

	case a.mode
	when a_reg then			!modrm can only ref to a single register
		code:=getregcodeb(a.reg)
		return makeam(makemodrm(3,opc,code), sib, dispsize)

	when a_mem then

	when a_xreg then
		code:=getregcodebx(a.reg)
		return makeam(makemodrm(3,opc,code), sib, dispsize)		!NEW

	else
		axerror("genrm not mem")
	esac

	reg:=a.reg
	regix:=a.regix

	if reg=regix=0 then						!address only
		mode:=0
		rm:=4
		scale:=1
		index:=4
		base:=5
		dispsize:=4

	elsif a.scale<=1 and regix=0 then			!simple address mode (no sib)
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
			index:=4				!means no index
			base:=rm
			scale:=1				!force sib

		fi
	elsif regix and reg=0 then
		dispsize:=4
		mode:=0
		rm:=4
		scale:=(a.scale|a.scale|1)
		base:=5
		index:=regcodes[regix]
		if regix=rstack then axerror("Scaled rstack?") fi

	else										!assume regix used; optional reg and disp
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
		fi

		if regix and not reg then
			dispsize:=4
		fi

		if regix=rstack and scale>1 then axerror("Can't scale rstack") fi

	fi

	if index>=8 then rex ior:= xmask; index iand:=7 fi
	if base>=8  then rex ior:= bmask; base  iand:=7 fi

	if scale then
		sib:=scaletable[scale]<<6 + index<<3 + base
	fi
	rm iand:=7

	return makeam(makemodrm(mode:mode,opc:opc,rm:rm), sib, dispsize)
end

function makeam(int m,s,d)int=
!convert mode, sib, dispsize into 32-bit value::
! ssssssss ssssssss mmmmmmmm dddddddd
!return m<<16+s<<8+d
!note: s can be -1, so allow to extend into sign bit::
	return s<<16+m<<8+d
end

proc do_arith(mcloperand a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
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
		CPL OPNDNAMES_MA[A.MODE]
		axerror("Can't add to this opnd")
	esac
end

proc do_mov(mcloperand a,b)=
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
!			if value not in -32768..65535 then axerror("exceeding word16 value") fi
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

			else							!assum 8 bytes
				if getdef(b) then
					rex ior:=wmask
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,8)
				else
					if value>=0 and value<=0xFFFF'FFFF then
						goto doreg32			!load 32-bit value which is zero-extended to 64
					fi
!there might be short form for negative values that fit into 32 bits, using other opcs
!but ignore that for now
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
			CPL OPNDNAMES_MA[A.MODE]
			CPL OPNDNAMES_MA[B.MODE]
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
!do not translate reg code (I think, when xmm reg code etc)

	int regcode

	regcode:=reg-1
	if regcode>=8 then
		regcode-:=8
		rex ior:=bmask
	fi
	return regcode
end

function getregcoderx(int reg)int=
!do not translate reg code (I think, when xmm reg code etc)
	int regcode

	regcode:=reg-1
	if regcode>=8 then
		regcode-:=8
		rex ior:=rmask
	fi
	return regcode
end


proc do_lea(mcloperand a,b)=
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

proc do_movsx(mcloperand a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
	int am, regcode

	if a.mode<>a_reg then axerror("movsx not reg") fi

	if a.size=8 and b.size=4 then
		if opc=0xBE then
			do_movsxd(a,b)
		else						!movsx 4->8 bytes, do normal move 4->4
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

proc checkhighreg(mcloperand a)=
	if a.mode=a_reg then
		case a.reg
		when r5,r3,r14,r15 then
			rex ior:=0x40
		esac
	fi
end

proc do_exch(mcloperand a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
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

proc do_movsxd(mcloperand a,b)=
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

proc do_imul2(mcloperand a,b)=
	int regcode, am, opc
	int64 value

	if a.mode<>a_reg then
		axerror("imul2 opnds")
	fi
	if b.size=0 then b.size:=a.size fi
	if a.size=1 then axerror("imul2 byte") fi

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then axerror("imul2 size") fi
		regcode:=getregcoder(a.reg)
		am:=genrm(b,regcode)

		setopsize(a)
		genrex()
		genbyte(0x0F)
		genbyte(0xAF)
		genamode(b,am)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if getdef(b) then axerror("mul/label") fi
		value:=b.value
		regcode:=getregcoder(a.reg)		!same reg used in two places
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

proc do_shift(mcloperand a,b,int opc)=
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

proc do_test(mcloperand a,b)=
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

proc do_loop(mcloperand a,int opc)=
	int offset

	offset:=getrel32(getdef(a,1),getcurrdatalen(9)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			axerror("loop jmp out of range")
		fi
		genbyte(opc)
		genbyte(offset)
	else
		axerror("Can't do loopxx fwd jump")
	fi
end

proc do_jcxz(mcloperand a,int opsize)=
	int offset

	offset:=getrel32(getdef(a,1),getcurrdatalen(10)+1)
	if offset<0 then			!backjump
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

proc do_setcc(int cond, mcloperand a)=
!a is cond
!b is byte reg/mem
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

proc do_movxmm(mcloperand a,b,int size)=
!do movd/movq depending on size being 4 or 8
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

proc do_arithxmm(mcloperand a,b,int prefix,opc)=
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

proc do_logicxmm(mcloperand a,b,int opc,size)=
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

proc do_convertfloat(mcloperand a,b,int prefix)=
!cvtss2sd and cvtsd2ss
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

proc do_fix(mcloperand a,b,int prefix,opc)=
!cvtss2si and cvtsd2si opc=2d
!cvttss2si and cvttsd2si opc=2c
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

proc do_float(mcloperand a,b,int prefix)=
!cvtss2si and cvtsd2si
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

proc do_call(mcloperand a)=
	int am, regcode
	case a.mode
	when a_imm then
		genbyte(0xE8)
		genrel32(a)
	else				!indirect call
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("call[]size")
		esac
		am:=genrm(a,2)
		setopsize(a)
		setaddrsize(a)
		genrex()
		genbyte(0xFF)
		genamode(a,am)

	esac
end

proc do_jmp(mcloperand a,ref mclrec m)=
	int am, regcode, offset, shortjmp
	psymbol d

	case a.mode
	when a_imm then				!assume label_val
		case a.valtype
		when label_val,def_val then
			d:=getdef(a,1)
			offset:=getrel32(d,getcurrdatalen(11)+1)+a.offset
			if offset<0 and offset>-126 then
				genbyte(0xEB)
				genbyte(offset)
			else
				shortjmp:=0
				if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
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
	else				!indirect jump
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("jmp[]size")
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
!I think that zdata-seg is only likely when id=6

	if currseg=zdata_seg then
		return ss_zdatalen
	fi
	return bufferlength(currdata)
end

proc do_cmovcc(int cond, mcloperand a,b)=
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

proc do_fmem(mcloperand a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
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

proc genrel8(mcloperand a)=
!a is a known fwd reference, and expected to be <=127 bytes
	psymbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
!CPL "NEW3",D.FWDREFS, D.NAME,D
		genbyte(0)
	else								!external symbol
		axerror("genrel8")
	fi
end

function checkshortjump(ref mclrec m,psymbol d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
! d can be a named label, or a labelno; either should have .labelno set
	int n

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		case m.opcode
		when m_label then
!		++n
			if m.a.labelno=d.labelno then
				return 1
			fi
!		when m_comment, m_blank then
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

	case currseg						!reloc linked list roots must be updated
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
	esac							!else 0, done at end to update linked lists

end

proc do_movdqx(mcloperand a,b, int opc)=
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

proc do_popcnt(mcloperand a,b)=
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

proc do_bsf(mcloperand a,b, int opc)=
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
	ref[]psymbol oldsymboltable
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

!	while m do
!		if m.a then fixopnd(m.a) fi
!		if m.b then fixopnd(m.b) fi
!		m:=m.nextmcl
!	od
end

global proc initlib(int nlabels)=
	[256]char str

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0

	labeldeftable:=pcm_alloc(nlabels*ref void.bytes)
	for i to nlabels do
		labeldeftable[i]:=pcm_allocz(pstrec.bytes)
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
		println "dbuffer error"
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

=== pc_lex.m 0 0 7/25 ===
!Tokeniser Module

global tabledata() []ichar symbolnames=
	(errorsym,			$),		! Lex error
	(commasym,			$),		! ","
	(colonsym,			$),		! ":"
	(dcolonsym,			$),		! "::"
	(lsqsym,			$),		! [
	(rsqsym,			$),		! ]

	(addsym,			$),		! +
	(subsym,			$),		! -
	(mulsym,			$),		! *
	(addrsym,			$),		! &

	(eqsym,				$),		! =

	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen

!	(hashsym,			$),		! #
	(labelsym,			$),		! #123

	(intconstsym,		$),		! 123 64 bits signed
	(realconstsym,		$),		! 123.4 64 bits
	(stringconstsym,	$),		! "ABC"

	(namesym,			$),		! raw name

	(kopcodesym,		$),		! push etc
	(typesym,			$),		! i32 etc

	(kdummysym,			$)		!
end

macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10

global const hstsize=65536*4
global const hstmask=hstsize-1

global [0:hstsize]psymbol lexhashtable
int nsymbols

!the following returned by updated by lexreadtoken()

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names

global int lxsymbol		!* main symbol kind
global int lxsubcode	!* for some symbols, which specific keyword

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
global byte lxtruename
int lxhashvalue

global ref byte lxsptr		!@ points to next char in source
ref byte lxstart			!@ start of source code for this file
global psymbol lxsymptr	!set by lookuplex()

![0..255]char alphamap
![0..255]char digitmap
[0..255]char commentmap

global proc lex=
!lowest level lex() function, reads names, numbers etc but does no lookups or translations
!returns results in lx-vars. Current source pointer should be in lxsptr
int i, c, d, hsum, length
ref byte pstart

	lxsubcode:=0

	doswitch c:=lxsptr++^
	when 'a'..'z','$','_','.' then
		pstart:=lxsptr-1		!point to start of name in source buffer
	doname::
		hsum:=pstart^

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
		lxtruename:=0

		if lookuplex(cast(pstart),lxlength) then
			if lxsymptr.ksymbol then			!keywords take priority here
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
		c:=pstart^:=pstart^+32
		goto doname

	when '0'..'9' then
		readnumber(c)
		return

	when '`' then
		pstart:=lxsptr		!point to start of name in source buffer
		hsum:=0

		doswitch c:=lxsptr^
		when 'A'..'Z','a'..'z','0'..'9','_','$','.' then
			++lxsptr
			hsum:=hsum<<4-hsum+c
		else
			exit
		end

		lxsymbol:=namesym
		lxtruename:=1
		if pstart=lxsptr then
			lxerror("NULL ` name")
		fi
		lxlength:=lxsptr-pstart
		lxhashvalue:=hsum<<5-hsum

		if lookuplex(cast(pstart),lxlength) then
			lxsymbol:=lxsymptr.symbol			!can't be a keyword
			if lxsymbol=0 then					!assume was a keyword; use as name
				lxsymbol:=lxsymptr.symbol:=namesym
			fi
		fi
		return

	when '!',';' then			!comment to eol

		while commentmap[lxsptr++^] do od

		if (lxsptr-1)^=0 then --lxsptr fi
!
		++lxlineno

		lxsymbol:=eolsym
		return

	when '#' then				!label
		lxvalue:=0
		doswitch c:=lxsptr++^
		when '0'..'9' then
			lxvalue:=lxvalue*10+c-'0'
		else
			--lxsptr
			exit
		end
	
		if lxvalue=0 then lxerror("Bad label") fi
		if labelnooffset=0 then
			maxuserlabel max:=lxvalue
		else
			lxvalue+:=labelnooffset
		fi
		lxsymbol:=labelsym
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

	when '-' then
		c:=lxsptr++^
		if c not in '0'..'9' then lxerror("Bad no") fi
		readnumber(c,-1)
		return

	when '*' then
		lxsymbol:=mulsym
		return

	when '&' then
		lxsymbol:=addrsym
		return

	when '"' then
		readstring()
		return

	when ' ',9 then

	when cr then			!lf expected to follow
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
	static byte done=0

	return when done

	lxsubcode:=0
	lxsymbol:=errorsym

	lxlineno:=0

	for i:=0 to 255 do
!		switch i
!		when 'A'..'Z','a'..'z','$','_','0'..'9' then
!			alphamap[i]:=1
!		end
!		switch i
!		when '0'..'9' then
!			digitmap[i]:=1
!		end
		commentmap[i]:=1
	od

	commentmap[0]:=0
	commentmap[lf]:=0

	inithashtable()

	done:=1
end

proc readreal(ichar s,int slen)=
	int c

	c:=lxsptr^
	lxsptr^:=0
	lxxvalue:=strtod(s,nil)
	lxsptr^:=c

	lxsymbol:=realconstsym
end

proc readnumber(int c, signx=1)=
!A digit c 0..9 has just been read. Numeric formats are::
!1234
!0x1234
!2x1101
!Nx....		possible
	[256]char str
	int i,d,intlen,slen,isfloat,sepseen
	ichar s

	d:=lxsptr^
	case d
	when 'x','X' then			!other base
		case c
		when '0' then			!hex
			++lxsptr
			readhex()
			lxvalue*:=signx
			return
		else
			cpl c
			lxerror("Base not supported")
		esac
	esac

!assume decimal
	str[1]:=c
	s:=lxsptr-1				!start of number in source
	slen:=1
	isfloat:=sepseen:=0

	doswitch c:=lxsptr++^
	when '0'..'9' then
		str[++slen]:=c
	when '_','\'' then
		sepseen:=1
	when '.' then
		if isfloat then lxerror("float?") fi
		isfloat:=1
		intlen:=slen
	when 'e','E' then
		if isfloat=2 then lxerror("float?") fi
		isfloat:=2
	when '-','+' then
		if isfloat<>2 then lxerror("float?") fi
		isfloat:=3
	else
		--lxsptr
		exit
	end

	if isfloat then
		if sepseen then lxerror("seps in float?") fi
		readreal(s,lxsptr-s)
		lxxvalue*:=signx
		return
	fi

	if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
		lxerror("Overflows 64 bits")
		return
	fi

	lxsymbol:=intconstsym

	lxvalue:=0
	for i:=1 to slen do
		lxvalue:=lxvalue*10+str[i]-'0'
	od
	lxvalue*:=signx
end

proc readhex=
!positioned at start of hex seq; 0 chars read yet
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
	elsif ndigs>32 then
		lxerror("Overflow in hex number")
	elsif ndigs>16 then
		lxerror("hex/128 bits not ready")
!		stringtonumber128(str,slen,16)
		return
	fi
	lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
	PRINT CAPTION,LXSPTR,":"
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
		print @dev,"""",,lxsvalue,,""""!,,"end"
	when errorsym then
		print @dev,lxvalue
	when kopcodesym then
		print @dev,pclnames[lxsubcode]+1

	when typesym then
		print @dev,pstdnames[lxsubcode]

	else
		print @dev,symbolnames[lxsymbol]
		if lxsubcode then
			print " ",,lxsubcode
		fi

	end

	println @dev,$,lxlineno
end

proc inithashtable=
!initialise hash table from kwddata
	pstready:=0
	for i in pclnames.bounds do
		addreservedword(pclnames[i]+1,kopcodesym,i)
	od

	for i in pstdnames.bounds do
		addreservedword(pstdnames[i],typesym,i)
	od

	addreservedword("proc",kopcodesym,kprocdef)
	addreservedword("function",kopcodesym,kprocdef)
	addreservedword("end",kopcodesym,kendproc)
	addreservedword("endext",kopcodesym,kendextproc)

	pstready:=1

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
	psymbol r
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
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
	psymbol e
	int j,wrapped,insource,firstj

	insource:=length
	if length=0 then
		length:=strlen(name)
	fi

	firstj:=j:=(lxhashvalue iand hstmask)		!j=initial hash index

	wrapped:=0

	do
		lxsymptr:=lexhashtable[j]
		if lxsymptr=nil then				!unused entry, not found
			exit
		fi

		if lxsymptr.namelen=length and memcmp(lxsymptr.name,name,length)=0 then			!match
			return 1
		fi

		if ++j>=hstsize then		!wraparound
			if wrapped then
				println "???????HASHTABLE FULL",hstsize,lxlineno
				stop 1
			fi
			wrapped:=1
			j:=1
		fi
	od

!name not found
	if insource then
		name:=pcm_copyheapstringn(name,length)
	fi

	if ++nsymbols>((hstsize*7)/8) then
		lxerror("Hashtable getting full")
	fi

	if lxsymptr=nil then
		lxsymptr:=pcm_allocz(pstrec.bytes)
		lexhashtable[j]:=lxsymptr
	fi

	lxsymptr.name:=name
	lxsymptr.namelen:=length
	lxsymptr.symbol:=namesym
	lxsymptr.ksymbol:=0

	PCL_ADDSYM(LXSYMPTR)

	return 0
end

global proc startlex(ichar source)=
	initlex()
	lxstart:=lxsptr:=cast(source)
	lxlineno:=1
	lxsymbol:=errorsym
end

global function addnamestr(ichar name)psymbol=
!add a new name to the symbol table
!return symptr to new (or existing) generic name
	lxhashvalue:=gethashvalue(name)
	lookuplex(pcm_copyheapstring(name),0)
	return lxsymptr
end

global proc lxerror(ichar m)=			!LXERROR

	fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno
	stop 1
end

global function gethashvalue(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes S is lower-case, as conversion not done
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

proc readstring=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter

	ichar dest
	int c,d
	[8]char str

	lxsymbol:=stringconstsym

	lxsvalue:=lxsptr

	dest:=lxsptr				!form string into same buffer

	do
		switch c:=lxsptr++^
		when '\\' then			!escape char
			c:=lxsptr^
			if c>='A'  and c<='Z' then c+:=' ' fi
			++lxsptr
			switch c
			when 'a' then			!bell ('alert')
				c:=7
			when 'b' then			!backspace
				c:=8
			when 'c','r' then		!carriage return
					c:=cr
			when 'e' then			!end-of-text
				c:=26
			when 'f' then			!formfeed
				c:=12
			when 'l','n' then		!linefeed, or linux/c-style newline
				c:=lf
			when 's' then			!eScape
				c:=27
			when 't' then			!tab
				c:=9
!			when 'u' then			!reserved for unicode, like \x but with 4 hex digits
			when 'v' then			!vertical tab
				c:=11
			when 'w' then			!windows-style cr-lf
				dest++^:=cr
				c:=lf
			when 'x' then	!2-digit hex code follows
				c:=0
				to 2 do
					case d:=lxsptr++^
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
			when 'y' then			!CCI/SM backwards tab
				c:=16
			when 'z','0' then		!null (not fully supported in code)
				c:=0
			when '"','Q' then		!embedded double quote
				c:='"'
			when '\\' then
				c:='\\'
			when '\'' then			!embedded single quote
				c:='\''
			else
				println "<",,char(c),,">"
				lxerror("Unknown string escape")
			end
		when '"' then		!possible terminators
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
			else			!was end of string
				exit
			fi
		when cr,lf,0 then
			lxerror("String not terminated")
		endswitch

		dest++^:=c
	od
	lxlength:=dest-lxsvalue
	(lxsvalue+lxlength)^:=0		!overwrites final " or earlier
end

=== pc_libmcl.m 0 0 8/25 ===
const fasmformat=1
!const fasmformat=0

const fuseregtable=1
!const fuseregtable=0

const targetsize=8

global int fshowmsource=0

global macro isframex(d) = (d.pclop in [klocal, kparam])

[r1..rstack]mcloperand rd

global proc mclinit=
	mcloperand a
	int r,s

	for r:=r0 to r15 do
		regtable[r,1]:=mgenreg0(r,1)
		regtable[r,2]:=mgenreg0(r,2)
		regtable[r,4]:=mgenreg0(r,4)
		regtable[r,8]:=mgenreg0(r,8)
		regtable[r,16]:=mgenreg0(r,16)

		rd[r]:=regtable[r,8]
	od

	zero_opnd:=mgenint0(0)

	for i:=0 to maxsmallint do
		smallinttable[i]:=mgenint0(i)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=mgenreg(rframe,8)
	dstackopnd:=mgenreg(rstack,8)

	initmcdest()

	setsegment('C')

	stringtable:=pcm_alloc(ref void.bytes*initstringsize)
	stringlabtable:=pcm_alloc(int32.bytes*initstringsize)
	realtable:=pcm_alloc(real.bytes*initrealsize)
	reallabtable:=pcm_alloc(int32.bytes*initrealsize)

	nstrings:=0
	nreals:=0

	stringtablesize:=initstringsize
	realtablesize:=initrealsize

	pclstack:=cast(&pclopndstack[maxoperands])

	lab_funcnametable:=0
	lab_funcaddrtable:=0

end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
	clear rtsproclabels
end

export proc genmc(int opcode, mcloperand a=nil,b=nil)=
	ref mclrec m, oldm
	int labno

	m:=pcm_allocz(mclrec.bytes)

	m.opcode:=opcode

	m.a:=a
	m.b:=b

	case opcode
	when m_call then
		++inf_proccalls

	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_label then
		labno:=a.labelno
		if labno>maxlabelno then
	CPL =LABNO, MAXLABELNO
			merror("Too many labels")
		fi
		labeltable[labno]:=m

	esac

	if mccode then
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

global proc genmc_cond(int opcode, cond, mcloperand a=nil,b=nil)=
	genmc(opcode,a,b)
	mccodex.cond:=cond
end

global proc genmc_str(int opcode,ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode,mgenstring(s))
end

function newmclopnd:mcloperand a=
	a:=pcm_allocz(mclopndrec.bytes)
	return a
end

global function duplopnd(mcloperand a)mcloperand=
	mcloperand b
!	b:=pcm_alloc(mclopndrec.bytes)
	b:=newmclopnd()
	b^:=a^
	return b
end

export function mgenxreg(int xreg,size=8)mcloperand=
	mcloperand a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
	return a
end

export function mgenindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, psymbol def=nil)mcloperand=
!construct a mem address mode
	mcloperand a
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

global function getmclstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(dest)

	for i to nplibfiles when plibfiles[i]^<>'$' do
		asmstr("          ")
		asmstr((plibtypes[i]='D'|"importdll "|"importlib "))
		asmstr(plibfiles[i])
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

global proc mgencomment(ichar s)=
!if not debugmode then return fi
	if s=nil or s^=0 then
		genmc(m_blank)
	else
		genmc_str(m_comment,s)
	fi
end

global function mgenstring(ichar s,int length=-1)mcloperand=
	mcloperand a
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

global function mgenname(ichar s)mcloperand=
	[64]char str
	mcloperand a
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
	mcloperand a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=mcl.opcode

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

	case opcode
	when m_procstart then
!CPL "PROCSTART"
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

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
!ASMSTR("LABNAME/")
		case a.valtype
		when def_val then
			asmstr(getfullname(d))
		when stringimm_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.isexported then
!IF D=PSTENTRYPOINT THEN
!CPL "ASM ENTRY POINT",D.NAME,=PMAINLIB
!FI
			unless d=pstentrypoint and plibmode then
				asmstr("\n")
				asmstr(getbasename(d.name))
				asmstr("::")
			end
		fi
		return

	when m_label then
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
			asmstr(fgetregname(b.reg, b.size))
		esac
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

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode,a,b)
!
		asmopnd(a,sizepref)
		asmstr(",	")
		asmopnd(b,sizepref)

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0)
		else
			asmopnd(a,1)
		fi
	fi

DOCOMMENTS::
!IF PRODMODE THEN
!	RETURN
!FI
!	asmstr(" !")
!
!	if comment then
!!		asmstr("	!")
!		asmstr(comment)
!	fi

!	IF MCL.COMMENT THEN
!		ASMSTR(" ")
!		ASMSTR(MCL.COMMENT)
!	FI
!
!	for i in mcl.regend.bounds do
!		if mcl.regend[i] then
!			asmstr(" Free:")
!			asmstr(getregname(i))
!		fi
!	od

end

global proc asmopnd(mcloperand a,int sizeprefix=0,debug=0)=
	asmstr(stropnd(a,sizeprefix,debug))
end

global proc setsegment(int seg,align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
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
		genmc(m_align,mgenint(align))
	fi
end

global function getsizeprefix(int size,enable=0)ichar=
	if not enable then return "" fi
	case size
	when 1 then return "byte "
	when 2 then return "word16 "
	when 4 then return "word32 "
	when 8 then return "word64 "
	esac
	return ""
end

global function needsizeprefix(int opcode,mcloperand a,b)int=
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

global function changeopndsize(mcloperand a,int size)mcloperand=
	mcloperand b

	if a.size<>size then
		if a.mode=a_reg then
			b:=regtable[a.reg, size]
		else
			b:=duplopnd(a)
			b.size:=size
		fi
		return b
	fi
	return a
end

global function makeopndind(mcloperand a,int size=0)mcloperand=
	mcloperand b

	if a.mode<>a_reg then
		merror("makeopndind")
	fi

	return mgenireg(a.reg,size)
end

global function applyoffset(mcloperand a,int offset,int size=0)mcloperand=
!astr is an asm operand
!add possible byte offset
	mcloperand b

	if offset=0 and size=0 then
		return a
	fi
	b:=duplopnd(a)
	b.offset+:=offset
	if size then
		b.size:=size
	fi

	return b
end

export function mgenint(int64 x,int size=8)mcloperand=
	if x in 0..maxsmallint and size=8 then
		return smallinttable[x]
	fi

	return mgenint0(x,size)
end

global function mgenint0(int64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global function mgenrealmem(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem
	a.value:=getrealindex(x,size)
	a.valtype:=label_val
	a.size:=size
	return a
end

global function mgenrealimm(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=size
	return a
end

export function mgenlabel(int x=0)mcloperand a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm
	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val
	return a
end

global function mgenlabelmem(int x)mcloperand a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

global function mgenregvar(psymbol d)mcloperand a=
	a:=mgenreg(d.reg,8)
	isregvar[d.reg]:=1

	return a
end

global function mgenxregvar(psymbol d)mcloperand a=
	a:=mgenxreg(d.reg)
	isxregvar[d.reg]:=1

	return a
end

global function mgenmem(psymbol d)mcloperand a=
	pcl p:=d.pcldef
	int reg

IF P=NIL THEN
CPL D.NAME,".PCLDEF NOT SET"
STOP
FI

	if d.reg then
		if pfloat[p.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d)
		fi
	fi

	reg:=rnone
	if isframex(d) then
		if not optimflag and (int(d.offset) in -128..64) and p.size=8 then
			return frameregtable[d.offset]
		fi

		reg:=rframe
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
	a.valtype:=def_val

	a.size:=min(p.size,8)

	return a
end

global function mgenmemhigh(psymbol d)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem

	if isframex(d) then
		a.reg:=rframe
	fi
	++d.nrefs
	a.def:=d
	a.valtype:=def_val
	a.offset:=8
	a.size:=8

	return a
end

export function mgenmemaddr(psymbol d)mcloperand=
	mcloperand a

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

export function mgenreg(int reg,size=8)mcloperand=
	if fuseregtable then
		return regtable[reg,size]
	fi
	return mgenreg0(reg,size)
end

global function mgenreg0(int reg,size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size
	return a
end

global function mgenireg(int reg,size=8,offset=0)mcloperand=
	mcloperand a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=size
	a.offset:=offset

	return a
end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	while size iand (targetsize-1) do ++size od
	return size
end

global function getregname(int reg,size=8)ichar=
	static [1..17]ichar prefix=("B","W","","A","","","","D","","","","","","","","Q","N")
	static [32]char str
	[16]char str2
	ichar rs
	int size2

	size2:=size
	if size2>16 then
		size2:=17
	FI

	case reg
	when rnone then return "-"
	when rframe then rs:="frame"
	when rstack then rs:="stack"
	else
		getstrint(reg-r0,&.str2)
		rs:=&.str2
	esac

	print @&.str,prefix[size2],,rs
	return &.str
end

global function fgetregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" fi

	if fasmformat then
		print @&.str,"XMM",,reg-xr0
	else
		print @&.str,(size=8|"DX"|"SX"),,reg-xr0
	fi
	return &.str
end

global function sameoperand(mcloperand a,b)int=
	return memcmp(a,b,mclopndrec.bytes)=0
end

global function sameregopnd(mcloperand a,b)int=
!check if same register operand
	unless a.mode=b.mode=a_reg then return 0 end
	return a.reg=b.reg
end

global function getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if nstrings>=stringtablesize then
		extendstringtable()
	fi

	if nstrings and eqstring(stringtable[nstrings],s) then
		return stringlabtable[nstrings]
	fi

	stringtable[++nstrings]:=s
	stringlabtable[nstrings]:=++mlabelno

	return mlabelno
end

global function getrealindex(real x,int size)int=
	if nreals>=realtablesize then
		extendrealtable()
	fi

	realtable[++nreals]:=x
	++mlabelno
	reallabtable[nreals]:=(size=8|mlabelno|-mlabelno)
	return mlabelno
end

proc extendstringtable=
	ref[]ichar oldstringtable
	ref[]int32 oldstringlabtable
	int oldstringtablesize

	oldstringtablesize:=stringtablesize
	oldstringtable:=stringtable
	oldstringlabtable:=stringlabtable

	stringtablesize*:=2

	stringtable:=pcm_alloc(ichar.bytes*stringtablesize)
	stringlabtable:=pcm_alloc(int32.bytes*stringtablesize)

	for i:=1 to nstrings do
		stringtable[i]:=oldstringtable[i]
		stringlabtable[i]:=oldstringlabtable[i]
	od

	pcm_free(oldstringtable,ichar.bytes*oldstringtablesize)
	pcm_free(oldstringlabtable,int32.bytes*oldstringtablesize)
end

proc extendrealtable=
	ref[]real oldrealtable
	ref[]int32 oldreallabtable
	int oldrealtablesize

	oldrealtablesize:=realtablesize
	oldrealtable:=realtable
	oldreallabtable:=reallabtable

	realtablesize*:=2

	realtable:=pcm_alloc(real.bytes*realtablesize)
	reallabtable:=pcm_alloc(int32.bytes*realtablesize)

	for i:=1 to nreals do
		realtable[i]:=oldrealtable[i]
		reallabtable[i]:=oldreallabtable[i]
	od

	pcm_free(oldrealtable,real.bytes*oldrealtablesize)
	pcm_free(oldreallabtable,int32.bytes*oldrealtablesize)
end

proc asmstr(ichar s)=
	gs_str(dest,s)
end

proc asmchar(int c)=
	gs_char(dest,c)
end

global function getfullname(psymbol d)ichar=
	static [256]char str
	ichar name:=d.name

	if d.reg then
		fprint @str,"#.#",(d.isfloat|"X"|"R"), name
		return str
	fi

	if d.istruename and d.isimported then
		strcpy(str,"`")
		strcat(str,name)
		strcat(str,"*")
	elsif d.isimported then
		strcpy(str,name)
		strcat(str,"*")
	else		
		return name
	fi
!	esac
end 

global function getfulltempname(int tempno)ichar=
	RETURN "TEMP"
end 

export proc merror(ichar mess,ichar param="")=
!	fprintln "MCL Error: # (#) on Line: # in #",mess,param
	fprintln "MCL Error: # (#) [#]",mess,param,mseqno
!		mlineno iand 16777215, sourcefilenames[mlineno>>24]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

export proc merrort(ichar mess,int t)=
	fprintln "MCL Type not supported: # (#) [#]",mess,pstdnames[t], mseqno
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype], mseqno
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc genstringtable=
	int col

	setsegment('I',8)

	if kk0used then
		genmc(m_label,mgenlabel(kk0used))
		gendb(0)
	fi
	return unless nstrings

	for i to nstrings do
		genmc(m_label,mgenlabel(stringlabtable[i]))
		genstring(stringtable[i],1)
	od
end

global proc genstring(ichar s, int doterm)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
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
	genmc(m_db,mgenint(a))
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_db,mgenstring(s,length))
end

proc gendq(int a)=
	genmc(m_dq,mgenint(a))
end

proc gendqname(psymbol d)=
	genmc(m_dq,mgenmemaddr(d))
end

proc gendqlabel(int lab)=
	genmc(m_dq,mgenlabel(lab))
end

global proc genrealtable=
	real x

	return unless nreals

	mgencomment("Real Table")
	setsegment('I',8)

	for i to nreals do
		genmc(m_label,mgenlabel(abs(reallabtable[i])))
		x:=realtable[i]

		if reallabtable[i]>0 then
			genmc(m_dq, mgenrealimm(x,8))
		else
			genmc(m_dd, mgenrealimm(x,4))
		fi
	od
end

global proc genabsneg=
	setsegment('I',16)

	if lababs32 then
		mgencomment("lababs32")
		genmc(m_label,mgenlabel(lababs32))
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mgencomment("lababs64")
		genmc(m_label,mgenlabel(lababs64))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mgencomment("labneg32")
		genmc(m_label,mgenlabel(labneg32))
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mgencomment("labneg64")
		genmc(m_label,mgenlabel(labneg64))
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mgencomment("labzero")
		genmc(m_label,mgenlabel(labzero))
		gendq(0)
	fi

	if labmask63 then
		mgencomment("mask63/offset64")
		genmc(m_label,mgenlabel(labmask63))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc(m_label,mgenlabel(laboffset64))
		gendq(0x43E0'0000'0000'0000)
	fi
end

global function mdefinelabel:int =
	genmc(m_label,mgenlabel(++mlabelno))
	return mlabelno
end

global function mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label,mgenlabel(lab))
end

global function stropnd(mcloperand a,int sizeprefix=0,debug=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus,t
	int offset,tc

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
		strcpy(&.str,strvalue(a))

	when a_mem then
		case a.valtype
		when intimm_val then
			strcpy(&.str,strint(a.value))
		when realimm_val then
			strcpy(&.str,strreal(a.xvalue))
		when realmem_val then
			fprint @&.str,"M#",a.xvalue
		esac

		strcat(&.str,getsizeprefix(a.size,sizeprefix))
		strcat(&.str,"[")

		plus:=""
		if a.reg then
			strcat(&.str,strreg(a.reg,8))
			plus:="+"
		fi
		if a.regix then
			strcat(&.str,plus)
			strcat(&.str,strreg(a.regix,8))
			plus:="+"
			if a.scale>1 then
				strcat(&.str,"*")
				strcat(&.str,strint(a.scale))
			fi
		fi

		if a.valtype in [def_val,label_val, temp_val] then
			if plus^='+' then
				strcat(&.str,plus)
			fi
			strcat(&.str,strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2,offset:"+"
			strcat(&.str,&.str2)
		fi
		strcat(&.str,"]")

	when a_xreg then
		return strxreg(a.reg,a.size)

	else
		println "BAD OPND",A.MODE
		return "<BAD OPND>"
	esac

	return &.str
end

function strreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,0)

	if size=8 and d then
		return getfullname(d)
	else
		getregname(reg,size)
	fi
end

function checkregvar(int reg, isfloat)psymbol d=
	pcl p
!CPL "CHECKREGVAR",CURRASMPROC!.NAME
	if currasmproc=nil then return nil fi
	p:=currasmproc.pcldef+1

	while p.opcode<>kendproc, ++p do
		if p.opcode in [klocal, kparam] then
			d:=p.def
			if d.reg=reg then
!CPL D.NAME,=PSTDNAMES[P.MODE],=D.REG, REG
				if isfloat and pfloat[p.mode] then return d fi
				if not isfloat and not pfloat[p.mode] then return d fi
			fi
		fi
	od
!	od
	return nil
end

function strxreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,1)

	if size=8 and d then
		return getfullname(d)
	else
		return fgetregname(reg,size)
	fi
end

global function strvalue(mcloperand a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	int64 value,offset,length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(&.str,"")

	case a.valtype
	when def_val then
		strcat(&.str,getfullname(def))

	addoffset::
		if offset:=a.offset then
			print @&.str2,(offset>0|"+"|""),,offset
			strcat(&.str,&.str2)
		fi

	when temp_val then
		strcat(&.str,getfulltempname(a.tempno))

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

	when syscall_val then
		strcat(&.str,"XXX")

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

!	else
	esac

	return &.str

end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

global function ismemaddr(int n)int=
	if pclstack[n].fmt=imm_memaddr then return 1 fi
	return 0
end

global function isimm64(int n)int=
	if pclstack[n].fmt=imm_d64 then return 1 fi
	return 0
end

global function isregvaropnd(int n)int=
	if pclstack[n].loc=regvar_loc then return 1 fi
	return 0
end

global proc copyblock(mcloperand ax,bx, int n)=
	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg


	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg

	offset:=0

	if 1<=nwords<=4 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)
		bx:=changeopndsize(bx,targetsize)

		to nwords do
			genmc(m_mov,rx,applyoffset(bx,offset))
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop
		rcount:=mgenreg(countreg:=getnextreg())	!count
		lab:=++mlabelno

		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)

		genmc(m_mov,rcount,mgenint(nwords))
		genmc(m_label,mgenlabel(lab))
		genmc(m_mov,rx,bx)
		genmc(m_mov,ax,rx)

		genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))
		genmc(m_add,mgenreg(bx.reg),mgenint(targetsize))

		genmc(m_dec,rcount)
		genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

		offset:=0
		freereg(countreg)
	fi

	if oddbytes then
		n:=oddbytes						!1..7

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

	freereg(workreg)
end

function makesimpleaddr(mcloperand ax)mcloperand bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg

	if ax.reg and not ax.regix then return ax fi
	newreg:=(ax.reg | ax.reg | (ax.regix | ax.regix | getnextreg()))
	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
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

global function getrtsproclabel(int fnindex)int=
	if rtsproclabels[fnindex]=0 then
		rtsproclabels[fnindex]:=++mlabelno
		return mlabelno
	fi
	return rtsproclabels[fnindex]
end

global proc genrtsproctable=
	[256]char str
	int proclab
	psymbol d

	setsegment('C',16)

	for i to rtsnames.len when rtsproclabels[i] do
		print @str,"Generating code for",rtsnames[i]

		mgencomment(str)
		genmc(m_label, mgenlabel(rtsproclabels[i]))

		if not rtsproctable[i] then

!			case i
!			when rts_mul_i128 then
!				genrts_mul_i128()
!			when rts_div_i128 then
!				genrts_div_i128()
!			else
!				cpl rtsnames[i]
!				merror("RTS fn not defined")
!!				mgencomment("Code not ready")
!			esac
!			mgencomment("")
		else
			genmc(m_jmp, mgenmemaddr(rtsproctable[i]))
		fi
	od
end

proc genrtsentry=
	genmc(m_push, dframeopnd)
	genmc(m_mov, dframeopnd, dstackopnd)
	genmc(m_sub, dstackopnd, mgenint(32))
end

proc genrtsexit=
	genmc(m_add, dstackopnd, mgenint(32))
	genmc(m_pop, dframeopnd)
	genmc(m_ret)
end

global proc clearblock(mcloperand ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg


	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg
	genmc(m_xorx,rx,rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)

		to nwords do
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop

!SPLIT INTO TWO VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov,rcount,mgenint(nwords))
			genmc(m_label,mgenlabel(lab))
			genmc(m_mov,ax,rx)

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		else
			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)
			genmc(m_mov,rcount,mgenint(nwords/4))
			genmc(m_label,mgenlabel(lab))

			for i to 4 do
				genmc(m_mov,applyoffset(ax,offset),rx)
				offset+:=8
			od

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize*4))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		fi
	fi

	if oddbytes then
		n:=oddbytes						!1..7

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

global proc genfunctiontable=
	[256]char str
	ichar s,t
	pcl currpcl
	int firststringlab,nextlab,nprocs

	if lab_funcaddrtable=0 then return fi
	mgencomment("Function Table")
	nprocs:=0

	setsegment('C',16)
	genmc(m_label, mgenlabel(lab_funcaddrtable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			genmc(m_dq,mgenmemaddr(currpcl.def))
			++nprocs
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	firststringlab:=0

	genmc(m_label, mgenlabel(lab_funcnametable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			if firststringlab=0 then
				firststringlab:=nextlab:=++mlabelno
			else
				nextlab:=++mlabelno
			fi

			genmc(m_dq,mgenlabel(nextlab))
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	nextlab:=firststringlab
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			genmc(m_label,mgenlabel(nextlab))
			s:=currpcl.def.name
			t:=s

			while s^ do
				if s^='.' then
					t:=s+1
				fi
				++s
			od
			genstring(t,1)
			++nextlab
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	genmc(m_label, mgenlabel(lab_funcnprocs))
	genmc(m_dq, mgenint(nprocs))
end

global function mgenextname(ichar s)mcloperand=
	[64]char str
	psymbol d

	strcpy(&.str,s)
	str[strlen(s)]:=0

	d:=pcm_allocz(pstrec.bytes)
	
	d.name:=pcm_copyheapstring(&.str)
	d.isimported:=1

	return mgenmemaddr(d)
end

export proc pcl_setasmhandler(ref void fnaddr)=
	hostasmhandler:=fnaddr
end

global proc mgeninfo(ichar s, int value)=
	[256]char str
	fprint @&.str,"# #",s,value
	genmc_str(m_comment,&.str)
end

global proc mgeninfos(ichar s, svalue)=
	[256]char str
	fprint @&.str,"# #",s,svalue
	genmc_str(m_comment,&.str)
end

=== pc_libpcl.m 0 0 9/25 ===
global pcl pcstart			!start of pcl block
export pcl pccurr			!point to current pcl op
global pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

!const fshowsymbols=1
const fshowsymbols=0

int initpcalloc=65536
!int initpcalloc=65536*4
!int initpcalloc=4*1048576
!int initpcalloc=16*1048576

const pcelemsize = pclrec.bytes

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

!global const maxlabels=10000			!in one file
!global const maxlabels=100000			!in one file
!global const maxlabels=800000			!in one file
global const maxlabels=6000000			!in one file
export [maxlabels]int32 labelmap				!map user labels to global labels
export int plabelno					!current highest global labelno
global int maxuserlabel					!set by lex: highest #label
global int labelnooffset				!normally 0; set to offset (maxlabelno) when processing rts
GLOBAL INT NPCL

!const maxgloballabels=50000				!in all files
!const maxgloballabels=100000				!in all files
!const maxgloballabels=800000				!in all files
const maxgloballabels=6000000				!in all files
[maxgloballabels]pcl labeloffset		!pcl addr of label

global ichar longstring					!used in stropnd
global int longstringlen
global ichar errormess

global int mcldone

export proc pcl_start(int nunits=0)=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

	pcalloc:=initpcalloc

	if nunits then				!use approx alloc of 10% more
		nunits:=nunits*9/8		!approx expected number of pcl ops
		while pcalloc<nunits do
			pcalloc*:=2
		od
	fi

!CPL =PCALLOC,=NUNITS,=PCELEMSIZE

	pcstart:=pcm_allocz(pcalloc*pcelemsize)
	pcend:=pcstart+pcalloc-8

	pccurr:=pcstart-1
	pcfixed:=0
	pcseqno:=0
	pcneedfntable:=0

	plabelno:=maxuserlabel:=labelnooffset:=0
	mcldone:=0

	clear rtsproctable

	pstentrypoint:=nil

!CLEAR USED PORTIONS OF LABELMAP/LABELOFFSET FOR NEXT FILE
!...
end

export proc pcl_end(int fixup=0)=

	if pccurr>=pccurr and pccurr.opcode<>kendprogram then
		pcl_gen(kendprogram)
	fi	

	if fixup then
		fixuppcl()
	fi
	
!destroy assets associated with pcldescr, which should be the values
!currently in pcstart etc
!note that the values in pcldescr will likely be out of date
end

export proc pcl_free(int fixup)=
!destroy resources used for pcl data

	pcstart:=pccurr:=pcend:=nil
	pcfixed:=0
end

global proc fixuppcl=
	psymbol d
	pcl p, pextproc
	int labno, insideproc, extparams,isthreaded

	return when pcfixed

!CPL "FIXUPPCL"

	p:=pcstart
	insideproc:=0
	pextproc:=nil

	while p<=pcend, ++p do
		switch p.opcode
		when klocal, kparam then
			if insideproc<>1 then perror("Not allowed") fi
			d:=p.def
			d.pclop:=p.opcode
			d.pcldef:=p
			if d.isdefined then perror_s("Redefining local/param/ext:",d.name) fi
			d.isdefined:=1

		when kprocdef then
			isthreaded:=0
doprocdef::
			if insideproc then perror("Nested proc") fi
			insideproc:=1
			d:=p.def
			d.pclop:=p.opcode
			d.pcldef:=p
			if d.isdefined then perror_s("Redefining proc:",d.name) fi
			d.isdefined:=1
			d.isthreaded:=isthreaded
			assignlabeltoname(d)
		when kistatic, kzstatic then
			d:=p.def
			d.pclop:=p.opcode
			d.pcldef:=p
			if d.isdefined then perror_s("Redefining proc/static/label:",d.name) fi
			d.isdefined:=1
			assignlabeltoname(d)

		when kendproc then
			if insideproc<>1 then perror("End without proc") fi
			insideproc:=0

		when kendextproc then
			if insideproc<>2 then perror("End without extproc") fi
			insideproc:=0

		when kextproc then
			pextproc:=p
			extparams:=0
			if insideproc then perror("Nested proc") fi
			insideproc:=2
doextproc::
			d:=p.def
			if not d.isdefined then
				d.pclop:=p.opcode
				d.pcldef:=p
				if d.isdefined then perror_s("Redefining extproc:",d.name) fi
				d.isdefined:=1
				d.isimported:=1
			fi

		when kextparam then
			if insideproc<>2 then perror("Not allowed") fi
			++extparams

		when kextvariadics then
			if insideproc<>2 then perror("Not allowed") fi
			p.extvariadics:=extparams

		when kextvar then
			goto doextproc

		when kthreadedproc then
			isthreaded:=1
			doprocdef

		when klabelname then
			labno:=p.labelno
			if labno not in 1..maxlabels then
				perror_s("3:Label out of range",strint(labno))
			fi
			p.labelno:=fixlabel(labno)
			p.opndtype:=label_opnd

		when kdb then p.opcode:=kdata; p.mode:=tpu8;  p.size:=1
		when kdw then p.opcode:=kdata; p.mode:=tpu16; p.size:=2
		when kdd then p.opcode:=kdata; p.mode:=tpu32; p.size:=4
		when kdq then p.opcode:=kdata; p.mode:=tpu64; p.size:=8
		when kdata then if p.size=0 then p.size:=psize[p.mode] fi

		when kimportdll, kimportlib then
!CPL "IMPORTDLL",P.DEF
			goto doextproc

		elsecase p.opndtype
		when real_opnd then
			if p.mode=tpr32 or p.size=4 then
				p.opndtype:=real32_opnd
				p.xvalue32:=p.xvalue
			fi
		when label_opnd then
			p.labelno:=fixlabel(p.labelno)
		when kgetnprocs, kgetprocname, kgetprocaddr then
			pcneedfntable:=1

		end
	od

	if insideproc then perror("End missing") fi

	pcfixed:=1

	int undef:=0

!CPL "ALL SYMBOLS"
	d:=pallsymbols
	while d, d:=d.nextsym do

		if d.pcldef=nil then
			++undef
			println "Undefined:", d.name,REF VOID(D.NAME)
!
!		fprintln "# def:# imp:# exp:# sym:# pcldef:#",
!			D.NAME:"20JL",d.isdefined, d.isimported,d.isexported,
!			D.symbol, d.pcldef
		fi
	od


!	if undef then
!		perror_s("Undefined names:",strint(undef))
!	fi
end

proc extendpclblock=
	int newpcalloc, lengthused
	pcl newpcstart

	newpcalloc:=pcalloc*2
	lengthused:=pccurr-pcstart+1

	newpcstart:=pcm_alloc(pcelemsize*newpcalloc)

CPL "EXTEND PCL",NEWPCALLOC

	memcpy(newpcstart,pcstart, lengthused*pcelemsize)
	pcm_clearmem(newpcstart+lengthused,(newpcalloc-lengthused)*pcelemsize)

	pccurr:=newpcstart+(pccurr-pcstart)
	pcend:=newpcstart+newpcalloc-8

	pcm_free(pcstart,pcalloc*pcelemsize)

	pcstart:=newpcstart
	pcalloc:=newpcalloc
end

global function newpcl:pcl =
	if pccurr>=pcend then
		extendpclblock()
	fi

++NPCL


	++pccurr
	pccurr.seqno:=++pcseqno
	return pccurr
end

export proc pcl_gen(int opcode, pcl p=nil) =
	psymbol d

	if p=nil then
		p:=newpcl()
	fi

!	case opcode
!	when KLABELNAME THEN PERROR("PCLGEN/LABELNAME")
!	esac

	p.opcode:=opcode
end

export proc pcl_gent(int opcode, t, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	if t<0 then
		p.mode:=tpblock
		p.size:=-t
	else
		p.mode:=t
	fi
end

export proc pcl_genx(int opcode, int x, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
end

export proc pcl_genxy(int opcode, int x,y, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

export function pcl_genint(int a,mode=tpi64)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
!	p.mode:=mode
	return p
end

export function pcl_genreal(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=real_opnd
	return p
end

export function pcl_genreal32(real x)pcl p=
	p:=newpcl()
	p.xvalue32:=x
	p.opndtype:=real32_opnd
	return p
end

export function pcl_genstring(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=string_opnd
	return p
end

export function pcl_genlabel(int a)pcl p=
	p:=newpcl()
	p.labelno:=a
	p.opndtype:=label_opnd
	return p
end

export function pcl_genmem(psymbol d)pcl p=
	p:=newpcl()
	p.def:=d
	p.opndtype:=mem_opnd
	return p
end

export function pcl_genmemaddr(psymbol d)pcl p=
	p:=newpcl()
	p.def:=d
	p.opndtype:=memaddr_opnd
	return p
end

export proc pcl_gencomment(ichar s)=
	pcl_gen(kcomment,pcl_genstring(s))
end

export function pcl_genname(ichar s)pcl=
	return pcl_genmem(pcl_makesymbol(s))
end

export function pcl_gennameaddr(ichar s)pcl=
	return pcl_genmemaddr(pcl_makesymbol(s))
end

export function pcl_genassem(ref void code)pcl p=
	p:=newpcl()
	p.asmcode:=code
	p.opndtype:=assem_opnd
	return p
end

export function pcl_makesymbol(ichar s)psymbol d =
!CPL "MAKESYM",S
	d:=addnamestr(s)
	return d
end

global proc strpcl(pcl p)=
	[256]char pmodestr
	[256]char str
	int opcode,defused

	const showformatted=1

	opcode:=p.opcode

!CPL PCLNAMES[OPCODE]




	case opcode
	when klabel then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
		strlabel(p.labelno,1)
		return
	when kcomment then
		if p.svalue^ then
			psstr("!")
			psstr(p.svalue)
		fi
		return
	when kprocdef, kthreadedproc then
		if p.def.isrts then
			psstr("Procrts")
		elsif opcode=kthreadedproc then
			psstr("Threadedproc")
		else
			psstr("Proc")
		fi
		psstr(" ")
		psname(p.def)
		psstr((p.isexported|"::"|":"))
		if p.mode then
			psstr(" ")
			psstr(strpmode(p.mode,p.size))
		fi
		return

	when kendproc then
		psstr("End")
		psline()
		return

	when kendextproc then
		psstr("Endext")
		psline()
		return

	when kextproc then
		psstr("Extproc")
		psstr(" ")
		psname(p.def)
		if p.mode then
			psstr(" ")
			psstr(strpmode(p.mode,p.size))
		fi
		return


	when klabelname then
		psname(p.def)
		psstr((p.def.isexported|"::"|":"))
		return

	when kendprogram then
		psstr("Endprogram")
		return

	esac

	psstr("    ")
	strcpy(str,pclnames[opcode]+1)
	gs_leftstr(dest,str,15)

	if p.opndtype<>no_opnd then
		psstr(stropnd(p))
		psstr(" ")
!	else
!		pstabto(
	fi
	pstabto(30)

	if p.mode<>tpvoid then
		psstr(strpmode(p.mode, p.size))
		psstr(" ")
	fi

	if pclhastype[opcode]=2 then
		if p.mode=tpvoid then
			psstr("void")
		fi
		psstr(strpmode(p.oldmode,p.size))
		psstr(" ")
	fi

	if pclextra[opcode] then
		psint(p.x)
		if pclextra[opcode]=2 then
			psstr(" ")
			psint(p.y)
		fi
	fi

!	if p.isglobal then psstr(" Isglobal") fi
!	if p.isvariadic then psstr(" Isvariadic") fi
end

global function stropnd(pcl p)ichar=
	static[512]char str
	int length
	psymbol d

	if p=nil then
		return ""
	fi

	case p.opndtype
	when int_opnd then
		return strint(p.value)
	when real_opnd then
		print @str,p.xvalue:"e16.16"

	when real32_opnd then
		print @str,p.xvalue32:"e16.16"

	when string_opnd then
		if (length:=strlen(p.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1)
			strcat(str,"""")
		else
			if longstring then
				pcm_free(longstring,longstringlen)
			fi
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(p.svalue, longstring+1)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			return longstring
		fi

	when mem_opnd then
		d:=p.def
		print @str,(d.istruename|"`"|""),,d.name
		if p.opcode in [kistatic, kzstatic] then
			strcat(str,":")
			if p.isexported then
				strcat(str,":")
			fi
		fi

	when memaddr_opnd then
		d:=p.def
		fprint @str,"&##",(d.istruename|"`"|""),d.name

	when label_opnd then
		fprint @str,"## ","#",p.labelno

	when no_opnd then
		return ""

	when assem_opnd then
		return strint(int(p.asmcode))

	else
		println OPNDNAMES[P.OPNDTYPE]
		return "<PCLOPND?>"
	esac

	return str
end

proc psstr(ichar s)=
	gs_str(dest,s)
end

proc psline=
	gs_line(dest)
end

proc psint(int a)=
	gs_str(dest,strint(a))
end

proc psname(psymbol d)=
	if d.istruename then
		gs_str(dest,"`")
	fi
	gs_str(dest,d.name)
end

proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

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

proc strlabel(int labelno,colon=0)=
	psstr("#")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

proc psopnd(pcl p)=
	psstr(stropnd(p))
end

global function strpmode(int m, size)ichar=
	static [64]char str
	if m<>tpblock then
		strcpy(str, pstdnames[m])
	else
		fprint @str,"#:#", pstdnames[m],size
	fi
	return str
end

global proc writepcl(pcl p)=
!	gs_strint(dest,p.seqno)
!	gs_str(dest," ")
	strpcl(p)
	gs_line(dest)
end

global function writeallpcl:ichar,int=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d,e

	gs_init(dest)
	destlinestart:=dest.length

	p:=pcstart

	while p<=pccurr do
		writepcl(p)

		destlinestart:=dest.length
		++p
	od

	if fshowsymbols then
		writesymbols()
	fi

	if longstring then
		pcm_free(longstring,longstringlen)
	fi
	return (dest.strptr,dest.length)
end

global function nextlabel:int=
	if plabelno>=maxgloballabels then
		pclerror("Too many global labels")
	fi
	++plabelno
end

global function fixlabel(int userlab)int=
	if userlab not in 1..maxlabels then
		perror_s("Label no out of range:",strint(userlab))
	fi
	if labelmap[userlab]=0 then
		labelmap[userlab]:=nextlabel()
		return plabelno
	else
		return labelmap[userlab]
	fi
end

global proc pdefinelabel(int label,seqno=0)=
!define new global label to point just after pccurr
!label must be in range
	if labeloffset[label] then
		pclerror("Redefining label:",strint(label))
	fi
	pcl_gen(klabel, pcl_genlabel(label))
	pccurr.seqno:=seqno

	labeloffset[label]:=pccurr
end

global proc assignlabeltoname(psymbol d)=
!isdef=1 means can't already be assigned
	if d.labelno then
!		if isdef then
!			pclerror("Duplicate name def:",d.name,lineno)
!		fi
		return
	fi
	d.labelno:=nextlabel()
end

global proc pclerror(ichar mess,param=nil,int lineno=0)=
	print "PCC error:",mess
	if param then
		print param
	fi
	if lineno then
		print " on line:",lineno
	fi
	println
	stop 1
end

global function getpclstr(pcl p)ichar=
	gs_init(dest)
	destlinestart:=dest.length

	strpcl(p)
	(dest.strptr+dest.length)^:=0
	return dest.strptr
end

export function pcl_getopcode:int=
	return pccurr.opcode
end

export proc pcl_setopcode(int opc)=
	pccurr.opcode:=opc
end

export proc pcl_settype(int t,size=0)=
	pccurr.mode:=t
	pccurr.size:=size
	if t<0 then
		pccurr.mode:=tpblock
		pccurr.size:=-t
	fi
end

export proc pcl_setxy(int x,y)=
	pccurr.x:=x
	pccurr.y:=y
end

export proc pcl_setscale(int scale)=
	pccurr.scale:=scale
end

export proc pcl_setoffset(int offset)=
	pccurr.extra:=offset
end

export proc pcl_addoffset(int offset)=
	pccurr.extra+:=offset
end

export proc pcl_setincr(int n)=
	pccurr.stepx:=n
end

export proc pcl_setnargs(int n)=
	pccurr.nargs:=n
end

export proc pcl_setnmult(int n)=
	abortprogram("SETNMULT")
end

export proc pcl_setrettypes(ref[]int types, int n)=
	abortprogram("SETRETTYPES")
end

export proc pcl_setexported(int x)=
	psymbol def:=pccurr.def

	pccurr.isexported:=x
	if def then
		def.isexported:=x

!CPL "SETEXPORTED:",DEF.NAME,X,=DEF=PSTENTRYPOINT
!		if def=pstentrypoint then
!			def.name:=pcm_copyheapstring(getbasename(def. 

	fi
end

export proc pcl_isthreaded(int x)=
!	pccurr.isthreasexported:=1
	if pccurr.def then
		pccurr.def.isthreaded:=x
	fi
end

export proc pcl_setnvariadics(int n)=
	pccurr.nvariadics:=n
end

export proc pcl_setalign(int n)=
	pccurr.align:=n
end

export proc pcl_setrtsproc=
	if pccurr.def and pccurr.opcode=kprocdef then
		definertsproc(pccurr.def)
	fi
end

export proc pcl_setoldtype(int t)=
	pccurr.oldmode:=t
end

export proc pcl_setpos(int pos)=
	abortprogram("SETPOS")
end

export function pcl_lasterror:ichar=
	return errormess
end

export function pcl_writepclfile(ichar filename)int=
	ichar source
	int length

	fixuppcl()

	(source,length):=writeallpcl()

	return writefile(filename,source,length)
end

export proc pcl_addlibfile(ichar filename, int libtype)=
	ichar source
	int length

!CPL "PCL ADDLIBFILE",FILENAME,LIBTYPE:"C"

	for i to nplibfiles do
		if eqstring(filename,plibfiles[i]) then		!already present
			return
		fi
	od

	if nplibfiles>maxplibfile then
		perror("PCL:Too many libs")
	fi
	plibfiles[++nplibfiles]:=filename
	plibtypes[nplibfiles]:=libtype
end

export proc pcl_setentrypoint(psymbol d, int libmode=0)=
	pstentrypoint:=d
	plibmode:=libmode
end

export proc pcl_addsym(psymbol d)=
	if pstready then

!IF D.NAME=NIL THEN
!CPL "ADDSYM NULL NAME:",D, D.SYMBOL
!!ELSE
!!CPL "ADDSYM",D.NAME
!FI

		d.nextsym:=pallsymbols
		pallsymbols:=d
	fi
end

global proc perror(ichar mess)=
	perror_s(mess, nil)
end

global proc perror_s(ichar mess, param=nil)=
	print "PCL error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

global proc writesymbols=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d,e

	p:=pcstart

	psline()
	psstr("PROC PCL DEF OPS")
	psline()

	while p<=pccurr, ++p do
		case p.opcode
		when kprocdef, kistatic, kzstatic, kextproc, klabelname,
			klocal, kparam then

			d:=p.def
PSINT(INT(p))
PSSTR(" ")
PSINT(INT(d))
PSSTR(" ")

			psstr(d.name)
			psstr(": ")
			psint(p.seqno)
			psline()

			psstr("	Opcode:"); psstr(pclnames[p.opcode]); psline()
			psstr("	PCLdef:"); psstr((d.pcldef|pclnames[d.pcldef.opcode]|"---")); psline()
			psstr("	Isdefined:"); psint(d.isdefined); psline()
			psstr("	Isexported:"); psint(d.isexported); psline()
			psstr("	Isimported:"); psint(d.isimported); psline()
			psstr("	Extvariadics:"); psint(p.extvariadics); psline()
			psstr("	Isaddrof:"); psint(d.addrof); psline()
			psstr("	Label#:"); psint(d.labelno); psline()

		esac
	od

	p:=pcstart

	psline()
	psstr("PROC PCL UNDEFINED MEM REFS")
	psline()

!CPL =PCLCODE, =PCLCURR

	while p<=pccurr, ++p do
		if p.opndtype in [mem_opnd, memaddr_opnd] and not p.def.isdefined then
			d:=p.def
PSINT(INT(p))
PSSTR(" ")
PSINT(INT(d))
PSSTR(" ")
			psstr("Not defined: ")
			psstr(d.name)
			psstr(" ")
			psint(p.seqno)
			psline()
			d.isdefined:=1
		fi
	od
end

global function getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

global proc definertsproc(psymbol d)=
!request made that d is an RTS proc
	ichar s:=d.name
	int c

!look for rts name following $; just scan LTR for first $
	while (c:=s++^)<>'$' do od

	for i to rtsnames.len do
		if eqstring(rtsnames[i]+4, s) then
			d.isrts:=1
			d.rtsindex:=i
			rtsproctable[i]:=d
			return
		fi
	else
	od
end

global function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
	if n iand (align-1)=0 then return n fi
	return n+(align-(n iand (align-1)))
end

=== pc_objdecls.m 0 0 10/25 ===
global record imagefileheader =
	word16	machine
	word16	nsections
	word32	timedatestamp
	word32	symtaboffset
	word32	nsymbols
	word16	optheadersize
	word16	characteristics
end

global record imagedir =
	word32	virtualaddr
	word32	size
end

global record optionalheader =			!exe/dll only
	word16  magic
	byte     majorlv
	byte     minorlv
	word32 codesize
	word32 idatasize
	word32 zdatasize
	word32 entrypoint
	word32 codebase
!	word32 datebase		!32-bit exe files only
	word64	imagebase
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
		word32	physical_address
		word32	virtual_size
	end
	word32	virtual_address
	word32	rawdata_size
	word32	rawdata_offset
	word32	relocations_ptr
	word32	linenos_offset
	word16	nrelocs
	word16	nlinenos
	word32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			word32	shortx
			word32	longx
		end
		word64 longname
	end
	word32	value
	int16	sectionno
	word16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	word32	implookuprva
	word32	timedatestamp
	word32	fwdchain
	word32	namerva
	word32	impaddressrva
end

global record coffrelocrec =
	int32	virtualaddr
	int32	stindex
	int16	reloctype
end

global tabledata() [0:]ichar relocnames =
	(abs_rel = 0,	$),
	(addr64_rel,	$),
	(addr32_rel,	$),
	(addr32nb_rel,	$),
	(rel32_rel,		$),
	(rel321_rel,	$),
	(rel8_rel,		$),				!used within assembler only, not in coff format
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
		ref dbuffer data		!copy of ss_zdata etc
		ref byte bytedata		!added later, eg, import dir block
	end
	ichar name					!name like ".bss" as it will be in obj/exe file
	int segtype					!code_seg etc
	int rawsize					!in file
	int rawoffset				!offset in exe file
	int virtsize				!in image
	int virtoffset				!offset from imagebase
	ref relocrec relocs			!for idata/code: reloc info needs to be processed
	int nrelocs					!
end

global record importrec = 				!details about all imported symbols
	psymbol def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	psymbol def				!full st entry
	ichar name					!name of symbol (extracted from lib.name if needed)
end

global record dllrec =					!all imported libraries
	ichar name					!name of library, including .dll
	int nprocs					!no. of imports which use this library
	int nametableoffset			!start of name table in impdir
	int addrtableoffset			!start of addr table (IAT)
	int dllnameoffset			!offset of name within impdir
	int dllextraoffset			!offset of mysterious region just before the name
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
=== pc_optim.m 0 0 11/25 ===
import clib
import mlib
import* pci_core
import* pci_mcl


global proc peephole(ref mclrec m)=
	ref mclrec m2,m3,mtarget,lastmcl
	int lab1,lab2
STATIC INT COUNT=0

	lastmcl:=nil

!RETURN
	if optimflag<2 then return fi

	do
!ADDNOTE("XXXXXX")
		m2:=m.nextmcl
!CPL =M,=M2,MCLNAMES[M.OPCODE]
		while m2 and m2.opcode in [m_comment, m_deleted] do m2:=m2.nextmcl od

		switch m.opcode
		when m_procstart then
!CPL "PEEPHOLE",M.A.DEF.NAME

		when m_procend then
			exit

		when m_jmp then
dojmp::
			if m.a.valtype<>label_val then skip fi
!CPL VALTYPENAMES[M.A.VALTYPE]
			mtarget:=labeltable[m.a.labelno].nextmcl
			while mtarget.opcode=m_label do mtarget:=mtarget.nextmcl od
			if mtarget.opcode=m_jmp then
				m.a:=mgenlabel(mtarget.a.labelno)
			fi

			if m.opcode=m_jmp and m2.opcode=m_jmp then
				deletemcl(m2,101)
			fi


		when m_jmpcc then
			if m2.opcode<>m_jmp then goto dojmp fi
!jcc followed by jmp; detect jcc L1; jmp L2; L1: and replace with:
! jncc L2; <deleted>; L1
			lab1:=m.a.labelno
			m3:=m2.nextmcl
			if m3.opcode=m_label and m3.a.labelno=lab1 then
				m.a:=mgenlabel(m2.a.labelno)
				m.cond:=asmrevcond[m.cond]
				deletemcl(m2,102)
			fi

		when m_test then
			case lastmcl.opcode
			when m_andx, m_orx, m_xorx then
				if sameregopnd(m.a,m.b) and sameregopnd(m.a,lastmcl.a) then
					deletemcl(m,103)
!					m.opcode:=m_deleted
				fi
			esac

		when m_movzx then
!CPL "MOVZX",M.LINENO,SOURCEFILEPATHS[M.FILENO]
!ADDNOTE("MOVZX",M)
			if m.a.mode=a_reg and m.a.size=8 and m.b.size<4 then
				m.a:=changeopndsize(m.a,4)
			fi
!			if m2.opcode=m_test and isreg(m2.a,r0) and isreg(m2.b,r0) and
!					m2.nextmcl.opcode=m_jmpcc then
!				m.opcode:=m_cmp
!				m.a:=m.b
!				m.b:=mgenint(0)
!!				IF M2.NEXTMCL.REGEND[R0]<>1 THEN
!!					CPL "MOVZX/TEST; FREE NOT SEEN"
!!					ADDNOTE("FREENOT SEEN",M)
!!				FI
!				deletemcl(m2,104)
!ADDNOTE("MOVZX2",M2)
!			fi
!
		when m_mov then
!SKIP2
			if m.a.mode=a_reg and m.a.reg=r10 and m.b.mode=a_reg and m.b.reg<=r1 then
				if lastmcl.a.mode=a_reg and lastmcl.a.reg=m.b.reg and
						lastmcl.opcode in [m_mov, m_movsx, m_movzx, m_lea] then
					lastmcl.a:=mgenreg(r10)
					deletemcl(m,105)
				fi
			fi

			if isreg0(m.a) and isregopnd(m.b) then
				if isreg0(m2.b) and m2.regend[r0] AND M2.A.SIZE=8 then
					m2.b:=m.b
!					addnote("MOV D0,RV;OPC D0... => OPC RV...",m2)
					deletemcl(m,106)
					skip
				fi

				if not isreg0(m2.a) then skip fi
				m3:=m2.nextmcl

				if m2.opcode=m_cmp and m3.opcode=m_jmpcc and m3.regend[r0] then
					m2.a:=m.b
					deletemcl(m,107)
				elsif m2.opcode=m_test and isreg0(m2.b) and
						m3.opcode=m_jmpcc and m3.regend[r0] then
					m2.a:=m.b
					m2.b:=m.b
					deletemcl(m,108)
				elsif m2.opcode in [m_inc, m_dec] and isreg0(m2.a) then
					m.opcode:=m_lea
					m.b:=mgenindex(areg:m.b.reg,offset:(m2.opcode=m_inc|1|-1))
					deletemcl(m2,120)
					redo
				elsif m2.opcode in [m_add, m_sub] and isreg0(m2.a) then
					if isconst(m2.b) and (m2.b.value in int32.minvalue..int32.maxvalue) then
!ADDNOTE("IMM121",M)
						m.opcode:=m_lea
						m.b:=mgenindex(areg:m.b.reg,
							offset:(m2.opcode=m_add|m2.b.value|-m2.b.value))
						deletemcl(m2,121)
						redo
					elsif isregopnd(m2.b) and m2.opcode=m_add then
						m.opcode:=m_lea
						m.b:=mgenindex(areg:m.b.reg,ireg:m2.b.reg)
						deletemcl(m2,122)
						redo
					fi
				fi
			fi

			if isreg0(m.a) and isconst(m.b) and
					 (m.b.value in int32.minvalue..int32.maxvalue) then
				if isreg0(m2.b) and m2.regend[r0] then
					m2.b:=m.b
!ADDNOTE("IMM109",M2)
!					addnote("MOV D0,IMM; OPC XXX,DO => OPC XXX,IMM",M2)
					deletemcl(m,109)
				fi
			fi

!			if isreg0(m.b) and m2.opcode=m_mov and isreg0(m2.a) and sameoperand(m.a,m2.b) then
!!				CPL "RELOADING VALUE JUST STORED",++COUNT
!				deletemcl(m2,140)
!			elsif m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg and

			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg and
				m.b.reg=m2.a.reg and sameoperand(m.a,m2.b) then
!CPL "RELOADING VALUE/NOT REG0",++COUNT
!ADDNOTE("RELOAD VALUE/NOT R0",M2)
				deletemcl(m2,141)
			fi



			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg then
				 if m.a.mode=a_mem and sameoperand(m.a, m2.b) then		!mov [MEM1],Da; mov Db,[MEM1] => mov Db,Da
!CPL "MOV [MEMX],DA; MOV DB,[MEMX] DETECTED",++COUNT
					m2.b:=mgenreg(m.b.reg)
				fi
			fi
		when m_xorx then
			if isreg0(m.a) and isreg0(m.b) then
!CPL =M2.REGEND[R0]
				if isreg0(m2.b) and m2.regend[r0] then
					m2.b:=mgenint(0)
!					ADDNOTE("USE IMM 0",M2)
					deletemcl(m,110)
				fi
			fi

		when m_lea then
			if isreg0(m.a) and m2.opcode=m_mov then
				if isregopnd(m2.a) and isreg0(m2.b) and m2.regend[r0] then
!cpl "THIS"
					m.a:=m2.a
					deletemcl(m2,131)
				fi
			fi

		end switch

skip::
		lastmcl:=m
		m:=m2
	od
end

function isreg(mcloperand a, int reg=rnone)int=
	if not a then return 0 fi
	if a.mode<>a_reg then return 0 fi
	if reg=rnone then return 0 fi
	return reg=a.reg
end

function isreg0(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_reg and a.reg=r0 then return 1 fi
	return 0
end

function isregopnd(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_reg and isregvar[a.reg] then return 1 fi
	return 0
end

function isconst(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_imm and a.valtype=intimm_val then
		return 1
	fi
	return 0
end

proc deletemcl(ref mclrec m,int id=0)=
	[128]char str
!	fprint @&.str,"TO BE DELETED (#)",id

!	fprint @&.str,"DELETED (#)",id
!	addnote(&.str,m)

!if m.opcode=m_label then
!CPL "DELETING LABEL",ID
!FI

	m.opcode:=m_deleted
end
=== pc_parse.m 0 0 12/25 ===
int insideproc

global function parse(ichar source)int=
	psymbol d

	startlex(source)
	int labno

!	repeat
!		lex()
!		printsymbol()
!	until lxsymbol=eofsym
!	STOP

	lxsymbol:=errorsym

	do
		lex()

		case lxsymbol
		when kopcodesym, typesym then
			if lxsubcode=kendprogram then exit fi

			readinstr()

		when labelsym then
			dolabel()

		when namesym then
			SERROR("""NAME:"" NOT SUPPORTED")
!			d:=lxsymptr
!			lex()
!			case lxsymbol
!			when colonsym then
!			when dcolonsym then
!				d.isexported:=1
!			else
!				serror(": expected")
!			esac
!			assignlabeltoname(d)
!			pcl_gen(klabelname,pcl_genmemaddr(d))
!			pccurr.seqno:=lxlineno
!!			pcl_gen(klabel,genlabel(d.labelno))
!			lex()

		when intconstsym then
			next

		when eolsym then
			next
		when eofsym then
			exit
		else
			println =symbolnames[lxsymbol]
			serror("Bad input")
		esac
		checksymbol(eolsym)
	od

	if insideproc then serror("End missing") fi

	return 1
end

proc checksymbol(int symbol)=
	if lxsymbol<>symbol then
		println symbolnames[symbol],"expected, not",symbolnames[lxsymbol],,", on line:",lxlineno
		stop 1
	fi
end

proc dolabel=
	if lxvalue not in 1..maxlabels then
		serror_s("1:Label out of range:",strint(lxvalue))
	fi

	pdefinelabel(lxvalue,lxlineno)
	lex()
	checksymbol(colonsym)
	lex()
end

proc readmode(pcl p)=
	if p.mode<>tpvoid then
		p.oldmode:=lxsubcode
		lex()
		return
	fi

	p.mode:=lxsubcode
	p.size:=psize[lxsubcode]
	lex()
	if p.mode=tpblock then
		checksymbol(colonsym)
		lex()
		checksymbol(intconstsym)
		p.size:=lxvalue
		lex()
	fi
end

proc readinstr=
!I have a bare opcode; see if any other operands follow. Syntax is:
! push[:type[:size]] [operand [:type[:size]] [,A[,B]]]
	int opcode,mode,size
	pcl p

	p:=newpcl()				!all zeros
	p.seqno:=lxlineno
	mode:=tpvoid			!possible override
	size:=0

	if lxsymbol=typesym then
		readmode(p)
		checksymbol(kopcodesym)
	fi

	p.opcode:=opcode:=lxsubcode

	lex()

	if lxsymbol=typesym then
		readmode(p)
	fi

	case pclhasopnd[opcode]
	when 1 then
		case lxsymbol
		when intconstsym then
			p.value:=lxvalue
			p.opndtype:=int_opnd
			mode:=tpi64
			lex()
		when realconstsym then
			p.xvalue:=lxxvalue
			p.opndtype:=real_opnd
			mode:=tpu64
			lex()
		when stringconstsym then
			p.svalue:=pcm_copyheapstring(lxsvalue)
			p.opndtype:=string_opnd
			mode:=tpu64
			lex()
		when labelsym then
			if lxvalue not in 1..maxlabels then
				serror_s("2:Label out of range:",strint(lxvalue))
			fi
			p.labelno:=lxvalue
			p.opndtype:=label_opnd
			lex()
		when namesym then
			p.def:=lxsymptr
			p.opndtype:=mem_opnd
			p.def.istruename:=lxtruename

			lex()

		when addrsym then
			lex()
			checksymbol(namesym)
			p.def:=lxsymptr
			p.def.istruename:=lxtruename
			p.opndtype:=memaddr_opnd
			lex()
			if lxsymbol=mulsym then
				p.def.isimported:=1
				lex()
			fi
		else
			serror("Missing or Bad Operand")
		esac

	when 2 then			!must be name that defines a proc or static or named label
		checksymbol(namesym)
		p.def:=lxsymptr
		if opcode=kprocrts then
			p.opcode:=kprocdef
			definertsproc(p.def)
		fi
		p.opndtype:=mem_opnd
		lex()
		case lxsymbol
		when colonsym then
		when dcolonsym then
			p.def.isexported:=1
			p.isexported:=1
		else
			serror("Colon expected")
		esac
		lex()

	when 3 then			!must be name that defines a local or param
		checksymbol(namesym)
		p.def:=lxsymptr
		p.opndtype:=mem_opnd
		lex()
		if lxsymbol=colonsym then serror(": not used for locals") fi
	when 4 then			!must be name that defines a proc or static or named label
		checksymbol(namesym)
		p.def:=lxsymptr
		p.opndtype:=mem_opnd
		lex()

	esac

	if lxsymbol=typesym then
		readmode(p)
	fi

!if no type used, but available from operand, then use that

	if p.mode=tpvoid then
		if mode<>tpvoid then
			p.mode:=mode
			p.size:=size
		fi
	fi

	if pclextra[opcode] then
		checksymbol(intconstsym)
		p.x:=lxvalue
		lex()
		if pclextra[opcode]=2 then
			checksymbol(intconstsym)
			p.y:=lxvalue
			lex()
		fi
	fi
end

global proc serror(ichar mess)=
	serror_s(mess, nil)
end

global proc serror_s(ichar mess, param=nil)=
	print "Syntax error:",mess
	if param then
		print ":",param
	fi

	println " on line:",lxlineno
	stop 1
end

export function pcl_readpclfile(ichar filename, rtsfile=nil)int=
!note: currently only works one-time
	ichar source

	pcl_start()

	if not parse_readsource(filename) then return 0 fi
	
	if rtsfile then
		labelnooffset:=maxuserlabel
		if not parse_readrts() then
			println "No RTS found"
			return 0
		fi
	fi

	pcl_gen(kendprogram)
	pcl_end()

	return 1
end

global function parse_readsource(ichar filename)int=
	ichar source

	source:=readfile(filename)
	if source=nil then
		errormess:="Can't load file"
		return 0
	fi	

	if not parse(source) then
		errormess:="Couldn't parse"
		return 0
	fi
	return 1
end

global function parse_readrts:int=
	static ichar source = strinclude("rts.pcl")

	if not parse(source) then
		errormess:="Couldn't parse RTS"
		return 0
	fi
	return 1
end
=== pc_runmx.m 0 0 13/25 ===
int nsymimports=0, nsymexports=0

librec lib

global function writememlib(ichar filename)ref librec plib=
	int n, k

	clear lib

!CPL "WRITEMEMLIB",FILENAME

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	lib.version:="0.1234"

	lib.filespec:=filename
	lib.libname:=pcm_copyheapstring(extractbasefile(filename))
	lib.libno:=1

	scansymbols()
	writerelocs()

	lib.zdatasize:=ss_zdatalen
!	lib.zdataptr:=pcm_allocz(ss_zdatalen)
	lib.codesize:=bufferlength(ss_code)
	lib.idatasize:=bufferlength(ss_idata)

	lib.codeptr:=bufferelemptr(ss_code,0)
	lib.idataptr:=bufferelemptr(ss_idata,0)

	int ndlls:=0, nlibs:=0

	for i to nplibfiles do
		if plibtypes[i]='D' then ++ndlls else ++nlibs fi
	od

	lib.ndlllibs:=ndlls
	lib.nlibs:=nlibs

	lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
	lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

	k:=0
	for i to nplibfiles when plibtypes[i]='D' do
		lib.dllnames[++k]:=plibfiles[i]
	od

	k:=0
	for i to nplibfiles when plibtypes[i]='L' do
		lib.libnames[++k]:=plibfiles[i]
	od

	writesymbols()
	plib:=pcm_alloc(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n, k
	psymbol d
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
				if d.isimported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.isimported then
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

proc scansymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.isexported then d.expindex:=++nsymexports fi
		if d.isimported then d.impindex:=++nsymimports fi
	od
end

proc writesymbols=
	psymbol d
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
		lib.importnames[++k]:=ss_symboltable[i].name
	od

	k:=0
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			name:=getbasename(d.name)
			if epoffset=-1 and (eqstring(name,"start") or eqstring(name,"main")) then
				epoffset:=d.offset
			fi
			lib.exports[++k]:=name
			lib.exportsegs[k]:=d.segment
			lib.exportoffsets[k]:=d.offset
		fi
	od

	lib.entryoffset:=u32(epoffset)
end

proc roundsegment(ref dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	od
end
=== pc_stackmcl.m 0 0 14/25 ===
global macro freereg(r) =
	(regset[r]:=0; mccodex.regend[r]:=1)

global proc resetopnds1=
!after pass1

!CPL =RESETOPNDS1
	if mstackdepth then
		println "1:HW stack not empty",procdef.name,=mstackdepth
		MSTACKDEPTH:=0
!		merror("reset:mstackdepth?")
	fi
	if noperands then
		println "1:Reset:pcl stack not empty:",procdef.name,=noperands
		NOPERANDS:=0
!		merror("reset:pcl stack not empty?")
	fi
!	if ncalldepth then merror("reset:call stack not empty?") fi

!should set these to zero but should already be zero

!check reg flags
	for i in regset.bounds do
		if regset[i] or xregset[i] then
			println "Reset: reg flag set",procdef.name
			exit
		fi
	od

!--------------------------------------------
!!Work out values for the optimiser, and display as comments
	if inf_proccalls=0 then inf_leafproc:=1 fi

!	if nproccalls=0 and (nprocparams+nprocxparams)=0 then ++nzeroparamleaf fi

!	mgeninfos("High reg:  ",getregname(inf_highreg))
!	mgeninfos("High xreg: ",fgetregname(inf_highxreg))
!	mgeninfo ("Calls:     ",inf_proccalls)
!	mgeninfos("Leaf func: ",(inf_leafproc|"Yes"|"No"))
!	mgeninfo ("Locals:    ",inf_proclocals)
!	mgeninfo ("Xlocals:   ",inf_procxlocals)
!	mgeninfo ("Max args:  ",inf_maxargs)

!--------------------------------------------
!reset the values for next proc
!	memset(&.regset,0,regset.bytes)
!	memset(&.xregset,0,xregset.bytes)

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

	if not optimflag then			!else needed for pass 2 procentry
		inf_proccalls:=0
		inf_maxargs:=0
		inf_proclocals:=0
		inf_procxlocals:=0

		inf_leafproc:=0
!cpl "CLEAR HIGHREG",RNONE
		inf_highreg:=inf_highxreg:=rnone
		inf_assem:=0
	else
!optimising: need some of these so don't clear

		inf_proccalls:=0
		inf_maxargs:=0
		inf_proclocals:=0
		inf_procxlocals:=0

!		inf_leafproc:=0
!		inf_highreg:=inf_highxreg:=rnone
!		inf_assem:=0


	fi
end

global proc resetopnds2=
!after pass2

	if mstackdepth then
		println "2:HW stack not empty",procdef.name,=mstackdepth
	fi
	if noperands then
		println "2:Reset:pcl stack not empty:",procdef.name,=noperands
		NOPERANDS:=0
	fi
!	if ncalldepth then merror("reset:call stack not empty?") fi

!should set these to zero but should already be zero

!check reg flags
	for i in regset.bounds do
		if regset[i] or xregset[i] then
			println "2:Reset: reg flag set",regnames[i],procdef.name
			exit
		fi
	od

!MGENINFOS("2:RESETOPNDS",procdef.name)
	inf_proccalls:=0
	inf_maxargs:=0
	inf_proclocals:=0
	inf_procxlocals:=0

	inf_leafproc:=0
	inf_highreg:=inf_highxreg:=rnone
	inf_assem:=0

	inf_r10used:=inf_r11used:=inf_r13used:=0

!	memset(&.regset,0,regset.bytes)
!	memset(&.xregset,0,xregset.bytes)

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

end

proc newopnd(int fmt)=
	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi
	++noperands
	pclstack:=cast(&pclstack[0])
	pclstack[1]:=pclstackzero
	pclstack[1].fmt:=fmt
	pclstack[1].loc:=loccodes[fmt]
	pclstack[1].reg:=rnone
!	pclstack[1].value:=0
!	pclstack[1].high:=0
!	pclstack[1].low:=0
	pclstack[1].float:=floatloc[fmt]
end

global proc duploperand=
!assume 64-bit operand
	int reg

	++noperands
	pclstack:=cast(&pclstack[0])
	pclstack[1]:=pclstack[2]

!There is now a simple duplicate; but it will need more work depending
!on the current format
	case loccodes[pclstack[1].fmt]
	when imm_loc then			!can be shared
	when mem_loc then			!
	when regvar_loc then		!
	when reg_loc then			!need to be physically duplicated
		reg:=getnextreg(0)
		pclstack[1].reg:=reg
		genmc(m_mov, mgenreg(pclstack[2].reg),mgenreg(reg))

	when xreg_loc then			!need to be physically duplicated
		reg:=getnextxreg(0)
		pclstack[1].reg:=reg
		genmc(m_movq, mgenxreg(pclstack[2].reg),mgenreg(reg))
	when stack_loc then			!need to be physically duplicated
		reg:=getnextreg(0)
		genmc(m_mov, mgenreg(reg),dstackopnd)
		genmc(m_push, mgenreg(reg))
	esac
end

global proc addint(int a)=
	newopnd(imm_d64)
	pclstack[1].value:=a
end

global proc addreal(real x)=
	newopnd(imm_x64)
	pclstack[1].xvalue:=x
end

global proc addreal32(real x)=
	newopnd(imm_x32)
	pclstack[1].xvalue:=x
end

global proc addstring(ichar s)=
	newopnd(imm_str)
	pclstack[1].svalue:=s
end

global proc addmem(pcl p)=
	mcloperand ax
	psymbol d:=p.def

	case pcat[p.mode]
	when d64cat then
		if d.reg then
			newopnd(regvar_d64)
			pclstack[1].reg:=d.reg
		else
			newopnd(mem_d64)
		fi
	when x64cat then
		if d.reg then
			newopnd(xregvar_x64)
			pclstack[1].reg:=d.reg
		else
			newopnd(mem_x64)
		fi
	when x32cat then newopnd(mem_x32)
	when widecat then
		newopnd(memhigh_d64)
		pclstack[1].def:=d
		pclstack[1].wide:='H'
		newopnd(mem_d64)
		pclstack[1].wide:='L'
	when blockcat then
		newopnd(imm_memaddr)

	when shortcat then
		addreg_d64()
		ax:=genopnd(xa)
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mgenmem(d))
		return

!	when tvoid then

	else
		addreg_d64()
!		MGENCOMMENT("****ADDMEM?")
		merror("ADDMEM?")
	esac

	pclstack[1].def:=d
end

global proc addmemaddr(psymbol d)=
	newopnd(imm_memaddr)
	pclstack[1].def:=d
end

global proc addlabeladdr(int lab)=
	newopnd(imm_labaddr)
	pclstack[1].value:=lab
end

global proc addlabel(int lab,offset=0)=
	newopnd(imm_label)
	pclstack[1].value:=lab
	pclstack[1].offset:=offset
end

global proc addreg0(int reg)=
!turn return value in r0 into a new pclstack operand
!(modified for mult regs)
	newopnd(reg_d64)
	pclstack[1].reg:=reg
	if regset[reg] then
		merror("addreg0/reg in use")
	fi
	regset[reg]:=1
end

global proc addwidereg0(int reg)=
!turn return value in r0 into a new pclstack operand
	int reg2
	reg2:=reg+1
	if reg2=r3 then reg2:=r10 fi
	newopnd(reg_d64)
	newopnd(reg_d64)
	pclstack[2].reg:=reg2
	pclstack[1].reg:=reg
	if regset[reg] then
		merror("addwidereg/reg(s) in use")
	fi
	regset[reg]:=1
!	regset[reg2]:=1
	setwideopnd(1)
end

global proc addxreg0(int reg,fmt)=
!turn return value in x0 into a new pclstack operand
	newopnd(fmt)
	pclstack[1].reg:=reg
	if xregset[reg] then merror("addxreg0/reg in use") fi
	xregset[reg]:=1
end

global proc addreg_d64=
!create new pcl opnd in any d64 reg
	newopnd(reg_d64)
	pclstack[1].reg:=getnextreg()
end

global proc addreg_x64=
	newopnd(xreg_x64)
	pclstack[1].reg:=getnextxreg()
end

global proc addreg_x32=
	newopnd(xreg_x32)
	pclstack[1].reg:=getnextxreg()
end

global function stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=&.str, t
	pclstackrec pc

	if indent then
		fprint @s, "                                     ("
	else
		fprint @s, "("
	fi

	for i:=noperands downto 1 do
		pc:=pclstack[i]
		case pc.loc
		when reg_loc then
			strcat(s, regnames[pc.reg])

		when regvar_loc then
			strcat(s, regnames[pc.reg])
			strcat(s, "=")
			strcat(s, pc.def.name)

		when xreg_loc then
			strcat(s, xregnames[pc.reg])
			strcat(s, ":")
			strcat(s, fmtnames[pc.fmt])
		when xregvar_loc then
			strcat(s, xregnames[pc.reg])
			strcat(s, "=")
			strcat(s, pc.def.name)


		when stack_loc then
			strcat(s, "T")
		when mem_loc then
			strcat(s,"M:")
!			if pc.fmt=memhigh_d64 then strcat(s,"H:") fi
			strcat(s,pc.def.name)
		elsecase pc.fmt
		when imm_d64 then
			strcat(s, strint(pc.value))
		when imm_x64 then
				strcat(s, strreal(pc.xvalue))
		when imm_str then
			t:=pc.svalue
			if strlen(t)>20 then
				strcat(s,"LONG STR")
			else
				strcat(s,"""")
				convertstring(t,s+strlen(s))
				strcat(s,"""")
			fi
		when imm_memaddr then
			strcat(s,"&")
			strcat(s,pc.def.name)
		when imm_labaddr then
			strcat(s,"&#")
			strcat(s,strint(pc.labno))
		when imm_label then
			strcat(s,"#")
			strcat(s,strint(pc.labno))
		else
!			strcat(s,"??")
!			strcat(s,LOCNAMES[PCLLOC[I]])
			strcat(s,FMTNAMES[pc.fmt])
		esac
		if pc.wide then strcat(s,(pc.wide='H'|"(H)"|"(L)")) fi
!		if pc.high then strcat(s,"(H)") fi
!		if pc.low then strcat(s,"(L)") fi

		if i>1 then strcat(s,",") fi
	od
	strcat(s,") (")
	for r:=r0 to regmax do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") (")
	for r:=r0 to xregmax do
		strcat(s,(xregset[r]|"1 "|"0 "))
	od

	strcat(s,") hwstack:")
	strcat(s,strint(mstackdepth))
	strcat(s," noperands:")
	strcat(s,strint(noperands))
	strcat(s," ncalldepth:")
	strcat(s,strint(ncalldepth))
	strcat(s," callslots[]:")
	strcat(s,strint(callslots[ncalldepth]))
	return s
end

global proc loadopnd(int n=1, int nvreg=0)=
	int reg,value
	mcloperand ax

	if n>noperands then
		MGENCOMMENT("UNDERFLOW")
		RETURN
		merror("loadopnd/underflow")
	fi
	if pclstack[n].loc=reg_loc then
		return
	fi

	if pclstack[n].loc=xreg_loc then
		return
	fi

	if pclstack[n].float then
		reg:=getnextxreg(nvreg)
	else
		reg:=getnextreg(nvreg)
	fi

	case pclstack[n].loc
	when xreg_loc then
		MERROR("LOADOPND/XREG")
!	when stack_loc then
!		MERROR("LOADOPND/STACK")
	elsecase pclstack[n].fmt
	when imm_d64 then
		value:=pclstack[n].value

		if value=0 and not noxorclear then
			ax:=mgenreg(reg,4)
			genmc(m_xorx,ax,ax)
		else
			genmc(m_mov,mgenreg(reg),mgenint(pclstack[n].value))
		fi

	when imm_x64 then
		genmc(m_movq,mgenxreg(reg),mgenrealmem(pclstack[n].xvalue))

	when imm_x32 then
		genmc(m_movd,mgenxreg(reg),mgenrealmem(pclstack[n].xvalue,4))

	when imm_str then
		genmc(m_mov,mgenreg(reg),mgenlabel(getstringindex(pclstack[n].svalue)))

	when imm_labaddr then
		genmc(m_mov, mgenreg(reg), mgenlabel(pclstack[n].labno))

	when imm_label then
		genmc(m_mov, mgenreg(reg), mgenlabelmem(pclstack[n].labno))
		mccodex.b.offset:=pclstack[n].offset

	when mem_d64 then
		genmc(m_mov,mgenreg(reg),mgenmem(pclstack[n].def))

	when mem_x64 then
		genmc(m_movq,mgenxreg(reg),mgenmem(pclstack[n].def))

	when mem_x32 then
		genmc(m_movd,mgenxreg(reg),mgenmem(pclstack[n].def))

	when memhigh_d64 then
		genmc(m_mov,mgenreg(reg),mgenmemhigh(pclstack[n].def))

	when regvar_d64 then
		genmc(m_mov,mgenreg(reg),mgenregvar(pclstack[n].def))

	when xregvar_x64 then
		genmc(m_movq,mgenxreg(reg),mgenxregvar(pclstack[n].def))

	when stack_d64 then
!		checkstackorder(n)
		genmc(m_pop, mgenreg(reg))
		--mstackdepth

	when stack_x64 then
!		checkstackorder(n)
		if inf_r13used then merror("R13 in use") fi
		genmc(m_pop, mgenreg(r13))
		genmc(m_movq, mgenxreg(reg), mgenreg(r13))
		--mstackdepth

	when stack_x32 then
!		checkstackorder(n)
		if inf_r13used then merror("R13 in use") fi
		genmc(m_pop, mgenreg(r13))
		genmc(m_movd, mgenxreg(reg), mgenreg(r13,4))
		--mstackdepth

	when imm_memaddr then
		genmc(m_lea,mgenreg(reg),mgenmem(pclstack[n].def))

	else
		CPL STROPNDSTACK()
		MERROR("LOADOPND??",fmtnames[pclstack[n].fmt])
	esac

	pclstack[n].reg:=reg
	pclstack[n].fmt:=loadfmt[pclstack[n].fmt]
	pclstack[n].loc:=loccodes[pclstack[n].fmt]
end

global proc loadparam(int n=1, reg)=
	int oldreg, value
	mcloperand ax

	ax:=mgenreg(reg)
	oldreg:=pclstack[n].reg

	case pclstack[n].fmt
	when reg_d64, regvar_d64 then
		genmc(m_mov, ax, mgenreg(oldreg))
		freereg(oldreg)

	when xreg_x64, xregvar_x64 then
		genmc(m_movq, ax, mgenxreg(oldreg))
		return							!leave loc unchanged
	when xreg_x32 then
		genmc(m_movd, changeopndsize(ax,4), mgenxreg(oldreg))
		return

	when imm_d64 then
		value:=pclstack[n].value
		if value=0 then
			ax:=mgenreg(reg,4)
			genmc(m_xorx, ax,ax)
		else
			genmc(m_mov, ax, mgenint(pclstack[n].value))
		fi
	when imm_x64 then
		genmc(m_mov, ax, mgenrealmem(pclstack[n].xvalue))
	when imm_str then
		genmc(m_mov,ax, mgenlabel(getstringindex(pclstack[n].svalue)))
!		genmc(m_mov,ax,mgenstring(pclvalues[n].svalue))

	when mem_d64 then
		genmc(m_mov,ax,mgenmem(pclstack[n].def))

	when memhigh_d64 then
		genmc(m_mov,ax,mgenmemhigh(pclstack[n].def))

	when imm_memaddr then
		genmc(m_lea,ax,mgenmem(pclstack[n].def))
	when mem_x64 then
		genmc(m_mov,ax,mgenmem(pclstack[n].def))

	when stack_d64 then

!		checkstackorder(n)
		genmc(m_pop, ax)
		--mstackdepth

	else
		CPL "LOADPARAM:",FMTNAMES[pclstack[n].FMT]
		MGENCOMMENT("****LOADPARAM??")
		MERROR("LOADPARAM??",fmtnames[pclstack[n].fmt])
	esac
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=reg
end

global proc loadxparam(int n=1, reg)=
	mcloperand ax

!	if reg=rnone then
!		reg:=getnextreg(nvreg)
!	else
!		if regset[reg] then
!			merror("loadopnd/reg in use")
!		fi
!	fi

	ax:=mgenxreg(reg)

	case pclstack[n].fmt
	when reg_d64, regvar_d64 then
		genmc(m_movq, ax, mgenreg(pclstack[n].reg))

	when xreg_x64, xregvar_x64 then
		genmc(m_movq, ax, mgenxreg(pclstack[n].reg))
	when xreg_x32 then
		genmc(m_movd, ax, mgenxreg(pclstack[n].reg))
	when imm_x64 then
		genmc(m_movq, ax, mgenrealmem(pclstack[n].xvalue))

	when imm_x32 then
		genmc(m_movd, ax, mgenrealmem(pclstack[n].xvalue))

	when mem_d64, mem_x64 then
		genmc(m_movq,ax,mgenmem(pclstack[n].def))

	when mem_x32 then
		genmc(m_movd,ax,mgenmem(pclstack[n].def))
	else
		CPL "??LOADXPARAM",N,NOPERANDS
		MGENCOMMENT("****LOADXPARAM??")
		MERROR("LOADXPARAM??",fmtnames[pclstack[n].fmt])
	esac
end

global function genopnd(int index=1,size=8)mcloperand ax=
!int, float, or low half of wide
	int reg, value

	case pclstack[index].loc
	when reg_loc then
		return mgenreg(pclstack[index].reg,size)

	when regvar_loc then
		return mgenregvar(pclstack[index].def)

	when xreg_loc then
		return mgenxreg(pclstack[index].reg,size)

	when xregvar_loc then
		return mgenxregvar(pclstack[index].def)

	elsecase pclstack[index].fmt
	when mem_d64, mem_x64, mem_x32 then
		return mgenmem(pclstack[index].def)

	when memhigh_d64 then
		return mgenmemhigh(pclstack[index].def)
	when imm_d64 then
		value:=pclstack[index].value
		if int32.minvalue<=value<=int32.maxvalue then
			return mgenint(value)
		fi
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax, mgenint(value))
		pclstack[index].reg:=ax.reg
		pclstack[index].fmt:=reg_d64
		pclstack[index].loc:=reg_loc
		return ax

	when imm_x64 then
		return mgenrealmem(pclstack[index].xvalue)

	when imm_x32 then
		return mgenrealmem(pclstack[index].xvalue,4)

	when imm_memaddr then
		reg:=getnextreg()
		ax:=mgenreg(reg)
		genmc(m_lea,ax,mgenmem(pclstack[index].def))
		pclstack[index].reg:=ax.reg
		pclstack[index].fmt:=reg_d64
		pclstack[index].loc:=reg_loc
		return ax
	when imm_str then
		genmc(m_lea,ax:=mgenreg(getnextreg()),mgenlabel(getstringindex(pclstack[index].svalue)))
		pclstack[index].reg:=ax.reg
		pclstack[index].fmt:=reg_d64
		pclstack[index].loc:=reg_loc
		return ax

	else
		CPL =LOCNAMES[PCLSTACK[INDEX].LOC]
		CPL =FMTNAMES[PCLSTACK[INDEX].FMT]
		CPL("GENOPND??")
		MGENCOMMENT("****GENOPND??")
		merror("GENOPND? ",fmtnames[pclstack[index].fmt])
	esac

	return nil
end

global function genopnd_ld(int index=1,size=8)mcloperand=
	loadopnd(index)
	return genopnd(index,size)
end

global function genopnd_ind(int index=1,size=8)mcloperand=
!int, float, or low half of wide
!
	case pclstack[index].loc
	when reg_loc then
		return mgenireg(pclstack[index].reg,size)
		return mgenireg(pclstack[index].reg,size)
		return mgenireg(pclstack[index].reg,size)
	esac

	loadopnd(index)

	return genopnd_ind(index,size)
end

global function genopnd_d64:mcloperand=
!create new d64 register operand
	addreg_d64()
	return genopnd()
end

global proc setwideopnd(int n=1)=
	pclstack[n].wide:='L'
	pclstack[n+1].wide:='H'
end

global proc unsetwideopnd(int n=1)=
	pclstack[n].wide:=0
end

global function getnextreg(int nvreg=0)int=
	int reg,firstreg

	firstreg:=(nvreg|r3|r0)

	for r:=firstreg to regmax do
		if regset[r]=0 then
			regset[r]:=1
			inf_highreg max:=r
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN
!CPL "SET",=INF_HIGHREG,PROCDEF.NAME
!FI
			return r
		fi
	od

!all regs occupied; need to free one
	for i:=noperands downto 1 do
		if pclstack[i].loc=reg_loc then
			reg:=pclstack[i].reg
			if reg>=firstreg then
				pushopnd(i)
				return getnextreg(nvreg)
			fi
		fi
	od
	merror("NO FREE REGS")
	return 0
end

global function getnextxreg(int nvreg=0)int=
	int reg,firstreg

	firstreg:=(nvreg|r6|r4)

	for r:=firstreg to regmax do
		if xregset[r]=0 then
			xregset[r]:=1
			inf_highxreg max:=r
			return r
		fi
	od

!all regs occupied; need to free one
!	for i:=1 to noperands do
	for i:=noperands downto 1 do
		if pclstack[i].loc=reg_loc then
			reg:=pclstack[i].reg
			if reg>=firstreg then
				pushopnd(i)
				return getnextxreg(nvreg)
			fi
		fi
	od
	merror("NO FREE XREGS")
	return 0
end

global proc delopnd=
	if noperands<=0 then
		MGENCOMMENT("****DELND/UNDERFLOW")
		RETURN
!		merror("popopnd/underflow")
	fi

	case pclstack[1].loc
	when reg_loc,regvar_loc then
		freereg(pclstack[1].reg)
	when xreg_loc,xregvar_loc then
		freexreg(pclstack[1].reg)
	when mem_loc then
	when imm_loc then
!	when stack_opnd then
!	when str_loc then
	else
		merror("Can't pop opnd: #",locnames[pclstack[1].loc])
	esac

	--noperands
	pclstack:=cast(&pclstack[2])
end

global proc freexreg(int xr)=
	xregset[xr]:=0
end

global proc pushopnd(int n)=
!make sure operand n is on the hw stack; caller must now that all
!previous pclstack operands are already on the stack

	case pclstack[n].loc
	when reg_loc then
		genmc(m_push, mgenreg(pclstack[n].reg))
		freereg(pclstack[n].reg)

	when regvar_loc then
		genmc(m_push, mgenreg(pclstack[n].reg))

	when xreg_loc then
		if inf_r13used then merror("2:R13 in use") fi
		genmc(m_movq,mgenreg(r13), mgenxreg(pclstack[n].reg))
		genmc(m_push, mgenreg(r13))
		freexreg(pclstack[n].reg)

	when stack_loc then
		return
	elsecase pclstack[n].fmt
	when memhigh_d64 then
		genmc(m_push, mgenmemhigh(pclstack[n].def))

	when mem_d64, mem_x64 then
		genmc(m_push, mgenmem(pclstack[n].def))

	when mem_x32 then
		if inf_r13used then merror("4:R13 in use") fi
		genmc(m_mov,mgenreg(r13,4), mgenmem(pclstack[n].def))
		genmc(m_push, mgenreg(r13))

	when imm_d64 then
		genmc(m_push, mgenint(pclstack[n].value))

	when imm_x64 then
		genmc(m_push, mgenrealmem(pclstack[n].xvalue))

	when imm_str then
		genmc(m_push, mgenlabel(getstringindex(pclstack[n].svalue)))

	when imm_memaddr then
		if inf_r13used then merror("3:R13 in use") fi
		genmc(m_lea, mgenreg(r13), mgenmem(pclstack[n].def))
		genmc(m_push, mgenreg(r13))

	else
		merror("Can't push opnd: #",fmtnames[pclstack[n].fmt])
	esac

	pclstack[n].loc:=stack_loc
	pclstack[n].fmt:=pushfmt[pclstack[n].fmt]
	++mstackdepth
end

global proc pushallopnds(int n=1)=
	for i:=noperands downto n do
		pushopnd(i)
	od
end

global proc poparg=
	case pclstack[1].loc
	when reg_loc then freereg(pclstack[1].reg)
	when xreg_loc then freexreg(pclstack[1].reg)
	when stack_loc then
	when imm_loc then
	when mem_loc then
	when regvar_loc then
	when xregvar_loc then
	else
		CPL "POPARG:",LOCNAMES[PCLSTACK[1].LOC]
		MGENCOMMENT("****POPARG?")
		merror("poparg? #",locnames[pclstack[1].loc])
	esac
	--noperands
	pclstack:=cast(&pclstack[2])
end

global proc pushslots(int nslots)=
	pushstack(nslots*8)
	mstackdepth+:=nslots
end

global proc popslots(int nslots)=
	popstack(nslots*8)
	mstackdepth-:=nslots
end

global proc pushstack(int n)=
	if n then
		genmc(m_sub,dstackopnd,mgenint(n))
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add,dstackopnd,mgenint(n))
	fi
end

global proc saveopnd(int n)=
!make sure operand n is on the hw stack; caller must now that all
!previous pclstack operands are already on the stack
	int reg

!TO BE REVISED
	case pclstack[n].loc
	when reg_loc then
		reg:=pclstack[n].reg
		if reg in r0..r2 then
			pushopnd(n)
		fi

	when xreg_loc then
		reg:=pclstack[n].reg
		if reg in r0..r5 then
			pushopnd(n)
		fi
	when stack_loc then
	when regvar_loc, xregvar_loc then
	when imm_loc then
		pushopnd(n)
	elsecase pclstack[n].fmt
	when memhigh_d64 then
		pushopnd(n)
	when mem_d64, mem_x64,mem_x32 then
		pushopnd(n)

	else
		merror("Can't save opnd: #",fmtnames[pclstack[n].fmt])
	esac
end

global proc saveallopnds(int n=1)=
	for i:=noperands downto n do
!	for i to n do
		saveopnd(i)
	od
end

global proc movetoreg(int newreg)=
	int oldreg

	loadopnd()

	oldreg:=pclstack[1].reg

	if oldreg=newreg then
		return
	fi

	if regset[newreg] then merror("movereg/reg in use") fi
	genmc(m_mov, mgenreg(newreg), mgenreg(oldreg))
	freereg(oldreg)
	pclstack[1].reg:=newreg
	regset[newreg]:=1
	if newreg>=r10 then inf_highreg max:=newreg fi
end

global proc swapopnds(int m,n)=
!exchange top opndstack entry (m assumed to be 1) with n'th entry down
!uses notional index of stack with:
!	[1] meaning opndstack[noperands]
!	[n] meaning opndstack[noperands-n+1]
!NOTE: all operands m to n inclusive
!caller is responsible for this (getopnds(n) might ensure this when m=1)
!usually m=1
	pclstackrec t

	t:=pclstack[m]
	pclstack[m]:=pclstack[n]
	pclstack[n]:=t

!swap(pclstack[m],pclstack[n])
end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2

	int reg1:=pclstack[1].reg

	for i:=2 to noperands do
		if pclstack[i].loc=reg_loc and pclstack[i].reg=reg2 then
			swap(pclstack[1].reg, pclstack[2].reg)
			return
		fi
	else
		CPL PROCDEF.NAME
		merror("swapopndregs/reg not found")
	od
end

global proc showopndstack=
	mgencomment(stropndstack(1))
end

=== pc_tables.m 0 0 15/25 ===
!type system

export tabledata() 	[0:]ichar pstdnames,
		[0:]byte psize, [0:]byte psigned, [0:]byte pfloat, [0:]byte pcat =
!    type         name    bits SZ   S F Cat
	(tpvoid=0,    "void",    	0,	0,0, voidcat),

	(tpu8,        "u8",      	1,	0,0, shortcat),
	(tpu16,       "u16",    	2,	0,0, shortcat),
	(tpu32,       "u32",    	4,	0,0, shortcat),
	(tpu64,       "u64",    	8,	0,0, d64cat),

	(tpi8,        "i8",      	1,	1,0, shortcat),
	(tpi16,       "i16",    	2,	1,0, shortcat),
	(tpi32,       "i32",    	4,	1,0, shortcat),
	(tpi64,       "i64",    	8,	1,0, d64cat),

	(tpr32,       "r32",    	4,	0,1, x32cat),
	(tpr64,       "r64",    	8,	0,1, x64cat),

	(tpwide,      "wide",   	0,	0,0, widecat),
	(tpblock,     "block",   	0,	0,0, blockcat),

	(tplast,      "$last",   	0,	0,0, voidcat),
end

global tabledata() [0:]ichar catnames =
	(voidcat=0,		$),
	(d64cat,		$),
	(x32cat,		$),
	(x64cat,		$),
	(shortcat,		$),
	(widecat,		$),
	(blockcat,		$),
end

global tabledata() [0:]ichar opndnames =
	(no_opnd=0,			$),
	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(label_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),
	(real32_opnd,		$),
	(string_opnd,		$),
	(assem_opnd,		$),
end

!Stack operands are:
!	Xa				1st of 1
!   Xb, Ya			1st/2nd of 2
!   Xc, Yb, Za		1st/2nd/3rd of 3
!   Xd, Yc, Zb, Wa	1st/2nd/3rd/4th of 4
! X is always the 'left-most' operand, but will be at offset 0, 1 2 from top of stack
! a (as in Xa, Ya, Za, Wa) is always the top of stack

!Immediate operand:
!   A			(various)
!Extra info:
!   op			opindex
!   fn			fnindex
!   cc			cond code
!   t[:size]    type (:size for block types)
!   u           secondary type for some ops (convert etc)
!   n			nargs for calls
!   s x			scale and offset for ptr/offset ops
!   x y			min/max lab index for switch
!	B			Secondary operand in a following kopnd instruction
!	C			Tertiary operand in a following kopnd instruction

!Stack usage is represented by (a b):
! a is the number of stack elements that will be popped
! b is the number of new stack elements that will be pushed
! Something like (1 1) can mean the same element stays in place


export tabledata() [0:]ichar pclnames,
			[0:]byte pclhasopnd,			!1+ has operand; 2=name defines a name; 3=defines local/param; 4=imported func
			[0:]byte pclhastype,
			[0:]byte pclextra =

!                          Op T X
	(kzero=0,			$,	0,0,0),	! (0 0)
	(knop,				$,	0,0,0),	! (0 0)
	(kstop,				$,	0,0,0),	! (1 0)	Stop Xa
	(kcomment,			$,	1,0,0),	! (0 0)	Comment A (a string)

	(kimportdll,		$,	1,0,0),	! (0 0) A Use external dll A
	(kimportlib,		$,	1,0,0),	! (0 0) A Use external lib A
!	(kimport,			$,	1,0,0),	! (0 0) A Import symbol A
!	(kexport,			$,	1,0,0),	! (0 0) A Export symbol A (or declare using ::)
	(kistatic,			$,  2,1,0),	! (0 0) (A,t) Define idata label (must be followed by correct kdata ops)
	(kzstatic,			$,	2,1,0),	! (0 0) (A,t) Define zdata labe and reserve sufficient space
	(kequiv,			$,	2,0,0),	! (0 0) (A) Define equivalence to this var?

!	(kprocdef,			$,	2,1,1),	! (0 0) (A,t, isrts) Define proc A, of given return type
	(kprocdef,			$,	2,1,0),	! (0 0) (A,t) Define proc A, of given return type
	(kprocentry,		$,	0,0,0),	! (0 0)
	(kendproc,			$,	0,0,0),	! (0 0)
	(kendprogram,		$,	0,0,0),	! (0 0)
	(kextproc,			$,	4,1,0),	! (0 0) (t)
	(kextvar,			$,	4,1,0),	! (0 0) (t)
	(kextparam,			$,	0,1,0),	! (0 0) (t) Anonymous params
	(kextvariadics,		$,	0,1,0),	! (0 0) (t) ... parameter
	(kendextproc,		$,	0,0,0),	! (0 0)
	(kthreadedproc,		$,	2,1,0),	! (0 0) (A,t) Define proc A, of given return type
	(kprocrts,			$,	2,1,0),	! (0 0) (A,t) Alternate to procdef but for rts functions

	(klocal,			$,	3,1,0),	! (0 0) (A,t) Define local A of type t
	(kparam,			$,	3,1,0),	! (0 0) (A,t) Define param A of type t
	(klabel,			$,	1,0,0),	! (0 0) (L) Define numbered label L
	(klabelname,		$,	2,0,0),	! (0 0) (A) Define named label

	(kpush,				$,	1,1,0),	! (0 1) (X,t)	Push operand X of type t; X is anything pushable
	(kpop,				$,	1,1,0),	! (1 0) (L,t)	pop to label X
	(kstore,			$,	1,1,0),	! (1 1) (L,t)	store to label X but stays on the stack
	(kpushnc,			$,	1,1,0),	! (0 1) (X,t)	Push optimised for blocks (no copying)
	(kpushlabel,		$,	1,0,0),	! (0 1) (L)		Push address of label L

	(kopnd,				$,	1,0,0),	! (0 0) (X) Define auxiliary operand X (not sure about extra stuff yet)
	(ktype,				$,	0,1,0),	! (0 0) (t) Define auxiliary type t
	(kduplstack,		$,	0,1,0),	! (1 2) (t) Ya':=Xa; X stays on stack
	(kswapstack,		$,	0,1,1),	! (1 1) (t,N) Swap Xa with element +N away

	(kpushptroff,		$,	0,1,2),	! (2 1) (t,scale,offset) Xa:=(Xb+Ya*scale+offset)^ using given type
	(kpopptroff,		$,	0,1,2),	! (3 0) (t,scale,offset) (Yb+Za*scale+offset)^:=Xc
	(kstoreptroff,		$,	0,1,2),	! (3 1) (t,scale,offset) (Yb+Za*scale+offset)^:=Xc, Xc stays as Xa
!	(kindex,			$,	0,0,0),	! (0 0)
!	(kpopindex,			$,	0,0,0),	! (0 0)
!	(kstoreindex,		$,	0,0,0),	! (0 0)

	(kpushptr,			$,	0,1,0),	! (1 1) Xa:=Xa^
	(kpopptr,			$,	0,1,0),	! (2 0) Ya^:=Xb
	(kstoreptr,			$,	0,1,0),	! (2 1) Ya^:=Xb, keep Xb on stack as Xa

	(kdotindex,			$,	0,1,0),	! (2 1)	Xa:=Xb.[Ya]
	(kpopdotindex,		$,	0,1,0),	! (3 0) Yb^.[Za]:=Xc
	(kstoredotindex,	$,	0,1,0),	! (3 1) Yb^.[Za]:=Xc, keep Xc as Xa

	(kdotslice,			$,	0,1,0),	! (3 1) Xa:=Xc.[Yb..Za]
	(kpopdotslice,		$,	0,1,0),	! (4 0) Yc^.[Zb..Wa]:=Xd
	(kstoredotslice,	$,	0,1,0),	! (4 1) Yc^.[Zb..Wa]:=Xd, keep

!	(kfree,				$,	0,1,0),	! (1 0)	Pop Xa
	(kpopstack,			$,	0,1,0),	! (1 0)	Pop Xa
	(keval,				$,	0,1,0),	! (1 0) Evaluate Xa [load to an actual register], then pop

	(kcallproc,			$,	1,0,0),	! (n 0) (A) Call &A with nargs, then pop args
	(kcallprocptr,		$,	0,0,0),	! (n+1 0) Call Xa with nargs, then pop args
	(kretproc,			$,	0,0,0),	! (0 0) Return from proc

	(kcallfn,			$,	1,1,0),	! (n 1) (A, t), Call &A, then pop args, leave retval
	(kcallfnptr,		$,	0,1,0),	! (n+1 1) (t) Call Xa, then pops args, leave retval
	(kretfn,			$,	0,1,0),	! (0 0) (t) Return from function with Xa=retval

	(kjump,				$,	1,0,0),	! (0 0) (L) goto L
	(kjumpptr,			$,	0,0,0),	! (1 0) goto Xa

	(kjumpeq,			$,	1,1,0),	! (2 0) (L,t) goto L when Xb = Ya
	(kjumpne,			$,	1,1,0),	! (2 0) (L,t) goto L when <>
	(kjumplt,			$,	1,1,0),	! (2 0) (L,t) goto L when Xb < Ya
	(kjumple,			$,	1,1,0),	! (2 0) (L,t) goto L when <=
	(kjumpge,			$,	1,1,0),	! (2 0) (L,t) goto L when >=
	(kjumpgt,			$,	1,1,0),	! (2 0) (L) goto L when >

	(kjumptrue,			$,	1,1,0),	! (1 0) (L,t) goto L when Xa is true
	(kjumpfalse,		$,	1,1,0),	! (1 0) (L,t) goto L when Xa is false

	(kjumpinrange,		$,	1,1,0),	! (3 0) (L,t) goto L when Xc in Yb..Za
	(kjumpnotinrange,	$,	1,1,0),	! (3 0) (L,t) goto L when Xc not in Yb..Za

	(ksetjumpeq,		$,	1,1,0),	! (2 1) (L,t) goto L when Xb=Ya; pop Y, leave Xa
	(ksetjumpeqx,		$,	1,1,0),	! (0 0) (L,t) goto L when Xb=Ya; pop both
	(ksetjumpne,		$,	1,1,0),	! (0 0) (L,t) goto L when Xb<>Ya; pop both

!	(ksetcc,			$,	0,1,1),	! (2 1) (t,cc) Xa:=Xb cc Ya
	(kseteq,			$,	0,1,0),	! (2 1) (t) Xa:=Xb = Ya
	(ksetne,			$,	0,1,0),	! (2 1) (t) Xa:=Xb <> Ya
	(ksetlt,			$,	0,1,0),	! (2 1) (t) Xa:=Xb < Ya
	(ksetle,			$,	0,1,0),	! (2 1) (t) Xa:=Xb <= Ya
	(ksetge,			$,	0,1,0),	! (2 1) (t) Xa:=Xb >= Ya
	(ksetgt,			$,	0,1,0),	! (2 1) (t) Xa:=Xb > Ya

	(kcasejumpeq,		$,	1,1,1),	! (2 1) (L,t) goto L when Xb=Ya; pop Ya, leave Xa

!	(kselectcc,			$,	0,1,1),	! (4 1) (t,cc) Xa:=(Zb op Wa|Xd|Yc)
	(kselecteq,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb = Wa|Xd|Yc)
	(kselectne,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb <> Wa|Xd|Yc)
	(kselectlt,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb < Wa|Xd|Yc)
	(kselectle,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb <= Wa|Xd|Yc)
	(kselectge,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb >= Wa|Xd|Yc)
	(kselectgt,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb > Wa|Xd|Yc)

	(kselecttrue,		$,	0,1,0),	! (3 1) (t) Xa:=(Za|Xc|Yb)

	(kto,				$,	1,0,0),	! (0 0) (L)(B,t) --B (aux); goto L when B<>0 

	(kforup,			$,	1,1,1),	! (0 0) (L,t,n)(B,t)(C,t) B+:=n; goto L when B<=C
	(kfordown,			$,	1,1,1),	! (0 0) (L,t,n)(B,t)(C,t) B-:=n; goto L when B>=C

	(kswap,				$,	0,1,0),	! (2 0) (t) swap(Xb^,Yb^)

	(kmakeslice,		$,	0,1,0),	! (2 1) (t) Xa:=slice(Xb, Ya)

	(kswitch,			$,	1,0,2),	! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
	(kswitchlabel,		$,	1,0,0),	! (0 0) (L) jumptable entry
	(kendswitch,		$,	0,0,0),	! (0 0)	Mark end of switch jumptable

	(kclear,			$,	0,1,0),	! (1 0) (t) Clear Xa^

	(kcsegment,			$,	0,0,0),	! (0 0) Switch to that segment (usually automatic, so these override)
	(kisegment,			$,	0,0,0),	! (0 0) ..
	(kzsegment,			$,	0,0,0),	! (0 0) ..
	(krosegment,		$,	0,0,0),	! (0 0) ..

	(kdata,				$,	1,1,0),	! (0 0) (X,t) Define inline data of various kinds

	(kdb,				$,	1,0,0),	! (0 0) (X) Define a u8 data value
	(kdw,				$,	1,0,0),	! (0 0) (X) u16 value: ...
	(kdd,				$,	1,0,0),	! (0 0) (X) u32 value: u32/i32/r32, depends on operand
	(kdq,				$,	1,0,0),	! (0 0) (X) u64 value: u64/i64/r64/string/addr/label, depends on operan
	(kdstring,			$,	1,0,0),	! (0 0) (S) u8 sequence from string literal (no terminator)
	(kdstringz,			$,	1,0,0),	! (0 0) (S) u8 sequence from string literal, nul added

	(kreserve,			$,	0,1,0),	! (0 0) (t) Reserve space big enough for t
	(kassem,			$,	1,0,0),	! (0 0) to be worked out....

	(kadd,				$,	0,1,0),	! (2 1) (t) Xa := Xb + Ya
	(ksub,				$,	0,1,0),	! (2 1) (t)
	(kmul,				$,	0,1,0),	! (2 1) (t)
	(kdiv,				$,	0,1,0),	! (2 1) (t)
	(kidiv,				$,	0,1,0),	! (2 1) (t)
	(kirem,				$,	0,1,0),	! (2 1) (t)
	(kiand,				$,	0,1,0),	! (2 1) (t)
	(kior,				$,	0,1,0),	! (2 1) (t)
	(kixor,				$,	0,1,0),	! (2 1) (t)
	(kshl,				$,	0,1,0),	! (2 1) (t)
	(kshr,				$,	0,1,0),	! (2 1) (t)
	(kin,				$,	0,1,0),	! (2 1) (t)
	(knotin,			$,	0,1,0),	! (2 1) (t)
	(kmin,				$,	0,1,0),	! (2 1) (t)
	(kmax,				$,	0,1,0),	! (2 1) (t)
	(keq,				$,	0,1,0),	! (2 1) (t)
	(kne,				$,	0,1,0),	! (2 1) (t)
	(klt,				$,	0,1,0),	! (2 1) (t)
	(kle,				$,	0,1,0),	! (2 1) (t)
	(kge,				$,	0,1,0),	! (2 1) (t)
	(kgt,				$,	0,1,0),	! (2 1) (t)
	(ksame,				$,	0,1,0),	! (2 1) (t)
	(kandl,				$,	0,1,0),	! (2 1) (t)
	(korl,				$,	0,1,0),	! (2 1) (t)
	(kaddrefoff,		$,	0,1,2),	! (2 1) (t,scale,offset) Xa := Xb + Ya*scale + offset
	(ksubrefoff,		$,	0,1,2),	! (2 1) (t,scale,offset) Xa := Xb - Ya*scale + offset
	(ksubref,			$,	0,1,1),	! (2 1) (t,scale) Xa := (Xb - Ya)/scale

	(kneg,				$,	0,1,0),	! (1 1) (t) Xa:=-Xa
	(kabs,				$,	0,1,0),	! (1 1) (t)
	(kinot,				$,	0,1,0),	! (1 1) (t)
	(knotl,				$,	0,1,0),	! (1 1) (t)
	(kistruel,			$,	0,1,0),	! (1 1) (t)
	(ksqr,				$,	0,1,0),	! (1 1) (t)

	(ksqrt,				$,	0,1,0),	! (1 1) (t) Xa:=sqrt(Xa)
	(ksin,				$,	0,1,0),	! (1 1) (t)
	(kcos,				$,	0,1,0),	! (1 1) (t)
	(ktan,				$,	0,1,0),	! (1 1) (t)
	(kasin,				$,	0,1,0),	! (1 1) (t)
	(kacos,				$,	0,1,0),	! (1 1) (t)
	(katan,				$,	0,1,0),	! (1 1) (t)
	(kln,				$,	0,1,0),	! (1 1) (t)
	(klog,				$,	0,1,0),	! (1 1) (t)
	(kexp,				$,	0,1,0),	! (1 1) (t)
	(kround,			$,	0,1,0),	! (1 1) (t)
	(kfloor,			$,	0,1,0),	! (1 1) (t)
	(kceil,				$,	0,1,0),	! (1 1) (t)
	(kfract,			$,	0,1,0),	! (1 1) (t)
	(ksign,				$,	0,1,0),	! (1 1) (t)
	(katan2,			$,	0,1,0),	! (1 1) (t)
	(kpower,			$,	0,1,0),	! (1 1) (t)
	(kfmod,				$,	0,1,0),	! (1 1) (t)

	(kincr,				$,	0,1,1),	! (1 0) (t,step) Xa^+:=step
	(kdecr,				$,	0,1,1),	! (1 0) (t,step) Xa^-:=step
	(kincrload,			$,	0,1,1),	! (1 1) (t,step) Xa:=(Xa+:=step)^
	(kdecrload,			$,	0,1,1),	! (1 1) (t,step) Xa:=(Xa-:=step)^
	(kloadincr,			$,	0,1,1),	! (1 1) (t,step) Xa:=Xa++^ (difficult to express step)
	(kloaddecr,			$,	0,1,1),	! (1 1) (t,step) Xa:=Xa--^

	(kaddto,			$,	0,1,0),	! (2 0) (t) Xa^ +:= Ya
	(ksubto,			$,	0,1,0),	! (2 0) (t)
	(kmulto,			$,	0,1,0),	! (2 0) (t)
	(kdivto,			$,	0,1,0),	! (2 0) (t)
	(kidivto,			$,	0,1,0),	! (2 0) (t)
	(kiremto,			$,	0,1,0),	! (2 0) (t)
	(kiandto,			$,	0,1,0),	! (2 0) (t)
	(kiorto,			$,	0,1,0),	! (2 0) (t)
	(kixorto,			$,	0,1,0),	! (2 0) (t)
	(kshlto,			$,	0,1,0),	! (2 0) (t)
	(kshrto,			$,	0,1,0),	! (2 0) (t)
	(kminto,			$,	0,1,0),	! (2 0) (t)
	(kmaxto,			$,	0,1,0),	! (2 0) (t)
	(kandlto,			$,	0,1,0),	! (2 0) (t)
	(korlto,			$,	0,1,0),	! (2 0) (t)
	(kaddrefoffto,		$,	0,1,2),	! (2 0) (t,scale,offset) Xa^ +:= Ya
	(ksubrefoffto,		$,	0,1,2),	! (2 0) (t,scale,offset) Xa^ -:= Ya

	(knegto,			$,	0,1,0),	! (1 0) (t) -:=Xa^
	(kabsto,			$,	0,1,0),	! (1 0) (t)
	(kinotto,			$,	0,1,0),	! (1 0) (t)
	(knotlto,			$,	0,1,0),	! (1 0) (t)
	(kistruelto,		$,	0,1,0),	! (1 0) (t)

!for conversions, t is always the current operand type
!u is the new type. However, for conversions involving widening, the result
!must end up at at least 64 bit. So i64->u8 masks to 8 bits, the sign-extends to u64

!	(kconvert,			$,	0,2,1),	! (1 1) (t,u)
	(ktypepun,			$,	0,2,1),	! (1 1) (t,u)

	(ksoftconv,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) ??

	(kwiden,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) Widen int type, from t to wider int u
	(knarrow,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) I think reduces i128/u128 t to i64/u64 u
	(kfloat,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,t) Int u to real t
	(kfix,				$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,t) Real u to int t
	(ktruncate,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) Mask to width of u, but type is widend to i64/u64
	(kfwiden,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) r32 to r64
	(kfnarrow,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) r64 to r32
	(ksofttruncw,		$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) 128 bits to 64
	(kwidenw,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) 64 bits to 128

!These ones are currently still needed by or all PCL targets

	(kstartmult,		$,	0,0,0),
	(kresetmult,		$,	0,0,0),
	(kendmult,			$,	0,0,0),
	(ksetret,			$,	0,1,0),	! (0 0) (t) Set Xa as return value of type t
!	(ksetretmult,		$,	0,1,2),
	(ksetretmult,		$,	0,0,1), ! (0 0) (n) Set N return values
	(ksetargs,			$,	0,0,2), ! (nargs, nvars)

!these are special ones used reflection

	(kgetnprocs,		$,	0,0,0), ! (0 1) Get number of functions in function table
	(kgetprocname,		$,	0,0,0), ! (1 1) Xa:=Getprocname(Xa) Name of nth function (1-based)
	(kgetprocaddr,		$,	0,0,0), ! (1 1) Xa:=Getprocaddr(Xa) Addr of nth function (1-based)

!ops used internally by M compiler until they can be replaced
!(usually they will be turned into something else, constants etc)
	(klen,				$,	0,0,0),
	(klwb,				$,	0,0,0),
	(kupb,				$,	0,0,0),
	(kbounds,			$,	0,0,0),
	(klenstr,			$,	0,0,0),
	(kbitwidth,			$,	0,0,0),
	(kbytesize,			$,	0,0,0),
	(kbytes,			$,	0,0,0),
	(kminvalue,			$,	0,0,0),
	(kmaxvalue,			$,	0,0,0),
	(ktypestr,			$,	0,0,0),
	(kerror,			$,	0,0,0),
	(karraytoslice,		$,	0,0,0),
	(kichartoslice,		$,	0,0,0),
	(ksofttruncshort,	$,	0,0,0),
	(kcharaxtoichar,	$,	0,0,0),
	(ksliceptr,			$,	0,0,0),

	(klast,				$,	0,0,0),	! (0 0)
end

global tabledata() []ichar rtsnames =
	(rts_rts_unimpl,		$),
	(rts_power_i64,			$),
	(rts_float_u64r64,		$),
end
=== pc_writeexe.m 0 0 16/25 ===
!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order::
! initsectiontable()
! genexe()
! writeexe(filename)

[maxplibfile]int64 libinsttable
[maxplibfile]ichar libinstnames
[maxplibfile]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	ref basereloc nextitem
	word32 address				!virtual address
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
!const filealign = 32
const sectionalign = 4096
const exe_imagebase = 0x40'0000

const dll_imagebase = 0x1000'0000
!const dll_imagebase = 0x1'0000'0000

!const dll_imagebase = 0x6624'0000
global int imagebase

int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
psymbol stentrypoint				!symbol to be the entry point
psymbol stentrypoint2
psymbol stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir				!allowed section data for import directort in .idata

global const maximports = 3000
global [0..maximports]importrec importtable
global int nimports

global const maxexports = 1000
global [maxexports]exportrec exporttable
global int nexports
ichar dllfilename
int isdll

!global const maxlibs = 50
const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

ref byte datastart
ref byte dataptr
ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset				!from start of imp dir

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

!	println =filesize, =dataptr-datastart			!these should match

!	if fverbose>=2 then
!		println "Writing file:",outfile
!	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

global proc genexe(ichar entrypoint, outfile, int dodll)=
!manipulate the ss data to fill in all the details needed for exe format

!CPL =OUTFILE
	dllfilename:=extractfile(outfile)
!CPL =DLLFILENAME
	isdll:=dodll

	setuplibfiles()

	imagebase:=(isdll|dll_imagebase|exe_imagebase)

	userentrypoint:=entrypoint
	loadlibs()
	scanst()				!build dll/import tables

	getoffsets()

	relocdata(&sectiontable[csect])
	relocdata(&sectiontable[dsect])

end

proc loadlibs=
!load library instances
	int i
	int64 hinst
	ichar file
	[300]char filename

!	for i to nplibfiles when plibtypes[i]='D' do
	for i to nplibfiles do
		if plibtypes[i]='L' then
CPL =PLIBFILES[I],PLIBTYPES[I]:"c"
			axerror("Can't use LIB files with EXE")
		fi
		strcpy(&.filename,plibfiles[i])
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
!set up the section table

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
!	sectiontable[zsect].rawsize:=roundtoblock(ss_zdatalen,filealign)
	sectiontable[zsect].virtsize:=ss_zdatalen


!note: rawsize will be recalculated later after thunk table is added
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
!if name contains a dot, eg lib.abc, then set libno to index of "lib", and return "abc"
!otherwise return original name
	ref char s,name2
	[256]char str
	[256]char str2
	int i

	name2:=nil

	reenter::
	s:=name
	libno:=0

	while s^ do
		if s^='.' then			!assume lib.name
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

!do explicit search
	int n

	for i:=1 to nplibfiles when libinsttable[i] do
		if os_getdllprocaddr(libinsttable[i],name) then
			n:=i
			exit				!don't need the actual address; just whether it exists
		fi
	else
CPL NAME
		axerror("Can't find external function")
	od

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

!first use of this lib
	strcpy(&.str,plibfiles[n])
	strcat(&.str,".dll")
	if ndlls>=maxlibs then axerror("2:Too many libs") fi
	libno:=++ndlls

	dlltable[libno].name:=pcm_copyheapstring(&.str)
	dlltable[libno].nprocs:=1
	libnotable[n]:=libno

	return name
end

proc scanst=
!scan symbol table and build dll and imports list
!this version assumes dlls are encoded into the name of each import
!(otherwise, it means requiring a list of dlls and loading/searching for
!the names: doing real linker work.)

	int i,libno
	psymbol d
	ichar name, libname, dname, basename

	for i:=1 to ss_nsymbols do
!DNAME:="XXX"
		d:=ss_symboltable[i]
		dname:=d.name
		if d.isimported then
			if nimports>=maximports then axerror("genexe: Too many imports") fi
			++nimports
!			name:=extractlibname(dname,libno,d.moduleno)

!CPL "IMPTABLE",DNAME

			name:=extractlibname(dname,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.isexported then
!CPL "EXPORTED",D.NAME,=DNAME,MM.MODULETOSUB[D.MODULENO]
!CPL "EXPORTED",D.NAME,=DNAME
			basename:=getbasename(dname)
			if userentrypoint then
				if eqstring(basename,userentrypoint) then
					stentrypoint:=d
				fi
			else
				if eqstring(basename,"main") and not isdll then
					stentrypoint:=d
				elsif eqstring(basename,"start") and not isdll then
					stentrypoint2:=d
				elsif eqstring(basename,"dllmain") and isdll then
					stentrypoint:=d
				fi
			fi

			if nexports>=maxexports then axerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=getbasename(dname)
!CPL "EXPORTED",EXPORTTABLE[NEXPORTS].NAME

!OS_GETCH()

		fi
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref word64 p64
	psymbol d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset
!CPL "RELOCDATA",D.NAME,INDEX,THUNKOFFSET,=D.ISIMPORTED
!CPL "RELOCDATA",D.NAME,=INDEX

		case r.reloctype
		when rel32_rel then
			if not d.isimported then
				axerror("rel32/not imported")
			fi
!CPL "RELOC/REL32REL",D.NAME
			(ref word32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimported then
!CPL "IMPORTED",D.NAME
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
	psymbol d
	int index

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]

		case r.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimported then
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
!write 128-byte dos stub to dataptr
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
!offset is the next desired offset in the file
	dataptr:=datastart+offset			!data will have been cleared
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
				axerror("Entry point not found: main or start")
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
		header.dllcharacteristics:=0x40		!relocatable
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
!		sheader.characteristics:=0xC050'0080
	when idata_seg then
		aa:=0xC050'0040
		sheader.characteristics:=aa
!		sheader.characteristics:=0xC050'0040
	when code_seg then
		aa:=0x6050'0020
		sheader.characteristics:=aa
!		sheader.characteristics:=0x6050'0020
	when impdata_seg then
		aa:=0xC030'0040
		sheader.characteristics:=aa
!		sheader.characteristics:=0xC030'0040
	esac
	writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(ref sectionrec s)=
	case s.segtype
	when impdata_seg then
		writerecordx(s.bytedata,s.virtsize)		!rest of section will be zeros
		if s.rawsize>s.virtsize then
			dataptr+:=(s.rawsize-s.virtsize)
		fi

	when zdata_seg then					!nothing goes to disk
!		dataptr+:=s.rawsize
	else
		writerecordx(bufferelemptr(s.data,0),s.rawsize)
	esac
end

!proc addsearchlib(ichar name) =
!	name:=changeext(name,"")
!!CPL "ADD SEARRCH"
!
!	for i to npsearchlibs do
!		if eqstring(psearchlibs[i],name) then return fi
!	od
!
!	if npsearchlibs>=maxpsearchlibs then
!		axerror("Too many LIB files")
!	fi
!!CPL "ADDSEARCH",name
!!	searchlibs[++nsearchlibs]:=pcm_copyheapstring(changeext(name,""))
!	psearchlibs[++npsearchlibs]:=pcm_copyheapstring(name)
!end

proc setuplibfiles=
!collare external libs from multiple sources:
! 1  Fixed set of libraries
! 2  .dll provided on command line
! 3  CCLIB names
! 4 Set of IMPORTDLL names
	static []ichar stdlibs = ("msvcrt","gdi32","user32","kernel32")

	for i to stdlibs.len do
		pcl_addlibfile(stdlibs[i], 'D')
	od

!	npsearchlibs:=0
!
!	psearchlibs[1]:="msvcrt"
!	psearchlibs[2]:="gdi32"
!	psearchlibs[3]:="user32"
!	psearchlibs[4]:="kernel32"
!	npsearchlibs:=4	
!
!	for i to nplibfiles do addsearchlib(plibfiles[i]) od
!	for i to npdllnametable when pdllnametable[i]^<>'$' do
!		addsearchlib(pdllnametable[i])
!	od
!
!CPL "SEARCH LIBS:"
!FOR I TO NPSEARCHLIBS DO
!	CPL PSEARCHLIBS[I]
!OD

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
	psymbol d
	ichar basename

	phdr.timedatestamp:=0x5f89f4f8

	phdr.ordinalbase:=1
	phdr.naddrtable:=nexports
	phdr.nnamepointers:=nexports

!these are offsets from the start of the export data, from the start of the export dir
	addrtableoffset:=exportdirrec.bytes
	nametableoffset:=addrtableoffset+nexports*4
	ordtableoffset:=nametableoffset+nexports*4
	dllnameoffset:=ordtableoffset+nexports*2
	namesoffset:=dllnameoffset+strlen(dllfilename)+1

!virtoffset must be added to all above basic offsets, before being written to the file 
	virtoffset:=sectiontable[isect].virtoffset+exportdiroffset

!work out pointers into memory to receive the data
	paddrtable:=cast(pstart+addrtableoffset)
	pnametable:=cast(pstart+nametableoffset)
	pordtable:=cast(pstart+ordtableoffset)
	pdllname:=cast(pstart+dllnameoffset)
	pnames:=cast(pstart+namesoffset)

!fill in rest of export dir
	phdr.namerva:=dllnameoffset+virtoffset
	phdr.expaddressrva:=addrtableoffset+virtoffset
	phdr.namepointerrva:=nametableoffset+virtoffset
	phdr.ordtablerva:=ordtableoffset+virtoffset

	strcpy(pdllname,dllfilename)

!address table
	if nexports>maxexports then
		axerror("Too many exports - can't sort")
	fi

	sortexports(sortindex)

	for i to nexports do
!		d:=exporttable[i].def
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
	size+:=nexports*4			!address table entries
	size+:=nexports*4			!name pointers
	size+:=nexports*2			!ordinal table

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
!go through all the relocs and build the block tables, and work out overall size
!	int maxaddr:=maxrelocaddr+4096
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
!				println "	",addr:"h",addr-baseaddr:"h", relocnames[p.reloctype]
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
!
			q:=q.nextitem
		od
		if blockpadding[i] then p16++^:=0 fi

		p32:=cast(p16)

	od
end

proc sortexports([]int &sortindex)=
!sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
	psymbol d,e
!First, store 1..nexports into sortindex
	for i to nexports do
		sortindex[i]:=i
	od

!do bubble sort for now
	int swapped

!CPL "SORTEXPORTS"
!
!CPL =NEXPORTS
!RETURN

!		for i:=1 to nexports do
!
!			fprintln """#"",",exporttable[i].def.name
!		od

	repeat
		swapped:=0
		for i:=1 to nexports-1 do

			d:=exporttable[sortindex[i]].def
			e:=exporttable[sortindex[i+1]].def

!			if strcmp(d.name, e.name)>0 then
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
!apply file/image offsets to sectiontable
	int fileoffset, imageoffset,i,diroffset,impdirno,hinttableoffset,j,n
	int codesize,length,thunkoffset,offset,dirstartoffset

	fileoffset:=128+4+imagefileheader.bytes+optionalheader.bytes	!dosstub+sig
	fileoffset+:=imagesectionheader.bytes*nsections

	fileoffset:=roundtoblock(fileoffset,filealign)
	imageoffset:=4096

!Need to increase size of code segment to incorporate the thunk table
	ref byte pcode
	codesize:=sectiontable[csect].virtsize
	pcode:=bufferelemptr(ss_code,codesize)
	while codesize iand 7 do pcode++^:=0x90; ++codesize od
	thunkoffset:=codesize
	codesize+:=nimports*8

	sectiontable[csect].virtsize:=codesize
	sectiontable[csect].rawsize:=roundtoblock(codesize,filealign)

!have to actually add the extra memory now.
	buffercheck(ss_code, codesize-thunkoffset+16)		!just ensure it's there for now

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

!Work out offsets within import directory
!assume dll/imports have been set up
!diroffset starts off as virtual offset of start of impdata section

	diroffset+:=(ndlls+1)*importdirrec.bytes			!need blank entry as terminator

!diroffset now points to import name table
!usual arrangements is for all import name table, followed by all import addr tables

	for i to ndlls do
		dlltable[i].nametableoffset:=diroffset				!data will be filled in later
		diroffset+:=(dlltable[i].nprocs+1)*8
	od
	fileiatoffset:=diroffset
!CPL =DIROFFSET
	for i to ndlls do
		dlltable[i].addrtableoffset:=diroffset				!data will be filled in later
		diroffset+:=(dlltable[i].nprocs+1)*8
	od
	fileiatsize:=diroffset-fileiatoffset

!diroffset now points to hint/name table, which is shared by all libs
!At this point, I need to write into an actual impdata segment, which doesn't
!exist yet. So I need to do a first pass over the import names to work out the size
	hinttableoffset:=diroffset
	for i to nimports do
		length:=strlen(importtable[i].name)+3
		if length iand 1 then ++length fi		!keep even
		importtable[i].hintnameoffset:=diroffset
		diroffset+:=length
	od

!need also space for the names of the libs

!need I think to get to next multiple of four
	diroffset:=roundtoblock(diroffset,4)

	for i to ndlls do
		length:=strlen(dlltable[i].name)+1
		if length iand 1 then ++length fi		!keep even
		dlltable[i].dllextraoffset:=diroffset
		diroffset+:=dlltable[i].nprocs*4		!space for back-links to dir entry
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

!offset contains now the overall size of the import directory
!diroffset contains is the overall size of the image

!finish off last section data, and compute final file and image sizes
	sectiontable[impdirno].virtsize:=offset
	sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
	filesize:=roundtoblock(fileoffset+offset,filealign)

	imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

	ref byte pimpdir

	pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

!prepare the thunk area in the code segment
	ref importdirrec pdir
	ref int64 paddr,pname
	int iatoffset
	pdir:=cast(pimpdir)

!start fill in details within the import directory section
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

!Fill in the hint/name table
	ref byte phint
	ref word32 pextra

	for i to nimports do
		phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
		phint+:=2					!leave hint as 0
		strcpy(cast(phint),importtable[i].name)
	od
!same for lib names (no hint here, but re-use phint anyway)
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

!write the thunk table
	ref byte thunkptr,codebase
	int thunkaddr
	thunkptr:=bufferelemptr(ss_code,thunkoffset)
	codebase:=bufferelemptr(ss_code,0)


!-----------------------------------------------
!	for i to nimports do
!		importtable[i].thunkoffset:=thunkptr-codebase
!		thunkptr++^:=0x48
!		thunkptr++^:=0xFF
!		thunkptr++^:=0x24
!		thunkptr++^:=0x25
!		thunkaddr:=imagebase+importtable[i].iatoffset
!		(ref int32(thunkptr)^:=thunkaddr)
!
!		thunkptr+:=4
!	od
!-----------------------------------------------
	for i to nimports do
		importtable[i].thunkoffset:=thunkptr-codebase
!INT IMOFFSET:=THUNK

		thunkptr++^:=0xFF
		thunkptr++^:=0x25
!CPL "THUNK",IMPORTTABLE[I].NAME,=IMPORTTABLE[I].IATOFFSET:"H"
!CPL "THUNK2",=CODEBASE
!CPL =sectiontable[csect].virtoffset,=THUNKOFFSET

!INT IATOFFSET:=IMPORTTABLE[I].IATOFFSET

!		thunkaddr:=imagebase+importtable[i].iatoffset
!		thunkaddr:=importtable[i].iatoffset-(sectiontable[csect].virtoffset+thunkoffset+6)
		thunkaddr:=importtable[i].iatoffset-(sectiontable[csect].virtoffset+
			importtable[i].thunkoffset+6)

!		thunkaddr:=importtable[i].iatoffset
		(ref int32(thunkptr)^:=thunkaddr)
		thunkptr+:=4
		thunkptr++^:=0x90
		thunkptr++^:=0x90
	od
!-----------------------------------------------
end
=== pc_writemx.m 0 0 17/25 ===
!Translate SS data directly into MCB block, then write as mx/ml file

int nsymimports=0, nsymexports=0

ref dbuffer dest

global proc writemcx(ichar filename)=
	int n

!IF PSTENTRYPOINT THEN
!CPL "WRITEMCX",=PSTENTRYPOINT.NAME,=PMAINLIB
!FI
!
	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	dest:=buffercreate()

	genword32(mcxsig)

	genbyte(version_dir)
	genstring("0.1234")

	scansymbols()
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
	for i to nplibfiles do
		if plibtypes[i]='D' then ++ndlls else ++nlibs fi
	od

	genbyte(dlls_dir)
	genword32(ndlls)
	for i to nplibfiles when plibtypes[i]='D' do
		genstring(plibfiles[i])
	od

	genbyte(libs_dir)
	genword32(nlibs)
	for i to nplibfiles when plibtypes[i]='L' do
		genstring(plibfiles[i])
	od

	writesymbols()

	genbyte(end_dir)

	writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n,count
	psymbol d
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
				if d.isimported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.isimported then
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

proc scansymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.isexported then d.expindex:=++nsymexports fi
		if d.isimported then d.impindex:=++nsymimports fi
	od
end

proc writesymbols=
	psymbol d
!	u64 epoffset:=-1
	int n
	ichar name

	genbyte(importsymbols_dir)
	genword32(nsymimports)

	for i to ss_nsymbols when ss_symboltable[i].impindex do
		genstring(ss_symboltable[i].name)
	od

	genbyte(exportsymbols_dir)
	genword32(nsymexports)

	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then

			if d=pstentrypoint and plibmode then
				name:=d.name
!CPL "ENTRY POINT:",NAME,=D.OFFSET
			else
				name:=getbasename(d.name)
			fi
!			if epoffset=-1 and (eqstring(name,"start") or eqstring(name,"main")) then
!				epoffset:=d.offset
!			fi
			genstring(name)

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

	genbyte(entry_dir)		!must be present; writes 0xFFFFFFFF when no entry point
	if pstentrypoint then
		genword32(pstentrypoint.offset)
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
=== pc_writess.m 0 0 18/25 ===
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
			gs_str(dest,"	[")
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
		gs_str(dest,ss_symboltable[r.stindex].name)
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
		gs_strln(dest,ss_symboltable[i].name)
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
		gs_value("		Name Table Offset",dlltable[i].nametableoffset)
		gs_value("		Addr Table Offset",dlltable[i].addrtableoffset)
		gs_value("		DLL Name Offset  ",dlltable[i].dllnameoffset)
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

			gs_value("	IAT Offset        ",p.iatoffset)
			gs_value("	Thunk Offset      ",p.thunkoffset)
			gs_value("	Hint/Name Offset  ",p.hintnameoffset)

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

=== pci_mcl.m 0 0 19/25 ===
export type mcloperand = ref mclopndrec

export record mclopndrec =		!up to 32 bytes
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		int64 value		!immediate value
		real64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
		int tempno
	end

	byte size			!byte size of operand: usually 1,2,4,8,16
	byte mode			!a_reg etc, low level operand details
	byte reg			!0, or main register
	byte regix			!0, or index register

	byte valtype		!interpretation of def/code/value/svalue
	byte scale			!1, or scale factor for regix
!	int16 offset		!extra offset to label for mem/imm modes
	int32 offset		!extra offset to label for mem/imm modes

	byte addrsize		!4 or 8 for a_mem when regs are involved
!	byte valtypex		!valtypex is 0 (no value or int) or 'R'/'S' in ma

[13]BYTE DUMMY

end

export record mclrec =		!32 bytes
	ref mclrec nextmcl
	mcloperand a,b
	byte opcode
	union
		byte cond
		byte isglobal
		byte sysindex
	end
!	byte fileno
	byte c
!	byte spare1, spare2
	int pos:(lineno:24, fileno:8)
	ichar comment
	[r0..r15]byte regend		!1 indicates register freed.

[5]BYTE DUMMY

end

global tabledata() [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(temp_val,		$),		!temporary
	(label_val,		$),		!label index
!	(labelind_val,	$),		!label index
	(name_val,		$),		!immediate string must be output as ah unquoted name
	(syscall_val,	$),		!
end

export tabledata() []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_procstart,		$,		0,		0),		!
	(m_procend,			$,		0,		0),		!
	(m_comment,			$,		0,		0),		!
	(m_blank,			$,		0,		0),		!
	(m_deleted,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!
	(m_definereg,		$,		0,		0),		!

	(m_label,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
	(m_param,			$,		1,		0),		!
!	(m_assembly,		$,		1,		0),		!
!	(m_proc,			$,		1,		0),		!

	(m_mov,				$,		2,		0),		!
	(m_push,			$,		1,		0),		!
	(m_pop,				$,		1,		0),		!
	(m_lea,				$,		2,		0),		!
	(m_cmovcc,			$,		2,		0),		!

	(m_movd,			$,		2,		0),		!
	(m_movq,			$,		2,		0),		!

	(m_movsx,			$,		2,		0),		!
	(m_movzx,			$,		2,		0),		!
	(m_movsxd,			$,		2,		0),		!

	(m_call,			$,		1,		0xE8),		!
	(m_ret,				$,		0,		0xC3),	!
	(m_leave,			$,		0,		0xC9),	!
	(m_retn,			$,		1,		0),		!

	(m_jmp,				$,		1,		0xE9),	!
	(m_jmpcc,			$,		2,		0),		!
	(m_xchg,			$,		2,		0),		!

	(m_add,				$,		2,		0),		!
	(m_sub,				$,		2,		5),		!
	(m_adc,				$,		2,		2),		!
	(m_sbb,				$,		2,		3),		!
	(m_imul,			$,		1,		5),		!
	(m_mul,				$,		1,		4),		!
	(m_imul2,			$,		2,		0),		!
	(m_imul3,			$,		3,		0),		!

	(m_idiv,			$,		1,		7),		!
	(m_div,				$,		1,		6),		!

	(m_andx,			$,		2,		0x04),	!
	(m_orx,				$,		2,		0x01),	!
	(m_xorx,			$,		2,		0x06),	!
	(m_test,			$,		2,		0),		!

	(m_cmp,				$,		2,		0x07),	!

	(m_shl,				$,		2,		0x04),	!
	(m_sar,				$,		2,		0x07),	!
	(m_shr,				$,		2,		0x05),	!
	(m_rol,				$,		2,		0x00),	!
	(m_ror,				$,		2,		0x01),	!
	(m_rcl,				$,		2,		0x02),	!
	(m_rcr,				$,		2,		0x03),	!

	(m_neg,				$,		1,		3),		!
	(m_notx,			$,		1,		2),		!

	(m_inc,				$,		1,		0),		!
	(m_dec,				$,		1,		1),		!

	(m_cbw,				$,		0,		0),	!
	(m_cwd,				$,		0,		0),	!
	(m_cdq,				$,		0,		0),		!
	(m_cqo,				$,		0,		0),		!
	(m_setcc,			$,		2,		0),		!

	(m_bsf,				$,		2,		0xBC),	!
	(m_bsr,				$,		2,		0xBD),	!

	(m_sqrtsd,			$,		2,		0x51),	!
	(m_sqrtss,			$,		2,		0x51),	!
	(m_addss,			$,		2,		0x58),	!
	(m_subss,			$,		2,		0x5C),	!
	(m_mulss,			$,		2,		0x59),	!
	(m_divss,			$,		2,		0x5E),	!

	(m_addsd,			$,		2,		0x58),	!
	(m_subsd,			$,		2,		0x5C),	!
	(m_mulsd,			$,		2,		0x59),	!
	(m_divsd,			$,		2,		0x5E),	!

	(m_comiss,			$,		2,		0),		!
	(m_comisd,			$,		2,		0),		!
	(m_xorpd,			$,		2,		0x57),	!
	(m_xorps,			$,		2,		0x57),	!
	(m_andpd,			$,		2,		0x54),	!
	(m_andps,			$,		2,		0x54),	!
	(m_pxor,			$,		2,		0xEF),	!
	(m_pand,			$,		2,		0xDB),	!
	(m_cvtss2si,		$,		2,		0),		!
	(m_cvtsd2si,		$,		2,		0),		!
	(m_cvttss2si,		$,		2,		0),		!
	(m_cvttsd2si,		$,		2,		0),		!

	(m_cvtsi2ss,		$,		2,		0),		!
	(m_cvtsi2sd,		$,		2,		0),		!

	(m_cvtsd2ss,		$,		2,		0),		!
	(m_cvtss2sd,		$,		2,		0),		!

	(m_movdqa,			$,		2,		0x66),	!
	(m_movdqu,			$,		2,		0xF3),	!

	(m_pcmpistri,		$,		3,		0x63),	!
	(m_pcmpistrm,		$,		3,		0x62),	!

	(m_fld,				$,		1,		0),		!
	(m_fst,				$,		1,		2),		!
	(m_fstp,			$,		1,		3),		!

	(m_fild,			$,		1,		0),		!
	(m_fist,			$,		1,		2),		!
	(m_fistp,			$,		1,		3),		!

	(m_fadd,			$,		0,		0xC1),	!
	(m_fsub,			$,		0,		0xE9),	!
	(m_fmul,			$,		0,		0xC9),	!
	(m_fdiv,			$,		0,		0xF9),	!
	(m_fsqrt,			$,		0,		0xFA),	!
	(m_fsin,			$,		0,		0xFE),	!
	(m_fcos,			$,		0,		0xFF),	!
	(m_fsincos,			$,		0,		0xFB),	!
	(m_fptan,			$,		0,		0xF2),	!
	(m_fpatan,			$,		0,		0xF3),	!
	(m_fabs,			$,		0,		0xE1),	!
	(m_fchs,			$,		0,		0xE0),	!

	(m_minss,			$,		2,		0x5D),	!
	(m_maxss,			$,		2,		0x5F),	!
	(m_minsd,			$,		2,		0x5D),	!
	(m_maxsd,			$,		2,		0x5F),	!

	(m_db,				$,		1,		0),		!
	(m_dw,				$,		1,		0),		!
	(m_dd,				$,		1,		0),		!
	(m_dq,				$,		1,		0),		!
	(m_ddoffset,		$,		1,		0),		!

	(m_segment,			$,		1,		0),		!
	(m_isegment,		$,		0,		0),		!
	(m_zsegment,		$,		0,		0),		!
	(m_csegment,		$,		0,		0),		!

	(m_align,			$,		1,		0),		!
	(m_resb,			$,		1,		1),		!
	(m_resw,			$,		1,		2),		!
	(m_resd,			$,		1,		4),		!
	(m_resq,			$,		1,		8),		!

	(m_xlat,			$,		0,		0xD7),	!
	(m_loopnz,			$,		1,		0xE0),	!
	(m_loopz,			$,		1,		0xE1),	!
	(m_loopcx,			$,		1,		0xE2),	!
	(m_jecxz,			$,		1,		0xE3),	!
	(m_jrcxz,			$,		1,		0xE3),	!

	(m_cmpsb,			$,		0,		0),		!
	(m_cmpsw,			$,		0,		0),		!
	(m_cmpsd,			$,		0,		0),		!
	(m_cmpsq,			$,		0,		0),		!

	(m_rdtsc,			$,		0,		0x31),	!
	(m_popcnt,			$,		2,		0),		!

	(m_finit,			$,		0,		0),		!

	(m_fldz,			$,		0,		0xEE),	!
	(m_fld1,			$,		0,		0xE8),	!
	(m_fldpi,			$,		0,		0xEB),	!
	(m_fld2t,			$,		0,		0xE9),	!
	(m_fld2e,			$,		0,		0xEA),	!
	(m_fldlg2,			$,		0,		0xEC),	!
	(m_fldln2,			$,		0,		0xED),	!

	(m_cpuid,			$,		0,		0),		!

	(m_halt,			$,		0,		0xF4),	!
end

export tabledata() [0:]ichar regnames, [0:]byte regcodes =
	(rnone=0,	$,	0),			!
	(r0,		$,	0),			!d0 rax
	(r1,		$,	10),		!d1 r10
	(r2,		$,	11),		!d2 r11
	(r3,		$,	7),			!d3 rdi
	(r4,		$,	3),			!d4 rbx
	(r5,		$,	6),			!d5 rsi
	(r6,		$,	12),		!d6 r12
	(r7,		$,	13),		!d7 r13
	(r8,		$,	14),		!d8 r14
	(r9,		$,	15),		!d9 r15
	(r10,		$,	1),			!d10 rcx
	(r11,		$,	2),			!d11 rdx
	(r12,		$,	8),			!d12 r8
	(r13,		$,	9),			!d13 r9
	(r14,		$,	5),			!d14 rbp
	(r15,		$,	4),			!d15 rsp

	(r16,		$,	4),			!b0h ah
	(r17,		$,	7),			!b1h bh
	(r18,		$,	5),			!b10h ch
	(r19,		$,	6),			!b11h dh
end

export const rframe = r14
export const rstack = r15

global tabledata() [0:]ichar condnames, [0:]ichar asmcondnames,
		[0:]int asmrevcond =

	(ov_cond=0,		"ov",	"o",		nov_cond),
	(nov_cond=1,	"nov",	"no",		ov_cond),

	(ltu_cond=2,	"ltu",	"b",		geu_cond),
	(geu_cond=3,	"geu",	"ae",		ltu_cond),

	(eq_cond=4,		"eq",	"z",		ne_cond),
	(ne_cond=5,		"ne",	"nz",		eq_cond),

	(leu_cond=6,	"leu",	"be",		gtu_cond),
	(gtu_cond=7,	"gtu",	"a",		leu_cond),

	(s_cond=8,		"s",	"s",		ns_cond),
	(ns_cond=9,		"ns",	"ns",		s_cond),

	(p_cond=10,		"p",	"p",		np_cond),
	(np_cond=11,	"np",	"np",		p_cond),

	(lt_cond=12,	"lt",	"l",		ge_cond),
	(ge_cond=13,	"ge",	"ge",		lt_cond),

	(le_cond=14,	"le",	"le",		gt_cond),
	(gt_cond=15,	"gt",	"g",		le_cond),

	(flt_cond=16,	"flt",	"b",		fge_cond),		!special floating point codes
	(fge_cond=17,	"fge",	"ae",		flt_cond),
	(fle_cond=18,	"fle",	"be",		fgt_cond),
	(fgt_cond=19,	"fgt",	"a",		fle_cond)
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
	("d0",		8,	r0),		!rax	d0..d9 are for general use
	("d1",		8,	r1),		!r10	d0..d2 are volatile in ABI
	("d2",		8,	r2),		!r11

	("d3",		8,	r3),		!rdi	d3..d9 are preserved across funcs in ABI
	("d4",		8,	r4),		!rbx
	("d5",		8,	r5),		!rsi
	("d6",		8,	r6),		!r12
	("d7",		8,	r7),		!r13
	("d8",		8,	r8),		!r14
	("d9",		8,	r9),		!r15

	("d10",		8,	r10),		!rcx	d10..d13 are win64 ABI register passing regs
	("d11",		8,	r11),		!rdx	..
	("d12",		8,	r12),		!r8		..
	("d13",		8,	r13),		!r9		..

	("d14",		8,	r14),		!rbp	frame pointer
	("d15",		8,  r15),		!rsp	stack pointer

	("a0",		4,	r0),
	("a1",		4,	r1),
	("a2",		4,	r2),
	("a3",		4,	r3),
	("a4",		4,	r4),
	("a5",		4,	r5),
	("a6",		4,	r6),
	("a7",		4,	r7),
	("a8",		4,	r8),
	("a9",		4,	r9),
	("a10",		4,	r10),
	("a11",		4,	r11),
	("a12",		4,	r12),
	("a13",		4,	r13),
	("a14",		4,	r14),
	("a15",		4,  r15),

	("w0",		2,	r0),
	("w1",		2,	r1),
	("w2",		2,	r2),
	("w3",		2,	r3),
	("w4",		2,	r4),
	("w5",		2,	r5),
	("w6",		2,	r6),
	("w7",		2,	r7),
	("w8",		2,	r8),
	("w9",		2,	r9),
	("w10",		2,	r10),
	("w11",		2,	r11),
	("w12",		2,	r12),
	("w13",		2,	r13),
	("w14",		2,	r14),
	("w15",		2,  r15),


	("b0",		1,	r0),
	("b1",		1,	r1),
	("b2",		1,	r2),
	("b3",		1,	r3),
	("b4",		1,	r4),
	("b5",		1,	r5),
	("b6",		1,	r6),
	("b7",		1,	r7),
	("b8",		1,	r8),
	("b9",		1,	r9),
	("b10",		1,	r10),
	("b11",		1,	r11),
	("b12",		1,	r12),
	("b13",		1,	r13),
	("b14",		1,	r14),
	("b15",		1,  r15),
	("b16",		1,  r16),
	("b17",		1,  r17),
	("b18",		1,  r18),
	("b19",		1,  r19),

	("rax",		8,	r0),
	("rbx",		8,	r4),
	("rcx",		8,	r10),
	("rdx",		8,	r11),
	("rsi",		8,	r5),
	("rdi",		8,	r3),
	("rbp",		8,	r14),
	("rsp",		8,	r15),
	("r8",		8,	r12),
	("r9",		8,	r13),
	("r10",		8,	r1),
	("r11",		8,	r2),
	("r12",		8,	r6),
	("r13",		8,	r7),
	("r14",		8,	r8),
	("r15",		8,	r9),

	("eax",		4,	r0),
	("ebx",		4,	r4),
	("ecx",		4,	r10),
	("edx",		4,	r11),
	("esi",		4,	r5),
	("edi",		4,	r3),
	("ebp",		4,	r14),
	("esp",		4,	r15),
	("r8d",		4,	r12),
	("r9d",		4,	r13),
	("r10d",	4,	r1),
	("r11d",	4,	r2),
	("r12d",	4,	r6),
	("r13d",	4,	r7),
	("r14d",	4,	r8),
	("r15d",	4,	r9),

	("ax",		2,	r0),
	("bx",		2,	r4),
	("cx",		2,	r10),
	("dx",		2,	r11),
	("si",		2,	r5),
	("di",		2,	r3),
	("bp",		2,	r14),
	("sp",		2,	r15),
	("r8w",		2,	r12),
	("r9w",		2,	r13),
	("r10w",	2,	r1),
	("r11w",	2,	r2),
	("r12w",	2,	r6),
	("r13w",	2,	r7),
	("r14w",	2,	r8),
	("r15w",	2,	r9),


	("al",		1,	r0),
	("bl",		1,	r4),
	("cl",		1,	r10),
	("dl",		1,	r11),

	("ah",		1,	r16),
	("bh",		1,	r17),
	("ch",		1,	r18),
	("dh",		1,	r19),

	("sil",		1,	r5),
	("dil",		1,	r3),
	("bpl",		1,	r14),
	("spl",		1,	r15),

	("r8b",		1,	r12),
	("r9b",		1,	r13),
	("r10b",	1,	r1),
	("r11b",	1,	r2),
	("r12b",	1,	r6),
	("r13b",	1,	r7),
	("r14b",	1,	r8),
	("r15b",	1,	r9),

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

!global tabledata() [0:]ichar condnames =
!
!	(ov_cond	= 0,	"o"),
!	(nov_cond	= 1,	"no"),
!
!	(ltu_cond	= 2,	"b"),
!	(geu_cond	= 3,	"ae"),
!
!	(eq_cond	= 4,	"z"),
!	(ne_cond	= 5,	"nz"),
!
!	(leu_cond	= 6,	"be"),
!	(gtu_cond	= 7,	"a"),
!
!	(s_cond		= 8,	"s"),
!	(ns_cond	= 9,	"ns"),
!
!	(p_cond		= 10,	"p"),
!	(np_cond	= 11,	"np"),
!
!	(lt_cond	= 12,	"l"),
!	(ge_cond	= 13,	"ge"),
!
!	(le_cond	= 14,	"le"),
!	(gt_cond	= 15,	"g"),
!end

export tabledata []ichar jmpccnames, []byte jmpcccodes =
	("jo",		ov_cond),
	("jno",		nov_cond),
	("jb",		ltu_cond),
	("jae",		geu_cond),
	("jz",		eq_cond),
	("jnz",		ne_cond),
	("jbe",		leu_cond),
	("ja",		gtu_cond),
	("js",		s_cond),
	("jns",		ns_cond),
	("jp",		p_cond),
	("jnp",		np_cond),
	("jl",		lt_cond),
	("jge",		ge_cond),
	("jle",		le_cond),
	("jg",		gt_cond),
	("jc",		ltu_cond),
	("jnc",		geu_cond),
end


export tabledata []ichar setccnames, []byte setcccodes =
	("seto",	ov_cond),
	("setno",	nov_cond),
	("setb",	ltu_cond),
	("setae",	geu_cond),
	("setz",	eq_cond),
	("setnz",	ne_cond),
	("setbe",	leu_cond),
	("seta",	gtu_cond),
	("sets",	s_cond),
	("setns",	ns_cond),
	("setp",	p_cond),
	("setnp",	np_cond),
	("setl",	lt_cond),
	("setge",	ge_cond),
	("setle",	le_cond),
	("setg",	gt_cond),
end

export tabledata []ichar cmovccnames, []byte cmovcccodes =
	("cmovo",	ov_cond),
	("cmovno",	nov_cond),
	("cmovb",	ltu_cond),
	("cmovae",	geu_cond),
	("cmovz",	eq_cond),
	("cmovnz",	ne_cond),
	("cmovbe",	leu_cond),
	("cmova",	gtu_cond),
	("cmovs",	s_cond),
	("cmovns",	ns_cond),
	("cmovp",	p_cond),
	("cmovnp",	np_cond),
	("cmovl",	lt_cond),
	("cmovge",	ge_cond),
	("cmovle",	le_cond),
	("cmovg",	gt_cond),
end

!export tabledata() [0:]ichar segmentnames =
!	(no_seg=0,		$),
!	(code_seg,		$),
!	(idata_seg,		$),
!	(zdata_seg,		$),
!	(rodata_seg,	$),
!	(impdata_seg,	$),
!end

export tabledata() [0:]ichar reftypenames =	!use during pass2
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

export tabledata() [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_cond,	$),		! a condition code for jcc/setcc
	(a_xreg,	$),		! xmm register
	(a_wreg,	$),		! Wide integer register, means Ri and Ri+1
!	(a_regvar,	$),		! Reg holding a regvar
!	(a_xregvar,	$),		! Reg holding a regvar
end

global int mlabelno
!global byte foptimise

global const maxoperands=200

global [maxoperands+10]pclstackrec pclopndstack
global int noperands
global int mstackdepth

global record pclstackrec =
	byte fmt
	byte loc
	byte reg
	byte float
!	byte high			!1 when high word of wide
!	byte low			!1 when low word of wide
	byte wide			!0 or 'H' or 'L'
	int16 offset
	union
		int value
		real xvalue
		ichar svalue
		psymbol def
		int labno
	end
end

global ref[]pclstackrec pclstack
global pclstackrec pclstackzero

!Where any active operand is located:

global tabledata() [0:]ichar locnames =
	(no_loc=0,		$),			! not set
	(reg_loc,		$),			! in a d64 register
	(xreg_loc,		$),			! in an x64
	(stack_loc,		$),			! on the hardware stack (must be ordered properly)
	(imm_loc,		$),			! still as an immediate value
	(mem_loc,		$),			! still in variable
	(memhigh_loc,	$),			! still in variable
	(regvar_loc,	$),			! still in a reg variable
	(xregvar_loc,	$),			! still in an xreg variable
end

global tabledata() [0:]ichar fmtnames, [0:]byte loccodes, [0:]byte floatloc,
		[0:]byte loadfmt, [0:]byte pushfmt =
	(nofmt_void=0,	$,	0,				0,	0,			0),
	(reg_d64,		$,	reg_loc,		0,	0,			stack_d64),

	(xreg_x64,		$,	xreg_loc,		1,	0,			stack_x64),
	(xreg_x32,		$,	xreg_loc,		1,	0,			stack_x32),

	(stack_d64,		$,	stack_loc,		0,	reg_d64,	0),
	(stack_x64,		$,	stack_loc,		1,	xreg_x64,	0),
	(stack_x32,		$,	stack_loc,		1,	xreg_x32,	0),

	(imm_d64,		$,	imm_loc,		0,	reg_d64,	stack_d64),
	(imm_x64,		$,	imm_loc,		1,	xreg_x64,	stack_x64),
	(imm_x32,		$,	imm_loc,		1,	xreg_x32,	stack_x32),
	(imm_str,		$,	imm_loc,		0,	reg_d64,	stack_d64),
	(imm_memaddr,	$,	imm_loc,		0,	reg_d64,	stack_d64),

	(imm_label,		$,	imm_loc,		0,	reg_d64,	stack_d64),
	(imm_labaddr,	$,	imm_loc,		0,	reg_d64,	stack_d64),

	(mem_d64,		$,	mem_loc,		0,	reg_d64,	stack_d64),
	(mem_x64,		$,	mem_loc,		1,	xreg_x64,	stack_x64),
	(mem_x32,		$,	mem_loc,		1,	xreg_x32,	stack_x32),

	(memhigh_d64,	$,	mem_loc,		0,	reg_d64,	stack_d64),

	(regvar_d64,	$,	regvar_loc,		0,	reg_d64,	stack_d64),
	(xregvar_x64,	$,	xregvar_loc,	1,	xreg_x64,	stack_x64),
end

global const regmax=r9				!can use r0 to regmax inclusive; only those regs
global const xregmax=xr6

global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
global [r0..r15]byte xregset		!same for xregs

global [r0..r15]byte isregvar
global [r0..r15]byte isxregvar

!These vars give info on the resources used by a proc

!global [r0..r15]byte allregmap		!all regs used
!global [r0..r15]byte allxregmap		!all xregs used

global int inf_proccalls
global int inf_proclocals
global int inf_procxlocals

global int inf_leafproc
global int inf_highreg
global int inf_highxreg
global int inf_maxargs
export int inf_assem

global int inf_r10used		!these may be set in pass2 when occupied by params
global int inf_r11used
global int inf_r13used

!global [16]int inf_dsaveregs
!global [16]int inf_xsaveregs
!global int inf_ndsaveregs	!set in procentry; at one or both will be zero
!global int inf_ndsavepush
!global int inf_nxsaveregs
!global int inf_dsaveoffset
!global int inf_xsaveoffset

global [16]int dsaveregs
global [16]int xsaveregs
global int ndsaveregs	!set in procentry; at one or both will be zero
global int ndsavepush
global int nxsaveregs
global int dsaveoffset
global int xsaveoffset
global int needstackframe
global int framebytes
global int needshadow48
global int needshadow32		!has value 0, 32 or 40, the actual spaced needed

global byte noxorclear		!1 to suppress xor optimisation

global const wd = 4
global const xc = 3
global const yb = 2
global const za = 1

global const xb = 2
global const ya = 1

global const xa = 1

global tabledata() [0:]ichar xregnames =
	(xnone=0,	$),
	(xr0,		$),
	(xr1,		$),
	(xr2,		$),
	(xr3,		$),
	(xr4,		$),
	(xr5,		$),
	(xr6,		$),
	(xr7,		$),
	(xr8,		$),
	(xr9,		$),
	(xr10,		$),
	(xr11,		$),
	(xr12,		$),
	(xr13,		$),
	(xr14,		$),
	(xr15,		$)
end

global pcl procdefpcl
global psymbol procdef

global const maxcalldepth=16
global [maxcalldepth]int callshadow
global [maxcalldepth]int callslots
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callargs
global [maxcalldepth]byte callnvars
global int ncalldepth

global const maxparams=32
global const maxlocals=256

!these are reset at each procdef
global [maxparams]psymbol paramdefs
global [maxlocals]psymbol localdefs
global int nparams, nlocals
global int retmode
!global ref strec procdef
global int passno
global int sa_nargs

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)

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

global const rtos=rnone			!means stack operand

!global const rcx=r10
!global const rdx=r11
!global const r14=rframe
!global const r15=rstack

export ref mclrec mccode, mccodex		!genmc adds to this linked list

global int currsegment=0		!

global int currzdataalign=0
global int curridataalign=0

!global int framebytes			!local stackframe size
!global int parambytes
global int frameoffset
global int isthreadedproc
global int iscallbackproc

global int structretoffset			!0, or offset of R9 copy within struct
global ref mclrec stacksetinstr		!caller of any fn: instr that sets sp
global int currblocksize			!0, or set to largest block ret value
!global ref mclrec allmclcode
global ichar allasmstr
global int allasmstrlen

global mcloperand dstackopnd
global mcloperand dframeopnd

global mcloperand zero_opnd=nil
!global unit zero_unit

global [r0..r15,1..16]mcloperand regtable

global const maxsmallint=32
global [0..maxsmallint]mcloperand smallinttable

global [-128..64]mcloperand frameregtable

global const initstringsize	= 1024
global const initrealsize		= 16

global ref []ichar	stringtable
!global ref []int32    stringlentable
global ref []int32   stringlabtable
global ref []real	realtable
global ref []int32	reallabtable

global int stringtablesize
global int realtablesize

global int nstrings=0
global int nreals=0

!global const maxmlabelno=20'000
!global const maxlabelno=40'000
!global const maxlabelno=60'000
!global const maxlabelno=80'000
!global const maxlabelno=200'000
!global const maxlabelno=400'000
!global const maxlabelno=800'000
global const maxlabelno=6000'000
!global const maxlabelno=1400'000
global [maxlabelno]ref mclrec labeltable

!int framebytes, parambytes

global strbuffer sbuffer
global ref strbuffer dest=&sbuffer
global int destlinestart
global psymbol currasmproc
global int noregvar				!1 to inhibit strreg showing regvar names

global int mseqno

global [rtsnames.len]int rtsproclabels		!non-zero means rtsfn has been used

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

global record relocrec =			!informal version
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
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

!const max_ss_symbols=32768				!exported to coff
global const init_ss_symbols=32768				!exported to coff
!global const init_ss_symbols=16384
global ref []psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

!global ref stlipstrec globalimportlist		!all global vars and imports across all moduls

global ref[]psymbol labeldeftable

global int alineno
!global int pmlineno

global proc axerror(ichar mess)=
!	fprintln "MCL Error: # (#) on Line: # in #",mess,param
!	fprintln "MCL Error: # (#) [#]",mess,param,mseqno
	fprintln "MCL Error: #",mess
!		mlineno iand 16777215, sourcefilenames[mlineno>>24]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global const maxplibfile=50
global [0..maxplibfile]ichar plibfiles
global [0..maxplibfile]byte plibtypes
global int nplibfiles

global ref proc(ref void)hostasmhandler

global psymbol pstentrypoint
global int plibmode

!EXPORT INT NMCLREC
!EXPORT INT NMCLOPNDS
!EXPORT INT NGENSTR
!EXPORT INT NGENSTRSIZE
!EXPORT INT NGENEXT
!
!
!
!
=== pc_win64.m 0 0 20/25 ===
export function pcl_genmcl(int optim=0)int=
	fixuppcl()
	genmcl(optim)

	return 1
end

export function pcl_writeasmfile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)

	writeasmfile(filename)
	return 1
end

export function pcl_getasmstring(int optim=0)ichar=
	ref strbuffer asmstr
	pcl_genmcl(optim)
	asmstr:=getmclstr()
	return asmstr.strptr
end

export function pcl_writeexefile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)
	return writeexefile(filename,optim,0)
end

export function pcl_writelibfile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)

	genss()
	writemcx(filename)

	return 1
end

!export function pcl_writeobjfile(ichar filename, int optim=0)int=
!	return writeexefile(filename,optim,1)
!end

function writeexefile(ichar filename, int optim=0, int gendll=0)int=
	[300]char asmfilename
	[300]char str

	genss()

	initsectiontable()

	genexe(nil, filename, gendll)
	writeexe(filename, gendll)
	return 1
end

!function oldwriteexefile(ichar filename, int optim=0, int gendll=0)int=
!	[300]char asmfilename
!	[300]char str
!
!	strcpy(asmfilename, changeext(filename,"asm"))
!	if not pcl_writeasmfile(asmfilename,optim) then return 0 fi
!
!	fprint @str,"/m/aa # #",&.asmfilename,(gendll|"-dll"|"-exe")
!
!	if system(str)=0 then
!		return 1
!	else
!		return 0
!	fi
!end

proc writeasmfile(ichar filename)=
!already generated as mcl
	ref strbuffer asmstr
	asmstr:=getmclstr()
	writegsfile(filename,asmstr)
	gs_free(asmstr)
end

export function pcl_readrts(ichar filename)int=
	if maxuserlabel then		!probably via parsing
		labelnooffset:=maxuserlabel
	else						!probably via API
		labelnooffset:=++plabelno
	fi
	if not parse_readrts() then
		println "No RTS file found"
		return 0
	fi
	return 1
end

export proc pcl_endprog(int fixup=1, dorts=1)=

	if dorts then
		pcl_readrts("rts.pcl")
	fi

	pcl_end(fixup)
end

export proc pcl_showss(ichar filename,int fexe)=
	ref strbuffer ssstr

	gs_init(dest)
!*!	ssstr:=writessdata(fexe)
	writegsfile(filename,ssstr)
end

export proc pcl_writeclangfile(ichar filename)=
!	genclang(filename)
end

export function pcl_runlibfile(ichar filename, int optim=0, fshowmx, cmdskip)int=
	ref librec plib

!	REF BYTE PP
!	PP:=READFILE("PI.MX")
!	IF PP=NIL THEN STOP FI
!	PLIB:=LOADMEMMCB("DUMMYNAME",PP)

	pcl_genmcl(optim)

	genss()
	plib:=writememlib(filename)

	loadmemmcu(plib)

	fixuplib(plib)

	if fshowmx then
		initlogfile()
		showlibs()
		closelogfile()
	else
		runprogram(plib, cmdskip)
	fi
	return 1
end

=== rundecls.m 0 0 21/25 ===
!Declarations for M-Code scheme
!Terms:
! MCU		MCode Unit, binary code/data/imports/relocs for whole program (LIBREC)
! MCB		MCU rendered to flat data block, written out as .mx/.ml file
! MCX		MCU with allocations, imports and fixups done to make it ready to run
! LIB		Informal reference to MCU or MCX in-memory data; or to MX/ML file
!
! MCU is created from SS data (which normally was used to generate EXE)
! An MCU block is either converted to MCB which is then sent to a file;
! or it is directly fixed up into MCX to allow immediate execution

!single byte tags in mcx file

global const mcxsig = 'MCX\e'

global tabledata() [0:]ichar mcxdirnames =
	(pad_dir = 0,		$),		! nothing follows except next tag; for padding/alignment
	(version_dir,		$),		! STR string follows with version code (stringz)
	(code_dir,			$),		! N(u32) then N bytes of code data
	(idata_dir,			$),		! N(u32) then N bytes init data
	(zdata_dir,			$),		! N(u32) (no data follows)
	(reloc_dir,			$),		! N(u32) then N records follow
	(dlls_dir,			$),		! N(u32) then N STR items, the DLL base names
	(libs_dir,			$),		! N(u32) then N STR items, the MCX base names
	(importsymbols_dir,	$),		! N(u32) then N STR items, the imported names
	(exportsymbols_dir,	$),		! N(u32) then N STR items, the exported names
	(exportsegs_dir,	$),		! N(u32) then N u8 items, each is a segment code
	(exportoffsets_dir,	$),		! N(u32) then N u32 items, each an offset in the segment
	(entry_dir,			$),		! N(u32) N is a byte offset within code segment for entry point
	(end_dir,			$),		! nothing follows; end of file
end

!Relocation codes

global tabledata() [0:]ichar mcxrelocnames =
	(no_rel = 0,		$),

	(locabs32_rel,	"locabs32"),		! add target segment address to 32-bit offset
	(locabs64_rel,	"locabs64"),		! add target segment address to 64-bit offset

	(impabs32_rel,	"impabs32"),		! replace 32-bit 0-field with address of imported symbol
	(impabs64_rel,	"impabs64"),		! replace 64-bit 0-field with address of imported symbol

	(imprel32_rel,	"imprel32"),		! replace 32-bit 0-field with offset of thunk entry for symbol
end

! Explanation of reloc codes
! No reloc
!	For local call/jmp, which are /only/ within code segment, no fixups are needed
!
! Locabs32/Locabs64
!	Reloc field contains offset of location within target segment, plus any
!   constant offset (eg. A+3 has offset of A, plus 3)
!   Baseaddr of that segment is added to that offset
!
! Impabs32/64
!	Reloc field contains any local offset (eg. the 3 in A+3)
!	Symbol index is used (via xlate to global index) to get abs address of symbol
!   Reloc field is replaced with 32/64 bits of that address plus the original value
!
! Imprel32
!	Only used for imported names, and only for CALL. Reloc field may be zeros
!	Reloc field will be changed to relative offset thunk table at end of code segment
!	Thunk table (indexed by local import index), is populated with JMPs to values
!	stored in address table which follows immediately
!	That address table needs to be populated with abs addresses of those imports
!	(Calls to LIB rather than DLL can have CALL offset replaced with direct offset to the
!	imported function, provided top 31 bits of address are zero.)

export tabledata() []ichar segmentnames =
	(code_seg,		"code"),
	(idata_seg,		"idata"),
	(zdata_seg,		"zdata"),
	(rodata_seg,	"rodata"),
	(impdata_seg,	$),
end

!Describe an MCX program loaded into memory

global record librec=
!The first section describes data residing in a file and loaded into these vars
!(code block is loaded directly into an actual executable block, with thunk/
!address table space added)

	ichar version

	int codesize			! bytes in code block, excluding thunk/addr tables
	int idatasize			! bytes in idata block
	int zdatasize			! bytes in zdata block (no data; created on fixup)

	int nrelocs				! size of reloctable
	int	ndlllibs			! size of imported dll names
	int	nlibs				! size of imported libnames
	int nimports			! size of imports/importlib tables
	int nexports			! size of exports/exportsegs/exportoffsets tables

	ref byte codeptr		! executable code block (includes thunk/addr table)
	ref byte idataptr		! initialised data block

	ref[]mcxreloc	reloctable		! table of reloc entries
	ref[]ichar		dllnames		! base names of imported dll files (no extension)
	ref[]ichar		libnames		! base names of imported mcx files (no extension)
	ref[]ichar		importnames		! names of imported symbols
	ref[]ichar		exports			! names of exported symbols
	ref[]byte		exportsegs		! segment where each is located
	ref[]u64		exportoffsets	! offsets within each segment

	u64 entryoffset					! offset within code block where execution will start
									! value of 0xFFFFFFFF (is u32 in file) means not set

!The next section is filled in after loading

	ref byte zdataptr				! zeroed data block
	int codexsize					! bytes in thunk/addr tables that follow code
	ref[]u64		exportaddr		! fully fixed-up addresses of exported symbols (not in file)
	ref[]int16		importxreftable	! map symbol index to global one

	ichar			filespec		!full path
	ichar			libname			!base name of library
	ref byte		entryaddr		!start address (left at nil when entryoffset not set)
	int				libno			!index of this entry in progtable
end

!Reloc item record
! For Locabs-codes, the field contains the offset of the local symbol within target segment
! For Imp-codes, the field contains zero bytes

global record mcxreloc =
	u32		offset			! Offset with .segment of the reloc item
	union
		u16		stindex			! For Imp-codes, index into global import tables
		byte	targetsegment	! For Loc-codes, target segment refered to
	end
	byte	segment			! Segment containing the reloc item
	byte	reloctype		! Reloc code (see enums); also sets size of reloc item
end

global const maxdlls =		20
global const maxlibs =		20
global const maxsymbols =	3000

!Global DLL tables

global [maxdlls]ichar		dllnametable
global [maxdlls]u64			dllinsttable
global int ndlllibs

!Global Prog table

global [maxlibs]ichar		libnametable
global [maxlibs]ref librec	libtable
global [maxlibs]byte		librelocated		!1 when relocated
global [maxlibs]byte		libinitdone			!1 when entry point called
global int nlibs

!Global import tables

global [maxsymbols]ichar	symbolnametable	! Name of symbol
global [maxsymbols]byte		symboldefined	! 1 when fully resolved with address
global [maxsymbols]ref void	symboladdress	! Abs address
global [maxsymbols]int16	symbollibindex	! Lib index where defined
global [maxsymbols]byte		symboldllindex	! DLL index of library where found
global int nsymbols

=== runmx.m 0 0 22/25 ===

!const crtmode=1
const crtmode=0

global function loadmx(ichar filename)ref librec plib=
!load mx/ml into mcu then scan for other imported libraries
	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=addlib(name)


	plib:=loadlibfile(filename,newlib)

!CPL "LOADMX", PLIB.NLIBS
!FOR I TO PLIB.NLIBS DO
!	CPL I,PLIB.LIBNAMES[I]
!OD

	loadimports(plib)
!CPL =NDLLLIBS
!CPL =NLIBS

	return plib
end

global proc loadmemmcu(ref librec lib)=
	int newlib
	ichar name:=lib.libname

	checknew(name,lib.filespec)

	newlib:=addlib(name)
	libtable[newlib]:=lib

	loadimports(lib)
end

global function loadmemmcb(ichar filename, ref byte p)ref librec plib=
!read from mcb block in memory
!'filename' is just an identifying string

	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=addlib(name)
	plib:=readlibfile(filename,p)
	plib.libno:=newlib
	libtable[newlib]:=plib	

	loadimports(plib)
	return plib
end

global proc fixuplib(ref librec lib)=
!do second fixup pass, which is done across global symbols, but then 
!all relocs are done for all libs which are not yet relocated

!	alloclibdata(lib)
!	donewlib(lib)					!update global tables

!global fixups
!	loadimports()

	loaddlls()				!global
	scansymbols()			!global
	dorelocations()			!all libs
end

global proc loadimports(ref librec plib)=
! load imported libs
! do first fixup pass which sets up tables adds imports/exports to global table
! This is done per libs and can be called on imported sub-libs

	ref librec qlib
	ichar name

	for i to plib.nlibs do
		dosublib(plib.libnames[i])
	od

	if needcrt(plib) then
		dosublib("crt")
	fi		

	alloclibdata(plib)
	dosymbols(plib)
end

proc dosublib(ichar name)=
	ref librec qlib
	int n:=findlib(name)

	if not n then									!not already loaded
		n:=addlib(name)
		println "Loading sublib", name
		qlib:=loadlibfile(addext(name,"ml"),n)		!get mcu
		loadimports(qlib)						!recursive call
	fi
end

proc loaddlls=
!load all dll instances
	u64 inst

	for i to ndlllibs when not dllinsttable[i] do
		inst:=os_getdllinst(dllnametable[i])
		if inst=0 then
			error("Can't find DLL: #", dllnametable[i])
		fi
		dllinsttable[i]:=inst
    od
end

function loadlibfile(ichar filename, int libno)ref librec plib=
!read mcb file into memory, process it into a new librec
	ref byte p

	p:=readmxfile(filename)
	if p=nil then
		error("Can't find #",filename)
	fi

	plib:=readlibfile(filename,p)
	plib.libno:=libno
	libtable[libno]:=plib	
end

function xxxloadlibmem(ichar filename, int libno)ref librec plib=
!read mcb file into memory, process it into a new librec
	ref byte p

	p:=readmxfile(filename)
	if p=nil then
		error("Can't find #",filename)
	fi

	plib:=readlibfile(filename,p)
	plib.libno:=libno
	libtable[libno]:=plib	
end

function readlibfile(ichar filespec, ref byte p)ref librec plib=
!p points to an MCB block; scan that into an MCU descriptor (librec)

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
!		lib.zdataptr:=pcm_allocz(lib.zdatasize)

	when idata_dir then
		lib.idatasize:=n:=readu32(p)
		lib.idataptr:=pcm_alloc(n)
		memcpy(lib.idataptr, p, n)	
		p+:=n

	when code_dir then
		lib.codesize:=n:=readu32(p)
		lib.codeptr:=p				!for now, point into file image
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

proc alloclibdata(ref librec lib)=
	int tablesize, n
	ref byte p

	lib.zdataptr:=pcm_allocz(lib.zdatasize)

	tablesize:=lib.nimports*16			!add in thunk table+address table
	n:=lib.codesize

	p:=os_allocexecmem(n+tablesize)		!MUST BE EXECUTABLE MEMORY
	if p=nil then
		error("Can't alloc code memory")
	fi
	memcpy(p, lib.codeptr, n)

	memset(p+n, 0, tablesize)
!	memset(p+n, 0xAA, tablesize)

	lib.codeptr:=p
	lib.codexsize:=tablesize

	lib.exportaddr:=pcm_alloc(u64.bytes*lib.nexports)
	lib.importxreftable:=pcm_alloc(i16.bytes*lib.nimports)

	if lib.entryoffset<>0xFFFF'FFFF then
		lib.entryaddr:=lib.codeptr+lib.entryoffset
	fi
end

function readmxfile(ichar filename)ref byte p=
!read in mx/ml file into an mcb block, add end_dir byte at the end just in case
!return pointer to mcb block

	p:=readfile(filename)
	return nil when p=nil
	(p+rfsize)^:=end_dir		!add eof-marker

	return p
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

proc error(ichar mess, param="")=
	if param^ then
		fprintln mess,param
	else
		println mess
	fi
	println "Aborting"
	stop 1
end

function findlib(ichar name)int n=
!find an existing library existing

	for i to nlibs do
		if eqstring(name,libnametable[i]) then return i fi
	od
	return 0
end

function addlib(ichar name)int n=
!add a new lib slot with given name
	if nlibs>=maxlibs then 
		error("Too many libs")
	fi

	libnametable[++nlibs]:=name
	return nlibs
end

proc adddll(ichar name)=
!	if crtmode and eqstring(name,"msvcrt") then
	if crtmode then
		return
	fi

	for i to ndlllibs do
		if eqstring(name,dllnametable[i]) then return fi
	od

	if ndlllibs>=maxdlls then 
		error("Too many DLLs")
	fi

	dllnametable[++ndlllibs]:=name
end

!function addsymbol(ichar name, u64 address, int dllindex, libindex)int=
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

proc dosymbols(ref librec lib)=
!Add any dll libs to global table (libs already done)
!Then deal with imported and exported symbols

	int ix, libx, dllx
	ref byte baseaddr

!CPL "DOSYMBOLS",=LIB.NDLLLIBS,=LIB.NLIBS

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
!			error("Dupl symbol:",lib.exports[i])
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

function finddllsymbol(ichar name, int &dllindex)ref void p=
!look up symbol in any of the DLLs
!return address, or void if not found
!dllindex is set to dll where it was found

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

proc scansymbols=
	int dllindex,undef:=0
	ref void p

CPL "RUNMX SCANSS"

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
		error("Symbols Undefined")
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
	ref u64 qaddr		!to import address table
	mcxreloc r

!do thunk tables first
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
!		CPL "------",i,index,symbolnametable[index],symboladdress[index]
		qaddr++^:=cast(symboladdress[index])

	od

!Now do the actual relocations
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

			index:=lib.importxreftable[r.stindex]			!global index
			(ref u64(p)^+:=cast(symboladdress[index],u64))

		when impabs32_rel then
			index:=lib.importxreftable[r.stindex]			!global index
			(ref u32(p)^+:=cast(symboladdress[index],u64))

		when imprel32_rel then
			if r.segment<>code_seg then error("imprel32?") fi
			index:=r.stindex								!local index
			q:=lib.codeptr+lib.codesize+(index-1)*8

			(ref u32(p)^ := q-(p+4))	!offset to thunk entry
		esac

	od

	librelocated[lib.libno]:=1

end

proc setspecialglobals(int cmdskip)=
!adjust cmdparams visible to application by setting $cmdskip flag

	for i to nsymbols when symbolnametable[i]^='$' do
		if eqstring(symbolnametable[i],"$cmdskip") then
			(ref byte(symboladdress[i])^:=cmdskip)
		elsif eqstring(symbolnametable[i],"$callcrt") then
CPL "FOUND $CALLCRT"
			(ref ref void(symboladdress[i])^:=callcrt)
		fi
	od
end

global proc runprogram(ref librec lib, int cmdskip=0)=
	ref proc fnptr
!	ref function(int,int)int fnptr
	int libno:=lib.libno

!CPL "RUNPROG"

	for i to nlibs when i<>libno and not libinitdone[i] do
		calllibinit(libtable[i])
	od

	if lib.entryaddr=nil then
		error("No entry point found")
	fi

	setspecialglobals(cmdskip)


	fnptr:=cast(lib.entryaddr)
!CPL "RUN PROG:", ref byte(fnptr)^:"h"

	fnptr()

!CPL "DONE RUN"
	libinitdone[libno]:=1

end

global proc calllibinit(ref librec lib)=
	ref proc fnptr
!	ref function(int,int)int fnptr
	int libno:=lib.libno

!CPL "CALLLIBINIT",LIB.LIBNAME

!CPL "RUNPROG"

	if lib.entryaddr then
		fnptr:=cast(lib.entryaddr)
!CPL "STARTING LIB",LIB.LIBNAME
		fnptr()
!CPL "DONE LIB"
	fi
	libinitdone[lib.libno]:=1
end

proc checknew(ichar name, filename)=
	if findlib(name) then
		error("Lib already exists:",filename)
	fi
end

function callcrt(u64 name, []u64 &args, int nargs)u64 retval=
	macro a=args[1]
	macro b=args[2]
	macro c=args[3]

!	REF CHAR P:=cast(&NAME)
!	PRINTF("CALLCRT: ")
!	WHILE P^ DO
!		PRINTF("%c",p++^)
!	od
!	FOR I TO NARGS DO
!		PRINTF(" %llX",args[i])
!	od
!!	PUTS("")
!	PRINTF(" NARGS=%lld\n",nargs)
!
!CPL "INSIDE CALLCRT"

	case name
	when 'peek' then
		os_peek()

	when 'puts' then
		retval:=puts(ichar(a))
	when 'exit' then
		`exit(a)
	when 'printf' then
		case nargs
		when 1 then retval:=printf(ichar(a))
		when 2 then retval:=printf(ichar(a),b)
		when 3 then retval:=printf(ichar(a),b,c)
!		when 4 then retval:=printf(ichar(a),b,c,args[4])
		else
			abortprogram("printf too many args")
		esac
	when 'sprintf' then
!CPL "SPRINTF",=NARGS,ICHAR(B)
		case nargs
		when 2 then retval:=sprintf(cast(a),ichar(b))
		when 3 then retval:=sprintf(cast(a),ichar(b),c)
		when 4 then retval:=sprintf(cast(a),ichar(b),c,args[4])
		else
			abortprogram("sprintf too many args")
		esac
	when 'fprintf' then
		case nargs
		when 2 then retval:=fprintf(cast(a),ichar(b))
		when 3 then retval:=fprintf(cast(a),ichar(b),c)
		when 4 then retval:=fprintf(cast(a),ichar(b),c,args[4])
		else
			abortprogram("fprintf too many args")
		esac
	when '__getma' then
		retval:=__getmainargs(cast(a),cast(b),cast(c),args[4],cast(args[5]))

	when 'strlen' then
		retval:=strlen(cast(a))

	when 'strcpy' then
		retval:=cast(strcpy(cast(a),cast(b)))

	when 'strcat' then
		retval:=cast(strcat(cast(a),cast(b)))

	when 'strcmp' then
		retval:=strcmp(cast(a),cast(b))

	when 'strncmp' then
		retval:=strncmp(cast(a),cast(b), c)

	when 'memcpy' then
		memcpy(cast(a), cast(b), cast(c))

	when 'memset' then
		memset(cast(a), cast(b), cast(c))

	when 'memcmp' then
		retval:=memcmp(cast(a), cast(b), cast(c))

	when 'malloc' then
		retval:=cast(malloc(cast(a)))

	when 'free' then
		free(cast(a))

	when 'fopen' then
		retval:=cast(fopen(cast(a),cast(b)))

	when 'fclose' then
		retval:=fclose(cast(a))

	when 'ftell' then
		retval:=ftell(cast(a))

	when 'fseek' then
		retval:=fseek(cast(a),cast(b), cast(c))

	when 'fread' then
		retval:=fread(cast(a),cast(b), cast(c), cast(args[4]))

	when 'fwrite' then
		retval:=fwrite(cast(a),cast(b), cast(c), cast(args[4]))

	when 'strncpy' then
		retval:=strncpy(cast(a),cast(b), cast(c))

	when 'clock' then
		retval:=clock()

	when 'tolower' then
		retval:=i32(tolower(a))

	when 'toupper' then
		retval:=i32(toupper(a))

	when 'toupper' then
		retval:=i32(toupper(a))

	when 'strchr' then
		retval:=cast(strchr(cast(a), b))

	when 'getdll' then
		retval:=cast(os_getdllinst(cast(a)))

	when 'getproc' then
		retval:=cast(os_getdllprocaddr(a, cast(b)))
	when 'remove' then
		retval:=remove(cast(a))

	when 'strtod' then
		retval:=u64@(strtod(cast(a), cast(b)))

	when 'getsyst' then
		os_getsystime(cast(a))

	when 'atan' then
		retval:=u64@(atan(real@(a)))

	when 'initwin' then
		os_initwindows()

!	when 'peek' then
!		os_peek()

	else
		println name:"d"
		abortprogram("msvcrt function not ready")
	esac

	return retval
end

function needcrt(ref librec lib)int =
	return 0 when not crtmode
	return 0 when eqstring(lib.libname,"crt")

	return 1
end


=== runshow.m 0 0 23/25 ===
int logdest=2

const logfile="rx.log"

ref void logdev		!dest for diagnostics and output of tables

strbuffer destv
ref strbuffer dest=&destv

global proc initlogfile=
	case logdest
	when 2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	when 0,1 then
		logdev:=nil
	esac

end

global proc closelogfile=
	[512]char str

	if logdest=2 then
		fclose(logdev)

		print @&.str,f"\m\ed.bat",logfile

		os_execwait(&.str,1,nil)
	fi
end

global proc showlibs=
!show details of all libs, plus the global data
	showglobals(logdev)

	for i to nlibs do
!	for i to nlibs when libdefined[i] do
		showlib(libtable[i], logdev)
	od
end

global proc showlib(ref librec lib, filehandle logdev)=
	[300]char str
	u64 sig
	int dir,n
	ref byte q
	ref[]ichar names

	gs_init(dest)

	showstrln("-------------------------")
	showstr("LIBFILE: ")
	showstr(lib.libname)
	showstr(" ")
	showstrln(lib.filespec)

	print @str,"Version:",lib.version
	showstrln(str)

	showstrln("")

	fprint @str,"Zdata size: # #", lib.zdatasize, lib.zdataptr
	showstrln(str)

	fprint @str,"Idata size: # #", lib.idatasize, lib.idataptr
	showstrln(str)

	showsectiondata(lib.idataptr, lib.idatasize)

	fprint @str,"Code size: # # Extra:#", lib.codesize, lib.codeptr, lib.codexsize
	showstrln(str)
	showsectioncode(lib.codeptr, lib.codesize,lib.codexsize)
	showrelocs(lib)

	fprint @str,"DLL Libs #", n:=lib.ndlllibs
	showstrln(str)
	shownames(lib.dllnames,n)
	showstrln("")

	fprint @str,"Libs #", n:=lib.nlibs
	showstrln(str)
	shownames(lib.libnames,n)
	showstrln("")

	fprint @str,"Imports #", n:=lib.nimports
	showstrln(str)
	names:=lib.importnames
	for i to n do
		fprint @str,"   #: #", i, names[i]:"20jl"
		showstrln(str)
	od
	showstrln("")
!	shownames(lib.imports,n)

	fprint @str,"Exports #", n:=lib.nexports
	showstrln(str)
	names:=lib.exports
	showstrln("     Name                 Seg      Offset")
	showstrln("--------------------------------------------")

	for i to n do
		fprint @str,"#: # # #",
			i:"3", names[i]:"20jl",
			segmentnames[lib.exportsegs[i]]:"8jl",
			lib.exportoffsets[i]:"8zh"
		showstrln(str)
	od
	showstrln("")


	fprint @str,"Entry point offset:  #",lib.entryoffset
	showstrln(str)
	fprint @str,"Entry point address: #",lib.entryaddr
	showstrln(str)
	showstrln("")

FINISH::

	gs_println(dest,logdev)
end

proc showstr(ichar str)=
	gs_str(dest, str)
end

proc showstrln(ichar str)=
	gs_strln(dest, str)
end

proc showstrint(int a)=
	gs_strint(dest, a)
end

proc shownames(ref[]ichar names, int n)=
	[300]char str
	for i to n do
		fprint @str,"   #: #", i, names[i]
		showstrln(str)
	od
end

proc showrelocs(ref librec lib)=
	[300]char str
	mcxreloc r
	int n:=lib.nrelocs, m
	u64 targetoffset
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	fprint @str,"Relocs #", n:=lib.nrelocs
	showstrln(str)

	showstrln("     Type       Seg      Offset    Symbol/Target+Offset")
	showstrln("---------------------------------------------------------")

	for i to n do
		r:=lib.reloctable[i]
		fprint @str,"#: # # ##",
			i:"3", mcxrelocnames[r.reloctype]:"10jl",
			segmentnames[r.segment]:"8jl",
			r.offset:"8zh",,"  "

		m:=strlen(str)
		case r.reloctype
		when locabs32_rel, locabs64_rel then
			case r.segment
			when code_seg then baseptr64:=cast(lib.codeptr+r.offset)
			when idata_seg then baseptr64:=cast(lib.idataptr+r.offset)
			esac
			if r.reloctype=locabs32_rel then
				targetoffset:=baseptr32^
			else
				targetoffset:=baseptr64^
			fi

!CPL =BASEPTR32, =BASEPTR64,=BASEPTR32^, =BASEPTR64^


			print @&.str+m, segmentnames[r.targetsegment]:"6jlt:",,targetoffset:"8zh"
		else
!CPL "RELOC/OTHER",MCXRELOCNAMES[R.RELOCTYPE],R.STINDEX
			print @&.str+m, lib.importnames[r.stindex]
		esac
		showstrln(str)
	od
	showstrln("")
end

proc showsectiondata(ref byte p, int length)=
	int i,k,bb
	[128]char str,str2

	showstr("proc Section ")
	showstr("Idata:")
	showstr(" Size:")
	showstrint( length)
	gs_line(dest)
	gs_line(dest)

	k:=0

	str[1]:=0

	ref byte baseaddr:=nil

	print @&.str2,baseaddr:"Z8H",,": "

	showstr(&.str2)

	for i:=1 to length do
		bb:=p++^
		print @&.str2,bb:"z2H",," "
		showstr(&.str2)

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
					showstr("   ")
					strcat(&.str," ")
				od
			fi
			showstr("	[")
			showstr(&.str)
			showstrln("]")
			k:=0
			str[1]:=0
			baseaddr+:=16
			print @&.str2,baseaddr:"z8h",,": "
			showstr(&.str2)
		fi
	od
	if k=0 then
		gs_line(dest)
	fi

	gs_line(dest)
	if k then gs_line(dest) fi
end

proc showsectioncode(ref byte p, int length, extra)=
	ref byte codeptr,codeend, codeendx,codestart
	int offset
	ichar s
	[16]char str

	showstrln( "proc Section Code")

	codestart:=codeptr:=p
	codeend:=codeptr+length
	codeendx:=codeend+extra

!	ref byte baseaddr:=cast(imagebase+p.virtoffset)
	ref byte baseaddr:=nil

	while codeptr<codeendx do
		if codeptr=codeend then
			showstrln("")
		fi
		offset:=codeptr-codestart
!S:=NIL
		s:=decodeinstr(codeptr,baseaddr+offset)
		exit when s=nil

		print @&.str,offset:"4",," "
		showstr(&.str)

		showstrln(s)
	od

	gs_line(dest)
end

global proc showglobals(filehandle logdev)=
	[300]char str
	[300]char name

	gs_init(dest)
	showstrln("Global Tables\n")

	print @str, "DLLs:",ndlllibs
	showstrln(str)

	for i to ndlllibs do
		print @str,i,,":",dllnametable[i]:"16jl", dllinsttable[i]:"h"
		showstrln(str)
	od
	showstrln("")

	print @str, "LIBs:",nlibs
	showstrln(str)

	for i to nlibs do
		print @str,i,,":",libnametable[i]:"20jl", (librelocated[i]|"Relocated"|"-")
		showstrln(str)
	od
	showstrln("")

	print @str, "Global Symbols:",nsymbols
	showstrln(str)

	showstrln("     Name              Def Address       Lib        Dll")
	showstrln("-----------------------------------------------------------")

	for i to nsymbols do
		strcpy(name,symbolnametable[i])
		if strlen(name)>17 then
			strcat(name,"\n                      ")
		fi
		fprint @str,"#: # # #  # #",
			i:"3",
!			symbolnametable[i]:"17jl",
			&.name:"17jl",
			(symboldefined[i]|"Y"|"-"):"3JL",
			symboladdress[i]:"Z12H",
			(symbollibindex[i]|libnametable[symbollibindex[i]]|"-"):"10jl",
			(symboldllindex[i]|dllnametable[symboldllindex[i]]|"-"):"10jl"
!symboldllindex[i]
		showstrln(str)
	od
	showstrln("")

	gs_println(dest,logdev)
end
=== rts.pcl 0 1 24/25 ===
!
Procrts rts.$power_i64: i64
    param          rts.$power_i64.a i64 
    param          rts.$power_i64.n i64 
    procentry                 
!-------------------------------------------------
    startmult                 
    push           rts.$power_i64.n i64 
    push           0          i64 
    jumpge         #8         i64 
    push           0          i64 
    resetmult                 
    jump           #9         
#8: 
    push           rts.$power_i64.n i64 
    push           0          i64 
    jumpne         #10        i64 
    push           1          i64 
    resetmult                 
    jump           #9         
#10: 
    push           rts.$power_i64.n i64 
    push           1          i64 
    jumpne         #11        i64 
    push           rts.$power_i64.a i64 
    resetmult                 
    jump           #9         
#11: 
    push           rts.$power_i64.n i64 
    push           0          i64 
    dotindex                  i64 
    jumptrue       #12        i64 
    setargs                   2 0
    push           rts.$power_i64.n i64 
    push           2          i64 
    idiv                      i64 
    push           rts.$power_i64.a i64 
    sqr                       i64 
    callfn         &rts.$power_i64 i64 
    resetmult                 
    jump           #9         
#12: 
    setargs                   2 0
    push           rts.$power_i64.n i64 
    push           1          i64 
    sub                       i64 
    push           2          i64 
    idiv                      i64 
    push           rts.$power_i64.a i64 
    sqr                       i64 
    callfn         &rts.$power_i64 i64 
    push           rts.$power_i64.a i64 
    mul                       i64 
    endmult                   
#9: 
    setret                    i64 
    jump           #13        
!-------------------------------------------------
#13: 
    retfn                     i64 
End

!
    endprogram                
=== pchelp.txt 0 1 25/25 ===
PC.EXE PCL Processor

Usage:

    pc prog        Compile prog.pcl to prog.exe (.pcl optional)

Options:

    -exe           (Default) generate native x64 and write EXE file
    -dll           Generate native x64 code and write DLL file
    -asm           Generate native x64 code and write ASM source file
    -pcl           Generate PCL source code (use -out to set dest file)

    -clang         Generate linear C source code (if backend present)

    -opt           Apply optimiser to x64 backend
    -norts         Don't include runtime PCL code

    -out:file      Set name of output file (default is based on input file)
=== END ===
1 pc.m
2 pccli.m
3 pc_decls.m
4 pc_disasm.m
5 pc_genmcl.m
6 pc_genss.m
7 pc_lex.m
8 pc_libmcl.m
9 pc_libpcl.m
10 pc_objdecls.m
11 pc_optim.m
12 pc_parse.m
13 pc_runmx.m
14 pc_stackmcl.m
15 pc_tables.m
16 pc_writeexe.m
17 pc_writemx.m
18 pc_writess.m
19 pci_mcl.m
20 pc_win64.m
21 rundecls.m
22 runmx.m
23 runshow.m
24 rts.pcl
25 pchelp.txt
