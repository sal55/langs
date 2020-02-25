mafile 38
  1 qq.m                3729     1664   0
  2 msysnew.m          46919     5417   0
  3 clibnew.m           3397    52360   0
  4 mlib.m             26695    55778   0
  5 oswindows.m        12536    82499   0
  6 pci.m              19759    95055   0
  7 pc_types.m          2773   114839   0
  8 pc_decls.m         19679   117637   0
  9 pq_common.m        18142   137342   0
 10 pc_support.m       13427   155512   0
 11 pc_misc.m           1671   168964   0
 12 pc_pcfns.m         45642   170661   0
 13 pc_objlib.m         7819   216330   0
 14 pc_bignum.m         2754   224176   0
 15 mbignum.m          30191   226955   0
 16 pc_print.m         46284   257172   0
 17 pc_jhandlers.m     52793   303486   0
 18 pc_oslayer.m        5431   356307   0
 19 oswindll.m          2115   361764   0
 20 pc_host.m          30470   363904   0
 21 pc_dxfns.m          7368   394400   0
 22 pc_khandlers.m     59170   401798   0
 23 pc_assem.m         99912   460994   0
 24 var_decls.m        10045   560933   0
 25 var_types.m         2791   571005   0
 26 qc_tables.m        28264   573823   0
 27 qci.m              20913   602108   0
 28 qc_support.m        9259   623049   0
 29 qc_lex.m           29129   632332   0
 30 qc_parse.m         81923   661487   0
 31 qc_lib.m           36052   743434   0
 32 qc_name.m          26666   779511   0
 33 qc_pclgen.m        71110   806204   0
 34 qc_pcllib.m         9036   877341   0
 35 q_libs_dummy.m        92   886407   0
 36 ccm_fn.             2968   886522   1
 37 ccm_host.           1215   889515   1
 38 ccasm_fn.           4518   890755   1
=== qq.m 1/38 ===
mapmodule q_libs => q_libs_dummy

import msys
import mlib

import pci
import var_decls
import pc_decls
import pc_support
import qci

tabledata() []ichar optionnames =
	(fn_sw,			"fn"),
	(asm_sw,		"asm"),
	(debug_sw,		"debug"),
	(fdebug_sw,		"fdebug"),
	(verbose_sw,	"v"),
	(help_sw,		"help"),
	(ext_sw,		"ext"),
	(qa_sw,			"qa"),
	(compile_sw,	"c"),
end

ichar inputfile
!var int fwriteqa
int fcompile

proc start=
	int stopcode
	int filetype
	ichar outputfile

!CPL "HELLO MV"

	getinputoptions(filetype)

!CPL =DOINTLIBS

	if filetype=q_file then
		outputfile:=pcm_copyheapstring(changeext(inputfile,"pc"))
		qcompiler_prod(inputfile,outputfile,dointlibs,0)
		inputfile:=outputfile
		if fcompile then
			CPL "STOPPING AFTER COMPILING"
			STOP
		FI

		filetype:=pc_file

	fi

	if fverbose then showcaption() fi

	stopcode:=runpcl(inputfile,filetype)

	if stopcode<0 then
		qci.loaderror(errorcodenames[stopcode],inputfile)
	fi

	stop stopcode
end

proc getinputoptions(int &filetype)=
	int paramno,pmtype
	ichar ext,name,value,newfile

	paramno:=2

!	while pmtype:=nextcmdparam(paramno,name,value,"pc") do
	while pmtype:=nextcmdparam(paramno,name,value,"") do

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
				qci.loaderror("Only one input file allowed")
			fi
			inputfile:=pcm_copyheapstring(name)
			exit					!ignore rest which are used by user program
		else
			println "Bad command param"
			stop 9
		esac

	od

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "	",,sysparams[1],"filename[.pc]"
		println "	",,sysparams[1],"filename.q"
		println "	",,sysparams[1],"-help"
		stop 1
	fi

	ext:=extractext(inputfile)

	if eqstring(ext,"") then
		if checkfile(newfile:=changeext(inputfile,"q")) then
			inputfile:=pcm_copyheapstring(changeext(inputfile,"q"))
			ext:="q"
		else
			ext:="pc"
			inputfile:=pcm_copyheapstring(newfile)
		fi
	fi

	if eqstring(ext,"pc") then
		filetype:=pc_file
	elsif eqstring(ext,"q") then
		filetype:=q_file
	else						!also also source files
		filetype:=q_file
	fi

	getsyscmdline(paramno-1)	!copy to cmdparams starting from this inputfile

end

proc do_option(int sw, ichar value)=
	int length

	case sw
	when fn_sw then
		dispatch_type:=fn_dispatch

	when asm_sw then
		dispatch_type:=asm_dispatch

	when debug_sw then
!CPL "SET DEBUG DISPATCH"
		dispatch_type:=deb1_dispatch

	when fdebug_sw then
		dispatch_type:=deb2_dispatch

	when help_sw then
		showhelp()

	when verbose_sw then
		fverbose:=2

	when ext_sw then
		dointlibs:=0

	when qa_sw then
		fwriteqa:=1

	when compile_sw then
		fcompile:=1

	esac
end

proc getsyscmdline(int n)=
!get system args starting from sysparams[n] and store into local cmdparams of a task

	setcmdparam(0,sysparams[1])

	for i:=n to nsysparams do
		setcmdparam(i-n+1,sysparams[i])
	od
end

proc showcaption=
	println "PCL Interpreter",$date,"(",,dispatchnames[dispatch_type],$targetcode,,")"
end

proc showhelp=
	println "General usage:"
	println "	",,sysparams[1],"[options] filename[.pc]"
	println "	",,sysparams[1],"[options] filename.q"
	println
	println "	Options:"
	println "		-fn           Function table dispatcher (default)"
	println "		-asm          Fast ASM dispatcher"
	println "		-debug        Tracing dispatcher"
	println "		-fdebug       Tracing dispatcher starts on $setdebug(1)"
	stop 0
end
=== msysnew.m 2/38 ===
import clib
import mlib

global record procinforec=
	word16		fnindex
	byte		rettype
	byte		nparams
	[12]byte	paramlist
end

!for print/read routines
!------------------------------------------
record fmtrec=	! (default)
	byte	minwidth	! n (0)   min field width (0 if not used or don't care)
	i8		precision	! .n (0)   number of decimals/significant figures/max width
	byte	base		! B,H or Xn (10)  2 to 16

	char	quotechar	! Qc (0)   0 or '"' or c
	char	padchar		! Pc, Z (' ')
	char	realfmt		! E,F,G ('f') 'e' or 'f' or 'g'

	char	plus		! (0)   0 or '+'
	char	sepchar		! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
	char	lettercase	! A,a ('A') 'A' or 'a'
	char	justify		! JL, JR, JC ('R') 'L' or 'R' or 'C'?
	char	suffix		! Tc (0)   0 or 'B' or 'H' or c
	char	usigned		! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
	char	charmode	! C,D (0)  0 or 'C' or 'D'	o/p int as int or single char or double/multi-char
	char	heapmode	! M (0)  'M' for str-functions, return ptr tp heap string
	char	param		! Use int value for <fmtparam>
	byte	spare
end

int fmtparam			!as set with :'V'

enum (std_io,file_io,str_io)

const comma = ','

global int needgap			= 0
int outdev			= std_io
filehandle outchan	= nil
ref char fmtstr 	= nil

const maxiostack=10
[maxiostack]filehandle	outchan_stack
[maxiostack]int			outdev_stack
[maxiostack]ref char	fmtstr_stack
[maxiostack]byte		needgap_stack

[maxiostack]ref char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
const rd_buffersize = 16384	!total capacity of line buffer

ref char rd_buffer		! point to start of read buffer
int rd_length			! length of this line (as read by readln)
ref char rd_pos			! current position it's up to (next read starts here)
ref char rd_lastpos		! set by sread() just before reading used for reread()
int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

!------------------------------------------

const maxparam=128
global int nsysparams
global [maxparam]ichar sysparams

const maxcallback=8
[0..maxcallback,8]word64 callbackstack
int ncallbacks=0

word64 mask63	= 0x7FFF'FFFF'FFFF'FFFF
real offset64	= 9223372036854775808.0		! 2**63 as r64
real offset32	= 9223372036854775808.0		! 2**63 as r32

global proc m$init=
int32 nargs
int nargs64
ref[]ichar args
ref[]ichar env
static [128]byte startupinfo			! 68 or 104 bytes
int res
ichar s

res:=__getmainargs(&nargs,cast(&args),cast(&env),0,cast(&startupinfo))

nsysparams:=nargs

if nsysparams>maxparam then
	printf("Too many params\n")
	stop 50
fi

nargs64:=nargs			!bug when using 32-bit limit when compild with mm
for i:=1 to nargs64 do
	sysparams[i]:=args^[i]
od
end

global proc m$stop(int n)=

assem
	mov d10,[n]
	mov d0,`exit
	call m$callff_4
end

end

global threadedproc m$callff_4=
!0..4 params have been loaded to R10..13
!The foreign function address is in D0
!set up the stack, keeping it aligned, and call the function, adjusting the
!stack before returning.
!For functions rather than procs, the return value will be in A0/D0/XMM0

assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:
	sub Dstack,40			!add an extra 8 bytes to align
	call D0
	add Dstack,40			!unstack the dummy 4 params, plus 8 more bytes
	ret

aligned:
	sub Dstack,32
	call D0
	add Dstack,32
	ret
end

end

global threadedproc m$callff_5=
!one param already pushed. 
!
!There might be another way to do this:
!leave retaddr in place, move P5 this side of retaddr, but leave original P5
!there, and use retn 8 to skip it

assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr
	pop D2					!P5
	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack
	push D2					!P5
	sub Dstack,32
	call D0
	add Dstack,48			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr
	pop D2					!P5
	push D1					!push ret addr back
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,40			!pop all
	ret
end

end

global threadedproc m$callff_6=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,56			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6

	push D1					!push ret addr back

	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,48			!pop all
	ret
end

end

global threadedproc m$callff_7=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,64			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7

	push D1					!push ret addr back

	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,56			!pop all
	ret
end

end

global threadedproc m$callff_8=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,72			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8

	push D1					!push ret addr back

	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,64			!pop all
	ret
end

end

global threadedproc m$callff_9=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,80			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr
	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9

	push D1					!push ret addr back
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,72			!pop all
	ret
end

end

global threadedproc m$callff_10=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,88			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10

	push D1					!push ret addr back

	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,80			!pop all
	ret
end

end

global threadedproc m$callff_11=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,96			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11

	push D1					!push ret addr back

	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,88			!pop all
	ret
end

end

global threadedproc m$callff_12=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,104			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12

	push D1					!push ret addr back

	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,96			!pop all
	ret
end

end

global threadedproc m$callff_14=
static word64 p13,p14
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12
	pop u64 [p13]			!P12
	pop u64 [p14]			!P14

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push u64 [p14]		!P14
	push u64 [p13]		!P13
	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,120			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12
	pop u64 [p13]			!P12
	pop u64 [p14]			!P14

	push D1					!push ret addr back

	push u64 [p14]		!P14
	push u64 [p13]		!P13
	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,112			!pop all
	ret
end

end

global proc m$pushcallback=
!save registers rbx, rsi,rdi, r12..r15 to small stack
!Note must take care not to overwrite any of those while saving

!if ncallbacks=maxcallback then
!	println "Callback overflow"
!	stop 1
!fi

assem
	inc word32 [ncallbacks]
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]

	mov [D4],rbx
	mov [D4+8],rsi
	mov [D4+16],rdi
	mov [D4+24],r12
	mov [D4+32],r13
	mov [D4+40],r14
	mov [D4+48],r15
end
end

global proc m$popcallback=
!restore registers rbx, rsi,rdi, r12..r15 from small stack
assem
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]
	mov rbx,[D4]
	mov rsi,[D4+8]
	mov rdi,[D4+16]
	mov r12,[D4+24]
	mov r13,[D4+32]
	mov r14,[D4+40]
	mov r15,[D4+48]
	dec word32 [ncallbacks]
end
end

global function m$lenstr_stringz(ref char s)int=
	strlen(s)
end

global function m$getdotindex(word64 a,int i)int=
!return (a iand (1dw<<i))>>i
return (a iand (1<<i))>>i
end

global proc m$setdotindex(ref word64 a, int i,x)=
ref word32 a32

!see comments on setdotslice
if i>=32 then
	a^:=(a^ iand inot (1<<i)) ior (word64(x)<<i)

else
	a32:=cast(a)
	a32^:=(a32^ iand inot (1<<i)) ior (word(x)<<i)
fi
end

global function m$getdotslice(word64 a,int i,j)int=
if i>=j then
	return (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))
else
	return (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
fi
end

global proc m$setdotslice(ref word64 a, int i,j,word64 x)=
!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
int w
word64 mask64
word mask
ref word32 a32

if i>j then println "SETDOTSLICE?"; stop 52 fi

!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing
!to bytes beyond the 32-bit value
!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too

if j>=32 then
	mask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
	a^:=(a^ iand inot mask64) ior x<<i
else
	a32:=cast(a)
	mask:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
	a32^:=(a32^ iand inot mask) ior x<<i
fi

end

function m$get_nprocs:int=
	assem
		mov D0,[$nprocs]
	end
end

function m$get_procname(int n)ichar=
	assem
		lea D0,[$procnames]
		mov D1,[n]
		mov D0,[D0+D1*8-8]
!		mov D0,[sss]
	end
end

function m$get_procaddr(int n)ref proc=
	assem
		lea D0,[$procaddrs]
		mov D1,[n]
		mov D0,[D0+D1*8-8]
	end
end

global function m$get_nexports:int=
	assem
		mov D0,[$nexports]
	end
end

global function m$get_procexport(int n)ref void=
	assem
		lea D0,[$procexports]
		mov D1,[n]
		shl D1,1
		lea D0,[D0+D1*8-16]
	end
end

proc pushio=
	if niostack>=maxiostack then
		printf("Too many io levels\n")
		stop 53
	fi
	++niostack
	outchan_stack[niostack]	:= outchan
	outdev_stack[niostack]	:= outdev
	fmtstr_stack[niostack]	:= fmtstr
	needgap_stack[niostack]	:= needgap
	needgap:=0
	fmtstr:=nil
	outchan:=nil
end

global proc m$print_startfile(ref void dev)=
	pushio()
	outchan:=cast(dev)
	if dev then
		outdev:=file_io
	else
		outdev:=std_io
	fi
end

global proc m$print_startstr(ref char s)=
	ref ref char p
	pushio()

	ptr_stack[niostack]:=s
	p:=&ptr_stack[niostack]

	outchan:=cast(p)
	outdev:=str_io
end

global proc m$print_startptr(ref ref char p)=
	pushio()

	outchan:=cast(p)
	outdev:=str_io
end

global proc m$print_startcon=
	pushio()
	outdev:=std_io
end

global proc m$print_setfmt(ref char format)=
	fmtstr:=format
end

global proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=0 then return fi
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]
	--niostack
end

global proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
	array[20]char s

	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

!global proc m$print_bool(int a,ichar fmtstyle=nil)=
!	[20]char s
!	nextfmtchars()
!	printstr((a|"T"|"F"))
!	needgap:=1
!end

global proc m$print_i64(int64 a,ichar fmtstyle=nil)=
	array[40]char s
	fmtrec fmt
	int n

!CPL "PRINTI64",=FMTSTYLE
!

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
!CPL "SET FMTPARAM TO",A
			needgap:=0
		else
			tostr_i64(a,&fmt)
		fi
	fi
	needgap:=1
end

global proc m$print_u64(word64 a,ichar fmtstyle=nil)=
	array[40]char s
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

global proc m$print_i128(int128 a,ichar fmtstyle=nil)=
	array[40]char s
	fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	if a>=0 then
		tostr_u128(a,&fmt,0)
	else
		tostr_u128(-a,&fmt,1)
	fi

	needgap:=1
end

global proc m$print_u128(word128 a,ichar fmtstyle=nil)=
	array[40]char s
	fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	tostr_u128(a,&fmt,0)
	needgap:=1
end

global proc m$print_r64(real x,ichar fmtstyle=nil)=
	array[360]char s
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

global proc m$print_r32(real32 x,ichar fmtstyle=nil)=
	m$print_r64(x,fmtstyle)
end

global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
	array[40]char s
	fmtrec fmt
	int n

	nextfmtchars()

	s[1]:=a
	s[2]:=0
	printstr(&.s)
	needgap:=1
end

global proc m$print_str(ichar s, fmtstyle=nil)=
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

!global proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
!	nextfmtchars()
!	fmtrec fmt
!	if fmtstyle=nil then
!		printstr_n(cast(s.sliceptr),s.len)
!	else
!		abortprogram("FORMATED PRINT SLICE NOT READY")
!!		strtofmt(fmtstyle,-1,&fmt)
!!		tostr_str(s,&fmt)
!	fi
!	needgap:=1
!end

!global proc m$print_flexstr(object s, ichar fmtstyle=nil)=
!	nextfmtchars()
!	fmtrec fmt
!
!	if fmtstyle=nil then
!		if s^.length then
!			printstr_n(s^.strptr,s^.length)
!		fi
!	else
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(str_stringz(s),&fmt)
!	fi
!	needgap:=1
!end

global proc m$print_newline=
	needgap:=0
	nextfmtchars(1)
	printstr("\w")
end

global proc m$print_nogap=
	needgap:=0
end

global proc printstr(ichar s)=
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
	ref ref char p

	case n
	when -1 then n:=strlen(s)
	when 0 then return
	esac

	case outdev
	when str_io then
		p:=cast(outchan)
		memcpy(p^,s,n)
		p^+:=n
		p^^:=0
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	when std_io then
		printf("%.*s",n,s)
	esac
end

global proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
!	emitc "printf(""%.*s"",(i32)length,s);"
	if f=nil then
		printf("%.*s",length,s)
	else
		fprintf(f,"%.*s",length,s)
	fi
fi
end

proc printchar(int ch)=
	ref ref char p
	case outdev
	when std_io then
		printf("%c",ch)
	when file_io then
		fprintf(outchan,"%c",ch)
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

	if not fmtstr then			!format not in use
		if needgap then
			printchar(' ')
!		printstr_n(" ",1)
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

global proc strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n	Width
!.n	Max width/precision
!A	Convert to upper when
!a	Convert to lower when
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Show int as multi-bit (unicode) character
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
!M	HEAPMODE???
!O	Octal
!Pc	Use padding char c
!Q	Add double quotes around string (and deal with embedded quotes)
!'	Add single quotes around string (and deal with embedded quotes)
!Sc	Use separator char c between every 3 or 4 digits
!Tc	Use terminator char c (typically B or H)
!U	Show ints as unsigned
!V	For ints, don't display: store value as parameter for subsequent '*'
!W	Unsigned
!Xn	Use base n (n is hex 0 to F)
!Z	Use "0" padding
!+	Always have + or - in front of integers
!~	Quote char is ~
!*	Same as n but uses parameter set with :'V' on previous int

	char c
	byte wset
	int n
	array[0:100]char str

	fmt^:=defaultfmt

	if s=nil then return fi

	if slen=-1 then slen:=strlen(s) fi

	memcpy(&.str,s,slen)		!convert s/slen to zero-terminated string
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
! when '0','1','2','3','4','5','6','7','8','9' then
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
!			if wset then
!CPL "FMT/* WSET",FMTPARAM
!				fmt.minwidth:=fmtparam
!			else
!CPL "FMT/*",FMTPARAM
!				fmt.precision:=fmtparam
!			fi
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
	array[0:20]char str
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

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =		!EXPANDSTR
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
	if w=0 or w<=n then		! allow str to be longer than minwidth
		strncpy(t,s,n)
		(t+n)^:=0
		return n
	fi

	if fmt^.justify='L' then	! left-justify
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

	else				! centre-justify?

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

!function xdivrem(word64 a,b)word64,word64=
!	assem
!		xor rdx,rdx
!		mov rax,[a]
!		div u64 [b]
!		mov D1,rdx
!	end
!end

function xdivrem(word64 a,b, &remainder)word64=
	word64 q,r
	assem
		xor rdx,rdx
		mov rax,[a]
		div u64 [b]
		mov [q],rax	
		mov [r],rdx	
	end
	remainder:=r
	return q
end

function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	array[0:onesixty]char t
	u64 dd
	int i,j,k,g
	int cc
	int dummy
	ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		aa:=xdivrem(aa,base,dd)
		t[++i]:=digits[dd]

!		t[++i]:=digits[aa rem base]
!		aa:=aa/base

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

function u128tostr(u128 aa,ref char s,word base,int sep)int =
!convert 128-bit int a to string in s^
!base is number base, usually 10 but can be 2 to 16
	array[0:160]char t
	u64 dd
	int i,j,k,g
	int dummy
	ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		aa:=xdivrem128(aa,base,dd)
		t[++i]:=digits[dd]

!		t[++i]:=digits[aa rem base]
!		aa:=aa/base

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

function xdivrem128(word128 a, word64 b, &remainder)word128=
	word128 d,e,r
	word rlow

	d:=a/b
	r:=a-d*b

	assem
		mov d0,[r]
		mov [rlow],d0
	end
	remainder:=rlow
	return d
end

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)
!returns length of s
	array[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
	static u64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt^.usigned then
		usigned:=1
	fi

	if aa=mindint and not usigned then		! minint

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
	if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	array[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	array[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u128tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
	array[0:onesixty]char t
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
	array[256]char str
	int w,nheap		! whether any heap storage is used  bytes allocated

	nheap:=0

	if fmt^.quotechar or fmt^.lettercase then		! need local copy
		if n<256 then
			u:=&.str
		else
			nheap:=n+3					! allow for quotes+terminator
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
		when 'a' then	! need lower when
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
	array[360]char str
	int n

	case fmt^.charmode
	when 0 then
		n:=i64tostrfmt(a,&.str,fmt)
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	else						!assume 'C'
		printchar(a)			!no other formatting allowed
		return
	esac

	printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
	array[360]char str
	int n

	case fmt^.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		n:=u64tostrfmt(a,&.str,fmt)
	esac

	printstr_n(&.str,n)
end

proc tostr_u128(word128 a, ref fmtrec fmt,int neg)=
	array[360]char str
	int n

	case fmt^.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		if neg then
			str[1]:='-'
			n:=u128tostrfmt(a,&str[2],fmt)+1
		else
			n:=u128tostrfmt(a,&.str,fmt)
		fi
	esac

	printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
	array[360]char str,str2
	array[0:10]char cfmt
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

!(TRY TAKING N FROM RESULT OF SPRINTF ABOVE)
	n:=strlen(&.str)		! current length

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

	if fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase or fmt.precision then
		if fmt^.quotechar then
			newlen+:=2
		fi
		if fmt^.minwidth>newlen then
			newlen:=fmt^.minwidth
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

global function getfmt(ichar fmtstyle)ref fmtrec=
	static fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

global function strint(int64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global proc getstrint(int64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

global function strword(word64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global function strreal(real a, ichar fmtstyle=nil)ichar=
	static [320]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
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

global proc m$read_conline=
	initreadbuffer()

	readlinen(nil,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_fileline(filehandle f)=
	initreadbuffer()
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
!abortprogram("No readln")
	end unless


	s:=rd_pos

!scan string, eliminating leading white space
	while s^=' ' or s^=9 do
		++s
	od

	itemstr:=s				!assume starts here
	rd_lastpos:=rd_pos:=s

	if s^=0 then			! No more chars left to read return null string
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0			! Allow possible enclosing single or double quotes
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
		when ' ', 9, comma, '=' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar::
			if c=quotechar then
				if s^=quotechar then	! embedded quote
					p^:=c
					++s
					++p
				else					! end of name
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
	itemlength:=p-itemstr				! actual length of token
	rd_pos:=s

	return itemstr
end

global function strtoint(ichar s,int length=-1, base=10)int64=
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
	array[512]char str
	ref char s
	int length
	int32 numlength
	real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
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

global proc m$read_strold(ref char dest, int destlen=0,fmt=0)=
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

global function m$read_str(int fmt=0)ichar t=
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

	t:=pcm_alloc(length+1)
	memcpy(t,s,length)
	(t+length)^:=0
	return t
end

global proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_strold(dest,destlen,fmt)
end

global proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

global proc reread=
	rd_pos:=rd_lastpos
end

global function valint(ichar s, int fmt=0)int64=
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

global function valreal(ichar s)real=
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

proc iconvlcn(ref char s,int n) =		!ICONVLCN
to n do
	s^:=tolower(s^)
	++s
od
end

proc iconvucn(ref char s,int n) =		!ICONVUCN
to n do
	s^:=toupper(s^)
	++s
od
end

proc convlcstring(ref char s)=		!CONVLCSTRING
while (s^) do
	s^:=tolower(s^)
	++s
od
end

proc convucstring(ref char s)=		!CONVUCSTRING
while (s^) do
	s^:=toupper(s^)
	++s
od
end

!GLOBAL PROC M$PRINT_U32(word32 a, ref void fmt)=
!	m$print_u64(a,nil)
!end
!
!GLOBAL PROC M$PRINT_I32(int32 a, ref void fmt)=
!	m$print_i64(a,nil)
!end
!
!GLOBAL PROC M$STARTPRINT(ref void dev)=
!	m$print_startfile(dev)
!end
!
!GLOBAL PROC M$STARTPRINTCON=
!	m$print_startcon()
!end
!
!GLOBAL PROC M$ENDPRINT=
!	m$print_end()
!end

global threadedproc m$ufloat_r64u64=
	assem
		cmp D10,0
		jl fl1
!number is positive, so can treat like i64
		cvtsi2sd XMM15,D10
		ret
fl1:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2sd XMM15,D10
		addsd XMM15,[offset64]	!(add 2**63 back to result)
		ret
	end
end

global threadedproc m$ufloat_r64u32=
	assem
		mov D10,D10				! clear top half (already done if value just moved there)
		cvtsi2sd XMM15,D10
		ret
	end
end

global threadedproc m$ufloat_r32u32=
	assem
		mov D10,D10
		cvtsi2ss XMM15,D10
		ret
	end
end

global threadedproc m$ufloat_r32u64=
	assem
		cmp D10,0
		jl fl2
!number is positive, so can treat like i64
		cvtsi2ss XMM15,D10
		ret
fl2:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2ss XMM15,D10
		addss XMM15,[offset32]	!(add 2**63 back to result)
		ret
	end
end

!global function m$power_i64(int64 n,a)int64=
!if n<0 then
!	return 0
!elsif n=0 then
!	return 1
!elsif n=1 then
!	return a
!elsif (n iand 1)=0 then
!!	return ipower(a*a,n/2)
!	return m$power_i64(a*a,n/2)
!else			!assume odd
!	return m$power_i64(a*a,(n-1)/2)*a
!fi
!end

global function m$power_i64(int64 n,a)int64=
if n<0 then
	return 0
elsif n=0 then
	return 1
elsif n=1 then
	return a
elsif (n iand 1)=0 then
!	return ipower(a*a,n/2)
	return m$power_i64(n/2,sqr a)
else			!assume odd
	return m$power_i64((n-1)/2,sqr a)*a
fi
end

global proc m$intoverflow=
	abortprogram("Integer overflow detected")
end

global proc m$mul_i128(word128 bb,aa)=
!CPL "$MUL128"
	assem
		mov d2,[aa]			!a1
		mov d3,[aa+8]		!a2
		mov d4,[bb]			!b1
		mov d5,[bb+8]		!b2


		mov d0,d2			!a1
		imul2 d0,d5			!*b2	
		mov d6,d0			!=>d6

		mov d0,d3			!a2
		imul2 d0,d4			!*b1
		mov d7,d0			!=>d7

		mov d0,d2			!a1
		mul d4				!*b1
		add d11,d6			! + a1*b2<<64
		add d11,d7			! + a2*b1<<64
		mov d1,d11
	end
end

global proc m$idiv_i128(word128 bb,aa)=
!does 128/64 bits only
	assem
		mov d2,[aa]
		mov d3,[aa+8]

		mov d4,[bb]
		or d4,d4
		jz divbyzero

		mov d0,d3		!a2
		xor d11,d11
		div d4			!a2/b
		mov d6,d0		! => c2
		mul d4			!c2*b
		sub d3,d0		!a2-:=c2*b

		mov d0,d2
		mov d11,d3		!a2:a1
		div d4			!/b
		mov d1,d6
	end
	return

asm divbyzero:
CPL "DIV BY ZERO"
	stop 1
end

global proc m$dotindex(word i,a)=
!return a.[i] in d0
	assem
		mov d0,[a]
		mov cl,[i]
		shr d0,cl
		and d0,1
	end	
end

global proc m$dotslice(word j,i,a)=
!return a.[i..j] in d0; assumes j>=i
	assem
		mov d0,[a]
		mov rcx,[i]
		shr d0,cl
		sub rcx,[j]
		neg rcx				!j-1
		mov d2,0xFFFF'FFFF'FFFF'FFFE
		shl d2,cl
		not d2
		and d0,d2
	end	
end

global proc m$popdotindex(word i,ref word p,word x)=
!p^.[i]:=x
	assem
		mov d3,[p]
		mov cl,[i]
		mov d0,[d3]
		mov d1,1
		shl d1,cl			!000001000
		not d1				!111110111
		and d0,d1			!clear that bit in dest
		mov d1,[x]
		and d1,1
		shl d1,cl
		or d0,d1
		mov [d3],d0
	end	
end

global proc m$popdotslice(word j,i, ref word p, word x)=
!p^.[i..j]:=x
	assem
!d3 = p
!d4 = x, then shifted then masked x
!d5 = i
!d6 = clear mask

		mov d3,[p]
		mov d4,[x]
		mov d5,[i]
		mov rcx,d5			!i
		shl d4,cl			!x<<i
		mov rcx,[j]
		sub rcx,d5			!j-i
		inc rcx				!j-i+1
		mov d2,0xFFFF'FFFF'FFFF'FFFF
		shl d2,cl			!...111100000     (assume 5-bit slice)
		not d2				!...000011111
		mov rcx,d5			!i
		shl d2,cl			!...000011111000  (assume i=3)
		and d4,d2			!mask x (truncate extra bits)
		mov d0,[d3]
		not d2				!...111100000111
		and d0,d2			!clear dest bits
		or d0,d4			!add in new bits
		mov [d3],d0
	end	
end


global function m$sin(real x)real = {`sin(x)}
global function m$cos(real x)real = {`cos(x)}
global function m$tan(real x)real = {`tan(x)}
global function m$asin(real x)real = {`asin(x)}
global function m$acos(real x)real = {`acos(x)}
global function m$atan(real x)real = {`atan(x)}
global function m$ln(real x)real = {`log(x)}
!global function m$lg(real x)real = {`lg(x)}
global function m$log(real x)real = {`log10(x)}
global function m$exp(real x)real = {`exp(x)}
global function m$floor(real x)real = {`floor(x)}
global function m$ceil(real x)real = {`ceil(x)}
global function m$fract(real x)real = {abortprogram("FRACT");0}
global function m$round(real x)real = {abortprogram("ROUND");0}
=== clibnew.m 3/38 ===
global type filehandle=ref void

importlib $cstd=
!	clang function malloc	(wordm)ref void
	clang function malloc	(word64)ref void
	clang function realloc	(ref void, wordm)ref void
	clang proc     free		(ref void)
	clang proc     memset	(ref void, int32, wordm)
	clang proc     memcpy	(ref void, ref void, wordm)
	clang function clock	:int32
	clang function ftell	(filehandle)int32
	clang function fseek	(filehandle, int32, int32)int32
	clang function fread	(ref void, wordm, wordm, filehandle)wordm
	clang function fwrite	(ref void, wordm, wordm, filehandle)wordm
	clang function getc		(filehandle)int32
	clang function ungetc	(int32, filehandle)int32
	clang function fopen	(ichar,ichar="rb")filehandle
	clang function fclose	(filehandle)int32
	clang function fgets	(ichar, int, filehandle)ichar
	clang function remove	(ichar)int32
	clang function rename	(ichar, ichar)int32
	clang function getchar	:int32
	clang proc     putchar	(int32)
	clang proc     setbuf	(filehandle, ref byte)

	clang function strlen	(ichar)wordm
	clang function strcpy	(ichar, ichar)ichar
	clang function strcmp	(ichar, ichar)int32
	clang function strncmp	(ichar, ichar, wordm)int32
	clang function strncpy	(ichar, ichar, wordm)wordm
	clang function memcmp	(ref void, ref void, wordm)int32
	clang function strcat	(ichar, ichar)ichar
	clang function tolower	(int32)int32
	clang function toupper	(int32)int32
	clang function isalpha	(int32)int32
	clang function isupper	(int32)int32
	clang function islower	(int32)int32
	clang function isalnum	(int32)int32
	clang function isspace	(int32)int32
	clang function strstr	(ichar, ichar)ichar
	clang function atol		(ichar)intm
	clang function atoi		(ichar)int32
	clang function strtod	(ichar,ref ref char)real64

	clang function puts		(ichar)int32
	clang function printf	(ichar, ...)int32

	clang function sprintf	(ichar, ichar, ...)int32
!	clang function __mingw_sprintf	(ichar, ...)int32

	clang function sscanf	(ichar, ichar, ...)int32
	clang function scanf	(ichar, ...)int32

	clang function rand		:int32
	clang proc     srand	(word32)
	clang function system	(ichar)int32

	clang function fgetc	(filehandle)int32
	clang function fputc	(int32,  filehandle)int32
	clang function fprintf	(filehandle, ichar, ...)int32
	clang function fputs	(ichar,  filehandle)int32
	clang function feof		(filehandle)int32
	clang function getch	:int32
	clang function kbhit	:int32
	clang function _mkdir	(ichar)int32
	clang function mkdir	(ichar)int32
	clang function dummy	(real)real
	clang function strchr	(ichar,int32)ichar

	clang proc     _exit	(int32)
	clang proc     "exit"	(int32)
!	clang proc     `exit	(int32)
	clang function	pow		(real,real)real

	clang function	`sin	(real)real
	clang function	`cos	(real)real
	clang function	`tan	(real)real
	clang function	`asin	(real)real
	clang function	`acos	(real)real
	clang function	`atan	(real)real
	clang function	`log	(real)real
	clang function	`log10	(real)real
	clang function	`exp	(real)real
	clang function	`floor	(real)real
	clang function	`ceil	(real)real

	clang proc      qsort   (ref void, word64, word64, ref proc)

end

importlib $cstdextra=
	clang function __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

global const c_eof		=-1
global const seek_set	= 0
global const seek_curr	= 1
global const seek_end	= 2
=== mlib.m 4/38 ===
import msys
import clib
import oslib

!const mem_check=1
const mem_check=0

GLOBAL INT MDEBUG


global [0..300]u64 allocupper
global int alloccode				!set by heapalloc
global int allocbytes				!set by heapalloc
global int fdebug=0
global int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

byte pcm_setup=0

int show=0

global int memtotal=0
global int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=500000
[maxmemalloc+1]ref int32 memalloctable
[maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend			!points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8 		!2048
global const int maxblocksize = 2048

[0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

GLOBAL [0:9]ref wordp freelist

global record strbuffer =
	ichar strptr
	int32 length
	int32 allocated
end

global tabledata() [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global function pcm_alloc(int n)ref void =		!PCM_ALLOC
ref byte p
!int i

!IF MDEBUG THEN
!CPL "PCMALLOC",N
!FI
if not pcm_setup then
	pcm_init()
!	abortprogram("need pcm_init")
fi

if n>maxblocksize then			!large block allocation
	alloccode:=pcm_getac(n)
	allocbytes:=allocupper[alloccode]

	p:=allocmem(allocbytes)
	if not p then
		abortprogram("pcm_alloc failure")
	fi

if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

	return p
fi

alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

if alloccode=0 then					!sizes below 16 bytes (can I adjust sizeindextable to?)
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

SMALLMEMTOTAL+:=ALLOCBYTES
!IF MDEBUG THEN
!CPL "PCMALLOC/ALLOCBYTES",ALLOCBYTES
!FI

if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
	freelist[alloccode]:=ref wordp(int((freelist[alloccode])^))

	return p
fi

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

return p
end

global proc pcm_freestr(ichar s) =
pcm_free(s,strlen(s)+1)
end

global proc pcm_free(ref void p,int n) =		!PCM_FREE
!n can be the actual size requested it does not need to be the allocated size
int acode

if n=0 then return fi

if n>maxblocksize then		!large block
	if mem_check then removefrommemalloc(p,n) fi

	free(p)
	return
fi

if p then
	acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

	smallmemtotal-:=allocupper[acode]

	if mem_check then removefrommemalloc(p,allocupper[acode]) fi

!	(ref wordp(p))^:=wordp(int(freelist[acode]))
	cast(p,ref wordp)^:=wordp(int(freelist[acode]))
	freelist[acode]:=p
fi
end

global proc pcm_freeac(ref void p,int alloc) =		!PCM_FREEAC
pcm_free(p,allocupper[alloc])
end

global proc pcm_copymem4(ref void p,q,int n) =	!PCM_COPYMEM4
!copy n bytes of memory from q to p.
!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!n can be zero, and need not be a multiple of 4 bytes

memcpy(p,q,n)
end

global proc pcm_clearmem(ref void p,int n) =		!PCM_CLEARMEM
memset(p,0,n)
end

global proc pcm_init =		!PCM_INIT
!set up sizeindextable too
!sizeindextable[0] = 0
int j,k,k1,k2
int64 size
const limit=1<<33

if pcm_setup then
	return
fi

pcm_newblock(0)

for i to maxblocksize do	!table converts eg. 78 to 4 (4th of 16,32,64,128)
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
!	if size>4 billion then
!		size+:=alloc_step
!	fi
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

global function pcm_getac(int size)int =		!PCM_GETAC
! convert linear blocksize from 0..approx 2GB to 8-bit allocation code

!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9

if size<=maxblocksize then
	return sizeindextable[size]		!size 0 to 2KB
fi

size:=(size+255)>>8					!scale by 256

!now same sizetable can be used for 2KB to 512KB (288 to 2KB)

if size<=maxblocksize then
	return sizeindextable[size]+8
fi

!sizetable now used for 512KB to 128MB (to 2KB)
size:=(size+63)>>6					!scale by 256

if size<=maxblocksize then
	return sizeindextable[size]+14
fi


!size>2048, which means it had been over 128MB.

size:=(size-2048+2047)/2048+22
return size
end

global function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
static int totalheapsize
ref byte p

totalheapsize+:=pcheapsize
alloccode:=0
p:=allocmem(pcheapsize)	!can't free this block until appl terminates
if p=nil then
	abortprogram("Can't alloc pc heap")
fi

pcheapptr:=p
pcheapend:=p+pcheapsize

if pcheapstart=nil then		!this is first block
	pcheapstart:=p
fi
pcheapptr+:=itemsize
return ref u32(p)
end

global function pcm_round(int n)int =		!PCM_ROUND
!for any size n, return actual number of bytes that would be allocated
static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

if n>maxblocksize then
	return n
else
	return allocbytes[sizeindextable[n]]
fi
end

global function pcm_array(int n)int =		!PCM_ARRAY
!n bytes are needed for an array return the number of bytes to be actually allocated
int m

if n<=maxblocksize then	!automatic rounding up used for small heap
	return pcm_round(n)
! allocbytes[sizeindextable[n]]
else				!devise some strategy probably doubling up.
	m:=2048
	while n>m do
		m<<:=1
	od
	return m
fi

end

global proc pcm_printfreelist(int size,ref wordp p) =		!PCM_PRINTFREELIST
println "Size: ",size
while p do
!	printf(" %llX",u64(p))
	print " ",,p:"h"
	p:=ref wordp(int(p^))
od
puts("")
end

global proc pcm_diags(ref char caption) =		!PCM_DIAGS
int m

println "HEAP FREELISTS:",caption

m:=16
for i:=1 to 8 do
	pcm_printfreelist(m,freelist[i])
	m<<:=1
od
end

global function pcm_allocz(int n)ref void =		!PCM_ALLOCZ
ref void p
p:=pcm_alloc(n)

memset(p,0,n)
return p
end

global function pcm_copyheapstring(ref char s)ref char =
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

global function pcm_copyheapstringn(ref char s,int n)ref char =
ref char q
if s=nil then return nil fi

q:=pcm_alloc(n+1)
memcpy(q,s,n)
(q+n)^:=0
return q
end

global function pcm_copyheapblock(ref char s, int length)ref char =
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

!CPL "***************ADD TO ALLOC:",ptr,size

for i to maxmemalloc do
	if memalloctable[i]=ptr then
		CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
CPL
CPL
		stop 2
	fi

	if memalloctable[i]=nil then		!unused entry
		memalloctable[i]:=ptr
		memallocsize[i]:=size
		return
	fi
od
CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
!remove ptr to allocated table

!CPL "------------------************REMOVE FROM ALLOC:",ptr,size

for i to maxmemalloc do
	if memalloctable[i]=ptr then

if memallocsize[i]<>size then
	CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
!PCERROR("MEMERROR")
CPL
CPL
	abortprogram("MEMSIZE")
fi

		memalloctable[i]:=nil
		return
	fi
od
CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
CPL
CPL
abortprogram("MEM")
stop 4
end

global function allocmem(int n)ref void =		!ALLOCMEM
ref void p

p:=malloc(n)
if (p) then
	return p
fi
println n,memtotal
abortprogram("Alloc mem failure")
return nil
end

global function reallocmem(ref void p,int n)ref void =		!REALLOCMEM
p:=realloc(p,n)
return p when p
println n
abortprogram("Realloc mem failure")
return nil
end

global proc abortprogram(ref char s) =		!ABORTPROGRAM
println s
print   "ABORTING: Press key..."
os_getch()
stop 5
end

global function getfilesize(filehandle handlex)int=		!GETFILESIZE
	word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

global proc readrandom(filehandle handlex, ref byte mem, int offset, size) =		!READRANDOM
	int a
	fseek(handlex,offset,seek_set)
	a:=fread(mem,1,size,handlex)			!assign so as to remove gcc warning
end

global function writerandom(filehandle handlex, ref byte mem, int offset,size)int =		!WRITERANDOM
	fseek(handlex,offset,seek_set)
	return fwrite(mem,1,size,handlex)
end

global function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

global function getfilepos(filehandle file)int=
	return ftell(file)
end

global function readfile(ref char filename)ref byte =		!READFILE
filehandle f
int size
ref byte m,p

f:=fopen(filename,"rb")
if f=nil then
	return nil
fi
rfsize:=size:=getfilesize(f)

m:=malloc(size+4)		!allow space for etx/zeof etc

if m=nil then
	return nil
fi

readrandom(f,m,0,size)
p:=m+size			!point to following byte
p^:=0
(p+1)^:=26
(p+2)^:=0			!allow use as string

fclose(f)
return m
end

global function writefile(ref char filename,ref byte data,int size)int =
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

global function checkfile(ref char file)int=		!CHECKFILE
filehandle f
if f:=fopen(file,"rb") then
	fclose(f)
	return 1
fi
return 0
end

global proc readlinen(filehandle handlex,ref char buffer,int size) =		!READLINEN
!size>2
int ch
ref char p
int n
array[0:100]char buff
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

p:=buffer+n-1		!point to last char
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

global proc iconvlcn(ref char s,int n) =		!ICONVLCN
to n do
	s^:=tolower(s^)
	++s
od
end

global proc iconvucn(ref char s,int n) =		!ICONVUCN
to n do
	s^:=toupper(s^)
	++s
od
end

global proc convlcstring(ref char s)=		!CONVLCSTRING
while (s^) do
	s^:=tolower(s^)
	++s
od
end

global proc convucstring(ref char s)=		!CONVUCSTRING
while (s^) do
	s^:=toupper(s^)
	++s
od
end

global function changeext(ref char s,newext)ichar=		!CHANGEEXT
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
static [260]char newfile
array[32]char newext2
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


sext:=extractext(s,1)			!include "." when it is only extension

case sext^
when 0 then						!no extension not even "."
	strcat(&newfile[1],&newext2[1])
when '.' then						!no extension not even "."
	strcat(&newfile[1],&newext2[2])
else							!has extension
	n:=sext-s-2			!n is number of chars before the "."
	strcpy(&newfile[1]+n+1,&newext2[1])
esac

return &newfile[1]
end

global function extractext(ref char s,int period=0)ichar=		!EXTRACTEXT
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
ref char t,u

t:=extractfile(s)

if t^=0 then			!s contains no filename
	return ""
fi

!t contains filename+ext
u:=t+strlen(t)-1		!u points to last char of t

while u>=t do
	if u^='.' then		!start extension found
		if (u+1)^=0 then		!null extension
			return (period|"."|"")
		fi
		return u+1			!return last part of filename as extension exclude the dot
	fi
	--u
od
return ""			!no extension seen
end

global function extractpath(ref char s)ichar=		!EXTRACTPATH
static [0:260]char str
ref char t
int n

t:=s+strlen(s)-1		!t points to last char

while (t>=s) do
	switch t^
	when '\\','/',':' then		!path separator or drive letter terminator assume no extension
		n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
		memcpy(&.str,s,n)
		str[n]:=0
		return &.str
	endswitch
	--t
od
return ""			!no path found
end

global function extractfile(ref char s)ichar=		!EXTRACTFILE
ref char t

t:=extractpath(s)

if t^=0 then			!s contains no path
	return s
fi

return s+strlen(t)		!point to last part of s that contains the file
end

global function extractbasefile(ref char s)ichar=		!EXTRACTBASEFILE
static [0:100]char str
ref char f,e
int n,flen

f:=extractfile(s)
flen:=strlen(f)
if flen=0 then		!s contains no path
	return ""
fi
e:=extractext(f,0)

if e^ then			!not null extension
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

global function addext(ref char s,ref char newext)ichar=		!ADDEXT
!when filespec has no extension of its own, add newext
ref char sext

sext:=extractext(s,1)

if sext^=0 then						!no extension not even "."
	return changeext(s,newext)
fi

return s							!has own extension; use that
end

global function alloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
ref void p

p:=malloc((n+1)*size)

if not p then
	abortprogram("Alloctable failure")
fi
return p
end

global function zalloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
ref int p

p:=alloctable(n,size)

pcm_clearmem(p,(n+1)*size)
return p
end

global proc checkfreelists(ichar s)=
ref wordp p,q
int64 aa

for i:=2 to 2 do
	p:=freelist[i]

	while p do
		aa:=int64(p)
		if aa>0xffff'FFFF or aa<100 then
			CPL s,"FREE LIST ERROR",i,p,q
!			os_getch(); stop 1
		fi
		q:=p
		p:=ref wordp(int(p^))
	od

od
end

global function pcm_alloc32:ref void =		!PCM_ALLOC
ref byte p

allocbytes:=32
!smallmemtotal+:=32

!if p:=ref byte(freelist[2]) then		!Items of this block size available
!	freelist[2]:=ref wordp((freelist[2])^)
!	if mem_check then addtomemalloc(ref int32(p),32) fi
!	return p
!fi

!No items in freelists: allocate new space in this heap block

return pcm_alloc(32)
end

global proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

!CPL "PCMFREE32"
smallmemtotal-:=32
if mem_check then removefrommemalloc(p,32) fi
!(ref wordp(p))^:=wordp(int(freelist[2]))
cast(p,ref wordp)^:=wordp(int(freelist[2]))
freelist[2]:=p
end

global proc outbyte(filehandle f,int x)=
fwrite(&x,1,1,f)
end

global proc outword16(filehandle f,word x)=
fwrite(&x,2,1,f)
end

global proc outword(filehandle f,word x)=
fwrite(&x,4,1,f)
end

global proc outword64(filehandle f,word64 x)=
fwrite(&x,8,1,f)
end

global function myeof(filehandle f)int=
int c

c:=fgetc(f)
if c=c_eof then return 1 fi
ungetc(c,f)
return 0;
end

global function pcm_smallallocz(int n)ref void =
ref byte p

if (alloccode:=sizeindextable[n])=0 then
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	memset(p,0,n)
	return p
fi

memset(p,0,n)

return p
end

!global function pcm_fastalloc(int n)ref void =
global function pcm_smallalloc(int n)ref void =
ref byte p

if (alloccode:=sizeindextable[n])=0 then
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi

return p
end

global proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
int newlen,oldlen
ichar newptr

IF N=0 THEN CPL "N=0" FI

if n=-1 then
	n:=strlen(s)
fi

oldlen:=dest^.length

if oldlen=0 then				!first string
	dest^.strptr:=pcm_alloc(n+1)
	dest^.allocated:=allocbytes
	dest^.length:=n				!length always excludes terminator
	memcpy(dest^.strptr,s,n)
	(dest^.strptr+n)^:=0
	return
fi

newlen:=oldlen+n
if newlen+1>dest^.allocated then
!CPL "REALLOC",NEWLEN
	newptr:=pcm_alloc(newlen+1)
	memcpy(newptr,dest^.strptr,oldlen)
	dest^.strptr:=newptr
	dest^.allocated:=allocbytes
fi

memcpy(dest^.strptr+oldlen,s,n)
(dest^.strptr+newlen)^:=0

dest^.length:=newlen
end

global proc gs_init(ref strbuffer dest)=			!INITGENSTR
pcm_clearmem(dest,strbuffer.bytes)
end

global proc gs_free(ref strbuffer dest)=
if dest^.allocated then
	pcm_free(dest^.strptr,dest^.allocated)
fi
end

global proc gs_str(ref strbuffer dest,ichar s)=			!GENSTR
strbuffer_add(dest,s)
end

global proc gs_char(ref strbuffer dest,int c)=
array[16]char s

s[1]:=c
s[2]:=0

strbuffer_add(dest,&.s,1)
end

global proc gs_strn(ref strbuffer dest,ichar s,int length)=
strbuffer_add(dest,s,length)
end

global proc gs_strvar(ref strbuffer dest,s)=			!GENSTR
strbuffer_add(dest,s^.strptr)
end

global proc gs_strint(ref strbuffer dest,int64 a)=
strbuffer_add(dest,strint(a))
end

global proc gs_strln(ref strbuffer dest,ichar s)=		!GENSTRLN
gs_str(dest,s)
gs_line(dest)
end

global proc gs_strsp(ref strbuffer dest,ichar s)=
gs_str(dest,s)
gs_str(dest," ")
end

global proc gs_line(ref strbuffer dest)=
strbuffer_add(dest,"\w")
end

global function gs_getcol(ref strbuffer dest)int=
return dest^.length
end

global proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
int col,i,n,slen
array[2560]char str
col:=dest^.length
strcpy(&.str,s)
slen:=strlen(s)
n:=w-slen
!CPL =slen,=w,=n
if n>0 then
	for i:=1 to n do
		str[slen+i]:=padch
	od
	str[slen+n+1]:=0
fi
gs_str(dest,&.str)
end

global proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
gs_leftstr(dest,strint(a),w,padch)
end

global proc gs_padto(ref strbuffer dest,int col, ch=' ')=
int n
array[2560]char str

n:=col-dest^.length
if n<=0 then return fi
for i:=1 to n do
	str[i]:=ch
od
str[n+1]:=0
gs_str(dest,&.str)
end

global proc gs_println(ref strbuffer dest,filehandle f)=
(dest.strptr+dest.length)^:=0

if f=nil then
	println dest.strptr,,"\c"
else
	println @f,dest.strptr,,"\c"
fi
end

global function nextcmdparam(int &paramno, ichar &name, &value, ichar defext=nil)int=
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
	if readnextfileitem(fileptr,item)=0 then		!eof
		free(filestart)								!file allocated via malloc
		infile:=0
		goto reenter
	fi
else
	if paramno>nsysparams then
		return pm_end
	fi
	item:=sysparams[paramno]
	++paramno

	length:=strlen(item)

	if item^='@' then		!@ file
		filestart:=fileptr:=cast(readfile(item+1))
		if filestart=nil then
			println "Can't open",item
			stop 7
		fi
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

if fileext^=0 then							!no extension
	strcpy(&.str,name)
	if defext and not colonseen then
		name:=addext(&.str,defext)				!try .c
	fi
elsif eqstring(fileext,"dll") then
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
	when ' ','\t',13,10 then	!skip white space
		++p
	when 26,0 then				!eof
		return 0
	else
		exit
	esac
od

case p^
when '!', '#' then			!comment
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
when '"' then				!read until closing "
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

global proc ipadstr(ref char s,int width,ref char padchar=" ")=
int n

n:=strlen(s)
to width-n do
	strcat(s,padchar)
od
end

global function padstr(ref char s,int width,ref char padchar=" ")ichar=
static [256]char str

strcpy(&.str,s)
ipadstr(&.str,width,padchar)
return &.str
end

global function chr(int c)ichar=
static [8]char str

str[1]:=c
str[2]:=0
return &.str
end

global function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function cmpstringn(ichar s,t,int n)int=
	int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

global function cmpbytes(ref void p,q,int n)int=
	int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end


global proc mseed(word64 a,b=0)=
seed[1]:=a
if b then
	seed[2]:=b
else
	seed[2] ixor:=a
fi
end

global function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
	word64 x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

global function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

global function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

global function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

!global function mrandomreal:real =
!!positive random real value from 0 to 0.999999999999999999891579782751449556599254719913005828857421875
!!upper limit is (2**63-1)/(2**63)
!	return real(mrandomp())/9223372036854775808.0
!end

global function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

global function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807
end

global function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

int a,offset,i,size
array[100]char name
array[300]char exefile
ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
int packexesize				!byte size
ref char packfilename
int packfilesize
ref byte packfileptr

!macro getfileint(data,offset)=(ref int32(data+offset))^
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
!STOP

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
=== oswindows.m 5/38 ===
import clib
import mlib

const wm_destroy=2

type wt_word	= word16
type wt_wordpm	= word32
type wt_bool	= word32
type wt_dword	= word32
type wt_wchar	= word16
type wt_wcharpm	= word32
type wt_char	= byte
type wt_ichar	= ref char
type wt_ptr		= ref void
type wt_wndproc	= ref proc
type wt_handle	= ref void
type wt_int		= int32
type wt_uint	= word32
type wt_long	= int32
type wt_wparam	= wordm
type wt_lparam	= wordm
type wt_point	= rpoint

global record rsystemtime =
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
!	windows function "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	windows function "GetStdHandle"(wt_dword)wt_handle
	windows function "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	windows function "SetConsoleCtrlHandler"(wt_wndproc,int)int
	windows function "SetConsoleMode"(wt_handle,wt_dword)int
	windows function "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	windows function "GetLastError":wt_dword
	windows function "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	windows function "GetExitCodeProcess"(wt_handle,wt_ptr)int
	windows function "CloseHandle"(wt_handle)int
	windows function "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	windows function "FlushConsoleInputBuffer"(wt_handle)int
	windows function "LoadLibraryA"(wt_ichar)wt_handle
!	windows function "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	windows function "GetProcAddress"(wt_handle,wt_ichar)ref void
	windows function "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	windows function "RegisterClassExA"(wt_ptr)wt_wordpm
	windows function "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)intm
	windows function "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	windows proc     "Sleep"(wt_dword)
	windows function "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	windows proc     "ExitProcess"(wt_uint)
	windows proc	 "PostQuitMessage"(wt_int)

!	windows proc	 "MessageBoxA"(wt_int,wt_ichar,wt_ichar,wt_int)

	windows proc	 "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	windows function "QueryPerformanceCounter"(ref int64)wt_bool
	windows function "QueryPerformanceFrequency"(ref int64)wt_bool

	windows function "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	windows function "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	windows proc     "GetSystemTime"(ref rsystemtime)
	windows proc     "GetLocalTime"(ref rsystemtime)

	windows function "GetTickCount":wt_dword
	windows function "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

end

record input_record =
	wt_word	eventtype
	word16	padding
		wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		wt_word	repeatcount
		wt_word	virtualkeycode
		wt_word	virtualscancode
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
	wt_dword	size
!.if $64bit
	word32 dummy1
!.endif
	wt_ichar	reserved
	wt_ichar	desktop
	wt_ichar	title
	wt_dword	x
	wt_dword	y
	wt_dword	xsize
	wt_dword	ysize
	wt_dword	xcountchars
	wt_dword	ycountchars
	wt_dword	fillattribute
	wt_dword	flags
	wt_word		showwindow
	wt_word		reserved2
!.if $64bit
	word32 dummy2
!.endif
	wt_ptr		reserved4
	wt_handle	stdinput
	wt_handle	stdoutput
	wt_handle	stderror
end

record rprocess_information =
	wt_handle process
	wt_handle thread
	wt_dword processid
	wt_dword threadid
end

record rwndclassex =
	wt_uint		size
	wt_uint		style
	wt_wndproc	wndproc
	wt_int		clsextra
	wt_int		wndextra
	wt_handle	instance
	wt_handle	icon
	wt_handle	cursor
!	wt_handle	background
	wt_handle	background
	wt_ichar	menuname
	wt_ichar	classname
	wt_handle	iconsm
end

global record rmsg =
	wt_handle	hwnd
	wt_uint		message
!.if $64bit
	word32		dummy1
!.endif
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
!.if $64bit
	word32		dummy2
!.endif
	wt_point	pt
end

!wt_word x
const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

ref function(ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

global proc os_init=
int i,count
rconsole info

!general initialisation
hconsole:=GetStdHandle(u32(-11))
hconsolein:=GetStdHandle(u32(-10))

lastkey.repeatcount:=0
keypending:=0

!CPL "OSINIT"
SetConsoleCtrlHandler(nil,1)

SetConsoleMode(hconsole,1 ior 2)
!SetConsoleMode(hconsole,1 )

init_flag:=1

end

global function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
wt_dword exitcode
int status
int cflags:=0

rstartupinfo si
rprocess_information xpi

memset(&si,0,si.bytes)
memset(&xpi,0,xpi.bytes)

switch newconsole
when 0 then cflags := NORMAL_PRIORITY_CLASS
when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
endswitch

si.size := rstartupinfo.bytes

status:=CreateProcessA( nil,
	cmdline,
	nil,
	nil,
	1,
	cflags,

	nil,
	nil,
	&si,
	&xpi )

if status=0 then		!fails
	status:=GetLastError()
	println "Winexec error:",status
	return -1
end

WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
GetExitCodeProcess(xpi.process,&exitcode)

CloseHandle(xpi.process)
CloseHandle(xpi.thread)

return exitcode
end

global function os_execcmd(ichar cmdline, int newconsole=0)int =
wt_dword exitcode
int i,j,k

rstartupinfo si
rprocess_information xpi

memset(&si,0,si.bytes)
memset(&xpi,0,xpi.bytes)

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

global function os_getch:int=
int k

k:=os_getchx() iand 255

return k
end

global function os_kbhit:int=
wt_dword count
!os_init() unless init_flag

unless init_flag then os_init() end
!unless initflag then: os_init()

GetNumberOfConsoleInputEvents(hconsolein,&count)
return count>1
end

global proc os_flushkeys=
FlushConsoleInputBuffer(hconsolein)
end

global function os_getconsolein:ref void=
return ref void(hconsolein)
end

global function os_getconsoleout:ref void=
return ref void(hconsole)
end

global function os_proginstance:ref void=
abortprogram("PROGINST")
return nil
end

global function os_getdllinst(ichar name)u64=
wt_handle hinst

hinst:=LoadLibraryA(name)
return cast(hinst)
end

global function os_getdllprocaddr(intm hinst,ichar name)ref void=
return GetProcAddress(cast(int(hinst)),name)
end

global proc os_initwindows=
os_init()
os_gxregisterclass("pcc001")
end

global proc os_gxregisterclass(ichar classname)=
const idcarrow=32512
rwndclassex r
static byte registered

if registered then
	return
fi

!CPL "REG CLASS"

memset(&r,0,r.bytes)
r.size:=r.bytes
r.style:=8 ior 32		!CS_DBLCLKS | CS_OWNDC
r.wndproc:=cast(&mainwndproc)
!r.wndproc:=&xmainwndproc
!r.wndproc:=&cmainwndproc
r.instance:=nil

r.icon:=nil		!loadicon(proginstance,"SCW32")
r.cursor:=LoadCursorA(nil,ref void(idcarrow))		!IDC_ARROW)
r.background:=cast(15+1)					!COLOR_BTNFACE+1
r.menuname:=nil
r.classname:=classname
r.iconsm:=nil	!loadicon(proginstance,"SCW32")

if RegisterClassExA(&r)=0 then
	println classname,GetLastError
	abortprogram("Registerclass error")
end
registered:=1
end

global callback function mainwndproc (\
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)intm=
rmsg m
int i,result
intm l
static int count=0

!CPL "MAINWND/MV"

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

!callback proc timerproc(wt_handle hwnd, int msg, id, time)=
proc timerproc(wt_handle hwnd, int msg, id, time)=
println "TIMERPROC"
end

global proc os_setmesshandler(ref void addr)=
wndproc_callbackfn:=addr
end

global function os_getchx:int=
!Q! function os_getchx_c:int
!return a 32-bit value containing:
! 15..B0:	char code
! 23..16	virtual keycode
! 31..24	shift flags (.[24]=shift, .[25]=ctrl, .[26]=alt, .[27]=capslock)
const rightaltmask	= 1
const leftaltmask	= 2
const leftctrlmask	= 8
const rightctrlmask	= 4
const shiftmask		= 16
const capsmask		= 128
const scrollmask	= 64
int count
int charcode,keyshift,keycode
int altdown,ctrldown,shiftdown,capslock

!os_init() unless init_flag
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

!set shift flags

altdown		:= ((lastkey.controlkeystate iand (leftaltmask ior rightaltmask))|1|0)
ctrldown	:= ((lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask))|1|0)
shiftdown	:= ((lastkey.controlkeystate iand shiftmask)|1|0)
capslock	:= ((lastkey.controlkeystate iand capsmask)|1|0)

--lastkey.repeatcount		!count this key out

charcode:=lastkey.asciichar
keycode:=lastkey.virtualkeycode iand 255

if charcode<0 then
	if charcode<-128 then
		charcode:=0
	else
		charcode+:=256
	fi
fi

!CPL "CHARCODE2=%d %X\n",charcode,charcode
!for keycodes in range 186 to 223, which are all stand-alone punctuation keys, I might
!wish to set charcode to the appropriate printed char code (currently charcode will be
!zero, and keyboard handlers need to detect keycodes such as vkequals)
!....

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

global function os_getos=>ichar=
if $targetbits=32 then
	return "W32"
else
	return "W64"
fi
end

global function os_gethostsize=>int=
return $targetbits
end

global function os_shellexec(ichar opc, file)int=
return system(file)
end

global proc  os_sleep(int a)=
Sleep(a)
end

global function os_getstdin:filehandle =
return fopen("con","rb")
end

global function os_getstdout:filehandle =
return fopen("con","wb")
end

global function os_gethostname:ichar=
static [300]char name
static int n

GetModuleFileNameA(nil,&.name,name.bytes)
strcat(&.name,"/")
return &.name
end

global function os_getmpath:ichar=
return F"C:\m\"
end

global proc os_exitprocess(int x)=
stop x
!ExitProcess(x)
end

global function os_clock:int64=
return clock()
end

global function os_getclockspersec:int64=
return 1000
end

global function os_iswindows:int=
return 1
end

global function os_filelastwritetime(ichar filename)int64=
wt_handle f;
int64 ctime,atime,wtime;

if filename=nil then				!used to test whether supported
	return 1
fi

f:=CreateFileA(filename,0x80000000,1,nil, 3,3,nil);
if int64(f)=-1 then
	return 0
fi

GetFileTime(f,&ctime,&atime,&wtime);
CloseHandle(f);

return wtime;
end

global proc os_getsystime(ref rsystemtime tm)=
GetLocalTime(tm)
end

global proc os_messagebox(ichar s,t)=
messageboxa(0,s,t,0)
end

global function os_hpcounter:int64=
int64 a

queryperformancecounter(&a)
return a

end

global function os_hpfrequency:int64=
int64 a

queryperformancefrequency(&a)
return a

end

global proc os_peek=
int ticks
static int lastticks
array [100]byte m
	ticks:=GetTickCount()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
=== pci.m 6/38 ===
import msys

import mlib
import clib
import oslib

import pc_types
import pc_decls
import pc_support as pcs
!import pc_objects
import pc_print
import pc_jhandlers
import pc_khandlers
import pc_oslayer

mapmodule pc_assem => pc_assemc when ctarget

import pc_assem
import pq_common
import pc_pcfns

!global ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
!global int packexesize				!byte size
!global ref char packfilename
!global int packfilesize
!global ref byte packfileptr
!
global int dispatch_type = asm_dispatch
!var int dispatch_type = fn_dispatch

!global enum (pc_file, q_file, mem_file)
global tabledata() []ichar filetypenames=
	(pc_file,		$),
	(q_file,		$),		!one-time compilation (.pc file persistent)
	(qq_file,		$),		!always compile (.pc file temporary)
	(mem_file,		$),
end

global tabledata() [-4:]ichar errorcodenames =
	(runtime_error=-4,		"Runtime error"),
	(load_error,			"General load error:"),
	(compile_error,			"Couldn't compile:"),
	(filenotfound_error,	"File not found:"),
	(normal_exit,			$),
end

include "CCM_FN."

global function runpcl(ichar filename, int filetype=pc_file)int=
!run pc program given name of .pc file, .q file, or pointer to the pc file data
!in memory (eg. when part of the executable)
!return program stop code when >=0, or errorcode when <0

	ref byte pcfile			!in-memory pc file
	int stopcode
	ichar pcfilename

!CPL "RUNPCL",FILENAME,FILETYPENAMES[FILETYPE]

!DISPATCH_TYPE:=FN_DISPATCH


!CPL $lineno,=DISPATCHNAMES[DISPATCH_TYPE], =ASMAVAILABLE()

	if dispatch_type=asm_dispatch and not asmavailable() then
		dispatch_type:=fn_dispatch
	fi
!CPL =DISPATCHNAMES[DISPATCH_TYPE], =ASMAVAILABLE()

	run_initdata()

	case filetype
	when pc_file then
		pcfile:=readfile(filename)
		if pcfile=nil then
			return filenotfound_error
		fi

!	when q_file,qq_file then
!		if not checkfile(filename) then
!			return filenotfound_error
!		fi
!		pcfilename:=compileq(filename)
!		if pcfilename=nil then
!			return compile_error
!		fi
!		pcfile:=readfile(pcfilename)
!		if pcfile=nil then abortprogram("runpcl?") fi

	when mem_file then
		pcfile:=cast(filename)
	esac

	if not loadpcfile(pcfile) then
		return load_error
	fi

	if filetype=qq_file then			!remove .pc file created above
		remove(pcfilename)
	fi

	stopcode:=runpcprogram()

	return stopcode
end

global proc run_initdata=
	static int initialised=0

	if initialised then
		return
	fi

	if ncmdparams=0 then
		cmdparamtable[0]:=sysparams[1]
	fi

	pcm_init()
	inittypetables()
	initpcldata()
	initcalltables()
end

global function runpcprogram:int =
!run code that has been compiled from source (frompc=0) or loaded as bytecode (frompc=1)
!and processed into global pcl data via loadpcfile

int i,j,cmd,fmt
ref int lab
int exitcode
int64 progticks

initbytecode()

varstack:=malloc(stacksize*varrec.bytes)
if not varstack then
	abortprogram("varstack?")
fi

sptr:=&varstack^[stacksize]

--sptr

stacklimit:=&varstack^[1000]


frameptr:=cast(sptr)

pcptr:=&moduletable[0].pccode^[1]
pccode:=cast(&moduletable[0].pccode^[0])

stopped:=0				!for use  fn table dispatch only

disploop()				!ENTER DISPATCHER LOOP

if sptr^.tagx=tvoid then
	return 0
else
	return sptr^.value
fi
end

proc initbytecode=

os_initwindows()

allocatestrings()
pclinit()

pcl_initusertypes()

fixup_all_pc()		!expand ci/cr/etc, convert proc refs etc

end

proc disploop=			!DISPLOOP
int i,k

case dispatch_type

when fn_dispatch then
	disploop_fn(0)

when deb1_dispatch,deb2_dispatch then
	disploop_deb()

when asm_dispatch then
	k:=os_clock()
	disploop_asm()
	k:=os_clock()-k
esac
end

proc pclinit =

int i

!build cmdmap translation table

for i:=1 to klastcmd do
	case dispatch_type
	when fn_dispatch then
		cmdmap[i]:=disploop_fn(i)
	when deb1_dispatch, deb2_dispatch then
		cmdmap[i]:=cast(i)
	when asm_dispatch then
		cmdmap[i]:=cast(i)
	esac
od
end

proc fixup_all_pc =
int i,j,cmd,lab,a,index,startindex,recordtype,fieldtype,mx
int pp
int64 a64
genfielddatarec gd
ref intpc p

for mx:=1 to nmodules do
	fixup_module_pc(mx)
od
fixup_module_pc(0)

end

proc fixup_module_pc(int mx) =
int i,cmd,lab,index,startindex,recordtype,fieldtype
int pp,m
int64 a
genfielddatarec gd
ref intpc p,pccode
ref strec d,owner
static int 	SCOUNT=0
variant v

pccode:=p:=cast(moduletable[mx].pccode)
!CPL "FIXUP MODULE PC",mx

do
	cmd:=p^

	if mx=0 then
		if cmd=kstop and stopseq=nil then
			stopseq:=p-2
		elsif cmd=kraise and raiseseq=nil then
			raiseseq:=p
		fi
	fi
!
	p++^:=cast(cmdmap[cmd])			!convert cmd index to labeladdr/functionaddr/same index

	for i:=1 to cmdnopnds[cmd] do
		switch cmdfmt[cmd,i]
		when clabel then
			p^:=cast(pccode+p^-1)
		when cproc then
			d:=&pcsymboltable^[p^]
			if d^.address=nil then
				owner:=d^.owner
				while owner^.nameid=typeid do
					owner:=owner^.owner
				od
				m:=owner^.ax_moduleno
				d^.address:=cast(&(moduletable[m].pccode^[d^.index]))
			fi
			p^:=cast(d^.address)
		when cmemory then
			d:=&pcsymboltable^[p^]
			if d^.address=nil then
				v:=pcm_alloc(varsize)
				v^.tagx:=tvoid
				d^.address:=v
			fi
			p^:=cast(d^.address)
		when cstring then
			p^:=cast(&stringobjtable^[p^])
		endswitch
		++p
	od
	if cmd=kendmodule or cmd=0 then
		exit
	fi
od
if dispatch_type=asm_dispatch then
	fixup_asm(mx)
fi
end

function disploop_fn(int n)ref int =			!DISPLOOP_FN
int64 count
const intervalcount=1

type fnptr2=ref ref function: ref intm
!type fnptr=ref function: ref intm
!fnptr fn

word lastticks,ticks

if n then		! return jumptable item
	return ref int(handlertable[n])
fi

count:=intervalcount
lastticks:=os_clock()

repeat
	pcptr:=ref ref function:ref intpc(pcptr)^^()
until stopped

ticks:=os_clock()-lastticks

return nil
end

proc disploop_deb=
int a,b,i,j,k,totalcounts,lastcmd,cmd,t,u
int index,file,line,moduleno
variant p
int count
const intervalcount=1

type fnptr2=ref ref function: ref intm
type fnptr=ref function: ref intm
fnptr fn

word lastticks,ticks

count:=intervalcount
lastticks:=os_clock()

lastcmd:=knop

repeat
!	index:=findpcindex(pcptr,moduleno)
	cmd:=pcptr^

	if dispatch_type=deb1_dispatch or fdebug then
		ichar filename
		findlinenumber(pcptr, line, moduleno)
!CPL =LINE
!		index:=findlinenumber(pcptr)
!		if index then
!			line:=linetable^[index].line
!			filename:=sourcefiletable^[linetable^[index].file]
!		else
!			line:=0
!			filename:="?"
!		fi
!		cpl pcptr,"<",,cmdnames[cmd],,">",=line,=filename,=cmd,=sptr,ttname[sptr^.tag]
!		cpl "<",,cmdnames[cmd],,">",=line,=filename,=cmd,=sptr,ttname[sptr^.tag],=index
		index:=&varstack^[stacksize]-sptr
!		cpl "<",,cmdnames[cmd],,">",=line,moduletable[moduleno].name,=cmd,=sptr,
!			ttname[sptr^.tag], "	***",(PVAR|ttname[PVAR^.tag]|"PVAR=0"),=PVAR,=index
!		cpl "<",,cmdnames[cmd],,">",=line,
!			ttname[sptr^.tag], "	***",(PVAR|ttname[PVAR^.tag]|"PVAR=0"),=PVAR,=index
!CPL =CMD, KCALL,cmdnames[kcall]
		cpl PCPTR,"<",,cmdnames[cmd],,">",=line,=sptr,=index
!OS_GETCH()
	fi

	pcptr:=cast(fnptr(handlertable[pcptr^])^())
!cpl "AFTER EXEC",=SPTR

!	if ftrace then CPL "NEXT PCPTR2:",pcptr fi!,findpcindex(pcptr)
until stopped

CPL "DEB STOPPED"
end

global proc runproc(ref void fnptr,variant a,b,dest) =			!RUNPROC
!Directly call a pcl function by supplying it's pc-address
!sptr/frameptr etc should already have been set up (any start proc should have been called)
!Allows 0, 1, or 2 params: (), (a), or (a,b)
!Note: param data is not freed here caller should take care of that
!Return values are stored in dest (any non-int or void result is returned as 0)
!Use of the stack::
! The stack as it was at the time of the callext call (or via a callback from Windows)
! is entirely unaffected. However some things will be pushed onto it here::
! * Push void which is used for any return value of the function that is called
! * Push 0, 1 or 2 parameters (as supplied in a and b; a is pushed first)
! * The interpreter is then started, at the function call pc address supplied
! * This involves pushes a retaddr value. Since this is not a conventional call,
!   The return address is contrived to point to a STOP0 pc opcode
! * After the return from the function, STOP0 is executed, which pushes a zero
!   value to the stack.
! * If the called function eventually returns, it will execute STOP0, but
!   there is no Retaddr value left on the stack, and it will know to use the
!	actual return value (0 is used of the called function did not return a value)
! * If STOP is explicitly used, then a Retaddr value stays on the stack (for this
!	function, or any nested one), and the Stop value is used instead

variant oldsptr
ref byte oldframeptr
ref intpc oldpcptr
byte oldstopped
int nparams

dest^.tagx:=ti32
dest^.value:=0

oldstopped:=stopped		!not really need, as it can be assumed stopped=0
oldpcptr:=pcptr
oldsptr:=sptr
oldframeptr:=frameptr

(--sptr)^.tagx:=999				!put in marker (this location might be checked later)

if b and b^.tag then			!must stack in reverse order: (b,a) or (a)
	nparams:=2
	(--sptr)^:=b^
	(--sptr)^:=a^
elsif a and a^.tag then
	nparams:=1
	(--sptr)^:=a^
else
	nparams:=0
fi
(--sptr)^.tagx:=tretaddr

sptr^.uret.retaddr:=stopseq

sptr^.uret.frameptr_low:=int32@(frameptr)
sptr^.uret.stackadj:=nparams*varsize
frameptr:=cast(sptr)
pcptr:=fnptr

disploop()

!stack will either point to a stop-value, with a retaddr before it,
!or to the first param (or to the proc return value).
if (sptr+1)^.tag=tretaddr then		!probably stop used
	CPL "RUNPROC: STOP used"
	dest^:=sptr^
else								!assume normal return used
++SPTR
	dest^:=sptr^					!pick up return value

	if dest^.tag=tvoid then		!no value; return 0
		dest^.tagx:=ti32
		dest^.value:=0
	fi
fi

pcptr:=oldpcptr
stopped:=oldstopped

!NOTE: could do with freeing items on the stack between oldsptr and current sptr
sptr:=oldsptr			!need to reset these, as stop could have been executed anywhere
frameptr:=oldframeptr	! and these could have arbitrary values
stopped:=oldstopped
end

proc allocatestrings=
!for bc-mode only, take string table, and allocate corresponding
!set of strings in stringobjtable

!temporary version: s is a zero-terminated string encountered in
!the pc-code. Convert to an object reference to a immutable string object
!(Later, try and store the string lengths, and combine at least some common
!strings such as "" and " ", although "" should generate a special push op)
object p
int i
ref char s

stringobjtable:=ref void(zalloctable(nstrings,objrec.bytes))

for i:=1 to nstrings do
	s:=stringtable^[i]

	p:=&stringobjtable^[i]
	p^.refcount:=5
	p^.ustr.strptr:=s				!does not reallocate to heap
	p^.ustr.length:=stringlentable^[i]
	p^.ustr.objtype:=extslice_obj
od

end

global function compileq(ichar qfilename)ichar pcfilename=
!compile to .pc file; return name of new file
	[300]char cmdstr
	int status

ABORTPROGRAM("PCI:COMPILEQ NEEDS REVISING")
RETURN ""

!	sprintf(&.cmdstr, "qc -ext %s",qfilename)
!CPL &.CMDSTR
!
!	status:=system(&.cmdstr)
!	if status<>0 then
!		return nil
!	fi
!
!	return pcm_copyheapstring(changeext(qfilename,"pc"))
end

function loadpcfile(ref byte s)int=
!s points to memory block containing the pc-file data; it should not be nil
!process the pc-file and up all the tables necessary: modules, pcdata, strings etc
!return 1/0 status

[100]char modulename
[100]char name
ichar str,str2
modulerec m
int i,j,a,b,dir,symtype,x,id,t,modno,n,cmd,recordtype,length
ref strec d
ref [0:]word16 linetable
ref intpc pccode
real xvalue

!set up special module to represent the whole program
pcm_clearmem(&moduletable[0],modulerec.bytes)
moduletable[0].name:="PROGRAM"
moduletable[0].filename:="<->"
moduletable[0].sourcecode:="<program>"
moduletable[0].sourcelen:=strlen(moduletable[0].sourcecode)

!CPL "READFILE"
!T:=CLOCK()
!INT U
!if packfileptr=nil then				!regular file from disk
!	s:=readfile(filespec)
!else								!pack added to .exe
!	s:=packfileptr
!fi	

!U:=CLOCK();
!CPL "DONE READING",U-T; T:=U

!if s=nil then
!CPL =s,filespec
!	loaderror("Can't load pc file:",filespec)
!	return 0
!fi

a:=s++^						!read PCL
b:=s++^
if a<>'P' or b<>'C' then
	cpl "PC: bad sig"
	return 0
fi
s+:=2						!skip etx and 0

do
	dir:=readzint(&s)
	switch dir
	when kkpclversion then
		strpclversion:=readzstring(&s,nil)

	when kkmoduletable then
		nmodules:=readzint(&s)
		for i:=1 to nmodules do
			memset(&m,0,m.bytes)
			m.name:=pcm_copyheapstring(readzstring(&s,nil))
!			m.filename:="<no file>"
			m.filename:=m.name
			m.sourcecode:="<no source>"
			m.sourcelen:=0
			moduletable[i]:=m
		od

	when kkdlltable then
		ndlltable:=readzint(&s)
		for i:=1 to ndlltable do
			dlltable[i]:=pcm_copyheapstring(readzstring(&s,nil))
			dllinsttable[i]:=0
		od

	when kkdllproctable then
		ndllproctable:=readzint(&s)
		for i:=1 to ndllproctable do
			dllproctable[i].name:=pcm_copyheapstring(readzstring(&s,nil))
			dllproctable[i].dllindex:=readzint(&s)
		od

	when kkapplproctable then
		napplproctable:=readzint(&s)
!CPL =NAPPLPROCTABLE
		for i:=1 to napplproctable do
			applproctable[i].name:=pcm_copyheapstring(readzstring(&s,nil))
!CPL "JUST READ APPL PROC",APPLPROCTABLE[I].NAME
		od

	when kksymboltable then
		nsymbols:=readzint(&s)
		pcsymboltable:=zalloctable(nsymbols,strec.bytes)

		for i:=1 to nsymbols do
			symtype:=readzint(&s)			!'P' etc but stored as int
			case symtype
			when 'P' then id:=procid
			when 'S' then id:=staticid
			when 'M' then id:=moduleid
			when 'T' then id:=typeid
			esac
			str:=pcm_copyheapstring(readzstring(&s,nil))
			x:=readzint(&s)				!d is owner index
			a:=readzint(&s)				!general params
			b:=readzint(&s)				!general params
			d:=createstentry(i,str,x,id)
			d^.index:=a
			case id
			when moduleid then
				d^.ax_moduleno:=a
			when procid then
				str:=readzstring(&s,nil)
				if str^ then
					d^.metadata:=pcm_copyheapstring(str)
				fi
			esac
		od

	when kktypetable then
		n:=readzint(&s)
		ntypes:=n+tlast-1
		for i:=1 to n do
			t:=readzint(&s)
			ttname[t]:=pcm_copyheapstring(readzstring(&s,nil))
			ttnamedef[t]:=&pcsymboltable^[readzint(&s)]
			ttbasetype[t]:=readzint(&s)
			tttarget[t]:=readzint(&s)
			ttlower[t]:=readzint(&s)
			ttlength[t]:=readzint(&s)
			ttsize[t]:=readzint(&s)
		od

	when kkgenfieldnames then
		ngenfieldnames:=readzint(&s)
		for i:=1 to ngenfieldnames do
			genfieldnames[i].name:=pcm_copyheapstring(readzstring(&s,nil))
			genfieldnames[i].dataindex:=readzint(&s)
			genfieldnames[i].datalength:=readzint(&s)
		od

	when kkgenfielddata then
		ngenfielddata:=readzint(&s)
		if ngenfielddata>maxgenfields then
			pcs.loaderror("Too many genfields")
		fi
		for i:=1 to ngenfielddata do
			genfielddata[i].fieldindex:=readzint(&s)
			genfielddata[i].recordtype:=readzint(&s)
			genfielddata[i].fieldtype:=readzint(&s)
			a:=readzint(&s)
			if genfielddata[i].fieldtype=trefproc then
				genfieldpcaddress[i]:=getprocaddr(a)
			else
				genfielddata[i].offset:=a
			fi
		od

	when kknewstringtable then
		nstrings:=readzint(&s)
		stringtable:=alloctable(nstrings,ichar.bytes)
		stringlentable:=alloctable(nstrings,int.bytes)

		for i:=1 to nstrings do
			length:=readzint(&s)
			str:=cast(readzblock(&s,length))
			stringlentable^[i]:=length
			str2:=pcm_alloc(length+1)
			memcpy(str2,str,length)
			(str2+length)^:=0				!keep zero-terminated for now

			stringtable^[i]:=str2

		od

	when kkstructtable then
		nstructfields:=readzint(&s)
		pcfieldtable:=zalloctable(nstructfields,strec.bytes)

		t:=0
		n:=0
		for i:=1 to nstructfields do
			recordtype:=readzint(&s)				!field owner struct type
			if recordtype<>t then					!new struct
				if t then
					ttstructfields[t]:=n
				fi
				t:=recordtype
				ttstartfield[t]:=i
				n:=0
			fi
			++n
			pcfieldtable^[i].recordtype:=recordtype
			pcfieldtable^[i].name:=pcm_copyheapstring(readzstring(&s,nil))
			pcfieldtable^[i].fieldtype:=readzint(&s)
			pcfieldtable^[i].fieldoffset:=readzint(&s)
		od
		if t then
			ttstructfields[t]:=n
		fi

	when kkpccode then
		modno:=readzint(&s)
		n:=readzint(&s)
		moduletable[modno].linetable:=linetable:=zalloctable(n,int16.bytes)

		moduletable[modno].pccode:=zalloctable(n,intpc.bytes)
		pccode:=cast(moduletable[modno].pccode)
		moduletable[modno].pcindex:=n
		moduletable[modno].npccode:=n
		i:=0
		while ++i<=n do
			linetable^[i]:=readzint(&s)
			cmd:=readzint(&s)
			pccode++^:=cmd

			for j to cmdnopnds[cmd] do
				++i
				case cmdfmt[cmd,j]
				when creal then
					xvalue:=readzreal(&s)
					pccode++^:=intpc@(xvalue)
				else
					pccode++^:=readzint(&s)
				esac
			od
		od

	when kkend then
		exit
	else
		pcs.loaderror("PCDIR?")
	endswitch
od

!CPL "DONE READING PC",CLOCK()-T

return 1
end

global proc initpcldata=
int i,j,nn
for i:=1 to klastcmd do
	nn:=0
	for j:=1 to 4 do
		if cmdfmt[i,j]=0 then exit fi
		++nn
	od
	cmdnopnds[i]:=nn
od
end

function createstentry(int index,ichar name, int owner, id)ref strec=
!create special, unstructured st record for symbols in a .bc file
ref strec p

if index then					!part of linear symbol table
	p:=&pcsymboltable^[index]
else							!ad hoc
	p:=pcm_allocz(strec.bytes)
fi

p^.name:=name
p^.nameid:=id
if owner then
	p^.owner:=&pcsymboltable^[owner]
else
	p^.owner:=stprogram
fi
return p
end

function getprocaddr(int n)ref intpc=
ref strec d,owner
int m

d:=&pcsymboltable^[n]
if d^.address=nil then
	owner:=d^.owner
	while owner^.nameid=typeid do
		owner:=owner^.owner
	od
	m:=owner^.ax_moduleno
	d^.address:=cast(&(moduletable[m].pccode^[d^.index]))
fi
return cast(d^.address)
end

global proc pcl_initusertypes =			!PCL_INITUSERTYPES
int t,sig,basesig

for t:=tlast to ntypes do
	case ttbasetype[t]
	when trecord then
		free_table[t]:=cast(&j_free_m)
		dupl_table[t]:=cast(&j_dupl_l_m_d)
		tostr_table[t]:=cast(&j_tostr_l_m)
		len_table[t]:=len_table[trecord]
		lwb_table[t]:=lwb_table[trecord]
		upb_table[t]:=upb_table[trecord]
		bounds_table[t]:=bounds_table[trecord]

		basesig:=gettypesig(trecord,tint)
		sig:=gettypesig(t,tint)
		pushix_dtable[sig]:=pushix_dtable[basesig]
		pushdotix_dtable[sig]:=pushdotix_dtable[basesig]
		pushdotixref_dtable[sig]:=pushdotixref_dtable[basesig]

	when tstruct then
		free_table[t]:=free_table[tstruct]
		dupl_table[t]:=dupl_table[tstruct]
		tostr_table[t]:=tostr_table[tstruct]

	when tarray then
		free_table[t]:=free_table[tarray]
		dupl_table[t]:=dupl_table[tarray]
		tostr_table[t]:=tostr_table[tarray]
		len_table[t]:=len_table[tarray]
		lwb_table[t]:=lwb_table[tarray]
		upb_table[t]:=upb_table[tarray]

!now need to look at dual-dispatch tables
!for arrays, copy generic handler for array[int]
		basesig:=gettypesig(tarray,tint)
		sig:=gettypesig(t,tint)
		pushix_dtable[sig]:=pushix_dtable[basesig]
		pushixref_dtable[sig]:=pushixref_dtable[basesig]
	esac

od
end

global proc setcmdparam(int index, ichar s)=
!build cmd params for pc program, or set size (usually smaller) when s is nil
	if s=nil then
		ncmdparams:=index
	elsif index<=maxcmdparam then
		cmdparamtable[index]:=pcm_copyheapstring(s)
		ncmdparams max:=index
	fi
end
=== pc_types.m 7/38 ===
global tabledata() [0:]ichar stdtypenames, [0:]int stdtypewidths =
	(tvoid=0,		$,		128),	! means variant is unassigned

	(tint,			$,		64),	! 64-bit signed int
	(tword,			$,		64),	! 64-bit unsigned int
	(treal,			$,		64),	! 64-bit float
	(trange,		$,		64),	! 32+32-bit int:int

	(tstring,		$,		0),		! 8-bit string, flex and mutable
	(twstring,		$,		0),		! 16/32-bit string, flex and mutable
	(tbignum,		$,		0),		! Arbitrary precision integer
	(trational,		$,		0),		! Rational number, made of two longints
	(tset,			$,		0),		! Pascal-like bit-set
	(tdict,			$,		0),		! Dictionary of X:Y keys and values
	(tword128,		$,		0),		! 128-bit unsigned int

	(tenum,			$,		0),		! (Used in compiler, not at runtime)
	(ttype,			$,		64),	! Represents a type-code
	(toperator,		$,		64),	! Represents an operator (as a bytecode op)
	(tsymbol,		$,		64),	! Reference to a symbol: module, proc, class, etc
	(tretaddr,		$,		0),		! Return address descriptor, only on stack 
	(texception,	$,		0),		! Exception descriptor, only on stack
	(trefproc,		$,		64),	! Pointer to Q proc
	(trefdllproc,	$,		64),	! Pointer to foreign function
	(treflabel,		$,		64),	! Pointer to label
	(tstringz,		$,		64),	! C-style zero-terminated 8-bit string within struct field

	(trefvar,		$,		64),	! Pointer/slice to Variant
	(trefpacked,	$,		0),		! Pointer/slice to Packed (uses target tag)
	(trefbit,		$,		0),		! Pointer/slice to Bits
	(trecordlink,	$,		0),		! Link to record object

	(treflist,		$,		0),		! Pointer to list slice
	(trefarray,		$,		0),		! Pointer to array slice
	(trefbits,		$,		0),		! Pointer to bits slice

	(tlist,			$,		0),		! Sequence of variants
	(tarray,		$,		0),		! Sequence of packed
	(tbits,			$,		0),		! Sequence of bits

	(trecord,		$,		0),		! Record of shorts and longs
	(tstruct,		$,		0),		! Record of packed and flat arrays/structs

	(tuser,			$,		0),		! used for anonymous user types during exportspass
	(tvariant,		$,		128),	!marks transition to packed types

	(tc8,			$,		8),		! 8-bit character
	(ti8,			$,		8),		! 8-bit signed int
	(ti16,			$,		16),	! etc 
	(ti32,			$,		32),	! 
	(ti64,			$,		64),	! 
	(tbit,			$,		1),		! 1-bit unsigned
	(tbit2,			$,		2),		! 2-bit
	(tbit4,			$,		4),		! 4-bit
	(tu8,			$,		8),		! 8-bit unsigned int etc
	(tu16,			$,		16),	! 
	(tu32,			$,		32),	! 
	(tu64,			$,		64),	! 
	(tr32,			$,		32),	! 32-bit float
	(tr64,			$,		64),	! 
	(tintm,			$,		32),	! 32 or 64-bit host signed int 
	(twordm,		$,		32),	! 32 or 64-bit host unsigned int 
	(trefm,			$,		64),	! 32 or 64-bit host pointer
!	(tarray,		$,		0),		! Flat array of packed or flat (placeholder type)
!	(tstruct,		$,		0),		! Struct of packed or flat (placeholder type)

!User-defined types go here
	(tlast,			$,		0)		! 	!

end
=== pc_decls.m 8/38 ===
import msys
import clib
import pc_types
import pq_common

global macro getopnda = (pcptr+1)^
global macro getopndb = (pcptr+2)^
global macro getopndc = (pcptr+3)^
global macro getopndd = (pcptr+4)^

global macro ttelemtype = tttarget

global macro tu1 = tbit
global macro tu2 = tbit2
global macro tu4 = tbit4

global type variant = ref varrec
global type object  = ref objrec
global type intpc	= word64

global const maxmodule=50

global tabledata() [0:]ichar usercatnames =
	(std_cat=0,	$),
	(anon_cat,	$),
	(user_cat,	$)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record fieldrec =
	ichar name
	int16 recordtype
	int16 fieldtype
	int32 fieldoffset
end

global record strec =
	ichar name
	ref strec owner
	ref strec deflist
	ref strec nextdef
	ichar metadata

	byte symbol
	byte nameid
	int16 subcode

	int16 mode
	byte ax_at					!0 or 1 if @ used (fields only)
	byte ax_moduleno

	int32 index					!needs to hold pcindex (also indices for all symbols or .bc files)

	union
		ref void address
		int32 offset
		ref intpc pcaddress
	end
end

record listrec =
	word32		refcount
	word16		tag
	struct
		byte	objtype
		byte	mutable
	end

	union
		variant		vptr
		word64		padding1
	end

	word32		length
	int32		lower

	union
		object	objptr2
		word32	allocated
		word64	padding2
	end
end

record stringrec =
	word32		refcount
	word16		tag
	struct
		byte	objtype
		byte	mutable
	end

	union
		ichar		strptr
		word64		padding1
	end

	int32		length
	int32		spare3

	union
		object	objptr2
		word32	allocated
	end
end

record recordrec =
	word32		refcount
	word16		tag
	struct
		byte	spare
		byte	mutable
	end

	union
		variant		vptr
		ref byte	ptr
		word64		padding1
	end

	word32		length		!make is easier to index like a list
	int32		lower

	int			spare2
end

record decimalrec =
	word32		refcount
	word16		tag
	byte		spare1
	byte		spare2

	union
		ref void	bnptr
		word64		padding1
	end

	int			spare3
	int			spare4
end

record setrec =
	word32		refcount
	word16		tag
	struct
		byte	spare
		byte	mutable
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	int16		elemtag

	word64		allocated64
end

record dictrec =
	word32		refcount
	word16		tag
	struct
		byte	spare
		byte	mutable
	end

	union
		variant		vptr
		word64		padding1
	end

	word32		length
	int32		lower

	union
		struct
			word32		allocated
			word32		dictitems
		end
		object			objptr2
	end
end

record arrayrec =
	word32		refcount
	word16		tag
	struct
		byte	objtype
		byte	mutable
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	int16		elemtag

	union
		object	objptr2
		word32	allocated
	end
end

record bitsrec =
	word32		refcount
	word16		tag
	struct
		byte	objtype
		byte	mutable
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	byte		elemtag
	byte		bitoffset

	union
		object	objptr2
		word64	allocated64
	end
end

record structrec =
	word32		refcount
	word16		tag
	struct
		byte	spare
		byte	mutable
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	int16		elemtag

	union
		object	objptr2
		word32	allocated
	end
end

global record objrec =				!32 bytes
	union
		struct
			word32		refcount
			word16		tag
			byte		objtype		!normal/slice/ext for string/list/array/struct
			byte		spare1

			word64		spare2
			word64		spare3
			union
				object	objptr2		!share with string/list/array/struct?
				word64	dummy4
			end
		end

		listrec				ulist
		stringrec			ustr
		recordrec			urec
		decimalrec			udec
		setrec				uset
		dictrec				udict
		arrayrec			uarray
		bitsrec				ubits
		structrec			ustruct
	end
end

!Varrec has been revised:
!	Only types with simple varrec layouts have those fields defined directly:
!		The first half only contains fields common to all objects
!		The second half only has one simple value (ie. int value or a pointer)
!			(Range currently an exception)
!		More elaborate varrec layouts require the type to have its record,
!		which is unioned with the simple fields. This then needs an extra field
!		to be specified, eg. p.uret.retaddr
!spare2 is sometimes used for extra data:
!		When list/string used as iterator (via .uiter)
!		Possibly, used to store bignum digit count (not done yet)

record exceptionrec =
	union
		struct
			word16	tag
			byte	hasref
			byte	exceptiontype
		end
	end
	struct
		int16 		frameoffset
		int16 		nexceptions
	end

	ref byte		ptr
end

record returnrec =
	union
		struct
			word16	tag
			byte	hasref
			byte	stackadj
		end
	end
	int32			frameptr_low

	ref intpc		retaddr
end

record refrec =
	union
		struct
			word16	tag
			byte	hasref
			byte	spare1
		end
	end
	struct
		word16		elemtag
		byte		bitoffset
		byte		bitlength		!for refbit/tbit: 0=1 bit, N=bitfield
	end

	union
		ref byte		ptr
		ref int64		ptr64
	end
end

record operatorrec =
	union
		struct
			word16	tag
			byte	hasref
			byte	opdims
		end
	end
	word32 			spare2

	int				opcode
end

!iterrec used only when list/string are being used as iterators
record iterrec =
	union
		struct
			word16	tag
			byte	hasref
			byte	opdims
		end
	end
	word32 			itcount

	byte			ittype
	[3]byte			spare3
end

global record varrec =
	union
		struct
			union
				struct
					word16	tag
					byte	hasref
					byte	spare1
				end
				word32		tagx
			end
			word32 			spare2
			union
				int64		value
				real64		xvalue
				word64		uvalue
				struct
					int32	range_lower			!short range
					int32	range_upper
				end
				object		objptr				!objects where hasref=1
				variant		varptr				!for refvar
				ref byte	refptr				!for refproc etc
			end
		end

		exceptionrec			uexcept
		returnrec				uret
		refrec					uref
		operatorrec				uop
		iterrec					uiter

	end
end

global record genfieldnamerec =
	ichar name					!after bc load
	int32 dataindex
	union
		int32 datalength
		int32 datalast
	end
end

global record genfielddatarec =
	int32 fieldindex
	int32 recordtype
	int32 fieldtype			!-procid, -constid, -staticid, -typeid are special codes
	union
		int32 offset			!or const value
		word32 index			!into proctable, statictable, or type code
		word32 procoffset
	end
end

global record modulerec =
	ichar name
	ichar filename
	ichar sourcecode
!	ref strec stmodule
	ref[]intpc pccode
	ref[0:]word16 linetable
	int32 sourcelen
	int32 npccode				!current allocated size of pccode
	int32 pcindex				!index of last entry in pccode
!	int32 nlines
	int32 level
	int32 exported			!imported within export/endexport
	[maxmodule]byte importmap

end

global record dllprocrec =
	ichar name
	ref proc address
	int32 dllindex
end

global record applprocrec =
	ichar name
	ref proc address
	ref procinforec info
end

!global record procinforec=
!	word16		fnindex
!	byte		rettype
!	byte		nparams
!	[12]byte	paramlist
!end

global record procrec =
	ref strec def
	ref procrec nextproc
end

global record fmtrec=
	byte	minwidth	! (0)   min field width (0 if not used or don't care)
	i8	precision	! (0)   number of decimals/significant figures/max width
	byte	base		! (10)  2,8,10,16

	char	quotechar	! (0)   0/'"'/c
	char	padchar		! (' ') ' '/'0'/c
	char	realfmt		! ('f') 'e'/'f'/'g'

	char	plus		! (0)   0/'+'
	char	sepchar		! (0)   0/','/c placed every 3 (base=10) or 4 digits
	char	lettercase	! ('A') 'A'/'a'
	char	justify		! ('R') 'L'/'R'/'C'?
	char	suffix		! (0)   0/'B'/'H'/c
	char	usigned		! (0)   0/'U' force unsigned o/p for ints (eg. for hex display)
	char	charmode	! (0)  0/'U'/'M'	o/p int as int/single char/multi-char
	char	showtype	! (0) 0/'Y'	Show type
	[2]byte	spare
end

global const int maxtype=300

global int ntypes
global [0:maxtype]int32 ttmodule		!module number
global [0:maxtype]ref strec ttnamedef
global [0:maxtype]int32 ttbasetype	!basetype
global [0:maxtype]ichar ttname 	!name of type
global [0:maxtype]int32 ttbitwidth

global [0:maxtype]int64 ttsize 		!.size in bytes
global [0:maxtype]int32 ttlower 		!.lbound (default 1 or case unused)
!global [0:maxtype]int32 ttupper 		!.ubound == ttlength-ttlower
global [0:maxtype]word32 ttlength 		!elements in array/record (actual fields) (/string
!global [0:maxtype]int32 ttnallfields 	!number of fields, including aliases
global [0:maxtype]int32 ttstartfield 		!start index in pcfieldtable^[]
global [0:maxtype]int32 ttstructfields	!entries in pcfieldtable^[]

global [0:maxtype]int32 tttarget 		!for array/ref types
global [0:maxtype]byte ttusercat

global [0:maxtype]byte typestarterset

global const hasrefmask = 0x10000		!1st bit of 3rd byte, when writing to .tagx

global tabledata() [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global const int varsize=varrec.bytes
global const int objsize=objrec.bytes

global [0..255]object chrtable		!remember single-character objects

global [0..maxmodule]modulerec moduletable
global int nmodules
global [maxmodule]ichar pendingmodules
global int npendingmodules
global int currmoduleno				!used when compiling modules
global ref modulerec currmodule

global const maxsearchdirs=6
global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0

global ref strec stprogram		!root into the symbol table

global int optflag=0		!1=stdoptimise; 0=disabled

global const maxgenfields=1000
global [maxgenfields]genfieldnamerec genfieldnames
global [maxgenfields]genfielddatarec genfielddata
global [maxgenfields]ref intpc genfieldpcaddress
global int ngenfieldnames
global int ngenfielddata

global const sourceext="q"
global const arraylbound=1

global const maxlibpaths=10
global [maxlibpaths]ichar libpaths
global int nlibpaths

!global int fverbose=0		!whether to display message for each pass
global int ftrace=0			!whether to line trace
global int fdtrace=0		!whether to line trace, enabled by $setdebug(1)
global int foptimise=0		!whether to generate optimised j-codes

global int mlineno=0		!set in pclgen dispatcher

global int exportsprepass=0		!1 for preparse scan only

global int debug=0

global int NNAMES, NCHECKS

global int FORCHECK
global int NCLASHES
global int NLOOKUPS

global int ALLNAMES
global int ALLFOUNDNAMES
global int ALLNOTFOUNDNAMES

global varrec ttdeststrv
global variant ttdeststr = &ttdeststrv

global int totalstrings=0

!Interpreter stuff

global tabledata() []ichar dispatchnames=
	(lab_dispatch,	"-lab"),
	(fn_dispatch,	"-fn"),
	(deb1_dispatch,	"-deb1"),
	(deb2_dispatch,	"-deb2"),
	(asm_dispatch,	"-asm")
end

!Interpreter run variables

!global const int stacksize=20000
global const int stacksize=65536
!global const int stacksize=1620
!global const int stacksize=600000
!global const int stacksize=6000
global const int maxdllindex=30

global variant sptr
global variant stacklimit
global ref byte frameptr
global ref intpc pcptr

global ref[0:]varrec varstack
global ref[0:]objrec		stringobjtable

global int dllindex
global int dllcallindex
global [maxdllindex]int64 dllparams
global [maxdllindex]int64 dllcallstack
global [maxdllindex]int16 dlltypes

global const maxdlllib=50
global const maxdllproc=2000

global int ndlltable
global int ndllproctable
global [maxdlllib]ichar dlltable
global [maxdlllib]word64 dllinsttable
global [maxdllproc]dllprocrec dllproctable

global const maxapplproc=500
global int napplproctable
global [maxapplproc]applprocrec applproctable

global ref clang proc fprintf_ptr
global ref clang proc fgets_ptr

global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)

global ichar pcerror_mess=nil		!custom message for pcerror()

global varrec emptystringvar

global const int maxcmdparam=32
global int ncmdparams
global [0..maxcmdparam]ichar cmdparamtable

global ref procrec proclist			!linked list of all procs
global int nproclist

!for bc file generation
global int nstrings=0
global int nsymbols=0
global int nstructfields=0

global ref[]ichar stringtable
global ref[]int stringlentable

global ref[]strec pcsymboltable			!controlled with nsymbols

global ref[]fieldrec pcfieldtable		!controlled with nstructfields

!global int runfrompc=0					!1 means run from .pc file

!for files tacked on to the end of qa files
global const int maxextra=10
global [maxextra]ichar extrafiles,extratext
global [maxextra]int extrasizes
global int nextra

global ichar err_message
global varrec err_var1, err_var2
global ref intpc err_pcptr

global ref intpc stopseq		!point to a 'stop 0' sequence
global ref intpc raiseseq		!point to a sequence of several 'raise' cmdcodes

!code for a module is assembled into pccode
global ref[]intpc pccode
global int npccode=0				!current allocated size of pccode
global int pcindex					!index of last entry in pccode
global ref[0:]word16 linetable		!copy of table from moduletable
!ref[]byte labelmap

global [0..cmdnames.upb]int cmdnopnds

global int lastticks=0

global [0:maxtype+1]ref function:ref intpc neg_table
global [0:maxtype+1]ref function:ref intpc abs_table
global [0:maxtype+1]ref function:ref intpc inot_table
global [0:maxtype+1]ref function:ref intpc istrue_table
global [0:maxtype+1]ref function:ref intpc jumpf_table
global [0:maxtype+1]ref function:ref intpc jumpt_table
global [0:maxtype+1]ref function:ref intpc len_table
global [0:maxtype+1]ref function:ref intpc lwb_table
global [0:maxtype+1]ref function:ref intpc upb_table
global [0:maxtype+1]ref function:ref intpc bounds_table
global [0:maxtype+1]ref function:ref intpc incr_table
global [0:maxtype+1]ref function:ref intpc decr_table

global [0:maxtype+1]ref function:ref intpc add_table
global [0:maxtype+1]ref function:ref intpc sub_table
global [0:maxtype+1]ref function:ref intpc mul_table
global [0:maxtype+1]ref function:ref intpc div_table
global [0:maxtype+1]ref function:ref intpc idiv_table
global [0:maxtype+1]ref function:ref intpc rem_table
global [0:maxtype+1]ref function:ref intpc iand_table
global [0:maxtype+1]ref function:ref intpc ior_table
global [0:maxtype+1]ref function:ref intpc ixor_table
global [0:maxtype+1]ref function:ref intpc shl_table
global [0:maxtype+1]ref function:ref intpc shr_table
global [0:maxtype+1]ref function:ref intpc min_table
global [0:maxtype+1]ref function:ref intpc max_table
global [0:maxtype+1]ref function:ref intpc jumpeq_table
global [0:maxtype+1]ref function:ref intpc jumpne_table
global [0:maxtype+1]ref function:ref intpc jumplt_table
global [0:maxtype+1]ref function:ref intpc jumpgt_table
global [0:maxtype+1]ref function:ref intpc jumple_table
global [0:maxtype+1]ref function:ref intpc jumpge_table
global [0:maxtype+1]ref function:ref intpc jumptesteq_table
global [0:maxtype+1]ref function:ref intpc jumptestne_table
global [0:maxtype+1]ref function:ref intpc jumpfalse_table
global [0:maxtype+1]ref function:ref intpc jumptrue_table
global [0:maxtype+1]ref function:ref intpc eq_table
global [0:maxtype+1]ref function:ref intpc lt_table
global [0:maxtype+1]ref function:ref intpc le_table
global [0:maxtype+1]ref function:ref intpc concat_table
global [0:maxtype+1]ref function:ref intpc append_table

global [0:maxtype+1]ref function:ref intpc addto_table
global [0:maxtype+1]ref function:ref intpc subto_table
global [0:maxtype+1]ref function:ref intpc multo_table
global [0:maxtype+1]ref function:ref intpc divto_table
global [0:maxtype+1]ref function:ref intpc idivto_table
global [0:maxtype+1]ref function:ref intpc iandto_table
global [0:maxtype+1]ref function:ref intpc iorto_table
global [0:maxtype+1]ref function:ref intpc ixorto_table
global [0:maxtype+1]ref function:ref intpc shlto_table
global [0:maxtype+1]ref function:ref intpc shrto_table
global [0:maxtype+1]ref function:ref intpc minto_table
global [0:maxtype+1]ref function:ref intpc maxto_table
global [0:maxtype+1]ref function:ref intpc concatto_table
global [0:maxtype+1]ref function:ref intpc appendto_table

global ref[0:maxtype+1]ref function:ref intpc opc_tableptr

global [0:maxtype+1]ref proc (variant a,b,c,d) new_table
global [0:maxtype+1]ref proc(variant a) free_table
global [0:maxtype+1]ref proc(variant a) dupl_table
!global [0:maxtype+1]ref function(object)object dupl_table
global [0:maxtype+1]ref proc(variant a,b,ref fmtrec fmt,object p) tostr_table

global const maxdualtype=maxtype
global [0:maxdualtype+1]ref function(int)ref intpc in_dtable
global [0:maxdualtype+1]ref function(int)ref intpc inrev_dtable
global [0:maxdualtype+1]ref function:ref intpc pushix_dtable
global [0:maxdualtype+1]ref function:ref intpc pushixref_dtable
global [0:maxdualtype+1]ref function:ref intpc pushdotix_dtable
global [0:maxdualtype+1]ref function:ref intpc pushdotixref_dtable

!global [0:maxdualtype+1]ref function:int mixed_dtable
global [0:maxdualtype+1]ref function:ref intpc mixed_dtable

global [0:maxdualtype+1]ref function(int)ref intpc convert_dtable

global [0:maxdualtype+1]ref function:ref intpc mulx_dtable

global [0:maxdualtype+1]ref function:ref intpc add_dtable
global [0:maxdualtype+1]ref function:ref intpc sub_dtable
global [0:maxdualtype+1]ref function:ref intpc mul_dtable
global [0:maxdualtype+1]ref function:ref intpc div_dtable
global [0:maxdualtype+1]ref function:ref intpc idiv_dtable
global [0:maxdualtype+1]ref function:ref intpc rem_dtable
global [0:maxdualtype+1]ref function:ref intpc iand_dtable
global [0:maxdualtype+1]ref function:ref intpc ior_dtable
global [0:maxdualtype+1]ref function:ref intpc ixor_dtable
global [0:maxdualtype+1]ref function:ref intpc shl_dtable
global [0:maxdualtype+1]ref function:ref intpc shr_dtable
global [0:maxdualtype+1]ref function:ref intpc min_dtable
global [0:maxdualtype+1]ref function:ref intpc max_dtable

global [0:maxdualtype+1]ref function:ref intpc jumpeq_dtable
global [0:maxdualtype+1]ref function:ref intpc jumpne_dtable
global [0:maxdualtype+1]ref function:ref intpc jumplt_dtable
global [0:maxdualtype+1]ref function:ref intpc jumple_dtable
global [0:maxdualtype+1]ref function:ref intpc jumpge_dtable
global [0:maxdualtype+1]ref function:ref intpc jumpgt_dtable

global [0:maxdualtype+1]ref function:ref intpc addto_dtable
global [0:maxdualtype+1]ref function:ref intpc subto_dtable
global [0:maxdualtype+1]ref function:ref intpc multo_dtable
global [0:maxdualtype+1]ref function:ref intpc divto_dtable
global [0:maxdualtype+1]ref function:ref intpc idivto_dtable
global [0:maxdualtype+1]ref function:ref intpc iandto_dtable
global [0:maxdualtype+1]ref function:ref intpc iorto_dtable
global [0:maxdualtype+1]ref function:ref intpc ixorto_dtable
global [0:maxdualtype+1]ref function:ref intpc shlto_dtable
global [0:maxdualtype+1]ref function:ref intpc shrto_dtable
global [0:maxdualtype+1]ref function:ref intpc minto_dtable
global [0:maxdualtype+1]ref function:ref intpc maxto_dtable

global int nexttypesig=0
global [0:256,0:256]byte sigmap			!map two types to a single tt-code
!global [65536]byte sigmaplin			!map two types to a single tt-code

global int overloadtype=0

global const word maxobjlength = 4 billion
global ichar strpclversion

global [-1000..+1000]int intcounts
!global [-256..+256]int intcounts
global int nallints
global int nsmallints

=== pq_common.m 9/38 ===
!global const compilerversion = "8.00"
global const pclversion="404"

global type qd=[4]byte

global tabledata() [0:]ichar opndnames=
	(cnone=0,	$),

	(cmemory,	$),
	(cframe,	$),
	(cproc,		$),
	(cdllproc,	$),
	(cdllvar,	$),

	(cfield,	$),
	(cgenfield,	$),

	(clabel,	$),
	(cint,		$),
	(cword,		$),
	(creal,		$),
	(crange,	$),
	(cstring,	$),
	(ctype,		$),
	(coperator,	$),
	(capplproc,	$),

	(clast,		"?")
end

!these aliases are used so that the cmdfmt table is tidier
const m = cmemory
const f = cframe
const p = cproc
const x = cdllproc
const v = cdllvar
const g = cgenfield
const l = clabel
const i = cint
const u = cword
const r = creal
const n = crange
const s = cstring
const t = ctype
const o = coperator
const a = capplproc

!Stack operands labeled X,Y,Z::
!X		X is top of the stack (1 operand)
!X,Y	Y is top of the stack (2 operands)
!X,Y,Z	Z is top of the stack (3 operands)
!suffixes a,b,c help indicate which operand goes where::
!a		always top of the second
!b		always second from the top
!c		always third from the top
!So Xb and Ya when there are two operands; Y is on top

global tabledata()  [0:]ichar cmdnames, [0:]qd cmdfmt =
	(kzero=0,		$,	qd(0,0,0,0)),
	(knop,			$,	qd(0,0,0,0)),

	(kprocstart,	$,	qd(p,i,0,0)),		!Start of function def; m is address, n is param count
	(kprocend,		$,	qd(0,0,0,0)),
	(kendmodule,	$,	qd(0,0,0,0)),		!Last 'executable' opcode

	(kpush_m,		$,	qd(m,0,0,0)),		!Push static at address m
	(kpush_f,		$,	qd(f,0,0,0)),		!Push frame/param with offset m
	(kpush_am,		$,	qd(m,0,0,0)),		!Push address of static as refvar
	(kpush_af,		$,	qd(f,0,0,0)),		!Push address of frame/param as refvar
	(kpush_ap,		$,	qd(p,0,0,0)),		!push ^proc
	(kpush_al,		$,	qd(l,0,0,0)),		!push ^label

	(kpush_ci,		$,	qd(i,0,0,0)),		!Push constant signed int
	(kpush_cw,		$,	qd(u,0,0,0)),		!Push constant unsigned int
	(kpush_cr,		$,	qd(r,0,0,0)),		!Push constant real
	(kpush_cn,		$,	qd(n,0,0,0)),		!Push range
	(kpush_cs,		$,	qd(s,0,0,0)),		!Push constant string
	(kpush_t,		$,	qd(t,0,0,0)),		!Push type constant
	(kpush_op,		$,	qd(o,i,0,0)),		!Push operator constant; i is 1 or 2 operands expected
	(kpushz,		$,	qd(t,0,0,0)),		!Push Zero(A); push a 'zero' of type A; (void, int/dint/real, range, string, set, but not list/array)
	(kpushz_void,	$,	qd(0,0,0,0)),		!Push void
	(kpushz_str,	$,	qd(0,0,0,0)),		!Push "" empty string (not writable)
	(kpushz_list,	$,	qd(0,0,0,0)),		!Push () empty list with lwb 1
	(kpushz_listl,	$,	qd(i,0,0,0)),		!Push (i:) empty with lwb i
	(kpushz_set,	$,	qd(0,0,0,0)),		!Push [] empty set
	(kpushz_arrayl,	$,	qd(t,i,0,0)),		!Push an empty [bit]array with elemtype A, and lwb B

	(kpop_m,		$,	qd(m,0,0,0)),		!Pop to A
	(kpop_f,		$,	qd(f,0,0,0)),		!
	(kstore_m,		$,	qd(m,0,0,0)),		!Store A; store Xa in A; keep on stack
	(kstore_f,		$,	qd(f,0,0,0)),		!

	(kpushptr,		$,	qd(0,0,0,0)),		!Push Xa^
	(kpopptr,		$,	qd(0,0,0,0)),		!Ya^:=Xb; then pop both
	(kstoreptr,		$,	qd(0,0,0,0)),		!Ya^:=Xb; keep Xb on stack (as Xa)

	(kzpop_m,		$,	qd(m,0,0,0)),		!Pop A; do not free A first
	(kzpop_f,		$,	qd(f,0,0,0)),		!

	(kzstore_m,		$,	qd(m,0,0,0)),		!Store A; do not free A first
	(kzstore_f,		$,	qd(f,0,0,0)),		!

	(kcopy,			$,	qd(0,0,0,0)),		!Xa:=deepcopy(Xa)
	(kswap,			$,	qd(0,0,0,0)),		!Yb^:=:Xa^; Xa^:=:A; A:=:B

	(kconvptr,		$,	qd(0,0,0,0)),		!Change refvar in X to ref

	(kjump,			$,	qd(l,0,0,0)),		!Jump to L
	(kjumpptr,		$,	qd(0,0,0,0)),		!Jump to Xa^

	(kjumptrue,		$,	qd(l,0,0,0)),		!Jump to L when Xa is true
	(kjumpfalse,	$,	qd(l,0,0,0)),		!Jump to L when Xa is false

	(kjumpdef,		$,	qd(l,0,0,0)),		!Jump to L when Xa defined (X popped)
	(kjumpvoid,		$,	qd(l,0,0,0)),		!Jump to L when Xa is void

	(kjumpeq,		$,	qd(l,0,0,0)),		!Jump to L when Xb=Ya, Xa=A, A=B; (X,Y popped)
	(kjumpne,		$,	qd(l,0,0,0)),		!Jump to L when Xb<>Ya
	(kjumplt,		$,	qd(l,0,0,0)),		!Jump to L when Xb<Ya
	(kjumple,		$,	qd(l,0,0,0)),		!Jump to L when Xb<=Ya
	(kjumpge,		$,	qd(l,0,0,0)),		!Jump to L when Xb>=Ya
	(kjumpgt,		$,	qd(l,0,0,0)),		!Jump to L when Xb>Ya

	(kjumptesteq,	$,	qd(l,0,0,0)),		!Jump to L when Xb=Ya (Ya popped), or Xa=A; int/set and int/range use 'in' to compare
	(kjumptestne,	$,	qd(l,0,0,0)),		!Jump to L when Xb<>Ya (Ya popped?)

	(kjumplabel,	$,	qd(l,0,0,0)),		!Jumptable entry
	(kjumpclabel,	$,	qd(l,i,0,0)),		!Jumptable entry with value P (a cint)

	(kswitch,		$,	qd(i,i,0,0)),		!Jumptable has n entries, ci is lower bound. Jump indexed by Xa

	(kcswitch,		$,	qd(i,i,i,0)),		!Jumptable has n (label,value) entries, plus 'else' entry. Search for Xa value and jump to label

	(knew,			$,	qd(0,0,0,0)),		!To (L,P); --A and jump to L if not zero
	(kto_f,			$,	qd(l,f,0,0)),		!
	(kfor_fci,		$,	qd(l,f,i,0)),		!
	(kfor_ff,		$,	qd(l,f,f,0)),		!
	(kford_fci,		$,	qd(l,f,i,0)),		!
	(kford_ff,		$,	qd(l,f,f,0)),		!

	(kcall,			$,	qd(p,i,0,0)),		!Call &A; A is cmemoryref; B is stack adjust
	(kcallptr,		$,	qd(i,i,0,0)),		!Call X^; A is no. of params supplied; B is stack adjust
	(kreturn,		$,	qd(0,0,0,0)),		!Return from function, with optional value in caller's 	$retval

	(kstartdll,		$,	qd(0,0,0,0)),		!Start sequence of pushdll cmds
	(kpushdll,		$,	qd(t,0,0,0)),		!Set X as next param which shoud be of type B
	(kcalldll,		$,	qd(x,i,t,0)),		!Call dll function m; i=0/1=c/windows; t=result type (void for procs)

	(kcallhost,		$,	qd(i,0,0,0)),		!Call fixed host function &A, with B var-params; B has +128 added when a function

	(kstackframe,	$,	qd(i,0,0,0)),		!Allocate A vars on the stack, and initialise to void

	(kfree,			$,	qd(i,0,0,0)),		!Free and pop A values on stack
	(kaddsp,		$,	qd(i,0,0,0)),		!SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

	(kstop,			$,	qd(0,0,0,0)),		!Stop program and return value X to any calling program
	(ktest,			$,	qd(i,0,0,0)),		!Various tests on X etc

	(kmakelist,		$,	qd(i,i,0,0)),		!A items on stack; make list with lwb B
	(kmakerecord,	$,	qd(i,t,0,0)),		!A items on stack; make record of type B
	(kmakearray,	$,	qd(i,i,t,t)),		!A items on stack; make array with lwb B, type C and elemtype D
	(kmakestruct,	$,	qd(i,t,0,0)),		!A items on stack; make struct with type B
	(kmakeset,		$,	qd(i,0,0,0)),		!A items on stack; make set
	(kmakerange,	$,	qd(0,0,0,0)),		!2 items on stack; make range
	(kmakedict,		$,	qd(i,0,0,0)),		!A*2 items on stack (A key:val items); make dict

	(kpushdot,		$,	qd(g,0,0,0)),		!T:=Xa.A; T:=A.B; LHS must be a record, RHS is a generic field index
	(kpushdotref,	$,	qd(g,0,0,0)),		!T:=&Xa.A; T:=&A.B

	(ksoftconv,		$,	qd(t,0,0,0)),		!T:=A(Xa); T:=B(A); Type conversion; can only be a basic conversion (usually implicit)
	(khardconv,		$,	qd(t,0,0,0)),		!T:=A(Xa); T:=B(A); Type conversion; any conversion can be done provided it's possible (usually explicit)

	(kmixed,		$,	qd(0,0,0,0)),		!++Xa
	(kincrptr,		$,	qd(0,0,0,0)),		!++Xa^
	(kincrto_m,		$,	qd(m,0,0,0)),		!++A
	(kincrto_f,		$,	qd(f,0,0,0)),		!++A
	(kloadincr,		$,	qd(0,0,0,0)),		!T:=Xa^++
	(kincrload,		$,	qd(0,0,0,0)),		!T:=--Xa^

	(kdecrptr,		$,	qd(0,0,0,0)),		!--Xa^; pop X
	(kdecrto_m,		$,	qd(m,0,0,0)),		!--A
	(kdecrto_f,		$,	qd(f,0,0,0)),		!--A
	(kloaddecr,		$,	qd(0,0,0,0)),		!T:=Xa^--
	(kdecrload,		$,	qd(0,0,0,0)),		!T:=--Xa^

	(kincr,			$,	qd(0,0,0,0)),		!T:=++T
	(kdecr,			$,	qd(0,0,0,0)),		!T:=--T

	(kneg,			$,	qd(0,0,0,0)),		!T:=-Xa; T:=-A
	(kabs,			$,	qd(0,0,0,0)),		!abs Xa
	(knot,			$,	qd(0,0,0,0)),		!not Xa
	(kinot,			$,	qd(0,0,0,0)),		!inot Xa
	(kistrue,		$,	qd(0,0,0,0)),		!istrue Xa
	(kasc,			$,	qd(0,0,0,0)),		!asc Xa
	(kchr,			$,	qd(0,0,0,0)),		!chr Xa

	(ksqrt,			$,	qd(0,0,0,0)),		!sqrt Xa
	(ksqr,			$,	qd(0,0,0,0)),		!sqr Xa
	(kcube,			$,	qd(0,0,0,0)),		!cube Xa
	(ksin,			$,	qd(0,0,0,0)),		!sin Xa
	(kcos,			$,	qd(0,0,0,0)),		!cos Xa
	(ktan,			$,	qd(0,0,0,0)),		!tan Xa
	(kasin,			$,	qd(0,0,0,0)),		!asin Xa
	(kacos,			$,	qd(0,0,0,0)),		!acos Xa
	(katan,			$,	qd(0,0,0,0)),		!atan Xa
	(ksign,			$,	qd(0,0,0,0)),		!sign Xa
	(kln,			$,	qd(0,0,0,0)),		!ln Xa
	(klog,			$,	qd(0,0,0,0)),		!log Xa
	(klg,			$,	qd(0,0,0,0)),		!lg Xa
	(kexp,			$,	qd(0,0,0,0)),		!exp Xa
	(kround,		$,	qd(0,0,0,0)),		!round Xa
	(kfloor,		$,	qd(0,0,0,0)),		!floor Xa
	(kceil,			$,	qd(0,0,0,0)),		!ceil Xa
	(kfract,		$,	qd(0,0,0,0)),		!fract Xa

	(knegto,		$,	qd(0,0,0,0)),		!-:=Xa^; -:=A
	(kabsto,		$,	qd(0,0,0,0)),		!abs:=^Xa; pop Xa
	(knotto,		$,	qd(0,0,0,0)),		!not:=Xa^; pop Xa
	(kinotto,		$,	qd(0,0,0,0)),		!inot:=Xa^; pop Xa

	(klen,			$,	qd(0,0,0,0)),		!T:=Xa.len
	(klwb,			$,	qd(0,0,0,0)),		!Xa.lwb
	(kupb,			$,	qd(0,0,0,0)),		!Xa.upb
	(kbounds,		$,	qd(0,0,0,0)),		!Xa.bounds
	(kbits,			$,	qd(0,0,0,0)),		!Xa.bits
	(kbytes,		$,	qd(0,0,0,0)),		!Xa.bytes
	(ktype,			$,	qd(0,0,0,0)),		!Xa.type
	(kelemtype,		$,	qd(0,0,0,0)),		!Xa.elemtype
	(kbasetype,		$,	qd(0,0,0,0)),		!Xa.basetype
	(kminval,		$,	qd(0,0,0,0)),		!Xa.minval
	(kmaxval,		$,	qd(0,0,0,0)),		!Xa.maxval
	(kisint,		$,	qd(0,0,0,0)),		!Xa.isint
	(kisreal,		$,	qd(0,0,0,0)),		!Xa.isreal
	(kisstring,		$,	qd(0,0,0,0)),		!Xa.isstring
	(kisrange,		$,	qd(0,0,0,0)),		!Xa.isrange
	(kisnumber,		$,	qd(0,0,0,0)),		!Xa.isnumber
	(kisarray,		$,	qd(0,0,0,0)),		!Xa.isarray
	(kisrecord,		$,	qd(0,0,0,0)),		!Xa.isrecord
	(kispointer,	$,	qd(0,0,0,0)),		!Xa.ispointer
	(kismutable,	$,	qd(0,0,0,0)),		!Xa.ismutable
	(kisset,		$,	qd(0,0,0,0)),		!Xa.isset
	(kisvoid,		$,	qd(0,0,0,0)),		!Xa.isvoid
	(kisdef,		$,	qd(0,0,0,0)),		!Xa.isdef
	(ktostr,		$,	qd(0,0,0,0)),		!Xa.isnoneComment (may be suppressed from pcb file)
	(kisequal,		$,	qd(0,0,0,0)),		!Xb==Ya

	(kadd,			$,	qd(0,0,0,0)),		!T:=Xb+Ya
	(ksub,			$,	qd(0,0,0,0)),		!Xb-Ya
	(kmul,			$,	qd(0,0,0,0)),		!Xb*Ya
	(kdiv,			$,	qd(0,0,0,0)),		!Xb/Ya
	(kidiv,			$,	qd(0,0,0,0)),		!Xb%Ya
	(krem,			$,	qd(0,0,0,0)),		!Xb rem Ya
	(kdivrem,		$,	qd(0,0,0,0)),		!Xb divrem Ya
	(kiand,			$,	qd(0,0,0,0)),		!Xb iand Ya
	(kior,			$,	qd(0,0,0,0)),		!Xb ior Ya
	(kixor,			$,	qd(0,0,0,0)),		!Xb ixor Ya
	(kshl,			$,	qd(0,0,0,0)),		!Xb shl Ya
	(kshr,			$,	qd(0,0,0,0)),		!Xb shr Ya
	(kin,			$,	qd(0,0,0,0)),		!Xb in Ya
	(knotin,		$,	qd(0,0,0,0)),		!Xb notin Ya
	(kinrev,		$,	qd(0,0,0,0)),		!Xb inrev Ya
	(keq,			$,	qd(0,0,0,0)),		!Xb=Ya
	(kne,			$,	qd(0,0,0,0)),		!Xb<>Ya
	(klt,			$,	qd(0,0,0,0)),		!Xb<Ya
	(kle,			$,	qd(0,0,0,0)),		!Xb<=Ya
	(kge,			$,	qd(0,0,0,0)),		!Xb>=Ya
	(kgt,			$,	qd(0,0,0,0)),		!Xb>Ya
	(kmin,			$,	qd(0,0,0,0)),		!Xb min Ya
	(kmax,			$,	qd(0,0,0,0)),		!Xb max Ya
	(kconcat,		$,	qd(0,0,0,0)),		!Xb concat Ya
	(kappend,		$,	qd(0,0,0,0)),		!Xb append Ya

	(kpower,		$,	qd(0,0,0,0)),		!Xb power Ya
	(katan2,		$,	qd(0,0,0,0)),		!Xb atan2 Ya

	(kaddto,		$,	qd(0,0,0,0)),		!Xb^+:=Y or Xa^+:=A or A+:=B
	(ksubto,		$,	qd(0,0,0,0)),		!Xb^-:=Ya
	(kmulto,		$,	qd(0,0,0,0)),		!Xb^*:=Ya
	(kdivto,		$,	qd(0,0,0,0)),		!Xb^/:=Ya
	(kidivto,		$,	qd(0,0,0,0)),		!Xb^%:=Ya

	(kiandto,		$,	qd(0,0,0,0)),		!Xb^ iand:=Ya
	(kiorto,		$,	qd(0,0,0,0)),		!Xb^ ior:=Ya
	(kixorto,		$,	qd(0,0,0,0)),		!Xb^ ixor:=Ya
	(kshlto,		$,	qd(0,0,0,0)),		!Xb^ shl:=Ya
	(kshrto,		$,	qd(0,0,0,0)),		!Xb^ shr:=Ya
	(kminto,		$,	qd(0,0,0,0)),		!Xb^ min:=Ya
	(kmaxto,		$,	qd(0,0,0,0)),		!Xb^ max:=Ya
	(kconcatto,		$,	qd(0,0,0,0)),		!Xb^ concat:=Ya
	(kappendto,		$,	qd(0,0,0,0)),		!Xb^ concat:=Ya

	(kpushix,		$,	qd(0,0,0,0)),		!T:=Xb[Ya]
	(kpushdotix,	$,	qd(0,0,0,0)),		!T:=Xb.[Ya]
	(kpushkeyix,	$,	qd(0,0,0,0)),		!T:=Xb{Ya}
	(kpushkeyixd,	$,	qd(0,0,0,0)),		!T:=Xc{Yb,Za} Za is default value

	(kpushixref,	$,	qd(0,0,0,0)),		!^Xb[Ya]
	(kpushdotixref,	$,	qd(0,0,0,0)),		!^Xb.[Ya]
	(kpushkeyixref,	$,	qd(0,0,0,0)),		!^Xb{Ya}

	(kpushbyteix,	$,	qd(t,0,0,0)),		!Xb.A[Ya]
	(kpushbyteixref,$,	qd(t,0,0,0)),		!^Xb.A[Ya]

	(kappendset,	$,	qd(0,0,0,0)),		!Xb[Ya]:=1; pop Y, keep X on stack; Xa[A]:=1

	(kpushdotm,		$,	qd(t,p,0,0)),		!A=0 (module) or owner type; B=proc; check X has type A if A is a type
	(kpushdott,		$,	qd(t,t,0,0)),		!A=owner type; B=.type; check X has type A
	(kpush_ad,		$,	qd(x,0,0,0)),		!push index of dll proc
	(kpush_try,		$,	qd(l,i,i,0)),		!Push try/except into; label/except code/no. exceptions
	(kraise,		$,	qd(0,0,0,0)),		!Raise exception Xa
	(kapplyop,		$,	qd(i,0,0,0)),		!applyop(Ya,Xb); i is 1 or 2, number of operands provided

!A few experimental main pcl codes
	(kmakeiter,		$,	qd(i,0,0,0)),		!Turn Xa inter iterate object; A=0/1=fwd/rev
	(kforall,		$,	qd(l,f,f,0)),		!A=label; B=iter var; C=primary index var
	(kforallx,		$,	qd(l,f,f,f)),		!A=label; B=iter var; C=primary index var, D=secondary indat var

	(kforeach,		$,	qd(l,f,f,0)),		!A=label; B=iter var; C=primary index var
	(kforeachx,		$,	qd(l,f,f,f)),		!A=label; B=iter var; C=primary index var, D=secondary indat var

	(kexpandrange,	$,	qd(0,0,0,0)),		!expand range value to two ints
	(kcallappl,		$,	qd(a,i,0,0)),		!Call named host function &A, with B var-params

	(klastcmd,		$,	qd(0,0,0,0))

end

global [0..klastcmd]ref void cmdmap			!map cmd index to possible fn/label address

global tabledata()	[]ichar bcdirnames =
	(kkpclversion,		$),	!s	PCL version string
	(kkmoduletable,		$),	!n	Names of all modules
	(kkdlltable,		$),	!n	DLL imported libraries
	(kkdllproctable,	$),	!n	DLL proc table
	(kksymboltable,		$),	!n	Proc, Type, Static symbols
	(kktypetable,		$),	!n	Type table (user types only)
	(kkgenfieldnames,	$),	!n	Genfield names
	(kkgenfielddata,	$),	!n	Genfield data
	(kkstringtable,		$),	!n	
	(kkstructtable,		$),	!n	Struct field table
	(kkpccode,			$),	!n	PCL bytecode data
	(kkend,				$),	!-	End of file
	(kknewstringtable,	$),	!n	Counted blocks
	(kkapplproctable,	$),	!n	Host proc table
end

global tabledata() [0:]ichar hostfnnames, [0:]int hostnparams, [0:]int hostisfn =
	(host_dummy=0,			$,	0,	0),

	(host_startprint,		$,	1,	0),	!startprint(x)	Set o/p dev for following print items
	(host_startprintcon,	$,	0,	0),	!startprintcon()	Set console dev for following print items
	(host_strstartprint,	$,	0,	0),	!strstartprint()	Set o/p dev for internal string
	(host_setformat,		$,	1,	0),	!setformat(x)	Set up format string for following print items up to str/endprint
	(host_endprint,			$,	0,	0),	!endprint()	Restore o/p dev
	(host_strendprint,		$,	0,	1),	!strendprint()	Restore o/p dev, and return result as string
	(host_print,			$,	2,	0),		!print(x,[y])	Print x, using default format code or y

	(host_dprint,			$,	2,	0),	!dprint(x,[y])	As print, but with extra debug stuff
	(host_println,			$,	0,	0),	!println()	Print newline
	(host_printnogap,		$,	0,	0),	!printnogap()	Suppress any gap before next print item

	(host_readln,			$,	1,	0),	!sreadln(x)	Read line from console or device x, into read buffer
	(host_sreadln,			$,	1,	1),	!sreadln(x)	Read line from console or device x, into read buffer
	(host_sread,			$,	1,	1),	!sread([x])	Read item from read buffer, with/without format code
	(host_rereadln,			$,	0,	0),	!sread([x])	Read item from read buffer, with/without format code
	(host_reread,			$,	0,	0),	!sread([x])	Read item from read buffer, with/without format code

	(host_strtoval,			$,	2,	1),	!
	(host_tostr,			$,	2,	1),	!

	(host_leftstr,			$,	3,	1),
	(host_rightstr,			$,	3,	1),
	(host_convlc,			$,	2,	1),
	(host_convuc,			$,	2,	1),
	(host_iconvlc,			$,	2,	0),		!&
	(host_iconvuc,			$,	2,	0),		!&

	(host_stop,				$,	0,	0),	!stop(x)	Stop execution
	(host_stopx,			$,	1,	0),	!stopx(x)	Stop, with given return value
	(host_ismain,			$,	1,	1),	!ismain(x)	Return 1 when module name x is main module
	(host_waitkey,			$,	0,	1),
	(host_testkey,			$,	0,	1),
	(host_execwait,			$,	3,	1),
	(host_execcmd,			$,	3,	1),
	(host_shellexec,		$,	2,	1),
	(host_system,			$,	1,	1),

	(host_makestr,			$,	2,	1),
	(host_makestrslice,		$,	2,	1),
	(host_makeref,			$,	2,	1),

	(host_new,				$,	4,	1),
	(host_newheap,			$,	4,	1),
	(host_readlines,		$,	1,	1),
	(host_heapvar,			$,	1,	1),
	(host_dictitems,		$,	1,	1),
	(host_freeheap,			$,	1,	0),
	(host_setoverload,		$,	3,	0),

	(host_getcmdparam,		$,	1,	1),
	(host_gethostname,		$,	0,	1),

	(host_setpcerror,		$,	1,	0),
	(host_setdebug,			$,	1,	0),
	(host_test,				$,	2,	1),

	(host_ticks,			$,	0,	1),
	(host_sleep,			$,	1,	0),
	(host_random,			$,	1,	1),
	(host_findmetafunction,	$,	1,	1),
	(host_gethash,			$,	1,	1),
	(host_getos,			$,	0,	1),
	(host_gethostsize,		$,	0,	1),
	(host_iswindows,		$,	0,	1),
	(host_setmesshandler,	$,	1,	0),
	(host_setfprintf,		$,	2,	0),

	(host_loadpcl,			$,	2,	1),
	(host_runpcl,			$,	2,	1),
	(host_runtask,			$,	2,	1),
	(host_callext,			$,	3,	0),
	(host_pcldata,			$,	2,	1),
	(host_getcstring,		$,	1,	1),
	(host_getparam,			$,	1,	1),
	(host_clearlist,		$,	1,	0),
	(host_makelink,			$,	1,	1),
	(host_allparams,		$,	1,	1),
	(host_stackvars,		$,	0,	1),
	(host_makeempty,		$,	1,	1),
	(host_errorinfo,		$,	1,	1),

	(host_last,				$,	0,	0)
end

global tabledata() [0:]ichar namenames =
	(nullid=0,		$),		!Not assigned (sometimes converted to genfieldid)
	(programid,		$),		!Main root
	(moduleid,		$),		!Current or imported module
!	(extmoduleid,	$),		!Imported module
	(dllmoduleid,	$),		!
	(typeid,		$),		!Type name in type, proc or module
!	(classid,		$),		!Class name
	(procid,		$),		!Proc/method/function/op name
	(dllprocid,		$),		!Dll Proc/function name
	(dllvarid,		$),		!Dll variable name
	(applprocid,	$),		!Host proc/function name
	(constid,		$),		!Named constant in type, proc or module
	(staticid,		$),		!Static in type or proc or module
	(frameid,		$),		!Local var
	(paramid,		$),		!Local param
	(fieldid,		$),		!Field of Record or Class
	(genfieldid,	$),		!Generic Field of Record or Class
	(enumid,		$),		!Enum name, part of enum type only
	(labelid,		$),		!Label name in proc only
	(blockid,		$),		!Codeblock label name in proc only
	(aliasid,		$),		!Alias to another name
	(linkid,		$)		!Name in class defined in a base class
end

global tabledata() []ichar errornames =
	(pc_error,			$),
	(user_error,		$),
	(type_error,		$),
	(mixedtype_error,	$),
	(divide_error,		$),
	(stopmodule_error,	$),
	(bounds_error,		$)
end

=== pc_support.m 10/38 ===
import msys
import clib
import mlib
import oslib

import pc_types
import pc_decls
import pq_common
import pc_misc

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

!These z-prefix-codes are used purely when writing/reading the pc-file which stores packed
!data. For example, an operand marked as cint (integer) for a specific operand may be
!stored as (uncoded - 0..239 only, zint2, zint4 or zint8)
!For 0..239, that is the value that follows. For zint2, there is a zint2 byte (249 followed by
!2 bytes of data), etc.

! The following z-prefix-codes are used as the data type when the value is more
! complex than the 0..zmax values available with a single byte

const zmax=239             !maximum value in one byte

const zint240=245          !next byte is value 240 to 479
const zint480=246          !next byte is value 480 to 719
const zint720=247          !next byte is value 720 to 959

const zint1=248            !Int is neg of next byte value (mainly for small neg values)
const zint2=249            !Int in next 2 sign extended bytes
const zint4=250            !Int in next 4 bytes
const zint8=251            !Dint in next 8 bytes
const zreal4=252           !Real in next 4 bytes (add 32-bit zero lsw)
const zreal8=253           !Real in next 8 bytes
const zstring=254          !String
const zbytes=244          	!General data: count follows (in zint format) then data bytes
const zeof=255          	!eof marker

global proc prterror(ichar mess)=
println "Print error:",mess
os_getch()
stop 1
end

global function testelem(ref[0:]byte p,int n)int =		!TESTELEM
!caller must check that n is in range
return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =		!SETELEM
!CPL "SETELEM"

p^[n>>3] ior:= bytemasks[n iand 7]
end

global proc pcustype_def(ichar mess, variant x) =		!PCUSTYPE
int t

t:=x^.tag

showlinenumber()
!if pcerror_mess then
!	mess:=pcerror_mess
!fi
println "USTYPE:Type not supported: ",mess,":",ttname[t]
abortprogram("Stopping")
end

global function pcustype(ichar mess, variant x)ref intpc =		!PCUSTYPE
err_message:=mess
err_var1:=x^

err_pcptr:=pcptr
pcustype_def(mess,x)
return pcptr
end

global function pcustypet(ichar mess, int t)ref intpc =
static varrec v
v.tagx:=t
return pcustype(mess,&v)
end

global proc pcmxtypes_def(ichar mess, variant x,y) =		!PCMXTYPES
int s,t

s:=x^.tag
t:=y^.tag

showlinenumber()

println "MXTYPES:Mixed Types not supported:/",mess,"/:",ttname[s],":",ttname[t]
abortprogram("Stopping")
end

global function pcmxtypes(ichar mess, variant x,y)ref intpc =		!PCMXTYPES
err_message:=mess
err_var1:=x^
err_var2:=y^
err_pcptr:=pcptr
!CPL =err_message

pcmxtypes_def(mess,x,y)

!raise_error(mixedtype_error)
return pcptr
end

global function pcmxtypestt(ichar mess, int s,t)ref intpc=
static varrec u,v

u.tagx:=s
v.tagx:=t
return pcmxtypes(mess,&u,&v)
end

global function gettypename(int t)ichar=
return ttname[t]
end

global proc inittypetables=
int i,size,bitsize

!Initialise type tt-tables from std types first all fields initially zero

for i:=0 to tlast-1 do

	ttname[i]:=stdtypenames[i]
	ttbasetype[i]:=i

	case i
	when tintm, twordm, trefm then
		bitsize:=$targetbits
	else
		bitsize:=stdtypewidths[i]
	esac

	switch bitsize
	when 0 then
	when 1,2,4 then
		size:=1
	else
		size:=bitsize/8
	endswitch

	ttsize[i]:=size
	ttbitwidth[i]:=bitsize

	ttlower[i]:=1
od
ntypes:=tlast-1

tttarget[trefvar]:=tvariant
end

global function pcerror(ichar mess)ref intpc=
showlinenumber()
println "PCERROR:",mess
os_getch()
stop 1
return nil
end

global proc vxunimpl(ichar mess)=
showlinenumber()
println "Unimplemented VX op:",mess
os_getch()
stop 1
end

global proc pclunimpl(int cmd) =		!PCLUNIMPL
showlinenumber()
if cmd<>klastcmd then
	println "Unimplemented cmd:",cmdnames[cmd]
else
	println "J-opcode not allowed with -LAB or -FN"
fi
abortprogram("Stopping")
end

global function convCstring(ref char svalue,int length)ref char =		!CONVCSTRING
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

enum (strbufflen=2000)
static [0:strbufflen]char strbuffer1
static [0:strbufflen]char strbuffer2
static [0:strbufflen]char strbuffer3
static [0:strbufflen]char strbuffer4
static [0:strbufflen]char strbuffer5
static [0:strbufflen]char strbuffer6
static int strindex=0		!index of current buffer: cycles between 0,1,2
static [0:]ref [0:]char table=(
	&strbuffer1,&strbuffer2,&strbuffer3,
	&strbuffer4,&strbuffer5,&strbuffer6)
!	cast(strbuffer1),cast(strbuffer2),cast(strbuffer3),
!	cast(strbuffer4),cast(strbuffer5),cast(strbuffer6))
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
!(p+length)^:=0
p^[length]:=0
return cast(p)
end

global function getintvalue(variant p)int =		!GETINTVALUE
! return int value from variant, which should be a numeric type
switch p^.tag
when tint,ttype then
	return p^.value
when treal then
	return p^.xvalue
else
	pcustype("getintvalue",p)
endswitch
return 0
end

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

if x=0 then return 0 fi

int a:=1
while a<x do
	a<<:=1
od
return a
end

proc showlinenumber =		!SHOWLINENUMBER
int lineno,moduleno,count
ref intpc ptr
variant s,send

findlinenumber(pcptr,lineno,moduleno)

printlinenumber(lineno,moduleno)

!do stack trace
s:=sptr
send:=&varstack^[stacksize]
count:=0
while s<=send and count<15 do
	if s^.tag=tretaddr then
		ptr:=s^.uret.retaddr-3		!go back three to get to start of kcall/kcallptr instr
		findlinenumber(ptr,lineno,moduleno)
!		println "Called from:",lineno,moduleno
		printlinenumber(lineno,moduleno,"Called from:")
		++count
	fi
	++s
od
end

proc printlinenumber(int lineno,moduleno,ichar calledfrom="")=
println calledfrom,"LINE:",lineno,"in FILE:",moduletable[moduleno].filename
end

global proc findlinenumber(ref intpc ptr, int &lineno, &moduleno)=
int pcindex,i
modulerec m

lineno:=0
pcindex:=findpcindex(ptr,moduleno)

!CPL "FLN",=PCINDEX,=MODULENO

if pcindex then
	m:=moduletable[moduleno]
	for i:=pcindex downto 1 do
		lineno:=m.linetable^[i]
		if lineno then return fi
	od
fi
end

global function findpcindex(ref intpc ptr, int &moduleno)int=
!ptr points to one of the pccode blocks in a module
!find which one, and return the index within that block
int i,j
ref intpc p,q

for i:=0 to nmodules do
	p:=cast(moduletable[i].pccode)
	q:=p+moduletable[i].pcindex

	if ptr>=p and ptr<q then
		moduleno:=i
		return (ptr-p)+1
	fi
od
return 0
end

global proc showlinetable(ichar caption,int i)=
int j
CPL "MODULE",i,moduletable[i].pcindex,caption
!for j:=1 to moduletable[i].pcindex do
for j:=7 to 12 do
	cpl "	LINE",j,moduletable[i].linetable^[j]
od
end

global proc writezstring(filehandle f,ichar s)=
int i,n

outbyte(f,zstring)
n:=strlen(s)

to n do
	outbyte(f,s++^)
od
outbyte(f,0)
end

global proc writezint(filehandle f,int64 x)=
ref byte p

if x>=0 and x<=zmax then
!CP "Z1"
	outbyte(f,x)
elsif x>=240 and x<480 then
!CP "Z2"
	outbyte(f,zint240)
	outbyte(f,x-240)
elsif x>=480 and x<720 then
	outbyte(f,zint480)
	outbyte(f,x-480)
elsif x>=720 and x<960 then
	outbyte(f,zint720)
	outbyte(f,x-720)
elsif x>=-127 and x<0 then
	outbyte(f,zint1)
	outbyte(f,-x)
elsif x>=-32768 and x<=32767 then
!CP "Z3"
	outbyte(f,zint2)
	outword16(f,x)
elsif x>-0x8000'0000 and x<=0x7fff'ffff then
!CP "Z4"
	outbyte(f,zint4)
	outword(f,x)
else
!CP "Z8"
	p:=cast(&x)
	outbyte(f,zint8)
	to 8 do
		outbyte(f,p++^)
	od
fi
end

global proc writezint4(filehandle f,int x)=
!used when a value has to be written in a specific format
!(eg. when writing a tentative 0 to be filled in later with an unknown value)

outbyte(f,zint4)
outword(f,x)
end

global proc writezrange(filehandle f,ref byte p)=
outbyte(f,zint8)
to 8 do
	outbyte(f,p++^)
od
end

global proc writezreal(filehandle f,real x)=
ref byte p
ref int q

p:=cast(&x)
q:=cast(&x)

if q<>nil then
	outbyte(f,zreal8)
	to 8 do
		outbyte(f,p++^)
	od
else
	outbyte(f,zreal4)
	p+:=4
	to 4 do
		outbyte(f,p++^)
	od
fi
end

global proc writezeof(filehandle f)=
	outbyte(f,zeof)
end

proc zerror(ichar mess)=
println "Z error:",mess
stop 1
end

global function readzvalue(ref ref byte pp,ref int32 dest,dest2)int =		!READZVALUE
!pp points to next data in memory image of a file.
!Read next int/real/string at pp^^, and update pp^ as needed to point to next value in source.
!Store result as 32 or 64-bit int/real/string+length in dest provided.
!Return::
! 0 = eof reached
! 1 = return int value in *dest (can be interpreted as i32 or u32)
! 2 = return int64 value *dest, when used as i64 pointer (can be interpreted as i64 or u64)
! 3 = return real value in *dest (as r64 pointer)
! 4 = return string pointer in *dest, with length in *dest2. The string pointed to is in
!     the memory image of the file, and is zero-terminated
! (5 = return data pointer in *dest, and length in *dest2)
!Dest must point to a value big enough to store 8 bytes of data, unless caller is sure
!data being read will be 32 bits (code 1,4,5)

ref byte p
ref i8 sp
ref i64 destint
ref r64 destreal
ref ref void destptr
int t
u8 bb,c
i32 length,dummy

p:=pp^

bb:=p++^		!format byte, or small int
t:=1			!assume int result
if dest2=nil then
	dest2:=&dummy
fi

switch bb
when zint240 then
	dest^:=p++^ +240

when zint480 then
	dest^:=p++^ +480

when zint720 then
	dest^:=p++^ +720

when zint1 then
	dest^:=-ref i8(p)^

	++p

when zint2 then
	dest^:=ref i16(p)^
	p+:=2

when zint4 then
	dest^:=ref i32(p)^
	p+:=4

when zint8 then
	destint:=ref i64(dest)
	destint^:=ref i64(p)^

	p+:=8
	t:=2

when zreal4 then
	dest++^ := 0		!low 32 bits are zero
	dest^ := ref i32(p)^	!top 32 bits of double
	p+:=4
	t:=3

when zreal8 then
	destint:=ref i64(dest)
	destint^:=ref i64(p)^
	p+:=8
	t:=3

when zstring then
	destptr:=ref ref void(dest)
	destptr^:=cast(intpc(p))			!result is pointer to start of string
	length:=0

	repeat
		c:=p++^
		++length
	until not c
	dest2^:=--length
	t:=4

when zbytes then
	zerror("Can't deal with ZBYTES yet")
	stop 1

when zeof then
	return 0

else		!assume int 0..zmax
	dest^:=bb
endswitch

pp^:=p			!Update caller's ptr

return t
end

global function readzint(ref ref byte p)int64 =		!READZINT
!Read expected 32-bit int from memory increment caller's pointer, and return int value
!Will abort if next value is not an int a 64-bit int is not acceptable

i64 aa
int status

aa:=0
status:=readzvalue(p,ref int32(&aa),nil)
case status
when 1 then
	if int32(aa)<0 then
		aa ior:=0xFFFFFFFF00000000
	fi
when 2 then
else
	zerror("Z:Int32 Expected")
esac
return aa
!return int(aa)
end

global function readzdint(ref ref byte p)i64=		!READZDINT
i64 aa
int status

aa:=0
if (status:=readzvalue(p,ref int32(&aa),nil))<>2 then
	if status=1 then			!widen int to 64 bits
		if aa>0x7FFFFFFF then		!-ve int32
			aa ior:= 0xFFFFFFFF80000000		!sign-extend
		fi
	else
		zerror("ZformatD")
	fi
fi
return aa
end

global function readzreal(ref ref byte p)r64=		!READZREAL
r64 x
int status

if (status:=readzvalue(p,ref int32(&x),nil))<>3 then
	zerror("ZformatR")
fi
return x
end

global function readzstring(ref ref byte p,ref int ilength)ichar=		!READZSTRING
i64 aa
int32 length,status

if (status:=readzvalue(p,ref int32(&aa),&length))<>4 then
CPL =STATUS

	zerror("ZformatS")
fi
if ilength then
	ilength^:=length
fi
!return ref char(int(aa))
return ref char(aa)
end

global function readzblock(ref ref byte pp, int length)ref byte=		!READZSTRING
ref byte pdata

pdata:=pp^

pp^:=pdata+length

return pdata
end

global proc checkmt(int id)=
CPL "CHECKMT",id,":",moduletable[1].pccode
end

global function ipower(i64 a,int n)i64=		!IPOWER
!return a**b

if n<=0 then
	return 0
elsif n=0 then
	return 1
elsif n=1 then
	return a
elsif (n iand 1)=0 then
	return ipower(a*a,n/2)
else			!assume odd
	return a*ipower(a*a,(n-1)/2)
fi
end

global proc loaderror(ichar mess,mess2="")=
println "Load Error:",mess,mess2
!println "Stopping"
stop 1
end

global function gettypesig(int s,t)int=
!return typesig corresponding to types s and t
int typesig

typesig:=sigmap[s,t]

if typesig=0 then			! need to creat a new typesig
	typesig:=++nexttypesig
	sigmap[s,t]:=typesig
!	sigmaplin[s<<8+t]:=typesig
fi
return typesig
end

GLOBAL FUNCTION GETFNNAME(ref void fnaddr)ichar=
int i,n


n:=$get_nprocs()

for i to n do
	if $get_procaddr(i)=fnaddr then
!		CPL "FUNCTION:",m_getprocname(i)
		return $get_procname(i)
	fi
od
RETURN "<FUNCTION NOT FOUND>"
END

global proc junimpl(ichar s) =
[100]char mess
strcpy(&.mess,"J handler unimpl: ")
strcat(&.mess,s)
pcerror(&.mess)
end

=== pc_misc.m 11/38 ===
import clib
import mlib

import pq_common
import pc_types
import pc_decls
import pc_support
import pc_pcfns

global function raiseexception(int exceptno)ref intpc =
variant stackend,oldsptr

stackend:=&varstack^[stacksize]
oldsptr:=sptr
do
	if sptr>=stackend then
		sptr:=oldsptr
		default_exception(exceptno)
	fi
	if sptr^.tag=texception and (exceptno=0 or sptr^.uexcept.exceptiontype=exceptno) then
		exit
	fi
	pc_unshare(sptr) when sptr^.hasref
	++sptr
od

!found exception entry on stack; keep it there
frameptr:=ref byte(sptr)+sptr^.uexcept.frameoffset
return cast(sptr^.refptr)
end

global proc raise_error(int error_no)=
!exception raised internally (not in user code)
!caller may not be able to manipulate pcptr
!here, push the error number, and set pcptr to point to a
!block of several kraise opcodes, as it is not how the byte-code
!handler, when it proceeds, will step pcptr

(--sptr)^.tagx:=tint
sptr^.value:=error_no

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
	pcustype_def(err_message,&err_var1)

when mixedtype_error then
	pcptr:=err_pcptr
	pcmxtypes_def(err_message,&err_var1,&err_var2)

when divide_error then
!	println "Divide by zero"
!CPL "DIVERROR",pcptr
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
=== pc_pcfns.m 12/38 ===
import msys
import mlib
import clib
import oslib

import pc_types
import pc_decls
import pq_common
import pc_support
import pc_objlib
!import pc_objects
import pc_bignum
import pc_misc

import PC_PRINT

GLOBAL INT LOOKUPS
GLOBAL INT CLASHES
GLOBAL [65536]INT HIST

global proc pc_unshare(variant p)=
!p *must* be a reference
IF P.OBJPTR.REFCOUNT<=0 THEN
	PCERROR("UNSHARE/REF COUNT ERROR")
FI

	if --(p.objptr.refcount)=0 then
		pc_free(p)
	fi
end

!global proc pc_unsharex(variant p)=
!!called from asm when p IS a reference, and its refcount has just reached zero
!!p *must* be a reference
!	pc_free(p)
!end
!
global proc pc_free(variant p)=
!free p because has hasref, and its refcount has reached zero
	varrec v
	object pa

!CPL "PC/FREE"

!RETURN

	pa:=p.objptr
!CPL "////FREE",=PA,TTNAME[P.TAG],PA.REFCOUNT,=OBJTYPENAMES[PA.OBJTYPE],=PA.ULIST.OBJPTR2
IF PA.REFCOUNT>0 THEN
PCERROR("FREE: REFCOUNT NOT ZERO")
FI

	switch pa.objtype
	when normal_obj then
!CPL "FREETAB1"
		free_table[pa.tag]^(p)
	when slice_obj then
!CPL "FREESLICE",PA.OBJPTR2
		v.tagx:=pa.tag
!		v.objptr:=pa.ulist.objptr2
		v.objptr:=pa.objptr2
		pc_unshare(&v)
	else
!CPL "FREE OTHER"
		freeobject(pa)
	end
	p.tag:=tvoid
!P.OBJPTR:=NIL
end

global function pc_share(variant p)variant=
	++p^.objptr^.refcount
	return p
end

global proc pc_dupl(variant p)=
!force duplication of var
	varrec v

	if p^.hasref then
		v:=p^
		dupl_table[v.tag]^(p)
		pc_unshare(&v)				!original
	fi
end

global proc j_free_s(variant p) =
!T! string
	object pa
	pa:=p.objptr

	if pa.ustr.length then
		pcm_free(pa.ustr.strptr,pa.ustr.allocated)
	fi

	freeobject(pa)
end

global proc j_free_m(variant p) =
	int i,n
	object r
	variant q

	r:=p^.objptr
!CPL "FREEREC",R.REFCOUNT

	n:=ttlength[p.tag]
	q:=r.urec.vptr

	to n do
		if q.hasref then
			pc_unshare(q)
		fi
		++q
	od
	if n then
		record_free(r)
	fi

	freeobject(r)
end

global proc j_free_l_d(variant p) =
	int i,n
	object r
	variant q

	r:=p^.objptr

	n:=r^.ulist.length
	q:=r^.ulist.vptr

!CPL "FREELIST"
	to n do
		if q^.hasref then
			pc_unshare(q)
		fi
		++q
	od
	if n then
!CPL "DOING LIST_FREE"
		list_free(r)
	fi

	freeobject(r)
end

global proc j_free_k(variant p) =
object r
variant q

r:=p^.objptr

array_free(r)
freeobject(r)
end

global proc j_free_a(variant p) =
!caller should ensure this is a freeable (.copy:=cc_owner)
!empty arrays are ok
	object r

	r:=p^.objptr

	array_free(r)

	freeobject(r)
end

global proc j_free_j(variant p) =
!caller should ensure this is a freeable (.copy:=cc_owner)
!empty arrays are ok
object r
variant q

bx_free(p)
end

global proc j_free_b_e(variant p) =
!caller should ensure this is a freeable (.copy:=cc_owner)
!empty arrays are ok
object r
variant q

r:=p.objptr
bits_free(r)
freeobject(r)
end

global proc j_dupl_s(variant p) =
	int n
	object pa, newp

	pa:=p.objptr
	pc_makestring(pa.ustr.strptr,pa.ustr.length,p)

end

global proc j_dupl_l_m_d(variant p)=
!make a duplicate of object p
!return a new object containing a deep copy of p, with write access
	int i,j,n,nbytes
	variant q,r
	variant e
	object oldp
	object newp

	oldp:=p.objptr

	if oldp.refcount<0 then
	CPL "CIRC"
		pcerror("DUPL/LIST CIRC")
		RETURN
	fi

	if ttbasetype[p.tag]=trecord then
		n:=ttlength[p.tag]
		newp:=record_new(p.tag)
	else
		n:=oldp^.ulist.length
		newp:=list_new(n,oldp.ulist.lower,nil)
	fi


	p.objptr:=newp

	oldp.refcount:=-oldp.refcount

	if n then
		r:=newp.ulist.vptr
		pcm_copymem4(r,oldp.ulist.vptr,n*varsize)	!Copy top-level vars as a single block

		to n do
			if r^.hasref then
				if ttbasetype[r.tag]<>trecord then
					++r^.objptr^.refcount
					pc_dupl(r)
				else
					++r^.objptr^.refcount
				fi
			fi
			++r
		od
	fi

	oldp.refcount:=-oldp.refcount
end

global proc j_dupl_a(variant p) =

int i,j,n,nbytes
variant q,r
variant e
object oldp
object newp

oldp:=p^.objptr

n:=oldp.uarray.length

newp:=array_new(p.tag,oldp.uarray.elemtag,n,oldp.uarray.lower)

p.objptr:=newp

if n then
	pcm_copymem4(newp.uarray.ptr,oldp.uarray.ptr,n*ttsize[oldp.uarray.elemtag])
fi
end

global proc j_dupl_j(variant p) =
	bx_dupl(p)
end

global proc j_dupl_b(variant p) =

int i,j,n,nbytes
variant q,r
variant e
object oldp
object newp

oldp:=p.objptr

n:=oldp.ubits.length
newp:=bits_new(oldp.ubits.elemtag,n,oldp.ubits.lower)

p.objptr:=newp

if n then
	pcm_copymem4(newp.ubits.ptr,oldp.ubits.ptr,bits_bytesize(oldp))
fi
end

global proc j_dupl_e(variant p) =
int i,j,nbytes
int64 n
variant q,r
variant e
object oldp
object newp

oldp:=p.objptr

n:=oldp.uset.allocated64					!just duplicate all allocated bytes

newp:=set_new(oldp.uset.length,oldp.uset.lower)

!newp.length:=oldp^.length				!no. of bits not bytes

p.objptr:=newp

if n then
	pcm_copymem4(newp.uset.ptr,oldp.uset.ptr,bits_bytesize(oldp))
fi
end

global proc j_dupl_k(variant p) =
int i,j,n,nbytes
object oldp
object newp

oldp:=p^.objptr

nbytes:=ttsize[p^.tag]

newp:=struct_new(p.tag)

!newp.ustruct.length:=oldp.length				!no. of bits not bytes

p.objptr:=newp

pcm_copymem4(newp.ustruct.ptr,oldp.ustruct.ptr,nbytes)
end

global proc pc_makelist(int n, variant a,b, int lower)=
!a points to a series of n (>=1) vars, in reverse order.
!Create a list object containing these values, and store in b.
!Note that usually b corresponds to the last data element (with both data and result on a stack).

	variant p,q
	int i
	object l

	a+:=(n-1)		!point to last data elements, which is really the first...

	l:=list_new(n,lower,nil)

	l.ulist.mutable:=0

	p:=l.ulist.vptr

	q:=p+l.ulist.allocated-1	!point to last complete (unaligned) in allocation

	to n do				!copy vars one by one to the new list and duplicate each
		p^:=a--^
		pc_share(p) when p^.hasref
		++p
	od

! Set any over-allocated vars to void
	while (p<=q) do
		p^.tagx:=tvoid
		++p
	od

	objtovar(l,b)
end

global proc pc_makerecord(int n, t, variant a,b)=
!a points to a series of n (>=1) vars, in reverse order.
!Create a record object of type t, and store in b.
!Note that usually b corresponds to the last data element (with both data and result on a stack).

	variant p,q
	int i
	object r

	a+:=(n-1)		!point to last data elements, which is really the first...

	r:=record_new(t)
	p:=r.urec.vptr

	to n do				!copy vars one by one to the new list and duplicate each
		p^:=a--^
		pc_share(p) when p^.hasref
		++p
	od

	objtovar(r,b)
end

global proc pc_makearray(int n, arraytype, elemtype, lower, variant a,b)=
!a points to a series of n (>=1) vars, in reverse order.
!Create an array object containing these values, and store in b.
!Note that usually b corresponds to the list data element (with both data and result on a stack).
!note: n expected to be 1 or more (otherwise pushz_a would be used)
	ref byte p,q
	int i,esize,nbytes,basetag
	object l

	a+:=(n-1)			!point to last data elements, which is really the first...

	if elemtype=tvoid then	!use type of first element
		elemtype:=a^.tag
		basetag:=ttbasetype[elemtype]
		case basetag
		when tarray,tstruct then
		elsif basetag>tvariant then
		else
			pcerror("makearray elem")
		esac
	fi

	l:=array_new(arraytype,elemtype,n,lower)

	p:=l.uarray.ptr

	esize:=ttsize[elemtype]

	to n do		!copy vars one by one to the new list and duplicate each
		pc_storepacked(p,a,elemtype)
		p+:=esize
		--a
	od

	objtovar(l,b)
end

global proc pc_makerange(variant x,y,z)=
	if x^.tag=tint and y^.tag=tint then
		z^.tagx:=trange
		z^.range_upper:=y^.value
		z^.range_lower:=x^.value
	else
		pcmxtypes("vxmakerange",x,y)
	fi
end

global proc pc_makeset(int n, variant data,dest)=
! data points to n vars in a block (on the stack, but the caller takes care of that)
! These will be in reverse order, but it doesn't matter for sets.
! dest points to the place to put the resulting set.
! Note: dest will likely correspond to the last data element, so do not override until done.

	variant q
	ref byte p
	int top,a,b,i,j,t,size
	byte alloc
	object s
	static int count=0

!CPL "MAKESET"
!	if fdebug then CPL "MAKESET",=fdebug,++count; pcerror("XXX") fi

!First scan to work out size of set
	top:=0
	q:=data

	to n do
		switch (q^.tag)		!scan items, which should be ranges or integers
		when trange then

			a:=q^.range_lower
			b:=q^.range_upper

		when tint then
			a:=q^.value
			b:=a
		
!CPL "MAKESETINT",=A,=B

		else			!assume numeric value of some sort
			b:=a:=getintvalue(q)
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

!CPL =N
	s:=set_new(top+1,0)

!Second scan to store elements
	q:=data
	to n do
		switch q^.tag
		when trange then
			a:=q^.range_lower
			b:=q^.range_upper
			if a>b then
				t:=a; a:=b; b:=t
			fi

		when tint then
			b:=a:=q^.value

		else
			b:=a:=getintvalue(q)
		endswitch

		for j:=a to b do
			setelem(cast(s.uset.ptr),j)
		od
		++q
	od

	objtovar(s,dest)
!CPL "DONE"
end

global proc pc_makestruct(int n,t,variant a,b) =		!VX_MAKESTRUCT
!a points to a series of n (>=1) vars, in reverse order.
!Create a struct object of type t, and store in b.
!Note that usually b corresponds to the list data element (with both data and result on a stack).

	ref byte p,q
	int i,nfields,index
	object l
	ref strec d,f

!	if fdebug then CPL "MAKESTRUCT" fi

	l:=struct_new(t)

	p:=l.ustruct.ptr

!	if not runfrompc then
!		d:=ttnamedef[t]
!
!!create array of struct fields, which will be in reverse order
!		nfields:=0
!		f:=d.deflist
!		while f do
!			if f.nameid=fieldid and not f.ax_at then
!				++nfields
!				if nfields>n then
!					pcerror("Too few struct fields")
!				fi
!				pc_storepacked(p+f.offset,a,f.mode)
!				++a
!			fi
!			f:=f.nextdef
!		od
!	else
		index:=ttstartfield[t]			!into pcfieldtable[]
		nfields:=ttstructfields[t]
		if nfields<>n then
			pcerror("makestruct: wrong # fields")
		fi
		for i:=nfields downto 1 do
			pc_storepacked(p+pcfieldtable^[index+i-1].fieldoffset,a,pcfieldtable^[index+i-1].fieldtype)
			++a
		od
!	fi

	objtovar(l,b)
end

global proc pc_makedict(int n, variant a,b)=
!create a dict consisting of n key:value pairs each key is pushed before the value
!a points to a series of n*2 (>=1) vars, in reverse order.
!the elements are n key:value pairs
!Create a list object containing these values, and store in b.
!Note that usually b corresponds to the last data element (with both data and result on a stack).
!Implementation::
! * Like a list, except no lower bound is used
! * There are parallel arrays of variables, each of n elements

	varrec v
	variant p,q
	int i,m
	object l

	m:=n*2		!m is number of vars
	a+:=m-1		!point to last data element item (reall first), which is key of first pair

!	l:=list_new(max(16,nextpoweroftwo(m),1,nil)
!	l:=dict_new(max(16,nextpoweroftwo(m))
	l:=dict_new(n)

	v.tagx:=tdict+hasrefmask
	v.objptr:=l

	p:=l.udict.vptr
	q:=p+l.udict.allocated-1			!point to last complete (unaligned) in each allocation

!CPL "MAKEDICT BEFORE",V.OBJPTR, V.OBJPTR.UDICT.VPTR
	to n do		!copy vars one by one to the new list and duplicate each
		adddictitem(&v,a,a-1)
		a-:=2
	od

	b^:=v
!	objtovar(l,b)
end

global proc pc_storepacked(ref byte p,variant q,int t) =		!PC_STOREPACKED
!p points directly to a packed value of type t, which is to receive a value currently
!in variant q

	int plength,qlength
	int s,sbase,tbase
	object qa

	sbase:=ttbasetype[s:=q^.tag]		!storing coercible sbase type to fixed type tbase
	tbase:=ttbasetype[t]

	switch (sbase)
	when tint,tword then
		switch (tbase)
		when ti8,tu8 then
			(ref byte(p)^):=q^.value
			return
		when ti16,tu16 then
			(ref u16(p)^):=q^.value
			return
		when ti32,tu32 then
			(ref int32(p)^):=q^.value
			return
		when ti64,tu64,tint,tword then
			(ref i64(p)^):=q^.value
			return
		when tr32 then
			(ref r32(p)^):=q^.value
			return
		when tr64 then
			(ref r64(p)^):=q^.value
			return
		endswitch

	when treal then
		switch (tbase)
		when ti32,tu32 then
			(ref int32(p)^):=q^.xvalue
			return
		when tr32 then
		(ref r32(p)^):=q^.xvalue
			return
		when tr64 then
			(ref r64(p)^):=q^.xvalue
			return
		when ti16,tu16 then
			(ref int16(p)^):=q^.xvalue
			return
		endswitch

	when tstring then
		qa:=q^.objptr
		plength:=ttlength[t]
		qlength:=qa.ustr.length
		switch tbase
		when tstring then			!ref string assumed here to mean special 1-char string
			if t=tbase then			!if basetype, then means special 1-char string
				if qlength<>1 then
					pcerror("Str not len 1")
				fi
				(ref char(p)^):=ref char(qa.ustr.strptr)^
				return
			fi
			if qlength>plength then		!truncate
				qlength:=plength
			fi
			memcpy(p,qa.ustr.strptr,qlength)		!copy the number of chars provided
			setfslength(cast(p),plength,qlength)
			return

		when tstringz then
			if qlength>=plength then			!truncate as needed; no teminator to allow space for terminator
				memcpy(p,qa.ustr.strptr,plength)		!copy the number of chars provided
			else
				memcpy(p,qa.ustr.strptr,qlength)		!copy the number of chars provided
				(ref byte(p)+qlength)^:=0			!zero terminator
			fi

			return

		endswitch

	when tstruct then
		if s<>t then
			pcmxtypestt("spack struct",s,t)
		fi
		memcpy(p,q.objptr.ustruct.ptr,ttsize[t])
		return

	when tarray then
		if s<>t then				!not direct match: check whether compatible
!		if tbase<>tarray or q^.elemtype<>ttelemtype[t] or q^.length<>ttlength[t] then	!not compatible
				pcmxtypestt("spack array",s,t)
!		fi
		fi
		memcpy(p,q.objptr.uarray.ptr,ttsize[t])
		return

	endswitch

	pcmxtypestt("storepacked (source->dest)",s,t)
end

proc adddictitem(variant d, p, q)=
!d is a dict, p:q are akey:value pair to be added to it
	object da
	variant r

	da:=d^.objptr

	if da.udict.length=0 then				!cannot be empty
		pcerror("NULL DICT")
	fi

	r:=finddictitem(d,p,1)

	pc_unshare(r) when r^.hasref			!overwrite any existing value
	r^:=q^
	pc_share(r) when r^.hasref
end

global function gethashvalue(variant p)int=
int hsum,csum,c,n,i,result
ref char s,s0

switch p^.tag
when tstring then
	n:=p.objptr.ustr.length
	if not n then return 0 fi
	hsum:=0
	s:=p.objptr.ustr.strptr
	to n do
		c:=s++^
!		hsum:=(hsum<<3)+csum
!		hsum:=hsum<<3-hsum+csum
		hsum:=(hsum<<4-hsum) +c
	od
!	return ((c+(hsum<<3)) ixor csum ixor c) iand 0x7FFF'FFFF	!keep positive


!	result:=(hsum<<5-hsum) 
	result:=hsum<<5-hsum
!	result:=hsum

	return result iand 0x7FFF'FFFF'FFFF'FFFF		!keep positive

when tint,tword,treal,trange then
	return p^.value
else
CPL (p^.tag)
	pcustype("Can't hash:",p)
endswitch
return 0
end

global function finddictitem(variant vd, variant p,int doins)variant=
!look for key p in dict d
!when key is found:    will return a pointer to the value
!when key not found::
!   doins=1:     Will insert the key and a void value, and return a pointer to the value
!   doins=0:     Will return nil

int hash,index,size,keytag,wrapped,limit
int64 keyvalue
variant q
object pa,qa,d

retry::
d:=vd.objptr

size:=d.udict.length/2

!++LOOKUPS

index:=(gethashvalue(p) iand (size-1))		!0-based index

q:=d^.udict.vptr+index*2							!point to key of key/value pair
wrapped:=0
keytag:=p^.tag
keyvalue:=p^.value							!when int
pa:=p^.objptr								!when string

!CPL "FINDDICTITEM"

do
!IF SIZE<100 THEN CPL "	FD LOOP",=INDEX,=SIZE,=D.UDICT.DICTITEMS FI
	if q^.tag=tvoid then					!unused entry; not found
		exit

	elsif q^.tag=keytag then
		case keytag
		when tint,treal,tword,trange then
			if q.value=keyvalue then
				++q
				pc_share(q) when q^.hasref
				return q
			fi
		when tstring then
			qa:=q.objptr
			if pa.ustr.length=qa.ustr.length then	!match on length at least
				if memcmp(pa.ustr.strptr,qa.ustr.strptr,pa.ustr.length)=0 then
					++q
					pc_share(q) when q.hasref
					return q
				fi
			fi
		esac
	fi

!no match
++CLASHES
	++index
	q+:=2
	if index>=size then
		if wrapped then					!shouldn't happen if dict was properly expanded
!IF SIZE<100 THEN CPL =INDEX,=SIZE,=CLASHES,=TTNAME[Q.TAG],"HELLO2",=D.UDICT.LENGTH,
!	D.UDICT.DICTITEMS FI
!
!Q:=d^.udict.vptr
!FOR I TO SIZE DO
!	CPL I,TTNAME[Q.TAG]
!	Q+:=2
!OD


			pcerror("DICT FULL?")
		fi
!CPL "WRAPPED"
		wrapped:=1
!		index:=1
		index:=0
		q:=d.udict.vptr
	fi
od

!exit when not found
if doins then
!	limit:=size*15/16
	limit:=size*3/4
!	limit:=size*7/8
!IF SIZE<100 THEN CPL "DOINS:",=LIMIT,=SIZE,=D.UDICT.DICTITEMS FI
	if d.udict.dictitems>=limit then
		expanddict(vd)
		goto retry
	fi
	q^:=p^
	pc_share(q) when q^.hasref
	++(d.udict.dictitems)
	return q+1							!point to entry; leave value as void
else
	return nil
fi
end

proc expanddict(variant vd)=
!double the size of the dict
	int n,m,i,j,k
	object d,e
	variant p,q,r
	varrec ev

	d:=vd.objptr

	n:=d.udict.allocated			!nos of keys and values (all slots)
	m:=n/2					!number of dict slots

!CPL "EXPAND DICT BEFORE",=VD, =VD.OBJPTR, =VD.OBJPTR.UDICT.VPTR

	p:=d.udict.vptr							!old data

!double size of dict, but now with empty slots

	e:=dict_new(m*2)
	objtovar(e,&ev)

	q:=p

	for i:=1 to m do
		if q^.tag<>tvoid then
			r:=finddictitem(&ev,q,1)
			pc_unshare(q) when q^.hasref
			++q
			r^:=q++^					!transfer ownership of data
		else
			q+:=2
		fi
	od

!CPL "NOT FREEING DICT
	dict_free(d)
	vd.objptr:=e
!CPL "EXPAND DICT",=D, =D.UDICT.VPTR, =D.REFCOUNT
!CPL "EXPAND DICT AFTER ",=VD, =VD.OBJPTR, =VD.OBJPTR.UDICT.VPTR
end

proc setfslength(ref char s,int m,n) =		!SETFSLENGTH
!set up lengthcode of fixed string starting at s, of maximum length m, with actual length n
!a,b are the last two chars of the fixed string::
!a b
!0,N	Length is N
!0,0	Length is 0 (special case of 0,N)
!X,0	Length is M-1
!X,Y	Length is M
!NOTE: this only works up for m in 2..256, and the string can't contain zero bytes


if m=n then		!no length needed (x,y)
elsif n=m-1 then	!n=m-1, use (x,0)
	(s+m-1)^:=0
else			!n<=m-2, so encode length at end of string (0,0) or (0,n)
	(s+m-2)^:=0		!
	(s+m-1)^:=n		!store count n (n can be zero)
fi
end

global function getfslength(ref char s,int m)int =		!GETFSLENGTH
!s points to a packed string encoded with length at it's end. m is the max length (m>=2)
!return the actual encoded length (see setfslength for encoding scheme)
s+:=m-1			!point to last char

if (s-1)^=0 then		!(0,n) length is n
	return s^
elsif s^=0 then		!(x,0) length is m-1
	return m-1
else				!(x,y) length is m
	return m
fi
end

global proc pc_storeptr(variant p,q)=
! p points to a refvar, q is any var.
! Store q to p^
variant dest
variant pptr,qptr
varrec v
int i,n,etag
u32 ii,jj,mask
int poffset,qoffset,bitwidthx
ref byte pp,qq
int aa,bb

switch p^.tag
when trefvar then
	dest:=p^.varptr			!dest var
	pc_unshare(dest) when dest^.hasref
	pc_share(q) when q^.hasref
	dest^:=q^

when trefpacked then
	pc_storepacked(ref byte(p^.uref.ptr),q,p^.uref.elemtag)
	pc_unshare(q) when q^.hasref

when trefbit then

	pc_storebit(p^.uref.ptr,p^.uref.bitoffset,q,p^.uref.elemtag,p^.uref.bitlength)

when tlist then
	pc_popptrlist(p,q)

	pc_unshare(p) when p^.hasref
	pc_unshare(q) when q^.hasref

else
	pcustype("pc_popptr",p)
endswitch
end

global proc pc_storebit(ref byte p,int shift,variant q,int t,bitlength) =		!PC_STOREBIT
!t is tu1/tu2/tu4 store bitfield to p^ at given bit offset, from dest
!shift will be 0,1,2,3,4,5,6,7
ref word pd
byte bb
word mask1,mask2,newvalue

if q^.tag<>tint then
	pcerror("storebit not int")
fi

switch (t)
when tu1 then
	if bitlength=0 then
		p^:=(p^ iand inot(1<<shift)) ior ((q^.value iand 1)<<shift)
	else
!PCERROR("STOREBIT/BITLEN")
		pd:=cast(p)
		mask1:=0xFFFF'FFFF'FFFF'FFFE

!CPL "PD=",PD^:"Z64BS,"
!CPL =SHIFT
!CPL =BITLENGTH
		case bitlength
		when 1 then
		when 64 then
			mask1:=0
		else
			mask1<<:=bitlength-1
		esac

!		mask1 inot:=mask1
		mask1 :=inot mask1


		if shift then
			mask1<<:=shift
		fi

!CPL "     MASK1",MASK1:"Z64BS,"
		mask2:=inot mask1
!CPL "INOT MASK2",MASK2:"Z64BS,"
		newvalue:=q^.value
		if shift then
			newvalue<<:=shift
		fi
		pd^:=(pd^ iand mask2) ior (newvalue iand mask1)
	fi

when tu2 then
	p^:=(p^ iand inot(3<<shift)) ior ((q^.value iand 3)<<shift)
when tu4 then
	p^:=(p^ iand inot(15<<shift)) ior ((q^.value iand 15)<<shift)
else
	pcustypet("storebit",t)
endswitch
end

global proc pc_popptrlist(variant p, q) =
! p is a refvar pointing to a list (of lvalues).
! q shoudl be a refvar to a mult-value item (such as a list, record, range etc)
! Store each element in turn to successive destinations in the list.
! Excess elements are ignored if there are too few, void is stored.

int i,nleft,nright
varrec v
variant pdata,qdata
object pp,qq

pp:=p.objptr

nleft:=pp.ulist.length
pdata:=pp.ulist.vptr

v.tagx:=tvoid

switch ttbasetype[q.tag]
when tlist then
	qq:=q.objptr
	nright:=qq.ulist.length
dolist::
	qdata:=qq.ulist.vptr
	for i to nleft do
		if i<=nright then
			pc_storeptr(pdata,qdata++)	!still some values on rhs
		else
			pc_storeptr(pdata,&v)		!fill with void
		fi
		++pdata  
	od

when trange then			!expand to two ints
	for i to nleft do
		if i<=2 then
			v.tagx:=tint
			v.value:=(i=1|q.range_lower|q.range_upper)
			pc_storeptr(pdata,&v)
		else
			v.tagx:=tvoid
			pc_storeptr(pdata,&v)
		fi
		++pdata
	od

when trecord then
	qq:=q.objptr
	nright:=ttlength[q.tag]
	goto dolist

when tarray then
	pcerror("POPPTRLIST ARRAY")

else
	pcustype("popptrlist",q)
endswitch

end

global proc pc_loadpacked(ref void p,int t,variant dest, object ownerobj) =		!PC_LOADPACKED
! p is a direct pointer to a packed type of type t.
! Extract target and store in varrec dest, which should have been freed.
!ownerobj is nil, or points to an array obj of which an element is being accessed
!this is mainly for arrays of structs
int length
variant q,r
ref int pp
object s
ref char ss

switch (ttbasetype[t])
when ti8 then
	dest^.tagx:=tint
	dest^.value:=ref i8(p)^

when ti16 then
	dest^.tagx:=tint
	dest^.value:=ref i16(p)^

when ti32 then
	dest^.tagx:=tint
	dest^.value:=ref int32(p)^

when ti64,tint then
	dest^.tagx:=tint
	dest^.value:=ref i64(p)^

when tu8 then
	dest^.tagx:=tint
	dest^.value:=ref byte(p)^

when tu16 then
	dest^.tagx:=tint
	dest^.value:=ref u16(p)^

when tu32 then
	dest^.tagx:=tint		!BETTER I64
	dest^.value:=ref u32(p)^

when tu64 then
	dest^.tagx:=tword		!BETTER I64
	dest^.value:=ref u32(p)^

when tr64 then
	dest^.tagx:=treal
	dest^.xvalue:=ref r64(p)^

when tr32 then
	dest^.tagx:=treal
	dest^.xvalue:=ref r32(p)^

when tstring then
	dest^.tagx:=tstring ior hasrefmask
	length:=ttlength[t]
	if length>=2 then		!normal fixed string
		length:=getfslength(p,length)
	else				!assume string basetype: char target (length can be 0)
		length:=1
	fi
	s:=make_strslicexobj(p,length)
	dest^.objptr:=s

when tstringz then		!zero-terminated string
	dest^.tagx:=tstring ior hasrefmask
	ss:=p
	to ttlength[t] do
		exit when ss^=0
		++ss
	od

	s:=make_strslicexobj(p,ss-ref char(p))
	dest^.objptr:=s

when trefpacked then
	dest^.tagx:=trefpacked
	dest^.uref.ptr:=cast(ref ref int32(p)^)
	dest^.uref.elemtag:=ttelemtype[t]

when tstruct then
	s:=obj_new(t)
	s.ustruct.mutable:=1
	s.ustruct.ptr:=p
dostruct::
	dest.objptr:=s
	dest.tagx:=t ior hasrefmask
	if ownerobj then
		s.objtype:=slice_obj
		s.ustruct.objptr2:=ownerobj
		++ownerobj^.refcount
	else
		s.objtype:=extslice_obj
	fi
when tarray then
	s:=array_new(t,ttelemtype[t],ttlength[t],ttlower[t])
	s.uarray.mutable:=1
	s.uarray.lower:=ttlower[t]
	s.uarray.ptr:=p
	s.uarray.length:=ttlength[t]
	s.uarray.elemtag:=ttelemtype[t]
	goto dostruct

!when tbits then
!	dest^.tagx:=t
!	dest^.copy:=cc_copy
!	dest^.ptr:=p
!	dest^.length:=ttlength^[t]
!	dest^.elemtype:=ttelemtype^[t]
!	dest^.bitslower:=ttlower^[t]
!	dest^.bitoffset:=0
!
else
CPL =t,ttbasetype[t]
	pcmxtypestt("loadpacked",t,ttbasetype[t])
endswitch
end

global proc pc_loadbit(ref byte p,int shift,t,bitlength,variant dest) =		!PC_LOADBIT
!t is tu1/tu2/tu4 load bitfield from p^ at given bit offset, to dest
ref word pd
word mask

dest^.tagx:=tint
switch (t)
when tu1 then
	if bitlength=0 then
		dest^.value:=not not (p^ iand (1<<shift))
	else
		pd:=cast(p)
		mask:=0xFFFF'FFFF'FFFF'FFFE
CPL "PD=",PD^:"Z64BS,"
CPL =MASK
CPL =SHIFT
CPL =BITLENGTH
		case bitlength
		when 1 then
		when 64 then
			mask:=0
		else
			mask<<:=bitlength-1
		esac

CPL "MASK",MASK:"Z64BS,"
CPL "INOT MASK",INOT MASK:"Z64BS,"

		dest^.value:=(pd^>>shift) iand (inot mask)
!		dest^.value:=(pd^) iand (inot mask)
	fi

when tu2 then
	dest^.value:=(p^ iand (3<<shift))>>shift
when tu4 then
	dest^.value:=(p^ iand (15<<shift))>>shift
else
	pcustypet("loadbit",t)
endswitch

end

global proc pc_loadptr(variant x,y)=
!y:=x^

switch x^.tag
when trefvar then
	y^:=(x^.varptr)^
	if y^.hasref then
		++y^.objptr^.refcount
	fi

when trefpacked then
	pc_loadpacked(x.uref.ptr,x.uref.elemtag,y,nil)

else
	pcustype("pc_loadptr",x)
endswitch
end

global proc pc_storestring(variant p, q)=
!p is a slice which must be a string
!store q to p^; q must be a string
object pp,qq

pp:=p^.objptr
qq:=q^.objptr

if pp^.objtype=normal_obj then
	pcerror("popstr not slice")
fi
if q^.tag<>tstring then
	pcerror("popstr not str")
fi
if pp.ustr.length<>qq.ustr.length then
	pcerror("popstr diff lengths")
fi
if not pp.ustr.mutable then
	pcerror("popstr not mut")
fi
if pp.ustr.length then
	memcpy(pp.ustr.strptr,qq.ustr.strptr,pp.ustr.length)
fi
pc_unshare(p)
pc_unshare(q)
end

global proc pc_iconvert(int t, variant x)=
!x:=t(x); does in-place conversion
int s,tbase
i64 aa
varrec bn

!CPL "ICONV1",TTNAME[X^.TAG],TTNAME[T]
s:=x^.tag
if s=t and s<tlist then		!same type
	return 							!Note: heap types such as arrays must match on elemtypes too
fi
tbase:=ttbasetype[t]

x^.tag:=t			!assume works, so pre-set tag

switch ttbasetype[s]
when tint then
	switch tbase
	when tint then			!no changes needed
	when treal then
		x^.xvalue:=x^.value
	when tword then
	when tbignum then
		bx_makeint(sptr^.value,sptr)
	else
		pcustypet("conv dint=>",t)
	endswitch

when tword then
	switch tbase
	when tint then
	when tword then
	when treal then
	else
		pcustypet("conv dint=>",t);
	endswitch

when treal then
	switch (tbase)
	when tint then
		x^.value:=x^.xvalue
!	when tword then
!		x^.uvalue:=x^.xvalue
	else
		pcustypet("conv real=>",t)
	endswitch

when trefpacked,trefvar,trefbit,trefproc then
	switch tbase
	when tint,tword then
	else
		pcustypet("conv ptr=>",t)
	endswitch
when tstring then
	switch tbase
	when tbignum then
		bx_makestr(x^.objptr.ustr.strptr,x^.objptr.ustr.length,&bn)
		x^.tagx:=tstring ior hasrefmask		!set it back in order to free
		pc_unshare(x)
		x^:=bn
	when tstring then
	else
		pcustypet("string=>",t)
	endswitch

when ttype then
	if tbase<>tint then
		pcustypet("type=>",t)
	fi

when tbignum then
	switch (tbase)
	when tint then
		aa:=bx_int(x)
		x^.tagx:=tbignum ior hasrefmask		!set it back in order to free
		pc_unshare(x)
		x^.tagx:=tint
		x^.value:=aa
		x^.tagx:=t
	else
		pcustypet("bignum=>",t)
	endswitch

else
	pcmxtypestt("HARDCONV s^.t",s,t)
endswitch

end

global proc pc_iconvcase(variant a,b,int upper)=
!do in-place conversion of string in a^ to lower or upper (upper=0/1).
!a points directly to a varrec to be modified
!b is optional if supplied, gives number of chars to convert at left of string
!
int i,n
ref char s
object pa

pa:=a^.objptr

if b^.tag>tvoid then		!allow void param to be regarded as missing one
	n:=getintvalue(b)
else
	n:=pa.ustr.length			!default is the whole length of the string
fi
!CPL =n

if a.tag<>tstring then
	pcerror("convcase/notstr")
fi

if n<0 then
	pcerror("CONVCASE N<0")
fi

if n=0 then
	return
fi

if n>pa.ustr.length then
cpl =n,pa.ustr.length
	pcerror("convcase/N?")
fi
s:=pa.ustr.strptr

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

global function pc_eqstring_nf(variant x,y)int =
!return 1 if strings in x,y are equal, otherwise 0
int n
object px,py

px:=x.objptr
py:=y.objptr

n:=px.ustr.length

if n<>py.ustr.length then
	return 0					!unequal lengths
fi
if n=0 then
	return 1					!same zero length
fi

return cmpstringn(px.ustr.strptr,py.ustr.strptr,n)=0
end

global function pc_equal_nf(variant x,y,int shallow)int =
!compare two vars for equality
!return 1 (equal) or 0 (unequal)
!shallow=0 means do recursive compare on records and lists (=)
!shallow=1 means do identity compare only records and lists (==)
!in normal code, = is used, and deepcompare is 1
!but for 'in' operator, or used in case statements, then deepcompare=0
!this function does not free operands via refcounts

int xt,yt,xbase,ybase
int xval,yval,i,nbits,nbytes,n
variant p,q
object px,py

xbase:=ttbasetype[xt:=x^.tag]
ybase:=ttbasetype[yt:=y^.tag]

if ybase=tvoid then
	pcerror("pcequal/void")
fi
px:=x^.objptr
py:=y^.objptr

switch (xbase)
when tint then
	switch (ybase)
	when tint,tword then
		return (x^.value=y^.value|1|0)
	when treal then
		return (x^.value=y^.xvalue|1|0)
	endswitch

when tword then
	switch (ybase)
	when tu32 then
		return (x^.uvalue=y^.uvalue|1|0)
	endswitch

when treal then
	switch (ybase)
	when tint then
		return (x^.xvalue=y^.value|1|0)
	when treal then
		return (x^.xvalue=y^.xvalue|1|0)
	endswitch

when trange then
	if ybase=trange then			!compare all 64-bits using i64 type
		return (x^.value=y^.value|1|0)
	fi

when trefvar then
	switch (ybase)
	when trefvar,tint then
		return x^.value=y^.value
	endswitch

when trefpacked then
	switch (ybase)
	when trefpacked,tint then
		return x^.value=y^.value
	endswitch

when trefproc then
	switch (ybase)
	when trefproc,tint then
		return x^.value=y^.value
	endswitch

when tlist then
	if ybase=tlist then
		if shallow then
			return px=py
		fi

		if px.ulist.length<>py.ulist.length then
			return 0
		fi
		p:=px.ulist.vptr
		q:=py.ulist.vptr
		to px.ulist.length do			!compare all elements
			if pc_equal_nf(p++,q++,shallow)=0 then	!mismatched elements, so whole thing can't match
				return 0
			fi
		od
		return 1				!no mismatches (or both empty)
	fi

when tstring then
!++NSTRINGEQ
	switch (ybase)
	when tstring then			!ref string assumed here to mean special 1-char string
		return pc_eqstring_nf(x,y)
	endswitch

when tstruct then
	if xt<>yt then
		return 0
	fi
	return comparebytes(px.ustruct.ptr,py.ustruct.ptr,ttsize[xt])

when tset then
	if ybase<>tset then
		return 0
	fi
!compare sets is more complex one set can be bigger, provided the extra elements are zero
	if px.uset.length<>py.uset.length then
		return 0
	fi
	nbytes:=(px.uset.length-1)/64+1
	return comparebytes(px.uset.ptr,py.uset.ptr,nbytes)

when tvoid then
	pcerror("Comparing void types")
! return ybase=tvoid

when tbignum then
!pcerror("VXEQUAL/BIGINT")
	if ybase<>tbignum then
		return 0
	fi
	return bx_equal(x,y)=1

when trecord then
	if xt<>yt then
		return 0
	fi 
	if shallow then
		return px=py
	fi
	p:=px.urec.vptr
	q:=py.urec.vptr
	n:=ttlength[xt]
	to n do				!compare all elements
		if pc_equal_nf(p++,q++,shallow)=0 then	!mismatched elements, so whole thing can't match
			return 0
		fi
	od
	return 1				!no mismatches (or both empty)
when tarray then
	if xt<>yt or xbase<>ybase or px.uarray.elemtag<>py.uarray.elemtag then
		return 0
	fi
	if px.uarray.length<>py.uarray.length then
		return 0
	fi
	return comparebytes(px.uarray.ptr,py.uarray.ptr,px.uarray.length*ttsize[px.uarray.elemtag])

when ttype then
	if ybase=ttype and x^.value=y^.value then
		return 1
	fi

else
CPL "DIFF TYPES"
	return 0

endswitch

return 0
end

global function comparebytes(ref byte p,q,int n)int =		!PC_COMPAREBYTES
return memcmp(p,q,n)=0
end

global function pc_compare_nf(variant x,y)int =
!do relational compare on vars x and y
!return -1 (lt) 0 (equal) or 1 (gt)
!not-equal is not a return option
int xt,yt,xbase,ybase
int xval,yval,i,nbits,nbytes
variant p,q
object px,py

ybase:=ttbasetype[yt:=y^.tag]
xbase:=ttbasetype[xt:=x^.tag]

switch (xbase)
when tint then
	switch (ybase)
	when tint then
		return (x^.value < y^.value | -1 | (x^.value > y^.value | 1 | 0))
	when treal then
		return (x^.value < y^.xvalue | -1 | (x^.value > y^.xvalue | 1 | 0))
	else
		goto badcmp
	endswitch

when tword then

	switch (ybase)
	when tword then
		return (x^.uvalue < y^.uvalue | -1 | (x^.uvalue > y^.uvalue | 1 | 0))
	else
		goto badcmp
	endswitch

when treal then
	switch (ybase)
	when tint then
		return (x^.xvalue < y^.value | -1 | (x^.xvalue > y^.value | 1 | 0))
	when treal then
		return (x^.xvalue < y^.xvalue | -1 | (x^.xvalue > y^.xvalue | 1 | 0))
	else
		goto badcmp
	endswitch

when trefpacked then
	switch (ybase)
	when trefpacked,tint then
		return (x^.value < y^.value | -1 | (x^.value > y^.value | 1 | 0))
	else
		goto badcmp
	endswitch

when trefvar then
	switch (ybase)
	when trefvar,tint then
		return (x^.value < y^.value | -1 | (x^.value > y^.value | 1 | 0))
	else
		goto badcmp
	endswitch

when tstring then
	switch (ybase)
	when tstring then			!ref string assumed here to mean special 1-char string
		px:=x.objptr
		py:=y.objptr
		return cmpstring_len(px.ustr.strptr,py.ustr.strptr,px.ustr.length,py.ustr.length)
	else
		goto badcmp
	endswitch

when tbignum then
!pcerror("CMP BIGINT")
	if ybase=tbignum then
		return bx_cmp(x,y)
	else
		goto badcmp
	fi

else
badcmp::
	pcmxtypes("pc_compare",x,y)

endswitch

return 0
end

global function cmpstring_len(ref char s,t,int slen,tlen)int =
!compare the given strings with these lengths, and return -1,0,1

if slen=0 then
	if tlen=0 then
		return 0		!empty:empty
	else
		return -1		!empty:str
	fi
elsif tlen=0 then	!str:empty
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

global function pc_eqstring(variant x,y)int=
!version that frees x,y after use
!return 1 when strings are the same
object px,py
int res,n

res:=pc_eqstring_nf(x,y)
px:=x^.objptr
py:=y^.objptr

pc_unshare(x) when x^.hasref
pc_unshare(y) when y^.hasref
return res
end

global function pc_equal(variant x,y,int shallow=0)int=
!version that frees operands after use
!return 1 when strings are the same
int res,n

res:=pc_equal_nf(x,y,shallow)
pc_unshare(x) when x^.hasref
pc_unshare(y) when y^.hasref
return res
end

global function pc_compare(variant x,y)int=
!version that frees operands after use
!return 1 when strings are the same
int res,n

res:=pc_compare_nf(x,y)
pc_unshare(x) when x^.hasref
pc_unshare(y) when y^.hasref
return res
end

global function u8inarray(byte a,object p)int=
!look for byte value a within array
!return index of first matching value, or lowerbound-1 (ie. 0 for 1-based arrays)
int i
ref byte q

i:=p.uarray.lower
q:=p.uarray.ptr

to p.uarray.length do
	if q^=a then
		return i
	fi
	++q
	++i
od
return p.uarray.lower-1
end

global function u16inarray(word16 a,object p)int=
int i
ref word16 q

i:=p.uarray.lower
q:=cast(p.uarray.ptr)

to p.uarray.length do
	if q^=a then
		return i
	fi
	++q
	++i
od
return p.uarray.lower-1
end

global function u32inarray(word32 a,object p)int=
int i
ref word32 q

i:=p.uarray.lower
q:=cast(p.uarray.ptr)

to p.uarray.length do
	if q^=a then
		return i
	fi
	++q
	++i
od
return p.uarray.lower-1
end

global function u64inarray(word64 a,object p)int=
int i
ref word64 q

i:=p.uarray.lower
q:=cast(p.uarray.ptr)

to p.uarray.length do
	if q^=a then
		return i
	fi
	++q
	++i
od
return p.uarray.lower-1
end

global function bitinbits(byte a,object p)int=
!look for value a (1 or 0) within bit array
!return index of first matching value, or lowerbound-1 (ie. 0 for 1-based arrays)
int i,offset,mask
ref byte q

i:=p.ubits.lower
q:=p.ubits.ptr
offset:=p.ubits.bitoffset-1
mask:=1
if offset then
	mask :=mask << offset
fi

to p.ubits.length do
CPL =q^, q^ iand mask,=mask,i,=a
	if q^ iand mask then			!1 stored
		if a then return i fi
	elsif a=0 then return i
	fi
	++i
	mask<<:=1
	if mask>=256 then
		mask:=1
		++q
	fi
od
return p.ubits.lower-1
end

global function pc_strinstr(variant x,y)int =
!return start index of string x in y, or 0
int xlen,ylen,result,i,j,k
ref char sx, sy
object px,py

px:=x.objptr
py:=y.objptr

xlen:=px.ustr.length
ylen:=py.ustr.length

if xlen=0 or ylen=0 then		!at least one is empty
	return 0
fi

k:=ylen-xlen
for i:=0 to k do			!all start positions
	sx:=px.ustr.strptr
	sy:=py.ustr.strptr+i
	for j:=1 to xlen do			!all chars in y
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

global function getbitoffset(ref byte p, int offset, index, t, byte &newoffset)ref byte=
!p and offset form an existing pointer to a bit*N value, depending on t
!offset is 0 to 7, the start of the current field within the byte pointer to by p
!t is tu1, tu2 or tu4
!(Usually, these will be properly aligned, and will not cross a byte
!boundary. Pointers to bitfields of 1 to 64 bits, or to an arbitrary
!sequence of bit:N fields, are handled separately. But getbitoffset can
!still be used to process the start of such a field, with t=tu1)
!index is an 0-based offset, in terms of bit:N elements

!Add on the index offset to p/offset form a new pointer/offset.
!Returns the new byte pointer, and returns the new bitoffset via the refeference param

switch t
when tu1 then
	index+:=offset				!adjust index to be from start of byte
	p+:=index>>3				!add number of whole bytes
	newoffset:=index iand 7
when tu2 then
	index+:=offset>>1
	p+:=index>>2
	newoffset:=(index iand 3)*2
when tu4 then
	index+:=offset>>2
	p+:=index>>1
	newoffset:=(index iand 1)*4
end

return p
end

global proc pc_iappendlist(variant a,b)=
!do in-place append of b to list a
int n,lower
variant q
object p

p:=a.objptr

if p^.objtype<>normal_obj then
	pcerror("Can't extend slice")
fi

if not p.ulist.mutable then
!PCERROR("APPENDLIST/COW")
	p:=copyonwrite(p,tlist)
fi

n:=p.ulist.length+1			!new length

if n>p.ulist.allocated then		!need more space
	list_resize(p,n)
else
	p.ulist.length:=n
fi

(p.ulist.vptr+n-1)^.tagx:=tvoid		!set new element to void

a.objptr:=p

q:=p.ulist.vptr+p.ulist.length-1
if b then
	q^:=b^						!xfer reference
else
	q.tagx:=tvoid
fi
end

global proc pc_iappendarray(variant a,b)=
!do in-place append of b to array a
int n,lower
ref byte q
object p

p:=a^.objptr

if p^.objtype<>normal_obj then
	pcerror("Can't extend slice")
fi

if not p.uarray.mutable then
!PCERROR("ARRAY/APPEND/COW")
	p:=copyonwrite(p,a^.tag)
fi

n:=p.uarray.length+1			!new length

if n>p.uarray.allocated then		!need more space
	array_resize(p,n)
else
	p.uarray.length:=n
fi

a.objptr:=p

q:=p.uarray.ptr+(p.uarray.length-1)*ttsize[p.uarray.elemtag]

if b then
	pc_storepacked(cast(q),b,p.uarray.elemtag)
fi

end

global proc pc_mul_listi(variant a,b,c) =		!PC_MUL_LISTI
!a is a list, b is an int; c::=a*b
!Usually c coincides with a
variant newptr,oldptr,q
int newlength,newalloc,oldlength,k
int i,newtag,lwr
i64 dvalue
varrec d
object pa,pc

d:=a^

pa:=d.objptr

oldlength:=pa.ulist.length
newlength:=oldlength*b.value
oldptr:=pa.ulist.vptr			!point to first element of array to be duplicated

if not oldlength then		!duplicating b times has no effect (leave c=a)
	return
fi

if newlength<0 then
	pcerror("mullist 0")
elsif newlength=0 then
	pc_unshare(a) when a.hasref
	c.tagx:=tlist
	c.objptr:=emptylist
	++emptylist^.refcount
	return
fi

!check for common when of duplicating a single, simple value
!BUG HERE: with ((),)*N, then the () has a simple .copy flag, but needs
!96-bits of data to be copied. Now copying 96 bits, but really needs .copy
!flag set to cc_owner.

if oldlength=1 then
	pc:=list_new(newlength,pa.ulist.lower)
	newptr:=pc.ulist.vptr

	c.tagx:=d.tagx
	c.objptr:=pc

!Now duplicate <newlength> copies of <value>
	q:=d.objptr.ulist.vptr
	to newlength do
		newptr^:=q^
		if newptr.hasref then
			++newptr^.objptr^.refcount
			pc_dupl(newptr)
		fi
		++newptr
	od

	pc_unshare(&d) when d.hasref

else
	pcerror("MULLISTINT/COMPLEX")
fi
end

global proc pc_mul_stri(variant a,b,c) =		!PC_MUL_STRI
!a is a string, b is an int; c::=a*b
!Usually c coincides with a

int i,m,oldlen,newlen
ref char newptr,p
varrec v
object pa,s

m:=getintvalue(b)

if m<0 then
	pcerror("neg str mul")
elsif m=0 then		!result is empty str
	pc_emptystring(c)
	return
elsif m=1 then		!leave a unchanged
	if a<>c then
		c^:=a^
		pc_share(c) when c^.hasref
	fi
	return
else				!multiple non-null string by m
	pa:=a.objptr
	oldlen:=pa.ustr.length
	if oldlen then			!not empty string
		newlen:=oldlen*m

		pc_makestringn(newlen,&v)
		p:=v.objptr.ustr.strptr
		to m do
			memcpy(p,pa.ustr.strptr,oldlen)
			p+:=oldlen
		od
		pc_unshare(a)
		c^:=v
	else				!was empty string: copy to v
		c^:=a^
		pc_share(a)
		return
	fi
fi
end

global proc pc_duplvar(variant p)=
!force duplication of var
if p^.hasref then
	dupl_table[p^.tag]^(p)
fi
end

global proc pc_iconcatlist(variant a,b)=
!do in-place append of b to list a
!both a,b must be lists
!a must own its data
variant newptr,c,d
int n,alen,blen,newlen,oldbytes,newbytes
variant v
object pa,pb

pa:=a^.objptr

if not pa.ulist.mutable then
!PCERROR("CONCATLIST/COW")
	pa:=copyonwrite(pa,a^.tag)
	a.objptr:=pa
fi

pb:=b.objptr

alen:=pa.ulist.length
blen:=pb.ulist.length

if alen=0 then					!concat to empty list
	if blen then				!copy b to a (else leave a as empty)
		list_resize(pa,blen)
		d:=pa.ulist.vptr
		memcpy(d,pb.ulist.vptr,blen*varsize)
		to blen do
			pc_share(d) when d.hasref
			++d
		od
	fi
elsif blen then					!neither list is empty (else leave a unchanged)
	newlen:=alen+blen
	list_resize(pa,newlen)
	d:=pa.ulist.vptr+alen
	memcpy(d,pb.ulist.vptr,blen*varsize)
	to blen do
		pc_share(d) when d.hasref
		++d
	od
fi
end

global proc pc_iappendbits(variant a,b)=
!do in-place append of b to list a
int lower,elemtype,index
int64 n
byte bitoffset
ref byte q
object p

p:=a^.objptr

if not p.ubits.mutable then
!PCERROR("IAPPEND/BITS/COW")
	a^.objptr:=p:=copyonwrite(p,a^.tag)
fi

n:=p.ubits.length+1			!new length

if n>p.ubits.allocated64 then		!need more space
	bits_resize(p,n)
else
	p.ubits.length:=n
fi

elemtype:=p.ubits.elemtag

q:=getbitoffset(cast(p.ubits.ptr),p.ubits.bitoffset-1,p.ubits.length-1, elemtype, bitoffset)

if b then
	pc_storebit(q,bitoffset,b,elemtype,0)
fi
end

global proc pc_makestring(ichar s, int length, variant dest)=
!create a variant string from given string
!string will be copied to heap
ref char t

if s=nil then
	pc_makestringx(nil,0,0,dest)
	return
fi

if length=-1 then
	length:=strlen(s)
fi

if length=0 then
	pc_makestringx(t,0,0,dest)
else
	t:=pcm_alloc(length)

	memcpy(t,s,length)

	pc_makestringx(t,length,allocbytes,dest)
fi
end

global proc pc_makestringx(ichar s, int length,allocated, variant dest)=
!create a variant string from given string
!string is already on the heap
ref char t
object p

if length=-1 then
	length:=strlen(s)
fi

dest.tagx:=tstring+hasrefmask

dest.objptr:=p:=obj_new(tstring)

if length=0 then
	p.ustr.strptr:=nil
else
	p.ustr.strptr:=s
	p.ustr.length:=length
	p.ustr.allocated:=allocated
fi
p.ustr.mutable:=1

end

global proc pc_makestringn(int length, variant dest)=
!create an empty, uninitialised string of given length
object p

dest.tagx:=tstring+hasrefmask
dest.objptr:=p:=obj_new(tstring)

if length>maxobjlength then
	pcerror("String*n too long")
fi

p.ustr.strptr:=pcm_alloc(length)
p.ustr.mutable:=1
p.ustr.length:=length
p.ustr.allocated:=allocbytes
end

global proc pc_emptystring(variant dest)=
dest.tagx:=tstring ior hasrefmask
dest.objptr:=emptystring
++emptystring^.refcount
end

global proc pc_makechar(int ch,variant dest)=
varrec v

[10]char str
object p

dest.tagx:=tstring ior hasrefmask

p:=chrtable[ch]
if p=nil then			!create single-char constant
	str[1]:=ch
	str[2]:=0
	pc_makestring(&.str,1,&v)
	p:=v.objptr
	p.ustr.mutable:=0
	chrtable[ch]:=p
fi
++p^.refcount
dest.objptr:=p
end

=== pc_objlib.m 13/38 ===
import clib
import msys
import mlib
import oslib

import pc_decls
import pc_types
import pc_pcfns
import pc_support
!import pc_tables
!import pc_lists

global object zeroobj
global object emptylist
global object emptystring
global object emptyset
!global objrec voidobj

proc $init=
	zeroobj:=pcm_allocz(objrec.bytes)
	zeroobj.refcount:=1

	emptylist:=obj_new(tlist)
	emptylist.ulist.lower:=1
	emptylist.objtype:=normal_obj

	emptystring:=obj_new(tstring)
	emptystring.objtype:=normal_obj

	emptyset:=obj_new(tset)
	emptyset.objtype:=normal_obj

!	voidobj:=zeroobj
!	voidobj.refcount:=0x8000'0000		!void calls to free

end

global function obj_new(int tag)object p=
!create new object descriptor, which all fields set to zero
!except refcount=1

!CPL "OBJNEW1"
	p:=pcm_alloc32()
	p^:=zeroobj^			!includes refcount=1
	p.tag:=tag
!
	return p
end

global proc freeobject(object p)=
	pcm_free32(p)
end

global function array_new(int ta, elemtype, length,lower)object p=
!create a packed array type ta, with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int elemsize

	elemsize:=ttsize[elemtype]

	p:=obj_new(ta)
	p.uarray.mutable:=1
	p.uarray.lower:=lower
	p.uarray.length:=length
	p.uarray.objtype:=normal_obj
	p.uarray.elemtag:=elemtype

	if length then
		q:=p.uarray.ptr:=pcm_allocz(length*elemsize)
		p.uarray.allocated:=allocbytes/elemsize
	fi

	return p
end

global function list_new(int length,lower=1, variant defval=nil)object p=
	variant q

	p:=obj_new(tlist)
	p.ulist.mutable:=1
	p.ulist.lower:=lower
	p.ulist.length:=length
	p.ulist.objtype:=normal_obj

	if length then
		q:=p.ulist.vptr:=pcm_alloc(length*varrec.bytes)
		p.ulist.allocated:=allocbytes/varrec.bytes
		to length do
			if defval then
				q^:=pc_share(defval)^
			else
				q.tagx:=tvoid
			fi
			++q
		od
	fi

	return p
end

global proc objtovar(object p, variant q)=
	q.tagx:=p.tag ior hasrefmask
	q.objptr:=p
end

global function set_new(int length,lower)object p=
!create a packed array type ta, with element-type t, given length and lower bound.
!it will be initialised to zeros

	p:=bits_new(tu1,length,lower)
	p.tag:=tset

	return p
end

global function bits_new(int elemtype,length,lower)object p=
!create a packed array type ta, with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int bitwidthx,nbits,nbytes

	p:=obj_new(tbits)
	p.ubits.mutable:=1
	p.ubits.lower:=lower
	p.ubits.length:=length
	p.ubits.objtype:=normal_obj
	p.ubits.elemtag:=elemtype

	bitwidthx:=ttbitwidth[elemtype]		!should be 1, 2 or 4 bits
	nbits:=length*bitwidthx				!total bits needed

	nbytes := ((nbits-1)/64+1)*8		!bytes required in 64-bit blocks

!CPL "BITSNEW",=LENGTH,P.UBITS.LENGTH


	if length then
!		p.ubits.ptr := pcm_allocz(nbytes)              !(turns total allocated in 'allocbytes')
		p.ubits.ptr := pcm_alloc(nbytes)              !(turns total allocated in 'allocbytes')

!CPL "BITS/NEW",=NBYTES

		p.ubits.allocated64 := word64(allocbytes)*(8/bitwidthx)

		pcm_clearmem(p.ubits.ptr,allocbytes)
	else
		p.ubits.ptr:=nil
	fi

	return p
end

global function struct_new(int t)object p=
	p:=obj_new(t)
	p.ustruct.mutable:=1

	p.ustruct.ptr:=pcm_allocz(ttsize[t])
	p.ustruct.allocated:=allocbytes

	return p
end

global function dict_new(int n)object p=
	int m

	m:=max(16,nextpoweroftwo(n*2))		!list has 2n entries, min 16, rounded up to 2**x


	p:=list_new(m,1,nil)
	p.tag:=tdict
	p.udict.dictitems:=0
	return p

end

global function record_new(int rectype)object p=
	variant q

	p:=obj_new(rectype)
	p.urec.mutable:=1
!	p.urec.lower:=1
!	p.urec.length:=ttlength[rectype]
!	p.urec.objtype:=normal_obj

!	p.urec.vptr:=pcm_alloc(p.urec.length*varrec.bytes)
	p.urec.vptr:=pcm_allocz(ttlength[rectype]*varsize)
!	p.urec.allocated:=allocbytes/varrec.bytes
!	to p.urec.length do
!		q.tagx:=tvoid
!	od
!
	return p
end

global proc list_free(object p)=
!free contents of list, but not p itself
	if p.ulist.length then
!CPL "FREEING",P.ULIST.ALLOCATED*VARREC.BYTES
		pcm_free(p.ulist.vptr,p.ulist.allocated*varrec.bytes)
	fi
end

global proc record_free(object p)=
!free contents of list, but not p itself
	pcm_free(p.urec.vptr,ttlength[p.tag]*varsize)
end

global proc array_free(object p)=
!free contents of list, but not p itself
	if p.ulist.length then
		pcm_free(p.uarray.ptr,p.uarray.allocated*ttsize[p.uarray.elemtag])
	fi
end

global proc bits_free(object p)=
!free contents of list, but not p itself
	if p.ulist.length then
		pcm_free(p.ubits.ptr,bits_bytesize(p))
	fi
end

global proc dict_free(object p)=
!free contents of list, but not p itself

	if p.udict.length then
		pcm_free(p.udict.vptr,p.udict.allocated*varrec.bytes)
	fi

end

global function bits_bytesize(object p)int=
!return how many bytes are used by the object
!should be bits, but set should work; also array?
	int elemtype,nbits

	elemtype:=p.ubits.elemtag

	case elemtype
	when tu1,tu2,tu4 then
		nbits:=ttbitwidth[elemtype]*p.ubits.length
		if nbits iand 7 then			!fractional number of bytes
			return nbits/8+1
		else
			return nbits/8
		fi
	esac
	return ttsize[elemtype]*p.uarray.length		!array?
end

global proc list_resize(object p,int n)=
	variant q

	if n<=p.ulist.allocated then
		p.ulist.length:=n
	else
!CPL "LIST RESIZE"
		q:=pcm_alloc(n*varsize)
		if p.ulist.length then
			memcpy(q,p.ulist.vptr,p.ulist.length*varsize)
			pcm_free(p.ulist.vptr,p.ulist.allocated*varsize)
		fi
		p.ulist.vptr:=q
		p.ulist.length:=n
		p.ulist.allocated:=allocbytes/varsize
	fi
end

global proc array_resize(object p,int n)=
	ref byte q
	int elemsize

	elemsize:=ttsize[p.uarray.elemtag]

	if n<=p.uarray.allocated then
		p.uarray.length:=n
	else
		q:=pcm_alloc(n*elemsize)
		if p.uarray.length then
			memcpy(q,p.uarray.ptr,p.uarray.length*elemsize)
			pcm_free(p.uarray.ptr,p.uarray.allocated*elemsize)
		fi
		p.uarray.ptr:=q
		p.uarray.length:=n
		p.uarray.allocated:=allocbytes/elemsize
	fi
end

global proc bits_resize(object p,int n)=
	object pnew
	ref byte q
	int elemsize,oldrefcount

	if n<=p.ubits.allocated64 then
		p.ubits.length:=n
		return
	fi

	pnew:=bits_new(p.ubits.elemtag,p.ubits.length,p.ubits.lower)

	memcpy(pnew.ubits.ptr, p.ubits.ptr, bits_bytesize(p))

	oldrefcount:=p.ubits.refcount
	bits_free(p)
	p^:=pnew^
	p.refcount:=oldrefcount
end

global proc string_resize(object p,int n)=
	ref char q
	int elemsize

	if n<=p.ustr.allocated then
		p.ustr.length:=n
	else
		q:=pcm_alloc(n)
		if p.ustr.length then
			memcpy(q,p.ustr.strptr,p.ustr.length)
			pcm_free(p.ustr.strptr,p.ustr.allocated)
		fi

		p.ustr.strptr:=q
		p.ustr.length:=n
		p.ustr.allocated:=allocbytes
	fi
end

global function copyonwrite(object p,int tag)object=
!if p is not writable, then make a writable copy
!return new object
object q
varrec v

if p.ulist.mutable then return p fi

v.tagx:=tag+hasrefmask
v.objptr:=p

pc_dupl(&v)

q:=v.objptr
q.ulist.mutable:=1
return q
end

global function make_strslicexobj(ichar s, int length)object=
!s is an existing non-allocated or external string
!create a special string slice object, which for now has the format::
! .objtype=extslice, but .objptr2=0
!length can be 0, then s can be nil or ""
!
	object p

	if length=0 then s:=nil fi

	p:=obj_new(tstring)
	p.ustr.strptr:=s
	p.ustr.mutable:=1
	p.ustr.length:=length
	p.objtype:=extslice_obj		!.objptr2 will be zero
	return p
end

global function bignum_make(ref void bn)object p=

	p:=obj_new(tbignum)
	p.udec.bnptr:=bn

	return p
end

=== pc_bignum.m 14/38 ===

import clib
import mlib
import oslib

import pc_types
import pc_decls
import pc_support
import pc_objlib
!import pc_pcfns
!import bnlib
import mbignum

global proc bx_makestr(ref char s, int length, variant p)=
!convert string to bigint
makebnvar(p,bn_makestr(s,length))
end

global function bx_tostring(variant a,int fmt)ichar=
return bn_tostring(a^.objptr^.udec.bnptr,fmt)
end

global proc bx_dupl(variant p)=
bignum a

a:=bn_init()
bn_dupl(a,p^.objptr^.udec.bnptr)

makebnvar(p,a)
end

global proc bx_negto(variant p)=
bn_negto(p^.objptr^.udec.bnptr)
end

global proc bx_absto(variant p)=
bn_absto(p^.objptr^.udec.bnptr)
end

function makebnvar(variant dest,bignum bn=nil)bignum=
!dest is an uninitialised variant
!set it up to point to in initialised bignum handle
dest^.tagx:=tbignum ior hasrefmask

if bn=nil then
	bn:=bn_init()
fi

dest^.objptr:=bignum_make(bn)
return bn
end

global proc bx_free(variant a)=
bn_free(a^.objptr^.udec.bnptr)
freeobject(a^.objptr)
end

global proc bx_makeint(i64 aa, variant dest)=
!convert normal int to bigint
makebnvar(dest,bn_makeint(aa))
end

global proc bx_add(variant a,b,c)=
!c:=a+b
varrec v
bn_add(makebnvar(&v),a^.objptr^.udec.bnptr,b^.objptr^.udec.bnptr)
c^:=v
end

global proc bx_sub(variant a,b,c)=
varrec v
bn_sub(makebnvar(&v),a^.objptr^.udec.bnptr,b^.objptr^.udec.bnptr)
c^:=v
end

global proc bx_mul(variant a,b,c)=
varrec v
bn_mul(makebnvar(&v),a^.objptr^.udec.bnptr,b^.objptr^.udec.bnptr)
c^:=v
end

global proc bx_div(variant a,b,c)=
varrec v
bn_div(makebnvar(&v),a^.objptr^.udec.bnptr,b^.objptr^.udec.bnptr,100)
!bn_idiv(makebnvar(&v),a^.objptr^.udec.bnptr,b^.objptr^.udec.bnptr)
c^:=v
end

global proc bx_idiv(variant a,b,c)=
varrec v
bn_idiv(makebnvar(&v),a^.objptr^.udec.bnptr,b^.objptr^.udec.bnptr)
c^:=v
end

global function bx_equal(variant a,b)int=
return bn_equal(a^.objptr^.udec.bnptr, b^.objptr^.udec.bnptr)
end

global function bx_cmp(variant a,b)int=
!compare two numbers, return -1,0,1
return bn_cmp(a^.objptr^.udec.bnptr, b^.objptr^.udec.bnptr)
end

global function bx_int(variant p)i64=
!convert bigint to int64 value
bignum a,b
int64 x

a:=p^.objptr^.udec.bnptr

if bn_isint(a) then
	return bn_toint(a)
fi
b:=bn_init()
bn_fix(b,a)
x:=bn_toint(b)
bn_free(b)
return x
end

global proc bx_power(variant a,int64 n,variant dest)=
!return a**b for bigints
varrec e

bn_ipower(makebnvar(&e),a^.objptr^.udec.bnptr,n)
dest^:=e
end

global proc bx_reduce(variant bn)=
!when bn can be represented as an ordinary int,
!then change to that.
PCERROR("BX_REDUCE")
end

global function bx_length(variant bn)int=
return bn_digits(bn^.objptr^.udec.bnptr)
end
=== mbignum.m 15/38 ===
!(Decimal 'bignumber' library for integers and floats)

import clib
import mlib
import oslib

const digitwidth   = 9
const digitbase	= 1000000000
const digitfmt	 = "%09d"
const mdigitfmt	 = "z9"

INT NMAKE
INT NFREE


const digitmax	 = digitbase-1

global type bignum  = ref bignumrec
type elemtype = int32
const elemsize = elemtype.bytes

record bignumrec =
	ref[0:]elemtype num
	int length
	int expon
	int32 neg
	int32 numtype
end

record constrec =
	int64 value
	bignum bnvalue
	ref constrec nextconst
end

!special values for bignum types
tabledata() [0:]ichar fpnames =
	(zero_type = 0,	 $),
	(normal_type,	   $),
	(inf_type,	 	 $),
	(nan_type,	 	 $),
end

!recognised combinations of bignum types (bintypes)
enum (
	nn_types,	 	  ! both numbers (non-zero)
	zz_types,	 	  ! both zero
	ii_types,	 	  ! both infinity
	xx_types,	 	  ! one or both is nan

	nz_types,	 	  ! number/zero
	ni_types,	 	  ! number/infinity

	zn_types,	 	  ! zero/number
	in_types,	 	  ! infinity/number

	zi_types,	 	  ! zero/infinity
	iz_types)	 	  ! infinity/zero

const maxprec	  = 10 million
!int currprec	   = 100/digitwidth
int currprec	   = 300/digitwidth

int stblz	 	 	 !global set by smalltobig

ref constrec constlist=nil	  !use linked list of constant values

global function bn_init()bignum=
	bignum a

	a:=makebignum(0)
	return a
end

function readexpon(ichar s)int=
!s points just after 'e' or 'E'
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
		bn_error("make expon?")
	end doswitch

	return (neg|-expon|expon)
end

global proc bn_print(bignum a,int format=0)=
	ichar s

	s:=bn_tostring(a,format)
	print s
!   free(s)
end

global proc bn_println(bignum a, int format=0)=
	bn_print(a,format)
	println
end

function getbintype(bignum a,b)int=
!return bintype code for combination of a and b
	int atype:=a^.numtype, btype:=b^.numtype

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

function makebignum(int length)bignum=
!ndigits=0 to create a zero value
!these are wide digits
	bignum a

!CPL "MAKEBIGNUM",++NMAKE
	a:=bn_alloc(bignumrec.bytes)
	if length then
		a^.num:=bn_alloc(length*elemsize)
		a^.numtype:=normal_type
	else
		a^.num:=nil
		a^.numtype:=zero_type
	fi
	a^.length:=length
	a^.expon:=0
	a^.neg:=0

	return a
end

function makesmallnum(int length)ref elemtype=
	return bn_alloc(length*elemsize)
end

function smalltobig(bignum c, ref elemtype a, int length,alloc,offset=0)bignum =
!copy numeric data from smallnum into new bignum
!also normalises by removing trailing zeros and leading zeros
!sets up expon with assumption that sequence represents an int
!will also free alloc elemente of a, provided memory is not reused
!offset is to be added to a, when a doesn't point to original allocation

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

		if newlength=length=alloc then	 	 !can use data in a directly
	 	   c^.num:=cast(a)
		else
	 	   c^.num:=cast(makesmallnum(newlength))
	 	   memcpy(c^.num,a+leadingzeros,newlength*elemsize)
	 	   freesmall(a+offset,alloc)
		fi
		c^.length:=newlength
		c^.numtype:=normal_type
		c^.expon:=length-1-leadingzeros	 		!include trailing zeros, but not leading ones?
	elsif alloc then	 	 	 	 	 	 	  !result stays at zero
		freesmall(a+offset,alloc)
	fi

	return c
end

proc freesmall(ref elemtype p, int length)=
	freemem(p,length*elemsize)
end

global function bn_alloc(int size)ref void=
	ref void p

	p:=pcm_alloc(size)
	if p=nil then
		abortprogram("bignum:out of memory")
	fi

	return p
end

global function checkedmalloc(int size)ref void=
	ref void p

	p:=malloc(size)
	if p=nil then
		abortprogram("CM:Out of memory")
	fi

	return p
end

global proc bn_free(bignum a)=
!free digit memory and descriptor
	if a then
!CPL "	FREE BIG NUM",++NFREE
		bn_setzero(a)
		freemem(a,bignumrec.bytes)
	fi
end

proc freemem(ref void p, int size)=
!(my own deallocator needs the size; C's free() doesn't)
!   free(p)
	pcm_free(p,size)
end

global proc bn_setzero(bignum a)=
!clear digit memory only; clear descriptor to a zero number
	if a then
		if a^.num then
	 	   freesmall(cast(a^.num),a^.length)
		fi
		a^.num:=nil
		a^.length:=0
		a^.neg:=0
		a^.expon:=0
		a^.numtype:=zero_type
	fi
end

global proc bn_move(bignum a,b)=
!move contents of b to a. Original value of a is cleared; b becomes zero

bn_setzero(a)
a^:=b^
memset(b,0,bignumrec.bytes)
end

global proc bn_dupl(bignum a,b)=
!copy contents of b to a. Each copy is independent
	bignum c
	int size

!   if a=b then
		c:=bn_init()
		c^:=b^
		if c^.length then
			c^.num:=cast(makesmallnum(size:=c^.length))
			memcpy(c^.num,b^.num, size*elemsize)
		fi
		bn_move(a,c)
		bn_free(c)
!   fi

!   bn_setzero(a)
!   a^:=b^
!   if a^.length then
!	   a^.num:=bn_alloc(a^.length*elemtype.bytes)
!   fi
end

global proc bn_setinf(bignum dest) =
	bn_setzero(dest)
	dest^.numtype:=inf_type
end

global proc bn_setnan(bignum dest) =
	bn_setzero(dest)
	dest^.numtype:=nan_type
end

proc bn_error(ichar mess) =
	print "BN:"
	abortprogram(mess)
end

global function bn_iszero(bignum a)int=
	return a^.numtype=zero_type
end

global proc bn_negto(bignum a)=
	if not bn_iszero(a) then
		a^.neg:=not a^.neg
	fi
end

global proc bn_absto(bignum a)=
	a^.neg:=0
end

global function bn_isint(bignum a)int =
	return a^.length<=a^.expon+1
end

global function bn_getprec(bignum a)int=
	return a^.length*digitwidth
end

global proc bn_setprec(bignum a,int prec)=
	int oldlength,newlength
	bignum c

	if a^.numtype<>normal_type then
		return
	fi

	if prec<1 or prec>maxprec then
		return
	fi

!prec is digit count, not words
	prec:=((prec-1)/digitwidth+1)*digitwidth		!must be multiple of digitwidth

!prec should be rounded up as needed to next multiple of digitwith
	newlength:=prec/digitwidth	 	 	 	   !no. words

	oldlength:=a^.length

!CPL =OLDLENGTH,=NEWLENGTH
!   if oldlength=newlength then
	if oldlength<=newlength then
!   if oldlength>=newlength then
		return
	fi

	c:=makebignum(newlength)
	c^.neg:=a^.neg
	c^.expon:=a^.expon

	for i:=0 to newlength-1 do
		if i<oldlength then
	 	   c^.num^[i]:=a^.num^[i]
		else
	 	   c^.num^[i]:=0
		fi
	od

	bn_move(a,c)
	bn_free(c)
end

global function bn_getglobalprec:int=
	return currprec*digitwidth
end

global proc bn_setglobalprec(int prec)=
	currprec:=((prec-1)/digitwidth+1)
end

global function bn_makeint(int x)bignum =
	bignum a
	[256]char str

	if x=0 then
		a:=makebignum(0)
	elsif x in 0..digitmax then
		a:=makebignum(1)
		a^.num^[0]:=x
	elsif -x in 0..digitmax then
		a:=makebignum(1)
		a^.num^[0]:=-x
		a^.neg:=1
	else
		sprintf(&.str,"%lld",x)
		a:=bn_makestr(&.str)
	fi

	return a
end

global function bn_makefloat(real64 x)bignum =
	bignum a
	[2048]char str

	sprintf(&.str,"%.30g",x)
!   sprintf(&.str,"%.17e",x)

CPL =&.STR

	return bn_makestr(&.str)
end

global proc bn_ipower(bignum d, a,int64 n)=
!return a**b for bigints
	bignum e,f

	if n<0 then
		bn_setzero(d)

	elsif n=0 then
		bn_move(d,bn_makeint(1))

	elsif n=1 then
		bn_dupl(d,a)
!
	elsif (n iand 1)=0 then
		e:=bn_init()
		bn_mulu(e,a,a)
		bn_ipower(d,e,n/2)
		bn_free(e)	  

	else	 	   !assume odd
		e:=bn_init()
		f:=bn_init()
		bn_mulu(e,a,a)
		bn_ipower(f,e,(n-1)/2)
		bn_mulu(d,a,f)
		bn_free(e)
		bn_free(f)

	fi
end

function smallsubto(ref elemtype p,q, int plen, qlen)int=
!subtract q from p, return new length. New p will be moved up if smaller
!p>=q, and plen>=qlen
	ref elemtype pp,qq
	int carry,diff,z

	pp:=p+plen-1
	qq:=q+qlen-1
	carry:=0
	z:=0	 	 	 	 !leading zeros

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
	if carry then bn_error("SSUBTO/CARRY?") fi

	if z=plen then --z fi	 	  !result is zero, needs at least one digit

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

function smallmulto(ref elemtype p,q, int plen, m)int=
!multiply bignum sequence p inplace, by single digit m
!return new length (will be plen or plen+1, unless result is zero)
!p must be long enough to store the extra digit

	ref elemtype pp,qq
	int carry,d

	case m
	when 0 then
		p^:=0
		return 1
	when 1 then
		memcpy(p,q,plen*elemsize)
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

	if carry then	 	 	 !need extra digit
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

global function bn_equal(bignum a,b)int=
	if a^.length<>b^.length or \
	   a^.numtype<>b^.numtype or \
	   a^.neg<>b^.neg or \
	   a^.expon<>b^.expon then
		return 0
	fi

	if a^.length=0 then return 1 fi

	return eqbytes(a^.num,b^.num,a^.length*elemsize)
end

global proc bn_addu(bignum dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry,expona,exponb
	int dc
	word j
	ref[0:]elemtype pa,pb
	ref elemtype pax,pbx
	ref elemtype c,c2

	if a^.expon<b^.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
	fi

	expona:=a^.expon
	exponb:=b^.expon
	preca:=a^.length
	precb:=b^.length

	offset:=expona-exponb	 	  !for indexing B elements shift to match A
	uppera:=preca-1
	upperb:=precb-1

	if uppera>(upperb+offset) then  !A defines overall precision; B contained within A
		upperc:=uppera
	else	 	 	 	 		!B extends overall precision
		upperc:=upperb+offset
	fi
	precc:=upperc+1

	c:=makesmallnum(precc)	 	 !no space for carry
	carry:=0
	pa:=a^.num
	pb:=b^.num

	for i:=upperc downto 0 do	 	  !do the add, starting from ls digit

		j:=i-offset	 	 	 	  !index of A/C in terms of B
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
		memcpy(c2+1,c,precc*elemsize)
		freesmall(c,precc)
		c:=c2
		++precc
	fi

	smalltobig(dest,c,precc,precc)

	dest^.expon:=expona+carry
end

proc bn_subu(bignum dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry, expona
	int da,db,dc, isneg, z, newprec,diff
	word j
	ref[0:]elemtype pa,pb
	ref elemtype c

!can only do subtract when a>=b; do some basic checks
	isneg:=0
	if a^.expon<b^.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
		isneg:=1
	fi

!know that a>=b, and that isneg might be true
retry::
	expona:=a^.expon
	preca:=a^.length
	precb:=b^.length

	offset:=expona-b^.expon	 	!for indexing B elements shift to match A
	uppera:=preca-1
	upperb:=precb-1

	if uppera>(upperb+offset) then  !A defines overall precision; B contained within A
		upperc:=uppera
	else	 	 	 	 		!B extends overall precision
		upperc:=upperb+offset
	fi
	precc:=upperc+1

	c:=makesmallnum(precc)
	carry:=0
	pa:=a^.num
	pb:=b^.num

	for i:=upperc downto 0 do	 	  !do the add, starting from ls digit
		j:=i-offset	 	 	 	  !index of A/C in terms of B
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
		if isneg then	 	  !already swapped
	 	   bn_error("SUBU/CARRY")
		fi
		swap(a,b)
		isneg:=1
		freesmall(c,precc)
		goto retry
	fi

	smalltobig(dest,c,precc,precc)
	dest^.neg:=isneg
	dest^.expon:=expona-stblz

end

proc bn_mulu(bignum dest, a,b) =
!unsigned multiply, c:=a*b
!general scheme A1/B1 are least significant words
!x is total overflows (product overflow and carry) from previous column

!(A4 A3 A2 A1) * (B3 B2 B1)
!
!0	 0	 x	 A4.B1 A3.B1 A2.B1 A1.B1
!0	 x	 A4.B2 A3.B2 A2.B2 A1.B2 0
!x	 A4.B3 A3.B3 A2.B3 A1.B3 0	 0

	int uppera, upperb, upperc
	int precc,expona,exponb
	int ax,bx,cx		!indices within a,b,c
	int i,cx1, nc2
	i64 p,carry,x
	bignum d
	ref elemtype c
	i64 pdquot,pdrem

	expona:=a^.expon
	exponb:=b^.expon
	uppera:=a^.length-1
	upperb:=b^.length-1

	precc:=uppera+upperb+2
	nc2:=precc

	c:=makesmallnum(nc2)
	memset(c,0,precc*elemsize)
!   c^.expon:=a^.expon+b^.expon+1
	cx:=precc-1

	for bx:=upperb downto 0 do
		carry:=0

		cx1:=cx
		for ax:=uppera downto 0 do
	 	   p:=i64((a^.num^[ax]))*i64((b^.num^[bx]))+carry
	 	   pdquot:=p/digitbase
!	 	  x:=int(c^.num^[cx1])+p rem digitbase
	 	   x:=int64((c+cx1)^)+p rem digitbase
	 	   if x>digitmax then
	 	 	  carry:=pdquot+x/digitbase
!	 	 	 c^.num^[cx1--]:=x rem digitbase
	 	 	  (c+cx1--)^:=x rem digitbase
	 	   else
	 	 	  carry:=pdquot
!	 	 	 c^.num^[cx1--]:=x
	 	 	  (c+cx1--)^:=x
	 	   fi

		od
		(c+cx1)^:=carry
		--cx	 	 	  !for next row, start at next column in dest
	od

	smalltobig(dest,c,precc,nc2)
	dest^.expon:=expona+exponb+1-stblz

end

function smalldiv(ref elemtype x, b, int &xlen, nb)elemtype =
!x,b are smallnums: arrays of elements, of the exact lengths given
!x is same length as b, or at most one element longer
!(x can also be smaller, but then result is just 0)
!return integer x/b as machine word type 0..digitmax
!when digits are 0..9, then result of x/b is always going to be 0 to 9.

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
		if nx<nb then	 	 	 !completed this k
	 	   exit
		elsif nx>nb then	 	   !x will be at most 1 digit wider than b
	 	   xx:=int64(x^)*digitbase+int64((x+1)^)
	 	   y:=xx/(b^+1)
		else	 	 	 	 	 	   !x,b are same length
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
	 	   BN_ERROR("smalldiv:Y=0")
		fi
	od

	freesmall(e,esize)
	xlen:=nx	 	 	 	 !return modified x, and new length of x
	return k
end

global proc bn_idivu(bignum dest,a,b,rm=nil)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper
	int uppera, upperb, upperc
	int n, k, nexta
	int64 xx,y
	ref elemtype pa,pb

	na:=a^.length
	nb:=b^.length
	expona:=a^.expon
	exponb:=b^.expon
	badjust:=exponb+1-nb

	if na>expona+1 or nb>exponb+1 then
		bn_error("idivu:a or b not int")
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
	pa:=cast(a^.num)
	pb:=cast(b^.num)	 	   !p is not zero, and all digits of interest are present

!x is the moving and changing window into a that b is divided into get next digit of result
!use a permanently allocated smallnum, 1 digit wider than b
	n:=nb	 	 	 	!n is also how many digits of a we're into so far
	x:=makesmallnum(nx2:=n+1)	   !allow one extra digit
	nx:=n	 	 	 	 	   !current x size
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
		if n>=nupper then	 	 	 	!finished with A 
	 	   exit
		fi

		nexta:=(n>uppera|0|(pa+n)^)
		++n
		if nx=1 and x^=0 then
	 	   x^:=nexta	 	 	 !x is 1 digit long
		else
	 	   (x+nx)^:=nexta	 	 !next digit from a
	 	   ++nx
		fi
	od

	if rm and exponb<nb then		!no trailing zeros in b
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

	if c^=0 and cx>=2 then	 	 	!leading zero (may not need cx check)
		smalltobig(dest,c+1,cx-1,nc,-1)
	else
		smalltobig(dest,c,cx,nc)
	fi
!   freesmall(c,nc)

	if rm and exponb>=nb then	 	  !has trailing zeros so natural rem doesn't work
		bignum d
		d:=bn_init()
		bn_mulu(d,b,dest)
		bn_subu(rm,a,d)
		bn_free(d)
	fi

end

function strvaln(ref char s,int n)int=	  !STRVALN
!convert first n chars of s to int value and return result will fit into 32 bits
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

global function bn_makestr(ichar s, int length=0)bignum=
	ichar t,u
	int neg,dpindex,expon,nonzeros,talloc,dpseen
	int leadingzeros, trailingzeros,zerosafterdp
	int d,n,wd,dp,wdp,w,d2,na,nb
	bignum a

	if length=0 then
		length:=strlen(s)
	fi
	if length<=0 then
		return badnumber()
	fi
	talloc:=length+1+10	 	!allow for extending last wdigit group

	neg:=0
	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	t:=u:=bn_alloc(talloc)	  !accummulate sig digits into t
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
!	   trailingzeros:=0
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
	length:=u-t	 	 	   !new length of extracted digits
	if dpindex<0 then
		if dpseen then
	 	   dpindex:=-zerosafterdp
		else
	 	   dpindex:=length
		fi
	fi
	length-:=trailingzeros	  !adjust precision to ignore trailing zeros
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
	a^.neg:=neg
	a^.expon:=wd
	u:=t
	a^.num^[0]:=strvaln(u,na)
	u+:=na
	
	for i:=1 to length-1 do
		a^.num^[i]:=strvaln(u,w)
		u+:=w
	od

	freemem(t,talloc)

	return a
end

proc bn_fdivu(bignum dest,a,b,int precision)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper,nc2
	int uppera, upperb, upperc
	int n, k, nexta
	int64 xx,y
	ref elemtype pa,pb

	na:=a^.length
	nb:=b^.length
	expona:=a^.expon
	exponb:=b^.expon

	if precision then
		precision:=((precision-1)/digitwidth+1)	 	!must be multiple of digitwidth
	else
		precision:=currprec
	fi
	nc:=precision

	uppera:=na-1
	upperb:=nb-1
	upperc:=nc-1
	pa:=cast(a^.num)
	pb:=cast(b^.num)	 	   !p is not zero, and all digits of interest are present

!x is the moving and changing window into a that b is divided into get next digit of result
!use a permanently allocated smallnum, 1 digit wider than b
	n:=nb	 	 	 	!n is also how many digits of a we're into so far
	x:=makesmallnum(nx2:=n+1)	   !allow one extra digit
	nx:=n	 	 	 	 	   !current x size

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

		if cx>nc then	 	 	 !reached given precision
	 	   exit
		fi

		nexta:=(n>uppera|0|(pa+n)^)
		++n
		if nx=1 and x^=0 then
	 	   x^:=nexta	 	 	 !x is 1 digit long
		else
	 	   (x+nx)^:=nexta	 	 !next digit from a
	 	   ++nx
		fi
	od

	freesmall(x,nx2)

	if cx=1 and c^=0 then
		freesmall(c,nc2)
		bn_setzero(dest)
		return
	fi

	if c^=0 and cx>=2 then	 	 	!leading zero (may not need cx check)
		smalltobig(dest,c+1,cx-1,nc2,-1)
		dest^.expon:=expona-exponb-1
	else
		smalltobig(dest,c,cx,nc2)
		dest^.expon:=expona-exponb
	fi
!   freesmall(c,nc2)
end

function tostring_float(bignum a,int fmt)ichar=
!a is an actual number (not zero, infinity etc)
	int expon,upper,nchars,w,prel,n,showdot
	ichar s,t

	expon:=a^.expon
	upper:=a^.length-1

	if fmt='I' and bn_isint(a) then
		showdot:=0
	else
		showdot:=1
	fi

	w:=digitwidth
	nchars:=3	 	 	 !sign and trailing .0
	if expon<0 then
		nchars+:=abs(expon-1)*w
	fi
	nchars+:=a^.length*w
	if expon-upper>0 then
		nchars+:=(expon-upper)*w
	fi
	nchars+:=8	 	 		!margin

!   s:=t:=bn_alloc(nchars)
	s:=t:=checkedmalloc(nchars)
	
	if a^.neg then
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
!	   t++^:='*'
		n:=sprintf(t,(i>0 or prel|digitfmt|"%d"),a^.num^[i])
		t+:=n
!	   print a^.num^[i]
		if expon=i and i<upper and showdot then
	 	   t++^:='.'
		fi
	od

	to expon-upper do
!print "+"
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

global function bn_tostring(bignum a,int fmt=0)ichar=
	int expon,upper
	ichar s,t

	t:=nil
	if a=nil then
		t:="<void>"
	else
		case a^.numtype
		when zero_type then t:=(fmt='E' or fmt='F'|"0.0"|"0")
		when inf_type then t:="<inf>"
		when nan_type then t:="<nan>"
		esac
	fi

	if t then
		s:=checkedmalloc(strlen(t)+1)
		strcpy(s,t)
		return s
	fi

	if fmt=0 or fmt='A' then
		if bn_isint(a) and (a^.expon-a^.length)*digitwidth<60 then
	 	   fmt:='I'
		elsif abs(a^.expon*digitwidth)<60 then
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

function tostring_scient(bignum a)ichar=
!a is an actual number
	ichar s,t
	int expon,nchars,n,shift
	int64 x,scale

	nchars:=3

	expon:=a^.expon*digitwidth

	x:=a^.num^[0]
	scale:=1
	shift:=0
	while x>=10 do
		x:=x/10
		scale*:=10
		++expon
		++shift
	od

	nchars:=a^.length*digitwidth+16	 !allow for 1., and exponent

	s:=t:=checkedmalloc(nchars)

	if a^.neg then
		t++^:='-'
	fi

!	n:=sprintf(t,"%d.",x)
	print @t,x,,"."
	t+:=strlen(t)

	if shift then
!		n:=sprintf(t,"%0*d", shift, a^.num^[0]-x*scale)
		print @t, shift:"v",,a^.num^[0]-x*scale:"z*"
		t+:=strlen(t)
	fi

	for i to a^.length-1 do
!		n:=sprintf(t,digitfmt, a^.num^[i])
!		fprint @t,digitfmt, a^.num^[i]
		print @t,a^.num^[i]:mdigitfmt
		t+:=strlen(t)
	od

	while (t-1)^='0' and (t-2)^<>'.' do
		--t
	od

!	n:=sprintf(t,"e%d", expon)
	print @t,"e",,expon
	t+:=strlen(t)
	t^:=0

	return s
end

global function bn_add(bignum dest,a,b)int=
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

	nega:=a^.neg
	negb:=b^.neg

	if not nega and not negb then	   !both positive
		bn_addu(dest,a,b)
	elsif nega and negb then	 	   !both negative
		bn_addu(dest,a,b)
		bn_negto(dest)
	elsif not nega and negb then		!a positive, b negative
		bn_subu(dest,a,b)
	else
		bn_subu(dest,b,a)	 	 	 !a negative, b positive
	fi

	return 1
end

global function bn_sub(bignum dest,a,b)int=
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

	nega:=a^.neg
	negb:=b^.neg

	if not nega and not negb then	   !both positive
		bn_subu(dest,a,b)
	elsif nega and negb then	 	   !both negative
		bn_subu(dest,b,a)
	elsif not nega and negb then		!a positive, b negative
		bn_addu(dest,a,b)
	else	 	 	 	 	 	   !a negative, b positive
		bn_subu(dest,b,a)
	fi

	return 1
end

global function bn_mul(bignum dest,a,b)int=
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

	neg:=a^.neg<>b^.neg
	bn_mulu(dest,a,b)
	if neg then	 !different signs
		bn_negto(dest)
	fi
	return 1
end

global function bn_mulp(bignum dest,a,b, int prec)int=
	int res:=bn_mul(dest,a,b)
	if res then
		bn_setprec(dest,(prec=0|currprec|prec))
	fi
	return res
end

global function bn_div(bignum dest,a,b,int prec=0)int=
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

	neg:=a^.neg<>b^.neg

	bn_fdivu(dest,a,b,prec)
!   bn_idivu(dest,a,b)

	if neg then
		bn_negto(dest)
	fi
	return 1
end

global function bn_idiv(bignum dest,a,b)int=
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

	neg:=a^.neg<>b^.neg
	bn_idivu(dest,a,b)
	if neg then
		bn_negto(dest)
	fi
	return 1
end

global function bn_idivrem(bignum dest,rm,a,b)int=
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

	nega:=a^.neg
	negb:=b^.neg
	bn_idivu(dest,a,b,rm)
	if nega<>negb then	  !different signs
		bn_negto(dest)
	fi
	if nega then bn_negto(rm) fi
	return 1
end

global function bn_irem(bignum dest,a,b)int=
	bignum rm,d
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

	nega:=a^.neg
	d:=bn_init()
	bn_idivu(d,a,b,dest)
	if nega then bn_negto(dest) fi
	bn_free(d)
	return 1
end

global function bn_cmp(bignum a,b)int=
	bignum d
	int neg

	if bn_equal(a,b) then
		return 0
	fi

	d:=bn_init()
	bn_sub(d,a,b)
	neg:=d^.neg
	bn_free(d)
	return (neg|-1|1)
end

global function bn_const(int value)bignum =
	ref constrec p
	bignum c

	p:=constlist

	while p do
		if p^.value=value then
	 	   return p^.bnvalue
		fi
		p:=p^.nextconst
	od

!not encountered before
	p:=bn_alloc(constrec.bytes)
	p^.bnvalue:=bn_makeint(value)
	p^.value:=value
	p^.nextconst:=constlist
	constlist:=p
	return p^.bnvalue
end

global function bn_sign(bignum a)int=
	if bn_iszero(a) then
		return 0
	elsif a^.neg then
		return -1
	else
		return 0
	fi
end

function badnumber:bignum=
	bignum c
	c:=makebignum(0)
	c^.numtype:=nan_type
	return c
end

global function bn_digits(bignum a)int=
!return number of digits in integer a
	int n
	[32]char str

	if not bn_isint(a) then
		return 0
	fi
	if bn_iszero(a) then
		return 1
	fi

	n:=sprintf(&.str,"%d",a^.num^[0])
	return n+a^.expon*digitwidth
end

global function bn_toint(bignum a)int64=
	int64 x
	if not bn_isint(a) then
		return 0
	fi
	if bn_iszero(a) then
		return 0
	fi

	x:=0
	for i:=0 to a^.length-1 do
		x:=x*digitbase+a^.num^[i]
	od

	if a^.neg then
		return -x
	else
		return x
	fi
end

global function bn_tofloat(bignum a)real64=
	real64 x
	ichar s

	if bn_iszero(a) then
		return 0.0
	fi

	s:=bn_tostring(a,'E')

	sscanf(s,"%lf", &x)
	return x
end

global proc bn_fix(bignum c, a) =
	if bn_iszero(a) or a^.expon<0 then
		bn_setzero(c)
		return
	fi

	bn_dupl(c,a)
	if not bn_isint(c) then
		bn_setprec(c,(c^.expon+1))
	fi
end
=== pc_print.m 16/38 ===
import clib
import msys
import mlib
import oslib

import pc_types
import pc_decls
import pq_common
import pc_support
import pc_objlib
import pc_bignum
import pc_pcfns

!Vars for i/o
!Makes use of stdio/fileio/strio/windio as used by Q system
global  int mindev		!one of stdio/fileio/strio/windio
global  int moutdev
global  ref int minchan		!actual file handles
global  filehandle moutchan
global  varrec minvar		!strio: vars to be used as source or dest
global  varrec moutvar		!str: used for sprint(=string) and @&.string (=refvar)
global  ichar mfmtstr		!used for format string is nil (no fmt string) or points to fmt string
global  ichar mfmtcurr	!point to next char to use in fmtstr
global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,(0,0))
global  filehandle testfilech	!non-zero means contains handle for test file o/p

!I/O Constants: print/read i/o channels
global const std_io	= 0		!console i/o
global const file_io	= 1		!uses file channel inchan or outchan
global const str_io	= 2		!uses string instr^ or outstr^
global const wind_io	= 3		!uses window inwind^ or outwind^
global const istr_io	= 4		!used by pcx interpreter

const maxstrlen=256

const onesixty=1024
const comma=','

const maxoclevel=6
[0:maxoclevel]int32	moutdevstack
[0:maxoclevel]filehandle	moutchanstack
[0:maxoclevel]varrec	moutvarstack
[0:maxoclevel]byte	mgapstack
[0:maxoclevel]ref char	mfmtstrstack
[0:maxoclevel]ref char	mfmtcurrstack
int noclevels
byte mgapneeded

!const maxlistdepth=2
const maxlistdepth=4
int listdepth=0		!recursive nesting levels for lists/records

[0:]char digits=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F')

const minkb_size=262144		! start size of kb buffer
ref char kb_start		! point to start of read buffer
ref char kb_pos			! current position it's up to (next read starts here)
ref char kb_lastpos		! set by sread() just before reading used for reread()
int kb_size			! total available length of entire read buffer (which is not zero-terminated)
int kb_linelength		! length of this current line (set by readln)
int kb_length			! length of current contents of buffer (can be zero)
				! use kb_length-(kb_pos-kb_start) for length starting from kb_pos
int kb_lastlength		! used with kb_lastpos to remember start of last read item
char termchar		! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

global proc pch_print(variant p,fmt=nil) =		!PC_PRINT
!print at p^
!fmt=nil (default formatting), or fmt^ is a string containing format string
[0:onesixty]char str
varrec v
varrec emptyfmt

!CPL "PRINT1"

if fmt=nil then
	fmt:=&emptyfmt
	emptyfmt.tagx:=tvoid
fi

!CPL "PRINT2"
if mfmtstr=nil then
	if mgapneeded then
		printstr_n(" ",1)
	else
		mgapneeded:=1
	fi
else
	printnextfmtchars(0)
fi

!CPL "PRINT3"
switch ttbasetype[p^.tag]
when tstring then
!CPL "PRINT35"
	if fmt=nil or fmt^.tag=tvoid then
		printstr_n(cast(p.objptr.ustr.strptr),p.objptr.ustr.length)
!CPL "PRINT36"
		return
	fi
when tint, treal, trange, tword then	! small numeric types: use local string
!CPL "PRINT4",=P,=FMT,=&V
	pch_tostr(p,fmt,&v)
!CPL "PRINT5"
	printstr_n(v.objptr.ustr.strptr,v.objptr.ustr.length)
	pc_unshare(&v)
	return
endswitch

pch_tostr(p,fmt,&v)

printstr_n(v.objptr.ustr.strptr,v.objptr.ustr.length)

pc_unshare(&v)

end

global proc pch_println =		!PC_PRINTLN
	if mfmtstr then
		printnextfmtchars(1)
	fi
	printstrz("\r\n")
end

global proc pch_startprintcon =		!PC_STARTPRINTCON
	varrec v

	v.tagx:=tint
	v.value:=0
	pch_startprint(&v)
end

global proc pch_startprint(variant p) =
object s

switch ++noclevels
when 0, 1 then		! no action needed

when maxoclevel+1 then		! overflow
	printerror("print #x overflow")
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
switch p^.tag
when tint then
	switch p^.value
	when 0 then
doconsole::
		moutdev:=std_io
		moutchan:=nil
	
	when 1 then			! special sprint string
		moutdev:=str_io
		moutchan:=nil
		moutvar.tagx:=tstring ior hasrefmask

		s:=obj_new(tstring)
		s.ustr.mutable:=1
		moutvar.objptr:=s

	when 2 then
		if testfilech=nil then
			prterror("@2: file not open")
		fi
		moutdev:=file_io
		moutchan:=testfilech
	
	else
		moutdev:=file_io
		moutchan:=cast(filehandle(p^.value))
	endswitch

when trefvar then
	p:=p^.varptr
	switch p^.tag
	when tstring then
		moutdev:=istr_io
		moutchan:=nil
		moutvar.tagx:=trefvar
		moutvar.varptr:=p
	
	else
	PRINTLN ttname[p^.tag]
		prterror("Print@^?")
	endswitch

else
	switch ttbasetype[p^.tag]
	when trecord, tstruct then		! check for specific records
		moutdev:=std_io
	else
	PRINTLN ttname[p^.tag]
		printerror("Can't do startprint...")
	endswitch
endswitch

mgapneeded:=0

end

global proc pch_endprint =		!PC_ENDPRINT
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
	printerror("resetoc??")
fi

if noclevels=0 then
	moutdev:=std_io

else			! exit from higher nesting level
	moutdev:=moutdevstack[noclevels]
	moutchan:=cast(moutchanstack[noclevels])
	moutvar:=moutvarstack[noclevels]
	mgapneeded:=mgapstack[noclevels]
	mfmtstr:=mfmtstrstack[noclevels]
	mfmtcurr:=mfmtcurrstack[noclevels]
fi
end

global proc pch_strstartprint =		!PC_STRSTARTPRINT
varrec p

p.tagx:=tint
p.value:=1
pch_startprint(&p)		! do equivalent of @1
end

global proc pch_strendprint(variant dest) =		!PC_STRENDPRINT
if mfmtstr then
	printnextfmtchars(1)
fi
if moutdev<>str_io then
	prterror("STRendPRT/NOT STR")
fi

dest^:=moutvar						!transfer ownership
moutvar.tagx:=tvoid

pch_endprint()
end

global proc pch_setformat(variant p) =		!PC_SETFORMAT
int n
ref char s

if p.tag<>tstring then
	prterror("(str)")
fi
if mfmtstr then
	prterror("Setfmt?")
fi
n:=p.objptr.ustr.length
mfmtstr:=pcm_alloc(n+1)
if n then
	memcpy(mfmtstr,p.objptr.ustr.strptr,n)
fi
s:=mfmtstr+n
s^:=0
mfmtcurr:=mfmtstr
end

global proc pch_setformat2(variant p) =		!PC_SETFORMAT2
CPL "PC/SETFORMAT2"
end

global proc pch_dprint(variant p, fmt) =		!PC_DPRINT
!printstr_n("(",1)

pch_print(p,fmt)

switch p^.tag
when tint then printstrz("d")
when tword then printstrz("u")
endswitch
! printstr_n(")",1) 
end

global proc pch_printnogap =		!PC_PRINTNOGAP
mgapneeded:=0
end

proc initfmtcode(ref fmtrec f) =		!INITFMTCODE
f^:=defaultfmt
end

function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
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

function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
[0:onesixty]char t
int i,j,k,g
int dummy
ref char s0

i:=0
k:=0
g:=(base=10|3|4)

repeat
	t[++i]:=digits[aa rem base]

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

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt,int usigned)int =		!I64TOSTRFMT
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)
!returns length of s
[0:onesixty]char str				! allow for binary with separators!
int i,j,k,n,w
static u64 mindint=0x8000'0000'0000'0000

if fmt^.usigned then
	usigned:=1
fi

if aa=mindint and not usigned then		! minint

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
if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
	convlcstring(&.str)
fi

!at this point, n is the str length including signs and suffix
return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
[0:onesixty]char str				! allow for binary with separators!
int i,j,k,n,w
!static u64 mindint=0x8000'0000'0000'0000

n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

if fmt^.suffix then
	str[n]:=fmt^.suffix
	str[++n]:=0
fi

!str uses upper cases for hex/etc see if lc needed
if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
	convlcstring(&.str)
fi

!at this point, n is the str length including signs and suffix
return expandstr(&.str,s,n,fmt)
end

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =		!STRTOSTRFMT
!s is a string process according to fmtrec fmt^, and return result in t
!caller should check whether any changes are required to s (now it can just use s), but this
!check is done here anyway (with a simple copy to t)
!n is current length of s
!return length of t
!Three processing stages::
!1 Basic input string s
!2 Additions or mods: quotes, suffix, when conversion
!3 Width adjustment
!1 is detected here, 2 is done here, 3 is done by expandstr
ref char u,v
[256]char str
int w,nheap		! whether any heap storage is used # bytes allocated

nheap:=0

if fmt^.quotechar or fmt^.lettercase then		! need local copy
	if n<256 then
		u:=&.str
	else
		nheap:=n+3					! allow for quotes+terminator
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
	when 'a' then	! need lower when
		convlcstring(u)
	when 'A' then
		convucstring(u)
	endswitch
	s:=u
fi

w:=fmt^.minwidth
if w>n then
! fmt^.base:=0
	n:=expandstr(s,t,n,fmt)
else
	memcpy(t,s,n)
fi
if nheap then
	pcm_free(u,nheap)
fi
return n
end

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =		!EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string caller must take care of this
!result will be zero-terminated, for use in this module

int i,w,m

!check to see if result is acceptable as it is
w:=fmt^.minwidth
if w=0 or w<=n then		! allow str to be longer than minwidth
	strncpy(t,s,n)
	(t+n)^:=0
	return n
fi

if fmt^.justify='L' then	! left-justify
! strcpy(t,s)
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

else				! centre-justify?

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

global proc pc_strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!'	Add single quotes around string (and deal with embedded quotes)
!+	Always have + or - in front of integers
!n	Width
!.n	Max width/precision
!A	Convert to upper when
!a	Convert to lower when
!B	Binary
!C	Show int as single n-bit (unicode) character
!E,F,G	Specify format for double (corresponds to C format codes)
!H	Hex
!JL	Justify left
!JR	Justify right
!JC	Justify centre
!M	Show int as multiple 8-bit characters (lsb on left)
!O	Octal
!Pc	Use padding char c
!Q	Add double quotes around string (and deal with embedded quotes)
!Sc	Use separator char c between every 3 or 4 digits
!Tc	Use terminator char c (typically B or H)
!U	Show ints as unsigned
!Y	Add type suffix
!Z	Use 0 padding

char c
byte wset
int n
[0:100]char str

initfmtcode(fmt)

memcpy(&.str,s,slen)		!convert s/slen to zero-terminated string
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

! fmt^.base:=8
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
	when 'U', 'u' then fmt^.usigned:='U'
	when 'E', 'e' then fmt^.realfmt:='e'
	when 'F', 'f' then fmt^.realfmt:='f'
	when 'G', 'g' then fmt^.realfmt:='g'
! when '0','1','2','3','4','5','6','7','8','9' then
	when '.' then
		wset:=1
	when comma,'_' then fmt^.sepchar:=c
	when '+' then fmt^.plus:='+'
	when 'M', 'm' then fmt^.charmode:='M'
	when 'C', 'c' then fmt^.charmode:='C'
	when 'Y', 'y' then fmt^.showtype:='Y'
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
				fmt^.minwidth:=min(n,onesixty-1)
				wset:=1
			else
				fmt^.precision:=min(n,100)
			fi
		fi
	endswitch
od
end

proc printstrz(ref char s) =		!PRINTSTRZ
!print zero-terminated string
type fntype= ref clang function (filehandle f, ichar s,t)int
int x

switch moutdev
when std_io then
	printf("%s",s)

when file_io then
	fprintf(cast(moutchan),"%s",s)

when str_io then
	addstring(moutvar.objptr,s,-1)

when istr_io then
	printstr_n(s,strlen(s))

when wind_io then 

endswitch
end

proc printstr_n(ref char s,int n) =		!PRINTSTRN
!send string s to current m output device
!n is::
! -1:	s is z-terminated calculate length
! 0:	s is empty string (no output)
! >0:	n is length of string
variant  p
int x
type fntype= ref clang function (filehandle f, ichar s, int i, ichar t)int

if n=-1 then		! was stringz
	n:=strlen(s)
fi

if n=0 then
	return
fi

switch moutdev
when std_io then

!	printf("%.*s",int32(n),s)
	printstrn_app(s,n,nil)

when file_io then
!	fprintf(cast(moutchan),"%.*s",int32(n),s)
	printstrn_app(s,n,cast(moutchan))

when str_io then
	addstring(moutvar.objptr,s,n)

when istr_io then
	p:=moutvar.varptr
	if p^.tag<>tstring then
		prterror("prtstrn1")
	fi
	addstring(moutvar.objptr,s,n)

when wind_io then
	
endswitch
end

global proc printerror(ref char s) =		!PRINTERROR
prterror(s)
!abortprogram(s)
end

global proc addstring(object p,ref char t,int n) =		!ADDSTRING
!p is a pointer to an object string data, initially with an empty string
!store string t to to p, or append to an existing string
!n is the length of the string (-1 if not known) =
int oldlen,newlen,oldbytes,newbytes
ref char newptr

if n=0 or t^=0 then
	return
fi
if n<0 then
	n:=strlen(t)
fi

oldlen:=p.ustr.length

if p.refcount=0 then	! assume a fixed buffer
	if oldlen=0 then		! first string
		memcpy(p.ustr.strptr,t,n)
		p.ustr.length:=n
	else				! append to existing string
		memcpy(p.ustr.strptr+oldlen,t,n)
		p.ustr.length:=oldlen+n
	fi
	return
fi

if oldlen=0 then		! first or only string
	p.ustr.strptr:=pcm_alloc(n)
	p.ustr.length:=n
	p.ustr.allocated:=allocbytes
	memcpy(p.ustr.strptr,t,n)

else				! append to existing string
	newlen:=oldlen+n
	oldbytes:=p.ustr.allocated
	newbytes:=oldlen+n
	if newbytes<=oldbytes then 		! fits in current allocation
		memcpy(p.ustr.strptr+oldlen,t,n)
	else					! need new allocation
		newptr:=pcm_alloc(newbytes)
		memcpy(newptr,p.ustr.strptr,oldlen)	! existing chars
		memcpy(newptr+oldlen,t,n)		! add new chars
		p.ustr.allocated:=allocbytes
		pcm_free(p.ustr.strptr,oldbytes)
		p.ustr.strptr:=newptr
	fi
	p.ustr.length:=newlen
fi
end

global proc j_tostr_i (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_I
[0:onesixty]char str

!CPL "TSI1"
switch fmt^.charmode
when 'M' then
	domultichar(ref char(&p^.value),8,&.str,fmt)

when 'C' then
	str[1]:=p^.value
	str[2]:=0

else
	i64tostrfmt(p^.value,&.str,fmt,0)
endswitch
!CPL "TSI4"

if fmt^.showtype then
	addstring(dest,"I:",2)
fi
!CPL "TSI6"

addstring(dest,&.str,strlen(&.str))
!CPL "TSI7"
end

global proc j_tostr_r (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_R
[0:onesixty]char str,str2
[0:10]char cfmt
int n

cfmt[0]:='%'

if fmt^.precision then
	cfmt[1]:='.'
	cfmt[2]:='*'
	cfmt[3]:=fmt^.realfmt
	cfmt[4]:=0
	sprintf(&.str,&.cfmt,fmt^.precision,p^.xvalue)
else
	cfmt[1]:=fmt^.realfmt
	cfmt[2]:=0
	sprintf(&.str,&.cfmt,p^.xvalue)
fi

!at this point, n is the str length including signs and suffix
n:=strlen(&.str)		! current length
if n<fmt^.minwidth then
	expandstr(&.str,&.str2,n,fmt)
	strcpy(&.str,&.str2)
fi

addstring(dest,&.str,strlen(&.str))
end

global proc j_tostr_w (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_U
[0:onesixty]char str

switch fmt^.charmode
when 'M' then
	domultichar(ref char(&p^.uvalue),8,&.str,fmt)

when 'C' then
	str[1]:=p^.uvalue
	str[2]:=0

else
	u64tostrfmt(p^.value,&.str,fmt)
endswitch

if fmt^.showtype then
	addstring(dest,"W:",2)
fi

addstring(dest,&.str,strlen(&.str))
end

global proc j_tostr_n (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_RANGE
[0:onesixty]char str,str2
[0:10]char cfmt
int n
i64tostrfmt(p^.range_lower,&.str,fmt,0)
strcat(&.str,"..")
addstring(dest,&.str,-1)
i64tostrfmt(p^.range_upper,&.str,fmt,0)
addstring(dest,&.str,-1)
end

global proc j_tostr_s (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_S
int oldlen,newlen
ref char s
[0:100]char str
object q

!try and work out size of formatted string
q:=p.objptr
oldlen:=q.ustr.length
newlen:=oldlen

if fmt.quotechar or fmt.minwidth>newlen then
	if fmt.quotechar then
		newlen+:=2
	fi
	if fmt.minwidth>newlen then
		newlen:=fmt^.minwidth
	fi
	s:=pcm_alloc(newlen+1)
	strtostrfmt(q.ustr.strptr,s,oldlen,fmt)
	addstring(dest,s,newlen)
	pcm_free(s,newlen+1)
else
	addstring(dest,q.ustr.strptr,oldlen)
fi
end

global proc j_tostr_l_m (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_LIST
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

addstring(dest,"(",1)

r.refcount:=-r.refcount
q:=r.ulist.vptr

if p.tag=tlist then
	n:=p.objptr.ulist.length
else
	n:=ttlength[p.tag]
fi

!for i:=p.objptr.ulist.length downto 1 do
for i:=n downto 1 do
	calltostrtable(q,fmtstr,fmt,dest)
	++q
	if i<>1 then
		addstring(dest,",",1)
	fi
od
addstring(dest,")",1)
r.refcount:=-r.refcount
--listdepth
end

global proc j_tostr_a (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_AX
[0:onesixty]char str
ref byte q
int i,m,elemtype,a,b
varrec v
object pa
ref byte ptr

if fmt=nil then
	fmt:=&defaultfmt
fi

m:=p.tag
pa:=p.objptr

a:=pa.uarray.lower
elemtype:=pa.uarray.elemtag
b:=pa.uarray.length+a-1

q:=pa.uarray.ptr

!sprintf(&.str,"%s[%d:%s]",ttname[m],pa.uarray.lower,ttname[elemtype])
fprint @&.str,"#[#:#]",ttname[m],pa.uarray.lower,ttname[elemtype]
addstring(dest,&.str,-1)
addstring(dest,"A(",-1)

for i:=a to b do

	pc_loadpacked(q,elemtype,&v,nil)
!	pc_loadpacked(q,elemtype,&v,pa)
	q+:=ttsize[elemtype]
!	do_tostr(&v,fmtstr,fmt,dest)
	calltostrtable(&v,fmtstr,fmt,dest)
	if i<b then
		addstring(dest,",",1)
	fi
od
addstring(dest,")",1)
end

global proc j_tostr_b (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_BITS
[0:onesixty]char str
ref char q
int i,m,elemtype,a,b,offset,bitwidthx,n
varrec v
ref byte pbyte
object pa

if fmt=nil then
	fmt:=&defaultfmt
fi

m:=p.tag
pa:=p.objptr

a:=pa.ubits.lower
elemtype:=pa.ubits.elemtag

!CPL =PA.UBITS.BITOFFSET

!offset:=pa.ubits.bitoffset-1
offset:=pa.ubits.bitoffset

b:=pa.ubits.length+a-1
bitwidthx:=ttbitwidth[elemtype]

!sprintf(&.str,"%s[%d:%s]",ttname[m],pa.ubits.lower,ttname[elemtype])
!addstring(dest,&.str,-1)

!addstring(dest,"B(",-1)
addstring(dest,"(",-1)

q:=ref char(pa.ubits.ptr)

for i:=a to b do
	pc_loadbit(cast(q),offset,elemtype,0,&v)
!CPL "PRINT/",I,A,B,=V.VALUE,REF VOID(Q),OFFSET
	offset+:=bitwidthx
	if offset>=8 then
		offset:=0
		++q
	fi
!	tostr_table[v.tag]^(&v,fmt,dest)
!	do_tostr(&v,fmtstr,fmt,dest)
	calltostrtable(&v,fmtstr,fmt,dest)
	if i<b then
		addstring(dest,",",1)
	fi
od
addstring(dest,")",1)
end

global proc j_tostr_e (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_SET
[0:onesixty]char str
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
while (i<s.uset.length) do
	if testelem(cast(s.uset.ptr),i) then	! element i included
		j:=i+1				! now search for end of this '1' block
		while (j<s.uset.length and testelem(cast(s.uset.ptr),j)) do
			++j
		od
		--j				! last '1' in group
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
!		tostr_table[v.tag]^(&v,fmt,dest)
!		do_tostr(&v,fmtstr,fmt,dest)
		calltostrtable(&v,fmtstr,fmt,dest)
		i:=j+1
	else
		++i
	fi
od
addstring(dest,"]",1)
end

global proc j_tostr_k(variant p,fmtstr,ref fmtrec fmt,object dest) =		!POPC_TOSTR_STRUCT
[0:onesixty]char str
byte needcomma
int i,j
int stag,ftype,offset,index
ref byte ptr
varrec v
object pa
ref strec d,f
const int maxfields=100
[maxfields]int fieldtypes
[maxfields]int fieldoffsets
int nfields

if fmt=nil then
	fmt:=&defaultfmt
fi

stag:=p^.tag

!if not runfrompc then
!
!	d:=ttnamedef[stag]
!
!!create array of struct fields, which will be in reverse order
!	nfields:=0
!	f:=d^.deflist
!	while f do
!		if f^.nameid=fieldid and not f^.ax_at then
!			++nfields
!			fieldtypes[nfields]:=f^.mode
!			fieldoffsets[nfields]:=f^.offset
!		fi
!		f:=f^.nextdef
!	od
!else
	index:=ttstartfield[stag]			!into pcfieldtable[]
	nfields:=ttstructfields[stag]
	for i:=1 to nfields do
		fieldtypes[nfields-i+1]  :=pcfieldtable^[index+i-1].fieldtype
		fieldoffsets[nfields-i+1]:=pcfieldtable^[index+i-1].fieldoffset
	od
!fi

pa:=p.objptr

ptr:=pa.ustruct.ptr
addstring(dest,"(",-1)

needcomma:=0

for i:=nfields downto 1 do
	ftype:=fieldtypes[i]
	offset:=fieldoffsets[i]

	pc_loadpacked(ptr+offset,ftype,&v,nil)
	if needcomma then
		addstring(dest,",",1)
	fi
	needcomma:=1

!	tostr_table[v.tag]^(&v,fmt,dest)
!	do_tostr(&v,fmtstr,fmt,dest)
	calltostrtable(&v,fmtstr,fmt,dest)
!	++findex
od
addstring(dest,")",1)
end

global proc j_tostr_j (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_LONG
!formatting not used
!to control width etc, use tostr(), and use controls on resulting string
int i,w,length,n,onheap,g,k
[0:onesixty+1]char str
[0:onesixty*2+1]char str2
[100]char strtemp
object pp
ref char s,t,u

!pp:=p^.objptr

!PRINTLN "TEMP PRINT LONGINT:"
!addstring(dest,"BIGNUM:",-1)
s:=bx_tostring(P,0)
addstring(dest,s,-1)
free(s)
end

global proc j_tostr_d (variant p,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_DICT
!T! dict
[0:onesixty]char str
variant q
int i,length,needcomma:=0
object pa

if fmt=nil then
	fmt:=&defaultfmt
fi
addstring(dest,"[",-1)

pa:=p.objptr
q:=pa.udict.vptr		!keys/value pairs

length:=pa.udict.length/2				!number of pairs

for i:=length downto 1 do
	if q.tag=tvoid then
		q+:=2
		next
	fi
	if needcomma then
		addstring(dest,",",1)
	fi
	needcomma:=1
	calltostrtable(q,fmtstr,fmt,dest)
	q++
	addstring(dest,":",1)
	calltostrtable(q,fmtstr,fmt,dest)
	q++
od
addstring(dest,"]",1)
end

global proc j_tostr_z (variant a,fmtstr,ref fmtrec fmt,object dest) =		!PCOP_TOSTR_DICT
!T! dict
int i,n,t,u
!wordp cmd
int cmd
[0:onesixty]char str
variant q
static int nest

switch a^.tag
when tvoid then
	addstring(dest,"<Void>",-1)

when trefpacked then
!	sprintf(&.str,"Ref %s:%p",ttname[a^.uref.elemtag],a^.uref.ptr)
	fprint @&.str,"Ref #:#",ttname[a^.uref.elemtag],a^.uref.ptr
	addstring(dest,&.str,-1)

when trefbit then
!	sprintf(&.str,"Refbit %s:%p @%d [*%d]",ttname[a^.uref.elemtag],a^.uref.ptr,a^.uref.bitoffset,a^.uref.bitlength)
	fprint @&.str,"Refbit #:# @# [*#]",ttname[a^.uref.elemtag],a^.uref.ptr,a^.uref.bitoffset,a^.uref.bitlength
	addstring(dest,&.str,-1)

when trefvar then
!	sprintf(&.str,"Refvar:%p",a^.varptr)
	print @&.str,"Refvar:",,a^.varptr
	addstring(dest,&.str,-1)
	if a^.varptr then
!		sprintf(&.str," <%s>",ttname[a^.varptr^.tag])
		fprint @&.str," <#>",ttname[a^.varptr^.tag]
		addstring(dest,&.str,-1)
	fi

when trecordlink then
!	sprintf(&.str,"Link:%p",a^.varptr)
!	addstring(dest,&.str,-1)
!	sprintf(&.str,"Link:<%s>",ttname[a^.uref.elemtag])
	fprint @&.str,"Link:<#>",ttname[a^.uref.elemtag]
	addstring(dest,&.str,-1)

when trefproc then
!	sprintf(&.str,"Refproc:%p",a^.refptr)
	print @&.str,"Refproc:",,a.refptr
	addstring(dest,&.str,-1)

when treflabel then
!	sprintf(&.str,"Reflabel:%p",a^.refptr)
	print @&.str,"Reflabel:",,a^.refptr
	addstring(dest,&.str,-1)

!when trefbitfield then
!	sprintf(&.str,"Bitfield %p %s.[%d..%d]",a^.ptr,ttname[a^.uref.elemtag],a^.uref.bitoffset,a^.length+a^.uref.bitoffset-1)
!	addstring(dest,&.str,-1)
!
when ttype then
	addstring(dest,"<",1)
	addstring(dest,ttname[a^.value],-1)
	addstring(dest,">",1)

when toperator then
	addstring(dest,"<OP:",-1)
	cmd:=a^.value

	addstring(dest,cmdnames[cmd]+1,-1)
	addstring(dest,(a^.uop.opdims=1|":1"|":2"),2)
	addstring(dest,">",1)

else

	pcustype("tostr_def",a)
endswitch
end

proc printnextfmtchars(int lastx) =		!PRINTNEXTFMTCHARS
!o/p chars from fmtstr until # or eos is encountered
char c
ref char pstart
int n

pstart:=mfmtcurr
n:=0

while (1) do
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

function getreadfmtcode(variant p)int =		!GETREADFMTCODE
!p is a variant  which should point to a string containing a read format code.
!return that code as an upper when char code, eg. 'I'
char c

!if p=nil or p^.tag=tvoid then
if p=nil or p^.tag=tvoid then
!	return 'I'
	return 'A'
fi
if p.tag<>tstring then
CPL "P=%s",ttname[p^.tag]
	prterror("Readfmt?")
fi
if p.objptr.ustr.length=0 then
!	return 'I'
	return 'A'
fi

c:=toupper(p.objptr.ustr.strptr^)

switch c
when 'I', 'R', 'N', 'S', 'F', 'T', 'Z', 'C', 'L', 'H','B','A','E' then
	return c
endswitch

prterror("Readfmt2?")
return 0
end

global proc pch_sreadln(variant dev, variant dest) =		!PC_SREADLN
pch_readln(dev)

pc_makestring(kb_start,kb_length,dest)
end

global proc pch_strtoval(variant p,variant fmt,variant dest) =		!PC_STRVAL
!p should be a string, fmt is nil, or contains a string format code for read
!convert string to value, then store in dest
int fmtcode,length
byte oldmutable
object q
[1024]char str
ref char s:=&.str

q:=p.objptr

if q.ustr.length<str.len then
	memcpy(s,q.ustr.strptr,q.ustr.length)
	str[q.ustr.length+1]:=0
else
	pcerror("STRTOVAL/string too long")
fi

fmtcode:=getreadfmtcode(fmt)
if p.tag<>tstring then
	prterror("strval")
fi
length:=p.objptr.ustr.length

switch fmtcode
when 'I' then
	readint(s,length,dest)
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
!
else
	prterror("strval:fmt?")
endswitch
end

global proc pch_reread =		!PC_REREAD
kb_pos:=kb_lastpos
kb_length:=kb_lastlength
end

global proc pch_rereadln =		!PC_REREADLN
kb_pos:=kb_start
kb_length:=kb_linelength
end

function readname(ref char s,int length,variant dest)ref char =		!READNAME
ref char send
ref char itemstr
int itemlength
send:=readitem(s,length,itemstr,itemlength)
pc_makestring(itemstr,itemlength,dest)

iconvlcn(dest.objptr.ustr.strptr,dest.objptr.ustr.length)
return send
end

function readstring(ref char s,int length,variant dest)ref char =		!READSTRING
ref char send
ref char itemstr
int itemlength
send:=readitem(s,length,itemstr,itemlength)
pc_makestring(itemstr,itemlength,dest)
return send
end

function readint(ref char sold,int length,variant dest)ref char =		!READINT
!return point to next char after terminator (which can be just off length of string)
ref char p,s				! s points to ^str
ref char send
ref char itemstr
int itemlength,numlength

send:=readitem(sold,length,s,itemlength)

strtoint(s,itemlength,dest)

return send
end

function readhex(ref char sold,int length,variant dest)ref char =		!READHEX
[0:maxstrlen]char str		! local copy
ref char p,s			! s points to ^str
byte res
i64 aa
int a,t,nalloc
char c

if length=0 then
	dest^.tagx:=tint
	dest^.value:=0
	termchar:=0
	return sold
fi

!copy to buffer first skip leading spaces, and any sign
while (length and (sold^=' ' or sold^=9)) do
	++sold; --length
od

if length<=maxstrlen then	! use local buffer
	s:=&.str
	nalloc:=0
else
	nalloc:=length+1
	s:=pcm_alloc(nalloc)
fi

p:=s				! p points to next char available
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
p^:=0				! use zero terminator for local string
length:=p-s			! length of s

! try and work out type
if length<=16 then
	t:=tint
else
	t:=tbignum
fi
p:=s
switch t
when tint then
	aa:=0
	while (1) do
		c:=p^; ++p
		if c=0 then
			exit
		fi
		if c<'A' then			! assume digit '0'..'9'
			aa:=aa*16+c-'0'
		else				! assume letter 'A'..'F'
			aa:=aa*16+(c-'A')+10
		fi
	od
	dest^.tagx:=tint
	dest^.value:=aa
else
!	bx_makeu_base(s,strlen(s),dest,16)
	prterror("Readhex/long")
endswitch

if nalloc then
	pcm_free(s,nalloc)
fi

return sold
end

function readbin(ref char sold,int length,variant dest)ref char =		!READBIN
[0:maxstrlen]char str		! local copy
ref char p,s			! s points to ^str
byte res
i64 aa
int a,t,nalloc
char c

if length=0 then
	dest^.tagx:=tint
	dest^.value:=0
	termchar:=0
	return sold
fi

!copy to buffer first skip leading spaces, and any sign
while (length and (sold^=' ' or sold^=9)) do
	++sold; --length
od

if length<=maxstrlen then	! use local buffer
	s:=&.str
	nalloc:=0
else
	nalloc:=length+1
	s:=pcm_alloc(nalloc)
fi

p:=s				! p points to next char available
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
p^:=0				! use zero terminator for local string
length:=p-s			! length of s

!try and work out type
if length<=64 then
	t:=tint
else
	t:=tbignum
fi

p:=s
switch t
when tint then
	aa:=0
	while (1) do
		c:=p^; ++p
		if c=0 then
			exit
		fi
		aa:=aa*2+c-'0'
	od
	dest^.tagx:=tint
	dest^.value:=aa

else
!	bx_makeu_base(s,strlen(s),dest,2)
	prterror("Readbin/long")
endswitch

if nalloc then
	pcm_free(s,nalloc)
fi
return sold
end

function readreal(ref char sold,int length,variant dest)ref char =
[512]char str		! local copy
real x
ref char send
ref char itemstr
int itemlength,numlength

send:=readitem(sold,length,itemstr,itemlength)
strtoreal(itemstr,itemlength,dest)

return send
end

global proc pch_readln(variant dev) =		!PC_READLN
!note: generally, at least one spare given should be left at the of the buffer.
!(readline zero-terminates the input line anyway)
!Sometimes C-functions might be called directly, and a zero-terminator is added (eg. readreal/sscanf)
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

switch dev^.tag
when tvoid then
doconsole::
	readlinen(nil,kb_start,kb_size)	! reads as zero-terminated
	kb_length:=strlen(kb_start)

when tint then
	switch dev^.value
	when 0 then
		goto doconsole
	when 1 then
		if testfilech=nil then
			prterror("R@2: file not open")
		fi
		ch:=cast(testfilech)

	else
		ch:=filehandle(dev^.value)
	endswitch
	pc_readlinen(cast(ch),kb_start,kb_size)			! reads as zero-terminated
	kb_length:=strlen(kb_start)

when tstring then
	pdev:=dev.objptr
	length:=pdev.ustr.length
	if length=0 then
		kb_length:=0
		kb_start^:=0
	elsif length>=kb_size then
		prterror("KB overflow")
	else
		kb_length:=length
		memcpy(kb_start,pdev.ustr.strptr,length)
	fi
else
CPL gettypename(dev.tag)
	prterror("readln@")
endswitch

kb_pos:=kb_start
kb_lastpos:=kb_pos
kb_linelength:=kb_length
end

proc stepkbpos(ref char s) =		!STEPKBPOS
!a readxxx function has been called with kb_pos/kb_length, and has returned s to point to
!the character after the terminator
!adjust kb_pos/kb_length to point to that position
int newlen

newlen:=s-kb_pos

if newlen=0 then		! nothing read probably was at end of buffer
	return
fi
if newlen>=kb_length then	! at end of buffer
	kb_pos:=kb_pos+kb_length	! point to just past buffer (but should never be accessed when kb_length=0)
	kb_length:=0
else
	kb_pos:=kb_pos+newlen
	kb_length-:=newlen
fi
end

global proc pch_sread(variant fmt,variant dest) =		!PC_SREAD
int fmtcode
char c

!pc_cfree(dest)
fmtcode:=getreadfmtcode(fmt)
kb_lastpos:=kb_pos
kb_lastlength:=kb_length

switch fmtcode
when 'I' then
	stepkbpos(readint(kb_pos,kb_length,dest))

when 'R' then
	stepkbpos(readreal(kb_pos,kb_length,dest))

when 'N' then
!PUTS("SREAD/N")
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
!doemptystring::
		pc_emptystring(dest)
	else
		pc_makestring(kb_pos,kb_length,dest)
		kb_pos+:=kb_length
		kb_length:=0
	fi

when 'C' then
	if kb_length=0 then
		pc_emptystring(dest)
	else
		termchar:=kb_pos^
dochar::
		dest^.tagx:=tint
		dest^.value:=termchar
		++kb_pos
		--kb_length
	fi

when 'Z' then			! last terminator!
!	if termchar=0 then
!		goto doemptystring
!	fi
	goto dochar
when 'E' then
	dest^.tagx:=tint
	dest^.value:=itemerror

else
	prterror("SREAD/FMT?")
endswitch
end

proc domultichar (ref char p,int n,ref char dest,ref fmtrec fmt) =		!DOMULTICHAR
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

expandstr(&.str,dest,nchars,fmt)
end

global proc pch_tostr(variant a, b, result)=
fmtrec fmt
ref fmtrec ifmt
object p

!CPL "PCH/TOS1",=RESULT
ifmt:=pc_getfmt(b,&fmt)

!CPL "PCH/TOS2",A,A.TAG
p:=obj_new(tstring)
p.ustr.mutable:=1
!p^.length:=0
!p^.elemtag:=tu8

listdepth:=0

!CPL "PCH/TOS3",TTNAME[A.TAG],=RESULT
calltostrtable(a,b,ifmt,p)
!CPL "PCH/TOS4",=RESULT

result^.tagx:=tstring ior hasrefmask
result^.objptr:=p
!CPL "PCH/TOS5"
end

global function pc_getfmt(variant p,ref fmtrec fmt)ref fmtrec=
!p is an optional fmt string to tostr and print
!turn into a proper format
!return pointer to a format, or to the default format is not supplied
!fmt points to a fmtrec in the caller to contain the processed format

if p=nil or p^.tag=tvoid then
	return &defaultfmt
else
	if p^.tag<>tstring then
		prterror("pc_getfmt/not str?")
	fi
	if p.objptr.ustr.strptr=nil then
		return &defaultfmt
	else
		pc_strtofmt(p.objptr.ustr.strptr,p.objptr.ustr.length,fmt)
		return fmt
	fi
fi
end

proc pc_readlinen(filehandle handlex,ref char buffer,int size) =		!PC_READLINEN
ref char p
int n,x
[0:100]char buff
byte crseen
type fntype=ref clang function (ichar,int32,filehandle)int
int oldpos

!if handlex=0 then
!	handlex:=filehandle(os_getstdin())
!fi

buffer^:=0

fgets(buffer,size-2,handlex)

n:=strlen(buffer)
if n=0 then
	return
fi

p:=buffer+n-1		!point to last char
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

function readitem(ref char s,int length,ref char &itemstr,int &itemlength)ref char =		!READSTRING
!s points into the line buffer
!length is number of chars remaining in buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!Note that this is destructive. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
ref char p
char quotechar, c

!scan string, eliminating leading white space
while (length and (s^=' ' or s^=9)) do
	++s; --length
od

itemstr:=s				!assume starts here

if length=0 then		! No more chars left to read return null string
	termchar:=0
	itemlength:=0
	return s
fi

quotechar:=0			! Allow possible enclosing single or double quotes
if s^='"' then
	quotechar:='"'
	++s
	--length
elsif s^='\'' then
	quotechar:='\''
	++s
	--length
fi

!loop reading characters until separator or end reached
p:=itemstr:=s

while length do
	c:=s++^; --length
	switch c
	when ' ', 9, comma, '=' then		! separator
		if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
			goto normalchar
		fi
		termchar:=c
		exit
	else
normalchar::
		if c=quotechar then
			if length and s^=quotechar then	! embedded quote
				p^:=c
				++s
				++p
			else					! end of name
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
itemlength:=p-itemstr				! actual length of token

return s
end

function readany(ref char sold,int length,variant dest)ref char =		!READINT
!read item as int, real or string depending on content
!return point to next char after terminator (which can be just off length of string)
[0:maxstrlen]char str			! local copy
ref char p,s				! s points to ^str
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

!now analyse item
!ints consist only of 0123456789+-_'
!reals consist only of 0123456789+-Ee.

p:=s
digits:=expon:=other:=0

to itemlength do
	switch p++^
	when '0'..'9','+','-','_' then digits:=1
	when 'E','e','.' then expon:=1
	else other:=1
	end
od

dest^.tagx:=tint

if other or itemlength=0 then
	dest^.value:='STR'
	pc_makestring(s,itemlength,dest)
elsif expon then
	strtoreal(s,itemlength,dest)
else
	strtoint(s,itemlength,dest)
fi

return send
end

proc strtoreal(ichar s,int length,variant dest)=
[512]char str		! local copy
real x
int32 numlength

dest^.tagx:=treal

if length>=str.bytes or length=0 then		!assume not a real
	dest^.xvalue:=0.0
	return
fi
memcpy(&.str,s,length)
str[length+1]:=0

itemerror:=0

if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
	if numlength=length then x:=0.0 fi
	itemerror:=1
fi

dest^.xvalue:=x
end

proc strtoint(ichar s,int length, variant dest)=
!return point to next char after terminator (which can be just off length of string)
[0:maxstrlen]char str			! local copy
ref char p,q
byte signd
i64 aa
int a,res,cat
int t,nalloc
char c

itemerror:=0

if length=0 then
	dest^.tagx:=tint
	dest^.value:=0
	return
fi

!check for sign
signd:=0
if length and s^='-' then
	signd:=1; ++s; --length
elsif length and s^='+' then
	++s; --length
fi

while s^='0' and length>1 do
	++s; --length
od

p:=q:=s				! p points to next char available

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
p^:=0				! use zero terminator for local string
length:=p-s			! length of s

!classify magnitude of value as::
!'A' 0 to 2**63-1 or 0..9223372036854775807
!'B' 2**63 or 9223372036854775808
!'C' 2**63+1 to 2**64-1 or 9223372036854775809..18446744073709551615
!'D' 2**64 and over or 18446744073709551616 and over

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

!now look at sign::
if signd then
	case cat
	when 'B' then cat:='A'		!-922...808 can be int64
	when 'C' then cat:='D'		!needs longint
	esac
fi

!convert cat to type

case cat
when 'A' then t:=tint
when 'B','C' then t:=tword
else t:=tbignum
esac

p:=s
if t<>tbignum then
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
	dest^.tagx:=t
	dest^.value:=aa

else
	bx_makestr(s,length,dest)
fi
end

proc calltostrtable(variant q, fmtstr,ref fmtrec fmt, object dest)=
varrec v

!CPL "CST1"
overloadtype:=q^.tag

!CPL "CST2"
if fmtstr^.tag=tvoid then		!replace with ""; force 2 params to be pushed
	pc_emptystring(&v)
	fmtstr:=&v
fi
!RETURN

!NOTE::
! tostr handlers here will mainly use the <fmt> parameter which as already
! been processed from the format string
! But when calling handlings inside Q code, they need the original string

!CPL "CST3",TTNAME[OVERLOADTYPE],TOSTR_TABLE[OVERLOADTYPE],J_TOSTR_I

tostr_table[overloadtype]^(q,fmtstr,fmt,dest)
!CPL "CXT4"
end

function printbn(variant a0, ref fmtrec fmt,int &length)ichar=
static strbuffer destx
static ref strbuffer dest=&destx
varrec a,b,b10,vbase
int d,base, alen,b10len,n
ref char s

PCERROR("PRINTBN")

!base:=fmt^.base
!
!gs_init(dest)
!
!pc_share(a0)
!a:=a0^
!
!bx_makeint(base, &vbase)
!
!if fmt^.suffix then
!	gs_char(dest,fmt^.suffix)
!fi
!
!do
!	bx_divu(&a,&vbase,&b)
!
!	muldigit(&b,base,0,&b10)
!
!	alen:=a.objptr^.length
!	b10len:=b10.objptr^.length
!
!	d:=a.objptr^.oldbnptr^[alen-1]-b10.objptr^.oldbnptr^[b10len-1]
!	gs_char(dest,digits[d])
!
!	freebigint(&a)
!	freebigint(&b10)
!	if b.objptr^.length=2 and b.objptr^.oldbnptr^[1]=0 then
!		exit
!	fi
!	a:=b
!od
!freebigint(&b)
!
!if a0^.objptr^.oldbnptr^[0]=1 then
!	gs_char(dest,'-')
!fi
!
!if fmt^.showtype then
!	gs_str(dest,":L")
!fi
!
!n:=length:=dest^.length
!
!s:=pcm_alloc(length+1)
!
!for i:=1 to n do
!	(s+n-i)^:=(dest^.strptr+i-1)^
!od
!
!(s+n)^:=0
!
!gs_free(dest)
!
!return s
RETURN "XXX"
end
=== pc_jhandlers.m 17/38 ===
import msys
import clib
import mlib
import pc_types
import pc_decls
import pq_common
import pc_support
import pc_oslayer
import pc_host
import pc_print
import pc_bignum
import oslib
import pc_objlib
import pc_pcfns
import pc_dxfns

!converts code 'a' to 'z' to actual base type
['a'..'z']int32 typemap = (
	tarray,		!'a'
	tbits,		!'b'
	0,			!'c'
	tdict,		!'d'
	tset,		!'e'
	trefproc,	!'f'
	trefdllproc,!'g'
	trecordlink,!'h'
	tint,		!'i'
	tbignum,	!'j'
	tstruct,	!'k'
	tlist,		!'l'
	trecord,	!'m'
	trange,		!'n'
	toperator,	!'o'
	trefpacked,	!'p'
	0,			!'q'
	treal,		!'r'
	tstring,	!'s'
	ttype,		!'t'
	0,			!'u'
	trefvar,	!'v'
	tword,		!'w'
	0,			!'x'
	0,			!'y'
	tvariant)	!'z'

tabledata []ichar tabnames, []ref void singletable, []ref void doubletable =
	("add",			&add_table,			&add_dtable),
	("sub",			&sub_table,			&sub_dtable),
	("mul",			&mul_table,			&mul_dtable),
	("div",			&div_table,			&div_dtable),
	("idiv",		&idiv_table,		&idiv_dtable),
	("rem",			&rem_table,			&rem_dtable),
	("iand",		&iand_table,		&iand_dtable),
	("ior",			&ior_table,			&ior_dtable),
	("ixor",		&ixor_table,		&ixor_dtable),
	("shl",			&shl_table,			&shl_dtable),
	("shr",			&shr_table,			&shr_dtable),
	("min",			&min_table,			&min_dtable),
	("max",			&max_table,			&max_dtable),

	("jumpeq",		&jumpeq_table,		&jumpeq_dtable),
	("jumpne",		&jumpne_table,		&jumpne_dtable),
	("jumplt",		&jumplt_table,		&jumplt_dtable),
	("jumple",		&jumple_table,		&jumple_dtable),
	("jumpge",		&jumpge_table,		&jumpge_dtable),
	("jumpgt",		&jumpgt_table,		&jumpgt_dtable),

	("jumptesteq",	&jumptesteq_table,	nil),
	("jumptestne",	&jumptestne_table,	nil),

	("jumpfalse",	&jumpfalse_table,	nil),
	("jumptrue",	&jumptrue_table,	nil),

	("eq",			&eq_table,			nil),
	("lt",			&lt_table,			nil),
	("le",			&le_table,			nil),

	("concat",		&concat_table,		nil),
	("append",		&append_table,		nil),

	("addto",		&addto_table,		&addto_dtable),
	("subto",		&subto_table,		&subto_dtable),
	("multo",		&multo_table,		&multo_dtable),
	("divto",		&divto_table,		&divto_dtable),
	("idivto",		&idivto_table,		&idivto_dtable),
	("iandto",		&iandto_table,		&iandto_dtable),
	("iorto",		&iorto_table,		&iorto_dtable),
	("ixorto",		&ixorto_table,		&ixorto_dtable),
	("shlto",		&shlto_table,		&shlto_dtable),
	("shrto",		&shrto_table,		&shrto_dtable),
	("minto",		&minto_table,		&minto_dtable),
	("maxto",		&maxto_table,		&maxto_dtable),
	("concatto",	&concatto_table,	nil),
	("appendto",	&appendto_table,	nil),

	("neg",			&neg_table,			nil),
	("abs",			&abs_table,			nil),
	("inot",		&inot_table,		nil),
	("istrue",		&istrue_table,		nil),
	("jumpf",		&jumpf_table,		nil),
	("jumpt",		&jumpt_table,		nil),
	("len",			&len_table,			nil),
	("lwb",			&lwb_table,			nil),
	("upb",			&upb_table,			nil),
	("bounds",		&bounds_table,		nil),

	("incr",		&incr_table,		nil),
	("decr",		&decr_table,		nil),

	("decr",		&decr_table,		nil),
	("free",		&free_table,		nil),
	("dupl",		&dupl_table,		nil),
	("tostr",		&tostr_table,		nil),

	("in",			nil,				&in_dtable),
	("inrev",		nil,				&inrev_dtable),
	("pushix",		nil,				&pushix_dtable),
	("pushixref",	nil,				&pushixref_dtable),
	("pushdotix",	nil,				&pushdotix_dtable),
	("pushdotixref",nil,				&pushdotixref_dtable),

	("convert",		nil,				&convert_dtable),

	("mixed",		nil,				&mixed_dtable)
end

ref[0:]byte mixedmap

global proc initcalltables=
int n,i,j,ttdefault,slen
ref char name
ref void fnaddr
ref[0:]ref void stable, dtable
[0..maxdualtype]byte localmixedmap

mixedmap:=&localmixedmap
memset(mixedmap,0,localmixedmap.bytes)

n:=$get_nprocs()

for i to n do
	name:=$get_procname(i)
!CPL =NAME
	if name^='j' and (name+1)^='_' then		!j_ prefix: assume handler
		initjhandler(name,$get_procaddr(i))
	fi
od

!now fill in empty entries with specific DEF or generic default handlers
for i to tabnames.len do
	stable:=singletable[i]
	dtable:=doubletable[i]

	if stable then
	    fnaddr:=stable^[0]
		if fnaddr=nil then fnaddr:=&def_handler fi
		for j:=0 to maxtype do
			if stable^[j]=nil then
				stable^[j]:=fnaddr
			fi
		od
	fi
	if dtable then
		fnaddr:=dtable^[0]
		if fnaddr=nil then fnaddr:=&ddef_handler fi
		for j:=0 to maxdualtype do
			if dtable^[j]=nil then
				if localmixedmap[j] then			!generic mixed handler exists
					name:=tabnames[i]
					slen:=strlen(name)
					if eqstring(name+slen-2,"to") then
						goto donormal			!mixed can't deal with ref lhs, andd anyway it can't convert lhs
					fi
					dtable^[j]:=mixed_dtable[j]
				else
donormal::
					dtable^[j]:=fnaddr
				fi
			fi
		od
	fi
od
end

proc initjhandler(ref char p, ref void fnaddr)=
!p assmed to be a jhandler function name starting with j_
!extract pcl op name, the types supported, and fill in call-table entries
[32]char opname
ref char q
char c,d
int t,u,i
ref[0:]ref void stable, dtable

p+:=2			!skip j_ prefix
q:=p
while q^<>'_' and q^<>0 do
	++q
od
memcpy(&.opname, p, q-p)
opname[q-p+1]:=0

if q^=0 then return fi			!don't report as error; just ignore

for i to tabnames.len do
	if eqstring(tabnames[i],&.opname) then
		stable:=singletable[i]
		dtable:=doubletable[i]
		exit
	fi
else
	loaderror("Init: Can't find Jhandler op:",&.opname)
	stop
od

while q^='_' do
	c:=(++q)^
	if c<'a' or c>'z' then return fi
	d:=(++q)^
	if d>='a' and d<='z' then		!double-dispatch
		++q
	else
		d:=0
	fi

	t:=typemap[c]
	if d then
		u:=typemap[d]
	else
		u:=tvoid
	fi

	if d then

		if dtable=nil then
			loaderror("No d-calltable for:",p-2)
		fi
		add_dtable_entry(cast(dtable),t,u,fnaddr)
	else
		if stable=nil then
			loaderror("No s-calltable for:",p-2)
		fi
		add_stable_entry(stable,t,fnaddr)
	fi
od
end

proc add_stable_entry(ref[0:]ref void table, int t, ref void fnaddr)=
int i
if t=tvariant then t:=0 fi
table^[t]:=fnaddr
end

proc add_dtable_entry(ref[0:]ref function:ref word table, int s,t, ref void fnaddr)=
int i,j,typesig

if s=tvariant then
	typesig:=0
else
	typesig:=gettypesig(s,t)

	if table=&mixed_dtable then			!remember a generic mixed handler exists
		mixedmap^[typesig]:=1
	fi
fi

table^[typesig]:=fnaddr
end

proc def_handler=
pcerror("Single disp: no handler")
end

proc ddef_handler=
pcerror("Double disp: no handler")
end

global function j_add_i_w:ref intpc =
variant y

y:=sptr++
sptr^.value+:=y^.value
return pcptr+1
end

global function j_add_r:ref intpc =
variant y

y:=sptr++
sptr^.xvalue+:=y^.xvalue
return pcptr+1
end

global function j_add_s:ref intpc =
variant x,y
object z
int xlen,ylen,newlen
!object z
ref char s

y:=sptr
x:=++sptr

xlen:=x.objptr.ustr.length
ylen:=y.objptr.ustr.length

if xlen=0 then

	if ylen then		!y to top of stack
		sptr^:=y^
	fi					!else use empty y unchanged

elsif ylen=0 then		!x+"" use x unchanged

else 				!x+y: need to do some actual work!
	newlen:=xlen+ylen
	s:=pcm_alloc(newlen)
	memcpy(s,x.objptr.ustr.strptr,xlen)
	memcpy(s+xlen,y.objptr.ustr.strptr,ylen)
	pc_unshare(x)
	pc_unshare(y)
	pc_makestringx(s,newlen,allocbytes,sptr)

fi

return pcptr+1
end

global function j_add_j:ref intpc =  !****************************
variant x,y
varrec result

y:=sptr++
x:=sptr

bx_add(x,y,&result)
pc_unshare(x)
pc_unshare(y)
sptr^:=result
!bx_reduce(sptr)
return pcptr+1
end

global function j_add_e:ref intpc =  !********************************
variant x,y

y:=sptr
x:=++sptr
dx_iorset(x,y)
return pcptr+1
end

global function j_add_z:ref intpc =
pcmxtypes("add_def",sptr+1,sptr)
return pcptr+1
end

global function j_add_iw_wi:ref intpc =
variant y

y:=sptr++
sptr^.value+:=y^.value
sptr^.tag:=tint
return pcptr+1
end

global function j_add_zz:ref intpc =
variant y
int yt

y:=sptr++
yt:=y^.tag

switch sptr^.tag
when trefpacked then
	switch (yt)
	when tint then
		sptr^.uref.ptr:=sptr^.uref.ptr+y^.value*ttsize[sptr^.uref.elemtag]
		return pcptr+1
	endswitch
when trefvar then
	switch (yt)
	when tint then
		sptr^.varptr:=sptr^.varptr+y^.value
		return pcptr+1
	endswitch
endswitch

pcmxtypes("add/mixed_def",sptr,y)
return pcptr+1
end

global function j_sub_i_w:ref intpc =
variant y

y:=sptr++
sptr^.value-:=y^.value
return pcptr+1
end

global function j_sub_r:ref intpc =
variant y

y:=sptr++
sptr^.xvalue-:=y^.xvalue
return pcptr+1
end

global function j_sub_p:ref intpc =
variant y
ref byte p,q
int elemsize

y:=sptr++
p:=sptr^.uref.ptr
q:=y^.uref.ptr

case elemsize:=ttsize[sptr^.uref.elemtag]
when 1 then sptr^.value:=p-q
when 2 then sptr^.value:=(p-q)>>1
when 4 then sptr^.value:=(p-q)>>2
else sptr^.value:=(p-q)/elemsize
esac

sptr^.tagx:=tint
return pcptr+1
end

global function j_sub_j:ref intpc =
ref  varrec x,y
varrec result

y:=sptr++
x:=sptr

bx_sub(x,y,&result)
pc_unshare(x)
pc_unshare(y)
sptr^:=result
!bx_reduce(sptr)
return pcptr+1
end

global function j_sub_e:ref intpc =
ref varrec x,y
y:=sptr
x:=++sptr
dx_subset(x,y)
return pcptr+1
end

global function j_sub_z:ref intpc =
pcmxtypes("sub_def",sptr+1,sptr)
return pcptr+1
end

global function j_sub_zz:ref intpc =
variant y
int yt

y:=sptr++
yt:=y^.tag

switch sptr^.tag
when trefpacked then
	switch (yt)
	when tint then
		sptr^.uref.ptr:=sptr^.uref.ptr-y^.value*ttsize[sptr^.uref.elemtag]
		return pcptr+1
	endswitch
when trefvar then
	switch (yt)
	when tint then
		sptr^.varptr:=sptr^.varptr-y^.value
		return pcptr+1
	endswitch
endswitch


pcmxtypes("sub/mixed_def",sptr,y)
return pcptr+1
end

function j_mixed_iw_wi:ref intpc=
sptr^.tag:=tint

return opc_tableptr^[tint]^()
end

function j_mixed_ir:ref intpc=
variant x

x:=sptr+1
x^.tag:=treal
x^.xvalue:=x^.value
return opc_tableptr^[treal]^()
end

function j_mixed_ri:ref intpc=
sptr^.tag:=treal
sptr^.xvalue:=sptr^.value
return opc_tableptr^[treal]^()
end

function j_mixed_ij:ref intpc=
bx_makeint((sptr+1)^.value,sptr+1)
return opc_tableptr^[tbignum]^()
end

function j_mixed_ji:ref intpc=
bx_makeint(sptr^.value,sptr)
return opc_tableptr^[tbignum]^()
end

global function j_mul_i_w:ref intpc =
variant y

y:=sptr++
sptr^.value:=sptr^.value*y^.value
return pcptr+1
end

global function j_mul_r:ref intpc =
variant y

y:=sptr++
sptr^.xvalue*:=y^.xvalue
return pcptr+1
end

global function j_mul_j:ref intpc =
ref  varrec x,y
varrec result

y:=sptr++
x:=sptr

bx_mul(x,y,&result)
pc_unshare(x)
pc_unshare(y)
sptr^:=result
!bx_reduce(sptr)
return pcptr+1
end

global function j_mul_z:ref intpc =
pcmxtypes("mul_def",sptr+1,sptr)
return pcptr+1
end

global function j_mul_li:ref intpc=
variant x,y
y:=sptr
x:=++sptr

pc_mul_listi(x,y,sptr)

return pcptr+1
end

global function j_mul_si:ref intpc=
variant x,y
y:=sptr
x:=++sptr

pc_mul_stri(x,y,sptr)
return pcptr+1
end

global function j_mul_e:ref intpc =
return j_iand_e()
end

global function j_mul_zz:ref intpc=
pcmxtypes("mul/zz",sptr+1,sptr)
return pcptr+1
end

global function j_div_i:ref intpc =
variant y

y:=sptr++
sptr^.xvalue:=real(sptr^.value)/real(y^.value)
sptr^.tagx:=treal
return pcptr+1
end

global function j_div_r:ref intpc =
variant y

y:=sptr++
sptr^.xvalue:=sptr^.xvalue/y^.xvalue
sptr^.tagx:=treal
return pcptr+1
end

global function j_div_j:ref intpc =
ref  varrec x,y
varrec result

y:=sptr++
x:=sptr

bx_div(x,y,&result)
pc_unshare(x)
pc_unshare(y)

sptr^:=result

return pcptr+1
end

global function j_div_z:ref intpc =
pcmxtypes("div_def",sptr+1,sptr)
return pcptr+1
end

global function j_jumple_i:ref intpc =
if (sptr+1)^.value<=sptr^.value then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumple_r:ref intpc =
if (sptr+1)^.xvalue<=sptr^.xvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

!global function j_jumple_j:ref intpc =
!pcmxtypes("jumple_longint",sptr+1,sptr)
!return pcptr+1
!end

global function j_jumple_s:ref intpc =
variant x,y
int res
object px,py

y:=sptr++
x:=sptr++

px:=x^.objptr
py:=y^.objptr

res:=cmpstring_len(px.ustr.strptr,py.ustr.strptr,px.ustr.length,py.ustr.length)

pc_unshare(x)
pc_unshare(y)

if res<=0 then
	return ref intpc(getopnda)
fi 
return pcptr+2
end

global function j_jumple_z:ref intpc =
variant x,y
int res

y:=sptr++
x:=sptr++

res:=pc_compare(x,y)
if res<=0 then
	return ref intpc(getopnda)
fi 
return pcptr+2
end

global function j_jumpeq_i_r_t_o:ref intpc =  !************************
!assumes that int and real are both 8 bytes
if (sptr+1)^.value=sptr^.value then
!if (sptr+1)^.xvalue=sptr^.xvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpeq_v_p_f_g:ref intpc =  !*************************
if (sptr+1)^.refptr=sptr^.refptr then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpeq_s:ref intpc =  !**************************
variant x,y
int res

y:=sptr++
x:=sptr++

res:=pc_eqstring(x,y)
if res then
	return ref intpc(getopnda)
fi
return pcptr+2
end

global function j_jumpeq_z:ref intpc =  !*****************************
int res
variant x,y

y:=sptr++
x:=sptr++

res:=pc_equal(x,y,0)
if res then
	return ref intpc(getopnda)
fi
return pcptr+2
end

global function j_jumpeq_zz:ref intpc =
int res
variant x,y
return j_jumpeq_z()
end

global function j_jumpne_i_r_t_o:ref intpc =
!assumes that int and real are both 8 bytes
if (sptr+1)^.value<>sptr^.value then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpne_v_p_f_g:ref intpc =
if (sptr+1)^.refptr<>sptr^.refptr then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpne_s:ref intpc =
variant x,y
object px,py
int res,n

y:=sptr++
x:=sptr++
px:=x^.objptr
py:=y^.objptr

res:=0						!assume different

	n:=px.ustr.length

	if n<>py.ustr.length then
	elsif n=0 then
		res:=1					!same zero length
	else
		if cmpstringn(px.ustr.strptr,py.ustr.strptr,n)=0 then
			res:=1
		fi
	fi
!fi

pc_unshare(x)
pc_unshare(y)

if res=0 then
	return ref intpc(getopnda)
fi
return pcptr+2
end

global function j_jumpne_z:ref intpc =
int res
variant x,y

y:=sptr++
x:=sptr++

res:=pc_equal(x,y,0)
if not res then
	return ref intpc(getopnda)
fi

return pcptr+2
end

global function j_jumpne_zz:ref intpc =
int res
variant x,y
return j_jumpne_z()
end

global function j_jumpge_i:ref intpc =
if (sptr+1)^.value>=sptr^.value then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpge_w:ref intpc =
if (sptr+1)^.uvalue>=sptr^.uvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpge_r:ref intpc =
if (sptr+1)^.xvalue>=sptr^.xvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpge_z:ref intpc =
variant x,y
int res

y:=sptr++
x:=sptr++

res:=pc_compare(x,y)
if res>=0 then
	return ref intpc(getopnda)
fi 
return pcptr+2
end

global function j_jumpgt_i:ref intpc =
if (sptr+1)^.value>sptr^.value then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpgt_w:ref intpc =
if (sptr+1)^.uvalue>sptr^.uvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpgt_r:ref intpc =
if (sptr+1)^.xvalue>sptr^.xvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumpgt_z:ref intpc =
variant x,y
int res

y:=sptr++
x:=sptr++

res:=pc_compare(x,y)
if res>0 then
	return ref intpc(getopnda)
fi 
return pcptr+2
end

global function j_jumplt_i:ref intpc =  !*****************************
if (sptr+1)^.value<sptr^.value then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumplt_v_p:ref intpc =  !*****************************
if (sptr+1)^.refptr<sptr^.refptr then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumplt_r:ref intpc =  !****************************
if (sptr+1)^.xvalue<sptr^.xvalue then
	sptr+:=2
	return ref intpc(getopnda)
fi 
sptr+:=2
return pcptr+2
end

global function j_jumplt_z:ref intpc =  !*****************************
variant x,y
int res

y:=sptr++
x:=sptr++

res:=pc_compare(x,y)
if res<0 then
	return ref intpc(getopnda)
fi 
return pcptr+2
end

global function j_pushix_li:ref intpc =
object p
varrec va
int index,length,acopy

va:=(sptr+1)^
p:=va.objptr
index:=sptr.value-p.ulist.lower

if u32(index)>=u32(p.ulist.length) then		!bounds error
	pcerror("list[int] bounds")
fi

(++sptr)^:=(p.ulist.vptr+index)^
if sptr^.hasref then
	++(sptr^.objptr.ulist.refcount)
fi
pc_unshare(&va) when va.hasref

return pcptr+1
end

global function j_pushix_mi:ref intpc =
object p
varrec va
int index,length,acopy

va:=(sptr+1)^
p:=va.objptr
index:=sptr.value-1

if u32(index)>=u32(ttlength[va.tag]) then		!bounds error
	pcerror("rec[int] bounds")
fi

(++sptr)^:=(p.urec.vptr+index)^
if sptr^.hasref then
	++(sptr^.objptr.refcount)
fi
pc_unshare(&va) !when va.hasref

return pcptr+1
end

global function j_pushix_vi:ref intpc =
object p
int index,length

index:=sptr^.value
++sptr

(sptr)^:=(sptr^.varptr+index)^

return pcptr+1
end

global function j_pushix_ln:ref intpc =
!create a slice of a list
varrec v

variant a,x
int i,j,alower,ahasref
object p,q

x:=sptr++			!x is the range
a:=sptr				!a is the list
p:=a^.objptr

i:=x.range_lower
j:=x.range_upper

alower:=p.ulist.lower

if i<alower or j>p.ulist.length+alower-1 or i>j then
!	cpl i,j,alower,p^.length+alower-1
	pcerror("list/slice bounds")
fi

sptr^.tagx:=tlist ior hasrefmask

q:=obj_new(tlist)
sptr.objptr:=q

q.objtype:=slice_obj
q.ulist.mutable:=p.ulist.mutable
q.ulist.lower:=1

case p.objtype
when slice_obj then				!slice of a slice!
	q.ulist.objptr2:=p.ulist.objptr2		!link to original
	++(q.objptr2.ulist.refcount)
	q.ulist.vptr:=p.ulist.vptr+i-alower
	v.tagx:=tlist ior hasrefmask
	v.objptr:=p
	pc_unshare(&v)
when extslice_obj then
	q.objptr2:=nil
	q.objtype:=extslice_obj
	q.ulist.vptr:=p.ulist.vptr+i-alower
else
	q.ulist.objptr2:=p				!link to original
	q.ulist.vptr:=p.ulist.vptr+i-alower
esac
q.ulist.length:=j-i+1

return pcptr+1
end

global function j_pushix_ll:ref intpc =
junimpl("pushix_listlist")
return pcptr+1
end

global function j_pushix_le:ref intpc =
junimpl("pushix_list_set")
return pcptr+1
end

global function j_pushix_ai:ref intpc =
object p
variant a
int index,length,elemtype
ref byte q
varrec va

va:=(sptr+1)^
p:=va.objptr
index:=sptr.value-p.uarray.lower

if u32(index)>=u32(p.uarray.length) then		!bounds error
	pcerror("ax[int] bounds")
fi

if (elemtype:=p.uarray.elemtag)=tu8 then
	++sptr
	sptr.value:=(p.uarray.ptr+index)^
	sptr^.tagx:=tint
else
	pc_loadpacked(p.uarray.ptr+index*ttsize[elemtype],elemtype,++sptr,p)
fi

!NOTE: A AND SPTR POINT TO THE SAME PLACE
pc_unshare(&va) when va.hasref

return pcptr+1
end

global function j_pushix_bi_ei:ref intpc =
object p
variant a
int index,length,elemtype,offset,shift
ref byte q
varrec va

va:=(sptr+1)^
p:=va.objptr
index:=sptr^.value-p.ubits.lower
!offset:=p.ubits.bitoffset-1
offset:=p.ubits.bitoffset
q:=p.ubits.ptr

if u32(index)>=u32(p.ubits.length) then		!bounds error
	pcerror("bits[int] bounds")
fi

++sptr
sptr.tagx:=tint

switch p.ubits.elemtag
when tu1 then
	index+:=offset
	sptr.value:=not not ((q+index>>3)^ iand (1<<(index iand 7)))
when tu2 then
	index+:=offset>>1
	shift:=(index iand 3)*2
	sptr.value:=((q+index>>2)^ iand (3<<shift))>>shift
when tu4 then
	index+:=offset>>2
	shift:=(index iand 1)*4
	sptr.value:=((q+index>>1)^ iand (15<<shift))>>shift
else
	pcustypet("bitix",p.ubits.elemtag)
end

pc_unshare(&va) when va.hasref

return pcptr+1
end

global function j_pushix_an:ref intpc =
varrec v
variant a,x
int i,j,value,offset
object p,q

x:=sptr++
a:=sptr
!acopy:=a.copy
p:=a.objptr

i:=x.range_lower
j:=x.range_upper

if i<p.uarray.lower or j>p.uarray.length or i>j then
	pcerror("ax[slice] bounds")
fi

sptr.tagx:=a.tagx

q:=obj_new(a.tagx)
sptr.objptr:=q

q.objtype:=slice_obj
q.uarray.mutable:=p.uarray.mutable
q.uarray.elemtag:=p.uarray.elemtag
q.uarray.lower:=p.uarray.lower

offset:=(i-p.uarray.lower)*ttsize[p.uarray.elemtag]

case p.objtype
when slice_obj then				!slice of a slice!
	q.uarray.objptr2:=p.uarray.objptr2		!link to original
	++(q.objptr2.refcount)
	q.uarray.ptr:=p.uarray.ptr+offset
	v.tagx:=tstring ior hasrefmask
	v.objptr:=p
	pc_unshare(&v)
when extslice_obj then
	q.uarray.objptr2:=nil
	q.objtype:=extslice_obj
	q.uarray.ptr:=p.uarray.ptr+offset
else
	q.uarray.objptr2:=p				!link to original
	q.uarray.ptr:=p.uarray.ptr+offset
esac

q.uarray.length:=j-i+1

return pcptr+1
end

global function j_pushix_si:ref intpc =
varrec v
variant a,x
int index,length,i
object p,q

x:=sptr++
a:=sptr
!acopy:=a^.copy
p:=a.objptr

i:=x.value-1

if u32(i)>=u32(p.ustr.length) then
	pcerror("string[int] bounds")
fi

sptr.tagx:=tstring ior hasrefmask

q:=obj_new(tstring)
sptr.objptr:=q

q.objtype:=slice_obj
q.ustr.mutable:=p.ustr.mutable

case p^.objtype
when slice_obj then				!slice of a slice!
	q.ustr.objptr2:=p.ustr.objptr2		!link to original
	++(q.objptr2.refcount)
	q.ustr.strptr:=p.ustr.strptr+i
	v.tagx:=tstring ior hasrefmask
	v.objptr:=p
	pc_unshare(&v)
when extslice_obj then
	q.ustr.objptr2:=nil
	q.objtype:=extslice_obj
	q.ustr.strptr:=p.ustr.strptr+i
else
	q.ustr.objptr2:=p				!link to original
	q.ustr.strptr:=p.ustr.strptr+i
esac

q.ustr.length:=1

return pcptr+1
end

global function j_pushix_sn:ref intpc =
return j_pushdotix_sn()
end

global function j_pushix_bn:ref intpc =
varrec v
variant a,x
int i,j,value,bitoffset
object p,q
ref byte pbits

x:=sptr++
a:=sptr
p:=a.objptr

i:=x.range_lower
j:=x.range_upper

if i<p.ubits.lower or j>p.ubits.length or i>j then
	pcerror("bits[slice] bounds")
fi

sptr.tagx:=a.tagx

q:=obj_new(tbits)

sptr.objptr:=q

q.objtype:=slice_obj
q.ubits.mutable:=p.ubits.mutable
q.ubits.elemtag:=p.ubits.elemtag
q.ubits.lower:=p.ubits.lower

q.ubits.ptr:=cast(getbitoffset(p.ubits.ptr,p.ubits.bitoffset,(i-p.ubits.lower),
		 p.ubits.elemtag, q.ubits.bitoffset))

++q.ubits.bitoffset					!is 1-based

case p.objtype
when slice_obj then				!slice of a slice!
	q.ubits.objptr2:=p.ubits.objptr2		!link to original
	++(q.objptr2.refcount)
	v.tagx:=tstring ior hasrefmask
	v.objptr:=p
	pc_unshare(&v)
when extslice_obj then
	q.ubits.objptr2:=nil
	q^.objtype:=extslice_obj
else
	q.ubits.objptr2:=p				!link to original
esac

q.ubits.length:=j-i+1

return pcptr+1
end

global function j_pushix_di:ref intpc =
!get a key:value pair indexing directly into a dict
varrec v

variant a,x
int i,j,alower,ahasref
object p,q

x:=sptr++			!x is the index
a:=sptr				!a is the dict
p:=a.objptr

i:=x.value
j:=x.range_upper

alower:=p.udict.lower

if i<1 or j>p.udict.allocated then
	pcerror("dict[] bounds")
fi

sptr^.tagx:=tlist ior hasrefmask

q:=obj_new(tdict)
sptr.objptr:=q

q.objtype:=slice_obj
q.udict.lower:=1
q.udict.length:=2

q.udict.objptr2:=p				!link to original
q.udict.vptr:=p.udict.vptr+(i-1)*2

return pcptr+1
end

global function j_pushix_zz:ref intpc =
variant sptr1
pcmxtypes("pushix/def",sptr+1,sptr)
return nil
end

global function j_pushixref_li:ref intpc =
object p
variant a
int index

!CPL "PUSHIXREF/LI"

a:=(sptr+1)^.varptr
!CPL "PX01",A
p:=a^.objptr
!CPL "PX02"

unless p.ulist.mutable then
!PCERROR("PUSHIXREF/L/COW")
	p:=a^.objptr:=copyonwrite(p,tlist)
end

!CPL "PX1"

index:=sptr.value-p.ulist.lower
!CPL "PX2"

if u32(index)>=u32(p.ulist.length) then				!outside current bounds
!CPL "PX3"
	if index<0 then
		PCERROR("LWB")
	else
		if u32(index)=u32(p.ulist.length) and a.tag=tlist then
			pc_iappendlist(a,nil)
			p:=a.objptr
		else
			pcerror("&pushix list[i] bounds")
		fi
	fi
fi
!CPL "PX4"

(++sptr)^.tagx:=trefvar
!CPL "PX5"
sptr.varptr:=p.ulist.vptr+index
!CPL "PX7"

return pcptr+1
end

global function j_pushixref_ln:ref intpc =
junimpl("pushixref_list_range")
return pcptr+1
end

global function j_pushixref_ai:ref intpc =
int index,elemtype,elemsize
object p
variant a

a:=(sptr+1)^.varptr
p:=a.objptr
index:=sptr.value-p.uarray.lower

if u32(index)>=u32(p.uarray.length) then				!outside current bounds
	if index<0 then
		PCERROR("&AXLWB")
	else
		if u32(index)=u32(p.uarray.length) then
			pc_iappendarray(a,nil)
			p:=a.objptr
		else
			PCERROR("&AXBOUNDS")
		fi
	fi
fi

elemtype:=p.uarray.elemtag

++sptr

sptr.tagx:=trefpacked
sptr.uref.elemtag:=elemtype
sptr.uref.ptr:=p.uarray.ptr+index*ttsize[elemtype]
return pcptr+1
end

global function j_pushixref_si:ref intpc =
variant a
int index,newlength,newbytes
object pa

a:=(sptr+1)^.varptr
pa:=a.objptr
index:=sptr.value-1
if not pa.ustr.mutable then
!PCERROR("PUSHIXREF/S/COW")
	a^.objptr:=pa:=copyonwrite(pa,tstring)
fi

if u32(index)>=u32(pa.ustr.length) then				!outside current bounds
	pcerror("&str[int] bounds")
fi

(++sptr)^.tagx:=trefpacked
sptr^.uref.elemtag:=tstring
sptr^.uref.ptr:=ref byte(pa.ustr.strptr)+index

return pcptr+1
end

!global function j_pushixref_bi_ei:ref intpc =
global function j_pushixref_bi:ref intpc =
int index,elemtype,elemsize,offset
object p
variant a

a:=(sptr+1)^.varptr
p:=a.objptr
index:=sptr.value-p.ubits.lower

if u32(index)>=u32(p.ubits.length) then				!outside current bounds
	if index<0 then
		PCERROR("&BITSLWB")
	else
		if u32(index)=u32(p.ubits.length) then
			pc_iappendbits(a,nil)
			p:=a.objptr
		else
			PCERROR("&BITSBOUNDS")
		fi
	fi
fi

elemtype:=p.ubits.elemtag

++sptr

sptr.tagx:=trefbit
sptr.uref.elemtag:=elemtype
!sptr.uref.ptr:=cast(getbitoffset(cast(p.ubits.ptr),p.ubits.bitoffset-1,index, elemtype,
sptr.uref.ptr:=cast(getbitoffset(cast(p.ubits.ptr),p.ubits.bitoffset,index, elemtype,
		 sptr.uref.bitoffset))
sptr.uref.bitlength:=0

return pcptr+1
end

global function j_pushixref_zz:ref intpc =
variant p,sptr1
PCERROR("JPUSHIXREF/DEF")

return nil
end

global function j_pushdotix_si:ref intpc =  !*******************
variant a,x
int index,value
object p

x:=sptr++
a:=sptr
p:=a^.objptr
index:=x^.value-1

if u32(index)>=u32(p.ustr.length) then	!bounds error
cpl =index,=p.ustr.length
	pcerror("string.[int] bounds")
fi
value:=(p.ustr.strptr+index)^

pc_unshare(a) when a.hasref

sptr.tagx:=tint
sptr.value:=value
return pcptr+1
end

global function j_pushdotix_sn:ref intpc =  !*******************
varrec v
variant a,x
int i,j,value
object p,q

x:=sptr++
a:=sptr
p:=a.objptr

i:=x.range_lower
j:=x.range_upper

if i<1 or j>p.ustr.length or i>j then
!CPL =I,=J,=P^.LENGTH
	pcerror("string[slice] bounds")
fi

sptr^.tagx:=tstring ior hasrefmask

q:=obj_new(tstring)
sptr.objptr:=q

q.objtype:=slice_obj
q.ustr.mutable:=p.ustr.mutable

case p^.objtype
when slice_obj then				!slice of a slice!
	q.ustr.objptr2:=p.ustr.objptr2		!link to original
	++(q.objptr2.refcount)
	q.ustr.strptr:=p.ustr.strptr+i-1
	v.tagx:=tstring ior hasrefmask
	v.objptr:=p
	pc_unshare(&v)
when extslice_obj then
	q.ustr.objptr2:=nil
	q.objtype:=extslice_obj
	q.ustr.strptr:=p.ustr.strptr+i-1
else
	q.ustr.objptr2:=p				!link to original
	q.ustr.strptr:=p.ustr.strptr+i-1
esac

q.ustr.length:=j-i+1

return pcptr+1
end

global function j_pushdotix_ii:ref intpc =  !**********************
int index

index:=sptr^.value
++sptr
if index<0 or index>=64 then
	pcerror("int.[int] bounds")
fi

sptr^.value:=(sptr^.value>>index) iand 1
return pcptr+1
end

global function j_pushdotix_in:ref intpc =  !**********************
int i,j

i:=sptr^.range_lower
j:=sptr^.range_upper
++sptr

if j<i then
	swap(i,j)
fi

if i>=64 or j>=64 then
	pcerror("int.[slice] bounds")
fi

sptr^.value:=(sptr^.value>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
return pcptr+1
end

global function j_pushdotix_ei:ref intpc =  !**********************
return j_pushix_bi_ei()
!return pcptr+1
end

global function j_pushdotix_mi:ref intpc =  !**********************
!return j_pushix_li_mi()
return j_pushix_mi()
end

global function j_pushdotix_zz:ref intpc =  !**********************
pcmxtypes("pushdotix/def",sptr+1,sptr)
return nil
end

global function j_pushdotixref_si:ref intpc =  !********************
variant a
int index,newlength,newbytes
object pa

a:=(sptr+1)^.varptr
pa:=a^.objptr
index:=sptr^.value-1

if u32(index)>=u32(pa.ustr.length) then				!outside current bounds
	pcerror("&str.[int] bounds")
fi

(++sptr)^.tagx:=trefpacked
sptr.uref.elemtag:=tu8
sptr.uref.ptr:=ref byte(pa.ustr.strptr)+index

return pcptr+1
end

global function j_pushdotixref_sn:ref intpc =  !********************
variant a,x
ichar p				!string data
int i,j,length
PCERROR("JPUSHDOTIXREF/SRANGE")

return pcptr+1
end

global function j_pushdotixref_ii:ref intpc =  !***********************
int index
ref byte p

!CPL "PUSHDOTIXREF/INT/INT"
index:=sptr^.value
++sptr
if index<0 or index>=64 then
	pcerror("int.[int] bounds")
fi

p:=sptr^.uref.ptr		!point to start of int var

sptr^.tagx:=trefbit
sptr^.uref.elemtag:=tu1
sptr^.uref.bitoffset:=index iand 7
sptr^.uref.ptr:=p+8+index>>3
sptr^.uref.bitlength:=0

return pcptr+1
end

global function j_pushdotixref_in_wn:ref intpc =  !***********************
int i,j
!var ref int64 p

!CPL "PUSHDOTIXREF/INT/RANGE"
i:=sptr^.range_lower
j:=sptr^.range_upper
if i>j then swap(i,j) fi

!CPL =I,=J
!PCERROR("NOT READY")

++sptr
if i<0 or j>=64 then
	pcerror("int.[slice] bounds")
fi

!p:=sptr^.dptr		!point to start of int var

sptr^.tagx:=trefbit
sptr^.uref.bitlength:=j-i+1
sptr^.uref.elemtag:=tu1
sptr^.uref.bitoffset:=i
++sptr^.uref.ptr64				!step 8 bytes past start of var

return pcptr+1
end

global function j_pushdotixref_ei:ref intpc =  !***********************
PCERROR("PUSHDOTIXREF/SET")
!return j_pushixref_bi_ei()
end

global function j_pushdotixref_zz:ref intpc =  !**********************
pcmxtypes("ZZpushdotixref/def",sptr+1,sptr)
return nil
end

global function j_addto_i:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value+:=y^.value
return pcptr+1
end

global function j_addto_r:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue+:=y^.xvalue
return pcptr+1
end

global function j_addto_s:ref intpc =  !********************************
int xlen,ylen,newlen
object px,py
variant x,y

y:=sptr++
x:=sptr^.varptr

++sptr

px:=x.objptr
py:=y.objptr

if not px.ustr.mutable then
	x.objptr:=px:=copyonwrite(px,tstring)
fi
if px.objtype<>normal_obj then
	pcerror("extending string slice")
fi

xlen:=px.ustr.length
ylen:=py.ustr.length

if xlen=0 then
	if ylen then		!copy y to x (else x is unchanged)
		px.ustr.strptr:=pcm_alloc(ylen)
		px.ustr.allocated:=allocbytes
		px.ustr.length:=ylen
		memcpy(px.ustr.strptr,py.ustr.strptr,ylen)
	fi
elsif ylen=1 then			!x+:=y when y is a single character
	if ++xlen>px.ustr.allocated then
		string_resize(px,xlen)
	fi
	px.ustr.length:=xlen
	(px.ustr.strptr+xlen-1)^:=py.ustr.strptr^
elsif ylen then			!x+:=y; ylen>1 neither are empty (else leave x alone)
	newlen:=xlen+ylen
	if newlen>px.ustr.allocated then
		string_resize(px,newlen)
	fi
	px.ustr.length:=newlen
	memcpy(px.ustr.strptr+xlen,py.ustr.strptr,ylen)
fi

pc_unshare(y)

return pcptr+1
end

global function j_addto_p:ref intpc =  !********************************
variant x,y
!y:=sptr++
!x:=sptr^.vptr
!++sptr
!x^.value+:=y^.value

PCERROR("ADDTO REF PACK")

return pcptr+1
end

global function j_addto_z:ref intpc =  !********************************
int xlen,ylen,newlen
object px,py
variant x,y
varrec result
 
y:=sptr++
x:=sptr^.varptr

++sptr

px:=x.objptr
py:=y.objptr

case y^.tag
when tbignum then
	bx_add(x,y,&result)
	pc_unshare(x)
	pc_unshare(y)
	x^:=result
else
	pcmxtypes("addto_def",x,y)
esac
return pcptr+1
end

global function j_addto_si:ref intpc =  !********************************
int ch
variant x,y
int xlen
object px

y:=sptr++
x:=sptr.varptr
++sptr
ch:=y.value

px:=x.objptr

if not px.ustr.mutable then
!PCERROR("ADDTO/SI/COW")

	x^.objptr:=px:=copyonwrite(px,tstring)
fi

xlen:=px.ustr.length

if xlen=0 then
!	px.ustr.vptr:=make_arraydata(1,tu8,px.ustr.allocated)
	px.ustr.strptr:=pcm_alloc(1)
	px.ustr.allocated:=allocbytes
	px.ustr.length:=1
	px.ustr.strptr^:=ch
else
	if ++xlen>px.ustr.allocated then
!PCERROR("ADDTO/SI/RESIZE")
		string_resize(px,xlen)
	fi
	px.ustr.length:=xlen
	(px.ustr.strptr+xlen-1)^:=ch
fi


return pcptr+1
end

global function j_addto_ir:ref intpc =  !********************************
variant x,y
static int oldreg,newreg

y:=sptr++
x:=sptr^.varptr
++sptr
x^.value+:=y^.xvalue

return pcptr+1
end

global function j_addto_ri:ref intpc =  !********************************
variant x,y

y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue+:=y^.value
return pcptr+1
end

global function j_addto_zz:ref intpc =  !********************************
pcmxtypes("addto_zz",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_subto_i:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value-:=y^.value
return pcptr+1
end

global function j_subto_r:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue-:=y^.xvalue
return pcptr+1
end

global function j_subto_z:ref intpc =  !********************************
pcmxtypes("subto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_subto_ir:ref intpc =  !********************************
variant x,y

y:=sptr++
x:=sptr^.varptr
++sptr
x^.value-:=y^.xvalue
return pcptr+1
end

global function j_subto_ri:ref intpc =  !********************************
variant x,y

y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue-:=y^.value
return pcptr+1
end

global function j_subto_zz:ref intpc =  !********************************
pcmxtypes("subto_zz",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_multo_i_w:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value*:=y^.value
return pcptr+1
end

global function j_multo_r:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue*:=y^.xvalue
return pcptr+1
end

global function j_multo_z:ref intpc =  !********************************
pcmxtypes("multo_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_divto_i_w:ref intpc =  !********************************
variant x,y
pcerror("divto int?")
!y:=sptr++
!x:=sptr^.varptr
!++sptr
!x^.value/:=y^.value
return pcptr+1
end

global function j_divto_r:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue/:=y^.xvalue
return pcptr+1
end

global function j_divto_z:ref intpc =  !********************************
pcmxtypes("divto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_idivto_i:ref intpc =  !********************************
variant y
pcustype("idivto",sptr)
!y:=sptr++
!sptr^.value+:=y^.value
return pcptr+1
end

global function j_idivto_z:ref intpc =  !********************************
pcmxtypes("idivto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_iand_i_w:ref intpc =  !*******************************
variant y

y:=sptr++
sptr^.value iand:=y^.value
return pcptr+1
end

global function j_iand_e:ref intpc =  !*******************************
variant x,y

y:=sptr
x:=++sptr
dx_iandset(x,y)
return pcptr+1
end

global function j_iand_z:ref intpc =  !*******************************
pcmxtypes("iand_def",sptr+1,sptr)
return pcptr+1
end

global function j_ior_i_w:ref intpc =  !********************************
variant y

y:=sptr++
sptr^.value ior:=y^.value
return pcptr+1
end

global function j_ior_e:ref intpc =  !********************************
variant x,y

return j_add_e()
end

global function j_ior_z:ref intpc =  !********************************
pcmxtypes("ior_def",sptr+1,sptr)
return pcptr+1
end

global function j_ixor_i_w:ref intpc =  !*******************************
variant y

y:=sptr++
sptr^.value ixor:=y^.value
return pcptr+1
end

global function j_ixor_e:ref intpc =  !*******************************
variant x,y

y:=sptr++
x:=sptr

dx_ixorset(x,y)
return pcptr+1
end

global function j_ixor_z:ref intpc =  !*******************************
pcmxtypes("ixor_def",sptr+1,sptr)
return pcptr+1
end

global function j_iandto_i_w:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value iand:=y^.value
return pcptr+1
end

global function j_iandto_z:ref intpc =  !********************************
pcmxtypes("iandto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_iorto_i_w:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value ior:=y^.value
return pcptr+1
end

global function j_iorto_z:ref intpc =  !********************************
pcmxtypes("iorto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_ixorto_i_w:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value ixor:=y^.value
return pcptr+1
return pcptr+1
end

global function j_ixorto_z:ref intpc =  !********************************
pcmxtypes("ixorto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_shlto_i:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value:=x^.value<<y^.value
return pcptr+1
end

global function j_shlto_z:ref intpc =  !********************************
pcmxtypes("shlto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_shrto_i:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value:=x^.value>>y^.value
return pcptr+1
end

global function j_shrto_z:ref intpc =  !********************************
pcmxtypes("shrto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_concat_s:ref intpc =  !**************************
return j_add_s()
end

global function j_concat_l:ref intpc =  !**************************
variant x,y
y:=sptr
x:=++sptr

pc_duplvar(x)				!x needs to be owner of list
pc_iconcatlist(x,y)

pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_concat_z:ref intpc =  !*****************************
pcmxtypes("concat_def",sptr+1,sptr)
return pcptr+1
end

global function j_concatto_s:ref intpc =  !**************************
junimpl("concatto_string")
return pcptr+1
end

global function j_concatto_l:ref intpc =  !**************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr

pc_iconcatlist(x,y)
pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_concatto_z:ref intpc =  !*****************************
pcmxtypes("concatto_def",sptr+1,sptr)
return pcptr+1
end

global function j_append_s:ref intpc =  !**************************
if sptr^.tag<>tstring then
	pcustype("append/s",sptr)
fi
return j_add_s()
end

global function j_append_l:ref intpc =  !****************************
variant x,y
y:=sptr
x:=++sptr

pc_duplvar(x)				!x needs to be owner of list

pc_iappendlist(x,y)

pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_append_a:ref intpc =  !***************************
variant x,y
y:=sptr
x:=++sptr

pc_duplvar(x)				!x needs to be owner of list

pc_iappendarray(x,y)

pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_append_b:ref intpc =  !***************************
variant x,y
y:=sptr
x:=++sptr

pc_duplvar(x)				!x needs to be owner of list

pc_iappendbits(x,y)

return pcptr+1
end

global function j_append_z:ref intpc =  !*****************************
pcmxtypes("append_def",sptr+1,sptr)
return pcptr+1
end

global function j_appendto_s:ref intpc =  !**************************
return j_addto_s()
end

global function j_appendto_l:ref intpc =  !****************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr

pc_iappendlist(x,y)
!pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_appendto_a:ref intpc =  !***************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr

pc_iappendarray(x,y)
!pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_appendto_b_e:ref intpc =  !***************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr

pc_iappendbits(x,y)
!pc_unshare(y) when y^.hasref

return pcptr+1
end

global function j_appendto_z:ref intpc =  !*****************************
pcmxtypes("appendto_def",sptr+1,sptr)
return pcptr+1
end

global function j_max_i:ref intpc =
variant y

y:=sptr++
sptr^.value max:=y^.value
return pcptr+1
end

global function j_max_r:ref intpc =
variant y


y:=sptr++
sptr^.xvalue max:=y^.xvalue
return pcptr+1
end

global function j_max_z:ref intpc =
int res
variant y

y:=sptr++

res:=pc_compare_nf(sptr,y)

if res>=0 then			!keep x
	pc_unshare(y) when y.hasref
else					!use y
	pc_unshare(sptr) when sptr.hasref
	sptr^:=y^
fi

return pcptr+1
end

global function j_min_z:ref intpc =
int res
variant y

y:=sptr++
res:=pc_compare_nf(sptr,y)

if res<=0 then			!keep x
	pc_unshare(y) when y.hasref
else					!use y
	pc_unshare(sptr) when sptr.hasref
	sptr^:=y^
fi

return pcptr+1
end

global function j_len_l_a_e_s_b_d:ref intpc =		!*****************************
int length

length:=sptr.objptr.ulist.length
pc_unshare(sptr)	! when sptr^.hasref

sptr^.tagx:=tint
sptr^.value:=length
return pcptr+1
end

global function j_len_m_k:ref intpc =		!*****************************
int length

length:=ttlength[sptr.tag]
pc_unshare(sptr)	!when sptr^.hasref

sptr^.tagx:=tint
sptr^.value:=length
return pcptr+1
end

global function j_len_n:ref intpc =  !******************************
sptr^.tagx:=tint
sptr^.value:=sptr^.range_upper-sptr^.range_lower+1
return pcptr+1
end

global function j_len_z:ref intpc =  !********************************
case sptr^.tag
when tbignum then
	sptr^.value:=bx_length(sptr)
when tdict then
	sptr^.value:=sptr^.objptr.udict.allocated/2
else
	pcustype("len_def",sptr)
esac
sptr^.tagx:=tint
return pcptr+1
end

global function j_lwb_l:ref intpc =  !*********************
int n
object p

p:=sptr.objptr
n:=p.ulist.lower
pc_unshare(sptr)

sptr.tagx:=tint
sptr.value:=n

return pcptr+1
end

global function j_lwb_a_b:ref intpc =  !*********************
int n
object p

p:=sptr.objptr
n:=p.uarray.lower
pc_unshare(sptr)

sptr.tagx:=tint
sptr.value:=n

return pcptr+1
end

global function j_lwb_s_m_k_d:ref intpc =  !*********************
pc_unshare(sptr)

sptr.tagx:=tint
sptr.value:=1

return pcptr+1
end

global function j_lwb_e:ref intpc =  !*********************
object p

pc_unshare(sptr)

sptr^.tagx:=tint
sptr^.value:=0

return pcptr+1
end

global function j_lwb_n:ref intpc =  !******************************
sptr^.value:=sptr^.range_lower
sptr^.tagx:=tint
return pcptr+1
end

global function j_lwb_z:ref intpc =  !********************************
case sptr^.tag
when tint then
	sptr^.value:=0
	sptr^.tagx:=tint
else
	pcustype("lwb_def",sptr)
esac
return pcptr+1
end

global function j_upb_l:ref intpc =  !*********************
int n
object p

p:=sptr^.objptr
n:=word32(p.ulist.length+p.ulist.lower-1)
pc_unshare(sptr)

sptr^.tagx:=tint
sptr^.value:=n

return pcptr+1
end

global function j_upb_a:ref intpc =  !*********************
int n
object p

p:=sptr^.objptr
n:=word32(p.uarray.length+p.uarray.lower-1)
pc_unshare(sptr)

sptr^.tagx:=tint
sptr^.value:=n

return pcptr+1
end

!global function j_upb_s_m_k_d:ref intpc =  !*********************
global function j_upb_s_d:ref intpc =  !*********************
int n

n:=sptr^.objptr.ustr.length
pc_unshare(sptr)

sptr^.tagx:=tint
sptr^.value:=n

return pcptr+1
end

global function j_upb_m_k:ref intpc =  !*********************
int n
n:=ttlength[sptr.tag]
pc_unshare(sptr)

sptr^.tagx:=tint
sptr^.value:=n

return pcptr+1
end

global function j_upb_e:ref intpc =  !*********************
int n

n:=sptr.objptr.uset.length-1
pc_unshare(sptr)

sptr^.tagx:=tint
sptr^.value:=n

return pcptr+1
end

global function j_upb_n:ref intpc =  !******************************
sptr^.value:=sptr^.range_upper
sptr^.tagx:=tint
return pcptr+1
end

global function j_upb_z:ref intpc =  !********************************
case sptr^.tag
when tdict then
	sptr^.value:=sptr.objptr.udict.allocated/2
when tint then
	sptr^.value:=63
else
	pcustype("upb_def",sptr)
esac
sptr^.tagx:=tint
return pcptr+1
end

global function j_bounds_l_a_b_s_e:ref intpc =  !**************************
int a,b
object r

r:=sptr^.objptr

case ttbasetype[sptr^.tag]
when tstring,trecord,tstruct then
	a:=1
when tset then
	a:=0
when tarray,tbits then
	a:=r.uarray.lower

else
	a:=r.ulist.lower
esac
b:=r.ulist.length+a-1

pc_unshare(sptr) when sptr^.hasref

sptr^.tagx:=trange
sptr^.range_lower:=a
sptr^.range_upper:=b
return pcptr+1
end

global function j_bounds_m_k:ref intpc =  !**************************
int b

b:=ttlength[sptr.tag]
pc_unshare(sptr)

sptr^.tagx:=trange
sptr^.range_lower:=1
sptr^.range_upper:=b
return pcptr+1
end

global function j_bounds_n:ref intpc =  !***************************
!bounds are same as the range
return pcptr+1
end

global function j_bounds_z:ref intpc =  !*****************************
pcustype("bounds_def",sptr)
return pcptr+1
end

global function j_minto_i:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value min:=y^.value
return pcptr+1
end

global function j_minto_r:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue min:=y^.xvalue
return pcptr+1
end

global function j_minto_z:ref intpc =  !********************************
pcmxtypes("minto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_maxto_i:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.value max:=y^.value
return pcptr+1
end

global function j_maxto_r:ref intpc =  !********************************
variant x,y
y:=sptr++
x:=sptr^.varptr
++sptr
x^.xvalue max:=y^.xvalue
return pcptr+1
end

global function j_maxto_z:ref intpc =  !********************************
pcmxtypes("maxto_def",(sptr+1)^.varptr,sptr)
return pcptr+1
end

global function j_neg_i_w:ref intpc =  !********************************
sptr^.value:=-sptr^.value
return pcptr+1
end

global function j_neg_r:ref intpc =  !*******************************
sptr^.xvalue:=-sptr^.xvalue
return pcptr+1
end

global function j_neg_j:ref intpc =  !****************************
pc_duplvar(sptr)
bx_negto(sptr)
!sptr^.objptr^.oldbnptr^[0] ixor:=1
return pcptr+1
end

global function j_neg_e:ref intpc =  !****************************
dx_inotset(sptr)
return pcptr+1
end

global function j_neg_z:ref intpc =  !********************************
pcustype("neg_def",sptr)
return pcptr+1
end

global function j_abs_i_w:ref intpc =  !********************************
sptr^.value:=abs sptr^.value
return pcptr+1
end

global function j_abs_r:ref intpc =  !*******************************
sptr^.xvalue:=abs sptr^.xvalue
return pcptr+1
end

global function j_abs_j:ref intpc =  !****************************
pc_duplvar(sptr)
bx_absto(sptr)
!sptr^.objptr^.oldbnptr^[0]:=0
return pcptr+1
end

global function j_abs_z:ref intpc =  !********************************
pcustype("abs_def",sptr)
return pcptr+1
end

global function j_inot_i_w:ref intpc =  !*******************************
sptr^.value:=inot sptr^.value
return pcptr+1
end

global function j_inot_e:ref intpc =  !*******************************
dx_inotset(sptr)
return pcptr+1
end

global function j_inot_z:ref intpc =  !*******************************
pcustype("inot_def",sptr)
return pcptr+1
end

global function j_istrue_i_w_r:ref intpc =  !************************
sptr^.tag:=tint
sptr^.value:=not not sptr^.value
return pcptr+1
end

global function j_istrue_l_a_e_s_b:ref intpc =  !************************
int res

res:=sptr^.objptr.ulist.length<>0
pc_unshare(sptr)
sptr^.tagx:=tint
sptr^.value:=res
return pcptr+1
end

global function j_istrue_k_m_h:ref intpc =  !************************
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=tint
sptr^.value:=1
return pcptr+1
end

global function j_istrue_j:ref intpc =  !*************************
junimpl("istrue_longint")
return pcptr+1
end

global function j_istrue_z:ref intpc =  !*****************************
pcustype("istrue_def",sptr)
return pcptr+1
end

global function j_jumpfalse_i_w_r_v_p_f:ref intpc=
if not sptr^.value then
	++sptr
	return ref intpc(getopnda)
fi
++sptr
return pcptr+2
end

global function j_jumpfalse_s_l_e_a_b:ref intpc =  !************
int n

n:=sptr^.objptr.ulist.length
pc_unshare(sptr)
++sptr
if n=0 then
	return ref intpc(getopnda)
fi
return pcptr+2
end

global function j_jumpfalse_z:ref intpc =  !******************************
switch ttbasetype[sptr^.tag]
when trecord,tstruct,trecordlink then			!always false (sometimes can be nil or record)
	++sptr
	return pcptr+2
end
pcustype("jumpfalse_def",sptr)
return pcptr+2
end

global function j_jumptrue_i_r_w_v_p_f:ref intpc =  !*************************
if sptr^.value then
	++sptr
	return ref intpc(getopnda)
fi
++sptr
return pcptr+2
end

global function j_jumptrue_s_l_e_a_b:ref intpc =  !************
int n

n:=sptr^.objptr.ulist.length
pc_unshare(sptr)
++sptr
if n then
	return ref intpc(getopnda)
fi
return pcptr+2
end

global function j_jumptrue_z:ref intpc =  !******************************
switch ttbasetype[sptr^.tag]
when trecord,tstruct,trecordlink then			!always true (sometimes can be nil or record)
	++sptr
	return ref intpc(getopnda)
end

pcustype("jumptrue_def",sptr)
return pcptr+2
end

global function j_shl_i_w:ref intpc =  !********************************
variant y

y:=sptr++
sptr^.value :=sptr^.value<<y^.value
return pcptr+1
end

global function j_shl_z:ref intpc =  !********************************
pcmxtypes("shl_def",sptr+1,sptr)
return pcptr+1
end

global function j_shr_i:ref intpc =  !********************************
variant y

y:=sptr++
sptr^.value :=sptr^.value>>y^.value
return pcptr+1
end

global function j_shr_z:ref intpc =  !********************************
pcmxtypes("shr_def",sptr+1,sptr)
return pcptr+1
end

global function j_shr_wi:ref intpc =  !********************************
variant y

y:=sptr++
sptr^.uvalue :=sptr^.uvalue>>y^.value
return pcptr+1
end

global function j_shr_zz:ref intpc =  !********************************
variant y

return pcmxtypes("shr_zz",sptr+1,sptr)
!return pcptr+1
end

global function j_idiv_i_w:ref intpc =
variant y

y:=sptr++
sptr^.value:=sptr^.value/y^.value
return pcptr+1
end

global function j_idiv_j:ref intpc =
ref  varrec x,y
varrec result

y:=sptr++
x:=sptr

bx_idiv(x,y,&result)
pc_unshare(x) when x^.hasref
pc_unshare(y) when y^.hasref
sptr^:=result
!bx_reduce(sptr)
return pcptr+1
end

global function j_idiv_z:ref intpc =
pcmxtypes("idiv_def",sptr+1,sptr)
return pcptr+1
end

=== pc_oslayer.m 18/38 ===
import clib
import mlib
import oslib
import osdll

import pc_types
import pc_decls
import pc_support
import pc_pcfns
import pci

global function runproc_m(ref void amsg)int=
varrec a,b,dest
static int rmsg_typeno
int i,result
objrec obj


!!if $64bit
!const rmsgname="ws_msg64"
!!.else
!!const rmsgname="ws_msg32"
!!.endif

if rmsg_typeno=0 then
	for i to ntypes do
		if eqstring(ttname[i],($targetbits=32|"ws_msg32"|"ws_msg64")) then
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
obj.ustruct.ptr:=ref byte(amsg)

a.tagx:=rmsg_typeno ior hasrefmask
a.objptr:=&obj

runproc(pcl_callbackfn,&a,nil,&dest)
result:=dest.value

result:=0			!WTF? BUT QX HAS THIS TOO, AND IT WORKS!

return result
end

global proc os_getconsize(variant result)=
u64 aa
pcerror("GETCONSIZE")
end

global proc pch_setmesshandler(variant fn)=
if fn^.tag<>trefproc then
	pcerror("Not refproc")
fi
pcl_callbackfn:=cast(fn^.refptr)
os_setmesshandler(&runproc_m)
end

global proc pch_gethostname(variant result) =
static [0:256]char name

strcpy(&.name,os_gethostname())

pc_makestring(&.name,-1,result)
end

global proc os_initdllmodules =
int i
intm hinst
ichar dllname

for i:=1 to ndlltable do
	dllname:=dlltable[i]

	hinst:=os_loaddllmodule(dllname)
	if hinst=0 then
!*!		println "1:Can't load DLL :",dllname
	fi
	dllinsttable[i]:=hinst;
od
end

global function os_loaddllmodule(ichar dllname)intm =
intm hinst

!check for special module names
if eqstring(dllname,"jpeglib") then
	dllname:="jpeglib64"
elsif eqstring(dllname,"jpeglibc") then
!.if $ctarget
!	dllname:="jpeglibc32"
!.else
	dllname:="jpeglibc64"
!.endif
fi

hinst:=os_getdllinst(dllname)
return hinst
end

global proc os_initdllfunctions =
ref proc fnaddr
intm hinst
int i,dllmodno

for i:=1 to ndllproctable do
	dllmodno:=dllproctable[i].dllindex;
	hinst:=dllinsttable[dllmodno];

	fnaddr:=os_getdllprocaddr(hinst,dllproctable[i].name)
	if fnaddr=nil then
		if hinst=0 then
			next
		fi


		println "dllfns: fnaddr=0:",dllproctable[i].name,"from",dlltable[dllmodno]
	fi
	dllproctable[i].address:=cast(fnaddr)
od
end

global function os_loaddllfunction(int fnindex)ref void=
int dllmodno
intm hinst
ref proc fnaddr

dllmodno:=dllproctable[fnindex].dllindex
hinst:=dllinsttable[dllmodno]
if not hinst then
	hinst:=os_loaddllmodule(dlltable[dllmodno])
	if not hinst then
!		cpl "Can't load DLL",dlltable[dllmodno]
!		stop
	else
		dllinsttable[dllmodno]:=hinst
	fi
fi

fnaddr:=os_getdllprocaddr(hinst,dllproctable[fnindex].name)
if not fnaddr then
	cpl dllproctable[fnindex].name
	pcerror("Can't find DLL function")
	stop 1
else
	dllproctable[fnindex].address:=cast(fnaddr)
fi
return fnaddr
end

global proc pch_getos(variant result)=
pc_makestring(os_getos(),-1,result)
end

global proc pch_gethostsize(variant result)=
result^.tagx:=tint
result^.value:=os_gethostsize()
end

global proc pch_iswindows(variant result)=
result^.tagx:=tint
result^.value:=os_iswindows()
end

global proc os_calldll(int calltype,fnindex,offset,nparams,restype,variant dest)=
!calltype is 'C', 'W', or 'M'
!fnindex is index into dllproc table
!offset if offset from dllparams[1]
!nparams is number of params to be pushed from and including dllparams[1+offset]
!restype is the packtype code of the result of the call, which can be tvoid too
!When restype is void, *dest does not point to a valid destination var* (no void
!has been pushed to receive the result)
!Otherwise any result is put into dest^, which does not need freeing

int baserestype
ref proc fnaddr
i64 retval,retcode
r64 fretval
ref[]i64 iparams
ref[]i16 iparamtypes
[100]byte paramcodes

fnaddr:=cast(dllproctable[fnindex].address)

!FOR I TO NPARAMS DO
!	CPL "PARAM:",I,TTNAME[DLLTYPES[I+OFFSET]]
!OD

!CPL =DLLPROCTABLE[FNINDEX].NAME



if not fnaddr then
!CPL "LOADING DLL"
	fnaddr:=os_loaddllfunction(fnindex)
	if fnaddr then
		dllproctable[fnindex].address:=cast(fnaddr)
	else
		cpl dllproctable[fnindex].name
		pcerror("Calldll nil fn:")
	fi
fi

!CPL =TTNAME[RESTYPE]

iparams:=cast(&dllparams[offset+1])
iparamtypes:=cast(&dlltypes[offset+1])

for i to nparams do
	if ttbasetype[iparamtypes^[i]]=treal then
		paramcodes[i]:='R'
	else
		paramcodes[i]:='I'
	fi
od

baserestype:=ttbasetype[restype]
!CPL =TTNAME[BASERESTYPE]
!retcode:=(baserestype=treal|'R'|'I')
retcode:=(baserestype=tr64|'R'|'I')

!CPL "CALLDLL1"
retval:=os_calldllfunction(fnaddr, retcode, nparams, iparams, &paramcodes)

!cpl =RETVAL

switch baserestype
when tvoid then
!	dest.tagx:=tvoid
when treal, tr64 then
	dest.tagx:=treal
	dest.xvalue:=real@(retval)
when tr32 then
PCERROR("dll/r32ret")
when ti64,tu64,tintm,twordm,trefm then
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
when trefpacked then
	dest.tagx:=trefpacked
	dest.uref.ptr:=cast(retval)
	dest.uref.elemtag:=ttelemtype[restype]

else
 	cpl ttname[baserestype]
	pcerror("Rettype not supported")
endswitch
end
=== oswindll.m 19/38 ===
import clib
import mlib

!IMPORT OSWINDLLC

global function os_calldllfunction(ref proc fnaddr,
		int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
	word64 a
	real64 x
	int oddstack, nextra, pushedbytes

!	return os_calldllfunctionc(fnaddr,retcode,nargs,args,argcodes)

	oddstack:=nextra:=0

	assem
		test astack,8
		jz L100
		mov byte [oddstack],1
L100:
	end

	if oddstack then
		if nargs<5 then
			nextra:=5-nargs
		elsif nargs.even then
			nextra:=1
		fi

	else
		if nargs<4 then
			nextra:=4-nargs
		elsif nargs.odd then
			nextra:=1
		fi
	fi

	pushedbytes:=(nextra+nargs)*8

!RETURN 0

!CPL "D4"
	to nextra do
		assem
			push 0
		end
	od
!CPL "D5"

	for i:=nargs downto 1 do
		a:=args^[i]					!get generic 64-bit value to push
		assem
			push word64 [a]
		end
	od

!CPL =NEXTRA+NARGS,=pushedbytes,=oddstack

!load first 4 args to registers; this first version will blindly load 4 args
!(even if there are less) to both integer and xmm registers. Should be int/pointer
!types to integer regs; float types to xmm; and variadic to both
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
!	a:=os_pushargs(&wordargs, na, nextra, fnaddr, retttype=tp_r64)
!implements central part of 'callapplproc' which needs to be in asm
	word64 a
	real64 x

CPL "PUSH ARGS",NARGS, NEXTRA

	to nextra do
		asm	push 0
	end

CPL "PUSH ARGS2"
	for i to nargs do
		a:=args[i]
		asm push word64 [a]
	od
CPL "PUSH ARGS3"

	if isfloat then
		x:=((ref function:real64(fnaddr))^())
		a:=int64@(x)
	else
		a:=((ref function:int64(fnaddr))^())
	fi

	return a
end
=== pc_host.m 20/38 ===
import msys
import clib
import mlib
import oslib
import osdll

import pc_types
import pc_decls
import pc_support
import pc_objlib
import pc_print
import pq_common
import pc_oslayer
import pc_pcfns
import pci
import pc_print
!import pc_jpeg

importdll $dummy=
	clang function imgload_bgr(ichar filename, ref int x,y,channels,int needchannels)ref byte
end

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
	ref intpc pchandler
	ref overloadrec nextrec
end

global tabledata() [0:]ichar packtypenames, [0:]int packtypewidths, [0:]int packconvtypes =
	(tp_void=0,		$,	0,		tvoid),
	(tp_i64,		$,	64,		tint),
	(tp_u64,		$,	64,		tword),
	(tp_r64,		$,	64,		treal),

	(tp_pvoid,		$,	64,		trefpacked),	!here to trp64, must be same order as tvoid..tr64
	(tp_pi8,		$,	64,		trefpacked), 
	(tp_pi16,		$,	64,		trefpacked),
	(tp_pi32,		$,	64,		trefpacked),
	(tp_pi64,		$,	64,		trefpacked),
	(tp_pi128,		$,	64,		trefpacked),
	(tp_pu8,		$,	64,		trefpacked),
	(tp_pu16,		$,	64,		trefpacked),
	(tp_pu32,		$,	64,		trefpacked),
	(tp_pu64,		$,	64,		trefpacked),
	(tp_pu128,		$,	64,		trefpacked),
	(tp_pr32,		$,	64,		trefpacked),
	(tp_pr64,		$,	64,		trefpacked),

	(tp_pstruct,	$,	64,		trefpacked),	
	(tp_stringz,	$,	64,		tstringz),
	(tp_variant,	$,	64,		tvariant),
end

ref overloadrec tostr_list			!list of user overloads for tostr
ref overloadrec convert_list

const noparamtag=tvoid
const nodefault=-999999

ref procrec proclistptr
varrec applresult

include "CCM_HOST."

global proc callhostfunction(int hostfn, calledasfn) =		!CALLHOSTFN
ref proc fnaddr
int nparams,isfn
object p

fnaddr:=hosttable[hostfn]
nparams:=hostnparams[hostfn]
isfn:=hostisfn[hostfn]

!CPL "CALLHOST",=hostfn,hostfnnames[hostfn],=nparams,=isfn

if isfn then		!functions

	switch nparams
	when 0 then
		hostfn0(fnaddr)^(sptr)
	when 1 then
		hostfn1(fnaddr)^(sptr,sptr+1)
	when 2 then
		hostfn2(fnaddr)^(sptr,sptr+1,sptr+2)
	when 3 then
		hostfn3(fnaddr)^(sptr,sptr+1,sptr+2,sptr+3)
	when 4 then
		hostfn4(fnaddr)^(sptr,sptr+1,sptr+2,sptr+3,sptr+4)
	else
		pcerror("callhost/fn")
	endswitch
else					!procs

	switch nparams
	when 0 then
		hostproc0(fnaddr)^()
	when 1 then
		hostproc1(fnaddr)^(sptr)
	when 2 then
		hostproc2(fnaddr)^(sptr,sptr+1)
	when 3 then
		hostproc3(fnaddr)^(sptr,sptr+1,sptr+2)
	when 4 then
		hostproc4(fnaddr)^(sptr,sptr+1,sptr+2,sptr+3)
	else
		pcerror("callhost/proc")
	endswitch
fi

to nparams do
	pc_unshare(sptr) when sptr^.hasref
	++sptr
od
end

proc pch_leftstr(variant a, b, c, result)=
int n,length,padchar
ref char s
object pa

padchar:=' '
case c^.tag
when tvoid then
when tstring then
	if c.objptr.ustr.length=1 then
		padchar:=c.objptr.ustr.strptr^
	else
		pcerror("left/padx")
	fi
when tint then
	padchar:=c^.value
else
	pcerror("left/pad?")
esac

case b^.tag
when tvoid then
	n:=1
when tint then
	n:=b^.value
else
	pcerror("left:bad n")
esac
if a^.tag<>tstring then
	pcerror("left:not str")
fi

pa:=a.objptr
length:=pa.ustr.length
s:=pa.ustr.strptr


if n=0 then
	pc_emptystring(result)
	return
fi

result.tagx:=tstring ior hasrefmask
if n>0 then			!leftmost n chars
	if n<=length then
		leftstring(a,n,result)
	else				!n>length
		padstring_right(a,n,padchar,result)
	fi
else					!left chars chars excluding rightmost n
	n:=-n
	if n<length then
		leftstring(a,length-n,result)
	else
		pc_emptystring(result)
	fi
fi
end

proc pch_rightstr(variant a, b, c, result)=
int n,length,padchar
ref char s
object pa

padchar:=' '
case c^.tag
when tvoid then
when tstring then
	if c.objptr.ustr.length=1 then
		padchar:=c.objptr.ustr.strptr^
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

length:=pa.ustr.length
s:=pa.ustr.strptr

result.tagx:=tstring ior hasrefmask

if n=0 then
	pc_emptystring(result)
	return
fi

if n>0 then			!rightmost n chars
	if n<=length then
		rightstring(a,n,result)
	else				!n>length
		padstring_left(a,n,padchar,result)
	fi
else					!right chars chars excluding leftmost n
	n:=-n
	if n<length then
		rightstring(a,length-n,result)
	else
		pc_emptystring(result)
	fi
fi
end

proc pch_convlc(variant a, b, result)=
checkparam(a,tstring)
result^:=a^
++result^.objptr^.refcount
pc_dupl(result) when result^.hasref
pc_iconvcase(result,b,0)
end

proc pch_convuc(variant a, b, result)=
checkparam(a,tstring)
result^:=a^
++result^.objptr^.refcount
pc_dupl(result) when result^.hasref
pc_iconvcase(result,b,1)
end

proc pch_iconvlc(variant a, b)=
checkparam(a,trefvar)
pc_iconvcase(a^.varptr,b,0)
end

proc pch_iconvuc(variant a, b)=
checkparam(a,trefvar)
pc_iconvcase(a^.varptr,b,1)
end

proc pch_stop=
pcerror("host_stop not impl")
end

proc pch_stopx(variant a)=
pcerror("host_stopx not impl")
end

proc pch_ismain(variant a, result)=
int mainmod,ismain

checkparam(a,tstring)
result^.tagx:=tint

if eqstring(strpclversion,"404") then
	mainmod:=1
else
	mainmod:=nmodules
fi

ismain:=cmpstring_len(a.objptr.ustr.strptr,moduletable[mainmod].name,
							a.objptr.ustr.length,strlen(moduletable[mainmod].name))=0
result^.value:=ismain
end

proc pch_waitkey(variant result)=
result^.tagx:=tint
result^.value:=os_getch()
end

proc pch_testkey(variant result)=
result^.tagx:=tint
result^.value:=os_kbhit()
end

proc pch_execwait(variant a, b, c, result)=
ref char workdir
int flag
object pa

checkparam(a,tstring)
pa:=a^.objptr

flag:=checkparam(b,tint,0)

if c^.tag=tvoid then
	workdir:=nil
else
	checkparam(c,tstring)
	workdir:=convCstring(c.objptr.ustr.strptr,c.objptr.ustr.length)
fi
result^.tagx:=tint
result^.value:=os_execwait(convCstring(pa.ustr.strptr,pa.ustr.length),flag,workdir)
end

proc pch_execcmd(variant a, b, c, result)=
ref char workdir
int flag
object pa

checkparam(a,tstring)
pa:=a.objptr

flag:=checkparam(b,tint,0)

if c^.tag=tvoid then
	workdir:=nil
else
	checkparam(c,tstring)
	workdir:=convCstring(c.objptr.ustr.strptr,c.objptr.ustr.length)
fi
result.tagx:=tint
result.value:=os_execcmd(convCstring(pa.ustr.strptr,pa.ustr.length),flag)
end

proc pch_makestr(variant a, b, result)=
	int n
	object s

	switch a^.tag
	when trefpacked then
	when tint then
	else
		pcerror("makestr")
	endswitch

	n:=getintvalue(b)
	result^.tagx:=tstring ior hasrefmask

	s:=make_strslicexobj(cast(a^.uref.ptr),n)
	result^.objptr:=s
end

proc pch_makestrslice(variant a, b, result)=
pcerror("MAKESTRSLICE")
end

proc pch_makeref(variant a,b,result) =		!PCH_MAKEREF
ref byte ptr

switch (ttbasetype[a^.tag])
when trefvar,trefpacked,tint then
	ptr:=a.uref.ptr
when tstring,tarray,tlist,tset then
	ptr:=a.objptr.uarray.ptr
else
	pcerror("makeref")
endswitch

result^.tagx:=trefpacked
result^.uref.ptr:=ptr
result^.uref.elemtag:=getintvalue(b)

case result^.uref.elemtag
when tbit,tbit2,tbit4 then
	result^.tag:=trefbit
	result^.uref.bitoffset:=0
	result^.uref.bitlength:=0
esac

end

proc pch_new(variant a, b, c, d, result)=
varrec v
int i,t,nbytes,ival,nwords,nbits,offset,elemtype,n
dimrec dims
variant qvar
ref int64 qint
ref byte qbyte
ref byte ptr
object p

t:=getintvalue(a)
!CPL "PCH_NEW",gettypename(t),a^.tag
!CPL "PCH_NEW",gettypename(ttbasetype[t]),a^.tag

if t<0 or t>ntypes then
	pcustypet("New:bad type",t)
fi
v.tagx:=t ior hasrefmask

switch ttbasetype[t]
when tlist then
	getbounds(b,&dims,1)
	p:=list_new(dims.length,dims.lbound)

	v.objptr:=p

	if dims.length then
		if c and c^.tag<>tvoid then		!init value provided
			qvar:=p.ulist.vptr
			to dims.length do
				qvar^:=c^
				pc_share(qvar) when qvar^.hasref
				++qvar
			od
		fi
	fi

when tarray then
	if ttlength[t] then			!length defined must be an array usertype
		elemtype:=ttelemtype[t]
		dims.length:=ttlength[t]
		dims.lbound:=ttlower[t]
		dims.upper:=dims.length+dims.lbound-1

		d:=b					!any init value: move to d
		goto doarray2
	fi

	elemtype:=getintvalue(b)
	getbounds(c,&dims,1)
	if elemtype>=tu1 and elemtype<=tu4 then
		v.tag:=t:=tbits
		goto dobits2
	fi

doarray2::
!PCERROR("DOARRAY2")
	p:=array_new(t, elemtype, dims.length,dims.lbound)

	v.objptr:=p

	if dims.length then
		if d and d^.tag<>tvoid then		!initial value supplied
			qbyte:=p.uarray.ptr
			to dims.length do
				pc_storepacked(qbyte,d,elemtype)
				qbyte+:=ttsize[elemtype]
			od
		fi
	fi

when tbits then
	if ttlength[t] then			!length defined must be a bits usertype
		elemtype:=ttelemtype[t]
		dims.length:=ttlength[t]
		dims.lbound:=ttlower[t]
!		dims.upper:=ttupper[t]
		dims.upper:=dims.length+dims.lbound-1

		d:=b					!any init value: move to d
		goto dobits2
	fi

	elemtype:=getintvalue(b)

	if elemtype<tu1 or elemtype>tu4 then
		pcerror("new: bad bits elem")
	fi
	getbounds(c,&dims,1)
dobits2::				!entry point from arrays, when element is bit type

	p:=bits_new(elemtype,dims.length,dims.lbound)
	v.objptr:=p

	if dims.length then
		if d and d^.tag<>tvoid then		!initial value supplied
			qbyte:=p.ubits.ptr

			offset:=0
			to dims.length do
				pc_storebit(qbyte,offset,d,elemtype,0)
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

	p:=set_new(dims.length,0)
	v.objptr:=p

when trecord then
	p:=record_new(t)
	objtovar(p,&v)

	if b and b.tag<>tvoid then
		qvar:=p.urec.vptr
!PCERROR("NEED RECORD LENGTH/HOST")
		to ttlength[t] do
!		to p.urec.length do
			qvar^:=b^
			pc_share(qvar) when qvar^.hasref
			++qvar
		od
	fi

when tstruct then

	p:=struct_new(t)

	objtovar(p,&v)

	if b and b^.tag<>tvoid then
		pcerror("New: struct init")
	fi

when tint,tword,treal,trefproc,trefvar then
	v.value:=0
	v.hasref:=0
	if b and b^.tag<>tvoid then
		pcerror("NEW(int/value)")
	fi


when tstring then
	getbounds(b,&dims,0)
	pc_makestringn(dims.length,&v)

when tdict then
	getbounds(b,&dims,1)
	n:=nextpoweroftwo(dims.length)

	p:=dict_new(n)
	p.udict.dictitems:=0
	v.objptr:=p

else
	pcustypet("new",t)
endswitch
finish::

result^:=v

end

proc pch_newheap(variant a, b, c, d, result)=
variant p

p:=pcm_alloc(varsize)
pch_new(a,b,c,d,p)
result^.tagx:=trefvar
result^.varptr:=p
end

proc pch_heapvar(variant a, result)=
!pcerror("host_heapvar not impl")
variant p
result.tagx:=tint
result.value:=12345678

!pcerror("HEAPVAR")
end

proc pch_freeheap(variant a)=
pcerror("FREEHEAP")
end

proc pch_getcmdparam(variant a, result)=
int n
ref char s

if a^.tag=noparamtag then		!return number of cmds
	result^.tagx:=tint
	result^.value:=ncmdparams+1
	return
fi

n:=getintvalue(a)

pc_makestring(cmdparamtable[n],-1,result)
end

proc pch_setpcerror(variant a)=
object pa
checkparam(a,tstring)
pa:=a^.objptr

if pcerror_mess then
	free(pcerror_mess)
	pcerror_mess:=nil
fi

if pa.ustr.length then
	pcerror_mess:=malloc(pa.ustr.length+1)
	memcpy(pcerror_mess,pa.ustr.strptr,pa.ustr.length)
	(pcerror_mess+pa.ustr.length)^:=0
fi
end

proc pch_setdebug(variant a)=
checkparam(a,tint)

CPL "SETDEBUG................."
!PCERROR("DEB")
fdebug:=a^.value
end

proc pch_setfprintf(variant a, b)=
checkparam(a,trefdllproc)
checkparam(b,trefdllproc)
fprintf_ptr:=cast(a^.refptr)
fgets_ptr:=cast(b^.refptr)
end

proc pch_ticks(variant result)=
result^.tagx:=tint
result^.value:=os_clock()
end

proc pch_sleep(variant a)=
checkparam(a,tint)
os_sleep(a^.value)
end

proc pch_random(variant a, result)=
! a=0		Result is pure int
! a=1		Result is 0.0 to 0.9999999...
! a=n		Result is 0 to n-1
! a=x..y	Result is x to y inclusive

int n,x

result^.tagx:=tint			!assume int result (can be real too)

if a^.tag=trange then
	x:=mrandomrange(a^.range_lower, a^.range_upper)
else
	checkparam(a,tint)
	n:=a^.value
	if n>1 then					!0 to n-1
		x:=mrandomint(n)
	elsif n=0 then				!pure rand
		x:=mrandom()
	elsif n=1 then				!0.0 to 0.99999999
		result^.tagx:=treal
		result^.xvalue:=mrandomreal()
		return
	else
!		mseed(-n)
!		x:=0
	fi
fi
result^.value:=x
end

proc pch_findmetafunction(variant a, result)=
!scan proc table for function which has metadata a
!return
int i
ref char sdata
object pa
ref procrec pp
ref strec d

checkparam(a,tstring)
pa:=a.objptr
result.tagx:=trefproc
result.value:=0

if pa.ustr.length then
	sdata:=convCstring(pa.ustr.strptr,pa.ustr.length)
else
	return
fi 

!if runfrompc then
	d:=&pcsymboltable^[1]
	to nsymbols do
		if d^.nameid=procid then
			if d^.metadata and strstr(d^.metadata,sdata)<>nil then
				result.refptr:=d^.address
				return
			fi
		fi
		++d
	od
!else
!	pp:=proclist
!	while pp do
!		d:=pp^.def
!		if d^.metadata and strstr(d^.metadata,sdata)<>nil then
!			result^.refptr:=d^.address
!			return
!		fi
!		pp:=pp^.nextproc
!	od
!fi
end

proc pch_loadpcl(variant a, b, result)=
pcerror("host_loadpcl not impl")
end

proc pch_runpcl(variant a, b, result)=
pcerror("host_runpcl not impl")
end

proc pch_runtask(variant a, b, result)=
pcerror("host_runtask not impl")
end

proc pch_callext(variant a, b, c)=
pcerror("host_callext not impl")
end

proc pch_system(variant a,result) =		!PCH_SYSTEM
checkparam(a,tstring)
result^.tagx:=tint
result^.value:=system(convCstring(a.objptr.ustr.strptr,a.objptr.ustr.length))
end

proc pch_shellexec(variant a,b,result) =		!PCH_SHELLEXEC
object pa,pb

checkparam(a,tstring)
checkparam(b,tstring)
pa:=a.objptr
pb:=b.objptr

result.tagx:=tint
result.value:=os_shellexec(convCstring(pa.ustr.strptr,pa.ustr.length),
		convCstring(pb.ustr.strptr,pb.ustr.length))
end

proc pch_gethash(variant a,result) =		!PCH_GETHASH
!convert a to hash value
result^.tagx:=tint
result^.value:=gethashvalue(a)
end

proc pch_test(variant a,b,result) =		!PCH_TEST
!varrec v
!static int lastnobjects=0
object p,q
int i
!variant pp
REF BYTE PP

p:=a^.objptr

CP "*****************************TEST:",=TTNAME[A^.TAG],"//"
PP:=P.USET.PTR
TO 16 DO
	CP PP^,," "
	++PP
OD; CPL


!IF A^.TAG>100 THEN
!PCERROR("BAD TAG")
!FI
result^.tagx:=tint
end

proc pch_pcldata(variant a, b, result)=
int res,length,i
ref strec d
checkparam(a,tint)

res:=0

case a^.value
when 'PROCINIT' then
	if proclist=nil then			!build proclist
		d:=&pcsymboltable^[1]
		to nsymbols do
			if d^.nameid=procid then
				addtoproclist(d)
			fi
			++d
		od
	fi
	proclistptr:=proclist

when 'PROC' then
	if proclistptr then
		getproctabledata(proclistptr,result)
		proclistptr:=proclistptr^.nextproc
		return
	fi

when 'MODULE' then
	CPL "MODULE"

when 'CMDNAME' then
!	result^.tagx:=tstring ior hasrefmask
!	result^.objptr:=make_str(cmdnames[b^.value],-1)
	pc_makestring(cmdnames[b^.value],-1,result)
	return

when 'CMDCODE' then
	for i:=1 to klastcmd do
		length:=strlen(cmdnames[i])
		if b.objptr.ustr.length=length and memcmp(b.objptr.ustr.strptr,
				cmdnames[i],length)=0 then
			res:=i
			exit
		fi
	od
when 'NOPNDS' then
	res:=cmdnopnds[b^.value]
else
	pcerror("pcldata/bad table")
esac

result^.tagx:=tint
result^.value:=res
end

proc pch_getcstring(variant a, result)=
!a is a string
!return an int which is a pointer to a zero-terminated temporary string
pcerror("PCH/GETCSTRING")
end

proc pch_getparam(variant a, result)=
checkparam(a,tint)

!CPL a^.value*varsize

result^:=variant(frameptr+a^.value*varsize)^		!param 1/2/3... = offset 16/32/48... (varsize=16)
if result^.hasref then
	++result^.objptr^.refcount
fi
end

proc pch_clearlist(variant a)=
int n
pcerror("PCH CLEARLIST")
end

proc pch_makelink(variant a, result)=

case ttbasetype[a^.tag]
when trecord then
	result^.tagx:=trecordlink
	result^.uref.elemtag:=a^.tag
	result^.objptr:=a^.objptr

when tint then				!makelink(0) is allowed, it just returns nil
	if a^.value then
		pcerror("makelink/int")
	fi
	result^.tagx:=tint
	result^.value:=0
when trecordlink then		!already link
else
CPL ttname[a^.tag]
CPL ttname[ttbasetype[a^.tag]]
	pcerror("makelink: not record/list")
esac
end

proc pch_allparams(variant a,result)=
!return all parameters as an external slice
object p
int nparams,isfn,i
variant q
ref intpc fnptr

checkparam(a,trefproc)
fnptr:=cast(a^.refptr)
nparams:=(fnptr-1)^

p:=obj_new(tlist)
p.objtype:=extslice_obj
p.ulist.length:=nparams
p.ulist.lower:=1
p.ulist.vptr:=variant(frameptr)+1

result.tagx:=tlist ior hasrefmask
result.objptr:=p
end

proc pch_stackvars(variant result)=
pcerror("STACKVARS")
end

proc pch_makeempty(variant a,result)=
object p
int t

t:=ttbasetype[a^.tag]
if t=ttype then
	t:=a^.value
fi

case t
when tlist then
	p:=emptylist
	++p.refcount
when tstring then
	pc_emptystring(result)
	return
when tarray then
	p:=array_new(tarray,a.objptr.uarray.elemtag,0,1)
else
	pcustypet("makeempty?",t)
esac

result.tagx:=t ior hasrefmask
result.objptr:=p
end

proc pch_readlines(variant a,result)=
!a is a string containing a filename
!read lines from file into a list of strings, and return as result
!strings don't include the eol characters
!returns 0 on error 
ref byte p,q,pstart
varrec v
variant r
object l
int nlines,n

checkparam(a,tstring)
if a^.objptr.ustr.length=0 then
error::
	result.tagx:=tint
	result.value:=0
	return
fi

p:=readfile(a.objptr.ustr.strptr)
if p=nil then goto error fi

!easiest to do two passes; first to count how many lines there are

q:=p
nlines:=0
doswitch q++^
when 26 then
	exit
when 13 then
	++nlines
	if q^=10 then
		++q
	fi

when 10 then
	++nlines

end doswitch

!for now, just return number of lines
v.tagx:=tlist ior hasrefmask

l:=list_new(nlines)
v.objptr:=l
r:=v.objptr.ulist.vptr

!populate with strings
q:=p
pstart:=q
doswitch q++^
when 26 then
	exit
when 13 then
	n:=q-pstart-1
	if q^=10 then
		++q
	fi
addline::
	pc_makestring(cast(pstart),n,r)
	++r
	pstart:=q

when 10 then
	n:=q-pstart-1
	goto addline

end doswitch

result^:=v

free(p)
end

proc pch_dictitems(variant a,result)=
if a^.hasref then
	result.tagx:=tint
	result.value:=a.objptr.udict.dictitems
else
	pcerror(".alloclen/not heap")
fi
end

proc pch_setoverload(variant a,b,c)=
!a is the operator (as pcl cmd index)
!b is the type
!c is a pointer to a pcl function

!u64 hinst
ref[0:]ref void tableptr
ref proc handlerptr
ref ref overloadrec ovptr
[256]char str
int i,t,cmd
u64 fnptr,oldfnptr
record rec =
	int cmd
	ref void tableptr
	ref void handleptr
	ref ref overloadrec ovlist
end

static []rec table = (
	(ktostr,		&tostr_table,	&tostr_handler,			&tostr_list),
	(khardconv,		&convert_dtable,	&convert_handler,	&convert_list),
)

checkparam(a,toperator)
checkparam(b,ttype)
checkparam(c,trefproc)
cmd:=a^.value

!need to map operator code to table number
for i to table.len do
	if table[i].cmd=cmd then
		tableptr:=table[i].tableptr
		handlerptr:=table[i].handleptr
		ovptr:=table[i].ovlist
		exit
	fi
else
!	sprintf(&.str,"Setoverload: can't find calltable: %s",cmdnames[cmd]+1)
	print @&.str,"Setoverload: can't find calltable:",cmdnames[cmd]+1
	pcerror(&.str)
od

t:=b^.value

tableptr^[t]:=cast(handlerptr)

addovrecord(ovptr, t, cast(c^.refptr))
end

proc pch_errorinfo(variant a,result)=
checkparam(a,tint)
result^.tagx:=tint
result^.value:=int64(err_pcptr)
end

!proc addovrecord(ref ref overloadrec p, int t, ref intpc fnptr)=
!!create a new overload record, and link it to the list pointer to by p
!ref overloadrec q
!
!q:=pcm_allocz(overloadrec.bytes)
!q^.optype:=t
!q^.pchandler:=fnptr
!
!q^.nextrec:=p^
!p^:=q
!end

proc getbounds(variant p,ref dimrec dims,int lower) =		!GETBOUNDS
! extract length or bounds from p, and return in dims
! p will be an int, range, or other value coerceable to int
! lower is default lower bound
int n

if not p then
	pcerror("New: no bounds")
fi

switch p^.tag
when noparamtag then
	dims^.lbound:=lower
	dims^.upper:=0
	dims^.length:=0
when trange then
	dims^.lbound:=p^.range_lower
	dims^.upper:=p^.range_upper
	dims^.length:=p^.range_upper-p^.range_lower+1
	if dims^.length<0 then
		dims^.length:=0
		dims^.upper:=dims^.lbound-1
	fi
else
	n:=getintvalue(p)
	dims^.lbound:=lower
	dims^.upper:=dims^.length:=n
endswitch
end

function checkparam(variant p,int tag,defaultx=nodefault)int64=
!check out a host param, usually for ints
!void:	return default value (assuming int needed), unless default=nodefault
!		then it's an error
!=tag:	return value
!other:	error

case p^.tag
when tvoid then
	if defaultx=nodefault then
		pcerror("Missing host param")
	fi
!CPL "CHECKPARAM/VOID; return defaultx",defaultx
	return defaultx
when tag then
	return p^.value
esac

if tag=tint then
	case p^.tag
	when treal then
		return p^.xvalue
	esac
fi
cpl ttname[p^.tag]
pcerror("Host param wrong type")
return 0
end

proc leftstring(variant a, int n, variant result)=
!a is an existing string on varstack, which could have be cc_copy or cc_owner
!This can be ""
!return slice of left n chars (n<=length, but n is never zero) in result
!When a is a copy, then returns a view into a, otherwise it will create a new
!string
object p

!NOTE can create slice here

pc_makestring(a.objptr.ustr.strptr,n,result)
end

proc rightstring(variant a, int n, variant result)=
!a is an existing string on varstack, which could have be cc_copy or cc_owner
!This can be ""
!return slice of right n chars (n<=length, but n is never zero) in result
!When a is a copy, then returns a view into a, otherwise it will create a new
!string
object p

!NOTE can create slice here
!pc_makestring(a^.objptr^.strptr+(a^.objptr^.length-n),n,result)
pc_makestring(a.objptr.ustr.strptr+(a.objptr.ustr.length-n),n,result)
end

proc padstring_right(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the first a^.length are from a,
!and the rest are filled with <fillchar>
!n>length always
ref char s
int length

length:=a.objptr.ustr.length

pc_makestringn(n,result)
s:=result.objptr.ustr.strptr

if length then
	memcpy(s,a.objptr.ustr.strptr,length)
	s+:=length
fi
to n-length do
	s^:=fillchar
	++s
od
end

proc padstring_left(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the last a^.length are from a,
!and the rest are filled on the left with <fillchar>
!n>length always
ref char s
int length,padlen

length:=a.objptr.ustr.length
padlen:=n-length

pc_makestringn(n,result)

s:=result.objptr.ustr.strptr
s+:=padlen

if length then
	memcpy(s,a.objptr.ustr.strptr,length)
fi
to padlen do
	--s
	s^:=fillchar
od
end

proc pcld_makevint(variant p,int64 a)=

p^.tagx:=tint
p^.value:=a
end

proc pcld_makelist(variant p,result,int n)=
!p points to a list of n varrecs
!Create a list pointing to those vars, and duplicate
!the data
variant q
object r

result^.tagx:=tlist ior hasrefmask

!PCERROR("PCLD.MAKELIST|")

r:=list_new(n,1)

result.objptr:=r
q:=r.ulist.vptr

to n do
	q^:=p^
	pc_dupl(q)
	++q
	++p
od

end

proc getproctabledata(ref procrec p,variant result)=
[4]varrec table
varrec l
ref strec d
int moduleno

!if p=nil then
!	result^.tagx:=tint
!	result^.value:=0
!fi
d:=p^.def

pc_makestring(d^.name,-1,&table[1])
pc_makestring(d^.metadata,-1,&table[2])

moduleno:=d^.ax_moduleno
pcld_makevint(&table[3],moduleno)

table[4].tagx:=trefproc
table[4].refptr:=cast(d^.pcaddress)

pcld_makelist(&table[1],result,4)
end

function convert_handler(int)ref intpc=
return nil
end

proc addtoproclist(ref strec d)=
	ref procrec pp
	++nproclist
	pp:=pcm_alloc(procrec.bytes)
	pp^.nextproc:=proclist
	proclist:=pp
	pp^.def:=d
end

function tostr_handler(variant p,FMTSTR,ref fmtrec fmt,object dest)ref intpc =
ref overloadrec q
varrec vdest
object vp

q:=tostr_list

q:=tostr_list
while q do
	if q^.optype=overloadtype then
		exit
	fi
	q:=q^.nextrec
od

if q=nil then
	cpl ttname[p^.tag]
	pcerror("tostr/overload")
fi

vdest.tagx:=tvoid
runproc(q^.pchandler,p,fmtstr,&vdest)
if vdest.tag<>tstring then
	PCERROR("custom tostr needs string result")
fi

vp:=vdest.objptr

if vp.ustr.length then
	addstring(dest,vp.ustr.strptr,vp.ustr.length)
fi
pc_unshare(&vdest)

return nil
end

proc addovrecord(ref ref overloadrec p, int t, ref intpc fnptr)=
!create a new overload record, and link it to the list pointer to by p
ref overloadrec q

q:=pcm_allocz(overloadrec.bytes)
q^.optype:=t
q^.pchandler:=fnptr

q^.nextrec:=p^
p^:=q
end

function findapplproc(int fnindex)ref proc =
	ichar name
	int n,tp,OPT
	ref procinforec p

!PCERROR("EXPORTS NOT DONE")

	name:=applproctable[fnindex].name

	n:=$get_nexports()

	for i to n do
		p:=$get_procexport(i)
		if eqstring(name,$get_procname(p.fnindex)) then
			applproctable[fnindex].address:=cast($get_procaddr(p.fnindex))
			applproctable[fnindex].info:=p
			return applproctable[fnindex].address
		fi
	od

	println name
	pcerror("Can't find appl proc")
	return nil
end

global proc do_callapplproc(int fnindex, nargs, variant result)=
!call applproc (new hostproc) at fnindex within applproctable
!nargs is number of variant args on pc stack
!result is place to receive any function result, as a variant
!arguments start just before result, and are in reverse order
!(eg. 1st at (result-1), 2nd at (result-2) etc)
!args should not be freed here, or their refcounts affected unless
!they need to be shared
!First call of a particular function requires a lookup, with the result
!stored in the applproctable
	ref proc fnaddr
	ref[12]byte paramlist
	int nparams, rettype, nextra, tag,na
	word64 a
	[100]word64 wordargs
	real64 x
	variant args, retv
	ref procinforec info

	fnaddr:=applproctable[fnindex].address
	if fnaddr=nil then
		fnaddr:=findapplproc(fnindex)
	fi
	info:=applproctable[fnindex].info

	rettype:=info.rettype
	nparams:=info.nparams
	paramlist:=&info^.paramlist

	args:=result-1				!point to first argument (increments downwards)

	nextra:=0
	if nparams>nargs then
		for i:=nargs+1 to nparams do
			unless paramlist[i] iand 0x40 then		!optional flag
				pcerror("callappl: too few args or not optional")
			end unless
		od
		nextra:=nparams-nargs

	elsif nparams<nargs then
		pcerror("callappl: too many args")
	fi

	na:=0
	for i:=nargs downto 1 do
		wordargs[++na]:=vartopack(args, paramlist[i] iand 63)		!get generic 64-bit value to push
		--args
	od

!CPL "ABOUT TO CALL APPL FN",FNADDR, DUMMYFN
	a:=os_pushargs(&wordargs, na, nextra, fnaddr, rettype=tp_r64)

	result.tagx:=packconvtypes[rettype]
	result.value:=a

!CPL =TTNAME[RESULT.TAGX]

	switch rettype
	when tp_pi8..tp_pi64 then result.uref.elemtag:=rettype-tp_pi8+ti8
	when tp_pu8..tp_pu64 then result.uref.elemtag:=rettype-tp_pu8+tu8
	when tp_variant then
		retv:=cast(a)
		result^:=retv^
	end
end

function vartopack(variant p, int tp)int64=
!convert variant arg p to M packtype tp, and return as 64-bit values
!suitable for pushing
	int tag:=p.tag
	int64 a:=p.value
	ichar ss
	real xx

	switch tp
	when tp_variant then
		return cast(p)

	when tp_i64 then
		case tag
		when tint,tword then return a
		when treal then return int(p.xvalue)
		esac

	when tp_r64 then
		case tag
		when tint,tword then
			xx:=a
			return int@(xx)
!			return int@(real(a))
		when treal then return int@(p.xvalue)
		esac

	when tp_stringz then
		case p.tag
		when tstring then
			ss:=convcstring(p.objptr.ustr.strptr,p.objptr.ustr.length)
			return word64@(ss)
!			return word64@(convcstring(p.objptr.ustr.strptr,p.objptr.ustr.length))
		esac

	when tp_pi8..tp_pr64 then
		case ttbasetype[p.tag]
		when tarray then
			return int(p.objptr.uarray.ptr)
		when trefpacked then
			return a
		esac

	when tvoid then
		return 0
	endswitch

	println ttname[tag],"=>", packtypenames[tp]
	pcerror("vartopack?")
	return 0
end

export function dummyfn(int a,b,c)int=
CPL "DUMMY FN",a,b,c
	return a+b+c
end

export function new_random(variant a)variant result=
! a=0		Result is pure int
! a=1		Result is 0.0 to 0.9999999...
! a=n		Result is 0 to n-1
! a=x..y	Result is x to y inclusive
	int n,x

	result:=&applresult
	result^.tagx:=tint			!assume int result (can be real too)

	if a^.tag=trange then
		x:=mrandomrange(a^.range_lower, a^.range_upper)
	else
		checkparam(a,tint)
		n:=a^.value
		if n>1 then					!0 to n-1
			x:=mrandomint(n)
		elsif n=0 then				!pure rand
			x:=mrandom()
		elsif n=1 then				!0.0 to 0.99999999
			result^.tagx:=treal
			result^.xvalue:=mrandomreal()
			return result
		else
!		mseed(-n)
!		x:=0
		fi
	fi
	result^.value:=x
	return result
end

export function new_heapvar(variant a)variant result=
!pcerror("host_heapvar not impl")
variant p
	result:=&applresult
	result.tagx:=tint
	result.value:=12345679
	return result
!pcerror("HEAPVAR")
end

export function mfib(int n)int=
	if n<3 then
		return 1
	else 
		return mfib(n-1)+mfib(n-2)
	fi
end

!export function imgloadbgr(ichar filename, int &x,&y,&channels,needchannels)ref byte=
!
!!char* imgload_bgr(char *filename, int *x, int *y, int *channels, int needchannels) {
!	return imgload_bgr(filename,&x,&y,&channels,needchannels)
!
!end
=== pc_dxfns.m 21/38 ===
import mlib
import clib

import pc_types
import pc_decls
import pq_common
import pc_support
import pc_objlib
import pc_bignum
import pc_misc
import pc_pcfns

global proc dx_iorset(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
int xlen,ylen
int n,i
ref int p
object px,py
ref byte pp

px:=x.objptr
py:=y.objptr

xlen:=px.uset.length
ylen:=py.uset.length

if ylen=0 then		!x/[] + [] return X unchanged
elsif xlen=0 then		![] + y set x to y
	x^:=y^			!y won't be used again so x takes over copy bits
else				!x + y

	pc_duplvar(x)				!make independent copy if needed (note: can be inefficient if resize needed anyway)
	px:=x.objptr

	iresizeset(x,ylen)		!make sure x is at least as big as y

	iorsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)

	pc_unshare(y)
fi
end

global proc dx_iandset(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
int xlen,ylen
int n,i
ref int p
object px,py
ref byte pp

px:=x.objptr
py:=y.objptr

xlen:=px.uset.length
ylen:=py.uset.length

if ylen=0 then		!x/[] + [] return X unchanged
elsif xlen=0 then		![] + y set x to y
	x^:=y^			!y won't be used again so x takes over copy bits
else				!x + y

	pc_duplvar(x)				!make independent copy if needed (note: can be inefficient if resize needed anyway)
	px:=x.objptr
	iresizeset(x,ylen)		!make sure x is at least as big as y

	iandsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)

	pc_unshare(y)
fi
end

global proc dx_ixorset(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
int xlen,ylen
int n,i
ref int p
object px,py
ref byte pp

px:=x.objptr
py:=y.objptr

xlen:=px.uset.length
ylen:=py.uset.length

if ylen=0 then		!x/[] + [] return X unchanged
elsif xlen=0 then		![] + y set x to y
	x^:=y^			!y won't be used again so x takes over copy bits
else				!x + y

	pc_duplvar(x)				!make independent copy if needed (note: can be inefficient if resize needed anyway)
	px:=x.objptr
	iresizeset(x,ylen)		!make sure x is at least as big as y

	ixorsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)

	pc_unshare(y)
fi
end

global proc dx_inotset(variant x) =
object px

px:=x.objptr

if px.uset.length then
	pc_duplvar(x)				!make independent copy if needed (note: can be inefficient if resize needed anyway)
	px:=x.objptr
	inotsetbits(cast(px.uset.ptr),px.uset.length)
fi
end

global proc dx_subset(variant x,y)=
!z:=x-y
int xlen,ylen
object px,py

px:=x.objptr
py:=y.objptr

xlen:=px.uset.length
ylen:=py.uset.length

if xlen and ylen then		!x-y
	pc_dupl(x)
	px:=x.objptr
	iresizeset(x,ylen)		!make sure x is at least as big as y
	subsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)
fi				!else: []-y, x-[], []-[] all have x as result, so leave unchanged
pc_unshare(y)
end

global proc inotsetbits(ref int p,int n)=
int i
to (n-1)/64+1 do
	p^ :=inot p^
	++p
od
end

global proc iorsetbits(ref int p,q,int n)=
int i
to (n-1)/64+1 do
	p++^ ior:= q++^
od
end

global proc iandsetbits(ref int p,q,int n)=		!PC_IANDSETBITS
int i
to (n-1)/64+1 do
	p++^ iand:= q++^
od
end

global proc ixorsetbits(ref int p,q,int n)=		!PC_IXORSETBITS
int i
to (n-1)/64+1 do
	p++^ ixor:= q++^
od
end

global proc subsetbits(ref int p,q,int n)=		!PC_SUBSETBITS
int i
!to (n-1)/64+1 do
to (n-1)/64+1 do
	p^ ior:= q^
	p^ ixor:= q^
	++p
	++q
od
end

global proc iresizeset(variant p,int n)=
!make sure set x has at least n elements, extending as needed
!this is done in-place, so caller must ensure p can be modified
!x should also be a cc_owner type (as it makes use of .alloc)
object pp

pp:=p.objptr

if pp.uset.length>=n then		!already large enough
	return
fi

bits_resize(pp,n)
!pp^.length:=n
end

global function dx_varinvar(variant x, y)int =		!DX_VARINVAR
!test whether x is in y return index of x in y, or 0
!a,b are freed if necessary
int i,xt,yt,n,a
int64 nn,aa
variant p,q
object px,py

xt:=x.tag
yt:=ttbasetype[y.tag]

px:=x.objptr
py:=y.objptr

switch xt
when tint then
doi64invar::
	switch (yt)
	when tset then
		n:=x^.value
doi64inset::
		if u32(n)>=u32(py.uset.length) then	!out of bounds so not in set
			pc_unshare(y) when y.hasref
			return 0
		fi
		n:=testelem(cast(py.uset.ptr),n)
		pc_unshare(y)
		return n
	when tlist then
		a:=x.value
		n:=py.ulist.length
		p:=py.ulist.vptr
		for i to n do
			if p.tag=tint and p.value=a then
				pc_unshare(y) when y.hasref
				return i
			fi
			++p
		od
		pc_unshare(y)
		return 0
	when tarray then
		case py.uarray.elemtag
		when ti8,tu8 then
			n:=u8inarray(x.value,py)
		when ti16,tu16 then
			n:=u16inarray(x.value,py)
		when ti32,tu32 then
			n:=u32inarray(x.value,py)
		when ti64,tu64 then
			n:=u64inarray(x.value,py)
		else
			pcustypet("x in array",py.uarray.elemtag)
		esac
		pc_unshare(y)
		return n
	when tbits then
		case py.ubits.elemtag
		when tu1 then
			n:=bitinbits(x.value,py)
		else
			pcustypet("x in bits",py.ubits.elemtag)
		esac
		pc_unshare(y)
		return n
	when trange then
		n:=x^.value
		return n>=y.range_lower and n<=y.range_upper
	endswitch

when tstring then
	switch (yt)
	when tstring then
		n:=pc_strinstr(x,y)
		pc_unshare(x) when x.hasref
		pc_unshare(y) when y.hasref
		return n
	when tlist then
		n:=py.ulist.length
		p:=py.ulist.vptr
		i:=py.ulist.lower
		to n do
			if p.tag=tstring then
				if pc_eqstring_nf(x,p) then
					pc_unshare(x) when x.hasref
					pc_unshare(y) when y.hasref
					return i!-py^.lower
				fi 
			fi
			++p
			++i
		od
		pc_unshare(x) when x.hasref
		pc_unshare(y) when y.hasref
		return 0
	endswitch

else
	switch(yt)
	when tlist then		!x can be anything
		n:=py.ulist.length
		p:=py.ulist.vptr
		for i to n do
			if pc_equal_nf(x,p,1)=1 then
				pc_unshare(x) when x.hasref
				pc_unshare(y) when y.hasref
				return i
			fi
			++p
		od
		pc_unshare(x) when x.hasref
		pc_unshare(y) when y.hasref
		return 0
	endswitch
endswitch
pcmxtypes("varinvar:",x,y)
return 0
end

global function dx_mixed (variant x, y)int =		!DX_MIXED
! x,y are different types try and coerce one to the dominant type of the other,
! if they are numeric. Then return this dominant type.
! Or return 0 if they cannot be coerced.
! Assumes that caller has already established that the types are different.
!int a

switch x^.tag
when tint then
	switch y^.tag
	when treal then
		x^.xvalue:=x^.value
		x^.tagx:=treal
		return treal
	when tword then
		return tint
	when tbignum then
		bx_makeint(x^.value,x)
		return tbignum

	when tvoid then
		goto dxvoid

	else
		return 0
	endswitch
when treal then
	switch y^.tag
	when tint then
		y^.xvalue:=y^.value
		y^.tagx:=treal
		return treal
	when tvoid then
		goto dxvoid

	else
		return 0
	endswitch
when tword then
	switch y^.tag
	when tint then
		return tword
	when tvoid then
		goto dxvoid

	else
		return 0
	endswitch

when tbignum then
	switch y^.tag
	when tint then
		bx_makeint(y^.value,y)
		return tbignum
	else
		return 0
	endswitch

when tvoid then
dxvoid::
	pcerror("dxmix/void")

else
	if y^.tag=tvoid then
		goto dxvoid
	fi
	return 0
endswitch
return 0
end
=== pc_khandlers.m 22/38 ===
import clib
import mlib
import oslib

import pc_types
import pc_decls
import pc_support
import pc_objlib
import pc_print
import pq_common
import pc_host
import pc_oslayer
import pc_misc
import pc_bignum
import pc_jhandlers
import pc_pcfns
import pc_dxfns

global object zerostringobj

!global ref intpc stopcode		!point to a 'stop 0' sequence
global byte stopped

global function k_zero:ref void =	! K_ZERO
	pclunimpl(kzero)
	return pcptr+1
end

global function k_nop:ref void =	! K_NOP
	return pcptr+1
end

global function k_procstart:ref void =	! K_PROCSTART
	pclunimpl(kprocstart)
	return pcptr+3
end

global function k_procend:ref void =	! K_PROCEND
	pclunimpl(kprocend)
	return pcptr+1
end

global function k_endmodule:ref void =	! K_ENDMODULE
	pclunimpl(kendmodule)
	return pcptr+1
end

global function k_push_m:ref void =	! K_PUSH_M
!(--sptr)^:= variant(getopnda)^
	--sptr
	sptr^:= variant(getopnda)^

	if sptr.hasref then
		++sptr^.objptr^.refcount
	fi
	return pcptr + 2
end

global function k_push_f:ref void =	! K_PUSH_F
!(--sptr)^:=variant(frameptr+getopnda)^
	--sptr
	sptr^:=variant(frameptr+getopnda)^

	if sptr.hasref then
		++(sptr.objptr.refcount)
	fi
	return pcptr + 2
end

global function k_push_am:ref void =	! K_PUSH_AM
	--sptr
	sptr^.tagx:=trefvar
	sptr^.varptr:=variant(getopnda)
	return pcptr + 2
end

global function k_push_af:ref void =	! K_PUSH_AF
	--sptr
	sptr^.tagx:=trefvar
	sptr^.varptr:=variant(frameptr+getopnda)

	return pcptr + 2
end

global function k_push_ap:ref void =	! K_PUSH_AP
	--sptr
	sptr.tagx:=trefproc
	sptr.refptr:=ref byte(getopnda)
	return pcptr + 2
end

global function k_push_al:ref void =	! K_PUSH_AL
	--sptr
	sptr.tagx:=treflabel
	sptr.refptr:=ref byte(getopnda)
	return pcptr + 2
end

global function k_push_ci:ref void =	! K_PUSH_CI
	--sptr
	sptr.tagx:=tint
	sptr.value:=getopnda

	return pcptr + 2
end

global function k_push_cw:ref void =	! K_PUSH_CW
	--sptr
	sptr^.tagx:=tword
	sptr^.uvalue:=getopnda
	return pcptr + 2
end

global function k_push_cr:ref void =	! K_PUSH_CR
	--sptr
	sptr^.tagx:=treal
	sptr^.uvalue:=getopnda

	return pcptr + 2
end

global function k_push_cn:ref void =	! K_PUSH_CN
	--sptr
	sptr^.tagx:=trange
	sptr^.uvalue:=getopnda

	return pcptr + 2
end

global function k_push_cs:ref void =	! K_PUSH_CS
	--sptr
	sptr^.tagx:=tstring ior hasrefmask

	sptr^.objptr:=object(getopnda)
	++sptr^.objptr^.refcount

	return pcptr+2
end

global function k_push_t:ref void =	! K_PUSH_T
	--sptr
	sptr^.tagx:=ttype
	sptr^.value:=getopnda
	return pcptr + 2
end

global function k_push_op:ref void =	! K_PUSH_OP
	--sptr
	sptr^.tagx:=toperator
	sptr^.value:=getopnda			!operator code (is a pcl cmd index)
	sptr^.uop.opdims:=getopndb			!number of operands expected, 1 or 2
	return pcptr + 3
end

global function k_pushz:ref void =	! K_PUSHZ
--sptr
sptr^.tagx:=getopnda
sptr^.value:=0
return pcptr + 2
end

global function k_pushz_void:ref void =	! K_PUSHZ_VOID
--sptr
sptr^.tagx:=tvoid
return pcptr + 1
end

global function k_pushz_str:ref void =	! K_PUSHZ_STR
ref proc f

--sptr

!if zerostringobj=nil then
!*!	zerostringobj:=newobject()
!	ZEROSTRINGOBJ^.REFCOUNT:=2
!fi
sptr^.objptr:=emptystring
sptr^.tagx:=tstring ior hasrefmask
++emptystring^.refcount

return pcptr+1
end

global function k_pushz_list:ref void =	! K_PUSHZ_LIST
ref proc f
--sptr
sptr^.tagx:=tlist ior hasrefmask
sptr^.objptr:=emptylist
++emptylist^.refcount

return pcptr+1
end

global function k_pushz_listl:ref void =	! K_PUSHZ_LISTL
object p

--sptr
sptr^.tagx:=tlist ior hasrefmask
sptr.objptr:=list_new(0,getopnda)

return pcptr+2
end

global function k_pushz_set:ref void =	! K_PUSHZ_SET
--sptr
sptr^.tagx:=tset ior hasrefmask
sptr^.objptr:=emptyset
++emptyset^.refcount
return pcptr+1
end

global function k_pushz_arrayl:ref void =	! K_PUSHZ_ARRAYL
pclunimpl(kpushz_arrayl)
return pcptr+3
end

global function k_pop_m:ref void =	! K_POP_M
variant a

a:=variant(getopnda)
pc_unshare(a) when a^.hasref
a^:=sptr++^
return pcptr + 2
end

global function k_pop_f:ref void =	! K_POP_F
variant a

a:=variant(frameptr+getopnda)

pc_unshare(a) when a^.hasref
a^:=sptr++^					!transfer reference to a

return pcptr + 2
end

global function k_store_m:ref void =	! K_STORE_M
variant a

a:=variant(getopnda)

pc_share(sptr) when sptr.hasref
pc_unshare(a) when a^.hasref
a^:=sptr^
return pcptr + 2
end

global function k_store_f:ref void =	! K_STORE_F
variant a

a:=variant(frameptr+getopnda)
pc_share(sptr) when sptr.hasref
pc_unshare(a) when a^.hasref
a^:=sptr^
return pcptr + 2
end

global function k_pushptr:ref void =	! K_PUSHPTR
switch sptr^.tag
when trefvar then
	sptr^:=(sptr^.varptr)^
	if sptr.hasref then
		++sptr^.objptr^.refcount
	fi

when trecordlink then				!convert trecordlink to record
	sptr^.tagx:=sptr^.uref.elemtag ior hasrefmask
	++sptr^.objptr^.refcount

when trefpacked then
	pc_loadpacked(sptr^.uref.ptr,sptr^.uref.elemtag,sptr,nil)
when trefbit then
	pc_loadbit(sptr^.uref.ptr,sptr^.uref.bitoffset,sptr^.uref.elemtag,sptr^.uref.bitlength,sptr)
else
	pcustype("pushptr",sptr)
endswitch

return pcptr+1
end

global function k_popptr:ref void =	! K_POPPTR
variant p,q

p:=sptr++			!p is pointer p^:=sptr

!CPL "POPPTR",gettypename(p^.tag)

switch p^.tag
when trefvar then		!
	q:=p^.varptr			!dest var
	pc_unshare(q) when q^.hasref
	q^:=sptr++^
when tstring then
	pc_storestring(p,sptr)
	++sptr
else
	pc_storeptr(p,sptr)		!pop to p^
	++sptr
end
return pcptr + 1
end

global function k_storeptr:ref void =	! K_STOREPTR
variant p,q

p:=sptr++			!p is pointer p^:=sptr

if p^.tag=trefvar then
	q:=p^.varptr			!dest var
	pc_share(sptr) when sptr.hasref
	pc_unshare(q) when q^.hasref
	q^:=sptr^
else
	pc_storeptr(p,sptr)		!pop to p^
	pc_unshare(sptr) when sptr.hasref
fi
return pcptr + 1
end

global function k_zpop_m:ref void =	! K_ZPOP_M
variant a

a:=variant(getopnda)
a^:=sptr++^					!transfer reference

return pcptr + 2
end

global function k_zpop_f:ref void =	! K_ZPOP_F
variant a

a:=variant(frameptr+getopnda)
a^:=sptr++^					!transfer reference

return pcptr + 2
end

global function k_zstore_m:ref void =	! K_ZSTORE_M
pclunimpl(kzstore_m)
return pcptr+2
end

global function k_zstore_f:ref void =	! K_ZSTORE_F
variant a

a:=variant(frameptr+getopnda)
a^:=sptr^
pc_share(a) when a^.hasref
if sptr.hasref then
	++sptr^.objptr^.refcount
fi
return pcptr + 2
end

global function k_copy:ref void =	! K_COPY
if not sptr.hasref then
	return pcptr+1
fi

pc_dupl(sptr)

return pcptr+1
end

global function k_swap:ref void =	! K_SWAP
[1024]byte tempbuffer
variant x,y
varrec v
int xt,yt,s,t,n
ref byte p,q
int a

x:=sptr++
y:=sptr++
xt:=x^.tag
yt:=y^.tag

if xt=trefvar and yt=trefvar then
	v:=(x^.varptr)^
	(x^.varptr)^:=(y^.varptr)^
	(y^.varptr)^:=v
elsif xt=trefpacked and yt=trefpacked then
	s:=x^.uref.elemtag
	t:=y^.uref.elemtag
!	if x^.uref.elemtag=y^.uref.elemtag=tu8 then
	if s<>t then goto swaperror fi
	n:=ttsize[s]
	case n
	when 1 then
		p:=x^.uref.ptr
		q:=y^.uref.ptr
		a:=p^
		p^:=q^
		q^:=a
	elsif ttsize[s]<=tempbuffer.bytes then
		memcpy(&tempbuffer,x^.uref.ptr,n)
		memcpy(x^.uref.ptr,y^.uref.ptr,n)
		memcpy(y^.uref.ptr,&tempbuffer,n)
	else
		goto swaperror
	esac

else
swaperror::
	pcmxtypes("SWAP",x,y)
fi
return pcptr + 1
end

global function k_convptr:ref void =	! K_CONVPTR
variant a
int tag,elemtype
ref void p
object pa

switch sptr^.tag
when trefvar then
	a:=sptr^.varptr
	pa:=a^.objptr
	switch ttbasetype[a^.tag]
	when tint,tword then
		p:=&a^.value
		elemtype:=ti64
	when treal then
		p:=&a^.value
		elemtype:=tr64
	when tarray then
		p:=pa.uarray.ptr
		elemtype:=a.objptr.uarray.elemtag
	when tstring then
		p:=pa.ustr.strptr
		elemtype:=tu8
		if p=nil then
			p:=""
!			pcerror("&null str")
		fi
	when tstruct then
		p:=pa.ustruct.ptr
		elemtype:=a^.tag
	else
		if a^.hasref then
			p:=pa
			elemtype:=tvoid
			goto done
		fi
		CPL gettypename(ttbasetype[a^.tag])
		pcustype("Getrefpack1",a)
		return pcptr
	end switch
when trefpacked,trefbit then
	return pcptr+1

else
	pcustype("Getrefpack2",sptr)
	return pcptr
endswitch
done::

sptr^.tagx:=trefpacked
sptr^.uref.ptr:=p
sptr^.uref.elemtag:=elemtype

return pcptr + 1
end

global function k_jump:ref void =	! K_JUMP
return ref int(getopnda)
end

global function k_jumpptr:ref void =	! K_JUMPPTR
if sptr^.tag<>treflabel then
	pcerror("Bad label ptr")
fi
pcptr:=ref intpc(sptr^.refptr)
++sptr
return pcptr
end

global function k_jumptrue:ref void =	! K_JUMPTRUE
return jumptrue_table[sptr^.tag]^()
end

global function k_jumpfalse:ref void =	! K_JUMPFALSE
return jumpfalse_table[sptr^.tag]^()
end

global function k_jumpdef:ref void =	! K_JUMPDEF
if sptr^.tag<>tvoid then
	pc_unshare(sptr) when sptr.hasref
	++sptr
	return ref intpc(getopnda)
fi
pc_unshare(sptr) when sptr.hasref
++sptr

return pcptr + 2
end

global function k_jumpvoid:ref void =	! K_JUMPVOID
if sptr^.tag=tvoid then		!for now, allow tnone as void
	pc_unshare(sptr) when sptr.hasref
	++sptr
	return ref intpc(getopnda)
fi
pc_unshare(sptr) when sptr.hasref
++sptr

return pcptr + 2
end

global function k_jumpeq:ref void =	! K_JUMPEQ
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	return jumpeq_table[yt]^()
fi

opc_tableptr:=&jumpeq_table
return jumpeq_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_jumpne:ref void =	! K_JUMPNE
int yt

yt:=sptr^.tag

!CPL "JUMPNE1",TTNAME[(SPTR+1)^.TAG],TTNAME[YT]; OS_GETCH()

if (sptr+1)^.tag=yt then
	return jumpne_table[yt]^()
fi
!CPL "JUMPNE2",TTNAME[(SPTR+1)^.TAG],TTNAME[YT]; OS_GETCH()

opc_tableptr:=&jumpne_table
return jumpne_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_jumplt:ref void =	! K_JUMPLT
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	return jumplt_table[yt]^()
fi

opc_tableptr:=&jumplt_table
return jumplt_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_jumple:ref void =	! K_JUMPLE
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	return jumple_table[yt]^()
fi

opc_tableptr:=&jumple_table
return jumple_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_jumpge:ref void =	! K_JUMPGE
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		if sptr^.value>=y^.value then
			++sptr
			return ref intpc(getopnda)
		fi
		++sptr
		return pcptr+2
	else
		return jumpge_table[yt]^()
	end
fi

opc_tableptr:=&jumpge_table
return jumpge_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_jumpgt:ref void =	! K_JUMPGT
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		if sptr^.value>y^.value then
			++sptr
			return ref intpc(getopnda)
		fi
		++sptr
		return pcptr+2
	else
		return jumpgt_table[yt]^()
	end
fi

opc_tableptr:=&jumpgt_table
return jumpgt_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_jumptesteq:ref void =	! K_JUMPTESTEQ
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
variant x,y
int xt,yt,res
object py

y:=sptr++
x:=sptr
xt:=x^.tag
yt:=y^.tag

if xt<>yt then
	if xt:=dx_mixed(x,y) then
		goto retry
	fi
	xt:=x^.tag
	yt:=y^.tag
	switch xt
	when tint then
		switch (yt)
		when trange then
			if x^.value<y^.range_lower or x^.value>y^.range_upper then	!out of range, so false
				return pcptr + 2					!x<>y don't jump, just skip
			fi
		when tset then
			py:=y^.objptr
			if py.uset.length=0 or x.value>=py.uset.length or 
				not testelem(cast(py.uset.ptr),x.value) then 	!not in set, so false
				return pcptr + 2					!x<>y skip
			fi
		else						!type mismatch assume x<>y, so skip
			return pcptr + 2					!x<>y skip
		endswitch
	else							!other types assume x<>y as can't match
!  ++sptr
		return pcptr + 2					!x<>y skip
	endswitch
! assume the same jump to L
	++sptr
	return ref intpc(getopnda)
fi
retry::

switch xt
!when tvoid then
when tint,ttype then
	if x^.value=y^.value then
		++sptr		!pop x
		return ref intpc(getopnda)
	fi

when treal then
	if x^.xvalue=y^.xvalue then
		++sptr		!pop x
		return ref intpc(getopnda)
	fi
when trange then
	if x^.value=y^.value then
		++sptr		!pop x
		return ref intpc(getopnda)
	fi
when tstring then
	res:=pc_eqstring_nf(x,y)
	pc_unshare(y) when y^.hasref
	if res then
		pc_unshare(x) when x^.hasref
		++sptr
		return ref intpc(getopnda)
	fi
else
	res:=pc_equal_nf(x,y,0)
	pc_unshare(x) when x^.hasref
	pc_unshare(y) when y^.hasref
	if res then
		++sptr		!pop x
		return ref intpc(getopnda)
	fi
endswitch
return pcptr + 2
end

global function k_jumptestne:ref void =	! K_JUMPTESTNE
variant x,y
int xt,yt,res
object px,py

!if fdebug then CPL "JUMPTESTNE" fi

y:=sptr++
x:=sptr
xt:=x^.tag
yt:=y^.tag

if xt<>yt then
	if xt:=dx_mixed(x,y) then
		goto retry
	fi
	xt:=x^.tag			!will be unequal
	yt:=y^.tag
	switch (xt)
	when tint then
		switch (yt)
		when trange then
			if x^.value>=y^.range_lower and x^.value<=y^.range_upper then	!in range, so false
				++sptr
				return pcptr + 2
			fi
		when tset then
			py:=y^.objptr
			if x.value<py.uset.length and testelem(cast(py.uset.ptr),x.value) then 	!in set, so false
				++sptr
				return pcptr + 2
			fi
		endswitch
	endswitch
! different types or not in range/set : assume unequel
		return ref intpc(getopnda)
fi
retry::

switch xt
when tint,ttype then
	if x^.value<>y^.value then
		return ref intpc(getopnda)
	fi
	++sptr
when treal then
	if x^.xvalue<>y^.xvalue then
		return ref intpc(getopnda)
	fi
	++sptr
when trange then
	if x^.value<>y^.value then
		return ref intpc(getopnda)
	fi
	++sptr
when tstring then
	res:=pc_eqstring_nf(x,y)
	pc_unshare(y) when y^.hasref
	if not res then
		return ref intpc(getopnda)
	fi
	pc_unshare(x) when x^.hasref
	++sptr 
else
	res:=pc_equal_nf(x,y,0)
	pc_unshare(y) when y^.hasref
	if not res then
		return ref intpc(getopnda)
	fi
	pc_unshare(x) when x^.hasref
	++sptr
endswitch
return pcptr + 2
end

global function k_jumplabel:ref void =	! K_JUMPLABEL
pclunimpl(kjumplabel)
return pcptr+2
end

global function k_jumpclabel:ref void =	! K_JUMPCLABEL
pclunimpl(kjumpclabel)
return pcptr+3
end

global function k_switch:ref void =	! K_SWITCH
int index,n,lower

n:=getopnda
lower:=getopndb

case sptr^.tag
when tint,ttype then
else
CPL ttname[sptr^.tag]
	pcerror("switch not int")
esac
index:=(sptr++)^.value-lower		!now 0-based index

if u32(index)>=u32(n) then			!out of range
!if u64(index)>=u64(n) then			!out of range
	return ref int((pcptr+n*2+4)^)
else					!in range
	return ref int((pcptr+index*2+4)^)	!+3 for sw cmd + 1 to label part of (kjumptable,label) pair
fi
end

global function k_cswitch:ref void =	! K_CSWITCH
pclunimpl(kcswitch)
return pcptr+4
end

global function k_new:ref void =	! K_NEW
pclunimpl(knew)
return pcptr + 1
end

global function k_to_f:ref void =	! K_TO_F
if --(variant(frameptr+getopndb))^.value then
	return ref intpc(getopnda)
else
	return pcptr + 3
fi
end

global function k_for_fci:ref void =	! K_FOR_FCI
if ++((variant(frameptr+getopndb))^.value)<=getopndc then
	return ref intpc(getopnda)
else
	return pcptr + 4
fi
end

global function k_for_ff:ref void =	! K_FOR_FF
if ++((variant(frameptr+getopndb))^.value)<=((variant(frameptr+getopndc))^.value) then
	return ref int(getopnda)
else
	return pcptr + 4
fi
end

global function k_ford_fci:ref void =	! K_FORD_FCI
if --((variant(frameptr+getopndb))^.value)>=getopndc then
	return ref intpc(getopnda)
else
	return pcptr + 4
fi
end

global function k_ford_ff:ref void =	! K_FORD_FF
if --((variant(frameptr+getopndb))^.value)>=((variant(frameptr+getopndc))^.value) then
	return ref intpc(getopnda)
else
	return pcptr + 4
fi
end

global function k_call:ref void =	! K_CALL
!const countinterval=100
const countinterval=10
!const countinterval=1
static int count=countinterval
static ref byte dummyfp
int  ticks
!var [100]byte m

if --count=0 then
	count:=countinterval
	os_peek()
fi

if sptr<=stacklimit then
	PCERROR("STACK OVERFLOW")
fi


(--sptr)^.tagx:=tretaddr

sptr^.uret.retaddr:=pcptr+3

sptr^.uret.frameptr_low:=int32@(frameptr)
sptr^.uret.stackadj:=getopndb
frameptr:=cast(sptr)

return ref intpc(getopnda)
end

global function k_callptr:ref void =	! K_CALLPTR
ref intpc newpc

if sptr^.tag<>trefproc then
	cpl ttname[sptr^.tag]
	pcerror("callptr: not refproc")
fi
newpc:=ref intpc(sptr^.value)

if getopnda<>(newpc-1)^ then
	cpl getopnda,(newpc-1)^
	pcerror("callptr wrong # params")
fi

sptr^.tagx:=tretaddr
sptr^.uret.retaddr:=pcptr+3

sptr^.uret.frameptr_low:=int32@(frameptr)
sptr^.uret.stackadj:=getopndb
frameptr:=cast(sptr)

return newpc
end

global function k_return:ref void =	! K_RETURN
if sptr^.tag<>tretaddr then
	cpl ttname[sptr^.tag]
	pcerror("Return error")
fi
sptr^.tag:=0
pcptr:=sptr^.uret.retaddr
(ref int32(&frameptr))^:=sptr^.uret.frameptr_low

sptr:=variant((ref byte(sptr)+sptr^.uret.stackadj))
++sptr

return pcptr
end

global function k_startdll:ref void =	! K_STARTDLL
if ++dllcallindex>maxdllindex then
	pcerror("nested dll max")
fi
dllcallstack[dllcallindex]:=dllindex	!remember dllindex just before params are pushed
return pcptr + 1
end

global function k_pushdll:ref void =	! K_PUSHDLL
int s,t,u
object p

if ++dllindex>(maxdllindex-1) then	!allow 1-elem margin in case this is a double
	pcerror("dll params")
fi

s:=sptr^.tag

if s=tvoid then
	pcerror("pushdll void arg")
fi

t:=ttbasetype[getopnda]
!dlltypes[dllindex]:=t
u:=tword

!CPL "PUSHDLL",=ttname[s],=ttname[t]

switch t
when tvoid then				!used in variadic params; type is not known
	t:=s

	switch s
	when tstring then
		dllparams[dllindex]:=cast(convCstring(sptr^.objptr.ustr.strptr,
			sptr.objptr.ustr.length))
	else
		dllparams[dllindex]:=sptr^.value
	endswitch

	if s=treal then u:=treal fi

when ti64,tu64,ti32,tu32 then
	if $targetbits=32 then
!		pcerror("pushdll i64=>i32,i32 needed")
	fi
	case s
	when tint,tword then
	when treal then
		sptr^.value:=sptr^.xvalue
	when trefpacked then
		sptr^.value:=int64(sptr^.uref.ptr)
	else
error::
		cpl ttname[s],"should be",ttname[t]
		pcerror("DLL: param wrong type")
	esac
	dllparams[dllindex]:=sptr^.value

when tr64 then
	if $targetbits=32 then
!		pcerror("pushdll r64=>i32,i32 needed")
	fi
	case s
	when tint,tword then
		sptr^.xvalue:=sptr^.value
	when treal then
	else
		goto error
	esac
	u:=treal
	dllparams[dllindex]:=sptr^.value

when tstring then
	case s
	when tstring then
		p:=sptr^.objptr
		dllparams[dllindex]:=cast(convCstring(p.ustr.strptr,p.ustr.length))
	when tint then
		if sptr^.value<>0 then
			goto error
		fi
		dllparams[dllindex]:=0
	else
		goto error
	esac

when trefpacked then
	if sptr.hasref then
		dllparams[dllindex]:=int64(sptr.objptr.uarray.ptr)
	else
		dllparams[dllindex]:=int64(sptr.refptr)
	fi
when trefm,tintm,twordm,tu16 then
	dllparams[dllindex]:=sptr^.value
else
CPL TTNAME[T]
pcerror("OTHER DLL PARAM")
!CPL "OTHER"
	dllparams[dllindex]:=sptr^.value
endswitch

!CPL "PUSHDLL",REAL@(DLLPARAMS[DLLINDEX])
!CPL "PUSHDLL",(DLLPARAMS[DLLINDEX])

dlltypes[dllindex]:=u

if sptr^.tag=tstring then
	pc_unshare(sptr) when sptr.hasref
fi

++sptr					!pop stack

return pcptr + 2
end

global function k_calldll:ref void =	! K_CALLDLL
int nparams,offset

if dllcallindex<=0 then
	pcerror("calldll??")
fi
offset:=dllcallstack[dllcallindex]		!dll param index just before the first param
nparams:=dllindex-offset

os_calldll(getopndb,getopnda,offset,nparams,getopndc,sptr)

dllindex:=dllcallstack[dllcallindex]
--dllcallindex

return pcptr + 4
end

global function k_callhost:ref void =	! K_CALLHOST
callhostfunction(getopnda,0)
return pcptr+2
end

global function k_stackframe:ref void =	! K_STACKFRAME
int i,n

n:=getopnda

to n do
	(--sptr)^.tagx:=tvoid
	sptr^.value:=0
od

return pcptr + 2
end

global function k_free:ref void =	! K_FREE
int i,n
object p

n:=getopnda
to n do
!CPL "FREE/UNSHARE",TTNAME[SPTR.TAG]
!IF SPTR.TAG=TLIST THEN
!	CPL =SPTR.OBJPTR.REFCOUNT,SPTR.OBJPTR.ULIST.LENGTH
!FI

	pc_unshare(sptr) when sptr.hasref
!CPL "DONE UNSHARE"
	++sptr
od
return pcptr + 2
end

global function k_addsp:ref void =	! K_ADDSP
sptr:=variant((ref byte(sptr)+getopnda))
return pcptr+2
end

global function k_stop:ref void =	! K_STOP
stopped:=1
return pcptr+1
end

global function k_test:ref void =	! K_TEST
pclunimpl(ktest)
return pcptr+2
end

global function k_makelist:ref void =	! K_MAKELIST
int n,lower

n:=getopnda
lower:=getopndb

pc_makelist(n,sptr,sptr+n-1,lower)
sptr+:=(n-1)

return pcptr + 3
end

global function k_makerecord:ref void =	! K_MAKERECORD
int n,t

n:=getopnda
t:=getopndb

pc_makerecord(n,t,sptr,sptr+n-1)
sptr+:=(n-1)

return pcptr + 3
end

global function k_makearray:ref void =	! K_MAKEARRAY
int n,lower,t

n:=getopnda
lower:=getopndb
t:=getopndc
if ttbasetype[t]=tarray then
	pc_makearray(n,t,getopndd,lower,sptr,sptr+n-1)
else
PCERROR("MAKEBITS")
!	pc_makebits(n,t,getopndd,lower,sptr,sptr+n-1)
fi
sptr+:=(n-1)

return pcptr + 5
end

global function k_makestruct:ref void =	! K_MAKESTRUCT
int n,t

n:=getopnda
t:=getopndb

pc_makestruct(n,t,sptr,sptr+n-1)
sptr+:=(n-1)

return pcptr + 3
end

global function k_makeset:ref void =	! K_MAKESET
int n

n:=getopnda
pc_makeset(n,sptr,sptr+n-1)
sptr+:=(n-1)

return pcptr + 2
end

global function k_makerange:ref void =	! K_MAKERANGE
variant x,y
y:=sptr++
pc_makerange(sptr,y,sptr)

return pcptr + 1
end

global function k_makedict:ref void =	! K_MAKEDICT
int n

n:=getopnda

pc_makedict(n,sptr,sptr+n*2-1)
sptr+:=(n*2-1)
return pcptr + 2
end

global function k_pushdot:ref void =	! K_PUSHDOT
variant x,p
int index,j,k,n,fieldtype,needfree
ref byte xptr
int i,dx,ix
ref genfielddatarec gd,gd0
varrec v

v:=sptr^

case v.tag
when trecordlink then
	v.tagx:=v.uref.elemtag
elsif not v.hasref then
	pcerror("pushdot/not record")
esac

xptr:=cast(v.objptr.urec.vptr)

gd0:=gd:=&genfielddata[ix:=genfieldnames[getopnda].dataindex]
n:=genfieldnames[getopnda].datalength

to n do
	if gd.recordtype=v.tag then	!find matching entry in field data
		fieldtype:=gd.fieldtype
		if fieldtype=tvariant then			!record type
			sptr^:=variant((xptr+gd^.offset))^
			if sptr.hasref then
				++(sptr.objptr.refcount)
			fi
			pc_unshare(&v) when v.hasref
			return pcptr + 2

		elsif fieldtype=trefproc then		!method name
			dx:=gd-gd0
			sptr^.tagx:=trefproc
			sptr^.refptr:=cast(genfieldpcaddress[ix+(gd-gd0)])
			pc_unshare(&v) when v.hasref
			return pcptr + 2
		else						!struct type
			pc_loadpacked(xptr+gd^.offset,fieldtype,sptr,nil)
			pc_unshare(&v) when v.hasref
			return pcptr + 2
		fi
	fi
	++gd
od

cpl "Field:",ngenfieldnames,genfieldnames[getopnda].name
pcustypet("Dotg: wrong record type",v.tag)
return pcptr
end

global function k_pushdotref:ref void =	! K_PUSHDOTREF
varrec v
variant p
int index,n,xtag,fieldtype,isrefvar,rectype,offset
ref genfielddatarec gd

v:=sptr^

case v.tag
when trefvar then
	p:=v.varptr				!point to ref target
	isrefvar:=1
	rectype:=p^.tag		!record/struct type pointed to

	if rectype=trecordlink then
!		p:=p^.varptr
		rectype:=p^.uref.elemtag
	fi
	unless p.objptr.urec.mutable then
!PCERROR("PUSHDOTREF/COW")
		p^.objptr:=copyonwrite(p^.objptr,p^.tag)
	end

when trefpacked then
	isrefvar:=0
	rectype:=v.uref.elemtag		!struct type pointed to

else
	pcustype("&dotg not ref",&v)
	return pcptr
esac

!index:=getopnda		!index into genfieldnames

gd:=&genfielddata[genfieldnames[getopnda].dataindex]
n:=genfieldnames[getopnda].datalength

to n do
	if gd^.recordtype=rectype then	!find matching entry in field data
		fieldtype:=gd^.fieldtype
		offset:=gd^.offset
		if isrefvar then			!assume fieldtype will be variant
			if fieldtype=tvariant then		!p points to record var
				sptr.refptr:=ref byte(p^.objptr.urec.vptr)+offset	!modify refvar to point to field

			else					!refvar points to struct
				sptr^.tagx:=trefpacked
				sptr^.refptr:=p.objptr.uarray.ptr+offset	!modify refvar to point to field
				sptr^.uref.elemtag:=fieldtype
			fi
		else					!was ref, presumably to a packed struct
			sptr^.uref.ptr +:= offset
			sptr^.uref.elemtag:=fieldtype
		fi
		return pcptr+2
	fi
	++gd
od

pcustypet("&Dotg: wrong record type",rectype)

return pcptr + 2
end

global function k_softconv:ref void =	! K_SOFTCONV
!used to do type_punning
int t

t:=getopnda
sptr^.tagx:=t
return pcptr + 2
end

global function k_hardconv:ref void =	! K_HARDCONV
int s,t

s:=sptr^.tag
t:=getopnda

if sptr^.tag<>t then
	pc_iconvert(t,sptr)
fi
return pcptr+2
end

global function k_mixed:ref void =	! K_MIXED
pclunimpl(kmixed)
return pcptr+1
end

global function k_incrptr:ref void =	! K_INCRPTR
variant p
varrec v

p:=sptr++

switch ttbasetype[p^.tag]
when trefvar then			!increment what ptr points to
	p:=p^.varptr
	switch p^.tag
	when tint then
		++p^.value
	when trefvar then			!incr the pointer
		++p^.varptr
	when trefpacked then			!incr the pointer
		p^.uref.ptr+:=ttsize[p^.uref.elemtag]
	else
		pcustype("incrptr/refvar",p)
	endswitch
when trefpacked then			!incr the packed type pointed to
	switch p^.uref.elemtag
!	when ti32 then
!		++(p^.iptr)^
	when tu8,ti8 then

		++(p^.uref.ptr)^
	else
		pcustypet("incrptr/ref",p^.uref.elemtag)
	endswitch

else
	pcustype("incrptr",p)
endswitch
return pcptr + 1
end

global function k_incrto_m:ref void =	! K_INCRTO_M
variant a

a:=variant(getopnda)

switch ttbasetype[a^.tag]
when tint  then
	++a^.value
when trefvar then
	++a^.varptr
when trefpacked then
	a^.uref.ptr+:=ttsize[a^.uref.elemtag]
else
	pcustype("INCRTO_M",a)
endswitch

return pcptr + 2
end

global function k_incrto_f:ref void =	! K_INCRTO_F
variant a
int offset

a:=variant(frameptr+getopnda)

switch a^.tag
when tint then
	++a^.value
	return pcptr+2
when trefvar then
	++a^.varptr

when trefpacked then
	a^.uref.ptr+:=ttsize[a^.uref.elemtag]
when trefbit then
	if a^.uref.bitlength then
PCERROR("INCR/BITFIELD")
	fi
	offset:=a^.uref.bitoffset+stdtypewidths[a^.uref.elemtag]
	if offset>=8 then
		offset:=0
		++a^.uref.ptr
	fi
	a^.uref.bitoffset:=offset
else
	pcustype("INCRTO_F",a)
endswitch

return pcptr + 2
end

global function k_loadincr:ref void =	! K_LOADINCR
varrec ptr

ptr:=sptr^				!copy of pointer

pc_loadptr(sptr,sptr)			

--sptr
sptr^:=ptr

return k_incrptr()				!increment the target looks after pcptr too
end

global function k_incrload:ref void =	! K_INCRLOAD
ref int pc
varrec ptr

ptr:=sptr^				!copy of pointer
pc:=k_incrptr()				!increment the target looks after pcptr too
pc_loadptr(&ptr,--sptr)			
return pc
end

global function k_decrptr:ref void =	! K_DECRPTR
variant p
varrec v

p:=sptr++

switch ttbasetype[p^.tag]
when trefvar then			!decrement what ptr points to
	p:=p^.varptr
	switch p^.tag
	when tint then
		--p^.value
	when trefvar then			!decr the pointer
		--p^.varptr
	when trefpacked then			!decr the pointer
		p^.uref.ptr-:=ttsize[p^.uref.elemtag]
	else
		pcustype("decrptr/refvar",p)
	endswitch
when trefpacked then			!decr the packed type pointed to
	switch (p^.uref.elemtag)
	when ti32 then
		--(p^.uref.ptr64)^
	when tu8,ti8 then
		--(p^.uref.ptr)^
	else
		pcustypet("decrptr/ref",p^.uref.elemtag)
	endswitch

else
	pcustype("decrptr",p)
endswitch
return pcptr + 1
end

global function k_decrto_m:ref void =	! K_DECRTO_M
variant a

a:=variant(getopnda)

switch a^.tag
when tint then
	--a^.value
when trefvar then
	--a^.varptr
when trefpacked then
	a^.uref.ptr-:=ttsize[a^.uref.elemtag]
else
	pcustype("DECRTO_M",a)
endswitch

return pcptr + 2
end

global function k_decrto_f:ref void =	! K_DECRTO_F
variant a

a:=variant(frameptr+getopnda)

switch a^.tag
when tint then
	--a^.value
when trefvar then
	--a^.varptr
when trefpacked then
	a^.uref.ptr-:=ttsize[a^.uref.elemtag]
else
	pcustype("DECRTO_F",a)
endswitch

return pcptr + 2
end

global function k_loaddecr:ref void =	! K_LOADDECR
varrec ptr

ptr:=sptr^				!copy of pointer

pc_loadptr(sptr,sptr)			
(--sptr)^:=ptr
return k_decrptr()				!decrement the target looks after pcptr too
end

global function k_decrload:ref void =	! K_DECRLOAD
ref int pc
varrec ptr

ptr:=sptr^				!copy of pointer
pc:=k_decrptr()				!increment the target looks after pcptr too
pc_loadptr(&ptr,--sptr)			
return pc
end

global function k_incr:ref void =	! K_INCR
pclunimpl(kincr)
return pcptr+1
end

global function k_decr:ref void =	! K_DECR
case sptr^.tag
when tint then
	--sptr^.value
else
	pcustype("decr",sptr)
esac
return pcptr+1
end

global function k_neg:ref void =	! K_NEG
!vx_neg(sptr,sptr)
return neg_table[sptr^.tag]^()
end

global function k_abs:ref void =	! K_ABS
return abs_table[sptr^.tag]^()
end

global function k_not:ref void =	! K_NOT
sptr^.value:=not sptr^.value
return pcptr+1
end

global function k_inot:ref void =	! K_INOT
return inot_table[sptr^.tag]^()
end

global function k_istrue:ref void =	! K_ISTRUE
return istrue_table[sptr^.tag]^()
end

global function k_asc:ref void =	! K_ASC
int a
object s

switch sptr^.tag
when tstring then
	s:=sptr.objptr
	if s.ustr.length=0 then
		a:=0
	else
		a:=(s.ustr.strptr)^
		pc_unshare(sptr) when sptr.hasref
	fi
	sptr^.tagx:=tint
	sptr^.value:=a
else
	pcustype("ASC",sptr)
endswitch
return pcptr + 1
end

global function k_chr:ref void =	! K_CHR

!CPL "CHR",SPTR^.UVALUE
switch sptr^.tag
when tint then
	if sptr^.uvalue>u32(255) then
		pcerror("chr>255")
	fi

	pc_makechar(sptr^.value,sptr)
else
	pcustype("CHR",sptr)
endswitch
return pcptr + 1
end

global function k_sqrt:ref void =	! K_SQRT
pcptr +:= 1
switch sptr^.tag
when tint then
	sptr^.tagx:=treal
	sptr^.xvalue:=sqrt(sptr^.value)
when treal then
	sptr^.xvalue:=sqrt(sptr^.xvalue)
else
	pcustype("SQRT",sptr)
endswitch
return pcptr
end

global function k_sqr:ref void =	! K_SQR
variant x
varrec result
x:=sptr

switch x^.tag
when tint then
	sptr^.value:=x^.value*x^.value
when treal then
	sptr^.xvalue:=x^.xvalue*x^.xvalue
when tbignum then
	bx_mul(x,x,&result)
	pc_unshare(x)
	sptr^:=result

else
	pcustype("SQR",x)
endswitch

return pcptr + 1
end

global function k_cube:ref void =	! K_CUBE
pclunimpl(kcube)
return pcptr+1
end

global function k_sin:ref void =	! K_SIN
variant x
x:=sptr

switch x^.tag
when treal then
	sptr^.xvalue:=sin(x^.xvalue)
else
	pcustype("SIN",x)
endswitch

return pcptr + 1
end

global function k_cos:ref void =	! K_COS
variant x
x:=sptr

switch x^.tag
when treal then
	sptr^.xvalue:=cos(x^.xvalue)
else
	pcustype("COS",x)
endswitch

return pcptr + 1
end

global function k_tan:ref void =	! K_TAN
pclunimpl(ktan)
return pcptr+1
end

global function k_asin:ref void =	! K_ASIN
pclunimpl(kasin)
return pcptr+1
end

global function k_acos:ref void =	! K_ACOS
pclunimpl(kacos)
return pcptr+1
end

global function k_atan:ref void =	! K_ATAN
variant x
x:=sptr

switch x^.tag
when tint then
	sptr^.xvalue:=atan(x^.value)
	sptr^.tagx:=treal
when treal then
	sptr^.xvalue:=atan(x^.xvalue)
else
	pcustype("ATAN",x)
endswitch

return pcptr+1
end

global function k_sign:ref void =	! K_SIGN
pclunimpl(ksign)
return pcptr+1
end

global function k_ln:ref void =	! K_LN
real x

switch sptr^.tag
when treal then
	sptr^.xvalue:=log(sptr^.xvalue)
!	sptr^.xvalue:=ln(sptr^.xvalue)
when tint then
	x:=sptr^.value
	sptr^.xvalue:=log(x)
else
	pcustype("LN",sptr)
endswitch

return pcptr + 1
end

global function k_log:ref void =	! K_LOG
pclunimpl(klog)
return pcptr+1
end

global function k_lg:ref void =	! K_LG
pclunimpl(klg)
return pcptr+1
end

global function k_exp:ref void =	! K_EXP
pclunimpl(kexp)
return pcptr+1
end

global function k_round:ref void =	! K_ROUND
variant x
x:=sptr

switch x^.tag
when treal then
	if x^.xvalue>=0.0 then
		sptr^.xvalue:=floor(x^.xvalue+0.5)
	else
		sptr^.xvalue:=ceil(x^.xvalue-0.5)
	fi
when tint then
else
	pcustype("ROUND",x)
endswitch

return pcptr + 1
end

global function k_floor:ref void =	! K_FLOOR
variant x
x:=sptr

switch x^.tag
when treal then
	if x^.xvalue>=0.0 then
!		sptr^.xvalue:=floor(x^.xvalue+0.5)
		sptr^.xvalue:=floor(x^.xvalue)
	else
!		sptr^.xvalue:=ceil(x^.xvalue-0.5)
		sptr^.xvalue:=ceil(x^.xvalue)
	fi
else
	pcustype("ROUND",x)
endswitch

return pcptr + 1
end

global function k_ceil:ref void =	! K_CEIL
pclunimpl(kceil)
return pcptr+1
end

global function k_fract:ref void =	! K_FRACT
pclunimpl(kfract)
return pcptr+1
end

global function k_negto:ref void =	! K_NEGTO
pclunimpl(knegto)
return pcptr+1
end

global function k_absto:ref void =	! K_ABSTO
pclunimpl(kabsto)
return pcptr+1
end

global function k_notto:ref void =	! K_NOTTO
pclunimpl(knotto)
return pcptr+1
end

global function k_inotto:ref void =	! K_INOTTO
pclunimpl(kinotto)
return pcptr+1
end

global function k_len:ref void =	! K_LEN
return len_table[sptr^.tag]^()
end

global function k_lwb:ref void =	! K_LWB
return lwb_table[sptr^.tag]^()
end

global function k_upb:ref void =	! K_UPB
return upb_table[sptr^.tag]^()
end

global function k_bounds:ref void =	! K_BOUNDS
return bounds_table[sptr^.tag]^()
end

global function k_bits:ref void =	! K_BITS
case sptr^.tag
when ttype then
	sptr^.value:=ttbitwidth[sptr^.value]
else
	sptr^.value:=ttbitwidth[sptr^.tag]
esac

sptr^.tagx:=tint
return pcptr+1
end

global function k_bytes:ref void =	! K_BYTES
int m,n
object p

m:=sptr^.tag
if m=ttype then
	m:=sptr^.value
fi
p:=sptr^.objptr

case ttbasetype[m]
when tstring then
	n:=p.ustr.length
when tarray then
	n:=p.uarray.length*ttsize[p.uarray.elemtag]
when tset then
	n:=p.uset.length/8
when tbits then
	case p.ubits.elemtag
	when tbit then n:=p.ubits.length/8
	when tbit2 then n:=p.ubits.length/4
	when tbit4 then n:=p.ubits.length/2
	esac
when tlist then
	n:=p.ulist.length*varsize
!when tstruct then
else
	n:=ttsize[m]
esac

pc_unshare(sptr) when sptr.hasref

sptr^.tagx:=tint
sptr^.value:=n
return pcptr+1
end

global function k_type:ref void =	! K_TYPE
int res
res:=sptr^.tag
pc_unshare(sptr) when sptr.hasref

sptr^.tagx:=ttype
sptr^.value:=res

return pcptr + 1
end

global function k_elemtype:ref void =	! K_ELEMTYPE
int res

case ttbasetype[sptr^.tag]
when tarray then
	res:=sptr.objptr.uarray.elemtag
	pc_unshare(sptr)
when tbits then
	res:=sptr.objptr.ubits.elemtag
	pc_unshare(sptr)
when trefpacked,trefbit then
	res:=sptr.uref.elemtag
else
	pcerror("elemtype")
esac

sptr^.tagx:=ttype
sptr^.value:=res
return pcptr+1
end

global function k_basetype:ref void =	! K_BASETYPE
int res

if sptr^.tag=ttype then
	res:=ttbasetype[sptr^.value]
else
	res:=ttbasetype[sptr^.tag]
fi
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=ttype
sptr^.value:=res

return pcptr + 1
end

global function k_minval:ref void =	! K_MINVAL
int t
int64 a

switch (sptr^.tag)
when tint then
	t:=ti64
when treal then
	t:=tr64
when ttype then
	t:=sptr^.value
when tbignum then
	t:=tbignum
else
	pcustype("Maxval",sptr)
endswitch

case t
when tu8,tu16,tu32,tu64 then a:=0
when ti8 then a:=-128
when ti16 then a:=-32768
when ti32 then a:=-0x8000'0000
when ti64 then a:=-0x8000'0000'0000'0000
when tbignum then
 a:=-0x8000'0000'0000'0000
else
cpl gettypename(t)
	pcerror("MINVALUE")
esac
sptr^.tagx:=tint
sptr^.value:=a

return pcptr + 1
end

global function k_maxval:ref void =	! K_MAXVAL
int t
int64 a

switch (sptr^.tag)
when tint then
	t:=ti64
when treal then
	t:=tr64
when ttype then
	t:=sptr^.value
else
	pcustype("Maxval",sptr)
endswitch

case t
when tu8 then a:=255
when tu16 then a:=65536
when tu32 then a:=0xFFFF'FFFF
when tu64 then a:=0xFFFF'FFFF'FFFF'FFFF
when ti8 then a:=127
when ti16 then a:=32767
when ti32 then a:=0x7FFF'FFFF
when ti64 then a:=0x7FFF'FFFF'FFFF'FFFF
else
cpl gettypename(t)
	pcerror("MAXVALUE")
esac
sptr^.tagx:=tint
sptr^.value:=a

return pcptr + 1
end

global function k_isint:ref void =	! K_ISINT
if sptr^.tag=tint or sptr^.tag=tword then
	sptr^.value:=1
else
	pc_unshare(sptr) when sptr.hasref
	sptr^.value:=0
fi
sptr^.tagx:=tint
return pcptr + 1
end

global function k_isreal:ref void =	! K_ISREAL
if sptr^.tag=treal then
	sptr^.value:=1
else
	pc_unshare(sptr) when sptr.hasref
	sptr^.value:=0
fi
sptr^.tagx:=tint
return pcptr + 1
end

global function k_isstring:ref void =	! K_ISSTRING
int n

n:=(sptr^.tag=tstring)
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=tint
sptr^.value:=n
return pcptr + 1
end

global function k_isrange:ref void =	! K_ISRANGE
int n

n:=(sptr^.tag=trange)
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=tint
sptr^.value:=n
return pcptr + 1
end

global function k_isnumber:ref void =	! K_ISNUMBER
pclunimpl(kisnumber)
return pcptr+1
end

global function k_isarray:ref void =	! K_ISARRAY
int n
switch ttbasetype[sptr^.tag]
when tlist,tarray,tbits then
	n:=1
else
	n:=0
endswitch
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=tint
sptr^.value:=n
return pcptr + 1
end

global function k_isrecord:ref void =	! K_ISRECORD
int n
n:=0
switch ttbasetype[sptr^.tag]
when trecord,tstruct then
	n:=1
endswitch
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=tint
sptr^.value:=n
return pcptr + 1
end

global function k_ispointer:ref void =	! K_ISPOINTER
int n
switch ttbasetype[sptr^.tag]
when trefpacked,trefvar,trefbit,trefproc,treflabel then
	n:=1
else n:=0
endswitch
sptr^.tagx:=tint
sptr^.value:=n
return pcptr + 1
end

global function k_ismutable:ref void =	! K_ISMUTABLE
pclunimpl(kismutable)
return pcptr+1
end

global function k_isset:ref void =	! K_ISSET
int n
n:=ttbasetype[sptr^.tag]=tset
pc_unshare(sptr) when sptr.hasref
sptr^.tagx:=tint
sptr^.value:=n
return pcptr + 1
end

global function k_isvoid:ref void =	! K_ISVOID
if sptr^.tag=tvoid then
	sptr^.tagx:=tint
	sptr^.value:=1
else
	pc_unshare(sptr) when sptr.hasref
	sptr^.tagx:=tint
	sptr^.value:=0
fi
return pcptr + 1
end

global function k_isdef:ref void =	! K_ISDEF
if sptr^.tag<>tvoid then
	sptr^.tagx:=tint
	sptr^.value:=1
else
	pc_unshare(sptr) when sptr.hasref
	sptr^.tagx:=tint
	sptr^.value:=0
fi
return pcptr + 1
end

global function k_tostr:ref void =	! K_TOSTR
pclunimpl(ktostr)
return pcptr+1
end

global function k_isequal:ref void =	! K_ISEQUAL
!compare 2 objects. But one or both can be nil (ie. int with value of 0; no other value allowed)
!x==y	result 1 or 0
!x==0	result 0
!0==y	result 0
!0==0	result 0 as the handles don't point to the same thing; they point nowhere

variant x,y
int xt,yt

y:=sptr
x:=++sptr
xt:=x^.tag
yt:=y^.tag
if xt=trecordlink then
	xt:=x^.uref.elemtag
	x^.hasref:=1
fi
if yt=trecordlink then
	yt:=y^.uref.elemtag
fi

if xt=yt and x^.hasref then
	sptr^.tagx:=tint
	sptr^.value:=x^.objptr=y^.objptr
	return pcptr+1
fi

if (xt=tint and x^.value=0 and y^.hasref) or\
   (yt=tint and y^.value=0 and x^.hasref) or\
   (xt=tint and yt=tint and x^.value=0 and y^.value) then
	sptr^.tagx:=tint
	sptr^.value:=0
	return pcptr+1
fi
pcmxtypes("ISEQUAL",x,y)
return nil
end

global function k_add:ref void =	! K_ADD
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value+:=y^.value
		return pcptr+1
	when treal then
		y:=sptr++
		sptr^.xvalue+:=y^.xvalue
		return pcptr+1
	else
		return add_table[yt]^()
	end
fi

opc_tableptr:=&add_table
return add_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_sub:ref void =
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value-:=y^.value
		return pcptr+1
	when treal then
		y:=sptr++
		sptr^.xvalue-:=y^.xvalue
		return pcptr+1
	else
		return sub_table[yt]^()
	end
fi

opc_tableptr:=&sub_table
return sub_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_mul:ref void =	! K_MUL
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value*:=y^.value
		return pcptr+1
	when treal then
		y:=sptr++
		sptr^.xvalue*:=y^.xvalue
		return pcptr+1
	else
		return mul_table[yt]^()
	end
fi

opc_tableptr:=&mul_table
return mul_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_div:ref void =	! K_DIV
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.xvalue:=real(sptr^.value)/real(y^.value)
		sptr^.tag:=treal
		return pcptr+1
	when treal then
		y:=sptr++
		sptr^.xvalue/:=y^.xvalue
		return pcptr+1
	else
		return div_table[yt]^()
	end
fi

opc_tableptr:=&div_table
return div_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_idiv:ref void =	! K_IDIV
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value:=sptr^.value/y^.value
		return pcptr+1
	else
		return idiv_table[yt]^()
	end
fi

opc_tableptr:=&idiv_table
return idiv_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_rem:ref void =	! K_REM
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value:=sptr^.value rem y^.value
		return pcptr+1
	else
		return rem_table[yt]^()
	end
fi

opc_tableptr:=&rem_table
return rem_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_divrem:ref void =	! K_DIVREM
variant x,y
int d
word div,remainder

y:=sptr
x:=++sptr
d:=x.value

if x.tag=y.tag=tint then
	x.range_lower:=d/y.value
	x.range_upper:=d rem y.value
!	div:=xdivrem(d,y.value,remainder)
!	x.range_lower:=div
!	x.range_upper:=remainder

	x.tagx:=trange

else
	pcmxtypes("DIVREM",x,y)
fi
return pcptr+1
end

!function xdivrem(word64 a,b, &remainder)word64=
!	word64 q,r
!	assem
!		xor rdx,rdx
!		mov rax,[a]
!		div qword [b]
!		mov [q],rax	
!		mov [r],rdx	
!	end
!	remainder:=r
!	return q
!end

global function k_iand:ref void =	! K_IAND
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value iand:=y^.value
		return pcptr+1
	else
		return iand_table[yt]^()
	end
fi

opc_tableptr:=&iand_table
return iand_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_ior:ref void =	! K_IOR
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value ior:=y^.value
		return pcptr+1
	else
		return ior_table[yt]^()
	end
fi

opc_tableptr:=&ior_table
return ior_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_ixor:ref void =	! K_IXOR
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value ixor:=y^.value
		return pcptr+1
	else
		return ixor_table[yt]^()
	end
fi

opc_tableptr:=&ixor_table
return ixor_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_shl:ref void =	! K_SHL
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value <<:=y^.value
		return pcptr+1
	else
		return shl_table[yt]^()
	end
fi

opc_tableptr:=&shl_table
return shl_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_shr:ref void =	! K_SHR
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value >>:=y^.value
		return pcptr+1
	else
		return shr_table[yt]^()
	end
fi

opc_tableptr:=&shr_table
return shr_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_in:ref void =	! K_IN
variant x,y
int n

y:=sptr
x:=++sptr

n:=dx_varinvar(x,y)
sptr^.tagx:=tint
sptr^.value:=n

return pcptr + 1
end

global function k_notin:ref void =	! K_NOTIN
variant x,y
int n

y:=sptr
x:=++sptr

n:=dx_varinvar(x,y)
sptr^.tagx:=tint

sptr^.value:=not n

return pcptr + 1
end

global function k_inrev:ref void =	! K_INREV
pclunimpl(kinrev)
return pcptr+1
end

global function k_eq:ref void =	! K_EQ
variant x,y
int res

y:=sptr
x:=++sptr

res:=pc_equal(x,y,0)
sptr^.tagx:=tint
sptr^.value:=res
return pcptr + 1
end

global function k_ne:ref void =	! K_NE
variant x,y
int res

y:=sptr
x:=++sptr

res:=pc_equal(x,y,0)
sptr^.tagx:=tint
sptr^.value:=not res
return pcptr + 1
end

global function k_lt:ref void =	! K_LT
variant x,y
int res

y:=sptr
x:=++sptr

res:=pc_compare(x,y)
sptr^.tagx:=tint
sptr^.value:=res<0
return pcptr + 1
end

global function k_le:ref void =	! K_LE
variant x,y
int res

y:=sptr
x:=++sptr

res:=pc_compare(x,y)
sptr^.tagx:=tint
sptr^.value:=res<=0
return pcptr + 1
end

global function k_ge:ref void =	! K_GE
variant x,y
int res

y:=sptr
x:=++sptr

res:=pc_compare(x,y)
sptr^.tagx:=tint
sptr^.value:=res>=0
return pcptr + 1
end

global function k_gt:ref void =	! K_GT
variant x,y
int res

y:=sptr
x:=++sptr

res:=pc_compare(x,y)
sptr^.tagx:=tint
sptr^.value:=res>0
return pcptr + 1
end

global function k_min:ref void =	! K_MIN
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value min:=y^.value
		return pcptr+1
	when treal then
		y:=sptr++
		sptr^.xvalue min:=y^.xvalue
		return pcptr+1
	else
		return min_table[yt]^()
	end
fi

opc_tableptr:=&min_table
return min_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_max:ref void =	! K_MAX
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	switch yt
	when tint then
		y:=sptr++
		sptr^.value max:=y^.value
		return pcptr+1
	when treal then
		y:=sptr++
		sptr^.xvalue max:=y^.xvalue
		return pcptr+1
	else
		return max_table[yt]^()
	end
fi

opc_tableptr:=&max_table
return max_dtable[sigmap[(sptr+1)^.tag,yt]]^()
end

global function k_concat:ref void =	! K_CONCAT
variant y
int yt

yt:=sptr^.tag

if (sptr+1)^.tag=yt then
	return concat_table[yt]^()
fi
pcmxtypes("CONCAT",sptr,sptr)
return pcptr+1
end

global function k_append:ref void =	! K_APPEND
return append_table[(sptr+1)^.tag]^()
end

global function k_power:ref void =	! K_POWER
variant x,y
int xt,yt
varrec result

y:=sptr
x:=++sptr
xt:=x^.tag
yt:=y^.tag

if xt<>yt then		!types are mixed
!	if xt=tbignum and yt=tint then
!		bx_power(x,y^.value,&result)
!		pc_unshare(x)
!		sptr^:=result
!		return pcptr+1
!	fi
	if (xt:=dx_mixed(x,y))=0 then	!couldn't find a common type
		pcmxtypes("**MIXED",x,y)
	fi
fi

switch (xt)
when tint then
	sptr^.value:=ipower(x^.value,y^.value)
when treal then
	sptr^.xvalue:=x^.xvalue**y^.xvalue
when tbignum then
	bx_power(x,bx_int(y),&result)
	pc_unshare(x)
	pc_unshare(y) when y^.hasref
	sptr^:=result

else
	pcustype("**",x)
endswitch
return pcptr+1
end

global function k_atan2:ref void =	! K_ATAN2
pclunimpl(katan2)
return pcptr+1
end

global function k_addto:ref void =	! K_ADDTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return addto_table[yt]^()
	fi

	opc_tableptr:=&addto_table
	return addto_dtable[sigmap[x^.tag,yt]]^()

else
	pcerror("addto not ptr")
fi

pcmxtypes("addto",x,sptr)
return pcptr+1
end

global function k_subto:ref void =	! K_SUBTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return subto_table[yt]^()
	fi

	opc_tableptr:=&subto_table
	return subto_dtable[sigmap[x^.tag,yt]]^()

else
	pcerror("subto not ptr")
fi

pcmxtypes("subto",x,sptr)
return pcptr+1
end

global function k_multo:ref void =	! K_MULTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return multo_table[yt]^()
	fi
else
	pcerror("multo not ptr")
fi

pcmxtypes("multo",x,sptr)
return pcptr+1
end

global function k_divto:ref void =	! K_DIVTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return divto_table[yt]^()
	fi
else
	pcerror("divto not ptr")
fi

pcmxtypes("divto",x,sptr)
return pcptr+1
end

global function k_idivto:ref void =	! K_IDIVTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return idivto_table[yt]^()
	fi
else
	pcerror("idivto not ptr")
fi

pcmxtypes("idivto",x,sptr)
return pcptr+1
end

global function k_iandto:ref void =	! K_IANDTO
variant x,p
varrec ptr
int yt
ref intpc pc

yt:=sptr^.tag
p:=sptr+1

if p^.tag=trefvar then
	x:=p^.varptr
	if x^.tag=yt then
		return iandto_table[yt]^()
	fi
	pcmxtypes("iandto",x,sptr)
else
	ptr:=p^
	pc_loadptr(p,p)
	pc:=k_iand()
	pc_storeptr(&ptr,sptr++)
	return pc
fi

return pcptr+1
end

global function k_iorto:ref void =	! K_IORTO
variant x,p
varrec ptr
int yt
ref intpc pc

yt:=sptr^.tag
p:=sptr+1

if p^.tag=trefvar then
	x:=p^.varptr
	if x^.tag=yt then
		return iorto_table[yt]^()
	fi
	pcmxtypes("iorto",x,sptr)
else
	ptr:=p^
	pc_loadptr(p,p)
	pc:=k_ior()
	pc_storeptr(&ptr,sptr++)
	return pc
fi

return pcptr+1
end

global function k_ixorto:ref void =	! K_IXORTO
variant x,p
varrec ptr
int yt
ref intpc pc

yt:=sptr^.tag
p:=sptr+1

if p^.tag=trefvar then
	x:=p^.varptr
	if x^.tag=yt then
		return ixorto_table[yt]^()
	fi
	pcmxtypes("ixorto",x,sptr)
else
	ptr:=p^
	pc_loadptr(p,p)
	pc:=k_ixor()
	pc_storeptr(&ptr,sptr++)
	return pc
fi

return pcptr+1
end

global function k_shlto:ref void =	! K_SHLTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return shlto_table[yt]^()
	fi
else
	pcerror("Shlto not ptr")
fi

pcmxtypes("Shlto",x,sptr)
return pcptr+1
end

global function k_shrto:ref void =	! K_SHRTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return shrto_table[yt]^()
	fi
else
	pcerror("Shrto not ptr")
fi

pcmxtypes("Shrto",x,sptr)
return pcptr+1
end

global function k_minto:ref void =	! K_MINTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return minto_table[yt]^()
	fi
else
	pcerror("minto not ptr")
fi

pcmxtypes("minto",x,sptr)
return pcptr+1
end

global function k_maxto:ref void =	! K_MAXTO
variant x
int yt
yt:=sptr^.tag

if (sptr+1)^.tag=trefvar then
	x:=(sptr+1)^.varptr
	if x^.tag=yt then
		return maxto_table[yt]^()
	fi
else
	pcerror("maxto not ptr")
fi

pcmxtypes("maxto",x,sptr)
return pcptr+1
end

global function k_concatto:ref void =	! K_CONCATTO
if (sptr+1)^.tag=trefvar then
	return concatto_table[(sptr+1)^.varptr^.tag]^()
fi
pcerror("Concatto not ptr")
return pcptr+1
end

global function k_appendto:ref void =	! K_APPENDTO
if (sptr+1)^.tag=trefvar then
	return appendto_table[(sptr+1)^.varptr^.tag]^()
fi
pcerror("Appendto not ptr")
return pcptr+1
end

global function k_pushix:ref void =	! K_PUSHIX

!CPL "PUSHIX",TTNAME[(SPTR+1).TAG],TTNAME[SPTR.TAG]

return pushix_dtable[sigmap[(sptr+1)^.tag,sptr^.tag]]^()
end

global function k_pushdotix:ref void =	! K_PUSHDOTIX
return pushdotix_dtable[sigmap[(sptr+1)^.tag,sptr^.tag]]^()
end

global function k_pushkeyix:ref void =	! K_PUSHKEYIX
variant d,k,p

d:=sptr++			!d is the dict
k:=sptr			!k is the key

if d^.tag<>tdict then
	pcustype("keyix",d)
fi

!p:=finddictitem(d.objptr,k,0)
p:=finddictitem(d,k,0)

pc_unshare(d)
pc_unshare(k) when k^.hasref

if p then			!found
	sptr^:=p^
	return pcptr+1
fi
sptr^.tagx:=tvoid		!return none when not found
return pcptr+1
end

global function k_pushkeyixd:ref void =	! K_PUSHKEYIXD
variant d,k,p,def

def:=sptr++			!def is any default value to be used
d:=sptr++			!d is the dict
k:=sptr			!k is the key
if d^.tag<>tdict then
	pcustype("keyix",d)
fi

p:=finddictitem(d,k,0)
pc_unshare(d)
pc_unshare(k) when k^.hasref

if p then			!found
	sptr^:=p^
	pc_unshare(def) when def^.hasref
	return pcptr+1
fi
sptr^:=def^			!use given default value when not found
return pcptr+1
end

global function k_pushixref:ref void =	! K_PUSHIXREF
variant p
ref int qq

p:=sptr+1

if p^.tag=trefvar then
!CPL "PUSHUXREF",TTNAME[P.VARPTR.TAG],TTNAME[SPTR.TAG]
	return pushixref_dtable[sigmap[p^.varptr^.tag,sptr^.tag]]^()
fi
return pcerror("pushixref/not ptr")
end

global function k_pushdotixref:ref void =	! K_PUSHDOTIXREF
variant p

p:=sptr+1

if p^.tag=trefvar then
	return pushdotixref_dtable[sigmap[p^.varptr^.tag,sptr^.tag]]^()
fi
pcerror("pushdotixref/not ptr")
return pcptr+1
end

global function k_pushkeyixref:ref void =	! K_PUSHKEYIXREF
variant d,k,p,pd

d:=sptr^.varptr			!d is the dict
k:=++sptr				!k is the key

if d^.tag<>tdict then
	pcustype("keyixref",d)
fi

!p:=finddictitem(d^.objptr,k,1)
p:=finddictitem(d,k,1)
pc_unshare(k) when k^.hasref

sptr^.tagx:=trefvar
sptr^.varptr:=p
return pcptr+1
end

global function k_pushbyteix:ref void =	! K_PUSHBYTEIX
int64 a
word index
variant x

if sptr^.tag<>tint then
	pcerror("byteix/bad index")
fi
index:=sptr^.value
++sptr
if sptr^.tag<>tint then
	pcerror("byteix/not int")
fi
a:=sptr^.value

case getopnda
when tu8 then
	if index>=8 then
		if index>=12 then
			pcerror("byteix bounds")
		fi
		a:=sptr^.uret.frameptr_low
		sptr^.value:=(a>>((index-8)*8)) iand 255

	else
		sptr^.value:=(a>>(index*8)) iand 255
	fi
else
	pcerror("byteix/bad type")
esac
return pcptr+2
end

global function k_pushbyteixref:ref void =	! K_PUSHBYTEIXREF
ref byte a
word index
variant p

if sptr^.tag<>tint then
	pcerror("&byteix/bad index")
fi
index:=sptr^.value
++sptr
if sptr^.tag<>trefvar then
	pcerror("&byteix/not ptr")
fi
p:=sptr^.varptr
if p^.tag<>tint then
	pcerror("&bytix/not int")
fi
sptr^.tagx:=trefpacked

case getopnda
when tu8 then
	if index>=8 then
		if index>=12 then
			pcerror("&byteix bounds")
		fi
		sptr^.uref.ptr:=ref byte(p)+index-4
	else
		sptr^.uref.ptr:=ref byte(p)+8+index
	fi
	sptr^.uref.elemtag:=tu8
else
	pcerror("&byteix/bad type")
esac
return pcptr+2
end

global function k_appendset:ref void =	! K_APPENDSET
pclunimpl(kappendset)
return pcptr+1
end

global function k_pushdotm:ref void =	! K_PUSHDOTM
pclunimpl(kpushdotm)
return pcptr+3
end

global function k_pushdott:ref void =	! K_PUSHDOTT
pclunimpl(kpushdott)
return pcptr+3
end

global function k_push_ad:ref void =	! K_PUSH_AD
(--sptr)^.tagx:=trefdllproc
sptr^.refptr:=ref void(dllproctable[getopnda].address)
return pcptr+2
end

global function k_push_try:ref void =	! K_PUSH_TRY
(--sptr)^.tagx:=texception
sptr^.refptr:=ref byte(getopnda)
sptr^.uexcept.frameoffset:=frameptr-ref byte(sptr)		!byte offset
sptr^.uexcept.exceptiontype:=getopndb
sptr^.uexcept.nexceptions:=getopndc
return pcptr+4
end

global function k_raise:ref void =	! K_RAISE

if sptr^.tag<>tint then
!	if (sptr-1)^.tag=tint then
!		sptr.hasref:=0
!		--sptr
!	else
		pcerror("Raise: not Int on stack [not proceeding direct to RAISE]")
!	fi
fi
!exceptno:=sptr^.value
return raiseexception(sptr^.value)				!will unwind stack and set pcptr to address of exception code
end

global function k_applyop:ref void =	! K_APPLYOP
static [10]intpc codeseq

codeseq[1]:=cast(cmdmap[sptr^.value])
if sptr^.tag<>toperator then
	pcerror("Apply:no op")
fi
if sptr^.uop.opdims<>getopnda then
cpl getopnda,sptr^.uop.opdims
	pcerror("Apply:wrong #opnds")
fi

++sptr
codeseq[2]:=(pcptr+2)^			!copy jump lab which follows the applyop
codeseq[3]:=(pcptr+3)^			!include the dest label
return &codeseq[1]				!pass control this short sequence
end

global function k_makeiter:ref void =	! K_MAKEITER
object p
p:=sptr^.objptr
sptr^.uiter.itcount:=p.ulist.length+1			!pre-increment

case sptr^.tag
when tlist then							!list keeps it's +refcount during loop
	sptr.varptr:=p.ulist.vptr				!point to first element (ptr can be nil for empty list)
	sptr.tagx:=trefvar
	sptr.uiter.ittype:=tlist
when tstring then
	sptr.uref.ptr:=cast(p.ustr.strptr)			!point to first char
	sptr.tagx:=trefpacked				!doesn't make use of refelemtag
	sptr.uiter.ittype:=tstring
else
	pcustype("makeiter",sptr)
esac

return pcptr+2
end

global function k_forall:ref void =	! K_FORALL
!a=label, b=it var, c=loop var
variant pit, ploopvar, pelem

pit:=variant(frameptr+getopndb)		!p should point to refvar used as iterator
ploopvar:=variant(frameptr+getopndc)

if --pit^.uiter.itcount<=0 then			!end reached
	return pcptr+4
fi
if ploopvar^.hasref then			!assume won't get to zero as copy as in list
	--ploopvar^.objptr^.refcount
fi

case pit^.uiter.ittype
when tlist then
	pelem:=pit^.varptr
	ploopvar^:=pelem^
	if ploopvar^.hasref then ++ploopvar^.objptr^.refcount fi
	++pit^.varptr
when tstring then
	pc_makechar(pit^.uref.ptr^,ploopvar)
	++pit^.uref.ptr
else
	pcerror("forall/type?")
esac
return ref intpc(getopnda)
end

global function k_forallx:ref void =	! K_FORALLX
pclunimpl(kforallx)
return pcptr+5
end

global function k_foreach:ref void =	! K_FOREACH
pclunimpl(kforeach)
return pcptr+4
end

global function k_foreachx:ref void =	! K_FOREACHX
pclunimpl(kforeachx)
return pcptr+5
end

global function k_expandrange:ref void =	! K_EXPANDRANGE
!convert known range type to two ints
variant x

x:=sptr--
sptr^.tagx:=tint
!sptr^.value:=x^.range_lower
sptr^.value:=x^.range_upper
!x^.value:=x^.range_upper
x^.value:=x^.range_lower
x^.tagx:=tint

return pcptr+1
end

global function k_callappl:ref void =
	int index, nargs

	index:=getopnda
	nargs:=getopndb

!!PCERROR("CALLAPPL NOT DONE YET")
!cpl "CALLAPPL NOT DONE YET", =INDEX, APPLPROCTABLE[INDEX].NAME,=NARGS

	do_callapplproc(index, nargs, sptr+nargs)

	to nargs do
		pc_unshare(sptr) when sptr.hasref
		++sptr
	od

	return pcptr+3
end
=== pc_assem.m 23/38 ===
!'PCL' byte-code dispatcher, ASSEM OVERLAY
!
!This dispatcher uses a tight loop of threaded handlers which examine
!each byte-code instruction in turn. It keeps the program counter, stack pointer
!and frame pointer (for the interpreter) in registers.

! It will do one of three things:
!(1) Handle the whole instruction here.
!(2) Handle part of the instruction, for example if there are int operands,
!     otherwise it passes control to the HLL handler
!(3) Passes control directly to the HLL handler
!
!When passing back to HLL, those dedicated registers have to be saved in globals
!used by the HLL, then loaded again when it returns
!This transition slows it down; if every handler did (3), it would run more slowly
!than just using the HLL dispatcher by itself. But for most programs, enough
!byte-codes are handled here in whole or in part, for there to be a net increase
!in performance.
!
!An optimiser is also applied: some common sequences (for example, push_f followed
!by push_f) are recognised, and make use of a dedicated handler. (The JX- functions,
!while the fixed byte-codes use KA- functions). A modest improvement.
!
!The whole thing will only work the M language when targetted at x64.
!When targetted at C, this module is replaced by a dummy one. The -asm
!dispatcher option won't work.
!
!'threadedproc' is just 'proc', but denoting no entry/exit code to be generated
!'endasmproc' just means 'end assem' followed by 'end proc'

import clib
import Mlib
import oslib

import pc_types
import pc_decls
import pc_support
import pc_khandlers
import pq_common
import pc_pcfns

const dooptimise=1
!const dooptimise=0

const cc_copy=0
const cc_copy16=cc_copy<<16

const kopnda	= 8
const kopndb	= 16
const kopndc	= 24
const kopndd	= 32
const varsize	= 16
const varshift	= 4
const intpsize	= 8

const intpsize2	= intpsize*2
const intpsize4	= intpsize*4
const intpsize6	= intpsize*6

const copymask	= 0x10000
const hasrefmask= 0x10000
const xa		= 0
const xb		= varsize
const xc		= varsize*2
const ya		= 0
const yb		= varsize
const za		= 0

const kdataindex	= 8
const kdatalength	= 12

const krecordtype	= 4
const kfieldtype	= 8
const kfieldoffset	= 12

const ktag			= 0
const khasref		= 2
const kstackadj		= 3
const kopdims		= 3
const kittype		= 3
const krefelemtag	= 4
const kitcount		= 4
const kbndigits		= 4
const kvarptr		= 8
const kobjptr		= 8
const kpackptr		= 8
const kvalue		= 8
const kretaddr		= 8
const kframeptr_low	= 4
const krange_lower	= 8
const krange_upper	= 12

const jrefcount		= 0
const jelemtag		= 22
const jobjtype		= 6
const jmutable		= 7
const jptr			= 8
const jstrptr		= 8
const jvptr			= 8

const jlength		= 16
const jlower		= 20
const jallocated	= 24
const jobjptr2		= 24

!macro Dprog = D8
!macro Dsptr = D9

macro cloadregs  =
	assem
		mov Dprog,[pcptr]
		mov Dsptr,[sptr]
		mov Dframe,[frameptr]
	end

macro csaveregs  =
	assem
		mov [pcptr],Dprog
		mov [sptr],Dsptr
		mov [frameptr],Dframe
	end

macro jumpnext   =
	assem
		mov D0,[Dprog]
		jmp D0
	end

!macro jumpnext   =
!	assem
!		*csaveregs
!		call showasmcmd
!		*cloadregs
!
!		mov D0,[Dprog]
!		jmp D0
!	end

macro saveregs   =
	assem
		mov [pcptr],Dprog
		mov [sptr],Dsptr
		mov [frameptr],Dframe
	end

macro loadregs   =
	assem
		mov Dprog,D0; mov Dsptr,[sptr]; mov Dframe,[frameptr]
	end

macro bjumpnext  =
	assem
		mov D0,[Dprog]; jmp D0
	end

macro pushvar    = asm sub Dsptr, varsize

macro popvar     = asm add Dsptr, varsize

macro pushvar2   = asm sub Dsptr, varsize+varsize

macro popvar2    = asm add Dsptr, varsize+varsize

macro pushvar3   = asm sub Dsptr, varsize+varsize+varsize

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

macro callvxufree_d4    =
	assem
		push D4
		*csaveregs
		call pc_unshare
		*cloadregs
	end

macro callvxufree_dsptr =
	assem
		push Dsptr
		*csaveregs
		call pc_unshare
		*cloadregs
	end

macro callfreex_dsptr   =
	assem
		push Dsptr
		*csaveregs
		call pc_free
		*cloadregs
	end

macro callfreex_d4      =
	assem
		push D4
		*csaveregs
		call pc_free
		*cloadregs
	end

macro callfreex_d5      =
	assem
		push D5
		*csaveregs
		call pc_free
		*cloadregs
	end

macro callm(fn) =
	assem
!		sub dstack,8
		call fn
!		pop d0
	end

include "CCASM_FN."

function optimise_asm(ref intpc p, int cmd)ref intpc=
!look at a single bytecode cmd, and see if it can be optimised
!p points to the start of cmd in pcdata. p^ (ie. what had been cmd) has already
!been fixed up to the handler address, but not any following cmd codes
!It will look at cmd, and some following opcodes. If they can be reduced to
!a single new jx-handler, then the fixups are done. And will return a pointer to
!the following bytecode after that group of ops.
!If the cmd is not optimised, then it returns a pointer to the normally following
!bytecode
ref intpc q
int64 a,b
int n

q:=p+cmdnopnds[cmd]+1			!point to following bytecode
if not dooptimise then
	return q
fi

case cmd
when kpush_f then
	case (p+2)^
	when kpush_f then			!push_f, push_f
		case (p+4)^
		when kpush_f then		!=> jx_push_fff
			p^:=intpc(&jx_push_fff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kadd then			!=> jx_add_ff
			p^:=intpc(&jx_add_ff)
			(p+2)^:=(p+3)^
			return p+5
		when ksub then			!=> jx_sub_ff
			p^:=intpc(&jx_sub_ff)
			(p+2)^:=(p+3)^
			return p+5
		when kjumpeq then
			p^:=intpc(&jx_jumpeq_ff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumpne then
			p^:=intpc(&jx_jumpne_ff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumplt then
			p^:=intpc(&jx_jumplt_ff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumple then
			p^:=intpc(&jx_jumple_ff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumpge then
			p^:=intpc(&jx_jumpge_ff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumpgt then
			p^:=intpc(&jx_jumpgt_ff)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kpushix then
			p^:=intpc(&jx_pushix_ff)
			(p+2)^:=(p+3)^
			return p+5
		else					!=> jx_push_ff
			p^:=intpc(&jx_push_ff)
			(p+2)^:=(p+3)^
			return p+4
		esac

	when kpush_m then			!=> jx_push_fm
		p^:=intpc(&jx_push_fm)
		(p+2)^:=(p+3)^
		return p+4

	when kpush_ci then			!push_f, push_ci
		case (p+4)^
		when kadd then			!=> jx_add_fci
			p^:=intpc(&jx_add_fci)
			(p+2)^:=(p+3)^
			return p+5
		when ksub then			!=> jx_sub_fci
			p^:=intpc(&jx_sub_fci)
			(p+2)^:=(p+3)^
			return p+5
		when kjumpeq then
			p^:=intpc(&jx_jumpeq_fci)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumpne then
			p^:=intpc(&jx_jumpne_fci)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumplt then
			p^:=intpc(&jx_jumplt_fci)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumple then
			p^:=intpc(&jx_jumple_fci)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumpge then
			p^:=intpc(&jx_jumpge_fci)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		when kjumpgt then
			p^:=intpc(&jx_jumpgt_fci)
			(p+2)^:=(p+3)^
			(p+3)^:=(p+5)^
			return p+6
		else					!=> jx_push_fci
			p^:=intpc(&jx_push_fci)
			(p+2)^:=(p+3)^
			return p+4
		esac

	when kpop_m then			!=> jx_move_mf
		p^:=intpc(&jx_move_mf)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4

	when kpop_f then			!=> jx_move_ff
		p^:=intpc(&jx_move_ff)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	when kzpop_f then			!=> jx_zmove_ff
		p^:=intpc(&jx_zmove_ff)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	when kswitch then
		p^:=intpc(&jx_switch_f)
		(p+2)^:=(p+3)^
		(p+3)^:=(p+4)^
		return p+5

!	when kupb then			!=> jx_upb_f
!		p^:=intpc(&jx_upb_f)
!		return p+3

	when klen then
		p^:=intpc(&jx_len_f)
		return p+3
	when kjumpdef then
		p^:=intpc(&jx_jumpdef_f)
		(p+2)^:=(p+3)^
		return p+4

	when kjumpvoid then
		p^:=intpc(&jx_jumpvoid_f)
		(p+2)^:=(p+3)^
		return p+4
	when kpushptr then
		p^:=intpc(&jx_pushptr_f)
		return p+3
	esac

when kpush_m then
	case (p+2)^
	when kpush_m then			!=> jx_push_mm
		p^:=intpc(&jx_push_mm)
		(p+2)^:=(p+3)^
		return p+4

	when kpush_ci then			!=> jx_push_mci
		p^:=intpc(&jx_push_mci)
		(p+2)^:=(p+3)^
		return p+4
	when kpop_m then			!=> jx_move_mm
		p^:=intpc(&jx_move_mm)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	when kpop_f then			!=> jx_move_fm
		p^:=intpc(&jx_move_fm)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	esac

when kpush_ci then
	case (p+2)^
	when kpop_m then			!=> jx_move_mci
		p^:=intpc(&jx_move_mci)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	when kpop_f then			!=> jx_move_fci
		p^:=intpc(&jx_move_fci)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	when kzpop_f then			!=> jx_zmove_fci
		p^:=intpc(&jx_zmove_fci)
		a:=(p+1)^
		b:=(p+3)^
		(p+1)^:=b
		(p+2)^:=a
		return p+4
	esac

when kpushz_void then
	case (p+1)^
	when kpushz_void then
		if (p+2)^=kpushz_void then
			p^:=intpc(jx_pushz_void3)
			return p+3
		fi
		p^:=intpc(jx_pushz_void2)
		return p+2
	esac

when kpush_af then
	case (p+2)^
	when kpush_f then
		if (p+4)^=kpushixref and (p+5)^=kpopptr then
			p^:=intpc(jx_popix_ff)
			(p+2)^:=(p+3)^
			return p+5					!start processing from popptr as that could be used
		elsif (p+4)^=kaddto then
			p^:=intpc(&jx_addto_ff)
			(p+2)^:=(p+3)^
			return p+5
		fi
	when kpush_ci then
		if (p+4)^=kaddto then
			p^:=intpc(&jx_addto_fci)
			(p+2)^:=(p+3)^
			return p+5
		fi
	when kloadincr then
		case (p+3)^
		when kpushptr then
			p^:=intpc(&jx_pushincrptr_f)
			return (p+3)				!takes 4, but do the pushptr as it might be needed
		when kpopptr then
			p^:=intpc(&jx_popincrptr_f)
			return (p+3)				!takes 4, but do the pushptr as it might be needed
		esac

	esac

when kpush_am then
	case (p+2)^
	when kloadincr then
		case (p+3)^
		when kpushptr then
			p^:=intpc(&jx_pushincrptr_m)
			return (p+3)				!takes 4, but do the pushptr as it might be needed
		when kpopptr then
			p^:=intpc(&jx_popincrptr_m)
			return (p+3)				!takes 4, but do the pushptr as it might be needed
		esac
	esac

when kfree then
!CPL "OPTIMISE FREE?"
	n:=(p+1)^
	if n>=1 and n<=10 then
		p^:=getfreelabel(n)
		return p+2
	fi
when kisint,kisreal,kisstring,kisrange,kisnumber,kisarray,kisrecord,
	kispointer,kisset,kisequal then
	case (p+1)^
	when kjumptrue then
		(p+1)^:=intpc(&jx_jumptrue_i)
		return p+3
	when kjumpfalse then
		(p+1)^:=intpc(&jx_jumpfalse_i)
		return p+3
	esac

esac

return q
end

global proc fixup_asm(int mx) =			!ASM PCFIXUP
!mainly, convert opcodes to KA- handler addresses
int cmd,j,a,b,n,cmd2
ref intpc lab
ref intpc p

p:=cast(moduletable[mx].pccode)

do
	cmd:=p^
	lab:=asmhandlertable[cmd]
	p^:=intpc(lab)

	case cmd
	when kendmodule, 0 then
		exit

	else
		p:=optimise_asm(p,cmd)

	esac
od

return
end

global function asmavailable:int= return 1 end

function getfreelabel(int n)intp=
static [2]intpc p
assem
	mov A0,[n]
	mov D0,[D0*8+freetable]	!get &ka_free_1, etc
	mov [p],D0
end
return	p[1]
assem
freetable:
	dq 0
	dq jx_free_10xx.jx_free_1
	dq jx_free_10xx.jx_free_2
	dq jx_free_10xx.jx_free_3
	dq jx_free_10xx.jx_free_4
	dq jx_free_10xx.jx_free_5
	dq jx_free_10xx.jx_free_6
	dq jx_free_10xx.jx_free_7
	dq jx_free_10xx.jx_free_8
	dq jx_free_10xx.jx_free_9
	dq jx_free_10xx.jx_free_10

!	dq jx_free_2
!	dq jx_free_3
!	dq jx_free_4
!	dq jx_free_5
!	dq jx_free_6
!	dq jx_free_7
!	dq jx_free_8
!	dq jx_free_9
!	dq jx_free_10
end
end

function findcmd(ref void cmd)int =
int i
assem
	mov D3,asmhandlertable
	mov D4,[cmd]
	mov D2,0
L1:
	cmp D2,klastcmd
	jge L2
	cmp D4,[D3]
	jz L3
	add D3,8
	inc D2
	jmp L1
L2:
	mov word64 [i],0
	jmp L4
L3:
	mov [i],D2
L4:
end
!for i:=0 to klastcmd do
!	if getasmjump(i)=intpc(cmd) then
!		return i
!	fi
!od
!return 0

return i
end

global proc showasmcmd =
int i
int cmd

!IF RENT<=1 THEN RETURN FI

cmd:=pcptr^

CPL "SHOWASMCMD",CMDNAMES[FINDCMD(CAST(PCPTR^))]
RETURN


!!if not fdebug then return fi
!unless (fdebug and fdtrace) or ftrace then
!	return
!end
!
!for i:=1 to klastcmd-1 do
!
!	if asmhandlertable[i]=cmd then
!		println cmdnames[i]
!
!		return
!	fi
!od
!CPL "CAN'T FIND SHOWCMD CMD"
end

global function disploop_asm:ref int =				!ASM DISPATCHER

disploop()

return nil
end

proc disploop=

freddy::
assem
	push Dframe
	*cloadregs
	*jumpnext
end
charlie::

stoplabel::
asm	pop Dframe

end

!global function getasmjump(int cmd)u32=
!return u32(asmhandlertable[cmd])
!end

threadedproc ka_procstart =	! KA_PROCSTART
assem
	*saveregs
	*callm k_procstart
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_procend =	! KA_PROCEND
assem
	*saveregs
	*callm k_procend
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_nop =	! KA_NOP
assem
	*saveregs
	*callm k_nop
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_endmodule =	! KA_ENDMODULE
assem
	*saveregs
	*callm k_endmodule
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_push_m =	! KA_PUSH_M
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

!	*saveregs
!	*callm k_push_m
!	*loadregs
!	*bjumpnext	! size= 2
endassem endproc

threadedproc ka_push_f =	! KA_PUSH_F
assem
	mov D4,[Dprog+kopnda]
	*pushvar
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+kvalue],D1
	and A0,hasrefmask
	jz L2
	inc word32 [D1+jrefcount]
L2:
	*jumpskip2

!	*saveregs
!	*callm k_push_f
!	*loadregs
!	*bjumpnext	! size= 2
endassem endproc

threadedproc ka_push_am =	! KA_PUSH_AM
assem
	*pushvar
	mov word32 [Dsptr+ktag],trefvar
	mov D4,[Dprog+kopnda]
	mov [Dsptr+kvarptr],D4
	*jumpskip2

!	*saveregs
!	*callm k_push_am
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_af =	! KA_PUSH_AF
assem
	*pushvar
	mov word32 [Dsptr+ktag],trefvar
	mov D4,[Dprog+kopnda]
	lea D0,[D4+Dframe]
	mov [Dsptr+kvarptr],D0
	*jumpskip2

!	*saveregs
!	*callm k_push_af
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_ap =	! KA_PUSH_AP
assem
	*saveregs
	*callm k_push_ap
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_push_al =	! KA_PUSH_AL
assem
	*saveregs
	*callm k_push_al
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_push_ci =	! KA_PUSH_CI
assem
	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov D0,[Dprog+kopnda]
	mov [Dsptr+kvalue],D0
	*jumpskip2

!	*saveregs
!	*callm k_push_ci
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_cw =	! KA_PUSH_CW
assem
	*saveregs
	*callm k_push_cw
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_push_cr =	! KA_PUSH_CR
assem
	*pushvar
	mov word32 [Dsptr+ktag],treal
	mov D0,[Dprog+kopnda]
	mov [Dsptr+kvalue],D0
	*jumpskip2

!	*saveregs
!	*callm k_push_cr
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_cn =	! KA_PUSH_CN
assem
	*pushvar
	mov word32 [Dsptr+ktag],trange
	mov D0,[Dprog+kopnda]
	mov [Dsptr+kvalue],D0
	*jumpskip2

!	*saveregs
!	*callm k_push_cn
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_cs =	! KA_PUSH_CS
assem
	*pushvar
	mov word32 [Dsptr+ktag],tstring+hasrefmask
	mov D0,[Dprog+kopnda]
	mov [Dsptr+kobjptr],D0
	inc word32 [D0+jrefcount]
	*jumpskip2

!	*saveregs
!	*callm k_push_cs
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_t =	! KA_PUSH_T
assem
	*pushvar
	mov A0,[Dprog+kopnda]
	mov word32 [Dsptr+ktag],ttype
	mov [Dsptr+kvalue],D0
	*jumpskip2

!	*saveregs
!	*callm k_push_t
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_push_op =	! KA_PUSH_OP
assem
	*pushvar
	mov A0,[Dprog+kopnda]
	mov word32 [Dsptr+ktag],toperator
	mov [Dsptr+kvalue],D0
	mov A0,[Dprog+kopndb]
	mov [Dsptr+kopdims],B0
	*jumpskip3

!	*saveregs
!	*callm k_push_t
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_pushz =	! KA_PUSHZ
assem
	*pushvar
	mov A0,[Dprog+kopnda]
	mov [Dsptr+ktag],A0
	mov word64 [Dsptr+kvalue],0
	*jumpskip2

!	*saveregs
!	*callm k_pushz
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_pushz_void =	! KA_PUSHZ_VOID
assem
	*pushvar
	mov word32 [Dsptr+ktag],tvoid
	*jumpskip1

!	*saveregs
!	*callm k_pushz_void
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_pushz_str =	! KA_PUSHZ_STR
assem
	*saveregs
	*callm k_pushz_str
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushz_list =	! KA_PUSHZ_LIST
assem
	*saveregs
	*callm k_pushz_list
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushz_listl =	! KA_PUSHZ_LISTL
assem
	*saveregs
	*callm k_pushz_listl
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushz_set =	! KA_PUSHZ_SET
assem
	*saveregs
	*callm k_pushz_set
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushz_arrayl =	! KA_PUSHZ_ARRAYL
assem
	*saveregs
	*callm k_pushz_arrayl
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pop_m =	! KA_POP_M
assem
	mov D4,[Dprog+kopnda]

	cmp byte [D4+khasref],1
	jnz L2
	*callvxufree_d4
	mov D4,[Dprog+kopnda]
L2:
	mov D0,[Dsptr+ktag]
	mov [D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [D4+kvalue],D1
	*popvar
	*jumpskip2

!	*saveregs
!	*callm k_pop_m
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_pop_f =	! KA_POP_F
assem
	mov D4,[Dprog+kopnda]

	cmp byte [Dframe+D4+khasref],1
	jnz L2

	lea D4,[Dframe+D4]
	*callvxufree_d4
	mov D4,[Dprog+kopnda]

L2:
	mov D0,[Dsptr+ktag]
	mov [Dframe+D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [Dframe+D4+kvalue],D1

	*popvar
	*jumpskip2

!	*saveregs
!	*callm k_pop_f
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_store_m =	! KA_STORE_M
assem
	cmp byte[Dsptr+khasref],1
	jnz L1
	mov D1,[Dsptr+kobjptr]
	inc word32 [D1+jrefcount]
L1:
	mov D4,[Dprog+kopnda]
	cmp byte[D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D0,[Dsptr+ktag]
	mov [D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [D4+kvalue],D1
	*jumpskip2

	*saveregs
	*callm k_store_m
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_store_f =	! KA_STORE_F
assem

	cmp byte[Dsptr+khasref],1
	jnz L1
	mov D1,[Dsptr+kobjptr]
	inc word32 [D1+jrefcount]
L1:
	mov D4,[Dprog+kopnda]
	add D4,Dframe
	cmp byte[D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D0,[Dsptr+ktag]
	mov [D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [D4+kvalue],D1
	*jumpskip2

!	*saveregs
!	*callm k_store_f
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_pushptr =	! KA_PUSHPTR
assem
!jmp L99
	cmp word16 [Dsptr+ktag],trefvar
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
	cmp word16 [Dsptr+ktag],trefpacked
	jnz L2
	mov D4,[Dsptr+kpackptr]
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
	*saveregs
	*callm k_pushptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_popptr =	! KA_POPPTR
assem
!jmp L99
	cmp word16 [Dsptr+ya+ktag],trefvar
	jnz L2

	mov D4,[Dsptr+ya+kvarptr]
	cmp byte [D4+khasref],1
	jnz L1
	push D4
	*callvxufree_d4
	pop D4

L1:
	mov D0,[Dsptr+xb+ktag]
	mov [D4+ktag],D0
	mov D1,[Dsptr+xb+kvalue]
	mov [D4+kvalue],D1
	*popvar2
	*jumpskip1

L2:
	cmp word16 [Dsptr+ya+ktag],trefpacked
	jnz L3
	cmp word16 [Dsptr+xb+ktag],tint		!storing i32 to pack dest?
	jnz L3
!	mov A1,[Dsptr+xb+kvalue]		!int value to be stored
	mov D1,[Dsptr+xb+kvalue]		!int value to be stored
	mov D4,[Dsptr+ya+kpackptr]			!dest address
	movzx A0,word16 [Dsptr+ya+krefelemtag]

	cmp A0,tu8
	jnz L20
	mov [D4],B1
	*popvar2
	*jumpskip1

L20:
	cmp A0,ti16
	jnz L21
	mov [D4],W1
	*popvar2
	*jumpskip1
L21:
	cmp A0,ti32
	jnz L22
	mov [D4],A1
	*popvar2
	*jumpskip1

L22:
	cmp A0,ti64
	jnz L23
	mov [D4],D1
	*popvar2
	*jumpskip1
L23:
L3:
L99:
	*saveregs
	*callm k_popptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_storeptr =	! KA_STOREPTR
assem
	*saveregs
	*callm k_storeptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_zpop_m =	! KA_ZPOP_M
assem
	*saveregs
	*callm k_zpop_m
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_zpop_f =	! KA_ZPOP_F
assem
	mov D4,[Dprog+kopnda]
	mov D0,[Dsptr+ktag]
	mov [Dframe+D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [Dframe+D4+kvalue],D1
	*popvar
	*jumpskip2
!L99:
!	*saveregs
!	*callm k_zpop_f
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_zstore_m =	! KA_ZSTORE_M
assem
	*saveregs
	*callm k_zstore_m
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_zstore_f =	! KA_ZSTORE_F
assem
	mov D4,[Dprog+kopnda]
	add D4,Dframe
L2:
	mov D0,[Dsptr+ktag]
	mov [D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [D4+kvalue],D1

	and A0,hasrefmask
	jz L3
	inc word32 [D1+jrefcount]
L3:

	*jumpskip2
L99:
!	*saveregs
!	*callm k_zstore_f
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_copy =	! KA_COPY
assem
	*saveregs
	*callm k_copy
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_swap =	! KA_SWAP
assem
!jmp L99
	cmp word16 [Dsptr+xb+ktag],trefvar
	jnz L2
	cmp word16 [Dsptr+ya+ktag],trefvar
	jnz L2
	mov D4,[Dsptr+xb+kvarptr]
	mov D5,[Dsptr+ya+kvarptr]

	mov D0,[D4]
	mov D1,[D5]
	mov [D4],D1
	mov [D5],D0

	mov D0,[D4+kvalue]
	mov D1,[D5+kvalue]
	mov [D4+kvalue],D1
	mov [D5+kvalue],D0

	*popvar2
	*jumpskip1

L2:
	cmp word16 [Dsptr+xb+ktag],trefpacked
	jnz L3
	cmp word16 [Dsptr+ya+ktag],trefpacked
	jnz L3
	cmp word16 [Dsptr+xb+krefelemtag],tu8
	jnz L3
	cmp word16 [Dsptr+ya+krefelemtag],tu8
	jnz L3
	mov D4,[Dsptr+xb+kpackptr]
	mov D5,[Dsptr+ya+kpackptr]
	mov B0,[D4]
	mov B1,[D5]
	mov [D4],B1
	mov [D5],B0
	*popvar2
	*jumpskip1

L3:
L99:
	*saveregs
	*callm k_swap
	*loadregs
	*bjumpnext	! size= 1
endassem endproc

threadedproc ka_convptr =	! KA_CONVPTR
assem
	*saveregs
	*callm k_convptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jump =	! KA_JUMP
assem
	mov Dprog,[Dprog+kopnda]
	*bjumpnext

!	*saveregs
!	*callm k_jump
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_jumpptr =	! KA_JUMPPTR
assem
	*saveregs
	*callm k_jumpptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumptrue =	! KA_JUMPTRUE
assem
	cmp word16 [Dsptr+xa+ktag],tint
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
!
L1:
	*saveregs
	*callm k_jumptrue
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumpfalse =	! KA_JUMPFALSE
assem
	cmp word16 [Dsptr+xa+ktag],tint
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
	*saveregs
	*callm k_jumpfalse
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumpdef =	! KA_JUMPDEF
assem
!NOTE: should perhaps check for copy=cc_owner, but that is very unlikely! .isdef only
!meaningful for names, not expressions
	cmp word16 [Dsptr+ktag],tvoid
	jg Lisdef1
	*popvar
	*jumpskip2

Lisdef1:
	*popvar
	mov Dprog,[Dprog+kopnda]
	*jumpnext

!	*saveregs
!	*callm k_jumpdef
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_jumpvoid =	! KA_JUMPVOID
assem
	cmp word16 [Dsptr+ktag],tvoid
	jbe L1
	*popvar
	*jumpskip2

L1:
	*popvar
	mov Dprog,[Dprog+kopnda]
	*bjumpnext

!	*saveregs
!	*callm k_jumpvoid
!	*loadregs
!	*jumpnext	! size= 2
endassem endproc

threadedproc ka_jumpeq =	! KA_JUMPEQ
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1

	mov D0,[Dsptr+xb+kvalue]
	cmp D0,[Dsptr+ya+kvalue]
	jz L2
	*popvar2
	*jumpskip2

L2:
	*popvar2
	mov Dprog,[Dprog+kopnda]
	*jumpnext
L1:
	*saveregs
	*callm k_jumpeq
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumpne =	! KA_JUMPNE
assem
	movzx A0,word16 [Dsptr+xb+ktag]
	movzx A1,word16 [Dsptr+ya+ktag]
	cmp A0,A1
	jnz L99					!unequal types

	cmp A0,tint
	jnz L2

	mov D0,[Dsptr+xb+kvalue]
	cmp D0,[Dsptr+ya+kvalue]
	jnz L1
L0:
	*popvar2						!same
	*jumpskip2

L1:
	*popvar2						!different
	mov Dprog,[Dprog+kopnda]
	*jumpnext

L2:
!CAN'T USE FAST STRING OPS, AS REFCOUNTS NEED DEALING WITH
!	cmp A0,tstring
!	jnz L3
!!string equality compare: quick test that lengths are different
!	mov D4,[Dsptr+yb+kobjptr]
!	mov D5,[Dsptr+xa+kobjptr]
!
!	mov A0,[D4+jlength]
!	cmp A0,[D5+jlength]
!	jnz L1			!lengths are different;share jump code with i32
!	cmp A0,1		!length not 1; do full compare
!	jnz L99
!
!	mov A1,[D4+jstrptr]
!	mov A2,[D5+jstrptr]
!	mov B0,[A1]		!compare single char
!	cmp B0,[A2]
!	jnz L1
!	jmp L0
!
!!	jz L99			!same length: can't determine here, need actual compare

L3:

L99:
	*saveregs
	*callm k_jumpne
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumplt =	! KA_JUMPLT
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1

	mov D0,[Dsptr+xb+kvalue]
	cmp D0,[Dsptr+ya+kvalue]
	jl L2
	*popvar2
	*jumpskip2

L2:
	*popvar2
	mov Dprog,[Dprog+kopnda]
	*jumpnext
L1:
	*saveregs
	*callm k_jumplt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumple =	! KA_JUMPLE
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1

	mov D0,[Dsptr+xb+kvalue]
	cmp D0,[Dsptr+ya+kvalue]
	jle L2
	*popvar2
	*jumpskip2

L2:
	*popvar2
	mov Dprog,[Dprog+kopnda]
	*jumpnext
L1:
	*saveregs
	*callm k_jumple
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumpge =	! KA_JUMPGE
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1

	mov D0,[Dsptr+xb+kvalue]
	cmp D0,[Dsptr+ya+kvalue]
	jge L2
	*popvar2
	*jumpskip2

L2:
	*popvar2
	mov Dprog,[Dprog+kopnda]
	*jumpnext
L1:
	*saveregs
	*callm k_jumpge
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumpgt =	! KA_JUMPGT
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1

	mov D0,[Dsptr+xb+kvalue]
	cmp D0,[Dsptr+ya+kvalue]
	jg L2
	*popvar2
	*jumpskip2

L2:
	*popvar2
	mov Dprog,[Dprog+kopnda]
	*jumpnext
L1:
	*saveregs
	*callm k_jumpgt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumptesteq =	! KA_JUMPTESTEQ
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
	*saveregs
	*callm k_jumptesteq
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumptestne =	! KA_JUMPTESTNE
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
	*saveregs
	*callm k_jumptestne
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumplabel =	! KA_JUMPLABEL
assem
	*saveregs
	*callm k_jumplabel
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_jumpclabel =	! KA_JUMPCLABEL
assem
	*saveregs
	*callm k_jumpclabel
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_switch =	! KA_SWITCH
assem
	cmp word16 [Dsptr+ktag],tint
	jnz L1				!get C deal with errors
	mov D4,[Dsptr+kvalue]		!switch index
	*popvar
	sub D4,[Dprog+kopndb]		!index-lower! now 0-based index
	cmp D4,[Dprog+kopnda]		!index0>=n?
	jae L2				!out of range
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
	*saveregs
	*callm k_switch
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_cswitch =	! KA_CSWITCH
assem
	*saveregs
	*callm k_cswitch
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_new =	! KA_NEW
assem
	*saveregs
	*callm k_new
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_to_f =
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	dec word32 [Dframe+D5+kvalue]
	jz L1
	mov Dprog,D4
	*jumpnext
L1:
	*jumpskip3

!	*saveregs
!	*callm k_to_f
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_for_fci =	! KA_FOR_FCI
assem
	mov D4,[Dprog+kopnda]		!label
	mov D0,[Dprog+kopndb]		!a
	inc word64 [Dframe+D0+kvalue]	!++a
	mov D0,[Dframe+D0+kvalue]
	cmp A0,[Dprog+kopndc]
	jg L1
	mov Dprog,D4
	*jumpnext
L1:
	*jumpskip4

!	*saveregs
!	*callm k_for_fci
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_for_ff =	! KA_FOR_FF
assem
	mov D4,[Dprog+kopnda]		!label
	mov D0,[Dprog+kopndb]		!b
	mov D5,[Dprog+kopndc]		!c
	inc word64 [Dframe+D0+kvalue]
	mov D0,[Dframe+D0+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jg L1
	mov Dprog,D4
	*jumpnext
L1:
	*jumpskip4

!	*saveregs
!	*callm k_for_ff
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_ford_fci =	! KA_FORD_FCI
assem
	mov D4,[Dprog+kopnda]		!label
	mov D0,[Dprog+kopndb]		!a
	dec word64 [Dframe+D0+kvalue]	!++a
	mov D0,[Dframe+D0+kvalue]
	cmp A0,[Dprog+kopndc]
	jl L1
	mov Dprog,D4
	*jumpnext
L1:
	*jumpskip4

!	*saveregs
!	*callm k_ford_fci
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_ford_ff =	! KA_FORD_FF
assem
	*saveregs
	*callm k_ford_ff
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_call =	! KA_CALL
const countinterval=10
static int count=countinterval

assem
	dec word32 [count]
	jz L99
end

assem

!jmp L99
	*pushvar
	mov word32 [Dsptr+ktag],tretaddr
	lea D0,[Dprog+24]		! return address
	mov [Dsptr+kretaddr],D0
	mov [Dsptr+kframeptr_low],Aframe
	mov D0,[Dprog+kopndb]		! stack adjust count
	mov [Dsptr+kstackadj],B0
	mov Dframe,Dsptr
	mov [frameptr],Dframe
	mov Dprog,[Dprog+kopnda]
	*jumpnext

L99:
	mov word32 [count],countinterval
	*saveregs
	*callm k_call
	*loadregs
	*bjumpnext	! size= 3
endassem endproc

threadedproc ka_callptr =	! KA_CALLPTR
assem
!jmp L99
	cmp word16 [Dsptr+ktag],trefproc
	jnz L99
	mov D5,[Dsptr+kvalue]		!newpc

	mov A0,[Dprog+kopnda]
	cmp A0,[A5-intpsize]			!check L params
	jnz L99

	mov word16 [Dsptr+ktag],tretaddr
	lea D0,[Dprog+24]
	mov [Dsptr+kvalue],D0
	mov [Dsptr+kframeptr_low],Aframe
	mov A0,[Dprog+kopndb]
	mov byte [Dsptr+kstackadj],B0
	mov Dframe,Dsptr
	mov [frameptr],Dframe
	mov Dprog,D5
	*jumpnext

L99:
	*saveregs
	*callm k_callptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_return =	! KA_RETURN

assem
!jmp L99
	mov word16 [Dsptr+ktag],0
	mov Dprog,[Dsptr+kretaddr]
	mov Aframe,[Dsptr+kframeptr_low]
	mov [frameptr],Dframe

!NOTE: STACK ADJUST LIMITED TO 15 PARAMS
	movzx A0,byte [Dsptr+kstackadj]
	add Dsptr,D0
	*popvar
	*jumpnext


L99:
	*saveregs
	*callm k_return
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_startdll =	! KA_STARTDLL
assem
	*saveregs
	*callm k_startdll
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdll =	! KA_PUSHDLL
assem
	*saveregs
	*callm k_pushdll
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_calldll =	! KA_CALLDLL
assem
	*saveregs
	*callm k_calldll
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_callhost =	! KA_CALLHOST
assem
	*saveregs
	*callm k_callhost
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_stackframe =	! KA_STACKFRAME
assem
	mov A3,[Dprog+kopnda]
Lloop1:
	*pushvar
	mov word32 [Dsptr+ktag],tvoid
	mov word64 [Dsptr+kvalue],0
	dec A3
	jnz Lloop1
	*jumpskip2

!	*saveregs
!	*callm k_stackframe
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_allocvars =	! KA_ALLOCVARS
assem
	mov D0,[Dprog+kopnda]
	shl D0,varshift
	sub Dsptr,D0
	*jumpskip2

!	*saveregs
!	*callm k_allocvars
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_free =	! KA_FREE
assem
!jmp L99
	mov A3,[Dprog+kopnda]		!n
	cmp A3,1
	jnz Lloop1
	cmp byte [Dsptr+khasref],1
	jnz L0
	push D3
	*callvxufree_dsptr
	pop D3

L0:
	*popvar
	*jumpskip2

Lloop1:
	cmp byte [Dsptr+khasref],1
	jnz L1
	push D3
	*callvxufree_dsptr
	pop D3

L1:
	*popvar
	dec D3
	jnz Lloop1
	*jumpskip2

L99:
	*saveregs
	*callm k_free
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_addsp =	! KA_ADDSP
assem
	add Dsptr,[Dprog+kopnda]
	*jumpskip2

!	*saveregs
!	*callm k_addsp
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_stop =	! KA_STOP
assem
	*csaveregs
	jmp disploop.stoplabel
endassem endproc

threadedproc ka_test =	! KA_TEST
assem
	*saveregs
	*callm k_test
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makelist =	! KA_MAKELIST
assem
	*saveregs
	*callm k_makelist
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makerecord =	! KA_MAKERECORD
assem
	*saveregs
	*callm k_makerecord
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makearray =	! KA_MAKEARRAY
assem
	*saveregs
	*callm k_makearray
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makestruct =	! KA_MAKESTRUCT
assem
	*saveregs
	*callm k_makestruct
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makeset =	! KA_MAKESET
assem
	*saveregs
	*callm k_makeset
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makerange =	! KA_MAKERANGE
assem
	*saveregs
	*callm k_makerange
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makedict =	! KA_MAKEDICT
assem
	*saveregs
	*callm k_makedict
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdot =	! KA_PUSHDOT
static int fieldtype,offset,xtag
static varrec v
static variant xptr

assem
jmp L99

	mov D0,[Dsptr]			!v:=sptr^
	mov [v],D0
	mov D1,[Dsptr+8]
	mov [v+8],D1


	cmp W0,trecordlink		!W0 is v.tag Is it in fact a recordlink?
	jnz L50
	mov W1,[v+krefelemtag]	!leave hasref at zero
	mov [v+ktag],W1
	jmp L51
!	and D5,0xFFFF			!clear .hasref byte (don't want to change refcounts)
L50:
	cmp byte [v+khasref],0
	jz L99					!error
L51:

!is normal record

L1:
!do inline assembly for the above
!A2 = n
!A3 = genfielddata
!A4 = i
!A5 = xtag
!A6 = x
!assem
!gd:=&genfielddata[genfieldnames[getopnda].dataindex]
!n:=genfieldnames[getopnda].datalength

	mov D7,[Dprog+kopnda]
	shl A7,4						!D7 is getopnda*16
	lea D0,[D7+genfieldnames-16]
	mov A0,[A0+kdataindex]
	shl A0,4
	lea D6,[A0+genfielddata-16]		!D6 is gd = &genfielddata[...]

	lea D0,[D7+genfieldnames-16]
	mov A5,[D0+kdatalength]			!D5 is n = genfieldnames[genopnda].datalength

	mov W4,word16 [v+ktag]			!W5 is v.tag
	mov D0,[v+kobjptr]				!A5 is v.tag
	mov D0,[D0+jvptr]				!xptr:=v.objptr^.vptr
	mov [xptr],D0

L70:
	mov W0,[D6+krecordtype]				!gd^.recordtype
	cmp W0,W4							!=v.tag
	jz Lfound
!matching entry
	add D6,16							!++gd
	dec A5
	jnz L70
	jmp Lnotfound

Lfound:
	mov A0,[D6+kfieldtype]		!fieldtype:=gd^.fieldtype
	mov A3,[D6+kfieldoffset]	!offset:=gd^.offset
	cmp A0,tvariant
	jnz L4
!variant field

	mov D4,[xptr]
	mov D0,[D4+D3]				!(xptr+gd^.offset)^
	mov [Dsptr+ktag],D0
	mov D1,[D4+D3+kvalue]
	mov [Dsptr+kvalue],D1

	and A0,hasrefmask			!sptr^.tagx iand hasrefmask
	jz L31
	mov D2,[Dsptr+kobjptr]
	inc word32 [D2+jrefcount]	!++sptr^.objptr^.refcount
L31:
	cmp byte [v+khasref],1		!might have been a recordlink
	jnz L32
	mov D4,[v+kobjptr]
	dec word32 [D4+jrefcount]	!original record
	jnz L32
	lea D4,[v]
	*callfreex_d4

L32:
	*jumpskip2


L4:
JMP L99
	cmp A0,trefproc
	jnz ispacked

	mov D4,[D6+kpackptr]			!x^.ptr
end assem
pcerror("ASM/REFPROC")
assem

!*!	mov A2,[pcdata]				!A1 contains .procoffset
	lea D0,[A2+A1*8]			!&pcdata^[genfielddata^[k].proc
	mov [Dsptr+kvalue],D0

	mov word32 [Dsptr+ktag],trefproc

	lea D4,[v]
	*callvxufree_d4
	*jumpskip2

ispacked:

	mov [fieldtype],A0			!fieldtype:=
	mov [offset],A1				!offset:=
!	mov [x],D6
!	mov [xtag],A5
end
	assem
		*saveregs
	end
	pc_loadpacked(v.objptr.urec.ptr+offset,fieldtype,sptr,nil)
	if --v.objptr^.refcount=0 then
		pc_unshare(&v)
	fi

assem
	*loadskip2
end

asm Lnotfound:

xtag:=tint
asm jmp L99

assem
	*saveregs
end
!os_getch()
pcustypet("Dotg: wrong record type",xtag)

asm Lnotcopy:
!pcerror("A:dotg/not copy")
assem
L99:
	*saveregs
	*callm k_pushdot
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdotref =	! KA_PUSHDOTREF
assem
	*saveregs
	*callm k_pushdotref
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_softconv =	! KA_SOFTCONV
assem
	*saveregs
	*callm k_softconv
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_hardconv =	! KA_HARDCONV
assem
	*saveregs
	*callm k_hardconv
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_mixed =	! KA_MIXED
assem
	*saveregs
	*callm k_mixed
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_incr =	! KA_PUSH_INCR
assem
	*saveregs
	*callm k_incr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_decr =	! KA_PUSH_DECR
assem
	*saveregs
	*callm k_decr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_incrptr =	! KA_INCRPTR
assem
	*saveregs
	*callm k_incrptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_incrto_m =	! KA_INCRTO_M
assem
	mov D4,[Dprog+kopnda]
	cmp word16 [D4+ktag],tint
	jnz L1
	inc word64 [D4+kvalue]
	*jumpskip2

L1:
	cmp word16 [D4+ktag],trefpacked
	jnz L2
	movzx A0,word16 [D4+krefelemtag]
!	mov D1,[ttsize]
	mov A0,[D0*8+ttsize]
	add [D4+kvarptr],D0
	*jumpskip2

L2:
	*saveregs
	*callm k_incrto_m
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_incrto_f =	! KA_INCRTO_F
assem
	mov D4,[Dprog+kopnda]
	cmp word16 [Dframe+D4+ktag],tint
	jnz L1
	inc word64 [Dframe+D4+kvalue]
	*jumpskip2

L1:
	cmp word16 [Dframe+D4+ktag],trefpacked
	jnz L2
	movzx A0,word16 [Dframe+D4+krefelemtag]
!	mov D1,[ttsize]
	mov A0,[D0*8+ttsize]
	add [Dframe+D4+kvarptr],D0
	*jumpskip2

L2:
	*saveregs
	*callm k_incrto_f
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_loadincr =	! KA_LOADINCR
assem
	*saveregs
	*callm k_loadincr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_incrload =	! KA_INCRLOAD
assem
	*saveregs
	*callm k_incrload
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_decrptr =	! KA_DECRPTR
assem
	*saveregs
	*callm k_decrptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_decrto_m =	! KA_DECRTO_M
assem
	*saveregs
	*callm k_decrto_m
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_decrto_f =	! KA_DECRTO_F
assem
	mov D4,[Dprog+kopnda]
	cmp word16 [Dframe+D4+ktag],tint
	jnz L1
	dec word64 [Dframe+D4+kvalue]
	*jumpskip2

L1:
	cmp word16 [Dframe+D4+ktag],trefpacked
	jnz L2
	movzx A0,word16 [Dframe+D4+krefelemtag]
!	mov D1,[ttsize]
	mov A0,[D0*8+ttsize]
	sub [Dframe+D4+kpackptr],D0
	*jumpskip2

L2:
	*saveregs
	*callm k_decrto_f
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_loaddecr =	! KA_LOADDECR
assem
	*saveregs
	*callm k_loaddecr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_decrload =	! KA_DECRLOAD
assem
	*saveregs
	*callm k_decrload
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_neg =	! KA_NEG
assem
	*saveregs
	*callm k_neg
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_abs =	! KA_ABS
assem
	*saveregs
	*callm k_abs
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_not =	! KA_NOT
assem
	mov al,[dsptr+kvalue]
	xor al,1
	mov [dsptr+kvalue],al
	*jumpskip1

	*saveregs
	*callm k_not
	*loadregs
	*bjumpnext
!sptr^.value:=not sptr^.value
endassem endproc

threadedproc ka_inot =	! KA_INOT
assem
	*saveregs
	*callm k_inot
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_istrue =	! KA_ISTRUE
assem
	*saveregs
	*callm k_istrue
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_asc =	! KA_ASC
assem
	*saveregs
	*callm k_asc
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_chr =	! KA_CHR
assem
	cmp word16 [Dsptr+ktag],tint
	jnz L99						!not int; B deals with the error
	mov D0,[Dsptr+kvalue]
	cmp D0,255
	ja L99						!not in range
!	movzx A0,byte [Dsptr+kvalue]
!	extern chrtable
	mov D0,[D0*8+chrtable]
	and D0,D0
	jz L99						!value not cached; B will fill it in
	mov word32 [Dsptr+ktag],tstring+hasrefmask
	mov [Dsptr+kvalue],D0				!point to object
	inc word32 [D0+jrefcount]
	*jumpskip1

L99:
	*saveregs
	*callm k_chr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_sqrt =	! KA_SQRT
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

!	fld word64 [Dsptr+kvalue]
!	fsqrt
!	fstp word64 [Dsptr+kvalue]

	movq xmm0,[Dsptr+kvalue]
	sqrtsd xmm0,xmm0
	movq [Dsptr+kvalue],xmm0

	*jumpskip1
L2:
	*saveregs
	*callm k_sqrt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_sqr =	! KA_SQR
assem
	cmp word16 [Dsptr+ktag],tint
	jnz L1
	mov D0,[Dsptr+kvalue]
	imul word64 [Dsptr+kvalue]
!	movsxd D0,A0
	mov [Dsptr+kvalue],D0
	*jumpskip1
L1:
	cmp word16 [Dsptr+ktag],treal
	jnz L2

!	fld word64 [Dsptr+kvalue]
!	fmul word64 [Dsptr+kvalue]
!	fstp word64 [Dsptr+kvalue]

	movq xmm0,[Dsptr+kvalue]
	mulsd xmm0,xmm0
	movq [Dsptr+kvalue],xmm0

	*jumpskip1
L2:
	*saveregs
	*callm k_sqr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_cube =	! KA_CUBE
assem
	*saveregs
	*callm k_cube
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_sin =	! KA_SIN
assem
	fld word64 [Dsptr+kvalue]
	fsin
	fstp word64 [Dsptr+kvalue]
	*jumpskip1

	*saveregs
	*callm k_sin
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_cos =	! KA_COS
assem
	*saveregs
	*callm k_cos
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_tan =	! KA_TAN
assem
	*saveregs
	*callm k_tan
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_asin =	! KA_ASIN
assem
	*saveregs
	*callm k_asin
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_acos =	! KA_ACOS
assem
	*saveregs
	*callm k_acos
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_atan =	! KA_ATAN
assem
	*saveregs
	*callm k_atan
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_sign =	! KA_SIGN
assem
	*saveregs
	*callm k_sign
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ln =	! KA_LN
assem
	*saveregs
	*callm k_ln
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_log =	! KA_LOG
assem
	*saveregs
	*callm k_log
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_lg =	! KA_LG
assem
	*saveregs
	*callm k_lg
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_exp =	! KA_EXP
assem
	*saveregs
	*callm k_exp
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_round =	! KA_ROUND
assem
	*saveregs
	*callm k_round
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_floor =	! KA_FLOOR
assem
	*saveregs
	*callm k_floor
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ceil =	! KA_CEIL
assem
	*saveregs
	*callm k_ceil
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_fract =	! KA_FRACT
assem
	*saveregs
	*callm k_fract
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_negto =	! KA_NEGTO
assem
	*saveregs
	*callm k_negto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_absto =	! KA_ABSTO
assem
	*saveregs
	*callm k_absto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_notto =	! KA_NOTTO
assem
	*saveregs
	*callm k_notto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_inotto =	! KA_INOTTO
assem
	*saveregs
	*callm k_inotto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_len =	! KA_LEN
static varrec v

assem
	cmp byte [Dsptr+khasref],1
	jnz L2
	movzx A2,word16 [Dsptr+ktag]
cmp A2,tbignum
jz L2
	mov D4,[Dsptr+kobjptr]			!assume this is an object
	mov word32 [Dsptr+ktag],tint
	mov A0,[D4+jlength]
	mov [Dsptr+kvalue],D0

	dec word32 [D4+jrefcount]
	jnz L1
	mov word32 [v+ktag],A2
	mov word64 [v+kobjptr],D4
	lea D4,[v]
	*callfreex_d4
L1:

	*jumpskip1

L2:
!!return len_table[sptr^.tag]^()
!	movzx A0,word16 [Dsptr+ktag]
!	mov D0,[D0*8+len_table]
!	*csaveregs
!	call D
!	*loadregs
!	*jumpnext

	*saveregs
	*callm k_len
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_lwb =	! KA_LWB
assem
	*saveregs
	*callm k_lwb
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_upb =	! KA_UPB
static varrec v

assem
	cmp byte [Dsptr+khasref],1
	jnz L2
	movzx A2,word16 [Dsptr+ktag]
	cmp A2,tlist
	jz L3
	cmp A2,tarray
	jnz L2

L3:
	mov D4,[Dsptr+kobjptr]			!assume this is an object

	mov word32 [Dsptr+ktag],tint
	mov A0,[D4+jlength]
!	add A0,[D4+jlower]
	movsx A1,word16 [D4+jlower]		!could be array; uses 16 bits for all lwbs
	add A0,A1
	dec A0
!mov A0,2
	mov [Dsptr+kvalue],D0

	dec word32 [D4+jrefcount]
	jnz L1
	mov word32 [v+ktag],A2
	mov word64 [v+kobjptr],D4
	lea D4,[v]
	*callfreex_d4
L1:

	*jumpskip1

L2:
	*saveregs
	*callm k_upb
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_bounds =	! KA_BOUNDS
assem
	*saveregs
	*callm k_bounds
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_bits =	! KA_BITS
assem
	*saveregs
	*callm k_bits
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_bytes =	! KA_BYTES
assem
	*saveregs
	*callm k_bytes
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_type =	! KA_TYPE
assem
	*saveregs
	*callm k_type
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_elemtype =	! KA_ELEMTYPE
assem
	*saveregs
	*callm k_elemtype
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_basetype =	! KA_BASETYPE
assem
	*saveregs
	*callm k_basetype
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_minval =	! KA_MINVAL
assem
	*saveregs
	*callm k_minval
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_maxval =	! KA_MAXVAL
assem
	*saveregs
	*callm k_maxval
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isint =	! KA_ISINT
assem
	movzx A0,word16 [Dsptr+ktag]
	cmp A0,tint
	jz L1
	cmp A0,tword
	jnz L2
L1:
	mov word64 [Dsptr+kvalue],1
	mov word32 [Dsptr+ktag],tint
	*jumpskip1
L2:
	cmp byte [Dsptr+khasref],1
	jnz L3
	*callvxufree_dsptr
L3:
	mov word64 [Dsptr+kvalue],0
	mov word32 [Dsptr+ktag],tint
	*jumpskip1

!	*saveregs
!	*callm k_isint
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_isreal =	! KA_ISREAL
assem
	*saveregs
	*callm k_isreal
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isstring =	! KA_ISSTRING
assem
	*saveregs
	*callm k_isstring
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isrange =	! KA_ISRANGE
assem
	*saveregs
	*callm k_isrange
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isnumber =	! KA_ISNUMBER
assem
	*saveregs
	*callm k_isnumber
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isarray =	! KA_ISARRAY
assem
	movzx A0,word16 [Dsptr+ktag]
!	mov D1,[ttbasetype]
	mov D0,[D0*4+ttbasetype]
	cmp A0,tlist
	jz L1
	cmp A0,tarray
	jnz L2
L1:
	mov D5,1
	jmp L3
L2:
	mov D5,0
L3:
	cmp byte [Dsptr+khasref],1
	jnz L4
	push D5
	*callvxufree_dsptr
	pop D5
L4:
	mov [Dsptr+kvalue],D5
	mov word32 [Dsptr+ktag],tint
	*jumpskip1

!	*saveregs
!	*callm k_isarray
!	*loadregs
!	*bjumpnext
endassem endproc

threadedproc ka_isrecord =	! KA_ISRECORD
assem
	*saveregs
	*callm k_isrecord
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ispointer =	! KA_ISPOINTER
assem
	*saveregs
	*callm k_ispointer
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ismutable =	! KA_ISMUTABLE
assem
	*saveregs
	*callm k_ismutable
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isset =	! KA_ISSET
assem
	*saveregs
	*callm k_isset
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isvoid =	! KA_ISVOID
assem
	*saveregs
	*callm k_isvoid
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isdef =	! KA_ISDEF
assem
	*saveregs
	*callm k_isdef
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_tostr =	! KA_TOSTR
assem
	*saveregs
	*callm k_tostr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_isequal =	! KA_ISEQUAL
assem
	movzx A0,word16 [Dsptr+xb+ktag]
	cmp W0, [Dsptr+ya+ktag]
	jnz L2
	cmp byte [Dsptr+xb+khasref],1
	jl L2

!Both same type, and both use a heap object otherwise .copy<cc_owner
!assume both will be .copy (??) so need no freeing
	mov D0,[Dsptr+xb+kobjptr]
	cmp D0,[Dsptr+ya+kobjptr]
	setz B0
	movzx A0,B0
	*popvar
	mov [Dsptr+xa+kvalue],D0
	mov word32 [Dsptr+xa+ktag],tint
	*jumpskip1

L2:

L99:
	*saveregs
	*callm k_isequal
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_add =	! KA_ADD
assem
!jmp L99

!	movzx A0,word16 [Dsptr+xb+ktag]
!	movzx A1,word16 [Dsptr+ya+ktag]
!	cmp A0,A1
!	jnz L99
!
!	mov D0,[D0*8+add_table]
!	*csaveregs
!	call D0
!	*loadregs
!	*jumpnext
!
!!
!
!
!	cmp W0,tint
!	jnz L1
!	mov D0,[Dsptr+ya+kvalue]
!	add [Dsptr+xb+kvalue],D0
!	*popvar
!	*jumpskip1
!
!------------------------
	movzx A0,word16 [Dsptr+xb+ktag]
	movzx A1,word16 [Dsptr+ya+ktag]
	cmp A0,A1
	jnz L99
	cmp W0,tint
	jnz L1
	mov D0,[Dsptr+ya+kvalue]
	add [Dsptr+xb+kvalue],D0

!	jo L999


	*popvar
	*jumpskip1

L1:
	cmp W0,treal
	jnz L2

!	fld word64 [Dsptr+xb+kvalue]
!	fadd word64 [Dsptr+ya+kvalue]
!	fstp word64 [Dsptr+xb+kvalue]

	fld word64 [Dsptr+xb+kvalue]
	fld word64 [Dsptr+ya+kvalue]
	fadd
	fstp word64 [Dsptr+xb+kvalue]

!	movq xmm0,[Dsptr+xb+kvalue]
!	addsd xmm0,[Dsptr+ya+kvalue]
!	movq [Dsptr+xb+kvalue],xmm0


	*popvar
	*jumpskip1

L999:
end
	PCERROR("ADDI64/OVERFLOW")
!
assem
!

L2:
!	mov D0,[D0*8+add_table]
!	*csaveregs
!	call D
!	*loadregs
!	*jumpnext
L99:
	*saveregs
	*callm k_add
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_sub =	! KA_SUB
assem
	movzx A0,word16 [Dsptr+xb+ktag]
	movzx A1,word16 [Dsptr+ya+ktag]
	cmp A0,A1
	jnz L99
	cmp W0,tint
	jnz L1
	mov D0,[Dsptr+ya+kvalue]
	sub [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L1:
	cmp W0,treal
	jnz L2

!	fld word64 [Dsptr+xb+kvalue]
!	fsub word64 [Dsptr+ya+kvalue]
!	fstp word64 [Dsptr+xb+kvalue]

!	fld word64 [Dsptr+xb+kvalue]
!	fld word64 [Dsptr+ya+kvalue]
!	fsub
!	fstp word64 [Dsptr+xb+kvalue]

!	movq xmm0,[Dsptr+xb+kvalue]
!	subsd xmm0,[Dsptr+ya+kvalue]
!	movq [Dsptr+xb+kvalue],xmm0
L2:
L99:
	*saveregs
	*callm k_sub
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_mul =	! KA_MUL
assem
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1

	mov D0,[Dsptr+xb+kvalue]
	imul word64 [Dsptr+ya+kvalue]
	mov [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L1:
	cmp word16 [Dsptr+xb+ktag],treal
	jnz L2
	cmp word16 [Dsptr+ya+ktag],treal
	jnz L2

!	fld word64 [Dsptr+xb+kvalue]
!	fmul word64 [Dsptr+ya+kvalue]
!	fstp word64 [Dsptr+xb+kvalue]

	fld word64 [Dsptr+xb+kvalue]
	fld word64 [Dsptr+ya+kvalue]
	fmul
	fstp word64 [Dsptr+xb+kvalue]

	*popvar
	*jumpskip1

L2:
	*saveregs
	*callm k_mul
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_div =	! KA_DIV
assem
jmp L1
!	cmp word16 [Dsptr+xb+ktag],treal
!	jnz L1
!	cmp word16 [Dsptr+ya+ktag],treal
!	jnz L1
!
	fld word64 [Dsptr+xb+kvalue]
	fld word64 [Dsptr+ya+kvalue]
	fdiv
	fstp word64 [Dsptr+xb+kvalue]

	*popvar
	*jumpskip1

L1:
	*saveregs
	*callm k_div
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_idiv =	! KA_IDIV
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
	mov D0,[Dsptr+xb+kvalue]
	cqo
	idiv word64 [Dsptr+ya+kvalue]
	mov [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L1:
	*saveregs
	*callm k_idiv
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_rem =	! KA_REM
assem
	*saveregs
	*callm k_rem
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_divrem =	! KA_DIVREM
assem
	*saveregs
	*callm k_divrem
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_iand =	! KA_IAND
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
	mov D0,[Dsptr+ya+kvalue]
	and [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L1:
	*saveregs
	*callm k_iand
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ior =	! KA_IOR
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
	mov D0,[Dsptr+ya+kvalue]
	or [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L1:
	*saveregs
	*callm k_ior
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ixor =	! KA_IXOR
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
	mov D0,[Dsptr+ya+kvalue]
	xor [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L1:
	cmp word16 [Dsptr+xb+ktag],tword
	jnz L2
	cmp word16 [Dsptr+ya+ktag],tword
	jnz L2
	mov D0,[Dsptr+ya+kvalue]
	xor [Dsptr+xb+kvalue],D0
	*popvar
	*jumpskip1
L2:
	*saveregs
	*callm k_ixor
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_shl =	! KA_SHL
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
L3:
	mov rdx,Dsptr
	mov cl,[Dsptr+ya+kvalue]
	shl word64 [rdx+xb+kvalue],cl
	mov Dsptr,rdx
	*popvar
	*jumpskip1
L1:
!	cmp word16 [Dsptr+xb+ktag],tword
!	jnz L2
!	cmp word16 [Dsptr+ya+ktag],tword
!	jz L3
L2:
	*saveregs
	*callm k_shl
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_shr =	! KA_SHR
assem
	cmp word16 [Dsptr+xb+ktag],tint
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L1
	mov rdx,Dsptr
	mov cl,[Dsptr+ya+kvalue]
	sar word64 [rdx+xb+kvalue],cl
	mov Dsptr,rdx
	*popvar
	*jumpskip1
L1:
	*saveregs
	*callm k_shr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_in =	! KA_IN
assem
	*saveregs
	*callm k_in
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_notin =	! KA_NOTIN
assem
	*saveregs
	*callm k_notin
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_inrev =	! KA_INREV
assem
	*saveregs
	*callm k_inrev
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_eq =	! KA_EQ
assem
	*saveregs
	*callm k_eq
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ne =	! KA_NE
assem
	*saveregs
	*callm k_ne
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_lt =	! KA_LT
assem
	*saveregs
	*callm k_lt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_le =	! KA_LE
assem
	*saveregs
	*callm k_le
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ge =	! KA_GE
assem
	*saveregs
	*callm k_ge
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_gt =	! KA_GT
assem
	*saveregs
	*callm k_gt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_min =	! KA_MIN
assem
	*saveregs
	*callm k_min
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_max =	! KA_MAX
assem
	*saveregs
	*callm k_max
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_concat =	! KA_CONCAT
assem
	*saveregs
	*callm k_concat
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_append =	! KA_APPEND
assem
	*saveregs
	*callm k_append
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_power =	! KA_POWER
assem
	*saveregs
	*callm k_power
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_atan2 =	! KA_ATAN2
assem
	*saveregs
	*callm k_atan2
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_addto =	! KA_ADDTO
assem
	mov D4,[Dsptr+xb+kvarptr]
	cmp word16 [Dsptr+xb+ktag],trefvar	!lhs is ref var?
	jnz L99

	cmp word16 [D4+ktag],tint				!lhs is ref var:int?
	jnz L1								!mixed
	cmp word16 [Dsptr+ya+ktag],tint		!rhs is int
	jnz L99
	mov D0,[Dsptr+kvalue]
	add [D4+kvalue],D0
	*popvar2
	*jumpskip1

L1:
	cmp word16 [Dsptr+ya+ktag],treal		!rhs is real
	jnz L2
	cmp word16 [D4+ktag],treal			!lhs is ref var:real?
	jnz L99								!mixed

!	fld word64 [D4+kvalue]
!	fadd word64 [Dsptr+kvalue]
!	fstp word64 [D4+kvalue]

	movq xmm0,[D4+kvalue]
	addsd xmm0,[Dsptr+kvalue]
	movq [D4+kvalue],xmm0
	*popvar2
	*jumpskip1
L2:
	cmp word16 [D4+ktag],tstring				!lhs is ref var:string?
	jnz L3									!mixed
	cmp word32 [Dsptr+ya+ktag],tstring+cc_copy16		!rhs is string?
!	cmp word16 [Dsptr+ya+ktag],tstring
	jnz L25
	mov D5,[Dsptr+ya+kobjptr]				!D5 points to rhs object descr
	cmp word32 [D5+jlength],1
	jnz L99									!only deal with 1-char strings on rhs
	mov D5,[D5+jstrptr]						!point to rhs string data
	mov B1,[D5]								!char in rhs string

	mov D5,[D4+kobjptr]						!point to lhs string descr
	cmp byte [D5+jmutable],0
	jz L99									!lhs not mutable
	mov A2,[D5+jlength]
	inc A2									!add extra char
	cmp A2,[D5+jallocated]
	jg L99									!need extra allocation
	mov [D5+jlength],A2						!update length
	mov D3,[D5+jstrptr]
!inc B1
	mov [D3+D2-1],B1						!store new char
	*popvar2
	*jumpskip1

L25:
	cmp word16 [Dsptr+ya+ktag],tint			!rhs is int?
	jnz L3
	mov B1,[Dsptr+ya+kvalue]				!B1 is int char value (should be checked reall)

	mov D5,[D4+kobjptr]						!point to lhs string descr
	cmp byte [D5+jmutable],0
	jz L99									!lhs not mutable
	mov A2,[D5+jlength]
	inc A2									!add extra char
	cmp A2,[D5+jallocated]
	jg L99									!need extra allocation
	mov [D5+jlength],A2						!update length
	mov D3,[D5+jstrptr]
	mov [D3+D2-1],B1						!store new char
	*popvar2
	*jumpskip1

L3:

L99:
	*saveregs
	*callm k_addto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_subto =	! KA_SUBTO
assem
	cmp word16 [Dsptr+xb+ktag],trefvar	!lhs is ref var?
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint		!rhs is int
	jnz L1
	mov D4,[Dsptr+xb+kvarptr]
	cmp word16 [D4+ktag],tint			!lhs is ref var:int?
	jnz L1
	mov D0,[Dsptr+kvalue]
	sub [D4+kvalue],D0
	*popvar2
	*jumpskip1

L1:
	*saveregs
	*callm k_subto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_multo =	! KA_MULTO
assem
	mov D4,[Dsptr+xb+kvarptr]
	cmp word16 [Dsptr+xb+ktag],trefvar	!lhs is ref var?
	jnz L99

	cmp word16 [D4+ktag],tint				!lhs is ref var:int?
	jnz L1								!mixed
	cmp word16 [Dsptr+ya+ktag],tint		!rhs is int
	jnz L99
	mov D0,[D4+kvalue]
	imul word64 [Dsptr+kvalue]
	mov [D4+kvalue],D0
	*popvar2
	*jumpskip1

L1:
!	cmp word16 [Dsptr+ya+ktag],treal		!rhs is real
!	jnz L2
!	cmp word16 [D4+ktag],treal			!lhs is ref var:real?
!	jnz L99								!mixed
!	fld word64 [D4+kvalue]
!	fmul word64 [Dsptr+kvalue]
!	fstp word64 [D4+kvalue]
!	*popvar2
!	*jumpskip1

L2:
L99:
	*saveregs
	*callm k_multo
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_divto =	! KA_DIVTO
assem
	*saveregs
	*callm k_divto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_idivto =	! KA_IDIVTO
assem
	*saveregs
	*callm k_idivto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_iandto =	! KA_IANDTO
assem
	*saveregs
	*callm k_iandto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_iorto =	! KA_IORTO
assem
	*saveregs
	*callm k_iorto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_ixorto =	! KA_IXORTO
assem
	*saveregs
	*callm k_ixorto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_shlto =	! KA_SHLTO
assem
	cmp word16 [Dsptr+xb+ktag],trefvar	!lhs is ref var?
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint		!rhs is int
	jnz L1
	mov D4,[Dsptr+xb+kvarptr]
	cmp word16 [D4+ktag],tint			!lhs is ref var:int?
	jnz L1
	mov cl,[Dsptr+kvalue]
!	shl word64 [D4+kvalue],cl
	mov D0,[D4+kvalue]
	shl D0,cl
	mov [D4+kvalue],D0
	*popvar2
	*jumpskip1
L1:
	*saveregs
	*callm k_shlto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_shrto =	! KA_SHRTO
assem
	cmp word16 [Dsptr+xb+ktag],trefvar	!lhs is ref var?
	jnz L1
	cmp word16 [Dsptr+ya+ktag],tint		!rhs is int
	jnz L1
	mov D4,[Dsptr+xb+kvarptr]
	cmp word16 [D4+ktag],tint			!lhs is ref var:int?
	jnz L1
	mov cl,[Dsptr+kvalue]
	sar word64 [D4+kvalue],cl
	*popvar2
	*jumpskip1
L1:

	*saveregs
	*callm k_shrto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_minto =	! KA_MINTO
assem
	*saveregs
	*callm k_minto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_maxto =	! KA_MAXTO
assem
	*saveregs
	*callm k_maxto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_concatto =	! KA_CONCATTO
assem
	*saveregs
	*callm k_concatto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_appendto =	! KA_APPENDTO
assem
	*saveregs
	*callm k_appendto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushix =	! KA_PUSHIX
static varrec v
assem
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L99

!int index:
	mov D6,[Dsptr+xb+ktag]
	cmp W6,tlist
	jnz L2

!list[int]
	mov D5,[Dsptr+xb+kobjptr]

	mov A4,[Dsptr+ya+kvalue]		!index
	sub A4,[D5+jlower]		!0-base
	cmp A4,[D5+jlength]
	jae L99					!bounds error: let C deal with it

	shl A4,varshift				!index*varsize
	add D4,[D5+jptr]			!point to element

	*popvar					!pop list, stack contains list descriptor

	mov D0,[D4+ktag]
	mov [Dsptr+ktag],D0			!replace index by list element

	mov D1,[D4+kvalue]
	mov [Dsptr+kvalue],D1

	and A0,hasrefmask
	jz L11
	inc word32 [D1+jrefcount]
L11:
	dec word32 [D5+jrefcount]	!dec count of original list
	jnz L12
	mov [v+ktag],D6
	mov [v+kobjptr],D5
	lea D5,[v]
	*callfreex_d5
L12:
	*jumpskip1

L2:
!	cmp W6,tstring
!	jnz L2
!!string[int]
!	mov A4,[Dsptr+ya+kvalue]		!index
!	sub A4,1				!0-base
!	cmp A4,[Dsptr+xb+klength]
!	jae L99					!bounds error: let C deal with it
!
!	add A4,[Dsptr+xb+kptr]			!point to element
!	*popvar					!pop string, stack contains index
!
!	mov word32 [Dsptr+ktag],tstring+cc_copy*65536		!replace index by char from string
!	mov [Dsptr+kptr],A4			!point to char
!	mov word32 [Dsptr+klength],1
!	*jumpskip1
L3:
L99:
	*saveregs
	*callm k_pushix
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdotix =	! KA_PUSHDOTIX
assem
	*saveregs
	*callm k_pushdotix
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushkeyix =	! KA_PUSHKEYIX
assem
	*saveregs
	*callm k_pushkeyix
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushkeyixd =	! KA_PUSHKEYIXD
assem
	*saveregs
	*callm k_pushkeyixd
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushixref =	! KA_PUSHIXREF
assem
jmp L99
	cmp word16 [Dsptr+xb+ktag],trefvar
	jnz L3
	cmp word16 [Dsptr+ya+ktag],tint
	jnz L3
	mov D4,[Dsptr+xb+kvarptr]		!A4 points to array etc
	cmp word16 [D4+ktag],tlist
	jnz L2

!	mov D6,[D4+kobjptr]			!D6 points to list object
!	cmp byte[D6+jmutable],0
!	jz L99
JMP L99

	mov A5,[Dsptr+ya+kvalue]	!index
	sub A5,[D6+jlower]		!index=lower = 0-based index
	cmp A5,[D6+jlength]
	jae L3				!bounds overflow
	*popvar				!lose list ptr
	mov word32 [Dsptr+ktag],trefvar	!create new ref
	shl A5,varshift
	add D5,[D6+jptr]		!A5 points to element
	mov [Dsptr+kvarptr],D5
	*jumpskip1

!check for other refvar[int] types
L2:
!	movzx A0,word16 [A4+ktag]
!	mov A0,[A0*4+???ttbasetype]
!	cmp A0,tarray
!	jnz L21
!	cmp word16 [A4+kelemtypex],ti32
!	jnz L21
!!arrays
!	mov A5,[Dsptr+xb+kvalue]	!index
!	movsx A1,word16 [A4+kshortlower]
!	sub A5,A1			!index=lower = 0-based index
!	cmp A5,[A4+klength]
!	jae L3				!bounds overflow
!	*popvar				!lose list ptr
!	mov word32 [Dsptr+ktag],trefpack	!create new ref
!	mov word16 [Dsptr+kelemtypex],ti32
!	shl A5,2			!*4 for ints
!	add A5,[A4+kptr]		!construct pointer to element
!	mov [Dsptr+kptr],A5
!	*jumpskip1
!L21:

L3:
L99:
	*saveregs
	*callm k_pushixref
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdotixref =	! KA_PUSHDOTIXREF
assem
	*saveregs
	*callm k_pushdotixref
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushkeyixref =	! KA_PUSHKEYIXREF
assem
	*saveregs
	*callm k_pushkeyixref
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushbyteix =	! KA_PUSHBYTEIX
assem
	*saveregs
	*callm k_pushbyteix
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushbyteixref =	! KA_PUSHBYTEIXREF
assem
	*saveregs
	*callm k_pushbyteixref
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_appendset =	! KA_APPENDSET
assem
	*saveregs
	*callm k_appendset
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdotm =	! KA_PUSHDOTM
assem
	*saveregs
	*callm k_pushdotm
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_pushdott =	! KA_PUSHDOTT
assem
	*saveregs
	*callm k_pushdott
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_push_ad =	! KA_PUSH_AD
assem
	*saveregs
	*callm k_push_ad
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_push_try =	! KA_PUSH_TRY
assem
	*saveregs
	*callm k_push_try
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_raise =	! KA_RAISE
assem
	*saveregs
	*callm k_raise
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_applyop =	! KA_APPLYOP
static [10]intpc codeseq
assem
	*saveregs
end
pcerror("APPLY OP NEEDS REVISING")
codeseq[1]:=sptr^.value			!copy jump lab which follows the applyop
++sptr
codeseq[2]:=(pcptr+1)^			!copy jump lab which follows the applyop
codeseq[3]:=(pcptr+2)^			!include the dest label
pcptr:=&codeseq[1]
assem
	mov D0,[pcptr]
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_makeiter =	! KA_MAKEITER
assem
	*saveregs
	*callm k_makeiter
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_forall =	! KA_FORALL
assem
!jmp L30
	mov D3,[Dprog+kopndb]		!D3=pit
	mov D4,[Dprog+kopndc]		!D4=poolvar
	add D3,Dframe
	add D4,Dframe
	dec word32 [D3+kitcount]
	jz L30						!reached end of loop
	cmp byte [D3+kittype],tlist
	jnz L3
!list
	mov D5,[D3+kvarptr]			!pelem
	mov A0,[D4+ktag]

!	and A0,hasrefmask
!	jz L21
!	mov D6,[D4+kobjptr]
!
!	mov A0,[D6+jrefcount]
!	sub A0,1
!	mov [D6+jrefcount],A0
!
L21:
	mov A0,[D5+ktag]
	mov [D4+ktag],A0
	mov A1,[D5+kvalue]
	mov [D4+kvalue],A1

!	and A0,hasrefmask
!	jz L22
!	inc word32 [D1+jrefcount]

L22:
	add D5,varsize
	mov [D3+kvarptr],D5
L29:
	mov Dprog,[Dprog+kopnda]
	*jumpnext


L3:
	jmp L99

L30:
	*jumpskip4

L99:
	*saveregs
	*callm k_forall
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_forallx =	! KA_FORALLX
assem
	*saveregs
	*callm k_forallx
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_foreach =	! KA_FOREACH
assem
	*saveregs
	*callm k_foreach
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_foreachx =	! KA_FOREACHX
assem
	*saveregs
	*callm k_foreachx
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_expandrange =	! KA_EXPANDRANGE
assem
	*saveregs
	*callm k_expandrange
	*loadregs
	*bjumpnext
endassem endproc

threadedproc ka_callappl =
assem
	*saveregs
	*callm k_callappl
	*loadregs
	*bjumpnext
endassem endproc

!threadedproc ka_lastcmd =	! KA_LASTCMD
!assem
!	*saveregs
!	*callm k_lastcmd
!	*loadregs
!	*bjumpnext
!endassem endproc

threadedproc jx_push_fff= 	! JX_PUSH_FFF
assem
	*pushvar3
	mov D4,[Dprog+kopnda]
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+xc+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+xc+kvalue],D1
	and A0,hasrefmask
	jz L1
	inc word32 [D1+jrefcount]
L1:

	mov D4,[Dprog+kopndb]
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+yb+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+yb+kvalue],D1
	and A0,hasrefmask
	jz L2
	inc word32 [D1+jrefcount]
L2:

	mov D4,[Dprog+kopndc]
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+za+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+za+kvalue],D1
	and A0,hasrefmask
	jz L3
	inc word32 [D1+jrefcount]
L3:
	add Dprog,intpsize6
	*jumpnext

!	*saveregs
!	*callm k_push_fff
!	*loadregs
!	*jumpnext
!!	*jumpskip4
endassem endproc

threadedproc jx_push_ff= 	! JX_PUSH_FF
assem
	*pushvar2
	mov D4,[Dprog+kopnda]
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+xb+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+xb+kvalue],D1
	and A0,hasrefmask
	jz L1
	inc word32 [D1+jrefcount]
L1:
	mov D4,[Dprog+kopndb]
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+ya+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+ya+kvalue],D1
	and A0,hasrefmask
	jz L2
	inc word32 [D1+jrefcount]
L2:
	*jumpskip4
endassem endproc

threadedproc jx_push_mm= 	! JX_PUSH_MM
assem
	*pushvar2
	mov D4,[Dprog+kopnda]
	mov D0,[D4+ktag]
	mov [Dsptr+xb+ktag],D0
	mov D1,[D4+kvalue]
	mov [Dsptr+xb+kvalue],D1
	and A0,hasrefmask
	jz L1
	inc word32 [D1+jrefcount]
L1:

	mov D4,[Dprog+kopndb]
	mov D0,[D4+ktag]
	mov [Dsptr+ya+ktag],D0
	mov D1,[D4+kvalue]
	mov [Dsptr+ya+kvalue],D1
	and A0,hasrefmask
	jz L2
	inc word32 [D1+jrefcount]
L2:

	*jumpskip4
endassem endproc

threadedproc jx_push_fm= 	! JX_PUSH_FM
assem
	*pushvar2
	mov D4,[Dprog+kopnda]
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+xb+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+xb+kvalue],D1
	and A0,hasrefmask
	jz L1
	inc word32 [D1+jrefcount]
L1:

	mov D4,[Dprog+kopndb]
	mov D0,[D4+ktag]
	mov [Dsptr+ya+ktag],D0
	mov D1,[D4+kvalue]
	mov [Dsptr+ya+kvalue],D1
	and A0,hasrefmask
	jz L2
	inc word32 [D1+jrefcount]
L2:

	*jumpskip4
endassem endproc

threadedproc jx_push_fci= 	! JX_PUSH_FCI
assem
	mov D4,[Dprog+kopnda]
	*pushvar2
	mov D0,[Dframe+D4+ktag]
	mov [Dsptr+xb+ktag],D0
	mov D1,[Dframe+D4+kvalue]
	mov [Dsptr+xb+kvalue],D1
	and A0,hasrefmask
	jz L1
	inc word32 [D1+jrefcount]
L1:

	mov word32 [Dsptr+ya+ktag],tint
	mov D0,[Dprog+kopndb]
	mov [Dsptr+ya+kvalue],D0

	*jumpskip4
endassem endproc

threadedproc jx_push_mci= 	! JX_PUSH_MCI
assem
	mov D4,[Dprog+kopnda]
	*pushvar2
	mov D0,[D4+ktag]
	mov [Dsptr+xb+ktag],D0
	mov D1,[D4+kvalue]
	mov [Dsptr+xb+kvalue],D1
	and A0,hasrefmask
	jz L1
	inc word32 [D1+jrefcount]
L1:

	mov word32 [Dsptr+ya+ktag],tint
	mov D0,[Dprog+kopndb]
	mov [Dsptr+ya+kvalue],D0

	*jumpskip4
endassem endproc

threadedproc jx_move_ff= 	! JX_MOVE_FF
assem
	mov D5,[Dprog+kopndb]
	cmp byte [Dframe+D5+khasref],1
	jnz L1
	mov D1,[Dframe+D5+kobjptr]
	inc word32 [D1+jrefcount]		!increment before freeing (in case of a:=a)
L1:
	mov D4,[Dprog+kopnda]
	add D4,Dframe
	cmp byte [D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D5,[Dprog+kopndb]
	mov D0,[Dframe+D5+ktag]
	mov [D4+ktag],D0
	mov D1,[Dframe+D5+kvalue]
	mov [D4+kvalue],D1

	*jumpskip4
endassem endproc

!threadedproc jx_move_ff= 	! JX_MOVE_FF
!assem
!	mov D4,[Dprog+kopnda]
!	add D4,Dframe
!	cmp byte [D4+khasref],1
!	jnz L1
!	push D4
!	*callvxufree_d4
!	pop D4
!L1:
!	mov D5,[Dprog+kopndb]
!	mov D0,[D5+Dframe+ktag]
!	mov [D4+ktag],D0
!	mov D1,[D5+Dframe+kvalue]
!	mov [D4+kvalue],D1
!	and A0,hasrefmask
!	jz L2
!	inc word32 [D1+jrefcount]
!L2:
!	*jumpskip4
!endassem endproc

threadedproc jx_zmove_ff= 	! JX_ZMOVE_FF
assem
	mov D4,[Dprog+kopnda]
	add D4,Dframe

	mov D5,[Dprog+kopndb]
	mov D0,[D5+Dframe+ktag]
	mov [D4+ktag],D0
	mov D1,[D5+Dframe+kvalue]
	mov [D4+kvalue],D1
	cmp byte [D4+khasref],1
	jnz L2
	inc word32 [D1+jrefcount]
L2:
	*jumpskip4
endassem endproc

!threadedproc jx_move_mf= 	! JX_MOVE_MF
!assem
!	mov D4,[Dprog+kopnda]
!	cmp byte [D4+khasref],cc_owner
!	jnz L1
!	push D4
!	*callvxufree_d4
!	pop D4
!L1:
!	mov D5,[Dprog+kopndb]
!	add D5,Dframe
!	mov D0,[D5+ktag]
!	mov [D4+ktag],D0
!	mov D1,[D5+kvalue]
!	mov [D4+kvalue],D1
!	cmp byte [D4+khasref],1
!	jnz L2
!	inc word32 [D1+jrefcount]
!L2:
!	*jumpskip4
!endassem endproc

threadedproc jx_move_mf= 	! JX_MOVE_MF
assem
	mov D5,[Dprog+kopndb]
	cmp byte [Dframe+D5+khasref],1
	jnz L1
	mov D1,[Dframe+D5+kobjptr]
	inc word32 [D1+jrefcount]		!increment before freeing (in case of a:=a)
L1:
	mov D4,[Dprog+kopnda]
	cmp byte [D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D5,[Dprog+kopndb]
	mov D0,[Dframe+D5+ktag]
	mov [D4+ktag],D0
	mov D1,[Dframe+D5+kvalue]
	mov [D4+kvalue],D1

	*jumpskip4
endassem endproc


!threadedproc jx_move_fm= 	! JX_MOVE_FM
!assem
!	mov D4,[Dprog+kopnda]
!	add D4,Dframe
!	cmp byte [D4+khasref],1
!	jnz L1
!	push D4
!	*callvxufree_d4
!	pop D4
!L1:
!	mov D5,[Dprog+kopndb]
!	mov D0,[D5+ktag]
!	mov [D4+ktag],D0
!	mov D1,[D5+kvalue]
!	mov [D4+kvalue],D1
!	and D0,hasrefmask
!	jz L2
!	inc word32 [D1+jrefcount]
!L2:
!	*jumpskip4
!endassem endproc

threadedproc jx_move_fm= 	! JX_MOVE_FM
assem
	mov D5,[Dprog+kopndb]
	cmp byte [D5+khasref],1
	jnz L1
	mov D1,[D5+kobjptr]
	inc word32 [D1+jrefcount]		!increment before freeing (in case of a:=a)
L1:
	mov D4,[Dprog+kopnda]
	add D4,Dframe
	cmp byte [D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D5,[Dprog+kopndb]
	mov D0,[D5+ktag]
	mov [D4+ktag],D0
	mov D1,[D5+kvalue]
	mov [D4+kvalue],D1

	*jumpskip4
endassem endproc

!threadedproc jx_move_mm= 	! JX_MOVE_MM
!assem
!	mov D4,[Dprog+kopnda]
!	cmp byte [D4+khasref],cc_owner
!	jnz L1
!	push D4
!	*callvxufree_d4
!	pop D4
!L1:
!	mov D5,[Dprog+kopndb]
!	mov D0,[D5+ktag]
!	mov [D4+ktag],D0
!	mov D1,[D5+kvalue]
!	mov [D4+kvalue],D1
!	cmp byte [D4+khasref],1
!	jnz L2
!	inc word32 [D1+jrefcount]
!L2:
!	*jumpskip4
!endassem endproc

threadedproc jx_move_mm= 	! JX_MOVE_MM
assem
	mov D5,[Dprog+kopndb]
	cmp byte [D5+khasref],1
	jnz L1
	mov D1,[D5+kobjptr]
	inc word32 [D1+jrefcount]		!increment before freeing (in case of a:=a)
L1:
	mov D4,[Dprog+kopnda]
	cmp byte [D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D5,[Dprog+kopndb]
	mov D0,[D5+ktag]
	mov [D4+ktag],D0
	mov D1,[D5+kvalue]
	mov [D4+kvalue],D1

	*jumpskip4
endassem endproc


threadedproc jx_move_mci= 	! JX_MOVE_MCI
assem
	mov D4,[Dprog+kopnda]
	cmp byte [D4+khasref],1
	jnz L1
	push D4
	*callvxufree_d4
	pop D4
L1:
	mov word32 [D4+ktag],tint
	mov D0,[Dprog+kopndb]
	mov [D4+kvalue],D0
	*jumpskip4
endassem endproc

threadedproc jx_move_fci= 	! JX_MOVE_FCI
assem
	mov D4,[Dprog+kopnda]
	add D4,Dframe
	cmp byte [D4+khasref],1
	jnz L1
	push D4
	*callvxufree_d4
	pop D4
L1:
	mov word32 [D4+ktag],tint
	mov D0,[Dprog+kopndb]
	mov [D4+kvalue],D0
	*jumpskip4
endassem endproc

threadedproc jx_zmove_fci= 	! JX_ZMOVE_FCI
assem
	mov D4,[Dprog+kopnda]
	mov word32 [Dframe+D4+ktag],tint
	mov D0,[Dprog+kopndb]
	mov [Dframe+D4+kvalue],D0
	*jumpskip4
endassem endproc

threadedproc jx_pushz_void2 =	! JX_PUSHZ_VOID2
assem
	*pushvar2
	mov word32 [Dsptr+xb+ktag],tvoid
	mov word32 [Dsptr+ya+ktag],tvoid
	*jumpskip2
endassem endproc

threadedproc jx_pushz_void3 =	! JX_PUSHZ_VOID3
assem
	*pushvar3
	mov word32 [Dsptr+xc+ktag],tvoid
	mov word32 [Dsptr+yb+ktag],tvoid
	mov word32 [Dsptr+za+ktag],tvoid
	*jumpskip3
endassem endproc

threadedproc jx_switch_f =	! JX_SWITCH
assem
	mov D3,[Dprog+kopnda]
	cmp word16 [D3+Dframe+ktag],tint
	jnz L99				!get C deal with errors
	mov D4,[D3+Dframe+kvalue]		!switch index
	sub D4,[Dprog+kopndc]		!index-lower! now 0-based index
	cmp D4,[Dprog+kopndb]		!index0>=n?
	jae L2				!out of range
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
pcerror("jxswitchf/not int")
end

threadedproc jx_upb_f =	! JX_UPB_F
assem
	mov D3,[Dprog+kopnda]
	cmp byte [D3+Dframe+khasref],1
	jnz L2
	mov D4,[D3+Dframe+kobjptr]			!assume this is an object
	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov A0,[D4+jlength]
!	add A0,[D4+jlower]
	movsx A1,word16 [D4+jlower]
	add A0,A1
	dec A0
	mov [Dsptr+kvalue],D0
	*jumpskip3
L2:
	cmp word16 [D3+Dframe+ktag],trange
	jnz L99
	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov A0,[D3+Dframe+krange_upper]
	mov [Dsptr+kvalue],D0
	*jumpskip3

L99:
	*csaveregs
end
pcerror("jxupb/not obj")
end

threadedproc jx_len_f =	! JX_LEN_F
static variant p

assem
	mov D3,[Dprog+kopnda]
	mov [p],D3
	cmp word16 [D3+Dframe+ktag],tbignum
	jz L3

	cmp byte [D3+Dframe+khasref],1
	jnz L2
	mov D4,[D3+Dframe+kobjptr]			!assume this is an object
	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov A0,[D4+jlength]
	mov [Dsptr+kvalue],D0
	*jumpskip3
L2:
	cmp word16 [D3+Dframe+ktag],trange
	jnz L99
	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov A0,[D3+Dframe+krange_upper]
	sub A0,[D3+Dframe+krange_lower]
	inc A0
	mov [Dsptr+kvalue],D0
	*jumpskip3

L3:
	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov A0,[D3+Dframe+kbndigits]
	imul2 A0,6
	mov [Dsptr+kvalue],D0
	*jumpskip3

L99:
	*csaveregs
end
p:=cast(frameptr+getopnda)
CPL gettypename(p^.tag),p^.hasref
pcerror("jxlen/not obj")
end

threadedproc jx_jumpdef_f =	! JX_JUMPDEF_F
assem
	mov D3,[Dprog+kopnda]
	cmp word16 [D3+Dframe+ktag],tvoid
	jg Lisdef1
	*jumpskip4

Lisdef1:
	mov Dprog,[Dprog+kopndb]
	*jumpnext
endassem endproc

threadedproc jx_jumpvoid_f =	! JX_JUMPVOID_F
assem
	mov D3,[Dprog+kopnda]
	cmp word16 [D3+Dframe+ktag],tvoid
	jbe L1
	*jumpskip4

L1:
	mov Dprog,[Dprog+kopndb]
	*jumpnext
endassem endproc

threadedproc jx_free_10xx= 	! JX_FREE_10
assem
jx_free_10:
	cmp byte [Dsptr+khasref],1
	jnz L1
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L1
	*callfreex_dsptr
L1:
	*popvar

jx_free_9:
	cmp byte [Dsptr+khasref],1
	jnz L2
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L2
	*callfreex_dsptr
L2:
	*popvar

jx_free_8:
	cmp byte [Dsptr+khasref],1
	jnz L3
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L3
	*callfreex_dsptr
L3:
	*popvar

jx_free_7:
	cmp byte [Dsptr+khasref],1
	jnz L4
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L4
	*callfreex_dsptr
L4:
	*popvar

jx_free_6:
	cmp byte [Dsptr+khasref],1
	jnz L5
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L5
	*callfreex_dsptr
L5:
	*popvar

jx_free_5:
	cmp byte [Dsptr+khasref],1
	jnz L6
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L6
	*callfreex_dsptr
L6:
	*popvar

jx_free_4:
	cmp byte [Dsptr+khasref],1
	jnz L7
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L7
	*callfreex_dsptr
L7:
	*popvar

jx_free_3:
	cmp byte [Dsptr+khasref],1
	jnz L8
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L8
	*callfreex_dsptr
L8:
	*popvar

jx_free_2:
	cmp byte [Dsptr+khasref],1
	jnz L9
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L9
	*callfreex_dsptr
L9:
	*popvar

jx_free_1:
	cmp byte [Dsptr+khasref],1
	jnz L10
	mov D1,[Dsptr+kobjptr]
	dec word32 [D1+jrefcount]
	jnz L10
	*callfreex_dsptr
L10:
	*popvar

	*jumpskip2
endassem endproc

threadedproc jx_jumptrue_i =	! JX_JUMPTRUE_I
assem
	mov D0,[Dsptr+xa+kvalue]
	and D0,D0
	jz L2
	mov Dprog,[Dprog+kopnda]
	*popvar
	*jumpnext

L2:
	*popvar
	*jumpskip2
endassem endproc

threadedproc jx_jumpfalse_i =	! JX_JUMPFALSE_I
assem
	mov D0,[Dsptr+xa+kvalue]
	and D0,D0
	jnz L2
	mov Dprog,[Dprog+kopnda]
	*popvar
	*jumpnext

L2:
	*popvar
	*jumpskip2
endassem endproc

threadedproc jx_add_ff= 	! JX_ADD_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L1
	cmp word16 [D5+Dframe+ktag],tint
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
!	jmp ka_add

	*saveregs
	*callm k_add
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_add_fci= 	! JX_ADD_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
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
!	jmp ka_add

	*saveregs
	*callm k_add
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_sub_ff= 	! JX_SUB_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L1
	cmp word16 [D5+Dframe+ktag],tint
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
!	jmp ka_sub
	*saveregs
	*callm k_sub
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_sub_fci= 	! JX_SUB_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
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
!	jmp ka_sub
	*saveregs
	*callm k_sub
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpeq_ff= 	! JX_JUMPEQ_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99
	cmp word16 [D5+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jnz Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpeq

	*saveregs
	*callm k_jumpeq
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpeq_fci= 	! JX_JUMPEQ_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dprog+kopndb]
	jnz Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpeq

	*saveregs
	*callm k_jumpeq
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpne_ff= 	! JX_JUMPNE_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99
	cmp word16 [D5+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jz Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpne

	*saveregs
	*callm k_jumpne
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpne_fci= 	! JX_JUMPNE_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dprog+kopndb]
	jz Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpne

	*saveregs
	*callm k_jumpne
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumplt_ff= 	! JX_JUMPLT_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99
	cmp word16 [D5+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jge Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumplt

	*saveregs
	*callm k_jumplt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumplt_fci= 	! JX_JUMPLT_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dprog+kopndb]
	jge Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumplt

	*saveregs
	*callm k_jumplt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumple_ff= 	! JX_JUMPLE_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99
	cmp word16 [D5+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jg Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumple

	*saveregs
	*callm k_jumple
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumple_fci= 	! JX_JUMPLE_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dprog+kopndb]
	jg Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumple

	*saveregs
	*callm k_jumple
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpge_ff= 	! JX_JUMPGE_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99
	cmp word16 [D5+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jl Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpge

	*saveregs
	*callm k_jumpge
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpge_fci= 	! JX_JUMPGE_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dprog+kopndb]
	jl Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpge

	*saveregs
	*callm k_jumpge
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpgt_ff= 	! JX_JUMPGT_FF
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99
	cmp word16 [D5+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dframe+D5+kvalue]
	jle Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpgt

	*saveregs
	*callm k_jumpgt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_jumpgt_fci= 	! JX_JUMPGT_FCI
assem
	mov D4,[Dprog+kopnda]
	mov D5,[Dprog+kopndb]
	cmp word16 [D4+Dframe+ktag],tint
	jnz L99

	mov D0,[Dframe+D4+kvalue]
	cmp D0,[Dprog+kopndb]
	jle Lfalse
	mov Dprog,[Dprog+kopndc]
	*jumpnext
Lfalse:
	add Dprog,intpsize6
	*jumpnext

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
!	jmp ka_jumpgt

	*saveregs
	*callm k_jumpgt
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_pushix_ff =	! JX_PUSHIX_FF
static varrec vvv
!static word64 reg6

assem
	mov D2,[Dprog+kopnda]
	mov D3,[Dprog+kopndb]
!jmp L99
	cmp word16 [D2+Dframe+ktag],tlist
	jnz L99
	cmp word16 [D3+Dframe+ktag],tint
	jnz L99

!list[int]
	mov D6,[D2+Dframe+ktag]

	mov D5,[D2+Dframe+kobjptr]
	mov D4,[D3+Dframe+kvalue]		!index
	sub A4,[D5+jlower]		!0-base
	cmp A4,[D5+jlength]
	jae L99					!bounds error: let B deal with it

!jmp L99
	shl A4,varshift				!index*varsize
	add D4,[D5+jptr]			!point to element

	*pushvar
	mov D0,[D4+ktag]
	mov [Dsptr+ktag],D0			!replace index by list element
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
	and A0,hasrefmask
	jz L13
	inc word32 [D1+jrefcount]
L13:

	add Dprog,intpsize4
	jmp ka_pushix

	*saveregs
	*callm k_pushix
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_popix_ff =	! JX_POPIX_FF
assem
	mov D2,[Dprog+kopnda]
	mov D3,[Dprog+kopndb]
!jmp L99
	cmp word16 [D2+Dframe+ktag],tlist
	jnz L99
	cmp word16 [D3+Dframe+ktag],tint
	jnz L99

!list[int]
	mov D5,[D2+Dframe+kobjptr]

	cmp byte[D5+jmutable],0
	jz L99


	mov D4,[D3+Dframe+kvalue]		!index
	sub A4,[D5+jlower]		!0-base
	cmp A4,[D5+jlength]
	jae L99					!bounds error: let B deal with it (might be extending too)

	shl A4,varshift				!index*varsize
	add D4,[D5+jptr]			!point to element

!now need to pop stack to at D4

	cmp byte [Dsptr+khasref],1
	jnz L1
	mov D1,[Dsptr+kobjptr]
	inc word32 [D1+jrefcount]
L1:
	cmp byte [D4+khasref],1
	jnz L2
	push D4
	*callvxufree_d4
	pop D4
L2:
	mov D0,[Dsptr+ktag]
	mov [D4+ktag],D0
	mov D1,[Dsptr+kvalue]
	mov [D4+kvalue],D1
	*popvar

	add Dprog,intpsize6
	*jumpnext

L99:
	*pushvar2

	mov word32 [Dsptr+xb+ktag],trefvar
	lea D0,[D2+Dframe]
	mov [Dsptr+xb+kvarptr],D0

	mov D0,[Dframe+D3+ktag]
	mov [Dsptr+ya+ktag],D0
	mov D1,[Dframe+D3+kvalue]
	mov [Dsptr+ya+kvalue],D1
	and D0,hasrefmask
	jz L100
	inc word32 [D1+jrefcount]
L100:
	add Dprog,intpsize4		!pushixref followed by popptr next
!	jmp ka_pushixref

	*saveregs
	*callm k_pushixref
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_pushptr_f =	! JX_PUSHPTR_F
assem
	*pushvar
	mov D2,[Dprog+kopnda]
	add D2,Dframe
!jmp L99
	cmp word16 [D2+ktag],trefvar
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
	cmp word16 [D2+ktag],trefpacked
	jnz L2

	mov D4,[D2+kpackptr]
	movzx A0,word16 [D2+krefelemtag]
	cmp A0,ti32
	jnz L10
	mov word32 [Dsptr+ktag],tint
	mov A0,[D4]
	movsxd D0,A0
	mov [Dsptr+kvalue],D0
	*jumpskip3
L10:
	cmp A0,tu8
	jnz L11
	mov word32 [Dsptr+ktag],tint
	movzx A0,byte [D4]
	mov [Dsptr+kvalue],D0
	*jumpskip3
L11:

L2:
L99:
	mov D0,[D2+ktag]
	mov [Dsptr+xa+ktag],D0
	mov D1,[D2+kvalue]
	mov [Dsptr+xa+kvalue],D1			!assume pointer is not a heap object
	add Dprog, intpsize2
	jmp ka_pushptr

	*saveregs
	*callm k_pushptr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_pushincrptr_m =	! JX_PUSHINCRPTR_M
!do push_am/loadincr/pushptr, optimised for byte-pointers
!used in the rvalue: p++^ 

assem
	mov D2,[Dprog+kopnda]
!jmp L99
jx_pushipm1:
	cmp word16 [D2+ktag],trefpacked
	jnz L99							!not pointer to packed type
	cmp word16 [D2+krefelemtag],tu8
	jnz L99

!pointer to byte
	mov D5,[D2+kpackptr]
	inc word64 [D2+kpackptr]
	movzx A0,byte [D5]

	*pushvar
	mov word32 [Dsptr+ktag],tint
	mov [Dsptr+kvalue],D0
!	add Dprog,intpsize4
	*jumpskip4

L99:
	*pushvar

	mov word32 [Dsptr+ktag],trefvar
	lea D0,[D2]
	mov [Dsptr+kvarptr],D0
	add Dprog,intpsize2			!point at loadincr (with pushptr next)

	*saveregs
	*callm k_loadincr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_pushincrptr_f =	! JX_PUSHINCRPTR_F
!do push_af/loadincr/pushptr, optimised for byte-pointers
!used in the rvalue: p++^ 

assem
	mov D2,[Dprog+kopnda]
	add D2,Dframe
!jmp L99
	jmp jx_pushincrptr_m.jx_pushipm1
endassem endproc

threadedproc jx_popincrptr_m =	! JX_POPINCRPTR_M

assem
	mov D2,[Dprog+kopnda]
!jmp L99
jx_popipm1:
	cmp word16 [D2+ktag],trefpacked
	jnz L99							!not pointer to packed type
	cmp word16 [Dsptr+ktag],tint
	jnz L99							!let B deal with conversions or errors

	cmp word16 [D2+krefelemtag],tu8
	jnz L2

!pointer to byte
	mov D5,[D2+kpackptr]
	inc word64 [D2+kpackptr]

	mov D0,[Dsptr+kvalue]
	mov [D5],B0
	*popvar
	*jumpskip4

L2:
	cmp word16 [D2+krefelemtag],ti32
	jnz L3

!pointer to int32
	mov D5,[D2+kpackptr]
	add word64 [D2+kpackptr],4

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
	add Dprog,intpsize2			!point at loadincr (with pushptr next)

	*saveregs
	*callm k_loadincr
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_popincrptr_f =	! JX_POPINCRPTR_F
assem
	mov D2,[Dprog+kopnda]
	add D2,Dframe
!jmp L99
	jmp jx_popincrptr_m.jx_popipm1
endassem endproc

threadedproc jx_addto_ff =	! JX_ADDTO_FF
assem
	mov D2,[Dprog+kopnda]
	mov D3,[Dprog+kopndb]
!jmp L99
	cmp word16 [D2+Dframe+ktag],tint
	jnz L1
	cmp word16 [D3+Dframe+ktag],tint
	jnz L1

	mov D0,[D3+Dframe+kvalue]
	add [D2+Dframe+kvalue],D0
	*jumpskip5

L1:
!	cmp word16 [D2+Dframe+ktag],treal
!	jnz L2
!	cmp word16 [D3+Dframe+ktag],treal
!	jnz L2
!
!	fld word64 [D2+Dframe+kvalue]
!	fadd word64 [D3+Dframe+kvalue]
!	fstp word64 [D2+Dframe+kvalue]
!	*jumpskip5

L2:
L99:
	*pushvar2

	mov word32 [Dsptr+xb+ktag],trefvar
	lea D0,[D2+Dframe]
	mov [Dsptr+xb+kvarptr],D0

	mov D0,[Dframe+D3+ktag]
	mov [Dsptr+ya+ktag],D0
	mov D1,[Dframe+D3+kvalue]
	mov [Dsptr+ya+kvalue],D1
	and A0,hasrefmask
	jz L100
	inc word32 [D1+jrefcount]
L100:

	add Dprog,intpsize4		!push_af then push_f
	jmp ka_addto

	*saveregs
	*callm k_addto
	*loadregs
	*bjumpnext
endassem endproc

threadedproc jx_addto_fci =	! JX_ADDTO_FCI
assem
	mov D2,[Dprog+kopnda]
!jmp L99
	cmp word16 [D2+Dframe+ktag],tint
	jnz L1

	mov D0,[Dprog+kopndb]
	add [D2+Dframe+kvalue],D0
	*jumpskip5
L1:
L99:
	*pushvar2

	mov word32 [Dsptr+xb+ktag],trefvar
	lea D0,[D2+Dframe]
	mov [Dsptr+xb+kvarptr],D0

	mov word32 [Dsptr+ya+ktag],tint
	mov D0,[Dprog+kopndb]
	mov [Dsptr+ya+kvalue],D0

	add Dprog,intpsize4
	jmp ka_addto

	*saveregs
	*callm k_addto
	*loadregs
	*bjumpnext
endassem endproc

proc calltest=
assem
	*pushvar
	mov word32 [Dsptr+ktag],tretaddr
	lea D0,[Dprog+24]		! return address
	mov [Dsptr+kretaddr],D0
	mov [Dsptr+kframeptr_low],Aframe
!	mov D0,D4
	mov [Dsptr+kstackadj],B4
	mov Dframe,Dsptr
	mov [frameptr],Dframe
	mov Dprog,D5
	ret
end
end
=== var_decls.m 24/38 ===
import clib
import mlib
import var_types
import qc_tables

global const tu1=tbit
global const tu2=tbit2
global const tu4=tbit4

!global type intpc = intm
global type intpc = int64
global type unit = ref unitrec

global const maxmodule=50

global tabledata() [0:]ichar usercatnames =
	(std_cat=0,	$),
	(anon_cat,	$),
	(user_cat,	$)
end

global record attribrec = !keep this 16 bytes
	byte ax_global				!1=global module-level name; 0=local or not module-level
	byte ax_static				!0 or 1
	byte ax_equals					!0 or 1 if @ used (static/frame vars only)
	byte ax_at					!0 or 1 if @ used (fields only)
	byte ax_byrefmode				!0 or 1 if & used
	byte ax_optional				!0 or 1	
	byte ax_varparams				!0 or 1	
	byte ax_used				!0 or 1	
	byte ax_forward				!0 or 1: 1 means forward decl of label or function
	byte ax_frame				!0 or 1: 1 when frameid/paramid
	byte ax_autovar				!0 or 1: 1 when an autovar with "$" in the name
	byte ax_nparams				!no. formal params for procid/dllprocid

	byte ax_fflang				!0 windowsff. etc.
	byte ax_moduleno
	byte ax_baseclass			!type of owner class (limited up to type 255 only)
	union
		byte ax_align				!0, 2, 4, 8, 16 or 255 (auto-align)
!		byte ax_defined			!for procs during pcl fixup: 1 means has been defined
		byte ax_dllindex		!for dllproc: which dll in dlltable
		byte ax_extmodno		!for proc call chains: module no of proc owner
	end
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record fieldrec =
	ichar name
	int16 recordtype
	int16 fieldtype
	int fieldoffset
end

global record strec =
	ichar name
	ref strec owner
	ref strec deflist
	ref strec nextdef
	ref strec nextdupl
	ref strec prevdupl

	union
		ref strec nextparam
		ref unitrec callchain
	end
	ref unitrec code
	union
		ref strec paramlist
		uflagsrec uflags
	end
	union
		ref strec equiv		!for aliasid/linkid only; or .attribs.ax-at=1
		ichar docstring		!for procid only
	end
	union
		ichar truename
		ichar metadata
		ichar macrovalue
!		ichar docstring
!		ref void macrotoken
	end

	byte namelen
	byte symbol
	byte nameid
	byte SPAREBYTE

	int16 subcode

	int16 mode
	int32 index					!needs to hold pcindex (also indices for all symbols or .bc files)
!	int16 SPAREINT16

!	[4]byte FILLER

	union
		ref void address
		int32 offset
		ref intpc pcaddress
		int32 base_class
		int32 bcindex
!		int lastprocindex		!create fwd chains during fixups
	end
	int32 lineno
	attribrec attribs
end

global record unitrec =
	int32 tag			!kcode tag number
	int32 lineno			!source lineno associated with item; fileno is in top byte

	ref unitrec a	!single items, or linked lists
	ref unitrec b
	ref unitrec c
	ref unitrec nextunit
!	ref unitrec nextcall

	union
		ref strec def
		int64 value
		word64 uvalue
		real xvalue
		ichar svalue
		ref strec labeldef
		struct
			int32 range_lower
			int32 range_upper
		end
!		ref[]int exceptionlist
	end
	union
		int32 opcode
		int32 index
		int32 trylevel
!		int nexceptions
		int32 slength
		int32 length
		byte dottedname		!for j_name: 1=resolved from fully qualified dotted seq
	end

	int16 mode
	int16 moduleno
end

global record genfieldnamerec =
	union
		ref strec def				!when compiling
		ichar name					!after bc load
	end
	int32 dataindex
	union
		int32 datalength
		int32 datalast
	end
end

global record genfielddatarec =
	int32 fieldindex
	int32 recordtype
	int32 fieldtype			!-procid, -constid, -staticid, -typeid are special codes
	union
		int32 offset			!or const value
		word32 index			!into proctable, statictable, or type code
		word32 procoffset
	end
end

global record modulerec =
	ichar name
	ichar filename
	ichar sourcecode
	ichar qafilestr
	ref strec stmodule
	ref[]intpc pccode
	ref[0:]word16 linetable
	int32 sourcelen
	int32 npccode				!current allocated size of pccode
	int32 pcindex				!index of last entry in pccode
!	int32 nlines
	int32 level
	int32 exported			!imported within export/endexport
	[maxmodule]byte importmap

end

global record dllprocrec =
	ichar name
!	ref proc address
	int32 dllindex				!dll module index
end

global record procrec =
	ref strec def
	ref procrec nextproc
end

global record userxrec =
	ref strec owner
	ref int16 pmode
	ref userxrec nextmode
end

global const int maxtype=500
global const int maxuserxtype=5000

global int ntypes
global int nuserxtypes
global int userxtypebase			!first index (growing downwards) of userxtypes in current module
global ref userxrec userxmodelist	!list of all references to userx modes

global [0:maxtype]int32 ttmodule		!module number
global [0:maxtype]ref strec ttnamedef
global [0:maxtype]int32 ttbasetype	!basetype
global [0:maxtype]ichar ttname 	!name of type
global [0:maxtype]int32 ttbitwidth

global [0:maxtype]int32 ttsize 		!.size in bytes
global [0:maxtype]int32 ttlower 		!.lbound (default 1 or case unused)
global [0:maxtype]int32 ttlength 		!elements in array/record (actual fields) (/string

global [0:maxtype]unit ttlowerexpr		!these are processed later to lower/length
global [0:maxtype]unit ttlengthexpr
global [0:maxtype]ref strec ttowner		!for ttlowerexpr/rtlengthexpr

global [0:maxtype]int32 ttstartfield 		!start index in pcfieldtable^[]
global [0:maxtype]int32 ttstructfields	!entries in pcfieldtable^[]

!global [0:maxtype]int32 tttarget 		!for array/ref types
global [0:maxtype]int16 tttarget 		!for array/ref types
global [0:maxtype]byte ttusercat

global [0:maxtype]byte typestarterset
global [0:maxtype]int ttlineno
global [0:maxtype]byte ttmoduleno

global [0:maxuserxtype]ref strec ttnamedefx
global [0:maxuserxtype]ref strec ttnamedefx2
global [0:maxuserxtype]int ttlinenox
global [0:maxuserxtype]int ttxmap
global [0:maxuserxtype]byte ttxmoduleno

global [0:256]int16 baseclasstable
global [0:256]ref strec baseclassdef
global int nbaseclasses

global const hasrefmask = 0x10000		!1st bit of 3rd byte, when writing to .tagx

!global [0..host_last]byte hostlvset

global tabledata() [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global [0:symbolnames.len]byte exprstarterset

global const int varsize=16

global [0..maxmodule]modulerec moduletable
global int nmodules
global int currmoduleno				!used when compiling modules
global ref modulerec currmodule
global [maxmodule]int moduleinitorder
global int ninitmodules

global [0..maxmodule]ichar inputfiles
global int ninputfiles

global const maxsearchdirs=6
global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0

global ref strec stprogram		!root into the symbol table
global ref strec stmodule		!main module
global ref strec stsysmodule	!optional sys module (needed for name resolving)
global ref strec alldeflist		!link together all (user) symbols

global int optflag=0		!1=stdoptimise; 0=disabled

global const maxgenfields=1000
global [maxgenfields]genfieldnamerec genfieldnames
global [maxgenfields]genfielddatarec genfielddata
global int ngenfieldnames
global int ngenfielddata

global const sourceext="b"
global const arraylbound=1
global ref unitrec nullunit

global const maxlibpaths=10
global [maxlibpaths]ichar libpaths
global int nlibpaths

global int fverbose=0		!whether to display message for each pass
global int ftrace=0			!whether to line trace
global int fdtrace=0		!whether to line trace, enabled by $setdebug(1)
global int foptimise=0		!whether to generate optimised j-codes

global int mlineno=0		!set in pclgen dispatcher

global int dointlibs=1		!whether to use internal libraries
!global int dointlibs=0		!whether to use internal libraries

global int exportsprepass=0		!1 for preparse scan only

global int debug=0

global int totalstrings=0

!Interpreter run variables

!global const int stacksize=70000
!global const int maxdllindex=30
!
global const maxdlllib=50
global const maxdllproc=2000

global int ndlltable
global [maxdlllib]ref strec dllsttable
global int ndllproctable
global [maxdlllib]ichar dlltable
global [maxdllproc]dllprocrec dllproctable

global const maxapplproc=500
global int napplproctable
global [maxapplproc]ref strec applproctable

!global ref clang proc fprintf_ptr
!global ref clang proc fgets_ptr
!
!global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)
!
!global ichar pcerror_mess=nil		!custom message for pcerror()

global const int maxcmdparam=32
global int ncmdparams
global [0..maxcmdparam]ichar cmdparamtable

global ref procrec proclist			!linked list of all procs
global int nproclist

global int nstrings=0
global int nsymbols=0
!global int nstructfields=0

!global const int maxpcstrings=5000
global const int maxpcstrings=150000
!global const int maxpcstrings=600000
global ref[]ichar stringtable
global ref[]int stringlentable

global ref[]fieldrec pcfieldtable		!controlled with nstructfields


global ichar err_message
global ref intpc stopseq		!point to a 'stop 0' sequence
global ref intpc raiseseq		!point to a sequence of several 'raise' cmdcodes

global ref byte PPP
global int prescanmode			!1 when reading import info only

global int progstart,progend
global int loadstart,loadend
global int parsestart,parseend
global int namesstart,namesend
global int genstart,genend

strbuffer docstrx
global ref strbuffer docstring=&docstrx

global ref strec currproc

global int ALLLINES

!.ma file directory
global const maxqafiles=maxmodule+50
global [0..maxqafiles]ichar qafilenames
global [0..maxqafiles]int qafilesizes
global [0..maxqafiles]int qafileoffsets
global [0..maxqafiles]ichar qafiletext
global [0..maxqafiles]byte qafilefileno			!0 or index into sourcefile tables
global [0..maxqafiles]byte qafilemult			!1 means could be parsed multiple times
global int nqafiles
global ichar qafilesource

global int fbundled=0		!set to 1 when compiling .qa
global int fwriteqa
=== var_types.m 25/38 ===
global tabledata() [0:]ichar stdtypenames, [0:]int stdtypewidths =
	(tvoid=0,		$,		128),	! means variant is unassigned

	(tint,			$,		64),	! 64-bit signed int
	(tword,			$,		64),	! 64-bit unsigned int
	(treal,			$,		64),	! 64-bit float
	(trange,		$,		64),	! 32+32-bit int:int

	(tstring,		$,		0),		! 8-bit string, flex and mutable
	(twstring,		$,		0),		! 16/32-bit string, flex and mutable
	(tlongint,		$,		0),		! Arbitrary precision integer
	(trational,		$,		0),		! Rational number, made of two longints
	(tset,			$,		0),		! Pascal-like bit-set
	(tdict,			$,		0),		! Dictionary of V:V keys and values
	(tword128,		$,		0),		! 128-bit unsigned int

	(tenum,			$,		0),		! (Used in compiler, not at runtime)
	(ttype,			$,		64),	! Represents a type-code
	(toperator,		$,		64),	! Represents an operator (as a bytecode op)
	(tsymbol,		$,		64),	! Reference to a symbol: module, proc, class, etc
	(tretaddr,		$,		0),		! Return address descriptor, only on stack 
	(texception,	$,		0),		! Exception descriptor, only on stack
	(trefproc,		$,		64),	! Pointer to Q proc
	(trefdllproc,	$,		64),	! Pointer to foreign function
	(treflabel,		$,		64),	! Pointer to label
	(tstringz,		$,		64),	! C-style zero-terminated 8-bit string within struct field

	(trefvar,		$,		64),	! Pointer/slice to Variant
	(trefpacked,	$,		0),		! Pointer/slice to Packed (uses target tag)
	(trefbit,		$,		0),		! Pointer/slice to Bits
	(trecordlink,	$,		0),		! Link to record object

	(treflist,		$,		0),		! Pointer to list slice
	(trefarray,		$,		0),		! Pointer to array slice
	(trefbits,		$,		0),		! Pointer to bits slice

	(tlist,			$,		0),		! Sequence of variants
	(tarray,		$,		0),		! Sequence of packed
	(tbits,			$,		0),		! Sequence of bits

	(trecord,		$,		0),		! Record of shorts and longs
	(tstruct,		$,		0),		! Record of packed and flat arrays/structs

	(tuser,			$,		0),		! used for anonymous user types during exportspass
	(tvariant,		$,		128),	!marks transition to packed types

	(tc8,			$,		8),		! 8-bit character
	(ti8,			$,		8),		! 8-bit signed int
	(ti16,			$,		16),	! etc 
	(ti32,			$,		32),	! 
	(ti64,			$,		64),	! 
	(tbit,			"tbit",		1),		! 1-bit unsigned
	(tbit2,			"tbit2",		2),		! 2-bit
	(tbit4,			"tbit4",		4),		! 4-bit
	(tu8,			$,		8),		! 8-bit unsigned int etc
	(tu16,			$,		16),	! 
	(tu32,			$,		32),	! 
	(tu64,			$,		64),	! 
	(tr32,			$,		32),	! 32-bit float
	(tr64,			$,		64),	! 
	(tintm,			$,		32),	! 32 or 64-bit host signed int 
	(twordm,		$,		32),	! 32 or 64-bit host unsigned int 
	(trefm,			$,		64),	! 32 or 64-bit host pointer
!	(tarray,		$,		0),		! Flat array of packed or flat (placeholder type)
!	(tstruct,		$,		0),		! Struct of packed or flat (placeholder type)

!User-defined types go here
	(tlast,			$,		0)		! 	!

end
=== qc_tables.m 26/38 ===
import var_types
import pq_common

global tabledata() [0:]ichar jtagnames=
!Basic units; these don't follow normal rules of params needing to be units or lists

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
![a=u] means a is a unit/list, or is nil

	(j_none=0,		$), ! For tagname lookups when tag is zero
	(j_const,		$), ! value/etc=value, typeno=type code
	(j_null,		$), ! Place holder unit: means 'param no present' when used where a param is expected
	(j_name,		$), ! def=nameptr
	(j_block,		$), ! a=L			List of statements
	(j_codeblock,	$), ! a=L,labeldef=label, def, def=autovar		L=List of statements
	(j_blockdef,	$), ! a=L,labeldef=label, def=autovar		L=List of statements
	(j_doblock,		$), ! a=x
	(j_typeval,		$), ! typeno=T			T=t_void or t_none; value of void or none
	(j_longint,		$), ! svalue=str			Longint constructor

!!Comments and Info
!
!	(j_comment,		$), ! a=string	General comment

!Assorted sub-units

	(j_whenthen,	$), ! a=L b=u
	(j_elsif,		$), ! condcode, a=u.
	(j_fmtitem,		$), ! a=x b=fmtstr
	(j_nogap,		$), ! 

!Statements

	(j_callproc,	$), ! a=x b=L
	(j_callmproc,	$), ! a=x b=L
	(j_return,		$), ! a=x or nil

	(j_assign,		$), ! a b
	(j_shallowcopy,	$), ! a b
	(j_deepcopy,	$), ! a b
	(j_to,			$), ! a=x b=u [def=nameptr]		c is optional temp name
	(j_if,			$), ! condcode a=then b=else
	(j_longif,		$), ! a=(elsif ...) b=else		L is series of kelsif pairs
	(j_forup,		$), ! a=x b=x c=x d=Body [e=Else]		Body is single item or list
	(j_fordown,		$), ! a=x b=x c=x d=Body [e=Else]
	(j_forstep,		$), ! a=x b=x c=x d=Body [e=Else] [f=Step]
	(j_forall,		$), ! a=L b=L     d=Body [e=Else]	a is list of vars, b is list of values
	(j_forallrev,	$), ! a=x b=x     d=Body [e=Else]
	(j_foreach,		$), ! a=L b=L     d=Body [e=Else]	a is list of vars, b is list of values
	(j_foreachrev,	$), ! a=x b=x     d=Body [e=Else]
	(j_cfor,		$), ! a=x b=x     d=Body [e=Else]
	(j_while,		$), ! a=x b=u
	(j_repeat,		$), ! a=u b=x
	(j_goto,		$), ! a=x
	(j_gotoblock,	$), ! a=x
	(j_labeldef,	$), ! def=nameptr
	(j_restart,		$), ! [a=x]
	(j_redo,		$), ! [a=x]
	(j_next,		$), ! [a=x]
	(j_exit,		$), ! [a=x]
	(j_break,		$), ! [a=x]
	(j_do,			$), ! [a=u
	(j_case,		$), ! a=x b=L [c=else]		L is series of whenthen pairs
	(j_docase,		$), ! a=x b=L [c=else]
	(j_switch,		$), ! a=x b=L [c=else]
	(j_doswitch,	$), ! a=x b=L [c=else]
	(j_swap,		$), ! a b
	(j_select,		$), ! Not implemented

	(j_print,		$), ! [a=dev] b=L
	(j_println,		$), ! [a=dev] b=L
	(j_fprint,		$), ! [a=dev] b=fmtstr c=L
	(j_fprintln,	$), ! [a=dev] b=fmtstr c=L
	(j_cprint,		$), ! [a=dev] b=fmtstr c=L
	(j_cprintln,	$), ! [a=dev] b=fmtstr c=L
	(j_sprint,		$), !         b=L 
	(j_sfprint,		$), !         b=L
	(j_scprint,		$), !         b=L
	(j_read,		$), ! [a=dev] b=L
	(j_readln,		$), ! [a=dev] b=L
	(j_sread,		$), ! [a=dev] b=L
	(j_sreadln,		$), ! [a=dev] b=L
	(j_stop,		$), ! [a=x]
	(j_try,			$), ! a=try block; b=except list
	(j_except,		$), ! a=except block; b=exception code list (constants)
	(j_yield,		$), ! "
	(j_raise,		$), ! "
	(j_callhostproc,$), ! "
	(j_callapplproc,$), ! "
	(j_callapplfn,	$), ! "
	(j_eval,		$), ! "

	(j_listcomp,	$), ! a=for typeno=T	Start listcomp evaluation
	(j_appendlc,	$), ! a	typeno=T		Add a to current listcomp

	(j_startiter,	$), ! a typeno=T		Add a to current listcomp
	(j_nextiter,	$), ! a	typeno=T		Add a to current listcomp

!Expressions and Operators

!Logical Operators

	(j_andl,		$), ! a b	This group are for conditional expressions (no result)
	(j_orl,			$), ! a b
	(j_xorl,		$), ! a b
	(j_notl,		$), ! a
	(j_istruel,		$), ! a

!Expressions and Operators

	(j_makelist,	$), ! a=L [lower=Lwb]
	(j_makeconstr,	$), ! a=L	Like makelist, but elements intended for a construction (avoid const reduction)
	(j_makesetlist,	$), ! a=L
	(j_makerange,	$), ! a b		Range
	(j_makedict,	$), ! a L	
	(j_exprlist,	$), ! a=u...	List of expressions, as (a;b;c), rather than (a,b,c)
	(j_multexpr,	$), !

	(j_keyword,		$), ! def=nameptr a
	(j_keyvalue,	$), ! a b
	(j_assignx,		$), ! a b
	(j_deepcopyx,	$), ! a b
	(j_callfn,		$), ! a b
	(j_callmfn,		$), ! a b
	(j_ifx,			$), ! a b c
	(j_selectx,		$), ! a L c
	(j_callhostfn,	$), ! a L c
	(j_applyop,		$), ! opcode b c
	(j_applyopx,	$), ! opcode b c

!Binary Ops

	(j_andand,		$), ! a b

	(j_eq,			$), ! a b		int
	(j_ne,			$), ! a b
	(j_lt,			$), ! a b
	(j_le,			$), ! a b
	(j_gt,			$), ! a b
	(j_ge,			$), ! a b

	(j_isequal,		$), ! a b		int

	(j_add,			$), ! a b
	(j_sub,			$), ! a b
	(j_mul,			$), ! a b
	(j_div,			$), ! a b
	(j_idiv,		$), ! a b
	(j_fdiv,		$), ! a b
	(j_ddiv,		$), ! a b
	(j_rem,			$), ! a b
	(j_divrem,		$), ! a b
	(j_iand,		$), ! a b
	(j_ior,			$), ! a b
	(j_ixor,		$), ! a b
	(j_shl,			$), ! a b		int?
	(j_shr,			$), ! a b
	(j_in,			$), ! a b		int
	(j_notin,		$), ! a b		int
	(j_inrev,		$), ! a b
	(j_min,			$), ! a b
	(j_max,			$), ! a b
	(j_addptr,		$), ! a b
	(j_subptr,		$), ! a b
	(j_concat,		$), ! a b
	(j_append,		$), ! a b
	(j_clamp,		$), ! a b

	(j_index,		$), ! a b		a[b]
	(j_indexref,	$), ! a b		a[b]
	(j_slice,		$), ! a b		a[b] (b is a range)
	(j_keyindex,	$), ! a b		a{b}

	(j_dotindex,	$), ! a b		a.[b]
	(j_dotleft,		$), ! a b		a.[b:]
	(j_dotright,	$), ! a b		a.[:b]
	(j_dotslice,	$), ! a b		a.[b] (b is a range)
	(j_dotkeyindex,	$), ! a b		a.{b}
	(j_anddotindex,	$), ! a b		a&.[b]
	(j_anddotslice,	$), ! a b		a&.[b] (b is a range)
	(j_byteindex,	$), ! a b		a.c[b] (c is a type)

	(j_dot,			$), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotref,		$), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotattr,		$), ! a b		a.&b
	(j_atan2,		$), ! a b	atan2(a,b)			real
	(j_power,		$), ! a b	a**b				int/real

	(j_ptr,			$), ! a		a^
	(j_ptrto,		$), ! a		^a
	(j_addrof,		$), ! a		&a
	(j_convert,		$), ! typeno=T a		T(a)			T
	(j_typepun,		$), ! typeno=T a		T@(a)			T
	(j_typeconst,	$), ! typeno=T			typeconst(T)
	(j_operator,	$), ! opcode=opc
	(j_packtypeconst,
					$), ! typeno=T		typeconst(T)
	(j_classconst,	$), ! typeno=T	dummy unit used in rx-processing
	(j_upper,		$), ! a		$					T

!Monadic Ops

	(j_neg,			$), ! a		-a
	(j_abs,			$), ! a		abs a
	(j_inot,		$), ! a
	(j_chr,			$), ! a
	(j_asc,			$), ! a

	(j_sqrt,		$), ! a
	(j_sqr,			$), ! a
	(j_cube,		$), ! a
	(j_sign,		$), ! a
	(j_sin,			$), ! a
	(j_cos,			$), ! a
	(j_tan,			$), ! a
	(j_asin,		$), ! a
	(j_acos,		$), ! a
	(j_atan,		$), ! a
	(j_ln,			$), ! a
	(j_lg,			$), ! a
	(j_log,			$), ! a
	(j_exp,			$), ! a
	(j_round,		$), ! a
	(j_floor,		$), ! a
	(j_ceil,		$), ! a
	(j_fract,		$), ! a
	(j_fmod,		$), ! a

	(j_lwb,			$), ! a		a.lwb				int
	(j_upb,			$), ! a							int
	(j_len,			$), ! a							int
	(j_bounds,		$), ! a							Range
	(j_bitwidth,	$), ! a
	(j_bytesize,	$), ! a
	(j_dictitems,	$), ! a

	(j_gettype,		$), ! a
	(j_getbasetype,	$), ! a
	(j_getelemtype,	$), ! a
	(j_isvoid,		$), ! a
	(j_isnone,		$), ! a
	(j_isdef,		$), ! a
	(j_isint,		$), ! a
	(j_isreal,		$), ! a
	(j_isstring,	$), ! a
	(j_isrange,		$), ! a
	(j_islist,		$), ! a
	(j_isrecord,	$), ! a
	(j_isclass,		$), ! a
	(j_isarray,		$), ! a
	(j_isset,		$), ! a
	(j_istype,		$), ! a
	(j_ispointer,	$), ! a
	(j_ismutable,	$), ! a

	(j_minvalue,	$), ! a
	(j_maxvalue,	$), ! a
	(j_min1,		$), ! a
	(j_max1,		$), ! a

!Increment

	(j_preincrx,	$), ! a	++a
	(j_predecrx,	$), ! a	--a
	(j_postincrx,	$), ! a	a++
	(j_postdecrx,	$), ! a	a--

!In-place operators

	(j_addto,		$), ! a b	a+:=b
	(j_subto,		$), ! a b
	(j_multo,		$), ! a b
	(j_divto,		$), ! a b
	(j_idivto,		$), ! a b
	(j_fdivto,		$), ! a b
	(j_iandto,		$), ! a b
	(j_iorto,		$), ! a b
	(j_ixorto,		$), ! a b
	(j_shlto,		$), ! a b
	(j_shrto,		$), ! a b
	(j_minto,		$), ! a b
	(j_maxto,		$), ! a b
	(j_concatto,	$), ! a b
	(j_appendto,	$), ! a b

	(j_negto,		$), ! a		-:=a
	(j_absto,		$), ! a
	(j_inotto,		$), ! a

	(j_preincr,		$), ! a	++a
	(j_predecr,		$), ! a	--a
	(j_postincr,	$), ! a	a++
	(j_postdecr,	$), ! a	a--

!Translator Variables

	(j_cvlineno,	$), ! 
	(j_cvstrlineno,	$), ! 
	(j_cvmodulename,$), ! 
	(j_cvfilename,	$), ! 
	(j_cvfunction,	$), ! 
	(j_cvdate,		$), ! 
	(j_cvtime,		$), ! 
	(j_cvversion,	$), ! 
	(j_cvpclversion,$), ! 

!Misc opsyms for overload/applyop 
	(j_new,			$),	!
	(j_mixed,		$),	!
	(j_tostr,		$),	!
	(j_free,		$),	!
	(j_dupl,		$),	!

	(j_dummy,		$)
end


global tabledata() [0:]ichar scopenames=
	(noscope=0,		$), ! 
	(localscope,	$), ! 
	(importscope,	$), ! 
	(exportscope,	$)
end

!Foreign function Specifiers
global tabledata() [0:]ichar fflangnames=

	(windowsff,	"WL"), ! 
	(clangff,	"CL"), ! 
	(qlangff,	"QL"), ! 
	(mlangff,	"ML"), ! 

	(dummyff,	$) ! 
end

global const linuxff=clangff

global tabledata() []ichar symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$),		! Lex error
	(dotsym,			$),		! "."
	(lexdotsym,			$),		! ".", used at bol to prefix lexical 
	(anddotsym,			$),		! "&."
	(commasym,			$),		! ","
	(semisym,			$),		! ";"
	(colonsym,			$),		! ":"
	(dcolonsym,			$),		! "::"
	(assignsym,			$),		! :=
	(deepcopysym,		$),		! :== or ::= ?
	(sendtosym,			$),		! =>
	(lbracksym,			$),		! (
	(rbracksym,			$),		! )
	(lsqsym,			$),		! [
	(rsqsym,			$),		! ]
	(lcurlysym,			$),		! {
	(rcurlysym,			$),		! }
	(ptrsym,			$),		! ^
	(barsym,			$),		! |
	(dbarsym,			$),		! ||
	(atsym,				$),		! @
	(datsym,			$),		! @@
	(questionsym,		$),		! ?
	(addrsym,			$),		! &
	(daddrsym,			$),		! &&
	(poundsym,			$),		! Œ Hmm, should be Pound A+156
	(curlsym,			$),		! ~
	(gatesym,			$),		! ª
	(rangesym,			$),		! ..
	(ellipsissym,		$),		! ...
	(opsym,				$),		! Any operator or property tag (use sets to distinguish)
	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen
	(rawnamesym,		$),		! unassigned name before lookup
	(docstringsym,		$),		! ! #comment used as documentation string
	(incrsym,			$),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$),		! 123 32 bits signed
	(longintconstsym,	$),		! 123 longint (always signed)
	(realconstsym,		$),		! 123.4 64 bits
	(charconstsym,		$),		! 'A' or 'ABCD'
	(wcharconstsym,		$),		! 'A'W or 'ABCD'W (but don't have a syntax yet)
	(stringconstsym,	$),		! "ABC"
	(wstringconstsym,	$),		! "ABC"W

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$),		! 
	(namesym,			$),		! identifier symbol
	(ksourcedirsym,		$),		! 
	(lexmacronamesym,	$),		! 

	(stdtypesym,		$),		! INT, CHAR etc
	(packtypesym,		$),		! Byte etc
	(kifsym,			$),		! 
	(kthensym,			$),		! 
	(kelsifsym,			$),		! 
	(kelsesym,			$),		! 
	(kelsecasesym,		$),		! 
	(kelseswitchsym,	$),		! 
	(kelseselectsym,	$),		! 
	(kendsym,			$),		! 
	(kunlesssym,		$),		! 
	(kcasesym,			$),		! CASE
	(kdocasesym,		$),		! DOCASE
	(kwhensym,			$),		! 
	(kforsym,			$),		! 
	(kforallsym,		$),		! FORALL
	(ktosym,			$),		! TO/DOWNTO
	(kbysym,			$),		! 
	(kdosym,			$),		! 
	(kwhilesym,			$),		! 
	(krepeatsym,		$),		! 
	(kuntilsym,			$),		! 
	(kreturnsym,		$),		! 
	(kstopsym,			$),		! 
	(kloopsym,			$),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kbreaksym,			$),		! BREAK
	(kgotosym,			$),		! GO/GOTO
	(kswitchsym,		$),		! SWITCH
	(kdoswitchsym,		$),		! DOSWITCH
	(kprintsym,			$),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		$),		! SPRINT/SFPRINT
	(kreadsym,			$),		! READ/READLN
	(ksreadsym,			$),		! SREAD
	(ksreadlnsym,		$),		! SREADLN
	(khostfnsym,		$),		! LEFT, CONVLC etc
	(kprocsym,			$),		! PROC
	(kfunctionsym,		$),		! FUNCTION
	(kmethodsym,		$),		! METHOD
	(krecordsym,		$),		! RECORD
	(kstructsym,		$),		! STRUCT
	(kunionsym,		$),		! UNION
	(kimportsym,		$),		! IMPORT
	(kimportmodulesym,	$),		! IMPORTDLL/IMPORTMODULE
	(kimportpathsym,	$),		! IMPORTPATH
	(kmodulesym,		$),		! 
	(kapplprocsym,		$),		! HOSTPROC
	(kapplsym,			$),		! HOST
	(ktypesym,			$),		! TYPE
	(ktypeattrsym,		$),		! COMPACT/DERIVED
	(krefsym,			$),		! REF
	(kmacrosym,			$),		! MACRO/DEFINE
	(kconstsym,			$),		! 
	(kvarsym,			$),		! 
	(klocalssym,		$),		! LOCALS
	(klabelsym,			$),		! 
	(kenumsym,			$),		! 
	(knewsym,			$),		! NEW/HEAP
	(kclasssym,			$),		! CLASS
	(kdoblocksym,		$),		! DOBLOCK
	(kblockdefsym,		$),		! BLOCKDEF
	(kdirectivesym,		$),		! TARGET/MODULE
	(kfflangsym,		$),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		$),		! global
	(kstaticsym,		$),		! STATIC

	(kbeginsym,			$),		! 
	(ktrysym,			$),		! 
	(kexceptsym,		$),		! 
	(kfinallysym,		$),		! 
	(kraisesym,			$),		! 
	(kyieldsym,			$),		! 
	(kextendsym,		$),		!
	(kblocksym,			$),		!
	(kcastsym,			$),		! CAST
	(ktypeconstsym,		$),		! TYPECONST
	(compilervarsym,	$),		! $lineno etc
	(dollarsym,			$),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$),		! EVAL
	(ktabledatasym,		$),		! tabledata
	(kmapsym,			$),		! MAP/MAPL/MAPR/MAPLR
	(kapplyopsym,		$),		! APPLYOP
	(kstacksym,			$),		! STACK/UNSTACK
	(kforwardsym,		$),		! FORWARD
	(kclampsym,			$),			! CLAMP
	(kswapsym,			$),		! SWAP
	(kcondcompsym,		$),		! $WHEN
	(kerrorsym,			$),		! PC_ERROR etc
	(sysconstsym,		$),		! nil, etc
	(kdummysym,			$)		!
end

global tabledata() []ichar sourcedirnames =
	(definedir,	$),
	(emitdir,	$),
	(ifdir,		$),
	(elsifdir,	$),
	(elsedir,	$),
	(endifdir,	$),
	(debuglinedir,	$),
	(includedir,	$),
	(endincludedir,	$),
	(exportdir,	$),
	(endexportdir,	$),
	(commentdir,	$),
	(endcommentdir,	$),
	(strincludedir,	$),

	(modulenamedir,	$),	!these codes are used for parser-level directives
	(targetlangdir,	$),
	(cincludedir,	$),
	(pyimportdir,	$),
!(commentdir,	$),
	(enddir,	$)
end

global tabledata() =
	(nil_const),
	(pi_const),
	(tab_const),
	(con_const)
end

global tabledata() =
	(thousand_unit),
	(million_unit),
	(billion_unit),
	(kilo_unit),
	(mega_unit),
	(giga_unit)

end

global tabledata []ichar stnames, []int stsymbols, []int stsubcodes=

	("if",			kifsym,			j_if),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		j_if),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	j_case),
	("elseswitch",	kelseswitchsym,	j_switch),
	("case",		kcasesym,		j_case),
	("docase",		kdocasesym,		j_docase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("forall",		kforallsym,		j_forall),
	("foreach",		kforallsym,		j_foreach),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		1),
	("return",		kreturnsym,		0),
	("yield",		kyieldsym,		0),
	("stop",		kstopsym,		0),
	("restart",		kloopsym,		j_restart),
	("redo",		kloopsym,		j_redo),
	("loop",		kloopsym,		j_next),
	("next",		kloopsym,		j_next),
	("exit",		kloopsym,		j_exit),
	("break",		kbreaksym,		j_break),
	("goto",		kgotosym,		0),
	("go",			kgotosym,		1),
	("switch",		kswitchsym,		j_switch),
	("doswitch",	kdoswitchsym,	j_doswitch),
	("tabledata",	ktabledatasym,	0),
!	("map",			kmapsym,		0),
!	("mapl",		kmapsym,		1),
!	("mapr",		kmapsym,		2),
!	("maplr",		kmapsym,		3),
	("applyop",		kapplyopsym,	0),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
	("$windows",	kcondcompsym,	windowsff),
	("$linux",		kcondcompsym,	linuxff),

	("print",		kprintsym,		j_print),
	("println",		kprintsym,		j_println),
	("fprint",		kprintsym,		j_fprint),
	("fprintln",	kprintsym,		j_fprintln),
	("cprint",		kprintsym,		j_cprint),
	("cprintln",	kprintsym,		j_cprintln),
	("sprint",		ksprintsym,		j_sprint),
	("sfprint",		ksprintsym,		j_sfprint),
	("scprint",		ksprintsym,		j_scprint),

	("cp",			kprintsym,		j_print),
	("cpl",			kprintsym,		j_println),

	("read",		kreadsym,		j_read),
	("readln",		kreadsym,		j_readln),
	("cast",		kcastsym,		j_convert),
	("typeconst",	ktypeconstsym,	j_typeconst),

	("proc",		kprocsym,		0),
	("function",	kfunctionsym,	0),
	("method",		kmethodsym,		0),

! ("operator",	koperatorsym,		0),
	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("doblock",		kdoblocksym,	0),
	("blockdef",	kblockdefsym,	0),

	("record",		krecordsym,		trecord),
	("struct",		kstructsym,		tstruct),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),

!	("targetlang",	kdirectivesym,	targetlangdir),
	("module",		kdirectivesym,	modulenamedir),
	("cinclude",	kdirectivesym,	cincludedir),
	("pyimport",	kdirectivesym,	pyimportdir),

	("include",		ksourcedirsym,	includedir),
!	("endinclude",	ksourcedirsym,	endincludedir),
	("strinclude",	ksourcedirsym,	strincludedir),
	("define",		ksourcedirsym,	definedir),
	("macro",		ksourcedirsym,	definedir),
!	("emit",		ksourcedirsym,	emitdir),
	("export",		ksourcedirsym,	exportdir),
	("endexport",	ksourcedirsym,	endexportdir),
!	("comment",		ksourcedirsym,	commentdir),
!	("endcomment",	ksourcedirsym,	endcommentdir),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),
	("var",			kvarsym,		0),
	("variant",		kvarsym,		0),
	("enum",		kenumsym,		0),

	("importdll",	kimportmodulesym,	0),
!	("importlib",	kimportmodulesym,	0),
!	("module",		kmodulesym,		0),
	("import",		kimportsym,		0),
	("importpath",	kimportpathsym,	0),
	("hostproc",	kapplprocsym,	0),
	("applproc",	kapplprocsym,	0),
	("host",		kapplsym,		0),
	("appl",		kapplsym,		0),

	("begin",		kbeginsym,		0),
	("unless",		kunlesssym,		0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("finally",		kfinallysym,	0),
	("raise",		kraisesym,		0),

	("global",		kglobalsym,		0),

	("qlang",		kfflangsym,		qlangff),
	("clang",		kfflangsym,		clangff),
	("mlang",		kfflangsym,		mlangff),
	("windows",		kfflangsym,		windowsff),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),
	("word",		stdtypesym,		tword),
	("real",		stdtypesym,		treal),

	("refvar",		stdtypesym,		trefvar),
	("pointer",		stdtypesym,		trefvar),
	("range",		stdtypesym,		trange),
	("longint",		stdtypesym,		tlongint),
	("string",		stdtypesym,		tstring),
	("set",			stdtypesym,		tset),
	("list",		stdtypesym,		tlist),
	("dict",		stdtypesym,		tdict),

	("array",		stdtypesym,		tarray),
	("bits",		stdtypesym,		tbits),
	("recordtype",	stdtypesym,		trecord),
	("structtype",	stdtypesym,		tstruct),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),
	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),
	("sreal",		stdtypesym,		tr32),

	("bit",			stdtypesym,		tbit),
	("bit2",		stdtypesym,		tbit2),
	("bit4",		stdtypesym,		tbit4),
	("byte",		stdtypesym,		tu8),
!	("u1",			stdtypesym,		tbit),
!	("u2",			stdtypesym,		tu2),
!	("u4",			stdtypesym,		tu4),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),

	("stringz",		stdtypesym,		tstringz),

	("intm",		stdtypesym,		tintm),
	("wordm",		stdtypesym,		twordm),
	("refm",		stdtypesym,		trefm),
!!	("refx",		stdtypesym,		trefx),
!	("handle",		stdtypesym,		thandle),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
	("thousand",	unitnamesym,	thousand_unit),
	("kb",			unitnamesym,	kilo_unit),
	("mb",			unitnamesym,	mega_unit),
	("gb",			unitnamesym,	giga_unit),


	("$lineno",		compilervarsym,	j_cvlineno),
	("$strlineno",	compilervarsym,	j_cvstrlineno),
	("$filename",	compilervarsym,	j_cvfilename),
	("$modulename",	compilervarsym,	j_cvmodulename),
	("$function",	compilervarsym,	j_cvfunction),
	("$date",		compilervarsym,	j_cvdate),
	("$time",		compilervarsym,	j_cvtime),
	("$version",	compilervarsym,	j_cvversion),
	("$pclversion",	compilervarsym,	j_cvpclversion),
	("$",			dollarsym,		0),

	("and",			opsym,			j_andl),
	("or",			opsym,			j_orl),
	("xor",			opsym,			j_xorl),
	("iand",		opsym,			j_iand),
	("ior",			opsym,			j_ior),
	("ixor",		opsym,			j_ixor),
	("in",			opsym,			j_in),
	("notin",		opsym,			j_notin),
	("inrev",		opsym,			j_inrev),
	("rem",			opsym,			j_rem),
	("divrem",		opsym,			j_divrem),
	("min",			opsym,			j_min),
	("max",			opsym,			j_max),

	("not",			opsym,			j_notl),
	("inot",		opsym,			j_inot),
	("istrue",		opsym,			j_istruel),
	("abs",			opsym,			j_abs),
	("$neg",		opsym,			j_neg),
	("asc",			opsym,			j_asc),
	("chr",			opsym,			j_chr),
	("sqrt",		opsym,			j_sqrt),
	("sqr",			opsym,			j_sqr),
	("cube",		opsym,			j_cube),
	("cos",			opsym,			j_cos),
	("sin",			opsym,			j_sin),
	("tan",			opsym,			j_tan),
	("asin",		opsym,			j_asin),
	("acos",		opsym,			j_acos),
	("atan",		opsym,			j_atan),
	("atan2",		opsym,			j_atan2),
	("sign",		opsym,			j_sign),
	("ln",			opsym,			j_ln),
	("log",			opsym,			j_log),
	("lg",			opsym,			j_lg),
	("exp",			opsym,			j_exp),
	("round",		opsym,			j_round),
	("floor",		opsym,			j_floor),
	("ceil",		opsym,			j_ceil),
	("fract",		opsym,			j_fract),
	("fmod",		opsym,			j_fmod),

	("len",			opsym,			j_len),
	("lwb",			opsym,			j_lwb),
	("upb",			opsym,			j_upb),
	("bounds",		opsym,			j_bounds),
	("bitwidth",	opsym,			j_bitwidth),
	("bytes",		opsym,			j_bytesize),
	("basetype",	opsym,			j_getbasetype),
	("dictitems",	opsym,			j_dictitems),
	("elemtype",	opsym,			j_getelemtype),
	("defined",		opsym,			j_isdef),
	("isdef",		opsym,			j_isdef),
	("isvoid",		opsym,			j_isvoid),
	("isnone",		opsym,			j_isnone),
	("isint",		opsym,			j_isint),
	("isreal",		opsym,			j_isreal),
	("isarray",		opsym,			j_isarray),
	("isset",		opsym,			j_isset),
	("islist",		opsym,			j_isarray),
	("isrecord",	opsym,			j_isrecord),
	("isrange",		opsym,			j_isrange),
	("isstring",	opsym,			j_isstring),
	("ispointer",	opsym,			j_ispointer),
	("ismutable",	opsym,			j_ismutable),
	("minvalue",	opsym,			j_minvalue),
	("maxvalue",	opsym,			j_maxvalue),

	("concat",		opsym,			j_concat),
	("append",		opsym,			j_append),

	("$free",		opsym,			j_free),
	("$dupl",		opsym,			j_dupl),
	("$mixed",		opsym,			j_mixed),
	("$index",		opsym,			j_index),
!	("$indexref",	opsym,			j_indexref),
	("$dotindex",	opsym,			j_dotindex),
!	("$dotindexref",opsym,			j_dotindexref),
	("$convert",	opsym,			j_convert),
	("$new",		opsym,			j_new),
	("$tostr",		opsym,			j_tostr),
	("$dot",		opsym,			j_dot),
	("$dotref",		opsym,			j_dotref),

	("endif",		kendsym,	kifsym),
	("fi",			kendsym,	kifsym),
	("endcase",		kendsym,	kcasesym),
	("esac",		kendsym,	kcasesym),
	("enddocase",	kendsym,	kdocasesym),
	("endswitch",	kendsym,	kswitchsym),
	("enddoswitch",	kendsym,	kdoswitchsym),
	("endfor",		kendsym,	kforsym),
	("endforall",	kendsym,	kforallsym),
	("od",			kendsym,	kdosym),
	("endproc",		kendsym,	kprocsym),
	("endfunction",	kendsym,	kfunctionsym),
	("endmethod",	kendsym,	kmethodsym),
	("endwhile",	kendsym,	kwhilesym),
	("endto",		kendsym,	ktosym),
	("enddo",		kendsym,	kdosym),
	("endunless",	kendsym,	kunlesssym),
	("endmodule",	kendsym,	kmodulesym),
	("endimportmodule",	kendsym,kimportmodulesym),
	("endtry",		kendsym,	ktrysym),
	("endrecord",	kendsym,	krecordsym),
	("endclass",	kendsym,	kclasssym),
	("endblock",	kendsym,	kblocksym),

	("nil",			sysconstsym,	nil_const),
	("con",			sysconstsym,	con_const),
	("tab",			sysconstsym,	tab_const),
	("pi",			sysconstsym,	pi_const),
!
	("sreadln",			khostfnsym,		host_sreadln),
	("sread",			khostfnsym,		host_sread),
	("rereadln",		khostfnsym,		host_rereadln),
	("reread",			khostfnsym,		host_reread),

	("strtoval",		khostfnsym,		host_strtoval),
	("tostr",			khostfnsym,		host_tostr),

	("leftstr",			khostfnsym,		host_leftstr),
	("rightstr",		khostfnsym,		host_rightstr),
	("convlc",			khostfnsym,		host_convlc),
	("convuc",			khostfnsym,		host_convuc),
	("iconvlc",			khostfnsym,		host_iconvlc),
	("iconvuc",			khostfnsym,		host_iconvuc),

	("ismain",			khostfnsym,		host_ismain),
	("waitkey",			khostfnsym,		host_waitkey),
	("testkey",			khostfnsym,		host_testkey),
	("execwait",		khostfnsym,		host_execwait),
	("execcmd",			khostfnsym,		host_execcmd),
	("shellexec",		khostfnsym,		host_shellexec),
	("system",			khostfnsym,		host_system),

	("makestr",			khostfnsym,		host_makestr),
	("makestrslice",	khostfnsym,		host_makestrslice),
	("makeref",			khostfnsym,		host_makeref),

	("new",				khostfnsym,		host_new),
	("newheap",			khostfnsym,		host_newheap),
	("readlines",		khostfnsym,		host_readlines),
	("heapvar",			khostfnsym,		host_heapvar),
	("freeheap",		khostfnsym,		host_freeheap),
!	("resize",			khostfnsym,		host_resize),
!	("delete",			khostfnsym,		host_delete),

	("getcmdparam",		khostfnsym,		host_getcmdparam),
	("gethostname",		khostfnsym,		host_gethostname),

	("$setpcerror",		khostfnsym,		host_setpcerror),
	("$setdebug",		khostfnsym,		host_setdebug),
	("$test",			khostfnsym,		host_test),

	("ticks",			khostfnsym,		host_ticks),
	("sleep",			khostfnsym,		host_sleep),
	("random",			khostfnsym,		host_random),
	("findmetafunction",khostfnsym,		host_findmetafunction),
	("gethash",			khostfnsym,		host_gethash),
	("getos",			khostfnsym,		host_getos),
	("gethostsize",		khostfnsym,		host_gethostsize),
	("iswindows",		khostfnsym,		host_iswindows),
	("setmesshandler",	khostfnsym,		host_setmesshandler),
	("$setfprintf",		khostfnsym,		host_setfprintf),
	("clearlist",		khostfnsym,		host_clearlist),

	("loadpcl",			khostfnsym,		host_loadpcl),
	("runpcl",			khostfnsym,		host_runpcl),
	("runtask",			khostfnsym,		host_runtask),
	("callext",			khostfnsym,		host_callext),
	("$pcldata",		khostfnsym,		host_pcldata),
	("getcstring",		khostfnsym,		host_getcstring),
	("$getparam",		khostfnsym,		host_getparam),
	("makelink",		khostfnsym,		host_makelink),
	("allparams",		khostfnsym,		host_allparams),
	("stackvars",		khostfnsym,		host_stackvars),
	("makeempty",		khostfnsym,		host_makeempty),
	("$errorinfo",		khostfnsym,		host_errorinfo),
	("$setoverload",	khostfnsym,		host_setoverload),

	("pc_error",		kerrorsym,		pc_error),
	("user_error",		kerrorsym,		user_error),
	("type_error",		kerrorsym,		type_error),
	("mixedtype_error",	kerrorsym,		mixedtype_error),
	("divide_error",	kerrorsym,		divide_error),
	("stopmodule_error",kerrorsym,		stopmodule_error),
	("bounds_error",	kerrorsym,		bounds_error)

!	("$$dummy",		0,				0)
end

!this pair of lists associates each of a set of ops, with a priority
global tabledata []int oplist,[]int oppriolist =
	(j_add,			4),
	(j_sub,			4),
	(j_mul,			3),
	(j_div,			3),
	(j_idiv,		3),
	(j_rem,			3),
	(j_divrem,		3),
	!(j_ddiv,		3),
	(j_andl,		7),
	(j_orl,			8),
	(j_xorl,		6),
	(j_iand,		4),
	(j_ior,			4),
	(j_ixor,		4),
	(j_shl,			3),
	(j_shr,			3),
	!(j_rol,		3),
	!(j_ror,		3),
	(j_in,			6),
	(j_notin,		6),
	(j_inrev,		6),
	(j_eq,			6),
	(j_ne,			6),
	(j_lt,			6),
	(j_ge,			6),
	(j_le,			6),
	(j_gt,			6),
	(j_isequal,		6),
	!(j_testeq,		6),
	(j_min,			4),
	(j_max,			4),
	(j_power,		2),
	(j_atan2,		3),
	(j_addptr,		4),
	(j_subptr,		4),
	(j_concat,		4),
	(j_append,		4),
	(j_assignx,		1),
	(j_deepcopyx,	1),
	(j_makerange,	5)
end

global [0:jtagnames.len]byte jtagpriotable		!set up from the above
!
!symbols that can be starters for an expression (that can return a value)
global []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,opsym,namesym,
	incrsym,intconstsym,longintconstsym,realconstsym,charconstsym,stringconstsym,stdtypesym,
	ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
	khostfnsym,kapplyopsym,kerrorsym,kapplsym)

global []int D_typestarterset= (stdtypesym,lsqsym,kvarsym,krefsym,kenumsym,krecordsym)

global [0..host_last]byte hostlvset

global [0..jtagnames.upb]byte condopset			!contains 1 for j_eq/ne/lt/le/gt/ge

=== qci.m 27/38 ===
import msys
import mlib
import clib
import oslib
!import osdll

import var_types
import var_decls
import qc_support
import qc_lex
import qc_tables
import qc_parse
import qc_lib
import qc_pclgen
import qc_pcllib
import pq_common

import q_libs

import qc_name

!var byte fshowst
!var byte fshowstflat
!var byte fshowtypes
!!var byte fshowfields
!
!var byte fshowtiming
!var byte fdocs
!
!var byte fshowast1
!var byte fshowast2
!var byte fshowpcl
!
!tabledata() []ichar optionnames=
!	(time_sw,		"time"),
!	(v_sw,			"v"),
!	(v2_sw,			"v2"),
!	(help_sw,		"h"),
!	(help2_sw,		"help"),
!	(docs_sw,		"docs"),
!	(ext_sw,		"ext"),
!	(compile_sw,	"c"),
!end
!
!const logfile="bx.log"
!
!var ichar infile
!
!var int clockstart
!
int totalpclopcodes=0

int totallines=0
int nstringobjects=0

!var int modulelevel
!var ichar outfile

!global proc qcstart=
global function qcompiler_prod(ichar locinfile,locoutfile,int intlibs, fdocs=0)int=
ichar ext, infile, outfile

println "Q Compiler [Production/Integrated Version 6]"

initdata()

!getinputoptions()
inputfiles[1]:=infile:=locinfile
ninputfiles:=1
outfile:=locoutfile

!CPL =INFILE
!CPL =OUTFILE

ext:=extractext(infile)

convlcstring(ext)

if not eqstring(ext,"q") then
	loaderror("Unknown file extension:",infile)
fi

dointlibs:=intlibs

println "Compiling",infile,"to",outfile
!CPL =DOINTLIBS

do_loadmodules(infile)
do_parse()
do_writeqa(outfile)
do_name()
do_pclgen()
do_writepcfile()

if fdocs then
	writedocs()
fi
return 1
end

global proc do_loadmodules(ichar infile)=
!CPL "LOADMODULES",NINPUTFILES,INFILE

if fverbose then
	CPL "Loading:",infile
fi

loadmainmodule(infile)
end

function loadmainmodule(ichar filespec)int=
!Used for main module. Will always be first module loaded, module list
!will be empty.
!Load file as string
!extract modulename
!call compilemodile(modulename, filename, source)
	[100]char modulename
	[300]char path
	ref char source
	int status
	modulerec m
	int i,flag

!set up special module to represent the whole program
	pcm_clearmem(&moduletable[0],modulerec.bytes)
	moduletable[0].name:="PROGRAM"
	moduletable[0].filename:="<->"
	moduletable[0].sourcecode:="<program>"
	moduletable[0].sourcelen:=strlen(moduletable[0].sourcecode)

	stprogram:=getduplnameptr(nil,addnamestr("$prog"),programid)
	moduletable[0].stmodule:=stprogram

	source:=cast(readfile(filespec))
	if source=nil then
		loaderror("Can't load main file:",filespec)
		return 0
	fi

	strcpy(&.modulename,extractbasefile(filespec))
	strcpy(&.path,extractpath(filespec))
	if path[1] then
		++nsearchdirs
		for i:=nsearchdirs downto 2 do
			searchdirs[i]:=searchdirs[i-1]
		od
		searchdirs[1]:=ref char(pcm_copyheapstring(&.path))
	fi

	addmodule(&.modulename,filespec,source,rfsize,moduleid,flag)

return 1
end

function addmodule(ichar modulename,filespec, source, int length,id,&exportflag)int=
!Add new module with given name
!Source for module is already loaded in <source>, of given length
!return module no just added

modulerec m
const maximports=maxmodule
[maximports]ichar importnames
[0..maximports]byte importflags
[maximports]int importmoduleno
int nimports,i,status,k,flag,j,newmodno
ref modulerec pmodule

pcm_clearmem(&m,m.bytes)

m.name:=pcm_copyheapstring(modulename)

m.filename:=pcm_copyheapstring(filespec)
m.sourcecode:=source
m.sourcelen:=length
if fwriteqa then
	addqafile(m.filename,m.sourcecode, m.sourcelen)
!	m.qafilestr:=pcm_copyheapstring(source)
fi

stmodule:=getduplnameptr(stprogram,addnamestr(m.name),id)
adddef(stprogram,stmodule)
m.stmodule:=stmodule

if nmodules>=maxmodule then
	loaderror("Too many modules",modulename)
fi

pmodule:=&moduletable[newmodno:=++nmodules]

pmodule^:=m
pmodule^.importmap[newmodno]:=1
m.stmodule^.attribs.ax_moduleno:=newmodno

memset(&importflags,0,importflags.bytes)

nimports:=readimportlist(&m,&importnames,&importflags,maximports)

for i to nimports do

	flag:=0
	if fverbose=2 then
		cpl "Load import for",modulename
	fi
	k:=loadimport(importnames[i],flag,modulename)
	if flag then
		importflags[i]:=1
	fi
	pmodule^.importmap[k]:=1
	importmoduleno[i]:=k
od

!Deal with any "*" imports (or where export/endexport were used)
for i:=1 to nimports when importflags[i] do
	k:=importmoduleno[i]
	for j:=1 to nmodules do
		if moduletable[k].importmap[j] then		!add that to this module
			pmodule^.importmap[j]:=1
		fi
	od
od

exportflag:=importflags[0]

moduleinitorder[++ninitmodules]:=newmodno

return newmodno
end

function loadimport(ichar modulename,int &exportflag, ichar ownername)int=
!Look at request for adding module, which might already be loaded
!Checks also that modules are added in the right order
!Calls loadmodule to process the module and deal with /its/ imports
!return modulen no of the existing or newly added module

int i
ichar ifilespec
[300]char filespec
ref char source
ichar newname

newname:=modulename

for i:=1 to nmodules do
	if eqstring(moduletable[i].name,newname) then		!already loaded
		return i
	fi
od

source:=getmodulestr(modulename,&.filespec)

return addmodule(newname,&.filespec,source,rfsize,moduleid, exportflag)
end

function readimportlist(ref modulerec m, ref[]ichar importnames,
							ref[0:]byte importflags, int maximports)int=
int n,flag,exportflag
ichar s
[100]char name,libname

startlex("IMPORTS",m^.sourcecode)

exportflag:=0

n:=0
do
	lexreadtoken()
	case nextlx.symbol
	when eofsym then
		exit
	when semisym,eolsym then

	when rawnamesym then
		flag:=0
		if checkname("import") then
			lexreadtoken()
			if nextlx.symbol=opsym and nextlx.subcode=j_mul then
				flag:=1
				lexreadtoken()
			fi

			if nextlx.symbol<>rawnamesym then
				abortprogram("import: modulename expected")
			fi
			if ++n>maximports then
				abortprogram("too many imports")
			fi

			strcpy(&.name,convertzstring(nextlx.svalue,nextlx.length))
			importnames^[n]:=pcm_copyheapstring(&.name)
			importflags^[n]:=flag

!for now, accept export/endexport so that I can share B sources with older QX
		elsif checkname("importpath") then
			prescanmode:=1
			lexreadtoken()
			prescanmode:=0
			if nextlx.symbol=stringconstsym then
				strcpy(&.name,convertzstring(nextlx.svalue,nextlx.length))
				addsearchdir(&.name)
				lexreadtoken()
			else
				abortprogram("string path expected")
			fi
		elsif checkname("export") or checkname("endexport") then
			exportflag:=1
			next
		elsif checkname("$windows") then
			if os_iswindows() then		!proceed with rest of line
				next
			else
skipthisline::
				repeat
					lexreadtoken()
				until nextlx.symbol=eolsym or nextlx.symbol=eofsym
			fi
		elsif checkname("$linux") then
			if not os_iswindows() then		!proceed with rest of line
				next
			else
				goto skipthisline
			fi

		else
			exit
		fi
	else
		exit
	esac
od
importflags^[0]:=exportflag

return n
end

global proc initdata=
!CPL "ID1"
	pcm_init()
!CPL "ID2"
	lexsetup()
!CPL "ID3"
	inittypetables()
!CPL "ID4"
	initsearchdirs()
!CPL "ID5"
	initqclib()
!CPL "ID6"
	initpclgen()
!CPL "ID8"
	initpcldata()
end

global proc initsearchdirs=
[300]char str1,str2
int i

searchdirs[++nsearchdirs]:=""
strcpy(&.str1,os_gethostname())
if str1[1] then
	strcpy(&.str2,extractpath(&.str1))

	searchdirs[++nsearchdirs]:=ref char(pcm_copyheapstring(&.str2))
fi

strcpy(&.str1,os_getmpath())
if str1[1] then
	searchdirs[++nsearchdirs]:=ref char(pcm_copyheapstring(&.str1))
fi
searchdirs[++nsearchdirs]:=pcm_copyheapstring("c:/qcc/")

!CPL "SEARCH DIRS:"
!for i to nsearchdirs do
!	cpl i,,":",searchdirs[i]
!od
!OS_GETCH()
end

proc addsearchdir(ichar path)=
	for i to nsearchdirs do
		if eqstring(searchdirs[i],path) then return fi
	od
	searchdirs[++nsearchdirs]:=pcm_copyheapstring(path)
end

function getmodulestr(ichar modulename, filespec)ichar=
!locate module in file system, load it, and return string pointer to in-memory version
!for selected system libraries, can also locate the module from in-program string
!returns nil on error
ichar ifile
ref byte source

if dointlibs then		!internal libs are used first
	source:=cast(getintlib(modulename))
	if source then
!		CPL "FOUND INTERNAL LIB1:",modulename
		strcpy(filespec,"<Internal>")
		return cast(source)
	fi
fi

ifile:=findmodule(modulename)
if ifile=nil then
	if not dointlibs then
		source:=cast(getintlib(modulename))
		if source then
			if fverbose=2 then
!					CPL "FOUND INTERNAL LIB:",modulename
			fi
			strcpy(filespec,"<Internal>")
			return cast(source)
		fi
	fi
	loaderror("Can't locate import module:",modulename)
fi

strcpy(filespec,ifile)

source:=readfile(filespec)

if source=nil then				!unlikely as already checked above
	loaderror("?Read file error:",filespec)
fi
return cast(source)
end

function findmodule(ichar modulename)ichar=
!locate module within search paths
!return full filespec
[300]char file
static [300]char filespec
ichar s
int i

strcpy(&.file,modulename)
strcpy(&.file,addext(&.file,".q"))

if fverbose=2 then
	println "Locating:",&.file
fi
for i to nsearchdirs do
	strcpy(&.filespec,searchdirs[i])
	strcat(&.filespec,&.file)
	if fverbose=2 then
		println "	",,i,,": Checking File",&.filespec,"in <",,searchdirs[i],,">"
	fi
	if checkfile(&.filespec) then
		if fverbose=2 then
			println "	Found:",&.filespec
		fi
		return &.filespec
	fi
od

return nil
end

function checkname(ichar name,int length=0)int=
!nextlx contains a rawnamesym
!return if if it is the same as name
!length is the name length, or 0 (-1 better for empty strings) to work it out
!note that nextlx.svalue is not zero-terminated

if length=0 then
	length:=strlen(name)
fi
if nextlx.length=length and memcmp(nextlx.svalue,name,length)=0 then
	return 1
fi
return 0
end

global proc do_parse=
int m

for i:=1 to nmodules do
!	m:=moduleinitorder[i]
	m:=i

	currmoduleno:=m
	currmodule:=&moduletable[m]
	parsemodule(m)
od

fixusertypes()
end

global proc do_name=
	tx_typetable()

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
end

global proc do_pclgen =
int status
for i to nmodules do
	codegen(i)
od
end

global proc loaderror(ichar mess,mess2="")=
println "Load Error:",mess,mess2
println "Stopping"
stop
end

proc getsyscmdline=			!GETSYSCMDLINE
!get system args and store into local cmdparams of a task (usually the main
!one loaded)
int i

for i:=1 to nsysparams do
	if i<=maxcmdparam then
		cmdparamtable[i-1]:=pcm_copyheapstring(sysparams[i])
	fi
od
ncmdparams:=nsysparams-1
end

proc checkkeyword(ichar kwd)=
int length

lexreadtoken()

length:=strlen(kwd)

unless nextlx.symbol=rawnamesym and nextlx.length=length and memcmp(nextlx.svalue,kwd,length)=0 then
	loaderror("BA: expected:",kwd)
endunless

end

function readinttoken:int=
lexreadtoken()
if nextlx.symbol<>intconstsym then
	loaderror("Int expected")
fi
return nextlx.value
end

global proc do_writepcfile=
filehandle f
int i,length, symbolpos,currpos
modulerec m
[300]char filename
ichar file, s,t

strcpy(&.filename,moduletable[1].filename)
strcpy(&.filename,changeext(&.filename,".pc"))

initpcdest()

doprogramstartup()

!------------------------- sig and version
writezint('P')
writezint('C')
writezint(26)
writezint(0)

writezint(kkpclversion)
writezstring(pclversion)

!------------------------- modules
writezint(kkmoduletable)
writezint(nmodules)

for i:=1 to nmodules do
	writezstring(moduletable[i].name)
od

writezint(kkdlltable)
writezint(ndlltable)
!------------------------- dll modules
for i:=1 to ndlltable do
	writezstring(dlltable[i])
od

writezint(kkdllproctable)
writezint(ndllproctable)
!------------------------- dll functions
for i:=1 to ndllproctable do
	writezstring(dllproctable[i].name)
	writezint(dllproctable[i].dllindex)
od

writezint(kkapplproctable)
writezint(napplproctable)
!------------------------- host functions
for i:=1 to napplproctable do
	writezstring(applproctable[i].name)
od

!------------------------- symbols
writezint(kksymboltable)
symbolpos:=getpcpos()
writezint4(0)

for i:=1 to nmodules do
	showpcsymbol(moduletable[i].stmodule)
od

for i:=tlast to ntypes do
	showpcsymbol(ttnamedef[i])
od

for i:=0 to nmodules do
	writesymbols(i)
od

currpos:=getpcpos()
setpcpos(symbolpos)
writezint4(nsymbols)
setpcpos(currpos)

!------------------------- user types
writezint(kktypetable)
writezint(ntypes-tlast+1)
for i:=tlast to ntypes do
	writezint(i)
	writezstring(ttname[i])
	writezint(ttnamedef[i]^.bcindex)
	writezint(ttbasetype[i])
	writezint(tttarget[i])

	writezint(ttlower[i])
	writezint(ttlength[i])
	writezint(ttsize[i])
!	writezint(ttnallfields[i])
od
fixup_genfields()

writezint(kknewstringtable)
writezint(nstrings)
for i:=1 to nstrings do
	writezint(stringlentable^[i])
	writezblock(cast(stringtable^[i]),stringlentable^[i])
od

!------------------------- structs

writestructfields()

!------------------------- pccode
for i:=0 to nmodules do
	writepccode2pc(i)
od

!------------------------- genfield tables
writezint(kkgenfieldnames)
writezint(ngenfieldnames)
for i:=1 to ngenfieldnames do
	writezstring(genfieldnames[i].def^.name)
	writezint(genfieldnames[i].dataindex)
	writezint(genfieldnames[i].datalength)
od

writezint(kkgenfielddata)
writezint(ngenfielddata)
for i:=1 to ngenfielddata do
	writezint(genfielddata[i].fieldindex)
	writezint(genfielddata[i].recordtype)
	writezint(genfielddata[i].fieldtype)
	writezint(genfielddata[i].offset)
od

!------------------------- done
writezint(kkend)
writezeof()

length:=getpcpos()

writepcdata(&.filename)

if fverbose then
	println "Finished writing",&.filename,length,"bytes"
fi
end

proc writesymbols(int mx)=
!for .bc file output, scan pccode for module mx, and build string table
!also fixup pccode for output
ref intpc p,pccode
ref strec d
int cmd,i,index

pccode:=p:=cast(moduletable[mx].pccode)

do
	cmd:=p++^

	for i:=1 to cmdnopnds[cmd] do
		switch cmdfmt[cmd,i]
!		when clabel then
		when cproc then
!			d:=ref strec(p^)
			d:=ref strec(int(p^))
			showpcsymbol(d)
			p^:=d^.bcindex

		when cdllproc then
			d:=ref strec(int(p^))
			p^:=d^.index

		when cmemory then
			d:=ref strec(int(p^))
			showpcsymbol(d)
			p^:=d^.bcindex
		when cframe then
			d:=ref strec(int(p^))
			p^:=d^.index*varsize

!		when cint32 then
		when cint then
			if cmd=kcall and i=2 then
				p^:=p^*varsize		!var offsets to byte offsets
			elsif cmd=kcallptr and i=2 then
				p^:=p^*varsize
			elsif cmd=kaddsp and i=1 then
				p^:=p^*varsize
			fi
!		when cstring then
!			if nstrings>=maxpcstrings then
!				loaderror("pc: too many strings")
!			fi
!			p^:=addstringtotable(ichar(p^))
		endswitch
		++p
	od
	if cmd=kendmodule or cmd=0 then
		exit
	fi
od
end

proc showpcsymbol(ref strec d)=
	int a,b
	char c

	a:=b:=0

	if d^.bcindex=0 then
		d^.bcindex:=++nsymbols
		case d^.nameid
		when procid then
			c:='P'
			a:=d^.index
		when staticid then
			c:='S'
		when moduleid then
			c:='M'
			a:=d^.attribs.ax_moduleno

		when typeid then
			c:='T'
			a:=d^.mode
		when programid then
			return
		else
!CPL namenames[d^.nameid]
			loaderror("SHOWPCSYM?")
		esac

		writezint(c)
		writezstring(d^.name)
		writezint(d^.owner^.bcindex)
		writezint(a)
		writezint(b)

		if c='P' then
			if d^.metadata then
				writezstring(d^.metadata)
			else
				writezstring("")
			fi
		fi
	fi
end

proc writepccode2pc(int mx)=
!scan pccode for module mx, write to bc file
ref intpc p,pccode
ref[0:]word16 linetable
ref strec d
int cmd,i,index,pcindex,startindex

writezint(kkpccode)
writezint(mx)
writezint(moduletable[mx].pcindex)

pccode:=p:=cast(moduletable[mx].pccode)
linetable:=moduletable[mx].linetable
pcindex:=1

do
	cmd:=p++^

	writezint(linetable^[pcindex])
	writezint(cmd)

	startindex:=pcindex++

	for i:=1 to cmdnopnds[cmd] do
		switch cmdfmt[cmd,i]
		when clabel, cproc, cdllproc, cmemory, cframe,
!			 cint32,cint,cword, cstring, ctype, cgenfield, coperator then
			 cint,cword, cstring, ctype, cgenfield, coperator, capplproc then
			writezint(p^)
		when creal then
			writezreal(real@(p^))
		when crange then
			writezrange(cast(p))
		else
cpl opndnames[cmdfmt[cmd,i]]
			loaderror("writepc2bc/opnd?")
		endswitch
		++pcindex
		++p
	od

	if cmd=kendmodule or cmd=0 then
		exit
	fi
od

end

proc writestructfields=
int structpos,currpos
int nstructfields,i,j,t
ref strec d,e
const maxfields=100
[maxfields]ref strec fieldlist
[maxfields]byte ignore
int nfields

!------------------------- structs
writezint(kkstructtable)
structpos:=getpcpos()
writezint4(0)

nstructfields:=0
!nstructs:=0

for t:=tlast to ntypes when ttbasetype[t]=tstruct do
	d:=ttnamedef[t]^.deflist
	nfields:=0

	while d do
		if d^.nameid=fieldid and not d^.attribs.ax_at then
			++nfields
			if nfields>maxfields then
				loaderror("wsf: too many fields in struct")
			fi
			fieldlist[nfields]:=d
			ignore[nfields]:=0
		fi
		d:=d^.nextdef
	od
	for i:=1 to nfields do
		d:=fieldlist[i]
		for j:=i+1 to nfields when i<>j do
			e:=fieldlist[j]
			if d^.offset=e^.offset then
				ignore[i]:=1
			fi
		od
	od

	for i:=nfields downto 1 when not ignore[i] do
		d:=fieldlist[i]
		++nstructfields
!		writezint(nstructs)
		writezint(t)
		writezstring(d^.name)
		writezint(d^.mode)
		writezint(d^.offset)
	od

od

currpos:=getpcpos()
setpcpos(structpos)
writezint4(nstructfields)
setpcpos(currpos)
end

proc fixup_genfields=
int recordtype,fieldtype,i,offset
ref strec d,p,q

ngenfielddata:=0

for i:=1 to ngenfieldnames do
	d:=genfieldnames[i].def
	genfieldnames[i].dataindex:=ngenfielddata+1

	p:=d^.nextdupl
	while p do
		offset:=p^.offset
		if p^.nameid=fieldid then
!			fielddata[++ngenfielddata]:=(p.owner.mode.typeno,p.mode.typeno,p.offset,i)
			recordtype:=p^.owner^.mode
			fieldtype:=p^.mode

		elsif p^.nameid=procid then
			recordtype:=p^.owner^.mode
			fieldtype:=trefproc

		elsif p^.nameid=linkid and p^.owner^.nameid<>moduleid then
			q:=p
			repeat
				q:=q^.equiv
			until q^.nameid<>linkid
			recordtype:=p^.owner^.mode
			fieldtype:=trefproc
			offset:=q^.offset
		else
			goto skip
		fi
		if ngenfielddata>=maxgenfields then
			loaderror("GENFIELDDATA OVERFLOW")
		fi

		++ngenfielddata
		genfielddata[ngenfielddata].fieldindex:=i
		genfielddata[ngenfielddata].recordtype:=recordtype
		genfielddata[ngenfielddata].fieldtype:=fieldtype
		genfielddata[ngenfielddata].offset:=offset
skip::
		p:=p^.nextdupl
	od
	genfieldnames[i].datalength:=ngenfielddata-(genfieldnames[i].dataindex)+1
od
end

proc showhelp=
!ichar helptext=\
!strinclude "help.txt"
static ichar helptext="THIS IS THE HELP TEXT"

println helptext

stop
end

global function NEXTCMD(int &paramno, ichar &name, &value, ichar defext=nil)int=
static int infile=0
static ichar filestart=nil
static ichar fileptr=nil
static byte colonnext=0
ref char q
ichar item,fileext
int length
static [300]char str

RETURN 0
end

global proc writedocs=
[300]char filename
[50]ref strec params
int nparams
filehandle f
ref strec def,e
ref procrec p

strcpy(&.filename,moduletable[nmodules].filename)
strcpy(&.filename,changeext(&.filename,".txt"))

f:=fopen(&.filename,"w")
println "Writing Docstring file",&.filename

p:=proclist
while p do

	def:=p^.def
	if def^.docstring and not def^.attribs.ax_at then

		e:=def^.deflist
		nparams:=0
		while e do
			if e^.nameid=paramid and e^.name^<>'$' then
				params[++nparams]:=e
			fi
			e:=e^.nextdef
		od

		println @f,"================================================"
		print   @f,(def^.mode=tvoid|"Proc "|"Function ")
		print   @f,def^.owner^.name,,".",,def^.name,,"("
		for i:=nparams downto 1 do
			e:=params[i]
			if e^.attribs.ax_byrefmode then
				print @f,"&"
			fi
			print @f,e^.name
			if e^.code then
				print @f," =",strexpr(e^.code)^.strptr
			fi
			if i<>1 then print @f,"," fi
		od
		println @f,")"
		println @f,"================================================"
	    println @f,def^.docstring
	fi
	p:=p^.nextproc
od

fclose(f)
end

proc do_writeqa(ichar outfile)=
	[300]char newoutfile

!CPL "WRITEQA",FWRITEQA

	if fwriteqa then
		if fbundled then
			loaderror("-qa used with .qa input")
		fi
		strcpy(&.newoutfile, changeext(outfile,"qa"))

!CPL "WRITEQA",&.NEWOUTFILE

		writeqafile(&.newoutfile)
		stop
	fi
end
=== qc_support.m 28/38 ===
import clib
import mlib
import oslib

import var_types
import var_decls
import qc_lex
!import qc_pcldata
import pq_common
import qc_tables
import qci
!import pc_misc

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

!These z-prefix-codes are used purely when writing/reading the pc-file which stores packed
!data. For example, an operand marked as cint (integer) for a specific operand may be
!stored as (uncoded - 0..239 only, zint2, zint4 or zint8)
!For 0..239, that is the value that follows. For zint2, there is a zint2 byte (249 followed by
!2 bytes of data), etc.

! The following z-prefix-codes are used as the data type when the value is more
! complex than the 0..zmax values available with a single byte

const zmax=239             !maximum value in one byte

const zint240=245          !next byte is value 240 to 479
const zint480=246          !next byte is value 480 to 719
const zint720=247          !next byte is value 720 to 959

const zint1=248            !Int is neg of next byte value (mainly for small neg values)
const zint2=249            !Int in next 2 sign extended bytes
const zint4=250            !Int in next 4 bytes
const zint8=251            !Dint in next 8 bytes
const zreal4=252           !Real in next 4 bytes (add 32-bit zero lsw)
const zreal8=253           !Real in next 8 bytes
const zstring=254          !String
const zbytes=244          	!General data: count follows (in zint format) then data bytes
const zeof=255          	!eof marker

!vars for writing into expanding buffer
ref byte pcstart,pcdest,pcend
int pcalloc


global proc prterror(ichar mess)=
println "Print error:",mess
os_getch()
stop 1
end

global proc serror_gen(ichar mess)=
if currproc and currproc^.nameid=procid then
	print "In function",currproc^.name,," "
fi

println "On line",lx.lineno,"in file",currmodule^.filename
!showmacrolineno()

println
println "**** Syntax Error:",mess,"****"
println
println
println
stop 1
end

global proc serror(ichar mess)=
serror_gen(mess)
end

global proc serror_ss(ichar mess,a,b)=
[256]char str
fprint @&.str,mess,a,b
serror_gen(&.str)
end

global proc serror_s(ichar mess,a)=
[256]char str
!sprintf(&.str,mess,a)
fprint @&.str,mess,a
serror_gen(&.str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
int lineno,fileno
ichar poss

if p then
!	fileno:=p^.lineno>>24
	lineno:=p^.lineno
	fileno:=p^.moduleno
	poss:=""
else
	fileno:=currmoduleno
	poss:="?"
	lineno:=mlineno iand 16777215
fi

if currproc and currproc^.nameid=procid then
	print "In function",currproc^.name,," "
fi

println "On line",lineno iand 16777215,"in file",moduletable[fileno].filename,poss
println
case pass
when 'R' then print "**** RX Name Error: "
when 'T' then print "**** TX Type Error: "
when 'G' then print "**** GX Code Gen Error: "
esac
println mess
println
println


os_getch()

stop 1
end

global proc rxerror(ichar mess,unit p=nil)=
error_gen('R',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
error_gen('G',mess,p)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
[256]char str
!sprintf(&.str,mess,a)
fprint @&.str,mess,a
error_gen('N',&.str,p)
end

global function testelem(ref[0:]byte p,int n)int =		!TESTELEM
!caller must check that n is in range
return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =		!SETELEM
p^[n>>3] ior:= bytemasks[n iand 7]
end

global proc inittypetables=
int i,size,bitsize

!Initialise type tt-tables from std types first all fields initially zero

for i:=0 to tlast-1 do

	ttname[i]:=stdtypenames[i]
	ttbasetype[i]:=i

	case i
	when tintm, twordm, trefm then
		bitsize:=$targetbits
	else
		bitsize:=stdtypewidths[i]
	esac

	switch bitsize
	when 0 then
	when 1,2,4 then
		size:=1
	else
		size:=bitsize/8
	endswitch

	ttsize[i]:=size
	ttbitwidth[i]:=bitsize

	ttlower[i]:=1
od
ntypes:=tlast-1

tttarget[trefvar]:=tvariant
end

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

if x=0 then return 0 fi

int a:=1
while a<x do
	a<<:=1
od
return a
end

global proc initpcdest=
pcalloc:=16384

pcstart:=pcm_alloc(pcalloc)
pcend:=pcstart+pcalloc
pcdest:=pcstart
end

global function getpcpos:int=
return pcdest-pcstart
end

global proc setpcpos(int pos)=
pcdest:=pcstart+pos
end

global function writepcdata(ichar filename)int=
int nbytes
filehandle f

f:=fopen(filename,"wb");
if f=nil then
	println "Couldn't create",filename
	stop 1
fi

fwrite(pcstart,pcdest-pcstart,1,f)
fclose(f)

return 1
end

global proc writezstring(ichar s)=
int i,n

outpcbyte(zstring)
n:=strlen(s)

to n do
	outpcbyte(s++^)
od
outpcbyte(0)
end

global proc writezblock(ref byte s, int length)=
int i

to length do
	outpcbyte(s++^)
od
end

global proc writezint(int64 x)=
ref byte p

if x>=0 and x<=zmax then
!CP "Z1"
	outpcbyte(x)
elsif x>=240 and x<480 then
	outpcbyte(zint240)
	outpcbyte(x-240)
elsif x>=480 and x<720 then
	outpcbyte(zint480)
	outpcbyte(x-480)
elsif x>=720 and x<960 then
	outpcbyte(zint720)
	outpcbyte(x-720)
elsif x>=-127 and x<0 then
	outpcbyte(zint1)
	outpcbyte(-x)
elsif x>=-32768 and x<=32767 then
	outpcbyte(zint2)
	outpcword16(x)
elsif x>-0x8000'0000 and x<=0x7fff'ffff then
	outpcbyte(zint4)
	outpcword(x)
else
	p:=cast(&x)
	outpcbyte(zint8)
	to 8 do
		outpcbyte(p++^)
	od
fi
end

global proc writezint4(int x)=
!used when a value has to be written in a specific format
!(eg. when writing a tentative 0 to be filled in later with an unknown value)

outpcbyte(zint4)
outpcword(x)
end

global proc writezrange(ref byte p)=
outpcbyte(zint8)
to 8 do
	outpcbyte(p++^)
od
end

global proc writezreal(real x)=
ref byte p
ref int32 q

p:=cast(&x)
q:=cast(&x)

if q<>nil then
	outpcbyte(zreal8)
	to 8 do
		outpcbyte(p++^)
	od
else
	outpcbyte(zreal4)
	p+:=4
	to 4 do
		outpcbyte(p++^)
	od
fi
end

global proc writezeof=
	outpcbyte(zeof)
end

global function ipower(i64 a,int n)i64=		!IPOWER
!return a**b

if n<=0 then
	return 0
elsif n=0 then
	return 1
elsif n=1 then
	return a
elsif (n iand 1)=0 then
	return ipower(a*a,n/2)
else			!assume odd
	return a*ipower(a*a,(n-1)/2)
fi
end

global proc gs_additem(ref strbuffer dest,ichar s)=		!GENITEM
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

function isalphanum(int c)int=
if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
	return 1
fi
return 0
end

proc outpcbyte(int x)=
int newalloc,oldbytes
ref byte pcnew

if pcdest>=pcend then			!need new allocatiob
	newalloc:=pcalloc*2
	pcnew:=pcm_alloc(newalloc)
	oldbytes:=pcdest-pcstart
	memcpy(pcnew,pcstart,oldbytes)
	pcstart:=pcnew
	pcend:=pcstart+newalloc
	pcdest:=pcstart+oldbytes
	pcalloc:=newalloc
fi
pcdest++^:=x
end

proc outpcword(int x)=
ref byte p:=cast(&x)
outpcbyte(p++^)
outpcbyte(p++^)
outpcbyte(p++^)
outpcbyte(p^)
end

proc outpcword16(int x)=
ref byte p:=cast(&x)
outpcbyte(p++^)
outpcbyte(p^)
end

global proc writeqafile(ichar destfile)=
	filehandle f
	[maxqafiles]int fileoffsets, headeroffsets
	int offset,nn,NEWOFFSET

	println "Writing QA File",destfile

	f:=fopen(destfile,"wb")
	if not f then loaderror("Can't create qa file #",destfile) fi

CPL "WRITEQAFILE"

!CPL =NSOURCEFILES
!

	println @f,"qafile",nqafiles

	for i to nqafiles do
		print @f,i:"3",qafilenames[i]:"16jl"
		print @f, qafilesizes[i]:"8"
		headeroffsets[i]:=getfilepos(f)+1
		println @f,"         "
	od

	for i to nqafiles do
		fprintln @f,"=== # #/# ===",qafilenames[i],i,nqafiles

		offset:=getfilepos(f)
		fileoffsets[i]:=offset
!		nn:=writerandom(f,cast(mafiletext[i]),offset,sourcefilesizes[i])
		nn:=writerandom(f,cast(qafiletext[i]),offset,qafilesizes[i])
	od

!Note: the first "=" of the "===" that follows each file may be replaced
!by a zero-terminator after the .ma is loaded
	println @f,"=== end ==="

	for i to nqafiles do
		setfilepos(f,headeroffsets[i])
		print @f,fileoffsets[i]:"8"
	od

	fclose(f)
end

global proc addqafile(ichar filespec, source, int length) =
!global const maxqafile=maxmodule+50
!global [0..maxqafile]ichar qafilenames
!global [0..maxqafile]int qafilesizes
!global [0..maxqafile]int qafileoffsets
!global [0..maxqafile]ichar qafiletext
!global [0..maxqafile]byte qafilefileno			!0 or index into sourcefile tables
!global [0..maxqafile]byte qafilemult			!1 means could be parsed multiple times
!global int nqafiles
!global ichar qafilesource

	if nqafiles>=maxqafiles then
		loaderror("Too many qa files")
	fi
	++nqafiles
	qafilenames[nqafiles]:=pcm_copyheapstring(extractfile(filespec))
	qafiletext[nqafiles]:=pcm_copyheapstring(source)
	qafilesizes[nqafiles]:=length
end

=== qc_lex.m 29/38 ===
import msys
import mlib
import clib
import oslib

import var_types
import var_decls
import qc_tables
import qc_support

GLOBAL INT NLOOKUPS, NCLASHES

global record lexrec =		!should be 32-byte record
	union
		int64 value				!64-bit int
		real64 xvalue			!64-bit float
		word64 uvalue			!64-bit word
		ref char svalue			!pointer to string or charconst (not terminated)
		ref strec symptr		!pointer to symbol table entry for name
	end
	int32 symbol
	int32 subcode
	int32 length				!length of name/string/char
	int32 lineno
	int32 fileno
	int32 hashvalue
end

global lexrec lx				!provides access to current token data
global lexrec nextlx

const maxmacrodepth=10
[maxmacrodepth]ref byte macrostack
int macrolevel=0

const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

ref byte lxstart
!var ref char lxsptr
ref byte lxsptr
ref strec lxsymptr

!const hstsize	= 131072
!const hstsize	= 524288
const hstsize	= 1048576
const hstmask	= hstsize-1

global [0:hstsize]strec hashtable

[]ichar maxnumlist=(
	"",					!1
	"1111111111111111111111111111111111111111111111111111111111111111",   	!2
	"11112220022122120101211020120210210211220",                          	!3
	"33333333333333333333333333333333",                                   	!4
	"2214220303114400424121122430",                                       	!5
	"3520522010102100444244423",                                          	!6
	"45012021522523134134601",                                            	!7
	"1777777777777777777777",                                             	!8
	"145808576354216723756",                                              	!9
	"18446744073709551615",                                               	!10
	"335500516A429071284",                                                	!11
	"839365134A2A240713",                                                 	!12
	"219505A9511A867B72",                                                 	!13
	"8681049ADB03DB171",                                                  	!14
	"2C1D56B648C6CD110",                                                  	!15
	"FFFFFFFFFFFFFFFF")                                                   	!16
[maxnumlist.len]int maxnumlen

global proc lexreadtoken=
!read next token into nextlx
int c,hsum,commentseen
ref byte pstart,pnext,p

nextlx.subcode:=0

doswitch lxsptr++^
when 'a'..'z','$','_' then
	nextlx.svalue:=cast(lxsptr-1)
doname::
	hsum:=nextlx.svalue^
	nextlx.hashvalue:=0

	doswitch c:=lxsptr++^
	when 'A'..'Z' then
		(lxsptr-1)^:=c+' '
		hsum:=hsum<<4-hsum+c+' '
	when 'a'..'z','0'..'9','_','$' then
		hsum:=hsum<<4-hsum+c
	when '"' then
		--lxsptr
		if nextlx.svalue+1=ref char(lxsptr) and (nextlx.svalue^='F' or nextlx.svalue^='f') then
			readrawstring()
			return
		fi
		exit
	else
		--lxsptr
		exit
	end doswitch

	nextlx.symbol:=rawnamesym
	nextlx.length:=lxsptr-ref byte(nextlx.svalue)

	nextlx.hashvalue:=hsum<<5-hsum
	return

when 'A'..'Z' then
	nextlx.svalue:=cast(lxsptr-1)
	nextlx.svalue^+:=int(' ')
	goto doname

when '0'..'9' then
	c:=(lxsptr-1)^
	case lxsptr^
	when ' ',')',cr,',','|' then		!assume single digit decimal
!	when ' ',')',cr,',' then		!assume single digit decimal
		nextlx.symbol:=intconstsym
		nextlx.subcode:=tint
		nextlx.value:=c-'0'
	when 'x','X' then
		case c
		when '0' then		!0x
			++lxsptr
			readnumber(16)
		when '1' then
			lxerror("Bad base")
		else				!other base 2..9
			++lxsptr
			readnumber(c-'0')
		esac
	else
		--lxsptr
		readnumber(10)
	esac
	return

when '!' then			!comment to eol
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
	++nextlx.lineno

	nextlx.symbol:=eolsym
	return

when '#' then			!docstring to eol
!	goto docomment

	nextlx.svalue:=cast(lxsptr)

	doswitch c:=lxsptr++^
	when 13,10,etx,0 then			!leave eol for next symbol
		--lxsptr
		exit
	end

	nextlx.length:=lxsptr-ref byte(nextlx.svalue)
	nextlx.symbol:=docstringsym
	return

when '\\' then			!line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
!pstart:=lxsptr
	commentseen:=0
	doswitch lxsptr++^			!read until end of this line
	when cr then
		++nextlx.lineno
		++lxsptr				!skip lf
		exit
	when lf then
		++nextlx.lineno
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
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

	doswitch lxsptr++^
	when cr then
		++nextlx.lineno
		++lxsptr				!skip lf
	when lf then
		++nextlx.lineno
	when ' ',tab then
	else
		--lxsptr
		exit
	enddoswitch
!	next

when '{' then
	nextlx.symbol:=lcurlysym
	return

when '}' then
	nextlx.symbol:=rcurlysym
	return

when '.' then
	switch lxsptr^
	when '.' then				!.. or ...
		++lxsptr
		if lxsptr^='.' then
			++lxsptr
			nextlx.symbol:=ellipsissym
		else
			nextlx.symbol:=rangesym
			nextlx.subcode:=j_makerange		!helps treat as opsym which all have k-code as subcode
		fi
		return
	when '0'..'9' then			!real const: deal with this after the switch
		--lxsptr
		readrealnumber(nil,0,10)
		return
	else
		p:=lxsptr-2
		if p<lxstart or p^=cr or p^=lf then
			nextlx.symbol:=lexdotsym
		else
			nextlx.symbol:=dotsym
		fi
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
		case lxsptr^
!		when ':' then
!			++lxsptr
!			nextlx.symbol:=kswapsym
		when '=' then
			++lxsptr
			nextlx.symbol:=deepcopysym
			nextlx.subcode:=j_deepcopyx
		else
			nextlx.symbol:=assignsym
			nextlx.subcode:=j_assignx		!helps treat as opsym which all have k-code as subcode
		esac
	when ':' then
		++lxsptr
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=deepcopysym
			nextlx.subcode:=j_deepcopyx
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
	nextlx.subcode:=j_ptrto
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

when 178 then
	nextlx.symbol:=opsym
	nextlx.subcode:=j_sqr
	return
		!'œ' in ansi font or whatever
!when 'œ' then		!'œ' in ansi font or whatever
!	nextlx.symbol:=poundsym
!	return

when '~' then
	nextlx.symbol:=curlsym
	return

!when 'ª' then
!	nextlx.symbol:=gatesym
!	return

when '+' then
	nextlx.symbol:=opsym
	if lxsptr^='+' then
		++lxsptr
		nextlx.symbol:=incrsym
		nextlx.subcode:=j_preincrx
		return
	else
		nextlx.subcode:=j_add
	fi
	return

when '-' then
	nextlx.symbol:=opsym
	if lxsptr^='-' then
		++lxsptr
		nextlx.symbol:=incrsym
		nextlx.subcode:=j_predecrx
		return
	else
		nextlx.subcode:=j_sub
	fi
	return

when '*' then
	nextlx.symbol:=opsym
	if lxsptr^='*' then
		++lxsptr
		nextlx.subcode:=j_power
	else
		nextlx.subcode:=j_mul
	fi
	return

when '/' then
	nextlx.symbol:=opsym
	case lxsptr^
	when '/' then
		++lxsptr
		nextlx.subcode:=j_ddiv
	else
		nextlx.subcode:=j_div
	esac
	return

when '%' then
	nextlx.symbol:=opsym
	nextlx.subcode:=j_idiv
	return

when '=' then
	case lxsptr^
	when '>' then
		nextlx.symbol:=sendtosym
		++lxsptr
	when '=' then
		nextlx.symbol:=opsym
		nextlx.subcode:=j_isequal
		++lxsptr
	else
		nextlx.symbol:=opsym
		nextlx.subcode:=j_eq
	esac
	return

when '<' then
	nextlx.symbol:=opsym
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.subcode:=j_le
	when '>' then
		++lxsptr
		nextlx.subcode:=j_ne
	when '<' then
		++lxsptr
		nextlx.subcode:=j_shl
	else
		nextlx.subcode:=j_lt
	endswitch
	return

when '>' then
	nextlx.symbol:=opsym
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.subcode:=j_ge
	when '>' then
		++lxsptr
		nextlx.subcode:=j_shr
	else
		nextlx.subcode:=j_gt
	endswitch
	return

when '&' then
	case lxsptr^
	when '&' then
		++lxsptr
		nextlx.symbol:=opsym
		nextlx.subcode:=j_andand
	when '.' then
		++lxsptr
		nextlx.symbol:=anddotsym
		nextlx.subcode:=0
	else
		nextlx.symbol:=addrsym
		nextlx.subcode:=j_addrof
	esac
	return

when '\'','`' then
	lxreadstring('\'')
	return

when '"' then
	lxreadstring('"')
	return

when ' ',tab then

when cr then
	++lxsptr				!skip lf
	++nextlx.lineno
	nextlx.symbol:=eolsym
	return
when lf then			!only lfs not preceded by cr
	++nextlx.lineno
	nextlx.symbol:=eolsym
	return

when etx,0 then
	if macrolevel then
		unstackmacro()
	else
		nextlx.symbol:=eofsym
		--lxsptr
		return
	fi

else
	nextlx.symbol:=errorsym
	nextlx.value:=c
	return

end doswitch
!end switch
!od

end

proc lxreadstring(int termchar)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter
!
!NOTE: "(For this to work, \w is changed to \wl, as two characters are generated)"
!I don't get that, as the CR,LF replace the \ and w respectively. So I've removed
!the need to have \wl
static [256]char psname
ichar dest
int c,d

if termchar='"' then
	nextlx.symbol:=stringconstsym
	nextlx.subcode:=tstring
else
	nextlx.symbol:=charconstsym
	nextlx.subcode:=tint
fi

!nextlx.symbol:=(termchar='"'|stringconstsym|charconstsym)

if not prescanmode then
	dest:=cast(lxsptr)				!form string into same buffer
else								!put strings into local space
	dest:=&.psname					!must not overflow 255 characters
fi
nextlx.svalue:=dest

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
!		when 'u' then			!reserved for unicode, like \x but with 4 hex digits
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
			println c,char(c),=nextlx.lineno
			lxerror("Unknown string escape")
		end
	when '"','\'' then		!possible terminators
		if c=termchar then		!terminator char
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
			else			!was end of string
				exit
			fi
		fi
	when cr,lf,etx,0 then
		cpl =nextlx.lineno
		lxerror("String not terminated")
!		--lxsptr
!		exit
	endswitch

	if not prescanmode then
		dest++^:=c
	else
		if dest-nextlx.svalue<(psname.len-5) then
			dest++^:=c
		fi
	fi
od
nextlx.length:=dest-nextlx.svalue
(nextlx.svalue+nextlx.length)^:=0
end

proc readnumber(int base)=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
ref byte pstart,dest
int numtype,c
ref char p

dest:=pstart:=lxsptr

if base=10 then
	doswitch c:=lxsptr++^
	when '0'..'9' then
!		++lxsptr
		dest++^:=c
	when '_','\'','`' then
!		++lxsptr
	else
		--lxsptr
		exit
	end doswitch
else
	dest:=scannumber(base)
	c:=lxsptr^
fi

numtype:=0

switch c			!terminator character
when '.' then		!possible real number
	if (lxsptr+1)^<>'.' then

		readrealnumber(cast(pstart),dest-pstart,base)
		return
	fi
!	--lxsptr		!assume range
when 'e','E' then
	if base<=10 then
		readrealnumber(cast(pstart),dest-pstart,base)
		return
	fi
when 'p','P' then
	if base=16 then
		readrealnumber(cast(pstart),dest-pstart,base)
		return
	fi
when 'i','I' then
	++lxsptr
	numtype:=tint
when 'w','W','u','U' then
	++lxsptr
	numtype:=tword
when 'L','l' then
	++lxsptr
	numtype:=tlongint
end switch

stringtonumber(cast(pstart),dest-pstart,base, numtype)
end

proc readrealnumber(ichar intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, or is nil
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
ref byte fractstart
int fractlen,expon,i,c
real basex,x
const maxrealdigits=500
[maxrealdigits]char realstr


fractstart:=nil
fractlen:=0
expon:=0

if lxsptr^='.' then		!read
	fractstart:=++lxsptr
	fractlen:=scannumber(base)-fractstart
fi

case lxsptr^
when 'e','E' then
	if base<>16 then
		++lxsptr
		expon:=readexponent(base)
	fi
when 'p','P' then
	if base=16 then
		++lxsptr
		expon:=readexponent(base)
	fi
esac

if intlen+fractlen>maxrealdigits then
	lxerror("Real too long")
fi
if intlen then
	memcpy(&realstr,intstart,intlen)
fi
if fractlen then
	memcpy(&realstr[1]+intlen,fractstart,fractlen)
fi

basex:=base
expon-:=fractlen
x:=0.0
for i:=1 to intlen+fractlen do		!digits already range-checked
	c:=realstr[i]
	if c>='0' and c<='9' then
		x:=x*basex+c-int('0')
	elsif c>'a' then
		x:=x*basex+c-'a'+10
	else
		x:=x*basex+c-'A'+10
	fi
od

if expon>=0 then
	to expon do
		x*:=basex
	od
else
	to -expon do
		x/:=basex
	od
fi

nextlx.symbol:=realconstsym
nextlx.subcode:=treal
nextlx.xvalue:=x
end

function readexponent(int base)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
ref byte numstart,numend
int expon,length,neg

neg:=0
case lxsptr^
when '+' then ++lxsptr
when '-' then ++lxsptr; neg:=1
esac

numstart:=lxsptr
length:=scannumber(base)-numstart

if length=0 then
	lxerror("Bad expon")
fi

stringtonumber(cast(numstart), length, base, 0)
return (neg|-nextlx.value|nextlx.value)
end

proc lxerror(ichar mess)=
CPL nextlx.lineno,"LEX ERROR",mess,"in",stmodule^.name
abortprogram("Stopping")
end

global proc printsymbol(ref lexrec lp)=
lexrec l
l:=lp^

printf("%-18s",symbolnames[l.symbol])

case l.symbol
when rawnamesym then
	printstrn_app(l.svalue,l.length)
	print " (",,l.hashvalue,,")"
when namesym then
!	print l.symptr^.name
	printstrn_app(l.symptr^.name,l.symptr^.namelen)
when intconstsym then
	case l.subcode
	when tint then print l.value,"int"
	when tword then print l.uvalue,"word"
	else print l.value
	esac

when realconstsym then
	print l.xvalue

!printf(" %33.33f",l.xvalue)

when stringconstsym then
!	printf("\"%.*s\"",l.length,l.svalue)
	print """"
	printstrn_app(l.svalue,l.length)
	print """"
when charconstsym then
!	printf("'%.*s'",l.length,l.svalue)
	print "'"
	printstrn_app(l.svalue,l.length)
	print "'"
when longintconstsym then
	printstrn_app(l.svalue,l.length)
	print "L"
when opsym,assignsym,addrsym,ptrsym,deepcopysym,rangesym then
	print jtagnames[l.subcode]
elsif l.subcode then
	print "#",l.subcode
end

println

end

proc stringtonumber(ichar s, int length, base,numtype)=
!convert decimal number s to an i64 value
!s contains only digits
!for hex, then a..f and A..F have been converted to '9'+1 to '9'+6
int64 a
word64 b
int c
ref char t

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

if numtype=tlongint or length>maxnumlen[base] or \
		(length=maxnumlen[base] and strncmp(s,maxnumlist[base],length)>0) then
	nextlx.symbol:=longintconstsym
	nextlx.svalue:=s
	nextlx.length:=length
	switch base
	when 10 then
	when 16 then
		t:=pcm_alloc(2+length+1)
		strcpy(t,"0x")
		strcat(t,s)
		nextlx.svalue:=t
		nextlx.length+:=2
	when 2..9 then
		t:=pcm_alloc(2+length+1)
		nextlx.svalue:=t
		t++^:=base+'0'
		t++^:='x'
		strcat(t,s)
		nextlx.length+:=2

	else
		lxerror("longint/base?")
	endswitch

	if numtype and numtype<>tlongint then		!can't narrow result
		lxerror("Can't apply width override to longint")
	fi
	return
fi

a:=0

if base<=10 then
	to length do
		a:=a*base+s++^-'0'
!		a:=a*10+s++^-'0'
	od
else
	to length do
		c:=s++^
		if c>='a' then
			a:=a*base+c-'a'+10
		elsif c>='A' then
			a:=a*base+c-'A'+10
		else
			a:=a*base+c-'0'
		fi
	od
fi

nextlx.symbol:=intconstsym
nextlx.value:=a

if numtype then
	nextlx.subcode:=numtype
	return
fi

b:=word64(a)

if b<word64(0x7FFF'FFFF'FFFF'FFFF) then
	nextlx.subcode:=tint
else						!needs 1 more bit
	nextlx.subcode:=tword
fi
end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
int i!,n
static int n

for i to maxnumlist.len do
	maxnumlen[i]:=strlen(maxnumlist[i])
od

inithashtable()
n:=0
for i:=0 to hstmask do
	if hashtable[i].name then
		++n
	fi
od
end

!global proc printstrn_app(ichar s, int length)=
!if length then
!	emitc "printf(""%.*s"",(i32)length,s);"
!!	printf("%.*s",length,s)
!fi
!end

function scannumber(int base)ref byte=
!lxsptr is at possible first digit of number sequence
!scan digits until non-digit
!return pointer to next char after compacted sequence
!sequence can be updated in-place (to close gaps caused by separators)
!start of sequence will be at lxsptr
ref byte dest
int c

dest:=lxsptr

doswitch c:=lxsptr++^
when '0'..'9' then
	dest++^:=c
	if c>='0'+base then
		lxerror("Digit out of range")
	fi
when 'A'..'F','a'..'f' then
	if base=16 then
		dest++^:=c
	else
		--lxsptr
		exit
	fi
when '_','\'','`' then
else
	--lxsptr
	exit
end doswitch
return dest
end

proc readrawstring=
!positioned at " of F"
!read raw string
ichar dest
int c

nextlx.symbol:=stringconstsym
nextlx.subcode:=tstring
nextlx.svalue:=cast(++lxsptr)

dest:=cast(lxsptr)				!form string into same buffer

doswitch c:=lxsptr++^
when '"' then
	if lxsptr^='"' then		!repeated, assume embedded term char
		dest++^:='"'
		++lxsptr
	else			!was end of string
		(lxsptr-1)^:=0
		exit
	fi
when cr,lf,etx,0 then
	lxerror("Raw string not terminated")
	--lxsptr
	exit
else
	dest++^:=c
enddoswitch
nextlx.length:=dest-nextlx.svalue
end

function lookup:int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in nextlx.hashvalue
!return 1 (found) or 0 (not found)
!in either case, lxsymptr set to entry where name was found, or will be stored in
int j, wrapped

++NLOOKUPS

j:=nextlx.hashvalue iand hstmask
lxsymptr:=&hashtable[j]
wrapped:=0

do
	if lxsymptr^.name=nil then			!unused entry; finish
		exit
	fi
	if lxsymptr^.namelen=nextlx.length then	!match on length
		if memcmp(lxsymptr^.name,nextlx.svalue,nextlx.length)=0 then	!match
			return 1
		fi
	fi

++NCLASHES

	++lxsymptr
	if ++j>=hstsize then
		if wrapped then
			abortprogram("HASHTABLE FULL")
		fi
		wrapped:=1
		lxsymptr:=&hashtable[0]
		j:=0
	fi
od

!exit when not found; new name will go in entry pointed to by lxsymptr

lxsymptr^.name:=nextlx.svalue
lxsymptr^.namelen:=nextlx.length
lxsymptr^.symbol:=rawnamesym

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
	hsum:=hsum<<4-hsum + c
od
return hsum<<5-hsum
end

proc inithashtable=
!populate hashtable with standard symbols
int i
memset(&hashtable,0,hashtable.bytes)

for i:=1 to stnames.len do
	nextlx.svalue:=stnames[i]
	nextlx.length:=strlen(nextlx.svalue)
!	nextlx.hashvalue:=gethashvaluez(nextlx.svalue)
	nextlx.hashvalue:=gethashvaluez(nextlx.svalue)

	if lookup() then
		println stnames[i]
		abortprogram("Duplicate symbol table entry")
	fi

	lxsymptr^.symbol:=stsymbols[i]

	case stsymbols[i]
	when unitnamesym then
		lxsymptr^.index:=stsubcodes[i]
		lxsymptr^.subcode:=unitnamesym
		lxsymptr^.symbol:=rawnamesym		!masquerades as normal identifier
		else
		lxsymptr^.subcode:=stsubcodes[i]
	esac
od
end

function dolexdirective(int index)int=
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
ref strec symptr
ref byte p
ichar file
int i

case index
when definedir then
	lexreadtoken()
	if nextlx.symbol<>rawnamesym then lxerror("define: name expected") fi

!macro names are stored in the general symbol table
	if lookup() and lxsymptr^.symbol=lexmacronamesym then
		printstrn_app(nextlx.svalue,nextlx.length)
		lxerror("Macro already defined")
	fi
	symptr:=lxsymptr

	lexreadtoken()

	unless nextlx.symbol=opsym and nextlx.subcode=j_eq then
		lxerror("""="" expected")
	end unless
	
	p:=lxsptr
	lexreadline()

	addmacro(symptr,cast(p),lxsptr-p)
	lexreadtoken()			!should be eol
	if nextlx.symbol<>eolsym then
		lxerror("Bad define")
	fi
	return 0

when strincludedir then
	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("strincl: string expected") fi
	file:=nextlx.svalue
	nextlx.svalue:=cast(readfile(file))
	if nextlx.svalue=nil then
		cpl file
		lxerror("Can't find strinclude file")
	fi
	nextlx.symbol:=stringconstsym
	nextlx.subcode:=tstring
	nextlx.length:=rfsize

	if fwriteqa then
		addqafile(file,cast(nextlx.value), rfsize)
	fi


	(nextlx.svalue+rfsize)^:=0			!sometimes .length is not used (eg. in newstringobj())
	return 1							!so get it right. Don't need the etx

else
	cpl sourcedirnames[index]
	lxerror("Directive not implemented")
esac
return 0
END

proc lexreadline=
!read lex chars until eol
!returns with lxsptr pointing to what follows (crlf, etx etx)
!caller should remember lxsptr as start of text
!processing of next symbol deals with line counting

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

global proc startlex(ichar caption,sourcecode)=
!s is an etx and 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it
!(This lex system doesn't deal with include files so there no nested sourcefiles.
!There are only macro expansions which are dealt with locally.)

lxsptr:=cast(sourcecode)
nextlx.lineno:=1

nextlx.symbol:=semisym
nextlx.subcode:=0
end

global function convertzstring(ichar s, int length)ichar=
static [300]char str

if length>str.len then
	abortprogram("convertzstr")
fi
memcpy(&str,s,length)
str[length+1]:=0
return &.str
end

global function addnamestr(ichar name)ref strec=
lexrec oldlx

oldlx:=nextlx
nextlx.hashvalue:=gethashvaluez(name)

nextlx.length:=strlen(name)
nextlx.svalue:=pcm_alloc(nextlx.length+1)
memcpy(nextlx.svalue,name,nextlx.length+1)
lookup()

nextlx:=oldlx

return lxsymptr
end

global proc PS1(ichar caption)=
print caption,,":::"
printsymbol(&lx)
end

global proc PS2(ichar caption)=
print "	",,caption,,":##"
printsymbol(&nextlx)
end

global proc PS(ichar caption)=
PS1(caption)
PS2(caption)
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
!static int lastline=0
int lineno,n
ref char p

lx:=nextlx				!grab that already read basic token

reenter::

lexreadtoken()			!read new token for next time around

if lx.symbol=namesym then			!zero-terminate identifiers
	(lx.symptr^.name+lx.length)^:=0		!can only do so after next symbol is read
!note: there might be a problem if etx is next; answer is to use two etxs)
fi

!but... needs some processing first!

switch nextlx.symbol

when eolsym then
	case lx.symbol
	when commasym, lsqsym, lbracksym then		!ignore eol
		goto reenter
	when semisym then							!don't need ;;
		goto reenter
	else										!convert to semicolon
		nextlx.symbol:=semisym
	esac

when rawnamesym then
	if not lookup() then					!name not found
		nextlx.symbol:=namesym				!convert to actual identifier
		nextlx.symptr:=lxsymptr				
		return
	fi

	nextlx.symbol:=lxsymptr^.symbol			!convert to reserved word, type, op etc
	nextlx.subcode:=lxsymptr^.subcode

!deal with new set of symbols...
	switch nextlx.symbol
	when lexmacronamesym then
		stackmacro(lxsymptr^.macrovalue)
		goto reenter

	when ksourcedirsym then
		if not dolexdirective(nextlx.subcode) then
			goto reenter
		fi
	when rawnamesym then					!might be user identifier (points to generic entry)
		if nextlx.subcode=unitnamesym and \
				(lx.symbol=intconstsym or lx.symbol=realconstsym) then
			case lx.symbol
			when intconstsym then
				case lxsymptr^.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				when thousand_unit then lx.value *:= 1 thousand
				when kilo_unit then lx.value *:= 1024
				when mega_unit then lx.value *:= 1048576
				when giga_unit then lx.value *:= (1048576*1024)
				else
					lxerror("Can't do this unit index")
				esac
			else
				lxerror("Unit suffix after float not implem")
			esac
			goto reenter
		else
			nextlx.symbol:=namesym
			nextlx.symptr:=lxsymptr
		fi
	when namesym then						!matches existing name
		lxerror("NEXT NAME!!!")

	when kifsym,kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,kforallsym,
			kdosym,ktosym,kprocsym,kfunctionsym,kmethodsym,kimportmodulesym,kunlesssym,
			krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
			ktrysym,ktabledatasym then
		if lx.symbol=kendsym then
			lx.subcode:=nextlx.symbol			!turn end if to single end/if tokeb
			goto reenter
		fi
	when opsym then
		goto doopsym
	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
		case nextlx.subcode
		when nil_const, con_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when pi_const then
			nextlx.symbol:=realconstsym
			nextlx.xvalue:=3.1415926535897932384626
			nextlx.subcode:=treal
		when tab_const then
			nextlx.symbol:=stringconstsym
			nextlx.subcode:=tstring
			nextlx.svalue:="\t"
			nextlx.length:=1
		else
			lxerror("sysconst?")
		esac
	end switch

when lexdotsym then
	goto reenter

when eofsym then

when stringconstsym then
	if lx.symbol=stringconstsym then
		n:=nextlx.length+lx.length
		p:=pcm_alloc(n+1)
		memcpy(p,lx.svalue,lx.length)
		memcpy(p+lx.length,nextlx.svalue,nextlx.length)
		(p+n)^:=0
		lx.svalue:=p
		lx.length:=n
		goto reenter
!		lxerror("string/string")
	fi


when opsym then
doopsym::
	if nextlx.subcode=j_in and lx.symbol=opsym and lx.subcode=j_notl then
		lx.subcode:=j_notin
		goto reenter
	fi
endswitch

end

global proc showhashtablesize=
int i,n

n:=0
for i:=0 to hstmask do
	if hashtable[i].name then
		++n
	fi
od

CPL "FINAL HASHTABLE",n,hstsize
end

proc addmacro(ref strec symptr, ichar value, int length)=
!add the single token just read in nextlx to a new macro name at symptr
ichar s
int i

!trim macro value to exclude comments
s:=value
for i to length do
	case s^
	when '"' then		!contains string, abandon the idea; comments not allowed
	when '#','!' then	!assume start of comment
		length:=i-1
		if length=0 then lxerror("Null macro") fi
		exit
	esac
	++s
od

symptr^.symbol:=lexmacronamesym

(symptr^.name+symptr^.namelen)^:=0

s:=pcm_alloc(length+2)
memcpy(s,value,length)
(s+length+1)^:=etx
(s+length+2)^:=0
symptr^.macrovalue:=s
END

proc stackmacro(ichar s)=
if macrolevel>=maxmacrodepth then
	lxerror("Too many nested macros")
fi

++macrolevel
macrostack[macrolevel]:=lxsptr

!need to create separate copies of each macro invocation as lex process
!is destructive
lxsptr:=cast(pcm_copyheapstring(s))
end

proc unstackmacro=
if macrolevel<=0 then lxerror("unstack macro?") fi
lxsptr:=macrostack[macrolevel--]
end

=== qc_parse.m 30/38 ===
import msys
import mlib
import clib
import oslib

import var_types
import var_decls
!import var_ops
import qc_support
import pq_common
import qc_tables
import qc_lex
import qc_lib
import qc_name
!import qc_parselib

int intabledata=0		!1 means reading table data line; $ gives tabledataname
int inreadprint=0
int inparamlist=0
int inrecordbody=0
int inimportmodule=0
int labelseen=0
ichar tabledataname=nil

uflagsrec unionstring, unionpend
ref strec unionlastvar=nil

int try_level=0
int varattribs=0

const maxdollarstack=10
[maxdollarstack]ref unitrec dollarstack		!used for a[$]
int ndollar=0
int inmultexpr=0

ref strec currimport

global function parsemodule(int n)int=
modulerec m
ref strec p, owner
int globalflag,status

initparser()

m:=moduletable[n]

if fverbose then
	println "Parsing::",m.name,N
fi

stmodule:=moduletable[n].stmodule

startlex("PARSEMODULE",m.sourcecode)

owner:=stmodule
lex()

!VAR INT NTOKENS:=0
!
!REPEAT
!	lex()
!++NTOKENS
!UNTIL LX.SYMBOL=EOFSYM
!CPL =NLOOKUPS
!CPL =NCLASHES
!
!CPL =NTOKENS
!
!STOP
!

status:=readmoduledefs(owner)
if not status then
	return 0
fi

ALLLINES+:=LX.LINENO

return status
end

global function readmoduledefs(ref strec owner)int=
!first symbol has been read
ref strec p
int globalflag,i,found

globalflag:=0

do
	switch lx.symbol
	when kglobalsym then
		if globalflag then serror("global global?") fi
		globalflag:=1
		lex()

	when kprocsym,kfunctionsym,kmethodsym then	!todo
		readprocdef(owner,globalflag)

		globalflag:=0

	when kvarsym then
		readvardef(owner,globalflag,0,staticid)
		globalflag:=0

	when kimportmodulesym then
		readimportmodule(owner)

	when ktypesym then
		readtypedef(owner,globalflag)
		globalflag:=0

	when kconstsym then
		readconstdef(owner,globalflag)
		globalflag:=0

	when kclasssym,krecordsym then
		readclassdef(owner,globalflag)
		globalflag:=0

	when kenumsym then
		lex()
		readenumtype(owner,0)

	when ktabledatasym then
		readtabledef(owner,globalflag)
		globalflag:=0

	when docstringsym then
		gs_strn(docstring,lx.svalue,lx.length)
		gs_line(docstring)
		lex()

	when kimportsym then
		if globalflag then serror("glob/import?") fi
		lex()
		if lx.symbol=opsym and lx.subcode=j_mul then
			lex()
		fi
		checksymbol(namesym)

!need to check that the import has been done (in case import stmt is badly placed)
!(note: can't detect a badly placed import if that lib has been loaded elsewhere)
		found:=0
		for i:=1 to nmodules do
			if eqstring(lx.symptr^.name, moduletable[i].name) then
				found:=1
				exit
			fi
		od
		if not found then
			CPL lx.symptr^.name
			serror("Import stmt out of position?")
		fi

		lex()

	when kimportpathsym then
!CPL "PARSE/PATH"
		lex()
		checksymbol(stringconstsym)
		lex()

	when kapplprocsym then
		readapplprocs(owner)

	when kcondcompsym then
		case lx.subcode
		when windowsff then
			if os_iswindows() then
				lex()
			else
skiptoeol::
				repeat lex() until lx.symbol=semisym or lx.symbol=eofsym

			fi
		when linuxff then
			if not os_iswindows() then
				lex()
			else
				goto skiptoeol
			fi
		else
			serror("condcomp")
		esac

	when semisym then
		lex()

	when eofsym then
		exit

	else
		PS1("symbol")
		serror("Not allowed at module level")
	endswitch
od

return 1
end

proc initparser=

unless nullunit then
	nullunit:=createunit0(j_null)
end unless

try_level:=0
currproc:=nil
varattribs:=0

intabledata:=0		!1 means reading table data line; $ gives tabledataname
inreadprint:=0
inparamlist:=0
inrecordbody:=0
inimportmodule:=0
ichar tabledataname:=""
labelseen:=0
currimport:=nil

ndollar:=0

gs_init(docstring)

end

proc skipsemi=
while lx.symbol=semisym do lex() od
end

proc addalias(ref strec stold,stnew)=			!ADDALIAS
stnew:=getduplnameptr(stold^.owner,stnew,aliasid)
adddef(stold^.owner,stnew)
stnew^.equiv:=stold
end

function makeblock(ref unitrec p)ref unitrec=
return createunit1(j_block,p)
end

proc checkequals=			!CHECKEQUALS
!check that "=" is current symbol
if not (lx.symbol=opsym and lx.subcode=j_eq) then
	serror("""="" expected")
fi
end

function getcurrline:int=
return lx.lineno
end

function checkbegin(int fbrack)int=				!CHECKBEGIN
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
int closesym

skipsemi()

if lx.symbol=lbracksym and fbrack then
	closesym:=rbracksym
	lex()
elsif lx.symbol=kbeginsym then
	closesym:=kendsym
	lex()
elsif lx.symbol=lcurlysym then
	closesym:=rcurlysym
	lex()
else
	closesym:=kendsym
fi
return closesym
end

proc checkbeginend(int closesym,kwd,startline=0)=		!CHECKBEGINEND
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
skipsemi()
!if closesym=rbracksym then
if closesym=rbracksym or closesym=rcurlysym then
	checksymbol(closesym)
else
	checkend(closesym,kwd,startline)
fi
lex()
end

proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=		!CHECKEND
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
!		sprintf(&.str+strlen(&.str)," (from line %d)",startline)
		fprint @&.str+strlen(&.str)," (from line #)",startline
	fi
	serror(&.str)
fi

!'end', seen, but check that lx.subcode, if non-zero, is endkeywords or is in that set
if lx.subcode=0 then					!plain end; for now, that always matches
!	serror("'end' by itself no longer valid")
	return
fi

unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
	strcpy(&.str,"Mismatched 'end'")
	goto error
end unless
end

proc addgenfield(ref strec d)=
!d is an strec corresponding to the nominal nullid version of any name,
!unless already converted to a genfieldid
!make it a genfield and add it to the table of genfieldnames
!should be called for names that are fields/methods of local classes and records

if d^.nameid=genfieldid then return fi
if ngenfieldnames>=maxgenfields then
	serror("Too many genfields")
fi

d^.nameid:=genfieldid		!convert base/nullid name to genfield
genfieldnames[++ngenfieldnames].def:=d
d^.offset:=ngenfieldnames	!use instead of .index, which might be needed for some nullid names (unitsuffixes for example?)
end

proc readvardef(ref strec owner,int isglobal=0,isstatic=0,varid=staticid)=
!positioned at 'var'
!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid/staticid for procs, otherwise staticid
int nvars,m
ref strec stname

lex()
m:=tvariant

nvars:=0
while lx.symbol=namesym do
	++nvars
	stname:=getduplnameptr(owner,lx.symptr,varid)
	stname^.mode:=m
	stname^.attribs.ax_global:=isglobal
	stname^.attribs.ax_static:=isstatic

	adddef(owner,stname)

	lex()

	if lx.symbol=assignsym or lx.symbol=opsym and lx.subcode=j_eq then
		if lx.symbol=assignsym then
			if varid=staticid then
				serror("Need = on static not :=")
			fi
		else
			if varid=frameid then
				stname^.nameid:=staticid
				stname^.attribs.ax_frame:=0
			fi
		fi
		lex()
		stname^.code:=readexpression()
		stname^.attribs.ax_equals:=1
	fi

	if lx.symbol<>commasym then
		exit
	fi
	lex()
od

if nvars=0 then
	serror("No vars declared")
fi
end

proc readconstdef(ref strec owner,int isglobal=0)=
!at 'const' symbol
int nconsts,deft,t
ref strec stname

lex()
deft:=tvoid

nconsts:=0

while lx.symbol=namesym do
	stname:=getduplnameptr(owner,lx.symptr,constid)

	lex()

	checkequals()
	lex()
	stname^.code:=readconstexpr(owner,1)
!		stname.code:=readexpression()
	if deft=tvoid then
		t:=stname^.code^.mode
	else
		t:=deft
	fi
	stname^.mode:=t
	++nconsts

	stname^.attribs.ax_global:=isglobal

	adddef(owner,stname)

	if lx.symbol<>commasym then
		exit
	fi
	lex()
od

if nconsts=0 then
	serror("No consts declared")
fi

end

function readexpression:ref unitrec=	!READEXPRESSION
!this reads an expression that will return a value.
!CPL "READEXPR"
return readfactor(8)
end

function readfactor(int level)ref unitrec=			!READFACTOR
ref unitrec p,q,r
int opc,opprio,lineno

!CPL "READFACTOR",LEVEL

if level<=1 then		!level might be 0
	p:=readterm()
else
	p:=readfactor(level-1)
fi

!CPL "GOT P",=LEVEL
doswitch lx.symbol
when opsym, assignsym, rangesym, addrsym, deepcopysym then
!	if lx.symbol=assignsym and inmultexpr then
!		exit
!	fi

	opc:=lx.subcode				!will be kadd, kassign, etc
!	opprio:=getopprio(opc)
	opprio:=jtagpriotable[opc]
	lineno:=lx.lineno
!CPL =JTAGNAMES[OPC],=OPPRIO,=LEVEL
!STOP
	if opprio<>level then exit fi


	lex()
	if opc=j_assignx or opc=j_deepcopyx then			!assign is right-to-left but also special
		q:=readexpression()
	elsif opc=j_power then			!power is right-to-left
		q:=readfactor(level)
	else
		q:=readfactor(level-1)
	fi
	p:=createunit2(opc,r:=p,q)
	p^.lineno:=lineno

else
	exit
enddoswitch
return p
end

function readterm:ref unitrec=		!READTERM
ref unitrec p,q,r
ref char pbyte
word64 a
int oldipl,opc,oldinrp,lineno,shift

lineno:=lx.lineno

!PS("READTER")

switch lx.symbol
when namesym then
	p:=createname(lx.symptr)
	p^.lineno:=lx.lineno
	if nextlx.symbol=lbracksym then
		p^.def:=lx.symptr
!	else
	fi
	lex()

when intconstsym,realconstsym then
	p:=createconstunit(lx.value,lx.subcode)
	lex()

when stringconstsym then
	p:=createstringconstunit(lx.svalue,lx.length)
    p^.slength:=lx.length
	lex()

when longintconstsym then
	(lx.svalue+lx.length)^:=0
	p:=createunit0(j_longint)
	p^.svalue:=lx.svalue
	p^.slength:=lx.length
	lex()

when charconstsym then
	a:=0
	shift:=0
	pbyte:=lx.svalue
	to lx.length do
		a:=a ior word64(pbyte^)<<shift
		shift+:=8
		++pbyte
	od
	p:=createconstunit(a,tint)
	lex()

when lbracksym then
	p:=readlbrack()

when lsqsym then
	oldipl:=inparamlist
	inparamlist:=0
	p:=readlsqbrack()
	inparamlist:=oldipl

when stdtypesym,krefsym then
	p:=readcast()

when opsym then
	p:=readopc()

when incrsym then
	opc:=lx.subcode
	lex()
	p:=createunit1(opc,readterm())

when ksprintsym then
	p:=readsprint()

when ksreadsym,ksreadlnsym then
	p:=readsread()

when ptrsym,addrsym then
	opc:=lx.subcode
	lex()
	p:=createunit1(opc,readterm())
	if p^.a^.tag=j_callfn then
		if p^.a^.b then
			serror("Params not allowed")
		fi
		p^.a:=p^.a^.a			!lose the call
	fi

when compilervarsym then
	p:=readcompilervar()
	lex()

when kerrorsym then
	p:= createconstunit(lx.subcode,tint)
	lex()

when dollarsym then
	if intabledata then
		p:=createstringconstunit(tabledataname,-1)
	else
		if ndollar<=0 then
			serror("[$] No array")
		fi
		p:=createunit1(j_upb,dollarstack[ndollar])
	fi
	lex()

when kapplyopsym then
	p:=readapplyop(1)

when kcastsym then
	p:=readcastx()

when ktypeconstsym then
	lex()
	checksymbol(lbracksym)
	lex()
	p:=createunit0(j_typeconst)
	p^.mode:=readtypespec(nil,0)
	checksymbol(rbracksym)
	lex()

when kclampsym then
	lex()
	checksymbol(lbracksym)
	lex()
	p:=readexpression()
	checksymbol(commasym)
	lex()
	q:=readexpression()
	if lx.symbol=rbracksym and q^.tag=j_makerange then
		r:=q^.b
		q:=q^.a
	else
		checksymbol(commasym)
		lex()
		r:=readexpression()
		checksymbol(rbracksym)
	fi
	lex()
	p:=createunit3(j_clamp,p,q,r)

when khostfnsym then
	p:=readhostparams(nil,1)

when kapplsym then
	p:=readapplcall()

else
	cpl symbolnames[lx.symbol]
	serror("readterm?")
endswitch

!now look at the suffix, which can modify the p unit above
doswitch lx.symbol
when lbracksym then
	lex()
	oldinrp:=inreadprint
	inreadprint:=0
	q:=readslist(1,1)
	checksymbol(rbracksym)
	lex()
	if p.tag=j_callapplfn then
		p.b:=q
	else
		p:=createunit2(j_callfn,p,q)
		p:=testconstruct(p)
	fi
	inreadprint:=oldinrp

when ptrsym then
	p:=createunit1(j_ptr,p)
	lex()

when lsqsym then
	p:=readindex(p,0)

when lcurlysym then
	p:=readkeyindex(p,0)

when dotsym then
	p:=readdotsuffix(p)

when colonsym then
	if inreadprint then exit fi
	lex()
	q:=readexpression()
	p:=createunit2((inparamlist|j_keyword|j_keyvalue),p,q)

when incrsym then
	case lx.subcode
	when j_preincrx then opc:=j_postincrx	!1
	when j_predecrx then opc:=j_postdecrx	!1
	esac
	lex()
	p:=createunit1(opc,p)

when anddotsym then
	lex()
	checksymbol(lsqsym)
	lex()
	q:=readexpression()
	if q^.tag=j_makerange then
		p:=createunit2(j_anddotslice,p,q)
	else
		p:=createunit2(j_anddotindex,p,q)
	fi
	checksymbol(rsqsym)
	lex()
when opsym then
	case lx.subcode
	when j_sqr then
		p:=createunit1(j_sqr,p)
		lex()
!CPL "APPLYING SQR OP"

	else
		exit
	esac

else
	exit
enddoswitch

p^.lineno:=lineno

return p

end

function readlbrack:ref unitrec=
!positioned at "("
!termsym is rbracksym
!read one of the following::
! (x)		simple expression
! ()		list with no elements
! (x,)		list with one element
! (x,x,...)		list
! (x|x|x])		if then else fi
! (x|x,... |x])	select then else end

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
ref unitrec plower, ulist,ulistx, p,q,r
int lcmode,oldirp

lex()					!first symbol of first expression
plower:=nil
lcmode:=tlist
ulist:=ulistx:=nil

if lx.symbol=atsym then			!lwb override
	lex()
	oldirp:=inreadprint
	inreadprint:=1
	plower:=readexpression()
	inreadprint:=oldirp
	checksymbol(colonsym)
	lex()
elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
	plower:=createconstunit(lx.value,lx.subcode)
	lex()
	lex()
elsif lx.symbol=opsym and nextlx.symbol=rbracksym then	!operator constant
	p:=createunit0(j_operator)
	p^.opcode:=lx.subcode
	lex()
	lex()
	return p
elsif lx.symbol=opsym and nextlx.symbol=assignsym then	!operator:= constant
	p:=createunit0(j_operator)
	p^.opcode:=getoptocode(lx.subcode)
	lex()			!read :=
	lex()			!read )
	checksymbol(rbracksym)
	lex()
	return p
elsif lx.symbol=ktypesym and nextlx.symbol=rbracksym then
	p:=createunit0(j_operator)
	p^.opcode:=j_gettype
	lex()
	lex()
	return p
fi

if lx.symbol=stdtypesym and lx.subcode=tstring then
	lcmode:=tstring
	lex()
	checksymbol(colonsym)
	lex()
fi

!check symbol after "("
case lx.symbol
when rbracksym then			!empty list
	lex()
	p:=createunit0(j_makelist)
	if plower<>nil then
		p^.b:=plower
	fi
	return p
else					!assume normal expression follows
	p:=readexpression()
esac

!check symbol after "(expr"
case lx.symbol
when rbracksym then			!simple (x) expression
	lex()

	if plower then				!pattern is (y:x), so read as key:value pair
		return createunit2(j_keyvalue,plower,p)
	else
		return p
	fi
when semisym then			!multiple expression
	ulist:=ulistx:=p
	repeat
		lex()					!skip comma
		addlistunit(&ulist,&ulistx,readexpression())
	until lx.symbol<>semisym
	checksymbol(rbracksym)
	lex()

	p:=ulist
	while p do
		case p^.tag
		when j_assignx then p^.tag:=j_assign
		when j_deepcopyx then p^.tag:=j_deepcopy
		when j_preincrx then p^.tag:=j_preincr
		when j_predecrx then p^.tag:=j_predecr
		when j_postincrx then p^.tag:=j_postincr
		when j_postdecrx then p^.tag:=j_postdecr
		when j_callfn then p^.tag:=j_callproc
		when j_callapplfn then p^.tag:=j_callapplproc
		when j_callmfn then p^.tag:=j_callmproc
!			CPL "CALL MAKEMPROC2"
		esac
		p:=p^.nextunit
	od
	return createunit1(j_exprlist,ulist)

when commasym then
	if nextlx.symbol=rbracksym then		!means one-element list
		lex()
		lex()
		return createunit2(j_makelist,p,plower)
	fi

!must be regular list
	ulist:=ulistx:=p
	repeat
		lex()							!skip comma
		if lx.symbol=rbracksym then		!allow ,) to end list
			exit
		fi
		if lx.symbol=commasym then
			serror(",, null expr not allowed")
		fi
		addlistunit(&ulist,&ulistx,readexpression())
		skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
	until lx.symbol<>commasym
	checksymbol(rbracksym)
	lex()
	return createunit2(j_makelist,ulist,plower)

when barsym then			!ifx/selectx expression; p is selector expression
	lex()
	q:=readexpression()
	if lx.symbol=barsym then		!(a|b|c)
		lex()
		r:=readexpression()
		checksymbol(rbracksym)
		lex()
		return createunit3(j_ifx,p,q,r)
	fi

!assume selectx expression
	addlistunit(&ulist,&ulistx,q)	!start with one-element list
	checksymbol(commasym)
	if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
		repeat
			lex()				!skip comma
			addlistunit(&ulist,&ulistx,readexpression())
		until lx.symbol<>commasym
		checksymbol(barsym)
	else
		lex()					!skip |
	fi
	lex()
	r:=readexpression()
	checksymbol(rbracksym)
	lex()
	return createunit3(j_selectx,p,ulist,r)

when dbarsym,kforsym,kforallsym then
!*!	return readlistcomp(p,lcmode)
serror("READLISTCOMP")
else
	serror("(x ...")
esac
return nil
end

proc addlistunit(ref ref unitrec ulist,ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextunit:=p
fi
ulistx^:=p			!update end-of-list pointer
end

proc addlistparam(ref ref strec ulist,ulistx,ref strec p)=
!add unit p to unit structure ulist,^ulistx  which can be null
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextparam:=p
fi
ulistx^:=p			!update end-of-list pointer
end

function readlsqbrack:ref unitrec=			!READLSQBRACK
ref unitrec ulist,ulistx, p,q

!at '['
lex()
ulist:=ulistx:=nil

case lx.symbol
when rsqsym then			!empty set
	lex()
	p:=createunit1(j_makesetlist,nil)
	if not checkdict(p) then
		checkconstlist(p)
	fi
	return p
when colonsym then			!empty dict
	lex()
	checksymbol(rsqsym)
	lex()
	p:=createunit1(j_makedict,nil)
	return p

esac

do
	addlistunit(&ulist,&ulistx,readexpression())
	skipsemi()
	exit when lx.symbol<>commasym
	lex()
	if lx.symbol=rsqsym then			!allow ,] ending
		exit
	fi
od
checksymbol(rsqsym)
lex()
p:=createunit1(j_makesetlist,ulist)
if not checkdict(p) then
	checkconstlist(p)
fi
return p
end

function readcast:ref unitrec=			!READCAST
!also reads standalone type value
ref unitrec p
int t,opc

t:=readtypespec(nil,0)

!check for standalone value
case lx.symbol
when atsym,lbracksym then
else						!convert to typeconst
	if t=tvoid then
		p:=createunit0(j_typeval)
	else
		p:=createunit0(j_typeconst)
	fi
	p^.mode:=t
	return p
esac

checkunpackedtype(t)

if lx.symbol=atsym then
	lex()
	opc:=j_typepun
else
	opc:=j_convert
fi
checksymbol(lbracksym)
p:=readlbrack()

if p^.tag=j_makelist then p^.tag:=j_makeconstr fi

p:=createunit1(opc,p)
p^.mode:=t
return p
end

function readopc:ref unitrec=			!READOPC
!op sym seen just before a term
ref unitrec p,q
int opc,opc2

opc:=lx.subcode
lex()
case opc
when j_add then			!ignore +
	return readterm()
when j_sub then			!convert minus to negate
	opc:=j_neg
when j_min,j_max,j_atan2,j_concat,j_append then	!allow some binary ops to have function format

	checksymbol(lbracksym)
	lex()
	p:=readexpression()
	if lx.symbol=commasym then
		lex()
		q:=readexpression()
		checksymbol(rbracksym)
		lex()
		return createunit2(opc,p,q)
	else
		checksymbol(rbracksym)
		lex()
		case opc
		when j_min then opc2:=j_min1
		when j_max then opc2:=j_max1
		else serror("readopc")
		esac
		return createunit1(opc,p)
	fi
esac

if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
	serror("op:= not allowed")
fi

p:=createunit1(opc,readterm())
!evalmonop(p)
return p
end

function readsprint:ref unitrec=			!READSPRINT
int oldinreadprint,opc,isfprint
ref unitrec pformat, pdev, printlist, printlistx, p

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lex()
checksymbol(lbracksym)
lex()

!*!isfprint:=(opc=j_sfprint ior opc=j_scprint) #DOESN'T WORK
case opc
when j_sfprint,j_cprint then
	isfprint:=1
else
	isfprint:=0
esac

printlist:=printlistx:=nil
pformat:=pdev:=nullunit

if lx.symbol=atsym then
	lex()
	pdev:=readexpression()
	if lx.symbol=commasym then lex() else goto finish fi
fi
if isfprint then
	pformat:=readexpression()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if lx.symbol=rbracksym then
	goto finish
fi

do
	if lx.symbol=commasym then		!assume extra comma, meaning nogap
		addlistunit(&printlist,&printlistx,createunit0(j_nogap))
	else
		p:=readexpression()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(j_fmtitem,p,readexpression())
		fi
		addlistunit(&printlist,&printlistx,p)
	fi
	if lx.symbol<>commasym then exit fi
	lex()
od

checksymbol(rbracksym)

finish::
lex()
inreadprint:=oldinreadprint
if (opc=j_print or opc=j_fprint) and printlist=nil then
	serror("No print items")
fi

if isfprint then
	if pformat^.tag=j_null then
		serror("No fmt str")
	fi
	return createunit3(opc,pdev,pformat,printlist)
else
	return createunit2(opc,pdev,printlist)
fi
end

function readsread:ref unitrec=		!READSREAD
!NEED TO CHECK WHAT SREAD/SREADLN actually mean. I think they are actually supposed
!to work an item at a time::
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
int oldinreadprint,opc
ref unitrec pformat,pdev,p, readlist,readlistx

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lex()
checksymbol(lbracksym)
lex()

readlist:=readlistx:=nil
pformat:=pdev:=nullunit

if lx.symbol=atsym then
	if opc=j_read then
		serror("@ on read")
	fi
	lex()
	pdev:=readexpression()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if lx.symbol=rbracksym then
	goto finish
fi

do
	p:=readexpression()
	if lx.symbol=colonsym then
		lex()
		p:=createunit2(j_fmtitem,p,readexpression())
	fi
	addlistunit(&readlist,&readlistx,p)
	if lx.symbol<>commasym then exit fi
	lex()
od

checksymbol(rbracksym)

finish::
lex()
inreadprint:=oldinreadprint
if opc=j_read and readlist=nil then
	serror("No read items")
fi

return createunit2(opc,pdev,readlist)
end

function readcompilervar:ref unitrec=		!READCOMPILERVAR
!unit p
[100]char str
rsystemtime tm
static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

case lx.subcode
when j_cvlineno then
	return createconstunit(lx.lineno,tint)
!	lex()
!	return p
when j_cvstrlineno then
!	sprintf(&.str,"%d",lx.lineno)
	getstrint(lx.lineno,&.str)

when j_cvmodulename then
	strcpy(&.str,moduletable[currmoduleno].name)

when j_cvfilename then
	strcpy(&.str,moduletable[currmoduleno].filename)
when j_cvfunction then
	strcpy(&.str,(currproc|currproc^.name|"<none>"))
when j_cvdate then
	os_getsystime(&tm)

!	sprintf(&.str,"%d-%s-%4d",tm.day,monthnames[tm.month],tm.year)
	fprint @&.str,"#-#-%#",tm.day,monthnames[tm.month],tm.year:"4"

when j_cvtime then
	os_getsystime(&tm)
!	sprintf(&.str,"%2d:%02d:%02d",tm.hour,tm.minute,tm.second)
	fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

!when j_cvversion then x:=compilerversion
!when j_cvpclversion then x:=pclversion
else
	serror("compiler not impl")
esac

return createstringconstunit(pcm_copyheapstring(&.str),-1)
end

function readcastx:ref unitrec=			!READCASTX
!explicit cast using syntax::
! cast(expr,type)
! cast@(expr,type)
!at 'cast'
int opc
ref unitrec pexpr,p
int ptype

lex()
if lx.symbol=atsym then
	lex()
	opc:=j_typepun
else
	opc:=j_convert
fi
checksymbol(lbracksym)
lex()
pexpr:=readexpression()
checksymbol(commasym)
lex()
ptype:=readtypespec(nil,0)
checksymbol(rbracksym)
lex()
p:=createunit1(opc,pexpr)
p^.mode:=ptype
return p
end

global proc checksymbol(int symbol)=
[100]char str

if lx.symbol<>symbol then
!	sprintf(&.str,"%s expected, not %s",symbolnames[symbol],symbolnames[lx.symbol])
	fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]

	serror(&.str)
fi
end

function readtypespec(ref strec owner,int typedefx)int=			!READTYPESPEC
!at initial symbol of a type
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either::
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using int16 etc
ref strec d
int t,kwd
unit x, lowerx,upperx,lengthx
int p
const maxdim=10
[maxdim]unit lowerdims,lengthdims
int ndims,i,n

case lx.symbol
when lsqsym then		!array bounds
arraybounds::
	lex()
	ndims:=0
	inreadprint:=1
	do
		lowerx:=lengthx:=nil
		if lx.symbol=rsqsym or lx.symbol=commasym then		![]
		else
			x:=readconstexpr(owner,0)			!reduce if possible
			if x^.tag=j_makerange then			![a..b] variable
				lowerx:=x^.a
				upperx:=x^.b
				if lowerx^.tag=j_const and upperx^.tag=j_const then
					lengthx:=createconstunit(upperx^.value-lowerx^.value+1,tint)
				else
					lengthx:=createunit2(j_sub,upperx,lowerx)
					lengthx:=createunit2(j_add,lengthx,createconstunit(1,tint))
				fi
			else
				case lx.symbol
				when rsqsym,commasym then			![n]
					lengthx:=x
				when colonsym then				!a:n
					lowerx:=x
					lex()
					if not (lx.symbol=commasym or lx.symbol=rsqsym) then
						lengthx:=readconstexpr(owner)
					fi
				esac
			fi
		fi
		lowerdims[++ndims]:=lowerx
		lengthdims[ndims]:=lengthx
		exit when lx.symbol<>commasym
		lex()
	od
	inreadprint:=0
	checksymbol(rsqsym)
	lex()
	t:=readtypespec(owner,0)
	checkpackedtype(t)
	for i:=ndims downto 1 do
!		(lower,length,lx):=dims[i]
		t:=createarraymode(owner,t,lowerdims[i],lengthdims[i],(i=1|typedefx|0))
	od
	return t

when stdtypesym then
	t:=lx.subcode
	lex()
	case t
	when tstring,tset,tstringz then
		if lx.symbol=opsym and lx.subcode=j_mul then
			lex()
!			n:=readconstexprvalue(owner)
!			if n<=0 then serror("Bad *N") fi
			p:=createstringmode(owner,t,readconstexpr(owner),typedefx)
		else
			p:=t
		fi
	else
		p:=t	!createtype(t)	!use code as it is
	esac

when namesym then
	d:=lx.symptr
	lex()
	if lx.symbol=dotsym then
		lex()
		checksymbol(namesym)
		p:=newusertypex(d,lx.symptr)
		lex()
	else
		p:=newusertypex(d)
	fi

when kvarsym then		!'var' or 'variant' used as typespec, rather than decl keyword
	p:=tvariant
	lex()

when kenumsym then		!enum
	lex()
	p:=readenumtype(owner,typedefx)

when lbracksym then
	p:=readenumtype(owner,typedefx)

when krecordsym then
	serror("Use 'record name=', not 'type name=record'")

when kstructsym then
	kwd:=lx.symbol
	lex()
	if lx.symbol=datsym then
		if owner=nil then serror("record@@") fi
		lex()
		checksymbol(intconstsym)
		case lx.value
		when 1,2,4,8,16 then
		else
			serror("record@@ bad align")
		esac
		owner^.attribs.ax_align:=lx.value
		lex()
	fi
	p:=readstructdef(owner,typedefx,kwd)

when kunionsym then
	serror("Top-level union not allowed")

when krefsym then		!ref T
	lex()
	case lx.symbol
	when kprocsym,kfunctionsym,kmethodsym then	!function pointer being created
		serror("CAN'T DO REF PROC")
	else						!assume normal type
		t:=readtypespec(owner,0)
!*!		checkpackedtype(t)
	esac
	p:=createrefpackmode(owner,t,typedefx)

else
	serror("Bad type starter")
esac
return p
end

function readhostparams(ref unitrec lhs,int isfn)ref unitrec=		!READHOSTPARAMS
!hostfn name has been read
!lhs is not null when lhs.hostfn(...) has been used
!currently at hostfn symbol
int fnindex,oldinrp
ref unitrec p,q

fnindex:=lx.subcode
lex()
checksymbol(lbracksym)
lex()
oldinrp:=inreadprint
inreadprint:=0
q:=readslist(1,1)

checksymbol(rbracksym)
lex()
inreadprint:=oldinrp

if lhs then
	lhs^.nextunit:=q
	q:=lhs
!	insert(q,1,lhs)	
fi

p:=createunit1((isfn|j_callhostfn|j_callhostproc),q)
p^.opcode:=fnindex

return p
end

function readslist(int iscall=0,donulls)ref unitrec=		!READSLIST
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (j_ust comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a function-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a,b,c	)
!eg: (a		!
ref unitrec ulist,ulistx
int oldinparamlist

ulist:=ulistx:=nil

skipsemi()
if lx.symbol=rbracksym then		!empty list
	return ulist
fi

oldinparamlist:=inparamlist
inparamlist:=iscall

do
	skipsemi()
	case lx.symbol
	when commasym then
		if donulls then
!			addlistunit(&ulist,&ulistx,nullunit)
			addlistunit(&ulist,&ulistx,createunit0(j_null))
		else
			serror("null comma expr not allowed")
		fi
		lex()
	when rbracksym then
		if donulls then
			addlistunit(&ulist,&ulistx,nullunit)
		fi
		exit
	else
		addlistunit(&ulist,&ulistx,readexpression())
		if lx.symbol=commasym then
			lex()
			if lx.symbol=rbracksym then
				exit
			fi
		else
			if lx.symbol=semisym and nextlx.symbol=rbracksym then
				lex()
			fi
			exit
		fi
	esac
od
inparamlist:=oldinparamlist

return ulist
end

function readindex(ref unitrec p,int dot)ref unitrec=		!READINDEX
!at '['; dot=0/1 for a[]/a.[]
!syntax is::
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
ref unitrec q

lex()

do
	if ndollar>=maxdollarstack then
		serror("Too many nested a[$]")
	fi
	dollarstack[++ndollar]:=p
	q:=readexpression()
	--ndollar

	if q^.tag=j_makerange then		!convert into a discrete slice
		p:=createunit2((dot|j_dotslice|j_slice),p,q)
	else
		p:=createunit2((dot|j_dotindex|j_index),p,q)
	fi
	exit when lx.symbol<>commasym
	lex()
od
checksymbol(rsqsym)
lex()
return p
end

function readkeyindex(ref unitrec p,int dot)ref unitrec =		!READKEYINDEX
!at '{['; dot=0/1 for a{}/a.{}
!syntax is: {x}
!![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!!I don't need to allow indexing and section select within the same [...]
!!exit with symbol just after closing ]
ref unitrec q,r

lex()

q:=readexpression()
r:=nil
if lx.symbol=commasym then		!default value follows
	lex()
	r:=readexpression()
fi
checksymbol(rcurlysym)
lex()
return p:=createunit3((dot|j_dotkeyindex|j_keyindex),p,q,r)
end

function readdotsuffix(ref unitrec p)ref unitrec=		!READDOTSUFFIX
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
ref unitrec q
int t

while lx.symbol=dotsym do
	lex()
	switch lx.symbol
	when lsqsym then
		p:=readindex(p,1)
	when lcurlysym then
		p:=readkeyindex(p,1)
	when namesym then
		p:=createunit2(j_dot,p,createname(lx.symptr))
		lex()
	when opsym then			!ought to check whether op is allowed in this form
		p:=createunit1(lx.subcode,p)
!		evalmonop(p)
		lex()
	when lbracksym then			!use for variable attributes
		lex()
		p:=createunit2(j_dotattr,p,readexpression())
		checksymbol(rbracksym)
		lex()
	when ktypesym then			!.type, convert to .gettype
		case p^.tag
		when j_typeconst then			!int.type=>int

		when j_typeval then			!void/none.type => void/none
			p^.tag:=j_typeconst
		else
			p:=createunit1(j_gettype,p)
		esac
		lex()
	when khostfnsym then
		p:=readhostparams(p,1)

	when stdtypesym then
		t:=lx.subcode
		lex()
		checksymbol(lsqsym)
		lex()
		q:=readexpression()
		checksymbol(rsqsym)
		lex()
		case t
		when tu8,tu16,tu32 then
		else
			serror("Bad .type[]")
		esac
		p:=createunit2(j_byteindex,p,q)
		p^.mode:=t
	else
		serror("Unknown dot suffix")
	endswitch
od
return p
end

global function isconstexpr(ref unitrec p)int=		!ISCONSTEXPR
return p^.tag=j_const
end

function readconstexpr(ref strec owner,int needconst=1)ref unitrec=
!wrapper around readexpression(), which can evaluate if needed and also check for a constant::
!needconst = 0	Evaluate only; result need not be a constant
!needconst = 1	Evaluate only, and check result is a constant (error if resuling unit not a j_const)
!		Evaluate will also fold any constant expressions
!The above will return a unit representing the expression (a j_const if evaluation results
! in a constant value)
!needconst = 2  Is like 1, but returns the actual value, when only this is of interest
!Alternatively, getconstvalue() can be used to convert a const unit to its value
!The owner param is needed for the name resolution that is necessary.
!Should be positioned at first symbol of expression; returns positioned at following symbol
ref unitrec p

p:=readexpression()
return p
end

function readconstexprvalue(ref strec owner)int64=
unit p
p:=readexpression()

if p^.tag<>j_const then
	serror("RCEV:Not const expr")
fi
return getconstvalue(p)
end

function readconstint:int=		!READCONSTINT
!read expression that must yield a constant int value *now*; return value
int64 x

!keep it simple for now
if lx.symbol=intconstsym then
	x:=lx.value
	lex()
	return x
fi
!later can only arbitrary expressions, provided they can be evaluated in this pass
serror("Can't do complex expr")
return 0
end

proc readprocdef(ref strec procowner,int isglobal,fflang=0)=
!at 'proc' etc symbol; read proc def or declaration
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
int kwd,startline,closesym
ref strec stproc,q

kwd:=lx.symbol
stproc:=readprocdecl(procowner,isglobal,fflang)

checkequals()
lex()

startline:=getcurrline()
closesym:=checkbegin(0)

currproc:=stproc
nextavindex:=0

stproc^.code:=readblock(stproc)

if docstring^.length then
	currproc^.docstring:=docstring^.strptr
	gs_init(docstring)
fi

checkbeginend(closesym,kwd,startline)
stproc^.attribs.ax_equals:=1

!SEE IMPORTANT NOTES WITHIN PURGEPROC()
!*!purgeproc(currproc,0)
currproc:=nil
end

global function readprocdecl(ref strec procowner,int isglobal,fflang)ref strec=
!at 'proc'  or 'function' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

int kwd,varparams,try_level, prettype, nparams
ichar metadata, truename
ref strec pequiv, stproc, owner, paramlist,nameptr

kwd:=lx.symbol				!remember keyword
pequiv:=nil
metadata:=""
truename:=nil
varparams:=0
try_level:=0

lex()

if lx.symbol=stringconstsym then		!assume dll truename
	truename:=pcm_copyheapstring(lx.svalue)
	convlcstring(lx.svalue)
	lx.symptr:=addnamestr(lx.svalue)
else
	checksymbol(namesym)
fi

nameptr:=lx.symptr

stproc:=createprocdef(procowner,nameptr,(currimport|dllprocid|procid),truename)

if getscope(procowner)<>importscope and procowner^.nameid=typeid then
	addgenfield(nameptr)				!defining a method in a class
fi

owner:=stproc
currproc:=stproc

lex()
if lx.symbol=namesym and eqstring(lx.symptr^.name,"as") then
	lex()
	checksymbol(namesym)
	addalias(stproc,lx.symptr)
	lex()
fi

paramlist:=nil
prettype:=tvoid
nparams:=0

if lx.symbol=opsym and lx.subcode=j_lt then			!look for metadata
	if stproc^.nameid=dllprocid then
		serror("Metadata on dllproc")
	fi
	lex()
	checksymbol(stringconstsym)
	stproc^.metadata:=lx.svalue

	lex()
	unless lx.symbol=opsym and (lx.subcode=j_gt or lx.subcode=j_ge) then
		serror(""">"" expected")
	end
	if lx.symbol=opsym and lx.subcode=j_ge then	!>= must become just =
		lx.subcode:=j_eq
	else
		lex()
	fi
fi

if lx.symbol=lbracksym then		!possible params
	lex()
	if lx.symbol<>rbracksym then
		if fflang=0 or fflang=qlangff then
			varparams:=0
			paramlist:=readparams(stproc,nparams)
		else
			paramlist:=readparamsff(procowner,stproc,varparams,nparams)
		fi
		checksymbol(rbracksym)
	fi
	lex()
	if lx.symbol=colonsym or lx.symbol=sendtosym then
		lex()
		prettype:=readtypespec(owner,0)
!	elsif lx.symbol in typestarterset or lx.symbol=namesym then
	elsif typestarterset[lx.symbol] or lx.symbol=namesym then
		prettype:=readtypespec(owner,0)
	fi
elsif lx.symbol=colonsym or lx.symbol=sendtosym then
	lex()
	prettype:=readtypespec(owner,0)
fi

unless prettype<>tvoid or (kwd<>kfunctionsym and kwd<>kmethodsym) then		!function: no result given
!assign default variant result type
!should only do this for dynamic targets...
	prettype:=tvariant
endunless

if prettype<>tvoid and (kwd<>kfunctionsym and kwd<>kmethodsym) then		!proc: result given
	serror("Proc can't return value")
fi

if prettype<>tvoid and fflang<>0 and fflang<>qlangff then
	checkdlltype(prettype)
elsif prettype<>tvoid and (fflang=0 or fflang=qlangff) then
	checkunpackedtype(prettype)
fi

stproc^.paramlist:=paramlist

stproc^.attribs.ax_nparams:=nparams

stproc^.mode:=prettype

if lx.symbol=atsym then			!equivalence
	lex()
	checksymbol(namesym)
!	serror("JEQUIV?")
	lex()
	stproc^.attribs.ax_at:=1
fi

stproc^.code:=nil

case fflang
when clangff,windowsff,mlangff then
!	if procowner^.nameid<>dllmoduleid then
	if currimport=nil then
		cpl stproc^.name
		serror("FF should be in dll import")
	fi
else			!assume this language
	case procowner^.nameid
	when moduleid then
	when dllmoduleid then
		serror("Need FF specifier")
	esac
esac

if currimport then isglobal:=1 fi

stproc^.attribs.ax_global:=isglobal


stproc^.attribs.ax_varparams:=varparams
stproc^.attribs.ax_fflang:=fflang

if procowner=stmodule and \
	(stproc^.namelen=5 and eqstring(stproc^.name,"start")) or \
	(stproc^.namelen=4 and eqstring(stproc^.name,"main")) then
	stproc^.attribs.ax_global:=1
fi

return stproc
end

function readparams(ref strec owner,int &nparams)ref strec=
!positioned at first symbol after '('
ref strec stlist, stlistx, stname, d
int foptional, fbyref

[30]char str
stlist:=stlistx:=nil
foptional:=0
fbyref:=0
stname:=nil
nparams:=0

do
	switch lx.symbol
	when questionsym then
		if foptional then serror("??") fi
		lex()
		foptional:=1
	when addrsym then
		if fbyref then serror("& &") fi
		lex()
		fbyref:=1
	when namesym then
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
		stname^.mode:=tvariant
		addlistparam(&stlist,&stlistx,stname)

		lex()
		case lx.symbol
		when assignsym then
			lex()
			stname^.code:=readexpression()
			stname^.attribs.ax_equals:=1

		when opsym then
			if lx.subcode<>j_eq then
				serror("param op?")
			fi
			lex()
			foptional:=1
			stname^.code:=readexpression()
		esac
		if fbyref then stname^.attribs.ax_byrefmode:=1 fi
		if foptional then stname^.attribs.ax_optional:=1 fi

	when rbracksym then
		exit
	when commasym then
		fbyref:=foptional:=0
		lex()

	else
		cpl symbolnames[lx.symbol]
		serror("param")
	end switch
od

return stlist
end

function readparamsff(ref strec procowner,owner,int &varparams,&nparams)ref strec=
!positioned at first symbol after '('
!read list of params, return that list
!syntax is a list of names and/or types
!a & prefix means by ref, and applies to the name if present, otherwise to the type
!a ? prefix means optional, and works the same way
!each param can optionally be followed by a default value
!ref strec stlist, stlistx, stname, d
!int foptional, fbyref, pmode, fparam,firstparam,m
!ref unitrec pdefvalue

int pmode

if typestarterset[lx.symbol] then
	pmode:=readtypespec(procowner,0)
elsif lx.symbol=namesym then			!user type
	pmode:=readtypespec(procowner,0)
else
	serror("Readparams/type expected")
fi

if lx.symbol in [namesym,addrsym] then			!type+names
	return readparamsff_names(pmode,procowner,owner, varparams,nparams)
else
	return readparamsff_types(pmode,procowner,owner, varparams,nparams)
fi
end

function readparamsff_types(int pmode,ref strec procowner,owner,int &varparams,&nparams)ref strec=
!read types-only parameter list
!pmode is first parameter. At next symbol after type, should be , or ) or ...
!exit at ')'
ref strec stlist, stlistx, stname
[30]char str

stlist:=stlistx:=nil
varparams:=0
goto gottype

do
	if typestarterset[lx.symbol] then
		pmode:=readtypespec(procowner,0)
	elsif lx.symbol=namesym then			!user type
		pmode:=readtypespec(procowner,0)
	else
		serror("Readparams/type expected")
	fi

gottype::
	checkdlltype(pmode)
	++nparams
!	sprintf(&.str,"$%d",nparams)
	print @&.str,"$",,nparams
	stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
	adddef(owner,stname)
	storemode(owner,pmode,&stname^.mode)
	addlistparam(&stlist,&stlistx,stname)

	case lx.symbol
	when commasym then
		lex()					!prepare for next type
		if lx.symbol=ellipsissym then
			varparams:=1
			lex()
			checksymbol(rbracksym)
			exit
		fi
	when rbracksym then
		exit
	else
		serror("ffparams/types")
	esac
od
return stlist
end

function readparamsff_names(int pmode,ref strec procowner,owner,int &varparams,&nparams)ref strec=
!read types and names parameter list
!pmode is first parameter. At next symbol after type, should be name or &,
!which has been verified by caller
!exit at ')'
ref strec stlist, stlistx, stname
int m,fbyref

stlist:=stlistx:=nil
varparams:=0

do
	checkdlltype(pmode)

	do									!loop on names
		fbyref:=0
		if lx.symbol=addrsym then
			lex()
			fbyref:=1
		fi

		if lx.symbol<>namesym then serror("Params: name expected") fi
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
		if fbyref then
!			m:=createrefpackmode(pmode,0)
			m:=createrefpackmode(procowner,pmode,0)
			stname^.attribs.ax_byrefmode:=1
		else
			m:=pmode
		fi
		storemode(owner,m,&stname^.mode)
		addlistparam(&stlist,&stlistx,stname)
		lex()

!check if default value follows
		case lx.symbol
		when assignsym then
doexpr::
			lex()
			stname^.code:=readexpression()
			stname^.attribs.ax_equals:=1
			stname^.attribs.ax_optional:=1
		when opsym then
			if lx.subcode<>j_eq then serror("param op?") fi
			goto doexpr
		esac

!check if comma or ) follows
		case lx.symbol
		when commasym then
			lex()					!prepare for next type or name
		when rbracksym then
			exit all
		esac

!after comma: check whether type, param name or ... follows
		case lx.symbol
		when addrsym then			!param name next
		when ellipsissym then
			if lx.symbol=ellipsissym then
				varparams:=1
				lex()
				checksymbol(rbracksym)
				exit all
			fi
		when namesym then			!param name or type
			if nextlx.symbol in [namesym, addrsym] then		!assume usertype
!			if nextlx.symbol=namesym or nextlx.symbol=addrsym then		!assume usertype
				pmode:=readtypespec(procowner,0)
!				exit
			fi
		elsif typestarterset[lx.symbol] then
			pmode:=readtypespec(procowner,0)
			exit
		else
			serror("ffparams/types")
		esac
	od
od
return stlist
end

function readblock(ref strec owner)ref unitrec=
!positioned at first symbol of statement (which can be empty)
!read linear block or sequence of statements
!any 'end' etc is handled by caller
!return knode containing statement, or nil if not found (at 'end etc)
int lineno,globalflag
ref unitrec ulist,ulistx,p

skipsemi()

lineno:=lx.lineno
ulist:=ulistx:=nil

do
	switch lx.symbol
	when kstaticsym then
		lex()
		checksymbol(kvarsym)
		readvardef(owner,0,1,staticid)

	when kprocsym,kfunctionsym,kmethodsym then	!todo
		readprocdef(owner,0)
		globalflag:=0

	when kvarsym then
		readvardef(owner,0,0,frameid)

	when ktypesym then
		readtypedef(owner,0)

	when kconstsym then
		readconstdef(owner,0)

	when kclasssym,krecordsym then
		readclassdef(owner,globalflag)
		globalflag:=0

	when docstringsym then
		gs_strn(docstring,lx.svalue,lx.length)
		gs_line(docstring)
		lex()

	when kenumsym then		!enum
		lex()
		readenumtype(owner,0)

	when eofsym then
		cpl owner^.name
		serror("Unexpected EOF in proc")

	when rbracksym,kelsifsym,kelsesym,kuntilsym,kwhensym,
			kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
		exit

	when semisym then
		lex()

	else							!assume a statement
		p:=readexecstmt(owner)
		if int64(p)=1 or p=nil then
			serror("READEXEC RETURNED 1 OR NIL")
		fi
		addlistunit(&ulist,&ulistx,p)
	endswitch
od

return createunit1(j_block,ulist)
end

function readexecstmt(ref strec owner)ref unitrec=
!read single executable statement, and exit pointing to following symbol
!at first symbol of what could be start of statement
!any possible declarations/non-executable code already taken care of by caller
ref unitrec p,q
ref strec stname

switch lx.symbol
when namesym then
	case nextlx.symbol
	when colonsym then
		stname:=getduplnameptr(currproc,lx.symptr,labelid)
		adddef(currproc,stname)


		p:=createunit0(j_labeldef)
		p^.def:=stname
		p^.trylevel:=try_level
		stname^.offset:=try_level
		setnameptr(p)
		lex()
		lx.symbol:=semisym

	else
		p:=readstmtexpr(owner)
	esac
when kgotosym then
	p:=readgoto(owner)

when kifsym then
	p:=readif(owner)

when kunlesssym then
	p:=readunless(owner)

when kcasesym,kswitchsym then
	p:=readswitchcase(owner)

when kdocasesym,kdoswitchsym then
	p:=readswitchcase(owner)

when kforsym then
	p:=readfor(owner)

when kforallsym then
	p:=readforall(owner)

when ktosym then
	p:=readto(owner)

when kdosym then
	p:=readdo(owner)

when kwhilesym then
	p:=readwhile(owner)

when krepeatsym then
	p:=readrepeat(owner)

when kloopsym then
	p:=readloopcontrol(owner)

when kreturnsym then
	p:=readreturn(owner)

when kstopsym then
	p:=readstop(owner)

when kprintsym then
	p:=readprint(owner)

when kreadsym then
	p:=readread(owner)

when ktrysym then	!todo
	p:=readtry(owner)

when kraisesym then	!todo
	p:=readraise(owner)

when opsym then
	if lx.subcode=j_mul and nextlx.symbol=lbracksym then
		serror("KTERM?")
!		p:=readkterm()
	else
		p:=readstmtexpr(owner)
	fi

when kswapsym then			!swap using function syntax
	lex()
	checksymbol(lbracksym)
	lex()
	p:=readexpression()
	checksymbol(commasym)
	lex()
	q:=readexpression()
	checksymbol(rbracksym)
	lex()
	p:=createunit2(j_swap,p,q)

when lbracksym,incrsym,ksreadsym,ksreadlnsym,kapplsym then
	p:=readstmtexpr(owner)

when khostfnsym then
	p:=readhostparams(nil,0)

when kevalsym then
	lex()
	p:=createunit1(j_eval,readexpression())

!when kapplsym then
!	p:=readterm()

else
	cpl symbolnames[lx.symbol]
	serror("Stmt error")
endswitch

return p
end

function readstmtexpr(ref strec owner)ref unitrec=
!This reads an expression that is parsed as a statement, and therefore not
!expected to return a value. Usually it will not be a full expression, but one of::
!term:=expr		!assignment
!term op:=expr		!addto
!++term			!preincr
!term++			!postincr
!op:=term		!negto
!term(...)		!proc call
int opc
ref unitrec p,q,ulist,ulistx

if lx.symbol=opsym and nextlx.symbol=assignsym then		!negto combination
	opc:=lx.subcode
	case opc
	when j_sub then opc:=j_neg
	esac

	lex()				!skip :=
	lex()
	return createunit1(getoptocode(opc),readterm())			!op:=term
fi

p:=readterm()

!convert -x versions of incr ops to non-expr versions
	case p^.tag
	when j_preincrx then p^.tag:=j_preincr
	when j_predecrx then p^.tag:=j_predecr
	when j_postincrx then p^.tag:=j_postincr
	when j_postdecrx then p^.tag:=j_postdecr
	esac

!check some terminator symbols (others will cause a syntax error in the caller)
case lx.symbol
when assignsym then
	lex()				!skip :=
	q:=readexpression()
	p:=createunit2(j_assign,p,q)			!term:=expr
	return readcondsuffix(p)

when deepcopysym then
	lex()				!skip ::=
	q:=readexpression()
	p:=createunit2(j_deepcopy,p,q)			!term::=expr
	return readcondsuffix(p)
when opsym then
	if nextlx.symbol=assignsym then					!term op:=expr
		opc:=getoptocode(lx.subcode)
		lex()				!skip :=
		lex()				!first part of rhs expr
		return createunit2(opc,p,readexpression())			!term op:=expr
	else
		serror("op expr not allowed as statement")			!presumably a full expression;
	fi								! not allowed in this context
when kswapsym then
	lex()
	return createunit2(j_swap,p,readterm())			!term swap term
when commasym then
	ulist:=ulistx:=p
	inmultexpr:=1
	repeat
		lex()					!skip comma
		addlistunit(&ulist,&ulistx,readexpression())
	until lx.symbol<>commasym
	inmultexpr:=0

	p:=createunit1(j_multexpr,ulist)
	checksymbol(assignsym)
	lex()
	q:=readexpression()
	if lx.symbol<>commasym then
		serror("mult-expr expected after :=")
	fi
	ulist:=ulistx:=q
	repeat
		lex()					!skip comma
		addlistunit(&ulist,&ulistx,readexpression())
	until lx.symbol<>commasym
	q:=createunit1(j_multexpr,ulist)
	p:=createunit2(j_assign,p,q)
	return p
esac

case p^.tag
when j_assignx then
	p^.tag:=j_assign
when j_deepcopyx then
	p^.tag:=j_deepcopy
when j_callfn then
	p^.tag:=j_callproc
	p:=readcondsuffix(p)
when j_callapplfn then
	p^.tag:=j_callapplproc
	p:=readcondsuffix(p)
when j_callmfn then
	p^.tag:=j_callmproc
	p:=readcondsuffix(p)
else
!check the kind of term expected
	case p^.tag
	when j_preincr,j_predecr,j_postincr,j_postdecr,j_callproc,j_callapplproc,
		  j_sread,j_sreadln then
	else
		cpl jtagnames[p^.tag]
		serror("Expression not allowed as statement")
	esac
esac

return p			!assume p is a bona fide term
end

function readcondsuffix(ref unitrec p)ref unitrec=			!READCONDSUFFIX
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond

case lx.symbol
when kifsym,kwhensym then
	lex()
	return createunit2(j_if,readexpression(),createunit1(j_block,p))
when kunlesssym then
	lex()
	return createunit2(j_if, createunit1(j_notl,readexpression()),createunit1(j_block,p))
else
	return p
esac
return nil
end

function readif(ref strec owner)ref unitrec=	!READIF
!at 'if'
int line, kwd, lineno
ref unitrec pthen,pcond, plist,plistx, pelse, p, pelsif

line:=lx.lineno

kwd:=lx.symbol			!in case coming from elsecase etc

lex()
pcond:=readexpression()
skipsemi(); checksymbol(kthensym)
lex()

pthen:=readblock(owner)

if lx.symbol=kelsifsym then
	lineno:=lx.lineno
	plist:=plistx:=createunit2(j_elsif,pcond,pthen)

	while lx.symbol=kelsifsym do
		lineno:=lx.lineno
		lex()
		pcond:=readexpression()
		checksymbol(kthensym)
		lex()
		pthen:=readblock(owner)
		pelsif:=createunit2(j_elsif,pcond,pthen)
		pelsif^.lineno:=lineno
		addlistunit(&plist,&plistx,pelsif)

	od

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readblock(owner)
		checkend(kendsym,kwd,0)
		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase(owner))
	else
		pelse:=createunit0(j_block)
		checkend(kendsym,kwd,0)
		lex()
	esac

!	p:=createunit2(j_longif,createunit1(j_block,plist),pelse)
	p:=createunit2(j_longif,plist,pelse)
	p^.lineno:=line
	return p
fi

case lx.symbol
when kelsesym then		!get r=any else stmt or nil
	lex()
	pelse:=readblock(owner)
	checkend(kendsym,kwd)
	lex()
else
	pelse:=createunit0(j_block)
	checkend(kendsym,kwd)
	lex()
esac

p:=createunit3(j_if,pcond,pthen,pelse)
p^.lineno:=line
return p
end

function readgoto(ref strec owner,int gototag=j_goto)ref unitrec=	!READGOTO
ref strec d
ref unitrec p

if lx.subcode=1 then		!go used
	lex()
	checksymbol(ktosym)
fi
lex()

p:=readexpression()

return readcondsuffix(createunit1(gototag,p))
end

function readunless(ref strec owner)ref unitrec=	!READUNLESS
int line
ref unitrec pcond, pthen, pelse, p
line:=lx.lineno
lex()
pcond:=readexpression()
checksymbol(kthensym)
lex()

pthen:=readblock(owner)

if lx.symbol=kelsesym then
	lex()
	pelse:=readblock(owner)
else			!assume simple if-then
	pelse:=createunit0(j_block)
fi
checkend(kendsym,kunlesssym)
lex()
p:=createunit3(j_if,createunit1(j_notl,pcond),pthen,pelse)
p^.lineno:=line
return p
end

function readswitchcase(ref strec owner)ref unitrec=	!READSWITCHCASE
int line, kwd, opc, lineno
ref unitrec pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

line:=lx.lineno
kwd:=lx.symbol			!remember kcasesym etc
opc:=lx.subcode			!pick up tag: kcase etc

lex()
pexpr:=readexpression()		!index expression

pwhenlist:=pwhenlistx:=nil

skipsemi()
while lx.symbol=kwhensym do	!read list of when-then pairs
	lineno:=lx.lineno
	lex()
	pwhen:=pwhenx:=nil
	do
		p:=readexpression()
		p^.lineno:=lineno
		addlistunit(&pwhen,&pwhenx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od
	checksymbol(kthensym)
	lex()
	pthen:=readblock(owner)
!	pwhenthen:=createunit2(j_whenthen,q:=createunit1(j_block,pwhen),pthen)
	pwhenthen:=createunit2(j_whenthen,pwhen,pthen)
	pwhenthen^.lineno:=lineno
!	q^.lineno:=lineno
	addlistunit(&pwhenlist,&pwhenlistx,pwhenthen)
od

case lx.symbol
when kelsesym then		!get r=any else stmt or nil
	lex()
	pelse:=readblock(owner)

	checkend(kendsym,kwd)
	lex()
when kelsifsym then
	lx.symbol:=kwd
	pelse:=makeblock(readif(owner))
when kelsecasesym, kelseswitchsym then
	lx.symbol:=kwd
	pelse:=makeblock(readswitchcase(owner))
else
	pelse:=makeblock(nil)
	checkend(kendsym,kwd)
	lex()
esac

p:=createunit3(opc,pexpr,pwhenlist,pelse)
p^.lineno:=line
return p
end

function readstop(ref strec owner)ref unitrec=	!READSTOP
ref unitrec p
int i
lex()
if exprstarterset[lx.symbol] then
	p:=createunit1(j_stop,readexpression())
else
	p:=createunit0(j_stop)
fi
return readcondsuffix(p)
end

function readreturn(ref strec owner)ref unitrec=	!READRETURN
ref unitrec p

lex()
if exprstarterset[lx.symbol] then
	p:=createunit1(j_return,readexpression())
else
	p:=createunit0(j_return)
fi

return readcondsuffix(p)
end

function readdo(ref strec owner)ref unitrec=	!READDO
ref unitrec p
int line

line:=lx.lineno
lex()
p:=readblock(owner)
checkend(kendsym,kdosym)
lex()
p:=createunit1(j_do,p)
p^.lineno:=line
return p
end

function readto(ref strec owner)ref unitrec=	!READTO
int line,id
ref unitrec p, pcount, pbody

line:=lx.lineno
lex()
pcount:=readexpression()
checksymbol(kdosym)
lex()
pbody:=readblock(owner)
checkend(kendsym,ktosym,kdosym)
lex()
id:=frameid
if owner^.nameid<>procid then id:=staticid fi

p:=createunit3(j_to,pcount,pbody,createname(getavname(owner,id)))
p^.lineno:=line
return p
end

function readwhile(ref strec owner)ref unitrec=	!READWHILE
int line,id
ref unitrec pcond, pa, pb, pc, pbody, p

line:=lx.lineno
lex()
pcond:=readexpression()

checksymbol(kdosym)
lex()
pbody:=readblock(owner)
checkend(kendsym,kwhilesym,kdosym)
lex()

p:=createunit2(j_while,pcond,pbody)
p^.lineno:=line
return p
end

function readrepeat(ref strec owner)ref unitrec=	!READREPEAT
int line
ref unitrec pbody, pcond, p

line:=lx.lineno
lex()
pbody:=readblock(owner)
checksymbol(kuntilsym)
lex()
pcond:=readexpression()
p:=createunit2(j_repeat,pbody,pcond)
p^.lineno:=line
return p
end

function readloopcontrol(ref strec owner)ref unitrec=	!READLOOPCONTROL
int opc
ref unitrec p

opc:=lx.subcode
lex()
if lx.symbol=namesym and eqstring(lx.symptr^.name,"all") then
	lex()
	p:=createunit1(opc,createconstunit(0,tint))

elsif exprstarterset[lx.symbol] then
	p:=createunit1(opc,readconstexpr(owner,1))
else
	p:=createunit0(opc)
fi
return readcondsuffix(p)
end

function readprint(ref strec owner)ref unitrec=	!READPRINT
int oldinreadprint, opc, isfprint, fshowname, length
ref unitrec pformat, pdev, printlist,printlistx, p,q
ref strbuffer expr

ichar s

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode

case opc
when j_fprint,j_fprintln,j_cprint,j_cprintln then
	isfprint:=1
else
	isfprint:=0
esac

lex()

printlist:=printlistx:=nil
pformat:=pdev:=nil

if lx.symbol=atsym then
	lex()
	pdev:=readexpression()
	if lx.symbol=commasym then lex() else goto finish fi
fi
if isfprint then
	if not exprstarterset[lx.symbol] and opc=j_cprintln then
		goto finish
	fi
	pformat:=readexpression()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if not exprstarterset[lx.symbol] then
	goto finish
fi

do
	if lx.symbol=commasym then		!assume extra comma, meaning nogap
		addlistunit(&printlist,&printlistx, createunit0(j_nogap))
	else

		fshowname:=0
		if lx.symbol=opsym and lx.subcode=j_eq then
			fshowname:=1
			lex()
		fi

		p:=readexpression()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(j_fmtitem,p,readexpression())
		fi
		if fshowname then
!			serror("CAN'T DO PRINT '='")
			expr:=strexpr(p)
			length:=expr^.length
			strbuffer_add(expr,"=?",-1)
!			vx_addtostring_imm(expr,"=?")		!? becomes terminator
			s:=expr^.strptr
			iconvucn(s,length)
			(s+length+1)^:=0					!? => terminator

			addlistunit(&printlist,&printlistx,q:=createstringconstunit(s,-1))
		fi
		addlistunit(&printlist,&printlistx,p)
	fi
	if lx.symbol<>commasym then exit fi
	lex()
od

finish::
inreadprint:=oldinreadprint
if opc=j_print and printlist=nil then
	serror("No print items")
fi
if opc=j_fprint and printlist=nil and pformat=nil then
	serror("No print items")
fi
if opc=j_cprint and printlist=nil and pformat=nil then
	serror("No cprint items")
fi

if isfprint then
	if pformat=nil and opc<>j_cprintln then
		serror("No fmt str")
	fi
	return createunit3(opc,pdev,pformat,printlist)
else
	return createunit2(opc,pdev,printlist)
fi
end

function readread(ref strec owner)ref unitrec=	!READREAD
int oldinreadprint,opc
ref unitrec pformat, pdev, readlist, readlistx, p

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lex()

readlist:=readlistx:=nil
pformat:=pdev:=nil

if lx.symbol=atsym then
	if opc=j_read then
		serror("@ on read")
	fi
	lex()
	pdev:=readexpression()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if not exprstarterset[lx.symbol] then
	goto finish
fi

do
	p:=readexpression()
	if lx.symbol=colonsym then
		lex()
		p:=createunit2(j_fmtitem,p,readexpression())
	fi
	addlistunit(&readlist,&readlistx,p)
	if lx.symbol<>commasym then exit fi
	lex()
od

finish::
inreadprint:=oldinreadprint
if opc=j_read and readlist=nil then
	serror("No read items")
fi

return createunit2(opc,pdev,readlist)
end

function readtry(ref strec owner)ref unitrec=	!READTRY
ref unitrec ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
!const maxexceptions=20
![maxexceptions]int exlist
!int nexceptions

++try_level
lex()

ptry:=readblock(owner)
pexceptlist:=pexceptlistx:=nil			!list of j_except items

while lx.symbol=kexceptsym do
	lex()
	exlist:=exlistx:=nil				!list of exception codes for this 'except'
	do
		addlistunit(&exlist,&exlistx,readconstexpr(owner))
		if lx.symbol<>commasym then exit fi
		lex()
	od
	checksymbol(kthensym)
	lex()
	px:=readblock(owner)
	addlistunit(&pexceptlist,&pexceptlistx,createunit2(j_except,exlist,px))
od
checkend(kendsym,ktrysym)
lex()

--try_level

return createunit2(j_try,ptry,pexceptlist)
end

function readraise(ref strec owner)ref unitrec=	!READRAISE
ref unitrec p

lex()
p:=readexpression()
return createunit1(j_raise,p)
end

function readfor(ref strec owner)ref unitrec=	!READFOR
!on 'for'; syntax is::
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

int line, opc, down
ref unitrec pstep, pvar, pcond, pfrom, pto, pelse, prange, prangex, pautovar, pbody, p
ref strbuffer S

line:=lx.lineno
lex()			!skip 'for'
pvar:=readterm()

if pvar^.tag<>j_name then
	serror("For: name expected")
fi

opc:=j_forup
pstep:=nil
pcond:=nil

if lx.symbol=opsym then	!assume in/inrev
	if lx.subcode=j_inrev then
		down:=j_fordown
	elsif lx.subcode<>j_in then
		serror("in/inrev expected")
	fi
	lex()
	prange:=readexpression()

!now split prange into from/to parts
	pfrom:=getrangelwbunit(prange)
	pto:=getrangeupbunit(prange)

else
	if lx.symbol=assignsym then
		lex()
		pfrom:=readexpression()
	else
		pfrom:=createconstunit(1,tint)
	fi
	checksymbol(ktosym)
	opc:=(lx.subcode=1|j_fordown|j_forup)
	lex()
	pto:=readexpression()

	if lx.symbol=kbysym then
		if opc=j_fordown then
			serror("downto/by")
		fi
		opc:=j_forstep
		lex()
		pstep:=readconstexpr(owner,0)
		if pstep^.tag=j_const and pstep^.value=1 then		!by 1
			opc:=j_forup
			pstep:=nil
		elsif pstep^.tag=j_const and pstep^.value=-1 then	!by -1
			opc:=j_fordown
			pstep:=nil
		fi

	else
		pstep:=nil
	fi
fi

if lx.symbol=kwhensym then
	lex()
	pcond:=readexpression()
fi
checksymbol(kdosym)
lex()
pbody:=readblock(owner)

if lx.symbol=kelsesym then
	lex()
	pelse:=readblock(owner)
else
	pelse:=nil
fi
checkend(kendsym,kforsym,kdosym)
lex()

if pcond<>nil then
	pbody:=makeblock(createunit2(j_if,pcond,pbody))
fi

!deal with complex limit
pautovar:=nil
unless pto^.tag=j_const or (pto^.tag=j_name and pto^.def^.attribs.ax_frame) then
	if opc=j_forstep then
		serror("for: 'by' uses complex limit")
	fi
	pautovar:=createname(getavname(owner))
end

!have the following elements::
!pvar		never nil
!pfrom		never nil
!pto		never nil
!pstep		can be nil
!pbody		never nil (might be empty block)
!pelse		can be nil
!pautovar	can be nil

!suggest layout is::

! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
! c = [pautovar]

pvar^.nextunit:=pfrom
pfrom^.nextunit:=pto
pto^.nextunit:=pstep
pbody^.nextunit:=pelse

p:=createunit3(opc,pvar,pbody,pautovar)
p^.lineno:=line
return p
end

function readforall(ref strec owner)ref unitrec=	!READFORALL
!on 'forall'; syntax is::
! forall term|, in/inrev expr|, [when expr] do stmts [else stmts] end/od
int opc, line, isforall
ref unitrec pindex, pvar, pcond, plist, pbody, pelse,p, pfor, pautovar

line:=lx.lineno
opc:=lx.subcode
isforall:=opc=j_forall

lex()			!skip 'forall'
pvar:=readterm()
pindex:=nil

if lx.symbol=commasym then
	lex()
	pindex:=pvar
	pvar:=readterm()
fi

if pvar^.tag<>j_name then
	serror("forall not name")
fi

pcond:=nil

if lx.symbol=opsym and (lx.subcode=j_in or lx.subcode=j_inrev) then
	if lx.subcode=j_inrev then opc:=(opc=j_forall|j_forallrev|j_foreachrev) fi
	lex()
	plist:=readexpression()
else
	serror("in/inrev expected")
fi

if lx.symbol=kwhensym then
	lex()
	pcond:=readexpression()
fi
checksymbol(kdosym)
lex()
pbody:=readblock(owner)

if lx.symbol=kelsesym then
	lex()
	pelse:=readblock(owner)
else
	pelse:=nil
fi
checkend(kendsym,kforallsym,kdosym)
lex()

if pindex=nil then		!need to add auto-var
	pindex:=createname(getavname(owner))
fi

if pcond<>nil then
	pbody:=createunit2(j_if,pcond,pbody)
	pbody^.lineno:=line
fi

pautovar:=createname(getavname(currproc))		!stores limit

!now have the following elements
!pindex		user or auto
!pvar
!plist
!pbody		never nil
!pelse		can be nil
!plimitav	can be nil when limit is const
!plistav	can be nil

!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, pvar, plist
! b = pbody, [pelse]
! c = pautovar

pindex^.nextunit:=pvar
pvar^.nextunit:=plist
pbody^.nextunit:=pelse

pfor:=createunit3(opc, pindex, pbody, pautovar)
pfor^.lineno:=line
return pfor
end

global proc readtypedef(ref strec owner,int isglobal=0)=
!at 'type' symbol
ref strec sttype,stname
int t,m

lex()
checksymbol(namesym)
stname:=lx.symptr

lex()
checkequals()
lex()

sttype:=nil

if sttype=nil then
	sttype:=getduplnameptr(owner,stname,typeid)
	adddef(owner,sttype)
	m:=createusertype(sttype)
else
	m:=sttype^.mode
fi

t:=readtypespec(sttype,m)		!should return filled-in version of m

sttype^.attribs.ax_global:=isglobal

sttype^.mode:=t
end

function readstructdef(ref strec owner,int typedefx, kwd)int=
!read struct def, and return typespec corresponding
!typedefx is nil, or part-populated moderec for explicit usertype
!positioned at possible begin symbol (or at first declaration in the record)
int m,startline,closesym, t
ref strec recordowner, d

recordowner:=owner
if not typedefx then			!informal declaration
	if lx.symbol=namesym then		!name provided
		owner:=getduplnameptr(owner,lx.symptr,typeid)
		lex()
		checkequals()
		lex()
	else
		owner:=getduplnameptr(stmodule,addnamestr(nextautotype()),typeid)
	fi
	adddef(recordowner,owner)
	checksymbol(lbracksym)
	lex()
else
	owner:=ttnamedef[typedefx]
	startline:=getcurrline()
	closesym:=checkbegin(1)
fi

m:=createrecordmode(owner,tstruct,typedefx)
owner^.mode:=m

unionstr_clear(&unionstring)
unionstr_clear(&unionpend)

do
	case lx.symbol
	when kstructsym,kunionsym then
		unionstr_append(&unionpend,(lx.symbol=kstructsym|'S'|'U'))
		unionlastvar:=nil
		lex()
	when kendsym then
		if unionstring.ulength then
			checkend(kendsym,(unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			lex()
			if unionlastvar=nil or unionpend.ulength then
				serror("Empty union group")
			fi
			case unionstr_last(&unionlastvar^.uflags)
			when 'E','*' then
			else
				unionstr_append(&unionlastvar^.uflags,'*')
			esac
			unionstr_append(&unionlastvar^.uflags,'E')
			unionstring.ulength--
		else
			exit
		fi
	when semisym then
		lex()
	else
		if typestarterset[lx.symbol] or lx.symbol=namesym then
			t:=readtypespec(owner,0)
			readstructfields(owner,t)
		else
			exit
		fi
	esac
od

if not typedefx then
	checksymbol(rbracksym)
	lex()
else
	checkbeginend(closesym,kwd,startline)
fi

return m
end

global proc readstructfields(ref strec owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
int nvars
ref strec stname

nvars:=0
while lx.symbol=namesym do

	stname:=getduplnameptr(owner,lx.symptr,fieldid)
!	stname^.mode:=m
	storemode(owner,m,&stname^.mode)
	++nvars

	if unionpend.ulength then
		unionstr_copy(&stname^.uflags,&unionpend)
		unionstr_concat(&unionstring,&unionpend)
		unionstr_clear(&unionpend)
	else
		unionstr_clear(&stname^.uflags)
	fi
	unionlastvar:=stname			!filled in from outside with 'E' codes
	if getscope(owner)<>importscope then
		addgenfield(lx.symptr)
	fi

	adddef(owner,stname)

	lex()

	if lx.symbol=atsym then
		lex()
		stname^.attribs.ax_at:=1
		stname^.equiv:=readequivfield(owner)

	elsif lx.symbol=datsym then
		lex()
		checksymbol(intconstsym)
		case lx.value
		when 1,2,4,8 then
			stname^.attribs.ax_align:=lx.value
		when 0 then
			stname^.attribs.ax_align:=255
		else
			serror("@@ bad align")
		esac
		lex()	
	fi

	if lx.symbol<>commasym then
		exit
	fi
	lex()
od

if nvars=0 then
	serror("No fields declared")
fi
end

global proc readtabledef(ref strec owner,int isglobal=0)=
!at 'tabledata' symbol
int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym,vartype
ref unitrec plower
ichar enumtypename
ref strec stvar,stenum,stgen
const maxcols=20
[maxcols]ref strec varnameptrs
[maxcols]unit plist,plistx
const maxrows=500
[maxrows]int enumvalues
!int nenums

lex()
enums:=0						!whether there is an enums column
enumtypename:=nil

if lx.symbol=lbracksym then		!tabledate(...) read enum type
	enums:=1
	lex()
	if lx.symbol=namesym then		!named type
		enumtypename:=lx.symptr^.name
		lex()
	fi					!else unnamed type (just named constants)
	checksymbol(rbracksym)
	lex()
fi

nextenumvalue:=1
nrows:=0			!number of data rows appearing
ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
while lx.symbol=namesym do
	if ++ncols>maxcols then
		serror("tabledata/too many columns")
	fi
	varnameptrs[ncols]:=lx.symptr

	lex()
	if lx.symbol=commasym then
		lex()
	else
		exit
	fi
od

checkequals()
lex()					!skip =

skipsemi()
startline:=getcurrline()
closesym:=checkbegin(0)

skipsemi()
firstval:=lastval:=0

for i:=1 to ncols do
	plist[i]:=plistx[i]:=nil
od

intabledata:=1
do			!loop per row
	skipsemi()
	checksymbol(lbracksym)
	lex()
	if ++nrows>maxrows then
		serror("tabledata:too many rows")
	fi

	if enums then
		checksymbol(namesym)
		stgen:=lx.symptr				!generic symbol entry
		tabledataname:=stgen^.name		!allow to be picked up by $ lx.symbol
		lex()
		if lx.symbol=opsym and lx.subcode=j_eq then
			lex()
			nextenumvalue:=readconstint()
		fi
		enumvalues[nrows]:=nextenumvalue

		stenum:=getduplnameptr(owner,stgen,constid)
		stenum^.mode:=tint
		stenum^.code:=createconstunit(nextenumvalue,tint)
		stenum^.attribs.ax_global:=isglobal
		adddef(owner,stenum)

		if nrows=1 then firstval:=nextenumvalue fi
		lastval:=nextenumvalue

		++nextenumvalue
		if ncols then				!comma always expected
			checksymbol(commasym)		!check it
		fi
		lex()
	fi

	for i:=1 to ncols do
		addlistunit(&plist[i],&plistx[i],readexpression())
		if i=ncols then
			checksymbol(rbracksym)
		else
			checksymbol(commasym)
		fi
		lex()
	od

	if lx.symbol<>commasym then exit fi
	lex()					!should be ( for next entry
	if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
od

intabledata:=0

skipsemi()
checkbeginend(closesym,ktabledatasym,startline)

!Here, I have::

!enum				1 means enum 0th column present, 0 means not
!ncols				0, or number of actual expression columns
!nrows				Number of data rows
!enumtypename			"", or enum user type name to be created for enum names/values

!varnameptrs[1..ncols]		!Names of the list variables ([]strec]
!varlisttypes[1..ncols]		!Type of each list (or 0 if not provided)
!varelemttypes[1..ncols]	!Type of each element (or 0 if not provided)
!plist[1..ncols]		Each entry is a list of expression units for the column

!enumnames[1..nrows]	When enum=1, list of names/values in 0th column
!enumvalues[1..nrows]

if nrows=0 then serror("No table data") fi

!for each variable, add a vardef initialised to the list
!add the decls for the vars
vartype:=tvariant

for i:=1 to ncols do

	stvar:=getduplnameptr(owner,varnameptrs[i],staticid)
	if enums then
		plower:=createconstunit(enumvalues[1],tint)
	else
		plower:=nil
	fi

	stvar^.code:=createunit2(j_makelist,plist[i],plower)

	stvar^.attribs.ax_global:=isglobal

	adddef(owner,stvar)
od
end

global proc readclassdef(ref strec owner,int isglobal)=
!at 'class' symbol
!read enough of the class to be able to generate export data
int kwd, baseclass, m, startline, closesym, mrec, normalexit
ref strec nameptr, sttype, newd, d,e

kwd:=lx.symbol

lex()
checksymbol(namesym)
nameptr:=lx.symptr

lex()
baseclass:=0
if lx.symbol=lbracksym then
	lex()
	baseclass:=readtypespec(owner,0)
	checksymbol(rbracksym)
	lex()
fi

checkequals()
lex()

sttype:=getduplnameptr(owner,nameptr,typeid)

adddef(owner,sttype)
m:=createusertype(sttype)

mrec:=createrecordmode(owner,trecord,m)
sttype^.mode:=mrec

if baseclass then
	if baseclass>0 then serror("baseclass?") fi
	if nbaseclasses>=255 then
			serror("Too many base classes")
	fi
	++nbaseclasses
	storemode(owner,baseclass,&baseclasstable[nbaseclasses])
	sttype^.attribs.ax_baseclass:=nbaseclasses
	baseclassdef[nbaseclasses]:=sttype
fi

closesym:=checkbegin(1)

startline:=getcurrline()

readclassbody(sttype,kwd)

checkbeginend(closesym,kwd,startline)

sttype^.attribs.ax_global:=isglobal
end

proc readclassbody(ref strec owner,int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
!classkwd=kclasssym or krecordsym
!int kwd

doswitch lx.symbol
when kconstsym then
	readconstdef(owner,0)
when kvarsym then
	readrecordfields(owner)
when kmethodsym,kfunctionsym,kprocsym then
!	kwd:=lx.symbol
	readprocdef(owner,0)

when kclasssym then
	lex()
	serror("CLASS CLASS")
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
else
	if typestarterset[lx.symbol] then
		serror("Packed types not allowed in class")
	else
		serror("Unknown class decl")
	fi
enddoswitch
end

function readenumtype(ref strec owner,int typedefx,isglobal=0)int=		!READENUMTYPE
!read enum def, and return typespec corresponding
!typedefx is nil, or an existing, but not yet filled-in, moderec
!positioned at possible begin symbol (or at first declaration in the record)
!This is because it can be called in two ways:
!1: type name = enum <begin>...	Formal record definition
!2: enum [name=]<begin>...		Informal definition (where strictly only (...) allowed)
ref strec enumowner, stname, nameptr
int isanon, index, startline, closesym

enumowner:=owner			!owner of enum typeid
isanon:=0
if not typedefx then			!informal declaration
	if lx.symbol=namesym then		!name provided
!		stname:=getduplnameptr(owner,lx.symptr^.name,typeid)
		stname:=getduplnameptr(owner,lx.symptr,typeid)
		owner:=stname
		lex()
		checkequals()
		lex()
		adddef(enumowner,owner)
	else
		isanon:=1
	fi
	checksymbol(lbracksym)
	lex()
else
	owner:=ttnamedef[typedefx]
	startline:=getcurrline()
	closesym:=checkbegin(1)
fi

!now loop reading enum items
index:=1

while lx.symbol=namesym do
	nameptr:=lx.symptr
	lex()
	if lx.symbol=opsym and lx.subcode=j_eq then	!= follows
		lex()
		index:=readconstint()
	fi

	if not isanon then
		stname:=getduplnameptr(owner,nameptr,enumid)
		stname^.index:=index
		stname^.mode:=tint

		adddef(owner,stname)
	else
		stname:=getduplnameptr(enumowner,nameptr,constid)
		stname^.code:=createconstunit(index,tint)
		stname^.mode:=tint
		adddef(enumowner,stname)
	fi
	++index

	stname^.attribs.ax_global:=isglobal

	if lx.symbol<>commasym then exit fi
	lex()
od

if not typedefx then
	checksymbol(rbracksym)
	lex()
else
	checkbeginend(closesym,kenumsym,startline)
fi

if not isanon then
	return createenummode(owner,typedefx)
else
	return tvoid
fi
end

global proc readrecordfields(ref strec owner)=
!positioned at 'var'
!read vars inside class or record for one line of record body
int m, nvars
ref strec stname

lex()
m:=tvariant

nvars:=0
while lx.symbol=namesym do

	stname:=getduplnameptr(owner,lx.symptr,fieldid)
	stname^.mode:=m
	++nvars
	if getscope(owner)<>importscope then
		addgenfield(lx.symptr)
	fi

	adddef(owner,stname)

	lex()

	if lx.symbol=atsym then
		lex()
		stname^.attribs.ax_at:=1
		stname^.equiv:=readequivfield(owner)
	fi

	if lx.symbol<>commasym then
		exit
	fi
	lex()
od

if nvars=0 then
	serror("No fields declared")
fi
end

proc readimportmodule(ref strec owner)=
!at 'importmodule' symbol
int isnew,startline,closesym
ref strec d,stname,stname0

lex()
if lx.symbol=stringconstsym then
	stname:=addnamestr(lx.svalue)
else
	checksymbol(namesym)
	stname:=lx.symptr
fi
!CPL "IMP3",stname^.name

lex()
checkequals()
lex()

!stname points to a nullid symbol
!check whether this module already exists

isnew:=1
d:=stname^.nextdupl
while d do
	if d^.nameid=dllmoduleid then
		stname:=d
		isnew:=0
		exit
	fi
	d:=d^.nextdupl
od

if isnew then			!new
	stname:=getduplnameptr(stprogram,stname,dllmoduleid)
	if eqstring(stname^.name,"sys") then
		stsysmodule:=stname
	fi
	adddef(stprogram,stname)
	if ndlltable>=maxdlllib then
		serror("Too many DLL libs")
	fi
	dlltable[++ndlltable]:=stname^.name
	dllsttable[ndlltable]:=stname
	stname^.attribs.ax_dllindex:=ndlltable
!else

fi

startline:=getcurrline()
closesym:=checkbegin(0)

currimport:=stname

readimportbody(stmodule)
currimport:=nil

checkbeginend(closesym,kimportmodulesym,startline)
end

proc readimportbody(ref strec owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
int lineno,fflang

lineno:=lx.lineno

do
	skipsemi()

	switch lx.symbol
	when kfflangsym then
		fflang:=lx.subcode
		lex()
		case lx.symbol
		when kprocsym,kfunctionsym,kmethodsym then
			readprocdecl(owner,0,fflang)
		esac	

	when kprocsym,kfunctionsym,kmethodsym then
		readprocdecl(owner,0,0)

	when kvarsym then
		readvardef(owner,0,0,staticid)

	when ktypesym then
		readtypedef(owner,0)

	when kconstsym then
		readconstdef(owner,0)

	when kclasssym,krecordsym then
		readclassdef(owner,0)

	when eofsym then
		exit

	when kendsym then
		exit
	else
		PS1("symbol")
		serror("Not allowed in importmodule")
	endswitch
od
end

function createprocdef(ref strec owner, stname,int id,ichar truename=nil)ref strec=
ref strec d,e
ref procrec pp
ref unitrec u

	d:=getduplnameptr(owner,stname,id)
!	adddef_nodupl(owner,d)
	adddef(owner,d)
	addtoproclist(d)

	d^.index:=0

	if id=dllprocid then
		if ndllproctable>=maxdllproc then
			serror("Too many DLL procs")
		fi
		++ndllproctable
		if not truename then
			truename:=d^.name
		fi
		d^.truename:=truename
		dllproctable[ndllproctable].name:=truename

		dllproctable[ndllproctable].dllindex:=currimport^.attribs.ax_dllindex
		d^.index:=ndllproctable
	else					!scan for ext-proc chains
		e:=stname^.nextdupl
		while e do
			if e^.nameid=procid and e^.callchain then
				u:=e^.callchain
				repeat
					if u^.tag<>j_name then serror("CPD1") fi
					u^.def:=d
					u:=u^.c
				until u=nil


			fi
			e:=e^.nextdupl
		od
	fi

return d
end

function readequivfield(ref strec owner)ref strec=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
ref strec p,d

!lex()
checksymbol(namesym)
d:=lx.symptr
lex()

p:=owner^.deflist
while p do
	if eqstring(p^.name,d^.name) then
		return p
	fi

	p:=p^.nextdef
od
cpl d^.name
serror("Can't find @ field")
return nil
end

function testconstruct(ref unitrec p)ref unitrec=
!a callfn (and callproc?) unit has just been made
!if the left side is a type name, then convert to Convert/Makeconstr
ref unitrec q,paramlist,r
ref strec d
int mode

q:=p^.a
d:=q^.def
unless q^.tag=j_name and d^.nameid=typeid then
	return p
end unless

paramlist:=p^.b

mode:=d^.mode
p^.tag:=j_makeconstr		!turn the original call to a makeconstr
p^.a:=paramlist
p^.b:=nil

r:=createunit1(j_convert,p)
r^.mode:=mode

return r
end

function readapplyop(int inexpr)ref unitrec=
ref unitrec p,a,b

lex()
checksymbol(lbracksym)
lex()
p:=readexpression()
checksymbol(commasym)
lex()
a:=readexpression()
b:=nil

if lx.symbol=commasym then
	lex()
	b:=readexpression()
fi
checksymbol(rbracksym)
lex()

return createunit3((inexpr|j_applyopx|j_applyop),p,a,b)
end

proc readapplprocs(ref strec owner)=
!at 'applproc'

	int nprocs,n
	ref strec stname

	lex()

	nprocs:=0
	while lx.symbol=namesym do
		++nprocs

		stname:=findapplproc(lx.symptr.name)
		if stname=nil then
			stname:=getduplnameptr(owner,lx.symptr,applprocid)
			stname^.attribs.ax_global:=1
			adddef(owner,stname)
			addapplproc(stname)
!		else
!			adddef(owner,stname)
		fi

		lex()

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nprocs=0 then
		serror("No procs declared")
	fi
!CPL "RHP2"
end

function defineapplproc:ref strec d=
!current lx symbol is a name; turn that into a defined appl proc
!and return that non-generic strec

	if currproc=nil then
		serror("appl. outside of proc")
	fi

	d:=findapplproc(lx.symptr.name)
	if d then return d fi

	d:=getduplnameptr(currproc,lx.symptr,applprocid)
!	d^.attribs.ax_global:=1
	adddef(currproc,d)
	addapplproc(d)

	return d
end

function findapplproc(ichar name)ref strec=
	for i to napplproctable do
		if eqstring(applproctable[i].name,name) then
			return applproctable[i]
		fi
	od
	return nil
end

proc addapplproc(ref strec d)=
	if napplproctable>=maxapplproc then
		serror("Too many appl procs")
	fi

	applproctable[++napplproctable]:=d
	d.index:=napplproctable
end

function readapplcall:unit p=
!at 'appl'
	ref strec d

	lex()
	checksymbol(dotsym)
	lex()
	checksymbol(namesym)

!CPL "RHC",=CURRPROC.NAME
	d:=defineapplproc()

	p:=createname(d)
	p.lineno:=lx.lineno
	if nextlx.symbol=lbracksym then
		p^.def:=d
	fi
	p:=createunit1(j_callapplfn, p)
	lex()
	checksymbol(lbracksym)
	return p
end
=== qc_lib.m 31/38 ===
import msys
import mlib
import clib
import oslib

import var_types
import var_decls
import qc_support
import pq_common
import qc_tables
import qc_lex

int autotypeno=0
int currlineno
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

tabledata []int opc_codes, []ichar opc_names =
	(j_add,		"+"),
	(j_sub,		"-"),
	(j_mul,		"*"),
	(j_div,		"/"),
	(j_idiv,	"%"),
	(j_neg,		"-"),
	(j_eq,		"="),
	(j_ne,		"<>"),
	(j_lt,		"<"),
	(j_le,		"<="),
	(j_gt,		">"),
	(j_ge,		">="),
	(j_iand,	"iand"),
	(j_ior,		"ior"),
	(j_ixor,	"ixor"),
	(j_inot,	"inot"),
	(j_shl,		"<<"),
	(j_shr,		">>"),
	(j_andl,	"and"),
	(j_orl,		"or"),

	(j_notl,	"not"),
	(j_min1,	"min"),
	(j_max1,	"max"),

	(j_addto,	"+:="),
	(j_subto,	"-:="),
	(j_multo,	"*:="),
	(j_divto,	"/:="),
	(j_negto,	"-:="),
	(j_shlto,	"<<:="),
	(j_shrto,	">>:="),

	(j_preincrx,	"++"),
	(j_postincrx,	"++"),
	(j_predecrx,	"--"),
	(j_postdecrx,	"--"),

	(0,		"")
end

function newstrec:ref strec=
ref strec p
p:=pcm_alloc(strec.bytes)
memset(p,0,strec.bytes)

!p:=pcm_allocz(strec.bytes)

p^.lineno:=lx.lineno
p^.attribs.ax_moduleno:=currmoduleno
return p
end

global proc initqclib=
int i

!translate into an instant lookup format
for i:=1 to oplist.len do
	jtagpriotable[oplist[i]]:=oppriolist[i]
od

!for i:=1 to Dexecstarterset do execstarterset[i]:=1 od

for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od
for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od

hostlvset[host_iconvlc]:=1
hostlvset[host_iconvuc]:=1

condopset[j_eq]:=1
condopset[j_ne]:=1
condopset[j_lt]:=1
condopset[j_le]:=1
condopset[j_ge]:=1
condopset[j_gt]:=1

end

global function getduplnameptr(ref strec owner,symptr,int id)ref strec=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
ref strec p,q

p:=newstrec()

p^.name:=symptr^.name
p^.namelen:=symptr^.namelen
p^.symbol:=namesym
p^.owner:=owner
p^.nameid:=id
if id=frameid or id=paramid then
	p^.attribs.ax_frame:=1
fi

if q:=symptr^.nextdupl then			!1st in dupl list
	q^.prevdupl:=p
fi
p^.nextdupl:=q
p^.prevdupl:=symptr
symptr^.nextdupl:=p

return p
end

global proc adddef(ref strec owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
ref strec q

!q:=p
!while q:=q^.nextdupl do
!	if q^.owner=owner then
!		cpl q^.name,"in",owner^.name
!		serror("Duplicate name")
!	fi
!od

if q:=p^.nextdupl then
	if q^.owner=owner then
		cpl q^.name,"in",owner^.name
		serror("Duplicate name")
	fi
fi

p^.nextdef:=owner^.deflist
owner^.deflist:=p

end

global proc adddef_nodupl(ref strec owner,p)=
!version of adddef() that doen't check for duplicates

p^.nextdef:=owner^.deflist
owner^.deflist:=p
end

global proc printst(filehandle f,ref strec p,int level=0)=	!PRINTST
ref strec q

if p^.symbol<>namesym then
	CPL("PRINTST not name\n\n\n")
	stop
fi

printstrec(f,p,level)

q:=p^.deflist

while q<>nil do
	printst(f,q,level+1)
	q:=q^.nextdef
od
end

proc printstrec(filehandle f,ref strec p,int level)=		!PRINTSTREC
attribrec attrs
ref byte q
strbuffer v
ref strbuffer d:=&v
int col,offset,t
const tabstr="    "
[2560]char str

gs_init(d)

offset:=0
to level do
	gs_str(d,tabstr)
	offset+:=4
od
gs_str(d,":")

gs_leftstr(d,p^.name,23-offset,'-')
gs_leftstr(d,namenames[p^.nameid],12,'.')

col:=gs_getcol(d)
attrs:=p^.attribs

strcpy(&.str,"[")

case getscope(p)
when localscope then 
	strcat(&.str," L")
when exportscope then
	strcat(&.str," G")
when importscope then
	strcat(&.str," E")
esac
if attrs.ax_static then
	strcat(&.str," S")
fi
if attrs.ax_fflang then
	strcat(&.str," ")
	strcat(&.str,fflangnames[attrs.ax_fflang])
fi
if attrs.ax_byrefmode then
	strcat(&.str," BR")
fi
if attrs.ax_align then
	strcat(&.str," @@")
	strcat(&.str,strint(attrs.ax_align))
	strcat(&.str," ")
fi
if attrs.ax_optional then
	strcat(&.str," Opt")
fi
if attrs.ax_varparams then
	strcat(&.str," VP")
fi
if attrs.ax_used then
	strcat(&.str," Used")
fi
if attrs.ax_frame then
	strcat(&.str," F")
fi
if attrs.ax_autovar then
	strcat(&.str," A")
fi
if attrs.ax_nparams then
	strcat(&.str," Pm:")
	strcat(&.str,strint(attrs.ax_nparams))
fi
if attrs.ax_moduleno then
	strcat(&.str," M:")
	strcat(&.str,strint(attrs.ax_moduleno))
fi
strcat(&.str,"]")
if str[2]=' ' then
	str[2]:='['
	gs_leftstr(d,&str[2],16)
else
	gs_leftstr(d,&.str,16)
fi

case p^.mode
when tvoid then
	gs_str(d," Vd") 
when tvariant then
	gs_str(d," Va") 
else
	gs_str(d," ")
	strcpy(&.str,strmode(p^.mode))
	gs_leftstr(d,&.str,12)
esac

case p^.nameid
when fieldid,paramid then
	gs_str(d," Off:")
	gs_strint(d,p^.offset)

!	sprintf(&.str,"%.*s",p^.uflags.ulength,&p^.uflags.codes)
	print @&.str,p.uflags.ulength:"v",ref char(&p^.uflags.codes):".*"

	if p^.code then
		gs_str(d," DEFAULT:")
		gs_strvar(d,strexpr(p^.code))
	fi

when genfieldid then
	gs_str(d," Ind:")
	gs_strint(d,p^.offset)

when procid then
	gs_str(d," Ind:")
	gs_strint(d,p^.index)
	gs_str(d," Addr:")
!	sprintf(&.str,"%X",p^.address)
	print @&.str,p^.address:"H"
	gs_str(d,&.str)

	if p^.docstring then
		GS_STR(D,"{")
		GS_STR(D,P^.DOCSTRING)
		GS_STR(D,"}")
	fi

when dllprocid then
	gs_str(d," DLLProc#:")
	gs_strint(d,p^.index)
	if p^.truename then
		gs_str(d," Truename:")
		gs_str(d,p^.truename)
	fi

when applprocid then
	gs_str(d," APPLProc#:")
	gs_strint(d,p^.index)

when constid then
	gs_str(d," Const:")
	gs_strvar(d,strexpr(p^.code))

when typeid then
	if t:=p^.attribs.ax_baseclass then
		gs_str(d," Baseclass:")
		if t<255 then
			gs_str(d,ttname[t])
		else
			gs_str(d,"(Unavailable,>=255)")
		fi
	fi
when enumid then
	gs_str(d," Enum:")
	gs_strint(d,p^.index)

when dllmoduleid then
	gs_str(d," DLL#:")
	gs_strint(d,p^.attribs.ax_dllindex)

when labelid then
	gs_str(d," L")
	gs_strint(d,p^.index)

esac

if p^.equiv then
	case p^.nameid
	when aliasid then
		gs_str(d," Alias for:")
		gs_str(d,p^.equiv^.name)
	when linkid then
		gs_str(d," Link to:")
		gs_str(d,getdottedname(p^.equiv))
	else
		if p^.attribs.ax_at then
			gs_str(d," @")
			gs_str(d,p^.equiv^.name)
		fi
	esac
	gs_str(d," ")
fi

gs_str(d," Lineno:")
gs_strint(d,p^.lineno)

gs_println(d,f)
end

global proc printstflat(filehandle f)=
int i
ref strec p
ref lexrec lx
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
	p:=&hashtable[i]
	if p^.name then
		case p^.symbol
		when rawnamesym,lexmacronamesym then
			println @f,i,p,":",p^.name,symbolnames[p^.symbol],namenames[p^.nameid]
			if p^.symbol=lexmacronamesym then
				println @f,"			",p^.macrovalue
			fi
			p:=p^.nextdupl
			while p do
				println @f,"	",p,p^.name,symbolnames[p^.symbol],namenames[p^.nameid],
					p^.prevdupl,"(From",(p^.owner|p^.owner^.name|"-"),,")"
				p:=p^.nextdupl
			od
		esac
	fi
od
end

function newunitrec:ref unitrec=
ref unitrec p
p:=pcm_alloc(unitrec.bytes)
memset(p,0,unitrec.bytes)
p^.lineno:=lx.lineno
p^.moduleno:=currmoduleno
return p
end

global function createname(ref strec p)ref unitrec=
ref unitrec u

u:=newunitrec()
u^.tag:=j_name
u^.def:=p

return u
end

global function createunit0(int tag)ref unitrec=
ref unitrec u

u:=newunitrec()
u^.tag:=tag
return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
ref unitrec u

u:=newunitrec()
u^.tag:=tag
u^.a:=p
return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
ref unitrec u

u:=newunitrec()
u^.tag:=tag
u^.a:=p
u^.b:=q
return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
ref unitrec u

u:=newunitrec()
u^.tag:=tag
u^.a:=p
u^.b:=q
u^.c:=r
return u
end

global function createconstunit(word64 a, int t)ref unitrec=
ref unitrec u
u:=newunitrec()
u^.tag:=j_const
u^.value:=a
u^.mode:=t
return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
ref unitrec u
u:=newunitrec()
u^.tag:=j_const
u^.svalue:=s
u^.mode:=tstring
if length=-1 then
	u^.slength:=strlen(s)
else
	u^.slength:=length
fi
return u
end

global function getoptocode(int opc)int=		!GETOPTOCODE
!opc is kadd etc
!return matching kaddto, etc
static [0:jtagnames.len]int16 opctotable
int n,opcto,i
[20]char str

opcto:=opctotable[opc]
if opcto then return opcto fi				!find

!assume memoising table not filled in for this opc

strcpy(&.str,jtagnames[opc])					!"add" etc
strcat(&.str,"to")							!"addto" etc

for i:=0 to jtagnames.upb do
	if eqstring(jtagnames[i],&.str) then
		opctotable[opc]:=i
		return i
	fi
od

cpl jtagnames[opc]
serror("Can't find -to version")
return 0
end

global function checkpackedtype(int m)int=
!make sure m is a packed type

switch ttbasetype[m]
when ti8,ti16,ti32,ti64,
	tu8,tu16,tu32,tu64,
	tr32,tr64,
	trefpacked,tstring,tstruct,tstringz,tarray,trefm,tintm,twordm then
	return 1
endswitch
cpl strmode(m),strmode(ttbasetype[m]),m
serror("Invalid Packed type")
return 0
end

global proc checkunpackedtype(int t)=
!make sure m is not a packed type

if ttbasetype[t]>tvariant then
	serror("Pack type not allowed")
fi
end

global function checkdlltype(int m)int=
!return true of m is a proper packed type suitable as dll parameter and return type

if m<0 then			!user type: not resolved yet; assume OK
	return 1
fi

switch ttbasetype[m]
when ti8,ti16,ti32,ti64,
	tu8,tu16,tu32,tu64,
	tr32,tr64,
	tintm,twordm,trefm,
	trefpacked,tstring,tstruct,tstringz then
	return 1
endswitch
cpl ttname[m]
serror("Invalid DLL param/ret type")
return 0
end

global function createtype(ref strec d)int=			!CREATETYPE
!name can be a string, or pointer to the st, or might be a type already
!convert to a moderec handle, and return that

!might be a string; turn it into a 
if d^.nameid=typeid then	!has already been resolved as type
	return d^.mode
fi
return createusertype(d)
end

global function createusertype(ref strec stname)int=		!CREATEUSERTYPE
!create new, named user type
++ntypes
ttname[ntypes]:=stname^.name
ttnamedef[ntypes]:=stname
ttbasetype[ntypes]:=tvoid
ttusercat[ntypes]:=user_cat
ttlineno[ntypes]:=lx.lineno
ttmoduleno[ntypes]:=currmoduleno

return ntypes
end

global function createusertypefromstr(ichar name)int=		!CREATEUSERTYPE
!create new, named user type
ref strec stname
stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
adddef(stmodule,stname)
return createusertype(stname)
end

global function getconstvalue(ref unitrec p,int ID=0)int64=	!GETCONSTVALUE
!extract value from kconst
if p and p^.tag=j_const then
	return p^.value
fi
CPL =ID
CPL =ID
CPL =ID
CPL =ID
serror("GCV Not constant")
return 0
end

global function getrangelwb(ref unitrec p)int=				!GETRANGELWB
if p^.tag=j_makerange then
	return p^.range_lower
else
	serror("getrangelwb")
fi
return 0
end

global function getrangeupb(ref unitrec p)int=				!GETRANGEUPB
if p^.tag=j_makerange then
	return p^.range_upper
else
	serror("getrangeupb")
fi
return 0
end

global function getrangelwbunit(ref unitrec p)ref unitrec=				!GETRANGELWB
if p^.tag=j_makerange then
	return p^.a
else
	return createunit1(j_lwb,p)
fi
end

global function getrangeupbunit(ref unitrec p)ref unitrec=				!GETRANGEUPB
if p^.tag=j_makerange then
!	return createconstunit(p^.range_upper,tint)
	return p^.b
else
	return createunit1(j_upb,p)
fi
end

global function createarraymode(ref strec owner,int target,unit lower,length, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
!length is length, unless lx<>nil!
int atype,k,m

case target
when tu1,tu2,tu4 then
	atype:=tbits
else
	atype:=tarray
esac

if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=anon_cat and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=atype
ttlowerexpr[m]:=lower
ttlengthexpr[m]:=length
tttarget[m]:=target
ttowner[m]:=owner

return m
end

global function nextautotype:ichar=
static [32]char str

!sprintf(&.str,"$T%d",++autotypeno)
fprint @&.str,"$T#",++autotypeno
return &.str
end

global function createstringmode(ref strec owner,int t,unit lengthx, int typedefx)int=		!CREATESTRINGMODE
!create fixed-bound string mode
!length is max length of string (including any count or terminator)
!ts is tstring or tcstring
int k,m

if typedefx=0 then			!typedefx=1 means creating usertype; can't share
!	for k:=tlast to ntypes do
!		if ttusercat[k]=anon_cat and ttbasetype[k]=t and ttlength[k]=length then
!			return k
!		fi
!	od
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=t
ttlower[m]:=(t=tstring|1|0)
ttsize[m]:=0
ttlengthexpr[m]:=lengthx
ttowner[m]:=owner

return m
end

global function createrefpackmode(ref strec owner,int target,typedefx)int=
int k,m

case target
when tvariant then
	return trefvar
when tu1,tu2,tu4 then
	serror("CREATEREFBIT")
esac

if typedefx=0 then		!anon type
	for k:=tlast to ntypes do
		if ttusercat[k]=anon_cat and ttbasetype[k]=trefpacked and tttarget[k]=target then
			return k
		fi
	od
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

storemode(owner,target,&tttarget[m])
!tttarget[m]:=target
ttbasetype[m]:=trefpacked

return m
end

global function getscope(ref strec p)int=		!GETSCOPE
!return localscope, importscope, exportscope
ref strec owner

if p=nil then return localscope fi
case p^.nameid
when moduleid,programid then return exportscope
when dllmoduleid then return importscope
esac

do
	owner:=p^.owner
	case owner^.nameid
	when moduleid, dllmoduleid then
		exit
	esac
	p:=owner
od

!owner is not some module
case owner^.nameid
when moduleid then
	return (p^.attribs.ax_global|exportscope|localscope)
else
	return importscope
esac
return 0
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j_...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

p^.def^.code:=p
end

global proc printcode(filehandle f,ichar caption)=
int i
ref strec p,m

m:=stprogram^.deflist

println @f, caption, "PROGRAM"

while m do
	p:=m^.deflist
	println @f,"Module:",m^.name:"A12P*JC","=":"50p="
	while p do
		case p^.nameid
		when procid then
			println @f,p^.name,,"=",(p^.attribs.ax_global|"Global"|"Local")
			printunit(p^.code,,"1",dev:f)
			println @f
		esac
		p:=p^.nextdef
	od
	m:=m^.nextdef
od
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
ref unitrec q
ref strec d
int t
ichar idname

!CPL $FUNCTION,1
if p=nil then
	return
fi

if p^.lineno then
	currlineno:=p^.lineno
fi

!CPL "PRINTU"

print @dev,p,": "

print @dev,getprefix(level,prefix,p)
idname:=jtagnames[p^.tag]
print @dev,idname,,": "

if fdebug then
CPL =idname
fi

case p^.tag
when j_name then
	d:=p^.def

	print @dev,d^.name,namenames[d^.nameid]

	if d^.code then
		print @dev," {",,jtagnames[d^.code^.tag],,"}"
	fi

	print @dev," ",,getdottedname(d)!,q
	print @dev,(p^.dottedname|" {Dotted}"|"")

	if p^.c then
		print @dev," Lastcall:",p^.c
	fi

	print @dev," ",d:"z8h"

when j_labeldef then
	println @dev,p^.def^.name

when j_const then
	t:=p^.mode
	case t
	when tstring then
		if p^.slength>256 then
			print @dev,"""",,"(LONGSTR)",""" *",,p^.slength
		else
			print @dev,"""",,p^.svalue,,""" *",,p^.slength
		fi
	when tint then
		print @dev,p^.value
	when tword then
		print @dev,p^.uvalue
	when treal then
		print @dev,p^.xvalue
	when trange then
		print @dev,p^.range_lower,,"..",,p^.range_upper
	else
		cpl ttname[t]
		serror("PRINTUNIT BAD CONST")
	esac
	print @dev," ",,stdtypenames[t]

when j_longint then
	print @dev,p^.svalue,"Len:",p^.slength

when j_callhostfn,j_callhostproc then
	print @dev,hostfnnames[p^.opcode]

when j_typeconst then
	print @dev,ttname[p^.mode]

when j_operator then
	print @dev,jtagnames[p^.opcode]+2

when j_convert then
	print @dev,ttname[p^.mode]

esac

println @dev
printunitlist(dev,p^.a,level+1,"1")
printunitlist(dev,p^.b,level+1,"2")
printunitlist(dev,p^.c,level+1,"3")
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
if p=nil then return fi

while p do
	printunit(p,level,prefix,dev:dev)
	p:=p^.nextunit
od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
static [512]char str
[512]char indentstr

!if prefix then prefix:=prefix+" " end

indentstr[1]:=0
if level>10 then level:=10 fi

to level do
!	strcat(&indentstr,"[****]")
	strcat(&.indentstr,"- - ")
od

strcpy(&.str,getlineinfok())
strcat(&.str,&.indentstr)
strcat(&.str,prefix)
if prefix^ then
	strcat(&.str," ")
fi

return &.str
end

global function getdottedname(ref strec p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
static [256]char str
[256]char str2
ref strec owner

strcpy(&.str,p^.name)
owner:=p^.owner
while owner and owner^.nameid<>programid do
	strcpy(&.str2,&.str)
	strcpy(&.str,owner^.name)
	strcat(&.str,".")
	strcat(&.str,&.str2)
	owner:=owner^.owner
od
return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
static [40]char str

!sprintf(&.str,"%04d ",currlineno)
fprint @&.str,"# ",currlineno:"z4"
return &.str
end

global function getavname(ref strec owner,int id=frameid)ref strec=
!create auto-var name and return pointer to st entry
ref strec p
[32]char str
ichar name

if id=frameid and owner^.nameid<>procid then
	serror("Auto frame not in proc")
fi

if id=frameid then
!	sprintf(&.str,"av$%d",++nextavindex)
	print @&.str,"av$",,++nextavindex
else
!	sprintf(&.str,"sv$%d",++nextsvindex)
	print @&.str,"sv$",,++nextsvindex
fi

name:=pcm_copyheapstring(&.str)
addnamestr(name)

p:=getduplnameptr(owner,addnamestr(name),id)

p^.mode:=tint
p^.attribs.ax_autovar:=1

adddef(owner,p)
return p
end

global proc unionstr_clear(ref uflagsrec u)=
u^.ulength:=0
end

global proc unionstr_append(ref uflagsrec u, int c)=
if u^.ulength=u^.codes.len then
	serror("Uflags overflow/a")
fi
++u^.ulength
u^.codes[u^.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
int ulen,vlen,i

ulen:=u^.ulength
vlen:=v^.ulength
if ulen+vlen>u^.codes.len then
	serror("Uflags overflow/c")
fi
for i:=1 to vlen do
	u^.codes[i+ulen]:=v^.codes[i]
od
u^.ulength:=ulen+vlen
end

global function unionstr_last(ref uflagsrec u)int=
if u^.ulength then
	return u^.codes[u^.ulength]
fi
return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
memcpy(u,v,uflagsrec.bytes)
end

global proc unionstr_print(ref uflagsrec u)=
printstrn_app(cast(&u^.codes),u^.ulength)
end

global function createrecordmode(ref strec owner,int t,typedefx)int=	!CREATERECORDMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def:
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=t			!record/struct/union

return m
end

global function createenummode(ref strec owner,int typedefx)int=		!CREATEENUMMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def:
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=tenum

return m
end

global proc convertstring(ichar s, t)=		!CONVERTSTRING
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
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

global function strexpr(ref unitrec p)ref strbuffer=		!STREXPR
!vx_makestring("",exprstr)
gs_init(exprstr)
jeval(exprstr,p)
return exprstr
end

proc jeval(ref strbuffer dest, ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
ref unitrec q
[500]char str

!CPL "JEVAL",JTAGNAMES[P^.TAG]

case p^.tag
when j_const then
	case p^.mode
	when tstring then
dostring::
		if p^.slength>str.len/2 then
			strcpy(&.str,"LONGSTR)")
		else
			convertstring(p^.svalue,&.str)
		fi
		gs_additem(dest,"""")
!		gs_additem(dest,"YYYY")
		gs_additem(dest,&.str)
		gs_additem(dest,"""")
		return
	when tint then
!		sprintf(&.str,"%lld",p^.value)
		print @&.str,p.value
	when tword then
!		sprintf(&.str,"%llu",p^.uvalue)
		print @&.str,p.uvalue
	when treal then
!		sprintf(&.str,"%f",p^.xvalue)
		print @&.str,p.xvalue
	when trange then
!		sprintf(&.str,"%d..%d",p^.range_lower,p^.range_upper)
		fprint @&.str,"#..#",p^.range_lower,p^.range_upper
	else
		CPL ttname[p^.mode]
		rxerror("EVAL/CONST",p)
	esac
	gs_additem(dest,&.str)

when j_name then
	gs_additem(dest,p^.def^.name)

!when binopset,j_andl,j_orl then
when j_andl,j_orl,j_andand,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,
	j_idiv,j_fdiv,j_ddiv,j_rem,j_iand,j_ior,j_ixor,j_shl,j_shr,j_in,j_notin,j_inrev,j_min,j_max,j_addptr,j_subptr,
	j_concat,j_atan2,j_power, j_xorl, j_isequal, j_divrem, j_append then
!
	strcpy(&.str,getopcjname(p^.tag))
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,&.str)
	jeval(dest,p^.b)
	gs_additem(dest,")")

when j_neg,j_abs,j_inot,j_chr,j_asc,j_sqrt,j_sqr,j_cube,j_sign,j_sin,j_cos,j_tan,j_asin,
	j_acos,j_atan,j_ln,j_lg,j_log,j_exp,j_round,j_floor,j_ceil,j_fract,j_fmod, j_lwb,j_upb,j_len,j_bounds,
	j_bitwidth,j_bytesize,j_gettype,j_getbasetype,j_getelemtype,j_isvoid,j_isdef,j_isint,j_isreal,j_isstring,
	j_isrange,j_islist,j_isrecord,j_isarray,j_isset,j_ispointer,j_minvalue,j_maxvalue,j_min1,j_max1,
	j_notl,j_istruel, j_isnone, j_ismutable then

	strcpy(&.str,getopcjname(p^.tag))
	gs_additem(dest,&.str)
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,")")

when j_callfn,j_callproc then
	jeval(dest,p^.a)
	gs_additem(dest,"(")

	q:=p^.b
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,")")

when j_callhostfn then
	gs_additem(dest,"Host<")
	gs_additem(dest,hostfnnames[p^.opcode]+5)
	gs_additem(dest,">(")

	q:=p^.b
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,")")

when j_index,j_dotindex,j_slice,j_dotslice then
	jeval(dest,p^.a)
	if p^.tag=j_dotindex or p^.tag=j_dotslice then
		gs_additem(dest,".")
	fi
	gs_additem(dest,"[")
	jeval(dest,p^.b)
	gs_additem(dest,"]")

when j_keyindex,j_dotkeyindex then
	jeval(dest,p^.a)
	if p^.tag=j_dotkeyindex then
		gs_additem(dest,".")
	fi
	gs_additem(dest,"{")
	jeval(dest,p^.b)
	gs_additem(dest,"}")

when j_dot then
	jeval(dest,p^.a)
	gs_additem(dest,".")
	jeval(dest,p^.b)

when j_makelist,j_makesetlist,j_makeconstr,j_makedict then
	gs_additem(dest,(p^.tag=j_makelist or p^.tag=j_makeconstr|"("|"["))

	q:=p^.a
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,(p^.tag=j_makelist|")"|"]"))

when j_makerange then
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,"..")
	jeval(dest,p^.b)
	gs_additem(dest,")")

when j_assignx then
	jeval(dest,p^.a)
	gs_additem(dest,":=")
	jeval(dest,p^.b)

when j_ifx then
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,"|")
	jeval(dest,p^.b)
	gs_additem(dest,"|")
	jeval(dest,p^.c)
	gs_additem(dest,")")

when j_typeconst then
	gs_additem(dest,strmode(p^.mode))
when j_classconst then
!	gs_additem(dest,"Class<")
	gs_additem(dest,p^.def^.name)
	gs_additem(dest,">")

when j_convert then

!	gs_additem(dest,strmode(p^.mode))
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,")")
when j_keyvalue then
	jeval(dest,p^.a)
	gs_additem(dest,":")
	jeval(dest,p^.b)

when j_longint then
	gs_str(dest,"Longint:")
	goto dostring

when j_ptr then
	jeval(dest,p^.a)
	gs_additem(dest,"^")

when j_addrof then
	gs_additem(dest,"&")
	jeval(dest,p^.a)

when j_ptrto then
	jeval(dest,p^.a)

when j_clamp then
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,",")
	jeval(dest,p^.b)
	gs_additem(dest,",")
	jeval(dest,p^.c)
	gs_additem(dest,")")

when j_block then
	gs_additem(dest,"<JBLOCK>")

when j_multexpr then
	gs_additem(dest,"MULTEXPR(")
	q:=p^.a
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	
	gs_additem(dest,")")

!when j_upper then
!	gs_additem(dest,"<JUPPER>")

else
	CPL jtagnames[p^.tag]
	gerror("CAN'T DO JEVAL",p)
!	PCERROR("CAN'T DO JEVAL:"+jtagnames[p^.tag])
end
!CPL "JEVALX"
end

global function getopcjname(int opc)ichar=		!GETOPCJNAME
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
int i
[20]char str

for i:=1 to opc_codes.len do		!look for dedicated op name
	if opc=opc_codes[i] then
		return opc_names[i]
	fi
od

return jtagnames[opc]+2				!return standard jtag name
end

global function strmode(int m,expand=1)ichar=		!STRMODE
static [4096]char str
istrmode(m,expand,&.str)
return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
ref strec d,q
int value,needcomma,x,i

if m<0 then
	strcpy(dest,"*")
	strcat(dest,ttnamedefx[-m]^.name)
	if ttnamedefx2[-m] then
		strcat(dest,".")
		strcat(dest,ttnamedefx2[-m]^.name)
	fi
	return
fi

if m<tlast then
	strcpy(dest,ttname[m])
	return
fi

case ttbasetype[m]
when trefvar,trefpacked then
	strcpy(dest,"ref ")
	if ttbasetype[tttarget[m]]=tstruct then
		strcat(dest,ttname[tttarget[m]])
	else
		istrmode(tttarget[m],0,dest+strlen(dest))
	fi

when tstring then
	strcpy(dest,"string*")
!	sprintf(dest+strlen(dest),"%d",ttlength[m])
	print @dest+strlen(dest),ttlength[m]

when tstringz then
	strcpy(dest,"stringz*")
!	sprintf(dest+strlen(dest),"%d",ttlength[m])
	print @dest+strlen(dest),ttlength[m]

when tset then
	strcpy(dest,"set*")
!	sprintf(dest+strlen(dest),"%d",ttlength[m])
	print @dest+strlen(dest),ttlength[m]

when tarray then
	if ttlength[m] then
!		sprintf(dest,"[%d..%d]",ttlower[m],ttlength[m]+ttlower[m]-1)
		fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
	else
!		sprintf(dest,"[%d:]",ttlower[m])
		fprint @dest,"[#:]",ttlower[m]
	fi
	istrmode(tttarget[m],0,dest+strlen(dest))

when tenum then
	strcpy(dest,"enum(")
	d:=ttnamedef[m]

	value:=1
	needcomma:=0
	q:=d^.deflist
	while q do
!	forall i,q in d.deflist do
		if needcomma then strcat(dest,",") fi
		needcomma:=1
		strcat(dest,q^.name)
		x:=q^.index
		if x<>value then
			value:=x
!			sprintf(dest+strlen(dest),"%d",value)
			print @dest+strlen(dest),value
		fi
		++value
		q:=q^.nextdef
	od

	strcat(dest,")")

when trecord,tstruct then
	if not expand then
		strcpy(dest,ttname[m])
		return
	fi
	strcat(dest,ttname[ttbasetype[m]])
	strcat(dest,"(")
	d:=ttnamedef[m]
	needcomma:=0

!	forall i,q in d.deflist do
	q:=d^.deflist
	while q do
		if needcomma then strcat(dest,",") fi
		needcomma:=1
		istrmode(q^.mode,0,dest+strlen(dest))
		strcat(dest," ")
		strcat(dest,q^.name)
		q:=q^.nextdef
	od
	strcat(dest,")")

when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
	strcpy(dest,ttname[m])

when tuser then
	strcpy(dest,ttname[m])
else
	serror("NEWSTRMODE")
esac
end

global function countunits(ref unitrec p)int=
int n
n:=0
while p do
	++n
	p:=p^.nextunit
od
return n
end

global function finddefstr(ref strec owner,ichar name)ref strec=	!FINDDEFSTRING
!scan owner looking for a name
!return symptr if found, or nil
ref strec d

d:=owner^.deflist
while d do
	if eqstring(d^.name,name) then
		return d
	fi
	d:=d^.nextdef
od

return nil
end

proc purgesymbol(ref strec p,prev,int del)=
!unlink and (when del=1) recover memory from st entry p
!p is removed from deflist chain of its owner, and from the dupllist chain
!prev is nil when p is the first entry in its owner's deflist, or points
!to the previous still existing entry in the chain. It is necessary so that
!the .nextdef link can be maintained, of either prev or owner
ref strec q

case p^.nameid
when fieldid then			!needed for genfieldtables
	return
esac

!Unlink child symbols
purgesymbollist(p^.deflist,0,del)

!unlink from deflist and continue deflist around it
if prev then
	prev^.nextdef:=p^.nextdef
else
	p^.owner^.deflist:=p^.nextdef
fi

!now need to unlink from dupllist. Here, the .prevdupl field will always
!be valid, pointing to the generic entry if necessary (that can't be unlinked
!here)

q:=p^.prevdupl
q^.nextdupl:=p^.nextdupl

!Now delete the entry
if del then
	pcm_free(p,strec.bytes)
fi
end

global proc purgesymbollist(ref strec p,int ismodule, del)=
!scan the list in p (the deflist of an owner symbol)
!and unlink a symbol when ismodule is 0 or it's not global
!when del=1, then also recover the memory used
!ismodule should be 1 for a module, then the global flag is checked

ref strec q,prev

prev:=nil
while p do
	q:=p^.nextdef			!pick up next before q ceases to exist
	if ismodule=0 or not p^.attribs.ax_global then
		purgesymbol(p,prev,del)
	else
		prev:=p				!when deleted, prev stays unchanged
	fi
	p:=q
od
end

global proc purgeprocs(ref strec p, int del)=
!scan procs in the list p, and remove frame vars


while p do
	if p^.nameid=procid then
		purgeproc(p,del)
	fi
	p:=p^.nextdef
od
end

global proc purgeproc(ref strec p, int del)=
!scan procs in the list p, and remove frame vars
ref strec q,prev,r

!NOTE: THIS CAN'T BE USED AT THE MINUTE, AS THE STRECS COMPRISING THE
!FRAME VARS CONTAIN FRAME OFFSETS NEEDED BY THE CODE GENERATOR.
!POSSIBLY, ENTRIES CAN BE UNLINKED INSTEAD, BUT CAN STILL BE POINTED
!TO BY REFERENCES WITHIN THE BYTE-CODE 

q:=p^.deflist
prev:=nil
while q do
	r:=q^.nextdef
	if q^.nameid=frameid then
		purgesymbol(q,prev,del)
	else
		prev:=q
	fi
	q:=r
od
end

global proc printmodelist(filehandle f)=		!PRINTMODELIST
const wtypeno	= 4
const wname		= 13
const wbasetype	= 13
const wbitsize	= 3
const wtarget	= 12
const wnamedef	= 4
const wlower	= 3
const wupper	= 3
const wlength	= 4
!const wnallfields	= 4
const wsize		= 5
const wusercat	= 4
const wused		= 4
const wmode		= 32
[256]char str
ichar mstr
strbuffer destv
ref strbuffer dest := &destv
int m

gs_init(dest)

gs_leftstr(dest,"#",wtypeno)
gs_leftstr(dest,"Name",wname)
gs_leftstr(dest,"Base",wbasetype)
gs_leftstr(dest,"Bit",wbitsize)
gs_leftstr(dest,"Target",wtarget)
gs_leftstr(dest,"Def",wnamedef)
gs_leftstr(dest,"Lwb",wlower)
gs_leftstr(dest,"Upb",wupper)
gs_leftstr(dest,"Len",wlength)
!gs_leftstr(dest,"AF",wnallfields)
gs_leftstr(dest,"Size",wsize)
gs_leftstr(dest,"Cat",wusercat)
gs_leftstr(dest,"Used",wused)
gs_leftstr(dest,"Mode",wmode)
gs_println(dest,f)

for m:=0 to ntypes do
	gs_init(dest)

	gs_leftint(dest,m,wtypeno)
	gs_leftstr(dest,ttname[m],wname)
	gs_leftstr(dest,ttname[ttbasetype[m]],wbasetype)
	gs_leftint(dest,ttbitwidth[m],wbitsize)
	if tttarget[m] then
		gs_leftstr(dest,ttname[tttarget[m]],wtarget)
	else
		gs_leftstr(dest,"-",wtarget)
	fi
	if ttnamedef[m] then
		gs_leftstr(dest,"+",wnamedef)
	else
		gs_leftstr(dest,"-",wnamedef)
	fi


	case ttbasetype[m]
	when tstring,tset,tarray,trecord,tstruct,tenum then
		gs_leftint(dest,ttlower[m],wlower)
		gs_leftint(dest,ttlower[m]+ttlength[m]-1,wlower)
!		if ttlengthx[m] then
!			gs_leftint(dest,ttlengthx[m],wlength)
!		else
			gs_leftint(dest,ttlength[m],wlength)
!		fi
	else
		gs_leftstr(dest,"",wlower)
		gs_leftstr(dest,"",wlower)
		gs_leftstr(dest,"",wlength)
	esac

!	gs_leftint(dest,ttnallfields[m],wnallfields)
	gs_leftint(dest,ttsize[m],wsize)
	gs_leftint(dest,ttusercat[m],wusercat)
!	gs_leftint(dest,ttused[m],wused)

	mstr:=strmode(m)
	if strlen(mstr)<16 then
		gs_str(dest,mstr)
	else
		gs_println(dest,f)
		gs_init(dest)
		gs_str(dest,mstr)
	fi
	gs_println(dest,f)
od

println @f
end

global proc printgenfieldtable(filehandle f, ichar caption)=
int i
println @f,caption,ngenfieldnames
for i to ngenfieldnames do
	println @f,i,genfieldnames[i].def^.name,genfieldnames[i].dataindex,
		genfieldnames[i].datalength
od
println @f
println @f,"Genfielddata:",ngenfielddata
for i to ngenfielddata do
	println @f,genfielddata[i].fieldindex,
		ttname[genfielddata[i].recordtype],ttname[genfielddata[i].fieldtype],
		genfielddata[i].offset
od
println @f
end

global proc addtoproclist(ref strec d)=
	ref procrec pp
	++nproclist
	pp:=pcm_alloc(procrec.bytes)
	pp^.nextproc:=proclist
	proclist:=pp
	pp^.def:=d
end

global function newusertypex(ref strec d,e=nil)int=
int i

if nuserxtypes>=maxuserxtype then
	serror("Too many external user types")
fi
++nuserxtypes
ttnamedefx[nuserxtypes]:=d
ttnamedefx2[nuserxtypes]:=e
ttxmoduleno[nuserxtypes]:=currmoduleno
ttlinenox[nuserxtypes]:=lx.lineno iand 16777215
return -nuserxtypes

end

global proc storemode(ref strec owner, int m, ref int16 p)=
ref userxrec q
p^:=m
if m>=0 then return fi

q:=pcm_alloc(userxrec.bytes)
q^.owner:=owner

IF OWNER=NIL THEN
!CPL =ID
SERROR("STOREMODE/OWNER=0")
FI

q^.pmode:=p
q^.nextmode:=userxmodelist
userxmodelist:=q
end

global function duplunit(unit p)unit=
unit q
if p=nil then return nil fi

q:=createunit0(p^.tag)

q^.a:=duplunit(p^.a)
q^.b:=duplunit(p^.b)
q^.c:=duplunit(p^.c)
q^.lineno:=p^.lineno
q^.value:=p^.value			!copy main field of each union
q^.opcode:=p^.opcode
q^.mode:=p^.mode
q^.moduleno:=p^.moduleno

return q
end


=== qc_name.m 32/38 ===
import mlib
import clib
import oslib

import var_types
import var_decls
import qc_support
import pq_common
import qc_tables
import qc_lex
import qc_lib

ref strec currstproc
int allowmodname=0

int nfields,nallfields		!set up during converttype

global proc rx_unit(ref strec owner, unit p)=
ref strec d
unit a,b
int n

a:=p^.a
b:=p^.b
mlineno:=p^.lineno

switch p^.tag
when j_name then
	resolvename(owner,p)

when j_keyword then
	rx_unit(owner,b)		!do param value only

when j_dot then
	resolvedot(owner,p)

when j_callproc, j_callfn then
	rx_unit(owner,a)
	rx_unitlist(owner,b)
	if a^.tag=j_typeconst then
!CPL "TYPECONST",STRMODE(A^.MODE)
		p^.tag:=j_convert
		p^.a:=b
		p^.b:=nil
		p^.mode:=a^.mode
		if b^.nextunit then
			p^.a:=createunit1(j_makelist,b)
			n:=0
			while b do
				++n
				b:=b^.nextunit
			od
			p^.a^.length:=n
		fi
	fi

!when j_callproc, j_callfn then
when j_goto then
	if a^.tag<>j_name then rxerror("Not simple label") fi
	resolvename(owner,a)
	if a^.def^.nameid<>labelid then rxerror_s("Not a label or not found: #",a^.def^.name) fi

when j_add, j_sub, j_mul, j_div, j_idiv, j_rem, j_iand, j_ior, j_ixor,
	j_shl, j_shr, j_makerange then
	rx_unitlist(owner,a)
	if not b then rxerror("Binop missing opnd") fi
	rx_unitlist(owner,b)
	evalbinop(p,a,b)

!when j_andl,j_orl,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,
!	j_idiv,j_fdiv,j_rem,j_iand,j_ior,j_ixor,j_shl,j_shr,j_in,j_notin,
!	j_inrev,j_min,j_max,j_addptr,j_subptr,j_concat,j_power, j_xorl, j_isequal,
!	j_divrem, j_atan2,j_fmod,j_imul then

when j_eq,j_ne, j_lt,j_le,j_ge,j_gt then

	case p^.a^.tag
	when j_eq,j_ne, j_lt,j_le,j_ge,j_gt then

		converteqeq(owner,p)
	else
		go to doabc
	esac
	goto doabc

!when j_neg, j_abs,j_len, j_bytesize then
when j_neg, j_abs,j_len, j_bytesize,j_sqrt then
	rx_unitlist(owner,a)
	evalmonop(p)

!when j_makelist then
!when j_index,j_dotindex then
!when j_upper then
!when j_hardconv then

when j_forup,j_fordown then			!a will be j_name unit
	resolvename(owner,a,ti64)
	a:=a^.nextunit
	goto doabc

when j_convert then
!CPL "RX/CONVERT"
	rx_unit(owner,a)
	if a^.tag=j_const then
		evalmonop(p)
	fi
!	GOTO DOABC

else
doabc::
	rx_unitlist(owner,a)
	if b then rx_unitlist(owner,b) fi
	if p^.c then rx_unitlist(owner,p^.c) fi
endswitch
end

global function rx_module(int n)int=
modulerec m
ref strec stmodule, d
int globalflag,status

currmoduleno:=n

rx_passdef(stprogram,moduletable[n].stmodule)

return 1
end

global proc rx_deflist(ref strec owner,p)=
	while p do
		rx_passdef(owner,p)
		p:=p^.nextdef
	od
end

global proc rx_passdef(ref strec owner,p)=
ref strec d

case p^.nameid
when moduleid,dllmoduleid then
	rx_deflist(p,p^.deflist)

when procid then
	fixmode(owner,p)
	rx_deflist(p,p^.deflist)
	currstproc:=p
	rx_unit(p,p^.code)
	currstproc:=nil

when dllprocid then
	fixmode(owner,p)
	rx_deflist(p,p^.deflist)

when constid,staticid,frameid,paramid then
	fixmode(owner,p)
	if p^.code then
		rx_unit(owner,p^.code)
	fi
when typeid then
	fixmode(owner,p)
	rx_deflist(p,p^.deflist)

else

esac
end

proc rx_unitlist(ref strec owner, unit p)=
while p do
	rx_unit(owner,p)
	p:=p^.nextunit
od
end

global function resolvetopname(ref strec owner,stnewname,int moduleno,fmodule)ref strec=
!stnewname points to a symrec with nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)
!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match
!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

int i,m,extcount,modno
ref strec p,powner,d,e,dlldef,extdef,moddef,extmod
[10]ref strec ambiglist
INT DEB

!CPL "RESOLVETOPNAME",STNEWNAME^.NAME

p:=stnewname^.nextdupl

extcount:=0
extmod:=dlldef:=extdef:=moddef:=nil

while p do						!for each possibe st entry of the same name
	powner:=p^.owner			!the owner of that entry

	switch powner^.nameid
	when procid then
		if powner=owner then			!immediate match
			return p
		fi
	when moduleid then			!p is file-scope item
		if powner^.attribs.ax_moduleno=moduleno then		!same module
			if owner^.nameid=moduleid then	!immediate match
				return p
			fi
			moddef:=p			!take note, but continue searching (in case proc etc)
		elsif moduletable[moduleno].importmap[powner^.attribs.ax_moduleno] then
			if p^.attribs.ax_global then
								!matches an external module imported by this name's module
				++extcount			!if an ext match is closest, there can only be one
				extdef:=p
storeextdef::
				if extcount<ambiglist.len then
					ambiglist[extcount]:=extdef
				fi

			elsif p^.nameid=aliasid and p^.equiv^.attribs.ax_global then
				++extcount
				extdef:=p^.equiv
				goto storeextdef
			fi
		fi
	when dllmoduleid then

		modno:=powner^.attribs.ax_moduleno
		if modno=moduleno or moduletable[moduleno].importmap[modno] then
			dlldef:=p
		fi

	when typeid then
		if powner=owner then			!immediate match
			return p
		fi
	when programid then					!p is a module
		if p^.nameid=moduleid then		!match a module name
			if fmodule then
				return p			!immediate match (unless proc but that would have
			fi						!matched by now
		fi
	endswitch

	p:=p^.nextdupl
od

!if here, then no immediate match
!either of moddef/dlldef will be set
if moddef then				!go with that first
	return moddef
fi
if extdef then
	if extcount>1 then
		for i:=1 to extcount do
			extdef:=ambiglist[i]
			println i,extdef^.owner^.name,namenames[extdef^.owner^.nameid]
		od
		rxerror_s("Ambiguous ext name: #",extdef^.name)
	fi
	return extdef
fi
if extmod then return extmod fi
return dlldef				!will be nil when no match
end

global proc resolvename(ref strec owner, unit p, int mode=tvoid)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	ref strec d,e
	unit q
	int moduleno

	d:=p^.def
	moduleno:=p^.moduleno

	if d^.nameid not in [nullid,genfieldid] then			!assume already resolved
!	if d^.nameid<>nullid and d^.name<>genfieldid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)
	if not e then
		if owner^.nameid=procid then	!add as framevar
			e:=p^.def:=getduplnameptr(owner,p^.def,frameid)
			adddef(owner,e)				!note: no vardef exists
			e^.mode:=tvariant
		else
			cpl d^.name,p^.lineno,jtagnames[p^.tag]
			rxerror_s("Undefined: #",d^.name,p)
		fi
	else
retry::
		p^.def:=e			!update link in kcode

		case e^.nameid
		when constid then		!convert namedconst to const
			q:=e^.code			!q is knamedconst unit; q^.c is value
			rx_unit(owner,q)
			if q^.tag<>j_const then
				rxerror_s("Not const expr: #",jtagnames[q^.tag])
			fi

			e^.mode:=q^.mode
			p^.tag:=j_const
			p^.value:=q^.value
			p^.mode:=q^.mode
			p^.slength:=q^.slength
		when enumid then
			rxerror("FOUND ENUMID",p)
		when staticid then		!deal with python global accesses ?? WTF ???
		when typeid then
			p^.tag:=j_typeconst
			p^.mode:=p^.def^.mode
		when aliasid then		!replace by what it is shadowing
				!may never get here, if the substitution is done by resolvetopname()
			e:=e^.equiv
			goto retry

		when linkid then
			rxerror("FOUND LINK",p)
		esac
	fi
end

global function finddupl(ref strec d, pdupl)ref strec=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

if pdupl^.nameid not in [nullid,genfieldid] then		!assume already resolved
!if pdupl^.nameid<>nullid and pdupl^.nameid<>genfieldid then		!assume already resolved
	return pdupl
fi
pdupl:=pdupl^.nextdupl

while pdupl do
	if pdupl^.owner=d then
		if pdupl^.nameid in [aliasid,linkid] then
!		if pdupl^.nameid=aliasid or pdupl^.nameid<>linkid then
			return d^.equiv
		fi

		return pdupl
	fi
	pdupl:=pdupl^.nextdupl
od

return nil
end

proc resolvedot(ref strec owner,unit p)=
ref strec qdef,rdef,d,newd,e,fielddef
ref unitrec q,r
int nfields,oldallowmod

q:=p^.a			!lhs
r:=p^.b			!rhs
rdef:=r^.def							!st entry for the field

oldallowmod:=allowmodname
allowmodname:=q^.tag=j_name
rx_unit(owner,q)
allowmodname:=oldallowmod

case q^.tag
when j_name then		!continue below

	d:=q^.def
when j_typeconst then	!was type
	d:=q^.def
	goto dotype
else					!assume expression
	rdef:=r^.def
	goto doexprdot
esac

switch d^.nameid
when dllmoduleid,moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
dotype::
	newd:=finddupl(d, rdef)

	if newd then					!found
		switch newd^.nameid
		when enumid then			!convert whole thing to constant
			p^.tag:=j_const
			p^.value:=newd^.index
			p^.mode:=tint
		when constid then
			q:=newd^.code			!q is knamedconst unit; q^.c is value
			case q^.tag
			when j_const then
				p^.tag:=j_const
				p^.value:=q^.value
				p^.mode:=newd^.mode
				p^.a:=p^.b:=nil
			else
				rxerror("Rxdot:const?",p)
			esac
		when typeid then
			p^.tag:=j_typeconst
			p^.mode:=newd^.mode
			p^.def:=newd
		when staticid then
			p^.tag:=j_name
			p^.def:=newd

		when procid,dllprocid then
			p^.tag:=j_name
			p^.def:=newd
			p^.a:=p^.b:=nil
			p^.dottedname:=1
		when linkid then
			repeat
				newd:=newd^.equiv
			until newd^.nameid<>linkid
			p^.tag:=j_name
			p^.def:=newd
		else
			cpl namenames[newd^.nameid],,".",,newd^.name
			rxerror("Rxdot:.name not allowed here",p)
		endswitch

	else
		cpl d^.name,,".",,rdef^.name
		rxerror("Can't resolve",p)
	fi

when frameid, staticid, paramid, fieldid, genfieldid then	!X. normal lhs
doexprdot::
	nfields:=0
	fielddef:=nil
	e:=rdef^.nextdupl
	while e do
		case e^.nameid
		when fieldid, constid, procid, typeid, staticid, dllprocid, linkid then
			++nfields
			fielddef:=e				!use this when unique
		esac
		e:=e^.nextdupl
	od

	case nfields
	when 0 then				!no field exists with this name
		cpl rdef^.name
		rxerror("Can't find field",p)
	else					!dupl field
		if rdef^.nameid<>genfieldid then		!mcc converted nullid of generic name to genfield
			rdef^.nameid:=genfieldid			!convert base/nullid name to genfield
			genfieldnames[++ngenfieldnames].def:=rdef
			rdef^.offset:=ngenfieldnames
		fi
	esac

else
	cpl namenames[d^.nameid]
	rxerror("RXDOT:Unknown nameid",p)
endswitch
end

proc fixmode(ref strec owner, p)=
ref strec d,e
int m

m:=p^.mode

if m>=0 then return fi
m:=-m

if ttxmap[m] then				!already fixed
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
!if m is a userx type, fix it up and return fixed up mode
!otherwise just return m
ref strec d,e
[256]char str

if m>=0 then return m fi
m:=-m

if ttxmap[m] then				!already fixed
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
!	sprintf(&.str,"# in module %s, line:%d",d^.name,moduletable[ttxmoduleno[m]].name,ttlinenox[m])
	fprint @&.str,"# in module #, line:#",d^.name,moduletable[ttxmoduleno[m]].name,ttlinenox[m]

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
!CPL =P
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

!scan baseclasses
for i to nbaseclasses do
	dobaseclass(i)
od

end

global function resolve_equiv_name(ref strec owner,p)ref strec=
!@p or @p+offset used for a field offset
!owner is record type of which it should be a member
!currently, p is at strec that might be null
!return matching fieldid name
if p^.nameid=fieldid then
	return p
fi

RXERROR("RESOLVE EQUIV FIELD/COMPLEX")

!if p.dupllist.isdef then
!	forall d in p.dupllist do
!		if d.owner==owner then
!			return d
!		fi
!	od
!fi
!rxerror("@x not defined")
return nil
end

function addframevar(ref strec owner, d, int moduleno, mode)ref strec=
!owner should be a proc; d is a generic st entry
!add framevar with the name of d and given mode to the proc
	ref strec e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,&e^.mode)
	adddef(owner,e)
	return e
end

proc converteqeq(ref strec owner,ref unitrec p)=
!detect exprs such as a=b=c and convert to a=b and b=c
int leftop,rightop
ref unitrec w,y1,y2,z

w:=p^.a				!w is the x=y branch
y1:=w^.b				!split y into two
y2:=duplunit(y1)
z:=p^.b

leftop:=w^.tag
rightop:=p^.tag
p^.tag:=j_andl
p^.b:=createunit2(rightop,y2,z)
rx_unitlist(owner,w)
rx_unitlist(owner,y2)
rx_unitlist(owner,z)
end

global proc evalbinop(ref unitrec p,a,b)=		!EVALBINOP
int64 x,y,z
int xt,yt

unless a^.tag=b^.tag=j_const then
	return
end

xt:=a^.mode
yt:=b^.mode


!CPL "EVALB",JTAGNAMES[A^.TAG],STRMODE(XT)

if xt=yt=treal then
	evalbinop_real(p,a,b)
	return
fi
unless xt=yt=tint then
	return
end unless

x:=a^.value
y:=b^.value

switch p^.tag
when j_add then
	z:=x+y
when j_sub then
	z:=x-y
when j_mul then
	z:=x*y
!when j_div then
!	z:=x/y
!
when j_idiv then
	z:=x/y

when j_makerange then
	z:=y<<32 ior (x iand 0xffff'ffff)
	makenewconst(p,z,trange)

	return
else
	return
end

makenewconst(p,z,tint)
end

proc evalbinop_real(ref unitrec p,a,b)=
real x,y,z

x:=a^.xvalue
y:=b^.xvalue

switch p^.tag
when j_add then
	z:=x+y
when j_sub then
	z:=x-y
when j_mul then
	z:=x*y
when j_div then
	z:=x/y
else
	return
end

makenewconst(p,int64@(z),treal)
end

proc makenewconst(ref unitrec p,int64 value, int t)=	! MAKENEWCONST
!convert unit p, currently binop or monop, to a const
int a,b
p^.tag:=j_const
p^.value:=value
p^.mode:=t
p^.a:=nil
p^.b:=nil
end

global proc evalmonop(ref unitrec p)=			!EVALMONOP
int64 a,c
real64 x,z
unit pa

pa:=p^.a

case pa^.tag
when j_const then
when j_typeconst then
	case p^.tag
	when j_bytesize then
		makenewconst(p,ttsize[pa^.mode],tint)
		return			!AT PRESENT, TYPES ARE ONLY PROPERLY RESOLVED AFTER NX PASS???
	when j_len then
		return
	esac
else
	return
esac

a:=pa^.value
x:=pa^.xvalue

case p^.a^.mode
when tint then
	switch p^.tag
	when j_neg then
		c:=-a
	when j_abs then
		c:=abs a
	when j_convert then
		case ttbasetype[p^.mode]
		when treal then

			x:=real(a)
			makenewconst(p,int64@(x),treal)
			return
		else
			return
		esac
	when j_sqrt then
		z:=sqrt(a)
		makenewconst(p,int64@(z),treal)
		return

	else
		return
	end
when treal then
	switch p^.tag
	when j_neg then
		z:=-x
	when j_abs then
		z:=abs x
	when j_convert then
		case ttbasetype[p^.mode]
		when tint then
			makenewconst(p,int64(x),tint)
			return
		else
			return
		esac
	when j_sqrt then
		z:=sqrt(x)

	else
		return
	end
	makenewconst(p,int64@(z),treal)
	return

when tstring then
	switch p^.tag
	when j_len then
		c:=pa^.slength
	else
		return
	end

else
	return
esac

makenewconst(p,c,tint)
end

global proc tx_typetable=
	int i,u

	for i:=tuser to ntypes do
		converttype(i)
	od
end

proc checkconstexpr(unit a)=
if a^.tag=j_const then return fi

rxerror_s("Not const expr: %s",jtagnames[a^.tag])
end

global proc converttype(int m)=			!CONVERTTYPE
!This 'conversion' is mainly about working out lengths and sizes and offsets
ref strec d,owner
int first,a,b,nbits,recordsize,index,length,lower
const int maxfield=256
[maxfield]ref strec fieldlist
int nofields,oldmodno
unit plength, plower

!CPL "CONVERTTYPE",=M,=NTYPES,TTNAME[M],TTSIZE[M]

if ttsize[m] then return fi			!assume already done
owner:=ttowner[m]
plower:=ttlowerexpr[m]
plength:=ttlengthexpr[m]
mlineno:=ttlineno[m]

oldmodno:=currmoduleno
currmoduleno:=ttmoduleno[m]

case ttbasetype[m]
when tstring,tstringz then
	ttsize[m]:=ttlength[m]:=getconstint(owner,plength)

when tset then
	ttlength[m]:=getconstint(owner,plength)
	ttsize[m]:=((ttlength[m]-1)/64+1)*8			!in bytes

when tarray then
	if plower then
		ttlower[m]:=getconstint(owner,plower)
	else
		ttlower[m]:=1
	fi
	ttlength[m]:=getconstint(owner,plength)

	case ttbasetype[m]
	when tu1,tu2,tu4 then
		nbits:=ttlength[m]*ttbitwidth[tttarget[m]]
		ttsize[m]:=(nbits-1)/8+1
	else
		converttype(tttarget[m])
		ttsize[m]:=ttlength[m]*ttsize[tttarget[m]]
	esac

when tenum then
	first:=1
	a:=b:=0
	d:=ttnamedef[m]^.deflist
	while d do
		if d^.nameid=enumid then
!			++ttnallfields[m]
			if first then
				first:=0
				a:=b:=d^.index
			else
				a := a min d^.index
				b := b max d^.index
			fi
		fi
		d:=d^.nextdef
	od
	ttlower[m]:=a
	ttlength[m]:=b-a+1

when trefpacked then
	converttype(tttarget[m])
	ttsize[m]:=($targetbits=64|8|4)

when trecord then
	nofields:=0

	d:=ttnamedef[m]^.deflist
	while d do
		if d^.nameid=fieldid then
			if ttbasetype[d^.mode]>tvariant then
				cpl ttname[m]
				gerror("Packtype in record")
			fi
			if nofields>=maxfield then
				gerror("CT: too many fields")
			fi
			fieldlist[++nofields]:=d
			converttype(d^.mode)
		fi
		d:=d^.nextdef
	od

	nallfields:=nfields:=0			!should be recursive, provided nested modes processed above
	recordsize:=scanrecord(&fieldlist,nofields)

	ttlower[m]:=1
	ttsize[m]:=recordsize
	ttlength[m]:=nfields

when tstruct then
!CPL "CONVSTRUCT"
	nofields:=0
	d:=ttnamedef[m]^.deflist
	while d do
		if d^.nameid=fieldid then

			if ttbasetype[m]=tstruct then
				case ttbasetype[d^.mode]
				when tvariant  then
					cpl ttname[m]
					gerror("Var in struct")
				when tintm,twordm,trefm then
					cpl ttname[m]
					gerror("Intm/etc in struct")
				esac
			else
				if ttbasetype[d^.mode]<>tvariant then
					cpl ttname[m]
					gerror("Packtype in record")
				fi
			fi
			if nofields>=maxfield then
				gerror("CT: too many fields")
			fi
			fieldlist[++nofields]:=d
			converttype(d^.mode)
		fi
		d:=d^.nextdef
	od

	nallfields:=nfields:=0			!should be recursive, provided nested modes processed above
	index:=nofields
	recordsize:=scanstruct(1,&fieldlist,nofields,index,0,2)

    d:=ttnamedef[m]
    case d^.attribs.ax_align
    when 2 then while recordsize iand 1 do ++recordsize od
    when 4 then while recordsize iand 3 do ++recordsize od
    when 8 then while recordsize iand 7 do ++recordsize od
	esac

	ttlower[m]:=1
	ttsize[m]:=recordsize
	ttlength[m]:=nfields

!else
!CPL "CAN'T DO:",TTBASETYPE[M]
esac
currmoduleno:=oldmodno
end

function scanstruct(int fstruct,ref[]ref strec flist,
					int flistlen,&index,nextoffset,countmode)int=		!SCANSTRUCT
!!fstruct=1/0 for struct/union
!!flist if list of strecs comprising all the fields (because fields can be mixed with
!! other defs, main caller should extract all fieldid types to another list)
!!index refers to start def in flist: will scan until end of list, or a def with "E" flags
!!nextoffset is current offset within overall struct or record
!!countmode = 2/1/0 for top-level structs/unions/other; some logic to decide whether to count
!! the field as a main one
!!recursive routine to work out offsets of fields, which may have union/struct grouping
!! superimposed via S/U/E char codes in the .uflags string
!!return (index, size), where index is index of next item to look at (or is >flist.len)
!! and size is the overall size of this struct/union group
!!doesn't handle padding at the moment, which is assumed to be zero (ie. pack(1))
!!also updates globals nfields and nallfields
!
int startoffset,maxsize,exitflag,size,star,alignment
ref strec d,e
uflagsrec flags

startoffset:=nextoffset
maxsize:=0
exitflag:=0

while not exitflag and index>=1 do
	d:=flist^[index]

	flags:=d^.uflags
	case flags.codes[1]
	when 'S' then
		shiftflagsleft(&d^.uflags)
		size:=scanstruct(1,flist,flistlen,index,nextoffset,countmode)

	when 'U' then
		shiftflagsleft(&d^.uflags)
		size:=scanstruct(0,flist,flistlen,index,nextoffset,(countmode|1|0))

	when 'E' then
		shiftflagsleft(&d^.uflags)
		if d^.uflags.ulength=0 then --index fi
		exitflag:=1
		size:=0

	when '*' then				!normal file where E flags follow
		shiftflagsleft(&d^.uflags)
		star:=1
		goto dofield

	else					!an actual field
		star:=0
dofield::
		if d^.attribs.ax_at then
			e:=d^.equiv
			d^.offset:=e^.offset

			size:=0
		elsif d^.attribs.ax_equals then
			gerror("Can't init a field")
		else
			size:=ttsize[d^.mode]
			alignment:=d^.attribs.ax_align
			if alignment=255 then
				alignment:=max(size,8)
			fi
			case alignment
			when 2 then while nextoffset iand 1 do ++nextoffset od
			when 4 then while nextoffset iand 3 do ++nextoffset od
			when 8 then while nextoffset iand 7 do ++nextoffset od
			esac

			d^.offset:=nextoffset
			if countmode then
				++nfields
			fi
		fi
!		if not star then ++index fi
		if not star then --index fi
		++nallfields
	esac

	if fstruct then
		nextoffset+:=size
	else
		maxsize :=max(maxsize,size)
		countmode:=0
	fi
od

return (fstruct|nextoffset-startoffset|maxsize)
end

function scanrecord(ref[]ref strec flist,int flistlen)int=
!flist is list of strecs comprising all the fields (because fields can be mixed with
! other defs, main caller should extract all fieldid types to another list)

int size,index,nextoffset
ref strec d,e

nextoffset:=0

for index:=flistlen downto 1 do
	d:=flist^[index]
	if d^.attribs.ax_at then
		e:=d^.equiv
		d^.offset:=e^.offset
		size:=0
	elsif d^.attribs.ax_equals then
		gerror("Can't init a field")
	else
		size:=varsize

		d^.offset:=nextoffset
		++nfields
	fi
	++nallfields

	nextoffset+:=size
od

return nextoffset
end

proc shiftflagsleft(ref uflagsrec flags)=
int i

if flags^.ulength then
	for i:=1 to flags^.ulength-1 do
		flags^.codes[i]:=flags^.codes[i+1]
	od
	flags^.codes[flags^.ulength]:=0
	--flags^.ulength
fi
end

function getconstint(ref strec owner,ref unitrec a)int=
!process unit found in tt-tables, and convert to int

if a=nil then
	rxerror("GETCONSTINT A=NIL")
fi

rx_unit(owner, a)

checkconstexpr(a)

case a^.mode
when tint,tword then
	if ttsize[a^.mode]=16 then
		RXERROR("GETCONSTINT/128")
	fi
	return a^.value
when treal then
	return a^.xvalue
else
	cpl strmode(a^.mode)
	rxerror("Getconstint: not int32/64")
esac
return 0
end

global function checkdict(ref unitrec p)int=
!check whether p, which is a set construct, is a dict; return 1 or 0
!syntax for a dict is: [a:b, c:d ...]
int nkeywords, isconst
ref unitrec q
int n

if p^.tag<>j_makesetlist then
	return 0
fi

n:=nkeywords:=0
isconst:=1


q:=p^.a
while q do
	++n
	case q^.tag
	when j_keyvalue then
		++nkeywords
!		isconst iand:=isconstunit(q.a)
!		isconst iand:=isconstunit(q.b)
	esac
	q:=q^.nextunit
od

if nkeywords=0 then return 0 fi

if nkeywords<>n then
	rxerror("Dict: not all key:values")
fi
p^.tag:=j_makedict

!!is a constant dict?
!if isconst then
!	stname:=getavname(stmodule,staticid)
!	stname.mode:=m_variant
!	stname.code:=createunit(p.tag,p.a,p.b)		!j_makedict
!
!	stname.attribs.ax_equals:=1
!
!	p.tag:=j_name
!	p.a:=stname
!fi

return 1
end

global proc checkconstlist(ref unitrec p)=
!p is a j_makelist/j_makesetlist unit
!scan the list to see if all elements are constant (will not change during the program
!run)
!if they are, then create a new static, module-level auto-var initialised to the list
!replace the original makelist with a name unit pointing to the new autovar
ref unitrec q
ref strec stname
int n

q:=p^.a
n:=0

while q do
!forall q in p.a do
	++n
	case q^.tag
	when j_const then		!add nested autovars later
	when j_name then
		if not q^.def^.attribs.ax_autovar then
			return
		fi
	when j_makerange then
		if q^.a^.tag<>j_const or q^.b^.tag<>j_const then
			return
		fi
	else				!non-const; have to evaluate whole thing on each access
		return
	esac
	q:=q^.nextunit
od

if n=0 then			!()/[] don't need pre-evaluation
	return
fi

stname:=getavname(stmodule,staticid)
stname^.mode:=tvariant
stname^.code:=createunit2(p^.tag,p^.a,p^.b)		!j_makelist/makesetlist

stname^.attribs.ax_equals:=1

p^.tag:=j_name
p^.def:=stname
end

proc duplfield(ref strec p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

if p^.code then
	serror("DUPLFIELD")
fi
q^.attribs:=p^.attribs
q^.address:=p^.address
q^.uflags:=p^.uflags		!for ^.uflags
q^.mode:=p^.mode
end

proc dobaseclass(int baseclassindex)=
!do fixups needed for baseclass, that couldn't be in in parser until
!user types were fixed up
ref strec sttype,d,e,newd
int baseclass,normalexit

baseclass:=baseclasstable[baseclassindex]
sttype:=baseclassdef[baseclassindex]

!CPL "DOING BASECLASS FIXUP"
	d:=ttnamedef[baseclass]^.deflist
	while d do
		e:=sttype^.deflist
		normalexit:=1
		while e do
			if eqstring(d^.name,e^.name) then
				normalexit:=0
				exit
			fi
			e:=e^.nextdef
		od
		if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
			case d^.nameid
			when procid,linkid then
				newd:=getduplnameptr(sttype,d,linkid)
				newd^.equiv:=d
			else
				newd:=getduplnameptr(sttype,d,d^.nameid)
				duplfield(d,newd)
			esac
			adddef(sttype,newd)
		fi
		d:=d^.nextdef
	od
end
=== qc_pclgen.m 33/38 ===
import mlib
import clib
import oslib

import var_types
import var_decls
import qc_support
import qc_tables
import qc_lib
import qc_lex
import pq_common
import qc_pcllib

!not opcodes, just used internally here for conditional jump logic
const kjumpt = 1
const kjumpf = 0

const maxswitchrange=512

!var stcurrproc
int nprocframevars		!set up at procentry
ref strec stretval			!set up at procentry
int retindex
int nprocparamvars		!needed for $getparam(0); excludes $retval

const maxloopindex=20
[maxloopindex,4]ref int loopstack
[maxloopindex]int trylevelstack
int loopindex=0
int looptrylevel			!return by findlooplabel

int trylevel=0

const int maxparams=64
const int maxlocals=256

ref strec st_startproc			!strec of s$startproc(), or nil
!var ref strec ststartproc			!strec of start, or nil
const $startprocname="$startproc"

tabledata []int pcl_jcodes, []byte pcl_nopnds, []int pcl_kcodes=
	(j_add,			2,	kadd),
	(j_sub,			2,	ksub),
	(j_mul,			2,	kmul),
	(j_div,			2,	kdiv),
	(j_idiv,		2,	kidiv),
	(j_rem,			2,	krem),
	(j_divrem,		2,	kdivrem),
	(j_eq,			2,	keq),
	(j_ne,			2,	kne),
	(j_lt,			2,	klt),
	(j_le,			2,	kle),
	(j_gt,			2,	kgt),
	(j_ge,			2,	kge),
	(j_isequal,		2,	kisequal),
	(j_iand,		2,	kiand),
	(j_ior,			2,	kior),
	(j_ixor,		2,	kixor),
	(j_inot,		2,	kinot),
	(j_shl,			2,	kshl),
	(j_shr,			2,	kshr),
	(j_in,			2,	kin),
	(j_notin,		2,	knotin),
	(j_inrev,		2,	kinrev),
	(j_min,			2,	kmin),
	(j_max,			2,	kmax),
	(j_power,		2,	kpower),
	(j_atan2,		2,	katan2),
	(j_concat,		2,	kconcat),
	(j_append,		2,	kappend),

	(j_neg,			1,	kneg),
	(j_abs,			1,	kabs),
	(j_notl,		1,	knot),
	(j_istruel,		1,	kistrue),
	(j_lwb,			1,	klwb),
	(j_upb,			1,	kupb),
	(j_len,			1,	klen),
	(j_bounds,		1,	kbounds),
	(j_isvoid,		1,	kisvoid),
	(j_isdef,		1,	kisdef),

	(j_isint,		1,	kisint),
	(j_isreal,		1,	kisreal),
	(j_isarray,		1,	kisarray),
	(j_isrange,		1,	kisrange),
	(j_isstring,	1,	kisstring),
	(j_isrecord,	1,	kisrecord),
	(j_isset,		1,	kisset),
	(j_ispointer,	1,	kispointer),
	(j_ismutable,	1,	kismutable),
!	(j_ishandle,	1,	kishandle),
	(j_gettype,		1,	ktype),
	(j_getbasetype,	1,	kbasetype),
	(j_getelemtype,	1,	kelemtype),
	(j_bitwidth,	1,	kbits),
	(j_bytesize,	1,	kbytes),
	(j_minvalue,	1,	kminval),
	(j_maxvalue,	1,	kmaxval),

	(j_chr,			1,	kchr),
	(j_asc,			1,	kasc),
	(j_sqr,			1,	ksqr),
	(j_cube,		1,	kcube),
	(j_sqrt,		1,	ksqrt),
	(j_sign,		1,	ksign),
	(j_sin,			1,	ksin),
	(j_cos,			1,	kcos),
	(j_tan,			1,	ktan),
	(j_asin,		1,	kasin),
	(j_acos,		1,	kacos),
	(j_atan,		1,	katan),
	(j_ln,			1,	kln),
	(j_lg,			1,	klg),
	(j_log,			1,	klog),
	(j_exp,			1,	kexp),
	(j_round,		1,	kround),
	(j_ceil,		1,	kceil),
	(j_fract,		1,	kfract),
	(j_floor,		1,	kfloor),

	(j_addto,		2,	kaddto),
	(j_subto,		2,	ksubto),
	(j_multo,		2,	kmulto),
	(j_divto,		2,	kdivto),
	(j_negto,		2,	knegto),
	(j_iandto,		2,	kiandto),
	(j_iorto,		2,	kiorto),
	(j_ixorto,		2,	kixorto),
	(j_shlto,		2,	kshlto),
	(j_shrto,		2,	kshrto),
	(j_minto,		2,	kminto),
	(j_maxto,		2,	kmaxto),
	(j_concatto,	2,	kconcatto),
	(j_appendto,	2,	kappendto),

	(j_preincrx,	1,	kincrload),
	(j_postincrx,	1,	kloadincr),
	(j_predecrx,	1,	kdecrload),
	(j_postdecrx,	1,	kloaddecr),

	(j_tostr,		1,	ktostr),
	(j_mixed,		1,	kmixed),
	(j_new,			1,	knew),
	(j_convert,		1,	khardconv),
	(j_index,		2,	kpushix),
	(j_dotindex,	2,	kpushdotix),

	(0,				0,	0)
end
int noperands			!set by getpclop()
int64 dummyop=0			!target for lastopc

proc evalexpr(ref unitrec p)=
int oldmlineno,opc,n,m,t,lowerx,lab1,lab2,oldmodno
ref unitrec a,b,c
ref strec d,owner
int64 x,aa
ichar s
real fsize
[50]ref unitrec mlist
int i,nmult

a:=p^.a
b:=p^.b
c:=p^.c

oldmlineno:=mlineno
oldmodno:=currmoduleno
mlineno:=p^.lineno
currmoduleno:=p^.moduleno

!if dotrace then
!	CPL "DOTAG",jtagnames[p^.tag],=mlineno
!fi

switch p^.tag
when j_print, j_println then
	do_print(p,a,b)

when j_fprint,j_fprintln then
	do_fprint(p,a,b,c)

when j_read, j_readln then
	do_read(p,a,b)
when j_assign, j_deepcopy then
	do_assign(p,a,b)

when j_to then
	do_to(p,a,b,c)

when j_while then
!	do_while(p,checkeqeq(a),b,c)
	do_while(p,a,b,c)

when j_repeat then
!	do_repeat(p,a,checkeqeq(b))
	do_repeat(p,a,b)

when j_forstep, j_forup, j_fordown then
	do_forstep(p,a,b,c)

when j_forall, j_foreach, j_forallrev, j_foreachrev then
	do_forall(p,a,b,c)

when j_do then
	do_do(p,a)

when j_cfor then
	do_cfor(p,a,b)

when j_if then
!	do_if(p,checkeqeq(a),b,c)
	do_if(p,a,b,c)

when j_longif then
	do_longif(p,a,b)

when j_callproc then
	do_callproc(p,a,b)

when j_callhostproc, j_callhostfn then
	do_callhostproc(p,a)

when j_callapplproc then
	do_callappl(p,a,b,0)

when j_callapplfn then
	do_callappl(p,a,b,1)

when j_return then
	do_return(p,a)

when j_preincr, j_predecr, j_postincr, j_postdecr then
	do_preincr(p,a)

when j_swap then
	evalref(a)
	evalref(b)
	genpc(kswap)

when j_exit, j_restart, j_redo, j_next then
	do_exit(p,a)

when j_labeldef then
	lastopc:=cast(&dummyop)
!	lastopc:=cast(&dummyop)
	d:=p^.def
	if d^.index=0 then
		d^.index:=pcindex+1
	else
		lab1:=d^.index			!.index is 16-bit field; definefwdlabel uses ref int
		definefwdlabel(lab1)
	fi

!	genpc_int(klabel,d^.index)

when j_goto then
	do_goto(p,a)

when j_stop then
	if a then
		evalexpr(a)
	else
		genpc(kpushz_void)
	fi
	genpc(kstop)

when j_switch, j_doswitch then
	do_switch(p,a,b,c)

when j_case, j_docase then
	do_case(p,a,b,c)

when j_try then
	do_try(p,a,b)

when j_raise then
	evalexpr(a)
	genpc(kraise)

when j_applyop then
	do_applyop(p,a,b,c)

when j_callmproc then
	do_callmproc(p,a,b,0)

when j_eval then
	evalexpr(a)
	genpc_int(kfree,1)

when j_const then
	x:=p^.value
	switch p^.mode
	when tstring then
		s:=p^.svalue
		if p^.slength=0 then
			genpc(kpushz_str)
		else
			genpc_str(kpush_cs,s,p^.slength)
		fi

	when tint,ti64 then
		genpc_int(kpush_ci,p^.value)

	when treal,tr64 then
		if p^.xvalue=0.0 then
			genpc_int(kpushz,treal)
		else
			genpc_int(kpush_cr,p^.value)
		fi
	when tword,tu64 then
		genpc_int(kpush_cw,p^.value)

	when trange then
		genpc_int(kpush_cn,p^.value)

	else
		cpl ttname[p^.mode]
		gerror("CONST: Can't push this type",p)
	endswitch

when j_name then
	d:=p^.def
	switch d^.nameid
	when procid then
		genpc_s(kpush_ap,d)

	when staticid then
		genpc_s(kpush_m,d)

	when frameid then
		genpc_s(kpush_f,d)

	when paramid then
		genpc_s(kpush_f,d)
		if d^.attribs.ax_byrefmode then	!insert extra dereference
			genpc(kpushptr)
		fi

	when labelid then
		if d^.index=0 then
!			d^.index:=++labelno
			d^.index:=createfwdlabel()
		fi
		genpc_lab(kpush_al,d^.index)

	when dllprocid then
		genpc_s(kpush_ad,d)

	else
		println namenames[d^.nameid],d^.name
		gerror("Name?",p)
	endswitch

when j_andand,j_add,j_sub,j_mul,j_div,j_fdiv,j_ddiv,
					j_rem,j_iand,j_ior,j_ixor,j_shl,j_shr,j_in,j_notin,j_inrev,j_min,j_max,j_addptr,j_subptr,
					j_concat,j_append,j_atan2,j_power,j_isequal,j_divrem then
dobinop::
	if b=nil then
		gerror("Binop: opnd missing",p)
	fi

	opc:=getpclop(p^.tag)
	evalexpr(a)
	evalexpr(b)
	genpc(opc)

when j_eq,j_ne,j_lt,j_le,j_gt,j_ge then
	if b=nil then
		gerror("Binop: opnd missing",p)
	fi

!	p:=checkeqeq(p)

	opc:=getpclop(p^.tag)
	evalexpr(p^.a)
	evalexpr(p^.b)
	genpc(opc)

when j_addto,j_subto,j_multo,j_divto,j_idivto,j_fdivto,j_iandto,j_iorto,j_ixorto,
	 j_shlto,j_shrto,j_minto,j_maxto,j_appendto,j_concatto then
	opc:=getpclop(p^.tag)
	evalref(a)
	evalexpr(b)
	genpc(opc)

when j_idiv then
	do_idiv(a,b)

when j_andl then
	do_and(a,b)

when j_orl then
	do_or(a,b)

when j_xorl then
	evalexpr(a)
	if not islogical(a) then
		genpc(kistrue)
	fi
	evalexpr(b)
	if not islogical(a) then
		genpc(kistrue)
	fi
	genpc(kixor)

when j_ptr then			!de-ref
	if a^.tag=j_const then
		gerror("pushptr/const")
	fi
	evalexpr(a)
	genpc(kpushptr)

when j_notl then
	evalexpr(a)
	if not islogical(a) then
		genpc(kistrue)
	fi
	genpc(knot)

when j_bytesize then
	if a^.tag=j_typeconst then
		m:=a^.mode
		if ttbasetype[m]>=tu1 and ttbasetype[m]<=tu4 then
			fsize:=ttbitwidth[m]/8.0
			genpc_int(kpush_cr,int64@(fsize))
		else
!			genpc_int(kpush_ci,ttsize[m])
			genpushint(ttsize[m])
		fi
	else
		evalexpr(a)
		genpc(getpclop(j_bytesize))
	fi
when j_bitwidth then

	if a^.tag=j_typeconst then
		m:=a^.mode
!		genpc_int(kpush_ci,ttbitwidth[m])
		genpushint(ttbitwidth[m])
	else
		evalexpr(a)
		genpc(getpclop(j_bitwidth))
	fi

when j_minvalue then
	if a^.tag=j_typeconst then
		case ttbasetype[a^.mode]
		when tword,tu1,tu4,tu8,tu16,tu32,tu64 then
			aa:=0
		when ti8 then aa:=-128
		when ti16 then aa:=-32768
		when ti32 then aa:=-0x8000'0000
		when ti64,tint then aa:=0x8000'0000'0000'0000
		else
			goto dominval
		esac
!		genpc_int(kpush_ci,aa)
		genpushint(aa)
	else
dominval::
		evalexpr(a)
		genpc(getpclop(j_minvalue))
	fi

when j_maxvalue then
	if a^.tag=j_typeconst then
		opc:=kpush_ci
		case ttbasetype[a^.mode]
		when tu1 then aa:=1
		when tu2 then aa:=3
		when tu4 then aa:=15
		when tu8 then aa:=255
		when tu16 then aa:=65535
		when tu32 then aa:=0xFFFF'FFFF
		when tu64,tword then aa:=0xFFFF'FFFF'FFFF'FFFF; opc:=kpush_cw
		when ti8 then aa:=127
		when ti16 then aa:=32767
		when ti32 then aa:=0x7FFF'FFFF
		when ti64,tint then aa:=0x7FFF'FFFF'FFFF'FFFF
		when twordm then
			aa:=($targetbits=32|0xFFFF'FFFF|0xFFFF'FFFF'FFFF'FFFF)
		else
			goto domaxval
		esac
		if opc=kpush_ci then
			genpushint(aa)
		else
			genpc_int(opc,aa)
		fi
	else
domaxval::
		evalexpr(a)
		genpc(getpclop(j_maxvalue))
	fi

when j_neg,j_abs,j_inot,j_chr,j_asc,j_sqrt,j_sqr,j_cube,j_sign,j_sin,j_cos,j_tan,j_asin,
					j_acos,j_atan,j_ln,j_lg,j_log,j_exp,j_round,j_floor,j_ceil,j_fract,j_fmod, j_lwb,
					j_upb,j_len,j_bounds, j_gettype,j_getbasetype,j_getelemtype,
					j_isvoid,j_isdef,j_isint,j_isreal,j_isstring,j_isrange,j_islist,j_isrecord,j_isarray,
					j_isset,j_ispointer,j_min1,j_max1,j_istruel, j_isnone,
					j_ismutable then
	opc:=getpclop(p^.tag)
	evalexpr(a)
	genpc(opc)

when j_dictitems then
	genpc(kpushz_void)
	evalexpr(a)
	callhostfn(host_dictitems,1)

when j_callfn then
	if a^.tag=j_name then
		d:=a^.def
		if d^.mode=tvoid and d.nameid<>applprocid then
			cpl d^.name
			gerror("Proc return value")
		end
	fi
	do_callproc(p,a,b)

when j_callmfn then
!CPL "CALLMFN"
	do_callmproc(p,a,b,1)
!
!when j_callhostfn then
!	do_callhostproc(p,a,b)
!
when j_preincrx then
	evalref(a)
	genpc(kincrload)

when j_postincrx then
	evalref(a)
	genpc(kloadincr)

when j_predecrx then
	evalref(a)
	genpc(kdecrload)

when j_postdecrx then
	evalref(a)
	genpc(kloaddecr)


when j_index,j_slice then			!p.a[p.b]
	evalexpr(a)
	evalexpr(b)
	genpc(kpushix)

when j_dotindex,j_dotslice then		!p.a.[p.b]
	evalexpr(a)
	evalexpr(b)
	genpc(kpushdotix)

when j_byteindex then			!p.a.<type>[p.b]
	evalexpr(a)
	evalexpr(b)
	genpc_int(kpushbyteix,p^.mode)

when j_keyindex,j_dotkeyindex then		!p.a{p.b}
	evalexpr(b)
	evalexpr(a)
	if c then					!default value
		evalexpr(c)
		genpc(kpushkeyixd)
	else
		genpc(kpushkeyix)
	fi

when j_dot then
	evalexpr(a)		!lhs, should be a record
	d:=b^.def		!d should be field name
	case d^.nameid
	when procid, dllprocid then
		owner:=d^.owner
		case owner^.nameid
		when moduleid,dllmoduleid then		!set owner type to 0
			t:=0	!(won't be able to distinguish between different modules, but can't
					!specify the module in the source anyway
		else
			t:=owner^.mode
		esac
		genpc_int(kpushdotm,t)
		genopnd_s(d)
	when typeid then
		genpc_int2(kpushdott,d^.owner^.mode,d^.mode)
	else					!assume genfieldid
		genpc_int(kpushdot,d^.offset)
	esac

when j_makelist,j_makeconstr then
	if b then
		lowerx:=getconstvalue(b,100)
	else
		lowerx:=1
	fi
	if a=nil then
		if lowerx=1 then
			genpc(kpushz_list)
		else
			genpc_int(kpushz_listl,lowerx)
		fi
	else
		n:=0
		while a do
			++n
			evalexpr(a)
			a:=a^.nextunit
		od
		genpc_int2(kmakelist,n,lowerx)
	fi

when j_makesetlist then
	if a=nil then
		genpc(kpushz_set)
	else
		n:=0
		while a do
			++n
			evalexpr(a)
			a:=a^.nextunit
		od
		genpc_int(kmakeset,n)
	fi

when j_makedict then
	n:=0
	while a do
		++n
		evalexpr(a)
		a:=a^.nextunit
	od
	genpc_int(kmakedict,n)

when j_makerange then
	evalexpr(a)
	evalexpr(b)
	genpc(kmakerange)

when j_assignx,j_deepcopyx then
	do_assign(p,a,b)

when j_ifx then
	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	genjumpcond(kjumpf,a,lab1)
	evalexpr(b)
	genjumpl(lab2)
	definefwdlabel(lab1)
	evalexpr(c)
	genpc(knop)
	definefwdlabel(lab2)

when j_convert then
	do_convert(p^.mode,a)

when j_typepun then
	evalexpr(a)
	genpc_int(ksoftconv,p^.mode)

when j_ptrto then
	if a^.tag=j_ptr then			!^a^ cancel out (a might be byref param)
		evalexpr(a^.a)
	else
		evalref(a)
	fi

when j_addrof then
	evalref(a)
	genpc(kconvptr)

when j_typeconst then
	genpc_int(kpush_t,p^.mode)

when j_selectx then
	do_selectx(a,b,c)

when j_exprlist then
	while a and a^.nextunit do
		do_stmt(a)
		a:=a^.nextunit
	od
	evalexpr(a)

when j_listcomp then
	genpc(kpushz_list)
	do_stmt(a)

when j_typeval then
	genpc_int(kpushz,p^.mode)

when j_keyvalue then		!push values as pairs, to be dealt with by makedict
	evalexpr(a)
	evalexpr(b)

when j_longint then
	genpc_str(kpush_cs,p^.svalue,p^.slength)
	genpc_int(khardconv,tlongint)

when j_sprint then
	do_print(p,a,b)

when j_sfprint then
	do_fprint(p,a,b,c)

when j_clamp then
	do_clamp(a,b,c)

when j_applyopx then
	do_applyopx(a,b,c)

when j_operator then
	opc:=getpclop(p^.opcode)

	genpc_int2(kpush_op,opc,noperands)

when j_multexpr then
	nmult:=0
	while a do
		if nmult>=mlist.len then gerror("Too many mult elems") fi
		mlist[++nmult]:=a
		a:=a^.nextunit
	od
	for i:=nmult downto 1 do
		do_stmt(mlist[i])
	od

else
	cpl jtagnames[p^.tag]
	gerror("E:CAN'T EVALUATE",p)
end
mlineno:=oldmlineno
currmoduleno:=oldmodno
end

proc do_stmt(unit p)=evalexpr(p) end

global function codegen(int n)int=
!generate pccode in memory for module n
!when dowritepc=1, also writes the result to a given file
!when doshowpc=1, outputs the pccode as as text, returning the result as a single string,
!otherwise returns ""
static modulerec m
ref strec d,e

m:=moduletable[n]
stmodule:=m.stmodule
linetable:=m.linetable

initgenpcl(m.sourcelen)

d:=m.stmodule^.deflist

while d do
	if d^.nameid=procid then
		do_procdef(d)

!needs an 'allproclist' to contain all nested procs such as in classes
!but for now, just scan top-level classes for methods
		e:=d^.deflist
		while e do
			if e^.nameid=procid then
				do_procdef(e)
			fi
			e:=e^.nextdef
		od

	elsif d^.nameid=typeid then
		e:=d^.deflist
		while e do
			if e^.nameid=procid then
				do_procdef(e)
			fi
			e:=e^.nextdef
		od
	fi
	d:=d^.nextdef
od

genstartproc(m.stmodule)
genpc(kendmodule)

m.pccode:=pccode
m.npccode:=npccode
m.pcindex:=pcindex
m.linetable:=linetable

moduletable[n]:=m

return 1
end

proc scanidata(ref strec p)=			!SCANIDATA
!p should point to a def (not a list)
ref strec d
!const maxidata=1000
!const maxidata=2000
const maxidata=30000
!const maxidata=120000
[maxidata]ref strec defs
int i,ndefs

genidata(p)

d:=p^.deflist
if not d then return fi
ndefs:=0

while d do
	++ndefs
	if ndefs>maxidata then
		gerror("Too many idata defs")
	fi
	defs[ndefs]:=d
	d:=d^.nextdef
od

for i:=ndefs downto 1 do
	d:=defs[i]
	if d^.attribs.ax_autovar then
		scanidata(d)
	fi
od

for i:=ndefs downto 1 do
	d:=defs[i]
	if not d^.attribs.ax_autovar then
		scanidata(d)
	fi
od
end

proc genidata(ref strec p)=			!GENIDATA
ref unitrec e

if p^.nameid=moduleid then return fi		!done in header

if getscope(p)=importscope then			!no idata, EXCEPT MAYBE CONSTANTS?
	return
fi

if p^.nameid=staticid then
	e:=p^.code
	if e=nil then
		return
	fi
	evalexpr(e)
	genpc_s(kzpop_m+p^.attribs.ax_frame,p)
fi

end

proc initgenpcl(int sourcelen)=			!INITGENPCL
ref strec dgen

initpcl(max(sourcelen/2,1000))

loopindex:=0
stcurrproc:=nil

st_startproc:=getduplnameptr(stmodule,addnamestr($startprocname),procid)
st_startproc^.attribs.ax_global:=1
st_startproc^.mode:=tvoid
adddef_nodupl(stmodule,st_startproc)

end

global proc doprogramstartup=
int i,m
ref strec d

initpcl(1000)

for i:=1 to nmodules do
	m:=moduleinitorder[i]

	d:=finddefstr(moduletable[m].stmodule,$startprocname)
	if d=nil then
		cpl moduletable[m].name
		gerror("Can't find $startproc")
	fi
	genpc_s(kcall,d)
	genopnd_int(0)
od

stopseq:=&pccode^[pcindex+1]				!used as dest for runproc()

genpushint(0)
genpc(kstop)

raiseseq:=&pccode^[pcindex+1]		!used by raise_error()
genpc(kraise)						!bytecode handler step pcptr+n but it is not known
genpc(kraise)						!what n will be, so make sure it will point here!
genpc(kraise)
genpc(kraise)

genpc(kendmodule)
moduletable[0].pccode:=pccode
moduletable[0].npccode:=npccode
moduletable[0].pcindex:=pcindex
moduletable[0].linetable:=linetable

end

proc do_block(ref unitrec p)=			!DO_BLOCK
ref unitrec q

q:=p^.a
while q do
	do_stmt(q)
	q:=q^.nextunit
od
end

proc do_print(ref unitrec p,a,b)=		!DO_PRINT
int issprint
ref unitrec x

issprint:=p^.tag=j_sprint

if issprint then
	callhostfn(host_strstartprint)
else
	if a then
		evalexpr(a)
		callhostfn(host_startprint)
	else
		callhostfn(host_startprintcon)
	fi
fi

x:=b

while x do
	case x^.tag
	when j_fmtitem then
		evalexpr(x^.b)
		evalexpr(x^.a)
		callhostfn(host_print)
	when j_nogap then
		callhostfn(host_printnogap)
	else
		genpc(kpushz_void)
		evalexpr(x)
		callhostfn(host_print)
	esac
	x:=x^.nextunit
od

if p^.tag=j_println then
	callhostfn(host_println)
fi
if issprint then
	genpc(kpushz_void)
	callhostfn(host_strendprint,1)
else
	callhostfn(host_endprint)
fi
end

proc do_fprint (ref unitrec p,a,b,c)=		!DO_FPRINT
int issfprint
ref unitrec x

issfprint:=p^.tag=j_sfprint

if issfprint then
	callhostfn(host_strstartprint)
else
	if a then
		evalexpr(a)
		callhostfn(host_startprint)
	else
		callhostfn(host_startprintcon)
	fi
fi

evalexpr(b)					!format string
callhostfn(host_setformat)

x:=c
while x do
	case x^.tag
	when j_fmtitem then
		evalexpr(x^.b)
		evalexpr(x^.a)
		callhostfn(host_print)
	when j_nogap then
		callhostfn(host_printnogap)
	else
		genpc(kpushz_void)
		evalexpr(x)
		callhostfn(host_print)
	esac
	x:=x^.nextunit
od

if p^.tag=j_fprintln then
	callhostfn(host_println)
fi
if issfprint then
	genpc(kpushz_void)
	callhostfn(host_strendprint,1)
else
	callhostfn(host_endprint)
fi
end

proc do_read (ref unitrec p,a,b)=		!DO_READ
ref unitrec x,xloop

if p^.tag=j_readln then
	if a then
		evalexpr(a)
		callhostfn(host_readln)
	else
		genpc(kpushz_void)
		callhostfn(host_readln)
	fi
fi

xloop:=b
while xloop do
	x:=xloop
	genpc(kpushz_void)
	if x^.tag=j_fmtitem then
		evalexpr(x^.b)
		callhostfn(host_sread,1)
		x:=x^.a
	else
		genpc(kpushz_void)
		callhostfn(host_sread,1)
	fi
	if x^.tag=j_name then
		genpc_s(kpop_m+x^.def^.attribs.ax_frame,x^.def)
		x^.def^.attribs.ax_used:=1
	else
		evalref(x)
		genpc(kpopptr)
	fi
	xloop:=xloop^.nextunit
od
end

proc do_assign (ref unitrec p,a,b)=		!DO_ASSIGN
int fstore,n,mult
ref unitrec q
ref strec d

fstore:=(p^.tag=j_assignx) ior (p^.tag=j_deepcopyx)		!also called for in-expression assignments

evalexpr(b)		!rhs to stack

case p^.tag
when j_deepcopy, j_deepcopyx then
	genpc(kcopy)
esac

if a^.tag=j_multexpr then
	a:=a^.a
	mult:=1
else
	mult:=0
fi

while a do
switch a^.tag
when j_name then
	d:=a^.def
	case d^.nameid
	when frameid then
		d^.attribs.ax_used:=1
	when staticid then
	when paramid then
		if d^.attribs.ax_byrefmode then
			genpc_s(kpush_f,d)
			genpc((fstore|kstoreptr|kpopptr))
			return
		fi
	else
		cpl namenames[d^.nameid],d^.name
		gerror("Can't assign to")
	esac

	genpc_s((fstore|kstore_m|kpop_m)+d^.attribs.ax_frame,d)

when j_index,j_dotindex,j_slice,j_dotslice,j_keyindex,j_dotkeyindex,j_byteindex then
	evalref(a)
	genpc((fstore|kstoreptr|kpopptr))

when j_makelist then			!assign to multiple destinations
	q:=a^.a
	if q=nil then
		gerror("assign to ()?")
	else
		n:=0
		while q do
			++n
			evalref(q)
			q:=q^.nextunit
		od
		genpc_int2(kmakelist,n,1)
	fi
	genpc((fstore|kstoreptr|kpopptr))	!(storeptr unlikely to work with multiple destinations)

when j_dot then
	evalref(a^.a)		!lhs, should be a record
	d:=a^.b^.def		!d should be field name
	genpc_int(kpushdotref,d^.offset)
	genpc((fstore|kstoreptr|kpopptr))	!(storeptr unlikely to work with multiple destinations)

when j_ptr then
	evalref(a)
	genpc((fstore|kstoreptr|kpopptr))	!(storeptr unlikely to work with multiple destinations)

when j_ifx then
	evalref(a)
	genpc((fstore|kstoreptr|kpopptr))	!(storeptr unlikely to work with multiple destinations)

else
	cpl jtagnames[a^.tag]
	gerror("DOASSIGN?",p)
endswitch
if mult then
	a:=a^.nextunit
else
	exit
fi
od

end

proc do_to (ref unitrec p,a,b,c)=		!DO_TO
int lab_a,lab_b,lab_c,lab_d
ref strec temp


lab_a:=definelabel()
temp:=c^.def
evalexpr(a)
genpc_s(kzpop_f,temp)

lab_b:=createfwdlabel()
lab_c:=createfwdlabel()
lab_d:=createfwdlabel()
stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d)

!check for count being nonzero
if a^.tag<>j_const then			!assume const limit is non-zero
!	evalexpr(a)
	genpc_s(kpush_f,temp)
	genpc_int(kpush_ci,0)
	genpc_lab(kjumple,lab_d)

elsif a^.value<=0 then		!const <=0, skip body
	genpc_lab(kjump,lab_d)
fi

definefwdlabel(lab_b)
do_block(b)			!main body
definefwdlabel(lab_c)

genpc_lab(kto_f,lab_b)
genopnd_s(temp)

definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_while (ref unitrec p,a,b,c)=		!DO_WHILE
int lab_ab,lab_c,lab_d

lab_ab:=createfwdlabel()
lab_c:=createfwdlabel()
lab_d:=createfwdlabel()

stacklooplabels(&lab_ab, &lab_ab, &lab_c, &lab_d)

genjumpl(lab_c)		!direct to condition code which is at the end

definefwdlabel(lab_ab)

do_block(b)		!body
definefwdlabel(lab_c)

genjumpcond(kjumpt,a,lab_ab)

if c then
	do_block(c)		!optional else part
fi

definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_repeat (ref unitrec p,a,b)=		!DO_REPEAT
int lab_ab,lab_c,lab_d

lab_ab:=definelabel()
lab_c:=createfwdlabel()
lab_d:=createfwdlabel()
stacklooplabels(&lab_ab, &lab_ab, &lab_c, &lab_d)

do_block(a)

definefwdlabel(lab_c)

genjumpcond(kjumpf,b,lab_ab)
definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_forstep (ref unitrec p,pvar,pbody,pautovar)=		!DO_FORSTEP
! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
! c = [pautovar]
ref unitrec pfrom, pto, pstep, pelse,plimit
ref strec dvar, limitvar
int lab_a,lab_b,lab_c,lab_d,lab_e,opc
int step, fromval, limit, jumpinto

pfrom:=pvar^.nextunit
pto:=pfrom^.nextunit
pstep:=pto^.nextunit

pelse:=pbody^.nextunit
dvar:=pvar^.def
dvar^.attribs.ax_used:=1

!plimit:=0						!SET BECAUSE IT'S USED BELOW

case p^.tag
when j_forup then
	step:=1

when j_fordown then
	step:=-1
else
	step:=getconstvalue(pstep,101)
	if step<>1 and step<>-1 then
		gerror("Can't do for with odd step")
	fi
esac

jumpinto:=1			!assume jumping straight into increment

!now start generating code
lab_a:=definelabel()
lab_b:=createfwdlabel()
lab_c:=createfwdlabel()
lab_d:=createfwdlabel()
lab_e:=(pelse|createfwdlabel()|lab_d)
stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d)

if pfrom^.tag=j_const then		!can incr/decr directly
	fromval:=pfrom^.value
!see if limit is known
	if pto^.tag=j_const then
		limit:=pto^.value
		pto^.mode:=tint
		if (step=-1 and fromval>=limit) or (step=1 and fromval<=limit) then 	!at least 1 iteration
			jumpinto:=0
		fi
	fi
	if jumpinto then
		if step<0 then
			++fromval
		else
			--fromval
		fi
		pfrom^.value:=fromval
	fi
	genpushint(pfrom^.value)

	genpc_s(kpop_m+dvar^.attribs.ax_frame,dvar)
else
	evalexpr(pfrom)
!	genpc_s(kpop_m+isframe_s(dvar),dvar)
	genpc_s(kpop_m+dvar^.attribs.ax_frame,dvar)

	genpc_s((step<0|kincrto_m|kdecrto_m)+dvar^.attribs.ax_frame,dvar)
fi

if pautovar then
	if pto^.tag=j_name and pto^.def^.attribs.ax_frame then		!don't use autovar for frame limit
		pautovar:=nil									!(parser doesn't know if
	fi
fi

if pautovar then
	evalexpr(pto)
	limitvar:=pautovar^.def
	genpc_s(kzpop_f,limitvar)
	pto:=pautovar
else
	limitvar:=pto^.def
fi

if jumpinto then
	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C::
fi
definefwdlabel(lab_b)

do_block(pbody)				!do loop body

definefwdlabel(lab_c)

if pto^.tag=j_const then
	opc:=(step<0|kford_fci|kfor_fci)
else
	opc:=(step<0|kford_ff|kfor_ff)
fi

genpc_lab(opc,lab_b)
genopnd_s(dvar)
genopnd_s(limitvar)

if pelse then
	definefwdlabel(lab_e)
	do_block(pelse)			!any else part
fi

definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_forall (ref unitrec p,pindex,pbody,pautovar)=		!DO_FORALL
!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, pvar, plist
! b = pbody, [pelse]
! c = pautovar

int lab_a,lab_b,lab_c,lab_d,lab_e,step
ref unitrec pvar, plist, pelse, q
ref strec indexvar,vardef,autodef

pvar:=pindex^.nextunit
vardef:=pvar^.def
plist:=pvar^.nextunit
pelse:=pbody^.nextunit
indexvar:=pindex^.def

step:=(p^.tag=j_forall or p^.tag=j_foreach|1|-1)

!set up initial loop var
lab_a:=definelabel()
lab_b:=createfwdlabel()
lab_c:=createfwdlabel()
lab_d:=createfwdlabel()
lab_e:=(pelse|createfwdlabel()|lab_d)
stacklooplabels(&lab_a,&lab_b,&lab_c,&lab_d)

!assume plist is a where bounds are not known
!(can be optimised for a const range or a const list)
evalexpr(plist)			!load the list
genpc(kbounds)			!extract bounds as a range
genpc(kexpandrange)		!expand to two ints, lower then upper

autodef:=pautovar^.def

if step=1 then					!counting upwards
	genpc_s(kzpop_f,autodef)		!limit=upb
	genpc(kdecr)				!index starts at lwb-1 (as goes straight to incr)
!	genpc_s((indexvar^.attribs.ax_autovar|kzpop_f|kpop_f),autodef)
	genpc_s((indexvar^.attribs.ax_autovar|kzpop_f|kpop_f),indexvar)
else							!counting in reverse
	genpc(kincr)				!index starts at upb+1
	genpc_s((indexvar^.attribs.ax_autovar|kzpop_f|kpop_f),indexvar)
	genpc_s(kzpop_f,autodef)		!limit=lwb
fi

genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C::

!now start generating code
definefwdlabel(lab_b)

!start of iteration, set up next loop variable
evalexpr(plist)
evalexpr(pindex)
if p^.tag=j_forall or p^.tag=j_forallrev then
	genpc(kpushix)
else
	genpc(kpushdotix)
fi
genpc_s(kpop_f,vardef)

if pbody^.tag=j_block then		!normal block body
	do_block(pbody)			!do loop body
else					!might be single if-statement
	do_stmt(pbody)
fi

definefwdlabel(lab_c)

genpc_lab((step=1|kfor_ff|kford_ff),lab_b)
genopnd_s(indexvar)
genopnd_s(autodef)

if pelse then
	definefwdlabel(lab_e)
	do_block(pelse)			!any else part
fi

definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_do (ref unitrec p,a)=		!DO_DO
int lab_abc,lab_d,lab_test
lab_abc:=definelabel()
lab_d:=createfwdlabel()

stacklooplabels(&lab_abc, &lab_abc, &lab_abc, &lab_d)

do_block(a)

genjumpl(lab_abc)
definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_cfor (ref unitrec p,a,b)=		!DO_CFOR
!a=init, cond, step
!b=body

int lab_a,lab_b,lab_c,lab_d,lab_test
ref unitrec pinit, pcond, pstep

lab_a:=definelabel()
lab_b:=createfwdlabel()
lab_c:=createfwdlabel()
lab_d:=createfwdlabel()
lab_test:=createfwdlabel()

stacklooplabels(&lab_a, &lab_b, &lab_c, &lab_d)
!CPL =STREXPR(PINIT)
!evalexpr(pinit)
pinit:=a
pcond:=pinit^.nextunit
pstep:=pcond^.nextunit

do_stmt(pinit)

genjumpl(lab_test)		!direct to condition code which is at the end

definefwdlabel(lab_b)

do_block(b)		!body

definefwdlabel(lab_c)
do_stmt(pstep)
definefwdlabel(lab_test)
genjumpcond(kjumpt,pcond,lab_b)

definefwdlabel(lab_d)
unstacklooplabels()
end

proc do_if (ref unitrec p,a,b,pelse) =	!DO_IF
int lab1,lab2

lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)

if pelse then lab2:=createfwdlabel() fi	!label past else part

genjumpcond(kjumpf,a,lab1)

do_block(b)
if pelse then
	genjumpl(lab2)
	definefwdlabel(lab1)
	do_block(pelse)
	definefwdlabel(lab2)
else
	definefwdlabel(lab1)
fi
end

proc do_longif (ref unitrec p,a,b) =	!DO_LONGIF
ref unitrec q
int labend,lab2
labend:=createfwdlabel()

!q:=a^.a
q:=a
while q do					!each q is an elsif pair
	lab2:=createfwdlabel()
!	genjumpcond(kjumpf,checkeqeq(q^.a),lab2)
	genjumpcond(kjumpf,q^.a,lab2)
	do_block(q^.b)
	q:=q^.nextunit
	if q or b then
		genjumpl(labend)
	fi
	definefwdlabel(lab2)
od
if b then		!do else part
	do_block(b)
fi
definefwdlabel(labend)

end

proc do_callproc (ref unitrec p,a,b) =	!DO_CALLPROC
ref strec d,pm
ref unitrec pbody,q,x,r
int naparams,fkeyword,nparams,i,j,k,ffcode,isfn,fbyref

[maxparams]ref unitrec cparams
[maxparams]ref strec dparams
int dparamsdone:=0
ichar name
ref intpc pc

case a^.tag
when j_name then
when j_dot then
	do_callmproc(p,a,b,(p^.tag=j_callfn))
	return
else
	do_callptr(p,a,b)
	return
esac

d:=a^.def
case d^.nameid
when procid,dllprocid then
when applprocid then
	do_callappl(p,a,b,p.tag=j_callfn)
	return
!	GERROR("CALL HOST FN NOT READY")
else
	cpl d^.name,NAMENAMES[D^.NAMEID]
	gerror("Callproc: not proc",p)
esac

pbody:=d^.code

case ffcode:=d^.attribs.ax_fflang
when windowsff,clangff,mlangff then
	do_calldll(p,a,b)
	return
esac

isfn:=d^.mode<>tvoid

if isfn then
	genpc(kpushz_void)			!needed even for statement call
fi

nparams:=d^.attribs.ax_nparams

memset(&cparams,0,cparams[1].bytes*nparams)

!Start with empty, but complete, argument list
!Now fill that in using actual params
fkeyword:=0			!whether keywords used
x:=b						!scan actual params
naparams:=0
while x do
	++naparams
	if naparams>nparams then
		cpl d^.name,naparams,nparams
		gerror("Too many params",p)
	fi

	if fkeyword and x^.tag<>j_keyword then
		gerror("Normal param follows keyword param",p)
	fi
	case x^.tag
	when j_keyword then
		unless x^.a^.tag=j_name then
			gerror("Kwd: not name")
		end

		fkeyword:=1
		name:=x^.a^.def^.name
		if not dparamsdone then
			extractparams(d,&dparams)
			dparamsdone:=1
		fi
		j:=0
		k:=0
		for j:=1 to nparams do
			if eqstring(dparams[j]^.name,name) then
				k:=j
				exit
			fi
		od
		if k=0 then
			cpl name,"in",d^.name
			gerror("Can't find keyword param")
		fi
		if cparams[k] then
			cpl name
			gerror("Param already set")
		fi
		cparams[k]:=x^.b

	when j_null then		!missing paramater
	else
		cparams[naparams]:=x
	esac
	x:=x^.nextunit
od

!Now scan cparams taking care of missing params
for i:=1 to nparams do
	x:=cparams[i]
	if x=nil then		!param not set
		if not dparamsdone then
			extractparams(d,&dparams)
			dparamsdone:=1
		fi
		pm:=dparams[i]
		if pm^.code then	!use default code
			cparams[i]:=pm^.code
		elsif not pm^.attribs.ax_optional then	!error
			cpl pm^.name,"in",d^.name
			gerror("Param not optional")
		fi				!else leave as null (will push void value)
	fi
od

!Finally, push all the params, which need to be done in reverse order
for i:=nparams downto 1 do
	x:=cparams[i]
	if not dparamsdone then
		extractparams(d,&dparams)
		dparamsdone:=1
	fi
	fbyref:=dparams[i]^.attribs.ax_byrefmode

	if x=nil then			!official missing parameter; substitute with void
		if fbyref then
			gerror("&void param")
		fi
		genpc(kpushz_void)
	else
		if fbyref then
			evalref(x)
		else
			evalexpr(x)
		fi
	fi
od  

genpc_s(kcall,d)
genopnd_int(0)

for i:=1 to nparams do			!analyse pushed params in normal order
	r:=cparams[i]
	if r=nil or issimpleparam(r) then
		case lastopc^
		when kaddsp then
			++((lastopc+1)^)			!augment current addsp
		when kcall then
			if (lastopc+2)^<8 then		!stack adj field might be 8-bits and expressed in bytes
				++((lastopc+2)^)		!So limited to 15 vars, or 8 (32-bit varrec)
			else
				genpc_int(kaddsp,1)
			fi
		else
			genpc_int(kaddsp,1)
		esac
	else
		case lastopc^
		when kfree then
			++((lastopc+1)^)			!augment current addsp
		else
			genfree(1)
		esac
	fi
od

if isfn and p^.tag=j_callproc then	!get rid of result
	genfree(1)
fi
!if dotrace then CPL "CALLFNX" fi
end

proc do_callhostproc (ref unitrec p,a) =	!DO_CALLHOSTPROC
int calledasfn,isfn,index,nap,i,nparams,fparams
[10]ref unitrec plist
ref unitrec q

calledasfn:=p^.tag=j_callhostfn

index:=p^.opcode

isfn:=hostisfn[index]
if calledasfn and not isfn then
	gerror("Host proc is not function")
fi

if isfn and index=host_getparam and \
	(a and a^.nextunit=nil and a^.tag=j_const and a^.value=0) then
!	genpc_int(kpush_ci,nprocparamvars)
	genpushint(nprocparamvars)
	return
fi

if isfn then					!callasfn might be 0, but func returns value anyway
	genpc(kpushz_void)
fi

!now stack all param values
!note: some processing is needed for missing/optional/default/keyword params
!simple version for now

q:=a

nap:=0				!number actual params
while q do
	if nap>=plist.upb then
		gerror("far too many host params")
	fi
	++nap

	plist[nap]:=q
	q:=q^.nextunit
od

if index=host_allparams and a=nil then
	nparams:=1
else
	nparams:=nap
fi

if nparams=0 and hostlvset[index] then
	gerror("LV hostfn: needs 1+ params")
fi
fparams:=hostnparams[index]
if nparams>fparams then
	gerror("Hostfn too many params")
fi
to fparams-nparams do
	genpc(kpushz_void)
od

!Finally, push all the params, which need to be done in reverse order
for i:=nparams downto 1 do
	if i=1 and hostlvset[index] then
		evalref(plist[i])
	elsif i=1 and index=host_allparams and nap=0 then
		isfn:=stcurrproc^.mode<>tvoid
		genpc_s(kpush_ap,stcurrproc)
	else
		evalexpr(plist[i])
	fi
od  

callhostfn(index,calledasfn)
end

proc do_return (ref unitrec p,a) =	!DO_RETURN
int isfn

isfn:=stcurrproc^.mode<>tvoid

if a=nil then				!return from proc, or should be
	if isfn then				!probably picked up by TX anyway
		gerror("Fn needs return value",p)
	fi
	if trylevel then
		genpc_int(kaddsp,trylevel)
	fi
	if nprocframevars then
		genjumpl(retindex)
	else
		genpc(kreturn)
	fi
	return
fi

!return value present
if not isfn then
	cpl stcurrproc^.name
	gerror("Can't return value from proc")
fi

evalexpr(a)				!ret value to stack

genpc_s(kzpop_f,stretval)

if trylevel then
	genpc_int(kaddsp,trylevel)
fi

if nprocframevars then		!need to do cleanup at end
	genjumpl(retindex)
else
	genpc(kreturn)
fi
end

proc genstartproc(ref strec dmodule)=			!GENSTARTPROC
int retadjust,lab1,lab2
ref strec stmain, ststart

if st_startproc=nil then
	gerror("$startproc not present")
fi
retadjust:=0

genpc_s(kprocstart,st_startproc)
genopnd_int(0)

st_startproc^.index:=pcindex+1

scanidata(dmodule)

!Take care of startup code
stmain:=finddefstr(stmodule,"main")
ststart:=finddefstr(stmodule,"start")

if stmain and ststart then		!start/main both provided
	lab1:=createfwdlabel()
	lab2:=createfwdlabel()
	genpc(kpushz_void)
	genpc_str(kpush_cs,stmodule^.name,stmodule^.namelen)
	callhostfn(host_ismain,1)
	genpc_lab(kjumpfalse,lab1)
	genpc_s(kcall,stmain)
		genopnd_int(0)
	genpc_lab(kjump,lab2)
	definefwdlabel(lab1)
	genpc_s(kcall,ststart)
		genopnd_int(0)
	definefwdlabel(lab2)
elsif stmain then			!main only
	lab1:=createfwdlabel()
	genpc(kpushz_void)
	genpc_str(kpush_cs,stmodule^.name,stmodule^.namelen)
	callhostfn(host_ismain,1)
	genpc_lab(kjumpfalse,lab1)
	genpc_s(kcall,stmain)
		genopnd_int(0)
	definefwdlabel(lab1)
elsif ststart then			!start only
	genpc_s(kcall,ststart)
		genopnd_int(0)
fi

genpc(kreturn)

genpc(kprocend)
END

proc do_procdef (ref strec p) =		!DO_PROCDEF
int nfreevars,nnofreevars

if p=st_startproc then
	return
fi

stcurrproc:=p

retindex:=createfwdlabel()

genprocentry(p,nfreevars,nnofreevars)

if p^.code=nil then
	CPL "EMPTY PROC BODY",p^.name,scopenames[getscope(p)]
else
	do_block(p^.code)
fi

if p^.mode<>tvoid then			!is a function
	if p^.owner^.nameid<>typeid then		!not a method
		if not checkblockreturn(p^.code) then
			cpl p^.name
			gerror("Function needs explicit return statement",p^.code)
		fi
	fi
fi

definefwdlabel(retindex)			!common return point
genprocexit(nfreevars,nnofreevars)
genpc(kprocend)
end

proc genprocentry (ref strec p, int &nfreevars,&nnofreevars) =		!GENPROCENTRY
int nparamvars,nframevars,isfn,hasretval,fv,nallocvars,ninitvars
int i,j,nextoffset
ref strec d
[maxparams]ref strec varlist
[maxparams]int fvlist
ref unitrec expr
[maxlocals]ref strec locals
int nlocals

d:=p^.deflist			!list of names in proc
isfn:=p^.mode<>tvoid

if isfn then
	stretval:=getduplnameptr(p,addnamestr("$retval"),paramid)
	stretval^.mode:=p^.mode
	stretval^.attribs.ax_autovar:=1
	adddef_nodupl(p,stretval)
else
	stretval:=nil
fi

nparamvars:=nframevars:=0
hasretval:=0

d:=p^.deflist
nlocals:=0
while d do
	if nlocals>=maxlocals then
		gerror("Too many locals")
	fi
	locals[++nlocals]:=d
	d:=d^.nextdef
od

for i:=nlocals downto 1 do
	d:=locals[i]
	case d^.nameid
	when frameid then
		if nframevars>=maxparams then
			mlineno:=d^.lineno
			cpl p^.name,d^.name,nframevars
			gerror("Too many frame vars")
		fi
		++nframevars
		varlist[nframevars]:=d
		fvlist[nframevars]:=0

	when paramid then
		d^.index:=++nparamvars
		if d^.attribs.ax_autovar then		!assume $retval
			hasretval:=1
		fi
	esac
od

nprocframevars:=nframevars		!used elsewhere, in do_return for example
nprocparamvars:=nparamvars-isfn	!don't include $retval
nallocvars:=ninitvars:=0
nfreevars:=nnofreevars:=0

for i:=1 to nframevars do
	d:=varlist[i]
	expr:=nil
	if d^.code then
		expr:=d^.code			!init value
	fi
	if d^.attribs.ax_autovar then	!no init needed; always initialised once in the code
		fv:=0						!also, don't need freeing (always ints)
		++nallocvars
		++nnofreevars				!av$ don't need freeing
	elsif expr then					!no init needed as initialised immediately; but needs freeing
		fv:=1
		++nallocvars
		++nfreevars
	else							!init to void needed; need freeing
		fv:=2
		++ninitvars
		++nfreevars
	fi
	fvlist[i]:=fv
od

!Now sort in order of fv codes, but only results in the offset being assigned
!first offset is -1, ie. one item below frame ptr, next is -2, etc.

nextoffset:=0
for i:=2 downto 0 do			!have normal vars at top, init vars next, then av$ at bottom
	for j:=1 to nframevars do
		if fvlist[j]=i then
			nextoffset-:=1
			varlist[j]^.index:=nextoffset
		fi
	od
od

genpc_s(kprocstart,p)
genopnd_int(nparamvars-hasretval)
p^.index:=pcindex+1

if ninitvars+nallocvars then
	genpc_int(kstackframe,ninitvars+nallocvars)
fi

for i:=1 to nframevars do
	d:=varlist[i]
	if d^.code then
		evalexpr(d^.code)
		d^.attribs.ax_used:=1
		genpc_s(kzpop_f,d)
	fi
od
end

proc genprocexit(int nfree,nnofree)=		!GENPROCEXIT
if nnofree then
	genpc_int(kaddsp,nnofree)
fi
if nfree then
	genfree(nfree)
fi
genpc(kreturn)
end

proc do_preincr (ref unitrec p,a) =	!DO_PREINCR
int isincr

isincr:=(p^.tag=j_preincr) ior (p^.tag=j_postincr)

if a^.tag=j_name and a^.def^.nameid<>paramid then		!params might be byref
	genpc_s((isincr|kincrto_m|kdecrto_m)+a^.def^.attribs.ax_frame,a^.def)
else
	evalref(a)
	genpc((isincr|kincrptr|kdecrptr))
fi
end

proc do_exit (ref unitrec p,a) =		!DO_EXIT
int k,index,n

case p^.tag
when j_restart then k:=1
when j_redo then k:=2
when j_next then k:=3
when j_exit then k:=4
esac

if a then
	index:=a^.value
else
	index:=1
fi

n:=findlooplabel(k,index)
if n=0 then
	gerror("Bad exit/loop index",p)
else

	if trylevel>looptrylevel then
		genpc_int(kaddsp,trylevel-looptrylevel)
	fi
	genjumpl(n)
fi
end

proc do_goto (ref unitrec p,a) =	!DO_GOTO
ref strec d
int ntries,lab

case a^.tag
when j_name then
	d:=a^.def
	if d^.index=0 then
		d^.index:=createfwdlabel()
	fi
	case d^.nameid
	when labelid then
		ntries:=trylevel-d^.offset
		if ntries<0 then
			gerror("Jumping into try block")
		elsif ntries then
			genpc_int(kaddsp,ntries)
		fi
		genpc_lab(kjump,d^.index)
	else
		cpl d^.name
		gerror("Not label name")
	esac
!when j_ptr then
!	if trylevel then gerror("goto ptr/try") fi
!	evalexpr(a^.a)
!	genpc(kjumpptr)
!when j_index then
!	if trylevel then gerror("goto ix/try") fi
!	evalexpr(a)
!	genpc(kjumpptr)
else
	gerror("GOTO PTR")
esac
end

proc do_switch (ref unitrec p,pindex,pwhenthen,pelse) =	!DO_SWITCH
!proc do_switch (ref unitrec p,a,b,c) =	!DO_SWITCH
int minlab,maxlab,x,y,i,n
ref unitrec w,wt

!first a first scan over the when expressions; work out range and whether simple or complex
minlab:=1000000
maxlab:=-1000000			!highest index seen
!valueset:=[]

n:=0				!no. different values
wt:=pwhenthen

while wt do
	w:=wt^.a
	while w do
		case w^.tag
		when j_const then
			case w^.mode
			when trange then
				x:=w^.range_lower
				y:=w^.range_upper
dorange::
				for i:=x to y do
					minlab :=min(minlab,i)
					maxlab :=max(maxlab,i)

				od
			when tint then
				x:=y:=w^.value
				goto dorange
			else
				gerror("Switch when1: not const int",w)
			esac
		when j_typeconst then
			x:=y:=w^.mode
			goto dorange
		else
			cpl =strexpr(w),jtagnames[w^.tag]
			gerror("Switch when2: not const",w)
		esac
		w:=w^.nextunit
	od
	wt:=wt^.nextunit
od

if maxlab-minlab<=maxswitchrange then
	do_simpleswitch(p,pindex,pwhenthen,pelse, minlab,maxlab)
	return
fi

gerror("COMPLEX SWITCH/NOT COMPLETE")
end

proc do_simpleswitch(ref unitrec p,pindex,pwhenthen,pelse, int a,b) =		!DO_SIMPLESWITCH
!a..b is the range of values of the switch which have been checked to
!be in range in terms of span. But the actual values can be anything.
!For example, 1000000 to 10000250 is valid. So, an offset needs to be
!used to bring the range down to 0 to 250

ref unitrec w,wt,q
int loopsw,n,offset,x,y,x0,i,labstmt,elselab
[1..maxswitchrange+1]int labels
int lab_a,lab_b,lab_c,lab_d

loopsw:=p^.tag=j_doswitch

n:=b-a+1
offset:=a-1		!a..b becomes 1..n

if loopsw then
	lab_a:=definelabel()
	lab_d:=createfwdlabel()
	stacklooplabels(&lab_a,&lab_a,&lab_a,&lab_d)
else
	lab_d:=createfwdlabel()
fi

evalexpr(pindex)				!switch index

genpc_int2(kswitch,n,a)

for i:=1 to n do
	genpc_lab(kjumplabel,0)
	labels[i]:=pcindex			!for now, store destination code index
od

genpc_lab(kjumplabel,0)			!else label
labels[n+1]:=pcindex

!scan when statements again, o/p statements

wt:=pwhenthen
while wt do
	labstmt:=definelabel()
	w:=wt^.a
	while w do
		case w^.tag
		when j_const then
			if w^.mode=trange then
				x0:=w^.range_lower
				y:=w^.range_upper
			else				!assume int (as already checked)
				x0:=y:=w^.value
			fi
		when j_typeconst then
			x0:=y:=w^.mode
		esac
		for x:=x0 to y do
			i:=x-offset
			if pccode^[labels[i]] then			!should have been zero
				cpl x,char(x)
				gerror("Dupl switch value")
			fi
			pccode^[labels[i]]:=labstmt
		od
		w:=w^.nextunit
	od

	do_block(wt^.b)

	if not loopsw then
		genjumpl(lab_d)
	else
		genjumpl(lab_a)
	fi
	wt:=wt^.nextunit
od

if pelse then
	if pelse^.nextunit=nil then
		q:=pelse			!q is the only statement of the else
	else
		q:=nil
	fi
	if loopsw and q and q^.tag=j_exit and (q^.a=nil or getconstvalue(q^.a,102)=1) then	!one stmt consisting of exit/1
		elselab:=lab_d
		pelse:=nil
	else
		elselab:=createfwdlabel()
	fi
else
	elselab:=(loopsw|lab_a|lab_d)
fi

!fill in zero entries with else
if pelse then		!do else part
	definefwdlabel(elselab)
	do_block(pelse)
fi
for i:=1 to n do
	if pccode^[labels[i]]=0 then
		pccode^[labels[i]]:=elselab
	fi
od
pccode^[labels[n+1]]:=elselab

if loopsw then
	genjumpl(lab_a)
	definefwdlabel(lab_d)
	unstacklooplabels()
else
	definefwdlabel(lab_d)
fi
end

proc do_case (ref unitrec p,pindex,pwhenthen,pelse) =	!DO_CASE
int lab_a,lab_d

!proc do_case (ref unitrec p,a,b,c) =	!DO_CASE
int loopsw,fmult,labnextwhen,labstmtstart
ref unitrec w,wt

loopsw:=p^.tag=j_docase

if loopsw then
	lab_a:=definelabel()
	lab_d:=createfwdlabel()
	stacklooplabels(&lab_a,&lab_a,&lab_a,&lab_d)
else
	lab_d:=createfwdlabel()
fi

evalexpr(pindex)			!load test expr p to t

wt:=pwhenthen
while wt do
!forall wt in pwhenthen.a do		!for each when-then item (each wt is a block)
	w:=wt^.a
	fmult:=w^.nextunit<>nil
	labnextwhen:=createfwdlabel()

	if fmult then
		labstmtstart:=createfwdlabel()
	fi

	while w do
		evalexpr(w)
		w:=w^.nextunit
		if w then					!not last
			genpc_lab(kjumptesteq,labstmtstart)
		else
			genpc_lab(kjumptestne,labnextwhen)
		fi
	od
	if fmult then
		definefwdlabel(labstmtstart)
	fi
	do_block(wt^.b)

	if not loopsw then
		genjumpl(lab_d)
	else
		genjumpl(lab_a)
	fi
	definefwdlabel(labnextwhen)
	wt:=wt^.nextunit
od

!at else part
genfree(1)

if pelse then
	do_block(pelse)
fi
if loopsw then
	genjumpl(lab_a)
	definefwdlabel(lab_d)
	unstacklooplabels()
else
	definefwdlabel(lab_d)
fi
end

proc do_try (ref unitrec p,a,b) =	!DO_TRY
int labend,labx
ref unitrec ptry,x,pexcept,pexcode

++trylevel
labend:=createfwdlabel()
ptry:=a
labx:=createfwdlabel()

pexcept:=b

if pexcept=nil then
	gerror("try: no except")
elsif pexcept^.nextunit then
	gerror("Try:multiple except block not implemented")
fi

while pexcept do
	pexcode:=pexcept^.a
	if pexcode=nil or pexcode^.nextunit then
		gerror("Try:multiple except codes not implemented")
	fi
	genpc_lab(kpush_try,labx)
	genopnd_int(getconstvalue(pexcode,103))
	genopnd_int(1)
	do_block(ptry)
	genjumpl(labend)
	definefwdlabel(labx)
	do_block(pexcept^.b)
	definefwdlabel(labend)
	pexcept:=pexcept^.nextunit
od

genpc_int(kaddsp,1)
--trylevel
end

proc do_applyop (ref unitrec p,a,b,c) =	!DO_APPLYOP
int lab

if c then
	evalref(b)
	evalexpr(c)
	evalexpr(a)
	genpc_int(kapplyop,2)
else
	evalref(b)
	evalexpr(a)
	genpc_int(kapplyop,1)
fi
lab:=createfwdlabel()
genpc_lab(kjump,lab)		!dummy jump to be moved to runtime-generated code
genpc(knop)					!stop jump being optimised out
definefwdlabel(lab)
end

proc evalref(ref unitrec p)=		!EVALREF
!like evalexpr(), but evaluates a pointer to the value, which must be an l-value
!p represents an expression. It can be a unitrec, const, but not a list (a list constructor
!will use a kmakelist unit)
!dest is a destination string. Special routines such as additem() are used, which take care
!of separators
!evaluate, and concatenate string version to dest string
ref strec d
int lab1,lab2

switch p^.tag
when j_const then
	gerror("ref on const")

when j_name then
	d:=p^.def
	case d^.nameid
	when procid then
		genpc_s(kpush_ap,d)
	when staticid then
		genpc_s(kpush_am,d)
	when frameid then
		genpc_s(kpush_af,d)
		d^.attribs.ax_used:=1
	when paramid then
		if d^.attribs.ax_byrefmode then	!insert extra dereference
			genpc_s(kpush_f,d)
		else
			genpc_s(kpush_af,d)
	fi
	when labelid then
		if d^.index=0 then
			d^.index:=createfwdlabel()
		fi
		genpc_lab(kpush_al,d^.index)
	when dllprocid then
		genpc_s(kpush_ad,d)
	else
		cpl namenames[d^.nameid]
		gerror("&name")
	esac

when j_index,j_slice then
	evalref(p^.a)
	evalexpr(p^.b)
	genpc(kpushixref)

when j_dotindex then
	evalref(p^.a)
	evalexpr(p^.b)
	genpc(kpushdotixref)

when j_dotslice then

!need commented-out lines to make this work properly with bit-slicing
!	evalref(p^.a)
!	evalexpr(p^.b)
!	genpc(kpushdotixref)

!!but works like this for string-slicing, presumably because the rvalue
!!slice created is writable
!
	evalexpr(p^.a)
	evalexpr(p^.b)
	genpc(kpushdotix)

when j_byteindex then
	evalref(p^.a)
	evalexpr(p^.b)
	genpc_int(kpushbyteixref,p^.mode)

when j_keyindex,j_dotkeyindex then
	evalexpr(p^.b)
	evalref(p^.a)
	genpc(kpushkeyixref)

when j_ptr then
	evalexpr(p^.a)

when j_dot then
	evalref(p^.a)		!lhs, should be a record
	d:=p^.b^.def		!d should be field name
	genpc_int(kpushdotref,d^.offset)
when j_ifx then
	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	genjumpcond(kjumpf,p^.a,lab1)
	evalref(p^.b)
	genjumpl(lab2)
	definefwdlabel(lab1)
	evalref(p^.c)
	genpc(knop)
	definefwdlabel(lab2)
else
	cpl jtagnames[p^.tag],=mlineno
	gerror("EVALREF: Can't do tag")
endswitch
end

function getpclop(int opc)int=		!GETOPCNAMEC
!check for c-specific names
int i

for i:=1 to pcl_jcodes.len do
	if pcl_jcodes[i]=opc then
		noperands:=pcl_nopnds[i]
		return pcl_kcodes[i]
	fi
od

cpl jtagnames[opc]
gerror("PCL:GETOPC No Op")
return 0
end

proc genjumpl(int lab)=		!GENJUMPL
!generate unconditional jump to label
!lab is an actual label number (*not* label index)
genpc_lab(kjump,lab)
end

function definelabel:int=		!DEFINELABEL
int lab:=pcindex+1
!int lab:=pcindex

lastopc:=cast(&dummyop)

!genpc_int(klabel,lab)

return lab
end

function createfwdlabel:int=	!CREATEFWDLABEL
int lab
if nextfreelabel=0 then
	cpl maxlabels
	gerror("Too many labels")
fi
lab:=nextfreelabel
nextfreelabel:=labeltable[lab]
labeltable[lab]:=0

return -lab						!special fwd label identified by being negative
end

!function definefwdlabel(int &oldlab):int=
proc definefwdlabel(int &oldlab)=
!lab should be negative
int pc,nextpc,newlab,index

lastopc:=cast(&dummyop)

index:=oldlab

if index>=0 then gerror("deffwdlabel?") fi
index:=-index

newlab:=pcindex+1
!newlab:=pcindex

pc:=labeltable[index]			!start of fwd ref chain
while pc do						!pc is next pc-index of last label ref
	nextpc:=pccode^[pc]
	pccode^[pc]:=newlab
	pc:=nextpc
od

!return fwd label index to free list
labeltable[index]:=nextfreelabel
nextfreelabel:=index

oldlab:=newlab
end

proc stacklooplabels(ref int a,b,c,d)=	!STACKLOOPLABELS
!a is a list of labels associated with a loop, usually 4 in order A,B,C,D
if loopindex>=maxloopindex then
	gerror("Too many nested loops")
fi
++loopindex
loopstack[loopindex,1]:=a
loopstack[loopindex,2]:=b
loopstack[loopindex,3]:=c
loopstack[loopindex,4]:=d
trylevelstack[loopindex]:=trylevel
end

proc unstacklooplabels=	!UNSTACKLOOPLABELS
--loopindex
end

function findlooplabel(int k,n)int=	!FINDLOOPLABEL
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
int i

if n=0 then			!outermost loop
	i:=1
else
	i:=loopindex-(n-1)		!point to entry
fi

if i<1 or i>loopindex then
	gerror("Bad loop index")
fi

looptrylevel:=trylevelstack[i]
return loopstack[i,k]^
end

function issimpleparam(ref unitrec p)int=		!ISSIMPLEPARAM
!p is a unit; return 1 if it looks like the unit is a simple type
switch p^.tag
when j_const then		!will always be copies
	return 1

!when j_add,j_sub,j_mul,j_div,j_idiv, j_iand,j_ior,j_ixor then		!analyze
!	if p^.b^.tag=j_const and p^.b^.b in [tint,treal] then
!		return 1
!	fi
!when j_add,j_sub,j_div,j_idiv, j_iand,j_ior,j_ixor then		!analyze
!	if p^.b^.tag=j_const then
!		case p^.b^.mode
!		when tint, treal then
!!NOTE: possibility of being a bigint
!
!			return 1
!		esac
!	fi

when j_len,j_lwb,j_upb,j_eq,j_ne,j_lt,j_le,j_ge,j_gt,j_inot,j_predecrx,j_preincrx,
					j_postdecrx,j_postincrx then
	return 1
else

endswitch
return 0
END

proc genjumpcond(int opc,ref unitrec p, int lab)=		!GENJUMPCOND
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
int oldmlineno,lab2
ref unitrec q,r
int64 x

oldmlineno:=mlineno
mlineno:=p^.lineno
q:=p^.a
r:=p^.b

switch p^.tag
when j_andl then
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

when j_orl then
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

when j_notl then

	case opc
	when kjumpf then
		genjumpcond(kjumpt,q,lab)
	when kjumpt then
		genjumpcond(kjumpf,q,lab)
	esac

when j_istruel then
	genjumpcond(opc,q,lab)

when j_eq,j_ne,j_lt,j_le,j_ge,j_gt then

	gcomparejump(opc,p,q,r,lab)

when j_name then
	evalexpr(p)
	genpc_lab((opc|kjumptrue|kjumpfalse),lab)

when j_const then
	x:=p^.value

	if p^.mode=tint then goto doelse fi
	if (x<>0 and opc=kjumpt) or (x=0 and opc=kjumpf) then
		genjumpl(lab)
	fi

else				!other expression
doelse::
	case p^.tag
	when j_isdef, j_isvoid then
		evalexpr(q)
		if opc=kjumpt then
			genpc_lab((p^.tag=j_isdef|kjumpdef|kjumpvoid),lab)
		else
			genpc_lab((p^.tag=j_isdef|kjumpvoid|kjumpdef),lab)
		fi
	else
		evalexpr(p)
		genpc_lab((opc|kjumptrue|kjumpfalse),lab)
	esac
endswitch
mlineno:=oldmlineno
end

proc gcomparejump(int jumpopc,ref unitrec p,lhs,rhs, int lab)=	!GCOMPAREJUMP
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
int cond,opc

cond:=p^.tag				!j_eq etc
if jumpopc=kjumpf then			!need to reverse condition
	cond:=reversecond(cond)		!eqop => neop, etc
fi

case cond
when j_eq then opc:=kjumpeq
when j_ne then opc:=kjumpne
when j_lt then opc:=kjumplt
when j_le then opc:=kjumple
when j_ge then opc:=kjumpge
when j_gt then opc:=kjumpgt
esac

evalexpr(lhs)
evalexpr(rhs)
genpc_lab(opc,lab)
end

function reversecond(int op)int=			!REVERSECOND
!reverse conditional operator

case op
when j_eq then return j_ne
when j_ne then return j_eq
when j_lt then return j_ge
when j_le then return j_gt
when j_ge then return j_lt
when j_gt then return j_le
esac
return 0
end

proc do_convert(int m,ref unitrec p)=			!DO_CONVERT
!apply type-conversion t on expression p
!also do constructors
int n,elemmode,i,lowerx,lbound
const maxunits=50
[maxunits]ref unitrec plist

if p^.tag<>j_makelist and p^.tag<>j_makeconstr then		!assume regular type conversion
doconv::
	evalexpr(p)
	genpc_int(khardconv,m)
	return
fi

!a is a usertype
n:=unitstoarray(p^.a,&plist,maxunits)

case ttbasetype[m]
when trecord,tstruct then
	if n<ttlength[m] then
		cpl ttname[m]
		gerror("Too few fields",p)
	elsif n>ttlength[m] then
		cpl ttname[m]
		gerror("Too many fields",p)
	fi
	for i:=1 to n do
		evalexpr(plist[i])
	od
	genpc_int2((ttbasetype[m]=trecord|kmakerecord|kmakestruct),n,m)

when tlist then		!probably just a list prefix used
	if p^.b then
		lowerx:=getconstvalue(p^.b,104)
	else
		lowerx:=1
	fi
	if n=0 then
		genpc_int(kpushz_listl,lowerx)
	else
		for i:=1 to n do
			evalexpr(plist[i])
		od
		genpc_int2(kmakelist,n,lowerx)
	fi

when tarray then
	for i:=1 to n do		!any elements need to be pushed
		evalexpr(plist[i])
	od

	if m=tarray then	!generic array, not a user type; assume int elemtype
		if p^.b then
			lbound:=getconstvalue(p^.b,105)
		else
			lbound:=1
		fi
		if n=0 then
			genpc_int2(kpushz_arrayl,ti32,lbound)
		else
			genpc_int4(kmakearray,n,lbound,tarray,ti32)
		fi

	else						!assume user type
		elemmode:=tttarget[m]
		if p^.b then
			gerror("2:Can't override lwb")
		fi
		lbound:=ttlower[m]

		if ttlength[m] then				!need specific number of elements
			if n<ttlength[m] then
				cpl ttname[m]
				gerror("Too few elements",p)
			elsif n>ttlength[m] then
				cpl ttname[m]
				gerror("Too many elements",p)
			fi
			if n=0 then
				genpc_int2(kpushz_arrayl,elemmode,lbound)
			else
				genpc_int4(kmakearray,n,lbound,m,elemmode)
			fi
		else						!no length: can be anything
			if n=0 then
				genpc_int2(kpushz_arrayl,elemmode,lbound)
			else
				genpc_int4(kmakearray,n,lbound,m,elemmode)
			fi
		fi
	fi

else
	evalexpr(p)
	genpc_int(khardconv,m)
esac
end

proc do_selectx(ref unitrec pindex,pplist,pelse)=		!DO_SELECTX
!generate selectx expression
int n,labend,i,lab,elselab
ref unitrec x

[maxswitchrange]ref unitrec plist
[maxswitchrange+1]int labels

n:=unitstoarray(pplist,&plist,maxswitchrange)

if n>maxswitchrange then
	gerror("Selectx too complex")
fi

labend:=createfwdlabel()

evalexpr(pindex)
genpc_int2(kswitch,n,1)

for i:=1 to n do
	genpc_lab(kjumplabel,0)
	labels[i]:=pcindex		!for now, store destination code index
od
genpc_lab(kjumplabel,0)
labels[n+1]:=pcindex

!scan when statements again, o/p statements
i:=1
for i:=1 to n do
	x:=plist[i]
	lab:=definelabel()

	pccode^[labels[i]]:=lab
	evalexpr(x)

	genjumpl(labend)	!break to end of statement
od

elselab:=definelabel()

pccode^[labels[n+1]]:=elselab

evalexpr(pelse)
genpc(knop)

definefwdlabel(labend)
end

proc do_calldll (ref unitrec p,a,b) =	!DO_CALLDLL
ref strec d,pm
ref unitrec pbody,q,x
int naparams,fkeyword,nparams,i,j,k,resmode,m,varparams,isfn,fbyref,langcode
[maxparams]ref unitrec cparams
[maxparams]ref strec dparams
int dparamsdone:=0
ichar name

d:=a^.def

!check for variadic params
if varparams:=d^.attribs.ax_varparams then
	do_calldllvar(p,a,b)
	return
fi

resmode:=d^.mode

!CPL =D.NAME,D.MODE,=RESMODE

isfn:=resmode<>tvoid

if p^.tag=j_callfn and not isfn then
	gerror("DLL: needs function")
fi 
if p^.tag=j_callproc then		!result not needed
	isfn:=0
	resmode:=tvoid
fi

if isfn then
	genpc(kpushz_void)			!needed even for statement call
fi

!now stack all param values
!note: some processing is needed for missing/optional/default/keyword params
!simple version for now

!Now fill that in using actual params
fkeyword:=0			!whether keywords used
naparams:=0
nparams:=d^.attribs.ax_nparams
memset(&cparams,0,cparams[1].bytes*nparams)

x:=b
while x do
	++naparams
	cparams[naparams]:=nil

	if naparams>nparams then
		cpl d^.name,naparams,nparams
		gerror("Too many params",p)
	fi

	if fkeyword and x^.tag<>j_keyword then
		gerror("Normal param follows keyword param",p)
	fi
	case x^.tag
	when j_keyword then
		fkeyword:=1
		name:=x^.a^.def^.name
		if not dparamsdone then
			extractparams(d,&dparams)
			dparamsdone:=1
		fi
		k:=0
		for j:=1 to nparams do
			if eqstring(dparams[j]^.name,name) then		!found
				k:=j	!d.paramlist[j]
				exit
			fi
		od
		if k=0 then
			cpl name,"in",d^.name
			gerror("Can't find keyword param")
		fi
		if cparams[k] then
			cpl name
			gerror("Param already set")
		fi
		cparams[k]:=x^.b

	when j_null then		!missing paramater
	else
		cparams[naparams]:=x
	esac
	x:=x^.nextunit
od

!Now scan cparams taking care of missing params
for i:=1 to nparams do
	x:=cparams[i]

	if x=nil then		!param not set
		if not dparamsdone then
			extractparams(d,&dparams)
			dparamsdone:=1
		fi
		pm:=dparams[i]
		if pm^.code then	!use default code
			cparams[i]:=pm^.code
		elsif not pm^.attribs.ax_optional then	!error
			cpl pm^.name,"in",d^.name
			gerror("Param not optional")
		fi				!else leave as null (will push void value)
	fi
od

!Finally, push all the params, which need to be done in reverse order
genpc(kstartdll)

for i:=1 to nparams do
	x:=cparams[i]
	if not dparamsdone then
		extractparams(d,&dparams)
		dparamsdone:=1
	fi
	m:=dparams[i]^.mode
	fbyref:=dparams[i]^.attribs.ax_byrefmode
	if fbyref then
		gerror("Byref on dll call")
	fi

	if x=nil then			!official missing parameter; substitute with void
		gerror("Dll missing param - no default")
	else
		evalexpr(x)				!load to normal stack
		genpc_int(kpushdll,m)		!push to dll stack
	fi
od  

!NOTE: to start with, stick to 0 and 1 for C/Windows calls, otherwise current software
!may no longer work.
!Once all interpreters also recognise 'W' and 'C', then switch over to those codes

case d^.attribs.ax_fflang
when windowsff then
	langcode:='W'
when clangff then
	langcode:='C'
when mlangff then
	langcode:='M'
else
 gerror("Bad FF?")
esac

genpc_s(kcalldll,d)
genopnd_int(langcode)
genopnd_int(resmode)
end

function islogical(ref unitrec p)int=			!ISLOGICAL
!return 1 if p is known to have a logical value
case p^.tag
when j_istruel,j_notl,j_andl,j_orl,j_xorl then
	return 1
esac
return 0
end

proc do_and(ref unitrec x,y)=		!DO_AND
int a,b

	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpf,x,a)
	genjumpcond(kjumpf,y,a)

	genpc_int(kpush_ci,1)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpush_ci,0)
	genpc(knop)
	definefwdlabel(b)
end

proc do_or(ref unitrec x,y)=		!DO_OR
int a,b
	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpt,x,a)
	genjumpcond(kjumpt,y,a)
	genpc_int(kpush_ci,0)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpush_ci,1)
	genpc(knop)
	definefwdlabel(b)
end

proc do_callptr (ref unitrec p,pproc,pparams) =	!DO_CALLPTR
!don't know if call target is a proc or function, so assume function
int n,i,j
[maxparams]ref unitrec params
ref unitrec x,q

genpc(kpushz_void)

n:=0
q:=pparams
while q do
	params[++n]:=q
	q:=q^.nextunit
od

for i:=n downto 1 do
	x:=params[i]
	if x^.tag=j_null then			!official missing parameter; substitute with void
		genpc_int(kpushz,tvoid)
	else
		evalexpr(x)				!load to normal stack
	fi
od  

if pproc^.tag<>j_ptr then		!lhs should be a pointer op
!	cpl jtagnames[pproc^.tag]
	gerror("Callptr?")
fi
evalexpr(pproc^.a)				!push lhs, should be a function ptr

genpc_int2(kcallptr,n,0)			!call the function

if n then
	genfree(n)
fi

if p^.tag=j_callproc then	!get rid of result
	genfree(1)
fi

end

proc do_callmproc (ref unitrec p,pproc,pparams,int calledasfn) =	!DO_CALLMPROC
!call method
int n,isfn,i
[maxparams]ref unitrec params
ref unitrec x,pleft

isfn:=1

!don't know if call target is a proc or function, so assume function
if isfn then
	genpc(kpushz_void)
fi

n:=unitstoarray(pparams,&params,maxparams)

for i:=n downto 1 do
	x:=params[i]
	if x^.tag=j_null then			!official missing parameter; substitute with void
		genpc(kpushz_void)
	else
		evalexpr(x)				!load to normal stack
	fi
od  

if pproc^.tag<>j_dot then		!lhs should be a pointer op
	gerror("Callmproc/not dot")
fi
pleft:=pproc^.a

evalref(pleft)					!use part of dotted seq as expression
evalexpr(pproc)				!use all of it as proc address (should yield ref proc)

genpc_int2(kcallptr,n+1,0)			!call the function

genfree(n+1)

if not calledasfn then
!if not isfn then	!get rid of result
	genfree(1)
fi
end

function checkblockreturn(ref unitrec p)int=		!CHECKBLOCKRETURN
!p should be a block unit
!check that the last statement is a return; return 1/0 for return/not return
!just allow or check for return/if/longif for now
ref unitrec q,r

if p=nil then return 0 fi
if p^.tag<>j_block then gerror("CBR?") fi

q:=p^.a
if q=nil then return 0 fi		!empty block

while r:=q^.nextunit do			!get q=last stmt in block
	q:=r
od

case q^.tag
when j_return then			!that's an easy one...
	return 1

when j_if then
	return checkblockreturn(q^.b) and checkblockreturn(q^.c)		!all branches must have a return
when j_longif then
	r:=q^.a						!list of elsif units
	while r do
		if not checkblockreturn(r^.b) then
			return 0
		fi
		r:=r^.nextunit
	od
	return checkblockreturn(q^.b)		!else must have return too
esac
return 0
end

proc genfree(int n)=
genpc_int(kfree,n)
end

proc do_clamp(ref unitrec x,a,b)=
	evalexpr(x)
	evalexpr(a)
	genpc(kmax)
	evalexpr(b)
	genpc(kmin)
end

proc do_applyopx(ref unitrec x,a,b)=
int lab

if b then
	evalexpr(a)
	evalexpr(b)
	evalexpr(x)
	genpc_int(kapplyop,2)
else
	evalexpr(a)
	evalexpr(x)
	genpc_int(kapplyop,1)
fi
lab:=createfwdlabel()
genpc_lab(kjump,lab)		!dummy jump to be moved to runtime-generated code
genpc(knop)					!stop jump being optimised out
definefwdlabel(lab)
end

proc do_calldllvar (ref unitrec p,a,b) =	!DO_CALLDLL
!special version for varparams
ref strec d,pm
ref unitrec pbody,q,x
int naparams,fkeyword,nparams,i,j,k,resmode,m,isfn,t,langcode
[maxparams]ref unitrec cparams
[maxparams]ref strec dparams
int dparamsdone:=0
ichar name


d:=a^.def

!CPL "CALLDLLVAR",d^.name

resmode:=d^.mode
isfn:=resmode<>tvoid

if p^.tag=j_callfn and not isfn then
	gerror("DLL: needs function")
fi 
if p^.tag=j_callproc then		!result not needed
	isfn:=0
	resmode:=tvoid
fi

if isfn then
	genpc(kpushz_void)			!needed even for statement call
fi

!naparams:=b.len

nparams:=d^.attribs.ax_nparams
extractparams(d,&dparams)

!Finally, push all the params, which need to be done in reverse order
genpc(kstartdll)

x:=b
naparams:=0
i:=1

while x do
	++naparams
	evalexpr(x)			!load to normal stack
!need to use void to means that type is not known
	if i<=nparams then
		t:=dparams[i]^.mode
	else
		t:=tvoid					!type not known
	fi

	genpc_int(kpushdll,t)			!push to dll stack

	x:=x^.nextunit
	++i
od  

!NOTE: to start with, stick to 0 and 1 for C/Windows calls, otherwise current software
!may no longer work.
!Once all interpreters also recognise 'W' and 'C', then switch over to those codes

case d^.attribs.ax_fflang
when windowsff then
	langcode:='W'
when clangff then
	langcode:='C'
when mlangff then
	langcode:='M'
else
 gerror("Bad FF?")
esac

genpc_s(kcalldll,d)
genopnd_int(langcode)
genopnd_int(resmode)
end

proc callhostfn(int fnindex,calledasfn=0)=
!assume caller has verified that fn is a function when calledasfn is true
!called should have pushed retval as needed, and <aparams> params

genpc_int(kcallhost,fnindex)

if hostisfn[fnindex] and not calledasfn then
	genfree(1)
fi
end

!proc do_fastforall (ref unitrec p,ivar,xvar,avar,pbody)=		!DO_FASTFORALL
!!called from do_forall when suitable for new kforall bytecodes
!!xvar/avar are single expressions
!!ivar is a single unit, and initially refers to an autovar (for kforall, not kforallnext)
!
!tvar:=p.f
!!CPL "FASTFORALL",STREXPR(IVAR),STREXPR(XVAR),STREXPR(AVAR),STREXPR(TVAR)
!
!doindex:="$" not in ivar.a.name
!
!!CPL =DOINDEX
!
!pelse:=p.e
!
!!set up initial loop var
!lab_a:=definelabel()
!lab_b:=createfwdlabel()
!lab_c:=createfwdlabel()
!lab_d:=createfwdlabel()
!lab_e:=(notnull(pelse)|createfwdlabel()|lab_d)
!stacklooplabels(lab_a,lab_b,lab_c,lab_d)
!
!!genpc(kpush_f,avar)
!evalexpr(avar)
!genpc(kmakeiter,0)
!genpc(kzpop_f,tvar)
!if doindex then
!	evalexpr(avar)
!	genpc(klwb)
!	genpc(kstore_f,ivar)
!fi
!
!genpc(kpush_ci,0)			!need to free loop (as it's not freed in the loop)
!genpc(kpop_f,xvar)			!this is simple way to do it
!
!genjumpl(lab_c)				!straight to increment code
!
!definefwdlabel(lab_b)
!
!if pbody.tag in [j_block, j_codeblock, j_blockdef] then		!normal block body
!	do_block(pbody)			!do loop body
!else					!might be single if-statement
!	do_stmt(pbody)
!fi
!
!definefwdlabel(lab_c)
!
!if doindex then
!	genpc(kforallx,lab_b,tvar,xvar,ivar)
!else
!	genpc(kforall,lab_b,tvar,xvar)
!fi
!
!if notnull(pelse) then
!	definefwdlabel(lab_e)
!	do_block(pelse)			!any else part
!fi
!
!definefwdlabel(lab_d)
!
!!xvar can contain an un-refcounted ref to a list element
!!clear that
!genpc(kpush_ci,0)
!genpc(kzpop_f,xvar)
!
!unstacklooplabels()
!end

proc extractparams(ref strec d, ref[]ref strec params)=
!scan paramlist for proc d, and store in linear array
ref strec p
int i

p:=d^.paramlist
i:=0
while p do
	params^[++i]:=p
	p:=p^.nextparam
od
end

function unitstoarray(ref unitrec p, ref[]ref unitrec plist, int maxunits)int=
!convert a linked list of units to a linear list
!return number of units
int n

n:=0
while p do
	if n>=maxunits then
		gerror("UTO Too many units")
	fi
	plist^[++n]:=p
	p:=p^.nextunit
od
	
return n
end

proc do_idiv(unit a,b)=
int64 x
int n

if b=nil then gerror("Idiv?") fi
evalexpr(a)

if b^.tag=j_const and b^.mode=tint then
	case x:=b^.value
	when 0 then
		gerror("div 0")
	when 1 then
		return
	esac
fi

evalexpr(b)
genpc(kidiv)

end

function ispoweroftwo(int x)int=		!ISPOWEROFTO
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1 not a power of two, or more than 2**31
int a,n

a:=1
n:=0
to 30 do
	++n
	a:=a<<1
	if a=x then
		return n
	fi
od
return 0
end

proc genpushint(word64 a)=
genpc_int(kpush_ci,a)
end

!function checkeqeq(ref unitrec p)ref unitrec=
!int leftop,rightop
!ref unitrec w,y1,y2,z
!
!case p^.tag
!when j_eq,j_ne,j_lt,j_le,j_gt,j_ge then
!	case p^.a^.tag
!	when j_eq,j_ne, j_lt,j_le,j_ge,j_gt then
!	else
!		return p
!	esac
!when j_andl, j_orl then
!	checkeqeq(p^.a)
!	checkeqeq(p^.b)
!	return p
!else
!
!	return p
!esac
!
!w:=p^.a				!w is the x=y branch
!y1:=w^.b				!split y into two
!y2:=y1
!z:=p^.b
!
!leftop:=w^.tag
!rightop:=p^.tag
!p^.tag:=j_andl
!p^.b:=createunit2(rightop,y2,z)
!checkeqeq(w)
!checkeqeq(y2)
!checkeqeq(z)
!
!return p
!end

proc do_callappl (ref unitrec p,a,b, int callasfn) =
ref strec d,pm
ref unitrec pbody,q,x,r
int naparams,fkeyword,nparams,i,j,k,ffcode,isfn,fbyref

[maxparams]ref unitrec params
[maxparams]ref strec dparams
int dparamsdone:=0, index
ichar name
ref intpc pc

d:=a.def
index:=d.index

!CPL "CALLAPP",JTAGNAMES[A.TAG],A.DEF.NAME,NAMENAMES[A.DEF.NAMEID],=B,a.def.index
!
!CPL =INDEX
!CPL =APPLPROCTABLE[INDEX].NAME
!CPL =CALLASFN

pbody:=d^.code

genpc(kpushz_void)			!assume every proc is a function

nparams:=0

r:=b
while r do
	params[++nparams]:=r
	r:=r.nextunit
od

for i:=nparams downto 1 do
	evalexpr(params[i])
od

genpc_int2(kcallappl,index,nparams)

!!free params here (not done in callee)
!(freeing is done by kcallappl handler)
!for i:=1 to nparams do			!analyse pushed params in normal order
!	r:=params[i]
!	case lastopc^
!	when kfree then
!		++((lastopc+1)^)			!augment current addsp
!	else
!		genfree(1)
!	esac
!od

if not callasfn then	!get rid of result
	genfree(1)
fi
end

=== qc_pcllib.m 34/38 ===
!basic utilities for creating, converting, printing pcl instructions

import mlib
import clib
import oslib

import var_types
import var_decls
import qc_support
import qc_tables
import qc_lib
import pq_common
import qc_lex

const combineloads=0
const genlinenos=0

!code for a module is assembled into pccode
global ref[]intpc pccode
global int npccode=0				!current allocated size of pccode
global int pcindex					!index of last entry in pccode
global ref[0:]word16 linetable		!copy of table from moduletable
ref[]byte labelmap
global ref intpc lastopc			!points to start of last pcl instruction in pccode

global [0..cmdnames.upb]int32 cmdnopnds

global const maxlabels=1000
global [maxlabels]int32 labeltable
global int nextfreelabel

int nfields,nallfields		!set up during converttype

global int nconvertedtypes=tlast-1		!maximum type processed by converttype

global ref strec stcurrproc

strbuffer pclv
global ref strbuffer pcl = &pclv

global proc initpcl(int size)=		!INITPCL
!initialise pcl bytecode data
!May not need to free is last byte-code block was stored into moduledata
!if npccode then				!need to reset before each module
!	pcm_free(pccode,npccode)
!fi
int i,j,nn

!npccode:=size			!used fixed size to start off with
npccode:=size*2			!used fixed size to start off with

pccode:=pcm_alloc((npccode+16)*intpc.bytes)
linetable:=pcm_allocz(npccode*word16.bytes)
pcindex:=0

mlineno:=0
!labelno:=0

for i:=1 to klastcmd do
	nn:=0
	for j:=1 to 4 do
		if cmdfmt[i,j]=0 then exit fi
		++nn
	od
	cmdnopnds[i]:=nn
od
end

global proc initpcldata=
int i,j,nn
for i:=1 to klastcmd do
	nn:=0
	for j:=1 to 4 do
		if cmdfmt[i,j]=0 then exit fi
		++nn
	od
	cmdnopnds[i]:=nn
od
stringtable:=alloctable(maxpcstrings,ichar.bytes)
stringlentable:=alloctable(maxpcstrings,int.bytes)
end

global proc initpclgen=
int i

!set up available label entries in a freelist
for i:=1 to maxlabels-1 do
	labeltable[i]:=i+1
od
labeltable[maxlabels]:=0
nextfreelabel:=1

end

proc writepcl3(int pc)=		!WRITEPCL
!write pc instruction to ttdeststr, as a single line of pcl
!index is index of ins in pccode/pcdata
[512]char str
qd fmt
int cmdcode,a,needcomma,i,lineno
ref intpc ptr
ref strec d

ptr:=&pccode^[pc]

cmdcode:=ptr++^

memcpy(&fmt,&cmdfmt[cmdcode],fmt.bytes)

lineno:=linetable^[pc]

!sprintf(&.str,"%5d: %05d ",lineno,pc)
fprint @&.str,"#: # ",lineno:"5",pc:"z5"

gs_str(pcl,&.str)

case cmdcode
when kprocstart then
	gs_str(pcl,"PROC:")
	d:=ref strec(int(ptr^))
	gs_str(pcl,d^.name)
GS_STR(PCL," ")
GS_STRINT(PCL,(PTR+1)^)
	gs_strln(pcl,":")
	return
when kprocend then
	gs_line(pcl)
	return
esac

if labelmap^[pc] then
!	sprintf(&.str,"L%d:",pc)
	fprint @&.str,"L#:",pc
	gs_strln(pcl,&.str)
	gs_str(pcl,"             ")
fi

strcpy(&.str,cmdnames[cmdcode]+1)

a:=1
case cmdcode
when kcallhost then
	a:=2			!skip 1st operand which is the hostfn code already shown
	strcat(&.str,".")
	strcat(&.str,hostfnnames[ptr++^]+5)
esac

gs_leftstr(pcl," ",11,'-')
gs_leftstr(pcl,&.str,23)
gs_str(pcl,"     ")

needcomma:=0

for i:=a to 4 do
	case fmt[i]
	when cnone then
		exit
	else
		if needcomma then gs_str(pcl,", ") fi

		strcpy(&.str,writepclopnd3(fmt[i],ptr++^,i,cmdcode))
		gs_str(pcl,&.str)
		needcomma:=1
	esac
od

gs_line(pcl)
end

function writepclopnd3(int fmt,int64 x,int n,cmdcode)ichar=		!WRITEPCLOPND
!convert pcl operand to string for phase 1 bytecode:
!strings: pointer to zero terminated string
!cmemory: pointer to strec
!cframe:  pointer to strec
!cproc:   pointer to strec
!clabel:  label index

!f=o/p channel
!fmt=single operand code
!x is value of operand
!n is operand @ (1..4)
static [512]char str,str2
ref strec d
ichar suffix
int slen

d:=ref strec(x)

case fmt
when cnone then
	return "None"

when cint,cword then

	case fmt
	when cint then suffix:="i"
!	when tu32 then suffix:="u"
	when cword then suffix:="w"
	else
		suffix:=""
	esac
!	sprintf(&.str,"%lld%s",x,suffix)
	print @&.str,x,,suffix

when creal then
!	sprintf(&.str,"%f",x)
	print @&.str,x,real@(x)

when crange then
!	sprintf(&.str,"%lld..%lld",x iand 0xFFFF'FFFF,x>>32)
	fprint @&.str,"#..#",x iand 0xFFFF'FFFF,x>>32

when cstring then
	slen:=stringlentable^[x]
	if slen>=255 then slen:=255 fi
	memcpy(&.str,stringtable^[x],slen)			!truncate too-long strings
	str[slen+1]:=0
	convertstring(&.str,&.str2)
!	sprintf(&.str,"\"%s\"",&.str2)
	fprint @&.str,"\"#\"",&.str2

when cmemory then
	strcpy(&.str,"[")
	strcat(&.str,getdottedname(d))
	strcat(&.str,"]")

when cframe then
!	sprintf(&.str,"[%s:%d]",getdottedname(d),d^.index)
	fprint @&.str,"[#:#]",getdottedname(d),d^.index

when cproc then
!	sprintf(&.str,"[&%s] %d",getdottedname(d),d^.index)
	fprint @&.str,"[&#] #",getdottedname(d),d^.index

when cdllproc then
!	sprintf(&.str,"[DLL:%s]",getdottedname(d))
	fprint @&.str,"[DLL:#]",getdottedname(d)

when cgenfield then
!	sprintf(&.str,"GENFIELD:%d",int(x))
	print @&.str,"GENFIELD:",,x

when cfield then
!	sprintf(&.str,".%s",d^.name)
	print @&.str,".",,d^.name

when ctype then
!	sprintf(&.str,"T:%s <%d>",ttname[x],int(x))
	fprint @&.str,"T:# <#>",ttname[x],x

when clabel then
!	sprintf(&.str,"L%d",int(x))
	print @&.str,"L",,x

when coperator then
!	sprintf(&.str,"OP:%s",cmdnames[x])
	print @&.str,"OP:",,cmdnames[x]

when capplproc then
!	sprintf(&.str,"[Applproc:%d:%s]",x,applproctable[x].name)
	fprint @&.str,"[Applproc:#:#]",x,applproctable[x].name

else
!	sprintf(&.str,"<%d %s>",fmt,opndnames[fmt])
	fprint @&.str,"<# #>",fmt,opndnames[fmt]
esac
return &.str
end

global function writepccode(ichar caption,int n)ref strbuffer=
!display code currently in pccode/pcopnd
int cmd,pc,i,lastline,line,lab

gs_init(pcl)

gs_str(pcl,"PROC ")
gs_str(pcl,caption)
gs_str(pcl,"/MODULE:")
gs_str(pcl,moduletable[n].name)
gs_str(pcl,"/")
gs_strint(pcl,n)
gs_str(pcl,"/")
gs_strint(pcl,moduletable[n].pcindex)

gs_strln(pcl,":")
gs_line(pcl)

pccode:=moduletable[n].pccode
pcindex:=moduletable[n].pcindex
pc:=1
linetable:=moduletable[n].linetable

!set up labelmap to mark opcodes that are labelled
labelmap:=zalloctable(pcindex,labelmap^[1].bytes)

!scan operands looking for label refs
while pc<=pcindex do
	cmd:=pccode^[pc]
	if cmdfmt[cmd,1]=clabel then
		lab:=pccode^[pc+1]
		labelmap^[lab]:=1
	fi
	pc+:=cmdnopnds[cmd]+1
od

pc:=1

while pc<=pcindex do
	cmd:=pccode^[pc]
	writepcl3(pc)
	pc+:=cmdnopnds[cmd]+1
od
gs_line(pcl)
return pcl
end

global proc genpc(int opc)=
if pcindex>=npccode then
	cpl =pcindex,npccode
	gerror("pccode overflow")
fi
pccode^[++pcindex]:=opc
lastopc:=&pccode^[pcindex]

linetable^[pcindex]:=mlineno
end

global proc genopnd_int(int64 x)=
!no pcindex overflow check needed, as the genpc() check will be sufficient as
!it would allow for enough operands
pccode^[++pcindex]:=x
end

global proc genopnd_s(ref strec d)=
pccode^[++pcindex]:=int64(d)
end

global proc genpc_int(int opc, int64 a)=
genpc(opc)
pccode^[++pcindex]:=a
end

global proc genpc_int2(int opc, int64 a,b)=
genpc(opc)
pccode^[++pcindex]:=a
pccode^[++pcindex]:=b
end

global proc genpc_int4(int opc, int64 a,b,c,d)=
genpc(opc)
genopnd_int(a)
genopnd_int(b)
genopnd_int(c)
genopnd_int(d)
end

global proc genpc_s(int opc, ref strec d)=

if opc=kpush_f and lastopc and lastopc^=kpop_f or opc=kpush_m and lastopc^=kpop_m then
	if int64(d)=pccode^[pcindex] then
		lastopc^:=(opc=kpush_f|kstore_f|kstore_m)
		return
	fi
fi

genpc(opc)
pccode^[++pcindex]:=int64(d)
end

global proc genpc_str(int opc, ichar s, int length)=
genpc(opc)
genopnd_str(s,length)
end

global proc genopnd_str(ichar s, int length)=
!genopnd_int(dint(s))
genopnd_int(addstringtotable(s,length))
end

global proc genpc_lab(int opc, int a)=
int lastpc
genpc(opc)

!need special handling for forward labels (which are either neg ints or pointers)
if a>=0 then
	pccode^[++pcindex]:=a
	return
fi

!a<0 means fwd label index

a:=-a					!make positive
lastpc:=labeltable[a]		!will be 0 (if first ref) or pc index of last ref
pccode^[++pcindex]:=lastpc
labeltable[a]:=pcindex		!extend chain
end

global function isframe_s(ref strec p)int=		!ISFRAME
!return 1 if the in question is a frame/paramid

return p^.attribs.ax_frame
end

global function addstringtotable(ichar s,int length)int=
int i

!NEED A FASTER WAY OF CHECKING DUPL STRINGS, OR LEAVE IT OUT AS IT'S TAKING
!TOO LONG.
!COMPILING MC.QA, reduces mc.pc size from 390KB to 385KB (total strings around
!30KB). So space saving is minimal. Perhaps make it a bit faster...?

!for i:=1 to nstrings do
!	if strcmp(stringtable^[i],s)=0 then
!		return i
!	fi
!od
if nstrings>=maxpcstrings then
	gerror("Too many strings")
fi

++nstrings

stringtable^[nstrings]:=s
stringlentable^[nstrings]:=length
return nstrings
end

=== q_libs_dummy.m 35/38 ===
import mlib
import clib

global function getintlib(ichar name)ichar=
return nil
end

=== ccm_fn. 36/38 ===
mut [0:]ref void handlertable = (nil,
 &k_nop,
 &k_procstart,
 &k_procend,
 &k_endmodule,
 &k_push_m,
 &k_push_f,
 &k_push_am,
 &k_push_af,
 &k_push_ap,
 &k_push_al,
! &k_push_ch,
! &k_push_ci32,
! &k_push_cw32,
! &k_push_cr32,
! &k_push_cn32,
 &k_push_ci,
 &k_push_cw,
 &k_push_cr,
 &k_push_cn,
 &k_push_cs,
 &k_push_t,
 &k_push_op,
 &k_pushz,
 &k_pushz_void,
 &k_pushz_str,
 &k_pushz_list,
 &k_pushz_listl,
 &k_pushz_set,
 &k_pushz_arrayl,
 &k_pop_m,
 &k_pop_f,
 &k_store_m,
 &k_store_f,
 &k_pushptr,
 &k_popptr,
 &k_storeptr,
 &k_zpop_m,
 &k_zpop_f,
 &k_zstore_m,
 &k_zstore_f,
 &k_copy,
 &k_swap,
 &k_convptr,
 &k_jump,
 &k_jumpptr,
 &k_jumptrue,
 &k_jumpfalse,
 &k_jumpdef,
 &k_jumpvoid,
 &k_jumpeq,
 &k_jumpne,
 &k_jumplt,
 &k_jumple,
 &k_jumpge,
 &k_jumpgt,
 &k_jumptesteq,
 &k_jumptestne,
 &k_jumplabel,
 &k_jumpclabel,
 &k_switch,
 &k_cswitch,
 &k_new,
 &k_to_f,
 &k_for_fci,
 &k_for_ff,
 &k_ford_fci,
 &k_ford_ff,
 &k_call,
 &k_callptr,
 &k_return,
 &k_startdll,
 &k_pushdll,
 &k_calldll,
 &k_callhost,
 &k_stackframe,
 &k_free,
 &k_addsp,
 &k_stop,
 &k_test,
 &k_makelist,
 &k_makerecord,
 &k_makearray,
 &k_makestruct,
 &k_makeset,
 &k_makerange,
 &k_makedict,
 &k_pushdot,
 &k_pushdotref,
 &k_softconv,
 &k_hardconv,
 &k_mixed,
 &k_incrptr,
 &k_incrto_m,
 &k_incrto_f,
 &k_loadincr,
 &k_incrload,
 &k_decrptr,
 &k_decrto_m,
 &k_decrto_f,
 &k_loaddecr,
 &k_decrload,
 &k_incr,
 &k_decr,
 &k_neg,
 &k_abs,
 &k_not,
 &k_inot,
 &k_istrue,
 &k_asc,
 &k_chr,
 &k_sqrt,
 &k_sqr,
 &k_cube,
 &k_sin,
 &k_cos,
 &k_tan,
 &k_asin,
 &k_acos,
 &k_atan,
 &k_sign,
 &k_ln,
 &k_log,
 &k_lg,
 &k_exp,
 &k_round,
 &k_floor,
 &k_ceil,
 &k_fract,
 &k_negto,
 &k_absto,
 &k_notto,
 &k_inotto,
 &k_len,
 &k_lwb,
 &k_upb,
 &k_bounds,
 &k_bits,
 &k_bytes,
 &k_type,
 &k_elemtype,
 &k_basetype,
 &k_minval,
 &k_maxval,
 &k_isint,
 &k_isreal,
 &k_isstring,
 &k_isrange,
 &k_isnumber,
 &k_isarray,
 &k_isrecord,
 &k_ispointer,
 &k_ismutable,
 &k_isset,
 &k_isvoid,
 &k_isdef,
 &k_tostr,
 &k_isequal,
 &k_add,
 &k_sub,
 &k_mul,
 &k_div,
 &k_idiv,
 &k_rem,
 &k_divrem,
 &k_iand,
 &k_ior,
 &k_ixor,
 &k_shl,
 &k_shr,
 &k_in,
 &k_notin,
 &k_inrev,
 &k_eq,
 &k_ne,
 &k_lt,
 &k_le,
 &k_ge,
 &k_gt,
 &k_min,
 &k_max,
 &k_concat,
 &k_append,
 &k_power,
 &k_atan2,
 &k_addto,
 &k_subto,
 &k_multo,
 &k_divto,
 &k_idivto,
 &k_iandto,
 &k_iorto,
 &k_ixorto,
 &k_shlto,
 &k_shrto,
 &k_minto,
 &k_maxto,
 &k_concatto,
 &k_appendto,
 &k_pushix,
 &k_pushdotix,
 &k_pushkeyix,
 &k_pushkeyixd,
 &k_pushixref,
 &k_pushdotixref,
 &k_pushkeyixref,
 &k_pushbyteix,
 &k_pushbyteixref,
 &k_appendset,
 &k_pushdotm,
 &k_pushdott,
 &k_push_ad,
 &k_push_try,
 &k_raise,
 &k_applyop,
 &k_makeiter,
 &k_forall,
 &k_forallx,
 &k_foreach,
 &k_foreachx,
 &k_expandrange,
 &k_callappl,
 nil)
=== ccm_host. 37/38 ===
mut []ref void hosttable=(
	&pch_startprint,
	&pch_startprintcon,
	&pch_strstartprint,
	&pch_setformat,
	&pch_endprint,
	&pch_strendprint,
	&pch_print,
	&pch_dprint,
	&pch_println,
	&pch_printnogap,
	&pch_readln,
	&pch_sreadln,
	&pch_sread,
	&pch_rereadln,
	&pch_reread,
	&pch_strtoval,
	&pch_tostr,
	&pch_leftstr,
	&pch_rightstr,
	&pch_convlc,
	&pch_convuc,
	&pch_iconvlc,
	&pch_iconvuc,
	&pch_stop,
	&pch_stopx,
	&pch_ismain,
	&pch_waitkey,
	&pch_testkey,
	&pch_execwait,
	&pch_execcmd,
	&pch_shellexec,
	&pch_system,
	&pch_makestr,
	&pch_makestrslice,
	&pch_makeref,
	&pch_new,
	&pch_newheap,
	&pch_readlines,
	&pch_heapvar,
	&pch_dictitems,
	&pch_freeheap,
	&pch_setoverload,
	&pch_getcmdparam,
	&pch_gethostname,
	&pch_setpcerror,
	&pch_setdebug,
	&pch_test,
	&pch_ticks,
	&pch_sleep,
	&pch_random,
	&pch_findmetafunction,
	&pch_gethash,
	&pch_getos,
	&pch_gethostsize,
	&pch_iswindows,
	&pch_setmesshandler,
	&pch_setfprintf,
	&pch_loadpcl,
	&pch_runpcl,
	&pch_runtask,
	&pch_callext,
	&pch_pcldata,
	&pch_getcstring,
	&pch_getparam,
	&pch_clearlist,
	&pch_makelink,
	&pch_allparams,
	&pch_stackvars,
	&pch_makeempty,
	&pch_errorinfo,
)
=== ccasm_fn. 38/38 ===
mut [0:]ref void asmhandlertable = (nil,
 cast(&ka_nop),
 cast(&ka_procstart),
 cast(&ka_procend),
 cast(&ka_endmodule),
 cast(&ka_push_m),
 cast(&ka_push_f),
 cast(&ka_push_am),
 cast(&ka_push_af),
 cast(&ka_push_ap),
 cast(&ka_push_al),
! cast(&ka_push_ch),
! cast(&ka_push_ci32),
! cast(&ka_push_cw32),
! cast(&ka_push_cr32),
! cast(&ka_push_cn32),
 cast(&ka_push_ci),
 cast(&ka_push_cw),
 cast(&ka_push_cr),
 cast(&ka_push_cn),
 cast(&ka_push_cs),
 cast(&ka_push_t),
 cast(&ka_push_op),
 cast(&ka_pushz),
 cast(&ka_pushz_void),
 cast(&ka_pushz_str),
 cast(&ka_pushz_list),
 cast(&ka_pushz_listl),
 cast(&ka_pushz_set),
 cast(&ka_pushz_arrayl),
 cast(&ka_pop_m),
 cast(&ka_pop_f),
 cast(&ka_store_m),
 cast(&ka_store_f),
 cast(&ka_pushptr),
 cast(&ka_popptr),
 cast(&ka_storeptr),
 cast(&ka_zpop_m),
 cast(&ka_zpop_f),
 cast(&ka_zstore_m),
 cast(&ka_zstore_f),
 cast(&ka_copy),
 cast(&ka_swap),
 cast(&ka_convptr),
 cast(&ka_jump),
 cast(&ka_jumpptr),
 cast(&ka_jumptrue),
 cast(&ka_jumpfalse),
 cast(&ka_jumpdef),
 cast(&ka_jumpvoid),
 cast(&ka_jumpeq),
 cast(&ka_jumpne),
 cast(&ka_jumplt),
 cast(&ka_jumple),
 cast(&ka_jumpge),
 cast(&ka_jumpgt),
 cast(&ka_jumptesteq),
 cast(&ka_jumptestne),
 cast(&ka_jumplabel),
 cast(&ka_jumpclabel),
 cast(&ka_switch),
 cast(&ka_cswitch),
 cast(&ka_new),
 cast(&ka_to_f),
 cast(&ka_for_fci),
 cast(&ka_for_ff),
 cast(&ka_ford_fci),
 cast(&ka_ford_ff),
 cast(&ka_call),
 cast(&ka_callptr),
 cast(&ka_return),
 cast(&ka_startdll),
 cast(&ka_pushdll),
 cast(&ka_calldll),
 cast(&ka_callhost),
 cast(&ka_stackframe),
 cast(&ka_free),
 cast(&ka_addsp),
 cast(&ka_stop),
 cast(&ka_test),
 cast(&ka_makelist),
 cast(&ka_makerecord),
 cast(&ka_makearray),
 cast(&ka_makestruct),
 cast(&ka_makeset),
 cast(&ka_makerange),
 cast(&ka_makedict),
 cast(&ka_pushdot),
 cast(&ka_pushdotref),
 cast(&ka_softconv),
 cast(&ka_hardconv),
 cast(&ka_mixed),
 cast(&ka_incrptr),
 cast(&ka_incrto_m),
 cast(&ka_incrto_f),
 cast(&ka_loadincr),
 cast(&ka_incrload),
 cast(&ka_decrptr),
 cast(&ka_decrto_m),
 cast(&ka_decrto_f),
 cast(&ka_loaddecr),
 cast(&ka_decrload),
 cast(&ka_incr),
 cast(&ka_decr),
 cast(&ka_neg),
 cast(&ka_abs),
 cast(&ka_not),
 cast(&ka_inot),
 cast(&ka_istrue),
 cast(&ka_asc),
 cast(&ka_chr),
 cast(&ka_sqrt),
 cast(&ka_sqr),
 cast(&ka_cube),
 cast(&ka_sin),
 cast(&ka_cos),
 cast(&ka_tan),
 cast(&ka_asin),
 cast(&ka_acos),
 cast(&ka_atan),
 cast(&ka_sign),
 cast(&ka_ln),
 cast(&ka_log),
 cast(&ka_lg),
 cast(&ka_exp),
 cast(&ka_round),
 cast(&ka_floor),
 cast(&ka_ceil),
 cast(&ka_fract),
 cast(&ka_negto),
 cast(&ka_absto),
 cast(&ka_notto),
 cast(&ka_inotto),
 cast(&ka_len),
 cast(&ka_lwb),
 cast(&ka_upb),
 cast(&ka_bounds),
 cast(&ka_bits),
 cast(&ka_bytes),
 cast(&ka_type),
 cast(&ka_elemtype),
 cast(&ka_basetype),
 cast(&ka_minval),
 cast(&ka_maxval),
 cast(&ka_isint),
 cast(&ka_isreal),
 cast(&ka_isstring),
 cast(&ka_isrange),
 cast(&ka_isnumber),
 cast(&ka_isarray),
 cast(&ka_isrecord),
 cast(&ka_ispointer),
 cast(&ka_ismutable),
 cast(&ka_isset),
 cast(&ka_isvoid),
 cast(&ka_isdef),
 cast(&ka_tostr),
 cast(&ka_isequal),
 cast(&ka_add),
 cast(&ka_sub),
 cast(&ka_mul),
 cast(&ka_div),
 cast(&ka_idiv),
 cast(&ka_rem),
 cast(&ka_divrem),
 cast(&ka_iand),
 cast(&ka_ior),
 cast(&ka_ixor),
 cast(&ka_shl),
 cast(&ka_shr),
 cast(&ka_in),
 cast(&ka_notin),
 cast(&ka_inrev),
 cast(&ka_eq),
 cast(&ka_ne),
 cast(&ka_lt),
 cast(&ka_le),
 cast(&ka_ge),
 cast(&ka_gt),
 cast(&ka_min),
 cast(&ka_max),
 cast(&ka_concat),
 cast(&ka_append),
 cast(&ka_power),
 cast(&ka_atan2),
 cast(&ka_addto),
 cast(&ka_subto),
 cast(&ka_multo),
 cast(&ka_divto),
 cast(&ka_idivto),
 cast(&ka_iandto),
 cast(&ka_iorto),
 cast(&ka_ixorto),
 cast(&ka_shlto),
 cast(&ka_shrto),
 cast(&ka_minto),
 cast(&ka_maxto),
 cast(&ka_concatto),
 cast(&ka_appendto),
 cast(&ka_pushix),
 cast(&ka_pushdotix),
 cast(&ka_pushkeyix),
 cast(&ka_pushkeyixd),
 cast(&ka_pushixref),
 cast(&ka_pushdotixref),
 cast(&ka_pushkeyixref),
 cast(&ka_pushbyteix),
 cast(&ka_pushbyteixref),
 cast(&ka_appendset),
 cast(&ka_pushdotm),
 cast(&ka_pushdott),
 cast(&ka_push_ad),
 cast(&ka_push_try),
 cast(&ka_raise),
 cast(&ka_applyop),
 cast(&ka_makeiter),
 cast(&ka_forall),
 cast(&ka_forallx),
 cast(&ka_foreach),
 cast(&ka_foreachx),
 cast(&ka_expandrange),
 cast(&ka_callappl),
 nil)
=== end ===
