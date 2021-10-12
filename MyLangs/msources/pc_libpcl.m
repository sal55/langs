import clib
import mlib
import msys

import pc_lex
import* pci_core

global pcl pcstart			!start of pcl block
global pcl pccurr			!point to current pcl op
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
global const maxlabels=100000			!in one file
!global const maxlabels=800000			!in one file
global [maxlabels]int32 labelmap				!map user labels to global labels
global int labelno						!current highest global labelno
global int maxuserlabel					!set by lex: highest #label
global int labelnooffset				!normally 0; set to offset (maxlabelno) when processing rts
GLOBAL INT NPCL

!const maxgloballabels=50000				!in all files
const maxgloballabels=100000				!in all files
!const maxgloballabels=800000				!in all files
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

!CPL =PCALLOC,=NUNITS

	pcstart:=pcm_allocz(pcalloc*pcelemsize)
	pcend:=pcstart+pcalloc-8

	pccurr:=pcstart-1
	pcfixed:=0
	pcseqno:=0
	pcneedfntable:=0

	labelno:=maxuserlabel:=labelnooffset:=0
	mcldone:=0

	clear rtsproctable

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

end

proc extendpclblock=
	var int newpcalloc, lengthused
	var pcl newpcstart

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

export function pcl_genint128(int128 a,int mode=tpi128)pcl p=
	p:=newpcl()
	p.value128:=a
	p.opndtype:=int128_opnd
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
	d:=addnamestr(s)
	return d
end

global proc strpcl(pcl p)=
	[256]char pmodestr
	[256]char str
	int opcode,defused

	const showformatted=1

	opcode:=p.opcode

	case opcode
	when klabel then
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
		fi

	when memaddr_opnd then
		d:=p.def
		fprint @str,"&##",(d.istruename|"`"|""),d.name

	when label_opnd then
		fprint @str,"## ","#",p.labelno

	when no_opnd then
		return ""
	when int128_opnd then
		print @str,p.value128

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
	if labelno>=maxgloballabels then
		pclerror("Too many global labels")
	fi
	++labelno
end

global function fixlabel(int userlab)int=
	if userlab not in 1..maxlabels then
		perror_s("Label no out of range:",strint(userlab))
	fi
	if labelmap[userlab]=0 then
		labelmap[userlab]:=nextlabel()
		return labelno
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
	pccurr.isexported:=x
	if pccurr.def then
		pccurr.def.isexported:=x
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
