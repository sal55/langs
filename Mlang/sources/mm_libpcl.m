global pcl pcstart			!start of pcl block
global pcl pccurr			!point to current pcl op
global pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

int initpcalloc=65536

const pcelemsize = pclrec.bytes

global ichar longstring					!used in stropnd
global int longstringlen
global ichar errormess

global int mcldone

global proc pcl_start(int nunits=0)=
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

	pcstart:=pcm_allocz(pcalloc*pcelemsize)
	pcend:=pcstart+pcalloc-8

	pccurr:=pcstart-1
	pcfixed:=0
	pcseqno:=0
	pcneedfntable:=0

	mlabelno:=0
	mcldone:=0
end

global proc pcl_end=
	if pccurr>=pccurr and pccurr.opcode<>kendprogram then
		genpc(kendprogram)
	fi	
end

proc extendpclblock=
	int newpcalloc, lengthused
	pcl newpcstart

	newpcalloc:=pcalloc*2
	lengthused:=pccurr-pcstart+1

	newpcstart:=pcm_alloc(pcelemsize*newpcalloc)

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

	++pccurr
	pccurr.pos:=mlineno
	return pccurr
end

global proc genpc(int opcode, pcl p=nil) =
	symbol d
	static int seq

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	P.SEQ:=++SEQ

end

global proc genpc_x(int opcode, int x, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
end

global proc genpc_xy(int opcode, int x,y, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

global function genpc_int(int a)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
	return p
end

global function genpc_real(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=real_opnd
	p.r64index:=getrealindex(x)
	return p
end

global function genpc_realimm(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=realimm_opnd
	return p
end

global function genpc_real32(real x)pcl p=
	p:=newpcl()
	p.xvalue32:=x
	p.opndtype:=real32_opnd
	p.r32index:=getreal32index(x)
	return p
end

global function genpc_string(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=string_opnd
	p.strindex:=getstringindex(s)

	return p
end

global function genpc_strimm(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=strimm_opnd

	return p
end

global function genpc_label(int a)pcl p=
	p:=newpcl()
	p.labelno:=a
	p.opndtype:=label_opnd
	return p
end

global function genpc_mem(symbol d)pcl p=
	p:=newpcl()
	if d.atvar and d.equivvar then
		d:=d.equivvar.def
	fi
	p.def:=d

	p.opndtype:=mem_opnd
	return p
end

global function genpc_memaddr(symbol d)pcl p=
	p:=newpcl()
	if d.atvar and d.equivvar then
		d:=d.equivvar.def
	fi
	p.def:=d

	p.opndtype:=memaddr_opnd
	return p
end

global proc genpc_comment(ichar s)=
	genpc(kcomment,genpc_strimm(s))
end

global function genpc_name(ichar s)pcl=
	return genpc_mem(pcl_makesymbol(s))
end

global function genpc_nameaddr(ichar s)pcl=
	return genpc_memaddr(pcl_makesymbol(s))
end

global function genpc_assem(ref void code)pcl p=
	p:=newpcl()
	p.asmcode:=code
	p.opndtype:=assem_opnd
	return p
end

global function pcl_makesymbol(ichar s)symbol d =
	d:=addnamestr(s)
	return d
end

global function strpmode(int m, size)ichar=
	static [64]char str
	if ttisblock[m] then
		fprint @str,"#:#", ttname[m],size
	else
		strcpy(str, ttname[m])
	fi
	return str
end

global proc pcl_settype(int t,size=0)=
	pccurr.pmode:=t
	pccurr.psize:=size
	pccurr.pcat:=stdcat[t]
end

global proc pcl_setxy(int x,y)=
	pccurr.x:=x
	pccurr.y:=y
end

global proc pcl_setscale(int scale)=
	pccurr.scale:=scale
end

global proc pcl_setoffset(int offset)=
	pccurr.extra:=offset
end

global proc pcl_addoffset(int offset)=
	pccurr.extra+:=offset
end

global proc pcl_setincr(int n)=
	pccurr.stepx:=n
end

global proc pcl_setnargs(int n)=
	pccurr.nargs:=n
end

global proc pcl_setnvariadics(int n)=
	pccurr.nvariadics:=n
end

global proc pcl_setalign(int n)=
	pccurr.align:=n
end

global proc pcl_setoldtype(int t)=
	pccurr.oldmode:=t
end

global function pcl_writepclfile(ichar filename)int=
	ref strbuffer d

	d:=writeallpcl()

	return writefile(filename,d.strptr,d.length)
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

global function getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end
