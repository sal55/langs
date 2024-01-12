const pclinitalloc=128

global ref int pclstart				!point to start of current pcl block
global ref int pclnext				!point to next available int in pcl block
global ref int pclend				!point to last allocated int (with enough margin for on extra instr)
global ref int pcllast				!points to start of last pcl instruction in pcl block
global int pclalloc					!ints allocated

global ref int32 pclsrcstart
global ref int32 pclsrcnext

global int pclcurrlineno			!current line number
const pclelemsize=int.bytes
const pclsrcelemsize=int32.bytes

global const labelinitalloc=8192
global ref[]int labeloffsettable
global int labelalloc
global int nextlabelno
int labelflag

global [0..pclnames.upb]byte pclnopnds

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

!label/block tables are not needed after the pcl sequence has been
!generated. But they are not freed; they can be reused, with their
!current sizes, for the next module. (Might be inefficient if there is one
!very large module, then mainly small ones.)

	labelalloc:=labelinitalloc
	labeloffsettable:=pcm_alloc(int.bytes*labelalloc)
end

global proc resetpcl(int sourcesize)=
	int pclsize

	qpos:=0
	nextlabelno:=0
	pclcurrlineno:=0

!pcl dest is reallocated for each module
!Any current pcl data is presumably retained so that it can be run.

	pclsize:=sourcesize			!estimated num of pcl bytecode elements

	pclalloc:=1024					!min
	while pclalloc<pclsize do
		pclalloc<<:=1
	od

	pclstart:=pcm_alloc(pclalloc*pclelemsize)
	pclnext:=pclstart
	pclend:=pclstart+pclalloc-16			!allow margin for 1-2 pcl ops
	pcllast:=nil

	pclsrcstart:=pcm_alloc(pclalloc*pclsrcelemsize)
	pclsrcnext:=pclsrcstart

end

global proc genpc(int opc)=
!	if opc=0 then
!		GERROR("ZERO PCL OP?")
!	fi
!IF OPC>PCLNAMES.LEN THEN
!GERROR("GENPC:BAD OPC")
!FI

	if pclnext>=pclend then
		extendpcldata()
	fi

!only do overflow check at start of an instruction
	pclnext^:=opc
	pcllast:=pclnext

!IF QPOS=0 THEN
!	CPL "STORE QPOS",QPOS, PCLNAMES[OPC]
!FI
	pclsrcnext^:=qpos

	++pclnext
	++pclsrcnext
	labelflag:=0
end

global proc genopnd_int(int64 x)=
!no pcindex overflow check needed, as the genpc() check will be sufficient as
!it would allow for enough operands
	pclnext++^:=x
	++pclsrcnext
end

global proc genopnd_name(ref strec d)=
!	pclnext++^:=int@(d)
	pclnext++^:=int(d)
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
!	if pcllast^=kpopf and opc=kpushf and not labelflag and (pcllast+1)^=int64@(d) then
	if pcllast^=kpopf and opc=kpushf and not labelflag and (pcllast+1)^=int64(d) then
		pcllast^:=kstoref
		return
	fi

	genpc(opc)
!	pclnext++^:=int64@(d)
	pclnext++^:=int64(d)
	++pclsrcnext
end

global proc genopnd_strz(ichar s)=
!s must be a heap string, be a constant, or otherwise be persistent
!	pclnext++^:=int64@(s)
	pclnext++^:=int64(s)
	++pclsrcnext
end

global proc genopnd_str(object s)=
!s must be a heap string, be a constant, or otherwise be persistent
!	pclnext++^:=int64@(s)
	pclnext++^:=int64(s)
	++pclsrcnext
end

global proc genopnd_obj(object p)=
!	pclnext++^:=int64@(p)
	pclnext++^:=int64(p)
	++pclsrcnext
end

global proc genpc_real(int opc, real x)=
	genpc(opc)
	pclnext++^:=int64@(x)
!	pclnext++^:=int64(x)
	++pclsrcnext
end

global proc genpc_lab(int opc, int a)=
	int lastpc
	genpc(opc)
	genopnd_lab(a)
end

global proc genopnd_lab(int a)=
	int lastpc

	if a>=0 then				!normal, defined label
		pclnext++^:=a
!CPL "GENLAB",=A
		++pclsrcnext
		return
	fi

!a<0 means fwd label index
	a:=-a					!make positive
	lastpc:=labeloffsettable^[a]		!will be 0 (if first ref) or pc index of last ref
	labeloffsettable^[a]:=pclnext-pclstart
	pclnext++^:=lastpc
	++pclsrcnext
end

global proc gencomment(ichar s)=
	genpc(kcomment)
	genopnd_strz(pcm_copyheapstring(s))
end

!global function getdottedname(ref strec p)ichar=
!!build full dotted name for st item p
!	static [256]char str
!	[256]char str2
!	ref strec owner
!
!	strcpy(&.str,p.name)
!	return &.str
!end
!
proc extendpcldata=
	int newpclalloc
	ref int newpclstart
	ref int32 newpclsrcstart

	newpclalloc:=pclalloc*2

!CPL "EXTENDING PCL TABLE TO",=PCLSTART

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
!oldlab should be negative
	int newlab,index,laboffset,pc,nextpc

	index:=lab
	if index>=0 then gerror("deffwdlabel?") fi
	index:=-index

	laboffset:=pclnext-pclstart

	pc:=labeloffsettable^[index]			!start of fwd ref chain
	while pc do						!pc is next pc-index of last label ref
		nextpc:=(pclstart+pc)^
		(pclstart+pc)^:=laboffset
		pc:=nextpc
	od
	labeloffsettable^[index]:=laboffset

	lab:=laboffset
	labelflag:=1
end

!global function isstatic(symbol d)int=
!	return d.nameid=staticid
!end
