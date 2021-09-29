import clib
import mlib
import* pci_core
import* pci_mcl
import* pci_read

import pc_genss
import pc_writeexe
import pc_writess
!import pc_genc

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

export function pcl_writedllfile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)
	return writeexefile(filename,optim,1)
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
		labelnooffset:=++labelno
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
	ssstr:=writessdata(fexe)
	writegsfile(filename,ssstr)
end

export proc pcl_writeclangfile(ichar filename)=
!	genclang(filename)
end

