!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order::
! initsectiontable()
! genexe()
! writeexe(filename)

import clib
import mlib
import oslib
import ax_objdecls
import ax_tables
import ax_decls
import ax_lib
import ax_disasm

[maxsearchlibs]int64 libinsttable
[maxsearchlibs]ichar libinstnames
[maxsearchlibs]int libnotable			!index into dlltable

record sectionrec =
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

record importrec = 				!details about all imported symbols
	ref strec def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

record dllrec =					!all imported libraries
	ichar name					!name of library, including .dll
	int nprocs					!no. of imports which use this library
	int nametableoffset			!start of name table in impdir
	int addrtableoffset			!start of addr table (IAT)
	int dllnameoffset			!offset of name within impdir
	int dllextraoffset			!offset of mysterious region just before the name
end

const zsect=3
const dsect=2
const csect=1
const isect=4

const filealign = 512
const sectionalign = 4096
const imagebase = 0x40'0000
int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
ref strec stentrypoint				!symbol to be the entry point
ref strec stentrypoint2
ref strec stentrypoint3

const maxsection = 10
[maxsection]sectionrec sectiontable
int nsections

ref byte importdir				!allowed section data for import directort in .idata

const maximports = 3000
[maximports]importrec importtable
int nimports

const maxlibs = 50
[maxlibs]dllrec dlltable
int ndlls

ref byte datastart
ref byte dataptr
ichar userentrypoint

global proc writeexe(ichar outfile)=
	imagefileheader header
	optionalheader optheader
	int offset,i
	int64 aa

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

!println =filesize, =dataptr-datastart			!these should match

	if fverbose then
		CPL "Writing file:",outfile
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

global proc genexe(ichar entrypoint)=
!manipulate the ss data to fill in all the details needed for exe format

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

	for i to nsearchlibs do
		strcpy(&.filename,searchlibs[i])
		strcat(&.filename,".dll")
		hinst:=os_getdllinst(&.filename)
		if hinst=0 then
			cpl searchlibs[i]
			gerror("Can't load search lib")
		fi
		libinsttable[i]:=hinst
		libinstnames[i]:=pcm_copyheapstring(&.filename)
	od
end

global function writessdata(int fexe)ref strbuffer=
gs_init(dest)
	showssdata(fexe)

	gs_line(dest)
	return dest
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
	gs_str(dest,d^.name)
	gs_str(dest," Size:")
	gs_strint(dest,d^.virtsize)
	gs_line(dest)
	gs_line(dest)

	k:=0
	if d^.segtype<>impdata_seg then
		p:=bufferelemptr(d^.data,0)
	else
		p:=d^.bytedata
	fi
	length:=d^.virtsize

	str[1]:=0

	ref byte baseaddr:=cast(imagebase+d^.virtoffset)

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

	length:=p^.virtsize
	codestart:=codeptr:=bufferelemptr(p^.data,0)
	codeend:=codeptr+length

	ref byte baseaddr:=cast(imagebase+p^.virtoffset)

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
		gs_str(dest,relocnames[r^.reloctype])
		gs_str(dest," Offset: ")
		gs_strint(dest,r^.offset)
		gs_str(dest," ST Index: ")
		gs_strint(dest,r^.stindex)
		gs_str(dest," ")
		gs_str(dest,ss_symboltable^[r^.stindex]^.name)
		gs_line(dest)

		r:=r^.nextreloc
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
		gs_strln(dest,ss_symboltable^[i]^.name)
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

function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
	if n iand (align-1)=0 then return n fi

	return n+(align-(n iand (align-1)))
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

!CPL "EXTRACT:",NAME
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
			if ndlls>=maxlibs then gerror("Too many libs") fi
			libno:=++ndlls

			dlltable[libno].name:=pcm_copyheapstring(&.str)
			dlltable[libno].nprocs:=1
			return (name2|name2|s+1)
!		return s+1
		fi

		++s
	od

!do explicit search
	int n

	for i:=1 to nsearchlibs do
		if os_getdllprocaddr(libinsttable[i],name) then
			n:=i
			exit				!don't need the actual address; just whether it exists
		fi
	else
		println name,moduletable[moduleno].name
		gerror("Can't find external function")
	od

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

!first use of this lib
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
!scan symbol table and build dll and imports list
!this version assumes dlls are encoded into the name of each import
!(otherwise, it means requiring a list of dlls and loading/searching for
!the names: doing real linker work.)

	int i,libno
	ref strec d
	ichar name, libname

	for i:=1 to ss_nsymbols do

		d:=ss_symboltable^[i]
		case d^.symbol
		when importedsym then
			if nimports>=maximports then gerror("genexe: Too many imports") fi
			++nimports

			name:=extractlibname(d^.name,libno,d^.moduleno)

			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d^.importindex:=nimports
		when exportedsym then
			if userentrypoint then
				if eqstring(d^.name,userentrypoint) then
					stentrypoint:=d
				fi
			else
				if eqstring(d^.name,"main") then
					stentrypoint:=d
				elsif eqstring(d^.name,"start") then
					stentrypoint2:=d
!			elsif eqstring(d^.name,"WinMain") then
!				stentrypoint3:=d
				fi
			fi
		esac
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref strec d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s^.data,0)
	r:=s^.relocs

	while r do
		d:=ss_symboltable^[r^.stindex]
		index:=d^.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r^.reloctype
		when rel32_rel then
			if d^.symbol<>importedsym then
				gerror("rel32/not imported")
			fi
			(ref word32(p+r^.offset)^:=thunkoffset-r^.offset-4)

		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d^.symbol=importedsym then
				(ref word32(p+r^.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				case d^.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				esac

				p32:=cast(p+r^.offset)
				p32^:=p32^+u^.virtoffset+imagebase



			fi
		else
			cpl relocnames[r^.reloctype]
			gerror("Can't do this rel type")
		esac

		r:=r^.nextreloc
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

	memset(&header,0,header.bytes)

	header.machine:=0x8664
	header.nsections:=nsections
	header.optheadersize:=optionalheader.bytes
	header.characteristics:=0x22F

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
			gerror("Entry point not found: main or start")
		fi
	fi
	header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint^.offset

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

	header.importtable.size:=0X80

	header.iat.virtualaddr:=fileiatoffset
	header.iat.size:=fileiatsize

	writerecordx(&header,header.bytes)

end

proc writesectionheader(ref sectionrec s)=
	imagesectionheader sheader

	memset(&sheader,0,sheader.bytes)

	strcpy(&sheader.name[1],s^.name)
	sheader.virtual_size:=s^.virtsize
	sheader.virtual_address:=s^.virtoffset
	sheader.rawdata_offset:=s^.rawoffset
	sheader.rawdata_size:=s^.rawsize

	int64 aa
	case s^.segtype
	when zdata_seg then
		sheader.characteristics:=0xC050'0080
	when idata_seg then
		sheader.characteristics:=0xC050'0040
	when code_seg then
		sheader.characteristics:=0x6050'0020
	when impdata_seg then
		sheader.characteristics:=0xC030'0040
	esac
	writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(ref sectionrec s)=

	case s^.segtype
	when impdata_seg then
		writerecordx(s^.bytedata,s^.virtsize)		!rest of section will be zeros
		if s^.rawsize>s^.virtsize then
			dataptr+:=(s^.rawsize-s^.virtsize)
		fi

	when zdata_seg then					!nothing goes to disk
!	dataptr+:=s^.rawsize
	else
		writerecordx(bufferelemptr(s^.data,0),s^.rawsize)
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
		pdir^.implookuprva:=dlltable[i].nametableoffset
		pdir^.impaddressrva:=dlltable[i].addrtableoffset
		pdir^.namerva:=dlltable[i].dllnameoffset
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

!write the thunk table
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

