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

			print @&.str+m, segmentnames[r.targetsegment]:"6jlt:",,targetoffset:"8zh"
		else
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
S:=NIL
!		s:=decodeinstr(codeptr,baseaddr+offset)
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
