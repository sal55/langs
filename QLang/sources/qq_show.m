!labels are just numbers 1,2,3 which index both of these tables
!labelblocktable is the pclblock no (as all labels are shared across the program)
!labeloffsettable is the offset into the pclblock

!global const labelinitalloc=8192
!global ref[]int labeloffsettable
!global int labelalloc
!global int nextlabelno
ref[0:]int labelmap
int currlineno
symbol currpclproc

strbuffer pclv
global ref strbuffer pcldest = &pclv

const logfile="qq.log"

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	symbol d
	int t,flags
	ichar idname
	int64 a
	real32 x32

	if p=nil then
		return
	fi

	currlineno:=p.pos iand 16777215

!CPL "PRINTUNIT:",P,P.TAG,JTAGNAMES[P.TAG],=LEVEL,CURRLINENO


!if p.lineno then
!fi

	print @dev,p,":"
	print @dev,getprefix(level,prefix,p)

	idname:=jtagnames[p.tag]
	if idname^='j' then ++idname fi
	print @dev,idname,,": "


	case p.tag
	when jname then
		d:=p.def
!		print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
		if d.owner then print @dev,d.owner.name,,"." fi

		print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
		if d.truename and d.nameid=dllprocid then
			print @dev," ",d.truename
		fi

	when jintconst then
		print @dev,p.value

!	when jenumconst then
!		fprint @dev,"# (#:#)",p.value, strmode(p.mode),getenumname(p.mode, p.value)

	when jrealconst then
		print @dev,p.xvalue

	when jstringconst then
		fprint @dev,"""#""",p.svalue

	when jdecimal then
		print @dev,p.svalue,,"L"

	when jcmpchain then
		for i to 4 do
!CP P.CMPGENOP[I],$
			if p.cmpgenop[i]=0 then exit fi
			print @dev,jtagnames[p.cmpgenop[i]],$
		od

	when joperator then
		print @dev, pclnames[p.pclopcode]

!CPL JTAGNAMES.LEN

!when jmakestrtype then
!	print @dev, ttname[p.strtype]

!when jimport then
!	print @dev, p.def.name

!when jprocdef then
!	print @dev, p.def.name
!	if p.mglobal then print @dev, " (global)" fi
!	if p.isfn then print @dev, " (func)" fi
!
!when jrecorddef then
!	print @dev, p.def.name
!	if p.mglobal then print @dev, " (global)" fi
!
!
	when jmakelist then
		print @dev,p.lower,,":",=p.length,ttname[p.elemtype]

!when jnew then
!	print @dev,p.nparams

!when jframesize then
!	print @dev,"Framesize",p.value
!
!when jconvert,jtypepun then
!	print @dev,"Mode",ttname[p.mode]
!
	when jtypeconst,jconvert then
		print @dev,ttname[p.mode]

!when jpacktypeconst then
!	print @dev,getpacktypename(p.value)

!when kdecimal then
!	print @dev,p.svalue,"Len:",p.slength
!
!when ktypeconst then
!	print @dev,typename(p.mode),typename(p.value)
!
!when koperator then
!	print @dev,pclnames[p.opcode]+1
!
!when kconvert,ktypepun then
!	print @dev,convnames[p.opcode]," to:",strmode(p.newmode)
!
!when kmakelist,kmultexpr then
!	print @dev,"Len:",p.length
!
!when kdot then
!	print @dev,"Offset:",p.offset
!
	when jcallhost then
		print @dev,hostfnnames[p.index]+2

!when kindex, kptr then
!
!when kexit,kredo,krestart,knext then
!	print @dev,"#",,p.index
!
	esac

	println @dev
	flags:=jflags[p.tag]

	if flags>=1 then printunitlist(dev,p.a,level+1,"1") fi
	if flags=2 then printunitlist(dev,p.b,level+1,"2") fi

end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
	if p=nil then return fi

	while p do
		printunit(p,level,prefix,dev)
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr

	indentstr[1]:=0
	!if level>10 then level:=10 fi
	if level>20 then level:=10 fi

	to level do
		strcat(&.indentstr,"- ")
	od

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

!	sprintf(&.str,"%04d ",currlineno)
	strcpy(str,strint(currlineno,"z4"))
	return &.str
end

proc gstr(ichar s)=
	gs_str(pcldest,s)
end

proc gstrln(ichar s)=
	gs_strln(pcldest,s)
end

proc gline=
	gs_line(pcldest)
end

proc gstrint(int a)=
	gs_strint(pcldest,a)
end

proc glabeldef(word64 lab)=
	while lab do
		gstr("L")
		gstrint(lab iand 0xFFFF)
		gstr(": ")
		lab>>:=16
	od
	gline()
end

global proc printglobalsymbols(filehandle f=nil)=
	println @f,"PROC Global Symbol Table"
	println @f

	printst(f,stprogram)

!	println @f,"Global Proc Table",nglobalprocs
!	for i to nglobalprocs do
!		println @f,i,,":",globalproctable[i].name,namenames[globalproctable[i].nameid]
!	od

end

global proc printst(filehandle f,symbol p,int level=0)=
	ref strec q

	printstrec(f,p,level)

	q:=p.deflist

	while q<>nil do
		printst(f,q,level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f,symbol p,int level)=
	strec dd
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset,n
	const tabstr="    "
	[256]char str
	ichar s

!CPL "PRINTSTREC",P.NAME,NAMENAMES[P.NAMEID]

!RETURN

	offset:=0
	to level do
		print @f,tabstr
		offset+:=4
		col+:=4
	od

	print @f,padstr(p.name,22-offset,"-")
	print @f, padstr(namenames[p.nameid],12,".")

	col:=40
	dd:=p^


	if dd.isimport then
		print @f,"Imp "
	elsif dd.isglobal then
		print @f,(dd.isglobal|"Glob ","Exp "|"Local ")
	fi

	if dd.mbyref then
		print@f,"byref "
	fi
	if dd.moptional then
		print@f,"opt "
	fi

	if dd.moduleno then
		fprint @f,"Modno:#",dd.moduleno
	fi

	print @f,"=========="

	if dd.owner then
		fprint @str,"(#)",dd.owner.name
		print @f, padstr(&.str,18,"-")
	else
		print @f, padstr("()",18,"-")
	fi


	case dd.nameid
	when fieldid,frameid,paramid,enumid then
		print @f," Ix:",dd.index,," "
		if dd.nameid=fieldid and dd.atfield then
			print @f,"@",dd.atfield.name,$
		fi
		print @f," Offset:",dd.fieldoffset,," "

	when structfieldid then
		print @f," Offset:",dd.fieldoffset,," Ix:",DD.INDEX,$
	when recordid then
		print @f," Nfields:",dd.nfields,," "
	when procid, dllprocid, anonprocid then
		fprint @f," Nparms:# ",dd.nparams,=dd.misfunc

	esac	

	case dd.nameid
	when frameid, staticid,constid,macroid,paramid,dllparamid then
		if dd.code then
			case dd.initcode
			when 3 then s:="::="
			when 2 then s:=":="
			else s:="="
			esac

			print @f, s, strexpr(dd.code).strptr,$
		fi
	esac

	if dd.mode then
		fprint @f,"Mode:#",strmode(dd.mode),dd.mode
	fi
!	fprint @f,"Mode:#",(dd.mode)
!
!	PRINT @F," Moduleno:",P.MODULENO
!
	println @f
	ichar tab:="          "
end

global proc printtypetables(filehandle f)=
	symbol d

CPL "PRINT TYPE TABLES",NUSERXTYPES
	println @f,"PROC TYPE TABLES"
!	for m:=0 to ntypes do
	for m:=0 to ntypes do
!	for m:=tlast+1 to ntypes do
!		fprint @f, "#: #  (#)",i:"3",ttname[i]:"jl12",ttnamedef[i]
		fprintln @f, "#: # ",m:"3",ttname[m]:"jl12"
		d:=ttnamedef[m]

!		if d then
!			println @f,"	ST=",d
			println @f,"	ST=",d
			println @f,"	Len=",ttlength[m], "Lower",ttlower[m]
			println @f,"	Size=",ttsize[m]
			println @f,"	Basetype=",ttbasetype[m],ttname[ttbasetype[m]]
			println @f,"	Target=",tttarget[m],ttname[tttarget[m]]
!			println @f,"	Ispacked=",ttispacked[m]
			println @f,"	Caligned=",ttcaligned[m]

		d:=ttfields[m]
		if d then
			println @f,"	Fields:"
			while d do
				println @f,"		",d.name, (d.mode|strmode(d.mode)|"")
				d:=d.nextdef
			od
		fi

	od

	ref userxrec p
INT M
	p:=userxmodelist
!	rescan:=0

!global [0:maxuserxtype]symbol ttnamedefx
!!global [0:maxuserxtype]symbol ttnamedefx2
!global [0:maxuserxtype]int ttxmap
!global [0:maxuserxtype]byte ttxmoduleno
!
	for i:=1 to nuserxtypes do
		println @f, i, -i,ttnamedefx[i].name
	od
end

global proc showsttree=
	filehandle f
	ifile m
	symbol d
	ref genfieldrec g
	ref procrec p

	return unless fshowst

	f:=fopen("ST","w")
	printglobalsymbols(f)
!	printglobalsymbols_full(f)

	println @f
	println @f,"Modules",nmodules
	for i to nmodules do
		m:=modules[i]
		IF M THEN
			println @f,"	",,i,,":",m.name,=m.compiled,=m.pcstart,=m.pcsize
		ELSE
			PRINTLN @F,"MODULE",I,"MISSING"
		FI
	od

!	println @f
!	println @f,"Source Files",nsourcefiles
!	for i to nsourcefiles do
!!		println @f,"	",,i,,":",m.name,=m.startfn,=m.mainfn,=m.ast,=m.pcstart,=m.pcsize,
!		println @f,"	",,i,,":",sourcefilenames[i],=sourcefilesys[i],=sourcefilesupport[i]
!	od
!
	println @f
	println @f,"PROC Global GenField Table",ngenfields
	for i to ngenfields do
		g:=genfieldtable[i]
		if g=nil then nextloop fi
		fprintln @f,"   #) #:",i,g.def.name
		while g do
			d:=g.def
			println @f,"      ",d.name, namenames[d.nameid],d.owner.name
			g:=g.nextdef
		od
	od
	println @f


	println @f,"DLL Table", nlibfiles
	for i to nlibfiles do
		println @f, i,":",libtable[i].name, dllinsttable[i], libtypes[i]:"c"
	od
	println @f

	println @f,"DLL Proc Table", ndllprocs
	for i to ndllprocs do
		d:=dllproctable[i]
		println @f, i,":",d.name, dllproclibindex[i], dllprocaddr[i],(d.mvarparams|"Variadic"|""),
			libtypes[dllproclibindex[i]]:"c",=D.INDEX,=DLLPROCTABLE[D.INDEX],=D
	od
	println @f

	println @f,"All Proc Table",nproclist
	p:=proclist
	while p do
		println @f,"Proc:",p.def.name,p.def.owner.name
		p:=p.nextproc
	od
	println @f


	fclose(f)
end

global proc showtypes=
	filehandle f
	ref filerec m

	return unless fshowtypes
	return when runcode=run_cc

	f:=fopen("TYPES","w")
	printtypetables(f)

	fclose(f)
end

global proc showpcl(isubprog sp, int pass)=
	filehandle f

	return when runcode=run_cc

	gs_init(pcldest)
	gs_str(pcldest,"PROC ALL PCL pass:")
	gs_strint(pcldest,pass)
	gs_line(pcldest)

	if sp then
		showpcl2(sp, pass)
	else
		for i to nsubprogs do
			showpcl2(subprogs[i], pass)
		od
	fi

!CPL "SHOWPCL",PASS
	f:=fopen((pass|"PCL1","PCL2"|"PCL3"),"w")
	if not f then return fi
	gs_println(pcldest,f)
!CPL "WROTE PCL FILE"
!OS_GETCH()

	fclose(f)
end

global proc showpcl2(isubprog sp, int pass)=

!CPL "SHOWPCL2",SP.NAME

	for i:=sp.firstmodule to sp.lastmodule do
!CPL "WALLPCL",I,MODULES[I].NAME
		writeallpcl(modules[i],pass)
	od
end

global proc showast(isubprog sp, ichar file)=
	filehandle f
	ifile pm
	symbol d
	int k,i

	return when runcode=run_cc

	f:=fopen(file,"w")
CPL "SHOWAST",FILE,=F
	return unless f

	println @f,"PROC",file,,":"


	if sp then
		showast2(f, sp)
	else
		for i to nsubprogs do
			showast2(f, subprogs[i])
		od
	fi

	fclose(f)
end

global proc showast2(filehandle f, isubprog sp)=
	ifile pm
	symbol d, e
	int k,i

	println @f,"Proc Subprog",sp.name,,": ******\n"
	for i:=sp.firstmodule to sp.lastmodule do
		pm:=modules[i]

		println @f,"Module:",pm.name
		printunit(pm.ast, dev:f)
		d:=pm.def.deflist
		while d, d:=d.nextdef do
!			if d.nameid=procid then
			if d.nameid=procid then
!CPL "PROC:",D.NAME
				println @f,"\n---PROC",d.name
				printunit(d.code, dev:f)

				e:=d.deflist
				while e, e:=e.nextdef do
					if e.nameid=anonprocid then
						println @f,"\n---ANONPROC",e.name
CPL "ANON",E.CODE
						printunit(e.code, dev:f)
					fi
				od

			fi
		od
		println @f
	od
end

global proc showlogfile=
	[256]char str
	filehandle logdev

!CPL "SHOWLOG1"

	if fshowpcl1+fshowpcl2+fshowpcl3+fshowast1+fshowast2+
			fshowst+fshowtypes+fshowmodules+fshowstflat=0 then return fi
!CPL "SHOWLOG2",RUNCODE, RUN_CC
	if runcode=run_cc then
		return
	fi
!CPL "SHOWLOG2"

	if fshowst then
		showsttree()
	fi
!CPL "SHOWLOG3"

	if fshowstflat then
		showstflat()
	fi
!CPL "SHOWLOG4"

	if fshowtypes then
		showtypes()
	fi

!CPL "SHOWLOG5"
	logdev:=fopen(logfile,"w")

!CPL "SHOWLOG3",=FSHOWMODULES
	if fshowmodules then showmoduleinfo(logdev) fi
!CPL "SHOWLOG6"

	if runcode>=fixup_cc and fshowpcl3 then addtolog("PCL3",logdev) fi
	if runcode>=gencode_cc and foptimise and fshowpcl2 then addtolog("PCL2",logdev) fi
	if runcode>=gencode_cc and fshowpcl1 then addtolog("PCL1",logdev) fi
	if runcode>=names_cc and fshowast2 then addtolog("AST2",logdev) fi
	if runcode>=parse_cc and fshowast1 then addtolog("AST1",logdev) fi
	if fshowst then addtolog("ST",logdev) fi
	if fshowstflat then addtolog("STFLAT",logdev) fi
	if fshowtypes then addtolog("TYPES",logdev) fi
	fclose(logdev)

!	fprint @&.str,"c:/m/scripts/med.bat -w #",logfile
	fprint @&.str,"c:/m/scripts/med.bat #",logfile
!CPL =&.STR
!os_GETCH()

!	os_execwait(&.str,1,nil)
	os_execwait(str,0,nil)

end

proc addtolog(ichar filename, filehandle logdest)=
filehandle f
int c

f:=fopen(filename,"rb")
if f=nil then return fi

do
	c:=fgetc(f)
	exit when c=c_eof
	fputc(c,logdest)
od
fclose(f)
end

global proc showstflat=
	filehandle f
	symbol p

	return unless fshowstflat

	f:=fopen("STFLAT","w")

	println @f,"GLOBAL FLAT SYMBOL TABLE:"

	for i:=0 to hashtable.upb-1 do
		p:=cast(&hashtable[i])
		if p.name then
			case p.symbolcode
			when namesym then
				println @f,i,p,":",p.name,symbolnames[p.symbolcode]:"d",namenames[p.nameid]
				p:=p.nextdupl
				while p do
					int sym:=p.symbolcode
					if sym=0 then sym:=errorsym fi
					println @f,"	",p,p.name,symbolnames[sym]:"d",namenames[p.nameid],
						"(From",(p.owner|p.owner.name|"-"),,")"
					p:=p.nextdupl
				od
			esac
		fi
	od
!
	fclose(f)
end

global proc showmoduleinfo(filehandle dev)=
	ifile pm
	ref subprogrec ps
	static ichar tab="    "

CPL "SMI0"
	println @dev,"Project Structure:"
	println @dev,"---------------------------------------"
	println @dev,"Modules",nmodules
	for i to nmodules do
		pm:=modules[i]
!CPL "SMI",I,PM

!		if i>1 and pm.subprogno<>modules[i-1].subprogno then
!			println @dev
!		fi
!
		print @dev, tab,i:"2",pm.name:"16jl", "Lead:",pm.islead, "Sys:",pm.issyslib, "Path:",pm.path,
			"Sub:",subprogs[pm.subprogno].name,"File:",pm.filespec
!		if pm.stmacro then
!			print @dev," Alias:",pm.stmacro.name
!		fi
!		print @dev, "START:",pm.startfn
!		if i=mainmoduleno then print @dev, "<MAIN>" fi
!	PRINT @DEV,"<TEMP MODULE INFO>"
		println @dev
	od
	println @dev

	println @dev,"Subprograms",nsubprogs
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab,i,ps.name,"Sys:",ps.issyslib, "Path:",ps.path,
			 "Spec:",ps.filespec,"Comp:",ps.compiled
		if ps.firstmodule then
			print @dev, tab,tab,ps.firstmodule,ps.lastmodule,,": "
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, modules[j].name,$
			od
			println @dev
		fi
	od
	println @dev
!
!	println @dev,"Sourcefiles",nsourcefiles
!	for i to nsourcefiles do
!		println @dev, tab,i,sourcefilenames[i]
!		if sourcefilepaths[i]^ then println @dev, tab,tab,sourcefilepaths[i] fi
!		println @dev, tab,tab,sourcefilespecs[i]
!		println @dev, tab,tab,=sourcefilesizes[i]
!		println @dev, tab,tab,=sourcefilesys[i]
!		println @dev, tab,tab,=sourcefilesupport[i]
!!		println @dev, tab,tab,=sourcefiledupl[i]
!	od
!	println @dev
!
!!	println @dev,"Header Variables:"
!!	for i to headervars.len do
!!		fprintln @dev,"\t#: #",headervarnames[i],headervars[i]
!	od
!	println @dev
!	println @dev,"---------------------------------------"

	return unless stprogram
	println @dev,"Symboltable:"
	symbol d:=stprogram.deflist
	while d, d:=d.nextdef do
		ichar id
		case d.nameid
		when moduleid then id:="Mod"
		when subprogid then id:="Sub"
		else id:="---"
		esac
		fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
	od
	println @dev

end

global proc printsymbol(ref lexrec lp)=
	lexrec l
	l:=lp^

!	printf("%-18s",symbolnames[l.symbol])
!	print symbolnames[l.symbol]:"18 jl"
	print symbolnames[l.symbol]:"d 18 jl"

	case l.symbol
	when namesym then
!	print l.symptr.name

		printstr_n(l.symptr.name,l.symptr.namelen)
	when intconstsym then
		case l.subcode
		when tint then print l.value,"int"
!		when tword then print l.uvalue,"word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """",$
		printstr(l.svalue)
		print $,""""
	when charconstsym then
		print "'",$
		printstr(l.svalue)
		print $,"'"
	when decimalconstsym then
		printstr(l.svalue)
		print "L"
	when assignsym,addrsym,ptrsym,deepcopysym,rangesym then
		print jtagnames[l.subcode]
	elsif l.subcode then
		print "#",l.subcode
	end

	println

end

global function strmode(int t, expand=0)ichar=
	static [2048]char str

	istrmode(t,&.str,expand)
	return str
end

proc istrmode(int t, ichar dest,int expand=1)=
	static [2048]char str
	symbol d

	if t<0 then
		strcpy(dest,"*")
		strcat(dest,ttnamedefx[-t].name)
!		if ttnamedefx2[-t] then
!			strcat(dest,".")
!			strcat(dest,ttnamedefx2[-t].name)
!		fi
		return
	fi

!CPL "MM1",T

	if t<tlast then
!CPL "MM2",T,=TTNAME[T]
		strcpy(dest,ttname[t])
		return
	fi

	case ttbasetype[t]
	when trefpack then
		strcpy(dest,"ref ")
		istrmode(tttarget[t], dest+strlen(dest),0)
	when tvector then
		fprint @dest, "[#..#]",ttlower[t],ttlength[t]+ttlower[t]-1
		istrmode(tttarget[t], dest+strlen(dest),0)

!	when tslice then
!		strcpy(dest, "slice[]")
!		istrmode(tttarget[t], dest+strlen(dest),0)

	when tstruct then
!		if not expand then recase else fi
		if not expand then goto $else fi
		strcpy(dest,"struct(")
dostruct:
		d:=ttfields[t]
		while d, d:=d.nextdef do
			istrmode(d.mode, dest+strlen(dest),0)
			strcat(dest, " ")
			strcat(dest, d.name)
			if d.nextdef then
				strcat(dest, ", ")
			fi
		od
		strcat(dest,")")
	when trecord then
!		if not expand then recase else fi
		if not expand then goto $else fi
		strcpy(dest,"record(")
		goto dostruct

!	when tenum then
!!		if not expand then recase else fi
!		if not expand then $else fi
!		strcpy(dest,"enum(")
!		d:=ttfields[t]
!		while d, d:=d.nextdef do
!			if d.nameid=enumid and d.mode=t then
!				strcat(dest, d.name)
!				strcat(dest, " ")
!			fi
!		od
!		strcat(dest,")")

	else
$else:
!CPL "STRMODE BASETYPE"!,STDTYPENAMES[TTBASETYPE[T]]
		strcpy(dest,ttname[t])
	esac
end

proc writepcl(ref int pcstart,pc, ref int32 pclsource, int pass, ichar sourcecode)=
!write pc instruction to ttdeststr, as a single line of pcl
!index is index of ins in pccode/pcdata
	[512]char str
	qd fmt
	int cmdcode,a,needcomma,i,offset,labeldone,commentdone,soffset,moduleno
	ref strec d
	const tabx="!      ----------"

!CPL "WRITEPCL",PC,PC^!,PCLNAMES[PC^]

	cmdcode:=pc^

	memcpy(&fmt,&pclfmt[cmdcode],fmt.bytes)
	labeldone:=commentdone:=0

	case cmdcode
	WHEN KSKIP THEN
		RETURN

	when kprocdef then
!	CPL "--PROCDEF",SYMBOL(PC^).NAME
		currpclproc:=cast((pc+1)^)
		gstr(tabx)
		gstr("Procdef:")
		gstr(currpclproc.name)
		gline()
		return
	when kprocend then
		gstr(tabx)
		gstrln("End")
		return
	esac

	offset:=PC-PCSTART

	soffset:=(pclsource+offset)^
	currlineno:=soffset iand 16777215
!	moduleno:=soffset.[24..31]


	++pc

!	if currlineno then
		fprint @str,"# [#]: #: ",pc-1:"8zh", currlineno:"05jr", pc-pcstart-1:"4"
!		fprint @str,"# #M #L: #: ",pc-1:"8zh", moduleno:"2",currlineno:"5", pc-pcstart-1:"4"
!	else
!		fprint @str,"# #: ",pc-1:"8zh",pc-pcstart-1:"4"
!	fi

	gstr(&.str)

	offset:=pc-pcstart-1


	if labelmap^[offset] then
!CPL "WRITEPCL LABEL",=OFFSET
		glabeldef(offset)
		gstr(&.str)
	fi

	case cmdcode
	when kprocdef then
		currpclproc:=cast(pc^)
		return
	when kcomment then
		gstr("! ")
		gstrln(cast(pc^))
		return
	when klabeldef then
		gstr(ref strec(pc^).name)
		GSTR(" /")
		GSTR(NAMENAMES[REF STREC(PC^).NAMEID])
		++PC

		gstrln(":")
		return
	esac

	strcpy(&.str,pclnames[cmdcode]+1)

	a:=1
	gs_leftstr(pcldest," ",7,'-')
	gs_leftstr(pcldest,&.str,10)
	gstr("     ")

	needcomma:=0

	for i:=a to 4 do
		case fmt[i]
		when cnone then
			exit
		else
			if needcomma then gstr(", ") fi

			strcpy(&.str,writepclopnd(fmt[i],pc++^,i,cmdcode, pass, pcstart))
			gstr(&.str)
			needcomma:=1
		esac
	od

	gline()
end

function writepclopnd(int fmt,int64 x,int n,cmdcode, pass,ref int pcstart)ichar=		!WRITEPCLOPND
!f=o/p channel
!fmt=single operand code
!x is value of operand
!n is operand @ (1..4)
	static [512]char str,str2
	symbol d
	ichar suffix,s
	int slen
	object p
	ref stringrec ps

!IF PASS=3 THEN
!	RETURN "OPND"
!FI

	d:=ref strec(x)

	case fmt
	when cnone then
		return "None"

	when cint,cword then

		case fmt
		when cint then suffix:=""
		when cword then suffix:="u"
		else
			suffix:=""
		esac
		print @str,x,,suffix,$
		if cmdcode=kcallhost then
			strcat(str,hostfnnames[x]+2)
		fi

	when creal then
		strcpy(str,strreal(real@(x)))

	when crange then
		fprint @str,"#..#",x iand 0xFFFF'FFFF,x>>32

	when cstring then
		if pass<=2 then
!			s:=cast(x)
!			slen:=strlen(s)
!			goto dostring
			ps:=cast(x)
			s:=ps.svalue
			slen:=ps.length
			goto dostring
		else
!RETURN "<STR>"
			p:=cast(x)
			if (slen:=p.length)=0 then return """" fi
			s:=p.strptr
			goto dostring
		fi

	when cstringz then

		s:=cast(x)
		slen:=strlen(s)
	dostring:
		if slen>=255 then slen:=255 fi
		memcpy(&.str,s,slen)			!truncate too-long strings
		str[slen+1]:=0
		convertstring(&.str,&.str2)
		fprint @str,"""#""",&.str2

	when cmemory then
		if pass<=2 then
			strcpy(str,d.name)
		else
!RETURN "MEM"
			d:=nil
			for i to nstatics do
				if statictable[i]=variant(x) then
					d:=staticdefs[i]
					exit
				fi
			od

			fprint @str, "[#] (#:#)", x:"h",(d|d.owner.name|"?"),(d|d.name|"?")
		fi

	when cframe then
		if pass<=2 then
			strcpy(str,d.name)
		else
!RETURN "FRAME"
			d:=currpclproc.deflist

			while d do
				if d.nameid in [frameid, paramid] and d.index*16=x then
					fprint @str,"[#] (#)",x%16,d.name
					return str
				fi
				d:=d.nextdef
			od
		fi

	when csymbol then
		fprint @str,"[#]",d.name

	when cproc then
		if pass<=2 then
			strcpy(str,d.name)
		else
!RETURN "PROC"
			d:=nil
			for i to nprocs do
				if proctable[i]=ref int(x) then
					d:=procdefs[i]
					exit
				fi
			od

			fprint @str, "[#] (#)", x:"h",(d|d.name|"?")
		fi

	when cdllproc then
!		fprint @str,"[DLL:#]",getdottedname(d)
		fprint @str,"[DLL:#]",d.name

	when cgenfield then
		if pass<=2 then
			d:=symbol(x)
			fprint @str,".#",d.name
		else
!RETURN "GENFIELD"
			fprint @str,"## (#)","#",x, genfieldtable[x].def.name
		fi
!
	when ctype then
		fprint @str,"T:# <#>",strmode(x),int(x)

	when clabel then
		if pass<=2 then
			fprint @str,"L#",x
		else
!RETURN "LABEL"
			fprint @str,"&# (L#)",x:"h",ref int(x)-pcstart
		fi

	when coperator then
		fprint @str,"(#)",pclnames[x]

	else
	other:
		fprint @str,"<# #>",fmt,opndnames[fmt]
	esac
	return str
end

global proc writeallpcl(ifile pm, int pass)=
!display code currently in pccode/pcopnd
	int cmd,i,lastline,line,labno,offset,index,size,nopnds,x,y
	ref int pc,pclcode
	ref int32 pclsource
	ichar sourcecode
	ichar name

	currlineno:=0

!if n=1 then
!	gs_init(pcldest)
!fi

!CPL "WRITEALLPCL",PM.NAME, PM.PCSTART

	if pass=3 and not hasbytecodes then
		gstrln("Can't show PCL; use -debug")
		return
	fi

	gstr("PCL FOR MODULE:")
	gstrln(pm.name)

	pc:=pclcode:=pm.pcstart
	pclsource:=pm.pcsrcstart
	sourcecode:=pm.text

	size:=pm.pcsize
	labelmap:=pcm_allocz((size+1)*int.bytes)

!CPL "ALLPCL",PC,PC^
	repeat
		cmd:=pc^
!CPL "A1",PC,pclnames[CMD]
		nopnds:=pclnopnds[cmd]

		for i to nopnds do
			case pclfmt[cmd,i]
			when cnone then
				exit
			when clabel then
				x:=(pc+i)^
!CPL "CLABEL",X,LABELMAP,=SIZE,=PASS
				if pass=3 then
					x:=ref int(x)-pclcode
				fi
!CPL "	CLABEL",X,LABELMAP,=SIZE,=PASS
IF INT(X)>100000 THEN
!	CPL "X LABEL ALREADY CONVERTED"
ELSE

				labelmap^[x]:=1		!allow up to 4 labels at this loc
FI
			esac

		od
		pc+:=pclnopnds[cmd]+1
	until cmd in [kzero,kendmodule]

	pc:=pm.pcstart

!CPL "ALLPCL2",PC
	repeat
		cmd:=pc^

!CPL PC,=PCLNAMES[CMD],=PASS

		writepcl(pclcode,pc,pclsource, pass, sourcecode)
!CPL "DONEWRITEPCL"
		pc+:=pclnopnds[cmd]+1
	until cmd in [kzero,kendmodule]
!CPL "DONE2"

	gline()
	pcm_free(labelmap,(size+1)*int.bytes)



!CPL "DONE3"
end

global proc deletetempfiles=
	remove("PCL1")
	remove("PCL2")
	remove("PCL3")
	remove("AST1")
	remove("AST2")
	remove("TYPES")
	remove("STFLAT")
	remove("ST")
	remove(logfile)
end
