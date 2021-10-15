import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_headers
import cc_lex

strbuffer mmbuffer
ref strbuffer mm=&mmbuffer

global proc writemheader(ichar infile)=
	[300]char mfile
	ref strec d,e
	int m
	ref mparamrec q

	strcpy(&.mfile,pcm_copyheapstring(changeext(infile,".m")))

	gs_init(mm)

	mmstr("importdll ")
	mmstr(extractbasefile(infile))
	mmstrln(" =")

	stmodule:=moduletable[1].stmodule


	d:=stmodule.deflist

	while d do
		if isheaderfile(sourcefilenames[d.lineno>>24]) then
			d:=d.nextdef
			next
		fi
		case d.nameid
		when staticid then
			mmstr("    ")
			mmmode(d.mode)
			mmstr(" ")
			mmstr(fixname(d.name))
			if d.code then
				mmstr(" =")
				mmstr(strexpr(d.code).strptr)
			fi
			mmline()

		when procid then
			writefunction(d)
		when typeid then
		when enumid then
			mmstr("    const ")
			mmleftstr(fixname(d.name),34)
			mmstr(" = ")
			mmint(d.index)
			mmline()

		when macroid then
			mmstr("MACRO ")
			mmstrln(fixname(d.name))
		when structtagid then
			writerecord(d.mode)

		esac
		d:=d.nextdef
	od

	for i:=0 to hstmask do
		e:=hashtable^[i]
		if e.name and e.symbol=namesym and e.nameid=macroid then
			if not isheaderfile(sourcefilenames[e.lineno>>24]) then
				if e.tokenlist then
					mmstr("    global macro  ")
					mmstr(e.name)

					q:=e.mparamlist
					if q then
						mmstr("(")
						while q do
							mmstr(q.def.name)
							if q.nextmparam then
								mmstr(",")
							fi
							q:=q.nextmparam
						od

						mmstr(")")
					fi
					mmstr(" = ")
					showmacroseq(e.tokenlist)
					mmline()
				fi
			fi

		fi
	od
	mmstrln("end")

	moduletable[1].mhdrstr:=mm.strptr

	CPL "Writing M Header:",&.mfile
	writefile(&.mfile,cast(mm.strptr),mm.length)

end

proc showmacroseq(ref tokenrec tk)=
	while tk do
		emittoken(tk,mm)
		tk:=tk.nexttoken
	od
end

proc mmstr(ichar s)=
	gs_str(mm,s)
end

proc mmleftstr(ichar s,int n)=
	gs_leftstr(mm,s,n)
end

proc mmstrln(ichar s)=
	gs_strln(mm,s)
end

proc mmint(int a)=
	[32]char str
	getstrint(a,&.str)
	gs_str(mm,&.str)
end

proc mmline()=
	gs_line(mm)
end

proc writefunction(ref strec d)=
	ichar file
	ref paramrec pm
	int n,isvar

	if d.mode=tvoid then
		mmstr("    clang proc     ")
	else
		mmstr("    clang function ")
	fi
	mmstr("""")

	mmstr(d.name)

	mmstr("""")
	mmleftstr(" ",34-strlen(d.name))
	mmstr("(")

	pm:=d.paramlist
	n:=pm.nparams
	isvar:=pm.flags=pm_variadic
	for i to n do
		mmmode(pm.mode)
		if i<>n or isvar then
!		if i<>n then
			mmstr(",")
		fi
		pm:=pm.nextparam
	od
	if isvar then
		mmstr("...")
	fi

	mmstr(")")

	if d.mode<>tvoid then
		mmmode(d.mode)
	fi

	mmline()
end

proc mmmode(int m,expand=1) =
	int t,u

	t:=ttbasetype[m]
	case t
	when tref then
	mmstr("ref ")
	u:=tttarget[m]
	if ttbasetype[u]=tproc then
		writefnptr(u)
	else
		mmmode(tttarget[m])
	fi

when tarray then
	mmstr("[")
	if ttlength[m] then
		mmint(ttlength[m])
	fi
	mmstr("]")
	mmmode(tttarget[m])

when tenum then
	mmstr("int")

when tstruct,tunion then
	mmstr(fixname(ttnamedef[m].name))

when tproc then
	MMSTR("<PROC>")

else
	mmstr(stdtypemnames[t])
esac
end

proc writerecord(int m, rectype='R', level=1)=
	ref strec d,e
	int emode

	to level do
		mmstr("    ")
	od
	++level

	d:=ttnamedef[m]
	if rectype='R' then
		mmstr("record ")
		mmstr(fixname(d.name))
		mmstrln(" =")
	else
		mmstrln((rectype='S'|"struct"|"union"))
	fi

	e:=d.deflist
	if e=nil then
		to level do
			mmstr("    ")
		od
		mmstrln("var int dummy    !empty record")
	fi

	while e do
		emode:=e.mode
		to level do
			mmstr("    ")
		od

		if strchr(e.name,'$') then
			case ttbasetype[emode]
			when tunion then
				writerecord(emode,'U',level)
			when tstruct then
				writerecord(emode,'S',level)
			esac
		else
			mmstr("var ")
			mmmode(e.mode)
			mmstr(" ")
			mmstrln(fixname(e.name))
		fi
		e:=e.nextdef
	od
	to level-1 do
		mmstr("    ")
	od
	mmstrln("end")
	mmline()
end

proc writefnptr(int m)=
	ref paramrec pm
	int isvar,n,target

	target:=tttarget[m]

	if target=tvoid then
		mmstr("clang proc(")
	else
		mmstr("clang function(")
	fi

	pm:=ttparams[m]
	n:=pm.nparams
	isvar:=pm.flags=pm_variadic
	for i to n do
		mmmode(pm.mode)
		if i<>n or isvar then
			mmstr(",")
		fi
		pm:=pm.nextparam
	od
	if isvar then
		mmstr("...")
	fi

	mmstr(")")

	if target<>tvoid then
		mmmode(target)
	fi

end

function fixname(ichar name)ichar=
	static []ichar reservedwords = (
		"function",
		"read",
		"type",
		"next",
		"stop",
		"callback",
		"len",
		"$dummy"
	)
	[128]char str

	for i to reservedwords.len do
		if eqstring(reservedwords[i],name) then
			strcpy(&.str,name)
			strcat(&.str,"$")
			return pcm_copyheapstring(&.str)
		fi
	od

	return name
end
