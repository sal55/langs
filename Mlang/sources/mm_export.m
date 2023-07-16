strbuffer sbuffer
ref strbuffer dest=&sbuffer

const expscope=export_scope

global proc writeexports(ichar outfile, modulename)=
	ref strec d,e
	ref procrec pp
	array [300]char filename
	filehandle f

	println "Writing exports file to",outfile

	gs_init(dest)
	wxstr("importlib ")
	wxstr(modulename)
	wxstrln(" =")

	for i:=tuser to ntypes do
		d:=ttnamedef[i]
		if d.scope=export_scope and d.name^<>'$' then

			case ttbasetype[i]
			when trecord then
				exportrecord(d)
			else
				wxstr("    type ")
				wxstr(d.name)
				wxstr(" = ")
				wxstr(strmode(d.mode,0))
				wxline()
			esac
		fi
	od

	pp:=staticlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportstatic(d)
		fi
	od
	if staticlist then wxline() fi

	pp:=constlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportconst(d)
		fi
	od
	if constlist then wxline() fi

	pp:=proclist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportproc(d)
		fi
	od

	wxstrln("end importlib")

	f:=fopen(outfile,"wb")
	gs_println(dest,f)
	fclose(f)
end

proc exportstatic(ref strec d)=
	wxstr("    var ")
	wxmode(d.mode)
	wxstr(" ")
	wxstr(d.name)
	wxline()
end

proc exportconst(ref strec d)=
	wxstr("    const ")
	wxmode(d.mode)
	wxstr(" ")
	wxstr(d.name)
	wxstr(" = ")
	jevalx(dest,d.code)
	wxline()
end

proc exportproc(ref strec d)=
	ref strec e
	int currmode,needcomma

	wxstr("    mlang ")
	wxstr((d.mode=tvoid|"proc     "|"function "))
	wxstr(d.name)
	wxstr("(")

	e:=d.deflist
	needcomma:=0
	currmode:=tvoid

	while e do
		if e.nameid=paramid then
			if needcomma then wxstr(",") fi
			if e.parammode<>out_param then
				if e.mode<>currmode then
					wxmode(e.mode)
					wxstr(" ")
					currmode:=e.mode
				fi
			else
				wxmode(tttarget[e.mode])
				wxstr(" &")
				currmode:=tvoid
			fi
			wxstr(e.name)
			if e.code then
				wxstr("=")
				if ttisref[e.mode] and e.code.tag=jconst and e.code.value=0 then
					wxstr("nil")
				else
					jevalx(dest,e.code)
				fi
			fi
			needcomma:=1
		fi
		e:=e.nextdef
	od

	wxstr(")")
	if d.mode then
		wxstr(" => ")
		wxmode(d.mode)
	fi
	wxline()
end

proc wxstr(ichar s)=
	gs_str(dest,s)
end

proc wxstrln(ichar s)=
	gs_strln(dest,s)
end

proc wxline=
	gs_line(dest)
end

proc exportrecord(ref strec d)=
	ref strec e
	ref char flags
	int flag,indent
	const tab="    "

	e:=d.deflist

	wxstr("    record ")
	wxstr(d.name)
	wxstr(" = ")
	wxline()

	indent:=2

	while e do
		if e.nameid=fieldid then
			flags:=cast(&e.uflags)
			docase flags^
			when 'S' then
				to indent do wxstr(tab) od
				wxstrln("struct")
				++indent
				++flags
			when 'U' then
				to indent do wxstr(tab) od
				wxstrln("union")
				++indent
				++flags
			else
				exit
			end docase

			to indent do wxstr(tab) od
			wxmode(e.mode)
			wxstr(" ")
			wxstrln(e.name)

			do
				flag:=flags++^
				case flag
				when '*'  then
				when 'E' then
					--indent
					to indent do wxstr(tab) od
					wxstrln("end")
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	wxstrln("    end")
	wxline()
end

proc wxmode(int mode)=
	ichar name
	if mode>=tuser then
		name:=ttnamedef[mode].name
		if name^<>'$' then
			wxstr(name)
			return
		fi
	fi
	wxstr(strmode(mode,0))
end
