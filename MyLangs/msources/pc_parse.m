import msys
import mlib

import* pci_core
import* pci_read

int insideproc

global function parse(ichar source)int=
	psymbol d

	startlex(source)
	int labno

!	repeat
!		lex()
!		printsymbol()
!	until lxsymbol=eofsym
!	STOP

	lxsymbol:=errorsym

	do
		lex()

		case lxsymbol
		when kopcodesym, typesym then
			if lxsubcode=kendprogram then exit fi

			readinstr()

		when labelsym then
			dolabel()

		when namesym then
			SERROR("""NAME:"" NOT SUPPORTED")
!			d:=lxsymptr
!			lex()
!			case lxsymbol
!			when colonsym then
!			when dcolonsym then
!				d.isexported:=1
!			else
!				serror(": expected")
!			esac
!			assignlabeltoname(d)
!			pcl_gen(klabelname,pcl_genmemaddr(d))
!			pccurr.seqno:=lxlineno
!!			pcl_gen(klabel,genlabel(d.labelno))
!			lex()

		when intconstsym then
			next

		when eolsym then
			next
		when eofsym then
			exit
		else
			println =symbolnames[lxsymbol]
			serror("Bad input")
		esac
		checksymbol(eolsym)
	od

	if insideproc then serror("End missing") fi

	return 1
end

proc checksymbol(int symbol)=
	if lxsymbol<>symbol then
		println symbolnames[symbol],"expected, not",symbolnames[lxsymbol],,", on line:",lxlineno
		stop 1
	fi
end

proc dolabel=
	if lxvalue not in 1..maxlabels then
		serror_s("1:Label out of range:",strint(lxvalue))
	fi

	pdefinelabel(lxvalue,lxlineno)
	lex()
	checksymbol(colonsym)
	lex()
end

proc readmode(pcl p)=
	if p.mode<>tpvoid then
		p.oldmode:=lxsubcode
		lex()
		return
	fi

	p.mode:=lxsubcode
	p.size:=psize[lxsubcode]
	lex()
	if p.mode=tpblock then
		checksymbol(colonsym)
		lex()
		checksymbol(intconstsym)
		p.size:=lxvalue
		lex()
	fi
end

proc readinstr=
!I have a bare opcode; see if any other operands follow. Syntax is:
! push[:type[:size]] [operand [:type[:size]] [,A[,B]]]
	int opcode,mode,size
	pcl p

	p:=newpcl()				!all zeros
	p.seqno:=lxlineno
	mode:=tpvoid			!possible override
	size:=0

	if lxsymbol=typesym then
		readmode(p)
		checksymbol(kopcodesym)
	fi

	p.opcode:=opcode:=lxsubcode

	lex()

	if lxsymbol=typesym then
		readmode(p)
	fi

	case pclhasopnd[opcode]
	when 1 then
		case lxsymbol
		when intconstsym then
			p.value:=lxvalue
			p.opndtype:=int_opnd
			mode:=tpi64
			lex()
		when realconstsym then
			p.xvalue:=lxxvalue
			p.opndtype:=real_opnd
			mode:=tpu64
			lex()
		when stringconstsym then
			p.svalue:=pcm_copyheapstring(lxsvalue)
			p.opndtype:=string_opnd
			mode:=tpu64
			lex()
		when labelsym then
			if lxvalue not in 1..maxlabels then
				serror_s("2:Label out of range:",strint(lxvalue))
			fi
			p.labelno:=lxvalue
			p.opndtype:=label_opnd
			lex()
		when namesym then
			p.def:=lxsymptr
			p.opndtype:=mem_opnd
			p.def.istruename:=lxtruename

			lex()

		when int128constsym then
			p.value128:=lxuvalue128
			p.opndtype:=int128_opnd
			mode:=tpi128
			lex()

		when addrsym then
			lex()
			checksymbol(namesym)
			p.def:=lxsymptr
			p.def.istruename:=lxtruename
			p.opndtype:=memaddr_opnd
			lex()
			if lxsymbol=mulsym then
				p.def.isimported:=1
				lex()
			fi
		else
			serror("Missing or Bad Operand")
		esac

	when 2 then			!must be name that defines a proc or static or named label
		checksymbol(namesym)
		p.def:=lxsymptr
		if opcode=kprocrts then
			p.opcode:=kprocdef
			definertsproc(p.def)
		fi
		p.opndtype:=mem_opnd
		lex()
		case lxsymbol
		when colonsym then
		when dcolonsym then
			p.def.isexported:=1
			p.isexported:=1
		else
			serror("Colon expected")
		esac
		lex()

	when 3 then			!must be name that defines a local or param
		checksymbol(namesym)
		p.def:=lxsymptr
		p.opndtype:=mem_opnd
		lex()
		if lxsymbol=colonsym then serror(": not used for locals") fi
	when 4 then			!must be name that defines a proc or static or named label
		checksymbol(namesym)
		p.def:=lxsymptr
		p.opndtype:=mem_opnd
		lex()

	esac

	if lxsymbol=typesym then
		readmode(p)
	fi

!if no type used, but available from operand, then use that

	if p.mode=tpvoid then
		if mode<>tpvoid then
			p.mode:=mode
			p.size:=size
		fi
	fi

	if pclextra[opcode] then
		checksymbol(intconstsym)
		p.x:=lxvalue
		lex()
		if pclextra[opcode]=2 then
			checksymbol(intconstsym)
			p.y:=lxvalue
			lex()
		fi
	fi
end

global proc serror(ichar mess)=
	serror_s(mess, nil)
end

global proc serror_s(ichar mess, param=nil)=
	print "Syntax error:",mess
	if param then
		print ":",param
	fi

	println " on line:",lxlineno
	stop 1
end

export function pcl_readpclfile(ichar filename, rtsfile=nil)int=
!note: currently only works one-time
	ichar source

	pcl_start()

	if not parse_readsource(filename) then return 0 fi
	
	if rtsfile then
		labelnooffset:=maxuserlabel
		if not parse_readrts() then
			println "No RTS found"
			return 0
		fi
	fi

	pcl_gen(kendprogram)
	pcl_end()

	return 1
end

global function parse_readsource(ichar filename)int=
	ichar source

	source:=readfile(filename)
	if source=nil then
		errormess:="Can't load file"
		return 0
	fi	

	if not parse(source) then
		errormess:="Couldn't parse"
		return 0
	fi
	return 1
end

global function parse_readrts:int=
	static ichar source = strinclude("rts.pcl")

	if not parse(source) then
		errormess:="Couldn't parse RTS"
		return 0
	fi
	return 1
end
