ref int pc, pcstart

macro getopnda = (pc+1)^
macro getopndb = (pc+2)^
macro getopndc = (pc+3)^
macro getopndd = (pc+4)^
macro getopnde = (pc+5)^

macro putopnda(x) = (pc+1)^:=x
macro putopndb(x) = (pc+2)^:=x
macro putopndc(x) = (pc+3)^:=x
macro putopndd(x) = (pc+4)^:=x
macro putopnde(x) = (pc+5)^:=x

ref[0:]int labelmap

global proc optimise_module(int n)=
	int cmd, nopnds, size:=0, x
	pc := pcstart := modules[n].pcstart

!CPL "OPTIM",N

	size:=modules[n].pcsize+1
	labelmap:=pcm_allocz(size*int.bytes)

	repeat
		cmd:=pc^
		nopnds:=pclnopnds[cmd]

		for i to nopnds do
			case pclfmt[cmd,i]
			when cnone then
				exit
			when clabel then
				x:=(pc+i)^
				labelmap^[x]:=1		!allow up to 4 labels at this loc
			esac

		od
		pc+:=nopnds+1
	until cmd in [kzero,kendmodule]

	pc:=pcstart
	repeat
!CPL =PC
		cmd:=pc^
		optimise_op(cmd)
	until cmd=kendmodule

!CPL "DONE",SIZE
	if size then pcm_free(labelmap,size) fi
end

proc putnops(int offset,n)=
	to n do
		(pc+offset++)^:=kskip
!		(pc+offset++)^:=knop
	od
end

proc optimise_op(int cmd)=
!optimise a pcl op 'cmd' at pc^ (pc needs to be global)
!If not optimised, leaves it unchanged and sets pc to the following opcode
!When optimised, 1 or more opcodes are modified (usually into one op, with possible
!passing using nops) and sets pc to the next opcode following that sequence
	int skip, offset, secondlab, cmd2, skip2, x,y,z
	int a,b, thisloc, thislab, destcmd, destlab

	skip:=pclnopnds[cmd]+1			!default offset to next instruction
	a:=getopnda
	b:=getopndb
	offset:=pc-pcstart				!offset of this opcode

!CPL "OPTIM",PC,PCLNAMES[CMD],=SKIP

!	PC+:=SKIP
!	RETURN

!check single opcode optimisations first (free/procentry etc)
!otherwise they will not be done if followed by a label
	case cmd
	when kunshare then
		case a
		when 1,2,3 then
			pc^:=(a|kunshare1,kunshare2,kunshare3|0)
			putnops(1,1)
			skip:=2
			return
		esac

	when kprocentry then
		case a
		when 1,2 then
			pc^:=(a|kprocentry1,kprocentry2|0)
			putnops(1,1)
			skip:=2
			return
		esac
	when kjump,kjumpeq,kjumpne,kjumplt,kjumple,kjumpge,kjumpgt then
!						kjumptesteq,kjumptestne then
			thisloc:=offset
			thislab:=a
			if cmd=kjump and thisloc+2=thislab then
				pc^:=knop2
				putopnda(0)
				pc+:=skip
				return
			elsif destcmd=kjump then
				destcmd:=(pcstart+thislab)^
				destlab:=(pcstart+thislab+1)^
!				CPL thisloc,,": JUMP TO JUMP",thislab,PCLNAMES[CMD],=destlab
				putopnda(destlab)
				pc+:=skip
				return
			fi
	esac

	if labelmap[offset+skip] then	!followed by a label; don't bother
		pc+:=skip					!with 2/3-opcode optimising
		return
	fi

	secondlab:=0
	cmd2:=(pc+skip)^
	skip2:=pclnopnds[cmd2]+1
	if labelmap[offset+skip+skip2] then
		secondlab:=1
!		CPL "TWO LABELS"
	fi
	
!CPL "OPTIM3"

	switch cmd
	when kpushf then
		switch b				!opcode that follows
		when kpushf then			!pushf pushf
			if secondlab then dopushff fi
			switch getopndd
			when kpushf then
				pc^:=kpushfff
				putopndb(getopndc)
				putopndc(getopnde)
				putnops(4,2)
				skip:=6

			when kadd then
				pc^:=kaddff
				putopndb(getopndc)
				putnops(3,1)		!leave final opc
				skip:=5
			when ksub then
				pc^:=ksubff
				putopndb(getopndc)
				putnops(3,1)
				skip:=5

			when kjumpeq then
				pc^:=kjumpeqff
				dojumpxxff
			when kjumpne then
				pc^:=kjumpneff
				dojumpxxff
			when kjumplt then
				pc^:=kjumpltff
dojumpxxff:
				x:=getopnda
				y:=getopndc
				z:=getopnde
				putopnda(z)
				putopndb(x)
				putopndc(y)
!NOTE: these nops will overwrite the original LAB of the Jumpge opcode.
!Necessary when jumpgeff has to revert back to normal jumpge
!The Nops are not needed anyway because both outcomes, when the optimised
!jumpgeff is used, will skip these bytes, either by jumping to LAB, or skipping +6
!				putopndd(knop)
!				putopnde(knop)

				skip:=6
			when kjumple then
				pc^:=kjumpleff
				dojumpxxff
			when kjumpge then
				pc^:=kjumpgeff
				dojumpxxff
			when kjumpgt then
				pc^:=kjumpgtff
				dojumpxxff
			when kindex then
				pc^:=kindexff
				putopndb(getopndc)
				putnops(3,1)		!leave final opc
				skip:=5

			else
dopushff:
				pc^:=kpushff
				putopndb(getopndc)
				putnops(3,1)
				skip:=4
			end
		when kpushm then			!pushf pushm
!CPL "PUSHF PUSHM"
				pc^:=kpushfm
				putopndb(getopndc)
				putnops(3,1)
				skip:=4
		when kpushci then			!pushf pushci
!CPL "PUSHF/PUSHCI"
			if secondlab then finish fi
			switch getopndd
			when kadd then
				pc^:=kaddfci
				putopndb(getopndc)
				putnops(3,1)		!leave final opc
				skip:=5
			when ksub then
				pc^:=ksubfci
				putopndb(getopndc)
				putnops(3,1)
				skip:=5

			when kjumplt then
				pc^:=kjumpltfci
dojumpxxfci:
				x:=getopnda
				y:=getopndc
				z:=getopnde
				putopnda(z)
				putopndb(x)
				putopndc(y)
!				putopndd(knop)
!				putopnde(knop)
				skip:=6
			when kjumple then
				pc^:=kjumplefci
				dojumpxxfci
			when kjumpge then
				pc^:=kjumpgefci
				dojumpxxfci
			when kjumpgt then
				pc^:=kjumpgtfci
				dojumpxxfci
			when kjumpeq then
				pc^:=kjumpeqfci
				dojumpxxfci
			when kjumpne then
				pc^:=kjumpnefci
				dojumpxxfci
			end
!
		when kpopm then				!pushf popm
!CPL "PUSHM POPM"
			pc^:=kmovemf
			domoveff
		when kpopf then				!pushf popf
			pc^:=kmoveff
domoveff:
			x:=a
			putopnda(getopndc)
			putopndb(x)
			putnops(3,1)
			skip:=4
		when kzpopf then			!pushf zpopf
!CPL "PUSHF ZPOPF"
			pc^:=kzmoveff
			domoveff

		when kswitch then			!pushf switch
			pc^:=kswitchf
			putopndb(getopndc)
			putopndc(getopndd)
			putnops(4,1)
			skip:=5
	
		when klen then				!pushf len
!CPL "PUSHF LEN"
			pc^:=klenf
			putnops(2,1)
			skip:=3
		when kpushptr then			!pushf pushptr
			pc^:=kpushptrf
			putnops(2,1)
			skip:=3
		end
	when kpushm then
		case b
		when kpushm then			!pushm pushm
!CPL "PUSHM PUSHM"
			pc^:=kpushmm
			putopndb(getopndc)
			putnops(3,1)
			skip:=4
		when kpushf then			!pushm pushm
!CPL "PUSHM PUSHF"
			pc^:=kpushmf
			putopndb(getopndc)
			putnops(3,1)
			skip:=4
		when kpopm then				!pushm popm
!CPL "PUSHM POPM"
			pc^:=kmovemm
			domoveff
		when kpopf then				!pushm popf
!CPL "PUSHM POPF"
			pc^:=kmovefm
			domoveff
		esac
!
	when kpushci then
		case b
		when kpopm then				!pushci popm
!CPL "PUSHCI POPM"
			pc^:=kmovemci
			domoveff
		when kpopf then				!pushci popf
			pc^:=kmovefci
			domoveff
		when kzpopf then			!pushci zpopf
!CPL "PUSHCI ZPOPF"
			pc^:=kzmovefci
			domoveff
		elsif a=0 and b not in [kraise,kstop] then
			pc^:=kpushci0
		esac

	when kpushvoid then
		case a
		when kpushvoid then			!pushvoid pushvoid
			if not secondlab and b=kpushvoid then
				pc^:=kpushvoid3
				putnops(1,2)
				skip:=3
			else
				pc^:=kpushvoid2
				putnops(1,1)
				skip:=2
			fi
		esac
	when kpushfref then
		case b
!		when kpushf then			!pushfref pushf
!		when kpushci then			!pushfref pushci
		when kloadincr then
			if not secondlab then
				case getopndc
				when kpushptr then		!loadincr pushptr
					pc^:=kpushincrptrf
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				when kpopptr then		!loadincr popptr
					pc^:=kpopincrptrf
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				esac
			fi
		esac

	when kpushmref then
		case b
		when kloadincr then
			if not secondlab then
				case getopndc
				when kpushptr then		!loadincr pushptr
					pc^:=kpushincrptrm
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				when kpopptr then		!loadincr popptr
					pc^:=kpopincrptrm
					putnops(2,1)		!loadincr=>skip, but keep the pushptr
					skip:=4
				esac
			fi
		esac

!	when kpopretval then
!		if getopndb not in [kreturn0, kreturn] then
!			CPL "POPRETVAL NOT FOLLOWED BY RET:",PCLNAMES[GETOPNDB]
!		FI
!
!	when kisint, kislist then
!		case a
!		when kjumptrue then			!isint/etc jumptrue
!		when kjumpfalse then		!isint/etc jumpfalse
!		esac
	end

finish:
	pc+:=skip
end

