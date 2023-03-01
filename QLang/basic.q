!'Basic' interpreter
! Supports:
!* Keywords:   LET PRINT IF GOTO REM BYE
!* Operators:  + - * / % (int div) = <> < <= >= > and or 
!* Builtins:   SQR LEN CHR ASC
!* Types:      Number (float), STRING
!* Totally blank lines, or commented at start with ' (eg. '100 LET A=1)
!* Optional LET
!* ":" can separate multiple statements on a line
!* IF can be followed by any statements; THEN is optional
!* Quote (') for comments as well as REM
!* Case-insensitive
!* String variables can use A or A$
!* PRINT works on list of expressions; no automatic spacing
!* PRINT adds automatic newline at end unless ends with ';'
! !
var identstarter = ['A'..'Z', 'a'..'z']
var numericstarter = ['0'..'9']
var identchars = ['A'..'Z', 'a'..'z','0'..'9','_','$']
var numericchars = ['0'..'9']

var puncttable = ['(':tklbrack, ')':tkrbrack, '+':tkadd, '-':tksub, '*':tkmul, '/':tkdiv,
		'=':tkeq, ',':tkcomma, ';':tksemi, '%':tkidiv, ':':tkcolon]
var zero=chr(0)

enumdata tokennames, priotable, qoptable =
	(tkvar,     $,      0,      0),
	(tknumber,  $,      0,      0),
	(tkstring,  $,      0,      0),
	(tkeol,     $,      0,      0),

	(tklbrack,  $,      0,      0),
	(tkrbrack,  $,      0,      0),

	(tkadd,     $,      2,      +),
	(tksub,     $,      2,      -),
	(tkmul,     $,      1,      *),
	(tkdiv,     $,      1,      /),
	(tkidiv,    $,      1,      %),

	(tkeq,      $,      3,      =),
	(tkne,      $,      3,      <>),
	(tklt,      $,      3,      <),
	(tkle,      $,      3,      <=),
	(tkge,      $,      3,      >=),
	(tkgt,      $,      3,      >),

	(tkand,     "and",  4,      and),
	(tkor,      "or",   5,      or),

	(tkcomma,   $,      0,      0),
	(tksemi,    $,      0,      0),
	(tkcolon,   $,      0,      0),

	(tksqr,     "sqr",  0,      sqrt),
	(tklen,     "len",  0,      len),
	(tkchr,     "chr",  0,      chr),
	(tkasc,     "asc",  0,      asc),

	(tklet,     "let",  0,      0),
	(tkprint,   "print",0,      0),
	(tkif,      "if",   0,      0),
	(tkgoto,    "goto", 0,      0),
	(tkbye,     "bye",  0,      0),

	(tkrem,     "rem",  0,      0),
	(tkthen,    "then", 0,      0),

	(tkother,   $,      0,      0),
end

var binops      = [tkadd, tksub, tkmul, tkdiv,tkidiv, tkeq, tkne, tklt, tkle, tkge, tkgt, tkand, tkor]
var keywords    = [tklet, tkprint, tkif, tkgoto, tkrem]
var builtins    = [tksqr, tklen]

var lexstr, lexlen, lexpos
var tk, tkvalue

var vars::=[:]
record linerec = (var lineno, source)
var program::=()
var linecount

var pcindex

sub startlex(s) = (lexlen:=s.len; lexstr:=s+chr(0); lexpos:=1)

sub error(m) = abort(m+" on line "+tostr(program[pcindex].lineno))
sub loaderror(m) = abort(m)

proc nexttoken(tkexp=0) =

	tkvalue::=""

	docase c:=lexstr.[lexpos++]
	when identstarter then
		tkvalue::=chr(tolower(c))
		while (c:=lexstr.[lexpos++]) in identchars do tkvalue+:=tolower(c) od
		--lexpos

		tk:=tkvalue inx tokennames
		if not tk.isfound then
			tk:=tkvar
			if vars{tkvalue}.isvoid then
			    vars{tkvalue}:=0.0
			fi
		fi
		if tk=tkrem then tk:=tkeol fi
		exit

	when numericstarter then
		tkvalue:=c-'0'
		while (c:=lexstr.[lexpos++]) in numericchars do tkvalue:=tkvalue*10+c-'0' od
		--lexpos
		tk:=tknumber
		exit

	when ' ', '\t' then
	when '"' then
		while (c:=lexstr.[lexpos++]) not in ['"',0] do tkvalue+:=c od
		if c=0 then error("String?") fi
		tk:=tkstring
		exit

	when 0 then --lexpos; tk:=tkeol; exit
	when '<' then
		tk:=
			case lexstr.[lexpos]
			when '=' then ++lexpos; tkle
			when '>' then ++lexpos; tkne
			else tklt
			esac
			exit
	when '>' then tk:=(lexstr.[lexpos]='='|(++lexpos; tkge) | tkgt); exit
	when '\'' then
		tk:=tkeol
		exit
	elsif tk:=puncttable{c,0} then
		exit
	else tk:=tkother; exit
	end

	if tkexp then checktoken(tkexp) fi
end

sub checktoken(tkexp)= if tk<>tkexp then error(tokennames[tkexp]+" expected") fi

proc loadprogram(filename)=
	lines:=readtextfile(filename)
	if lines=0 then loaderror("Load open "+filename) fi

	lastn:=0

	for line in lines when leftstr(line)<>"'" and line do
		sreadln(line+zero)
		read n:"i", s:"L"
		if n<=lastn then loaderror("Line seq "+tostr(n)) fi

		program &:= linerec(n, s+zero)
		lastn:=n
	od
end

proc listprogram=
	for line in program do
		println line.lineno, line.source
	od
end

func readexpr(needtk=1)=
	if needtk then nexttoken() fi
	readfactor(5)
end

func readfactor(n)=
	x:=(n<=1 | readterm() | readfactor(n-1))

	while tk in binops and priotable[tk]=n do
		opc:=tk
		nexttoken()
		x:=mapss(qoptable[opc], x, readfactor(n-1))
		if x.type=bool then x:=(x|1|0) fi
	od
	x
end

func readterm=
	case tk
	when tknumber, tkstring then
		x:=tkvalue
		nexttoken()
	when tkvar then
		x:=vars{tkvalue}
		nexttoken()
	when tklbrack then
		x:=readexpr()
		checktoken(tkrbrack)
		nexttoken()
	when tksub then
		nexttoken()
		x:=-readterm()
	when builtins then
		fn:=qoptable[tk]
		nexttoken(tklbrack)
		x:=readexpr()
		checktoken(tkrbrack)
		nexttoken()
		x:=maps(fn, x)

	else
		error("Readterm?")
	esac

	x
end

func executeline(index)=

	startlex(s:=program[index].source)
	nexttoken()

nextstmt::
	case tk
	when tklet then
		nexttoken(tkvar)
dolet::
		varname:=tkvalue
		nexttoken()
		if tk<>tkeq then error("Missing =, or unknown keyword") fi
		vars{varname}:=readexpr()

	when tkvar then
		dolet

	when tkprint then
		nexttoken()
		needtk:=0
		if tk<>tkeol then
			repeat
				x:=readexpr(needtk)
				needtk:=1
				print x
			until tk<>tkcomma
		fi

		if tk=tksemi then           !suppress newline
			nexttoken()
		else
			println
		fi

	when tkgoto then
		nexttoken(tknumber)
dogoto::
		lineno:=tkvalue
		nexttoken()
		for i,l in program when l.lineno=lineno do
		    return i
		else
			error("Bad line:"+tostr(lineno))
		end

	when tkif then
		x:=readexpr()
		if x then
			if tk in [tkthen, tkcolon] then     !then is optional
			    nexttoken()
			    if tk=tknumber then
			        dogoto1
			    fi
			fi
			nextstmt
		else
			tk:=tkeol
		fi

	when tkeol then             !rem or blank
	when tkbye then
		stop
	else
		println tkvalue
		error("Unknown keyword "+s)
	esac

	case tk
	when tkcolon then               !multiple statements per line
		nexttoken()
		nextstmt
	when tkeol then                 !normal ending
	else
		error("EOL expected:"+s)
	esac

	index+1
end

proc runprogram=
	if not program then loaderror("Empty prog") fi
	pcindex:=1
	linecount:=0

	repeat
		pcindex:=executeline(pcindex)
		++linecount
	until pcindex>program.len

	println "Stopped",linecount
end

proc main=
	if ncmdparams>=1 then
		file:=changeext(cmdparams[1],"bas")
	else
		println "No REPL, submit .bas file only"
		stop
	fi

	loadprogram(file)
!   listprogram()
	runprogram()

!   println vars
end
