int currlineno
global int nextavindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const maxlocalunits=500
[maxlocalunits]unitrec unitpool
int nlocalunits

ichar errormess

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global proc reportcterror(ichar errortype,mess,int pos, symbol currproc=nil)=
	locrec loc
!CPL "CT1",POS IAND 16777215, CURRPROC
	loc:=geterrorinfo(pos,currproc)
!CPL "CT2"
	println errortype,"Error:"
	println "    ",,mess
	println

!CPL "CT3"
	showerrorsource(loc)
!CPL "CT4"
	stopcompiler(loc)
end

global func geterrorinfo(word pos, symbol currproc=nil)locrec=
!slow is the low word of a string pointer into source code
!moduleno is the module number if known, otherwise 0 to work it out here
!Set up global error vars: errorline, errorpointer, errormodule, errorlineno
	int soffset, moduleno
	locrec loc

	clear loc
	soffset:=pos.[0..23]
	moduleno:=pos.[24..31]

!CPL =SOFFSET
!CPL =MODULENO

	if moduleno=0 then
		ABORTPROGRAM("GETERRORINFO: no module")
	fi
	if currproc=nil then
		ABORTPROGRAM("GETERRORINFO: no currproc")
	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[loc.pm.subprogno]	
	loc.def:=currproc

!CPL "GEIX"
!	loc.lineno:=getlineno(loc.p.text, soffset, loc.loc.column)
	setlineno(&loc, soffset)

	return loc
end

!global function getlineno(ichar source, int offset, ichar startline, int &column)int=
global proc setlineno(ref locrec loc, int offset)=
!loc contains sp/pm, fill in startline/lineno/column given char offset within module
	ichar sline, s, source:=loc.pm.text
	

	sline:=source+offset

	while sline>source and sline^<>10 do --sline od
	if sline^=10 then ++sline fi
	loc.startline:=sline
	loc.column:=source+offset-sline

	s:=sline
	loc.lineno:=1
	while s>source do
		if s^=10 then ++loc.lineno fi
		--s
	od
end

proc showerrorsource(locrec loc)=
	ichar s
	println "Line:",loc.lineno,"in Module",loc.pm.name,,".q:"

	if loc.def then
		println "In function:",loc.def.name
	fi

!CPL "///STARTLINE",LOC.STARTLINE
	print " |"
	s:=loc.startline
	while s^ not in [13,10,26,0] do
		print s^
		++s
	od
	println "|"
	

!	println " |",errorline
!	println " |",errorpointer
end

global proc stopcompiler(locrec loc)=
	filehandle f
	f:=fopen("$error.tmp","w")
!	println @f,modulename,,".q",lineno
	println @f,loc.pm.filespec, loc.lineno
	fclose(f)
	println
	println

	OS_GETCH()

	stop 1
end

!global proc prterror(ichar mess)=
!	reportcterror("Print",mess,qpos)
!end

global proc gerror(ichar mess,unit p=nil)=
!CPL "G1"
	reportcterror("Code Gen",mess,(p|p.pos|qpos),stcurrproc)
end

global proc gerror_s(ichar mess, param,unit p=nil)=
	[300]char str
	print @str, mess, param
	reportcterror("Code Gen",&.str,(p|p.pos|qpos),stcurrproc)
end

global proc serror(ichar mess)=
	reportcterror("Syntax",mess,lx.pos,stcurrproc)
end

global proc serror_s(ichar mess,param)=
	[300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	reportcterror("Syntax",str,lx.pos,stcurrproc)
end

global proc rxerror(ichar mess,unit p=nil)=

!CPL =P, =QPOS, =STCURRPROC

	reportcterror("Resolve",mess,(p|p.pos|qpos),stcurrproc)
end

global proc rxerror_s(ichar mess,param, unit p=nil)=
	[300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	rxerror(str,p)
end

global proc lxerror(ichar mess)=
	reportcterror("Lex",mess,lx.pos,stcurrproc)
end

global proc pcnotmut=
	pcerror("Not mutable")
end

global proc pcerror(ichar mess)=
	errormess:=mess
	reportpcerror(mess,pcptr)
end

global proc pcerror_s(ichar mess, param)=
	[300]char str
	errormess:=mess
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	reportpcerror(str,pcptr)
end

global proc reportpcerror(ichar mess, ref int pcptr)=
	variant s,send
	ref int pc
	int count
	ifile pm
	locrec loc, loc2

!CPL $LINENO
	loc:=getpcerrorpos(pcptr)
!CPL $LINENO
	pm:=loc.pm		!remember first error (written to $error.tmp)

!CPL $LINENO
	println
!	println "*********************************************************"
	println " ":"80p*"
	println "PC Error:"
	println "    ",,mess
	println

!CPL $LINENO
	showerrorsource(loc)
!CPL $LINENO

	s:=sptr
	send:=&varstack[1]
!CPL $LINENO

	count:=0
	while s>=send and count<5 do
		if s.tag=tretaddr then
			pc:=s.retaddr-3		!go back three to get to start of kcall/kcallptr instr
			loc2:=getpcerrorpos(pc)
			println "Called from line",loc2.lineno,"in",loc2.pm.name
			++count
		fi
		--s
	od
!CPL $LINENO

	stopcompiler(loc)
end

global func getpcerrorpos(ref int pc)locrec =
!given pcptr, set up pcerrorpos, the lsw of the source pointer
!and set up pcerrormodule
	int offset, pos, soffset, moduleno
	ref int pcstart
	ref int32 pcsrcstart
	ifile pm
	locrec loc

	clear loc
	pm:=modules[findmodulefrompc(pc)]

	pcstart:=pm.pcstart
	pcsrcstart:=pm.pcsrcstart

	offset:=pc-pcstart
	pos:=(pcsrcstart+offset)^

	soffset:=pos.[0..23]
	moduleno:=pos.[24..31]

	if moduleno=0 then
MODULENO:=1; SOFFSET:=0
!		ABORTPROGRAM("GETPCPOS: no module")
	fi
!	if currproc=nil then
!		ABORTPROGRAM("GETPCPOS: no currproc")
!	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[pm.subprogno]	
!	loc.def:=currproc
	loc.def:=nil

	setlineno(&loc, soffset)

	return loc
end

!global proc loaderror(ichar mess,mess2="",mess3="")=
global proc loaderror(ichar mess,mess2="")=
	[512]char str
	if strchr(mess,'#') then
!		fprint @str,mess,mess2,mess3
		fprint @str,mess,mess2
	else
		print @str,mess
	fi

	println "Load Error:",str
	println "Stopping"
	stop 1
end

function findmodulefrompc(ref int pc)int=
!given pcptr, find which module it's currently executing
	for i to nmodules do
		if pc>=modules[i].pcstart and pc<modules[i].pcend then
			return i
		fi
	od
	println "Can't find pcptr module",pc
!RETURN 1
	if errormess then
		fprintln "(#)",errormess
	fi
	stop 1
	return 0
end

global proc prterror(ichar mess)=
	println "Print error:",mess
	os_getch()
	stop 1
end

global proc pcustype(ichar mess, variant x) =
	pcustype_t(mess, x.tag)
end

global proc pcustype_t(ichar mess, int t) =
	[256]char str

	fprint @str,"Type not supported: # : #",mess, ttname[t]
	reportpcerror(str,pcptr)
end

global proc pcmxtypes(ichar mess, variant x,y) =
	pcmxtypestt(mess,x.tag,y.tag)
end

global proc pcmxtypestt(ichar mess, int t,u) =
	[256]char str

	fprint @str, "Types not supported: # : #/#",
			mess,ttname[t],ttname[u]
	reportpcerror(str,pcptr)
end

global function allocunitrec:unit p=
	p:=pcm_alloc(unitrec.bytes)
!	p:=pcm_alloc32()
!	p:=pcm_alloc64()
!	p:=malloc(64)
!	clear p^
	p.word1:=p.nextunit:=p.a:=p.b:=nil
	p.nextunit:=p.a:=p.b:=nil
	p.pos:=lx.pos
	return p
end

global function createintunit(int64 a)unit=
	unit u
	u:=allocunitrec()
	u.tag:=jintconst
	u.value:=a
	return u
end

global function createrealunit(real64 x)unit=
	unit u
	u:=allocunitrec()
	u.tag:=jrealconst
	u.xvalue:=x
	return u
end

global function createstringunit(ichar s, int slength=-1)unit=
	unit u
	if slength=-1 then
		slength:=strlen(s)
	fi

	u:=allocunitrec()
	u.tag:=jstringconst
	u.svalue:=pcm_alloc(slength+1)
	if slength then
		memcpy(u.svalue,s,slength)
	fi
	(u.svalue+slength)^:=0
	u.slength:=slength
	return u
end

global function createunit0(int tag)unit=
	unit u
	u:=allocunitrec()
	u.tag:=tag
	return u
end

global function createunit1(int tag, unit p)unit=
	unit u
	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global function createunit2(int tag, unit p,q)unit=
	unit u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q

	return u
end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p

	return u
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
!p can be a list, then all are added

while p do

	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer

	p:=p.nextunit
od
end

global function createavname:unit=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

!	sprintf(&.str,"av$%d",++nextavindex)
	print @str, "av$",,++nextavindex

	name:=pcm_copyheapstring(&.str)
	p:=addnamestr(name)

	return createname(p)
end

global function convtostringz(ref char svalue,int length)ref char =
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

	const strbufflen=2000
	static [0:strbufflen]char strbuffer1
	static [0:strbufflen]char strbuffer2
	static [0:strbufflen]char strbuffer3
	static [0:strbufflen]char strbuffer4
	static [0:strbufflen]char strbuffer5
	static [0:strbufflen]char strbuffer6
	static int strindex=0		!index of current buffer: cycles between 0,1,2
	static [0:]ref [0:]char table=(
		&strbuffer1,&strbuffer2,&strbuffer3,
		&strbuffer4,&strbuffer5,&strbuffer6)
!	cast(strbuffer1),cast(strbuffer2),cast(strbuffer3),
!	cast(strbuffer4),cast(strbuffer5),cast(strbuffer6))
	ref[0:]char p
	static ichar longstr=nil


	if length>=strbufflen then
		if longstr then
			free(longstr)
		fi
		longstr:=malloc(length+1)
		memcpy(longstr,svalue,length)
		(longstr+length)^:=0
		return longstr
	fi

	if svalue=nil then
		return ""
	fi

	if ++strindex=table.len then
		strindex:=0
	fi
	p:=table[strindex]
	memcpy(p,svalue,length)
!(p+length)^:=0
	p^[length]:=0
	return cast(p)
end

global function findprocname(ref proc fnptr)ichar=
	ichar name
	int n:=$get_nprocs()

	for i to n do
		if $get_procaddr(i)=fnptr then
			return $get_procname(i)
		fi
	od

	return "?"
end

global function strexpr(unit p)ref strbuffer=
	gs_init(exprstr)
	jeval(p)
	return exprstr
end

global function strexpr_s(unit p)ichar=
	if p=nil then return "" fi
	gs_init(exprstr)
	jeval(p)
	return exprstr.strptr
end

proc jeval(unit p)=
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q
	[500]char str

!CPL "JEVAL",JTAGNAMES[P.TAG]

!	switch p.tag
	case p.tag
	when jintconst then
		additem(strint(p.value))

	when jrealconst then
		additem(strreal(p.value))

	when jstringconst then
		if p.slength>str.len/2 then
			strcpy(&.str,"LONGSTR)")
		else
			convertstring(p.svalue,&.str)
		fi
		additem("""")
		additem(&.str)
		additem("""")
!
	when jname then
		additem(p.def.name)

!	when jcall then
!		jeval(p.a)
!		additem("(")
!
!		q:=p.b
!		while q do
!			jeval(q)
!			q:=q.nextunit
!			if q then additem(",") fi
!		od
!		additem(")")

	when jcallhost then
		additem("Host<")
		additem(hostfnnames[p.index]+2)
		additem(">(")

		q:=p.a
		while q do
			jeval(q)
			q:=q.nextunit
			if q then additem(",") fi
		od
		additem(")")

	when jindex,jdotindex then
		jeval(p.a)
		if p.tag=jdotindex then
			additem(".")
		fi
		additem("[")
		jeval(p.b)
		additem("]")

	when jkeyindex then
		jeval(p.a)
		additem("{")
		jeval(p.b)
		additem("}")

	when jdot then
		jeval(p.a)
		additem(".")
		jeval(p.b)

	when jassign then
		jeval(p.a)
		additem(":=")
		jeval(p.b)

	when jtypeconst then
		additem(strmode(p.mode))
!
	when jconvert then

		additem(strmode(p.mode))
		additem("(")
		jeval(p.a)
		additem(")")

	when jkeyvalue then
		jeval(p.a)
		additem(":")
		jeval(p.b)

!	when jptr then
!		jeval(p.a)
!		additem("^")
!
!	when jptrto then
!		additem("^")
!		jeval(p.a)
!
	when jnil then
		additem("nil")

	when jsymbol then
		jeval(p.a)
		additem(".$")

	when jcmpchain then
		additem("CMPCHAIN:")
		q:=p.a
		jeval(q)

		for i to 4 do
			q:=q.nextunit
			if p.cmpgenop[i]=0 then exit fi
			additem(jtagnames[p.cmpgenop[i]])
			jeval(q)
		od

	elsif jflags[p.tag]=2 then
		strcpy(&.str,getopcname(p.tag))
		additem("(")
		jevallist(p.a)
		additem(&.str)
		jevallist(p.b)
		additem(")")

	elsif jflags[p.tag]=1 then
		strcpy(&.str,getopcname(p.tag))
		additem(&.str)
		additem("(")
		jevallist(p.a)
		additem(")")


	else
		CPL jtagnames[p.tag]
		loaderror("CAN'T DO JEVAL:",jtagnames[p.tag])
	end
end

proc jevallist(unit p)=
	unit q

	return unless p

	if p.nextunit then
		additem("(")
		q:=p
		while q do
			jeval(q)
			q:=q.nextunit
			if q then additem(",") fi
		od
		additem(")")
		return
	else
		jeval(p)
	fi
end

global proc additem(ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=exprstr.strptr

	if exprstr.length then
		lastchar:=(d+exprstr.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(exprstr," ")
		fi
	fi
	strbuffer_add(exprstr,s)
end

function isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
!	if c in 'A'..'Z' or c in 'a'..'z' or c in '0'..'9' then
		return 1
	fi
	return 0
end

global function getopcname(int opc)ichar=
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
!	[16]char str
!	u64 a @ str
	static [2]u64 a
!	ichar s

	a[2]:=0

	a[1]:=jshortnames[opc]
	if a[1]=0 then
		return jtagnames[opc]+1
	else
		return cast(&a)
	fi
!	s:=jshortnames[opc]
!	if s=nil then
!		s:=jtagnames[opc]+1
!	fi
!	return s
end

global proc convertstring(ichar s, t)=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
int c

	while c:=s++^ do
		switch c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='c'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		when 7,8,26,27 then
			t++^:='<'
			t++^:=c/10+'0'
			t++^:=(c rem 10)+'0'
			t++^:='>'
		else
			t++^:=c
		end switch
	od
	t^:=0
end

global function createavnamex(symbol owner)unit p=
!local autovar needed from genpcl
!needs to update local vars

	symbol d
	p:=createavname()
	resolvename(owner,p)
	d:=p.def

	if d.nameid=frameid then
		++nproclocals
		d.index:=nproclocals
		pproclocals^:=nproclocals
	fi							!else created at module level

	return p
end

global proc storemode(symbol owner, int m, ref int16 p)=
	ref userxrec q
!CPL "STOREMODE",STRMODE(M)
	p^:=m
	if m>=0 then return fi

	q:=pcm_alloc(userxrec.bytes)
	q.owner:=owner

	IF OWNER=NIL THEN
		SERROR("STOREMODE/OWNER=0")
	FI

	q.pmode:=p
	q.nextmode:=userxmodelist
	userxmodelist:=q
end

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

	if x=0 then return 0 fi

	int a:=1
	while a<x do
		a<<:=1
	od
	return a
end

global function raiseexception(int exceptno)ref int =
	variant stackend,oldsptr

	stackend:=&varstack[1]
	oldsptr:=sptr
	do
		if sptr<=stackend then
			sptr:=oldsptr
			PCERROR("DEFAULT EXCEPTION")
!			default_exception(exceptno)
		fi
		if sptr.tag=texception and (exceptno=0 or sptr.exceptiontype=exceptno) then
			exit
		fi
		var_unshare(sptr)
		--sptr
	od

!found exception entry on stack; keep it there
	frameptr:=ref byte(sptr)+sptr.frameoffset
	return cast(sptr.ptr)
end

global proc raise_error(int error_no)=
!exception raised internally (not in user code)
!caller may not be able to manipulate pcptr
!here, push the error number, and set pcptr to point to a
!block of several kraise opcodes, as it is not how the byte-code
!handler, when it proceeds, will step pcptr

	(++sptr).tagx:=tint
	sptr.value:=error_no

	err_pcptr:=pcptr

	pcptr:=raiseseq
end

global function testelem(ref[0:]byte p,int n)int =
!caller must check that n is in range
	return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =
	p^[n>>3] ior:= bytemasks[n iand 7]
end

global proc setelemblock(ref[0:]byte p, int a,b) =
	int ax, bx, nwords, nx, alast,bfirst
	ref u64 q

	if a>b then return fi

    ax:=a iand inot 63
    bx:=b iand inot 63 + 64
	nx:=ax				!start of whole words
	alast:=bfirst:=-1

	nwords:=(bx-ax)/64

	if nwords=1 then
		if ax<>a or b<>(bx-1) then		!neither aligned, sequence is inside one word
			for i:=a to b do
				setelem(cast(p),i)
			od
			return
		fi
	else								!2 words or more
		if ax<>a then					!a not aligned
			--nwords
			nx:=ax+64
			alast:=nx-1
		fi
		if b<>bx-1 then					!b not aligned
			--nwords
			bfirst:=b iand inot 63
		fi

	fi

	if alast>=0 then					!part-word elements before whole words
		for i:=a to alast do
			setelem(cast(p),i)
		od
	fi

	q:=cast(&p[nx>>3])
	to nwords do
		q^:=0xFFFF'FFFF'FFFF'FFFF
		++q
	od

	if bfirst>=0 then				!part-word elements after whole worlds
		for i:=bfirst to b do
			setelem(cast(p),i)
		od
	fi
end

global function ispoweroftwo(int64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	int64 a
	int n

	a:=1
	n:=0
	to 60 do
		++n
		a:=a<<1
		if a=x then
			return n
		fi
	od
	return 0
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global proc skipsemi=
	while lx.symbol=semisym do lex() od
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol]:"d",symbolnames[lx.symbol]:"d"
		serror(&.str)
	fi
end

global proc skipsymbol(int symbol)=
	checksymbol(symbol)
	lex()
end

