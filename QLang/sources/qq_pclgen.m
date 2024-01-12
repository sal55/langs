!not opcodes, just used internally here for conditional jump logic
const kjumpt = 1
const kjumpf = 0

!loop stack data reused by GENMPL
const maxloopindex=20
[maxloopindex,4]ref int loopstack
[maxloopindex]int trylevelstack
global int loopindex=0
int looptrylevel			!return by findlooplabel

const maxswitchrange=512
const maxlocals=300
const maxparams=100

const maxunits=400					!for constructors
int trylevel=0
int currfunction=0				!0/1/2 = not a function/proc/function

!vars within curr procdef
int retindex						!common return point; label no
int retvaloffset					!offset of return value for procs (as stack slots)
int nprocparams						!no. of params
global int nproclocals				!no. of locals
global ref int pproclocals			!pointer to pcl operand of kprocentry; may need updating
const retaddrslots = 1				!+1 or +2, added to param indices (depends on return info size)
int procskiplabel

global proc evalunit(unit p,int res=1)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
!note: sometimes res can be 2, (passing on a res=2 from an outer stmt)
!that should be treated here same as 1 (res=2 has special meaning from pclhasvalue[] only)
	unit a,b
	symbol d
	object ss
	int procflag,index

	qpos:=p.pos

	a:=p.a
	b:=p.b

	switch p.tag
	when jintconst      then
		genpc_int(kpushci,p.value)

	when jrealconst     then
		genpc_real(kpushcr,p.xvalue)

!	when jenumconst      then
!		genpc_int2(kpushenum, p.value, p.mode)

	when jnone then

	when jstringconst   then
		pushstring(p.svalue, p.length)

	when jname          then
		d:=p.def
		case d.nameid
		when frameid then
			genpc_name(kpushf,d)
		when paramid then
			genpc_name(kpushf,d)
			if d.mbyref then
				genpc(kpushptr)
			fi
		when staticid then
			genpc_name(kpushm,d)
		when labelid then
			if not res then
				if d.labelno=0 then
					d.labelno:=createfwdlabel()
				fi
				genpc_lab(kjump,d.labelno)
				return
			else
				genpc_name(kpushsymbol,d)
			fi

		when dllvarid then
			genpc_name(kpushx, d)

		else
			genpc_name(kpushsymbol,d)
		esac

	when jsymbol        then			!assume a is jname
		if a.tag=jname then
			genpc_name(kpushsymbol,a.def)
		else
			gerror(".$ name expected")
		fi

	when jblock         then
		if a then
			while a and a.nextunit do
				evalunit(a,0)
				a:=a.nextunit
			od
			if a then
				evalunit(a,res)
			fi
		else
!			gerror("empty block")
		fi

	when jdecimal then
		pushstring(p.svalue, p.length)
		genpc(kmakedecimal)

	when jcall          then do_call(p,a,b,res,procflag)
	when jreturn        then do_return(p,a)
	when jcallhost      then do_callhost(p,a,res)

	when jassign        then do_assign(a,b,res)
	when jdeepcopy      then do_assign(a,b,res,1)
	when jto            then do_to(p,a,b)
	when jif            then do_if(p,a,b,b.nextunit, res)
	when jforup,jfordown    then do_for(p,a,b)
	when jforupx,jfordownx  then do_forx(p,a,b)
	when jforall,jforallrev then do_forall(p,a,b)
	when jforeach       then do_forall(p,a,b)
	when jwhile         then do_while(p,a,b)
	when jrepeat        then do_repeat(p,a,b)
	when jgoto          then
		if a.tag=jname and a.def.nameid=labelid then
			d:=a.def
			if d.labelno=0 then
				d.labelno:=createfwdlabel()
			fi
			genpc_lab(kjump,d.labelno)
		else
			evalunit(a)
			genpc(kjumpptr)
		fi

	when jlabeldef      then
		d:=a.def
		if d.labelno=0 then
			d.labelno:=definelabel()
		else
			index:=d.labelno
			definefwdlabel(index)
		fi

	when jredo          then do_exit(p,1)
	when jnext          then do_exit(p,2)
	when jexit          then do_exit(p,3)
	when jdo 			then do_do(p,a)
	when jcase,jdocase then do_case(p,a,b,res)
	when jswitch, jdoswitch then do_switch(p,a,b,res)
	when jswap          then
		evalref(a)
		evalref(b)
		genpc(kswap)

	when jselect        then do_select(a,b,res)
	when jprint,jprintln,jsprint    then do_print(p,a,b)
	when jfprint,jfprintln,jsfprint then do_fprint(p,a,b,b.nextunit)
	when jread,jreadln  then do_read(p,a,b)
!	when jnew           then do_new(p)

	when jstop          then
		if a then
			evalunit(a)
		else
			genpc_int(kpushci,0)
		fi
		genpc(kstop)

	when jtry           then do_try(p,a,b)

	when jandl          then do_andl(a,b)
	when jorl          then do_orl(a,b)
	when jmakelist then
		do_pushlist(a,p.length)
		genpc_int2(kmakelist,p.length,p.lower)

	when jmakeset then
		do_pushlist(a,p.length)
		genpc_int(kmakeset,p.length)

	when jmakedict then do_makedict(a,p.length)

!	when jmakeclosure then do_makeclosure(a)

	when jkeyvalue      then
		evalunit(a)
		evalunit(b)
	when jmap           then do_map(p,a,b)

	when jadd, jsub, jmul, jdiv, jidivrem, jiand, jior, jixor,
		 jshl, jshr, jin, jnotin, jinx, jmin, jmax, jmakerange, jmakerangelen,
		 jeq, jne, jlt, jle, jge, jgt, jpower,
		 jconcat, jappend,jisequal then
		evalunit(a)
		evalunit(b)
		genpc(jpclcodes[p.tag])

	when jaddto, jsubto, jmulto, jdivto, jidivto, jiandto, jiorto, jixorto,
		 jshlto, jshrto, jminto, jmaxto, jconcatto, jappendto,
		 jandlto, jorlto then
		evalref(a)
		evalunit(b)
		genpc(jpclcodes[p.tag])

	when jidiv then
		do_idiv(p,a,b)

	when jirem then
		do_irem(p,a,b)

	when jneg, jabs, jlwb, jupb, jlen, jbounds, jnotl, jinot,jisarray,
		 jisint, jisreal, jbytesize, jisdef, jround, jisvoid, jtype, jbitwidth,
		 jistruel, jsqr, jsqrt, jislist, jasc,jchr, jisstring, jisset,
		 jbasetype, jusertype, jelemtype, jispointer, jisrange, jisrecord,
		 jfloor, jceil, jboundsx, jisnumber, jismutable, jsign,
		 jsin,jcos,jtan, jasin, jacos, jatan, jexp, jln,
		 jminvalue, jmaxvalue, jdictitems, jodd, jeven, jisfound then
		do_unary(a,jpclcodes[p.tag])

	when jnegto, jabsto, jinotto, jnotlto then
		evalref(a)
		genpc(jpclcodes[p.tag])

	when jdot           then! do_bin(a,b,kdot)
		evalunit(a)
		genpc_name(kdot,b.def)

	when jindex         then do_bin(a,b,kindex)
	when jdotindex      then do_bin(a,b,kdotindex)
	when jkeyindex      then
		evalunit(a)
		evalunit(b)
		if b.nextunit then
			evalunit(b.nextunit)
		else
			genpc(kpushvoid)
		fi
		genpc(kkeyindex)

	when jptr           then do_unary(a,kpushptr)
	when jptrto then
		if a.tag=jptr then			!^a^ cancel out (a might be byref param)
			evalunit(a.a)
		else
			evalref(a)
		fi

	when jaddrof        then
		evalref(a)
		genpc(kconvrefpack)

	when jconvert       then
		do_convert(p)

	when jtypepun       then
		evalunit(a)
		genpc_int(ktypepun,p.mode)

	when jtypeconst     then
		genpc_int(kpusht,p.mode)
	when joperator      then
		genpc_int(kpushoperator,p.pclopcode)


	when jincrload, jdecrload, jloadincr, jloaddecr then
		do_incr(p,a,res)
!
	when jnil           then
		genpc(kpushnil)

	when jraise         then do_unary(a,kraise)

	when jnull then
		genpc(kpushvoid)


	else
		gerror_s("UNSUPPORTED TAG:",JTAGNAMES[P.TAG],p)
	end switch

	case jhasvalue[p.tag]
	when 0 then
		if res then
			gerror_s("Value expected:",jtagnames[p.tag])
		fi
	when 1 then
		if not res then
			if p.tag=jcall and procflag=1 then		!procs have no ret value
			elsif p.tag in [jincrload,jdecrload,jloadincr,jloaddecr] then
			elsif p.tag=jcallhost and hostisfn[p.index]=0 then
			else
				genpc_int(kunshare,1)
			fi
		fi
	esac						!else ignore when 2, as already dealt with
end

global proc gencodemodule(isubprog sp, int moduleno)=
	const maxanonprocs=100
	[maxanonprocs]symbol anonprocs
	int nanonprocs:=0

	symbol d,e
	int lab
	int a:=sp.firstmodule
	int b:=sp.lastmodule
	ifile pm:=modules[moduleno]

	currmodule:=pm
	stcurrproc:=stcurrmodule:=currmodule.def

!CPL "GENCODE",SP.NAME,PM.NAME

	resetpcl(pm.size)

!GOTO FINISH

	gencomment("Module data init code:")

	qpos:=0
	qpos.[24..31]:=moduleno

!CPL "///////////////",QPOS

!jump around stop/raise block needed for reentry
!	if n=1 then
	if moduleno=a then
		lab:=createfwdlabel()
		genpc_lab(kjump,lab)
		genpc(kstoprunproc)
		stopseq:=pcllast

		raiseseq:=pcllast+1
		genpc_int(kpushci,0)
		genpc(kraise)
		definefwdlabel(lab)
	fi

	d:=stcurrmodule.deflist
	while d do
		if d.nameid=staticid and d.code then
			evalunit(d.code)
			if d.initcode=3 then
				genpc(kcopy)
			fi
			genpc_name(kzpopm,d)
		elsif d.nameid=procid then
			e:=d.deflist
			while e do
				if e.nameid=staticid and e.code then
					evalunit(e.code)
					genpc_name(kzpopm,e)
				elsif e.nameid=anonprocid then
					if nanonprocs>=maxanonprocs then gerror("Too many anons") fi
					anonprocs[++nanonprocs]:=e
				fi
				e:=e.nextdef
			od
		fi
		d:=d.nextdef
	od	

	if moduleno=a then
		for i:=b downto a+1 do
			genpc_name(kmodulecall, modules[i].def)
		od
		for i:=b downto a+1 do
			if modules[i].startfn then
				genpc_name(kcallproc, modules[i].startfn)
				genopnd_int(0)
			fi
		od

		if currmodule.startfn then
			genpc_name(kcallproc, currmodule.startfn)
			genopnd_int(0)
		fi

		if currmodule.mainfn then
			genpc_name(kcallproc, currmodule.mainfn)
			genopnd_int(0)
		fi

		evalunit(stcurrmodule.code,0)
		genpc_int(kpushci,0)
		genpc(kstop)
	else
		evalunit(stcurrmodule.code,0)
		genpc(kmodulereturn)
	fi

	gencomment("Procs:")
	d:=stcurrmodule.deflist
	while d do
		switch d.nameid
		when procid,anonprocid then
			do_procdef(d)
		when staticid then
!		when typeid then
		when recordid then
			e:=d.deflist
			while e, e:=e.nextdef do
				if e.nameid=procid then
					do_procdef(e)
				fi
			od

		when constid then
		when enumid then
		when labelid then
		when typeid then
		when dllprocid then
		when aliasid then
		when macroid then
		when dllvarid then
		else
			gerror_s("?Module def:",namenames[d.nameid])
		end switch

		d:=d.nextdef
	od	

	for i to nanonprocs do
		do_procdef(anonprocs[i])
	od

	genpc(kendmodule)

FINISH:
	pm.pcstart:=pclstart
	pm.pcend:=pclend
	pm.pcsize:=pclend-pclstart
	pm.pcsrcstart:=pclsrcstart

!CPL "------DONE GENPCL",PCLSTART,PCLNEXT-PCLSTART, PCLNAMES[PCLSTART^]

end

proc do_procdef(symbol p) =
	int nfreevars,nnofreevars
	int isfunc
	symbol oldcurrproc

	oldcurrproc:=stcurrproc			!might be a method

	stcurrproc:=p

	retindex:=createfwdlabel()
	isfunc:=p.misfunc

	genprocentry(p,nfreevars,nnofreevars)

	if p.code=nil then
		gerror_s("Empty proc body",p.name)
	else
		evalunit(p.code, isfunc)

	fi

	definefwdlabel(retindex)			!common return point
	genprocexit(nfreevars,nnofreevars,isfunc)
	genpc(kprocend)

	if pproclocals^=0 then
		p.labelno:=procskiplabel
	fi
	stcurrproc:=oldcurrproc
end

proc genprocentry(symbol p, int &nfreevars,&nnofreevars) =		!GENPROCENTRY
	[200]char str
	int n
	symbol d

	genpc_name(kprocdef, p)

	nprocparams:=nproclocals:=0

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			++nproclocals
			d.index:=nproclocals
		when paramid then
			++nprocparams
		esac

		d:=d.nextdef
	od

	d:=p.deflist
	n:=nprocparams

	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
			--n
			d.index:=-(n+retaddrslots)
		esac

	od

	retvaloffset:=-(nprocparams+retaddrslots)
!
	p.labelno:=definelabel()
	genpc_int(kprocentry, nproclocals)
	procskiplabel:=definelabel()

	pproclocals:=pclnext-1

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			if d.code then
				evalunit(d.code)
				if d.initcode=3 then
					genpc(kcopy)
				fi
				genpc_name(kzpopf, d)
			fi
		esac

		d:=d.nextdef
	od
end

proc genprocexit(int nfree,nnofree,isfunc)=		!GENPROCEXIT
	int offset

	if isfunc then
		offset:=-(nprocparams+1)*varsize
		genpc_int(kpopretval,offset)
	fi
	if nproclocals then
		genpc_int(kunshare,nproclocals)
	fi

	if nprocparams=0 then
		genpc(kreturn0)
	else
		genpc_int(kreturn,nprocparams)
	fi
end

proc evalref(unit p)=
	unit a,b,c
	symbol d
	int lab1,lab2
	a:=p.a
	b:=p.b

	switch p.tag
	when jname then
		d:=p.def
		if d.nameid in [procid,dllprocid] then
			gerror("^ not allowed")
		fi	

		if d.nameid=paramid and d.mbyref then
			genpc_name(kpushf,d)
		else
			genpc_name(kpushmref+d.isframe,d)
		fi

	when jdot then! do_binref(a,b,kdotref)
		evalunit(a)
		genpc_name(kdotref,b.def)
	when jindex then! do_binref(a,b,kindexref)
		evalunit(a)
		evalunit(b)
		genpc(kindexref)

	when jdotindex then! do_binref(a,b,kdotindexref)
!		evalunit(a)
		evalref(a)
		evalunit(b)
		genpc(kdotindexref)

	when jkeyindex then! do_binref(a,b,kkeyindexref)
		evalunit(a)
		evalunit(b)
		if b.nextunit then gerror("Def val not allowed") fi
		genpc(kkeyindexref)

	when jptr then
		evalunit(a)

	when jif then
		lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
		lab2:=createfwdlabel()

		genjumpcond(kjumpf,p.a,lab1)
		evalref(p.b)
		genjumpl(lab2)
		definefwdlabel(lab1)
		evalref(p.b.nextunit)
		definefwdlabel(lab2)
	else
!		case p.tag
!		when jif then
!			do_if(p,a,b,c,1)
!		when jlongif then
!			do_longif(p,a,b,1)
!		when jselect then
!			do_select(p,a,b,c,1)
!		when jswitch then
!			do_switch(p,a,b,c,0,1)
!		when jcase then
!			do_case(p,a,b,c,0,1)
!		else
!			PRINTUNIT(P)
			gerror_s("evalref",jtagnames[p.tag])
!		esac
	end switch
end

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q,r,s
	int oldpos, lab2, i

	q:=p.a
	r:=p.b

	switch p.tag
	when jandl then
		case opc
		when kjumpf then
			genjumpcond(kjumpf,q,lab)
			genjumpcond(kjumpf,r,lab)
		when kjumpt then
			lab2:=createfwdlabel()
			genjumpcond(kjumpf,q,lab2)
			genjumpcond(kjumpt,r,lab)
			definefwdlabel(lab2)
		esac

	when jorl then
		case opc
		when kjumpf then
			lab2:=createfwdlabel()
			genjumpcond(kjumpt,q,lab2)
			genjumpcond(kjumpf,r,lab)
			definefwdlabel(lab2)
		when kjumpt then
			genjumpcond(kjumpt,q,lab)
			genjumpcond(kjumpt,r,lab)
		esac

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when jistruel then
		genjumpcond(opc,q,lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when jeq,jne,jlt,jle,jge,jgt then
		evalunit(q)
		evalunit(r)
		gcomparejump(opc,p.tag,lab)

	when jcmpchain then
		r:=q.nextunit
		i:=1
		if opc=kjumpf then
			while r do
				evalunit(q)
				evalunit(r)
				gcomparejump(kjumpt,reversecond(p.cmpgenop[i]),lab)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(q)
				evalunit(r)
				if r.nextunit then
					gcomparejump(kjumpt,reversecond(p.cmpgenop[i]),lab2)
				else
					gcomparejump(kjumpt,p.cmpgenop[i],lab)
				fi
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else
		evalunit(p)
		genpc_lab((opc=kjumpt|kjumptrue|kjumpfalse),lab)
	end switch
	qpos:=oldpos

end

proc gcomparejump(int jumpopc,int cond, lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int opc

!	cond:=p.tag				!eqop,neop, etc

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	case cond
	when jeq then opc:=kjumpeq
	when jne then opc:=kjumpne
	when jlt then opc:=kjumplt
	when jle then opc:=kjumple
	when jge then opc:=kjumpge
	when jgt then opc:=kjumpgt
	else
		gerror("GCOMP: no cond")
	esac

	genpc_lab(opc,lab)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc_lab(kjump,lab)
end

function reversecond(int op)int=
!reverse conditional operator

	case op
	when jeq then return jne
	when jne then return jeq
	when jlt then return jge
	when jle then return jgt
	when jge then return jlt
	when jgt then return jle
	esac
	return 0
end

global proc stacklooplabels(ref int a,b,c)=
!a is a list of labels associated with a loop, usually 4 in order A,B,C,D
	if loopindex>=maxloopindex then
		gerror("Too many nested loops")
	fi
	++loopindex
	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c
!	loopstack[loopindex,4]:=d
!	trylevelstack[loopindex]:=trylevel
end

global proc unstacklooplabels=
	--loopindex
end

global function findlooplabel(int k,n)int=
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
	int i

	if n=0 then			!outermost loop
		i:=1
	else
		i:=loopindex-(n-1)		!point to entry
	fi

	if i<1 or i>loopindex then
		gerror("Bad loop index")
	fi

	looptrylevel:=trylevelstack[i]
	return loopstack[i,k]^
end

proc do_assign(unit a,b, int res,deepcopy=0)=
	unit q
	int n

	if a.tag=b.tag=jmakelist then
		if res then gerror("mult/ass::=") fi
!		if deepcopy then gerror("mult/ass::=") fi
		do_multassign(a,b, deepcopy, res)
		return
	fi

	evalunit(b)
	if deepcopy then
		genpc(kcopy)
	fi

	do_store(a,res)
end

proc do_bin(unit a,b, int opc)=
	evalunit(a)
	evalunit(b)
	genpc(opc)
end

proc do_binref(unit a,b, int opc)=
	evalref(a)
	evalunit(b)
	genpc(opc)
end

proc do_unary(unit a, int opc)=
	evalunit(a)
	genpc(opc)
end

proc do_unaryref(unit a, int opc)=
	evalref(a)
	genpc(opc)
end

proc do_pushlist(unit a, int n)=
	while a, a:=a.nextunit do
		evalunit(a)
	od
end

proc do_makedict(unit a, int n)=
	to n do
		if a.tag=jkeyvalue then
			evalunit(a.a)
			evalunit(a.b)
		else
			gerror("dict not key:val")
		fi
		a:=a.nextunit
	od
	genpc_int(kmakedict,n)
end

proc do_call(unit p,a,b,int res, &procflag)=
	int nargs, nsimple, isfunc, kwdindex
	symbol d
	unit c
	[maxparams]unit arglist

	isfunc:=1
	nargs:=nsimple:=0
	kwdindex:=0
	c:=b

	while c do
		arglist[++nargs]:=c
		if c.tag in [jintconst, jrealconst] then ++nsimple fi
		if c.tag=jkeyword then
			if kwdindex=0 then kwdindex:=nargs fi
		elsif kwdindex then
			gerror("Non-kwd follows kwd arg")
		fi
		c:=c.nextunit
	od

	case a.tag
	when jname then
		d:=a.def
retry:
		case d.nameid
		when procid, anonprocid then
			if d.misfunc then
				genpc(kpushvoid)
				nargs:=pushparams(d, arglist, nargs, kwdindex)
!				genpc_name(kcallfn,d)
				genpc_name(kcallproc,d)
			else					!proc call
				isfunc:=0
				nargs:=pushparams(d, arglist, nargs, kwdindex)
				genpc_name(kcallproc,d)
			fi
			genopnd_int(nargs)

		when dllprocid then
			if not d.misfunc then
				isfunc:=0
			else
				genpc(kpushvoid)
			fi
			nargs:=pushparams(d, arglist, nargs, kwdindex)
			genpc_name(kcalldll,d)
			genopnd_int(nargs)
!			genopnd_int(d.mode)
		when aliasid then
			d:=d.alias
			goto retry
		when staticid, frameid, paramid then
			goto docallptr
!
		else
			gerror_s("CAN'T CALL:",namenames[d.nameid])
		esac
	when jdot then
		if kwdindex then docallptr fi		!share error
		genpc(kpushvoid)
		evalref(a.a)					!push &self arg
		for i to nargs do				!any extra ones
			evalunit(arglist[i])
		od
		evalunit(a)						!push lhs again, this time for dot
		genpc(kcallptr)
		++nargs
		genopnd_int(nargs)
		genopnd_int(0)

	else
docallptr:
		if kwdindex then gerror("Kwd params not allowed for fnptr") fi
		genpc(kpushvoid)
		for i to nargs do
			evalunit(arglist[i])
		od
		evalunit(a)
		genpc(kcallptr)
		genopnd_int(nargs)
		genopnd_int(0)
	esac

	if res and not isfunc then
		gerror("Func ret value expected")
	fi

	procflag:=not isfunc
end

function pushparams(symbol d, []unit &arglist, int nargs, kwdindex)int=
!push args for a known, named function
!will deal with missing/optional args, default values, and keyword params
!should work also for dll procs
!In all cases, first nparams items in d.deflist will be parameter names,
!For dlls with no named params, the entries will be $1 etc.

	int nparams, extra,n
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	symbol e, p

	nparams:=d.nparams
	e:=d.deflist
	n:=0
	while e do
		++n
		paramlist[n]:=e
		byreflist[n]:=e.mbyref
		e:=e.nextdef
	od

	if kwdindex then
		pushkwdparams(d, arglist, nargs, kwdindex)
		return d.nparams
	fi

	extra:=0

	if nargs=nparams then
		for i to nargs do
			evalparam(arglist[i],byreflist[i])
		od
		return nargs
	elsif nargs<nparams then	!trailing args missing
		for i to nargs do
			evalparam(arglist[i],byreflist[i])
		od

		for i:=nargs+1 to nparams do
			p:=paramlist[i]
			if not p.code and not p.moptional then
				gerror_s("Param not optional:",strint(i))
			fi
			if p.code then
				if byreflist[i] then gerror("byref with default val") fi
				evalunit(p.code)
			else
				genpc(kpushvoid)
			fi
		od
		return nparams
	else						!nargs>nparams: variadic
		for i to nparams do
			evalparam(arglist[i],byreflist[i])
		od

		if not d.mvarparams then
			gerror("Too many args")
		fi
		for i:=nparams+1 to nargs do
			evalunit(arglist[i])			!o/p variadic args
		od
		return nargs
	fi
end

proc evalparam(unit a, int byref)=
	if byref then
		evalref(a)
	else
		evalunit(a)
	fi
end


proc pushkwdparams(symbol d, []unit &arglist, int nargs, kwdindex)=
	int nparams, i,j,k
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	[maxparams]unit keyunits
	unit p,q
	symbol e

	nparams:=d.nparams

	e:=d.deflist
	for i to nparams do
		paramlist[i]:=e
		byreflist[i]:=e.mbyref
		e:=e.nextdef
	od

	if nargs>nparams then
		gerror("Too many args")
	fi

	for i:=kwdindex to nparams do
		keyunits[i]:=nil			!indicate param not set
	od

	for i to kwdindex-1 do			!do positional params
		evalparam(arglist[i],byreflist[i])
	od

	for i:=kwdindex to nargs do
		p:=arglist[i]
		q:=p.a
		if q.tag<>jname then gerror("kwd not a name") fi
		e:=q.def
		k:=0
		for j:=1 to nparams do
			if eqstring(e.name, paramlist[j].name) then
				k:=j
				exit
			fi
		od

		if k=0 then gerror_s("Can't find kwd param:",e.name) fi
		if k<kwdindex then gerror_s("Kwd arg already positional:",e.name) fi
		if keyunits[k] then gerror_s("Repeating kwd arg:",e.name) fi

		keyunits[k]:=p.b
	od

	for i:=kwdindex to nparams do
		if keyunits[i]=nil then
			q:=paramlist[i].code
			if q=nil and not paramlist[i].moptional then
				gerror_s("Param not optional:",strint(i))
			fi
			keyunits[i]:=q			!q is nil when default value not set
		fi
	od

!	for i:=nparams downto kwdindex do
	for i:=kwdindex to nparams do
		if keyunits[i] then
			evalparam(keyunits[i],byreflist[i])
		elsif byreflist[i] then
			gerror("byref param not optional")
		else
			genpc(kpushvoid)
		fi
	od
end

proc do_if(unit p,a,b,pelse, int res)=
	int lab1,lab2

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)

	if pelse or res then lab2:=createfwdlabel() fi	!label past else part

	genjumpcond(kjumpf,a,lab1)

	evalunit(b,res)

	if pelse or res then
		genjumpl(lab2)
		definefwdlabel(lab1)
		if pelse then
			evalunit(pelse,res)
		else
			genpc(kpushvoid)
		fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_do(unit p,a)=
	int lab_abc,lab_d,lab_test
	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(&lab_abc, &lab_abc, &lab_d)

	evalunit(a,0)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_exit(unit p,int k) =
	int n,index

	index:=p.a.value
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k,index)
	if n=0 then
CPL "BAD LOOP"
!		gerror("Bad exit/loop index",p)
	else
		genjumpl(n)
	fi
end

proc do_to(unit p,pcount,pbody)=
	int lab_b,lab_c,lab_d
	symbol temp
	unit pav

	pav:=pcount.nextunit
	temp:=pav.def

	evalunit(pcount)
	genpc_name(kzpopm+temp.isframe,temp)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(&lab_b,&lab_c,&lab_d)

!check for count being nonzero
	if pcount.tag<>jintconst then			!assume const limit is non-zero
		genpc_name(kpushm+temp.isframe,temp)
		genpc_int(kpushci,0)
		genpc_lab(kjumple,lab_d)

	elsif pcount.value<=0 then		!const <=0, skip body
		genpc_lab(kjump,lab_d)
	fi

	definefwdlabel(lab_b)
	evalunit(pbody,0)
	definefwdlabel(lab_c)

	genpc_lab(ktom+temp.isframe,lab_b)
	genopnd_name(temp)


	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_while(unit p,pcond,pbody) =
	int lab_b,lab_c,lab_d,lab_incr
	unit pincr:=pcond.nextunit

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pincr then
		lab_incr:=createfwdlabel()
	else
		lab_incr:=lab_c
	fi

	stacklooplabels(&lab_b, &lab_c, &lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalunit(pbody,0)

	definefwdlabel(lab_c)

	if pincr then
		evalunit(pincr)
		definefwdlabel(lab_incr)
	fi

	genjumpcond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p,a,b) =
	int lab_b, lab_c, lab_d

	lab_b:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(&lab_b, &lab_c, &lab_d)

	evalunit(a,0)

	definefwdlabel(lab_c)

	unless b.tag=jintconst and b.value=0 then
		genjumpcond(kjumpf,b,lab_b)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_for(unit p,pvar,pbody)=
! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
	unit pfrom, pto, pstep, pelse,plimit,pautovar
	symbol dvar, limitvar
	int lab_b,lab_c,lab_d,lab_e,opc,oldqpos
	int step, fromval, limit, jumpinto

	pfrom:=pvar.nextunit
	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pautovar:=nil
	if pstep then
		gerror("By N not implem")
	fi

	pelse:=pbody.nextunit

	dvar:=pvar.def

	if pto.tag not in [jintconst,jname] or
		 pto.tag=jname and pto.def.isframe<>dvar.isframe then
		pautovar:=createavnamex(stcurrproc)
	fi

	case p.tag
	when jforup then
		step:=1

	when jfordown then
		step:=-1
	esac

	jumpinto:=1			!assume jumping straight into increment

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(&lab_b,&lab_c,&lab_d)

	if pfrom.tag=jintconst then		!can incr/decr directly
		fromval:=pfrom.value
!see if limit is known
		if pto.tag=jintconst then
			limit:=pto.value
			if (step=-1 and fromval>=limit) or (step=1 and fromval<=limit) then 	!at least 1 iteration
				jumpinto:=0
			fi
		fi
		if jumpinto then
			if step<0 then
				++fromval
			else
				--fromval
			fi
			pfrom.value:=fromval
		fi
		genpc_int(kpushci,pfrom.value)

		genpc_name(kpopm+dvar.isframe,dvar)
	else
		evalunit(pfrom)
		genpc_name(kpopm+dvar.isframe,dvar)

		genpc_name((step<0|kincrtom|kdecrtom)+dvar.isframe,dvar)
	fi

	if pautovar then
		evalunit(pto)
		limitvar:=pautovar.def
		genpc_name(kzpopm+limitvar.isframe,limitvar)
		pto:=pautovar
	else
		limitvar:=pto.def
	fi

	if jumpinto then
		genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	fi
	definefwdlabel(lab_b)

	evalunit(pbody,0)				!do loop body

	definefwdlabel(lab_c)

	if pto.tag=jintconst then
		opc:=(step<0|kfordmci|kformci)+dvar.isframe
	elsif dvar.isframe=limitvar.isframe then
		opc:=(step<0|kfordmm|kformm)+dvar.isframe
	else
		gerror("for:mixed m/f vars")
	fi

	oldqpos:=qpos
	qpos:=p.pos
	genpc_lab(opc,lab_b)
	qpos:=oldqpos
	genopnd_name(dvar)

	if pto.tag=jintconst then
		genopnd_int(pto.value)
	else
		genopnd_name(limitvar)
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse,0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_forx(unit p,pvar,pbody)=
! a = pvar, pbounds
! b = pbody [pelse]
	unit pbounds, pelse,plimit,pautovar
	symbol dvar, limitvar
	int lab_b,lab_c,lab_d,lab_e,opc

	pbounds:=pvar.nextunit

	pautovar:=createavnamex(stcurrproc)

	pelse:=pbody.nextunit
	dvar:=pvar.def

	if p.tag=jfordownx then
		gerror("Can't down inrev yet")
	fi

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(&lab_b,&lab_c,&lab_d)

	evalunit(pbounds)				!stack has lwb, upb
	limitvar:=pautovar.def
	genpc_name(kzpopm+limitvar.isframe,limitvar)

	genpc(kdecr)
	genpc_name(kpopm+dvar.isframe,dvar)		!from value

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	definefwdlabel(lab_b)

	evalunit(pbody,0)				!do loop body

	definefwdlabel(lab_c)

	if dvar.isframe=limitvar.isframe then
		genpc_lab(kformm+dvar.isframe,lab_b)
	else
		gerror("forx:mixed m/f")
	fi
	genopnd_name(dvar)
	genopnd_name(limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse,0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_print(unit p,a,b)=
	int issprint
	unit x

	issprint:=p.tag=jsprint

	if issprint then
CPL "//////ISSPRINT"
		callhostfn(h_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(h_startprint)
		else
			callhostfn(h_startprintcon)
		fi
	fi

	x:=b

	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(h_print)
		when jnogap then
			callhostfn(h_printnogap)
		when jspace then
			callhostfn(h_printspace)
		else
			evalunit(x)
			callhostfn(h_print_nf)
		esac
		x:=x.nextunit
	od

	if p.tag=jprintln then
		callhostfn(h_println)
	fi
	if issprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi
end

proc do_fprint(unit p,a,b,c)=
	int issfprint
	unit x

	issfprint:=p.tag=jsfprint

	if issfprint then
		callhostfn(h_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(h_startprint)
		else
			callhostfn(h_startprintcon)
		fi
	fi

	evalunit(b)					!format string
	callhostfn(h_setformat)

	x:=c
	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(h_print)
		when jnogap then
			callhostfn(h_printnogap)
		else
			genpc(kpushvoid)
			evalunit(x)
			callhostfn(h_print)
		esac
		x:=x.nextunit
	od

	if p.tag=jfprintln then
		callhostfn(h_println)
	fi
	if issfprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi

end

proc do_read(unit p,a,b)=
unit x,xloop

if p.tag=jreadln then
	if a then
		evalunit(a)
		callhostfn(h_readln)
	else
		genpc(kpushvoid)
		callhostfn(h_readln)
	fi
fi

xloop:=b
while xloop do
	x:=xloop
	genpc(kpushvoid)
	if x.tag=jfmtitem then
		evalunit(x.b)
		callhostfn(h_sread)
		x:=x.a
	else
		genpc(kpushvoid)
		callhostfn(h_sread)
	fi
	if x.tag=jname then
		genpc_name(kpopm+x.def.isframe,x.def)
	else
		evalref(x)
		genpc(kpopptr)
	fi
	xloop:=xloop.nextunit
od
end

proc do_forall(unit p,pindex,pbody)=
!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, plist, pvar
! b = pbody, [pelse]

	int lab_b,lab_c,lab_d,lab_e
	unit ploopvar, plist, pelse, plimitvar, plistvar
	symbol indexvar,limitvar,loopvar, listvar

	plist:=pindex.nextunit
	ploopvar:=plist.nextunit

	if ploopvar=nil then			!no discrete index var
		ploopvar:=pindex

		pindex:=createavnamex(stcurrproc)

	fi
	loopvar:=ploopvar.def

	plimitvar:=createavnamex(stcurrproc)

	limitvar:=plimitvar.def
	indexvar:=pindex.def

	if plist.tag<>jname or plist.def.isframe<>loopvar.isframe then			!complex list

		plistvar:=createavnamex(stcurrproc)

		listvar:=plistvar.def
		evalunit(plist)
		genpc_name(kzpopm+listvar.isframe,listvar)
	else
		plistvar:=plist
		listvar:=plistvar.def
	fi

	unless indexvar.isframe=loopvar.isframe=listvar.isframe then
		gerror("forall: mixed vars")
	end

	pelse:=pbody.nextunit

	if p.tag=jforallrev then
		gerror("Forall/rev not ready")
	fi

!set up initial loop var
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(&lab_b,&lab_c,&lab_d)

!assume plist is a var where bounds are not known
!(can be optimised for a const range or a const list)
	genpc_name(kpushm+listvar.isframe, listvar)			!load the list
	genpc(kboundsx)			!extract bounds as (lower, upper); upper is tos

	genpc_name(kzpopm+listvar.isframe,limitvar)		!limit:=upb
	genpc(kdecr)
	genpc_name(kzpopm+indexvar.isframe,indexvar)		!index:=lwb-1 (will incr first thing)

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:

	definefwdlabel(lab_b)

!start of iteration, set up next loop variable
	genpc_name(kpushm+listvar.isframe,listvar)
	evalunit(pindex)

	if p.tag in [jforall,jforallrev] then
		genpc(kindex)
	else
		genpc(kdotindex)
	fi
	genpc_name(kpopm+loopvar.isframe,loopvar)

	evalunit(pbody,0)			!do loop body

	definefwdlabel(lab_c)

	if indexvar.isframe=limitvar.isframe then
		genpc_lab(kformm+indexvar.isframe,lab_b)
	else
		gerror("forall:mixed m/f")
	fi
	genopnd_name(indexvar)
	genopnd_name(limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse,0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_case(unit p,pindex,pwhenthen,int res) =
!also temporarily deal wit switch/doswitch

	int lab_a,lab_d
	int loopsw,labnextwhen,labstmtstart,fmult
	unit w,wt,pelse

	if pindex.tag=jnone then
		do_case_nc(p,pindex,pwhenthen,res)
		return
	fi

	loopsw:=p.tag=jdocase or p.tag=jdoswitch
	pelse:=pindex.nextunit

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(&lab_a,&lab_a,&lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	evalunit(pindex)			!load test expr p to t

	wt:=pwhenthen
	while wt do
		w:=wt.a
		fmult:=w.nextunit<>nil

		labnextwhen:=createfwdlabel()

		if fmult then
			labstmtstart:=createfwdlabel()
		fi

		while w do
			evalunit(w)
			w:=w.nextunit
			if w then					!not last
				genpc_lab(kjumptesteq,labstmtstart)
			else
				genpc_lab(kjumptestne,labnextwhen)
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b,res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	genpc_int(kunshare,1)

	if pelse then
		evalunit(pelse,res)
	elsif res then
		genpc(kpushvoid)
	fi
	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi
end

proc do_case_nc(unit p,pindex,pwhenthen,int res) =
!when no control expression

	int lab_a,lab_d
	int labnextwhen,labstmtstart,fmult
	unit w,wt,pelse

	if p.tag<>jcase then gerror("case-nc") fi

	pelse:=pindex.nextunit

	lab_d:=createfwdlabel()

	wt:=pwhenthen
	while wt do
		w:=wt.a
		fmult:=w.nextunit<>nil

		labnextwhen:=createfwdlabel()

		if fmult then
			labstmtstart:=createfwdlabel()
		fi

		while w do
			evalunit(w)
			w:=w.nextunit
			if w then					!not last
				genpc_lab(kjumptrue,labstmtstart)
			else
				genpc_lab(kjumpfalse,labnextwhen)
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b,res)

		genjumpl(lab_d)
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	if pelse then
		evalunit(pelse,res)
	elsif res then
		gerror("Needs Else branch")
!		genpc(kpushvoid)
	fi

	definefwdlabel(lab_d)
end

proc do_try(unit p,a,b) =
	int labend,labx
	unit ptry,x,pexcept,pexcode

	++trylevel
	labend:=createfwdlabel()
	ptry:=a
	labx:=createfwdlabel()

	pexcept:=b

	if pexcept=nil then
		gerror("try: no except")
	elsif pexcept.nextunit then
		gerror("Try:multiple except block not implemented")
	fi

	while pexcept do
		pexcode:=pexcept.a
		if pexcode=nil or pexcode.nextunit then
			gerror("Try:multiple except codes not implemented")
		fi
		genpc_lab(kpushtry,labx)
		genopnd_int(getconstvalue(pexcode))
		genopnd_int(1)
		evalunit(ptry,0)
		genjumpl(labend)
		definefwdlabel(labx)
		evalunit(pexcept.b,0)
		definefwdlabel(labend)
		pexcept:=pexcept.nextunit
	od

	genpc_int(kaddsp,1)
	--trylevel
end

function unitstoarray(unit p, ref[]unit plist, int maxunits)int=
!convert a linked list of units to a linear list
!return number of units
	int n

	n:=0
	while p do
		if n>=maxunits then
			gerror("UTA Too many units")
		fi
		plist^[++n]:=p
		p:=p.nextunit
	od
	
	return n
end

proc do_select(unit pindex,pplist,int res)=
!generate selectx expression
	int n,labend,i,lab,elselab
	unit x,pelse

	[maxswitchrange]unit plist
	[maxswitchrange+1]int labels

	pelse:=pindex.nextunit

	n:=unitstoarray(pplist,&plist,maxswitchrange)

	if n>maxswitchrange then
		gerror("Selectx too complex")
	fi

	labend:=createfwdlabel()

	evalunit(pindex)
!	genpc_int2(kselect,n,1)
	genpc_int2(kswitch,n,1)

	for i:=1 to n do
		labels[i]:=pclnext-pclstart		!store destination code index
		genpc_lab(kjumplabel,0)
	od
	labels[n+1]:=pclnext-pclstart
	genpc_lab(kjumplabel,0)

!scan when statements again, o/p statements
	i:=1
	for i:=1 to n do
		x:=plist[i]
		lab:=definelabel()

		(pclstart+labels[i]+1)^:=lab
		evalunit(x,res)

		genjumpl(labend)	!break to end of statement
	od

	elselab:=definelabel()

	(pclstart+labels[n+1]+1)^:=elselab

	if pelse then
		evalunit(pelse,res)
	elsif res then
		genpc(kpushvoid)
	fi

	genpc(knop)

	definefwdlabel(labend)
end

proc do_andl(unit x,y)=
	int a,b

	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpf,x,a)
	genjumpcond(kjumpf,y,a)

	genpc_int(kpushci,1)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci,0)
	genpc(knop)
	definefwdlabel(b)
end

proc do_orl(unit x,y)=
	int a,b
	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpt,x,a)
	genjumpcond(kjumpt,y,a)
	genpc_int(kpushci,0)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci,1)
	genpc(knop)
	definefwdlabel(b)
end

proc do_incr(unit p,a, int res)=
	symbol d
	if res then
		do_unaryref(a,jpclcodes[p.tag])
	elsif a.tag=jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			goto dounary
		else
			genpc_name((p.tag in [jincrload,jloadincr]|kincrtom|kdecrtom)+a.def.isframe,a.def)
		fi
	else
dounary:
		do_unaryref(a,(p.tag in [jincrload,jloadincr]|kincrptr|kdecrptr))
	fi
end

proc do_new(unit p)=
	int n
	unit q

	n:=p.nparams
	if n<1 or n>3 then gerror("new args") fi

	q:=p.a

	genpc(kpushvoid)
	to n do
		evalunit(q)
		q:=q.nextunit
	od
	to 3-n do
		genpc(kpushvoid)
	od

	callhostfn(h_new)

end

function checkblockreturn(unit p)int=
!p should be a block unit
!check that the last statement is a return; return 1/0 for return/not return
!just allow or check for return/if/longif for now
	ref unitrec q,r

	if p=nil then return 0 fi
	if p.tag<>jblock then gerror("CBR?") fi

	q:=p.a
	if q=nil then return 0 fi		!empty block

	while r:=q.nextunit do			!get q=last stmt in block
		q:=r
	od

	case q.tag
	when jreturn then			!that's an easy one...
		return 1

	when jif then
		return checkblockreturn(q.b) and checkblockreturn(q.b.nextunit)		!all branches must have a return
	esac
	return 0
end

proc do_callhost(unit p, a, int res)=
	int index:=p.index
	int isfunc:=hostisfn[index]
	int nargs, nparams, fparams
	[10]unit plist
	unit q


	if res and not isfunc then
		gerror("Host proc not a function")
	fi

	if isfunc then
		genpc(kpushvoid)
	fi

	nargs:=0
	q:=a

	while q do
		if nargs>plist.upb then
			gerror("Too many host args")
		fi
		plist[++nargs]:=q

		q:=q.nextunit
	od

!	if index=h_allparams and a=nil then
!		nparams:=1
!	else
		nparams:=nargs
!	fi

	if nparams=0 and hostlvset[index] then
		gerror("LV hostfn: needs 1+ params")
	fi
	fparams:=hostnparams[index]
	if nparams>fparams then
		gerror("Hostfn too many params")
	fi

	to fparams-nparams do
		genpc(kpushvoid)
	od

!Finally, push all the params, which need to be done in reverse order
	for i:=nparams downto 1 do
		if i=1 and hostlvset[index] then
			evalref(plist[i])
!		elsif i=1 and index=h_allparams and nargs=0 then
!			genpc_name(kpushmref,stcurrproc)
		else
			evalunit(plist[i])
		fi
	od  

	callhostfn(index,res)
end

proc callhostfn(int fnindex,calledasfn=0)=
!assume caller has verified that fn is a function when calledasfn is true
!called should have pushed retval as needed, and <aparams> params

	genpc_int(kcallhost,fnindex)
end

proc genfree(int n)=
	genpc_int(kunshare,n)
end

proc do_return(unit p,a)=
	if a then
		evalunit(a)
	elsif currfunction=2 then
		gerror("function needs return value")
	fi

	genjumpl(retindex)
end

proc do_multassign(unit a,b, int deepcopy, res)=
	unit p,q
	[100]unit plist
	int n

	p:=a.a
	q:=b.a
	n:=0

	while p do
		if q=nil then gerror("Too few RHS elems") fi
		evalunit(q)
		if n>=plist.len then gerror("Too many elems") fi
		plist[++n]:=p

		p:=p.nextunit
		q:=q.nextunit
	od

	if q then gerror("Too few LHS elems") fi

	for i:=n downto 1 do
		if deepcopy then
			genpc(kcopy)
		fi

		do_store(plist[i])
	od
end

proc do_store(unit a,int res=0)=
!store stack value to a
	symbol d
	unit p
	[100]unit plist
	int n

	if res and a.tag<>jname then
		genpc(kdupl)
	fi

	case a.tag
	when jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			if res then genpc(kdupl) fi
			genpc_name(kpushf,d)
			genpc(kpopptr)
		elsif res then
			genpc_name(kstorem+d.isframe,d)
		elsif d.nameid=dllvarid then
			genpc_name(kpopx,d)

		else
			genpc_name(kpopm+d.isframe,d)
		fi

	when jdot then
		evalunit(a.a)
		genpc_name(kpopdot,a.b.def)

	when jindex then
		do_bin(a.a, a.b, kpopindex)
	when jdotindex then

		evalref(a.a)
		evalunit(a.b)
		genpc(kpopdotindex)
	when jptr then
		evalunit(a.a)
		genpc(kpopptr)

	when jkeyindex then
		do_bin(a.a, a.b, kpopkeyindex)

	when jmakelist then			!assign to multiple destinations
		n:=0
		p:=a.a
		while p do
			if n>=plist.len then gerror("Too many elems") fi
			plist[++n]:=p
			p:=p.nextunit
		od
		if n=0 then gerror("Empty lhs list") fi

		genpc_int(kexpand,n)
!		for i:=n downto 1 do
		for i:=1 to n do
			do_store(plist[i])
		od

	when jif then
		evalref(a)
		genpc(kpopptr)

	else
		gerror_s("Can't store to this unit yet:",jtagnames[a.tag], a)
	esac
end

function getconstvalue(unit p)int =
	if p and p.tag=jintconst then
		return p.value
	fi
	gerror("gcv Not const")
	return 0
end

proc do_convert(unit pconv)=
!apply type-conversion t on expression p

!also do constructors
	int n,elemmode,i,lowerx,lbound,m,mbase,nfields
	[maxunits]unit plist
	unit p

	m:=pconv.mode
	p:=pconv.a
	mbase:=ttbasetype[m]

!p.length is no. of elements, but it not used here(unitstoarray will count
!anyway). But a value of -1 (rather than 1) means a trailing comma was used.

	if p.tag<>jmakelist  OR MBASE=TREFPACK then		!assume regular type conversion
			if p.tag=jmakelist then
				deleteunit(p,p.a)
			fi
			evalunit(p)
			genpc_int(kconvert,m)
			return
!		fi
	fi

!a is a usertype
	n:=unitstoarray(p.a,&plist,maxunits)

	if n and plist[1].tag=jkeyvalue then
		case mbase
		when trecord, tstruct then
			do_makerecordkv(m,n,plist)
		else
			gerror("key:value not allowed")
		esac
		return
	fi

	for i:=1 to n do		!any elements need to be pushed
		evalunit(plist[i])
	od

	case mbase
	when trecord, tstruct then
		nfields:=ttlength[m]
		if n then
			checkelems(n,nfields,p)
		else				!allow 0 fields; use defaults of 0
			to nfields do
				genpc_int(kpushci,0)
			od
			n:=nfields
		fi
		genpc_int2((mbase=trecord|kmakerecord|kmakestruct),n,m)

	when tlist then		!probably just a list prefix used
		lowerx:=p.lower
		genpc_int2(kmakelist,n,lowerx)

	when tarray then
		genpc_int4(kmakearray,p.lower,n,tarray,p.elemtype)

!	when tvector then
	when tvector then
		elemmode:=tttarget[m]
		lowerx:=ttlower[m]

		checkelems(n,ttlength[m],p)
		genpc_int4(kmakearray,lowerx,n,m,elemmode)

	when tbits then
		if m=tbits then			!not user-defined
			genpc_int4(kmakebits,p.lower,n,tbits,(p.elemtype=tvoid|tu1|p.elemtype))
		else
			gerror("user-define bit array not ready")
		fi

	when tset then
		genpc_int(kmakeset,n)

	else
		gerror_s("Convert list",strmode(mbase))
	esac
end

!proc do_case(unit p,pindex,pwhenthen,int res) =
proc checkelems(int n,length, unit p)=
	if n<length then
		gerror("Too few elements")
	elsif n>length then
		gerror("Too many elements")
	fi
end

proc do_switch(unit p,pindex,pwhenthen, int res) =
	int minlab,maxlab,x,y,i,n
	unit w,wt, pelse

	pelse:=pindex.nextunit
!first a first scan over the when expressions; work out range and whether simple or complex
	minlab:=1000000
	maxlab:=-1000000			!highest index seen

	n:=0				!no. different values
	wt:=pwhenthen

	while wt do
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x:=getconstvalue(w.a)
				y:=getconstvalue(w.b)
dorange:
				for i:=x to y do
					minlab :=min(minlab,i)
					maxlab :=max(maxlab,i)
				od
			when jintconst then
				x:=y:=w.value
				goto dorange
			when jtypeconst then
				x:=y:=w.mode
				goto dorange
			else
				gerror_s("Switch when2: not const",strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if maxlab-minlab<=maxswitchrange then
		do_simpleswitch(p,pindex,pwhenthen,pelse, minlab,maxlab, res)
		return
	fi

	gerror("COMPLEX SWITCH/NOT COMPLETE")
end

proc do_simpleswitch(unit p,pindex,pwhenthen,pelse, int a,b, res) =
!a..b is the range of values of the switch which have been checked to
!be in range in terms of span. But the actual values can be anything.
!For example, 1000000 to 10000250 is valid. So, an offset needs to be
!used to bring the range down to 0 to 250

	unit w,wt,q
	int loopsw,n,offset,x,y,x0,i,labstmt,elselab
	[1..maxswitchrange+1]ref int labels
	int lab_a,lab_b,lab_c,lab_d

	loopsw:=p.tag=jdoswitch

	n:=b-a+1
	offset:=a-1		!a..b becomes 1..n

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(&lab_a,&lab_a,&lab_d)
	else
		lab_d:=createfwdlabel()
	fi
	elselab:=createfwdlabel()

	evalunit(pindex)

	genpc_int2(kswitch,n,a)

	for i:=1 to n do
		genpc_lab(kjumplabel,0)
		labels[i]:=pcllast+1		!for now, store destination code index
	od

	genpc_lab(kjumplabel,0)			!else label
	labels[n+1]:=pcllast+1

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x0:=getconstvalue(w.a)
				y:=getconstvalue(w.b)

			when jintconst then
				x0:=y:=w.value
			when jtypeconst then
				x0:=y:=w.mode
			esac

			for x:=x0 to y do
				i:=x-offset
				if labels[i]^ then			!should have been zero
					cpl x,char(x)
					gerror("Dupl switch value")
				fi
				labels[i]^:=labstmt
			od
			w:=w.nextunit
		od

		evalunit(wt.b, res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		wt:=wt.nextunit
	od

!fill in zero entries with else
	definefwdlabel(elselab)
	if pelse then		!do else part
		evalunit(pelse,res)
	fi	

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi

	for i:=1 to n do
		if labels[i]^=0 then
			labels[i]^:=elselab
		fi
	od
	labels[n+1]^:=elselab
end

proc do_makerecordkv(int m,nkeyvals, []unit &kvlist)=
	unit p
	[maxunits]unit plist
	int nfields, index
	symbol d:=ttnamedef[m], e, f, k

	e:=d.deflist
	nfields:=0

	while e,e:=e.nextdef do
		if e.nameid in [fieldid,structfieldid] and e.atfield=nil then
			++nfields
			plist[nfields]:=nil
		fi
	od

	for i to nkeyvals do
		k:=kvlist[i].a.def
		p:=kvlist[i].b

		e:=d.deflist
		f:=nil
		while e,e:=e.nextdef do
			if e.nameid in [fieldid,structfieldid] and e.firstdupl=k then
				f:=e
				exit
			fi
		od

		if not f then
			gerror_s("Can't find field:",k.name)
		fi
		index:=f.index
		if plist[index] then
			gerror_s("Dupl key:",k.name)
		fi
		plist[index]:=p
	od

	for i to nfields do
		if plist[i] then
			evalunit(plist[i])
		else
			genpc_int(kpushci,0)
		fi
	od

	genpc_int2(kmakerecord,nfields,m)
end

proc do_idiv(unit p,a,b)=
	int n

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		genpc_int(kpushci,n)
		genpc(kshr)
	else
		evalunit(b)
		genpc(kidiv)
	fi
end

proc do_irem(unit p,a,b)=
	int n
	word m

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		m:=inot(0xFFFF'FFFF'FFFF'FFFF << n)
		genpc_int(kpushci,M)
		genpc(kiand)
	else
		evalunit(b)
		genpc(kirem)
	fi
end

proc do_map(unit p,popcode,x)=
	evalunit(x)
	if x.nextunit then
		evalunit(x.nextunit)
	fi
	evalunit(popcode)
	genpc((x.nextunit|kmapss|kmaps))

	int lab:=createfwdlabel()
	genpc_lab(kjump,lab)		!dummy jump to be moved to runtime-generated code
	genpc(knop)					!stop jump being optimised out
	definefwdlabel(lab)
end

proc pushstring(ichar s, int length)=
	genpc(kpushcs)
	ref stringrec ps:=pcm_alloc(stringrec.bytes)
	ps.svalue:=s
	ps.length:=length
	genopnd_int(cast(ps))
end

