!const doretcheck=1
const doretcheck=0

macro steppc = ++pc
macro skip1 = pc+:=2
macro skip2 = pc+:=3

macro zz = sp
macro yy = sp-1

macro save = pcptr:=pc
!macro save = (pcptr:=pc; sptr:=sp)

macro pclerror(x) = (pcptr:=pc; pcerror(x))
macro pclerror2(x,y) = (pcptr:=pc; pcerror(x,y))
macro pclustype(x,t) = (pcptr:=pc; pcustype(x,t))
macro pclmxtypes(x,t,u) = (pcptr:=pc; pcmxtypes(x,t,u))

macro copyvar(x, y) = x^:=y^
macro copyvarv(x, y) = x:=y^
macro copyvar_v(x, y) = x^:=y
!
!macro copyvar(x, y) = (x.dummy:=y.dummy; x.value:=y.value)
!macro copyvarv(x, y) = (x.dummy:=y.dummy; x.value:=y.value)
!macro copyvar_v(x, y) = (x.dummy:=y.dummy; x.value:=y.value)

!macro jumpnext = goto jumptable[pc.opcode]
macro jumpnext = goto pc.labaddr

byte getjt

global ref[0:]ref label jumptable

global proc disploop =
	pcl pc
	variant sp
	ref byte fp

	variant x
	variant y
	variant z	
		int n
		int index
		variant dest
		variant px
		symbol d
		int nloc
		pcl pz
		object pp
		object q

	int xt,yt, res, lower, upper, moduleno, offset
	variant newsp
	symbol e

	varrec vx

	static [0:]ref label localjumptable = (
		jnop, jskip, jprocdef, jprocent, jprocend, jendmod, jcomment, jpushm, jpushf, jpushmref, jpushfref, jpopm, jpopf, jpushci, jpushvoid, jpushnil, jpushcr, jpushcs, jpushtype, jpushopc, jpushsym, jpushptr, jpopptr, jzpopm, jzpopf, jdupl, jcopy, jswap, jconvrefp, jjump, jjumpptr, jjumpt, jjumpf, jjumpeq, jjumpne, jjumplt, jjumple, jjumpge, jjumpgt, jwheneq, jwhenne, jjumplab, jswitch, jtom, jtof, jformci, jforfci, jformm, jforff, jcallproc, jcallptr, jretproc, jretfn, jmodcall, jmodret, jcalldll, jcallhost, junshare, jaddsp, jstop, jmakelist, jmakevrec, jmakeax, jmakebits, jmaketrec, jmakeset, jmakerang, jmakedict, jmakedec, jincrptr, jincrtom, jincrtof, jloadincr, jincrload, jneg, jabs, jnotl, jinot, jistruel, jasc, jchr, jsqr, jmaths, jmaths2, junaryto, jnotlto, jlen, jlwb, jupb, jbounds, jbytesize, jtype, jdictsize, jisfound, jminval, jmaxval, jistype, jisvoid, jconvert, jtypepun, jadd, jsub, jmul, jdiv, jidiv, jirem, jidivrem, jiand, jior, jixor, jshl, jshr, jin, jinx, jcmp, jmin, jmax, jconcat, jappend, jsame, jpower, jbinto, jandlto, jorlto, jconcatto, jappendto, jdot, jdot1, jpopdot, jpopdot1, jdotref, jindex, jpopix, jindexref, jkeyindex, jpopkeyix, jkeyixref, jdotix, jpopdotix, jdotixref, jexpand, jpushtry, jraise, jmap, jpushfff, jpushff, jpushmm, jpushfm, jpushmf, jpushmci, jpushfci, jmoveff, jmovemm, jmovefm, jmovemf, jzmoveff, jmovefci, jmovemci, jzmovefci, jpushv2, jpushv3, jjmpeqfci, jjmpnefci, jjmpltfci, jjmplefci, jjmpgefci, jjmpgtfci, jjmpeqff, jjmpneff, jjmpltff, jjmpleff, jjmpgeff, jjmpgtff, jaddfci, jsubfci, jaddff, jsubff, jaddci, jindexmf, jindexff, jswitchf, jpushptrf, jpushipm, jpushipf, jpopipm, jpopipf, jupbm, jupbf, jlenf, jstoref, jwheneqci, jwhenneci, jlastpcl)

	if getjt then
		jumptable:=&localjumptable
		return
	fi

	sp:=sptr
	pc:=pcptr
	fp:=frameptr

	jumpnext

jnop: ! simple nop
	steppc
	jumpnext

jskip: ! ignore on pcl listing
	unimpl
	steppc
	jumpnext

jprocdef: ! 
	unimpl
	steppc
	jumpnext

jprocent: ! n=number of locals; 
	to pc.n do
		++sp
		sp.tagx:=tvoid
	od
	steppc
	jumpnext

jprocend: 
	unimpl
	steppc
	jumpnext

jendmod: 
	unimpl
	steppc
	jumpnext

jcomment: 
	steppc
	jumpnext

jpushm: ! Push v
	++sp
	copyvar(sp, pc.varptr)
	var_share(sp)
	steppc
	jumpnext

jpushf: ! Push v
	++sp
	x:=cast(fp+pc.offset)
	copyvar(sp, x)

	var_share(sp)
	steppc
	jumpnext

jpushmref: ! push &v
	++sp
	sp.tagx:=trefvar
	sp.varptr:=pc.varptr
	steppc
	jumpnext

jpushfref: ! push &v
	++sp
	sp.tagx:=trefvar
	sp.varptr:=cast(fp+pc.offset)
	steppc
	jumpnext

jpopm: ! v:= Z
	x:=pc.varptr
	var_unshare(x)
	copyvar(x, sp)
	--sp
	steppc
	jumpnext

jpopf: ! v:= Z
	x:=cast(fp+pc.offset)
	var_unshare(x)
	copyvar(x, sp)
	--sp
	steppc
	jumpnext

jpushci: ! Push i
	++sp
	sp.tagx:=tint
	sp.value:=pc.value
	steppc
jpushcix:
	jumpnext

jpushvoid: ! Push void 
	++sp
	sp.tagx:=tvoid
	steppc
	jumpnext

jpushnil: ! Push nil (ref void)
	++sp
	sp.tagx:=trefpack
	sp.elemtag:=tvoid
	sp.ptr:=nil
	steppc
	jumpnext

jpushcr: ! Push r
	++sp
	sp.tagx:=treal
	sp.xvalue:=pc.xvalue
	steppc
	jumpnext

jpushcs: ! Push constant string object
	++sp
	sp.tagx:=tstring ior hasrefmask
	sp.objptr:=pc.objptr
	++sp.objptr.refcount
	steppc
	jumpnext

jpushtype: ! Push type constant
	++sp
	sp.tagx:=ttype
	sp.value:=pc.typecode
	steppc
	jumpnext

jpushopc: ! Push operator constant
	++sp
	sp.tagx:=toperator
	sp.value:=pc.pclop
	steppc
	jumpnext

jpushsym: ! Push symbol reference
	++sp
	sp.tagx:=tsymbol
	sp.def:=pc.def

	steppc
	jumpnext

jpushptr: ! Z':= Z^
	x:=sp
jpushptr2:
	case x.tag
	when trefvar then
		sp^:=x.varptr^

	when trefpack then
		case x.elemtag
		when tu8 then
			sp.tagx:=tint
			sp.value:=x.ptr^
			goto refpackend
		else
			save
			var_loadpacked(x.ptr, x.elemtag, sp, nil)
		esac

	when trefbit then
		save
		var_loadbit(x.ptr, x.bitoffset, x.elemtag, x.bitlength, sp)

	else
		pclustype("Pushptr",x)
	esac

	var_share(sp)
refpackend:
	steppc
	jumpnext

jpushptrf:
	x:=cast(fp+pc.offset)
	++sp
	++pc
	goto jpushptr2

jpopptr: ! Z^:= Y
	y:=sp--
	x:=sp--

	case y.tag
	when trefvar then
		var_unshare(y.varptr)
		y.varptr^:=x^
	when trefpack then
		save
		var_storepacked(y.ptr,x, y.elemtag)
	when trefbit then
		save
		var_storebit(y.ptr, y.bitoffset, x, y.elemtag, y.bitlength)

	else
		pclustype("Popptr",y)
	esac

	steppc
	jumpnext

jzpopm: ! v:= Z; don't free v first
	copyvar(pc.varptr, sp)
	--sp
	steppc
	jumpnext

jzpopf: ! v:= Z; don't free v first
	x:=cast(fp+pc.offset)
	copyvar(x, sp)
	--sp
	steppc
	jumpnext

jdupl: ! (Z',Y'):= (share(Z), Z)
	++sp
!		sp^:=(sp-1)^
	copyvar(sp, sp-1)
	var_share(sp)
	steppc
	jumpnext

jcopy: ! Z':= deepcopy(Z)
	if sp.hasref then
		copyvarv(vx, sp)
		save
		var_duplu(sp)
		var_unshareu(&vx)
	fi
	steppc
	jumpnext

jswap: ! swap(Z^, Y^)
	x:=sp--
	y:=sp--

	if x.tag=y.tag=trefvar then
		copyvar(&vx, x.varptr)
		copyvar(x.varptr, y.varptr)
		copyvar(y.varptr, &vx)
	else
		save
		k_swap(x,y)
	fi
	steppc
	jumpnext

jconvrefp: ! Change ref in Z to refpacked
	save
	k_convrefpack(sp)
	steppc
	jumpnext

jjump:
	pc:=pc.labelref
	jumpnext

jjumpptr: ! Jump to Z
	unimpl
	steppc
	jumpnext

jjumpt: ! Jump to L when Z is true
	x:=sp--

	if x.tag=tint then
		if x.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if var_istruel(x) then
			pc:=pc.labelref
		else
			steppc
		fi
		var_unshare(x)
	fi
	jumpnext

jjumpf: ! Jump to L when Z is false
	x:=sp--

	if x.tag=tint then
		if not x.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if not var_istruel(x) then
			pc:=pc.labelref
		else
			steppc
		fi
		var_unshare(x)
	fi
	jumpnext

jjumpeq: ! Jump to L when Y = Z
	y:=sp--
	x:=sp--

	if x.tag=y.tag=tint then
		if x.value=y.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if var_equal(x, y) then
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jjumpne: ! Jump to L when Y<>= Z
	y:=sp--
	x:=sp--

	if x.tag=y.tag=tint then
		if x.value<>y.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if not var_equal(x, y) then
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jjumplt: ! Jump to L when Y < Z
	y:=sp--
	x:=sp--

	if x.tag=y.tag=tint then
		if x.value<y.value then
			pc:=pc.labelref
		else
			steppc
		fi
	elsif x.tag=y.tag=treal then
		if x.xvalue<y.xvalue then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if var_compare(x,y)<0 then
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jjumple: ! Jump to L when Y <= Z
	y:=sp--
	x:=sp--

	if x.tag=y.tag=tint then
		if x.value<=y.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if var_compare(x,y)<=0 then
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jjumpge: ! Jump to L when Y >= Z
	y:=sp--
	x:=sp--

	if x.tag=y.tag=tint then
		if x.value>=y.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if var_compare(x,y)>=0 then
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jjumpgt: ! Jump to L when Y > Z
	y:=sp--
	x:=sp--

	if x.tag=y.tag=tint then
		if x.value>y.value then
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		if var_compare(x,y)>0 then
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jjmpeqfci: ! Jump to L when B = C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	if x.value=(pc+1).value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpnefci: ! Jump to L when B <> C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	if x.value<>(pc+1).value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpltfci: ! Jump to L when B < C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	if x.value<(pc+1).value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmplefci: ! Jump to L when B <= C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	if x.value<=(pc+1).value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpgefci: ! Jump to L when B >= C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	if x.value>=(pc+1).value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpgtfci: ! Jump to L when B > C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	if x.value>(pc+1).value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpeqff: ! Jump to L when B = C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	y:=cast(fp+(pc+1).offset)
	if x.value=y.value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpneff: ! Jump to L when B <> C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	y:=cast(fp+(pc+1).offset)
	if x.value<>y.value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpltff: ! Jump to L when B < C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	y:=cast(fp+(pc+1).offset)
	if x.value<y.value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpleff: ! Jump to L when B <= C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	y:=cast(fp+(pc+1).offset)
	if x.value<=y.value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpgeff: ! Jump to L when B >= C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	y:=cast(fp+(pc+1).offset)
	if x.value>=y.value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jjmpgtff: ! Jump to L when B > C
	x:=cast(fp+pc.offset)
	if x.tag<>tint then goto jpushf fi
	y:=cast(fp+(pc+1).offset)
	if x.value>y.value then
		pc:=(pc+2).labelref
	else
		skip2
	fi
	jumpnext

jwheneq: ! Y = Z:  pop both, jump to L
					  ! Y <> Z: pop Z only; don't jump
	y:=sp--
	x:=sp

	if x.tag=y.tag=tint then
		if x.value=y.value then
			--sp
			pc:=pc.labelref
		else
			steppc
		fi
	else
		save
		res:=k_when(x, y)
		var_unshare(y)
		if res then
			var_unshare(x)
			--sp
			pc:=pc.labelref
		else
			steppc
		fi
	fi
	jumpnext

jwhenne: ! Y <> Z:  pop Z only, jump to L
					  ! Y = Z:   pop both, step to next
	y:=sp--
	x:=sp

	if x.tag=y.tag=tint then
		if x.value<>y.value then
			pc:=pc.labelref
		else
			--sp
			steppc
		fi
	else
		save
		res:=k_when(x, y)
		var_unshare(y)
		if not res then
			pc:=pc.labelref
		else
			var_unshare(x)
			--sp
			steppc
		fi
	fi
	jumpnext

jjumplab: ! Jumptable entry
	unimpl
	steppc
	jumpnext

jswitch: ! Jumptable has y-x+1 entries
	lower:=pc.x
	n:=pc.y-lower+1

	case sp.tag
	when tint,ttype then
	else
		pclerror2("switch not int",ttname[sp.tag])
	esac

	index:=sp.value-lower		!now 0-based index
!
	--sp

	if u64(index)>=u64(n) then			!out of range
		pc:=(pc+n+1).labelref
	else							!in range
		pc:=(pc+index+1).labelref
	fi
	jumpnext

jtom: ! --v; jump to l when v<>0 in next op
	x:=(pc+1).varptr
	doto

jtof: ! --v; jump to l when v<>0 in next op
	x:=cast(fp+(pc+1).offset)
doto:
	if --x.value then
		pc:=pc.labelref
	else
		skip1
	fi
	jumpnext

jformci: ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
	x:=(pc+1).varptr
	doforfci

jforfci: ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
	x:=cast(fp+(pc+1).offset)
doforfci:
	++x.value
	if x.value<=(pc+2).value then
		pc:=pc.labelref
	else
		skip2
	fi
	jumpnext

jformm: ! ++v; jump to l when v<=v in next 2 ops
	x:=(pc+1).varptr
	y:=(pc+2).varptr
	doforff

jforff: ! ++v; jump to l when v<=v in next 2 ops
	x:=cast(fp+(pc+1).offset)
	y:=cast(fp+(pc+2).offset)
doforff:
	++x.value

	if x.value<=y.value then
		pc:=pc.labelref
	else
		skip2
	fi
	jumpnext

jcallproc: ! Call &A; n is no. args
	const countinterval=100
	static int count=countinterval

	if --count=0 then
		count:=countinterval
		os_peek()
	fi

	if sp>=stacklimit then
		pclerror("Stack Overflow")
	fi

	++sp
	sp.tagx:=tretaddr
	sp.retaddr:= pc+1

	sp.frameptr_low:= u64(fp)
	fp:=cast(sp)

	pc:=pc.labelref
	jumpnext

jcallptr: ! Call X^; n is no. of params supplied; x is stack adjust
	if sp.tag<>tsymbol then
		pclerror("Probably undefined function")
	fi

	d:=sp.def
	if d.nameid=linkid then d:=d.alias fi

	if d.nparams<>pc.n then
		pclerror2("Callptr: wrong # params; need:",strint(d.nparams))
	fi

	sp.tagx:=tretaddr
	sp.retaddr:= pc+1

	sp.frameptr_low:= word(fp)
	fp:=cast(sp)

	pc:=cast(d.labelref)
	jumpnext

jretproc:
doretproc:
	to pc.x do
		var_unshare(sp)
		--sp
	od

	n:=pc.n
	pc:=sp.retaddr
	fp:= cast(u64(fp) iand (0xFFFF'FFFF'0000'0000) ior sp.frameptr_low)
	--sp

	to n do
		var_unshare(sp)
		--sp
	od
	jumpnext

jretfn:
	x:=variant(fp+pc.y)
	copyvar(x, sp)		!transfer reference
	--sp
	doretproc
	jumpnext

jmodcall: ! 
	d:=pc.def
	moduleno:=d.moduleno

	++sp
	sp.tagx:=tretaddr
	sp.retaddr:= pc+1
	pc:=modules[moduleno].pcstart
	jumpnext

jmodret: ! 
	pc:=sp.retaddr
	jumpnext

jcalldll: ! Call dll function d (sysmbol); n=nargs
	n:=pc.n
	save
	SPTR:=SP

	calldll(pc.def, sp-n+1, sp-n, n)
	sp-:=n

	steppc
	jumpnext

jcallhost: ! Call Q host function h (Host index)
	save
	sp:=callhostfunction(pc.hostindex, sp)
	steppc
	jumpnext

junshare: ! Unshare and pop A var values on stack
	to pc.n do
		var_unshare(sp)
		--sp
	od
	steppc
	jumpnext

jstop: ! Stop program with stopcode Z; n=1 to stop runproc instead
	sptr:=sp
	return

jmakelist: ! x items on stack; make list with lwb y
	save
	sp:=k_makelist(sp, pc.y, pc.x)
	steppc
	jumpnext

jmakevrec: ! x items on stack; make record of type u
	n:=pc.x
	x:=sp-pc.x+1				!start of data

	save
	var_make_record(x, x, pc.x, pc.usertag)
	sp:=x
	sp.objptr.mutable:=0
	steppc
	jumpnext

jmakeax: ! x items on stack; make array with lwb y, type u and elemtype v
	unimpl
	steppc
	jumpnext

jmakebits: ! x items on stack; make bits with lwb y, type u and elemtype v
	unimpl
	steppc
	jumpnext

jmaketrec: ! x items on stack; make struct with type u
	n:=pc.x
	x:=sp-n+1				!start of data

	save
	var_make_struct(x, x, n, pc.usertag)
	sp:=x
	sp.objptr.mutable:=0
	steppc
	jumpnext

jmakeset: ! x items on stack; make set
	n:=pc.x

	x:=sp-n+1			!start of data

	save
	var_make_set(x, x, n)
	sp:=x
	sp.objptr.mutable:=0

	steppc
	jumpnext

jmakerang: ! 2 items on stack; make range
	y:=sp--
	x:=sp

	unless x.tag=y.tag=tint then
		pclerror("makerange/not int")
	end

	sp.tagx:=trange
	lower:=x.value
	upper:=y.value

	if lower not in -(2**48)..2**48-1 then
		pclerror("Range lwb bounds")
	end

	sp.range_upper:=upper
	sp.range_lower:=lower

	steppc
	jumpnext

jmakedict: ! x*2 items on stack (x key:val items); make dict
	n:=pc.x
	x:=sp-n*2+1			!start of data

	save
	var_make_dict(x, x, n)
	sp:=x
	steppc
	jumpnext

jmakedec: ! Turn string on stack to decimal number
	copyvarv(vx, sp)

	if vx.tag<>tstring then pclerror("Not str") fi
	pp:=vx.objptr
	if pp.length=0 then pclerror("Null str") fi

	save
	var_make_dec_str(pp.strptr, pp.length, sp)

	var_unshare(&vx)

	steppc
	jumpnext

jincrptr: ! Z^ +:= x
	save
	k_incrptr(sp, pc.x)
	--sp
	steppc
	jumpnext

jincrtom: ! v +:= x
	x:=pc.varptr
	doincrto

jincrtof: ! v +:= x
	x:=cast(fp+pc.offset)
doincrto:
	case x.tag
	when tint then
		x.value+:=pc.x
	when trefvar then
		x.varptr+:=pc.x
	when trefpack then
		x.ptr+:=ttsize[x.elemtag]*pc.x
	when treal then
		x.xvalue+:=pc.x
	else
		pclustype("incrto",x)
	end

	steppc
jincrtofx:
	jumpnext

jloadincr: ! T:= Z^; Z^ +:= x; Z':= T
	copyvarv(vx, sp)
	save
	var_loadptr(sp,sp)
	++sp
	copyvar_v(sp, vx)
	k_incrptr(sp, pc.x)
	--sp
	steppc
	jumpnext

jincrload: ! Z^ +:= x; Z':= Z^
	copyvarv(vx, sp)
	save
	k_incrptr(sp, pc.x)
	--sp
	var_loadptr(&vx, ++sp)
	steppc
	jumpnext

jneg: ! Z':= -Z
	copyvarv(vx, sp)
	save
	var_neg(sp)
	var_unshare(&vx)
	steppc
	jumpnext

jabs: ! Z':= abs Z
	copyvarv(vx, sp)

	save
	var_abs(sp)
	var_unshare(&vx)

	steppc
	jumpnext

jnotl: ! Z':= not Z
	save
	res:=not var_istruel(sp)
	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=res
	steppc
	jumpnext

jinot: ! Z':= inot Z
	if sp.tag=tint then
		sp.value:=inot sp.value
	else
		copyvarv(vx, sp)
		save
		var_inot(sp)
		var_unshare(&vx)
	fi

	steppc
	jumpnext

jistruel: ! Z':= istrue Z
	save
	n:=var_istruel(sp)
	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=n

	steppc
	jumpnext

jasc: ! Z':= asc(Z)
	case sp.tag
	when tstring then
		if sp.objptr.length then
			n:=sp.objptr.strptr^
		else
			n:=0
		fi
		var_unshareu(sp)
		sp.tagx:=tint
		sp.value:=n
	else
		pcustype("ASC",sp)
	esac
	steppc
	jumpnext

jchr: ! Z':= chr(Z)
	if sp.tag=tint then
		save
		var_makechar(sp.value, sp)
	else
		pclustype("CHR",sp)
	fi
	steppc
	jumpnext

jsqr: ! Z':= op(Z)
	case sp.tag
	when tint then
		sp.value:=sqr(sp.value)
	when treal then
		sp.xvalue:=sqr(sp.xvalue)
	else
		pclustype("sqr", sp)
	esac
	steppc
	jumpnext

jmaths: ! Z':= op(Z)
	save
	k_maths(sp, pc.mathscode)
	steppc
	jumpnext

jmaths2: ! Z':= op(Y, Z)
	unimpl
	steppc
	jumpnext

junaryto: ! Z^ op:= Z
	unimpl
	steppc
	jumpnext

jnotlto: ! Z^ not:= Z
	unimpl
	steppc
	jumpnext

jlen: ! Z':= Z.len
	save
	k_len(sp)
	steppc
	jumpnext

jlwb: ! Z':= Z.lwb
	save
	k_lwb(sp)
	steppc
	jumpnext

jupb: ! Z':= Z.upb
	save
	k_upb(sp)
	steppc
	jumpnext

jbounds: ! Z':= Z.bounds; n=1: one range value; n=2: two dims
	save
	k_bounds(sp, lower, upper)

	if pc.n=2 then				!push as 2 value
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=lower
		++sp
		sp.tagx:=tint
		sp.value:=upper

	else						!push as 1 range value
		var_unshare(sp)
		sp.tagx:=trange
		sp.range_lower:=lower
		sp.range_upper:=upper
	fi

	steppc
	jumpnext

jbytesize: ! Z':= Z.bytesize
	save
	res:=k_bytesize(sp)
	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=res
	steppc
	jumpnext

jtype: ! Z':= n=0/1/2 = basetype/tag/elemtype
	save
	n:=k_type(sp, pc.n)
	var_unshare(sp)
	sp.tagx:=ttype
	sp.value:=n

	steppc
	jumpnext

jdictsize: ! Z':= Z.dictsize
	unimpl
	steppc
	jumpnext

jisfound: ! Z':= Z.isfound
	if sp.tag<>tint then pclerror("isfound") fi
	sp.value:=sp.value<>i64.min
	steppc
	jumpnext

jminval: ! Z':= Z.minvalue
	unimpl
	steppc
	jumpnext

jmaxval: ! Z':= Z.maxvalue
	unimpl
	steppc
	jumpnext

jistype: ! Z':= Z.type/etc = t
	n:=0
	if pc.typecode=trefvar then
		if sp.tag in [trefvar, trefpack, trefbit] then n:=1 fi
	else
		if pc.typecode=sp.tag then n:=1 fi
	fi
	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=n
	steppc
	jumpnext

jisvoid: ! Z':= Z.isvoid (n=0) or not Z.isdef (n=1)
	res:=sp.tag=tvoid
	var_unshare(sp)
	sp.tagx:=tint
	sp.value:=res ixor pc.n
	steppc
	jumpnext

jconvert: ! Z':= t(Z)
	if sp.tag<>pc.usertag then
!			vx:=sp^
		copyvarv(vx, sp)
		save
		var_convert(&vx, pc.usertag, sp)
		var_unshare(&vx)
	fi

	steppc
	jumpnext

jtypepun: ! Z':= t@(Z)
	unimpl
	steppc
	jumpnext

jadd: ! Z':= Y + Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value+:=y.value
	elsif sp.tag=y.tag=treal then
		sp.xvalue+:=y.xvalue
	else
!			vx:=sp^
		copyvarv(vx, sp)

		save
		var_add(sp, y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jsub: ! Z':= Y - Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value-:=y.value
	elsif sp.tag=y.tag=treal then
		sp.xvalue-:=y.xvalue
	else
!			vx:=sp^
		copyvarv(vx, sp)

		save
		var_sub(sp, y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jmul: ! Z':= Y * Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value*:=y.value
	elsif sp.tag=y.tag=treal then
		sp.xvalue*:=y.xvalue
	else
		copyvarv(vx, sp)

		save
		var_mul(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi

	steppc
	jumpnext

jdiv: ! Z':= Y / Z
	y:=sp--
	copyvarv(vx, sp)

	if sp.tag=y.tag=treal then
		sp.xvalue/:=y.xvalue
	else	
		save
		var_div(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi

	steppc
	jumpnext

jidiv: ! Z':= Y % Z
	y:=sp--
	copyvarv(vx, sp)

	if sp.tag=y.tag=tint then
		sp.value/:=y.value
	else	
		save
		var_idiv(sp, y)

		var_unshare(&vx)
		var_unshare(y)
	fi


	steppc
	jumpnext

jirem: ! Z':= Y rem Z
	y:=sp--
	copyvarv(vx, sp)

	save
	var_irem(sp,y)

	var_unshare(&vx)
	var_unshare(y)

	steppc
	jumpnext

jidivrem: ! (Y', Z'):= Y divrem Z
	unimpl
	steppc
	jumpnext

jiand: ! Z':= Y iand Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value iand:=y.value
	else
!			vx:=sp^
		copyvarv(vx, sp)
		save
		var_iand(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jior: ! Z':= Y ior Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value ior:=y.value
	else
!			vx:=sp^
		copyvarv(vx, sp)
		save
		var_ior(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jixor: ! Z':= Y ixor Z
	y:=sp--
	if sp.tag=y.tag=tint then
		sp.value ixor:=y.value
	else
!			vx:=sp^
		copyvarv(vx, sp)

		save
		var_ixor(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jshl: ! Z':= Y << Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value <<:=y.value
	else
!			vx:=sp^
		copyvarv(vx, sp)

		save
		var_shl(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jshr: ! Z':= Y >> Z
	y:=sp--

	if sp.tag=y.tag=tint then
		sp.value >>:=y.value
	else
!			vx:=sp^
		copyvarv(vx, sp)

		save
		var_shr(sp,y)

		var_unshare(&vx)
		var_unshare(y)
	fi
	steppc
	jumpnext

jin: ! Z':= Y in Z (n=0) or Y not in Z (n=1)
	y:=sp
	x:=--sp

	save
	n:=var_in(x,y) ixor pc.n
	var_unshare(x)
	var_unshare(y)

	sp.tagx:=tint
	sp.value:=n
	steppc
	jumpnext

jinx: ! Z':= Y inx Z
	y:=sp
	x:=--sp

	save
	n:=var_inx(x,y)
	var_unshare(x)
	var_unshare(y)

	sp.tagx:=tint
	sp.value:=n

	steppc
	jumpnext

jcmp: ! Z':= Y c Z
	y:=sp
	x:=--sp

	save
	res:=k_cmp(pc.n, x, y)
	var_unshare(x)
	var_unshare(y)

	sp.tagx:=tint
	sp.value:=res
	steppc
	jumpnext

jmin: ! Z':= min(Y, Z)
	y:=sp--
	x:=sp

	save
	if var_compare(x,y)<0 then		!x is smaller
		var_unshare(y)
	else
		var_unshare(x)
		sp^:=y^
	fi

	steppc
	jumpnext

jmax: ! Z':= max(Y, Z)
	y:=sp--
	x:=sp

	save
	if var_compare(x,y)>=0 then		!x is bigger
		var_unshare(y)
	else
		var_unshare(x)
		sp^:=y^
	fi
	steppc
	jumpnext

jconcat: ! Z':= concat(Y, Z) or Y && Z
	unimpl
	steppc
	jumpnext

jappend: ! Z':= append(Y, Z) or Y & Z
	unimpl
	steppc
	jumpnext

jsame: ! Z':= Y == Z
	y:=sp--
	x:=sp

	if x.hasref and y.hasref and x.objptr=y.objptr then
		res:=1
	else
		res:=0
	fi

	var_unshare(x)
	var_unshare(y)
	sp.tagx:=tint
	sp.value:=res

	steppc
	jumpnext

jpower: ! Z':= Y ** Z
	y:=sp--
	copyvarv(vx, sp)

	save
	var_power(sp, y)

	var_unshare(&vx)
	var_unshare(y)
	steppc
	jumpnext

jbinto: ! Y^ op:= Z
	y:=sp--
	x:=sp--

	z:=x.varptr
	if pc.bintoindex=1 and x.tag=trefvar and z.tag=y.tag=tint then
		z.value+:=y.value
	else
		save
		var_inplace(pc.bintoindex, x, y)
		var_unshare(y)
	fi
	steppc
	jumpnext

jandlto: ! Y^ and:= Z
	unimpl
	steppc
	jumpnext

jorlto: ! Y^ or:= Z
	unimpl
	steppc
	jumpnext

jappendto: ! Y^ append:= Z or Y^ &:= Z
	y:=sp--
	px:=sp--

	case px.tag
	when trefvar then
		save
		var_appendto(px.varptr, y)
	else
		pclustype("Appendto", px)
	esac
	steppc
	jumpnext

jconcatto: ! Y^ concat:= Z or Y^ &&:= Z
	y:=sp--
	px:=sp--

	case px.tag
	when trefvar then
		save
		var_concatto(px.varptr, y)
	else
		pclustype("Concatto", px)
	esac
	steppc
	jumpnext

jdot: ! Z':= Z.g
	save
	k_dot(sp, pc.index)
	steppc
	jumpnext

jdot1: ! Z':= Z.g
	if sp.tag<>trecord then pclerror("Dot1: not rec") fi

	d:=genfieldtable[pc.index].def

	if sp.objptr.usertag<>d.owner.mode then pclerror("Dot1: wrong type") fi
	x:=sp.objptr.varptr+d.fieldoffset/varsize
	var_share(x)
	var_unshare(sp)
	copyvar(sp, x)
	steppc
	jumpnext

jpopdot: ! Z.g:= Y
	save
	sp:=k_popdot(sp, pc.index)
	steppc
	jumpnext

jpopdot1: ! Z.g:= Y
	x:=sp--
	y:=sp--

	if x.tag<>trecord then pclerror("Popdot1: not rec") fi
	if not x.objptr.mutable then
		save
		pcnotmut()
	fi
	e:=genfieldtable[pc.index].def

	if x.objptr.usertag<>e.owner.mode then pclerror("Popdot1: wrong type") fi
	z:=x.objptr.varptr+e.fieldoffset/varsize

	var_unshare(z)
	copyvar(z, y)
	var_unshare(x)
	steppc
	jumpnext

jdotref: ! Z':= &Z.g
	save
	k_dotref(sp, pc.index)
	steppc
	jumpnext

jindex: ! Z':= Y[Z]
	y:=sp--
	copyvarv(vx, sp)

	save
	case y.tag
	when tint then
		var_getix(sp,y.value)
	when trange then
		var_getslice(sp,y.range_lower,y.range_upper)
	else
		pclmxtypes("Index",&vx,y)
	esac

	var_unshare(&vx)

	steppc
	jumpnext

jpopix: ! Z':= Y[Z]:=X
	z:=sp--		!index
	y:=sp--		!list etc
	x:=sp--		!value to store

	save
	case z.tag
	when tint then
		var_putix(y, z.value, x)
		var_unshare(y)
	when trange then
		var_putslice(y, z.range_lower, z.range_upper, x)
		var_unshare(x)
		var_unshare(y)
	else
		pclmxtypes("Popix",y,z)
	esac

	steppc
	jumpnext

jindexref: ! Z':= &Y[Z]
	y:=sp--
	copyvarv(vx, sp)

	save
	case y.tag
	when tint then
		var_getixref(sp, y.value)
	else
		pclmxtypes("Indexref",sp,y)
	esac

	var_unshare(&vx)
	steppc
	jumpnext

jkeyindex: ! Z':= X{Y, Z}
	save
	sp:=k_keyindex(sp)
	steppc
	jumpnext

jpopkeyix: ! Y{Z}:= X
	save
	sp:=k_popkeyindex(sp)
	steppc
	jumpnext

jkeyixref: ! Z':= &X{Y, Z}
	save
	sp:=k_keyindexref(sp)
	steppc
	jumpnext

jdotix: ! Z':= Y.[Z]
	y:=sp--
	copyvarv(vx, sp)

	save
	case y.tag
	when tint then
		var_getdotix(sp, y.value)
	when trange then
		var_getdotslice(sp, y.range_lower, y.range_upper)
	else
		pcmxtypes("Dotindex", &vx, y)
	esac

	var_unshare(&vx)

	steppc
	jumpnext

jpopdotix: ! Y.[Z]:= X
	z:=sp--		!index
	y:=sp--		!ref to int, string etc
	x:=sp--		!value to store

	save
	case z.tag
	when tint then
		var_putdotix(y, z.value, x)
		var_unshare(y)
	when trange then
		var_putdotslice(y, z.range_lower, z.range_upper, x)
		var_unshare(x)
		var_unshare(y)
	else
		pclmxtypes("Popdotindex",y,z)
	esac
	
	steppc
	jumpnext

jdotixref: ! Z':= &Y.[Z]
	unimpl
	steppc
	jumpnext

jexpand: ! Z':= Expand Z into n objects are needed
	x:=sp+pc.n-1
	save
	var_expand(sp, x, pc.n)
	sp:=x

	steppc
	jumpnext

jpushtry: ! Push try/except into; label/except code/no. exceptions
	(++sp).tagx:=texception
	sp.ptr:=cast(pc.labelref)
	sp.frameoffset:=fp-ref byte(sp)		!byte offset
	sp.exceptiontype:=pc.x
	sp.nexceptions:=pc.y
	steppc
	jumpnext

jraise: ! Raise exception Z
	if sp.tag<>tint then
		pcerror("Raise: not Int")
	fi
PCLERROR("RAISE")
!		pc:=raiseexception(sp.value, sp, fp)		!will unwind stack and set pc to address of exception code
	jumpnext

jmap: ! Z':= map(Y, Z)
	save
	pc:=k_map(sp, pc, newsp)
	sp:=newsp
	jumpnext

jaddsp: ! SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)
	sp-:=pc.n
	steppc

	jumpnext

jpushff: ! Push f/f
	++sp
	sp^:=cast(fp+pc.offset, variant)^
	var_share(sp)
	++sp
	sp^:=cast(fp+(pc+1).offset, variant)^
	var_share(sp)
	skip1
	jumpnext

jpushfff: ! Push f/f/f
	++sp
	copyvar(sp, cast(fp+pc.offset, variant))
	var_share(sp)
	++sp
	copyvar(sp, cast(fp+(pc+1).offset, variant))
	var_share(sp)
	++sp
	copyvar(sp, cast(fp+(pc+2).offset, variant))
	var_share(sp)
	skip2
	jumpnext

jpushmci:
	++sp
	copyvar(sp, pc.varptr)
	var_share(sp)
	++sp
	sp.tagx:=tint
	sp.value:=(pc+1).value
	skip1
	jumpnext

jpushfci:
	++sp
	copyvar(sp, cast(fp+pc.offset, variant))
	var_share(sp)
	++sp
	sp.tagx:=tint
	sp.value:=(pc+1).value
	skip1
	jumpnext

jaddff: !
	x:=cast(fp+pc.offset, variant)
	y:=cast(fp+(pc+1).offset, variant)

	if x.tag=y.tag=tint then
		++sp
		sp.tagx:=tint
		sp.value:=x.value+y.value
		skip2
	else
		goto jpushf
	fi
	jumpnext

jaddfci: !
	x:=cast(fp+pc.offset, variant)

	if x.tag=tint then
		++sp
		sp.tagx:=tint
		sp.value:=x.value+(pc+1).value
		skip2
	else
		goto jpushf
	fi
	jumpnext

jaddci:
	if sp.tag=tint then
		sp.value+:=pc.value
		skip1
	else
		goto jpushci
	fi
	jumpnext

jmovefci:
	x:=cast(fp+(pc+1).offset)
	var_unshare(x)
	x.tagx:=tint
	x.value:=pc.value
	skip1
	jumpnext

jmoveff:
	x:=cast(fp+(pc+1).offset)
	y:=cast(fp+pc.offset)
	var_share(y)
	var_unshare(x)
	copyvar(x, y)
	skip1
	jumpnext

jindexff:
	x:=cast(fp+pc.offset)
doindexff:
	y:=cast(fp+(pc+1).offset)
	++sp
	copyvar(sp, x)

	save
	case y.tag
	when tint then
		var_getix(sp, y.value)
	when trange then
		var_getslice(sp, y.range_lower, y.range_upper)
	else
		pclmxtypes("Indexff",x,y)
	esac
	skip2
	jumpnext

jindexmf:
	x:=pc.varptr
	goto doindexff

jwheneqci:
	x:=sp
	if x.tag=tint then
		if x.value=pc.value then
			--sp
			pc:=(pc+1).labelref
		else
			skip1
		fi
	else
		goto jpushci
	fi
	jumpnext

jwhenneci: ! Y <> Z:  pop Z only, jump to L
					  ! Y = Z:   pop both, step to next
	x:=sp

	if x.tag=tint then
		if x.value<>pc.value then
			pc:=(pc+1).labelref
		else
			--sp
			skip1
		fi
	else
		goto jpushci
	fi
	jumpnext

jupbm:
	++sp
	copyvar(sp, pc.varptr)
	var_share(sp)
	save
	k_upb(sp)
	skip1
	jumpnext

jpushipm:
	x:=pc.varptr
	if x.tag<>trefpack or x.elemtag<>tu8 then goto jpushmref fi
	goto dopushipf

jpushipf:
	x:=cast(fp+pc.offset)
	if x.tag<>trefpack or x.elemtag<>tu8 then goto jpushfref fi
dopushipf:
	++sp
	sp.tagx:=tint
	case x.elemtag
	when tu8 then
		sp.value:=x.ptr^
		x.ptr+:=(pc+1).x
	esac
	skip2
	jumpnext

jpopipm:
	x:=pc.varptr
	if x.tag<>trefpack or x.elemtag<>tu8 or sp.tag<>tint then goto jpushmref fi
	goto dopopipf

jpopipf:
	x:=cast(fp+pc.offset)
	if x.tag<>trefpack or x.elemtag<>tu8 or sp.tag<>tint then goto jpushfref fi
dopopipf:
	case x.elemtag
	when tu8 then
		x.ptr^:=sp.value
		x.ptr+:=(pc+1).x
	esac
	--sp
	skip2
	jumpnext

jlastpcl:
jpushmm:
jpushmf:
jpushfm:
jmovemm:
jmovefm:
jmovemf:
jzmoveff:
jzmovemci:
jzmovefci:
jmovemci:
jpushv2:
jpushv3:
jsubfci:
jsubff:
jswitchf:
jupbf:
jlenf:
jstoref:

unimpl:
	pclerror2("Unimpl op:", pclnames[pc.opcode])
	stop 1
end

proc start=
!set up jumptable
	getjt:=1
	disploop()
	getjt:=0
end

global proc fixupcode(ifile pm)=
	pcl pc

	pc:=pm.pcstart

	while pc.opcode<>kendmod, ++pc do
		pc.labaddr:=jumptable[pc.opcode]
	od
end

global function runqprogram(isubprog sp, int ismain)int=
	
	return 0 when runcode<run_cc

	sptr:=&varstack[1]
	stacklimit:=&varstack[stacksize-100]
	pcptr:=modules[sp.firstmodule].pcstart
!
	int tt:=clock()

	disploop()
!
	tt:=clock()-tt

	println "Time:",TT
	println
	return sptr.value
end

