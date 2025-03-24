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

global ref[0:]ref label jumptable		!stays nil here

global proc disploop =
	pcl pc
	variant sp
	ref byte fp

	variant x
		int index @ x
		variant dest @ x
		variant px @ x
	variant y
		symbol d @ y
		int nloc @ y
	variant z	
		int n @ z
		pcl pz @ z
		object pp @ z
		object q @ z

	int xt,yt, res, lower, upper, moduleno, offset
	variant newsp
	symbol e

	varrec vx

	sp:=sptr
	pc:=pcptr
	fp:=frameptr

	doswitchu pc.opcode
	when knop      then   ! simple nop
!		unimpl
		steppc

	when kskip     then   ! ignore on pcl listing
		unimpl
		steppc

	when kprocdef  then   ! 
		unimpl
		steppc

	when kprocent  then   ! n=number of locals; 
		to pc.n do
			++sp
			sp.tagx:=tvoid
		od
		steppc

	when kprocend  then 
		unimpl
		steppc

	when kendmod   then 
		unimpl
		steppc

	when kcomment  then 
!		unimpl
		steppc

	when kpushm    then   ! Push v
		++sp
		copyvar(sp, pc.varptr)
		var_share(sp)
		steppc

	when kpushf    then   ! Push v
jpushf:
		++sp
		x:=cast(fp+pc.offset)
!		sp^:=x^
		copyvar(sp, x)

		var_share(sp)
		steppc

	when kpushmref then   ! push &v
jpushmref:
		++sp
		sp.tagx:=trefvar
		sp.varptr:=pc.varptr
		steppc

	when kpushfref then   ! push &v
jpushfref:
		++sp
		sp.tagx:=trefvar
		sp.varptr:=cast(fp+pc.offset)
		steppc

	when kpopm     then   ! v := Z
		x:=pc.varptr
		var_unshare(x)
!		x^:=sp^
		copyvar(x, sp)
		--sp
		steppc

	when kpopf     then   ! v := Z
		x:=cast(fp+pc.offset)
		var_unshare(x)
		copyvar(x, sp)
		--sp
		steppc

	when kpushci   then   ! Push i
jpushci:
		++sp
		sp.tagx:=tint
		sp.value:=pc.value
		steppc
jpushcix:

	when kpushvoid then   ! Push void 
		++sp
		sp.tagx:=tvoid
		steppc

	when kpushnil  then   ! Push nil (ref void)
		++sp
		sp.tagx:=trefpack
		sp.elemtag:=tvoid
		sp.ptr:=nil
		steppc

	when kpushcr   then   ! Push r
		++sp
		sp.tagx:=treal
		sp.xvalue:=pc.xvalue
		steppc

	when kpushcs   then   ! Push constant string object
		++sp
		sp.tagx:=tstring ior hasrefmask
		sp.objptr:=pc.objptr
		++sp.objptr.refcount
		steppc

	when kpushtype then   ! Push type constant
		++sp
		sp.tagx:=ttype
		sp.value:=pc.typecode
		steppc

	when kpushopc  then   ! Push operator constant
		++sp
		sp.tagx:=toperator
		sp.value:=pc.pclop
		steppc

	when kpushsym  then   ! Push symbol reference
		++sp
		sp.tagx:=tsymbol
		sp.def:=pc.def

		steppc

	when kpushptr  then   ! Z' := Z^
		x:=sp
jpushptr:
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

	when kpushptrf then
		x:=cast(fp+pc.offset)
		++sp
		++pc
		goto jpushptr

	when kpopptr   then   ! Z^ := Y
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

	when kzpopm    then   ! v := Z; don't free v first
		copyvar(pc.varptr, sp)
		--sp
		steppc

	when kzpopf    then   ! v := Z; don't free v first
		x:=cast(fp+pc.offset)
		copyvar(x, sp)
		--sp
		steppc

	when kdupl     then   ! (Z',Y') := (share(Z), Z)
		++sp
		copyvar(sp, sp-1)
		var_share(sp)
		steppc

	when kcopy     then   ! Z' := deepcopy(Z)
		if sp.hasref then
			copyvarv(vx, sp)
			save
			var_duplu(sp)
			var_unshareu(&vx)
		fi
		steppc

	when kswap     then   ! swap(Z^, Y^)
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

	when kconvrefp then   ! Change ref in Z to refpacked
		save
		k_convrefpack(sp)
		steppc

	when kjump     then   ! Jump to L
		pc:=pc.labelref

	when kjumpptr  then   ! Jump to Z
		unimpl
		steppc

	when kjumpt    then   ! Jump to L when Z is true
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

	when kjumpf    then   ! Jump to L when Z is false
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

	when kjumpeq   then   ! Jump to L when Y = Z
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

	when kjumpne   then   ! Jump to L when Y<>= Z
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

	when kjumplt   then   ! Jump to L when Y < Z
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

	when kjumple   then   ! Jump to L when Y <= Z
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

	when kjumpge   then   ! Jump to L when Y >= Z
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

	when kjumpgt   then   ! Jump to L when Y > Z
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

	when kjmpeqfci   then   ! Jump to L when B = C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value=(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpnefci   then   ! Jump to L when B <> C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value<>(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpltfci   then   ! Jump to L when B < C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value<(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmplefci   then   ! Jump to L when B <= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value<=(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgefci   then   ! Jump to L when B >= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value>=(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgtfci   then   ! Jump to L when B > C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		if x.value>(pc+1).value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpeqff   then   ! Jump to L when B = C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value=y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpneff   then   ! Jump to L when B <> C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value<>y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpltff   then   ! Jump to L when B < C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value<y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpleff   then   ! Jump to L when B <= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value<=y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgeff   then   ! Jump to L when B >= C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value>=y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kjmpgtff   then   ! Jump to L when B > C
		x:=cast(fp+pc.offset)
		if x.tag<>tint then goto jpushf fi
		y:=cast(fp+(pc+1).offset)
		if x.value>y.value then
			pc:=(pc+2).labelref
		else
			skip2
		fi

	when kwheneq   then   ! Y = Z:  pop both, jump to L
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

	when kwhenne   then   ! Y <> Z:  pop Z only, jump to L
						  ! Y = Z:   pop both, step to next
jwhenne:
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

	when kjumplab  then   ! Jumptable entry
		unimpl
		steppc

	when kswitch   then   ! Jumptable has y-x+1 entries
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

	when ktom      then   ! --v; jump to l when v<>0 in next op
		x:=(pc+1).varptr
		doto

	when ktof      then   ! --v; jump to l when v<>0 in next op
freddy:
		x:=cast(fp+(pc+1).offset)
doto:
		if --x.value then
			pc:=pc.labelref
		else
			skip1
		fi

	when kformci   then   ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
		x:=(pc+1).varptr
		doforfci

	when kforfci   then   ! ++v; jump to l when v<=i in next 2 ops: pushm/pushci
		x:=cast(fp+(pc+1).offset)
doforfci:
		++x.value
		if x.value<=(pc+2).value then
			pc:=pc.labelref
		else
			skip2
		fi

	when kformm    then   ! ++v; jump to l when v<=v in next 2 ops
		x:=(pc+1).varptr
		y:=(pc+2).varptr
		doforff

	when kforff    then   ! ++v; jump to l when v<=v in next 2 ops
		x:=cast(fp+(pc+1).offset)
		y:=cast(fp+(pc+2).offset)
doforff:
		++x.value

		if x.value<=y.value then
			pc:=pc.labelref
		else
			skip2
		fi

	when kcallproc then   ! Call &A; n is no. args
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
		sp.retaddr := pc+1

		sp.frameptr_low := u64(fp)
		fp:=cast(sp)

		pc:=pc.labelref

	when kcallptr  then   ! Call X^; n is no. of params supplied; x is stack adjust
		if sp.tag<>tsymbol then
			pclerror("Probably undefined function")
		fi

		d:=sp.def
		if d.nameid=linkid then d:=d.alias fi

		if d.nparams<>pc.n then
			pclerror2("Callptr: wrong # params; need:",strint(d.nparams))
		fi

		sp.tagx:=tretaddr
		sp.retaddr := pc+1

		sp.frameptr_low := word(fp)
		fp:=cast(sp)

		pc:=cast(d.labelref)

	when kretproc  then
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

	when kretfn  then
		x:=variant(fp+pc.y)
		copyvar(x, sp)		!transfer reference
		--sp
		doretproc

	when kmodcall  then   ! 
		d:=pc.def
		moduleno:=d.moduleno

		++sp
		sp.tagx:=tretaddr
		sp.retaddr := pc+1
		pc:=modules[moduleno].pcstart

	when kmodret   then   ! 
		pc:=sp.retaddr

	when kcalldll  then   ! Call dll function d (sysmbol); n=nargs
		n:=pc.n
		save
		SPTR:=SP

		calldll(pc.def, sp-n+1, sp-n, n)
		sp-:=n

		steppc

	when kcallhost then   ! Call Q host function h (Host index)
		save
		sp:=callhostfunction(pc.hostindex, sp)
		steppc

	when kunshare  then   ! Unshare and pop A var values on stack
		to pc.n do
			var_unshare(sp)
			--sp
		od
		steppc

	when kstop     then   ! Stop program with stopcode Z; n=1 to stop runproc instead
		stopped:=1
		sptr:=sp
		exit

	when kmakelist then   ! x items on stack; make list with lwb y
		save
		sp:=k_makelist(sp, pc.y, pc.x)
		steppc

	when kmakevrec then   ! x items on stack; make record of type u
		n:=pc.x
		x:=sp-pc.x+1				!start of data

		save
		var_make_record(x, x, pc.x, pc.usertag)
		sp:=x
		sp.objptr.mutable:=0
		steppc

	when kmakeax   then   ! x items on stack; make array with lwb y, type u and elemtype v
		unimpl
		steppc

	when kmakebits then   ! x items on stack; make bits with lwb y, type u and elemtype v
		unimpl
		steppc

	when kmaketrec then   ! x items on stack; make struct with type u
		n:=pc.x
		x:=sp-n+1				!start of data

		save
		var_make_struct(x, x, n, pc.usertag)
		sp:=x
		sp.objptr.mutable:=0
		steppc

	when kmakeset  then   ! x items on stack; make set
		n:=pc.x

		x:=sp-n+1			!start of data

		save
		var_make_set(x, x, n)
		sp:=x
		sp.objptr.mutable:=0

		steppc

	when kmakerang then   ! 2 items on stack; make range
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

	when kmakedict then   ! x*2 items on stack (x key:val items); make dict
		n:=pc.x
		x:=sp-n*2+1			!start of data

		save
		var_make_dict(x, x, n)
		sp:=x
		steppc

	when kmakedec  then   ! Turn string on stack to decimal number
!		vx:=sp^
		copyvarv(vx, sp)

		if vx.tag<>tstring then pclerror("Not str") fi
		pp:=vx.objptr
		if pp.length=0 then pclerror("Null str") fi

		save
		var_make_dec_str(pp.strptr, pp.length, sp)

		var_unshare(&vx)

		steppc

	when kincrptr  then   ! Z^ +:= x
		save
		k_incrptr(sp, pc.x)
		--sp
		steppc

	when kincrtom  then   ! v +:= x
		x:=pc.varptr
		doincrto

	when kincrtof  then   ! v +:= x
jincrtof:
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

	when kloadincr then   ! T := Z^; Z^ +:= x; Z' := T
		copyvarv(vx, sp)
!		vx:=sp^
		save
		var_loadptr(sp,sp)
		++sp
!		sp^:=vx
		copyvar_v(sp, vx)
		k_incrptr(sp, pc.x)
		--sp
		steppc

	when kincrload then   ! Z^ +:= x; Z' := Z^
!		vx:=sp^
		copyvarv(vx, sp)
		save
		k_incrptr(sp, pc.x)
		--sp
		var_loadptr(&vx, ++sp)
		steppc

	when kneg      then   ! Z':= -Z
!		vx:=sp^

		copyvarv(vx, sp)
		save
		var_neg(sp)
		var_unshare(&vx)
		steppc

	when kabs      then   ! Z' := abs Z
!		vx:=sp^
		copyvarv(vx, sp)

		save
		var_abs(sp)
		var_unshare(&vx)

		steppc

	when knotl     then   ! Z' := not Z
		save
		res:=not var_istruel(sp)
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=res
		steppc

	when kinot     then   ! Z' := inot Z
		if sp.tag=tint then
			sp.value:=inot sp.value
		else
			copyvarv(vx, sp)
			save
			var_inot(sp)
			var_unshare(&vx)
		fi

		steppc

	when kistruel  then   ! Z' := istrue Z
		save
		n:=var_istruel(sp)
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=n

		steppc

	when kasc      then   ! Z' := asc(Z)
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

	when kchr      then   ! Z' := chr(Z)
		if sp.tag=tint then
			save
			var_makechar(sp.value, sp)
		else
			pclustype("CHR",sp)
		fi
		steppc

	when ksqr    then   ! Z' := op(Z)
		case sp.tag
		when tint then
			sp.value:=sqr(sp.value)
		when treal then
			sp.xvalue:=sqr(sp.xvalue)
		else
			pclustype("sqr", sp)
		esac
		steppc

	when kmaths    then   ! Z' := op(Z)
		save
		k_maths(sp, pc.mathscode)
		steppc

	when kmaths2   then   ! Z' := op(Y, Z)
		unimpl
		steppc

	when kunaryto  then   ! Z^ op:= Z
		unimpl
		steppc

	when knotlto   then   ! Z^ not:= Z
		unimpl
		steppc

	when klen      then   ! Z' := Z.len
		save
		k_len(sp)
		steppc

	when klwb      then   ! Z' := Z.lwb
		save
		k_lwb(sp)
		steppc

	when kupb      then   ! Z' := Z.upb
jupb:
		save
		k_upb(sp)
		steppc

	when kbounds   then   ! Z' := Z.bounds; n=1: one range value; n=2: two dims
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

	when kbytesize then   ! Z' := Z.bytesize
		save
		res:=k_bytesize(sp)
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=res
		steppc

	when ktype     then   ! Z' := n=0/1/2 = basetype/tag/elemtype
		save
		n:=k_type(sp, pc.n)
		var_unshare(sp)
		sp.tagx:=ttype
		sp.value:=n

		steppc

	when kdictsize then   ! Z' := Z.dictsize
		unimpl
		steppc

	when kisfound  then   ! Z' := Z.isfound
		if sp.tag<>tint then pclerror("isfound") fi
		sp.value:=sp.value<>i64.min
		steppc

	when kminval   then   ! Z' := Z.minvalue
		unimpl
		steppc

	when kmaxval   then   ! Z' := Z.maxvalue
		unimpl
		steppc

	when kistype   then   ! Z' := Z.type/etc = t
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

	when kisvoid   then   ! Z' := Z.isvoid (n=0) or not Z.isdef (n=1)
		res:=sp.tag=tvoid
		var_unshare(sp)
		sp.tagx:=tint
		sp.value:=res ixor pc.n
		steppc

	when kconvert  then   ! Z' := t(Z)
		if sp.tag<>pc.usertag then
!			vx:=sp^
			copyvarv(vx, sp)
			save
			var_convert(&vx, pc.usertag, sp)
			var_unshare(&vx)
		fi

		steppc

	when ktypepun  then   ! Z' := t@(Z)
		unimpl
		steppc

	when kadd      then   ! Z' := Y + Z
jadd:
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

	when ksub      then   ! Z' := Y - Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value-:=y.value
		elsif sp.tag=y.tag=treal then
			sp.xvalue-:=y.xvalue
		else
			copyvarv(vx, sp)

			save
			var_sub(sp, y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kmul      then   ! Z' := Y * Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value*:=y.value
		elsif sp.tag=y.tag=treal then
			sp.xvalue*:=y.xvalue
		else
!			vx:=sp^
			copyvarv(vx, sp)

			save
			var_mul(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi

		steppc

	when kdiv      then   ! Z' := Y / Z
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

	when kidiv     then   ! Z' := Y % Z
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

	when kirem     then   ! Z' := Y rem Z
		y:=sp--
		copyvarv(vx, sp)

		save
		var_irem(sp,y)

		var_unshare(&vx)
		var_unshare(y)

		steppc

	when kidivrem  then   ! (Y', Z') := Y divrem Z
		unimpl
		steppc

	when kiand     then   ! Z' := Y iand Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value iand:=y.value
		else
			copyvarv(vx, sp)
			save
			var_iand(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kior      then   ! Z' := Y ior Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value ior:=y.value
		else
			copyvarv(vx, sp)
			save
			var_ior(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kixor     then   ! Z' := Y ixor Z
		y:=sp--
		if sp.tag=y.tag=tint then
			sp.value ixor:=y.value
		else
			copyvarv(vx, sp)

			save
			var_ixor(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kshl      then   ! Z' := Y << Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value <<:=y.value
		else
			copyvarv(vx, sp)

			save
			var_shl(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kshr      then   ! Z' := Y >> Z
		y:=sp--

		if sp.tag=y.tag=tint then
			sp.value >>:=y.value
		else
			copyvarv(vx, sp)

			save
			var_shr(sp,y)

			var_unshare(&vx)
			var_unshare(y)
		fi
		steppc

	when kin       then   ! Z' := Y in Z (n=0) or Y not in Z (n=1)
		y:=sp
		x:=--sp

		save
		n:=var_in(x,y) ixor pc.n
		var_unshare(x)
		var_unshare(y)

		sp.tagx:=tint
		sp.value:=n
		steppc

	when kinx      then   ! Z' := Y inx Z
		y:=sp
		x:=--sp

		save
		n:=var_inx(x,y)
		var_unshare(x)
		var_unshare(y)

		sp.tagx:=tint
		sp.value:=n

		steppc

	when kcmp      then   ! Z' := Y c Z
		y:=sp
		x:=--sp

		save
		res:=k_cmp(pc.n, x, y)
		var_unshare(x)
		var_unshare(y)

		sp.tagx:=tint
		sp.value:=res
		steppc

	when kmin      then   ! Z' := min(Y, Z)
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

	when kmax      then   ! Z' := max(Y, Z)
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

	when kconcat   then   ! Z' := concat(Y, Z) or Y && Z
		unimpl
		steppc

	when kappend   then   ! Z' := append(Y, Z) or Y & Z
		unimpl
		steppc

	when ksame     then   ! Z' := Y == Z
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

	when kpower    then   ! Z' := Y ** Z
		y:=sp--
		copyvarv(vx, sp)

		save
		var_power(sp, y)

		var_unshare(&vx)
		var_unshare(y)
		steppc

	when kbinto    then   ! Y^ op:= Z
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

	when kandlto   then   ! Y^ and:= Z
		unimpl
		steppc

	when korlto    then   ! Y^ or:= Z
		unimpl
		steppc

	when kappendto then   ! Y^ append:= Z or Y^ &:= Z
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

	when kconcatto then   ! Y^ concat:= Z or Y^ &&:= Z
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

	when kdot      then   ! Z' := Z.g
		save
		k_dot(sp, pc.index)
		steppc

	when kdot1     then   ! Z' := Z.g
		if sp.tag<>trecord then pclerror("Dot1: not rec") fi

		d:=genfieldtable[pc.index].def

		if sp.objptr.usertag<>d.owner.mode then pclerror("Dot1: wrong type") fi
		x:=sp.objptr.varptr+d.fieldoffset/varsize
		var_share(x)
		var_unshare(sp)
		copyvar(sp, x)
		steppc

	when kpopdot   then   ! Z.g := Y
		save
		sp:=k_popdot(sp, pc.index)
		steppc

	when kpopdot1   then   ! Z.g := Y
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

	when kdotref   then   ! Z' := &Z.g
		save
		k_dotref(sp, pc.index)
		steppc

	when kindex    then   ! Z' := Y[Z]
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

	when kpopix    then   ! Z' := Y[Z]:=X
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

	when kindexref then   ! Z' := &Y[Z]
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

	when kkeyindex then   ! Z' := X{Y, Z}
		save
		sp:=k_keyindex(sp)
		steppc

	when kpopkeyix then   ! Y{Z} := X
		save
		sp:=k_popkeyindex(sp)
		steppc

	when kkeyixref then   ! Z' := &X{Y, Z}
		save
		sp:=k_keyindexref(sp)
		steppc

	when kdotix    then   ! Z' := Y.[Z]
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

	when kpopdotix then   ! Y.[Z] := X
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

	when kdotixref then   ! Z' := &Y.[Z]
		unimpl
		steppc

	when kexpand   then   ! Z' := Expand Z into n objects are needed
		x:=sp+pc.n-1
		save
		var_expand(sp, x, pc.n)
		sp:=x

		steppc

	when kpushtry  then   ! Push try/except into; label/except code/no. exceptions
		(++sp).tagx:=texception
		sp.ptr:=cast(pc.labelref)
		sp.frameoffset:=fp-ref byte(sp)		!byte offset
		sp.exceptiontype:=pc.x
		sp.nexceptions:=pc.y
		steppc

	when kraise    then   ! Raise exception Z
		if sp.tag<>tint then
			pcerror("Raise: not Int")
		fi
PCLERROR("RAISE")
!		pc:=raiseexception(sp.value, sp, fp)		!will unwind stack and set pc to address of exception code

	when kmap      then   ! Z' := map(Y, Z)
		save
		pc:=k_map(sp, pc, newsp)
		sp:=newsp

	when kaddsp    then   ! SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)
		sp-:=pc.n
		steppc


	when kpushff    then   ! Push f/f
		++sp
		sp^:=cast(fp+pc.offset, variant)^
		var_share(sp)
		++sp
		sp^:=cast(fp+(pc+1).offset, variant)^
		var_share(sp)
		skip1

	when kpushfff    then   ! Push f/f/f
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

	when kpushmci then
		++sp
		copyvar(sp, pc.varptr)
		var_share(sp)
		++sp
		sp.tagx:=tint
		sp.value:=(pc+1).value
		skip1

	when kpushfci then
		++sp
		copyvar(sp, cast(fp+pc.offset, variant))
		var_share(sp)
		++sp
		sp.tagx:=tint
		sp.value:=(pc+1).value
		skip1

	when kaddff    then   !
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

	when kaddfci    then   !
		x:=cast(fp+pc.offset, variant)

		if x.tag=tint then
			++sp
			sp.tagx:=tint
			sp.value:=x.value+(pc+1).value
			skip2
		else
			goto jpushf
		fi

	when kaddci then
		if sp.tag=tint then
			sp.value+:=pc.value
			skip1
		else
			goto jpushci
		fi

	when kmovefci then
		x:=cast(fp+(pc+1).offset)
		var_unshare(x)
		x.tagx:=tint
		x.value:=pc.value
		skip1

	when kmoveff then
		x:=cast(fp+(pc+1).offset)
		y:=cast(fp+pc.offset)
		var_share(y)
		var_unshare(x)
		copyvar(x, y)
		skip1

	when kindexff then
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

	when kindexmf then
		x:=pc.varptr
		goto doindexff

	when kwheneqci then
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

	when kwhenneci then   ! Y <> Z:  pop Z only, jump to L
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

	when kupbm then
		++sp
		copyvar(sp, pc.varptr)
		var_share(sp)
		save
		k_upb(sp)
		skip1

	when kpushipm then
		x:=pc.varptr
		if x.tag<>trefpack or x.elemtag<>tu8 then goto jpushmref fi
		goto dopushipf

	when kpushipf then
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

	when kpopipm then
		x:=pc.varptr
		if x.tag<>trefpack or x.elemtag<>tu8 or sp.tag<>tint then goto jpushmref fi
		goto dopopipf

	when kpopipf then
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

	when klastpcl  then		! needed for switchu when to trap unimpl extended ops
		unimpl
	else
unimpl:
		pclerror2("Unimpl op:", pclnames[pc.opcode])
		stop 1
	end
end

global proc fixupcode(ifile pm)=
end

global function runqprogram(isubprog sp, int ismain)int=
	
	return 0 when runcode<run_cc

	sptr:=&varstack[1]
	stacklimit:=&varstack[stacksize-100]
	pcptr:=modules[sp.firstmodule].pcstart
	stopped:=0
!
	int tt:=clock()

	disploop()
!
	tt:=clock()-tt

	println "Time:",TT
	println

	return sptr.value
end

