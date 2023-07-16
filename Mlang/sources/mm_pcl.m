!type system

!.opndtype in pclrec

global enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),
	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(label_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),
	(real32_opnd,		$),
	(string_opnd,		$),
	(strimm_opnd,		$),
	(assem_opnd,		$),
	(realimm_opnd,		$),
end

global type pcl = ref pclrec

global record pclrec =
	byte opndtype
	byte opcode
	byte pcat
	byte pmode				!t in tables

	int32 psize

	union
		struct
			union
				int64 value
				real64 xvalue
				real32 xvalue32
				ichar svalue
				int labelno
				symbol def
				ref void asmcode
			end
			union						!two 32-bit params used according to opcode
				struct
					int32 x				!common access to these 1/2 extra attribs
					int32 y
				end

				struct					! (x,y) pointer ops
					int32 scale			! scale factor for offset
					int32 extra			! extra constant byte offset, already scaled
				end
				struct					! (x,y) call/etc
					int32 nargs			! number of args
					int32 nvariadics	! 0, or arg # that is first variadic
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end

				int32 oldmode			! (x) u in tables
				int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				int32 truncmode			! (x) convert/truncate: truncated mode
				int32 align
				int32 nret				! (x) setretmult: no. of return values
				int32 popone			! (x) jumpcc: leave X on stack
				int32 strindex			! (x) any string operand: index into string table
				int32 r64index			! (x) any real operand: index into real table
				int32 r32index			! (x) any real32 operand: index into real32 table

			end
		end
	end
	u32 pos:(sourceoffset:24, fileno:8)

	u16 spare2
	u16 SEQ
end


!Stack operands depend on A in (A B); the last (X/Y/Z) is pushed last and
!is top of stack:
!
! A = 1       X
! A = 2       X Y
! A = 3       X Y Z
! A = 4       X Y Z W
!
!It can be assumed that all are popped
!New stack contents are indicated by X' and sometimes X' Y' which are pushed

!Immediate operand:
!   A			(various)
!Extra info:
!   op			opindex
!   fn			fnindex
!   cc			cond code
!   t[:size]    type (:size for block types)
!   u           secondary type for some ops (convert etc)
!   n			nargs for calls
!   s x			scale and offset for ptr/offset ops
!   x y			min/max lab index for switch
!	B			Secondary operand in a following kopnd instruction
!	C			Tertiary operand in a following kopnd instruction

!Stack usage is represented by (a b):
! a is the number of stack elements that will be popped
! b is the number of new stack elements that will be pushed
! Something like (1 1) can mean the same element stays in place


export enumdata [0:]ichar pclnames,
			[0:]byte pclhastype,
			[0:]byte pclextra =
!                           T X     ! (A B)
	(kzero=0,			$,	0,0),	! (0 0)
	(knop,				$,	0,0),	! (0 0)
	(kstop,				$,	0,0),	! (1 0)	Stop X
	(kcomment,			$,	0,0),	! (0 0)	Comment A (a string)

	(kistatic,			$,  1,0),	! (0 0) (A,t) Define idata label (must be followed by correct kdata ops)
	(kzstatic,			$,	1,0),	! (0 0) (A,t) Define zdata labe and reserve sufficient space

	(kprocdef,			$,	1,0),	! (0 0) (A,t) Define proc A, of given return type
!	(kprocentry,		$,	0,0),	! (0 0)
	(kendproc,			$,	0,0),	! (0 0)
	(kendprogram,		$,	0,0),	! (0 0)
	(kthreadedproc,		$,	1,0),	! (0 0) (A,t) Define proc A, of given return type

	(klabel,			$,	0,0),	! (0 0) (L) Define numbered label L

	(kload,				$,	1,0),	! (0 1) (X,t)	Push operand X of type t; X is anything pushable
	(kstore,			$,	1,0),	! (1 0) (L,t)	pop to label X
	(kloadlabel,		$,	0,0),	! (0 1) (L)		Push address of label L
	(kpushhw,			$,	0,0),	! (0 0) ()		Push top of stack to hw stack
	(kdupl,				$,	1,0),	! (1 1+) ()		Step count for operand X
	(kduplopnds,		$,	1,0),	! (1 2) ()		X' := Y' :=X
	(kswapopnds,		$,	1,0),	! (2 2) ()		(X', Y') := (Y, X)
	(kunload,			$,	1,0),	! (1 0) ()		Pop stack

	(kopnd,				$,	0,0),	! (0 0) (X) Define auxiliary operand X (not sure about extra stuff yet)
	(ktype,				$,	1,0),	! (0 0) (t) Define auxiliary type t
	(kcopyblock,		$,	1,0),	! (1 1) (A, t) Copy block to A; X' := A

	(kiloadx,			$,	1,2),	! (2 1) (t,scale,offset) X' := (X+Y*scale+offset)^ using given type
	(kistorex,			$,	1,2),	! (3 0) (t,scale,offset) (Y+Z*scale+offset)^:=X

	(kiload,			$,	1,0),	! (1 1) X' := X^
	(kistore,			$,	1,0),	! (2 0) Y^:=X

	(kloadbit,			$,	1,0),	! (2 1)	X' := X.[Y]
	(kstorebit,			$,	1,0),	! (3 0) Y^.[Z]:=X

	(kloadbf,			$,	1,0),	! (3 1) X' := X.[Y..Z]
	(kstorebf,			$,	1,0),	! (4 0) Y^.[Zb..W]:=X

	(keval,				$,	1,0),	! (1 0) Evaluate X [load to an actual register], then pop

	(kcallp,			$,	0,2),	! (n 0) (A) Call &A with nargs, then pop args
	(kicallp,			$,	0,2),	! (n+1 0) Call X with nargs, then pop args
	(kretproc,			$,	0,0),	! (0 0) Return from proc

	(kcallf,			$,	1,2),	! (n 1) (A, t), Call &A, then pop args, leave retval
	(kicallf,			$,	1,2),	! (n+1 1) (t) Call X, then pops args, leave retval
	(kretfn,			$,	1,0),	! (0 0) (t) Return from function with X=retval

	(kjump,				$,	0,0),	! (0 0) (L) goto L
	(kjumpptr,			$,	0,0),	! (1 0) goto X

	(kjumpeq,			$,	1,1),	! (2 0/1) (L,t,p1) goto L when X = Y; p1=1: X':=X
	(kjumpne,			$,	1,1),	! (2 0/1) (L,t,p1) goto L when <>
	(kjumplt,			$,	1,1),	! (2 0/1) (L,t,p1) goto L when X < Y
	(kjumple,			$,	1,1),	! (2 0/1) (L,t,p1) goto L when <=
	(kjumpge,			$,	1,1),	! (2 0/1) (L,t,p1) goto L when >=
	(kjumpgt,			$,	1,1),	! (2 0/1) (L,t,p1) goto L when >

	(kjumptrue,			$,	1,0),	! (1 0) (L,t) goto L when X is true
	(kjumpfalse,		$,	1,0),	! (1 0) (L,t) goto L when X is false

	(kseteq,			$,	1,0),	! (2 1) (t) X' := X = Y
	(ksetne,			$,	1,0),	! (2 1) (t) X' := X <> Y
	(ksetlt,			$,	1,0),	! (2 1) (t) X' := X < Y
	(ksetle,			$,	1,0),	! (2 1) (t) X' := X <= Y
	(ksetge,			$,	1,0),	! (2 1) (t) X' := X >= Y
	(ksetgt,			$,	1,0),	! (2 1) (t) X' := X > Y

	(kcasejumpeq,		$,	1,1),	! (2 1) (L,t) goto L when X=Y; pop Y, leave X

	(kto,				$,	0,0),	! (0 0) (L)(B,t) --B (aux); goto L when B<>0 

	(kforup,			$,	1,1),	! (0 0) (L,t,n)(B,t)(C,t) B+:=n; goto L when B<=C
	(kfordown,			$,	1,1),	! (0 0) (L,t,n)(B,t)(C,t) B-:=n; goto L when B>=C

	(kswap,				$,	1,0),	! (2 0) (t) swap(X^,Y^) ref T/V

	(kstoreslice,		$,	1,0),	! (2 0) (t) A:=slice(X, Y); pop X,Y
	(kstoresliced,		$,	1,0),	! (2 1) (t) A:=slice(X, Y); leave A on stack
	(kmakelist,			$,	1,2),	! (n 1) (t, n,lower) X' := list(....)
	(kmakeset,			$,	1,1),	! (n 1) (t, n) X' := set(....)
	(kmakerange,		$,	1,0),	! (2 1) (t) X' := X..Y as variant

	(kswitch,			$,	0,2),	! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
	(kswitchu,			$,	0,2),	! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
	(kswitchlabel,		$,	0,0),	! (0 0) (L) jumptable entry
	(kendswitch,		$,	0,0),	! (0 0)	Mark end of switch jumptable

	(kclear,			$,	1,0),	! (1 0) (t) Clear X^

	(kdb,				$,	0,0),	! (0 0) (X) Define a u8 data value
	(kdw,				$,	0,0),	! (0 0) (X) u16 value: ...
	(kdd,				$,	0,0),	! (0 0) (X) u32 value: u32/i32/r32, depends on operand
	(kdq,				$,	0,0),	! (0 0) (X) u64 value: u64/i64/r64/string/addr/label, depends on operan

	(kassem,			$,	0,0),	! (0 0) to be worked out....

	(kadd,				$,	1,0),	! (2 1) (t) X' := X + Y
	(ksub,				$,	1,0),	! (2 1) (t)
	(kmul,				$,	1,0),	! (2 1) (t)
	(kdiv,				$,	1,0),	! (2 1) (t)
	(kidiv,				$,	1,0),	! (2 1) (t)
	(kirem,				$,	1,0),	! (2 1) (t)
	(kidivrem,			$,	1,0),	! (2 2) (t)
	(kiand,				$,	1,0),	! (2 1) (t)
	(kior,				$,	1,0),	! (2 1) (t)
	(kixor,				$,	1,0),	! (2 1) (t)
	(kshl,				$,	1,0),	! (2 1) (t)
	(kshr,				$,	1,0),	! (2 1) (t)
	(kin,				$,	1,0),	! (2 1) (t)
	(knotin,			$,	1,0),	! (2 1) (t)
	(kmin,				$,	1,0),	! (2 1) (t)
	(kmax,				$,	1,0),	! (2 1) (t)
	(ksame,				$,	1,0),	! (2 1) (t)
	(kandl,				$,	1,0),	! (2 1) (t)
	(korl,				$,	1,0),	! (2 1) (t)
	(kaddrefx,			$,	1,2),	! (2 1) (t,scale,offset) X' := X + Y*scale + offset
	(ksubrefx,			$,	1,2),	! (2 1) (t,scale,offset) X' := X - Y*scale + offset
	(ksubref,			$,	1,1),	! (2 1) (t,scale) X' := (X - Y)/scale
	(kappend,			$,	1,0),	! (2 1) (t)
	(kconcat,			$,	1,0),	! (2 1) (t)

	(kneg,				$,	1,0),	! (1 1) (t) X' := -X
	(kabs,				$,	1,0),	! (1 1) (t)
	(kinot,				$,	1,0),	! (1 1) (t)
	(knotl,				$,	1,0),	! (1 1) (t)
	(kistruel,			$,	1,0),	! (1 1) (t)
	(ksqr,				$,	1,0),	! (1 1) (t)

	(ksqrt,				$,	1,0),	! (1 1) (t) X' := sqrt(X)
	(ksin,				$,	1,0),	! (1 1) (t)
	(kcos,				$,	1,0),	! (1 1) (t)
	(ktan,				$,	1,0),	! (1 1) (t)
	(kasin,				$,	1,0),	! (1 1) (t)
	(kacos,				$,	1,0),	! (1 1) (t)
	(katan,				$,	1,0),	! (1 1) (t)
	(kln,				$,	1,0),	! (1 1) (t)
	(klog,				$,	1,0),	! (1 1) (t)
	(kexp,				$,	1,0),	! (1 1) (t)
	(kround,			$,	1,0),	! (1 1) (t)
	(kfloor,			$,	1,0),	! (1 1) (t)
	(kceil,				$,	1,0),	! (1 1) (t)
	(kfract,			$,	1,0),	! (1 1) (t)
	(ksign,				$,	1,0),	! (1 1) (t)
	(katan2,			$,	1,0),	! (1 1) (t)
	(kpower,			$,	1,0),	! (1 1) (t)
	(kfmod,				$,	1,0),	! (1 1) (t)

	(kincr,				$,	1,1),	! (1 0) (t,step) X^+:=step
	(kdecr,				$,	1,1),	! (1 0) (t,step) X^-:=step
	(kincrload,			$,	1,1),	! (1 1) (t,step) X' := (X+:=step)^
	(kdecrload,			$,	1,1),	! (1 1) (t,step) X' := (X-:=step)^
	(kloadincr,			$,	1,1),	! (1 1) (t,step) X' := X++^ (difficult to express step)
	(kloaddecr,			$,	1,1),	! (1 1) (t,step) X' := X--^

	(kaddto,			$,	1,0),	! (2 0) (t) X^ +:= Y
	(ksubto,			$,	1,0),	! (2 0) (t)
	(kmulto,			$,	1,0),	! (2 0) (t)
	(kdivto,			$,	1,0),	! (2 0) (t)
	(kidivto,			$,	1,0),	! (2 0) (t)
	(kiremto,			$,	1,0),	! (2 0) (t)
	(kiandto,			$,	1,0),	! (2 0) (t)
	(kiorto,			$,	1,0),	! (2 0) (t)
	(kixorto,			$,	1,0),	! (2 0) (t)
	(kshlto,			$,	1,0),	! (2 0) (t)
	(kshrto,			$,	1,0),	! (2 0) (t)
	(kminto,			$,	1,0),	! (2 0) (t)
	(kmaxto,			$,	1,0),	! (2 0) (t)
	(kandlto,			$,	1,0),	! (2 0) (t)
	(korlto,			$,	1,0),	! (2 0) (t)
	(kaddrefxto,		$,	1,2),	! (2 0) (t,scale,offset) X^ +:= Y
	(ksubrefxto,		$,	1,2),	! (2 0) (t,scale,offset) X^ -:= Y
	(kappendto,			$,	1,0),	! (2 0) (t)
	(kconcatto,			$,	1,0),	! (2 0) (t)

	(knegto,			$,	1,0),	! (1 0) (t) -:=X^
	(kabsto,			$,	1,0),	! (1 0) (t)
	(kinotto,			$,	1,0),	! (1 0) (t)
	(knotlto,			$,	1,0),	! (1 0) (t)
	(kistruelto,		$,	1,0),	! (1 0) (t)

!for conversions, t is always the current operand type
!u is the new type. However, for conversions involving widening, the result
!must end up at at least 64 bit. So i64->u8 masks to 8 bits, the sign-extends to u64

	(ktypepun,			$,	2,1),	! (1 1) (t,u)

	(ksoftconv,			$,	2,0),	! (1 1) (t,u) temporary opcode used internally

	(kfloat,			$,	2,0),	! (1 1) (t,u) X' := cast(X,t) Int u to real t
	(kfix,				$,	2,0),	! (1 1) (t,u) X' := cast(X,t) Real u to int t
	(ktruncate,			$,	2,0),	! (1 1) (t,u) X' := cast(X,u) Mask to width of u, but type is widened to i64/u64
	(kfwiden,			$,	2,0),	! (1 1) (t,u) X' := cast(X,u) r32 to r64
	(kfnarrow,			$,	2,0),	! (1 1) (t,u) X' := cast(X,u) r64 to r32

!These ones e currently still needed by or all PCL targets

	(kstartmx,			$,	0,0),
	(kresetmx,			$,	0,0),
	(kendmx,			$,	0,0),
	(ksetret,			$,	1,0),	! (0 0) (t) Set X as return value of type t
!	(ksetretmult,		$,	1,2),
	(ksetretmult,		$,	0,1), ! (0 0) (n) Set N return values
	(ksetcall,			$,	0,0), ! (nargs, nvars)
	(ksetarg,			$,	0,1), ! (nargs, nvars)
!	(ksetparam,			$,	1,0), ! (nargs, nvars)
!	(ksaveret,			$,	1,0), ! (t) Save D0/X0 value when local variants are freed
!	(krestoreret,		$,	1,0), ! (t) Restore D0/X0
	(kloadall,			$,	0,0), ! Load all pending pcl mem ops to regs or temps

!these are ecial ones used reflection

	(kgetnprocs,		$,	0,0), ! (0 1) X' := Get number of functions in function table
	(kgetprocname,		$,	0,0), ! (1 1) X' := Getprocname(X) Name of nth function (1-based)
	(kgetprocaddr,		$,	0,0), ! (1 1) X' := Getprocaddr(X) Addr of nth function (1-based)

!ops used ternally by M compiler until they can be replaced
!(usually they ll be turned into something else, constants etc)
	(keq,				$,	1,0),
	(kne,				$,	1,0),
	(klt,				$,	1,0),
	(kle,				$,	1,0),
	(kge,				$,	1,0),
	(kgt,				$,	1,0),

	(klen,				$,	0,0),
	(klwb,				$,	0,0),
	(kupb,				$,	0,0),
	(kbounds,			$,	0,0),
	(klenstr,			$,	0,0),
	(kbitwidth,			$,	0,0),
	(kbytesize,			$,	0,0),
	(kminvalue,			$,	0,0),
	(kmaxvalue,			$,	0,0),
	(ktypestr,			$,	0,0),
	(kerror,			$,	0,0),
	(kharderror,		$,	0,0),
	(karraytoslice,		$,	0,0),
	(kichartoslice,		$,	0,0),
	(ksofttruncshort,	$,	0,0),
	(kcharaxtoichar,	$,	0,0),
	(ksliceptr,			$,	0,0),

	(klast,				$,	0,0),	! (0 0)
end

!* LOCALS: all that have been USED
!* PARAMS: all including not used ones (but need the USED flag)
!* HIGHPARAM max param (of 0..4) that has been used, to determine
!  if any spare PREG exists
!* NCALLS all calls including those hidden inside POWER etc
!* HIGHARG max argument (of 0..4) of any calls.
!* ASSEMUSED
!* LEAFPROC
!* Need ADDROF flags for each LOCAL and PARAM
!* MAXREGVARS how many locals would qualify as regvars
!* MAXXREGVARS how many locals would qualify as xregvars
!* R3USED (see below)

!global symbol currproc
global ref pclrec pclprocdef	!ref to kprocdef opcode of currproc

global int frameoffset
global int paramoffset
global int framebytes

global const maxparams=32
global const maxlocals=256

!these are reset at each procdef
global [maxparams]symbol paramdefs
global [maxlocals]symbol localdefs
global int nlocals				!no. of locals that are used, both int/float
global int nparams				!no. of params whether used or not, int and float

global int usedparams			!how many of pregs needed for used incoming params
global int usedxparams			!how many of pxregs needed for used incoming params

global int highreg				!highest D-reg used
global int highxreg				!highest X-reg used
global int bspill, bxspill		!no. to spill
global int bxspilloffset		!base frame offset of bxreg spill area

global byte r10used				!these may be set in pass2 when occupied by params
global byte r11used
global byte r13used

global int maxregvars			!how many locals would qualify for regvars
global int maxxregvars			!how many locals would qualify for xregvars

global int nproccalls			!number of calls including implicit ones (eg. ** and sin)
global int highargs				!max number of args (0-4) of any call
global macro leafproc =	nproccalls=0			!1 if leaf function
global byte localshadow			!1 if local, proc-wide shadow space used for a call

global byte assemused			!1 if assem used

global int passno

