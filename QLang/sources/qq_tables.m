!!---
global enumdata	[0:]ichar stdtypenames,
					[0:]byte stdtypewidths =
!                   names       widths  V P M F
	(tvoid=0,		"void",			0),		! - means object is unassigned

! V Variant Types
	(tint,			"int",			64),	! - 64-bit signed int
	(treal,			"real",			64),	! - 64-bit float

	(tdecimal,		"decimal",		0),
	(trange,		"range",		64),	!
	(tset,			"set",			0),		! - Pascal-like bit-set (array of B)
	(tdict,			"dict",			0),		! - Dictionary of X:Y keys and values

	(tvector,		"vector",		0),		! - User-defined array
	(tbits,			"bits",			0),		! - Array of B

	(tstring,		"string",		0),		! - String of u8 elements
	(tlist,			"list",			0),		! - List of V
	(tarray,		"array",		0),		! - Array of T

	(trecord,		"record",		0),		! - Record of V
	(tstruct,		"struct",		0),		! - Record of T (contains tpackrecord instance)

	(trefvar,		"refvar",		64),	! - Pointer to V
	(trefbit,		"refbit",		128),	! - Pointer to B or bitfield
	(trefpack,		"refpack",		64),	! - Pounter to T

	(tsymbol,		"symbol",		64),	! - Named object
!	(tclosure,		"closure",		0),		! - Closure

	(ttype,			"type",			64),	! - Represents a type-code
	(toperator,		"operator",		64),	! - Represents an operator (as a bytecode op)
	(tretaddr,		"retaddr",		0),		! - Return address descriptor, only on stack 
	(texception,	"except",		0),		! - Exception descriptor, only on stack


! T Pack Types
	(ti8,			"i8",			8),		! - Narrow numeric types
	(ti16,			"i16",			16),
	(ti32,			"i32",			32),
	(ti64,			"i64",			64),
	(tu8,			"u8",			8),
	(tu16,			"u16",			16),
	(tu32,			"u32",			32),
	(tu64,			"u64",			64),
	(tr32,			"r32",			32),
	(tr64,			"r64",			64),

	(tu1,			"u1",			1),
	(tu2,			"u2",			2),
	(tu4,			"u4",			4),

	(tpackstrc,		"packstrc",		0),		! - counted string field (uses get/setfs) (placeholder)
	(tpackstrz,		"packstrz",		0),		! - zero-terminated string field (placeholder)

	(tstringz,		"stringz",		64),	! - Pointer to zero-terminated string
	(trefproc,		"refproc",		64),	! - Pointer to native function (placeholder)

	(tslice,		"slice",		0),		! - slice of T (placeholder)

end

global const tlast=stdtypenames.upb

global const tlastvartag	= trefbit

global enumdata [0:]ichar jtagnames,			! "jadd" etc
					[0:]u32 jshortnames,		! "+" etc, used in jeval()
					[0:]byte jflags,			! 0/1/2 = 0, 1 or 2 subtrees
					[0:]int16 jpclcodes,		! for arith, corresponding pcl opc
					[0:]byte jtocodes,	 		! for arith, corresponding jaddto op etc
					[0:]byte jhasvalue = 		! whether yields a value (0, 1 or 2=special)
	(jnone=0,			$,		0,		0,		0,			0,	0),	
	(jlabeldef,			$,		0,		1,		0,			0,	0),
	(jassign,			$,		0,		2,		0,			0,	2),
	(jdeepcopy,			$,		0,		2,		0,			0,	2),
	(jkeyword,			$,		0,		2,		0,			0,	1),
	(jkeyvalue,			$,		0,		2,		0,			0,	1),
	(joperator,			$,		0,		0,		0,			0,	1),
	(jblock,			$,		0,		1,		0,			0,	2),
	(jif,				$,		0,		2,		0,			0,	2),
	(jselect,			$,		0,		2,		0,			0,	2),
	(jwhenthen,			$,		0,		2,		0,			0,	0),
	(jcase,				$,		0,		2,		0,			0,	2),
	(jdocase,			$,		0,		2,		0,			0,	0),
	(jswitch,			$,		0,		2,		0,			0,	2),
	(jdoswitch,			$,		0,		2,		0,			0,	0),
	(jrecase,			$,		0,		1,		0,			0,	0),
	(jforup,			$,		0,		2,		0,			0,	0),
	(jforupx,			$,		0,		2,		0,			0,	0),
	(jfordown,			$,		0,		2,		0,			0,	0),
	(jfordownx,			$,		0,		2,		0,			0,	0),
	(jforall,			$,		0,		2,		0,			0,	0),
	(jforallrev,		$,		0,		2,		0,			0,	0),
	(jforeach,			$,		0,		2,		0,			0,	0),
	(jdo,				$,		0,		1,		0,			0,	0),
	(jto,				$,		0,		2,		0,			0,	0),
	(jwhile,			$,		0,		2,		0,			0,	0),
	(jrepeat,			$,		0,		2,		0,			0,	0),
	(jtry,				$,		0,		2,		0,			0,	0),
	(jexcept,			$,		0,		2,		0,			0,	0),
	(jraise,			$,		0,		1,		0,			0,	0),
	(jcall,				$,		0,		2,		0,			0,	1),
	(jcallhost,			$,		0,		1,		0,			0,	1),
	(jnil,				$,		0,		0,		0,			0,	1),
	(jswap,				$,		0,		2,		0,			0,	0),
	(jgoto,				$,		0,		1,		0,			0,	0),
	(jstop,				$,		0,		1,		0,			0,	0),
	(jreturn,			$,		0,		1,		0,			0,	2),
	(jtypeconst,		$,		0,		0,		0,			0,	1),
	(jeval,				$,		0,		1,		0,			0,	0),

	(jconvert,			$,		0,		1,		0,			0,	1),
	(jtypepun,			$,		0,		1,		0,			0,	1),
	(jmap,				$,		0,		2,		kmaps,		0,	1),

	(jcmpchain,			$,		0,		1,		0,			0,	1),
	(jname,				$,		0,		0,		0,			0,	1),
	(jsymbol,			$,		0,		1,		0,			0,	1),
	(jintconst,			$,		0,		0,		0,			0,	1),
	(jrealconst,		$,		0,		0,		0,			0,	1),
	(jstringconst,		$,		0,		0,		0,			0,	1),
	(jstrinclude,		$,		0,		1,		0,			0,	1),
	(jdot,				$,		0,		2,		0,			0,	1),
	(jindex,			$,		0,		2,		0,			0,	1),
	(jdotindex,			$,		0,		2,		0,			0,	1),
	(jkeyindex,			$,		0,		2,		0,			0,	1),
	(jredo,				$,		0,		2,		0,			0,	0),
	(jnext,				$,		0,		2,		0,			0,	0),
	(jexit,				$,		0,		2,		0,			0,	0),
	(jptr,				$,		0,		1,		0,			0,	1),
	(jaddrof,			$,		0,		1,		0,			0,	1),
	(jptrto,			$,		0,		1,		0,			0,	1),
!	(jdaddrof,			$,		0,		1,		0,			0,	1),
	(jnull,				$,		0,		0,		0,			0,	1),
	(jprint,			$,		0,		2,		0,			0,	0),
	(jprintln,			$,		0,		2,		0,			0,	0),
	(jfprint,			$,		0,		2,		0,			0,	0),
	(jfprintln,			$,		0,		2,		0,			0,	0),
	(jsprint,			$,		0,		2,		0,			0,	1),
	(jsfprint,			$,		0,		2,		0,			0,	1),
	(jnogap,			$,		0,		0,		0,			0,	0),
	(jspace,			$,		0,		0,		0,			0,	0),
	(jfmtitem,			$,		0,		2,		0,			0,	0),
	(jread,				$,		0,		2,		0,			0,	0),
	(jreadln,			$,		0,		2,		0,			0,	0),
	(jdecimal,			$,		0,		0,		0,			0,	1),
	(jincr,				$,		'++',		1,		0,			0,	1),
	(jdecr,				$,		'--',		1,		0,			0,	1),
	(jincrload,			$,		0,		1,		kincrload,	0,	1),
	(jdecrload,			$,		0,		1,		kdecrload,	0,	1),
	(jloadincr,			$,		0,		1,		kloadincr,	0,	1),
	(jloaddecr,			$,		0,		1,		kloaddecr,	0,	1),
	(jneg,				$,		'-',		1,		kneg,		jnegto,	1),
	(jabs,				$,		'abs',		1,		kabs,		jabsto,	1),
	(jnotl,				$,		'not',		1,		knotl,		jnotlto,	1),
	(jinot,				$,		'inot',		1,		kinot,		jinotto,	1),
	(jistruel,			$,		'isT',		1,		kistruel,	0,	1),
	(jasc,				$,		0,		1,		kasc,		0,	1),
	(jchr,				$,		0,		1,		kchr,		0,	1),
	(jsqrt,				$,		0,		1,		ksqrt,		0,	1),
	(jsqr,				$,		0,		1,		ksqr,		0,	1),
	(jsin,				$,		0,		1,		ksin,		0,	1),
	(jcos,				$,		0,		1,		kcos,		0,	1),
	(jtan,				$,		0,		1,		ktan,		0,	1),
	(jasin,				$,		0,		1,		kasin,		0,	1),
	(jacos,				$,		0,		1,		kacos,		0,	1),
	(jatan,				$,		0,		1,		katan,		0,	1),
	(jln,				$,		0,		1,		kln,		0,	1),
	(jlog,				$,		0,		1,		klog,		0,	1),
	(jlg,				$,		0,		1,		klg,		0,	1),
	(jexp,				$,		0,		1,		kexp,		0,	1),
	(jround,			$,		0,		1,		kround,		0,	1),
	(jfloor,			$,		0,		1,		kfloor,		0,	1),
	(jceil,				$,		0,		1,		kceil,		0,	1),
	(jfract,			$,		0,		2,		kfract,		0,	1),
	(jfmod,				$,		0,		2,		kfmod,		0,	1),
	(jsign,				$,		0,		1,		ksign,		0,	1),
	(jnegto,			$,		0,		1,		knegto,		0,	0),
	(jabsto,			$,		0,		1,		kabsto,		0,	0),
	(jnotlto,			$,		0,		1,		knotlto,	0,	0),
	(jinotto,			$,		0,		1,		kinotto,	0,	0),
	(jlen,				$,		0,		1,		klen,		0,	1),
	(jlwb,				$,		0,		1,		klwb,		0,	1),
	(jupb,				$,		0,		1,		kupb,		0,	1),
	(jbounds,			$,		0,		1,		kbounds,	0,	1),
	(jboundsx,			$,		0,		1,		kboundsx,	0,	1),
	(jbitwidth,			$,		0,		1,		kbitwidth,	0,	1),
	(jbytesize,			$,		0,		1,		kbytesize,	0,	1),
	(jtype,				$,		0,		1,		ktype,		0,	1),
	(jelemtype,			$,		0,		1,		kelemtype,	0,	1),
	(jbasetype,			$,		0,		1,		kbasetype,	0,	1),
	(jusertype,			$,		0,		1,		kusertype,	0,	1),
	(jdictitems,		$,		0,		1,		kdictitems,	0,	1),
	(jisfound,			$,		0,		1,		kisfound,	0,	1),
	(jminvalue,			$,		0,		1,		kminvalue,	0,	1),
	(jmaxvalue,			$,		0,		1,		kmaxvalue,	0,	1),
	(jisint,			$,		0,		1,		kisint,		0,	1),
	(jisreal,			$,		0,		1,		kisreal,	0,	1),
	(jisstring,			$,		0,		1,		kisstring,	0,	1),
	(jisrange,			$,		0,		1,		kisrange,	0,	1),
	(jisnumber,			$,		0,		1,		kisnumber,	0,	1),
	(jislist,			$,		0,		1,		kislist,	0,	1),
	(jisrecord,			$,		0,		1,		kisrecord,	0,	1),
	(jispointer,		$,		0,		1,		kispointer,	0,	1),
	(jisarray,			$,		0,		1,		kisarray,	0,	1),
	(jismutable,		$,		0,		1,		kismutable,	0,	1),
	(jisset,			$,		0,		1,		kisset,		0,	1),
	(jisvoid,			$,		0,		1,		kisvoid,	0,	1),
	(jisdef,			$,		0,		1,		kisdef,		0,	1),
	(jisequal,			$,		0,		2,		kisequal,	0,	1),
	(jodd,				$,		0,		2,		kodd,		0,	1),
	(jeven,				$,		0,		2,		keven,		0,	1),
	(jadd,				$,		'+',		2,		kadd,		jaddto,	1),
	(jsub,				$,		'-',		2,		ksub,		jsubto,	1),
	(jmul,				$,		'*',		2,		kmul,		jmulto,	1),
	(jdiv,				$,		'/',		2,		kdiv,		jdivto,	1),
	(jidiv,				$,		'%',		2,		kidiv,		jidivto,	1),
	(jirem,				$,		'rem',		2,		kirem,		0,	1),
	(jidivrem,			$,		0,		2,		kidivrem,	0,	1),
	(jiand,				$,		'iand',		2,		kiand,		jiandto,	1),
	(jior,				$,		'ior',		2,		kior,		jiorto,	1),
	(jixor,				$,		'ixor',		2,		kixor,		jixorto,	1),
	(jshl,				$,		'<<',		2,		kshl,		jshlto,	1),
	(jshr,				$,		'>>',		2,		kshr,		jshrto,	1),
	(jin,				$,		'in',		2,		kin,		0,	1),
	(jnotin,			$,		'~in',	2,		knotin,		0,	1),
	(jinx,				$,		'inx',		2,		kinx,		0,	1),
	(jinrev,			$,		0,		2,		0,			0,	0),
	(jandl,				$,		'and',		2,		0,		jandlto,	1),
	(jorl,				$,		'or',		2,		0,		jorlto,	1),
	(jeq,				$,		'=',		2,		keq,		0,	1),
	(jne,				$,		'<>',		2,		kne,		0,	1),
	(jlt,				$,		'<',		2,		klt,		0,	1),
	(jle,				$,		'<=',		2,		kle,		0,	1),
	(jge,				$,		'>=',		2,		kge,		0,	1),
	(jgt,				$,		'>',		2,		kgt,		0,	1),
	(jmin,				$,		'min',		2,		kmin,		jminto,	1),
	(jmax,				$,		'max',		2,		kmax,		jmaxto,	1),
	(jconcat,			$,		0,		2,		kconcat,	jconcatto,	1),
	(jappend,			$,		0,		2,		kappend,	jappendto,	1),
	(jpower,			$,		'**',		2,		kpower,		0,	1),
	(jatan2,			$,		0,		2,		katan2,		0,	1),
	(jaddto,			$,		0,		2,		kaddto,		0,	0),
	(jsubto,			$,		0,		2,		ksubto,		0,	0),
	(jmulto,			$,		0,		2,		kmulto,		0,	0),
	(jdivto,			$,		0,		2,		kdivto,		0,	0),
	(jidivto,			$,		0,		2,		kidivto,	0,	0),
	(jandlto,			$,		0,		2,		kandlto,	0,	0),
	(jorlto,			$,		0,		2,		korlto,		0,	0),
	(jiandto,			$,		0,		2,		kiandto,	0,	0),
	(jiorto,			$,		0,		2,		kiorto,		0,	0),
	(jixorto,			$,		0,		2,		kixorto,	0,	0),
	(jshlto,			$,		0,		2,		kshlto,		0,	0),
	(jshrto,			$,		0,		2,		kshrto,		0,	0),
	(jminto,			$,		0,		2,		kminto,		0,	0),
	(jmaxto,			$,		0,		2,		kmaxto,		0,	0),
	(jconcatto,			$,		0,		2,		kconcatto,	0,	0),
	(jappendto,			$,		0,		2,		kappendto,	0,	0),

	(jmakerange,		$,		0,		2,		kmakerange,	0,	1),
	(jmakerangelen,		$,		0,		2,		kmakerangelen,	0,	1),
	(jmakelist,			$,		0,		1,		kmakelist,	0,	1),
	(jmakeset,			$,		0,		1,		kmakeset,	0,	1),
	(jmakedict,			$,		0,		1,		kmakedict,	0,	1),

	(jcvlineno,			$,		0,		0,		0,	0,	1),
	(jcvstrlineno,		$,		0,		0,		0,	0,	1),
	(jcvmodulename,		$,		0,		0,		0,	0,	1),
	(jcvfilename,		$,		0,		0,		0,	0,	1),
	(jcvfunction,		$,		0,		0,		0,	0,	1),
	(jcvdate,			$,		0,		0,		0,	0,	1),
	(jcvtime,			$,		0,		0,		0,	0,	1),
	(jcvversion,		$,		0,		0,		0,	0,	1),
	(jcvpclversion,		$,		0,		0,		0,	0,	1),
end

global type qd=[4]byte

global enumdata [0:]ichar opndnames=
							!PCL1			PCL2
	(cnone=0,	$),
	(cmemory,	$),			!m Symbol		Address of static object
	(cframe,	$),			!f Symbol		Byte offset from Dframe
	(cproc,		$),			!p Symbol		Address of pccode entry point
	(cdllproc,	$),			!x Int			Int Index into dllproc table

	(cgenfield,	$),			!g Symbol		Index into genfieldtable

	(clabel,	$),			!l Label no		Address of pccode instruction
	(cint,		$),			!i
	(cword,		$),			!u
	(creal,		$),			!r
	(crange,	$),			!n
	(cstring,	$),			!s Stringz		Address of static Object with string
	(cstringz,	$),			!z Stringz
	(ctype,		$),			!t Typeno		Typeno
	(csymbol,	$),			!d Symbol		Symbol
	(coperator,	$),			!o Operator		Operator

	(clast,		"?")
end

!these aliases are used so that the cmdfmt table is tidier
const p = cproc
const m = cmemory
const f = cframe
const l = clabel
const x = cdllproc
const g = cgenfield
const i = cint
const u = cword
const r = creal
const n = crange
const s = cstring
const z = cstringz
const t = ctype
const d = csymbol
const o = coperator

!Stack operands labeled X,Y,Z:
!X		X is top of the stack (1 operand)
!X,Y	Y is top of the stack (2 operands)
!X,Y,Z	Z is top of the stack (3 operands)
!suffixes a,b,c help indicate which operand goes where:
!a		always top of the second
!b		always second from the top
!c		always third from the top
!So Xb and Ya when there are two operands; Y is on top
!flags abcB uses a '1' bit to indicate that .a, .b or .c contains a unit list

!! ----

global enumdata  [0:]ichar pclnames, [0:]qd pclfmt, [0:]REF PROC pclhandlers =
	(kzero=0,		$,	(0,0,0,0),	cast(kunimpl        )),
	(knop,			$,	(0,0,0,0),	cast(k_nop          )),		!simple nop
	(kskip,			$,	(0,0,0,0),	cast(kunimpl        )),		!ignore on pcl listing

	(kprocdef,		$,	(d,0,0,0),	cast(kunimpl        )),		!
	(kprocentry,	$,	(i,0,0,0),	cast(k_procentry    )),		!A=number of locals; 
	(kprocend,		$,	(0,0,0,0),	cast(kunimpl        )),
	(kendmodule,	$,	(0,0,0,0),	cast(kunimpl        )),		!Last 'executable' opcode
	(kcomment,		$,	(z,0,0,0),	cast(k_comment      )),

	(klabeldef,		"",	(d,0,0,0),	cast(kunimpl        )),		!

	(kpushm,		$,	(m,0,0,0),	cast(k_pushm        )),		!Push [A]
	(kpushf,		$,	(f,0,0,0),	cast(k_pushf        )),		!Push [A]
	(kpushmref,		$,	(m,0,0,0),	cast(k_pushmref     )),		!push &A
	(kpushfref,		$,	(f,0,0,0),	cast(k_pushfref     )),		!push &A
	(kpopm,			$,	(m,0,0,0),	cast(k_popm         )),		!A:=Xa
	(kpopf,			$,	(f,0,0,0),	cast(k_popf         )),		!A:=Xa
	(kstorem,		$,	(m,0,0,0),	cast(k_storem       )),		!A:=Xa
	(kstoref,		$,	(f,0,0,0),	cast(k_storef       )),		!A:=Xa
	(kpushx,		$,	(d,0,0,0),	cast(kunimpl        )),		!Push [A]	DLL VARs
	(kpopx,			$,	(d,0,0,0),	cast(kunimpl        )),		!Pop [A]

	(kpushci,		$,	(i,0,0,0),	cast(k_pushci       )),		!Push constant signed int
	(kpushvoid,		$,	(0,0,0,0),	cast(k_pushvoid     )),		!
	(kpushnil,		$,	(0,0,0,0),	cast(k_pushnil      )),		!
	(kpushcr,		$,	(r,0,0,0),	cast(k_pushcr       )),		!Push constant real
	(kpushcn,		$,	(n,0,0,0),	cast(kunimpl        )),		!Push range

	(kpushcs,		$,	(s,0,0,0),	cast(k_pushcs       )),		!Push constant string object

	(kpusht,		$,	(t,0,0,0),	cast(k_pusht        )),		!Push type constant
	(kpushsymbol,	$,	(d,0,0,0),	cast(k_pushsymbol   )),		!Push symbol reference
	(kpushoperator,	$,	(o,0,0,0),	cast(k_pushoperator )),		!Push operator code (pcl code)

	(kpushptr,		$,	(0,0,0,0),	cast(k_pushptr      )),		!Push Xa^
	(kpopptr,		$,	(0,0,0,0),	cast(k_popptr       )),		!Ya^:=Xb; then pop both

	(kzpopm,		$,	(m,0,0,0),	cast(k_zpopm        )),		!Pop A; do not free A first
	(kzpopf,		$,	(f,0,0,0),	cast(k_zpopf        )),		!Pop A; do not free A first

	(kdupl,			$,	(0,0,0,0),	cast(k_dupl         )),		!Xa:=share(Xa), keep original on stack
	(kcopy,			$,	(0,0,0,0),	cast(k_copy         )),		!Xa:=deepcopy(Xa)
	(kswap,			$,	(0,0,0,0),	cast(k_swap         )),		!Yb^:=:Xa^; Xa^:=:A; A:=:B

	(kconvrefpack,	$,	(0,0,0,0),	cast(k_convrefpack  )),		!Change ref in X to refpacked

	(kjump,			$,	(l,0,0,0),	cast(k_jump         )),		!Jump to L
	(kjumpptr,		$,	(0,0,0,0),	cast(k_jumpptr      )),		!Jump to Xa^

	(kjumptrue,		$,	(l,0,0,0),	cast(k_jumptrue     )),		!Jump to L when Xa is true
	(kjumpfalse,	$,	(l,0,0,0),	cast(k_jumpfalse    )),		!Jump to L when Xa is false

	(kjumpeq,		$,	(l,0,0,0),	cast(k_jumpeq       )),		!Jump to L when Xb=Ya, Xa=A, A=B; (X,Y popped)
	(kjumpne,		$,	(l,0,0,0),	cast(k_jumpne       )),		!Jump to L when Xb<>Ya
	(kjumplt,		$,	(l,0,0,0),	cast(k_jumplt       )),		!Jump to L when Xb<Ya
	(kjumple,		$,	(l,0,0,0),	cast(k_jumple       )),		!Jump to L when Xb<=Ya
	(kjumpge,		$,	(l,0,0,0),	cast(k_jumpge       )),		!Jump to L when Xb>=Ya
	(kjumpgt,		$,	(l,0,0,0),	cast(k_jumpgt       )),		!Jump to L when Xb>Ya

	(kjumptesteq,	$,	(l,0,0,0),	cast(k_jumptesteq   )),		!Jump to L when Xb=Ya (Ya popped), or Xa=A; int/set and int/range use 'in' to compare
	(kjumptestne,	$,	(l,0,0,0),	cast(k_jumptestne   )),		!Jump to L when Xb<>Ya

	(kjumplabel,	$,	(l,0,0,0),	cast(kunimpl        )),		!Jumptable entry

	(kswitch,		$,	(i,i,0,0),	cast(k_switch       )),		!Jumptable has n entries, ci is lower bound. Jump indexed by Xa

	(ktom,			$,	(l,m,0,0),	cast(k_tom          )),		!
	(ktof,			$,	(l,f,0,0),	cast(k_tof          )),		!

	(kformci,		$,	(l,m,i,0),	cast(k_formci       )),		!
	(kforfci,		$,	(l,f,i,0),	cast(k_forfci       )),		!
	(kformm,		$,	(l,m,m,0),	cast(k_formm        )),		!
	(kforff,		$,	(l,f,f,0),	cast(k_forff        )),		!

	(kfordmci,		$,	(l,m,i,0),	cast(k_fordmci      )),		!
	(kfordfci,		$,	(l,f,i,0),	cast(k_fordfci      )),		!
	(kfordmm,		$,	(l,m,m,0),	cast(k_fordmm       )),		!
	(kfordff,		$,	(l,f,f,0),	cast(k_fordff       )),		!

	(kcallproc,		$,	(p,i,0,0),	cast(k_callproc     )),		!Call &A; A is cmemoryref; B is no. args
	(kcallptr,		$,	(i,i,0,0),	cast(k_callptr      )),		!Call X^; A is no. of params supplied; B is stack adjust
	(kreturn0,		$,	(0,0,0,0),	cast(k_return0      )),		!A is no. params to free; Return from function, with optional value in caller's retval slot
	(kreturn,		$,	(i,0,0,0),	cast(k_return       )),		!A is no. params to free; Return from function, with optional value in caller's retval slot
	(kpopretval,	$,	(i,0,0,0),	cast(k_popretval    )),		!pop stack to caller's return slot; i=offset

	(kmodulecall,	$,	(d,0,0,0),	cast(k_modulecall   )),		!
	(kmodulereturn,	$,	(0,0,0,0),	cast(k_modulereturn )),		!

	(kcalldll,		$,	(x,i,0,0),	cast(k_calldll      )),		!Call dll function A (sysmbol); B=nargs

	(kcallhost,		$,	(i,0,0,0),	cast(k_callhost     )),		!Call Q host function A (Host index)

	(kunshare,		$,	(i,0,0,0),	cast(k_unshare      )),		!Unshare and pop A var values on stack
	(kaddsp,		$,	(i,0,0,0),	cast(k_addsp        )),		!SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

	(kstop,			$,	(0,0,0,0),	cast(k_stop         )),		!Stop program and return value X to any calling program
	(kstoprunproc,	$,	(0,0,0,0),	cast(k_stoprunproc  )),		!Used for reentrant callback calls

	(kmakelist,		$,	(i,i,0,0),	cast(k_makelist     )),		!A items on stack; make list with lwb B
	(kmakerecord,	$,	(i,t,0,0),	cast(k_makerecord   )),		!A items on stack; make record of type B
	(kmakearray,	$,	(i,i,t,t),	cast(k_makearray    )),		!A items on stack; make array with lwb B, type C and elemtype D
	(kmakebits,		$,	(i,i,t,t),	cast(k_makebits     )),		!A items on stack; make bits with lwb B, type C and elemtype D
	(kmakestruct,	$,	(i,t,0,0),	cast(k_makestruct   )),		!A items on stack; make struct with type B
	(kmakeset,		$,	(i,0,0,0),	cast(k_makeset      )),		!A items on stack; make set
	(kmakerange,	$,	(0,0,0,0),	cast(k_makerange    )),		!2 items on stack; make range
	(kmakerangelen,	$,	(0,0,0,0),	cast(k_makerangelen )),		!2 items on stack; make range; 2nd is length
	(kmakedict,		$,	(i,0,0,0),	cast(k_makedict     )),		!A*2 items on stack (A key:val items); make dict
	(kmakedecimal,	$,	(0,0,0,0),	cast(k_makedecimal  )),		!Turn string on stack to decimal number

	(kincrptr,		$,	(0,0,0,0),	cast(k_incrptr      )),		!++Xa^
	(kincrtom,		$,	(m,0,0,0),	cast(k_incrtom      )),		!++A
	(kincrtof,		$,	(f,0,0,0),	cast(k_incrtof      )),		!++A
	(kloadincr,		$,	(0,0,0,0),	cast(k_loadincr     )),		!T:=Xa^++
	(kincrload,		$,	(0,0,0,0),	cast(k_incrload     )),		!T:=--Xa^

	(kdecrptr,		$,	(0,0,0,0),	cast(k_decrptr      )),		!--Xa^; pop X
	(kdecrtom,		$,	(m,0,0,0),	cast(k_decrtom      )),		!--A
	(kdecrtof,		$,	(f,0,0,0),	cast(k_decrtof      )),		!--A
	(kloaddecr,		$,	(0,0,0,0),	cast(k_loaddecr     )),		!T:=Xa^--
	(kdecrload,		$,	(0,0,0,0),	cast(k_decrload     )),		!T:=--Xa^

	(kincr,			$,	(0,0,0,0),	cast(k_incr         )),		!T:=++T
	(kdecr,			$,	(0,0,0,0),	cast(k_decr         )),		!T:=--T

	(kneg,			$,	(0,0,0,0),	cast(k_neg          )),		!T:=-Xa; T:=-A
	(kabs,			$,	(0,0,0,0),	cast(k_abs          )),		!abs Xa
	(knotl,			$,	(0,0,0,0),	cast(k_notl         )),		!not Xa
	(kinot,			$,	(0,0,0,0),	cast(k_inot         )),		!inot Xa
	(kistruel,		$,	(0,0,0,0),	cast(k_istruel      )),		!istrue Xa
	(kasc,			$,	(0,0,0,0),	cast(k_asc          )),		!asc Xa
	(kchr,			$,	(0,0,0,0),	cast(k_chr          )),		!chr Xa

	(ksqrt,			$,	(0,0,0,0),	cast(k_sqrt         )),		!sqrt Xa
	(ksqr,			$,	(0,0,0,0),	cast(k_sqr          )),		!sqr Xa
	(ksin,			$,	(0,0,0,0),	cast(k_sin          )),		!sin Xa
	(kcos,			$,	(0,0,0,0),	cast(k_cos          )),		!cos Xa
	(ktan,			$,	(0,0,0,0),	cast(k_tan          )),		!tan Xa
	(kasin,			$,	(0,0,0,0),	cast(k_asin         )),		!asin Xa
	(kacos,			$,	(0,0,0,0),	cast(k_acos         )),		!acos Xa
	(katan,			$,	(0,0,0,0),	cast(k_atan         )),		!atan Xa
	(ksign,			$,	(0,0,0,0),	cast(k_sign         )),		!sign Xa
	(kln,			$,	(0,0,0,0),	cast(k_ln           )),		!ln Xa
	(klog,			$,	(0,0,0,0),	cast(k_log          )),		!log Xa
	(klg,			$,	(0,0,0,0),	cast(k_lg           )),		!lg Xa
	(kexp,			$,	(0,0,0,0),	cast(k_exp          )),		!exp Xa
	(kround,		$,	(0,0,0,0),	cast(k_round        )),		!round Xa
	(kfloor,		$,	(0,0,0,0),	cast(k_floor        )),		!floor Xa
	(kceil,			$,	(0,0,0,0),	cast(k_ceil         )),		!ceil Xa
	(kfract,		$,	(0,0,0,0),	cast(k_fract        )),		!fract Xa
	(kfmod,			$,	(0,0,0,0),	cast(k_fmod         )),		!fmod(Xb, Ya)

	(knegto,		$,	(0,0,0,0),	cast(k_negto        )),		!-:=Xa^; -:=A
	(kabsto,		$,	(0,0,0,0),	cast(k_absto        )),		!abs:=^Xa; pop Xa
	(kinotto,		$,	(0,0,0,0),	cast(k_inotto       )),		!inot:=Xa^; pop Xa
	(knotlto,		$,	(0,0,0,0),	cast(k_notlto       )),		!not:=Xa^; pop Xa

	(klen,			$,	(0,0,0,0),	cast(k_len          )),		!T:=Xa.len
	(klwb,			$,	(0,0,0,0),	cast(k_lwb          )),		!Xa.lwb
	(kupb,			$,	(0,0,0,0),	cast(k_upb          )),		!Xa.upb
	(kbounds,		$,	(0,0,0,0),	cast(k_bounds       )),		!Xa.bounds (as one range value)
	(kboundsx,		$,	(0,0,0,0),	cast(k_boundsx      )),		!Xa.bounds (as two ints)
	(kbitwidth,		$,	(0,0,0,0),	cast(k_bitwidth     )),		!Xa.bitwidth
	(kbytesize,		$,	(0,0,0,0),	cast(k_bytesize     )),		!Xa.bytesize
	(ktype,			$,	(0,0,0,0),	cast(k_type         )),		!Xa.type
	(kelemtype,		$,	(0,0,0,0),	cast(k_elemtype     )),		!Xa.elemtag
	(kbasetype,		$,	(0,0,0,0),	cast(k_basetype     )),		!Xa.basetype
	(kusertype,		$,	(0,0,0,0),	cast(k_usertype     )),		!Xa.usertype
	(kdictitems,	$,	(0,0,0,0),	cast(k_dictitems    )),		!Xa.dictitems
	(kisfound,		$,	(0,0,0,0),	cast(k_isfound      )),		!Xa.isfound
	(kminvalue,		$,	(0,0,0,0),	cast(k_minvalue     )),		!Xa.minvalue
	(kmaxvalue,		$,	(0,0,0,0),	cast(k_maxvalue     )),		!Xa.maxvalue
	(kisint,		$,	(0,0,0,0),	cast(k_isint        )),		!Xa.isint
	(kisreal,		$,	(0,0,0,0),	cast(k_isreal       )),		!Xa.isreal
	(kisstring,		$,	(0,0,0,0),	cast(k_isstring     )),		!Xa.isstring
	(kisrange,		$,	(0,0,0,0),	cast(k_isrange      )),		!Xa.isrange
	(kisnumber,		$,	(0,0,0,0),	cast(k_isnumber     )),		!Xa.isnumber
	(kislist,		$,	(0,0,0,0),	cast(k_islist       )),		!Xa.isarray
	(kisrecord,		$,	(0,0,0,0),	cast(k_isrecord     )),		!Xa.isrecord
	(kispointer,	$,	(0,0,0,0),	cast(k_ispointer    )),		!Xa.ispointer
	(kisarray,		$,	(0,0,0,0),	cast(k_isarray      )),		!Xa.isarray
	(kismutable,	$,	(0,0,0,0),	cast(k_ismutable    )),		!Xa.ismutable
	(kisset,		$,	(0,0,0,0),	cast(k_isset        )),		!Xa.isset
	(kisvoid,		$,	(0,0,0,0),	cast(k_isvoid       )),		!Xa.isvoid
	(kisdef,		$,	(0,0,0,0),	cast(k_isdef        )),		!Xa.isdef
	(kisequal,		$,	(0,0,0,0),	cast(k_isequal      )),		!Xb==Ya
	(kconvert,		$,	(t,0,0,0),	cast(k_convert      )),		!Xa==A(Xa)
	(ktypepun,		$,	(t,0,0,0),	cast(k_typepun      )),		!Xa==A@(Xa)
	(kodd,			$,	(0,0,0,0),	cast(k_odd          )),		!Xa==Xa.odd
	(keven,			$,	(0,0,0,0),	cast(k_even         )),		!Xa==Xa.even

	(kadd,			$,	(0,0,0,0),	cast(k_add          )),		!T:=Xb+Ya
	(ksub,			$,	(0,0,0,0),	cast(k_sub          )),		!Xb-Ya
	(kmul,			$,	(0,0,0,0),	cast(k_mul          )),		!Xb*Ya
	(kdiv,			$,	(0,0,0,0),	cast(k_div          )),		!Xb/Ya
	(kidiv,			$,	(0,0,0,0),	cast(k_idiv         )),		!Xb%Ya
	(kirem,			$,	(0,0,0,0),	cast(k_irem         )),		!Xb rem Ya
	(kidivrem,		$,	(0,0,0,0),	cast(k_idivrem      )),		!Xb divrem Ya
	(kiand,			$,	(0,0,0,0),	cast(k_iand         )),		!Xb iand Ya
	(kior,			$,	(0,0,0,0),	cast(k_ior          )),		!Xb ior Ya
	(kixor,			$,	(0,0,0,0),	cast(k_ixor         )),		!Xb ixor Ya
	(kshl,			$,	(0,0,0,0),	cast(k_shl          )),		!Xb shl Ya
	(kshr,			$,	(0,0,0,0),	cast(k_shr          )),		!Xb shr Ya
	(kin,			$,	(0,0,0,0),	cast(k_in           )),		!Xb in Ya
	(knotin,		$,	(0,0,0,0),	cast(k_notin        )),		!Xb notin Ya
	(kinx,			$,	(0,0,0,0),	cast(k_inx          )),		!Xb inx Ya
	(keq,			$,	(0,0,0,0),	cast(k_eq           )),		!Xb=Ya
	(kne,			$,	(0,0,0,0),	cast(k_ne           )),		!Xb<>Ya
	(klt,			$,	(0,0,0,0),	cast(k_lt           )),		!Xb<Ya
	(kle,			$,	(0,0,0,0),	cast(k_le           )),		!Xb<=Ya
	(kge,			$,	(0,0,0,0),	cast(k_ge           )),		!Xb>=Ya
	(kgt,			$,	(0,0,0,0),	cast(k_gt           )),		!Xb>Ya
	(kmin,			$,	(0,0,0,0),	cast(k_min          )),		!Xb min Ya
	(kmax,			$,	(0,0,0,0),	cast(k_max          )),		!Xb max Ya
	(kconcat,		$,	(0,0,0,0),	cast(k_concat       )),		!Xb concat Ya
	(kappend,		$,	(0,0,0,0),	cast(k_append       )),		!Xb append Ya

	(kpower,		$,	(0,0,0,0),	cast(k_power        )),		!Xb power Ya
	(katan2,		$,	(0,0,0,0),	cast(k_atan2        )),		!Xb atan2 Ya

	(kaddto,		$,	(0,0,0,0),	cast(k_addto        )),		!Xb^+:=Y or Xa^+:=A or A+:=B
	(ksubto,		$,	(0,0,0,0),	cast(k_subto        )),		!Xb^-:=Ya
	(kmulto,		$,	(0,0,0,0),	cast(k_multo        )),		!Xb^*:=Ya
	(kdivto,		$,	(0,0,0,0),	cast(k_divto        )),		!Xb^/:=Ya
	(kidivto,		$,	(0,0,0,0),	cast(k_idivto       )),		!Xb^%:=Ya

	(kandlto,		$,	(0,0,0,0),	cast(k_andlto       )),		!Xb^ and:=Ya
	(korlto,		$,	(0,0,0,0),	cast(k_orlto        )),		!Xb^ or:=Ya
	(kiandto,		$,	(0,0,0,0),	cast(k_iandto       )),		!Xb^ iand:=Ya
	(kiorto,		$,	(0,0,0,0),	cast(k_iorto        )),		!Xb^ ior:=Ya
	(kixorto,		$,	(0,0,0,0),	cast(k_ixorto       )),		!Xb^ ixor:=Ya
	(kshlto,		$,	(0,0,0,0),	cast(k_shlto        )),		!Xb^ shl:=Ya
	(kshrto,		$,	(0,0,0,0),	cast(k_shrto        )),		!Xb^ shr:=Ya
	(kminto,		$,	(0,0,0,0),	cast(k_minto        )),		!Xb^ min:=Ya
	(kmaxto,		$,	(0,0,0,0),	cast(k_maxto        )),		!Xb^ max:=Ya
	(kconcatto,		$,	(0,0,0,0),	cast(k_concatto     )),		!Xb^ concat:=Ya
	(kappendto,		$,	(0,0,0,0),	cast(k_appendto     )),		!Xb^ concat:=Ya

	(kdot,			$,	(g,0,0,0),	cast(k_dot          )),		!T:=Xa.A
	(kindex,		$,	(0,0,0,0),	cast(k_index        )),		!T:=Xb[Ya]
	(kdotindex,		$,	(0,0,0,0),	cast(k_dotindex     )),		!T:=Xb.[Ya]
	(kkeyindex,		$,	(0,0,0,0),	cast(k_keyindex     )),		!T:=Xc{Yb,Za}

	(kdotref,		$,	(g,0,0,0),	cast(k_dotref       )),		!T:=&Xa.A
	(kindexref,		$,	(0,0,0,0),	cast(k_indexref     )),		!T:=&Xb[Ya]
	(kdotindexref,	$,	(0,0,0,0),	cast(k_dotindexref  )),		!T:=&Xb.[Ya]
	(kkeyindexref,	$,	(0,0,0,0),	cast(k_keyindexref  )),		!T:=&Xc{Ya,Za}

	(kpopdot,		$,	(g,0,0,0),	cast(k_popdot       )),		!Ya.A:=Xb
	(kpopindex,		$,	(0,0,0,0),	cast(k_popindex     )),		!Yb[Za]:=Xc
	(kpopdotindex,	$,	(0,0,0,0),	cast(k_popdotindex  )),		!Yb.[Za]:=Xc
	(kpopkeyindex,	$,	(0,0,0,0),	cast(k_popkeyindex  )),		!Yb{Za}:=Xc

	(kexpand,		$,	(i,0,0,0),	cast(k_expand       )),		!Expand Xa when A objects are needed

	(kpushtry,		$,	(l,i,i,0),	cast(k_pushtry      )),		!Push try/except into; label/except code/no. exceptions
	(kraise,		$,	(0,0,0,0),	cast(k_raise        )),		!Raise exception Xa
	(kmaps,			$,	(0,0,0,0),	cast(k_maps         )),		!Xa:=map(Xb,Ya)
	(kmapss,		$,	(0,0,0,0),	cast(k_mapss        )),		!Xa:=map(Xc,Yb,Za)

!Special composite opcodes used in asm optimiser

	(kpushff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kpushmm,		$,	(m,m,0,0),	cast(nil)             ),		!
	(kpushfm,		$,	(f,m,0,0),	cast(nil)             ),		!
	(kpushmf,		$,	(m,f,0,0),	cast(nil)             ),		!

	(kmoveff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kzmoveff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kmovefm,		$,	(f,m,0,0),	cast(nil)             ),		!
	(kmovemf,		$,	(m,f,0,0),	cast(nil)             ),		!
	(kmovemm,		$,	(m,m,0,0),	cast(nil)             ),		!

	(kmovefci,		$,	(f,i,0,0),	cast(nil)             ),		!
	(kzmovefci,		$,	(f,i,0,0),	cast(nil)             ),		!
	(kmovemci,		$,	(m,i,0,0),	cast(nil)             ),		!

	(kpushfff,		$,	(f,f,f,0),	cast(nil)             ),		!
	(knop2,			$,	(i,0,0,0),	cast(nil)             ),		!
	(kpushci0,		$,	(i,0,0,0),	cast(nil)             ),		!
	(kpushvoid2,	$,	(0,0,0,0),	cast(nil)             ),		!
	(kpushvoid3,	$,	(0,0,0,0),	cast(nil)             ),		!

	(kunshare1,		$,	(0,0,0,0),	cast(nil)             ),		!
	(kunshare2,		$,	(0,0,0,0),	cast(nil)             ),		!
	(kunshare3,		$,	(0,0,0,0),	cast(nil)             ),		!
	(kprocentry1,	$,	(0,0,0,0),	cast(nil)             ),		!
	(kprocentry2,	$,	(0,0,0,0),	cast(nil)             ),		!

	(kjumpeqfci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpnefci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpltfci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumplefci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpgefci,	$,	(l,f,i,0),	cast(nil)             ),		!
	(kjumpgtfci,	$,	(l,f,i,0),	cast(nil)             ),		!

	(kjumpeqff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpneff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpltff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpleff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpgeff,		$,	(l,f,f,0),	cast(nil)             ),		!
	(kjumpgtff,		$,	(l,f,f,0),	cast(nil)             ),		!

	(kaddfci,		$,	(f,i,0,0),	cast(nil)             ),		!
	(ksubfci,		$,	(f,i,0,0),	cast(nil)             ),		!

	(kaddff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(ksubff,		$,	(f,f,0,0),	cast(nil)             ),		!
	(kindexff,		$,	(f,f,0,0),	cast(nil)             ),		!

	(kpushincrptrm,	$,	(m,0,0,0),	cast(nil)             ),		!
	(kpushincrptrf,	$,	(f,0,0,0),	cast(nil)             ),		!
	(kpopincrptrm,	$,	(m,0,0,0),	cast(nil)             ),		!
	(kpopincrptrf,	$,	(f,0,0,0),	cast(nil)             ),		!

	(kswitchf,		$,	(f,i,i,0),	cast(nil)             ),		!
	(klenf,			$,	(f,0,0,0),	cast(nil)             ),		!
	(kpushptrf,		$,	(f,0,0,0),	cast(nil)             ),		!

	(klastpcl,		$,	(0,0,0,0),	cast(nil)             )
end

global [0..klastpcl]ref void cmdmap			!map cmd index to possible fn/label address

global enumdata []u64 symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			'error'),		! Lex error
	(dotsym,			'dot'),		! "."
	(lexdotsym,			'lexdot'),		! ".", used at bol to prefix lexical 
	(commasym,			'comma'),		! ","
	(semisym,			'semi'),		! ";"
	(colonsym,			'colon'),		! ":"
	(dcolonsym,			'dcolon'),		! "::"
	(assignsym,			'assign'),		! :=
	(deepcopysym,		'deepcopy'),		! ::=
	(sendtosym,			'sendto'),		! =>
	(pipesym,			'pipe'),		! ->
	(lbracksym,			'lbrack'),		! (
	(rbracksym,			'rbrack'),		! )
	(lsqsym,			'lsq'),		! [
	(rsqsym,			'rsq'),		! ]
	(lcurlysym,			'lcurly'),		! {
	(rcurlysym,			'rcurly'),		! }
	(ptrsym,			'ptr'),		! ^
	(barsym,			'bar'),		! |
	(atsym,				'at'),		! @
	(questionsym,		'question'),		! ?
	(addrsym,			'addr'),		! &
	(daddrsym,			'daddr'),		! &&
!	(curlsym,			'curl'),		! ~
	(rangesym,			'range'),		! ..
	(ellipsissym,		'ellipsis'),		! ...
	(hashsym,			'hash'),		! #

	(addsym,			'add'),		! +
	(subsym,			'sub'),		! -
	(mulsym,			'mul'),		! *
	(divsym,			'div'),		! /
	(idivsym,			'idiv'),		! %
	(iremsym,			'irem'),		! rem
	(idivremsym,		'idivrem'),		! divrem
	(andlsym,			'andl'),		! and
	(orlsym,			'orl'),		! or
	(iandsym,			'iand'),		! iand
	(iorsym,			'ior'),		! ior
	(ixorsym,			'ixor'),		! xor
	(shlsym,			'shl'),		! <<
	(shrsym,			'shr'),		! >>

	(minsym,			'min'),		! min
	(maxsym,			'max'),		! max
	(appendsym,			'append'),		! append
	(concatsym,			'concat'),		! concat
	(insym,				'in'),		! in
	(notinsym,			'notin'),		! notin
	(inxsym,			'inx'),		! inx
	(inrevsym,			'inrev'),		! inrevsym
	(powersym,			'power'),		! **

	(eqsym,				'eq'),		! =
	(nesym,				'ne'),		! <>
	(ltsym,				'lt'),		! <
	(lesym,				'le'),		! <=
	(gesym,				'ge'),		! >=
	(gtsym,				'gt'),		! >
	(isequalsym,		'isequal'),		! ==

	(notlsym,			'notl'),		! not
	(inotsym,			'inot'),		! inot
	(istruelsym,		'istruel'),		! istrue
	(abssym,			'abs'),		! abs
	(sqrsym,			'sqr'),		! sqr
	(signsym,			'sign'),		! sign
	(ascsym,			'asc'),		! asc
	(chrsym,			'chr'),		! chr

	(mathssym,			'maths'),		! sin etc
	(maths2sym,			'maths2'),		! atan2 etc
	(propsym,			'prop'),		! len etc

	(incrsym,			'incr'),		! -
	(decrsym,			'decr'),		! -

	(eolsym,			'eol'),		! End of line
	(eofsym,			'eof'),		! Eof seen
	(rawnamesym,		'rawname'),		! unassigned name before lookup
	(intconstsym,		'intconst'),		! 123 32 bits signed
	(decimalconstsym,	'decconst'),		! 123 or 123.4 decimal
	(realconstsym,		'fpconst'),		! 123.4 64 bits
	(charconstsym,		'chrconst'),		! 'A' or 'ABCD'
	(stringconstsym,	'strconst'),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		'unitname'),		! 
	(namesym,			'name'),		! identifier symbol

	(stdtypesym,		'stdtype'),		! INT, CHAR etc
	(kicharsym,			'ichar'),		! ICHAR
	(kifsym,			'if'),		! 
	(kthensym,			'then'),		! 
	(kelsifsym,			'elsif'),		! 
	(kelsesym,			'else'),		! 
	(kelsecasesym,		'elsecase'),		! 
	(kelseswitchsym,	'elsesw'),		! 
	(kelseselectsym,	'elsesel'),		! 
	(kendsym,			'end'),		! 
	(kunlesssym,		'unless'),		! 
	(kcasesym,			'case'),		! CASE
	(kdocasesym,		'docase'),		! DOCASE
	(krecasesym,		'recase'),		! RECASE
	(kwhensym,			'when'),		! 
	(kforsym,			'for'),		! 
	(ktosym,			'to'),		! TO/DOWNTO
	(kbysym,			'by'),		! 
	(kdosym,			'do'),		! 
	(kwhilesym,			'while'),		! 
	(krepeatsym,		'repeat'),		! 
	(kuntilsym,			'until'),		! 
	(kreturnsym,		'return'),		! 
	(kstopsym,			'stop'),		! 
	(kloopsym,			'loop'),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			'goto'),		! GO/GOTO
	(kswitchsym,		'switch'),		! SWITCH
	(kdoswitchsym,		'doswitch'),		! DOSWITCH
	(kprintsym,			'print'),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		'sprint'),		! SPRINT/SFPRINT
	(kreadsym,			'read'),		! READ/READLN
	(ksreadsym,			'sread'),		! SREAD
	(ksreadlnsym,		'sreadln'),		! SREADLN
	(kprocsym,			'proc'),		! PROC
	(kfunctionsym,		'func'),		! FUNCTION
	(klabelsym,			'label'),		! LABEL
	(krecordsym,		'record'),		! RECORD
	(kstructsym,		'struct'),		! STRUCT
	(kunionsym,			'union'),		! UNION
	(kmodulesym,		'module'),		!
	(kimportsym,		'import'),		!
	(kimportdllsym,		'importd'),		! IMPORTDLL
	(ktypesym,			'type'),		! TYPE
	(krefsym,			'ref'),		! REF
	(kvarsym,			'var'),		! VAR
	(kslicesym,			'slice'),		! SLICE
	(kmacrosym,			'macro'),		! MACRO
	(koperatorsym,		'op'),		! OPERATOR
	(kconstsym,			'const'),		! 
	(kfflangsym,		'fflang'),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		'global'),		! global
	(kstaticsym,		'static'),		! STATIC
	(kcalignedsym,		'calign'),		! $CALIGNED

	(ktrysym,			'try'),		! 
	(kexceptsym,		'except'),		! 
	(kraisesym,			'raise'),		! 
	(kextendsym,		'extend'),		!
	(kblocksym,			'block'),		!
	(kcastsym,			'cast'),		! CAST
	(compilervarsym,	'compvar'),		! $lineno etc
	(dollarsym,			'dollar'),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			'eval'),		! EVAL
	(ktabledatasym,		'tabdata'),		! tabledata
	(kmapsym,			'map'),		! MAP
	(kclampsym,			'clamp'),		! CLAMP
	(kswapsym,			'swap'),		! SWAP
	(sysconstsym,		'sysconst'),		! nil, etc
	(khostfnsym,		'hostfn'),		! LEFT, CONVLC etc
	(khostsym,			'host'),		! HOST
	(knilsym,			'nil'),		! NIL/PNIL
	(kstrincludesym,	'strincl'),		! STRINCLUDE
	(specialopsym,		'specop'),		! $NEG, $INDEX etc
	(kdummysym,			'dummy')		!
end

global enumdata =
	pi_const,
	tab_const,
	con_const,
	true_const,
	false_const
end

global enumdata =
	thousand_unit,
	million_unit,
	billion_unit
end

global enumdata [0:]ichar namenames =
	(genericid=0,	$),		! - 		Generic name, not yet resolved
	(programid,		$),		!
	(subprogid,		$),		!
	(moduleid,		$),		!
	(dllmoduleid,	$),		!
	(procid,		$),		!sub/fun/method/op name
	(anonprocid,	$),		!closure
	(dllprocid,		$),		!
	(dllvarid,		$),		!
	(recordid,		$),		!
	(typeid,		$),		!
	(fieldid,		$),		!
	(structfieldid,	$),		!
	(staticid,		$),		!Static var in module/proc/record
	(frameid,		$),		!Local var in proc
	(paramid,		$),		!param in proc
	(dllparamid,	$),		!dll param in dllproc
	(labelid,		$),		!Label name in proc only
	(constid,		$),		!Label name in proc only
	(enumid,		$),		!Label name in proc only
	(aliasid,		$),		!
	(linkid,		$),		!
	(macroid,		$),		!
	(macroparamid,	$),		!
	(structblockid,	$),		! pseudo names used
	(unionblockid,	$),		!
	(endblockid,	$),		!
end

global enumdata [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global enumdata [0:]ichar scopenames=
	(local_scope=0,		$), ! 		!module
	(global_scope,		$), ! 		!global/inter-module
	(export_scope,		$), ! 		!export/inter-subprog
end

global tabledata []ichar stnames, []byte stsymbols, []byte stsubcodes=

	("if",			kifsym,			0),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		jif),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	jcase),
	("elseswitch",	kelseswitchsym,	jswitch),
	("case",		kcasesym,		jcase),
	("docase",		kdocasesym,		jdocase),
	("recase",		krecasesym,		jrecase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("foreach",		kforsym,		1),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),

	("redo",		questionsym,	0),
!	("redo",		kloopsym,		jredo),
	("redoloop",	kloopsym,		jredo),

	("next",		questionsym,	0),
!	("next",		kloopsym,		jnext),
	("nextloop",	kloopsym,		jnext),

	("exit",		kloopsym,		jexit),
	("exitloop",	kloopsym,		jexit),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("maps",		kmapsym,		0),
	("mapss",		kmapsym,		0),
	("eval",		kevalsym,		0),

	("print",		kprintsym,		jprint),
	("println",		kprintsym,		jprintln),
	("fprint",		kprintsym,		jfprint),
	("fprintln",	kprintsym,		jfprintln),
	("sprint",		ksprintsym,		jsprint),
	("sfprint",		ksprintsym,		jsfprint),

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),

	("cast",		kcastsym,		13),

	("proc",		kprocsym,		0),
	("sub",			kprocsym,		1),

	("function",	kfunctionsym,	0),

	("func",		kfunctionsym,	0),
	("fun",			kfunctionsym,	1),
	("method",		kfunctionsym,	0),

	("type",		ktypesym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),
	("slice",		kslicesym,		0),

	("macro",		kmacrosym,		0),

	("static",		kstaticsym,		0),
	("$caligned",	kcalignedsym,	0),
	
	("const",		kconstsym,		0),

	("module",		kmodulesym,		0),
	("import",		kimportsym,		0),

	("importdll",	kimportdllsym,	'D'),
	("strinclude",	kstrincludesym,	0),
	("unless",		kunlesssym,		0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("raise",		kraisesym,		0),

	("global",		kglobalsym,		global_scope),
	("export",		kglobalsym,		export_scope),

	("clang",		kfflangsym,		0),
	("windows",		kfflangsym,		0),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),
	("real",		stdtypesym,		treal),

	("string",		stdtypesym,		tstring),
	("list",		stdtypesym,		tlist),
	("array",		stdtypesym,		tarray),
	("vector",		stdtypesym,		tvector),
	("bits",		stdtypesym,		tbits),
	("set",			stdtypesym,		tset),
	("dict",		stdtypesym,		tdict),
	("decimal",		stdtypesym,		tdecimal),
	("longint",		stdtypesym,		tdecimal),
	("typetype",	stdtypesym,		ttype),
	("range",		stdtypesym,		trange),
	("recordtype",	stdtypesym,		trecord),

	("cvoid",		stdtypesym,		tvoid),
	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("bit",			stdtypesym,		tu1),
	("u1",			stdtypesym,		tu1),
	("u2",			stdtypesym,		tu2),
	("u4",			stdtypesym,		tu4),
	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),

	("stringc",		stdtypesym,		tpackstrc),
	("stringz",		stdtypesym,		tpackstrz),
	("cstring",		stdtypesym,		tpackstrz),
	("ichar",		stdtypesym,		tstringz),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
!	("thousand",	unitnamesym,	thousand_unit),
	("as",			unitnamesym,	0),

	("$lineno",		compilervarsym,	jcvlineno),
	("$strlineno",	compilervarsym,	jcvstrlineno),
	("$filename",	compilervarsym,	jcvfilename),
	("$modulename",	compilervarsym,	jcvmodulename),
	("$function",	compilervarsym,	jcvfunction),
	("$date",		compilervarsym,	jcvdate),
	("$time",		compilervarsym,	jcvtime),
!	("$version",	compilervarsym,	J_cvversion),
	("$",			dollarsym,		0),

	("and",			andlsym,		jandl),
	("or",			orlsym,			jorl),
	("iand",		iandsym,		jiand),
	("ior",			iorsym,			jior),
	("ixor",		ixorsym,		jixor),
	("in",			insym,			jin),
	("notin",		notinsym,		jnotin),
	("inx",			inxsym,			jinx),
	("inrev",		inrevsym,		jinrev),
	("rem",			iremsym,		jirem),
	("divrem",		idivremsym,		jidivrem),
	("min",			minsym,			jmin),
	("max",			maxsym,			jmax),

	("not",			notlsym,		jnotl),
	("inot",		inotsym,		jinot),
	("istrue",		istruelsym,		jistruel),
	("abs",			abssym,			jabs),
!	("$neg",		opsym,			jneg),
	("asc",			ascsym,			jasc),
	("chr",			chrsym,			jchr),
	("sqrt",		mathssym,		jsqrt),
	("sqr",			sqrsym,			jsqr),
	("cos",			mathssym,		jcos),
	("sin",			mathssym,		jsin),
	("tan",			mathssym,		jtan),
	("asin",		mathssym,		jasin),
	("acos",		mathssym,		jacos),
	("atan",		mathssym,		jatan),
	("atan2",		maths2sym,		jatan2),
	("sign",		signsym,		jsign),
	("ln",			mathssym,		jln),
	("log",			mathssym,		jlog),
	("exp",			mathssym,		jexp),
	("round",		mathssym,		jround),
	("floor",		mathssym,		jfloor),
	("ceil",		mathssym,		jceil),
	("fract",		mathssym,		jfract),
	("fmod",		maths2sym,		jfmod),

	("append",		appendsym,		jappend),
	("concat",		concatsym,		jconcat),

	("len",			propsym,		jlen),
	("lwb",			propsym,		jlwb),
	("upb",			propsym,		jupb),
	("bounds",		propsym,		jbounds),
	("bitwidth",	propsym,		jbitwidth),
	("bytes",		propsym,		jbytesize),
	("minvalue",	propsym,		jminvalue),
	("maxvalue",	propsym,		jmaxvalue),
	("basetype",	propsym,		jbasetype),
	("usertype",	propsym,		jusertype),
	("elemtype",	propsym,		jelemtype),
	("dictitems",	propsym,		jdictitems),
!	("decdigits",	propsym,		jdecdigits),
	("isfound",		propsym,		jisfound),
!	("type",		propsym,		ktype),

	("isvoid",		propsym,		jisvoid),
	("isdef",		propsym,		jisdef),
	("defined",		propsym,		jisdef),
	("isint",		propsym,		jisint),
	("isreal",		propsym,		jisreal),
	("islist",		propsym,		jislist),
	("isstring",	propsym,		jisstring),
	("isrange",		propsym,		jisrange),
	("ispointer",	propsym,		jispointer),
	("isarray",		propsym,		jisarray),
	("isrecord",	propsym,		jisrecord),
	("isset",		propsym,		jisset),
	("isnumber",	propsym,		jisnumber),
	("ismutable",	propsym,		jismutable),
	("odd",			propsym,		jodd),
	("even",		propsym,		jeven),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("nil",			knilsym,		0),
	("con",			sysconstsym,	con_const),
	("pi",			sysconstsym,	pi_const),
	("true",		sysconstsym,	true_const),
	("false",		sysconstsym,	false_const),

	("$neg",		specialopsym,	'-'),
	("$index",		specialopsym,	'[]'),

	("$$dummy",		0,				0)
end

global enumdata [0:]ichar hostfnnames, [0:]byte hostnparams, [0:]byte hostisfn,
			[0:]byte hostinternal, [0:]ref proc hosthandlers =
!                    name  np isfn int
	(h_dummy=0,			$,	0,	0,	1,	cast(nil)                ),

	(h_startprint,		$,	1,	0,	1,	cast(pch_startprint)     ),	!startprint(x)	Set o/p dev for following print items
	(h_startprintcon,	$,	0,	0,	1,	cast(pch_startprintcon)  ),	!startprintcon()	Set console dev for following print items
	(h_strstartprint,	$,	0,	0,	1,	cast(pch_strstartprint)  ),	!strstartprint()	Set o/p dev for internal string
	(h_setformat,		$,	1,	0,	1,	cast(pch_setformat)      ),	!setformat(x)	Set up format string for following print items up to str/endprint
	(h_endprint,		$,	0,	0,	1,	cast(pch_endprint)       ),	!endprint()	Restore o/p dev
	(h_strendprint,		$,	0,	1,	1,	cast(pch_strendprint)    ),	!strendprint()	Restore o/p dev, and return result as string
	(h_print,			$,	2,	0,	1,	cast(pch_print)          ),		!print(x,[y])	Print x, using default format code or y
	(h_print_nf,		$,	1,	0,	1,	cast(pch_print_nf)       ),		!print(x)		Print x, using default format code

	(h_println,			$,	0,	0,	1,	cast(pch_println)        ),	!println()	Print newline
	(h_printnogap,		$,	0,	0,	1,	cast(pch_printnogap)     ),	!printnogap()	Suppress any gap before next print item
	(h_printspace,		$,	0,	0,	1,	cast(pch_printspace)     ),	!printspace		Extra space at beg or end

	(h_readln,			$,	1,	0,	1,	cast(pch_readln)         ),	!sreadln(x)	Read line from console or device x, into read buffer
	(h_sreadln,			$,	1,	1,	0,	cast(pch_sreadln)        ),	!sreadln(x)	Read line from console or device x, into read buffer
	(h_sread,			$,	1,	1,	0,	cast(pch_sread)          ),	!sread([x])	Read item from read buffer, with/without format code
	(h_rereadln,		$,	0,	0,	0,	cast(pch_rereadln)       ),	!sread([x])	Read item from read buffer, with/without format code
	(h_reread,			$,	0,	0,	0,	cast(pch_reread)         ),	!sread([x])	Read item from read buffer, with/without format code

	(h_strtoval,		$,	2,	1,	0,	cast(pch_strtoval)       ),	!
	(h_tostr,			$,	2,	1,	0,	cast(pch_tostr)          ),	!

	(h_leftstr,			$,	3,	1,	0,	cast(pch_leftstr)        ),
	(h_rightstr,		$,	3,	1,	0,	cast(pch_rightstr)       ),
	(h_convlc,			$,	2,	1,	0,	cast(pch_convlc)         ),
	(h_convuc,			$,	2,	1,	0,	cast(pch_convuc)         ),

	(h_waitkey,			$,	0,	1,	0,	cast(pch_waitkey)        ),
	(h_testkey,			$,	0,	1,	0,	cast(pch_testkey)        ),
	(h_execwait,		$,	3,	1,	0,	cast(pch_execwait)       ),
	(h_execcmd,			$,	3,	1,	0,	cast(pch_execcmd)        ),
	(h_system,			$,	1,	1,	0,	cast(pch_system)         ),

	(h_makestr,			$,	2,	1,	0,	cast(pch_makestr)        ),
	(h_makeref,			$,	2,	1,	0,	cast(pch_makeref)        ),

	(h_new,				$,	4,	1,	0,	cast(pch_new)            ),
!	(h_setoverload,		$,	3,	0,	0,	cast(pch_setoverload)    ),

	(h_getcmdparam,		$,	1,	1,	0,	cast(pch_getcmdparam)    ),
	(h_gethostname,		$,	0,	1,	0,	cast(pch_gethostname)    ),
	(h_getprogname,		$,	0,	1,	0,	cast(pch_getprogname)    ),

!	(h_$setpcerror,		$,	1,	0,	0,	cast(pch_$setpcerror)    ),
	(h_$setdebug,		$,	1,	0,	0,	cast(pch_$setdebug)      ),
	(h_$test2,			$,	2,	1,	0,	cast(pch_$test2)         ),
	(h_$test,			$,	3,	1,	0,	cast(pch_$test)          ),
	(h_$refcount,		$,	1,	1,	0,	cast(pch_$refcount)      ),

	(h_ticks,			$,	0,	1,	0,	cast(pch_ticks)          ),
	(h_clock,			$,	0,	1,	0,	cast(pch_clock)          ),
	(h_sleep,			$,	1,	0,	0,	cast(pch_sleep)          ),
	(h_random,			$,	1,	1,	0,	cast(pch_random)         ),
	(h_gethash,			$,	1,	1,	0,	cast(pch_gethash)        ),
	(h_getos,			$,	0,	1,	0,	cast(pch_getos)          ),
	(h_iswindows,		$,	0,	1,	0,	cast(pch_iswindows)      ),
	(h_setmesshandler,	$,	1,	0,	0,	cast(pch_setmesshandler) ),
	(h_$getparam,		$,	1,	1,	0,	cast(pch_$getparam)      ),
	(h_makeempty,		$,	1,	1,	0,	cast(pch_makeempty)      ),
	(h_$smallmemtotal,	$,	0,	1,	0,	cast(pch_$smallmemtotal) ),
	(h_$id,				$,	1,	1,	0,	cast(pch_$id)            ),
	(h_copy,			$,	1,	1,	0,	cast(pch_copy)           ),
	(h_$nan,			$,	0,	1,	0,	cast(pch_$nan)           ),
	(h_$infinity,		$,	0,	1,	0,	cast(pch_$infinity)      ),

	(h_$nprocs,			$,	0,	1,	0,	cast(pch_$nprocs)        ),
	(h_$procname,		$,	1,	1,	0,	cast(pch_$procname)      ),
	(h_$procref,		$,	1,	1,	0,	cast(pch_$procref)       ),

	(h_allocexec,		$,	1,	1,	0,	cast(pch_allocexec)      ),
	(h_runnative,		$,	2,	1,	0,	cast(pch_runnative)      ),
	(h_setlwb,			$,	2,	0,	0,	cast(pch_setlwb)         ),

	(h_last,			$,	0,	0,	1,	cast(nil)                )
end

global []byte D_binopset = (
	andlsym, orlsym, eqsym, nesym, ltsym, lesym, gtsym, gesym, addsym,
	subsym, mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym,
	shlsym, shrsym, minsym, maxsym,	concatsym, powersym, isequalsym,
	idivremsym,  maths2sym, appendsym, addrsym )

global [0..symbolnames.upb]byte binopset

global []byte D_unaryopset = (
	notlsym, inotsym, abssym, istruelsym, sqrsym, signsym, ascsym, chrsym,
	mathssym)

global [0..symbolnames.upb]byte unaryopset

global []byte D_addopset=(addsym, subsym, iandsym, iorsym, ixorsym,
		concatsym, appendsym, minsym, maxsym, addrsym, daddrsym)

global []byte D_cmpopset=(eqsym, nesym, ltsym, lesym, gesym, gtsym, isequalsym)

global []byte D_mulopset=(mulsym, divsym, idivsym, iremsym, shlsym, shrsym)

global [0..symbolnames.upb]byte addopset
global [0..symbolnames.upb]byte cmpopset
global [0..symbolnames.upb]byte mulopset
global [0..symbolnames.upb]byte exprendset

global []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,namesym,
	incrsym,decrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,
	stringconstsym,stdtypesym, kmapsym, lcurlysym,
!	ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
	ksprintsym,ksreadsym,ksreadlnsym,dollarsym,compilervarsym, kclampsym,
	krefsym, kcastsym, ellipsissym,
	knilsym, khostfnsym, kifsym, krecordsym, kstructsym)

global [0:symbolnames.len]byte exprstarterset

!type tables
global const maxtype=250

global [0..maxtype]ichar ttname
global [0..maxtype]symbol ttnamedef
global [0..maxtype]int16 ttbasetype
global [0..maxtype]int16 tttarget

global [0..maxtype]int ttlower
global [0..maxtype]int ttlength

global [0..maxtype]unit ttlowerexpr
global [0..maxtype]unit ttlengthexpr

global [0..maxtype]int ttsize
global [0..maxtype]byte ttbitwidth
global [0..maxtype]symbol ttfields		!for initialially anonymous record field lists
global [0..maxtype]byte ttcaligned
global [0..maxtype]symbol ttowner
global int ntypes
global int firstusertype				!starts at ntypes+1, augments for each sp

global const int maxuserxtype=5000
global int nuserxtypes
global int userxtypebase			!first index (growing downwards) of userxtypes in current module
global ref userxrec userxmodelist	!list of all references to userx modes

global [0:maxuserxtype]symbol ttnamedefx
global [0:maxuserxtype]int ttxmap
global [0:maxuserxtype]byte ttxmoduleno

global [0..h_last]byte hostlvset

proc start=
!	translate into an instant lookup format
	int i

	for i:=1 to D_binopset.len do
		binopset[D_binopset[i]]:=1
		exprstarterset[D_binopset[i]]:=1
	od

	for i:=1 to D_unaryopset.len do
		unaryopset[D_unaryopset[i]]:=1
		exprstarterset[D_unaryopset[i]]:=1
	od

	for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od

	exprendset[semisym]:=1
	exprendset[commasym]:=1
	exprendset[rsqsym]:=1
	exprendset[rbracksym]:=1
	exprendset[kendsym]:=1
	exprendset[kdosym]:=1
	exprendset[ktosym]:=1

	for i:=1 to D_addopset.len do addopset[D_addopset[i]]:=1 od
	for i:=1 to D_mulopset.len do mulopset[D_mulopset[i]]:=1 od
	for i:=1 to D_cmpopset.len do cmpopset[D_cmpopset[i]]:=1 od

	for i in 0..tlast do
		ttname[i]:=stdtypenames[i]
		ttbasetype[i]:=i
		ttlower[i]:=1
		ttbitwidth[i]:=stdtypewidths[i]
		ttsize[i]:=stdtypewidths[i]/8
	od

	ntypes:=tlast
end

