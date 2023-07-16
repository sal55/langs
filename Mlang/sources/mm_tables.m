include "mm_types.m"

global enumdata []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
	(sf_init,				$,	0,	0),
	(sf_print_startfile,	$,	0,	0),
	(sf_print_startstr,		$,	0,	0),
	(sf_print_startptr,		$,	0,	0),
	(sf_print_startcon,		$,	0,	0),
	(sf_print_setfmt,		$,	0,	0),
	(sf_print_nogap,		$,	0,	0),
	(sf_print_space,		$,	0,	0),
	(sf_print_i64,			$,	0,	0),
	(sf_print_i64_nf,		$,	0,	0),
	(sf_print_u64,			$,	0,	0),
	(sf_print_r64,			$,	0,	0),
	(sf_print_r32,			$,	0,	0),
	(sf_print_str,			$,	0,	0),
	(sf_print_str_nf,		$,	0,	0),
	(sf_print_strsl,		$,	0,	0),
	(sf_print_ptr,			$,	0,	0),
	(sf_print_ptr_nf,		$,	0,	0),
	(sf_print_c8,			$,	0,	0),
	(sf_print_bool,			$,	0,	0),
!	(sf_print_var,			$,	0,	0),
	(sf_print_newline,		$,	0,	0),
	(sf_print_end,			$,	0,	0),
	(sf_read_i64,			$,	0,	0),
	(sf_read_r64,			$,	0,	0),
	(sf_read_str,			$,	0,	0),
	(sf_read_fileline,		$,	0,	0),
	(sf_read_strline,		$,	0,	0),
	(sf_read_conline,		$,	0,	0),

	(sf_getnprocs,			$,	0,	1),		!access functions
	(sf_getprocname,		$,	0,	1),
	(sf_getprocaddr,		$,	0,	1),

	(sf_gettttable,			$,	0,	1),
	(sf_getsttable,			$,	0,	1),
	(sf_getfftable,			$,	0,	1),

	(sf_power_i64,			$,	0,	1),
	(sf_unimpl,				$,	0,	1),

end
!
global [sysfnnames.len]symbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

global int mlineno
!global byte fshowpst


!!---
global enumdata [0:]ichar jtagnames,
				   [0:]byte jsubs, [0:]byte jisexpr =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil

![a=u] means a is a unit/list, or is nil

	(jnone=0,		$,	0,		0), ! For tagname lookups when tag is zero
	(jconst,		$,	0,		3), ! value/etc=value, typeno=type code
	(jnull,		$,	0,		3), ! Place holder unit: means 'param no present' when used where a param is expected
	(jvoidvar,		$,	0,		3), ! create void variant
	(jname,		$,	0,		3), ! def=nameptr
	(jnamelv,		$,	0,		3), ! def=nameptr
	(jblock,		$,	1,		0), ! a=L
	(jdecimal,		$,	0,		3), ! svalue=str, slength
	(jassem,		$,	3,		0), ! svalue=str, slength
	(jassemmacro,	$,	0,		0), !
	(jassemreg,	$,	0,		0), !
	(jassemxreg,	$,	0,		0), !
	(jassemmem,	$,	1,		0), !
	(jstrinclude,	$,	1,		0), !

!Logical Operators

	(jandl,		$,	2,		2), ! A B	This group are for conditional expressions (no result)
	(jorl,			$,	2,		2), ! A B
!	(jxorl,		$,	0,		2), ! A B
	(jnotl,		$,	1,		1), ! a
	(jistruel,		$,	1,		1), ! a

!Expressions and Operators

	(jmakelist,	$,	2,		3), ! a=L, b=[u], length=N; element list/lower bound expr
	(jmakerange,	$,	2,		3), ! A B
	(jmakeset,		$,	1,		3), ! a=L, length=N
	(jmakedict,	$,	1,		3), !
	(jmakeslice,	$,	1,		3), !
	(jreturnmult,	$,	0,		3), !

	(jkeyword,		$,	1,		3), ! def=st entry
	(jkeyvalue,	$,	2,		3), ! A B
	(jassign,		$,	2,		3), ! A B a := x
	(jassignmm,	$,	2,		3), ! A B (a,b,c) := (x,y,z)
	(jassignms,	$,	2,		3), ! A B (a,b,c) := x
	(jassignmdrem,	$,	2,		3), ! A B (a,b) := x divrem y
!	(jassignindex,	$,	2,		3), ! A B a[i] := x
!	(jassigndot,	$,	2,		3), ! A B a.m := x
!	(jassigndotix,	$,	2,		3), ! A B a.[i] := x
!	(jdeepcopy,	$,	0,		3), ! A B
	(jcopy,		$,	2,		3), ! A B
	(jcallfn,		$,	2,		3), ! A B
!	(japplyop,		$,	0,		0), ! opcode b c
!	(japplyopx,	$,	0,		1), ! opcode b c
!	(jnew,			$,	3,		3), ! newmode=T, a=L, length=N
!	(jnewvar,		$,	3,		3), ! newmode=T, a=L, length=N
	(jdestroy,		$,	1,		0), ! a=L, length=N
!	(jclear,		$,	0,		0), !

!	(jsetcc,		$,	0,		2), ! A B
	(jcmp,			$,	2,		2), ! A B
	(jcmpchain,	$,	2,		1), ! A B
	(jbin,			$,	2,		2), ! A B
	(junary,		$,	2,		1), ! A B
	(jbinto,		$,	2,		2), ! A B
	(junaryto,		$,	1,		1), ! A B
	(jincr,		$,	1,		3), ! a	++a

	(jinrev,		$,	2,		2), ! A B
	(jinrange,		$,	2,		2), ! A B
	(jinset,		$,	2,		2), ! A B
	(jclamp,		$,	3,		2), ! A B

!	(jflexptr,		$,	0,		3), ! A B
	(jstringz,		$,	0,		3), ! A B
!	(jsliceptr,	$,	0,		3), ! A B

	(jindex,		$,	2,		3), ! A B		a[b]
	(jindexlv,		$,	2,		3), ! A B		a[b]
	(jslice,		$,	2,		3), ! A B		a[b]
	(jdot,			$,	2,		3), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotlv,		$,	2,		3), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotindex,	$,	2,		3), ! A B		a[b]
	(jdotslice,	$,	2,		3), ! A B		a[b]
	(janddotslice,	$,	2,		3), ! A B		a[b]
	(janddotindex,	$,	2,		3), ! A B		a[b]

	(jptr,			$,	1,		3), ! a		a^
	(jptrlv,		$, 	1,		3), ! a		a^
	(jaddrof,		$,	1,		3), ! a		&a
	(jaddroffirst,	$,	1,		3), ! a		&a
!	(jaddrvar,		$,	1,		3), ! a		^a
	(jdaddrvv,		$,	1,		3), ! a		&&a
	(jdaddrtv,		$,	1,		3), ! a		&&a (from jdaddrvv)
	(jconvert,		$,	1,		3), ! typeno=T a		T(a)			T
	(jshorten,		$,	1,		3), !
	(jautocast,	$,	1,		3), ! typeno=T a		T(a)			T
	(jtypepun,		$,	1,		3), ! typeno=T a		T@(a)			T
	(jtypeconst,	$,	0,		3), ! typeno=T			typeconst(T)
	(joperator,	$,	0,		3), ! opcode=opc
	(jupper,		$,	1,		3), ! a		$					T

	(jbitwidth,	$,	1,		1), ! a
	(jbytesize,	$,	1,		1), ! a
	(jtypeof,		$,	1,		3), ! a
	(jtypestr,		$,	0,		1), ! a
!	(jsliceptr,	$,	0,		1), ! a
	(jbitfield,	$,	1,		3), ! a

	(jminvalue,	$,	1,		3), ! a
	(jmaxvalue,	$,	1,		3), ! a

!Translator Variables

	(jcvlineno,	$,	0,		3), !
	(jcvstrlineno,	$,	0,		3), ! 
	(jcvmodulename,$,	0,		3), ! 
	(jcvfilename,	$,	0,		3), ! 
	(jcvfunction,	$,	0,		3), ! 
	(jcvdate,		$,	0,		3), ! 
	(jcvtime,		$,	0,		3), ! 
	(jcvversion,	$,	0,		3), ! 
	(jcvtypename,	$,	0,		3), ! 
	(jcvtargetbits,$,	0,		3), ! 
	(jcvtargetsize,$,	0,		3), ! 
	(jcvtargetcode,$,	0,		3), ! 
	(jcvnil,		$,	0,		3), ! 
	(jcvpi,		$,	0,		3), ! 
	(jcvinfinity,	$,	0,		3), ! 
	(jcvtrue,		$,	0,		3), ! 
	(jcvfalse,		$,	0,		3), ! 

	(jwhenthen,	$,	2,		0), ! a=L b=u
	(jfmtitem,		$,	2,		3), ! A B  x/fmtstr
	(jnogap,		$,	0,		3), ! 
	(jspace,		$,	0,		3), ! 

!Statements

	(jcallproc,	$,	2,		0), ! a=fn b=L, length
	(jreturn,		$,	1,		0), ! a=x/nil
	(jsyscall,		$,	1,		3), ! a=x or nil

!	(jassign,		$,	0,		3), ! A B
	(jto,			$,	3,		0), ! a=N, b=body, c=tempvar/nil, def=name
	(jif,			$,	3,		3), ! condcode a=then b=else
!	(jlongif,		$,	0,		3), ! a=(elsif ...) b=else		L is series of kelsif pairs
	(jforup,		$,	3,		0), ! 
	(jfordown,		$,	3,		0), !
	(jforall,		$,	3,		0), !
	(jforallrev,	$,	3,		0), !
	(jwhile,		$,	3,		0), ! a=x b=u
	(jrepeat,		$,	2,		0), ! a=u b=x
	(jgoto,		$,	1,		0), ! a=x
	(jlabeldef,	$,	0,		0), ! def=nameptr
	(jredo,		$,	0,		0), ! [a=x]
	(jnext,		$,	0,		0), ! [a=x]
	(jexit,		$,	0,		0), ! [a=x]
	(jdo,			$,	1,		0), ! [a=u
	(jcase,		$,	3,		3), ! a=x b=L [c=else]		L is series of whenthen pairs
	(jdocase,		$,	3,		0), ! a=x b=L [c=else]
	(jswitch,		$,	3,		3), ! a=x b=L [c=else]
	(jdoswitch,	$,	3,		0), ! a=x b=L [c=else]
	(jdoswitchu,	$,	3,		0), ! a=x b=L [c=else]
	(jswap,		$,	2,		0), ! A B
	(jselect,		$,	3,		3), ! Not implemented
	(jrecase,		$,	1,		0), ! Not implemented
!	(jrecaseelse,	$,	0,		0), ! Not implemented

	(jprint,		$,	2,		0), ! [a=dev] b=L
	(jprintln,		$,	2,		0), ! [a=dev] b=L
	(jfprint,		$,	3,		0), ! [a=dev] b=fmtstr c=L
	(jfprintln,	$,	3,		0), ! [a=dev] b=fmtstr c=L
	(jsprint,		$,	2,		0), !         b=L 
	(jsfprint,		$,	2,		0), !         b=L
	(jread,		$,	2,		0), ! [a=dev] b=L
	(jreadln,		$,	2,		0), ! [a=dev] b=L
	(jsread,		$,	2,		0), ! [a=dev] b=L
	(jsreadln,		$,	2,		0), ! [a=dev] b=L
	(jstop,		$,	1,		0), ! [a=x]
	(jeval,		$,	1,		3), ! "
	(jstack,		$,	1,		0), ! "
	(junstack,		$,	1,		0), ! "
	(jempty,		$,	1,		1), ! "

	(jdummy,		$,	0,		3)
end

global enumdata []ichar bitfieldnames=
	(bf_msb,		$),
	(bf_lsb,		$),
	(bf_msbit,		$),
	(bf_lsbit,		$),
	(bf_msw,		$),
	(bf_lsw,		$),
	(bf_odd,		$),
	(bf_even,		$),
end

global enumdata [0:]ichar optypenames =
	(no_op=0,		$),
	(bin_op,		$),
	(mon_op,		$),
	(prop_op,		$),
end

!!---
global enumdata []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symbolgenops,
					[]byte symbolgentoops,
					[]byte symbolopprios,
					[]byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$,			0,	0,	0,	0,	0),		! Lex error
	(dotsym,			".",		0,	0,	0,	0,	0),		! "."
	(lexdotsym,			$,			0,	0,	0,	0,	0),		! ".", used at bol to prefix lexical 
	(anddotsym,			"&.",		0,	0,	0,	0,	1),		! "&."
	(commasym,			",",		0,	0,	0,	0,	0),		! ","
	(semisym,			";",		0,	0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,	0,	0,	0,	0),		! ":"
	(dcolonsym,			"::",		0,	0,	0,	0,	0),		! "::"
	(assignsym,			":=",		bin_op,	0,	0,	1,	0),		! :=
	(deepcopysym,		"::=",		0,	0,	0,	1,	0),		! ::=
	(sendtosym,			"=>",		0,	0,	0,	0,	0),		! =>
	(pipesym,			"->",		0,	0,	0,	0,	0),		! ->
	(lbracksym,			"(",		0,	0,	0,	0,	1),		! (
	(rbracksym,			")",		0,	0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,	0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,	0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,	0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,	0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,	0,	0,	0,	1),		! ^
	(barsym,			"|",		0,	0,	0,	0,	0),		! |
	(dbarsym,			"||",		0,	0,	0,	0,	0),		! ||
	(atsym,				"@",		0,	0,	0,	0,	0),		! @
	(datsym,			"@@",		0,	0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,	0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,	0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,	0,	0,	0,	0),		! &&
	(curlsym,			"~",		0,	0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,	0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,	0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,	0,	0,	0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(idivremsym,		"rem",		bin_op,		kidivrem,	0,			3,	0),
	(iandsym,			"iand",		bin_op,		kiand,		kiandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kior,		kiorto,		4,	0),
	(ixorsym,			"ixor",		bin_op,		kixor,		kixorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		kandl,		kandlto,	7,	0),
	(orlsym,			"or",		bin_op,		korl,		korlto,		8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		keq,		0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
	(appendsym,			"append",	bin_op,		kappend,	kappendto,	4,	0),
	(concatsym,			"concat",	bin_op,		kconcat,	kconcatto,	4,	0),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(samesym,			"==",		bin_op,		ksame,		0,			6,	0),
!	(ssmarkersym,		"===",		0,			0,			0,			0,	0),
	(insym,				"in",		bin_op,		kin,		0,			6,	0),
	(notinsym,			"notin",	bin_op,		knotin,		0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

	(negsym,			"$neg",		mon_op,		kneg,		0,			0,	1),
	(notlsym,			"not",		mon_op,		knotl,		knotlto,	0,	1),
	(istruelsym,		"istrue",	mon_op,		kistruel,	kistruelto,	0,	1),
	(inotsym,			"inot",		mon_op,		kinot,		kinotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),

	(propsym,				$,		prop_op,	0,			0,			0,	0),
	(mathsopsym,		$,		0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,		0,	0,	0,	0,	1),		! atan2 etc

	(bitfieldsym,		$,		0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,		0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,		0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,		0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(docstringsym,		$,		0,	0,	0,	0,	0),		! ! #comment used as documentation string
	(incrsym,			$,		0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,		0,	0,	0,	0,	1),		! 123 32 bits signed
	(decimalconstsym,	$,		0,	0,	0,	0,	1),		! 123 or 123.4 decimal
	(realconstsym,		$,		0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,		0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(wcharconstsym,		$,		0,	0,	0,	0,	1),		! 'A'W or 'ABCD'W (but don't have a syntax yet)
	(stringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"
	(astringconstsym,	$,		0,	0,	0,	0,	1),		! A"ABC"
	(wstringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
	(unitnamesym,		$,		0,	0,	0,	0,	0),		! 
	(namesym,			$,		0,	0,	0,	0,	1),		! identifier symbol
	(ksourcedirsym,		$,		0,	0,	0,	0,	0),		! 
	(kstrincludesym,	$,		0,	0,	0,	0,	1),		! 
!	(lexmacronamesym,	$,		0,	0,	0,	0,	0),		! 
	(regsym,			$,		0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,		0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,		0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,		0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,		0,	0,	0,	0,	0),		! 
	(setccsym,			$,		0,	0,	0,	0,	0),		! 
	(movccsym,			$,		0,	0,	0,	0,	0),		! 
	(segnamesym,		$,		0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,		0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,		0,	0,	0,	0,	1),		! INT, CHAR etc
!	(machinetypesym,	$,		0,	0,	0,	0,	1),		! INTM etc
!	(packtypesym,		$,		0,	0,	0,	0,	0),		! Byte etc
	(ktypeofsym,		$,		0,	0,	0,	0,	0),		! TYPEOF
	(ksubrangesym,		$,		0,	0,	0,	0,	0),		! SUBRANGE
	(koutsym,			$,		0,	0,	0,	0,	0),		! OUT
	(kicharsym,			$,		0,	0,	0,	0,	1),		! ICHAR
	(kifsym,			$,		0,	0,	0,	0,	1),		! 
	(kthensym,			$,		0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,		0,	0,	0,	0,	0),		! 
	(kelsesym,			$,		0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,		0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,		0,	0,	0,	0,	0),		! 
	(kelseselectsym,	$,		0,	0,	0,	0,	0),		! 
	(kendsym,			$,		0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,		0,	0,	0,	0,	0),		! 
	(kcasesym,			$,		0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,		0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,		0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,		0,	0,	0,	0,	0),		! 
	(kforsym,			$,		0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,		0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,		0,	0,	0,	0,	0),		! 
	(kdosym,			$,		0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,		0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,		0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,		0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,		0,	0,	0,	0,	0),		! 
	(kstopsym,			$,		0,	0,	0,	0,	0),		! 
	(kloopsym,			$,		0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kstepsym,			$,		0,	0,	0,	0,	0),		! STEP
	(kgotosym,			$,		0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,		0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,		0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,		0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		$,		0,	0,	0,	0,	0),		! SPRINT/SFPRINT
	(kreadsym,			$,		0,	0,	0,	0,	0),		! READ/READLN
	(ksreadsym,			$,		0,	0,	0,	0,	0),		! SREAD
	(ksreadlnsym,		$,		0,	0,	0,	0,	0),		! SREADLN
	(kprocsym,			$,		0,	0,	0,	0,	0),		! PROC
	(kfunctionsym,		$,		0,	0,	0,	0,	0),		! FUNCTION
!	(kmethodsym,		$,		0,	0,	0,	0,	0),		! METHOD
	(klabelsym,			$,		0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,		0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,		0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,		0,	0,	0,	0,	0),		! UNION
!	(kimportsym,		$,		0,	0,	0,	0,	0),		! IMPORT
	(kimportmodulesym,	$,		0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
!	(kimportpathsym,	$,		0,	0,	0,	0,	0),		! IMPORTPATH
!	(kmapmodulesym,		$,		0,	0,	0,	0,	0),		! MAPMODULE
	(ktypesym,			$,		0,	0,	0,	0,	0),		! TYPE
	(ktypealiassym,		$,		0,	0,	0,	0,	0),		! TYPEALIAS
	(kextendtypesym,	$,		0,	0,	0,	0,	0),		! EXTENDTYPE
	(krefsym,			$,		0,	0,	0,	0,	1),		! REF
	(kmutsym,			$,		0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,		0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,		0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(karraysym,			$,		0,	0,	0,	0,	0),		! ARRAY
	(kdictsym,			$,		0,	0,	0,	0,	0),		! DICT
!	(kflexsym,			$,		0,	0,	0,	0,	0),		! FLEX
	(kmacrosym,			$,		0,	0,	0,	0,	0),		! MACRO
	(kexpandsym,		$,		0,	0,	0,	0,	0),		! EXPAND
	(koperatorsym,		$,		0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,		0,	0,	0,	0,	0),		! 
!	(kenumsym,			$,		0,	0,	0,	0,	0),		! 
	(knewsym,			$,		0,	0,	0,	0,	0),		! NEW
!	(kdestroysym,		$,		0,	0,	0,	0,	0),		! DESTROY
	(kclearsym,			$,		0,	0,	0,	0,	0),		! CLEAR
	(kclasssym,			$,		0,	0,	0,	0,	0),		! CLASS
	(kheadersym,		$,		0,	0,	0,	0,	0),		! MODULE
	(kheadervarsym,		$,		0,	0,	0,	0,	0),		! MODULE
	(kfflangsym,		$,		0,	0,	0,	0,	0),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		$,		0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,		0,	0,	0,	0,	0),		! STATIC

!	(ktrysym,			$,		0,	0,	0,	0,	0),		! 
!	(kexceptsym,		$,		0,	0,	0,	0,	0),		! 
!	(kfinallysym,		$,		0,	0,	0,	0,	0),		! 
!	(kraisesym,			$,		0,	0,	0,	0,	0),		! 
!	(kyieldsym,			$,		0,	0,	0,	0,	0),		! 
	(kcastsym,			$,		0,	0,	0,	0,	1),		! CAST
	(compilervarsym,	$,		0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,		0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,		0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,		0,	0,	0,	0,	0),		! tabledata
	(kstacksym,			$,		0,	0,	0,	0,	0),		! STACK/UNSTACK
	(kclampsym,			$,		0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,		0,	0,	0,	0,	0),		! SWAP
	(kerrorsym,			$,		0,	0,	0,	0,	0),		! PC_ERROR etc
!	(sysconstsym,		$,		0,	0,	0,	0,	0),		! nil, etc
	(kassemsym,			$,		0,	0,	0,	0,	0),		! ASM/ASSEM
	(ksyscallsym,		$,		0,	0,	0,	0,	1),		! $get_procname etc
!	(kemitcsym,			$,		0,	0,	0,	0,	0),		! EMITC
	(kemptysym,			$,		0,	0,	0,	0,	0),		! EMPTY
	(kcopysym,			$,		0,	0,	0,	0,	1),		! COPY

	(kdummysym,			$,		0,	0,	0,	0,	0),		!
end

global enumdata []ichar sourcedirnames =
	(includedir,	$),
	(binincludedir,	$),
end

global enumdata []ichar headerdirnames =
!	(hdr_ssfile,		$),
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_subprog,		$),
	(hdr_sysmodule,		$),
	(hdr_sysimport,		$),
	(hdr_syssubprog,	$),
	(hdr_minclude,		$),
!	(hdr_alias,			$),
	(hdr_altpath,		$),
	(hdr_importpath,	$),
	(hdr_linkdll,		$),
	(hdr_linklib,		$),
	(hdr_exportmodule,	$),
	(hdr_file,			$),
	(hdr_runexe,		$),
	(hdr_setvar,		$),
	(hdr_showvar,		$),
end

global enumdata []ichar headervarnames =
	(hv_devpath,		$),
	(hv_mmpath,			$),
	(hv_hdrpath,		$),
	(hv_ctarget,		$),
	(hv_windows,		$),
	(hv_linux,			$),
	(hv_optim,			$),
	(hv_mainmodule,		$),
	(hv_a,				$),
	(hv_b,				$),
	(hv_c,				$),
end

global enumdata [0:]ichar fflangnames=
	(noff=0,		$), ! 
	(windowsff,		$), ! 
	(clangff,		$), ! 
	(mlangff,		$), ! 
	(callbackff,	$), ! 
end

global enumdata [0:]ichar scopenames=
	(Module_scope=0,	"Local"), ! 		!module
	(subprog_scope,		"Global"), ! 		!inter-subprog
	(program_scope,		"Program"), ! 		!inter-module
	(export_scope,		"Export"), ! 		!inter-program
end

global enumdata =
	thousand_unit,
	million_unit,
	billion_unit,
	kilo_unit,
	mega_unit,
	giga_unit
end

global enumdata [0:]ichar parammodenames=
	(var_param=0,		"Var "),
	(in_param,			"In "),
	(out_param,			"Out "),
	(optional_param,	"Opt "),
end

global enumdata [0:]ichar namenames
	(nullid=0,		$),		!Not assigned (sometimes converted to genfieldid)
	(programid,		$),		!Main root
	(subprogid,		$),
	(moduleid,		$),		!Current or imported module
	(dllmoduleid,	$),		!
	(typeid,		$),		!Type name in type, proc or module
	(procid,		$),		!Proc/method/function/op name
	(dllprocid,		$),		!Dll Proc/function name
	(dllvarid,		$),		!Dll variable name
	(genprocid,		$),		!generic proc name
	(constid,		$),		!Named constant in type, proc or module
	(staticid,		$),		!Static in type or proc or module
	(frameid,		$),		!Local var
	(paramid,		$),		!Local param
	(fieldid,		$),		!Field of Record or Class
	(genfieldid,	$),		!Generic Field of Record or Class
	(enumid,		$),		!Enum name, part of enum type only
	(labelid,		$),		!Label name in proc only
	(macroid,		$),		!Name of macro
	(macroparamid,	$),		!Macro formal parameter name
	(linkid,		$),		!Name in class defined in a base class
end

!!---
global tabledata []ichar stnames, []int stsymbols, []int stsubcodes=

	("if",			kifsym,			jif),
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
	("forall",		kforsym,		0),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		1),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),
	("redo",		kloopsym,		jredo),
	("loop",		kloopsym,		jredo),
	("next",		kloopsym,		jnext),
	("exit",		kloopsym,		jexit),
	("$step",		kstepsym,		0),
	("goto",		kgotosym,		0),
	("go",			kgotosym,		1),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("doswitchu",	kdoswitchsym,	jdoswitchu),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
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
	("cast",		kcastsym,		jconvert),

	("function",	kfunctionsym,	0),
	("func",		kfunctionsym,	0),
	("procedure",	kprocsym,		0),
	("proc",		kprocsym,		0),
	("fun",			kfunctionsym,	1),
	("sub",			kprocsym,		1),
	("threadedproc",		kprocsym,		2),

	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("pointer",		krefsym,		0),
	("returning",	sendtosym,		0),
	("mut",			kmutsym,		0),
	("var",			kmutsym,		0),
	("let",			kletsym,		0),

	("include",		ksourcedirsym,	includedir),
	("strinclude",	kstrincludesym,	0),
	("bininclude",	ksourcedirsym,	binincludedir),
	("macro",		kmacrosym,		0),

	("assem",		kassemsym,		1),
	("asm",			kassemsym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),

	("$get_nprocs",		ksyscallsym,		sf_getnprocs),
	("$getnprocs",		ksyscallsym,		sf_getnprocs),

	("$get_procname",	ksyscallsym,		sf_getprocname),
	("$getprocname",	ksyscallsym,		sf_getprocname),

	("$get_procaddr",	ksyscallsym,		sf_getprocaddr),
	("$getprocaddr",	ksyscallsym,		sf_getprocaddr),

	("$gettttable",		ksyscallsym,		sf_gettttable),
	("$getsttable",		ksyscallsym,		sf_getsttable),
	("$getfftable",		ksyscallsym,		sf_getfftable),

	("importdll",	kimportmodulesym,	'D'),
	("importlib",	kimportmodulesym,	'L'),
	("unless",		kunlesssym,			0),

	("out",			koutsym,		0),

	("global",		kglobalsym,		subprog_scope),
	("export",		kglobalsym,		export_scope),

	("clang",		kfflangsym,		clangff),
	("mlang",		kfflangsym,		mlangff),
	("windows",		kfflangsym,		windowsff),
	("callback",	kfflangsym,		callbackff),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),
	("int",			stdtypesym,		tint),
	("word",		stdtypesym,		tword),
	("real",		stdtypesym,		treal),

	("ichar",		kicharsym,		0),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),
	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("float32",		stdtypesym,		tr32),
	("float64",		stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
	("u1",			stdtypesym,		tu1),
	("u2",			stdtypesym,		tu2),
	("u4",			stdtypesym,		tu4),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),

	("char",		stdtypesym,		tc8),
	("char64",		stdtypesym,		tc64),

	("bool64",		stdtypesym,		tbool64),
	("bool",		stdtypesym,		tbool64),
	("bool8",		stdtypesym,		tbool8),

	("range",		stdtypesym,		trange),
	("auto",		stdtypesym,		tauto),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),
	("array",		karraysym,		0),
	("typeof",		ktypeofsym,			0),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
	("thousand",	unitnamesym,	thousand_unit),

	("$lineno",		compilervarsym,	jcvlineno),
	("$strlineno",	compilervarsym,	jcvstrlineno),
	("$filename",	compilervarsym,	jcvfilename),
	("$modulename",	compilervarsym,	jcvmodulename),
	("$function",	compilervarsym,	jcvfunction),
	("$date",		compilervarsym,	jcvdate),
	("$time",		compilervarsym,	jcvtime),
	("$version",	compilervarsym,	jcvversion),
	("$typename",	compilervarsym,	jcvtypename),
	("$targetbits",	compilervarsym,	jcvtargetbits),
	("$targetsize",	compilervarsym,	jcvtargetsize),
!	("$targetname",	compilervarsym,	jcvtargetname),
	("$targetcode",	compilervarsym,	jcvtargetcode),
	("nil",			compilervarsym,	jcvnil),
	("pi",			compilervarsym,	jcvpi),
	("true",		compilervarsym,	jcvtrue),
	("false",		compilervarsym,	jcvfalse),
	("infinity",	compilervarsym,	jcvinfinity),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
	("in",			insym,			kin),
	("notin",		notinsym,		knotin),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruelsym,		0),
	("abs",			abssym,			kabs),
	("$neg",		negsym,			0),

	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		ksin),
	("cos",			mathsopsym,		kcos),
	("tan",			mathsopsym,		ktan),
	("asin",		mathsopsym,		kasin),
	("acos",		mathsopsym,		kacos),
	("atan",		mathsopsym,		katan),
	("ln",			mathsopsym,		kln),
	("log",			mathsopsym,		klog),
	("exp",			mathsopsym,		kexp),
	("round",		mathsopsym,		kround),
	("floor",		mathsopsym,		kfloor),
	("ceil",		mathsopsym,		kceil),
	("fract",		mathsopsym,		kfract),

	("atan2",		maths2opsym,	katan2),
	("fmod",		maths2opsym,	kfmod),

	("append",		appendsym,		0),
	("concat",		concatsym,		0),
	("sliceptr",	propsym,		ksliceptr),

	("len",			propsym,	klen),
	("lwb",			propsym,	klwb),
	("upb",			propsym,	kupb),
	("bounds",		propsym,	kbounds),
!	("lenstr",		propsym,	klenstr),
	("bitwidth",	propsym,	kbitwidth),
	("bytes",		propsym,	kbytesize),
	("minvalue",	propsym,	kminvalue),
	("maxvalue",	propsym,	kmaxvalue),
	("typestr",		propsym,	ktypestr),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("fi",			kendsym,	kifsym),
	("esac",		kendsym,	kcasesym),
	("od",			kendsym,	kdosym),

	("$caligned",	atsym,			1),
	("empty",		kemptysym,		0),
	("clear",		kemptysym,		0),
	("copy",		kcopysym,		0),

	("module",		kheadersym,		hdr_module),
	("sysmodule",	kheadersym,		hdr_sysmodule),
	("import",		kheadersym,		hdr_import),
	("sysimport",	kheadersym,		hdr_sysimport),
	("minclude",	kheadersym,		hdr_minclude),
	("subprog",		kheadersym,		hdr_subprog),
	("syssubprog",	kheadersym,		hdr_syssubprog),
	("altpath",		kheadersym,		hdr_altpath),
	("importpath",	kheadersym,		hdr_importpath),
	("linkdll",		kheadersym,		hdr_linkdll),
	("linklib",		kheadersym,		hdr_linklib),
	("exportmodule",kheadersym,		hdr_exportmodule),
	("runexe",		kheadersym,		hdr_runexe),
	("setvar",		kheadersym,		hdr_setvar),
	("showvar",		kheadersym,		hdr_showvar),

	("$devpath",	kheadervarsym,	hv_devpath),
	("$mmpath",		kheadervarsym,	hv_mmpath),
	("$hdrpath",	kheadervarsym,	hv_hdrpath),
	("$ctarget",	kheadervarsym,	hv_ctarget),
	("$windows",	kheadervarsym,	hv_windows),
	("$linux",		kheadervarsym,	hv_linux),
	("$optim",		kheadervarsym,	hv_optim),
	("$mainmodule",	kheadervarsym,	hv_mainmodule),
	("$a",			kheadervarsym,	hv_a),
	("$b",			kheadervarsym,	hv_b),
	("$c",			kheadervarsym,	hv_c),

	("$$dummy",		0,				0)
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, ktypeofsym, kslicesym, kdictsym, karraysym)

!list of genops that have an int result, used to populate intresult[]
[]byte intresultlist = (
	kin, knotin, klwb, kupb, klen, klenstr, kbitwidth,
	kbytesize)

global [tc64..tr64, tc64..tr64]int16 softconvtable = (
!To: c64		u64			i64			r32			r64				 From:
	(ksoftconv,	ksoftconv,	ksoftconv,	kfloat,		kfloat),	 	!c64
	(ksoftconv,	ksoftconv,	ksoftconv,	kfloat,		kfloat),	 	!u64
	(ksoftconv,	ksoftconv,	ksoftconv,	kfloat,		kfloat), 		!i64
	(kfix,		kfix,		kfix,		ksoftconv,	kfwiden),	 	!r32
	(kfix,		kfix,		kfix,		kfnarrow,	ksoftconv)) 	!r64

global [pclnames.lwb..pclnames.upb]byte intresult

global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
			kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
			semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag

proc start=
	int genop, s,t, a, specop

!populate intresultlist
	for i in intresultlist.bounds do
		intresult[intresultlist[i]]:=1
	od

	for i to exprendsymbols.len do
		endsexpr[exprendsymbols[i]]:=1
	od

	isbooltag[jcmp]:=1
	isbooltag[jcmpchain]:=1
	isbooltag[jandl]:=1
	isbooltag[jorl]:=1
	isbooltag[jnotl]:=1
	isbooltag[jistruel]:=1
	isbooltag[jinrange]:=1
	isbooltag[jinset]:=1
end

