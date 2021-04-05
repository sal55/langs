!global const compilerversion = "8.00"
global const pclversion="404"

global type qd=[4]byte

global tabledata() [0:]ichar opndnames=
    (cnone=0,   $),

    (cmemory,   $),
    (cframe,    $),
    (cproc,     $),
    (cdllproc,  $),
    (cdllvar,   $),

    (cfield,    $),
    (cgenfield, $),

    (clabel,    $),
    (cint,      $),
    (cword,     $),
    (creal,     $),
    (crange,    $),
    (cstring,   $),
    (ctype,     $),
    (coperator, $),
    (capplproc, $),

    (clast,     "?")
end

!these aliases are used so that the cmdfmt table is tidier
const m = cmemory
const f = cframe
const p = cproc
const x = cdllproc
const v = cdllvar
const g = cgenfield
const l = clabel
const i = cint
const u = cword
const r = creal
const n = crange
const s = cstring
const t = ctype
const o = coperator
const a = capplproc

!Stack operands labeled X,Y,Z::
!X      X is top of the stack (1 operand)
!X,Y    Y is top of the stack (2 operands)
!X,Y,Z  Z is top of the stack (3 operands)
!suffixes a,b,c help indicate which operand goes where::
!a      always top of the stack
!b      always second from the top
!c      always third from the top
!So Xb and Ya when there are two operands; Y is on top

global tabledata()  [0:]ichar cmdnames, [0:]qd cmdfmt =
    (kzero=0,       $,  qd(0,0,0,0)),
    (knop,          $,  qd(0,0,0,0)),

    (kprocstart,    $,  qd(p,i,0,0)),       !Start of function def; m is address, n is param count
    (kprocend,      $,  qd(0,0,0,0)),
    (kendmodule,    $,  qd(0,0,0,0)),       !Last 'executable' opcode

    (kpush_m,       $,  qd(m,0,0,0)),       !Push static at address m
    (kpush_f,       $,  qd(f,0,0,0)),       !Push frame/param with offset m
    (kpush_am,      $,  qd(m,0,0,0)),       !Push address of static as refvar
    (kpush_af,      $,  qd(f,0,0,0)),       !Push address of frame/param as refvar
    (kpush_ap,      $,  qd(p,0,0,0)),       !push ^proc
    (kpush_al,      $,  qd(l,0,0,0)),       !push ^label

    (kpush_ci,      $,  qd(i,0,0,0)),       !Push constant signed int
    (kpush_cw,      $,  qd(u,0,0,0)),       !Push constant unsigned int
    (kpush_cr,      $,  qd(r,0,0,0)),       !Push constant real
    (kpush_cn,      $,  qd(n,0,0,0)),       !Push range
    (kpush_cs,      $,  qd(s,0,0,0)),       !Push constant string
    (kpush_t,       $,  qd(t,0,0,0)),       !Push type constant
    (kpush_op,      $,  qd(o,i,0,0)),       !Push operator constant; i is 1 or 2 operands expected
    (kpushz,        $,  qd(t,0,0,0)),       !Push Zero(A); push a 'zero' of type A; (void, int/dint/real, range, string, set, but not list/array)
    (kpushz_void,   $,  qd(0,0,0,0)),       !Push void
    (kpushz_str,    $,  qd(0,0,0,0)),       !Push "" empty string (not writable)
    (kpushz_list,   $,  qd(0,0,0,0)),       !Push () empty list with lwb 1
    (kpushz_listl,  $,  qd(i,0,0,0)),       !Push (i:) empty with lwb i
    (kpushz_set,    $,  qd(0,0,0,0)),       !Push [] empty set
    (kpushz_arrayl, $,  qd(t,i,0,0)),       !Push an empty [bit]array with elemtype A, and lwb B

    (kpop_m,        $,  qd(m,0,0,0)),       !Pop to A
    (kpop_f,        $,  qd(f,0,0,0)),       !
    (kstore_m,      $,  qd(m,0,0,0)),       !Store A; store Xa in A; keep on stack
    (kstore_f,      $,  qd(f,0,0,0)),       !

    (kpushptr,      $,  qd(0,0,0,0)),       !Push Xa^
    (kpopptr,       $,  qd(0,0,0,0)),       !Ya^:=Xb; then pop both
    (kstoreptr,     $,  qd(0,0,0,0)),       !Ya^:=Xb; keep Xb on stack (as Xa)

    (kzpop_m,       $,  qd(m,0,0,0)),       !Pop A; do not free A first
    (kzpop_f,       $,  qd(f,0,0,0)),       !

    (kzstore_m,     $,  qd(m,0,0,0)),       !Store A; do not free A first
    (kzstore_f,     $,  qd(f,0,0,0)),       !

    (kcopy,         $,  qd(0,0,0,0)),       !Xa:=deepcopy(Xa)
    (kswap,         $,  qd(0,0,0,0)),       !Yb^:=:Xa^; Xa^:=:A; A:=:B

    (kconvptr,      $,  qd(0,0,0,0)),       !Change refvar in X to ref

    (kjump,         $,  qd(l,0,0,0)),       !Jump to L
    (kjumpptr,      $,  qd(0,0,0,0)),       !Jump to Xa^

    (kjumptrue,     $,  qd(l,0,0,0)),       !Jump to L when Xa is true
    (kjumpfalse,    $,  qd(l,0,0,0)),       !Jump to L when Xa is false

    (kjumpdef,      $,  qd(l,0,0,0)),       !Jump to L when Xa defined (X popped)
    (kjumpvoid,     $,  qd(l,0,0,0)),       !Jump to L when Xa is void

    (kjumpeq,       $,  qd(l,0,0,0)),       !Jump to L when Xb=Ya, Xa=A, A=B; (X,Y popped)
    (kjumpne,       $,  qd(l,0,0,0)),       !Jump to L when Xb<>Ya
    (kjumplt,       $,  qd(l,0,0,0)),       !Jump to L when Xb<Ya
    (kjumple,       $,  qd(l,0,0,0)),       !Jump to L when Xb<=Ya
    (kjumpge,       $,  qd(l,0,0,0)),       !Jump to L when Xb>=Ya
    (kjumpgt,       $,  qd(l,0,0,0)),       !Jump to L when Xb>Ya

    (kjumptesteq,   $,  qd(l,0,0,0)),       !Jump to L when Xb=Ya (Ya popped), or Xa=A; int/set and int/range use 'in' to compare
    (kjumptestne,   $,  qd(l,0,0,0)),       !Jump to L when Xb<>Ya (Ya popped?)

    (kjumplabel,    $,  qd(l,0,0,0)),       !Jumptable entry
    (kjumpclabel,   $,  qd(l,i,0,0)),       !Jumptable entry with value P (a cint)

    (kswitch,       $,  qd(i,i,0,0)),       !Jumptable has n entries, ci is lower bound. Jump indexed by Xa

    (kcswitch,      $,  qd(i,i,i,0)),       !Jumptable has n (label,value) entries, plus 'else' entry. Search for Xa value and jump to label

    (knew,          $,  qd(0,0,0,0)),       !To (L,P); --A and jump to L if not zero
    (kto_f,         $,  qd(l,f,0,0)),       !
    (kfor_fci,      $,  qd(l,f,i,0)),       !
    (kfor_ff,       $,  qd(l,f,f,0)),       !
    (kford_fci,     $,  qd(l,f,i,0)),       !
    (kford_ff,      $,  qd(l,f,f,0)),       !

    (kcall,         $,  qd(p,i,0,0)),       !Call &A; A is cmemoryref; B is stack adjust
    (kcallptr,      $,  qd(i,i,0,0)),       !Call X^; A is no. of params supplied; B is stack adjust
    (kreturn,       $,  qd(0,0,0,0)),       !Return from function, with optional value in caller's  $retval

    (kstartdll,     $,  qd(0,0,0,0)),       !Start sequence of pushdll cmds
    (kpushdll,      $,  qd(t,0,0,0)),       !Set X as next param which shoud be of type B
    (kcalldll,      $,  qd(x,i,t,0)),       !Call dll function m; i=0/1=c/windows; t=result type (void for procs)

    (kcallhost,     $,  qd(i,0,0,0)),       !Call fixed host function &A, with B var-params; B has +128 added when a function

    (kstackframe,   $,  qd(i,0,0,0)),       !Allocate A vars on the stack, and initialise to void

    (kfree,         $,  qd(i,0,0,0)),       !Free and pop A values on stack
    (kaddsp,        $,  qd(i,0,0,0)),       !SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

    (kstop,         $,  qd(0,0,0,0)),       !Stop program and return value X to any calling program
    (ktest,         $,  qd(i,0,0,0)),       !Various tests on X etc

    (kmakelist,     $,  qd(i,i,0,0)),       !A items on stack; make list with lwb B
    (kmakerecord,   $,  qd(i,t,0,0)),       !A items on stack; make record of type B
    (kmakearray,    $,  qd(i,i,t,t)),       !A items on stack; make array with lwb B, type C and elemtype D
    (kmakestruct,   $,  qd(i,t,0,0)),       !A items on stack; make struct with type B
    (kmakeset,      $,  qd(i,0,0,0)),       !A items on stack; make set
    (kmakerange,    $,  qd(0,0,0,0)),       !2 items on stack; make range
    (kmakedict,     $,  qd(i,0,0,0)),       !A*2 items on stack (A key:val items); make dict

    (kpushdot,      $,  qd(g,0,0,0)),       !T:=Xa.A; T:=A.B; LHS must be a record, RHS is a generic field index
    (kpushdotref,   $,  qd(g,0,0,0)),       !T:=&Xa.A; T:=&A.B

    (ksoftconv,     $,  qd(t,0,0,0)),       !T:=A(Xa); T:=B(A); Type conversion; can only be a basic conversion (usually implicit)
    (khardconv,     $,  qd(t,0,0,0)),       !T:=A(Xa); T:=B(A); Type conversion; any conversion can be done provided it's possible (usually explicit)

    (kmixed,        $,  qd(0,0,0,0)),       !++Xa
    (kincrptr,      $,  qd(0,0,0,0)),       !++Xa^
    (kincrto_m,     $,  qd(m,0,0,0)),       !++A
    (kincrto_f,     $,  qd(f,0,0,0)),       !++A
    (kloadincr,     $,  qd(0,0,0,0)),       !T:=Xa^++
    (kincrload,     $,  qd(0,0,0,0)),       !T:=--Xa^

    (kdecrptr,      $,  qd(0,0,0,0)),       !--Xa^; pop X
    (kdecrto_m,     $,  qd(m,0,0,0)),       !--A
    (kdecrto_f,     $,  qd(f,0,0,0)),       !--A
    (kloaddecr,     $,  qd(0,0,0,0)),       !T:=Xa^--
    (kdecrload,     $,  qd(0,0,0,0)),       !T:=--Xa^

    (kincr,         $,  qd(0,0,0,0)),       !T:=++T
    (kdecr,         $,  qd(0,0,0,0)),       !T:=--T

    (kneg,          $,  qd(0,0,0,0)),       !T:=-Xa; T:=-A
    (kabs,          $,  qd(0,0,0,0)),       !abs Xa
    (knot,          $,  qd(0,0,0,0)),       !not Xa
    (kinot,         $,  qd(0,0,0,0)),       !inot Xa
    (kistrue,       $,  qd(0,0,0,0)),       !istrue Xa
    (kasc,          $,  qd(0,0,0,0)),       !asc Xa
    (kchr,          $,  qd(0,0,0,0)),       !chr Xa

    (ksqrt,         $,  qd(0,0,0,0)),       !sqrt Xa
    (ksqr,          $,  qd(0,0,0,0)),       !sqr Xa
    (kcube,         $,  qd(0,0,0,0)),       !cube Xa
    (ksin,          $,  qd(0,0,0,0)),       !sin Xa
    (kcos,          $,  qd(0,0,0,0)),       !cos Xa
    (ktan,          $,  qd(0,0,0,0)),       !tan Xa
    (kasin,         $,  qd(0,0,0,0)),       !asin Xa
    (kacos,         $,  qd(0,0,0,0)),       !acos Xa
    (katan,         $,  qd(0,0,0,0)),       !atan Xa
    (ksign,         $,  qd(0,0,0,0)),       !sign Xa
    (kln,           $,  qd(0,0,0,0)),       !ln Xa
    (klog,          $,  qd(0,0,0,0)),       !log Xa
    (klg,           $,  qd(0,0,0,0)),       !lg Xa
    (kexp,          $,  qd(0,0,0,0)),       !exp Xa
    (kround,        $,  qd(0,0,0,0)),       !round Xa
    (kfloor,        $,  qd(0,0,0,0)),       !floor Xa
    (kceil,         $,  qd(0,0,0,0)),       !ceil Xa
    (kfract,        $,  qd(0,0,0,0)),       !fract Xa

    (knegto,        $,  qd(0,0,0,0)),       !-:=Xa^; -:=A
    (kabsto,        $,  qd(0,0,0,0)),       !abs:=^Xa; pop Xa
    (knotto,        $,  qd(0,0,0,0)),       !not:=Xa^; pop Xa
    (kinotto,       $,  qd(0,0,0,0)),       !inot:=Xa^; pop Xa

    (klen,          $,  qd(0,0,0,0)),       !T:=Xa.len
    (klwb,          $,  qd(0,0,0,0)),       !Xa.lwb
    (kupb,          $,  qd(0,0,0,0)),       !Xa.upb
    (kbounds,       $,  qd(0,0,0,0)),       !Xa.bounds
    (kbits,         $,  qd(0,0,0,0)),       !Xa.bits
    (kbytes,        $,  qd(0,0,0,0)),       !Xa.bytes
    (ktype,         $,  qd(0,0,0,0)),       !Xa.type
    (kelemtype,     $,  qd(0,0,0,0)),       !Xa.elemtype
    (kbasetype,     $,  qd(0,0,0,0)),       !Xa.basetype
    (kminval,       $,  qd(0,0,0,0)),       !Xa.minval
    (kmaxval,       $,  qd(0,0,0,0)),       !Xa.maxval
    (kisint,        $,  qd(0,0,0,0)),       !Xa.isint
    (kisreal,       $,  qd(0,0,0,0)),       !Xa.isreal
    (kisstring,     $,  qd(0,0,0,0)),       !Xa.isstring
    (kisrange,      $,  qd(0,0,0,0)),       !Xa.isrange
    (kisnumber,     $,  qd(0,0,0,0)),       !Xa.isnumber
    (kisarray,      $,  qd(0,0,0,0)),       !Xa.isarray
    (kisrecord,     $,  qd(0,0,0,0)),       !Xa.isrecord
    (kispointer,    $,  qd(0,0,0,0)),       !Xa.ispointer
    (kismutable,    $,  qd(0,0,0,0)),       !Xa.ismutable
    (kisset,        $,  qd(0,0,0,0)),       !Xa.isset
    (kisvoid,       $,  qd(0,0,0,0)),       !Xa.isvoid
    (kisdef,        $,  qd(0,0,0,0)),       !Xa.isdef
    (ktostr,        $,  qd(0,0,0,0)),       !Xa.isnoneComment (may be suppressed from pcb file)
    (kisequal,      $,  qd(0,0,0,0)),       !Xb==Ya

    (kadd,          $,  qd(0,0,0,0)),       !T:=Xb+Ya
    (ksub,          $,  qd(0,0,0,0)),       !Xb-Ya
    (kmul,          $,  qd(0,0,0,0)),       !Xb*Ya
    (kdiv,          $,  qd(0,0,0,0)),       !Xb/Ya
    (kidiv,         $,  qd(0,0,0,0)),       !Xb%Ya
    (krem,          $,  qd(0,0,0,0)),       !Xb rem Ya
    (kdivrem,       $,  qd(0,0,0,0)),       !Xb divrem Ya
    (kiand,         $,  qd(0,0,0,0)),       !Xb iand Ya
    (kior,          $,  qd(0,0,0,0)),       !Xb ior Ya
    (kixor,         $,  qd(0,0,0,0)),       !Xb ixor Ya
    (kshl,          $,  qd(0,0,0,0)),       !Xb shl Ya
    (kshr,          $,  qd(0,0,0,0)),       !Xb shr Ya
    (kin,           $,  qd(0,0,0,0)),       !Xb in Ya
    (knotin,        $,  qd(0,0,0,0)),       !Xb notin Ya
    (kinrev,        $,  qd(0,0,0,0)),       !Xb inrev Ya
    (keq,           $,  qd(0,0,0,0)),       !Xb=Ya
    (kne,           $,  qd(0,0,0,0)),       !Xb<>Ya
    (klt,           $,  qd(0,0,0,0)),       !Xb<Ya
    (kle,           $,  qd(0,0,0,0)),       !Xb<=Ya
    (kge,           $,  qd(0,0,0,0)),       !Xb>=Ya
    (kgt,           $,  qd(0,0,0,0)),       !Xb>Ya
    (kmin,          $,  qd(0,0,0,0)),       !Xb min Ya
    (kmax,          $,  qd(0,0,0,0)),       !Xb max Ya
    (kconcat,       $,  qd(0,0,0,0)),       !Xb concat Ya
    (kappend,       $,  qd(0,0,0,0)),       !Xb append Ya

    (kpower,        $,  qd(0,0,0,0)),       !Xb power Ya
    (katan2,        $,  qd(0,0,0,0)),       !Xb atan2 Ya

    (kaddto,        $,  qd(0,0,0,0)),       !Xb^+:=Y or Xa^+:=A or A+:=B
    (ksubto,        $,  qd(0,0,0,0)),       !Xb^-:=Ya
    (kmulto,        $,  qd(0,0,0,0)),       !Xb^*:=Ya
    (kdivto,        $,  qd(0,0,0,0)),       !Xb^/:=Ya
    (kidivto,       $,  qd(0,0,0,0)),       !Xb^%:=Ya

    (kiandto,       $,  qd(0,0,0,0)),       !Xb^ iand:=Ya
    (kiorto,        $,  qd(0,0,0,0)),       !Xb^ ior:=Ya
    (kixorto,       $,  qd(0,0,0,0)),       !Xb^ ixor:=Ya
    (kshlto,        $,  qd(0,0,0,0)),       !Xb^ shl:=Ya
    (kshrto,        $,  qd(0,0,0,0)),       !Xb^ shr:=Ya
    (kminto,        $,  qd(0,0,0,0)),       !Xb^ min:=Ya
    (kmaxto,        $,  qd(0,0,0,0)),       !Xb^ max:=Ya
    (kconcatto,     $,  qd(0,0,0,0)),       !Xb^ concat:=Ya
    (kappendto,     $,  qd(0,0,0,0)),       !Xb^ concat:=Ya

    (kpushix,       $,  qd(0,0,0,0)),       !T:=Xb[Ya]
    (kpushdotix,    $,  qd(0,0,0,0)),       !T:=Xb.[Ya]
    (kpushkeyix,    $,  qd(0,0,0,0)),       !T:=Xb{Ya}
    (kpushkeyixd,   $,  qd(0,0,0,0)),       !T:=Xc{Yb,Za} Za is default value

    (kpushixref,    $,  qd(0,0,0,0)),       !^Xb[Ya]
    (kpushdotixref, $,  qd(0,0,0,0)),       !^Xb.[Ya]
    (kpushkeyixref, $,  qd(0,0,0,0)),       !^Xb{Ya}

    (kpushbyteix,   $,  qd(t,0,0,0)),       !Xb.A[Ya]
    (kpushbyteixref,$,  qd(t,0,0,0)),       !^Xb.A[Ya]

    (kappendset,    $,  qd(0,0,0,0)),       !Xb[Ya]:=1; pop Y, keep X on stack; Xa[A]:=1

    (kpushdotm,     $,  qd(t,p,0,0)),       !A=0 (module) or owner type; B=proc; check X has type A if A is a type
    (kpushdott,     $,  qd(t,t,0,0)),       !A=owner type; B=.type; check X has type A
    (kpush_ad,      $,  qd(x,0,0,0)),       !push index of dll proc
    (kpush_try,     $,  qd(l,i,i,0)),       !Push try/except into; label/except code/no. exceptions
    (kraise,        $,  qd(0,0,0,0)),       !Raise exception Xa
    (kapplyop,      $,  qd(i,0,0,0)),       !applyop(Ya,Xb); i is 1 or 2, number of operands provided

!A few experimental main pcl codes
    (kmakeiter,     $,  qd(i,0,0,0)),       !Turn Xa inter iterate object; A=0/1=fwd/rev
    (kforall,       $,  qd(l,f,f,0)),       !A=label; B=iter var; C=primary index var
    (kforallx,      $,  qd(l,f,f,f)),       !A=label; B=iter var; C=primary index var, D=secondary indat var

    (kforeach,      $,  qd(l,f,f,0)),       !A=label; B=iter var; C=primary index var
    (kforeachx,     $,  qd(l,f,f,f)),       !A=label; B=iter var; C=primary index var, D=secondary indat var

    (kexpandrange,  $,  qd(0,0,0,0)),       !expand range value to two ints
    (kcallappl,     $,  qd(a,i,0,0)),       !Call named host function &A, with B var-params

    (klastcmd,      $,  qd(0,0,0,0))

end

global [0..klastcmd]ref void cmdmap         !map cmd index to possible fn/label address

global tabledata()  []ichar bcdirnames =
    (kkpclversion,      $), !s  PCL version string
    (kkmoduletable,     $), !n  Names of all modules
    (kkdlltable,        $), !n  DLL imported libraries
    (kkdllproctable,    $), !n  DLL proc table
    (kksymboltable,     $), !n  Proc, Type, Static symbols
    (kktypetable,       $), !n  Type table (user types only)
    (kkgenfieldnames,   $), !n  Genfield names
    (kkgenfielddata,    $), !n  Genfield data
    (kkstringtable,     $), !n  
    (kkstructtable,     $), !n  Struct field table
    (kkpccode,          $), !n  PCL bytecode data
    (kkend,             $), !-  End of file
    (kknewstringtable,  $), !n  Counted blocks
    (kkapplproctable,   $), !n  Host proc table
end

global tabledata() [0:]ichar hostfnnames, [0:]int hostnparams, [0:]int hostisfn =
    (host_dummy=0,          $,  0,  0),

    (host_startprint,       $,  1,  0), !startprint(x)  Set o/p dev for following print items
    (host_startprintcon,    $,  0,  0), !startprintcon()    Set console dev for following print items
    (host_strstartprint,    $,  0,  0), !strstartprint()    Set o/p dev for internal string
    (host_setformat,        $,  1,  0), !setformat(x)   Set up format string for following print items up to str/endprint
    (host_endprint,         $,  0,  0), !endprint() Restore o/p dev
    (host_strendprint,      $,  0,  1), !strendprint()  Restore o/p dev, and return result as string
    (host_print,            $,  2,  0),     !print(x,[y])   Print x, using default format code or y

    (host_dprint,           $,  2,  0), !dprint(x,[y])  As print, but with extra debug stuff
    (host_println,          $,  0,  0), !println()  Print newline
    (host_printnogap,       $,  0,  0), !printnogap()   Suppress any gap before next print item

    (host_readln,           $,  1,  0), !sreadln(x) Read line from console or device x, into read buffer
    (host_sreadln,          $,  1,  1), !sreadln(x) Read line from console or device x, into read buffer
    (host_sread,            $,  1,  1), !sread([x]) Read item from read buffer, with/without format code
    (host_rereadln,         $,  0,  0), !sread([x]) Read item from read buffer, with/without format code
    (host_reread,           $,  0,  0), !sread([x]) Read item from read buffer, with/without format code

    (host_strtoval,         $,  2,  1), !
    (host_tostr,            $,  2,  1), !

    (host_leftstr,          $,  3,  1),
    (host_rightstr,         $,  3,  1),
    (host_convlc,           $,  2,  1),
    (host_convuc,           $,  2,  1),
    (host_iconvlc,          $,  2,  0),     !&
    (host_iconvuc,          $,  2,  0),     !&

    (host_stop,             $,  0,  0), !stop(x)    Stop execution
    (host_stopx,            $,  1,  0), !stopx(x)   Stop, with given return value
    (host_ismain,           $,  1,  1), !ismain(x)  Return 1 when module name x is main module
    (host_waitkey,          $,  0,  1),
    (host_testkey,          $,  0,  1),
    (host_execwait,         $,  3,  1),
    (host_execcmd,          $,  3,  1),
    (host_shellexec,        $,  2,  1),
    (host_system,           $,  1,  1),

    (host_makestr,          $,  2,  1),
    (host_makestrslice,     $,  2,  1),
    (host_makeref,          $,  2,  1),

    (host_new,              $,  4,  1),
    (host_newheap,          $,  4,  1),
    (host_readlines,        $,  1,  1),
    (host_heapvar,          $,  1,  1),
    (host_dictitems,        $,  1,  1),
    (host_freeheap,         $,  1,  0),
    (host_setoverload,      $,  3,  0),

    (host_getcmdparam,      $,  1,  1),
    (host_gethostname,      $,  0,  1),

    (host_setpcerror,       $,  1,  0),
    (host_setdebug,         $,  1,  0),
    (host_test,             $,  2,  1),

    (host_ticks,            $,  0,  1),
    (host_sleep,            $,  1,  0),
    (host_random,           $,  1,  1),
    (host_findmetafunction, $,  1,  1),
    (host_gethash,          $,  1,  1),
    (host_getos,            $,  0,  1),
    (host_gethostsize,      $,  0,  1),
    (host_iswindows,        $,  0,  1),
    (host_setmesshandler,   $,  1,  0),
    (host_setfprintf,       $,  2,  0),

    (host_loadpcl,          $,  2,  1),
    (host_runpcl,           $,  2,  1),
    (host_runtask,          $,  2,  1),
    (host_callext,          $,  3,  0),
    (host_pcldata,          $,  2,  1),
    (host_getcstring,       $,  1,  1),
    (host_getparam,         $,  1,  1),
    (host_clearlist,        $,  1,  0),
    (host_makelink,         $,  1,  1),
    (host_allparams,        $,  1,  1),
    (host_stackvars,        $,  0,  1),
    (host_makeempty,        $,  1,  1),
    (host_errorinfo,        $,  1,  1),
    (host_strrepl,          $,  3,  1),

    (host_last,             $,  0,  0)
end

global tabledata() [0:]ichar namenames =
    (nullid=0,      $),     !Not assigned (sometimes converted to genfieldid)
    (programid,     $),     !Main root
    (moduleid,      $),     !Current or imported module
!   (extmoduleid,   $),     !Imported module
    (dllmoduleid,   $),     !
    (typeid,        $),     !Type name in type, proc or module
!   (classid,       $),     !Class name
    (procid,        $),     !Proc/method/function/op name
    (dllprocid,     $),     !Dll Proc/function name
    (dllvarid,      $),     !Dll variable name
    (applprocid,    $),     !Host proc/function name
    (constid,       $),     !Named constant in type, proc or module
    (staticid,      $),     !Static in type or proc or module
    (frameid,       $),     !Local var
    (paramid,       $),     !Local param
    (fieldid,       $),     !Field of Record or Class
    (genfieldid,    $),     !Generic Field of Record or Class
    (enumid,        $),     !Enum name, part of enum type only
    (labelid,       $),     !Label name in proc only
    (blockid,       $),     !Codeblock label name in proc only
    (aliasid,       $),     !Alias to another name
    (linkid,        $)      !Name in class defined in a base class
end

global tabledata() []ichar errornames =
    (pc_error,          $),
    (user_error,        $),
    (type_error,        $),
    (mixedtype_error,   $),
    (divide_error,      $),
    (stopmodule_error,  $),
    (bounds_error,      $)
end

