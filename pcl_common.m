!PCL (intermediate language) pcodes
!Most comments are missing

!Stack operands are:
!   Xa          1st of 1
!   Xb, Ya      1st/2nd of 2
!   Xc, Yb, Za  1st/2nd/3rd of 3
!Extra info:
! op        opindex
! fn        fnindex
! cc        cond code
! n         nargs for calls
! sx        scale and offset for ptr/offset ops
!(a b) means that a stack operands are popped, and b are pushed

global tabledata() [0:]ichar pclnames, [0:]byte pcluseindex =
    (k_none = 0,        $,  0), !      (0 0) 
    (k_comment,         $,  0), !      (0 0) 
    (k_blank,           $,  0), !      (0 0) 
    (k_end,             $,  0), !      (0 0) 
    (k_debug,           $,  0), !      (0 0) 
    (k_test,            $,  0), !      (0 0) 

    (k_procdef,         $,  0), !      (0 0) 
    (k_procend,         $,  0), !      (0 0) 
    (k_procentry,       $,  0), !      (0 0) 
    (k_label,           $,  0), !      (0 0) 
    (k_labelname,       $,  0), !      (0 0) 
    (k_frame,           $,  0), !      (0 0) 
    (k_param,           $,  0), !      (0 0) 
    (k_istatic,         $,  0), !      (0 0) 
    (k_zstatic,         $,  0), !      (0 0) 
    (k_initmemz,        $,  0), !      (0 0) 
    (k_freemem,         $,  0), !      (0 0) 
    (k_equiv,           $,  0), !      (0 0) 
    (k_extern,          $,  0), !      (0 0) 
    (k_endextern,       $,  0), !      (0 0) 
    (k_info,            $,  0), !      (0 0) 

    (k_startmult,       $,  0), !      (0 0) 
    (k_resetmult,       $,  0), !      (0 0) 
    (k_endmult,         $,  0), !      (0 0) 

    (k_pushint,         $,  0), !      (0 1)
    (k_pushint128,      $,  0), !      (0 1)
    (k_pushreal,        $,  0), !      (0 1)
    (k_pushreal32,      $,  0), !      (0 1)
    (k_pushstring,      $,  0), !      (0 1)
    (k_pushmem,         $,  0), !      (0 1)
    (k_pushmemaddr,     $,  0), !      (0 1)
    (k_popmem,          $,  0), !      (1 0) 
    (k_storemem,        $,  0), !      (1 1) 
    (k_opnd,            $,  0), !      (0 0) 
    (k_addtoptr,        $,  0), !      (1 1) 
    (k_suboffset,       $,  0), !      (2 1) 
    (k_pushptroff,      $,  0), !      (2 1) 
    (k_popptroff,       $,  0), !      (3 0) 
    (k_storeptroff,     $,  0), !      (3 1) 
    (k_pushptr,         $,  0), !      (1 1) 
    (k_popptr,          $,  0), !      (2 0) 
    (k_storeptr,        $,  0), !      (2 1) 
    (k_free,            $,  0), !      (1 0) 
    (k_unstack,         $,  0), !      (1 0) 
    (k_eval,            $,  0), !      (1 0) 

    (k_callproc,        $,  0), ! n    (n 0) 
    (k_callfn,          $,  0), ! n    (n 1) 
    (k_callmult,        $,  0), ! n m  (n m) 
    (k_callprocptr,     $,  0), ! n    (n+1 0) 
    (k_callfnptr,       $,  0), ! n    (n+1 1) 
    (k_retproc,         $,  0), !      (0 0) 
    (k_retfn,           $,  0), !      (0 0) 
    (k_retmult,         $,  0), !      (0 0) 
    (k_syscallfn,       $,  0), ! n fn (n 1) 
    (k_syscallproc,     $,  0), ! n fn (n 0) 
    (k_setret,          $,  0), !      (1 1) 
    (k_setalign,        $,  0), ! n    (0 0) 

    (k_jump,            $,  0), !      (0 0) goto L
    (k_jumpcc,          $,  1), ! cc   (2 0) goto L when cc
    (k_jumptrue,        $,  1), !      (1 0) goto L when Xa is true
    (k_jumpfalse,       $,  1), !      (1 0) L
    (k_jumpptr,         $,  0), !      (1 0) goto Xa
    (k_jumpinrange,     $,  0), !      (3 0) goto L when Xc in Yb..Za
    (k_jumpnotinrange,  $,  0), !      (3 0) goto L when Xc not in Yb..Za
    (k_setjumpeq,       $,  0), !      (2 1) goto L when 
    (k_setjumpeqx,      $,  0), !      (2 2) 
    (k_setjumpne,       $,  0), !      (2 2) 
    (k_setcc,           $,  1), ! cc   (2 1) X:=Xb cc Ya
    (k_casejumpeq,      $,  0), !      (0 0) 

    (k_to,              $,  0), !      (1 0) L
    (k_forup,           $,  0), !      (2 0) L
    (k_fordown,         $,  0), !      (2 0) L

    (k_swap,            $,  0), !      (2 0) 
    (k_bin,             $,  1), ! op   (2 1) 
    (k_unary,           $,  1), ! op   (1 1) 
    (k_binto,           $,  1), ! op   (2 0) 
    (k_unaryto,         $,  1), ! op   (1 0) 
    (k_incr,            $,  1), !      (1 0) 
    (k_incrx,           $,  1), !      (1 1) 
    (k_convert,         $,  1), !      (1 1) 
    (k_typepun,         $,  1), !      (1 1) 
    (k_makerange,       $,  0), !      (2 1) 
    (k_makeslice,       $,  0), !      (2 1) 
    (k_makeset,         $,  0), !      (n 1) 
    (k_makearray,       $,  0), !      (n 1) 
    (k_dotindex,        $,  0), !      (0 0) 
    (k_dotslice,        $,  0), !      (0 0) 
    (k_popdotindex,     $,  0), !      (0 0) 
    (k_storedotindex,   $,  0), !      (0 0) 
    (k_storedotslice,   $,  0), !      (0 0) 
    (k_switch,          $,  0), !      (0 0) 
    (k_switchlabel,     $,  0), !      (0 0) 
    (k_db,              $,  0), !      (0 0) 
    (k_dw,              $,  0), !      (0 0) 
    (k_dd,              $,  0), !      (0 0) 
    (k_dq,              $,  0), !      (0 0) 
    (k_assem,           $,  0), !      (0 0) 
    (k_dummy,           $,  0), !      (0 0) 
end
