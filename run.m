enumdata []ichar opcodenames =
    (kadd,      $),
    (kneg,      $),
    (kmul,      $),
    (kdivd,     $),
    (kremd,     $),
    (kdiv2,     $),
    (krem2,     $),
    (keqli,     $),
    (kneqi,     $),
    (klssi,     $),
    (kleqi,     $),
    (kgtri,     $),
    (kgeqi,     $),
    (kdupl,     $),
    (kswap,     $),
    (kandb,     $),
    (korb,      $),
    (kload,     $),
    (kstor,     $),
    (khalt,     $),
    (kwri,      $),
    (kwrc,      $),
    (kwrl,      $),
    (krdi,      $),
    (krdc,      $),
    (krdl,      $),
    (keol,      $),

    (kldc,      $),     !instrs from here on have an inline operand
    (kldla,     $),
    (kldl,      $),
    (kldg,      $),
    (kstl,      $),
    (kstg,      $),
    (kmove,     $),
    (kcopy,     $),
    (kaddc,     $),
    (kmulc,     $),
    (kjump,     $),
    (kjumpz,    $),
    (kcall,     $),
    (kadjs,     $),
    (ksets,     $),
    (kexit,     $),
end

[0..1000]int m

[0:]int code = (
    include "fib.bc"
)

proc main =
    interpret()
    println
    println
end
 
proc error(ichar mess)=
    println "Runime error",mess
    stop 1
end

proc interpret =
    int pc, sp, ia
    int c, k, j, n

    pc:=0

    doswitchu ia:=code[pc+1]; pc+:=2; code[pc-2]
    when kadd  then
        m[sp+1]:=m[sp+1]+m[sp]
        ++sp

    when kneg  then
        m[sp]:=-m[sp]

    when kmul  then
        m[sp+1]:=m[sp+1]*m[sp]
        ++sp

    when kdivd then
        m[sp+1]:=m[sp+1] / m[sp]
        ++sp

    when kremd then
        m[sp+1]:=m[sp+1] rem m[sp]
        ++sp

    when kdiv2 then
        m[sp]:=m[sp] % 2

    when krem2 then
        m[sp]:=m[sp] rem 2

    when keqli then
        m[sp+1]:=m[sp+1]=m[sp]
        ++sp

    when kneqi then
        m[sp+1]:=m[sp+1]<>m[sp]
        ++sp

    when klssi then
        m[sp+1]:=m[sp+1]<m[sp]
        ++sp

    when kleqi then
        m[sp+1]:=m[sp+1]<=m[sp]
        ++sp

    when kgtri then
        m[sp+1]:=m[sp+1]>m[sp]
        ++sp

    when kgeqi then
        m[sp+1]:=m[sp+1]>=m[sp]
        ++sp

    when kdupl then
        --sp
        m[sp]:=m[sp+1]

    when kswap then
        swap(m[sp], m[sp+1])

    when kandb then
        if m[sp]=0 then m[sp+1]:=0 fi
        ++sp

    when korb  then
        if m[sp]=1 then m[sp+1]:=1 fi
        ++sp

    when kload then
        m[sp]:=m[m[sp]]

    when kstor then
        m[m[sp]]:=m[sp+1]
        sp+:=2

    when khalt then
        exit

    when kwri  then
        print m[sp+1]:"8 jr"
        sp+:=2

    when kwrc  then
        print char(m[sp])
        ++sp

    when kwrl  then
        println

    when krdi  then
        read m[m[sp]]
        ++sp

    when krdc  then
        m[m[sp]]:=c
        ++sp

    when krdl  then
        error("RDL")

    when keol  then
        --sp
        error("eol")

    when kldc  then
        --sp
        m[sp]:=ia
    when kldla then
        --sp
        m[sp]:=sp+1+ia

    when kldl  then
        --sp
        m[sp]:=m[sp+1+ia]

    when kldg  then
        --sp
        m[sp]:=m[ia]

    when kstl  then
        m[sp+ia]:=m[sp]
        ++sp

    when kstg  then
        m[ia]:=m[sp]
        ++sp

    when kmove then
        k:=m[sp]
        j:=m[sp+1]
        sp+:=2
        n:=ia
        repeat --n; m[k+n]:=m[j+n] until n=0

    when kcopy then
        j:=m[sp]
        n:=ia
        sp:=sp-n+1
        repeat --n; m[sp+n]:=m[j+n] until n=0

    when kaddc then
        m[sp]:=m[sp]+ia

    when kmulc then
        m[sp]:=m[sp]*ia

    when kjump then
        pc:=ia

    when kjumpz then
        if m[sp]=0 then pc:=ia fi
        ++sp

    when kcall then
        --sp
        m[sp]:=pc
        pc:=ia

    when kadjs then
        sp:=sp+ia

    when ksets then
        sp:=ia

    when kexit then
        pc:=m[sp]
        sp+:=ia
    end doswitchu
end
