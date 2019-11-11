!prng library

! mrandom()            Random unsigned 0 to 2**64-1
! mrandomp()           Random positive signed 0 to 2**63-1
! mrandomint(n)        Random positive signed 0 to n-1 (64 bits)
! mrandomrange(a,b)    Random positive signed a to b inclusive (64 bits)
! mrandomreal()        Random positive (64-bit) float 0.0 to 1.0-epsilon approx

! rem means % in C
! ixor means ^ in C
! iand means & in C
! seed[] array is 1-based
! real means 64-bit double in C

var [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global proc mseed(word64 a,b=0)=
    seed[1]:=a
    if b then
        seed[2]:=b
    else
        seed[2] ixor:=a
    fi
end

global function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
    var word64 x,y
    x:=seed[1]
    y:=seed[2]
    seed[1]:=y
    x ixor:=(x<<23)
    seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
    return seed[2]+y
end

global function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
    return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

global function mrandomint(int n)int=
!positive random int value from 0 to n-1
    return mrandomp() rem n
end

global function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
    var int span
    span:=b-a+1
    if span<=0 then
        return 0
    fi
    return (mrandomp() rem span)+a
end

global function mrandomreal:real =
!positive random real value from 0 to 0.999999999999999999891579782751449556599254719913005828857421875
!upper limit is (2**63-1)/(2**63)
    return real(mrandomp())/9223372036854775808.0
end
