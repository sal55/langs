import clib
import mlib

type dll0_intm=ref clang function:intm
type dll1_intm=ref clang function(intm)intm
type dll2_intm=ref clang function(intm,intm)intm
type dll3_intm=ref clang function(intm,intm,intm)intm
type dll4_intm=ref clang function(intm,intm,intm,intm)intm
type dll5_intm=ref clang function(intm,intm,intm,intm,intm)intm
type dll6_intm=ref clang function(intm,intm,intm,intm,intm,intm)intm
type dll9_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm)intm
type dll10_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm)intm
type dll11_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm)intm
type dll12_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm,intm)intm
type dll14_intm=ref clang function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm,intm, intm,intm)intm

type dll0_r64=ref clang function:r64
type dll1_r64=ref clang function(intm)r64
type dll2_r64=ref clang function(intm,intm)r64

type dll0_r64x=ref clang function:r64
type dll1_r64x=ref clang function(real)r64
type dll2_r64x=ref clang function(real,real)r64

type m_dll0_intm=ref function:intm
type m_dll1_intm=ref function(intm)intm
type m_dll2_intm=ref function(intm,intm)intm
type m_dll3_intm=ref function(intm,intm,intm)intm
type m_dll4_intm=ref function(intm,intm,intm,intm)intm
type m_dll5_intm=ref function(intm,intm,intm,intm,intm)intm
type m_dll12_intm=ref function(intm,intm,intm,intm, intm,intm,intm,intm, intm,intm,intm,intm)intm

type m_dll0_r64=ref function:r64
type m_dll1_r64=ref function(intm)r64
type m_dll2_r64=ref function(intm,intm)r64


global function os_calldllfunction(ref proc fnaddr,
        int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
!retcode is 'R' or 'I'
!each argcodes element is 'R' or 'I' too
!The x64 version can work with any combination.
!Here, for C, only some combinations are dealt with:
! I result, params all I (not all param counts)
! R result, params all I (not all param counts)
!Mixed params, for arbitrary return type, not handled (not really detected either)

    word64 a
    real64 x
    int oddstack, nextra, pushedbytes

    if retcode='I' then
        return calldll_cint(fnaddr,args,nargs)
    else
        return calldll_creal(fnaddr,args,nargs)
    fi
end 

global function os_pushargs(ref[]word64 args, int nargs, nextra,
                    ref proc fnaddr, int isfloat)word64=
    word64 a
    real64 x
    return os_calldllfunction(fnaddr, (isfloat|0|'I'), nargs, cast(args), nil)
end

function calldll_cint (ref proc fnaddr,ref[]i64 params,int nparams)i64=
switch nparams
when 0 then
    return dll0_intm(fnaddr)^()
when 1 then
    return dll1_intm(fnaddr)^(params^[1])
when 2 then
    return dll2_intm(fnaddr)^(params^[1],params^[2])
when 3 then
    return dll3_intm(fnaddr)^(params^[1],params^[2],params^[3])
when 4 then
    return dll4_intm(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4])
when 5 then
    return dll5_intm(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4], params^[5])
when 6 then
    return dll6_intm(fnaddr)^(params^[1],params^[2],params^[3],
            params^[4], params^[5],params^[6])
when 9 then 
    return (dll9_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],    params^[5],params^[6],
                params^[7],params^[8],params^[9])
when 10 then 
    return (dll10_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],   params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10])
when 11 then 
    return (dll11_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],   params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11])

when 12 then 
    return (dll12_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],   params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11],params^[12])

when 14 then 
    return (dll14_intm(fnaddr))^(params^[1],params^[2],params^[3],params^[4],   params^[5],params^[6],
                params^[7],params^[8],params^[9],params^[10],   params^[11],params^[12],
                params^[13],params^[14])

else
    cpl nparams
    println "calldll/c/int unsupported # of params", nparams
    stop 1
endswitch
return 0
end

function calldll_creal (ref proc fnaddr,ref[]i64 params,int nparams)i64=
real64 x

switch nparams
when 0 then
    return dll0_r64(fnaddr)^()
when 1 then
    os_dummycall(params^[1],params^[2],params^[3],params^[4])
    x:=dll1_r64(fnaddr)^(params^[1])
when 2 then
    x:=dll2_r64(fnaddr)^(params^[1],params^[2])
else
    println "calldll/c/real too many params"
    stop 1
endswitch
return int64@(x)
end

global proc os_dummycall(r64 a,b,c,d)=
end
