type dll0_int = ref func:int
type dll1_int = ref func(int)int
type dll2_int = ref func(int, int)int
type dll3_int = ref func(int, int, int)int
type dll4_int = ref func(int, int, int, int)int
type dll5_int = ref func(int, int, int, int, int)int
type dll6_int = ref func(int, int, int, int, int, int)int
type dll8_int = ref func(int, int, int, int, int, int, int, int)int
type dll9_int = ref func(int, int, int, int, int, int, int, int, int)int
type dll10_int = ref func(int, int, int, int, int, int, int, int, int, int)int
type dll11_int = ref func(int, int, int, int, int, int, int, int, int, int, int)int
type dll12_int = ref func(int, int, int, int, int, int, int, int, int, int, int, int)int
type dll14_int = ref func(int, int, int, int, int, int, int, int, int, int, int, int, int, int)int

type dll0_r64 = ref func:r64
type dll1_r64 = ref func(int)r64
type dll2_r64 = ref func(int, int)r64

type dll0_r64x = ref func:r64
type dll1_r64x = ref func(real)r64
type dll2_r64x = ref func(real, real)r64

type m_dll0_int = ref func:int
type m_dll1_int = ref func(int)int
type m_dll2_int = ref func(int, int)int
type m_dll3_int = ref func(int, int, int)int
type m_dll4_int = ref func(int, int, int, int)int
type m_dll5_int = ref func(int, int, int, int, int)int
type m_dll12_int = ref func(int, int, int, int, int, int, int, int, int, int, int, int)int

type m_dll0_r64 = ref func:r64
type m_dll1_r64 = ref func(int)r64
type m_dll2_r64 = ref func(int, int)r64


export func os_calldllfunc(ref proc fnaddr, 
        int retcode, nargs, ref[]i64 args, ref[]byte argcodes)u64  = 
!retcode is 'R' or 'I'
!each argcodes element is 'R' or 'I' too
!The x64 version (IE FULL FFI) can work with any combination.
!Here, for C, only some combinations are dealt with:
! I result, params all I (not all param counts)
! R result, params all I (not all param counts)
!Mixed params, for arbitrary return type, not handled (not really detected either)

    u64 a
    r64 x
    int oddstack, nextra, pushedbytes

    if retcode = 'I' then
        return calldll_cint(fnaddr, args, nargs)
    else
        return calldll_creal(fnaddr, args, nargs)
    fi
end 

func calldll_cint (ref proc fnaddr, ref[]i64 params, int nparams)i64 = 
    switch nparams
    when 0 then
        return dll0_int(fnaddr)^()
    when 1 then
        return dll1_int(fnaddr)^(params^[1])
    when 2 then
        return dll2_int(fnaddr)^(params^[1], params^[2])
    when 3 then
        return dll3_int(fnaddr)^(params^[1], params^[2], params^[3])
    when 4 then
        return dll4_int(fnaddr)^(params^[1], params^[2], params^[3], 
                params^[4])
    when 5 then
        return dll5_int(fnaddr)^(params^[1], params^[2], params^[3], 
                params^[4], params^[5])
    when 6 then
        return dll6_int(fnaddr)^(params^[1], params^[2], params^[3], 
                params^[4], params^[5], params^[6])
    when 8 then 
        return (dll8_int(fnaddr))^(params^[1], params^[2], params^[3], params^[4],  params^[5], params^[6], 
                    params^[7], params^[8])
    when 9 then 
        return (dll9_int(fnaddr))^(params^[1], params^[2], params^[3], params^[4],  params^[5], params^[6], 
                    params^[7], params^[8], params^[9])
    when 10 then 
        return (dll10_int(fnaddr))^(params^[1], params^[2], params^[3], params^[4],     params^[5], params^[6], 
                    params^[7], params^[8], params^[9], params^[10])
    when 11 then 
        return (dll11_int(fnaddr))^(params^[1], params^[2], params^[3], params^[4],     params^[5], params^[6], 
                    params^[7], params^[8], params^[9], params^[10],    params^[11])

    when 12 then 
        return (dll12_int(fnaddr))^(params^[1], params^[2], params^[3], params^[4],     params^[5], params^[6], 
                    params^[7], params^[8], params^[9], params^[10],    params^[11], params^[12])

    when 14 then 
        return (dll14_int(fnaddr))^(params^[1], params^[2], params^[3], params^[4],     params^[5], params^[6], 
                    params^[7], params^[8], params^[9], params^[10],    params^[11], params^[12], 
                    params^[13], params^[14])

    else
        cpl nparams
        println "calldll/c/int unsupported # of params", nparams
        stop 1
    end switch
    return 0
end

func calldll_creal (ref proc fnaddr, ref[]i64 params, int nparams)i64 = 
    r64 x

    switch nparams
    when 0 then
        return dll0_r64(fnaddr)^()
    when 1 then
        os_dummycall(params^[1], params^[2], params^[3], params^[4])
        x := dll1_r64(fnaddr)^(params^[1])
    when 2 then
        x := dll2_r64(fnaddr)^(params^[1], params^[2])
    else
        println "calldll/c/real too many params"
        stop 1
    end switch
    return i64@(x)            # type-punning cast
end
