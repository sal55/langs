! C to M Converter

import clib

importdll dummy =
end


enum (PowerSystemUnspecified = 0)
enum (PowerSystemWorking = 1)
enum (PowerSystemSleeping1 = 2)
enum (PowerSystemSleeping2 = 3)
enum (PowerSystemSleeping3 = 4)
enum (PowerSystemHibernate = 5)
enum (PowerSystemShutdown = 6)
enum (PowerSystemMaximum = 7)
enum (PowerActionNone = 0)
enum (PowerActionReserved = 0+1)
enum (PowerActionSleep = 0+2)
enum (PowerActionHibernate = 0+3)
enum (PowerActionShutdown = 0+4)
enum (PowerActionShutdownReset = 0+5)
enum (PowerActionShutdownOff = 0+6)
enum (PowerActionWarmEject = 0+7)
enum (PowerDeviceUnspecified = 0)
enum (PowerDeviceD0 = 0+1)
enum (PowerDeviceD1 = 0+2)
enum (PowerDeviceD2 = 0+3)
enum (PowerDeviceD3 = 0+4)
enum (PowerDeviceMaximum = 0+5)
enum (AclRevisionInformation = 1)
enum (AclSizeInformation = 1+1)
enum (SecurityAnonymous = 0)
enum (SecurityIdentification = 1)
enum (SecurityImpersonation = 2)
enum (SecurityDelegation = 3)
enum (SidTypeUser = 1)
enum (SidTypeGroup = 1+1)
enum (SidTypeDomain = 1+2)
enum (SidTypeAlias = 1+3)
enum (SidTypeWellKnownGroup = 1+4)
enum (SidTypeDeletedAccount = 1+5)
enum (SidTypeInvalid = 1+6)
enum (SidTypeUnknown = 1+7)
enum (TokenUser = 1)
enum (TokenGroups = 1+1)
enum (TokenPrivileges = 1+2)
enum (TokenOwner = 1+3)
enum (TokenPrimaryGroup = 1+4)
enum (TokenDefaultDacl = 1+5)
enum (TokenSource = 1+6)
enum (TokenType = 1+7)
enum (TokenImpersonationLevel = 1+8)
enum (TokenStatistics = 1+9)
enum (TokenPrimary = 1)
enum (TokenImpersonation = 1+1)
enum (LT_DONT_CARE = 0)
enum (LT_LOWEST_LATENCY = 1)
enum (SHGDN_NORMAL = 0)
enum (SHGDN_INFOLDER = 1)
enum (SHGDN_FOREDITING = 4096)
enum (SHGDN_FORADDRESSBAR = 16384)
enum (SHGDN_FORPARSING = 32768)
enum (MEMCTX_TASK = 1)
enum (MEMCTX_SHARED = 2)
enum (MEMCTX_MACSYSTEM = 3)
enum (MEMCTX_UNKNOWN = -1)
enum (MEMCTX_SAME = -2)
enum (CLSCTX_INPROC_SERVER = 1)
enum (CLSCTX_INPROC_HANDLER = 2)
enum (CLSCTX_LOCAL_SERVER = 4)
enum (CLSCTX_INPROC_SERVER16 = 8)
enum (CLSCTX_REMOTE_SERVER = 16)
enum (MSHLFLAGS_NORMAL = 0)
enum (MSHLFLAGS_TABLESTRONG = 1)
enum (MSHLFLAGS_TABLEWEAK = 2)
enum (MSHLFLAGS_NOPING = 4)
enum (MSHCTX_LOCAL = 0)
enum (MSHCTX_NOSHAREDMEM = 1)
enum (MSHCTX_DIFFERENTMACHINE = 2)
enum (MSHCTX_INPROC = 3)
enum (MSHCTX_CROSSCTX = 4)
enum (DVASPECT_CONTENT = 1)
enum (DVASPECT_THUMBNAIL = 2)
enum (DVASPECT_ICON = 4)
enum (DVASPECT_DOCPRINT = 8)
enum (STGC_DEFAULT = 0)
enum (STGC_OVERWRITE = 1)
enum (STGC_ONLYIFCURRENT = 2)
enum (STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE = 4)
enum (STGC_CONSOLIDATE = 8)
enum (STGMOVE_MOVE = 0)
enum (STGMOVE_COPY = 1)
enum (STGMOVE_SHALLOWCOPY = 2)
enum (STATFLAG_DEFAULT = 0)
enum (STATFLAG_NONAME = 1)
enum (STATFLAG_NOOPEN = 2)
enum (ExeNamePath = 0)
enum (DllNamePath = ExeNamePath+1)
enum (TlbNamePath = DllNamePath+1)
enum (CabFilePath = TlbNamePath+1)
enum (InfFilePath = CabFilePath+1)
enum (DrwFilePath = InfFilePath+1)
enum (SetupNamePath = DrwFilePath+1)
enum (JobObjectBasicAccountingInformation = 1)
enum (JobObjectBasicLimitInformation = 1+1)
enum (JobObjectBasicProcessIdList = 1+2)
enum (JobObjectBasicUIRestrictions = 1+3)
enum (JobObjectSecurityLimitInformation = 1+4)
enum (JobObjectEndOfJobTimeInformation = 1+5)
enum (JobObjectAssociateCompletionPortInformation = 1+6)
enum (JobObjectBasicAndIoAccountingInformation = 1+7)
enum (JobObjectExtendedLimitInformation = 1+8)
enum (JobObjectJobSetInformation = 1+9)
enum (MaxJobObjectInfoClass = 1+10)
enum (FindExInfoStandard = 0)
enum (FindExInfoMaxInfoLevel = 1)
enum (FindExSearchNameMatch = 0)
enum (FindExSearchLimitToDirectories = 1)
enum (FindExSearchLimitToDevices = 2)
enum (FindExSearchMaxSearchOp = 3)
enum (GetFileExInfoStandard = 0)
enum (GetFileExMaxInfoLevel = 1)
enum (AuditEventObjectAccess = 0)
enum (AuditEventDirectoryServiceAccess = 1)
enum (RelationProcessorCore = 0)
enum (RelationNumaNode = 1)
enum (ComputerNameNetBIOS = 0)
enum (ComputerNameDnsHostname = 1)
enum (ComputerNameDnsDomain = 2)
enum (ComputerNameDnsFullyQualified = 3)
enum (ComputerNamePhysicalNetBIOS = 4)
enum (ComputerNamePhysicalDnsHostname = 5)
enum (ComputerNamePhysicalDnsDomain = 6)
enum (ComputerNamePhysicalDnsFullyQualified = 7)
enum (ComputerNameMax = 8)
enum (FindStreamInfoStandard = 0)
enum (FindStreamInfoMaxInfoLevel = 1)
enum (HeapCompatibilityInformation = 0)
enum (WinNullSid = 0)
enum (WinWorldSid = 1)
enum (WinLocalSid = 2)
enum (WinCreatorOwnerSid = 3)
enum (WinCreatorGroupSid = 4)
enum (WinCreatorOwnerServerSid = 5)
enum (WinCreatorGroupServerSid = 6)
enum (WinNtAuthoritySid = 7)
enum (WinDialupSid = 8)
enum (WinNetworkSid = 9)
enum (WinBatchSid = 10)
enum (WinInteractiveSid = 11)
enum (WinServiceSid = 12)
enum (WinAnonymousSid = 13)
enum (WinProxySid = 14)
enum (WinEnterpriseControllersSid = 15)
enum (WinSelfSid = 16)
enum (WinAuthenticatedUserSid = 17)
enum (WinRestrictedCodeSid = 18)
enum (WinTerminalServerSid = 19)
enum (WinRemoteLogonIdSid = 20)
enum (WinLogonIdsSid = 21)
enum (WinLocalSystemSid = 22)
enum (WinLocalServiceSid = 23)
enum (WinNetworkServiceSid = 24)
enum (WinBuiltinDomainSid = 25)
enum (WinBuiltinAdministratorsSid = 26)
enum (WinBuiltinUsersSid = 27)
enum (WinBuiltinGuestsSid = 28)
enum (WinBuiltinPowerUsersSid = 29)
enum (WinBuiltinAccountOperatorsSid = 30)
enum (WinBuiltinSystemOperatorsSid = 31)
enum (WinBuiltinPrintOperatorsSid = 32)
enum (WinBuiltinBackupOperatorsSid = 33)
enum (WinBuiltinReplicatorSid = 34)
enum (WinBuiltinPreWindows2000CompatibleAccessSid = 35)
enum (WinBuiltinRemoteDesktopUsersSid = 36)
enum (WinBuiltinNetworkConfigurationOperatorsSid = 37)
enum (WinAccountAdministratorSid = 38)
enum (WinAccountGuestSid = 39)
enum (WinAccountKrbtgtSid = 40)
enum (WinAccountDomainAdminsSid = 41)
enum (WinAccountDomainUsersSid = 42)
enum (WinAccountDomainGuestsSid = 43)
enum (WinAccountComputersSid = 44)
enum (WinAccountControllersSid = 45)
enum (WinAccountCertAdminsSid = 46)
enum (WinAccountSchemaAdminsSid = 47)
enum (WinAccountEnterpriseAdminsSid = 48)
enum (WinAccountPolicyAdminsSid = 49)
enum (WinAccountRasAndIasServersSid = 50)
enum (WinNTLMAuthenticationSid = 51)
enum (WinDigestAuthenticationSid = 52)
enum (WinSChannelAuthenticationSid = 53)
enum (WinThisOrganizationSid = 54)
enum (WinOtherOrganizationSid = 55)
enum (WinBuiltinIncomingForestTrustBuildersSid = 56)
enum (WinBuiltinPerfMonitoringUsersSid = 57)
enum (WinBuiltinPerfLoggingUsersSid = 58)
enum (WinBuiltinAuthorizationAccessSid = 59)
enum (WinBuiltinTerminalServerLicenseServersSid = 60)
enum (LowMemoryResourceNotification = 0)
enum (HighMemoryResourceNotification = 1)
enum (VT_EMPTY = 0)
enum (VT_NULL = 1)
enum (VT_I2 = 2)
enum (VT_I4 = 3)
enum (VT_R4 = 4)
enum (VT_R8 = 5)
enum (VT_CY = 6)
enum (VT_DATE = 7)
enum (VT_BSTR = 8)
enum (VT_DISPATCH = 9)
enum (VT_ERROR = 10)
enum (VT_BOOL = 11)
enum (VT_VARIANT = 12)
enum (VT_UNKNOWN = 13)
enum (VT_DECIMAL = 14)
enum (VT_I1 = 16)
enum (VT_UI1 = 17)
enum (VT_UI2 = 18)
enum (VT_UI4 = 19)
enum (VT_I8 = 20)
enum (VT_UI8 = 21)
enum (VT_INT = 22)
enum (VT_UINT = 23)
enum (VT_VOID = 24)
enum (VT_HRESULT = 25)
enum (VT_PTR = 26)
enum (VT_SAFEARRAY = 27)
enum (VT_CARRAY = 28)
enum (VT_USERDEFINED = 29)
enum (VT_LPSTR = 30)
enum (VT_LPWSTR = 31)
enum (VT_RECORD = 36)
enum (VT_FILETIME = 64)
enum (VT_BLOB = 65)
enum (VT_STREAM = 66)
enum (VT_STORAGE = 67)
enum (VT_STREAMED_OBJECT = 68)
enum (VT_STORED_OBJECT = 69)
enum (VT_BLOB_OBJECT = 70)
enum (VT_CF = 71)
enum (VT_CLSID = 72)
enum (VT_VERSIONED_STREAM = 73)
enum (VT_BSTR_BLOB = 4095)
enum (VT_VECTOR = 4096)
enum (VT_ARRAY = 8192)
enum (VT_BYREF = 16384)
enum (VT_RESERVED = 32768)
enum (VT_ILLEGAL = 65535)
enum (VT_ILLEGALMASKED = 4095)
enum (VT_TYPEMASK = 4095)
record symbol_init =
    ref byte name
    ref clang function()int32 fun
    int32 argc
    ref clang function()int32 setfun
    int32 setargc
    int32 sym
end

global []symbol_init symi = (("NIL"),("T"),("&REST"),("&BODY"),("&OPTIONAL"),("&KEY"),("&WHOLE"),("&ENVIRONMENT"),("&AUX"),("&ALLOW-OTHER-KEYS"),("DECLARE",&eval_declare,-1),("SPECIAL"),("QUOTE",&eval_quote,1),("LET",&eval_let,-2),("LET*",&eval_letm,-2),("FLET",&eval_flet,-2),("LABELS",&eval_labels,-2),("MACROLET",&eval_macrolet,-2),("SYMBOL-MACROLET",&eval_symbol_macrolet,-2),("SETQ",&eval_setq,2),("FUNCTION",&eval_function,1),("TAGBODY",&eval_tagbody,-1),("GO",&eval_go,1),("BLOCK",&eval_block,-2),("RETURN-FROM",&eval_return_from,2),("CATCH",&eval_catch,-2),("THROW",&eval_throw,-2),("UNWIND-PROTECT",&eval_unwind_protect,-2),("IF",&eval_if,-3),("MULTIPLE-VALUE-CALL",&eval_multiple_value_call,-2),("MULTIPLE-VALUE-PROG1",&eval_multiple_value_prog1,-2),("PROGN",&eval_body,-1),("PROGV",&eval_progv,-3),("_SETF",&eval_setf,2),("FINISH-FILE-STREAM",&lfinish_fs,1),("MAKEI",&lmakei,-3),("DPB",&ldpb,3),("LDB",&lldb,2),("BACKQUOTE"),("UNQUOTE"),("UNQUOTE-SPLICING"),("IBOUNDP",&liboundp,2),("LISTEN-FILE-STREAM",&llisten_fs,1),("LIST",&llist,-1),("VALUES",&lvalues,-1),("FUNCALL",&lfuncall,-2),("APPLY",&lapply,-2),("EQ",&leq,2),("CONS",&lcons,2),("CAR",&lcar,1,&setfcar,2),("CDR",&lcdr,1,&setfcdr,2),("=",&lequ,-2),("<",&lless,-2),("+",&lplus,-1),("-",&lminus,-2),("*",&ltimes,-1),("/",&ldivi,-2),("MAKE-FILE-STREAM",&lmake_fs,2),("HASH",&lhash,1),("IERROR"),("GENSYM",&lgensym,0),("STRING",&lstring,-1),("FASL",&lfasl,1),("MAKEJ",&lmakej,2),("MAKEF",&lmakef,0),("FREF",&lfref,1),("PRINT",&lprint,1),("GC",&gc,0),("CLOSE-FILE-STREAM",&lclose_fs,1),("IVAL",&lival,1),("FLOOR",&lfloor,-2),("READ-FILE-STREAM",&lread_fs,3),("WRITE-FILE-STREAM",&lwrite_fs,4),("LOAD",&lload,1),("IREF",&liref,2,&setfiref,3),("LAMBDA"),("CODE-CHAR",&lcode_char,1),("CHAR-CODE",&lchar_code,1),("*STANDARD-INPUT*"),("*STANDARD-OUTPUT*"),("*ERROR-OUTPUT*"),("*PACKAGES*"),("STRING=",&lstring_equal,2),("IMAKUNBOUND",&limakunbound,2),("EVAL",&leval,-2),("JREF",&ljref,2,&setfjref,3),("RUN-PROGRAM",&lrp,-2),("UNAME",&luname,0))
global ref int32 memory
global ref int32 memf
global int32 memory_size
global ref int32 stack
global int32 xvalues = 8
global int32 dyns = 0
global [128]int32 top_jmp
global int32 pkg
global int32 pkgs
global int32 kwp = 0
global int32 gensymc = 0
global ref FILE ins
global []ref byte exmsg = ("variable unbound","function unbound","array index out of bounds","go tag not bound","block name not bound","catch tag not dynamically bound","too many arguments","too few arguments","dynamic extent of block exited","dynamic extent of tagbody exited")

global function o2c(int32 o)ref int32 =
    return ref int32(o-1)
end

global function c2o(ref int32 c)int32 =
    return int32(c)+1
end

global function cp(int32 o)int32 =
    return o iand 3=1
end

global function o2a(int32 o)ref int32 =
    return ref int32(o-2)
end

global function a2o(ref int32 a)int32 =
    return int32(a)+2
end

global function ap(int32 o)int32 =
    return o iand 3=2
end

global function o2s(int32 o)ref int32 =
    return ref int32(o-3)
end

global function o2z(int32 o)ref byte =
    return ref byte(o-3+2*4)
end

global function s2o(ref int32 s)int32 =
    return int32(s)+3
end

global function sp(int32 o)int32 =
    return o iand 3=3
end

global function car(int32 c)int32 =
    return (c iand 3=1|o2c(c)[0]|0)
end

global function cdr(int32 c)int32 =
    return (c iand 3=1|o2c(c)[1]|0)
end

global function caar(int32 c)int32 =
    return car(car(c))
end

global function lread(ref int32 g)int32 =
    int32 c
    real64 d

    c := getnws()
    if c=-1 then
        return 8
    fi
    if c='(' then
        return read_list(g)
    fi
    if c='"' then
        return stringify(g,read_string_list(g))
    fi
    if c=''' then
        return list2(g,12)
    fi
    if c='#' then
        c := getnws()
        if c=''' then
            return list2(g,20)
        fi
        return 0
    fi
    if c='`' then
        return list2(g,38)
    fi
    if c=',' then
        c := getnws()
        if c='@' then
            return list2(g,40)
        fi
        ungetc(c,ins)
        return list2(g,39)
    fi
    ungetc(c,ins)
    if isdigit(c) then
        fscanf(ins,"%lf",&d)
        return d2o(g,d)
    fi
    if c=':' then
        getnws()
    fi
    return is(g,(c=':'|kwp|pkg),stringify(g,read_symbol(g)))
end

global function cdar(int32 c)int32 =
    return cdr(car(c))
end

global function evca(ref int32 f, int32 co)int32 =
    int32 ex
    int32 x
    int32 m
    int32 fn
    int32 i
    ref int32 g

    ex := car(co)
    x := ex
ag:
    xvalues := 8
    if cp(ex) then
        fn := 8
        if ap(car(ex)) and o2a(car(ex))[1]=20 then
            i := o2a(car(ex))[7]>>3
            if i>11 and i<34 then
                return symi[i].fun(f,cdr(ex))
            fi
            fn := (binding(f,car(ex),1,&m))^
            if m then
                g := f+1
                ex := cdr(ex)
                while ex do
                    (++g)^ := car(ex)
                    ex := cdr(ex)
                od
                x := ex := call(f,fn,g-f-1)
                set_car(co,ex)
                goto ag
            fi
        fi
st:
        if fn=8 then
            if dbgr(f,1,car(ex),&fn) then
                return fn
            else
                goto st
            fi
        fi
        ex := cdr(ex)
        ex := call(f,fn,map_eval(f,ex))
    else
        if ap(ex) and o2a(ex)[1]=20 then
            ex := (binding(f,ex,0,&m))^
            if m then
                x := ex
                set_car(co,ex)
                goto ag
            fi
            if ex=8 then
                dbgr(f,0,x,&ex)
            fi
        fi
    fi
    return (ex=-8|o2a(x)[4]|ex)
end

global function cadr(int32 c)int32 =
    return car(cdr(c))
end

global function dbgr(ref int32 f, int32 x, val, ref int32 vp)int32 =
    int32 ex
    int32 i
    ref int32 h
    int32 l
    ref int32 g
    ref int32 j

    h := f
    l := 0
    g := f+0+3
    f[1] := 0
    g[-1] := 0<<5 ior 16
    g^ := f^
    ex := o2a(symi[59].sym)[5]
    if ex<>8 then
        h++
        (++h)^ := d2o(f,x)
        (++h)^ := val
        ex := call(f,ex,h-f-1)
        $mcclongjmp(top_jmp,1)
    fi
    printf(";exception: %s ",exmsg[x])
    if val then
        print(val)
    fi
    printf("\n;restarts:\n;[t]oplevel\n;[u]se <form> instead\n;[r]eturn <form> from function\n")
    do
        printf(";%d> ",l)
        ex := lread(g)
        if ex=8 then
            $mcclongjmp(top_jmp,1)
        fi
        if sp(ex) and o2s(ex)[1]=84 then
            h := f
            l := i := o2i(ex)
            while i do
                if not h[2] then
                    exit
                fi
                h := o2a(h[2])
                i--
            od
        else
            if ap(ex) and o2a(ex)[1]=20 then
                switch o2z(o2a(ex)[2])[0]
                when 'B' then
                    printf(";backtrace:\n")
                    j := f
                    i := 0
                    while j do
                        printf(";%d: ",i)
                        if j[0]>>5=4 then
                            print(o2a(j[5])[6])
                            printf(" ")
                            print(j[4])
                        fi
                        printf("\n")
                        if not j[2] then
                            exit
                        fi
                        j := o2a(j[2])
                        i++
                    od
                when 'R' then
                    vp^ := eval(g,lread(g))
                    return 1
                when 'T' then
                    $mcclongjmp(top_jmp,1)
                    fallthrough
                when 'U' then
                    vp^ := eval(g,lread(g))
                    return 0
                end switch
            else
                ep(h,ex)
            fi
        fi
    od
end

global function cddr(int32 c)int32 =
    return cdr(cdr(c))
end

global proc print(int32 x) =
    int32 i
    byte c

    switch x iand 3
    when 0 then
        if x then
            if x iand 8 then
                if x>>5<256 and isgraph(x>>5) then
                    printf("#\\%c",x>>5)
                else
                    printf("#\\U+%d",x>>5)
                fi
            else
                printf("%d",x>>5)
            fi
        else
            printf("nil")
        fi
    when 1 then
        printf("(")
        print(car(x))
        x := cdr(x)
        while cp(x) do
            printf(" ")
            print(car(x))
            x := cdr(x)
        od
        if x then
            printf(" . ")
            print(x)
        fi
        printf(")")
    when 2 then
        switch o2a(x)[1]
        when 212 then
            printf("#<function ")
            print(o2a(x)[6])
            printf(">")
        when 20 then
            psym(o2a(x)[9],o2a(x)[2])
        when 116 then
            printf("#(")
            i := 0
            while i<o2a(x)[0]>>8 do
                if i then
                    printf(" ")
                fi
                print(o2a(x)[i+2])
                i++
            od
            printf(")")
        when 180 then
            printf("#<package ")
            print(car(o2a(x)[2]))
            printf(">")
        else
            if ap(o2a(x)[1]) then
                printf("#<")
                print(o2a(o2a(o2a(x)[1]-4)[2])[2])
                printf(">")
            else
                printf("#(")
                i := 0
                while i<=o2a(x)[0]>>8 do
                    print(o2a(x)[i+1])
                    i++
                od
                printf(")")
            fi
        end switch
    when 3 then
        switch o2s(x)[1]
        when 20 then
            printf("\"")
            i := 0
            while i<o2s(x)[0]/64-4 do
                c := o2z(x)[i]
                printf((c='\' or c='"'|"\\%c"|"%c"),c)
                i++
            od
            printf("\"")
        when 84 then
            printf("%g",o2d(x))
        end switch
    end switch
end

global function set_car(int32 c, val)int32 =
    return o2c(c)[0] := val
end

global function set_cdr(int32 c, val)int32 =
    return o2c(c)[1] := val
end

global function binding(ref int32 f, int32 sym, type, ref int32 macro)ref int32 =
    int32 env
    int32 e

st:
    env := f^
    while env do
        e := caar(env)
        if (type or cp(e)|car(e)=sym and cdr(e)>>4=type|e=sym) then
            if macro then
                macro^ := cp(e) and cdr(e) iand 8
            fi
            return o2c(car(env))+1
        fi
        env := cdr(env)
    od
    if macro then
        macro^ := o2a(sym)[8]>>type iand 32
    fi
    if type>2 then
        dbgr(f,type,sym,&sym)
        goto st
    fi
    return o2a(sym)+4+type
end

global proc gcm(int32 v) =
    ref int32 t
    int32 i

st:
    t := ref int32(v iand inot 3)
    if v iand 3 and not t[0] iand 4 then
        t[0] ior:= 4
        switch v iand 3
        when 1 then
            gcm(t[0]-4)
            v := t[1]
            goto st
        when 2 then
            gcm(t[1]-4)
            if t[0]>>8 then
                i := 1
                while i<t[0]>>8 do
                    gcm(t[i+1])
                    i++
                od
                v := t[i+1]
                goto st
            fi
        end switch
    fi
end

global function gc(ref int32 f)int32 =
    int32 i
    ref int32 m
    int32 l
    int32 u
    int32 ml
    ref int32 n

    u := 0
    printf(";garbage collecting...\n")
    while memf do
        n := ref int32(memf[0])
        memset(memf,0,4*memf[1])
        memf := ref int32(n)
    od
    gcm(xvalues)
    gcm(pkgs)
    gcm(dyns)
    while f>stack do
        if f^ iand 3 and f^<memory or f^>memory+memory_size/4 then
            printf("%x\n",f^)
        fi
        gcm(f^)
        f--
    od
    memf := 0
    m := memory
    i := 0
    while m<memory+memory_size/4 do
        l := (m[1] iand 4|m[0]>>8|0)+1 iand inot 1
        if m[0] iand 4 then
            if u then
                m[-ml] := int32(memf)
                m[1-ml] := ml
                memf := m-ml
                u := 0
                i +:= ml
            fi
        else
            if not u then
                ml := 0
            fi
            ml +:= l+2
            u := 1
        fi
        m[0] iand:= inot 4
        m +:= l+2
    od
    if u then
        m[-ml] := int32(memf)
        m[1-ml] := ml
        memf := m-ml
        i +:= ml
    fi
    printf(";done. %d free.\n",i)
    return 0
end

global function m0(ref int32 g, int32 n)ref int32 =
    ref int32 m
    ref int32 p

    m := memf
    p := 0
    n := n+1 iand inot 1
    while m do
        if n<=m[1] then
            if m[1]=n then
                if p then
                    p[0] := m[0]
                else
                    memf := ref int32(m[0])
                fi
            else
                m[1] -:= n
                m +:= m[1]
            fi
            return m
        fi
        p := m
        m := ref int32(m[0])
    od
    return 0
end

global function ma0(ref int32 g, int32 n)ref int32 =
    ref int32 m

st:
    m := m0(g,n+2)
    if not m then
        gc(g)
        goto st
    fi
    m^ := n<<8
    return m
end

global function ms0(ref int32 g, int32 n)ref int32 =
    ref int32 m

st:
    m := m0(g,n+12/4)
    if not m then
        gc(g)
        goto st
    fi
    m^ := n+4<<6
    return m
end

global function mb0(ref int32 g, int32 n)ref int32 =
    ref int32 m

st:
    m := m0(g,n+95/32)
    if not m then
        gc(g)
        goto st
    fi
    m^ := n+31<<3
    return m
end

global function ma(ref int32 g, int32 n, ...)int32 =
    ref byte v
    int32 i
    ref int32 m

st:
    v := ref byte(&n)+8
    m := m0(g,n+2)
    if not m then
        i := -1
        while i<n do
            gcm((ref int32(v+:=8-8))^)
            i++
        od
        gc(g)
        goto st
    fi
    m^ := n<<8
    i := -1
    while i<n do
        m[2+i] := (ref int32(v+:=8-8))^
        i++
    od
    return a2o(m)
end

global function ms(ref int32 g, int32 n, ...)int32 =
    ref byte v
    int32 i
    ref int32 m

st:
    v := ref byte(&n)+8
    m := m0(g,n+2)
    if not m then
        gc(g)
        goto st
    fi
    m^ := n<<8
    i := -1
    while i<n do
        m[2+i] := (ref int32(v+:=8-8))^
        i++
    od
    return s2o(m)
end

global function o2d(int32 o)real64 =
    return (sp(o)|(ref real64(o2s(o)+2))^|o>>5)
end

global function d2o(ref int32 g, real64 d)int32 =
    int32 x
    ref int32 a

    x := int32(d)<<5 ior 16
    if o2d(x)=d then
        return x
    fi
    a := ma0(g,2)
    a[1] := 84
    (ref real64(a+2))^ := d
    return s2o(a)
end

global function o2i(int32 o)int32 =
    return int32(o2d(o))
end

global function o2u(int32 o)word32 =
    return word32(o2d(o))
end

global function cons(ref int32 g, int32 a, d)int32 =
    ref int32 c

    c := m0(g,2)
    if not c then
        gcm(a)
        gcm(d)
        gc(g)
        c := m0(g,2)
    fi
    c[0] := a
    c[1] := d
    return c2o(c)
end

global function string_equal_do(int32 a, b)int32 =
    int32 i

    i := 0
    while i<o2s(a)[0]/64-4 do
        if o2z(a)[i]<>o2z(b)[i] then
            return 0
        fi
        i++
    od
    return 1
end

global function string_equal(int32 a, b)int32 =
    return a=b or sp(a) and sp(b) and o2s(a)[1]=20 and o2s(b)[1]=20 and o2s(a)[0]=o2s(b)[0] and string_equal_do(a,b)
end

global function argi(int32 a, ref int32 b)int32 =
    if cp(a) then
        b^ := cdr(a)
        return car(a)
    fi
    b^ := 0
    return a
end

global function rest(ref int32 h, g)int32 =
    ref int32 f
    int32 r

    f := h-1
    r := 0
    while f>=g do
        r := cons(h,f^,r)
        f--
    od
    return r
end

global function args(ref int32 f, int32 m, c)int32 =
    ref int32 g
    ref int32 h
    int32 t
    int32 k
    ref int32 l
    int32 n

    g := f+1
    h := f+c+2
st:
    t := 0
    while cp(m) do
        n := car(m)
        m := cdr(m)
        switch (cp(n)|-1|o2a(n)[7]>>3)
        when 2,3 then
            t := 1
            next
            fallthrough
        when 4 then
            t := 2
            next
            fallthrough
        when 5 then
            t := -2
            next
            fallthrough
        when 6 then
            t := 4
            next
            fallthrough
        when 7 then
            t := 5
            next
            fallthrough
        else
            switch t
            when 0 then
                if g>=h-1 then
                    dbgr(g,7,0,h)
                    goto st
                fi
                h^ := argd(h,n,g^)
            when 1 then
                h^ := cons(h,cons(h,n,rest(h-1,g)),h^)
                t := -1
                next
                fallthrough
            when 2 then
                n := argi(n,&k)
                h^ := argd(h,n,(g<h-1|g^|evca(h,k)))
            when -2 then
                n := argi(n,&k)
                l := g
                while l<h-1 do
                    if string_equal(o2a(n)[2],o2a(l^)[2]) and o2a(l^)[9]=kwp then
                        k := l[1]
                        exit
                    fi
                    l +:= 2
                od
                h^ := argd(h,n,(l<h-1|k|evca(h,k)))
                next
                fallthrough
            when 4 then
                h^ := cons(h,cons(h,n,rest(h-1,f+1)),h^)
                t := 0
                next
                fallthrough
            when 5 then
                h^ := cons(h,cons(h,n,f[-1]),h^)
                t := 0
                next
            end switch
        end switch
        g++
    od
    if m then
        return cons(h,cons(h,m,rest(h-1,g)),h^)
    fi
    if g<h-1 and t>=0 then
        h[-1] := c<<5 ior 16
        dbgr(h,6,0,h)
        goto st
    fi
    return h^
end

global function argd(ref int32 f, int32 n, a)int32 =
    ref int32 h

    if cp(n) then
        h := f
        while a do
            (++h)^ := car(a)
            a := cdr(a)
        od
        ++h
        (++h)^ := f^
        return args(f,n,h-f-2)
    fi
    return cons(f,cons(f,n,a),f^)
end

global function eval_body(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    while ex do
        g[-2] := evca(g,ex)
        ex := cdr(ex)
    od
    return g[-2]
end

global function map_eval(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+3
    while ex do
        g[-1] := g-f-3<<5 ior 16
        g^ := f^
        g[-1] := evca(g,ex)
        ex := cdr(ex)
        g++
    od
    return g-f-3
end

global function eval(ref int32 f, int32 expr)int32 =
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    g[-2] := cons(g,expr,0)
    return evca(g,g[-2])
end

global function rvalues(ref int32 g, int32 v)int32 =
    return (xvalues=8|cons(g,v,0)|xvalues)
end

global function mvalues(int32 a)int32 =
    xvalues := a
    return car(a)
end

global function infn(ref int32 f, h)int32 =
    [128]int32 jmp
    int32 vs
    ref int32 g
    int32 fn
    int32 d

    g := h+1
    fn := f^
    d := h-f-1
    h[1] := o2a(fn)[3]
    g^ := args(f,o2a(fn)[4],d)
    g[-1] := cons(g,dyns,ms(g,1,20,&jmp))
    g^ := cons(g,cons(g,cons(g,o2a(fn)[6],64),g[-1]),g^)
    g[-1] := d<<5 ior 16
    if not vs := $mccsetjmp(jmp) then
        return eval_body(g,o2a(fn)[5])
    fi
    return mvalues(car(vs))
end

global function call(ref int32 f, int32 fn, word32 d)int32 =
    ref int32 g

    g := f+d+3
    xvalues := 8
    if o2a(fn)[1]=20 then
        fn := o2a(fn)[5]
    fi
    if o2a(fn)[0] iand 16 then
        fn := o2a(fn)[3]
    fi
    (++f)^ := fn
    fn := o2a(fn)[2]
    if d<word32(o2s(fn)[3]) then
        dbgr(g,7,0,f)
    fi
    if d>word32(o2s(fn)[4]) then
        dbgr(g,6,0,f)
    fi
    return ref clang function()int32(o2s(fn)[2])(f,f+d+1)
end

global function eval_quote(ref int32 g, int32 ex)int32 =
    return car(ex)
end

global function specp(ref int32 f, int32 ex, s)int32 =
    int32 e
    int32 sp

    while ex do
        if ap(caar(ex)) and o2a(caar(ex))[7]=3<<3 then
            e := cdar(ex)
            while e do
                if o2a(caar(e))[7]=4<<3 then
                    sp := cdar(e)
                    while sp do
                        if car(sp)=s then
                            return 1
                        fi
                        sp := cdr(sp)
                    od
                fi
                e := cdr(e)
            od
        else
            exit
        fi
        ex := cdr(ex)
    od
    return 0
end

global proc unwind(ref int32 f, int32 c) =
    int32 e
    ref int32 g

    g := f+0+3
    f[1] := 0
    g[-1] := 0<<5 ior 16
    g^ := f^
    while dyns<>c do
        if ap(car(dyns)) then
            if o2a(car(dyns))[1]=52 then
                g^ := o2a(car(dyns))[2]
                eval_body(g,o2a(car(dyns))[3])
            else
                e := o2a(car(dyns))[2]
                while e do
                    o2a(caar(e))[4] := cdar(e)
                    e := cdr(e)
                od
            fi
        else
            o2s(car(dyns))[2] := 0
        fi
        dyns := cdr(dyns)
    od
end

global function eval_let(ref int32 f, int32 ex)int32 =
    int32 r
    ref int32 g

    g := f+3+3
    f[1] := 0
    g[-1] := 3<<5 ior 16
    g^ := f^
    g[-2] := car(ex)
    g[-3] := f^
    g[-4] := 0
    r := ma(g,1,84,0)
    dyns := cons(g,r,dyns)
    while g[-2] do
        g[-4] := evca(g,cdar(g[-2]))
        if o2a(caar(g[-2]))[8] iand 128 or specp(g,cdr(ex),caar(g[-2])) then
            o2a(r)[2] := cons(g,cons(g,caar(g[-2]),g[-4]),o2a(r)[2])
        else
            g[-3] := cons(g,cons(g,caar(g[-2]),g[-4]),g[-3])
        fi
        g[-2] := cdr(g[-2])
    od
    r := o2a(r)[2]
    while r do
        g[-2] := o2a(caar(r))[4]
        o2a(caar(r))[4] := cdar(r)
        set_cdr(car(r),g[-2])
        g[-3] := cons(g,cons(g,caar(r),-8),g[-3])
        r := cdr(r)
    od
    g^ := g[-3]
    g[-2] := eval_body(g,cdr(ex))
    unwind(g,cdr(dyns))
    return g[-2]
end

global function eval_letm(ref int32 f, int32 ex)int32 =
    int32 r
    ref int32 g

    g := f+2+3
    f[1] := 0
    g[-1] := 2<<5 ior 16
    g^ := f^
    g[-2] := g[-3] := 0
    r := ma(g,1,84,0)
    dyns := cons(g,r,dyns)
    g[-2] := car(ex)
    while g[-2] do
        g[-3] := evca(g,cdar(g[-2]))
        if o2a(caar(g[-2]))[8] iand 128 or specp(g,cdr(ex),caar(g[-2])) then
            o2a(r)[2] := cons(g,cons(g,caar(g[-2]),o2a(caar(g[-2]))[4]),o2a(r)[2])
            o2a(caar(g[-2]))[4] := g[-3]
            g[-3] := -8
        fi
        g[-3] := cons(g,caar(g[-2]),g[-3])
        g^ := cons(g,g[-3],g^)
        g[-2] := cdr(g[-2])
    od
    g[-2] := eval_body(g,cdr(ex))
    unwind(g,cdr(dyns))
    return g[-2]
end

global function eval_progv(ref int32 f, int32 ex)int32 =
    int32 r
    ref int32 g

    g := f+2+3
    f[1] := 0
    g[-1] := 2<<5 ior 16
    g^ := f^
    g[-2] := g[-3] := 0
    r := ma(g,1,84,0)
    g[-2] := evca(g,ex)
    g[-3] := evca(g,cdr(ex))
    dyns := cons(g,r,dyns)
    while g[-2] and g[-3] do
        o2a(r)[2] := cons(g,cons(g,car(g[-2]),o2a(car(g[-2]))[4]),o2a(r)[2])
        o2a(car(g[-2]))[4] := car(g[-3])
        g[-2] := cdr(g[-2])
        g[-3] := cdr(g[-3])
    od
    g[-2] := eval_body(g,cddr(ex))
    unwind(f,cdr(dyns))
    return g[-2]
end

global function eval_flet(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+4+3
    f[1] := 0
    g[-1] := 4<<5 ior 16
    g^ := f^
    g[-4] := g[-5] := 0
    g[-3] := f^
    g[-2] := car(ex)
    while g[-2] do
        g[-4] := ma(g,5,212,ms(f,3,212,&infn,0,-1),f^,cadr(car(g[-2])),cddr(car(g[-2])),caar(g[-2]))
        g[-5] := cons(g,caar(g[-2]),16)
        g[-4] := cons(g,g[-5],g[-4])
        g[-3] := cons(g,g[-4],g[-3])
        g[-2] := cdr(g[-2])
    od
    g^ := g[-3]
    return eval_body(g,cdr(ex))
end

global function eval_labels(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+4+3
    f[1] := 0
    g[-1] := 4<<5 ior 16
    g^ := f^
    g[-4] := g[-5] := 0
    g[-3] := f^
    g[-2] := car(ex)
    while g[-2] do
        g[-3] := cons(g,0,g[-3])
        g[-2] := cdr(g[-2])
    od
    g^ := g[-3]
    g[-2] := car(ex)
    while g[-2] do
        g[-4] := ma(g,5,212,ms(f,3,212,&infn,0,-1),g^,cadr(car(g[-2])),cddr(car(g[-2])),caar(g[-2]))
        g[-5] := cons(g,caar(g[-2]),16)
        set_car(g[-3],cons(g,g[-5],g[-4]))
        g[-2] := cdr(g[-2])
        g[-3] := cdr(g[-3])
    od
    return eval_body(g,cdr(ex))
end

global function eval_macrolet(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+4+3
    f[1] := 0
    g[-1] := 4<<5 ior 16
    g^ := f^
    g[-4] := g[-5] := 0
    g[-3] := f^
    g[-2] := car(ex)
    while g[-2] do
        g[-4] := ma(g,5,212,ms(f,3,212,&infn,0,-1),f^,cadr(car(g[-2])),cddr(car(g[-2])),caar(g[-2]))
        g[-5] := cons(g,caar(g[-2]),24)
        g[-4] := cons(g,g[-5],g[-4])
        g[-3] := cons(g,g[-4],g[-3])
        g[-2] := cdr(g[-2])
    od
    g^ := g[-3]
    return eval_body(g,cdr(ex))
end

global function eval_symbol_macrolet(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+3+3
    f[1] := 0
    g[-1] := 3<<5 ior 16
    g^ := f^
    g[-4] := 0
    g[-3] := f^
    g[-2] := car(ex)
    while g[-2] do
        g[-4] := cons(g,caar(g[-2]),8)
        g[-4] := cons(g,g[-4],cadr(car(g[-2])))
        g[-3] := cons(g,g[-4],g[-3])
        g[-2] := cdr(g[-2])
    od
    g^ := g[-3]
    return eval_body(g,cdr(ex))
end

global function eval_setq(ref int32 f, int32 ex)int32 =
    int32 r

    repeat 
        r := evca(f,cdr(ex))
        (binding(f,car(ex),0,0))^ := r
        ex := cddr(ex)
    until not(ex)
    return r
end

global function eval_function(ref int32 f, int32 ex)int32 =
    int32 x
    int32 n

    ex := car(ex)
    if cp(ex) then
        if car(ex)=symi[75].sym then
            n := 0
            x := cddr(ex)
            if not cdr(x) and caar(x)=symi[23].sym then
                x := car(x)
                n := cadr(x)
                x := cddr(x)
            fi
            return ma(f,5,212,ms(f,3,212,&infn,0,-1),f^,cadr(ex),x,n)
        else
            x := (binding(f,cadr(ex),2,0))^
        fi
    else
        x := (binding(f,ex,1,0))^
    fi
    if x<>8 then
        return x
    fi
    dbgr(f,1,ex,&x)
    return x
end

global function eval_tagbody(ref int32 f, int32 ex)int32 =
    [128]int32 jmp
    int32 tag
    int32 e
    ref int32 g

    g := f+2+3
    f[1] := 0
    g[-1] := 2<<5 ior 16
    g^ := f^
    g[-2] := g[-3] := 0
    g[-3] := ms(g,1,52,&jmp)
    dyns := cons(g,g[-3],dyns)
    e := ex
    while e do
        if ap(car(e)) then
            g[-2] := cons(g,dyns,g[-3])
            g^ := cons(g,cons(g,cons(g,car(e),48),g[-2]),g^)
        fi
        e := cdr(e)
    od
    e := ex
again:
    if not tag := $mccsetjmp(jmp) then
        while e do
            if not ap(car(e)) then
                evca(g,e)
            fi
            e := cdr(e)
        od
    else
        e := ex
        while e do
            if car(e)=tag then
                e := cdr(e)
                goto again
            fi
            e := cdr(e)
        od
    fi
    unwind(g,cdr(dyns))
    return 0
end

global function eval_go(ref int32 f, int32 ex)int32 =
    int32 b

    b := (binding(f,car(ex),3,0))^
    if o2s(cdr(b))[2] then
        unwind(f,car(b))
        $mcclongjmp((ref [128]int32(o2s(cdr(b))[2]))^,car(ex))
    fi
    dbgr(f,9,car(ex),&ex)
    $mcclongjmp(top_jmp,1)
end

global function eval_block(ref int32 f, int32 ex)int32 =
    [128]int32 jmp
    int32 vs
    ref int32 g

    g := f+2+3
    f[1] := 0
    g[-1] := 2<<5 ior 16
    g^ := f^
    g[-2] := g[-3] := 0
    g[-2] := ms(g,1,52,&jmp)
    g[-3] := cons(g,dyns,g[-2])
    dyns := cons(g,g[-2],dyns)
    g^ := cons(g,cons(g,cons(g,car(ex),64),g[-3]),g^)
    if not vs := $mccsetjmp(jmp) then
        g[-2] := eval_body(g,cdr(ex))
        unwind(g,cdr(dyns))
        return g[-2]
    fi
    return mvalues(car(vs))
end

global function eval_return_from(ref int32 f, int32 ex)int32 =
    int32 b
    ref [128]int32 jmp
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    b := (binding(g,car(ex),4,0))^
    jmp := ref [128]int32(o2s(cdr(b))[2])
    if jmp then
        unwind(g,car(b))
        g[-2] := rvalues(g,evca(g,cdr(ex)))
        $mcclongjmp(jmp^,cons(g,g[-2],0))
    fi
    dbgr(g,8,car(ex),&g[-2])
    $mcclongjmp(top_jmp,1)
end

global function eval_catch(ref int32 f, int32 ex)int32 =
    [128]int32 jmp
    int32 vs
    int32 oc
    ref int32 g

    oc := dyns
    g := f+2+3
    f[1] := 0
    g[-1] := 2<<5 ior 16
    g^ := f^
    g[-2] := g[-3] := 0
    g[-3] := evca(g,ex)
    g[-2] := ms(g,1,20,&jmp)
    g[-2] := cons(g,g[-3],g[-2])
    dyns := cons(g,g[-2],dyns)
    if not vs := $mccsetjmp(jmp) then
        vs := eval_body(g,cdr(ex))
    else
        vs := mvalues(car(vs))
    fi
    dyns := oc
    return vs
end

global function eval_throw(ref int32 f, int32 ex)int32 =
    int32 c
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    g[-2] := evca(g,ex)
st:
    c := dyns
    while c do
        if cp(car(c)) and caar(c)=g[-2] then
            unwind(g,c)
            g[-2] := evca(g,cdr(ex))
            g[-2] := rvalues(g,g[-2])
            $mcclongjmp((ref [128]int32(o2s(cdar(c))[2]))^,cons(g,g[-2],0))
        fi
        c := cdr(c)
    od
    dbgr(g,5,g[-2],&g[-2])
    goto st
end

global function eval_unwind_protect(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    g[-2] := ma(g,2,52,f^,cdr(ex))
    dyns := cons(g,g[-2],dyns)
    g[-2] := evca(g,ex)
    g[-2] := rvalues(g,g[-2])
    unwind(g,cdr(dyns))
    return mvalues(g[-2])
end

global function eval_if(ref int32 f, int32 ex)int32 =
    return evca(f,(evca(f,ex)|cdr(ex)|cddr(ex)))
end

global function eval_multiple_value_call(ref int32 f, int32 ex)int32 =
    ref int32 g
    int32 l

    g := f+3
    f[1] := evca(f,ex)
    ex := cdr(ex)
    while ex do
        g^ := f^
        g[-1] := g-f-3<<5 ior 16
        l := rvalues(g,evca(g,ex))
        while l do
            g[-1] := car(l)
            g++
            l := cdr(l)
        od
        ex := cdr(ex)
    od
    xvalues := 8
    return call(f,f[1],g-f-3)
end

global function eval_multiple_value_prog1(ref int32 f, int32 ex)int32 =
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    g[-2] := evca(g,ex)
    g[-2] := rvalues(g,g[-2])
    eval_body(g,cdr(ex))
    return mvalues(g[-2])
end

global function eval_declare(ref int32 f, int32 ex)int32 =
    return 0
end

global function l2(ref int32 f, int32 a, b)int32 =
    return cons(f,a,cons(f,b,0))
end

global function eval_setf(ref int32 f, int32 ex)int32 =
    int32 r
    int32 m
    ref int32 g

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
ag:
    if not cp(car(ex)) then
        r := (binding(g,car(ex),0,&m))^
        if not m then
            return (binding(g,car(ex),0,0))^ := evca(g,cdr(ex))
        fi
        set_car(ex,r)
        goto ag
    fi
    r := (binding(g,caar(ex),2,0))^
    if r=8 then
        dbgr(g,1,l2(f,symi[33].sym,caar(ex)),&r)
    fi
    g[-2] := cons(g,cadr(ex),cdar(ex))
    return call(g,r,map_eval(g,g[-2]))
end

global function llist(ref int32 f, h)int32 =
    return rest(h,f+1)
end

global function lvalues(ref int32 f, h)int32 =
    return mvalues(rest(h,f+1))
end

global function lfuncall(ref int32 f, h)int32 =
    return call(f,f[1],h-f-2)
end

global function lapply(ref int32 f, h)int32 =
    while h[-1] do
        h[0] := cdr(h[-1])
        h[-1] := car(h[-1])
        h++
    od
    return call(f,f[1],h-f-3)
end

global function leq(ref int32 f)int32 =
    return (f[1]=f[2]|symi[1].sym|0)
end

global function lcons(ref int32 f)int32 =
    return cons(f,f[1],f[2])
end

global function lcar(ref int32 f)int32 =
    return car(f[1])
end

global function setfcar(ref int32 f)int32 =
    return set_car(f[2],f[1])
end

global function lcdr(ref int32 f)int32 =
    return cdr(f[1])
end

global function setfcdr(ref int32 f)int32 =
    return set_cdr(f[2],f[1])
end

global function lequ(ref int32 f, h)int32 =
    real64 s

    s := o2d(f[1])
    f +:= 2
    while f<h do
        if s<>o2d(f^) then
            return 0
        fi
        f++
    od
    return symi[1].sym
end

global function lless(ref int32 f, h)int32 =
    real64 s

    s := o2d(f[1])
    f +:= 2
    while f<h do
        if s<o2d(f^) then
            s := o2d(f^)
        else
            return 0
        fi
        f++
    od
    return symi[1].sym
end

global function lplus(ref int32 f, h)int32 =
    real64 s

    s := 0
    f++
    while f<h do
        s +:= o2d(f^)
        f++
    od
    return d2o(f,s)
end

global function lminus(ref int32 f, h)int32 =
    real64 s

    s := o2d(f[1])
    f +:= 2
    if f<h then
        while f<h do
            s -:= o2d(f^)
            f++
        od
    else
        s := -s
    fi
    return d2o(f,s)
end

global function ltimes(ref int32 f, h)int32 =
    real64 s

    s := 1
    f++
    while f<h do
        s *:= o2d(f^)
        f++
    od
    return d2o(f,s)
end

global function ldivi(ref int32 f, h)int32 =
    real64 s

    s := o2d(f[1])
    f +:= 2
    if f<h then
        while f<h do
            s /:= o2d(f^)
            f++
        od
    else
        s := 1/s
    fi
    return d2o(f,s)
end

global function ldpb(ref int32 f)int32 =
    int32 s
    int32 p
    int32 m

    s := o2i(car(f[2]))
    p := o2i(cdr(f[2]))
    m := 1<<s-1
    return d2o(f,o2i(f[1]) iand m<<p ior o2i(f[3]) iand inot m<<p)
end

global function lldb(ref int32 f)int32 =
    int32 s
    int32 p

    s := o2i(car(f[1]))
    p := o2i(cdr(f[1]))
    return d2o(f,o2i(f[2])>>p iand 1<<s-1)
end

global function lfloor(ref int32 f, h)int32 =
    real64 n
    real64 d
    real64 q

    n := o2d(f[1])
    d := (h-f>2|o2d(f[2])|1)
    q := floor(n/d)
    return mvalues(l2(f,d2o(f,q),d2o(f,n-q*d)))
end

global function lgensym(ref int32 f)int32 =
    ref int32 r

    r := ms0(f,4)
    r[1] := 20
    sprintf(ref byte(r+2),"g%3.3d",gensymc++)
    return ma(f,9,20,s2o(r),0,8,8,8,-8,16,0,0)
end

global function lcode_char(ref int32 f)int32 =
    word32 c

    c := o2u(f[1])
    return (c<256|32*c+24|0)
end

global function lchar_code(ref int32 f)int32 =
    return f[1] iand inot 8
end

global function lmakef(ref int32 f)int32 =
    return d2o(f,f-stack)
end

global function lfref(ref int32 f)int32 =
    return stack[o2i(f[1])]
end

global function stringify(ref int32 f, int32 l)int32 =
    int32 i
    ref int32 r
    int32 t

    t := l
    (++f)^ := l
    i := 0
    while t do
        i++
        t := cdr(t)
    od
    r := ms0(f,i)
    r[1] := 20
    ref byte(r)[i+8] := 0
    i := 8
    while l do
        ref byte(r)[i] := car(l)>>5
        i++
        l := cdr(l)
    od
    return s2o(r)
end

global function lstring(ref int32 f, h)int32 =
    return stringify(f,rest(h,f+1))
end

global function lival(ref int32 f)int32 =
    return d2o(f,f[1])
end

global function lmakei(ref int32 f, h)int32 =
    int32 i
    int32 l
    ref int32 r

    i := 2
    l := o2i(f[1])
    r := ma0(h,l)
    r[1] := f[2] ior 4
    memset(r+2,0,4*o2i(f[1]))
    f +:= 3
    while f<h do
        if i>=l+2 then
            printf("overinitializing in makei\n")
        fi
        r[i] := f^
        f++
        i++
    od
    return a2o(r)
end

global function liboundp(ref int32 f)int32 =
    return (o2a(f[1])[o2u(f[2])]=8|0|symi[1].sym)
end

global function limakunbound(ref int32 f)int32 =
    o2a(f[1])[o2u(f[2])] := 8
    return 0
end

global function liref(ref int32 f)int32 =
    if o2u(f[2])>=o2a(f[1])[0]/256+2 then
        write(1,"out of bounds in iref\n",22)
    fi
    return ref int32(f[1] iand inot 3)[o2u(f[2])] iand inot 4
end

global function setfiref(ref int32 f)int32 =
    int32 i

    i := o2i(f[3])
    if i>=o2a(f[2])[0]/256+2 then
        printf("out of bounds in setf iref\n")
    fi
    return ref int32(f[2] iand inot 3)[i] := (i=1|f[1] ior 4|f[1])
end

global function lmakej(ref int32 f)int32 =
    ref int32 r

    r := mb0(f,o2i(f[1]))
    r[1] := o2i(f[2])
    memset(r+2,0,o2i(f[1])+7/8)
    return s2o(r)
end

global function ljref(ref int32 f)int32 =
    return d2o(f,o2s(f[1])[o2u(f[2])])
end

global function setfjref(ref int32 f)int32 =
    return o2s(f[2])[o2u(f[3])] := o2u(f[1])
end

global function lmake_fs(ref int32 f)int32 =
    ref void fd

    fd := CreateFileA(o2z(f[1]),(f[2]|GENERIC_WRITE|GENERIC_READ),(f[2]|FILE_SHARE_WRITE|FILE_SHARE_READ),0,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0)
    return ms(f,4,116,1,fd,f[2],0)
end

global function lclose_fs(ref int32 f)int32 =
    CloseHandle(o2s(f[1])[3])
    return 0
end

global function llisten_fs(ref int32 f)int32 =
    return (WaitForSingleObject(o2s(f[1])[3],0)=WAIT_OBJECT_0|symi[1].sym|0)
end

global function lread_fs(ref int32 f)int32 =
    int32 l

    l := o2i(f[3])
    if not ReadFile(o2s(f[1])[3],o2z(f[2])+l,o2s(f[2])[0]>>6-4-l,&l,0) then
        return 0
    fi
    return d2o(f,l)
end

global function lwrite_fs(ref int32 f)int32 =
    int32 l

    l := o2i(f[3])
    if not WriteFile(o2s(f[1])[3],o2z(f[2])+l,o2i(f[4])-l,&l,0) then
        return 0
    fi
    return d2o(f,l)
end

global function lfinish_fs(ref int32 f)int32 =
    FlushFileBuffers(o2s(f[1])[3])
    return 0
end

global function lfasl(ref int32 f)int32 =
    ref void h
    ref clang function()int32 s

    h := LoadLibraryA(o2z(f[1]))
    s := GetProcAddress(h,"init")
    return s(f)
end

global function luname(ref int32 f)int32 =
    OSVERSIONINFO osvi

    osvi.dwOSVersionInfoSize := 20
    GetVersionExA(&osvi)
    f[1] := cons(f+1,strf(f+1,osvi.szCSDVersion),0)
    f[1] := cons(f+1,d2o(f+1,osvi.dwBuildNumber),f[1])
    f[1] := cons(f+1,d2o(f+1,osvi.dwMinorVersion),f[1])
    return cons(f+1,d2o(f,osvi.dwMajorVersion),f[1])
end

global proc load(ref int32 f, ref byte s) =
    int32 r
    ref FILE oldins

    oldins := ins
    ins := fopen(s,"r")
    if ins then
        repeat 
            r := eval(f,lread(f))
        until not(r<>8)
        fclose(ins)
    fi
    ins := oldins
end

global function lload(ref int32 f)int32 =
    load(f,o2z(f[1]))
    return symi[1].sym
end

global function lstring_equal(ref int32 f)int32 =
    return (string_equal(f[1],f[2])|symi[1].sym|0)
end

global function leval(ref int32 f, h)int32 =
    f[-1] := (h-f>2|f[2]|0)
    return eval(f-1,f[1])
end

global proc psym(int32 p, n) =
    int32 i
    int32 m

    if not p then
        printf("#:")
    else
        if p<>pkg then
            m := car(o2a(p)[2])
            i := 0
            while i<o2s(m)[0]/64-4 do
                putchar(o2z(m)[i])
                i++
            od
            putchar(':')
        fi
    fi
    i := 0
    while i<o2s(n)[0]/64-4 do
        putchar(o2z(n)[i])
        i++
    od
end

global function lprint(ref int32 f)int32 =
    print(f[1])
    return f[1]
end

global function ep(ref int32 g, int32 expr)int32 =
    int32 i
    int32 v

    v := rvalues(g,eval(g,expr))
    if car(v)=8 then
        return 0
    fi
    if v then
        i := 0
        while v do
            printf(";%d: ",i++)
            print(car(v))
            printf("\n")
            v := cdr(v)
        od
    else
        printf(";no values\n")
    fi
    return 1
end

global function getnws()int32 =
    int32 c

    repeat 
        c := getc(ins)
    until not(isspace(c))
    return c
end

global function read_list(ref int32 f)int32 =
    int32 c
    ref int32 g
    int32 r

    g := f+1+3
    f[1] := 0
    g[-1] := 1<<5 ior 16
    g^ := f^
    g[-2] := 0
    c := getnws()
    if c=')' then
        return 0
    fi
    if c='.' then
        r := lread(g)
        getnws()
        return r
    fi
    ungetc(c,ins)
    g[-2] := lread(g)
    return cons(g,g[-2],read_list(g))
end

global function read_string_list(ref int32 g)int32 =
    int32 c

    c := getc(ins)
    if c='"' then
        return 0
    fi
    if c='\' then
        c := getc(ins)
    fi
    return cons(g,c<<5 ior 24,read_string_list(g))
end

global function hash(int32 s)word32 =
    ref byte z
    word32 i
    word32 h
    word32 g

    z := o2z(s)
    i := 0
    h := 0
    while i<o2s(s)[0]/64-4 do
        h := h<<4+z[i++]
        g := h iand 4026531840
        if g then
            h := h ixor g>>24 ixor g
        fi
    od
    return h
end

global function lhash(ref int32 f)int32 =
    return d2o(f,hash(f[1]))
end

global function is(ref int32 g, int32 p, s)int32 =
    int32 h
    int32 i
    int32 m
    int32 y

    h := hash(s) rem 1021
    i := 3
    while i<5 do
        m := o2a(o2a(p)[i])[2+h]
        while m do
            y := car(m)
            if string_equal(o2a(y)[2],s) then
                return (o2a(y)[7]|y|0)
            fi
            m := cdr(m)
        od
        i++
    od
    m := ma(g,9,20,s,0,8,8,8,-8,16,p,0)
    if p=kwp then
        o2a(m)[4] := m
    fi
    o2a(o2a(p)[3])[2+h] := cons(g,m,o2a(o2a(p)[3])[2+h])
    return m
end

global function read_symbol(ref int32 g)int32 =
    int32 c

    c := getc(ins)
    if isspace(c) or c=')' or c=-1 then
        if c<>-1 then
            ungetc(c,ins)
        fi
        return 0
    fi
    if c>96 and c<123 then
        c -:= 32
    fi
    return cons(g,c<<5 ior 24,read_symbol(g))
end

global function list2(ref int32 g, int32 a)int32 =
    return l2(g,symi[a].sym,lread(g))
end

global function strf(ref int32 f, ref byte s)int32 =
    int32 j
    ref int32 str

    j := strlen(s)
    str := ms0(f,j)
    str[1] := 20
    j++
    while j do
        ref byte(str)[7+j] := s[j-1]
        j--
    od
    return s2o(str)
end

global function mkv(ref int32 f)int32 =
    int32 i
    ref int32 r

    i := 2
    r := ma0(f,1021)
    r[1] := 116
    while i<1023 do
        r[i++] := 0
    od
    return a2o(r)
end

global function mkp(ref int32 f, ref byte s0, s1)int32 =
    return ma(f,6,180,l2(f,strf(f,s0),strf(f,s1)),mkv(f),mkv(f),0,0,0)
end

global function fr(ref int32 o, p, s, c, b, int32 x)int32 =
    int32 t

    if not x iand 3 then
        return x
    fi
    t := x>>30 iand 3
    x iand:= 1073741823
    switch t
    when 0 then
        return (sp(x)|int32(o)+x|int32(b)+x)
    when 1 then
        return c[x/4]
    when 2 then
        return s[x/4]
    else
        return p[x/4]
    end switch
end

global function fasr(ref int32 f, p, int32 pz, ref int32 s, sp, int32 sz, ref int32 c, int32 cz, ref int32 v, int32 vz, ref int32 o, int32 oz, ref ref int32 rv, ro)int32 =
    ref int32 x
    ref int32 y
    int32 i
    int32 l
    int32 j
    int32 pc
    int32 nc

    y := ma0(f,oz-2)
    memcpy(y,o,4*oz)
    i := 0
    while i<pz do
        pc := o2a(symi[81].sym)[4]
        while pc do
            nc := o2a(car(pc))[2]
            while nc do
                if string_equal(car(nc),s2o(y+p[i])) then
                    p[i] := car(pc)
                    exit
                fi
                nc := cdr(nc)
            od
            pc := cdr(pc)
        od
        i++
    od
    i := 0
    while i<sz do
        s[i] := is(f,p[sp[i]],s2o(y+s[i]))
        i++
    od
    i := 0
    while i<cz do
        c[i] := o2a(s[c[i]])[3]
        i++
    od
    x := ma0(f,vz-2)
    memcpy(x,v,4*vz)
    i := 0
    while i<vz do
        if x[i+1] iand 4 then
            l := x[i]>>8
            x[i+1] := fr(y,p,s,c,x,x[i+1]-4)+4
            j := 0
            while j<l do
                x[i+j+2] := fr(y,p,s,c,x,x[i+j+2])
                j++
            od
        else
            l := 0
            x[i] := fr(y,p,s,c,x,x[i])
            x[i+1] := fr(y,p,s,c,x,x[i+1])
        fi
        i +:= l+3 iand inot 1
    od
    rv^ := x
    ro^ := y
end

global function lrp(ref int32 f, h)int32 =
    STARTUPINFO si
    PROCESS_INFORMATION pi

    si := (0)
    pi := (0)
    si.cb := 104
    if CreateProcessA(o2z(f[1]),o2z(f[2]),0,0,FALSE,0,0,0,&si,&pi) then
        return symi[1].sym
    fi
    return 0
end

global function main(int32 argc, ref ref byte argv)int32 =
    ref int32 g
    int32 i
    int32 sym

    memory_size := 4*2048*1024
    memory := malloc(memory_size)
    memf := memory
    memset(memory,0,memory_size)
    memf[0] := 0
    memf[1] := memory_size/4
    stack := malloc(256*1024)
    memset(stack,0,256*1024)
    g := stack+5
    pkg := mkp(g,"CL","COMMON-LISP")
    i := 0
    while i<88 do
        sym := is(g,pkg,strf(g,symi[i].name))
        if i<10 then
            o2a(sym)[4] := sym
        fi
        ins := stdin
        symi[i].sym := sym
        if symi[i].fun then
            o2a(sym)[5] := ma(g,5,212,ms(g,3,212,symi[i].fun,0,-1),0,0,0,sym)
        fi
        if symi[i].setfun then
            o2a(sym)[6] := ma(g,5,212,ms(g,3,212,symi[i].setfun,0,-1),8,0,0,sym)
        fi
        o2a(sym)[7] := i<<3
        i++
    od
    kwp := mkp(g,"","KEYWORD")
    o2a(symi[81].sym)[4] := pkgs := l2(g,kwp,pkg)
    o2a(symi[78].sym)[4] := ms(g,3,116,1,GetStdHandle(4294967286),0,0)
    o2a(symi[79].sym)[4] := ms(g,3,116,1,GetStdHandle(4294967285),symi[1].sym,0)
    o2a(symi[80].sym)[4] := ms(g,3,116,1,GetStdHandle(4294967284),symi[1].sym,0)
    i := 1
    while i<argc do
        load(g,argv[i])
        i++
    od
    $mccsetjmp(top_jmp)
    repeat 
        printf("? ")
    until not(ep(g,lread(g)))
    return 0
    return 0
end

