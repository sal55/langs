!Z80 Parser module

global var opndreg, opndst, opndvalue, opndprefix

var allowisp=0

global proc parse=
    lxsymbol:=eolsym
    allowisp:=0

    while lxsymbol=eolsym do
        lex()

        case lxsymbol
        when opcodesym then
            readinstr()
            checksymbol(eolsym)

        when namesym then
            d:=lxsymptr
            lex()
            case lxsymbol
            when eqsym then
                lex()
                checksymbol(intconstsym)
                addnamedconst(d,lxvalue)
                lex()
            when colonsym then
                addlabel(d,ramptr-ramstart)
                lex()
            else
                serror("Unknown opcode or missing colon:"+d.name)
            esac

        when forwardsym then
            defforwardlabel(lxsymptr, ramptr-ramstart)
            lex()
            checksymbol(colonsym)
            lex()

        when labelsym then
            lxerror("Redefining label:"+lxsymptr.name)

        when eolsym then
        when eofsym then
            exit
        else
            serror("Unexpected symbol:"+symbolnames[lxsymbol])
        esac

    od
    
    undef::=()
    forall d in symbollist do
        if d.ksymbol=forwardsym then
            undef append:=d
        fi
    od
    if undef then
        println("Labels undefined:")
        forall d in undef do
            println "   ",d.name
        od
        lxerror("Stopping")
    fi

end

proc readinstr=
    opcode:=lxsubcode
    lex()

    switch opcode
    when m_ld then
        readld()

    when m_nop then genbyte(0)
    when m_halt then genbyte(0x76)
    when m_db then
        readdb()
    when m_dw then
        do
            readopnd(imm_opnd)
            setdatabyte(ramptr)
            setdatabyte(ramptr+1)
            genabs16()
            if lxsymbol<>commasym then exit fi
            lex()
        od

    when m_jp then
        cond:=readcond()
        readopnd(imm_opnd)
        genbyte((cond|0xC2, 0xD2, 0xE2, 0xF2, 0xCA, 0xDA, 0xEA, 0xFA|0xC3))
        genabs16()

    when m_call then
        cond:=readcond()
        readopnd(imm_opnd)
        genbyte((cond|0xC4, 0xD4, 0xE4, 0xF4, 0xCC, 0xDC, 0xEC, 0xFC|0xCD))
        genabs16()

    when m_ret then
        if lxsymbol=condcodesym then
            cond:=lxsubcode
            lex()
        else
            cond:=0
        fi
        genbyte((cond|0xC0, 0xD0, 0xE0, 0xF0, 0xC8, 0xD8, 0xE8, 0xF8|0xC9))

    when m_inc then
        readincr(0)

    when m_dec then
        readincr(1)

    when m_push then
        readpushpop(4)

    when m_pop then
        readpushpop(0)

    when m_jr then
        cond:=readcond()
        readopnd(imm_opnd)
        case cond
        when 0 then genbyte(0x18)
        when 1 then genbyte(0x20)
        when 2 then genbyte(0x30)
        when 5 then genbyte(0x28)
        when 6 then genbyte(0x38)
        else serror("JR cc?")
        esac
        genrel8()

    when m_djnz then
        readopnd(imm_opnd)
        genbyte(0x10)
        genrel8()

    when m_rst then
        readopnd(imm_opnd)
        case opndvalue
        when 0,8,16,24,32,40,48,56 then
        else
            serror("RST bad addr")
        esac
        genbyte((opndvalue%8+1|0xC7,0xCF,0xD7,0xDF,0xE7,0xEF,0xF7|0xFF))

    when m_syscall then
        readopnd(imm_opnd)
        genbyte(0xCF)
        genbyte(opndvalue)

    when m_scf then genbyte(0x37)
    when m_ccf then genbyte(0x3F)
    when m_not then
        if lxsymbol=reg8sym then
            if lxsubcode<>reg_a then serror("not?") fi
            lex()
        fi
        genbyte(0x2F)

    when m_ex then
        readex()

    when m_exx then genbyte(0xD9)

!   when m_test then
!       readtest()

    when m_rol, m_ror, m_rcl, m_rcr, m_shl, m_shr, m_sar then
        readshift(mclcodes[opcode])

    when m_add, m_adc, m_sub, m_sbc, m_and, m_xor, m_or, m_cmp then
        readarith(mclcodes[opcode])

    when m_ldi then genbyte(0xED); genbyte(0xA0)
    when m_ldir then genbyte(0xED); genbyte(0xB0)
    when m_ldd then genbyte(0xED); genbyte(0xA8)
    when m_lddr then genbyte(0xED); genbyte(0xB8)
    when m_cpi then genbyte(0xED); genbyte(0xA1)
    when m_cpir then genbyte(0xED); genbyte(0xB1)
    when m_cpd then genbyte(0xED); genbyte(0xA9)
    when m_cpdr then genbyte(0xED); genbyte(0xB9)

    when m_resb then
        readopnd(imm_opnd)
        x:=opndvalue
        n:=1
        if lxsymbol=commasym then
            lex()
            readopnd(imm_opnd)
            n:=opndvalue
        fi
        to n do
            setdatabyte(ramptr)
            genbyte(x)
        od

    else
        serror("Can't do this opcode:"+mclnames[opcode])
    end switch
end

proc checksymbol(symbol)=
    if lxsymbol<>symbol then
        lxerror(symbolnames[symbol]+" expected")
    fi
end

func readcond=
    if lxsymbol=condcodesym then
        cond:=lxsubcode
        lex()
        checksymbol(commasym)
        lex()
        return cond
    else
        return un_cond
    fi
end

func readopnd(expected=0)=
!positioned at possible first token of operand
!return operand code, and any extra data in opndreg, opndst, opndvalue
!token is known not to be eol

    opndreg:=opndvalue:=0
    opndst:=nil

    case lxsymbol
    when reg8sym then
        opndreg:=lxsubcode
        lex()
        opnd:=reg8_opnd

    when reg16sym then
        opndreg:=lxsubcode
        lex()
        opnd:=reg16_opnd

    when lsqsym then
        lex()
        case lxsymbol
        when reg16sym then
            opndreg:=lxsubcode
            if opndreg=reg_sp and not allowisp then serror("[sp]?") fi
            lex()
            checksymbol(rsqsym)
            lex()
            opnd:=ireg_opnd
        when regixsym then
            opndreg:=lxsubcode
            opndprefix:=(opndreg=reg_ix|0xDD|0xFD)
            lex()
            if lxsymbol in [addsym, subsym] then
                readexpr()
                if opndvalue not in -128..127 then serror("Offset out of range") fi
            fi
            checksymbol(rsqsym)
            lex()
            opnd:=iregix_opnd

        else
            readexpr()
            checksymbol(rsqsym)
            lex()
            opnd:=mem_opnd
        esac

    when regmiscsym then
        opndreg:=lxsubcode
        lex()
        opnd:=regmisc_opnd

    when regixsym then
        opndreg:=lxsubcode
        opndprefix:=(opndreg=reg_ix|0xDD|0xFD)
        lex()
        opnd:=regix_opnd

    else
        readexpr()
        opnd:=imm_opnd
    esac

    if expected and expected<>opnd then serror(opndnames[expected]+" expected") fi

    return opnd
end

proc readexpr=
    if lxsymbol=namesym then
        addforwardsym(lxsymptr)
        lxsymbol:=forwardsym
    fi

    case lxsymbol
    when forwardsym, labelsym then
        opndst:=lxsymptr
        lex()
        if lxsymbol not in [eolsym, rsqsym] then
            readoffset()
        fi
    else
        readoffset()
    esac
end

proc readoffset=

    case lxsymbol
    when subsym then
        lex()
        checksymbol(intconstsym)
        lxvalue := -lxvalue
    when addsym then            !treat as unary +
        lex()
    esac

    if lxsymbol=syscallsym then
        opndvalue:=lxsubcode
    else
        checksymbol(intconstsym)
        opndvalue:=lxvalue
    fi
    lex()

    while lxsymbol in [addsym, subsym, mulsym, divsym] do
        opc:=lxsymbol

        lex()
        checksymbol(intconstsym)
        case opc
        when addsym then opndvalue+:=lxvalue
        when subsym then opndvalue-:=lxvalue
        when mulsym then opndvalue*:=lxvalue
        when divsym then opndvalue%:=lxvalue
        esac
        lex()
    od

!   checksymbol(eolsym)
end

proc readincr(isdec)=
!isdec is 0 (inc) or 1 (dec)
!CPL "READI"
!PS("RI")
    case readopnd()
    when reg16_opnd then
        case opndreg
        when reg_bc then opc:=0x03
        when reg_de then opc:=0x13
        when reg_hl then opc:=0x23
        when reg_sp then opc:=0x33
        esac
        genbyte((isdec|opc+8|opc))
    when reg8_opnd then
        case opndreg
        when reg_a then opc:=0x3C
        when reg_b then opc:=0x04
        when reg_c then opc:=0x0C
        when reg_d then opc:=0x14
        when reg_e then opc:=0x1C
        when reg_h then opc:=0x24
        when reg_l then opc:=0x2C
        else
            goto error
        esac
        genbyte(opc+isdec)
    when ireg_opnd then
        if opndreg<>reg_hl then serror("inc[]") fi
        genbyte(0x34+isdec)
    else
error:
        serror("inc?")
    esac
end

proc readpushpop(pushoffset)=
    case readopnd()
    when reg16_opnd then
    
        case opndreg
        when reg_bc then genbyte(0xC1+pushoffset)
        when reg_de then genbyte(0xD1+pushoffset)
        when reg_hl then genbyte(0xE1+pushoffset)
        else
            error
        esac
    when regix_opnd then
        genbyte(opndprefix)
        genbyte(0xE1+pushoffset)

    when regmisc_opnd then
        if opndreg<>reg_af then goto error fi
        genbyte(0xF1+pushoffset)
    else
error:

        serror("push/pop bad reg")
    esac
end

proc readld=
!at symbol following 'ld'
    opnd:=readopnd()
    checksymbol(commasym)
    lex()

    case opnd
    when reg8_opnd then
        destreg:=opndreg
        opnd:=readopnd()

        case opnd
        when reg8_opnd then         !ld r,r
            genbyte(x:=2x01'000'000 + regcodes[destreg]<<3 + regcodes[opndreg])

        when ireg_opnd then         !ld r,[hl/bc/de]
            if opndreg=reg_hl then          
                genbyte(01'000'110B + regcodes[destreg]<<3)
            elsif destreg<>reg_a then
                goto error
            elsif opndreg=reg_bc then
                genbyte(0x0A)
            else
                genbyte(0x1A)
            fi

        when iregix_opnd then
            genbyte(opndprefix)
            genbyte(01'000'110B + regcodes[destreg]<<3)
            genbyte(opndvalue)
        when mem_opnd then          !ld a,[mem]
            if destreg<>reg_a then goto error fi
            genbyte(0x3A)
            genabs16()
        when imm_opnd then          !ld a,imm
            if opndst then goto error fi
            genbyte(00'000'110B + regcodes[destreg]<<3)
            genbyte(opndvalue)
        else
            goto error
        esac    

    when ireg_opnd then
        destreg:=opndreg
        opnd:=readopnd()
        case destreg
        when reg_bc, reg_de then
            if opndreg<>reg_a then goto error fi
            genbyte((destreg=reg_bc|0x02|0x12))
        else
            case opnd
            when reg8_opnd then
                genbyte(01'110'000B + regcodes[opndreg])
            when imm_opnd then
                genbyte(0x36)
                genbyte(opndvalue)
            else
                goto error
            esac

        esac
    when iregix_opnd then
        genbyte(opndprefix)
        disp:=opndvalue
        opnd:=readopnd()
        case opnd
        when reg8_opnd then
            genbyte(01'110'000B + regcodes[opndreg])
            genbyte(disp)
        when imm_opnd then
            genbyte(0x36)
            genbyte(disp)
            genbyte(opndvalue)
        else
            goto error
        esac

    when mem_opnd then
        st:=opndst
        value:=opndvalue
        opnd:=readopnd()
        case opnd
        when reg8_opnd then
            if opndreg<>reg_a then goto error fi
            genbyte(0x32)
        when reg16_opnd then
            case opndreg
            when reg_hl then genbyte(0x22)
            when reg_bc then genbyte(0xED); genbyte(0x43)
            when reg_de then genbyte(0xED); genbyte(0x53)
            when reg_sp then genbyte(0xED); genbyte(0x73)
            esac
        when regix_opnd then
            genbyte(opndprefix)
            genbyte(0x22)
        else
            goto error
        esac

        opndst:=st; opndvalue:=value
        genabs16()

    when reg16_opnd then
        destreg:=opndreg
        opnd:=readopnd()
        case opnd
        when imm_opnd then
            case destreg
            when reg_bc then genbyte(0x01)
            when reg_de then genbyte(0x11)
            when reg_hl then genbyte(0x21)
            when reg_sp then genbyte(0x31)
            esac
            genabs16()

        when mem_opnd then
            if destreg<>reg_hl then genbyte(0xED) fi
            case destreg
            when reg_bc then genbyte(0x4B)
            when reg_de then genbyte(0x5B)
            when reg_hl then genbyte(0x2A)
            when reg_sp then genbyte(0x7B)
            esac
            genabs16()

        when reg16_opnd then
            if destreg=reg_sp and opndreg=reg_hl then genbyte(0xF9) else goto error fi
        when regix_opnd then
            if destreg=reg_sp then
                genbyte(opndprefix)
                genbyte(0xF9)
            else
                goto error
            fi
        else
            goto error
        esac

    when regix_opnd then
        genbyte(opndprefix)
        opnd:=readopnd()
        case opnd
        when imm_opnd then
            genbyte(0x21)
            genabs16()
        when mem_opnd then
            genbyte(0x2A)
            genabs16()
        else
            goto error
        esac
    else
error:
        serror("Bad LD opnd")
    esac
end

proc readex=
    allowisp:=1
    opnd:=readopnd()
    allowisp:=0

    case opnd
    when regmisc_opnd then      !assume af
        genbyte(0x08)
    when reg16_opnd then
        checksymbol(commasym)
        lex()
        if opndreg<>reg_de then
            goto error
        fi
        readopnd(reg16_opnd)
        if opndreg<>reg_hl then
            goto error
        fi
        genbyte(0xEB)

    when ireg_opnd then
        if opndreg<>reg_sp then
            goto error
        fi
        checksymbol(commasym)
        lex()
        case readopnd()
        when reg16_opnd then
            if opndreg<>reg_hl then goto error fi
            genbyte(0xE3)
        when regix_opnd then
            genbyte(opndprefix)
            genbyte(0xE3)
        else
            goto error
        esac
    else
error:
        serror("Invalid")
    esac
end

proc readshift(code)=
    opnd:=readopnd()

    if opnd=reg8_opnd and opndreg=reg_a and code in [0..3] then
        genbyte(00'000'111B+code<<3)
        return
    fi

    if opnd=iregix_opnd then
        genbyte(opndprefix)
    fi
    genbyte(0xCB)
    if opnd=iregix_opnd then
        genbyte(opndvalue)
    fi

    case opnd
    when reg8_opnd then
        genbyte(code<<3 + regcodes[opndreg])
    when ireg_opnd then
        if opndreg<>reg_hl then serror("shl[hl] only") fi
        genbyte(code<<3 + 00'000'110B)

    when iregix_opnd then
        genbyte(code<<3 + 00'000'110B)
    else
        serror("Bad shift")
    esac
end

proc readaddix(code)=
    if code<>0 then goto error fi

    genbyte(opndprefix)
    reg:=opndreg

    case readopnd()
    when reg16_opnd then
        case opndreg
        when reg_bc then genbyte(0x09)
        when reg_de then genbyte(0x19)
        when reg_sp then genbyte(0x39)
        else
            goto error
        esac
    when regix_opnd then
        if opndreg<>reg then goto error fi
        genbyte(0x29)
    else
error:
        serror("add ix, xxx?")
    esac

end

proc readarith16(code)=
    if opndreg<>reg_hl then
error:
        serror("ADD16?")
    fi

    case code
    when 0 then opc:=0x09                   !add
    when 1 then genbyte(0xED); opc:=0x4A    !adc
    when 3 then genbyte(0xED); opc:=0x42    !sbc
    else goto error
    esac

    readopnd(reg16_opnd)
    case opndreg
    when reg_bc then genbyte(opc)
    when reg_de then genbyte(opc+0x10)
    when reg_hl then genbyte(opc+0x20)
    when reg_sp then genbyte(opc+0x30)
    esac
end

proc readarith(code)=
    opnd:=readopnd()
    checksymbol(commasym)
    lex()

    case opnd
    when reg16_opnd then
        readarith16(code)
        return
    when regix_opnd then
        readaddix(code)
        return
    esac

    if opndreg<>reg_a then serror("Arith: not A") fi

    opnd:=readopnd()

    case opnd
    when reg8_opnd then
        genbyte(10'000'000B+code<<3+regcodes[opndreg])

    when imm_opnd then
        genbyte(11'000'110B+code<<3)
        genbyte(opndvalue)

    when ireg_opnd then
        if opndreg<>reg_hl then serror("Not (hl)") fi
        genbyte(10'000'110B+code<<3)

    when iregix_opnd then
        genbyte(opndprefix)
        genbyte(10'000'110B+code<<3)
        genbyte(opndvalue)

    else
error:
        serror("Invalid Arith opnds")
    esac
end

proc readdb=
    do
        if lxsymbol=stringconstsym then
            foreach c in lxvalue do
                setdatabyte(ramptr)
                genbyte(c)
            od
            setdatabyte(ramptr)
            genbyte(0)
            lex()

        else
            readopnd(imm_opnd)
            if opndst then serror("db/lab?") fi
            setdatabyte(ramptr)
            genbyte(opndvalue)
        fi
        if lxsymbol<>commasym then exit fi
        lex()
    od
end

proc setdatabyte(p)=
!set corresponding data flag for the ram byte that p points to
    ramflags[p-ramstart]:=1
end
