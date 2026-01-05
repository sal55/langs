!Z80 Emulator Test

const targetprog = "while.z"

[0:]byte program = binclude(targetprog)

const ramsize = 8192
[ramsize]byte memory

global record regfile =
    union
        struct
            u8 c, b,  e, d,  l, h,
            u8 flags: (cf:1, nf:1, pvf:1, x3:1, hf:1, x5:1, zf:1, sf:1)
            u8 a
        end
        [0..7]u8 abc            !access cbedlh-a via index 0..7
        struct
            u16 bc, de, hl, af
        end
    end
    u16 sp, ix, iy
    byte iff
    byte imode
end

!this maps regcode 0..7 (b c d e h l (hl) a) to something that can
!index regs.abc
global [0:]byte regmap = (1, 0, 3, 2, 5, 4, 7, 6)

global enumdata =
    sf_print_startfile,
    sf_print_startstr,
    sf_print_startptr,
    sf_print_startcon,
    sf_print_setfmt,
    sf_print_nogap,
    sf_print_space,
    sf_print_i16,
    sf_print_i16_nf,
    sf_print_u16,
    sf_print_hex,
    sf_print_r16,
    sf_print_r24,
    sf_print_str,
    sf_print_str_nf,
    sf_print_ptr,
    sf_print_ptr_nf,
    sf_print_c8,
    sf_print_bool,
    sf_print_newline,
    sf_print_end,
    sf_read_i16,
    sf_read_r24,
    sf_read_str,
    sf_read_fileline,
    sf_read_strline,
    sf_read_conline,

    sf_getnprocs,
    sf_getprocname,
    sf_getprocaddr,

    sf_power_i16,
    sf_sign_i16,
    sf_sign_r24,
    sf_print_bin,
    sf_print_bin8,
    sf_allocheap,
    sf_unimpl,
end

!these have values from 000B to 111B, bits 3..5 of CB shift/rot opcodes
global enumdata =
    m_rol = 0,
    m_ror,
    m_rcl,
    m_rcr,
    m_shl,
    m_sar,
    m_sal,
    m_shr,
end

const SF = 128
const ZF =  64
const YF =  32
const HF =  16
const XF =   8
const PF =   4
const NF =   2
const CF =   1

const rotmask = inot (HF ior NF)        !used to zero those flags

macro getsign8(value) = value iand 0x80
macro getzero (value) = (value=0|ZF|0)
macro getxy = regs.flags iand (YF ior XF)


global regfile regs
global regfile altregs

int pcptr
int clockcount
int instrcount

global type tmem = ^[0:]byte

global macro read16(a) = ^u16(&ram[a])^
global macro write16(a, x) = ^u16(&ram[a])^:=x

macro clocks(n) = ticks+:=n

tmem ram

ichar infile

global byte fquiet

proc main=
    int a

    ram:=&memory
    for i in program.bounds do
        ram[i]:=program[i]
    end

    println "Run ", targetprog,,":"
    runprogram(ram)

end

int pc
int ticks
int counter

func execloop: int =
    static int icount
    static int t
    pc:=pcptr
    ticks:=clockcount
    counter:=0
    icount:=0

PRINTLN "START EXEC"

!-----------------------------------------------
    doswitch ++icount;  ram[pc++]
!-----------------------------------------------

    when 0x00 then      ! nop
        clocks(4)

    when 0x01 then      ! ld bc, nn
        regs.bc:=read16(pc)
        pc+:=2
        clocks(10)

    when 0x02 then      ! ld (bc), a
        ram[regs.bc]:=regs.a
        clocks(7)

    when 0x03 then      ! inc bc
        ++regs.bc
        clocks(6)

    when 0x04 then      ! inc b
        regs.b:=inc8(regs.b)
        clocks(4)

    when 0x05 then      ! dec b
        regs.b:=dec8(regs.b)
        clocks(4)

    when 0x06 then      ! ld b, n
        regs.b:=ram[pc]
        ++pc
        clocks(7)

    when 0x07 then      ! rol a
        regs.a :=rol8(regs.a)
        clocks(4)

    when 0x08 then      ! ex af, af'
        swap(regs.af, altregs.af)
        clocks(4)

    when 0x09 then      ! add hl, bc
        regs.hl := add16(regs.hl, regs.bc)
        clocks(11)

    when 0x0A then      ! ld a, (bc)
        regs.a:=ram[regs.bc]
        clocks(7)

    when 0x0B then      ! dec bc
        --regs.bc
        clocks(6)

    when 0x0C then      ! inc c
        regs.c:=inc8(regs.c)
        clocks(4)

    when 0x0D then      ! dec c
        regs.c:=dec8(regs.c)
        clocks(4)

    when 0x0E then      ! ld c, n
        regs.c:=ram[pc]
        ++pc
        clocks(7)

    when 0x0F then      ! ror a
        regs.a := ror8(regs.a)
        clocks(4)

    when 0x10 then      ! djnz n
        --regs.b
        if regs.b then
            pc+:=i8(ram[pc])+1
            clocks(13)
        else
            ++pc
            clocks(8)
        end

    when 0x11 then      ! ld de, nn
        regs.de:=read16(pc)
        pc+:=2
        clocks(10)

    when 0x12 then      ! ld (de), a
        ram[regs.de]:=regs.a
        clocks(7)

    when 0x13 then      ! inc de
        ++regs.de
        clocks(6)

    when 0x14 then      ! inc d
        regs.d:=inc8(regs.d)
        clocks(4)

    when 0x15 then      ! dec d
        regs.d:=dec8(regs.d)
        clocks(4)

    when 0x16 then      ! ld d, n
        regs.d:=ram[pc]
        ++pc
        clocks(7)

    when 0x17 then      ! rcl a
        regs.a := ror9(regs.a)
        clocks(4)

    when 0x18 then      ! jr n
        pc+:=iwiden(ram[pc])+1
        clocks(7)

    when 0x19 then      ! add hl, de
        regs.hl := add16(regs.hl, regs.de)
        clocks(11)

    when 0x1A then      ! ld a, (de)
        regs.a:=ram[regs.de]
        clocks(7)

    when 0x1B then      ! dec de
        --regs.de
        clocks(6)

    when 0x1C then      ! inc e
        regs.e:=inc8(regs.e)
        clocks(4)

    when 0x1D then      ! dec e
        regs.e:=dec8(regs.e)
        clocks(4)

    when 0x1E then      ! ld e, n
        regs.e:=ram[pc]
        ++pc
        clocks(7)

    when 0x1F then      ! rcr a
        regs.a := ror9(regs.a)
        clocks(4)

    when 0x20 then      ! jr nz, n
        if not regs.zf then
            pc+:=iwiden(ram[pc])+1
            clocks(7)
        else
            ++pc
            clocks(2)
        end

    when 0x21 then      ! ld hl, nn
        regs.hl:=read16(pc)
        pc+:=2
        clocks(10)

    when 0x22 then      ! ld (nn), hl
        write16(read16(pc), regs.hl)
        pc+:=2
        clocks(16)

    when 0x23 then      ! inc hl
        ++regs.hl
        clocks(6)

    when 0x24 then      ! inc h
        regs.h:=inc8(regs.h)
        clocks(4)

    when 0x25 then      ! dec h
        regs.h:=dec8(regs.h)
        clocks(4)

    when 0x26 then      ! ld h, n
        regs.h:=ram[pc]
        ++pc
        clocks(7)

    when 0x27 then      ! daa
dodaa:
        counter:=0

    when 0x28 then      ! jr z, n
        if regs.zf then
            pc+:=iwiden(ram[pc])+1
            clocks(7)
        else
            ++pc
            clocks(2)
        end

    when 0x29 then      ! add hl, hl
        regs.hl := add16(regs.hl, regs.hl)
        clocks(11)

    when 0x2A then      ! ld hl, (nn)
        regs.hl:=read16(read16(pc))
        pc+:=2
        clocks(16)

    when 0x2B then      ! dec hl
        --regs.hl
        clocks(6)

    when 0x2C then      ! inc l
        regs.l:=inc8(regs.l)
        clocks(4)

    when 0x2D then      ! dec l
        regs.l:=dec8(regs.l)
        clocks(4)

    when 0x2E then      ! ld l, n
        regs.l:=ram[pc]
        ++pc
        clocks(7)

    when 0x2F then      ! not a
        regs.a := compl(regs.a)
        clocks(4)

    when 0x30 then      ! jr nc, n
        if not regs.cf then
            pc+:=iwiden(ram[pc])+1
            clocks(7)
        else
            ++pc
            clocks(2)
        end

    when 0x31 then      ! ld sp, nn
        regs.sp:=read16(pc)
        pc+:=2
        clocks(10)

    when 0x32 then      ! ld (nn), a
        ram[read16(pc)]:=regs.a
        pc+:=2
        clocks(13)

    when 0x33 then      ! inc sp
        ++regs.sp
        clocks(6)


    when 0x34 then      ! inc (hl)
        ram[regs.hl]:=inc8(ram[regs.hl])
        clocks(11)

    when 0x35 then      ! dec (hl)
        ram[regs.hl]:=dec8(ram[regs.hl])
        clocks(11)

    when 0x36 then      ! ld (hl), n
        ram[regs.hl]:=ram[pc]
        ++pc
        clocks(10)

    when 0x37 then      ! scf
        regs.cf:=1
        clocks(4)

    when 0x38 then      ! jr cy, n
        if regs.cf then
            pc+:=iwiden(ram[pc])+1
            clocks(7)
        else
            ++pc
            clocks(2)
        end

    when 0x39 then      ! add hl, sp
        regs.hl := add16(regs.hl, regs.sp)
        clocks(11)

    when 0x3A then      ! ld a, (nn)
        regs.a:=ram[read16(pc)]
        pc+:=2
        clocks(13)

    when 0x3B then      ! dec sp
        --regs.sp
        clocks(6)

    when 0x3C then      ! inc a
        regs.a:=inc8(regs.a)
        clocks(4)

    when 0x3D then      ! dec a
        regs.a:=dec8(regs.a)
        clocks(4)

    when 0x3E then      ! ld a, n
        regs.a:=ram[pc]
        ++pc
        clocks(7)

    when 0x3F then      ! ccf
        regs.cf:=regs.cf ixor 1
        clocks(4)

    when 0x40 then      ! ld b, b
        regs.b := regs.b
        clocks(4)

    when 0x41 then      ! ld b, c
        regs.b := regs.c
        clocks(4)

    when 0x42 then      ! ld b, d
        regs.b := regs.d
        clocks(4)

    when 0x43 then      ! ld b, e
        regs.b := regs.e
        clocks(4)

    when 0x44 then      ! ld b, h
        regs.b := regs.h
        clocks(4)

    when 0x45 then      ! ld b, l
        regs.b := regs.l
        clocks(4)

    when 0x46 then      ! ld b, (hl)
        regs.b := ram[regs.hl]
        clocks(7)

    when 0x47 then      ! ld b, a
        regs.b := regs.a
        clocks(4)

    when 0x48 then      ! ld c, b
        regs.c := regs.b
        clocks(4)

    when 0x49 then      ! ld c, c
        regs.c := regs.c
        clocks(4)

    when 0x4A then      ! ld c, d
        regs.c := regs.d
        clocks(4)

    when 0x4B then      ! ld c, e
        regs.c := regs.e
        clocks(4)

    when 0x4C then      ! ld c, h
        regs.c := regs.h
        clocks(4)

    when 0x4D then      ! ld c, l
        regs.c := regs.l
        clocks(4)

    when 0x4E then      ! ld c, (hl)
        regs.c := ram[regs.hl]
        clocks(7)

    when 0x4F then      ! ld c, a
        regs.c := regs.a
        clocks(4)

    when 0x50 then      ! ld d, b
        regs.c := regs.b
        clocks(4)

    when 0x51 then      ! ld d, c
        regs.c := regs.c
        clocks(4)

    when 0x52 then      ! ld d, d
        regs.d := regs.d
        clocks(4)

    when 0x53 then      ! ld d, e
        regs.d := regs.e
        clocks(4)

    when 0x54 then      ! ld d, h
        regs.d := regs.h
        clocks(4)

    when 0x55 then      ! ld d, l
        regs.d := regs.l
        clocks(4)

    when 0x56 then      ! ld d, (hl)
        regs.d := ram[regs.hl]
        clocks(7)

    when 0x57 then      ! ld d, a
        regs.d := regs.a
        clocks(4)

    when 0x58 then      ! ld e, b
        regs.e := regs.b
        clocks(4)

    when 0x59 then      ! ld e, c
        regs.e := regs.c
        clocks(4)

    when 0x5A then      ! ld e, d
        regs.e := regs.d
        clocks(4)

    when 0x5B then      ! ld e, e
        regs.e := regs.e
        clocks(4)

    when 0x5C then      ! ld e, h
        regs.e := regs.h
        clocks(4)

    when 0x5D then      ! ld e, l
        regs.e := regs.l
        clocks(4)

    when 0x5E then      ! ld e, (hl)
        regs.e := ram[regs.hl]
        clocks(7)

    when 0x5F then      ! ld e, a
        regs.e := regs.a
        clocks(4)

    when 0x60 then      ! ld h, b
        regs.h := regs.b
        clocks(4)

    when 0x61 then      ! ld h, c
        regs.h := regs.c
        clocks(4)

    when 0x62 then      ! ld h, d
        regs.h := regs.d
        clocks(4)

    when 0x63 then      ! ld h, e
        regs.h := regs.e
        clocks(4)

    when 0x64 then      ! ld h, h
        regs.h := regs.h
        clocks(4)

    when 0x65 then      ! ld h, l
        regs.h := regs.l
        clocks(4)

    when 0x66 then      ! ld h, (hl)
        regs.h := ram[regs.hl]
        clocks(7)

    when 0x67 then      ! ld h, a
        regs.h := regs.a
        clocks(4)

    when 0x68 then      ! ld l, b
        regs.l := regs.b
        clocks(4)

    when 0x69 then      ! ld l, c
        regs.l := regs.c
        clocks(4)

    when 0x6A then      ! ld l, d
        regs.l := regs.d
        clocks(4)

    when 0x6B then      ! ld l, e
        regs.l := regs.e
        clocks(4)

    when 0x6C then      ! ld l, h
        regs.l := regs.h
        clocks(4)

    when 0x6D then      ! ld l, l
        regs.l := regs.l
        clocks(4)

    when 0x6E then      ! ld l, (hl)
        regs.l := ram[regs.hl]
        clocks(7)

    when 0x6F then      ! ld l, a
        regs.l := regs.a
        clocks(4)

    when 0x70 then      ! ld (hl), b
        ram[regs.hl]:=regs.b
        clocks(7)

    when 0x71 then      ! ld (hl), c
        ram[regs.hl]:=regs.c
        clocks(7)

    when 0x72 then      ! ld (hl), d
        ram[regs.hl]:=regs.d
        clocks(7)

    when 0x73 then      ! ld (hl), e
        ram[regs.hl]:=regs.e
        clocks(7)

    when 0x74 then      ! ld (hl), h
        ram[regs.hl]:=regs.h
        clocks(7)

    when 0x75 then      ! ld (hl), l
        ram[regs.hl]:=regs.l
        clocks(7)

    when 0x76 then      ! halt
        clocks(4)
        exit

    when 0x77 then      ! ld (hl), a
        ram[regs.hl]:=regs.a
        clocks(7)

    when 0x78 then      ! ld a, b
        regs.a := regs.b
        clocks(4)

    when 0x79 then      ! ld a, c
        regs.a := regs.c
        clocks(4)

    when 0x7A then      ! ld a, d
        regs.a := regs.d
        clocks(4)

    when 0x7B then      ! ld a, e
        regs.a := regs.e
        clocks(4)

    when 0x7C then      ! ld a, h
        regs.a := regs.h
        clocks(4)

    when 0x7D then      ! ld a, l
        regs.a := regs.l
        clocks(4)

    when 0x7E then      ! ld a, (hl)
        regs.a := ram[regs.hl]
        clocks(7)

    when 0x7F then      ! ld a, a
        regs.a := regs.a
        clocks(4)

    when 0x80 then      ! add a, b
        regs.a:=add8(regs.a, regs.b, 0)
        clocks(4)

    when 0x81 then      ! add a, c
        regs.a:=add8(regs.a, regs.c, 0)
        clocks(4)


    when 0x82 then      ! add a, d
        regs.a:=add8(regs.a, regs.d, 0)
        clocks(4)


    when 0x83 then      ! add a, e
        regs.a:=add8(regs.a, regs.e, 0)
        clocks(4)

    when 0x84 then      ! add a, h
        regs.a:=add8(regs.a, regs.h, 0)
        clocks(4)

    when 0x85 then      ! add a, l
        regs.a:=add8(regs.a, regs.l, 0)
        clocks(4)

    when 0x86 then      ! add a, (hl)
        regs.a:=add8(regs.a, ram[regs.hl], 0)
        clocks(7)

    when 0x87 then      ! add a, a
        regs.a:=add8(regs.a, regs.a, 0)
        clocks(4)

    when 0x88 then      ! adc a, b
        regs.a:=add8(regs.a, regs.b, regs.cf)
        clocks(4)

    when 0x89 then      ! adc a, c
        regs.a:=add8(regs.a, regs.c, regs.cf)
        clocks(4)

    when 0x8A then      ! adc a, d
        regs.a:=add8(regs.a, regs.d, regs.cf)
        clocks(4)

    when 0x8B then      ! adc a, e
        regs.a:=add8(regs.a, regs.e, regs.cf)
        clocks(4)

    when 0x8C then      ! adc a, h
        regs.a:=add8(regs.a, regs.h, regs.cf)
        clocks(4)

    when 0x8D then      ! adc a, l
        regs.a:=add8(regs.a, regs.l, regs.cf)
        clocks(4)

    when 0x8E then      ! adc a, (hl)
        regs.a:=add8(regs.a, ram[regs.hl], regs.cf)
        clocks(7)

    when 0x8F then      ! adc a, a
        regs.a:=add8(regs.a, regs.h, regs.cf)
        clocks(4)

    when 0x90 then      ! sub a, b
        regs.a:=sub8(regs.a, regs.b, 0)
        clocks(4)

    when 0x91 then      ! sub a, c
        regs.a:=sub8(regs.a, regs.c, 0)
        clocks(4)

    when 0x92 then      ! sub a, d
        regs.a:=sub8(regs.a, regs.d, 0)
        clocks(4)

    when 0x93 then      ! sub a, e
        regs.a:=sub8(regs.a, regs.e, 0)
        clocks(4)

    when 0x94 then      ! sub a, h
        regs.a:=sub8(regs.a, regs.h, 0)
        clocks(4)

    when 0x95 then      ! sub a, l
        regs.a:=sub8(regs.a, regs.l, 0)
        clocks(4)

    when 0x96 then      ! sub a, (hl)
        regs.a:=sub8(regs.a, ram[regs.hl], 0)
        clocks(7)

    when 0x97 then      ! sub a, a
        regs.a:=sub8(regs.a, regs.a, 0)
        clocks(4)

    when 0x98 then      ! sbc a, b
        regs.a:=sub8(regs.a, regs.b, regs.cf)
        clocks(4)

    when 0x99 then      ! sbc a, c
        regs.a:=sub8(regs.a, regs.c, regs.cf)
        clocks(4)

    when 0x9A then      ! sbc a, d
        regs.a:=sub8(regs.a, regs.d, regs.cf)
        clocks(4)

    when 0x9B then      ! sbc a, e
        regs.a:=sub8(regs.a, regs.e, regs.cf)
        clocks(4)

    when 0x9C then      ! sbc a, h
        regs.a:=sub8(regs.a, regs.h, regs.cf)
        clocks(4)

    when 0x9D then      ! sbc a, l
        regs.a:=sub8(regs.a, regs.l, regs.cf)
        clocks(4)

    when 0x9E then      ! sbc a, (hl)
        regs.a:=sub8(regs.a, ram[regs.hl], regs.cf)
        clocks(7)

    when 0x9F then      ! sbc a, a
        regs.a:=sub8(regs.a, regs.a, regs.cf)
        clocks(4)

    when 0xA0 then      ! and a, b
        regs.a := and8(regs.a, regs.b)
        clocks(4)

    when 0xA1 then      ! and a, c
        regs.a := and8(regs.a, regs.c)
        clocks(4)

    when 0xA2 then      ! and a, d
        regs.a := and8(regs.a, regs.d)
        clocks(4)

    when 0xA3 then      ! and a, e
        regs.a := and8(regs.a, regs.e)
        clocks(4)

    when 0xA4 then      ! and a, h
        regs.a := and8(regs.a, regs.h)
        clocks(4)

    when 0xA5 then      ! and a, l
        regs.a := and8(regs.a, regs.l)
        clocks(4)

    when 0xA6 then      ! and a, (hl)
        regs.a:=and8(regs.a, ram[regs.hl])
        clocks(7)

    when 0xA7 then      ! and a, a
        regs.a := and8(regs.a, regs.a)
        clocks(4)

    when 0xA8 then      ! xor a, b
        regs.a := xor8(regs.a, regs.b)
        clocks(4)

    when 0xA9 then      ! xor a, c
        regs.a := xor8(regs.a, regs.c)
        clocks(4)

    when 0xAA then      ! xor a, d
        regs.a := xor8(regs.a, regs.d)
        clocks(4)

    when 0xAB then      ! xor a, e
        regs.a := xor8(regs.a, regs.e)
        clocks(4)

    when 0xAC then      ! xor a, h
        regs.a := xor8(regs.a, regs.h)
        clocks(4)

    when 0xAD then      ! xor a, l
        regs.a := xor8(regs.a, regs.l)
        clocks(4)

    when 0xAE then      ! xor a, (hl)
        regs.a:=xor8(regs.a, ram[regs.hl])
        clocks(7)

    when 0xAF then      ! xor a, a
        regs.a := xor8(regs.a, regs.a)
        clocks(4)

    when 0xB0 then      ! or a, b
        regs.a := or8(regs.a, regs.b)
        clocks(4)

    when 0xB1 then      ! or a, c
        regs.a := or8(regs.a, regs.c)
        clocks(4)

    when 0xB2 then      ! or a, d
        regs.a := or8(regs.a, regs.d)
        clocks(4)

    when 0xB3 then      ! or a, e
        regs.a := or8(regs.a, regs.e)
        clocks(4)

    when 0xB4 then      ! or a, h
        regs.a := or8(regs.a, regs.h)
        clocks(4)

    when 0xB5 then      ! or a, l
        regs.a := or8(regs.a, regs.l)
        clocks(4)

    when 0xB6 then      ! or a, (hl)
        regs.a:=or8(regs.a, ram[regs.hl])
        clocks(7)

    when 0xB7 then      ! or a, a
        regs.a := or8(regs.a, regs.a)
        clocks(4)

    when 0xB8 then      ! cmp a, b
        cmp8(regs.a, regs.b)
        clocks(4)

    when 0xB9 then      ! cmp a, c
        cmp8(regs.a, regs.c)
        clocks(4)

    when 0xBA then      ! cmp a, d
        cmp8(regs.a, regs.d)
        clocks(4)

    when 0xBB then      ! cmp a, e
        cmp8(regs.a, regs.e)
        clocks(4)

    when 0xBC then      ! cmp a, h
        cmp8(regs.a, regs.h)
        clocks(4)

    when 0xBD then      ! cmp a, l
        cmp8(regs.a, regs.l)
        clocks(4)

    when 0xBE then      ! cmp a, (hl)
        cmp8(regs.a, ram[regs.hl])
        clocks(7)

    when 0xBF then      ! cmp a, a
        cmp8(regs.a, regs.a)
        clocks(4)

    when 0xC0 then      ! ret nz
        if not regs.zf then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xC1 then      ! pop bc
        regs.bc := read16(regs.sp)
        regs.sp+:=2
        clocks(10)

    when 0xC2 then      ! jp nz, nn
        if not regs.zf then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xC3 then      ! jp nn
        pc:=read16(pc)
        clocks(10)

    when 0xC4 then      ! call nz, nn
        if not regs.zf then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xC5 then      ! push bc
        regs.sp-:=2
        write16(regs.sp, regs.bc)
        clocks(11)

    when 0xC6 then      ! add a, n
        regs.a := add8(regs.a, ram[pc], 0)
        ++pc
        clocks(7)

    when 0xC7 then      ! rst 00: use to call host fn; op code is in A
        dosyscall(ram, regs.a)
        clocks(11)

    when 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF then  !rst 00 to 38
        regs.sp-:=2
        write16(regs.sp, pc+1)
        pc:=ram[pc] iand 00111000b
        clocks(11)

    when 0xC8 then      ! ret z
        if regs.zf then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xC9 then      ! ret
        pc:=read16(regs.sp)
        regs.sp+:=2
        clocks(10)

    when 0xCA then      ! jp z, nn
        if regs.zf then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xCB then      ! bits
        dobits(ram[pc])
        ++pc

    when 0xCC then      ! call z, nn
        if regs.zf then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xCD then      ! call nn
        regs.sp-:=2
        write16(regs.sp, pc+2)
        pc:=read16(pc)
        clocks(17)

    when 0xCE then      ! adc a, n
        regs.a := add8(regs.a, ram[pc], regs.cf)
        ++pc
        clocks(7)

    when 0xD0 then      ! ret nc
        if not regs.cf then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xD1 then      ! pop de
        regs.de := read16(regs.sp)
        regs.sp+:=2
        clocks(10)

    when 0xD2 then      ! jp nc, nn
        if not regs.cf then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xD3 then      ! out (n), a
        outport(ram[pc], regs.a)
        ++pc
        clocks(11)

    when 0xD4 then      ! call nc, nn
        if not regs.cf then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xD5 then      ! push de
        regs.sp-:=2
        write16(regs.sp, regs.de)
        clocks(11)

    when 0xD6 then      ! sub a, n
        regs.a := sub8(regs.a, ram[pc], 0)
        ++pc
        clocks(7)

    when 0xD8 then      ! ret c
        if regs.cf then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xD9 then      ! exx
        swap(regs.af, altregs.af)
        swap(regs.bc, altregs.bc)
        swap(regs.de, altregs.de)
        swap(regs.hl, altregs.hl)
        clocks(4)

    when 0xDA then      ! jp cy, nn
        if regs.cf then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xDB then      ! in a, (n)
        regs.a:=inport(ram[pc])
        ++pc
        clocks(11)

    when 0xDC then      ! call cy, nn
        if regs.cf then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xDD then      ! doix
        doixiy(regs.ix)

    when 0xDE then      ! sbc a, n
        regs.a := sub8(regs.a, ram[pc], regs.cf)
        ++pc
        clocks(7)

    when 0xE0 then      ! ret po
        if regs.pvf=0 then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xE1 then      ! pop hl
        regs.hl := read16(regs.sp)
        regs.sp+:=2
        clocks(10)

    when 0xE2 then      ! jp po, nn
        if regs.pvf=0 then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xE3 then      ! ex [sp], hl
        t:=read16(regs.sp)
        write16(regs.sp, regs.hl)
        regs.hl:=t
        clocks(19)

    when 0xE4 then      ! call po, nn
        if regs.pvf=0 then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xE5 then      ! push hl
        regs.sp-:=2
        write16(regs.sp, regs.hl)
        clocks(11)

    when 0xE6 then      ! and a, n
        regs.a := and8(regs.a, ram[pc])
        ++pc
        clocks(7)

    when 0xE8 then      ! ret pe
        if regs.pvf=1 then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xE9 then      ! jp (hl)
        pc:=regs.hl
        clocks(4)

    when 0xEA then      ! jp pe, nn
        if regs.pvf then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xEB then      ! ex de, hl
        swap(regs.de, regs.hl)
        clocks(4)

    when 0xEC then      ! call pe, nn
        if regs.pvf=1 then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xED then      ! misc
        domisc()
!       clocks(t)

    when 0xEE then      ! xor a, n
        regs.a := xor8(regs.a, ram[pc])
        ++pc
        clocks(7)

    when 0xF0 then      ! ret p
        if regs.sf=0 then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xF1 then      ! pop af
        regs.af := read16(regs.sp)
        regs.sp+:=2
        clocks(10)

    when 0xF2 then      ! jp p, nn
        if regs.sf=0 then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xF3 then      ! di
        regs.iff:=0
        clocks(4)

    when 0xF4 then      ! call p, nn
        if regs.sf=0 then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xF5 then      ! push af
        regs.sp-:=2
        write16(regs.sp, regs.af)
        clocks(11)

    when 0xF6 then      ! or a, n
        regs.a := or8(regs.a, ram[pc])
        ++pc
        clocks(7)

    when 0xF8 then      ! ret m
        if regs.sf then
            pc:=read16(regs.sp)
            regs.sp+:=2
            clocks(11)
        else
            clocks(5)
        end

    when 0xF9 then      ! ld sp, hl
        regs.sp:=regs.hl
        clocks(6)

    when 0xFA then      ! jp m, nn
        if regs.sf then
            pc:=read16(pc)
            clocks(10)
        else
            pc+:=2
            clocks(3)
        end

    when 0xFB then      ! ei
        regs.iff:=1
        clocks(4)

    when 0xFC then      ! call m, nn
        if regs.sf then
            regs.sp-:=2
            write16(regs.sp, pc+2)
            pc:=read16(pc)
            clocks(17)
        else
            pc+:=2
            clocks(10)
        end

    when 0xFD then      ! doiy
        doixiy(regs.iy)
!       clocks(t)

    when 0xFE then      ! cmp a, n
        cmp8(regs.a, ram[pc])
        ++pc
        clocks(7)

    else
unimpl:
        badop("-", pc, ram[pc-1])
    end

    clockcount:=ticks
    instrcount:=icount
    pcptr:=pc
    return 0
end

proc domisc =
!pc points to byte following ED
!return pc pointing to next instr, and number of clock ticks used
    int x

    case ram[pc++]
    when 0x4A then          !adc hl, bc
        regs.hl := add16c(regs.hl, regs.bc, regs.cf)
        ticks+:=15

    when 0x5A then          !adc hl, de
        regs.hl := add16c(regs.hl, regs.de, regs.cf)
        ticks+:=15

    when 0x6A then          !adc hl, hl
        regs.hl := add16c(regs.hl, regs.hl, regs.cf)
        ticks+:=15

    when 0x7A then          !adc hl, sp
        regs.hl := add16c(regs.hl, regs.sp, regs.cf)
        ticks+:=15

    when 0x42 then          !sbc hl, bc
        regs.hl := sub16c(regs.hl, regs.bc, regs.cf)
        ticks+:=15

    when 0x52 then          !sbc hl, de
        regs.hl := sub16c(regs.hl, regs.de, regs.cf)
        ticks+:=15

    when 0x62 then          !sbc hl, hl
        regs.hl := sub16c(regs.hl, regs.hl, regs.cf)
        ticks+:=15

    when 0x72 then          !sbc hl, sp
        regs.hl := sub16c(regs.hl, regs.sp, regs.cf)
        ticks+:=15

    when 0x43 then          !ld (nn), bc
        write16(read16(pc), regs.bc)
        pc+:=2
        ticks+:=20

    when 0x53 then          !ld (nn), de
        write16(read16(pc), regs.de)
        pc+:=2
        ticks+:=20

    when 0x63 then          !ld (nn), hl
        write16(read16(pc), regs.hl)
        pc+:=2
        ticks+:=20

    when 0x73 then          !ld (nn), sp
        write16(read16(pc), regs.sp)
        pc+:=2
        ticks+:=20

    when 0x4B then          !ld bc, (nn)
        regs.bc:=read16(read16(pc))
        pc+:=2
        ticks+:=20

    when 0x5B then          !ld de, (nn)
        regs.de:=read16(read16(pc))
        pc+:=2
        ticks+:=20

    when 0x6B then          !ld hl, (nn)
        regs.hl:=read16(read16(pc))
        pc+:=2
        ticks+:=20

    when 0x7B then          !ld sp, (nn)
        regs.sp:=read16(read16(pc))
        pc+:=2
        ticks+:=20

    when 0xB0 then          !ldir
        repeat
            x:=ram[regs.hl]
            ram[regs.de]:=x
            ++regs.hl
            ++regs.de

            ticks+:=21
            --regs.bc
        until regs.bc=0

    else
        badop("ED", pc, ram[pc-1])
    end
end

proc dobits(int opcode) =
!opcode is the byte following CB. Pc-handling taken care of in host
!this function returns ticks, either 8 or (for '(hl)') 15
    int rotop, regcode
    u16 value

    if opcode>=0x40 then
error:
        badop("CB", pc, opcode)
        stop 1
    end

    rotop:=opcode.[5..3]            !code 0 to 7

    regcode:=opcode iand 7

    if regcode<>6 then  
        value:=regs.abc[regmap[regcode]]
        ticks+:=8
    else
        value:=ram[regs.hl]
        ticks+:=15
    end

    case rotop
    when m_rol then value:=rol8(value)
    when m_ror then value:=ror8(value)
    when m_rcl then value:=rol9(value)
    when m_rcr then value:=ror9(value)

    when m_shl then
        regs.cf:=value.[7]
        value<<:=1

    when m_sar then
        regs.cf:=value.[0]
        value:=iwiden(value)>>1

    when m_shr then
        regs.cf:=value.[0]
        value>>:=1
    else
        error
    end

    if regcode<>6 then  
        regs.abc[regmap[regcode]]:=value
    else
        ram[regs.hl]:=value
    end

    regs.sf:=value.[7]
    regs.zf:=u8(value)=0
    regs.pvf:=parity(u8(value))
end

proc doixiy(u16 &ireg) =
    int ticks
    ref byte p

    switch ram[pc++]
    when 0x09 then          !add ix, bc
        ireg := add16(ireg, regs.bc)
        ticks+:=15

    when 0x19 then          !add ix, de
        ireg := add16(ireg, regs.de)
        ticks+:=15

    when 0x21 then          !ld ix, nn
        ireg := read16(pc)
        pc+:=2
        ticks+:=14

    when 0x22 then          !ld (nn), ix
        ram[read16(pc)] := ireg
        pc+:=2
        ticks+:=20

    when 0x23 then          !inc ix
        ++ireg
        ticks+:=10

    when 0x29 then          !add ix, ix
        ireg := add16(ireg, ireg)
        ticks+:=15

    when 0x2A then          !ld ix, (nn)
        ireg:=read16(read16(pc))
        pc+:=2
        ticks+:=20

    when 0x2B then          !dec ix
        --ireg
        ticks+:=10

    when 0x24 then          !inc (ix+d)
        p:=&ram[ireg+iwiden(ram[pc])]
        p^ := inc8(p^)
        ticks+:=23

    when 0x25 then          !dec (ix+d)
        p:=&ram[ireg+iwiden(ram[pc++])]
        p^ := dec8(p^)
        ticks+:=23

    when 0x36 then          !ld (ix+d), n
        p:=&ram[ireg+iwiden(ram[pc++])]
        p^ := ram[pc++]
        ticks+:=19

    when 0x39 then          !add ix, sp
        ireg := add16(ireg, regs.sp)
        ticks+:=15

    when 0x46 then          !ld b, (ix+d)
        regs.b := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x4E then          !ld c, (ix+d)
        regs.c := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x54 then          !ld d, iyh
        regs.d:=ireg>>8


    when 0x56 then          !ld d, (ix+d)
        regs.d := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x5D then          !ld e, iyl
        regs.e:=ireg

    when 0x5E then          !ld e, (ix+d)
        regs.e := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x66 then          !ld h, (ix+d)
        regs.h := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x69 then          !ld iyl, c
        ireg.[7..0]:=regs.c
        ticks+:=8

    when 0x6E then          !ld l, (ix+d)
        regs.l := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x6F then          !ld iyl, a
        ireg.[7..0]:=regs.a
        ticks+:=8

    when 0x70 then          !ld (ix+d), b
        ram[ireg+iwiden(ram[pc++])] := regs.b
        ticks+:=19

    when 0x71 then          !ld (ix+d), c
        ram[ireg+iwiden(ram[pc++])] := regs.c
        ticks+:=19

    when 0x72 then          !ld (ix+d), d
        ram[ireg+iwiden(ram[pc++])] := regs.d
        ticks+:=19

    when 0x73 then          !ld (ix+d), e
        ram[ireg+iwiden(ram[pc++])] := regs.e
        ticks+:=19

    when 0x74 then          !ld (ix+d), h
        ram[ireg+iwiden(ram[pc++])] := regs.h
        ticks+:=19

    when 0x75 then          !ld (ix+d), l
        ram[ireg+iwiden(ram[pc++])] := regs.l
        ticks+:=19

    when 0x77 then          !ld (ix+d), a
        ram[ireg+iwiden(ram[pc++])] := regs.a
        ticks+:=19

    when 0x7A then          !ld a, (ix+d)
        regs.a := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19

    when 0x7D then          !ld a, iyl
        regs.a:=ireg
        ticks+:=8

    when 0x7E then          !ld a, (ix+d)
        regs.a := ram[ireg+iwiden(ram[pc++])]
        ticks+:=19


    when 0x86 then          !add a, (ix+d)
        regs.a := add8(regs.a, ram[ireg+iwiden(ram[pc++])], 0)
        ticks+:=19

    when 0x8E then          !adc a, (ix+d)
        regs.a := add8(regs.a, ram[ireg+iwiden(ram[pc++])], regs.cf)
        ticks+:=19

    when 0x96 then          !sub a, (ix+d)
        regs.a := sub8(regs.a, ram[ireg+iwiden(ram[pc++])], 0)
        ticks+:=19

    when 0x9E then          !sbc a, (ix+d)
        regs.a := sub8(regs.a, ram[ireg+iwiden(ram[pc++])], regs.cf)
        ticks+:=19

    when 0xA6 then          !and a, (ix+d)
        regs.a := and8(regs.a, ram[ireg+iwiden(ram[pc++])])
        ticks+:=19

    when 0xAE then          !xor a, (ix+d)
        regs.a := xor8(regs.a, ram[ireg+iwiden(ram[pc++])])
        ticks+:=19

    when 0xB6 then          !or a, (ix+d)
        regs.a := or8(regs.a, ram[ireg+iwiden(ram[pc++])])
        ticks+:=19

    when 0xBE then          !cp a, (ix+d)
        sub8(regs.a, ram[ireg+iwiden(ram[pc++])], 0)
        ticks+:=19

    when 0xE1 then          !pop ix
        ireg := read16(regs.sp)
        regs.sp+:=2
        ticks+:=14

    when 0xE5 then          !push ix
        regs.sp-:=2

        write16(regs.sp, ireg)
        ticks+:=15

    when 0xE9 then          !jp (ix)
        pc:=ireg
        clocks(8)

    when 0xF9 then          !ld sp, ix
        regs.sp:=ireg
        ticks+:=10

    else
unimpl:
        badop("DD/FD", pc, ram[pc-1])
        stop 1
    end
end

proc badop(ichar caption, int pc, opc)=
    println "Unimpl op (",,caption,,") at",pc, opc
    println
    stop 1
end

global proc runprogram(tmem ram)=
    byte exitcode
    real tpc, t4, t25
    int tt

    clear regs
    regs.sp:=ramsize            !start at top end of memory
    clear altregs
    clockcount:=0

    pcptr := 0
    exitcode:=execloop()

PRINTLN "FINISHED RUN"

    if fquiet then
        return
    end

end

global func add8(u16 lhs, rhs, carry)u16 =
    u8 locals
    u16 result

    result:=lhs+rhs+carry

!   locals:=result > 255                            !carry
!   locals ior:=(lhs ixor rhs ixor result) iand HF  !half

!   if iwiden(lhs)+iwiden(rhs)+carry not in i8.bounds then
!       locals ior:=PF                              !overflow
!   end

    result:=u8(result)

    regs.flags:=getsign8(result) ior locals ior getzero(result)

    return u8(result)
end

global func sub8(u16 lhs, rhs, carry)u16 =
    u8 locals
    u16 result

    result:=lhs-rhs-carry

!   locals:=lhs < (rhs+carry)                       !carry
!   locals ior:=(lhs ixor rhs ixor result) iand HF  !half

!   if iwiden(lhs)-iwiden(rhs)-carry not in i8.bounds then
!       locals ior:=PF                              !overflow
!   end

    result:=u8(result)

    regs.flags:=getsign8(result) ior locals ior getxy ior getzero(result) ior NF

    return u8(result)
end


global func and8(u16 lhs, rhs)u16 =
    u8 result

    result:=lhs iand rhs

    regs.flags:=parity(result) ior getsign8(result) ior getzero(result) ior HF

    return result
end

global func or8(u16 lhs, rhs)u16 =
    u8 result

    result:=lhs ior rhs

    regs.flags:=parity(result) ior getsign8(result) ior getzero(result)

    return result
end

global func xor8(u16 lhs, rhs)u16 =
    u8 result

    result:=lhs ixor rhs

    regs.flags:=parity(result) ior getsign8(result) ior getzero(result)

    return result
end

global func parity(u16 value)u16 =
0
!   (((0x9669 >> ((value ixor (value >> 4)) iand 0x0F)) iand 1) << 2)
end

global func inc8(u16 value)u16 =
    byte oldcy := regs.cf
    u8 result

    result:=add8(value, 1, 0)
    regs.cf:=oldcy
    return result
end

global func dec8(u16 value)u16 =
    byte oldcy := regs.cf
    u8 result

    result:=sub8(value, 1, 0)
    regs.cf:=oldcy
    return result
end

global func compl(u16 value)u16 =
    regs.flags ior := NF ior HF
    return u8(inot value)
end

!global func add16(u16 lhs, rhs)u16=
!   u16 result
!   u8 oldflags:=regs.flags iand (ZF ior PF ior SF)     !these must be unchanged
!
!   result:=add16c(lhs, rhs, 0)
!
!   return u16(result)
!end

global func add16(u16 lhs, rhs)u16=
    u8 locals
    u16 result
    u8 oldflags:=regs.flags iand (ZF ior PF ior SF)     !these must be unchanged

    result:=lhs+rhs
    locals:=result > u16.max                            !carry

    result:=u16(result)

    regs.flags:=locals ior getzero(result) ior oldflags

    return u16(result)
end

global func add16c(u16 lhs, rhs, carry)u16=
    u8 locals
    u16 result

    result:=lhs+rhs+carry
    locals:=result > u16.max                            !carry

    if i16(lhs)+i16(rhs)+carry not in i16.bounds then
        locals ior:=PF                              !overflow
    end
    
    if result.[15] then                             !sign set
        locals ior:= SF
    fi

    result:=u16(result)

    regs.flags:=locals ior getzero(result)

    return result
end

global func sub16c(u16 lhs, rhs, carry)u16=
    u8 locals
    u16 result

    result:=lhs-rhs-carry
!   locals:=result > u16.max                            !carry
    locals:=lhs < (rhs+carry)                       !carry

!   if i16(lhs)-i16(rhs)-carry not in i16.bounds then
!       locals ior:=PF                              !overflow
!   end
!   
    if result.[15] then                             !sign set
        locals ior:= SF
    fi
!
    result:=u16(result)
!
    regs.flags:=locals ior getzero(result) ior NF

    return result
end

global proc cmp8(u16 lhs, rhs) =
    sub8(lhs, rhs, 0)
end

global func inport(int port)int=
    77
end

global proc outport(int port, value)=
end

global func rol8(u16 value)u16=
    u16 topbit

    topbit:=value.[7]
    regs.cf:=topbit
    value := value<<1 ior topbit
    regs.flags iand:=rotmask

    return u8(value)
end

global func ror8(u16 value)u16=
    u16 lowbit

    lowbit:=value iand 1
    regs.cf:=lowbit
    value := value>>1 ior lowbit<<7
    regs.flags iand:=rotmask

    return u8(value)
end

global func rol9(u16 value)u16=
!these shift via carry
    u16 cf

    cf:=regs.cf
    regs.cf:=value.[7]
    value := value<<1 ior cf
    regs.flags iand:=rotmask

    return u8(value)
end

global func ror9(u16 value)u16=
    u16 cf

    cf:=regs.cf
    regs.cf:=value iand 1
    value:=value>>1 ior cf<<7
    regs.flags iand:=rotmask

    return u8(value)
end

global proc dosyscall(tmem ram, int sysop)=
    int hl:=regs.hl
    static byte needgap=0
    int heap, n

    case sysop
    when sf_print_str then
        if needgap then print " " end
        print ichar(&ram[hl])
        needgap:=1

    when sf_print_i16 then
        if needgap then print " " end
        print i16(hl):"4zh"
        needgap:=1

    when sf_print_u16 then
        if needgap then print " " end
        print u16(hl)
        needgap:=1

    when sf_print_hex, sf_print_ptr then
        if needgap then print " " end
        print u16(hl):"4zh"
        needgap:=1

    when sf_print_c8 then
        if needgap then print " " end
        print regs.l:"2zh"
        needgap:=1

    when sf_print_bin then
        if needgap then print " " end
        print regs.hl:"16zb"
        needgap:=1

    when sf_print_bin8 then
        if needgap then print " " end
        print regs.l:"8zb"
        needgap:=1

    when sf_print_bool then
        if needgap then print " " end
        print (regs.hl|"True"|"False")
        needgap:=1

    when sf_print_space then
        print " "
        needgap:=0

    when sf_print_newline then
        println

    when sf_print_end then
        needgap:=0

    when sf_print_nogap then
        needgap:=0

    when sf_allocheap then
        n:=regs.hl                  !bytes requested
        heap:=read16(3)
        regs.hl:=heap
        heap+:=n

        if int(regs.sp-heap)<256 then
            println "Heap overflow\n"
            stop 1
        end
        write16(3, heap)

    when sf_read_conline then
        readln

    else
        PRINTLN "CAN'T DO THIS:", SYSOP
    end
end

func iwiden(u8 a)i16=
!!treat a as i8 type
    if a>=128 then          !negative
        0xFF00 + a
    else
        a
    end
end
