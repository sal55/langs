/* Limited x64 Disassembler
   Supports subset of instructions only (eg. few SIMD/SSE/128-bit ops)
   Exports 'decodeinstr' function:

   char* decodeinstr(char** cptr, void* baseadde)

   cptr is a pointer to a char* that points to the code to be disassembled.
   It will be stepped in the caller to the start of the next instruction.

   decodeinstr() returns a pointer to a string of the disassembled text. It
   needs to be consumed before the next call.

   Caller needs to decide when end-of-code has been reached.


   Example driver program: */

//#include <stdio.h>
//
//extern char* decodeinstr(char** cptr, void* baseaddr);
//
//static void testfn(void) {
//    int a,b,c;
//    a = b + c;
//}
//
//int main(void) {
//    char* codeptr;
//    char* s;
//
//    codeptr = (char*)testfn;
//
//    for (int i=0; i<10; ++i) {
//        s = decodeinstr(&codeptr, NULL);
//        if (s == NULL) break;
//        puts(s);
//    }
//}


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#pragma pack(1)

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef unsigned char byte;

typedef void* var;


#if (UINTPTR_MAX<0xFFFFFFFFFFFFFFFF)
	#error "Need 64-bit target. Try -m64"
#endif

/* Forward Struct Declarations */

/* Struct Definitions */

/* PROCDECLS */
byte * decodeinstr(byte * * cptr,void * baseaddr);
static void disasm_decodetwobyteinstr(void);
static void disasm_decodeaddr(i64 w);
static i64 disasm_readbyte(void);
static i64 disasm_readsbyte(void);
static u64 disasm_readword16(void);
static i64 disasm_readint16(void);
static u64 disasm_readword32(void);
static i64 disasm_readint32(void);
static i64 disasm_readint64(void);
static i64 disasm_getreg(i64 regcode,i64 upper);
byte * disasm_strreg(i64 reg,i64 opsize);
static byte * disasm_strfreg(i64 freg);
static void disasm_printaddrmode(i64 xmm);
static void disasm_genstr(byte * s);
static void disasm_genintd(i64 a);
static void disasm_genhex(i64 a);
static i64 disasm_readimm(void);
static i64 disasm_readimm8(void);
static byte * disasm_strxmm(i64 reg);
static byte * disasm_strmmx(i64 reg);
static void disasm_decode8087(i64 ttt);
static void disasm_do87arith(byte * opcstr,i64 ttt,i64 freg);
static void disasm_do87mem(byte * opcstr,i64 mf);
static void disasm_getsil(i64 * reg);
static void disasm_getsilx(i64 * reg);

/* VARS */
static i64 disasm_nmodules;
static i64 disasm_xfchsmask_pd;
static byte *  disasm_opnames[8] = {(byte*)"add",(byte*)"or",(byte*)"adc",(byte*)"sbb",(byte*)"and",(byte*)"sub",(byte*)"xor",(byte*)"cmp"};
static byte *  disasm_condnames[16] = {
    (byte*)"o",
    (byte*)"no",
    (byte*)"b",
    (byte*)"ae",
    (byte*)"z",
    (byte*)"nz",
    (byte*)"be",
    (byte*)"a",
    (byte*)"s",
    (byte*)"ns",
    (byte*)"p",
    (byte*)"np",
    (byte*)"l",
    (byte*)"ge",
    (byte*)"le",
    (byte*)"g"
};
static byte *  disasm_addrmodenames[3] = {(byte*)"amreg",(byte*)"ammem",(byte*)"amrel"};
static i64 disasm_rex;
static i64 disasm_addrmode;
static i64 disasm_rmreg;
static i64 disasm_rmopc;
static i64 disasm_basereg;
static i64 disasm_indexreg;
static i64 disasm_scale;
static i64 disasm_opsize;
static i64 disasm_offset;
static i64 disasm_offsetsize;
static i64 disasm_sizeoverride;
static i64 disasm_addroverride;
static i64 disasm_f2override;
static i64 disasm_f3override;
static byte disasm_deststr[256];
static byte *  disasm_destptr;
static byte *  disasm_codeptr;


byte * decodeinstr(byte * * cptr,void * baseaddr) {
    i64 n;
    i64 w;
    i64 opc;
    i64 reg;
    i64 op;
    byte *  pstart;
    static byte str[256];
    byte str2[128];
    i64 av_1;
    i64 av_2;
    disasm_deststr[((i64)1)-1] = (u64)0u;
    pstart = (disasm_codeptr = (*cptr));
    disasm_rex = (i64)0;
    disasm_opsize = (i64)1;
    disasm_f2override = (disasm_f3override = (disasm_sizeoverride = (disasm_addroverride = (i64)0)));
    disasm_basereg = (disasm_indexreg = (disasm_offset = (i64)0));
    //retry:
L4 :;
;
    switch ((opc = (i64)((*disasm_codeptr++)))) {
    case 0:;
    case 1:;
    case 8:;
    case 9:;
    case 16:;
    case 17:;
    case 24:;
    case 25:;
    case 32:;
    case 33:;
    case 40:;
    case 41:;
    case 48:;
    case 49:;
    case 56:;
    case 57:;
    {
        op = (opc >> (i64)3);
        disasm_decodeaddr((opc & (i64)1));
        disasm_getsilx(&disasm_basereg);
        disasm_getsil(&disasm_rmreg);
        disasm_genstr(disasm_opnames[(op)]);
        disasm_printaddrmode((i64)0);
        disasm_genstr((byte*)", ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
    }break;
    case 2:;
    case 3:;
    case 10:;
    case 11:;
    case 18:;
    case 19:;
    case 26:;
    case 27:;
    case 34:;
    case 35:;
    case 42:;
    case 43:;
    case 50:;
    case 51:;
    case 58:;
    case 59:;
    {
        op = (opc >> (i64)3);
        disasm_decodeaddr((opc & (i64)1));
        disasm_genstr(disasm_opnames[(op)]);
        disasm_genstr((byte*)" ");
        disasm_getsil(&disasm_rmreg);
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 4:;
    case 5:;
    case 12:;
    case 13:;
    case 20:;
    case 21:;
    case 28:;
    case 29:;
    case 36:;
    case 37:;
    case 44:;
    case 45:;
    case 52:;
    case 53:;
    case 60:;
    case 61:;
    {
        disasm_genstr(disasm_opnames[((opc >> (i64)3))]);
        disasm_genstr((byte*)" ");
        if (!!((opc & (i64)1))) {
            disasm_opsize = (i64)4;
            if (!!(disasm_sizeoverride)) {
                disasm_opsize = (i64)2;
            };
            if (!!((disasm_rex & (i64)8))) {
                disasm_opsize = (i64)8;
            };
        };
        disasm_genstr(disasm_strreg((i64)1,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_genintd(disasm_readimm());
    }break;
    case 15:;
    {
        disasm_decodetwobyteinstr();
    }break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 72:;
    case 73:;
    case 74:;
    case 75:;
    case 76:;
    case 77:;
    case 78:;
    case 79:;
    {
        disasm_rex = opc;
        goto L4 ;
;
    }break;
    case 80:;
    case 81:;
    case 82:;
    case 83:;
    case 84:;
    case 85:;
    case 86:;
    case 87:;
    {
        reg = disasm_getreg((opc & (i64)7),(disasm_rex & (i64)1));
        disasm_genstr((byte*)"push ");
        disasm_genstr(disasm_strreg(reg,(i64)8));
    }break;
    case 88:;
    case 89:;
    case 90:;
    case 91:;
    case 92:;
    case 93:;
    case 94:;
    case 95:;
    {
        reg = disasm_getreg((opc & (i64)7),(disasm_rex & (i64)1));
        disasm_genstr((byte*)"pop ");
        disasm_genstr(disasm_strreg(reg,(i64)8));
    }break;
    case 99:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"movsxd ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_opsize = (i64)4;
        disasm_printaddrmode((i64)0);
    }break;
    case 102:;
    {
        disasm_sizeoverride = (i64)1;
        goto L4 ;
;
    }break;
    case 103:;
    {
        disasm_addroverride = (i64)1;
        goto L4 ;
;
    }break;
    case 104:;
    {
        disasm_genstr((byte*)"push ");
        disasm_genintd(disasm_readint32());
    }break;
    case 106:;
    {
        disasm_genstr((byte*)"push ");
        disasm_genintd(disasm_readsbyte());
    }break;
    case 105:;
    case 107:;
    {
        disasm_decodeaddr((i64)1);
        if ((disasm_basereg != disasm_rmreg)) {
            disasm_genstr((byte*)"imul3");
            disasm_genstr((byte*)" ");
            disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
            disasm_genstr((byte*)", ");
        } else {
            disasm_genstr((byte*)"imul2");
        };
        disasm_printaddrmode((i64)0);
        disasm_genstr((byte*)", ");
        disasm_opsize = (!!((opc & (i64)2))?(i64)1:disasm_opsize);
        disasm_genintd(disasm_readimm());
    }break;
    case 112:;
    case 113:;
    case 114:;
    case 115:;
    case 116:;
    case 117:;
    case 118:;
    case 119:;
    case 120:;
    case 121:;
    case 122:;
    case 123:;
    case 124:;
    case 125:;
    case 126:;
    case 127:;
    {
        disasm_genstr((byte*)"j");
        disasm_genstr(disasm_condnames[((opc & (i64)15))]);
        disasm_genstr((byte*)" ");
        disasm_genintd(disasm_readsbyte());
    }break;
    case 128:;
    case 129:;
    case 130:;
    case 131:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_genstr(disasm_opnames[(disasm_rmopc)]);
        disasm_getsilx(&disasm_basereg);
        disasm_printaddrmode((i64)0);
        disasm_genstr((byte*)", ");
        if ((opc != (i64)131)) {
            disasm_genintd(disasm_readimm());
        } else {
            disasm_genintd(disasm_readsbyte());
        };
    }break;
    case 132:;
    case 133:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_getsilx(&disasm_basereg);
        disasm_getsil(&disasm_rmreg);
        disasm_genstr((byte*)"test ");
        disasm_printaddrmode((i64)0);
        disasm_genstr((byte*)", ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
    }break;
    case 134:;
    case 135:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_genstr((byte*)"exch2 ");
        disasm_getsilx(&disasm_basereg);
        disasm_getsil(&disasm_rmreg);
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)",");
        disasm_printaddrmode((i64)0);
    }break;
    case 136:;
    case 137:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_genstr((byte*)"mov");
        disasm_getsilx(&disasm_basereg);
        disasm_getsil(&disasm_rmreg);
        disasm_printaddrmode((i64)0);
        disasm_genstr((byte*)", ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
    }break;
    case 138:;
    case 139:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_genstr((byte*)"mov ");
        disasm_getsilx(&disasm_basereg);
        disasm_getsil(&disasm_rmreg);
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 141:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"lea ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 143:;
    {
        disasm_decodeaddr((i64)1);
        disasm_opsize = (i64)1;
        disasm_genstr((byte*)"pop");
        disasm_printaddrmode((i64)0);
    }break;
    case 144:;
    {
        if (!!(disasm_rex)) {
            goto L5 ;
;
        };
        disasm_genstr((byte*)"nop");
    }break;
    case 145:;
    case 146:;
    case 147:;
    case 148:;
    case 149:;
    case 150:;
    case 151:;
    {
        //doexch:
L5 :;
;
        reg = ((opc & (i64)7) + (i64)1);
        if (!!((disasm_rex & (i64)1))) {
            reg += (i64)8;
        };
        disasm_opsize = (!!(disasm_sizeoverride)?(i64)2:(i64)4);
        if (!!((disasm_rex & (i64)8))) {
            disasm_opsize = (i64)8;
        };
        disasm_genstr((byte*)"xchg ");
        disasm_genstr(disasm_strreg((i64)1,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_genstr(disasm_strreg(reg,disasm_opsize));
    }break;
    case 152:;
    {
        if (!!(disasm_sizeoverride)) {
            disasm_genstr((byte*)"cbw");
        } else {
            disasm_genstr((byte*)"cbw???");
        };
    }break;
    case 153:;
    {
        if (!!(disasm_sizeoverride)) {
            disasm_genstr((byte*)"cwd");
        } else if (!!((disasm_rex & (i64)8))) {
            disasm_genstr((byte*)"cqo");
        } else {
            disasm_genstr((byte*)"cdq");
        };
    }break;
    case 155:;
    {
        disasm_genstr((byte*)"wait");
    }break;
    case 156:;
    {
        disasm_genstr((byte*)"pushf");
    }break;
    case 157:;
    {
        disasm_genstr((byte*)"popf");
    }break;
    case 158:;
    {
        disasm_genstr((byte*)"sahf");
    }break;
    case 159:;
    {
        disasm_genstr((byte*)"lahf");
    }break;
    case 164:;
    case 165:;
    case 166:;
    case 167:;
    case 170:;
    case 171:;
    case 172:;
    case 173:;
    case 174:;
    case 175:;
    {
        disasm_genstr((((opc >> (i64)1) & (i64)7)==1?(byte*)"?":(((opc >> (i64)1) & (i64)7)==2?(byte*)"movs":(((opc >> (i64)1) & (i64)7)==3?(byte*)"cmps":(((opc >> (i64)1) & (i64)7)==4?(byte*)"?":(((opc >> (i64)1) & (i64)7)==5?(byte*)"stos":(((opc >> (i64)1) & (i64)7)==6?(byte*)"lods":(((opc >> (i64)1) & (i64)7)==7?(byte*)"scas":(byte*)"?"))))))));
        if (((opc & (i64)1) == (i64)0)) {
            disasm_genstr((byte*)"b");
        } else {
            if (!!((disasm_rex & (i64)8))) {
                disasm_genstr((byte*)"q");
            } else if (!!(disasm_sizeoverride)) {
                disasm_genstr((byte*)"w");
            } else {
                disasm_genstr((byte*)"d");
            };
        };
    }break;
    case 168:;
    case 169:;
    {
        disasm_genstr((byte*)"test ");
        if (!!((opc & (i64)1))) {
            disasm_opsize = (!!(disasm_sizeoverride)?(i64)2:(i64)4);
            if (!!((disasm_rex & (i64)8))) {
                disasm_opsize = (i64)8;
            };
        };
        disasm_genstr(disasm_strreg((i64)1,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_genintd(disasm_readimm());
    }break;
    case 176:;
    case 177:;
    case 178:;
    case 179:;
    case 180:;
    case 181:;
    case 182:;
    case 183:;
    case 184:;
    case 185:;
    case 186:;
    case 187:;
    case 188:;
    case 189:;
    case 190:;
    case 191:;
    {
        reg = ((opc & (i64)7) + (i64)1);
        if (!!((disasm_rex & (i64)1))) {
            reg += (i64)8;
        };
        if (!!((opc & (i64)8))) {
            disasm_opsize = (!!(disasm_sizeoverride)?(i64)2:(i64)4);
            if (!!((disasm_rex & (i64)8))) {
                disasm_opsize = (i64)8;
            };
        };
        disasm_genstr((byte*)"mov ");
        disasm_getsil(&reg);
        disasm_genstr(disasm_strreg(reg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_genintd(disasm_readimm8());
    }break;
    case 192:;
    case 193:;
    case 208:;
    case 209:;
    case 210:;
    case 211:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_getsilx(&disasm_basereg);
        disasm_genstr(((disasm_rmopc + (i64)1)==1?(byte*)"rol":((disasm_rmopc + (i64)1)==2?(byte*)"ror":((disasm_rmopc + (i64)1)==3?(byte*)"rcl":((disasm_rmopc + (i64)1)==4?(byte*)"rcr":((disasm_rmopc + (i64)1)==5?(byte*)"shl":((disasm_rmopc + (i64)1)==6?(byte*)"shr":((disasm_rmopc + (i64)1)==7?(byte*)"?":((disasm_rmopc + (i64)1)==8?(byte*)"sar":(byte*)"?")))))))));
        disasm_printaddrmode((i64)0);
        if ((opc <= (i64)193)) {
            disasm_genstr((byte*)", ");
            disasm_genintd(disasm_readbyte());
        } else {
            disasm_genstr((!!((opc & (i64)2))?(byte*)", cl":(byte*)", 1"));
        };
    }break;
    case 194:;
    {
        disasm_genstr((byte*)"retn ");
        disasm_genintd((i64)(disasm_readword16()));
    }break;
    case 195:;
    {
        disasm_genstr((byte*)"ret");
    }break;
    case 198:;
    case 199:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_genstr((byte*)"mov");
        disasm_printaddrmode((i64)0);
        disasm_genstr((byte*)", ");
        disasm_genintd(disasm_readimm());
    }break;
    case 215:;
    {
        disasm_genstr((byte*)"xlat");
    }break;
    case 216:;
    case 217:;
    case 218:;
    case 219:;
    case 220:;
    case 221:;
    case 222:;
    case 223:;
    {
        disasm_decode8087((opc & (i64)7));
    }break;
    case 224:;
    {
        disasm_genstr((byte*)"loopnz ");
        disasm_genintd(disasm_readsbyte());
    }break;
    case 225:;
    {
        disasm_genstr((byte*)"loopz ");
        disasm_genintd(disasm_readsbyte());
    }break;
    case 226:;
    {
        disasm_genstr((byte*)"loop ");
        disasm_genintd(disasm_readsbyte());
    }break;
    case 227:;
    {
        if (!!(disasm_addroverride)) {
            disasm_genstr((byte*)"jecxz ");
        } else {
            disasm_genstr((byte*)"jrcxz ");
        };
        disasm_genintd(disasm_readsbyte());
    }break;
    case 232:;
    {
        disasm_genstr((byte*)"call ");
        disasm_genintd(disasm_readint32());
    }break;
    case 233:;
    {
        disasm_genstr((byte*)"[4] jmp ");
        disasm_genintd(disasm_readint32());
    }break;
    case 235:;
    {
        disasm_genstr((byte*)"jmp ");
        disasm_genintd(disasm_readsbyte());
    }break;
    case 242:;
    {
        if ((((i64)((u64)((*disasm_codeptr))) != (i64)15) && (((i64)((u64)((*disasm_codeptr))) < (i64)64) && ((i64)((u64)((*disasm_codeptr))) > (i64)79)))) {
            disasm_genstr((byte*)"repne");
        } else {
            disasm_f2override = (i64)1;
            goto L4 ;
;
        };
    }break;
    case 243:;
    {
        if ((((i64)((u64)((*disasm_codeptr))) != (i64)15) && (((i64)((u64)((*disasm_codeptr))) < (i64)64) && ((i64)((u64)((*disasm_codeptr))) > (i64)79)))) {
            disasm_genstr((byte*)"repe");
        } else {
            disasm_f3override = (i64)1;
            goto L4 ;
;
        };
    }break;
    case 244:;
    {
        return (byte *)(0);
    }break;
    case 246:;
    case 247:;
    {
        disasm_decodeaddr((opc & (i64)1));
        disasm_getsilx(&disasm_basereg);
        disasm_genstr(((disasm_rmopc + (i64)1)==1?(byte*)"test":((disasm_rmopc + (i64)1)==2?(byte*)"?":((disasm_rmopc + (i64)1)==3?(byte*)"not":((disasm_rmopc + (i64)1)==4?(byte*)"neg":((disasm_rmopc + (i64)1)==5?(byte*)"mul":((disasm_rmopc + (i64)1)==6?(byte*)"imul":((disasm_rmopc + (i64)1)==7?(byte*)"div":((disasm_rmopc + (i64)1)==8?(byte*)"idiv":(byte*)"?")))))))));
        disasm_printaddrmode((i64)0);
        if ((disasm_rmopc == (i64)0)) {
            if ((disasm_opsize == (i64)8)) {
                disasm_opsize = (i64)4;
            };
            disasm_genstr((byte*)", ");
            disasm_genintd(disasm_readimm());
        };
    }break;
    case 254:;
    {
        w = (i64)0;
        goto L6 ;
;
    }break;
    case 255:;
    {
        w = (i64)1;
        //doff:
L6 :;
;
        disasm_decodeaddr(w);
        if ((disasm_rmopc==(i64)0)) {
            disasm_getsilx(&disasm_basereg);
            disasm_genstr((byte*)"inc");
        }else if ((disasm_rmopc==(i64)1)) {
            disasm_getsilx(&disasm_basereg);
            disasm_genstr((byte*)"dec");
        }else if ((disasm_rmopc==(i64)2)) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"icall");
        }else if ((disasm_rmopc==(i64)4)) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"jmp");
        }else if ((disasm_rmopc==(i64)6)) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"push");
        } else {
            puts((i8 *)((byte*)"FFxx?"));
        };
        disasm_printaddrmode((i64)0);
    }break;
    default: {
        disasm_genstr((byte*)"Unknown opcode: ");
        disasm_genhex(opc);
    }
    } //SW
;
    if (!!(baseaddr)) {
        sprintf((i8 *)(str),(i8 *)((byte*)"%06X: "),baseaddr);
    } else {
        sprintf((i8 *)(str),(i8 *)((byte*)"%06X: "),pstart);
    };
    n = (disasm_codeptr - pstart);
    av_1 = n;
    while (av_1-- > 0) {
L7 :;
        sprintf((i8 *)(str2),(i8 *)((byte*)"%02X "),(u64)((*pstart++)));
        strcat((i8 *)(str),(i8 *)(str2));
L8 :;
    }L9 :;
    ;
    av_2 = ((i64)14 - n);
    while (av_2-- > 0) {
L10 :;
        strcat((i8 *)(str),(i8 *)((byte*)"-- "));
L11 :;
    }L12 :;
    ;
    strcat((i8 *)(str),(i8 *)(disasm_deststr));
    (*cptr) = disasm_codeptr;
    return str;
}

static void disasm_decodetwobyteinstr(void) {
    i64 opc;
    i64 rhssize;
    i64 third;
    i64 imm;
    byte *  opcstr;
    switch ((opc = (i64)((*disasm_codeptr++)))) {
    case 42:;
    {
        disasm_decodeaddr((i64)1);
        if (!!(disasm_f3override)) {
            disasm_genstr((byte*)"cvtsi2ss ");
        } else {
            disasm_genstr((byte*)"cvtsi2sd ");
        };
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 44:;
    {
        disasm_decodeaddr((i64)1);
        if (!!(disasm_f3override)) {
            disasm_genstr((byte*)"cvttss2si ");
            rhssize = (i64)4;
        } else {
            disasm_genstr((byte*)"cvttsd2si ");
            rhssize = (i64)8;
        };
        if (!!((disasm_rex & (i64)8))) {
            disasm_genstr(disasm_strreg(disasm_rmreg,(i64)8));
        } else {
            disasm_genstr(disasm_strreg(disasm_rmreg,(i64)4));
        };
        disasm_genstr((byte*)", ");
        disasm_opsize = rhssize;
        disasm_printaddrmode((i64)1);
    }break;
    case 45:;
    {
        disasm_decodeaddr((i64)1);
        if (!!(disasm_f3override)) {
            disasm_genstr((byte*)"cvtss2si ");
            rhssize = (i64)4;
        } else {
            disasm_genstr((byte*)"cvtsd2si ");
            rhssize = (i64)8;
        };
        if (!!((disasm_rex & (i64)8))) {
            disasm_genstr(disasm_strreg(disasm_rmreg,(i64)8));
        } else {
            disasm_genstr(disasm_strreg(disasm_rmreg,(i64)4));
        };
        disasm_genstr((byte*)", ");
        disasm_opsize = rhssize;
        disasm_printaddrmode((i64)1);
    }break;
    case 47:;
    {
        disasm_decodeaddr((i64)1);
        if (!!(disasm_sizeoverride)) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"comisd ");
        } else {
            disasm_opsize = (i64)4;
            disasm_genstr((byte*)"comiss ");
        };
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)1);
    }break;
    case 58:;
    {
        third = (i64)((*disasm_codeptr++));
        if ((third==(i64)99)) {
            disasm_genstr((byte*)"pcmpistri ");
        }else if ((third==(i64)98)) {
            disasm_genstr((byte*)"pcmpistrm ");
        } else {
            disasm_genstr((byte*)"Unknown opcode 2-byte opcode: 0F ");
            disasm_genhex(opc);
            return;
        };
        disasm_decodeaddr((i64)1);
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)1);
        disasm_genstr((byte*)", ");
        imm = (i64)((*disasm_codeptr++));
        disasm_genintd(imm);
    }break;
    case 64:;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 72:;
    case 73:;
    case 74:;
    case 75:;
    case 76:;
    case 77:;
    case 78:;
    case 79:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"cmov");
        disasm_genstr(disasm_condnames[((opc & (i64)15))]);
        disasm_genstr((byte*)" ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 81:;
    {
        disasm_decodeaddr((i64)1);
        disasm_opsize = (!!(disasm_f3override)?(i64)4:(i64)8);
        disasm_genstr(((disasm_opsize == (i64)4)?(byte*)"sqrtss ":(byte*)"sqrtsd "));
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)1);
    }break;
    case 84:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((!!(disasm_sizeoverride)?(byte*)"andpd ":(byte*)"andps "));
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_opsize = (!!(disasm_sizeoverride)?(i64)8:(i64)4);
        disasm_printaddrmode((i64)1);
    }break;
    case 87:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((!!(disasm_sizeoverride)?(byte*)"xorpd ":(byte*)"xorps "));
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_opsize = (!!(disasm_sizeoverride)?(i64)8:(i64)4);
        disasm_printaddrmode((i64)1);
    }break;
    case 88:;
    {
        opcstr = (byte*)"adds";
        //doarith:
L13 :;
;
        disasm_genstr(opcstr);
        disasm_decodeaddr((i64)1);
        if (!!(disasm_f2override)) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"d ");
        } else {
            disasm_opsize = (i64)4;
            disasm_genstr((byte*)"s ");
        };
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)1);
    }break;
    case 89:;
    {
        opcstr = (byte*)"muls";
        goto L13 ;
;
    }break;
    case 90:;
    {
        disasm_decodeaddr((i64)1);
        if (!!(disasm_f3override)) {
            disasm_genstr((byte*)"cvtss2sd ");
            rhssize = (i64)4;
        } else {
            disasm_genstr((byte*)"cvtsd2ss ");
            rhssize = (i64)8;
        };
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_opsize = rhssize;
        disasm_printaddrmode((i64)1);
    }break;
    case 92:;
    {
        opcstr = (byte*)"subs";
        goto L13 ;
;
    }break;
    case 93:;
    {
        opcstr = (byte*)"mins";
        goto L13 ;
;
    }break;
    case 94:;
    {
        opcstr = (byte*)"divs";
        goto L13 ;
;
    }break;
    case 95:;
    {
        opcstr = (byte*)"maxs";
        goto L13 ;
;
    }break;
    case 110:;
    {
        disasm_decodeaddr((i64)1);
        disasm_opsize = (!!((disasm_rex & (i64)8))?(i64)8:(i64)4);
        disasm_genstr(((disasm_opsize == (i64)4)?(byte*)"movd ":(byte*)"movq "));
        if (!!(disasm_sizeoverride)) {
            disasm_genstr(disasm_strxmm(disasm_rmreg));
        } else {
            disasm_genstr(disasm_strmmx(disasm_rmreg));
        };
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 111:;
    {
        disasm_decodeaddr((i64)1);
        disasm_opsize = (i64)16;
        if (!!(disasm_sizeoverride)) {
            disasm_genstr((byte*)"movdqa ");
        } else if (!!(disasm_f3override)) {
            disasm_genstr((byte*)"movdqu ");
        } else {
            disasm_genstr((byte*)"No 66/F3 ");
        };
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)1);
    }break;
    case 126:;
    {
        disasm_decodeaddr((i64)1);
        if (!!(disasm_f3override)) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"movq ");
            disasm_genstr(disasm_strxmm(disasm_rmreg));
            disasm_genstr((byte*)", ");
            disasm_printaddrmode((i64)1);
        } else if (!!((disasm_rex & (i64)8))) {
            disasm_opsize = (i64)8;
            disasm_genstr((byte*)"movq ");
            disasm_printaddrmode((i64)0);
            disasm_genstr((byte*)", ");
            disasm_genstr(disasm_strxmm(disasm_rmreg));
        } else {
            disasm_opsize = (i64)4;
            disasm_genstr((byte*)"movd ");
            disasm_printaddrmode((i64)0);
            disasm_genstr((byte*)", ");
            if (!!(disasm_sizeoverride)) {
                disasm_genstr(disasm_strxmm(disasm_rmreg));
            } else {
                disasm_genstr(disasm_strmmx(disasm_rmreg));
            };
        };
    }break;
    case 127:;
    {
        disasm_decodeaddr((i64)1);
        disasm_opsize = (i64)16;
        if (!!(disasm_sizeoverride)) {
            disasm_genstr((byte*)"movdqa ");
        } else if (!!(disasm_f3override)) {
            disasm_genstr((byte*)"movdqu ");
        } else {
            disasm_genstr((byte*)"No 66/F3 ");
        };
        disasm_printaddrmode((i64)1);
        disasm_genstr((byte*)", ");
        disasm_genstr(disasm_strxmm(disasm_rmreg));
    }break;
    case 128:;
    case 129:;
    case 130:;
    case 131:;
    case 132:;
    case 133:;
    case 134:;
    case 135:;
    case 136:;
    case 137:;
    case 138:;
    case 139:;
    case 140:;
    case 141:;
    case 142:;
    case 143:;
    {
        disasm_genstr((byte*)"[long] j");
        disasm_genstr(disasm_condnames[((opc & (i64)15))]);
        disasm_genstr((byte*)" ");
        if (!!(disasm_sizeoverride)) {
            disasm_genintd(disasm_readint16());
        } else {
            disasm_genintd(disasm_readint32());
        };
    }break;
    case 144:;
    case 145:;
    case 146:;
    case 147:;
    case 148:;
    case 149:;
    case 150:;
    case 151:;
    case 152:;
    case 153:;
    case 154:;
    case 155:;
    case 156:;
    case 157:;
    case 158:;
    case 159:;
    {
        disasm_decodeaddr((i64)0);
        disasm_genstr((byte*)"set");
        disasm_genstr(disasm_condnames[((opc & (i64)15))]);
        disasm_genstr((byte*)" ");
        disasm_getsilx(&disasm_basereg);
        disasm_printaddrmode((i64)0);
    }break;
    case 175:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"imul ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 182:;
    case 183:;
    case 190:;
    case 191:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr(((opc < (i64)190)?(byte*)"movzx ":(byte*)"movsx "));
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_opsize = (!!((opc & (i64)1))?(i64)2:(i64)1);
        disasm_printaddrmode((i64)0);
    }break;
    case 184:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"popcnt ");
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 188:;
    case 189:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr(((opc == (i64)188)?(byte*)"bsf ":(byte*)"bsr "));
        disasm_genstr(disasm_strreg(disasm_rmreg,disasm_opsize));
        disasm_genstr((byte*)", ");
        disasm_printaddrmode((i64)0);
    }break;
    case 214:;
    {
        disasm_decodeaddr((i64)1);
        disasm_opsize = (i64)8;
        disasm_genstr((byte*)"movq ");
        disasm_printaddrmode((i64)1);
        disasm_genstr((byte*)",");
        disasm_genstr(disasm_strxmm(disasm_rmreg));
    }break;
    case 219:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"pand ");
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_opsize = (i64)8;
        disasm_printaddrmode((i64)1);
    }break;
    case 239:;
    {
        disasm_decodeaddr((i64)1);
        disasm_genstr((byte*)"pxor ");
        disasm_genstr(disasm_strxmm(disasm_rmreg));
        disasm_genstr((byte*)", ");
        disasm_opsize = (i64)8;
        disasm_printaddrmode((i64)1);
    }break;
    default: {
        //error:
L14 :;
;
        disasm_genstr((byte*)"Unknown opcode 2-byte opcode: 0F ");
        disasm_genhex(opc);
    }
    } //SW
;
}

static void disasm_decodeaddr(i64 w) {
    i64 modrm;
    i64 xxx;
    i64 mode;
    i64 sib;
    i64 rm;
    disasm_basereg = (disasm_indexreg = (i64)0);
    disasm_scale = (i64)1;
    disasm_offset = (i64)0;
    if (!!(w)) {
        disasm_opsize = (!!(disasm_sizeoverride)?(i64)2:(i64)4);
        if (!!((disasm_rex & (i64)8))) {
            disasm_opsize = (i64)8;
        };
    } else {
        disasm_opsize = (i64)1;
    };
    modrm = (i64)((*disasm_codeptr++));
    mode = (modrm >> (i64)6);
    xxx = ((modrm >> (i64)3) & (i64)7);
    rm = (modrm & (i64)7);
    if ((mode == (i64)3)) {
        disasm_basereg = (rm + (i64)1);
        disasm_addrmode = (i64)1;
    } else if ((rm != (i64)4)) {
        if (((mode == (i64)0) && (rm == (i64)5))) {
            disasm_offset = disasm_readint32();
            disasm_addrmode = (i64)2;
        } else {
            disasm_basereg = (rm + (i64)1);
            disasm_addrmode = (i64)2;
            if ((mode==(i64)1)) {
                disasm_offset = disasm_readsbyte();
            }else if ((mode==(i64)2)) {
                disasm_offset = disasm_readint32();
            };
        };
    } else {
        disasm_addrmode = (i64)2;
        sib = disasm_readbyte();
        disasm_indexreg = (((sib >> (i64)3) & (i64)7) + (i64)1);
        disasm_basereg = ((sib & (i64)7) + (i64)1);
        disasm_scale = (((sib >> (i64)6) + (i64)1)==1?(i64)1:(((sib >> (i64)6) + (i64)1)==2?(i64)2:(((sib >> (i64)6) + (i64)1)==3?(i64)4:(((sib >> (i64)6) + (i64)1)==4?(i64)8:(i64)0))));
        if ((((mode == (i64)0) && (disasm_basereg == (i64)6)) && (disasm_indexreg == (i64)5))) {
            disasm_indexreg = (disasm_basereg = (i64)0);
            disasm_offset = disasm_readint32();
        } else if (((mode == (i64)0) && (disasm_basereg == (i64)6))) {
            disasm_basereg = (i64)0;
            disasm_offset = disasm_readint32();
        } else if (((mode == (i64)0) && (disasm_indexreg == (i64)5))) {
            disasm_indexreg = (i64)0;
        } else {
            if ((mode==(i64)1)) {
                disasm_offset = disasm_readsbyte();
            }else if ((mode==(i64)2)) {
                disasm_offset = disasm_readint32();
            };
            if ((disasm_indexreg == (i64)5)) {
                disasm_indexreg = (i64)0;
            };
        };
    };
    if ((!!(disasm_basereg) && !!((disasm_rex & (i64)1)))) {
        disasm_basereg += (i64)8;
    };
    if ((!!(disasm_indexreg) && !!((disasm_rex & (i64)2)))) {
        disasm_indexreg += (i64)8;
    };
    disasm_rmreg = (xxx + (i64)1);
    if (!!((disasm_rex & (i64)4))) {
        disasm_rmreg += (i64)8;
    };
    disasm_rmopc = xxx;
}

static i64 disasm_readbyte(void) {
    return (i64)((*disasm_codeptr++));
}

static i64 disasm_readsbyte(void) {
    return (i64)((*(i8 *)(disasm_codeptr++)));
}

static u64 disasm_readword16(void) {
    u64 a;
    a = (u64)((*(u16 *)(disasm_codeptr)));
    disasm_codeptr += (i64)2;
    return a;
}

static i64 disasm_readint16(void) {
    i64 a;
    a = (i64)((*(i16 *)(disasm_codeptr)));
    disasm_codeptr += (i64)2;
    return a;
}

static u64 disasm_readword32(void) {
    u64 a;
    a = (u64)((*(u32 *)(disasm_codeptr)));
    disasm_codeptr += (i64)4;
    return a;
}

static i64 disasm_readint32(void) {
    i64 a;
    a = (i64)((*(i32 *)(disasm_codeptr)));
    disasm_codeptr += (i64)4;
    return a;
}

static i64 disasm_readint64(void) {
    i64 a;
    a = (*(i64 *)(disasm_codeptr));
    disasm_codeptr += (i64)8;
    return a;
}

static i64 disasm_getreg(i64 regcode,i64 upper) {
    if (!!(upper)) {
        return ((regcode + (i64)8) + (i64)1);
    };
    return (regcode + (i64)1);
}

byte * disasm_strreg(i64 reg,i64 opsize) {
    static byte *  regnames8[20] = {
    (byte*)"al",
    (byte*)"cl",
    (byte*)"dl",
    (byte*)"bl",
    (byte*)"spl",
    (byte*)"bpl",
    (byte*)"sil",
    (byte*)"dil",
    (byte*)"r8b",
    (byte*)"r9b",
    (byte*)"r10b",
    (byte*)"r11b",
    (byte*)"r12b",
    (byte*)"r13b",
    (byte*)"r14b",
    (byte*)"r15b",
    (byte*)"ah",
    (byte*)"bh",
    (byte*)"ch",
    (byte*)"dh"
};
    static byte *  regnames16[16] = {
    (byte*)"ax",
    (byte*)"cx",
    (byte*)"dx",
    (byte*)"bx",
    (byte*)"sp",
    (byte*)"bp",
    (byte*)"si",
    (byte*)"di",
    (byte*)"r8w",
    (byte*)"r9w",
    (byte*)"r10w",
    (byte*)"r11w",
    (byte*)"r12w",
    (byte*)"r13w",
    (byte*)"r14w",
    (byte*)"r15w"
};
    static byte *  regnames32[16] = {
    (byte*)"eax",
    (byte*)"ecx",
    (byte*)"edx",
    (byte*)"ebx",
    (byte*)"esp",
    (byte*)"ebp",
    (byte*)"esi",
    (byte*)"edi",
    (byte*)"r8d",
    (byte*)"r9d",
    (byte*)"r10d",
    (byte*)"r11d",
    (byte*)"r12d",
    (byte*)"r13d",
    (byte*)"r14d",
    (byte*)"r15d"
};
    static byte *  regnames64[16] = {
    (byte*)"rax",
    (byte*)"rcx",
    (byte*)"rdx",
    (byte*)"rbx",
    (byte*)"rsp",
    (byte*)"rbp",
    (byte*)"rsi",
    (byte*)"rdi",
    (byte*)"r8",
    (byte*)"r9",
    (byte*)"r10",
    (byte*)"r11",
    (byte*)"r12",
    (byte*)"r13",
    (byte*)"r14",
    (byte*)"r15"
};
    static byte *  mregnames8[20] = {
    (byte*)"B0",
    (byte*)"B10",
    (byte*)"B11",
    (byte*)"B4",
    (byte*)"B15",
    (byte*)"B14",
    (byte*)"B5",
    (byte*)"B3",
    (byte*)"B12",
    (byte*)"B13",
    (byte*)"B1",
    (byte*)"B2",
    (byte*)"B6",
    (byte*)"B7",
    (byte*)"B8",
    (byte*)"B9",
    (byte*)"B16",
    (byte*)"B18",
    (byte*)"B19",
    (byte*)"B17"
};
    static byte *  mregnames16[16] = {
    (byte*)"W0",
    (byte*)"W10",
    (byte*)"W11",
    (byte*)"W4",
    (byte*)"Wsp",
    (byte*)"Wbp",
    (byte*)"W5",
    (byte*)"W3",
    (byte*)"W12",
    (byte*)"W13",
    (byte*)"W1",
    (byte*)"W2",
    (byte*)"W6",
    (byte*)"W7",
    (byte*)"W8",
    (byte*)"W9"
};
    static byte *  mregnames32[16] = {
    (byte*)"A0",
    (byte*)"A10",
    (byte*)"A11",
    (byte*)"A4",
    (byte*)"Astack",
    (byte*)"Aframe",
    (byte*)"A5",
    (byte*)"A3",
    (byte*)"A12",
    (byte*)"A13",
    (byte*)"A1",
    (byte*)"A2",
    (byte*)"A6",
    (byte*)"A7",
    (byte*)"A8",
    (byte*)"A9"
};
    static byte *  mregnames64[16] = {
    (byte*)"D0",
    (byte*)"D10",
    (byte*)"D11",
    (byte*)"D4",
    (byte*)"Dstack",
    (byte*)"Dframe",
    (byte*)"D5",
    (byte*)"D3",
    (byte*)"D12",
    (byte*)"D13",
    (byte*)"D1",
    (byte*)"D2",
    (byte*)"D6",
    (byte*)"D7",
    (byte*)"D8",
    (byte*)"D9"
};
    if ((reg == (i64)0)) {
        return (byte*)"<>";
    };
    if (!!((i64)0)) {
        if ((opsize==(i64)1)) {
            return mregnames8[(reg)-1];
        }else if ((opsize==(i64)2)) {
            return mregnames16[(reg)-1];
        }else if ((opsize==(i64)4)) {
            return mregnames32[(reg)-1];
        }else if ((opsize==(i64)8)) {
            return mregnames64[(reg)-1];
        };
    } else {
        if ((opsize==(i64)1)) {
            return regnames8[(reg)-1];
        }else if ((opsize==(i64)2)) {
            return regnames16[(reg)-1];
        }else if ((opsize==(i64)4)) {
            return regnames32[(reg)-1];
        }else if ((opsize==(i64)8)) {
            return regnames64[(reg)-1];
        };
    };
    return (byte*)"";
}

static byte * disasm_strfreg(i64 freg) {
    static byte *  fregnames[8] = {(byte*)"st0",(byte*)"st1",(byte*)"st2",(byte*)"st3",(byte*)"st4",(byte*)"st5",(byte*)"st6",(byte*)"st7"};
    return fregnames[(freg)-1];
}

static void disasm_printaddrmode(i64 xmm) {
    byte *  plus;
    i64 addrsize;
    disasm_genstr((byte*)" ");
    if ((disasm_addrmode==(i64)1)) {
        if (!!(xmm)) {
            disasm_genstr(disasm_strxmm(disasm_basereg));
        } else {
            disasm_getsilx(&disasm_basereg);
            disasm_genstr(disasm_strreg(disasm_basereg,disasm_opsize));
        };
        return;
    };
    if ((disasm_opsize==(i64)1)) {
        disasm_genstr((byte*)"byte ");
    }else if ((disasm_opsize==(i64)2)) {
        disasm_genstr((byte*)"word ");
    }else if ((disasm_opsize==(i64)4)) {
        disasm_genstr((byte*)"dword ");
    }else if ((disasm_opsize==(i64)8)) {
        disasm_genstr((byte*)"qword ");
    }else if ((disasm_opsize==(i64)10)) {
        disasm_genstr((byte*)"tword ");
    }else if ((disasm_opsize==(i64)16)) {
        disasm_genstr((byte*)"oword ");
    } else {
        printf((i8 *)((byte*)"OPSIZE? %lld\n"),disasm_opsize);
    };
    disasm_genstr((byte*)"[");
    plus = (byte*)"";
    addrsize = (!!(disasm_addroverride)?(i64)4:(i64)8);
    if (!!(disasm_basereg)) {
        disasm_genstr(disasm_strreg(disasm_basereg,addrsize));
        plus = (byte*)"+";
    };
    if (!!(disasm_indexreg)) {
        disasm_genstr(plus);
        disasm_genstr(disasm_strreg(disasm_indexreg,addrsize));
        if ((disasm_scale > (i64)1)) {
            disasm_genstr((byte*)"*");
            disasm_genintd(disasm_scale);
        };
        plus = (byte*)"+";
    };
    if ((!!(disasm_offset) || ((disasm_basereg == (i64)0) && (disasm_indexreg == (i64)0)))) {
        if (((disasm_basereg == (i64)0) && (disasm_indexreg == (i64)0))) {
            disasm_genhex(disasm_offset);
        } else {
            if ((disasm_offset > (i64)0)) {
                disasm_genstr(plus);
            };
            disasm_genintd(disasm_offset);
        };
    };
    disasm_genstr((byte*)"]");
    if ((disasm_addrmode == (i64)3)) {
        disasm_genstr((byte*)"+RIP");
    };
}

static void disasm_genstr(byte * s) {
    strcat((i8 *)(disasm_deststr),(i8 *)(s));
}

static void disasm_genintd(i64 a) {
    static byte str[128];
    sprintf((i8 *)(str),(i8 *)((byte*)"%lld"),a);
    disasm_genstr(str);
}

static void disasm_genhex(i64 a) {
    static byte str[128];
    sprintf((i8 *)(str),(i8 *)((byte*)"%llX"),a);
    disasm_genstr(str);
}

static i64 disasm_readimm(void) {
    if ((disasm_opsize==(i64)1)) {
        return disasm_readsbyte();
    }else if ((disasm_opsize==(i64)2)) {
        return disasm_readint16();
    }else if ((disasm_opsize==(i64)4) || (disasm_opsize==(i64)8)) {
        return disasm_readint32();
    };
    return (i64)0;
}

static i64 disasm_readimm8(void) {
    if ((disasm_opsize < (i64)8)) {
        return disasm_readimm();
    };
    return disasm_readint64();
}

static byte * disasm_strxmm(i64 reg) {
    static byte str[32];
    sprintf((i8 *)(str),(i8 *)((byte*)"xmm%lld"),(reg - (i64)1));
    return str;
}

static byte * disasm_strmmx(i64 reg) {
    static byte str[32];
    sprintf((i8 *)(str),(i8 *)((byte*)"mmx%lld"),(reg - (i64)1));
    return str;
}

static void disasm_decode8087(i64 ttt) {
    byte bb;
    i64 longopc;
    i64 freg;
    i64 shortopc;
    bb = (u64)((*disasm_codeptr++));
    longopc = ((ttt << (i64)8) + (i64)((u64)(bb)));
    freg = (((i64)((u64)(bb)) & (i64)7) + (i64)1);
    if ((longopc==(i64)1753)) {
        disasm_genstr((byte*)"fcompp");
    }else if ((longopc==(i64)484)) {
        disasm_genstr((byte*)"ftst");
    }else if ((longopc==(i64)485)) {
        disasm_genstr((byte*)"fxam");
    }else if ((longopc==(i64)494)) {
        disasm_genstr((byte*)"fldz");
    }else if ((longopc==(i64)488)) {
        disasm_genstr((byte*)"fld1");
    }else if ((longopc==(i64)491)) {
        disasm_genstr((byte*)"fldpi");
    }else if ((longopc==(i64)489)) {
        disasm_genstr((byte*)"fldl2t");
    }else if ((longopc==(i64)490)) {
        disasm_genstr((byte*)"fldl2e");
    }else if ((longopc==(i64)492)) {
        disasm_genstr((byte*)"fldlg2");
    }else if ((longopc==(i64)493)) {
        disasm_genstr((byte*)"fldln2");
    }else if ((longopc==(i64)506)) {
        disasm_genstr((byte*)"fsqrt");
    }else if ((longopc==(i64)510)) {
        disasm_genstr((byte*)"fsin");
    }else if ((longopc==(i64)511)) {
        disasm_genstr((byte*)"fcos");
    }else if ((longopc==(i64)507)) {
        disasm_genstr((byte*)"fsincos");
    }else if ((longopc==(i64)509)) {
        disasm_genstr((byte*)"fscale");
    }else if ((longopc==(i64)504)) {
        disasm_genstr((byte*)"fprem");
    }else if ((longopc==(i64)508)) {
        disasm_genstr((byte*)"frndint");
    }else if ((longopc==(i64)500)) {
        disasm_genstr((byte*)"fxtract");
    }else if ((longopc==(i64)481)) {
        disasm_genstr((byte*)"fabs");
    }else if ((longopc==(i64)480)) {
        disasm_genstr((byte*)"fchs");
    }else if ((longopc==(i64)498)) {
        disasm_genstr((byte*)"fptan");
    }else if ((longopc==(i64)499)) {
        disasm_genstr((byte*)"fpatan");
    }else if ((longopc==(i64)496)) {
        disasm_genstr((byte*)"f2xm1");
    }else if ((longopc==(i64)497)) {
        disasm_genstr((byte*)"fyl2x");
    }else if ((longopc==(i64)505)) {
        disasm_genstr((byte*)"fyl2xp1");
    }else if ((longopc==(i64)995)) {
        disasm_genstr((byte*)"finit");
    }else if ((longopc==(i64)992)) {
        disasm_genstr((byte*)"feni");
    }else if ((longopc==(i64)993)) {
        disasm_genstr((byte*)"fdisi");
    }else if ((longopc==(i64)994)) {
        disasm_genstr((byte*)"fclex");
    }else if ((longopc==(i64)503)) {
        disasm_genstr((byte*)"fincstp");
    }else if ((longopc==(i64)502)) {
        disasm_genstr((byte*)"fdecstp");
    }else if ((longopc==(i64)464)) {
        disasm_genstr((byte*)"fnop");
    } else {
        if (((longopc & (i64)2040)==(i64)448)) {
            disasm_genstr((byte*)"fld ");
            disasm_genstr(disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)1488)) {
            disasm_genstr((byte*)"fst ");
            disasm_genstr(disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)1496)) {
            disasm_genstr((byte*)"fstp ");
            disasm_genstr(disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)456)) {
            disasm_genstr((byte*)"fxch ");
            disasm_genstr(disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)208)) {
            disasm_genstr((byte*)"fcom ");
            disasm_genstr(disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)216)) {
            disasm_genstr((byte*)"fcomp ");
            disasm_genstr(disasm_strfreg(freg));
        }else if (((longopc & (i64)2040)==(i64)1472)) {
            disasm_genstr((byte*)"ffree ");
            disasm_genstr(disasm_strfreg(freg));
        } else {
            if (((longopc & (i64)504)==(i64)192)) {
                disasm_do87arith((byte*)"fadd",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)224)) {
                disasm_do87arith((byte*)"fsub",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)232)) {
                disasm_do87arith((byte*)"fsubr",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)200)) {
                disasm_do87arith((byte*)"fmul",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)240)) {
                disasm_do87arith((byte*)"fdiv",ttt,freg);
            }else if (((longopc & (i64)504)==(i64)248)) {
                disasm_do87arith((byte*)"fdivr",ttt,freg);
            } else {
                --disasm_codeptr;
                disasm_decodeaddr((i64)0);
                shortopc = ((ttt << (i64)3) + disasm_rmopc);
                if ((shortopc==(i64)61)) {
                    disasm_do87mem((byte*)"fld",(i64)4);
                }else if ((shortopc==(i64)29)) {
                    disasm_do87mem((byte*)"fld",(i64)5);
                }else if ((shortopc==(i64)60)) {
                    disasm_do87mem((byte*)"fldbcd",(i64)-1);
                }else if ((shortopc==(i64)63)) {
                    disasm_do87mem((byte*)"fstp",(i64)4);
                }else if ((shortopc==(i64)31)) {
                    disasm_do87mem((byte*)"fstp",(i64)5);
                }else if ((shortopc==(i64)62)) {
                    disasm_do87mem((byte*)"fstpbcd",(i64)-1);
                }else if ((shortopc==(i64)13)) {
                    disasm_do87mem((byte*)"fldcw",(i64)-1);
                }else if ((shortopc==(i64)15)) {
                    disasm_do87mem((byte*)"fstcw",(i64)-1);
                }else if ((shortopc==(i64)47)) {
                    disasm_do87mem((byte*)"fstsw",(i64)-1);
                }else if ((shortopc==(i64)14)) {
                    disasm_do87mem((byte*)"fstenv",(i64)-1);
                }else if ((shortopc==(i64)12)) {
                    disasm_do87mem((byte*)"fldenv",(i64)-1);
                }else if ((shortopc==(i64)46)) {
                    disasm_do87mem((byte*)"fsave",(i64)-1);
                }else if ((shortopc==(i64)44)) {
                    disasm_do87mem((byte*)"frstor",(i64)-1);
                } else {
                    if (((shortopc & (i64)15)==(i64)8)) {
                        disasm_do87mem((byte*)"fld",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)10)) {
                        disasm_do87mem((byte*)"fst",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)11)) {
                        disasm_do87mem((byte*)"fstp",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)2)) {
                        disasm_do87mem((byte*)"fcom",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)3)) {
                        disasm_do87mem((byte*)"fcomp",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)0)) {
                        disasm_do87mem((byte*)"fadd",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)4)) {
                        disasm_do87mem((byte*)"fsub",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)5)) {
                        disasm_do87mem((byte*)"fsubr",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)1)) {
                        disasm_do87mem((byte*)"fmul",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)6)) {
                        disasm_do87mem((byte*)"fdiv",(ttt >> (i64)1));
                    }else if (((shortopc & (i64)15)==(i64)7)) {
                        disasm_do87mem((byte*)"fdivr",(ttt >> (i64)1));
                    } else {
                        disasm_genstr((byte*)"UNKNOWN x87 OPCODE");
                    };
                };
            };
        };
    };
}

static void disasm_do87arith(byte * opcstr,i64 ttt,i64 freg) {
    i64 d;
    i64 p;
    d = (ttt & (i64)4);
    p = (ttt & (i64)2);
    disasm_genstr(opcstr);
    if (!!(p)) {
        disasm_genstr((byte*)"p");
    };
    disasm_genstr((byte*)" ");
    if ((d == (i64)0)) {
        disasm_genstr((byte*)"st0, ");
        disasm_genstr(disasm_strfreg(freg));
    } else {
        disasm_genstr(disasm_strfreg(freg));
        disasm_genstr((byte*)", st0");
    };
}

static void disasm_do87mem(byte * opcstr,i64 mf) {
    disasm_genstr((byte*)"f");
    if ((mf==(i64)0)) {
        disasm_opsize = (i64)4;
    }else if ((mf==(i64)1)) {
        disasm_genstr((byte*)"i");
        disasm_opsize = (i64)4;
    }else if ((mf==(i64)2)) {
        disasm_opsize = (i64)8;
    }else if ((mf==(i64)3)) {
        disasm_genstr((byte*)"i");
        disasm_opsize = (i64)2;
    }else if ((mf==(i64)4)) {
        disasm_genstr((byte*)"i");
        disasm_opsize = (i64)8;
    }else if ((mf==(i64)5)) {
        disasm_opsize = (i64)10;
    };
    disasm_genstr((opcstr + (i64)1));
    disasm_genstr((byte*)" ");
    disasm_printaddrmode((i64)0);
}

static void disasm_getsil(i64 * reg) {
    if (((((disasm_opsize == (i64)1) && !(!!(disasm_rex))) && ((*reg) >= (i64)5)) && ((*reg) <= (i64)8))) {
        (*reg) += (i64)12;
    };
}

static void disasm_getsilx(i64 * reg) {
    if ((((((disasm_addrmode == (i64)1) && (disasm_opsize == (i64)1)) && (disasm_rex == (i64)0)) && ((*reg) >= (i64)5)) && ((*reg) <= (i64)8))) {
        (*reg) += (i64)12;
    };
}


/* ********** End of C Code ********** */
