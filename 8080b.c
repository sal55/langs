/*
 * 8080 Interpreter
 * 12/20/90
 *
 * Copyright 1990 Erik Mueller
 *
 * Input ports:
 * 0: read character from standard input
 * 1: 3 if control-C typed, 0 otherwise
 *
 * Output ports:
 * 0: write character to standard output
 * 1: clear screen
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

typedef unsigned char byte_t;
byte_t regs[8];
unsigned short pc;
unsigned short sp;

#define B 000
#define C 001
#define D 002
#define E 003
#define H 004
#define L 005
#define PSW 006
#define A 007

#define MAKEWORD(h, l) (((h) << 8) | (l))
#define GETH(w) ((byte_t)((w) >> 8))
#define GETL(w) ((byte_t)(w & 0377))

#define PSW_CY ((short)(regs[PSW] & 0001))
#define PSW_P ((short)((regs[PSW] >> 2) & 0001))
#define PSW_AC ((short)((regs[PSW] >> 4) & 0001))
#define PSW_Z ((short)((regs[PSW] >> 6) & 0001))
#define PSW_S ((short)((regs[PSW] >> 7) & 0001))

#define FORM_PSW(s,z,ac,p,cy) ((byte_t)(((s)<<7)|((z)<<6)|((ac)<<4)|((p)<<2)|(cy)))

#define ADDRSPACE 65536
byte_t memory[ADDRSPACE];

#define I_LDA 0072
#define I_STA 0062
#define I_LHLD 0052
#define I_SHLD 0042
#define I_XCHG 0353
#define I_ADI 0306
#define I_ACI 0316
#define I_SUI 0326
#define I_SBI 0336
#define I_ANI 0346
#define I_XRI 0356
#define I_ORI 0366
#define I_CPI 0376
#define I_RLC 0007
#define I_RRC 0017
#define I_RAL 0027
#define I_RAR 0037
#define I_DAA 0047
#define I_CMA 0057
#define I_STC 0067
#define I_CMC 0077
#define I_JMP 0303
#define I_CALL 0315
#define I_RET 0311
#define I_PCHL 0351
#define I_XTHL 0343
#define I_SPHL 0371
#define I_IN 0333
#define I_OUT 0323
#define I_EI 0373
#define I_DI 0363
#define I_HLT 0166
#define I_NOP 0000

#define I_SSSDDD 0300
#define I_MOV 0100

#define I_RP 0317
#define I_LXI 0001
#define I_LDAX 0012
#define I_STAX 0002
#define I_INX 0003
#define I_DCX 0013
#define I_DAD 0011
#define I_PUSH 0305
#define I_POP 0301

#define I_SSS 0370
#define I_ADD 0200
#define I_ADC 0210
#define I_SUB 0220
#define I_SBB 0230
#define I_ANA 0240
#define I_XRA 0250
#define I_ORA 0260
#define I_CMP 0270

#define I_DDD 0307
#define I_INR 0004
#define I_DCR 0005

#define I_MVIM 0307
#define I_MVI  0006

#define I_CCC 0307
#define I_JCOND 0302
#define I_CCOND 0304
#define I_RCOND 0300

#define I_RST 0307

#define xor(a, b) ((a)^(b))

long	cntlc;

void regdump(void);
void instset(void);
void run(void);
void cycle(void);
void clear_screen(void);
void load_file(char*, long);
void set_psw(byte_t, short, short);
//int getopt(int argc, char** argv, char*);
//char* gets(char*);

void interr(void)
{
	printf("\n");
	regdump();
	cntlc = 1;
}

void setup_signals(void)
{
//	signal(SIGTERM, exit);
//	signal(SIGQUIT, interr);
//	signal(SIGHUP, exit);
//	signal(SIGINT, interr);
}

long	dbg;
unsigned short	spbot;
unsigned short	bkpt;
long	bkptset;
FILE	*instream;

int main(int argc, char** argv) {
	extern int	opterr, optind;
	extern char	*optarg;
	char		c;
	long		i;

  	for (i = 0; i < ADDRSPACE; i++) memory[i] = I_HLT;
	pc = 0;
	dbg = 0;
	spbot = 0;
        bkptset = 0;
	instream = stdin;
	opterr = 0;
	while ((c = getopt(argc,argv,"f:b:ida:l:")) != -1) {
		switch (c) {
			case 'f':
			/* File as input to 8080 program */
				if ((instream = fopen(optarg, "rb")) == NULL) {
					fprintf(stderr, "8080: trouble opening %s\n",
						optarg);
					instream = stdin;
				}
				break;
			case 'i':
			/* Print instruction set */
				instset();
				break;
			case 'd':
			/* 8080 debugging */
				dbg = 1;
				break;
			case 'a':
			/* Load address */
				pc = atoi(optarg);
				break;
			case 'l':
			/* Object code file to load at load address */
				load_file(optarg, pc);
				break;
			case 'b':
			/* Breakpoint */
				bkpt = atoi(optarg);
        			bkptset = 1;
				break;
			default:
				fprintf(stderr, "Usage: 8080 -a <addr> -l <file>\n");
				exit(1);
		}
	}
	setup_signals();
	run();
}

void instset(void)
{
	unsigned long	i;

	for (i = 0; i < 256; i++) {
		if (i == I_HLT) {
			fprintf(stderr, "hlt\n");
			continue;
		}
		memory[0] = i;
		memory[1] = 0;
		memory[2] = 0;
		pc = 0;
		cycle();
	}
}

void load_file(char* filename, long addr)
{
	FILE	*stream;
	long	cnt;

	if ((stream = fopen(filename, "rb")) == NULL) {
		fprintf(stderr, "8080: file %s not found\n", filename);
		exit(1);
	}
	while ((cnt = fread(&memory[addr], (size_t)1, (size_t)512, stream)) == 512) {
		addr += cnt;
		if (addr >= ADDRSPACE) {
			fprintf(stderr, "8080: file %s too big\n", filename);
			exit(1);
		}
	}
	if (ferror(stream)) {
		fprintf(stderr, "8080: error reading file\n");
		exit(1);
	}
	if (cnt >= 0) {
		if (addr >= ADDRSPACE) {
			fprintf(stderr, "8080: error reading file, cnt = %d\n",
				cnt);
			exit(1);
		}
	}
	fclose(stream);
}

byte_t getreg(short r)
{
	if (r == 006) return(memory[MAKEWORD(regs[H], regs[L])]);
	else return(regs[r]);
}

void setreg(short r, byte_t v)
{
	if (r == 006) memory[MAKEWORD(regs[H], regs[L])] = v;
	else regs[r] = v;
}

void getpair(short p, byte_t* hp, byte_t* lp)
{
	if (p == 003) {
		*hp = GETH(sp);
		*lp = GETL(sp);
	} else {
		*hp = regs[p<<1];
		*lp = regs[(p<<1)+1];
	}
}

void setpair(short p, byte_t h, byte_t l)
{
	if (p == 003) {
		sp = MAKEWORD(h, l);
	} else {
		regs[p<<1] = h;
		regs[(p<<1)+1] = l;
	}
}

void getpair_push(short p, byte_t* hp, byte_t* lp)
{
	*hp = regs[p<<1];
	*lp = regs[(p<<1)+1];
}

void setpair_pop(short p, byte_t h, byte_t l)
{
	regs[p<<1] = h;
	regs[(p<<1)+1] = l;
}

int getccc(short ccc)
{
	switch (ccc) {
		case 0: return(!PSW_Z);
		case 1: return(PSW_Z);
		case 2: return(!PSW_CY);
		case 3: return(PSW_CY);
		case 4: return(!PSW_P);
		case 5: return(PSW_P);
		case 6: return(!PSW_S);
		case 7: return(PSW_S);
		default: return(0);
	}
}

char *regname(short r)
{
	switch (r) {
		case B: return("b");
		case C: return("c");
		case D: return("d");
		case E: return("e");
		case H: return("h");
		case L: return("l");
		case PSW: return("m");
		case A: return("a");
		default: return("?");
	}
}

char *pairname(short p)
{
	switch (p) {
		case 0: return("bc");
		case 1: return("de");
		case 2: return("hl");
		case 3: return("sp");
		default: return("??");
	}
}

char *pairname_pushpop(short p)
{
	switch (p) {
		case 0: return("bc");
		case 1: return("de");
		case 2: return("hl");
		case 3: return("psw");
		default: return("??");
	}
}

char *condname(short ccc)
{
	switch (ccc) {
		case 0: return("nz");
		case 1: return("z");
		case 2: return("nc");
		case 3: return("c");
		case 4: return("po");
		case 5: return("pe");
		case 6: return("p");
		case 7: return("m");
		default: return("?");
	}
}

int add(byte_t a, byte_t b, short cy, short newcy)
{
	byte_t	s;

	s = a+b+cy;
	if (newcy) set_psw(s, ((short)(((a&0017)+(b&0017)+cy) > 0017)),
			((short)(a+b+cy)>0377));
	else set_psw(s, ((short)(((a&0017)+(b&0017)+cy) > 0017)), PSW_CY);
	return(s);
}

int sub(byte_t a, byte_t b, short cy, short newcy)
{
	byte_t	s;
	short	v;

	v = a-b-cy;
	s = v & 0377;
	if (newcy) set_psw(s, 0, ((short)(v < 0)));
	else set_psw(s, 0, PSW_CY);
	return(s);
}

int parity(byte_t v)
{
	byte_t	sum;
	long	i;

	for (sum = 0, i = 0; i < 8; i++) sum += ((v >> i) & 1);
	return(sum & 1);
}

void set_psw(byte_t v, short ac, short c)
{
	regs[PSW] = FORM_PSW(v >> 7, v == 0, ac, parity(v), c);
}

void set_cy(short c)
{
	regs[PSW] = FORM_PSW(PSW_S,PSW_Z,PSW_AC,PSW_P,c);
}

void halt(void)
{
	printf("\n");
	fprintf(stderr, "8080: program halted at %04x\n", pc-1);
	exit(0);
}

int in(byte_t port)
{
	unsigned char	c;
	long	d;

	if (port == 0) {
		d = getc(instream);
		while (d == EOF) {
			instream = stdin;
			d = getc(instream);
		}
		c = d;
		if (c == '\r') return(in(port));
		else if (c == '\n') return(0215);
		else return(0200 | c);
	} else if (port == 1) {
		if (cntlc) {
			cntlc = 0;
			return(3);
		}
	} else {
		fprintf(stderr, "8080: please enter input character from port 0x%x: ", port);
		c = getchar(); getchar();
		return(c);
	}
}

void out(byte_t port, byte_t value)
{
	if (port == 0) {
		value = 0177 & value;
		if (value == 0x0d) putchar('\n');
		else putchar(value);
	} else if (port == 1) clear_screen();
	else {
		fprintf(stderr, "8080: 0x%x output to port 0x%x\n", value, port);
	}
}

void clear_screen(void)
{
	/* For Sun */
/*
	putchar(0x1b);
	putchar(0x5b);
	putchar(0x48);
	putchar(0x1b);
	putchar(0x5b);
	putchar(0x32);
	putchar(0x4a);
	putchar(0x00);
 */
}

void run(void)
{
	char	buf[256];
	static long stop = 1;

	while (1) {
		cycle();
		if (dbg & stop) {
			fprintf(stderr, "> ");
			if (gets(buf) == NULL) exit(0);
			if (buf[0] == 'q') exit(0);
			if (buf[0] == 'c') stop = 0;
		}
		if (bkptset && (pc == bkpt)) {
			fprintf(stderr, "8080: breakpoint reached\n");
			dbg = 1;
			stop = 1;
		}
	}
}

void regdump(void)
{
	unsigned short		j;

	fprintf(stderr, "----------\n");
	fprintf(stderr, "  pc   sp  b c  d e  h l psw a  cy  p ac  z  s\n");
	fprintf(stderr,
"%04x %04x %02x%02x %02x%02x %02x%02x  %02x%02x  %2x %2x %2x %2x %2x\n",
			pc, sp, regs[B], regs[C], regs[D], regs[E],
			regs[H], regs[L], regs[PSW], regs[A],
			PSW_CY, PSW_P, PSW_AC, PSW_Z, PSW_S);
	if (spbot > 0) {
		fprintf(stderr, "stack: ");
		for (j = spbot; j > sp; j -= 2)
			fprintf(stderr, "%02x%02x ",
				memory[j-1], memory[j-2]);
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "----------\n");
	fprintf(stderr, "0cda: %02x\n", memory[0x0cda]);
}

void cycle(void)
{
	byte_t		i, v, vl, vh;
	unsigned short	hl;
	long		a, b;
	short		r1, r2, c, p;

	if (dbg) regdump();
	i = memory[pc]; pc++;
	if (dbg) fprintf(stderr, "%04x: %02x ", pc-1, i);
	switch (i) {
		case I_LDA:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "lda %02x%02x\n", vh, vl);
			regs[A] = memory[MAKEWORD(vh, vl)];
			return;
		case I_STA:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "sta %02x%02x\n", vh, vl);
			memory[MAKEWORD(vh, vl)] = regs[A];
			return;
		case I_LHLD:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "lhld %02x%02x\n", vh, vl);
			hl = MAKEWORD(vh, vl);
			regs[L] = memory[hl];
			regs[H] = memory[hl+1];
			return;
		case I_SHLD:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "shld %02x%02x\n", vh, vl);
			hl = MAKEWORD(vh, vl);
			memory[hl] = regs[L];
			memory[hl+1] = regs[H];
			return;
		case I_XCHG:
			if (dbg) fprintf(stderr, "xchg\n");
			vh = regs[D]; vl = regs[E];
			regs[D] = regs[H]; regs[E] = regs[L];
			regs[H] = vh; regs[L] = vl;
			return;
		case I_ADI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "adi %02x\n", v);
			regs[A] = add(regs[A], v, 0, 1);
			return;
		case I_ACI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "aci %02x\n", v);
			regs[A] = add(regs[A], v, PSW_CY, 1);
			return;
		case I_SUI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "sui %02x\n", v);
			regs[A] = sub(regs[A], v, 0, 1);
			return;
		case I_SBI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "sbi %02x\n", v);
			regs[A] = sub(regs[A], v, PSW_CY, 1);
			return;
		case I_ANI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "ani %02x\n", v);
			regs[A] = regs[A] & v;
			set_psw(regs[A], 1, 0);
			return;
		case I_XRI:
			a = regs[A];
			b = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "xri %02x\n", v);
			regs[A] = xor(a,b);
			set_psw(regs[A], 1, 0);
			return;
		case I_ORI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "ori %02x\n", v);
			regs[A] = regs[A] | v;
			set_psw(regs[A], 1, 0);
			return;
		case I_CPI:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "cpi %02x\n", v);
			sub(regs[A], v, 0, 1);
			return;
		case I_RLC:
			if (dbg) fprintf(stderr, "rlc\n");
			set_cy(((short)(regs[A] >> 7)));
			regs[A] = (regs[A] >> 7) | (regs[A] << 1);
			return;
		case I_RRC:
			if (dbg) fprintf(stderr, "rrc\n");
			set_cy(((short)(regs[A] & 1)));
			regs[A] = ((regs[A] & 1) << 7) | (regs[A] >> 1);
			return;
		case I_RAL:
			if (dbg) fprintf(stderr, "ral\n");
			v = regs[A];
			regs[A] = (regs[A] << 1) | PSW_CY;
			set_cy(((short)(v >> 7)));
			return;
		case I_RAR:
			if (dbg) fprintf(stderr, "rar\n");
			v = regs[A];
			regs[A] = (regs[A] >> 1) | (PSW_CY << 7);
			set_cy(((short)(v & 1)));
			return;
		case I_DAA:
			if (dbg) fprintf(stderr, "daa\n");
			if (((regs[A] & 0017) > 9) || PSW_AC)
				regs[A] = add(regs[A], (byte_t) 6, 0, 1);
			if ((((regs[A] & 0360)>>4) > 9) || PSW_CY)
				regs[A] = add(regs[A], (byte_t) (6 << 4), 0, 1);
			return;
		case I_CMA:
			if (dbg) fprintf(stderr, "cma\n");
			regs[A] = ~regs[A];
			return;
		case I_STC:
			if (dbg) fprintf(stderr, "stc\n");
			set_cy(1);
			return;
		case I_CMC:
			if (dbg) fprintf(stderr, "cmc\n");
			set_cy(!PSW_CY);
			return;
		case I_JMP:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "jmp %02x%02x\n", vh, vl);
			pc = MAKEWORD(vh, vl);
			return;
		case I_CALL:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "call %02x%02x\n", vh, vl);
			sp--; memory[sp] = GETH(pc);
			sp--; memory[sp] = GETL(pc);
			pc = MAKEWORD(vh, vl);
			return;
		case I_RET:
			if (dbg) fprintf(stderr, "ret\n");
			pc = MAKEWORD(memory[sp+1],memory[sp]);
			sp += 2;
			return;
		case I_PCHL:
			if (dbg) fprintf(stderr, "pchl\n");
			pc = MAKEWORD(regs[H], regs[L]);
			return;
		case I_XTHL:
			if (dbg) fprintf(stderr, "xthl\n");
			vl = memory[sp];
			vh = memory[sp+1];
			memory[sp] = regs[L];
			memory[sp+1] = regs[H];
			regs[H] = vh;
			regs[L] = vl;
			return;
		case I_SPHL:
			if (dbg) fprintf(stderr, "sphl\n");
			sp = MAKEWORD(regs[H], regs[L]);
			return;
		case I_IN:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "in %02x\n", v);
			regs[A] = in(v);
			return;
		case I_OUT:
			v = memory[pc]; pc++;
			if (dbg) fprintf(stderr, "out %02x\n", v);
			out(v, regs[A]);
			return;
		case I_EI:
			if (dbg) fprintf(stderr, "ei\n");
			fprintf(stderr, "8080: interrs enabled\n");
			return;
		case I_DI:
			if (dbg) fprintf(stderr, "di\n");
			fprintf(stderr, "8080: interrs disabled\n");
			return;
		case I_HLT:
			if (dbg) fprintf(stderr, "hlt\n");
			halt();
			return;
		case I_NOP:
			if (dbg) fprintf(stderr, "nop\n");
			return;
	}
	switch (i & I_RP) {
		case I_LXI:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			p = (i & 060) >> 4;
			if (dbg) {
				fprintf(stderr, "lxi %s,%02x%02x\n", pairname(p), vh, vl);
				if (p == 3) spbot = MAKEWORD(vh, vl);
			}
			setpair(p, vh, vl);
			return;
		case I_LDAX:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "ldax %s\n", pairname(p));
			getpair(p, &vh, &vl);
			regs[A] = memory[MAKEWORD(vh, vl)];
			return;
		case I_STAX:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "stax %s\n", pairname(p));
			getpair(p, &vh, &vl);
			memory[MAKEWORD(vh, vl)] = regs[A];
			return;
		case I_INX:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "inx %s\n", pairname(p));
			getpair(p, &vh, &vl);
			hl = MAKEWORD(vh, vl)+1;
			setpair((i & 060) >> 4, GETH(hl), GETL(hl));
			return;
		case I_DCX:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "dcx %s\n", pairname(p));
			getpair(p, &vh, &vl);
			hl = MAKEWORD(vh, vl)-1;
			setpair((i & 060) >> 4, GETH(hl), GETL(hl));
			return;
		case I_DAD:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "dad %s\n", pairname(p));
			getpair(p, &vh, &vl);
			a = MAKEWORD(vh, vl);
			b = MAKEWORD(regs[H], regs[L]);
			hl = a+b;
			regs[H] = GETH(hl);
			regs[L] = GETL(hl);
			set_cy((short)((a+b)>65535));
			return;
		case I_PUSH:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "push %s\n", pairname_pushpop(p));
			getpair_push(p, &vh, &vl);
			sp--; memory[sp] = vh;
			sp--; memory[sp] = vl;
			return;
		case I_POP:
			p = (i & 060) >> 4;
			if (dbg) fprintf(stderr, "pop %s\n", pairname_pushpop(p));
			vl = memory[sp]; sp++;
			vh = memory[sp]; sp++;
			setpair_pop(p, vh, vl);
			return;
	}
	if ((i & I_MVIM) == I_MVI) {
		r1 = (i & 070) >> 3;
		v = memory[pc]; pc++;
		if (dbg) fprintf(stderr, "mvi %s,%02x\n", regname(r1), v);
		setreg(r1, v);
		return;
	}
	switch (i & I_SSSDDD) {
		case I_MOV:
			r1 = i & 007;
			r2 = (i & 070) >> 3;
			if (dbg) fprintf(stderr, "mov %s,%s\n", regname(r2), regname(r1));
			v = getreg(r1);
			setreg(r2, v);
			return;
	}
	switch (i & I_SSS) {
		case I_ADD:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "add %s\n", regname(r1));
			regs[A] = add(regs[A], getreg(r1), 0, 1);
			return;
		case I_ADC:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "adc %s\n", regname(r1));
			regs[A] = add(regs[A], getreg(r1), PSW_CY, 1);
			return;
		case I_SUB:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "sub %s\n", regname(r1));
			regs[A] = sub(regs[A], getreg(r1), 0, 1);
			return;
		case I_SBB:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "sbb %s\n", regname(r1));
			regs[A] = sub(regs[A], getreg(r1), PSW_CY, 1);
			return;
		case I_ANA:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "ana %s\n", regname(r1));
			regs[A] = regs[A] & getreg(r1);
			set_psw(regs[A], 1, 0);
			return;
		case I_XRA:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "xra %s\n", regname(r1));
			a = regs[A];
			b = getreg(r1);
			regs[A] = xor(a,b);
			set_psw(regs[A], 0, 0);
			return;
		case I_ORA:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "ora %s\n", regname(r1));
			regs[A] = regs[A] | getreg(r1);
			set_psw(regs[A], 1, 0);
			return;
		case I_CMP:
			r1 = i & 007;
			if (dbg) fprintf(stderr, "cmp %s\n", regname(r1));
			sub(regs[A], getreg(r1), 0, 1);
			return;
	}
	switch (i & I_DDD) {
		case I_INR:
			r1 = (i & 070) >> 3;
			if (dbg) fprintf(stderr, "inr %s\n", regname(r1));
			setreg(r1, add(getreg(r1), (byte_t) 1, 0, 0));
			return;
		case I_DCR:
			r1 = (i & 070) >> 3;
			if (dbg) fprintf(stderr, "dcr %s\n", regname(r1));
			setreg(r1, sub(getreg(r1), (byte_t) 1, 0, 0));
			return;
	}
	switch (i & I_CCC) {
		case I_JCOND:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			c = (i & 0070) >> 3;
			if (getccc(c)) pc = MAKEWORD(vh, vl);
			if (dbg) fprintf(stderr, "j%s %02x%02x\n", condname(c), vh, vl);
			return;
		case I_CCOND:
			vl = memory[pc]; pc++;
			vh = memory[pc]; pc++;
			c = (i & 0070) >> 3;
			if (dbg) fprintf(stderr, "c%s %02x%02x\n", condname(c), vh, vl);
			if (getccc(c)) {
				sp--; memory[sp] = GETH(pc);
				sp--; memory[sp] = GETL(pc);
				pc = MAKEWORD(vh, vl);
			}
			return;
		case I_RCOND:
			c = (i & 0070) >> 3;
			if (dbg) fprintf(stderr, "r%s\n", condname(c));
			if (getccc(c)) {
				pc = MAKEWORD(memory[sp+1],memory[sp]);
				sp += 2;
			}
			return;
	}
	if ((i & I_RST) == I_RST) {
		if (dbg) fprintf(stderr, "rst %d\n", (i & 0070) >> 3);
		sp--; memory[sp] = GETH(pc);
		sp--; memory[sp] = GETL(pc);
		pc = MAKEWORD(0, i & 0070);
		return;
	}

	fprintf(stderr, "8080: unknown instruction 0x%x at 0x%x\n", i, pc);
	abort();
}

/* End of file. */
