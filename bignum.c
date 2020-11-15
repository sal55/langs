// Simple arbitrary-precision integer and float library
// Implementation code
// Docs: <not available>

// This source code is placed in the public domain.
// You can do with it as you please.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bignum.h" // try same url as this .c file, replace ".c" with ".h"

enum {digitwidth      = 9};

//enum {digitbase       = 1000000000};
extern int digitbase;

static char* digitfmt = "%09d";

//enum {digitmax     = digitbase-1};
enum {digitmax     = 999999999};

typedef int elemtype;
enum {elemsize = sizeof(elemtype)};

typedef struct _bignumrec {
	int* num;
	int length;
	int expon;
	int neg;
	int numtype;
} bignumrec;

typedef struct _constrec {
	long long int value;
	Bignum bnvalue;
	struct _constrec* nextconst;
} constrec;

// values for bignum types
enum {
	zero_type=0,
	normal_type,
	inf_type,
	nan_type
};

// recognised combinations of bignum types (bintypes)
enum {
	nn_types,         // both numbers (non-zero)
	zz_types,         // both zero
	ii_types,         // both infinity
	xx_types,         // one or both is nan

	nz_types,         // number/zero
	ni_types,         // number/infinity

	zn_types,         // zero/number
	in_types,         // infinity/number

	zi_types,         // zero/infinity
	iz_types         // infinity/zero
};

enum {maxprec       = 1000000};         // used by bn_setprec
static int currprec = 100/digitwidth;   // 99 sig figures when digitbase is 1e9

static int stblz;                       //global set by smalltobig

static constrec* constlist = NULL;      // linked list of constant values

static int       readexpon(char* s);
static int       getbintype(Bignum a, Bignum b);
static Bignum    makebignum(int length);
static elemtype* makesmallnum(int length);
static Bignum    smalltobig(Bignum c, elemtype* a, int length, int alloc, int offset);
static void      freesmall(elemtype* p, int length);
static void      freemem(void* p, int size);
static void      bn_error(char* mess);
static int       smallsubto(elemtype* p, elemtype* q, int plen, int qlen);
static int       smallmulto(elemtype* p, elemtype* q, int plen, int m);
static void      bn_addu(Bignum dest, Bignum a, Bignum b);
static void      bn_subu(Bignum dest, Bignum a, Bignum b);
static void      bn_mulu(Bignum dest, Bignum a, Bignum b);
static elemtype  smalldiv(elemtype* x, elemtype* b, int* xlen, int nb);
static int       bn_idivu(Bignum dest, Bignum a, Bignum b, Bignum rm);
static long long strvaln(char* s,int n);
static void      bn_fdivu(Bignum dest, Bignum a, Bignum b,int precision);
static char*     tostring_float(Bignum a,int fmt);
static char*     tostring_scient(Bignum a);
static Bignum    badnumber(void);
static void*     bn_alloc(long long int);

Bignum bn_init(void) {
	Bignum a;

	a=makebignum(0);
	return a;
}

Bignum bn_makestr(char* s) {
	char *t,*u;
	int length,neg,dpindex,expon,nonzeros,talloc,dpseen;
	int leadingzeros, trailingzeros,zerosafterdp;
	int d,n,wd,wdp,w,d2,na,nb, looping;
	Bignum a;

	length=strlen(s);

	if (length==0) {
		return badnumber();
	}
	talloc=length+1+10;			// allow for extending last wdigit group

	neg=0;
	if (*s=='+') {
		++s;
	} else if (*s=='-') {
		neg=1; ++s;
	}

	t=u=bn_alloc(talloc);		// accummulate sig digits into t
	dpindex=-1;
	dpseen=zerosafterdp=0;
	nonzeros=0;
	leadingzeros=trailingzeros=0;
	expon=0;
	looping=1;

	while (looping) {
		switch (*s) {
		case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9':
			*u++ = *s++;
			trailingzeros=0;
			nonzeros=1;
			break;

		case '0':
			if (nonzeros) {
				++trailingzeros;
				*u++ = *s++;
			} else {
				++leadingzeros;
				if (dpseen) {
					++zerosafterdp;
				}
				++s;
			}
			break;

		case '_': case '\'': case '`': case ' ': case 13: case 10:
			++s;
			break;

		case '.':
			if (dpseen || dpindex>=0) {
				return badnumber();
			}
			if (nonzeros) {
				dpindex=u-t;
			} else {
				dpseen=1;
			}
			++s;
			break;

		case 0:
			looping=0;
			break;

		case 'e': case 'E':
			expon=readexpon(s+1);
			looping=0;
			break;

		default:
			return badnumber();
		}
	}

	*u=0;
	length=u-t;					// new length of extracted digits

	if (dpindex<0) {
		if (dpseen) {
			dpindex=-zerosafterdp;
		} else {
			dpindex=length;
		}
	}
	length-=trailingzeros;		// adjust precision to ignore trailing zeros
	*(t+length)=0;

	if (length==0) {
		return bn_makeint(0);
	}

	d=dpindex-1+expon;
	n=length;
	na=1;
	nb=n-na;

	w=digitwidth;

	if (d>=0) {
		wd=d/w;
		wdp=d % w;
	} else {
		d2=abs(d+1);
		wd=-(d2/w+1);
		wdp=w-1-(d2 % w);
	}

	na=wdp+1;
	nb = (n-na>=0?n-na:0);

	while (nb % w) {
		++nb;
	}
	length=nb/w+1;
	u=t+n;
	for (int i=0; i<na+nb-n; ++i) {
		*u++='0';
	}
	n=na+nb;
	*(t+n)=0;

	a=makebignum(length);
	a->neg=neg;
	a->expon=wd;
	u=t;
	a->num[0]=strvaln(u,na);
	u+=na;
	
	for (int i=1; i<length; ++i) {
		a->num[i]=strvaln(u,w);
		u+=w;
	}

	freemem(t,talloc);

	return a;
}

Bignum bn_makeint (long long x) {
	Bignum a;
	char str[256];

	if (x==0) {
		a=makebignum(0);
	} else if (x>=0 && x<=digitmax) {
		a=makebignum(1);
		a->num[0]=x;
	} else if (-x>=1 && -x<=digitmax) {
		a=makebignum(1);
		a->num[0]=-x;
		a->neg=1;
	} else {
		sprintf(str,"%lld",x);
		a=bn_makestr(str);
	}

	return a;
}

Bignum bn_makefloat (double x) {
	char str[2048];

	sprintf(str,"%.30g",x);
	return bn_makestr(str);
}

int bn_add (Bignum c, Bignum a, Bignum b) {
	int nega,negb;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zz_types:
		bn_setzero(c);
		return 1;
	case nz_types:
		bn_dupl(c,a);
		return 1;
	case zn_types:
		bn_dupl(c,b);
		return 1;
	default:
		bn_setnan(c);
		return 0;
	}

	nega=a->neg;
	negb=b->neg;

	if (!nega && !negb) {		// both positive
		bn_addu(c,a,b);
	} else if (nega && negb) {			// both negative
		bn_addu(c,a,b);
		bn_negto(c);
	} else if (!nega && negb) {		// a positive, b negative
		bn_subu(c,a,b);
	} else {
		bn_subu(c,b,a);				// a negative, b positive;
	}

	return 1;
}

int bn_sub (Bignum c, Bignum a, Bignum b) {
	int nega,negb;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zz_types:
		bn_setzero(c);
		return 1;
	case nz_types:
		bn_dupl(c,a);
		return 1;
	case zn_types:
		bn_dupl(c,b);
		bn_negto(c);
		return 1;
	default:
		bn_setnan(c);
		return 0;
	}

	nega=a->neg;
	negb=b->neg;

	if (!nega && !negb) {
		bn_subu(c,a,b);
	} else if (nega && negb) {
		bn_subu(c,b,a);
	} else if (!nega && negb) {
		bn_addu(c,a,b);
	} else {
		bn_addu(c,a,b);
		bn_negto(c);
	}

	return 1;
}

int bn_mul (Bignum c, Bignum a, Bignum b) {
	int neg;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zz_types: case nz_types: case zn_types:
		bn_setzero(c);
		return 1;
	default:
		bn_setnan(c);
		return 0;
	}

	neg=a->neg != b->neg;
	bn_mulu(c,a,b);
	if (neg) {
		bn_negto(c);
	}
	return 1;
}

int bn_mulp(Bignum c, Bignum a, Bignum b, int prec) {
	int res=bn_mul(c,a,b);
	if (res) {
		bn_setprec(c,(prec==0?currprec:prec));
	}
	return res;
}

int bn_div (Bignum c, Bignum a, Bignum b, int prec) {
	int neg;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zn_types:
		bn_setzero(c);
		return 1;
	case zz_types: case nz_types:
		bn_setinf(c);
		return 0;
	default:
		bn_setnan(c);
		return 0;
	}

	neg=a->neg != b->neg;
	bn_fdivu(c,a,b,prec);
	if (neg) {
		bn_negto(c);
	}
	return 1;
}

int bn_idiv(Bignum c, Bignum a, Bignum b) {
	int neg;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zn_types:
		bn_setzero(c);
		return 1;
	case zz_types: case nz_types:
		bn_setinf(c);
		return 0;
	default:
		bn_setnan(c);
		return 0;
	}

	neg=a->neg != b->neg;
	if (bn_idivu(c,a,b,NULL)) {
		if (neg) {
			bn_negto(c);
		}
		return 1;
	}
	return 0;
}

int bn_irem(Bignum c, Bignum a, Bignum b) {
	Bignum d;
	int res,neg;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zn_types:
		bn_dupl(c,b);
		return 1;
	case zz_types: case nz_types:
		bn_setinf(c);
		bn_setzero(c);
		return 0;
	default:
		bn_setnan(c);
		return 0;
	}

	neg=a->neg;
	d=bn_init();
	if (bn_idivu(d,a,b,c)) {
		if (neg) {
			bn_negto(c);
		}
		res=1;
	} else {
		bn_setnan(c);
		res=0;
	}
	bn_free(d);
	return res;
}

int bn_idivrem (Bignum c, Bignum rm, Bignum a, Bignum b) {
	int res,nega,negb;

	switch (getbintype(a,b)) {
	case nn_types:
		break;
	case zn_types:
		bn_setzero(c);
		bn_setzero(rm);
		return 1;
	case zz_types: case nz_types:
		bn_setinf(c);
		bn_setzero(rm);
		return 0;
	default:
		bn_setnan(c);
		return 0;
	}

	nega=a->neg; negb=b->neg;

	if (bn_idivu(c,a,b,rm)) {
		if (nega != negb) {
			bn_negto(c);
		}
		if (nega) {
			bn_negto(rm);
		}
		return 1;
	} else {
		bn_setnan(rm);
		return 0;
	}
}

void bn_negto (Bignum a) {
	if (!bn_iszero(a)) {
		a->neg=!a->neg;
	}
}

void bn_absto (Bignum a) {
	a->neg=0;
}

int bn_ipower(Bignum d, Bignum a, int n) {
	Bignum e,f;
	int res;

	res=1;
	if (n<0) {
		bn_setzero(d);

	} else if (n==0) {
		bn_move(d,bn_makeint(1));

	} else if (n==1) {
		bn_dupl(d,a);
 
	} else if ((n & 1)==0) {
		e=bn_init();
		bn_mul(e,a,a);
		res=bn_ipower(d,e,n/2);
		bn_free(e)		;

	} else {			// assume odd
		e=bn_init();
		f=bn_init();
		res =  bn_mul(e,a,a);
		res &= bn_ipower(f,e,(n-1)/2);
		res &= bn_mul(d,a,f);

		bn_free(e);
		bn_free(f);

	}
	return res;
}

int bn_equal (Bignum a, Bignum b) {
	if (a->length != b->length ||
	   a->numtype != b->numtype ||
	   a->neg != b->neg ||
	   a->expon != b->expon) {
		return 0;
	}

	if (a->length==0) {
		return 1;
	}

//	return eqbytes(a->num,b->num,a->length*elemsize);
	return memcmp(a->num,b->num,a->length*elemsize)==0;
}

int bn_cmp (Bignum a, Bignum b) {
	Bignum d;
	int neg;

	if (bn_equal(a,b)) {
		return 0;
	}

	d=bn_init();
	bn_sub(d,a,b);
	neg=d->neg;
	bn_free(d);
	return (neg|-1|1);
}

void bn_free(Bignum a) {
// free digit memory && descriptor
	if (a) {
		bn_setzero(a);
		freemem(a,sizeof(bignumrec));
	}
}

void bn_move(Bignum c, Bignum a) {
// move contents of a to c. Original value of c is cleared; a becomes zero

	bn_setzero(c);
	*c = *a;
	memset(a,0,sizeof(bignumrec));
}

void bn_dupl(Bignum dest, Bignum a) {
// copy contents of b to a. Each copy is independent
	Bignum c;
	int size;

// 	if (dest==a) {
		c=bn_init();
		*c = *a;
		if (c->length) {;
			c->num=makesmallnum(size=c->length);
			memcpy(c->num,a->num, size*elemsize);
		};
		bn_move(dest,c);
		bn_free(c);
// 	}

// 	bn_setzero(a)
// 	a^=b^
// 	if (a->length) {
// 		a->num=bn_alloc(a->length*elemtype.bytes)
// 	}
}

void bn_setzero(Bignum a) {
// clear digit memory only; clear descriptor to a zero number
	if (a) {
		if (a->num) {
			freesmall(a->num,a->length);
		}
		a->num=NULL;
		a->length=0;
		a->neg=0;
		a->expon=0;
		a->numtype=zero_type;
	}
}

void bn_setinf(Bignum dest) {
	bn_setzero(dest);
	dest->numtype=inf_type;
}

void bn_setnan (Bignum dest) {
	bn_setzero(dest);
	dest->numtype=nan_type;
}

Bignum bn_const (long long value) {
	constrec* p;

	p=constlist;

	while (p) {
		if (p->value==value) {
			return p->bnvalue;
		}
		p=p->nextconst;
	}

// not encountered before
	p=bn_alloc(sizeof(constrec));
	p->bnvalue=bn_makeint(value);
	p->value=value;
	p->nextconst=constlist;
	constlist=p;
	return p->bnvalue;
}

int bn_iszero(Bignum a) {
	return a->numtype == zero_type;
}

int bn_isint (Bignum a) {
	return a->length <= a->expon+1;
}

int bn_isinf (Bignum a) {
	return a->numtype == inf_type;
}

int bn_isnan (Bignum a) {
	return a->numtype == nan_type;
}

int bn_getprec (Bignum a) {
	return a->length*digitwidth;
}

int bn_getglobalprec (void) {
	return currprec*digitwidth;
}

void bn_setprec (Bignum a, int prec) {
	int oldlength,newlength;
	Bignum c;

	if (a->numtype!=normal_type) {
		return;
	}

	if (prec<1 || prec>maxprec) {
		return;
	}

	prec=((prec-1)/digitwidth+1)*digitwidth;
	newlength=prec/digitwidth;

//	newlength=((prec-1)/digitwidth+1);
	oldlength=a->length;

	if (oldlength==newlength) {
		return;
	}

	c=makebignum(newlength);
	c->neg=a->neg;
	c->expon=a->expon;

	for (int i=0; i<newlength; ++i) {
		if (i<oldlength) {
			c->num[i]=a->num[i];
		} else {
			c->num[i]=0;
		}
	}

	bn_move(a,c);
	bn_free(c);
}

void bn_setglobalprec (int prec) {
	currprec=((prec-1)/digitwidth+1);
}

int bn_sign(Bignum a) {
	if (bn_iszero(a)) {
		return 0;
	} else if (a->neg) {
		return -1;
	} else {
		return 0;
	}
}

int bn_digits(Bignum a) {
// return number of digits in integer a
	int n;
	char str[32];

	if (!bn_isint(a)) {
		return 0;
	}
	if (bn_iszero(a)) {
		return 1;
	}

	n=sprintf(str,"%d",a->num[0]);
	return n+a->expon*digitwidth;
}

void bn_fix (Bignum c, Bignum a) {
	if (bn_iszero(a) || a->expon<0) {
		bn_setzero(c);
		return;
	}

	bn_dupl(c,a);
	if (!bn_isint(c)) {
		bn_setprec(c,c->expon+1);
	}
}

long long int bn_toint (Bignum a) {
	long long int x;
	if (!bn_isint(a)) {
		return 0;
	}
	if (bn_iszero(a)) {
		return 0;
	}

	x=0;
	for (int i=0; i<a->length; ++i) {
		x=x*digitbase+a->num[i];
	}

	if (a->neg) {
		return -x;
	} else {
		return x;
	}
}

double bn_tofloat (Bignum a) {
	double x;
	char* s;

	if (bn_iszero(a)) {
		return 0.0;
	}

	s=bn_tostring(a,'E');

	sscanf(s,"%lf", &x);
	return x;
}

char* bn_tostring(Bignum a, int fmt) {
	char *s,*t;

	t=NULL;
	if (a==NULL) {
		t="<void>";
	} else {
		switch (a->numtype) {
		case zero_type:
			 t=(fmt=='E' || fmt=='F'?"0.0":"0");
			break;
		case inf_type:
			t="<inf>";
			break;
		case nan_type:
			t="<nan>";
		}
	}

	if (t) {
		s=bn_alloc(strlen(t)+1);
		strcpy(s,t);
		return s;
	}

	if (fmt==0 || fmt=='A') {
		if (bn_isint(a) && (a->expon-a->length)*digitwidth<60) {
			fmt='I';
		} else if (abs(a->expon*digitwidth)<60) {
			fmt='F';
		} else {
			fmt='E';
		}
	}

	if (fmt=='E') {
		s=tostring_scient(a);
	} else {
		s=tostring_float(a,fmt);
	}

	return s;
}

void bn_print (Bignum a) {
	char* s;

	s=bn_tostring(a,0);
	printf("%s", s);
	free(s);
}

void bn_println (Bignum a) {
	bn_print(a);
	puts("");
}

static int readexpon(char* s) {
// s points just after 'e' or 'E'
	int neg, expon, looping;
	neg=expon=0;

	switch (*s) {
	case '+':
		++s;
		break;
	case '-':
		neg=1;
		++s;
		break;
	}

	looping=1;
	while (looping) {
		switch (*s) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			expon=expon*10+(*s-'0');
			++s;
			break;

		case '_': case '\'': case '`': case ' ':
			++s;
			break;

		case 0:
			looping=0;
			break;

		default:
			bn_error("make expon?");
		}
	}

	return (neg?-expon:expon);
}

static int getbintype(Bignum a, Bignum b) {
// return bintype code for combination of a && b
	int atype=a->numtype, btype=b->numtype;

	if (atype==nan_type || btype==nan_type) {
		return xx_types;
	}

	switch (atype) {
	case normal_type:
		switch (btype) {
		case normal_type:
			return nn_types;
		case zero_type:
			return nz_types;
		default:
			return ni_types;
		}
	case zero_type:
		switch (btype) {
		case normal_type:
			return zn_types;
		case zero_type:
			return zz_types;
		default:
			return zi_types;
		}
	default:
		switch (btype) {
		case normal_type:
			return in_types;
		case zero_type:
			return iz_types;
		default:
			return ii_types;
		}
	}
}

static Bignum makebignum(int length) {
// ndigits=0 to create a zero value
// these are wide digits
	Bignum a;

	a=bn_alloc(sizeof(bignumrec));
	if (length) {
		a->num=bn_alloc(length*elemsize);
		a->numtype=normal_type;
	} else {
		a->num=NULL;
//		a->num=0;
		a->numtype=zero_type;
	}
	a->length=length;
	a->expon=0;
	a->neg=0;

	return a;
}

static elemtype* makesmallnum(int length) {
	return bn_alloc(length*elemsize);
}

static Bignum smalltobig(Bignum c, elemtype* a, int length, int alloc, int offset) {
// copy numeric data from smallnum into new Bignum
// also normalises by removing trailing zeros && leading zeros
// sets up expon with assumption that sequence represents an int
	elemtype *p;
	int leadingzeros, trailingzeros, nonzeros, newlength;

	bn_setzero(c);

	p=a;
	leadingzeros=trailingzeros=nonzeros=0;
	for (int i=0; i<length; ++i) {
		if (*p++) {
			nonzeros=1;
			trailingzeros=0;
		} else {
			if (nonzeros) {
				++trailingzeros;
			} else {
				++leadingzeros;
			}
		}
	}

	stblz=leadingzeros;

	if (nonzeros) {
		newlength=length-trailingzeros-leadingzeros;
		if (newlength==length && newlength==alloc) {
			c->num=a;
		} else {
			c->num=makesmallnum(newlength);
			memcpy(c->num,a+leadingzeros,newlength*elemsize);
			freesmall(a+offset,alloc);
		}
		c->length=newlength;
		c->numtype=normal_type;
		c->expon=length-1-leadingzeros;
	} else {
		freesmall(a+offset,alloc);
	}
	return c;
}

static void freesmall(elemtype* p, int length) {
	freemem(p,length*elemsize);
}

static void freemem(void* p, int size) {
	free(p);
}

static void bn_error(char* mess){
	printf("BIGNUM: %s\n",mess);
	exit(1);
}

static int smallsubto(elemtype* p, elemtype* q, int plen, int qlen) {
// subtract q from p, return new length. New p will be moved up if smaller
// p>=q, && plen>=qlen
	elemtype *pp,*qq;
	int carry,diff,z;

	pp=p+plen-1;
	qq=q+qlen-1;
	carry=0;
	z=0;

	for (int i=1; i<=plen; ++i) {
		if (qq>=q) {
			diff=*pp-*qq-carry;
			--qq;
		} else {
			diff=*pp-carry;
		}

		if (diff<0) {
			carry=1;;
			*pp=diff+digitbase;
		} else {
			*pp=diff;
			carry=0;
		}
		if (*pp) {
			z=0;
		} else {
			++z;
		}
		--pp;
	}
	if (carry) {
//		bn_error("SSUBTO/CARRY?");
	}

	if (z==plen) {
		--z;
	}

	if (z) {
		plen-=z;
		pp=p;
		qq=p+z;
		for (int i=1; i<=plen; ++i) {
			*pp++ = *qq++;
		}
	}

	return plen;
}

static int smallmulto(elemtype* p, elemtype* q, int plen, int m) {
// p = q*m

	elemtype *pp,*qq;
	long long int carry,d;

	switch (m) {
	case 0:
		*p=0;
		return 1;
	case 1:
		memcpy(p,q,plen*elemsize);
		return plen;
	}

	pp=p+plen-1;
	qq=q+plen-1;
	carry=0;

	for (int i=1; i<=plen; ++i) {
		d=(long long)(*qq)*m+carry;
		*pp=d % digitbase;
		carry=d/digitbase;
		--qq;
		--pp;
	}

	if (carry) {				// need extra digit
		pp=p+plen;
		for (int i=1; i<=plen; ++i) {
			*pp=*(pp-1);
			--pp;
		}
		*pp=carry;
		++plen;
	}

	return plen;
}

static void bn_addu(Bignum dest, Bignum a, Bignum b) {
	int preca, precb, precc;
	int uppera,upperb,upperc, offset, carry, dc, expona;
	unsigned j;
	elemtype *pa,*pb, *c, *c2;
	Bignum temp;

	if (a->expon < b->expon) {		// A has definite smaller magnitude
		temp=a; a=b; b=temp;
	}

	expona=a->expon;
	preca=a->length;
	precb=b->length;

	offset=expona-b->expon;			// for indexing B elements shift to match A
	uppera=preca-1;
	upperb=precb-1;

	if (uppera>(upperb+offset)) {	// A defines overall precision; B contained within A
		upperc=uppera;
	} else {							// B extends overall precision
		upperc=upperb+offset;
	}
	precc=upperc+1;

	c=makesmallnum(precc);
	carry=0;
	pa=a->num;
	pb=b->num;

	for (int i=upperc; i>=0; --i) {
		j=i-offset;						// index of A/C in terms of B
		if (i<=uppera && j<=(unsigned)upperb) {
			dc=pa[i]+pb[j]+carry;
		} else if (i<=uppera) {
			dc=pa[i]+carry;
		} else if (j<=(unsigned)upperb) {
			dc=pb[j]+carry;
		} else {
			dc=carry;
		}

		if (dc>=digitbase) {
			carry=1;;
			*(c+i)=dc-digitbase;
		} else {
			*(c+i)=dc;
			carry=0;
		}
	}

	if (carry) {
		c2=makesmallnum(precc+1);
		*c2=carry;
		memcpy(c2+1,c,precc*elemsize);
		freesmall(c,precc);
		c=c2;
		++precc;
	}

	smalltobig(dest,c,precc,precc,0);
	dest->expon=expona+carry;
}

static void bn_subu(Bignum dest, Bignum a, Bignum b) {
	int preca, precb, precc;
	int uppera,upperb,upperc, offset, carry, expona;
	int isneg, diff;
	unsigned j;
	elemtype *pa,*pb, *c;
	Bignum temp;

	isneg=0;
	if (a->expon<b->expon) {		// A has definite smaller magnitude
		temp=a; a=b; b=temp;		// make sure A is always bigger or (approx) equal;
		isneg=1;
	}

// know that a>=b, and that isneg might be true
retry:
	expona=a->expon;
	preca=a->length;
	precb=b->length;

	offset=expona-b->expon;			// for indexing B elements shift to match A;
	uppera=preca-1;
	upperb=precb-1;

	if (uppera>(upperb+offset)) {	// A defines overall precision; B contained within A
		upperc=uppera;
	} else {							// B extends overall precision
		upperc=upperb+offset;
	}
	precc=upperc+1;

	c=makesmallnum(precc);
	carry=0;
	pa=a->num;
	pb=b->num;

	for (int i=upperc; i>=0; --i) {
		j=i-offset;						// index of A/C in terms of B
		if (i<=uppera && j<=(unsigned)upperb) {
			diff=pa[i]-pb[j]-carry;
		} else if (i<=uppera) {
			diff=pa[i]-carry;
		} else if (j<=(unsigned)upperb) {
			diff=-pb[j]-carry;
		} else {
			diff=-carry;
		}

		if (diff<0) {
			carry=1;;
			*(c+i)=diff+digitbase;
		} else {
			*(c+i)=diff;
			carry=0;
		}
		
	}

	if (carry) {
		if (isneg) {			// already swapped
			bn_error("SUBU/CARRY");
		}
		temp=a; a=b; b=temp;
		isneg=1;
		freesmall(c,precc);
		goto retry;
	}

	smalltobig(dest,c,precc,precc,0);
	dest->neg=isneg;
	dest->expon=expona-stblz;
}

static void bn_mulu(Bignum dest,  Bignum a, Bignum b) {

	int uppera, upperb, expona,exponb;
	int precc;
	int ax,bx,cx;
	int cx1, nc2;
	long long p,carry,x;
	elemtype* c;
	long long pdquot;

	expona=a->expon;
	exponb=b->expon;
	uppera=a->length-1;
	upperb=b->length-1;

	precc=uppera+upperb+2;
	nc2=precc;

	c=makesmallnum(nc2);
	memset(c,0,precc*elemsize);
	cx=precc-1;

	for (bx=upperb; bx>=0; --bx) {
		carry=0;

		cx1=cx;
		for (ax=uppera; ax>=0; --ax) {
			p=(long long)((a->num[ax]))*(long long)((b->num[bx]))+carry;
			pdquot=p/digitbase;
			x=(long long)(*(c+cx1))+p % digitbase;

			if (x>digitmax) {
				carry=pdquot+x/digitbase;
				*(c+cx1--)=x % digitbase;
			} else {
				carry=pdquot;
				*(c+cx1--)=x;
			}

		}
		*(c+cx1)=carry;

		--cx;
	}

	smalltobig(dest,c,precc,nc2,0);
	dest->expon=expona+exponb+1-stblz;
}

static elemtype smalldiv(elemtype* x, elemtype* b, int *xlen, int nb) {
	int k;
	long long int xx,y;
	elemtype xi,bi;
	elemtype *e;
	int esize,ne,nx;

	nx=*xlen;
	k=0;
	e=makesmallnum(esize=(nb+1));

	while (1) {
		if (nx<nb) {
			break;
		} else if (nx>nb) {
			xx=(long long)(*x)*digitbase+(long long)(*(x+1));
			y=xx/(*b+1);
		} else {
			if (*x>=(*b+1)) {
				y=*x/(*b+1);
			} else {
				y=1;
				for (int i=0; i<nb; ++i) {
					xi=*(x+i);
					bi=*(b+i);
					if (xi<bi) {
						y=0;
						goto endwhile;
					} else if (xi>bi) {
						break;
					}
				}

			}
		}
		k+=y;
		if (y>1) {
			ne=smallmulto(e,b,nb,y);
			nx=smallsubto(x,e,nx,ne);
		} else if (y) {
			nx=smallsubto(x,b,nx,nb);
		} else {
//			BN_ERROR("smalldiv:Y=0");
		}
	}
endwhile:

	freesmall(e,esize);
	*xlen=nx;					// return modified x, and new length of x (remainder)
	return k;
}

static int bn_idivu(Bignum dest, Bignum a, Bignum b, Bignum rm) {
// integer divide
// rm not null: return remainder too

	elemtype *c,*x;
	int expona, exponb, badjust;
	int na,nb,nc,nx,nx2, cx,nupper;
	int uppera, upperb;
	int n, k, nexta;
	elemtype *pa,*pb;

	na=a->length;
	nb=b->length;
	expona=a->expon;
	exponb=b->expon;
	badjust=exponb+1-nb;

	if (na>expona+1 || nb>exponb+1) {
		bn_setnan(dest);
		return 0;
	}
	nc=expona+1;

	if (expona<exponb) {
		bn_setzero(dest);
		if ( rm) {
			bn_dupl(rm,a);
		}
		return 1;
	}

	uppera=na-1;
	upperb=nb-1;
	pa=a->num;
	pb=b->num;

	n=nb;
	x=makesmallnum(nx2=n+1);
	nx=n;
	nupper=nc-badjust;

	for (int i=0; i<=upperb; ++i) {
		if (i<=uppera) {
			*(x+i)=*(pa+i);
		} else {
			*(x+i)=0;
		}
	}

	c=makesmallnum(nc);
	cx=0;

	while (1) {
		k=smalldiv(x,pb,&nx,nb);

		*(c+cx++)=k;
		if (n>=nupper) {					// finished with A 
			break;
		}

		nexta=(n>uppera?0:*(pa+n));
		++n;
		if (nx==1 && *x==0) {
			*x=nexta;				// x is 1 digit long;
		} else {
			*(x+nx)=nexta;			// next digit from a;
			++nx;
		}
	}

	if (rm && exponb<nb) {		// no trailing zeros in b
		smalltobig(rm,x,nx,nx2,0);
	} else {
		freesmall(x,nx2);
	}

	if (cx==1 && *c==0) {
		freesmall(c,nc);
		bn_setzero(dest);
		if (rm) {
			bn_dupl(rm,a);
		}
		return 1;
	}

	if (*c==0 && cx>=2) {				// leading
		smalltobig(dest,c+1,cx-1,nc,-1);
	} else {
		smalltobig(dest,c,cx,nc,0);
	}

	if (rm && exponb>=nb) {			// has trailing zeros so natural % doesn't work
		Bignum d;
		d=bn_init();
		bn_mulu(d,b,dest);
		bn_subu(rm,a,d);
		bn_free(d);
	}
	return 1;
}

static long long int strvaln(char* s,int n) {
// convert first n chars of s to int value
	int a;

	a=0;
	while (n--) {
		a=a*10+*s-'0';
		++s;
	}
	return a;
}

static void bn_fdivu(Bignum dest, Bignum a, Bignum b,int precision) {
// floating point divide

	elemtype *c,*x;
	int expona, exponb;
	int na,nb,nc,nx,nx2, cx,nc2;
	int uppera, upperb;
	int n, k, nexta;
	elemtype *pa,*pb;

	na=a->length;
	nb=b->length;
	expona=a->expon;
	exponb=b->expon;

	if (precision) {
		precision=((precision-1)/digitwidth+1);
	} else {
		precision=currprec;
	}
	nc=precision;

	uppera=na-1;
	upperb=nb-1;
	pa=a->num;
	pb=b->num;

	n=nb;
	x=makesmallnum(nx2=n+1);
	nx=n;

	for (int i=0; i<=upperb; ++i) {
		if (i<=uppera) {
			*(x+i)=*(pa+i);
		} else {
			*(x+i)=0;
		}
	}

	c=makesmallnum(nc2=nc+1);
	cx=0;

	while (1) {
		k=smalldiv(x,pb,&nx,nb);

		*(c+cx++)=k;

		if (cx>nc) {				// reached given precision
			break;
		}

		nexta=(n>uppera?0:*(pa+n));
		++n;
		if (nx==1 && *x==0) {
			*x=nexta;
		} else {
			*(x+nx)=nexta;
			++nx;
		}
	}

	freesmall(x,nx2);

	if (cx==1 && *c==0) {
		freesmall(c,nc2);
		bn_setzero(dest);
		return;
	}

	if (*c==0 && cx>=2) {
		smalltobig(dest,c+1,cx-1,nc2,-1);
		dest->expon=expona-exponb-1;
	} else {
		smalltobig(dest,c,cx,nc2,0);
		dest->expon=expona-exponb;
	}
}

static char* tostring_float(Bignum a,int fmt) {
// a is an actual number (not zero, infinity etc)
	int expon,upper,nchars,w,prel,n,showdot,dot;
	char *s,*t;

	expon=a->expon;
	upper=a->length-1;
	dot=0;

	if (fmt=='I' && bn_isint(a)) {
		showdot=0;
	} else {
		showdot=1;
	}

	w=digitwidth;
	nchars=3;
	if (expon<0) {
		nchars+=abs(expon-1)*w;
	}
	nchars+=a->length*w;
	if (expon-upper>0) {
		nchars+=(expon-upper)*w;
	}
	nchars+=8;

	s=t=bn_alloc(nchars);
	
	if (a->neg) {
		*t++='-';
	}

	prel=0;
	if (expon<0) {
		prel=1;
		*t++='0';
		*t++='.';
		dot=1;
		n=abs(expon)-1;
		while (n-- > 0) {
			for (int i=1; i<=digitwidth; ++i) {
				*t++='0';
			}
		}
	}

	for (int i=0; i<=upper; ++i) {
		n=sprintf(t,(i>0 || prel?digitfmt:"%d"),a->num[i]);
		t+=n;
		if (expon==i && i<upper && showdot) {
			*t++='.';
			dot=1;
		}
	}

	n=expon-upper;
	while (n-- > 0) {
		for (int i=1; i<=digitwidth; ++i) {
			*t++='0';
		}
	}
	if (expon>=upper && showdot) {
		*t++='.'; dot=1;
		*t++='0';
	}

	if (dot) {
		while (*(t-1)=='0' && *(t-2)!='.') {
			--t;
		}
	}

	*t=0;
	return s;
}

static char* tostring_scient(Bignum a) {
	char *s,*t;
	int expon,nchars,n,shift;
	long long int x,scale;

	nchars=3;

	expon=a->expon*digitwidth;

	x=a->num[0];
	scale=1;
	shift=0;
	while (x>=10) {
		x=x/10;
		scale*=10;
		++expon;
		++shift;
	}

	nchars=a->length*digitwidth+16;		// allow for 1., && exponent

	s=t=bn_alloc(nchars);

	if (a->neg) {
		*t++ = '-';
	}

	n=sprintf(t,"%d.",(int)x);
	t+=n;

	if (shift) {
		n=sprintf(t,"%0*d", shift, (int)(a->num[0]-x*scale));
		t+=n;
	}

	for (int i=1; i<a->length; ++i) {
		n=sprintf(t,digitfmt, a->num[i]);
		t+=n;
	}

	while (*(t-1)=='0' && *(t-2)!='.') {
		--t;
	}

	n=sprintf(t,"e%d", expon);
	t+=n;
	*t=0;

	return s;
}

static Bignum badnumber(void) {
	Bignum c;
	c=makebignum(0);
	c->numtype=nan_type;
	return c;
}

static void* bn_alloc(long long int size) {
	void* p;

	p=malloc(size);
	if (p==NULL) {;
		bn_error("Out of memory");
	}

	return p;
}
