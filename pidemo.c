// Calculate digits of of pi according to this benchmark:
//   https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/pidigits.html#pidigits
// Output is in that required format.
// Algorithm derived from the Python example on that site
// Build as follows (example for Windows when this file is pidemo.c):
//
//    gcc -O3 pidemo.c bignum.c -opidemo.exe
//
// Run as follows (example for 500 digits):
// 
//    pidemo 500
//
// Demo for my bignum library operating on integers (and floats but not used here)

#include <stdio.h>
#include <stdlib.h>

#include "bignum.h"

void pidigits(int ndigits) {
	Bignum w,n1,n2,d,f10,n10,u,v;
	Bignum k2minus1, k2plus1, kminus1,kplus2;
	int i,k,k2;
	int count;

	w=bn_makeint(0);
	n1=bn_makeint(4);
	n2=bn_makeint(3);
	d=bn_makeint(1);
	f10=bn_makeint(10);
	n10=bn_makeint(-10);

	u=bn_init();
	v=bn_init();

	k=1;

	i=0;
	while (1) {
		bn_idiv(u,n1,d);
		bn_idiv(v,n2,d);

		if (bn_equal(u,v)) {
			printf("%c",bn_toint(u)+'0');
			++i;
			if (i%10==0) {
				printf("\t:%d\n",i);
			}
			if (i==ndigits) {
				break;
			}

			bn_mul(u,n10,u);
			bn_mul(u,d,u);
			bn_mul(n1,n1,f10);
			bn_add(n1,n1,u);

			bn_mul(n2,n2,f10);
			bn_add(n2,n2,u);

		} else {
			k2=k<<1;
			k2minus1=bn_makeint(k2-1);
			k2plus1=bn_makeint(k2+1);
			kminus1=bn_makeint(k-1);
			kplus2=bn_makeint(k+2);

			bn_mul(u,n1,k2minus1);
			bn_add(v,n2,n2);

			bn_mul(w,n1,kminus1);
			bn_add(n1,u,v);

			bn_mul(u,n2,kplus2);
			bn_add(n2,w,u);

			bn_mul(d,d,k2plus1);

			++k;

			bn_free(k2minus1);
			bn_free(k2plus1);
			bn_free(kminus1);
			bn_free(kplus2);
		}
	}
	puts("");
}

int main(int nargs,char**args) {
	int n = 0;

	if (nargs>=2) {
		n=atoi(args[1]);
	}
	if (n<=0) {
		n=1000;
	}

	pidigits(n);
}
