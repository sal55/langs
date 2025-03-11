#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bignum.h"

int nextpidigit(void) {
	static Bignum w,n1,n2,d,f10,n10,u,v;
	static Bignum k2minus1, k2plus1, kminus1,kplus2;
	static int count, k, k2;
	static int i = 0, dot = 0;

	if (i>0) goto resume;

	w = bn_makeint(0);
	n1 = bn_makeint(4);
	n2 = bn_makeint(3);
	d = bn_makeint(1);
	f10 = bn_makeint(10);
	n10 = bn_makeint(-10);

	u = bn_init();
	v = bn_init();

	k = 1;

//	i = 0;
	while (1) {
		bn_idiv(u,n1,d);
		bn_idiv(v,n2,d);

		if (bn_equal(u,v)) {
			++i;

			return bn_toint(u)+'0';
resume:
			if (i == 1 && dot == 0) {
				dot = 1;
				return '.';
			}

			if (i == 2000) {
				exit(0);
			}

			bn_mul(u,n10,u);
			bn_mul(u,d,u);
			bn_mul(n1,n1,f10);
			bn_add(n1,n1,u);

			bn_mul(n2,n2,f10);
			bn_add(n2,n2,u);

		} else {
			k2 = k<<1;
			k2minus1 = bn_makeint(k2-1);
			k2plus1 = bn_makeint(k2+1);
			kminus1 = bn_makeint(k-1);
			kplus2 = bn_makeint(k+2);

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
	return '?';
}

int main(void) {
	while (1) {
		printf("%c",nextpidigit());
	}

}
