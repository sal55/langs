#include <stdio.h>
#include <stdio.h>

int	res2;

static int fann1(int n) {
int	signx;
int	maxflips;
int	sum;
int	i;
int	j;
int	k;
int	q1;
int	flips;
int	qq;
int	t;
int	sx;
int	tt;
int	p[100];
int	q[100];
int	s[100];
    signx = 1;
    maxflips = 0;
    sum = 0;
    for (i=1; i<=n; ++i) {
        p[i-1] = i;
        q[i-1] = i;
        s[i-1] = i;
    }
    while (1) {
        q1 = p[1-1];
        if (q1 != 1) {
            for (i=2; i<=n; ++i) {
                q[i-1] = p[i-1];
            }
            flips = 1;
            while (1) {
                qq = q[q1-1];
                if (qq == 1) {
                    sum += signx*flips;
                    if (flips > maxflips) {
                        maxflips = flips;
                    }
                    goto L12;
                }
                q[q1-1] = q1;
                if (q1 >= 4) {
                    i = 2;
                    j = q1-1;
                    do {
                        {int	temp;
                            temp = q[i-1];
                            q[i-1] = q[j-1];
                            q[j-1] = temp;
                        }
                        ++i;
                        --j;
                    } while (!(i >= j));
                }
                q1 = qq;
                ++flips;
            }
L12:;
        }
        if (signx == 1) {
            {int	temp;
                temp = p[2-1];
                p[2-1] = p[1-1];
                p[1-1] = temp;
            }
            signx = -1;
        }
        else {
            {int	temp;
                temp = p[2-1];
                p[2-1] = p[3-1];
                p[3-1] = temp;
            }
            signx = 1;
            for (i=3; i<=n; ++i) {
                sx = s[i-1];
                if (sx != 1) {
                    s[i-1] = sx-1;
                    goto L19;
                }
                if (i == n) {
                    res2 = maxflips;
                    return sum;
                }
                s[i-1] = i;
                tt = p[1-1];
                for (j=1; j<=i; ++j) {
                    p[j-1] = p[j+1-1];
                }
                p[i+1-1] = tt;
            }
L19:;
        }
    }
    return 0;
}

int main(void) {
    int res;
    enum {n=10};
    res = fann1(n);
    printf("fannkuch(%d) = %d %d\n",n,res,res2);
}
