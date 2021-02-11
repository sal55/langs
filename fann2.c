#include <stdio.h>
#include <stdio.h>

int	res2;

int fyjref(int n) {
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

int fgqbjy(int n) {
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

int fmikaf(int n) {
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

int fengeu(int n) {
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

int flhhdo(int n) {
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

int frhunz(int n) {
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

int fnitaw(int n) {
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

int fakxpk(int n) {
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

int fdaguo(int n) {
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

int firota(int n) {
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

int fktexl(int n) {
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

int fkiaec(int n) {
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

int fwuozk(int n) {
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

int fflhbg(int n) {
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

int fzacum(int n) {
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

int fniydb(int n) {
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

int fcucea(int n) {
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

int fnkgal(int n) {
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

int fvnqzz(int n) {
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

int fojkni(int n) {
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

int fztine(int n) {
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

int fwmqko(int n) {
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

int fwbnbe(int n) {
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

int fryrun(int n) {
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

int frarmx(int n) {
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

int flglyl(int n) {
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

int fxjlev(int n) {
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

int fuvpsb(int n) {
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

int frkdym(int n) {
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

int fmwxxl(int n) {
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

int fjayer(int n) {
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

int fbcwaw(int n) {
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

int fzhafs(int n) {
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

int fgrvdv(int n) {
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

int fmuvsk(int n) {
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

int fwgjjx(int n) {
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

int fvwpqc(int n) {
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

int fusdpx(int n) {
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

int fwmpbl(int n) {
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

int faoupj(int n) {
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

int fxkpcm(int n) {
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

int fwdecc(int n) {
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

int ffxjim(int n) {
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

int fhsfat(int n) {
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

int fzmrky(int n) {
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

int ferwlq(int n) {
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

int frrjfx(int n) {
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

int fmsdhq(int n) {
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

int faavfy(int n) {
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

int fxvlen(int n) {
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

int flwcoa(int n) {
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

int fxcaiw(int n) {
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

int fpgucp(int n) {
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

int fmapho(int n) {
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

int fstorc(int n) {
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

int fsvkku(int n) {
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

int fcefbq(int n) {
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

int fgylgf(int n) {
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

int fycjnl(int n) {
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

int fhydvw(int n) {
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

int fkbubh(int n) {
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

int fymrro(int n) {
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

int fxemfd(int n) {
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

int fgbftv(int n) {
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

int fiycpj(int n) {
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

int fwfnar(int n) {
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

int fqesel(int n) {
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

int favhzz(int n) {
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

int fgptfw(int n) {
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

int fmrmlg(int n) {
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

int fvgyro(int n) {
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

int fsthjf(int n) {
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

int fqfgfz(int n) {
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

int fnrfdx(int n) {
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

int fcyehr(int n) {
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

int fxnxoa(int n) {
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

int fnhycz(int n) {
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

int fhbwaz(int n) {
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

int fcobkp(int n) {
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

int fjqsfm(int n) {
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

int fzuyyr(int n) {
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

int fcqvae(int n) {
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

int fvoney(int n) {
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

int fzhabh(int n) {
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

int fsnclb(int n) {
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

int fpzmwu(int n) {
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

int fkxwky(int n) {
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

int fvjwjs(int n) {
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

int fptlzp(int n) {
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

int fbdwty(int n) {
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

int faarab(int n) {
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

int fntkag(int n) {
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

int fawwbk(int n) {
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

int fbvlwm(int n) {
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

int flqaok(int n) {
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

int fwtnaq(int n) {
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

int fdnuzh(int n) {
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

int fhjniw(int n) {
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

int fziaxu(int n) {
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

int fnekht(int n) {
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
fyjref(5);
fgqbjy(5);
fmikaf(5);
fengeu(5);
flhhdo(5);
frhunz(5);
fnitaw(5);
fakxpk(5);
fdaguo(5);
firota(5);
fktexl(5);
fkiaec(5);
fwuozk(5);
fflhbg(5);
fzacum(5);
fniydb(5);
fcucea(5);
fnkgal(5);
fvnqzz(5);
fojkni(5);
fztine(5);
fwmqko(5);
fwbnbe(5);
fryrun(5);
frarmx(5);
flglyl(5);
fxjlev(5);
fuvpsb(5);
frkdym(5);
fmwxxl(5);
fjayer(5);
fbcwaw(5);
fzhafs(5);
fgrvdv(5);
fmuvsk(5);
fwgjjx(5);
fvwpqc(5);
fusdpx(5);
fwmpbl(5);
faoupj(5);
fxkpcm(5);
fwdecc(5);
ffxjim(5);
fhsfat(5);
fzmrky(5);
ferwlq(5);
frrjfx(5);
fmsdhq(5);
faavfy(5);
fxvlen(5);
flwcoa(5);
fxcaiw(5);
fpgucp(5);
fmapho(5);
fstorc(5);
fsvkku(5);
fcefbq(5);
fgylgf(5);
fycjnl(5);
fhydvw(5);
fkbubh(5);
fymrro(5);
fxemfd(5);
fgbftv(5);
fiycpj(5);
fwfnar(5);
fqesel(5);
favhzz(5);
fgptfw(5);
fmrmlg(5);
fvgyro(5);
fsthjf(5);
fqfgfz(5);
fnrfdx(5);
fcyehr(5);
fxnxoa(5);
fnhycz(5);
fhbwaz(5);
fcobkp(5);
fjqsfm(5);
fzuyyr(5);
fcqvae(5);
fvoney(5);
fzhabh(5);
fsnclb(5);
fpzmwu(5);
fkxwky(5);
fvjwjs(5);
fptlzp(5);
fbdwty(5);
faarab(5);
fntkag(5);
fawwbk(5);
fbvlwm(5);
flqaok(5);
fwtnaq(5);
fdnuzh(5);
fhjniw(5);
fziaxu(5);
fnekht(5);
}
