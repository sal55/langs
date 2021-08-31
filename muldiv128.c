typedef unsigned long long uint64;
typedef struct {
	uint64 Lo, Hi;
} uint128;

void mult64to128(uint64 u, uint64 v, uint64* h, uint64* l)
{
    uint64 u1 = (u & 0xffffffff);
    uint64 v1 = (v & 0xffffffff);
    uint64 t = (u1 * v1);
    uint64 w3 = (t & 0xffffffff);
    uint64 k = (t >> 32);
    
    u >>= 32;
    t = (u * v1) + k;
    k = (t & 0xffffffff);
    uint64 w1 = (t >> 32);
    
    v >>= 32;
    t = (u1 * v) + k;
    k = (t >> 32);
    
    *h = (u * v) + w1 + k;
    *l = (t << 32) + w3;
}

void mult128(uint128 N, uint128 M, uint128* Ans)
{
    mult64to128(N.Lo, M.Lo, &Ans->Hi, &Ans->Lo);
    Ans->Hi += (N.Hi * M.Lo) + (N.Lo * M.Hi);
}

void mult128to256(uint128 N, uint128 M, uint128* H, uint128* L)
{
    mult64to128(N.Hi, M.Hi, &H.Hi, &H.Lo);
    mult64to128(N.Lo, M.Lo, &L.Hi, &L.Lo);
    
    uint128 T;
    mult64to128(N.Hi, M.Lo, &T.Hi, &T.Lo);
    L->Hi += T.Lo;
    if(L._Hi < T.Lo)  // if L.Hi overflowed
    {
        Increment(H);
    }
    H.Lo += T.Hi;
    if(H.Lo < T.Hi)  // if H.Lo overflowed
    {
        ++Ht.Hi;
    }
    
    mult64to128(N.Lo, M.Hi, T.Hi, T.Lo);
    L.Hi += T.Lo;
    if(L.Hi < T.Lo)  // if L.Hi overflowed
    {
        Increment(H);
    }
    H.Lo += T.Hi;
    if(H.Lo < T.Hi)  // if H.Lo overflowed
    {
        ++Ht.Hi;
    }
} 

void sqr64to128(uint64 r, uint64& h, uint64 &l)
{
    uint64 r1 = (r & 0xffffffff);
    uint64 t = (r1 * r1);
    uint64 w3 = (t & 0xffffffff);
    uint64 k = (t >> 32);
    
    r >>= 32;
    uint64 m = (r * r1);
    t = m + k;
    uint64 w2 = (t & 0xffffffff);
    uint64 w1 = (t >> 32);
    
    t = m + w2;
    k = (t >> 32);
    h = (r * r) + w1 + k;
    l = (t << 32) + w3;
}

void sqr128(uint128 R, uint128& Ans)
{
    sqr64to128(R.Lo, Ans.Hi, Ans.Lo);
    Ans.Hi += (R.Hi * R.Lo) << 1;
}

void sqr128to256(uint128 R, uint128& H, uint128& L)
{
    sqr64to128(R.Hi, H.Hi, H.Lo);
    sqr64to128(R.Lo, L.Hi, L.Lo);
    
    uint128 T;
    mult64to128(R.Hi, R.Lo, T.Hi, T.Lo);
    
    H.Hi += (T.Hi >> 63);
    T.Hi = (T.Hi << 1) | (T.Lo >> 63);  // Shift Left 1 bit
    T.Lo <<= 1;
    
    L.Hi += T.Lo;
    if(L.Hi < T.Lo)  // if L.Hi overflowed
    {
        Increment(H);
    }
    
    H.Lo += T.Hi;
    if(H.Lo < T.Hi)  // if H.Lo overflowed
    {
        ++H.Hi;
    }
}

void Increment(uint128& N)
{
    N.Lo++;
    if(N.Lo == 0)
    {
        N.Hi++;
    }  
} 

void Decrement(uint128& N)
{
    uint64 T = (N.Lo - 1);
    N.Hi -= ((T ^ N.Lo) & T) >> 63;   
    N.Lo = T;
}

// Optimized Version
void Increment(uint128& N)
{
    uint64 T = (N.Lo + 1);
    N.Hi += ((N.Lo ^ T) & N.Lo) >> 63;
    N.Lo = T;
}



void bindivmod128(uint128 M, uint128 N, uint128& Q, uint128 &R)
{
    Q.Hi = Q.Lo = 0;
    size_t Shift = nlz128(N) - nlz128(M);
    shiftleft128(N, Shift, N);

    do
    {
        shiftleft128(Q, 1, Q);
        if(compare128(M, N) >= 0)
        {
            sub128(M, N, M);
            Q.Lo |= 1;
        }

        shiftright128(N, 1, N);
    }while(Shift-- != 0);

    R.Hi = M.Hi;
    R.Lo = M.Lo;
}

void divmod128by64(const uint64 u1, const uint64 u0, uint64 v, uint64& q, uint64& r)
{
    const uint64 b = 1ll << 32;
    uint64 un1, un0, vn1, vn0, q1, q0, un32, un21, un10, rhat, left, right;
    size_t s;

    s = nlz64(v);
    v <<= s;
    vn1 = v >> 32;
    vn0 = v & 0xffffffff;

    if (s > 0)
    {
        un32 = (u1 << s) | (u0 >> (64 - s));
        un10 = u0 << s;
    }
    else
    {
        un32 = u1;
        un10 = u0;
    }

    un1 = un10 >> 32;
    un0 = un10 & 0xffffffff;

    q1 = un32 / vn1;
    rhat = un32 % vn1;

    left = q1 * vn0;
    right = (rhat << 32) + un1;
again1:
    if ((q1 >= b) || (left > right))
    {
        --q1;
        rhat += vn1;
        if (rhat < b)
        {
            left -= vn0;
            right = (rhat << 32) | un1;
            goto again1;
        }
    }

    un21 = (un32 << 32) + (un1 - (q1 * v));

    q0 = un21 / vn1;
    rhat = un21 % vn1;

    left = q0 * vn0;
    right = (rhat << 32) | un0;
again2:
    if ((q0 >= b) || (left > right))
    {
        --q0;
        rhat += vn1;
        if (rhat < b)
        {
            left -= vn0;
            right = (rhat << 32) | un0;
            goto again2;
        }
    }

    r = ((un21 << 32) + (un0 - (q0 * v))) >> s;
    q = (q1 << 32) | q0;
}

static void divmod128by128(uint128 M, uint128 N, uint128& Q, uint128& R)
{
    if (N.Hi == 0)
    {
        if (M.Hi < N.Lo)
        {
            divmod128by64(M.Hi, M.Lo, N.Lo, Q.Lo, R.Lo);
            Q.Hi = 0;
            R.Hi = 0;
            return;
        }
        else
        {
            Q.Hi = M.Hi / N.Lo;
            R.Hi = M.Hi % N.Lo;
            divmod128by64(R.Hi, M.Lo, N.Lo, Q.Lo, R.Lo);
            R.Hi = 0;
            return;
        }
    }
    else
    {
        size_t n = nlz64(N.Hi);

        uint128 v1;
        shiftleft128(N, n, v1);

        uint128 u1;
        shiftright128(M, 1, u1);

        uint128 q1;
        div128by64(u1.Hi, u1.Lo, v1.Hi, q1.Lo);
        q1.Hi = 0;
        shiftright128(q1, 63 - n, q1);

        if ((q1.Hi | q1.Lo) != 0)
        {
            dec128(q1, q1);
        }

        Q.Hi = q1.Hi;
        Q.Lo = q1.Lo;
        mult128(q1, N, q1);
        sub128(M, q1, R);

        if (compare128(R, N) >= 0)
        {
            inc128(Q, Q);
            sub128(R, N, R);
        }

        return;
    }
}

int compare128(uint128 N1, uint128 N2)
{
    return    (((N1.Hi > N2.Hi) || ((N1.Hi == N2.Hi) && (N1.Lo > N2.Lo))) ? 1 : 0) 
         -    (((N1.Hi < N2.Hi) || ((N1.Hi == N2.Hi) && (N1.Lo < N2.Lo))) ? 1 : 0);
}

void shiftleft128(uint128 N, unsigned S, uint128& A)
{
    S &= 127;

    if(S != 0)
    {
        if(S > 64)
        {
            A.Hi = N.Lo << (S - 64);
            A.Lo = 0;
        }
        else if(S < 64)
        {
            A.Hi = (N.Hi << S) | (N.Lo >> (64 - S));
            A.Lo = N.Lo << S;
        }
        else
        {
            A.Hi = N.Lo;
            A.Lo = 0;
        }
    }
    else
    {
        A.Hi = N.Hi;
        A.Lo = N.Lo;
    }
}

void shiftright128(uint128 N, unsigned S, uint128& A)
{
    S &= 127;
    
    if(S != 0)
    {
        if(S > 64)
        {
            A.Hi = N.Hi >> (S - 64);
            A.Lo = 0;
        }
        else if(S < 64)
        {
            A.Lo = (N.Lo >> S) | (N.Hi << (64 - S));
            A.Hi = N.Hi >> S;
        }
        else
        {
            A.Lo = N.Hi;
            A.Hi = 0;
        }
    }
    else
    {
        A.Hi = N.Hi;
        A.Lo = N.Lo;
    }
}

void OPTshiftleft128(uint128 N, unsigned S, uint128& A)
{
    uint64 M1, M2;
    S &= 127;

    M1 = ((((S + 127) | S) & 64) >> 6) - 1llu;
    M2 = (S >> 6) - 1llu;
    S &= 63;
    A.Hi = (N.Lo << S) & (~M2);
    A.Lo = (N.Lo << S) & M2;
    A.Hi |= ((N.Hi << S) | ((N.Lo >> (64 - S)) & M1)) & M2;
}

void OPTshiftright128(uint128 N, unsigned S, uint128& A)
{
    uint64 M1, M2;
    S &= 127;

    M1 = ((((S + 127) | S) & 64) >> 6) - 1llu;
    M2 = (S >> 6) - 1llu;
    S &= 63;
    A.Lo = (N.Hi >> S) & (~M2);
    A.Hi = (N.Hi >> S) & M2;
    A.Lo |= ((N.Lo >> S) | ((N.Hi << (64 - S)) & M1)) & M2;
}

size_t popcnt128(uint128 N)
{
    return popcnt64(N.Hi) + popcnt64(N.Lo);
}

size_t popcnt64(uint64 V)
{
    V -= ((V >> 1) & 0x5555555555555555);
    V = (V & 0x3333333333333333) + ((V >> 2) & 0x3333333333333333);
    return ((V + (V >> 4) & 0xF0F0F0F0F0F0F0F) * 0x101010101010101) >> 56;
}

//https://www.codeproject.com/Tips/784635/UInt-Bit-Operations

void Add(uint128& Ans, uint128 N, uint128 M)
{
    uint64 C = (((N.Lo & M.Lo) & 1) + (N.Lo >> 1) + (M.Lo >> 1)) >> 63;
    Ans.Hi = N.Hi + M.Hi + C;
    Ans.Lo = N.Lo + M.Lo;
}

void Subtract(uint128& Ans, uint128 N, uint128 M)
{
    Ans.Lo = N.Lo - M.Lo;
    uint64 C = (((Ans.Lo & M.Lo) & 1) + (M.Lo >> 1) + (Ans.Lo >> 1)) >> 63;
    Ans.Hi = N.Hi - (M.Hi + C);
}
