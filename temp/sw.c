#include <stdio.h>

typedef long long int i64;
typedef unsigned char byte;

enum {kloadimm, kload, kstore, kadd, kjumpne, kunload, kstop};

i64 x=0, y=1;
i64 stack[100];

byte code[] = {
    kloadimm,kloadimm,kadd,kunload,
    kload,kload,kadd,kstore,
    kload,kloadimm,kjumpne,
    kstop
};

i64 data[] = {
    42,56,0,0,
    (i64)&x,(i64)&y,0,(i64)&x,
    (i64)&x,100000000,0,0
};

void run(void) {
    i64* a;
    i64* sp;
    i64 pc;

    pc=1;
    sp=stack;

    while (1) {
        switch (code[pc]) {
        case kloadimm:
            ++sp;
            *sp=data[pc];
            ++pc;
            break;

        case kload:
            a=(i64*)data[pc];
            ++sp;
            *sp=*a;
            ++pc;
            break;

        case kstore:
            a=(i64*)data[pc];
            *a=*sp;
            --sp;
            ++pc;
            break;

        case kadd:
            --sp;
            *sp += *(sp+1);
            ++pc;
            break;

        case kjumpne:
            sp=sp-2;
            if (*(sp+1)!=*(sp+2)) {
                pc=data[pc];
            } else {
                ++pc;
            }
            break;

        case kunload:
            --sp;
            ++pc;
            break;

        case kstop:
            printf("Stop: x=%lld y=%lld\n", x, y);
            return;
        }
    }
}

int main(void) {
    run();
}
