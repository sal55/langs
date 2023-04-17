#include <stdio.h>
#include <stdlib.h>

typedef long long int i64;
typedef void(*funcptr)(void);

i64 stack[100];
funcptr* pcptr;
i64* dataptr;
i64* sptr=stack;

i64 x=0;
i64 y=1;

void loadimm(void);
void load(void);
void store(void);
void unload(void);
void add(void);
void jumpne(void);
void stop(void);

funcptr code[] = {
    loadimm,
    loadimm,
    add,
    unload,

    load,
    load,
    add,
    store,

    load,
    loadimm,
    jumpne,
    stop};

void* data[] = {
    (void*)42,
    (void*)56,
    0,
    0,

    &x,
    &y,
    add,
    &x,

    &x,
    (void*)100000000,
    &code,
    stop};

void loadimm(void) {
    ++sptr; *sptr=(i64)*dataptr;
    ++pcptr; ++dataptr;
}

void load(void) {
    i64* a;

    a=(i64*)*dataptr;
    ++sptr;
    *sptr=*a;

    ++pcptr; ++dataptr;
}

void store(void) {
    i64* a;

    a=(i64*)*dataptr;
    *a=*sptr;
    --sptr;

    ++pcptr; ++dataptr;
}

void unload(void) {
    --sptr;

    ++pcptr; ++dataptr;
}

void add(void) {
    --sptr;
    *sptr += *(sptr+1);

    ++pcptr; ++dataptr;
}

void jumpne(void) {
    sptr-=2;
    if (*(sptr+1)!=*(sptr+2)) {
        pcptr=(funcptr*)*dataptr;
        dataptr = (i64*)(data + (pcptr-code));
    }
    else {
        ++pcptr; ++dataptr;
    }
}

void stop(void) {
    printf("Stop x=%lld y=%lld\n",x,y);
    exit(0);
}

void run(void) {
    pcptr=code;
    dataptr=(i64*)data;
    sptr=stack;

    while(1) {
        (*pcptr)();
    }
}

int main(void) {
    run();
}
