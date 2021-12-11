#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>
#include <string.h>

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

/* PROCDECLS */
void * pc_alloc(int64_t n);
void pc_free(void * p,int64_t n);
void pc_init(void);
void * pc_newblock(int64_t itemsize);
void * allocmem(int64_t n);
static void abortprogram(uint8_t * mess);
int64_t pc_getac(int64_t size);

/* VARS */
uint64_t allocupper[301];
int64_t alloccode;
int64_t allocbytes;
static int64_t maxalloccode;
static byte pc_setup = (uint8_t)0u;
static byte *  pcheapstart;
static byte *  pcheapend;
static byte *  pcheapptr;
static byte sizeindextable[2049];
uint64_t *  freelist[9];

/* PROCDEFS */
void * pc_alloc(int64_t n) {
    byte *  p;
    if (!(!!((uint64_t)(pc_setup)))) {
        pc_init();
    };
    if ((n > (int64_t)2048)) {
        alloccode = pc_getac(n);
        allocbytes = (int64_t)(allocupper[(alloccode)]);
        p = (byte *)(allocmem(allocbytes));
        if (!(!!(p))) {
            abortprogram((uint8_t*)"pc_alloc failure");
        };
        return (void *)(p);
    };
    alloccode = (int64_t)(sizeindextable[(n)]);
    allocbytes = (int64_t)(allocupper[(alloccode)]);
    if (!!((p = (byte *)(freelist[(alloccode)])))) {
        freelist[(alloccode)] = (uint64_t *)((int64_t)((*freelist[(alloccode)])));
        return (void *)(p);
    };
    p = pcheapptr;
    pcheapptr += allocbytes;
    if ((pcheapptr >= pcheapend)) {
        p = (byte *)(pc_newblock(allocbytes));
        return (void *)(p);
    };
    return (void *)(p);
}

void pc_free(void * p,int64_t n) {
    int64_t acode;
    if ((n == (int64_t)0)) {
        return;
    };
    if ((n > (int64_t)2048)) {
        free(p);
        return;
    };
    if (!!(p)) {
        acode = (int64_t)(sizeindextable[(n)]);
        (*(uint64_t *)(p)) = (uint64_t)((int64_t)(freelist[(acode)]));
        freelist[(acode)] = (uint64_t *)(p);
    };
}

void pc_init(void) {
    int64_t j;
    int64_t k;
    int64_t size;
    int64_t av_1;
    int64_t i;
    alloccode = (int64_t)0;
    if (!!((uint64_t)(pc_setup))) {
        return;
    };
    pc_newblock((int64_t)0);
    L1 :;
    for (i=(int64_t)1;i<=(int64_t)2048;i+=(int64_t)1) {
L2 :;
        j = (int64_t)1;
        k = (int64_t)16;
        L5 :;
        while ((i > k)) {
            k = (k << (int64_t)1);
            ++j;
L6 :;
        }L7 :;
        ;
        sizeindextable[(i)] = (uint64_t)(j);
L3 :;
    }L4 :;
    ;
    allocupper[((int64_t)1)] = (uint64_t)((int64_t)16);
    size = (int64_t)16;
    L8 :;
    for (i=(int64_t)2;i<=(int64_t)27;i+=(int64_t)1) {
L9 :;
        size *= (int64_t)2;
        allocupper[(i)] = (uint64_t)(size);
        if ((size >= (int64_t)33554432)) {
            k = i;
            goto L11 ;
        };
L10 :;
    }L11 :;
    ;
    L12 :;
    for (i=(k + (int64_t)1);i<=(int64_t)300;i+=(int64_t)1) {
L13 :;
        size += (int64_t)33554432;
        if ((size < (int64_t)8589934592)) {
            allocupper[(i)] = (uint64_t)(size);
        } else {
            maxalloccode = (i - (int64_t)1);
            goto L15 ;
        };
L14 :;
    }L15 :;
    ;
    pc_setup = (uint64_t)((int64_t)1);
}

void * pc_newblock(int64_t itemsize) {
    static int64_t totalheapsize;
    byte *  p;
    totalheapsize += (int64_t)2097152;
    alloccode = (int64_t)0;
    p = (byte *)(allocmem((int64_t)2097152));
    if ((p == 0)) {
        abortprogram((uint8_t*)"Can't alloc pc heap");
    };
    pcheapptr = p;
    pcheapend = (p + (int64_t)2097152);
    if ((pcheapstart == 0)) {
        pcheapstart = p;
    };
    pcheapptr += itemsize;
    return (void *)((uint32_t *)(p));
}

void * allocmem(int64_t n) {
    void *  p;
    p = malloc((uint64_t)(n));
    if (!!(p)) {
        return p;
    };
    printf(PRId64 "\n",n);
    abortprogram((uint8_t*)"Alloc mem failure");
    return 0;
}

static void abortprogram(uint8_t * mess) {
    puts((int8_t *)(mess));
    puts((int8_t *)((uint8_t*)"Aborting"));
    exit((int64_t)1);
}

int64_t pc_getac(int64_t size) {
    if ((size <= (int64_t)2048)) {
        return (int64_t)(sizeindextable[(size)]);
    };
    size = ((size + (int64_t)255) >> (int64_t)8);
    if ((size <= (int64_t)2048)) {
        return ((int64_t)((uint64_t)(sizeindextable[(size)])) + (int64_t)8);
    };
    size = ((size + (int64_t)63) >> (int64_t)6);
    if ((size <= (int64_t)2048)) {
        return ((int64_t)((uint64_t)(sizeindextable[(size)])) + (int64_t)14);
    };
    size = ((((size - (int64_t)2048) + (int64_t)2047) / (int64_t)2048) + (int64_t)22);
    return size;
}

typedef struct _node {
    struct _node *left;
    struct _node *right;
    int item;
} treenode;

treenode* newtreenode(treenode* left, treenode* right, int item) {
    treenode* t;

    t=pc_alloc(sizeof(treenode));

    t->left=left;
    t->right=right;
    t->item=item;
    return t;
}

int itemcheck(treenode* tree) {
    if (tree->left==NULL)
        return tree->item;
    else
        return tree->item+itemcheck(tree->left)-itemcheck(tree->right);
}

treenode* bottomuptree(int item, int depth) {
    if (depth>0)
        return newtreenode(bottomuptree(2*item-1,depth-1),
                bottomuptree(2*item,depth-1),item);
    else
        return newtreenode(NULL,NULL,item);
}

void deletetree(treenode* tree) {
    if (tree->left) {
        deletetree(tree->left);
        deletetree(tree->right);
    }
    pc_free(tree, sizeof(*tree));
}

int main(void) {
    treenode *stretchtree, *longlivedtree, *temptree;
    int n,depth,mindepth,maxdepth,stretchdepth,check,iterations,i;

    n=19;
    mindepth=4;
    if ((mindepth+2)>n)
        maxdepth=mindepth+1;
    else
        maxdepth=n;

    stretchdepth=maxdepth+1;

    stretchtree=bottomuptree(0,stretchdepth);
    printf("Stretch tree of depth %d    check: %d\n",stretchdepth,itemcheck(stretchtree));

    deletetree(stretchtree);
    longlivedtree=bottomuptree(0,maxdepth);

    depth=mindepth;
    while (depth<=maxdepth) {
        iterations=1<<(maxdepth-depth+mindepth);
        check=0;
        for (i=1; i<=iterations; ++i) {
            temptree=bottomuptree(i,depth);
            check=check+itemcheck(temptree);
            deletetree(temptree);

            temptree=bottomuptree(-i,depth);
            check=check+itemcheck(temptree);
            deletetree(temptree);
        }
        printf("%d  Trees of depth %d check: %d\n",iterations*2,depth,check);
        depth=depth+2;
    }
    printf("%s %d %s %d\n","long lived tree of depth",maxdepth,"\tcheck:",itemcheck(longlivedtree));
}
