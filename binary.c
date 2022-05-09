// binary tree benchmark

#include <stdio.h>
#include <stdlib.h>

typedef struct _node {
    union {
        struct _node *left;
        void* nextfree;
    };
    struct _node *right;
    int item;
} treenode;

typedef unsigned char byte;

enum {heapsize = 32*1024*1024};
byte heap[heapsize];
byte* heapend = &heap[heapsize];    // point to following byte
byte* heapptr = &heap[0];           // next available memory
byte* freelist = NULL;

void* mymalloc(int n) {
    byte* p;
    if (freelist && n==sizeof(treenode)) {
        p=freelist;
        freelist=((treenode*)p)->nextfree;
        return p;
    }

    p = heapptr;
    heapptr += n;
    if (heapptr >= heapend) {
        printf("Out of memory. Bytes used: %d\n", p-&heap[0]);
        exit(1);
    }
    return p;
}

void* myfree(void* p, int n) {
    if (n==sizeof(treenode)) {
        ((treenode*)p)->nextfree = freelist;
        freelist = p;
    }
}

treenode* newtreenode(treenode* left, treenode* right, int item) {
    treenode* t;

    t=mymalloc(sizeof(treenode));

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
//    free(tree);
    myfree(tree, sizeof(treenode));
}

int main(void) {
    treenode *stretchtree, *longlivedtree, *temptree;
    int n,depth,mindepth,maxdepth,stretchdepth,check,iterations,i;

    n=17;
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
