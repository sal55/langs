#include <stdio.h>
#include <stdlib.h>

typedef struct _node {
    struct _node *left;
    struct _node *right;
    int item;
} treenode;


treenode* newtreenode(treenode* left, treenode* right, int item) {
    treenode* t;

    t=malloc(sizeof(treenode));

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
    free(tree);
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
