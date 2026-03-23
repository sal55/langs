// Run as: 'prog' or ./prog; no command line inputs used
// Reads input from local text file 'unsorted'

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

enum {nn = 100};

char** words;
int nwords;

char** big['z'+1];
int nbig['z'+1];

char* small['z'+1][nn];

int myeof(FILE* f) {
    int c=fgetc(f);
    if (c==EOF) return 1;
    ungetc(c, f);
    return 0;
}

void readlinen(FILE* f, char* buffer, int size) {
    int n;
    char* p;

    *buffer=0;
    if (fgets(buffer, size-2, f)==NULL) return;

    n=strlen(buffer);
    if (n==0) return;

    p=buffer+n-1;
    if (*p==10) *p=0;
}

int random2(int n) {
//  int a = (rand()<<15) | rand();     // these 2 lines are for Windows
//  return a%n;
    return rand() % n;
}

void readwords() {
    char buffer[512];
    FILE* f;
    int c;

    f=fopen("unsorted", "r");
    if (f==NULL) exit(1);

//first pass counts all words and subsets
    while (!myeof(f)) {
        readlinen(f, buffer, sizeof(buffer));
        ++nwords;
        ++nbig[(unsigned char)buffer[0]];
    }

//allocate memory
    words=malloc(sizeof(char*)*nwords);
    for (int c='a'; c<='z'; ++c) {
        big[c]=malloc(sizeof(char*)*nbig[c]);
        nbig[c]=0;
    }

//second pass reads the data
    rewind(f);
    int n=0;
    while (!myeof(f)) {
        readlinen(f, buffer, sizeof(buffer));
        words[n++]=strdup(buffer);
        c=buffer[0];
//      big[c][++nbig[c]]=words[n-1];
        big[c][nbig[c]++]=words[n-1];
    }
    fclose(f);
}

void getsmall() {
    char** s;
    char** d;
    int r, ns, found;
    char* w;

    for (int c='a'; c<='z'; ++c) {
        s=big[c];
        d=small[c];
        ns=0;

        do {
            r=random2(nbig[c]);
            w=s[r];
            found=0;
            for (int i=0; i<ns; ++i) {
                if (strcmp(w, d[i])==0) {found=1; break;}
            }
            if (!found) d[ns++]=w;
        } while (ns != nn);
    }
}

void isort(char** data, int ll, int rr) {
    char* temp;
    int i = ll, j = rr;
    char* pivot = data[(ll + rr) / 2];

    do {
        while (strcmp(pivot, data[i]) > 0 && i < rr) ++i;
        while (strcmp(pivot, data[j]) < 0 && j > ll) --j;

        if (i <= j) {
            temp = data[i]; data[i] = data[j]; data[j] = temp;
            ++i;
            --j;
        }
    } while (i <= j);

    if (ll < j) isort(data, ll, j);
    if (i < rr) isort(data, i, rr);
}

void challenge1() {
    puts("Letter    Words In   Words Out");
    for (int c='a'; c<='z'; ++c) {
        printf("%c       %-8d %-8d\n", c, nbig[c], nn);
    }
    puts("");
}

void challenge2() {
    enum {maxdupl=100};
    char* dupls[maxdupl];
    unsigned char flags[maxdupl];
    int ndupls=0;
    char** words2;

    words2=malloc(sizeof(char*)*nwords);
    for (int i=0; i<nwords; ++i) words2[i]=words[i];

    isort(words2, 0, nwords-1);

    for (int i=1; i<nwords; ++i) {
        if (strcmp(words2[i-1], words2[i])==0 && ndupls<maxdupl-1) {
            dupls[ndupls]=words2[i];
            flags[ndupls++]=0;
        }
    }

    printf("found:  ");
    for (int i=0; i<nwords; ++i) {
        for (int j=0; j<ndupls; ++j) if (!flags[j]) {
            if (strcmp(words[i], dupls[j])==0) {
                printf("%s ", dupls[j]);
                flags[j]=1;
                break;
            }
        }
    }
    puts("");

    isort(dupls, 0, ndupls-1);

    printf("output: ");
    for (int i=0; i<ndupls; ++i)
        printf("%c%s ", toupper(*dupls[i]), dupls[i]+1);
    puts("");
    puts("");
} 

void challenge3() {
    enum {size=nn*26};
    char* data[size];
    int ndata=0, maxwidth=0, rows, cols, i, len;

    for (int c='a'; c<='z'; ++c) {
        for (i=0; i<nn; ++i) {
            data[ndata++]=small[c][i];
            len = strlen(small[c][i]);
            if (len > maxwidth) maxwidth = len;
        }
    }

    rows=100;
    while(1) {
        cols=size/rows+1;
        if (cols<=3) break;
        rows+=100;
    }

    for (int r=1; r<=rows; ++r) {
        i=r-1;
        printf("%5d. ", r);
        for (int c=0; c<cols; ++c) if (i<size) {
            printf("%*s", -(maxwidth+1), data[i]);
            i+=rows;
        }
        puts("");
    }
}

int main() {
    readwords();
    getsmall();

    challenge1();
    challenge2();
    challenge3();
}
