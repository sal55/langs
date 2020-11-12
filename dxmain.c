#include <stdio.h>

extern char* decodeinstr(char** cptr, void* baseaddr);

static void testfn(void) {
    int a,b,c;
    a = b + c;
}

int main(void) {
    char* codeptr;
    char* s;

    codeptr = (char*)testfn;

    for (int i=0; i<10; ++i) {
        s = decodeinstr(&codeptr, NULL);
        if (s == NULL) break;
        puts(s);
    }
}

