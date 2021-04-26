#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char byte;

enum {rows=16, cols=16};
enum {N = rows*cols};

byte codedata[N];

char secretmessage[100];

void error(char* message) {
    puts(message);
    exit(1);
}

void decodetext(char* dest){
    char* s=dest;
    byte c;
    int k=0, bits;

    bits=0;
    c=0;
    for (int i=0; i<N; ++i) {
        c <<= 1;
        if (codedata[i]&1) c |=1;
        ++bits;
        if (bits==8) {
            *s=c;
            ++s;
            if (c==0) return;
            c=0;
            bits=0;
        }   
    }
}

void readimage(char* name, int cols, int rows, byte* data) {
    FILE *f;
    char buffer[256];

    f=fopen(name,"rb");

    fgets(buffer, sizeof(buffer), f);
    fgets(buffer, sizeof(buffer), f);
    fgets(buffer, sizeof(buffer), f);

    fread(data, cols*rows, 1, f);

    fclose(f);
}

int main(void) {
    char secretmessage[100];
    int i;

    readimage("code.pgm",cols, rows, codedata);
    decodetext(secretmessage);

    puts(secretmessage);

}
