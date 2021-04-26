#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char byte;

enum {rows=16, cols=16};

enum {N = rows*cols};

byte plaindata[N];
byte codedata[N];

char message[]="My password is 'drowssap'";

void encodetext(char* text){
    int length=strlen(text);
    int y,c,k;

    k=0;
    memcpy(codedata, plaindata, N);
    for (int i=0; i<=length; ++i) {
        c=text[i];
        for (int j=0; j<8; ++j) {
            if (c&0x80)
                codedata[k] |= 1;
            else
                codedata[k] &= 0xFE;
            c <<= 1;
            ++k;
        }
    }
}

void writeimage(char* name, int cols, int rows, byte* data) {
    FILE *f;

    f=fopen(name,"wb");

    fprintf(f,"P5\n");
    fprintf(f,"%d %d\n",cols, rows);
    fprintf(f,"255\n");

    fwrite(data, cols*rows, 1, f);

    fclose(f);
}

int main(void) {
    for(int k=0; k<N; ++k) plaindata[k]=k;

    writeimage("plain.pgm",cols, rows, plaindata);
    encodetext(message);
    writeimage("code.pgm",cols, rows, codedata);
}
