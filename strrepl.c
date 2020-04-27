#include <string.h>
#include <stdlib.h>

 
int strrepl( char *in,  char *pat,  char *rep, char *out)
{
     int patlen = strlen(pat), replen = strlen(rep), len = 0;
      char *src = in, *next;
     while (next = strstr(src, pat)) {
          if (out) memcpy(out + len, src, next - src);
          len += next - src;
          if (out) memcpy(out + len, rep, replen);
          len += replen;
          src = next + patlen;
     }
     int rest = strlen(src);
     if (out) memcpy(out + len, src, rest + 1);
     return len + rest;
}


#include <stdio.h>
int main(int argc,char *argv[])
{
	char *buffer,*tmp;
	size_t siz,siz1;
	FILE *f;

	if (argc < 5)  { 
		printf("Usage: %s: <file name> <str-to-find> <replacement> <output-file>",
			argv[0]); 
		exit(1); 
	}
	f = fopen(argv[1],"rb");
	if (f == NULL) { printf("Impossible to open %s\n",argv[1]);exit(2);}
	if (fseek(f,0,SEEK_END)) {printf("Impossible to seek to end\n");exit(3);}
	siz = ftell(f);
	if ((int)siz < 0) {printf("Impossible to query position\n"); exit(4);}
	buffer = malloc(siz+1);
	if (buffer == NULL) {printf("No more memory\n"); exit(6);}
	fseek(f,0,SEEK_SET);
	if (siz != fread(buffer,1,siz,f)) { printf("ReadError\n"); exit(5);}
	fclose(f);
	buffer[siz]=0;

//    siz = strrepl(buffer,argv[2],argv[3],NULL);
//    tmp = malloc(siz+100);
    for (int i=0; i<200;i++) {
        siz = strrepl(buffer,argv[2],argv[3],NULL);
        tmp = malloc(siz+100);
        siz1 = strrepl(buffer,argv[2],argv[3],tmp);
        if (i<99) free(tmp);
    }
    f = fopen(argv[4],"wb");
    fwrite(tmp,1,siz-1,f);
    fclose(f);
	return 0;
}
