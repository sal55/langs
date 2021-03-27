/* Header stdio.h */

#ifndef $stdio
#define $stdio 1

#define __attribute__(x)

#ifndef $valist
	typedef char* va_list;
	#define $valist
#endif

#include <stddef.h>

typedef long long int fpos_t;

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define	STDIN_FILENO	0
#define	STDOUT_FILENO	1
#define	STDERR_FILENO	2

#define EOF (-1)
#define FILENAME_MAX 260

#define L_tmpnam 10

typedef struct {
	char *_ptr;
	int   _cnt;
	char *_base;
	int   _flag;
	int   _file;
	int   _charbuf;
	int   _bufsiz;
	char *_tmpfname;
	} FILE;

extern char* __iob_func(void);

#define stdin ((FILE*)(__iob_func()))
#define stdout ((FILE*)(__iob_func()+sizeof(FILE)))
#define stderr ((FILE*)(__iob_func()+sizeof(FILE)*2))

#define _IOREAD 0x0001
#define _IOWRT 0x0002

#define _IOFBF 0x0000
#define _IOLBF 0x0040
#define _IONBF 0x0004

#define _IOMYBUF 0x0008
#define _IOEOF 0x0010
#define _IOERR 0x0020
#define _IOSTRG 0x0040
#define _IORW 0x0080

#define BUFSIZ 512

FILE* fopen(const char*, const char*);
int fclose(FILE*);
long ftell(FILE*);
long long int _ftelli64(FILE*);
int fseek(FILE*,long,int);
int _fseeki64(FILE*,long long int,int);

size_t fread(void*, size_t, size_t, FILE*);
size_t fwrite(const void*, size_t, size_t, FILE*);
int remove(const char*);
int rename(const char *,const char *);
FILE* freopen(const char*, const char*, FILE*);
FILE* _wfopen(const wchar_t*,const wchar_t *);

int printf(const char*, ...);
int sprintf(char*,const char*, ...);
int fprintf(FILE*,const char*, ...);
int sscanf(const char*, const char*, ...);
int scanf(const char*, ...);
int fscanf(FILE *,const char *, ...);
int _snprintf(char *,size_t,const char*,...);
#define snprintf _snprintf
int _vsnprintf(char*, size_t, const char*, va_list);
int vsnprintf(char*,size_t,const char*,va_list);
int vsprintf(char*, const char*, va_list);
int _wremove(const wchar_t*);
int _wrename(const wchar_t*,const wchar_t*);

typedef char* va_list;

int vfprintf(FILE*, const char*, va_list);
int vprintf(const char*, va_list);

int puts(const char*);
char* fgets(char*, int, FILE*);
int fputs(const char*, FILE*);
int fgetc(FILE*);
int fputc(int, FILE*);
int ungetc(int, FILE*);
int getchar(void);
int putchar(int);
int fflush(FILE *);
int getc(FILE *);
int putc(int, FILE *);

int feof(FILE*);
int ferror(FILE*);
void clearerr(FILE*);

int fileno(FILE*);
int _fileno(FILE*);
int setvbuf(FILE*,char*,int,size_t);
FILE* _popen(const char*, const char*);
int _pclose(FILE*);
int _unlink(const char *);
#define unlink _unlink;
FILE* _fdopen(int, const char *);
#define fdopen _fdopen
int fgetpos(FILE*, fpos_t*);
int fsetpos(FILE*, const fpos_t*);
void perror(char*);
void setbuf(FILE*, char*);

void rewind(FILE*);

FILE* tmpfile(void);

char* tmpnam(char*);
wchar_t getwc(FILE *);

extern void* _wenviron;

#endif
