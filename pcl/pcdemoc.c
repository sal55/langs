#include <stdio.h>
#include <stdlib.h>

typedef void* pcl;
typedef long long int Int;

extern void pcl_start(void);
extern void pcl_gen(Int, pcl);
extern void pcl_gent(Int, Int, pcl);
extern void pcl_genxy(Int, Int, Int, pcl);
extern pcl  pcl_genname(char*);
extern pcl  pcl_gennameaddr(char*);
extern pcl  pcl_genint(long long Int);
extern pcl  pcl_genstring(char*);
extern void pcl_setexported(Int);
extern void pcl_endprog(Int,Int);

extern void pcl_writepclfile(char*,Int);
extern void pcl_writeasmfile(char*,Int);
extern void pcl_writeexefile(char*,Int);

enum {kextproc=12};
enum {kextparam=13};
enum {kextvariadics=14};
enum {kendextproc=15};
enum {kprocdef=8};
enum {kprocentry=9};
enum {kendproc=10};
enum {kstop=2};
enum {kpush=21};
enum {kcallproc=44};
enum {ksetargs=196};
enum {tpi32=8};
enum {tpu64=4};

int main(void) {
    pcl_start();

    pcl_gent(kextproc, tpi32, pcl_genname("printf"));
        pcl_gent(kextparam, tpu64, NULL);
        pcl_gen(kextvariadics, NULL);
    pcl_gen(kendextproc, NULL);

    pcl_gen(kprocdef, pcl_genname("test.start"));
        pcl_setexported(1);
        pcl_gen(kprocentry, NULL);

        pcl_genxy(ksetargs, 1,0, NULL);
        pcl_gent(kpush, tpu64, pcl_genstring("Hello, World! [C/demo]\n"));
        pcl_gen(kcallproc, pcl_gennameaddr("printf"));

        pcl_gen(kpush, pcl_genint(0));
        pcl_gen(kstop, NULL);
    pcl_gen(kendproc, NULL);

    pcl_endprog(0,0);

//  pcl_writeclangfile("test.c",0);
//  pcl_writepclfile("test.pcl",0);
//  pcl_writeasmfile("test.asm",0);
    pcl_writeexefile("test.exe",0);
    system("test");
}
