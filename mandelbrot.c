#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef int32_t int32;
typedef unsigned char byte;

//enum {imwidth = 3072};
//enum {imheight = 2048};
enum {imwidth = 6144};
enum {imheight = 4096};

static byte	image[imheight][imwidth];

static int32	mandel	(double,double,int32);
static void	create_fractal	(double,double,double,double,int32);
static void	writepgm	(char *,int32);

static int32 mandel	(double x,double y,int32 max_iters) {
double	a,b,a2;
int32	i;
    a = b = 0.0;

    for (i=0; i<max_iters; ++i) {
        a2 = (a*a-b*b)+x;
        b = a*b*2.0+y;
        a = a2;
        if (a*a+b*b >= 4.0) {
            return i;
        }
    }
    return max_iters;
}

static void create_fractal	(double min_x,double max_x,double min_y,double max_y,int32 iters) {
double	pixel_size_x, pixel_size_y;
int32	x, y;
double	i, r;

    pixel_size_x = (max_x-min_x)/imwidth;
    pixel_size_y = (max_y-min_y)/imheight;
    i = min_y;
    for (y=0; y<imheight; ++y) {
        i += pixel_size_y;
        r = min_x;
        for (x=0; x<imwidth; ++x) {
            r += pixel_size_x;
            image[y][x] = mandel(r,i,iters);
        }
    }
}

static void writepgm	(char * file,int32 maxpixel) {
int32	x;
int32	y;
void *	f;
    f = fopen(file,"w");
    fprintf(f,"%s\n","P2");
    fprintf(f,"%d %d\n",imwidth,imheight);
    fprintf(f,"%d\n",maxpixel);
    for (y=0; y<imheight; ++y) {
        for (x=0; x<imwidth; ++x) {
            fprintf(f,"%u%s",image[y][x]," ");
        }
        fprintf(f,"\n");
    }
    fclose(f);
}

int main	(void) {
static int32	maxpixel=20;
int32	x;
int32	y;
int32	sum=0;
    maxpixel = 20;

    create_fractal(-2.0,1.0,-1.0,1.0,maxpixel);

    for (y=0; y<imheight; ++y) {
        for (x=0; x<imwidth; ++x) {
            sum += image[y][x];
        }
    }

    printf("%d\n",sum);

// Uncomment next two lines to write as PPM image:
    puts("Writing test.ppm:");
    writepgm("test.ppm",maxpixel);

}
