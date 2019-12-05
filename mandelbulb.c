

/*
    Experimental Power Stack Mandelbulb for Owen
    By: Chris M. Thomasson

(Modified by Bart. C. to avoid complex.h, tgmath.h, compound literals
and runtime struct initialisers.)


*_____________________________________________________________*/


#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
//#include <complex.h>
//#include <tgmath.h>
#include <math.h>
#include <stdbool.h>

#define CT_WIDTH 256    // width of plane
#define CT_HEIGHT 256   // height of plane
#define CT_FRAMES 256   // slices of the mbulb
#define CT_ITERS 64     // iterations per pixel

typedef struct {double r,i;} mycomplex;


/* The Canvas
___________________________________*/
struct ct_rgb
{
    unsigned char r;
    unsigned char g;
    unsigned char b;
};


struct ct_canvas
{
    unsigned long width;
    unsigned long height;
    struct ct_rgb* buf;
};

bool
ct_canvas_create(
    struct ct_canvas* const self,
    unsigned long width,
    unsigned long height
){
    size_t size = width * height * sizeof(*self->buf);

    self->buf = calloc(1, size);

    if (self->buf)
    {
        self->width = width;
        self->height = height;

        return true;
    }

    return false;
}

void
ct_canvas_destroy(
    struct ct_canvas const* const self
){
    free(self->buf);
}

bool
ct_canvas_save_ppm(
    struct ct_canvas const* const self,
    char const* fname
){
    FILE* fout = fopen(fname, "wb");

    if (fout)
    {
        char const ppm_head[] =
            "P6\n"
            "# Chris M. Thomasson Simple 2d Plane ver:0.0.0.0 (pre-alpha)";

        fprintf(fout, "%s\n%lu %lu\n%u\n",
                ppm_head,
                self->width, self->height,
                255U);

        size_t size = self->width * self->height;

        for (size_t i = 0; i < size; ++i)
        {
            //unsigned int c = self->buf[i];
            struct ct_rgb* c = self->buf + i;

            fprintf(fout, "%c%c%c", c->r, c->g, c->b);
        }

        if (! fclose(fout))
        {
            return true;
        }
    }

    return false;
}



/* The Axes
___________________________________*/
struct ct_axes
{
    double xmin;
    double xmax;
    double ymin;
    double ymax;
};

struct ct_axes
ct_axes_from_point(
    mycomplex z,
    double radius
){
    struct ct_axes axes;

	axes.xmin=z.r - radius;
	axes.xmax=z.r + radius;
	axes.ymin=z.i - radius;
	axes.ymax=z.i + radius;

    return axes;
}


/* The Plane
___________________________________*/
struct ct_plane
{
    struct ct_axes axes;
    double xstep;
    double ystep;
};


void
ct_plane_init(
    struct ct_plane* const self,
    struct ct_axes const* axes,
    unsigned long width,
    unsigned long height
){
    self->axes = *axes;

    double awidth = self->axes.xmax - self->axes.xmin;
    double aheight = self->axes.ymax - self->axes.ymin;

    assert(width > 0 && height > 0 && awidth > 0.0);

    double daspect = fabs((double)height / width);
    double waspect = fabs(aheight / awidth);

    if (daspect > waspect)
    {
        double excess = aheight * (daspect / waspect - 1.0);
        self->axes.ymax += excess / 2.0;
        self->axes.ymin -= excess / 2.0;
    }

    else if (daspect < waspect)
    {
        double excess = awidth * (waspect / daspect - 1.0);
        self->axes.xmax += excess / 2.0;
        self->axes.xmin -= excess / 2.0;
    }

    self->xstep = (self->axes.xmax - self->axes.xmin) / width;
    self->ystep = (self->axes.ymax - self->axes.ymin) / height;
}



/* The Plot
___________________________________*/
struct ct_plot
{
    struct ct_plane plane;
    struct ct_canvas* canvas;
};


void
ct_plot_init(
    struct ct_plot* const self,
    struct ct_axes const* axes,
    struct ct_canvas* canvas
){
    ct_plane_init(&self->plane, axes, canvas->width - 1, canvas->height - 1);
    self->canvas = canvas;
}


bool
ct_plot_add(
    struct ct_plot* const self,
    mycomplex z,
    struct ct_rgb const* color
){
    long x = (z.r - self->plane.axes.xmin) / self->plane.xstep;
    long y = (self->plane.axes.ymax - z.i) / self->plane.ystep;

    if (x > -1 && x < (long)self->canvas->width &&
        y > -1 && y < (long)self->canvas->height)
    {
        // Now, we can convert to index.
        size_t i = x + y * self->canvas->width;

        assert(i < self->canvas->height * self->canvas->width);

        struct ct_rgb exist = self->canvas->buf[i];

        exist.r += 1;

        self->canvas->buf[i] = exist;
        return true;
    }

    return true;
}


bool
ct_plot_pixel(
    struct ct_plot* const self,
    long x,
    long y,
    struct ct_rgb const* color
){
    if (x > -1 && x < (long)self->canvas->width &&
        y > -1 && y < (long)self->canvas->height)
    {
        // Now, we can convert to index.
        size_t i = x + y * self->canvas->width;

        assert(i < self->canvas->height * self->canvas->width);

        self->canvas->buf[i] = *color;
        return true;
    }

    return false;
}


void
ct_plot_clear(
    struct ct_plot* const self,
    struct ct_rgb const* color
){
    size_t size = self->canvas->height * self->canvas->width;

    for (size_t i = 0; i < size; ++i)
    {
        self->canvas->buf[i] = *color;
    }
}


mycomplex
ct_project_to_xy(
    struct ct_plot* const self,
    long x,
    long y
){
    mycomplex p;
        p.r=self->plane.axes.xmin + x * self->plane.xstep;
        p.i=self->plane.axes.ymax - y * self->plane.ystep;

    return p;
}


bool
ct_plot_point(
    struct ct_plot* const self,
    mycomplex z,
    struct ct_rgb const* color
){
    long x = (z.r - self->plane.axes.xmin) / self->plane.xstep;
    long y = (self->plane.axes.ymax - z.i) / self->plane.ystep;

    if (x > -1 && x < (long)self->canvas->width &&
        y > -1 && y < (long)self->canvas->height)
    {
        // Now, we can convert to index.
        size_t i = x + y * self->canvas->width;

        assert(i < self->canvas->height * self->canvas->width);

        self->canvas->buf[i] = *color;
        return true;
    }

    return false;
}


// slow, so what for now... ;^)
void ct_circle(
    struct ct_plot* const plot,
    mycomplex c,
    double radius,
    unsigned int n
){
    double abase = 6.2831853071 / n;

    for (unsigned int i = 0; i < n; ++i)
    {
        double angle = abase * i;

        mycomplex z;
            z.r=c.r + cos(angle) * radius,
            z.i=c.i + sin(angle) * radius;

        struct ct_rgb rgb = { 255, 255, 255 };

        ct_plot_point(plot, z, &rgb);
    }
}



/* Simple vec3 for just what I need
___________________________________*/
struct ct_vec3
{
    double x;
    double y;
    double z;
};


double ct_vev3_length(
    struct ct_vec3 const p
){
    double sum = p.x * p.x + p.y * p.y + p.z * p.z;
    return sqrt(sum);
}


struct ct_vec3
ct_vec3_add(
    struct ct_vec3 const p,
    struct ct_vec3 const addend
){
    struct ct_vec3 sum;
        sum.x=p.x + addend.x;
        sum.y=p.y + addend.y;
        sum.z=p.z + addend.z;

    return sum;
}



/* The Power Stack Mandelbulb
___________________________________*/
void
ct_iterate_mbulb_pixel(
    struct ct_plot* const plot,
    struct ct_vec3 const z0,
    struct ct_vec3 const c0,
    double power,
    long x,
    long y,
    unsigned int n
){
    struct ct_vec3 zs = z0;
    struct ct_vec3 cs = c0;

    for (unsigned long i = 0; i < n; ++i)
    {
        double r = ct_vev3_length(zs);
        double rpower = pow(r, power);

        double angle0 = atan2(zs.z, sqrt(zs.x * zs.x + zs.y * zs.y));
        double angle1 = atan2(zs.y, zs.x);

        struct ct_vec3 znew;
        znew.x = rpower * cos(power * angle1) * cos(power * angle0);
        znew.y = rpower * sin(power * angle1) * cos(power * angle0);
        znew.z = rpower * sin(power * angle0);

        zs = ct_vec3_add(znew, cs);

        if (r > 2.0)
        {
            return;
        }
    }

    struct ct_rgb rgb = { 0, 255, 0 };
    ct_plot_pixel(plot, x, y, &rgb);
}


// Gain the field...
void
ct_iterate_mbulb_plane(
    struct ct_plot* const plot,
    double zaxis,
    double power,
    unsigned int n
){
    for (long y = 0; y < plot->canvas->height; ++y)
    {
        for (long x = 0; x < plot->canvas->width; ++x)
        {
            mycomplex cz = ct_project_to_xy(plot, x, y);

            // Well, our per slice coords...
            struct ct_vec3 z;

            z.x = cz.r;
            z.y = cz.i;
            z.z = zaxis;

            // Iterate the slice
            ct_iterate_mbulb_pixel(plot, z, z, power, x, y, n);
        }

        if (! (y % (plot->canvas->height / 10)))
        {
            printf("ct_iterate_mbulb_plane: %lu of %lu\r",
                   y + 1, plot->canvas->height);

            fflush(stdout);
        }
    }

    printf("ct_iterate_mbulb_plane: %lu of %lu\n\n",
           plot->canvas->height, plot->canvas->height);

    fflush(stdout);
}


/* The Animation Driver
___________________________________*/
void ct_anime(
    struct ct_plot* const plot,
    unsigned long frame_n,
    unsigned long n
){
    assert(frame_n > 0);

    double zmin = 0.;
    double zmax = 1.;
    double zdif = zmax - zmin;
    double zbase = zdif / (frame_n - 1.);

    double pmin = 2.;
    double pmax = 10.;
    double pdif = pmax - pmin;
    double pbase = pdif / (frame_n - 1.);

    char fname[256] = { '\0' };

    for (unsigned long frame_i = 0; frame_i < frame_n; ++frame_i)
    {
        double zaxis = zmin + zbase * frame_i;
        double power = pmin + pbase * frame_i;

        sprintf(fname, "ct_anime_%03lu.ppm", frame_i);

        printf("ct_anime: %lu of %lu, zaxis(%lf), power(%lf), (%s)\n",
           frame_i, frame_n, zaxis, power, fname);

        fflush(stdout);

        struct ct_rgb black = { 0, 0, 0 };
        ct_plot_clear(plot, &black);

        ct_iterate_mbulb_plane(plot, zaxis, power, n);

        ct_canvas_save_ppm(plot->canvas, fname);
    }

    printf("\n\nct_anime complete! %lu frames: \n", frame_n);
    fflush(stdout);
}




void ct_test_plane(
    struct ct_plot* const plot
){
    printf("Generating...\n\n");
    fflush(stdout);

    ct_anime(plot, CT_FRAMES, CT_ITERS);
}


int main(void)
{
    struct ct_canvas canvas;

    if (ct_canvas_create(&canvas, CT_WIDTH, CT_HEIGHT))
    {
        mycomplex plane_origin = {0, 0};
        double plane_radius = 1.5;

        struct ct_axes axes = ct_axes_from_point(plane_origin, plane_radius);

        struct ct_plot plot;
        ct_plot_init(&plot, &axes, &canvas);


        ct_test_plane(&plot);

        mycomplex z00  = {0,0};
        mycomplex z20  = {2,0};
        mycomplex zm20 = {-2,0};
        mycomplex z02  = {0,2};
        mycomplex z0m2 = {0,-2};

        // Our unit circle
        ct_circle(&plot, z00,  1.0, 2048);
        ct_circle(&plot, z20,  2.0, 2048);
        ct_circle(&plot, zm20, 2.0, 2048);
        ct_circle(&plot, z02,  2.0, 2048);
        ct_circle(&plot, z0m2, 2.0, 2048);

        ct_canvas_save_ppm(&canvas, "ct_plane.ppm");

        ct_canvas_destroy(&canvas);

        return EXIT_SUCCESS;
    }

    return EXIT_FAILURE;
} 
