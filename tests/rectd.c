#include <stdio.h>

typedef struct {
    float x,y, width, height;
} Rectangle;

typedef struct {
    float x,y, width, height, a,b;
} Rectangle2;

typedef struct {
    float x,y;
} Point;

void PrintRect(Rectangle r) {
    printf("%f %f %f %f\n",r.x,r.y,r.width,r.height);
}

void PrintRect2(Rectangle2 r) {
    printf("%f %f %f %f %f %f\n",r.x,r.y,r.width,r.height,r.a,r.b);
}

void PrintPoint(Point pt) {
    printf("%f %f\n",pt.x,pt.y);
}


Rectangle NewRect(void) {
    static Rectangle r = {11,21,31,41};
     return r;
}

Rectangle2 NewRect2(void) {
    static Rectangle2 r = {11,21,31,41,51,61};
    return r;
}

Point NewPoint(void) {
   static Point pt = {101,201};
   return pt;
}

int DllMain(void* hinst, int reason, int reserved) {
  return 1;
}
