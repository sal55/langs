#include <stdio.h>
#include <windows.h>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);

void imagetest();

int main(void) {

    static char szAppName[] = "winhello";
    HWND        hwnd;
    MSG         msg;
    WNDCLASSEX  wndclass;
    HDC hdc;
    BOOL status;
    int i;

    wndclass.cbSize         = sizeof(wndclass);
    wndclass.style          = 0;//CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc    = WndProc;
    wndclass.cbClsExtra     = 0;
    wndclass.cbWndExtra     = 0;
    wndclass.hInstance      = 0;
    wndclass.hIcon          = LoadIcon(NULL, IDI_APPLICATION);
    wndclass.hIconSm        = LoadIcon(NULL, IDI_APPLICATION);
    wndclass.hCursor        = LoadCursor(NULL, IDC_ARROW);
    wndclass.hbrBackground  = (HBRUSH) GetStockObject(WHITE_BRUSH);
    wndclass.lpszClassName  = szAppName;
    wndclass.lpszMenuName   = NULL;

    RegisterClassEx(&wndclass);

    hwnd = CreateWindowEx(0,szAppName, "Hello, world!",
            WS_OVERLAPPEDWINDOW|WS_VISIBLE,
            300,100,
            1000,800,
            NULL, NULL, 0, NULL);

    imagetest(hwnd);

    while ( GetMessage(&msg, NULL, 0, 0) ) {
    TranslateMessage(&msg);    /*  for certain keyboard messages  */
    DispatchMessage(&msg);     /*  send message to WndProc        */
    } 

    return msg.wParam;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam) {
    PAINTSTRUCT ps;
    HDC         hdc;
    int i;
    static int paintdone=0;
    char mess[]="Hello, World 563";
    
    switch ( iMsg ) {
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

def:
    return DefWindowProc(hwnd, iMsg, wParam, lParam);
}

void imagetest(HWND hwnd) {
    unsigned char* imgdata, *p, *q;
    void* dibdata;
    int width, height, planes, status;
    BITMAPINFO bminfo;
    HBITMAP handle;
    int IMGRowBytes, DIBRowBytes;

    char* filename="card2.jpg";

    imgdata = stbi_load(filename, &width, &height, &planes, 0);

    printf("data=%p w=%d h=%d planes=%d \n", imgdata, width, height, planes);

    memset(&bminfo, 0, sizeof(bminfo));
    bminfo.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
    bminfo.bmiHeader.biWidth=width;
    bminfo.bmiHeader.biHeight=height;
    bminfo.bmiHeader.biPlanes=1;
    bminfo.bmiHeader.biBitCount=planes*8;   // assumed to be 24 bits
    bminfo.bmiHeader.biCompression=BI_RGB;  // assumption

    handle=CreateDIBSection(NULL,&bminfo, 0, &dibdata,NULL, 0);

    printf("DIB Handle=%p\n",handle);

    DIBRowBytes = IMGRowBytes = width*planes;
    while (DIBRowBytes & 3) ++DIBRowBytes;  // DIB needs 4n bytes/row

    p=dibdata;
    q=imgdata;

    for (int y=0; y<height; ++y) {
        memcpy(p, q, IMGRowBytes);
        p += DIBRowBytes;
        q += IMGRowBytes;
    }

    HDC windc, imgdc;

    windc=GetDC(hwnd);
    imgdc=CreateCompatibleDC(NULL);
    SelectObject(imgdc, handle);

    printf("WINDC=%p\n",windc);
    printf("IMGDC=%p\n",imgdc);

    status=StretchBlt(windc,0,0, width,height, imgdc, 0,0, width, height, SRCCOPY);

    printf("Status: %d\n",status);
    printf("Error: %d\n",GetLastError());
}
