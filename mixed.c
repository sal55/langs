#include <stdio.h>
#include <stdint.h>

#define issigned(x) _Generic((x),\
    int8_t: "S",\
    int16_t: "S",\
    int32_t: "S",\
    int64_t: "S",\
    uint8_t: "u",\
    uint16_t: "u",\
    uint32_t: "u",\
    uint64_t: "u",\
    default: "other")

int main(void) {
    uint8_t xu8;
    uint16_t xu16;
    uint32_t xu32;
    uint64_t xu64;

    int8_t xi8;
    int16_t xi16;
    int32_t xi32;
    int64_t xi64;

    uint8_t yu8;
    uint16_t yu16;
    uint32_t yu32;
    uint64_t yu64;

    int8_t yi8;
    int16_t yi16;
    int32_t yi32;
    int64_t yi64;

    puts("       u8  u16 u32 u64  i8  i16 i32 i64\n");

    printf("   u8",issigned(xu8*yu8));
    printf("%4s",issigned(xu8*yu8));
    printf("%4s",issigned(xu8*yu16));
    printf("%4s",issigned(xu8*yu32));
    printf("%4s",issigned(xu8*yu64));
    printf(" %4s",issigned(xu8*yi8));
    printf("%4s",issigned(xu8*yi16));
    printf("%4s",issigned(xu8*yi32));
    printf("%4s\n",issigned(xu8*yi64));

    printf("  u16",issigned(xu16*yu8));
    printf("%4s",issigned(xu16*yu8));
    printf("%4s",issigned(xu16*yu16));
    printf("%4s",issigned(xu16*yu32));
    printf("%4s",issigned(xu16*yu64));
    printf(" %4s",issigned(xu16*yi8));
    printf("%4s",issigned(xu16*yi16));
    printf("%4s",issigned(xu16*yi32));
    printf("%4s\n",issigned(xu16*yi64));

    printf("  u32",issigned(xu32*yu8));
    printf("%4s",issigned(xu32*yu8));
    printf("%4s",issigned(xu32*yu16));
    printf("%4s",issigned(xu32*yu32));
    printf("%4s",issigned(xu32*yu64));
    printf(" %4s",issigned(xu32*yi8));
    printf("%4s",issigned(xu32*yi16));
    printf("%4s",issigned(xu32*yi32));
    printf("%4s\n",issigned(xu32*yi64));

    printf("  u64",issigned(xu64*yu8));
    printf("%4s",issigned(xu64*yu8));
    printf("%4s",issigned(xu64*yu16));
    printf("%4s",issigned(xu64*yu32));
    printf("%4s",issigned(xu64*yu64));
    printf(" %4s",issigned(xu64*yi8));
    printf("%4s",issigned(xu64*yi16));
    printf("%4s",issigned(xu64*yi32));
    printf("%4s\n\n",issigned(xu64*yi64));

    printf("   i8",issigned(xi8*yu8));
    printf("%4s",issigned(xi8*yu8));
    printf("%4s",issigned(xi8*yu16));
    printf("%4s",issigned(xi8*yu32));
    printf("%4s",issigned(xi8*yu64));
    printf(" %4s",issigned(xi8*yi8));
    printf("%4s",issigned(xi8*yi16));
    printf("%4s",issigned(xi8*yi32));
    printf("%4s\n",issigned(xi8*yi64));

    printf("  i16",issigned(xi16*yu8));
    printf("%4s",issigned(xi16*yu8));
    printf("%4s",issigned(xi16*yu16));
    printf("%4s",issigned(xi16*yu32));
    printf("%4s",issigned(xi16*yu64));
    printf(" %4s",issigned(xi16*yi8));
    printf("%4s",issigned(xi16*yi16));
    printf("%4s",issigned(xi16*yi32));
    printf("%4s\n",issigned(xi16*yi64));

    printf("  i32",issigned(xi32*yu8));
    printf("%4s",issigned(xi32*yu8));
    printf("%4s",issigned(xi32*yu16));
    printf("%4s",issigned(xi32*yu32));
    printf("%4s",issigned(xi32*yu64));
    printf(" %4s",issigned(xi32*yi8));
    printf("%4s",issigned(xi32*yi16));
    printf("%4s",issigned(xi32*yi32));
    printf("%4s\n",issigned(xi32*yi64));

    printf("  i64",issigned(xi64*yu8));
    printf("%4s",issigned(xi64*yu8));
    printf("%4s",issigned(xi64*yu16));
    printf("%4s",issigned(xi64*yu32));
    printf("%4s",issigned(xi64*yu64));
    printf(" %4s",issigned(xi64*yi8));
    printf("%4s",issigned(xi64*yi16));
    printf("%4s",issigned(xi64*yi32));
    printf("%4s\n",issigned(xi64*yi64));
}
