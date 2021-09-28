#include <stdint.h>

typedef  int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef  uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float r32;
typedef double r64;

extern void exit(u64);
extern double sqrt(double);
extern i32 memcpy(void*, void*, u64);

void start(void);

typedef struct _i128 {u64 low, high;} i128;
typedef struct _u128 {u64 low, high;} u128;

#pragma pack(1)

int main(void) {
  	start();
}
