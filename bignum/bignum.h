// Simple arbitrary-precision integer and float library
// Header exporting functions and types
// <no available>

// This source code is placed in the public domain.
// You can do with it as you please.

typedef struct _bignumrec* Bignum;

// Initialisation

extern Bignum bn_init      (void);                          // Return a new bignum, set to 0.0
extern Bignum bn_makestr   (char* s);                       // Create a bignum from decimal string
extern Bignum bn_makeint   (long long a);                   // Create a bignum from int64
extern Bignum bn_makefloat (double a);                      //* Create a bignum from float64

// Basic Arithmetic

extern int    bn_add       (Bignum c, Bignum a, Bignum b);  // c = a+b, return 1/0 status
extern int    bn_sub       (Bignum c, Bignum a, Bignum b);  // c = a-b

extern int    bn_mul       (Bignum c, Bignum a, Bignum b);  // c = a*b
extern int    bn_mulp      (Bignum c, Bignum a, Bignum b,   //* c = a*b then set prec
                            int prec);

extern int    bn_div       (Bignum c, Bignum a, Bignum b,   //* c = a/b, float divide using precision
                            int prec);

extern int    bn_idiv      (Bignum c, Bignum a, Bignum b);  // c = a/b, integer divide
extern int    bn_irem      (Bignum c, Bignum a, Bignum b);  // c = a%b, integer mod
extern int    bn_idivrem   (Bignum c, Bignum r,
                            Bignum a, Bignum b);            // c = a/b, r=a%b

extern void   bn_negto     (Bignum a);                      // a = -a
extern void   bn_absto     (Bignum a);                      // a = abs(a)

extern int    bn_ipower    (Bignum c, Bignum a, int n);     // c = a**n, integer n, n>=0

// Comparison

extern int    bn_equal     (Bignum a, Bignum b);            // a == b
extern int    bn_cmp       (Bignum a, Bignum b);            // Return -1,0,-1 when a<b, a==b, a>b

// Copy, Free, Set, Constants

extern void   bn_free      (Bignum a);                      // Free all memory used by a
extern void   bn_move      (Bignum c, Bignum a);            // c = a, clear a to 0.0
extern void   bn_dupl      (Bignum c, Bignum a);            // c = a (make indep copy)

extern void   bn_setzero   (Bignum c);                      // Set c to 0.0
extern void   bn_setinf    (Bignum c);                      //* Set c to <infinity>
extern void   bn_setnan    (Bignum c);                      //* Set c to <nan>

extern Bignum bn_const     (long long n);                   // Small int64 constants as bignums

// Info

extern int    bn_iszero    (Bignum a);                      // 1 when a is 0.0
extern int    bn_isint     (Bignum a);                      //* 1 when a is integer
extern int    bn_isinf     (Bignum a);                      //* 1 when a is infinity
extern int    bn_isnan     (Bignum a);                      //* 1 when a is NaN

extern int    bn_getprec   (Bignum a);                      //* Return a's precision
extern int    bn_getglobalprec (void);                      //* Return global precision setting

extern void   bn_setprec   (Bignum a, int n);               //* Set a's precision
extern void   bn_setglobalprec       (int n);               //* Set global precision

extern int    bn_sign      (Bignum a);                      // Return -1, 0, 1
extern int    bn_digits    (Bignum a);                      // Return digits in integer

// Conversion and Output

extern void   bn_fix       (Bignum c, Bignum a);            //* c=(integer)a

extern long long int
              bn_toint     (Bignum a);                      // Convert a to C 64-bit int
extern double bn_tofloat   (Bignum a);                      //* Convert a to C 64-bit float

extern char*  bn_tostring  (Bignum a, int fmt);             // Convert a to allocated string
extern void   bn_print     (Bignum a);                      // print a to stdout
extern void   bn_println   (Bignum a);                      // bn_print then newline

extern void dummy(void);

//* Functions marked "*" in the comment are only needed for floating point.
//  A smaller subset used for integers can be extracted by ignoring
//  floating-point-only ones, although those remaining functions
//  will still have floating-point capability
