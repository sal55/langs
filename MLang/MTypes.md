Type       | Syntax            |  Cat  | Var      |  Notes
---        | ---               |  ---  | ---      | ---
**Primitives**  | -     |  -  | -      | -
i64        | **i64, int64, int**   | T |     int      |   Signed integer
u64        | **u64, word64, word** | T |     word     |  Unsigned integer
c64        |                  | T |     (int)    |   Char (internal)
r64        |**r64, real**         | T |     real     |   Float
r32        |**r32**               | T |     (real)   |
b64        |**bool64, bool**      | T |     bool     |   Boolean
**Storage Types**     |  -  | -      | - | -
i8         |**i8, int8**          | Ts|     (int) |
i16        |**i16, int16**        | Ts|     (int) |
i32        |**i32, int32**        | Ts|     (int) |
u8         |**u8, word8, byte**   | Ts|     (int) |
u16        |**u16, word16**       | Ts|     (int) |
u32        |**u32, word32**       | Ts|     (int) |
c8         |**char**              | Ts|     (int) |
bool8      |**bool8**            | Ts|     (int) |
**Composite**    |  -  | -      | - | -
range      |                  | T |     range   |    (Internal)
Record of T|**U**                 | T |     extstruct |
Array of T |**array[bounds]T**    | T |     extvector |
Slice of T |**slice[lwb]T**       | T |     extvector |
**Pointers**    |  -  | -      | - |
Ref to T   |**ref T**             | T |     refpack  |   Pack pointer
Ref to V   |**ref V**             | T |     refvar   |   Var pointer
Ref to B   |**ref B**             | T |     refbit   |   Bit pointer
Ref char   |**ichar**             | T |     string   |
ref proc   |**ref proc, ref func**| T |     symbol |
ref Void   |**ref void**          | T |     refpack |
**Bit Types**   |  -  | -      | - |
u1         |**u1, bit**           | B |     - |
u2         |**u2**                | B |     - |
u4         |**u4**                | B |     - |
**Variant Types**   |  -  | -      | - |
Variant    |**var**               | V |     -      |     Variant
Int        |                  | V |     - |
Word       |                  | V |     - |
Real       |                  | V |     - |
Decimal    |                  | V |     - |
Range  (or i64)     |                  | V |     - |
String (of u8)    |                  | V |     - |
List f V   |                  | V |     - |
Dict       |                  | V |     - |
Set (of B)        |                  | V |     - |
Record of V     |**U**                 | V |     - |
Vector of T     |**vector[]T**         | V |     - |     Hinted variant
Bits of B       |                  | V |     - |
Struct of T     |                  | V |     - |
Refpack (T)   |                  | V |     - |
Refvar (V)    |                  | V |     - |
Refbit (B)    |                  | V |     - |
Symbol     |                  | V |     - |
Type       |                  | V |     - |
