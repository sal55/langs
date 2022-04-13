## Q Types

### Classification

Q uses these broad groups for its types:

Code | Name | Characteristics
--- | --- | ---
**V** | **Variant** | Dynamic; tagged; managed; flexible
**T** | **Pack** | Static; flat; fixed-size; primitive (ie. C-style)
**B** | **Bit** | One of `u1 u2 u4` 1/2/4-bit types; used with Bits and Refbit only

My M systems language exclusively uses T types and deliberately keeps them low-level)

Q uses mainly V types, but these can incorporate T types (eg. by 'boxing').

The new M-style compiled functions within Q code also only work with T types so need type-checking and conversions V and T when calling across interpreted and compiled code.

### Dynamic `V` Types

These are tagged variant types.

Var Type | Val/Ref | Mut | Share | Hint? | M Type     | FFI  | Description
---      | ---     | --- | ---   | ---   | ---        | ---  | ---
**Void**     | Val     | N  | Y     | --    | --         | --   | Default value; unassigned
--       |         |     |       |       |            |      |
**Int**      | Val     | Y   |  N    | Y     |int, i64    | Y    | i64 signed
**Word**     | Val     | Y   |  N    | Y     |word, u64   | Y    | u64 signed
**Real**     | Val     | Y   |  N    | Y     |real, r64   | Y    | r64 float
**Decimal**  | Ref     | N   |  Y    | Y     |--          | --   | Arbitrary precision decimal int/float
**Bool**     | Val     | Y   |  N    | Y     |bool        | Y    | 1/0 or true/false value
--       |         |     |       |       |            |      |
**Enum**     | Val     | Y   |  N    | Y     |int, i64    | Y    | Simple enumeration
--       |         |     |       |       |            |      |
**Range**    | Val     | Y   |  N    | Y     |--          | --   | i32:i32 pair
--       |         |     |       |       |            |      |
**String**   | Ref     | Y/N |  Y    | Y     |slice, stringz | Y | Flex UTF8 string (array of u8)
**List**     | Ref     | Y/N |  Y    | Y     |--          | -- | List of V
**Dict**     | Ref     | Y/N |  Y    | Y     |--          | -- | Collection of key:value pairs
**Set**      | Ref     | Y/N |  Y    | Y     |--          | -- | Pascal-type bit-set (array of u1)
**Array**    | Ref     | Y/N |  Y    | Y     |[]T, slice  | --  | Flex Array of T
**Bits**     | Ref     | Y/N |  Y    | Y     |--          | --  | Flex Array of B
**Userarray** | Ref     | Y/N |  Y    | Y     |[]T, slice | Y?  | User-defined, fixed Array of T
--       |         |     |       |       |            |      |
**Record**   | Ref     | Y/N |  Y    | Y     |--          | --  | User-defined Record of V
**Struct**   | Ref     | Y/N |  Y    | Y     |record      | Y   | User-defined Record of T
--       |         |     |       |       |            |      |
**Refvar**   | Val     | Y   | N     | -     |(Ref T)     | --   | Pointer to V
**Refpack**  | Val     | Y   | N     | -     |ref T       | Y    | Pointer to T
**Refbit**   | Val     | Y   | N     | -     |--          | --   | Pointer to B or Bitfield
**Refproc**   | Val     | N   | N     | -     |Ref proc   | Y   | Pointer to native code function
--       |         |     |       |       |            |      |
**Symbol**   | Val     | N   | N     | -     |ref Tnt     | Y   | Reference to ST entry
**Type**     | Val     | N   | N     | -     |int. i64    | Y   | Represents any V T B type
**Operator** | Val     | N   | N     | -     |int. i64    | Y   | Represents an operator
**Retaddr**  | Val     | N   | -     | -     |--          | --  | Internal
**Exception** | Val     | N   | -     | -    |--          | --  | Internal

**Var Type** The type indicated by the variant's tag

**Val/Ref** Whether manipulated by value, or by reference using a reference counter

**Mut** Whether the value is in-place mutable. Y/N means it contains a mutable. Shared objects are generally mutable unless they are created with a constructor (eg. literals) or explicitly made immutable

**Share** Types using references usually are shared (`A`, `B` and `C[i]` can all refer to the same copy of the data).

**Hint?** Whether a type-hinted version of this variant is possible, even if usually nothing is done with that info. Most locals are either not declared, or declared with **var**, a generic variant.

Hinting can be used to provide more specific default values, or allows writing `p := (10, 20)` instead of `p := point(10, 20)`. But when a function is flipped from Q to M, then the hint can serve as static type info.

**M Type** Equivalent M static type. Calling between Q and M will do suitable runtime conversions.

**FFI** Whether the variant type can ever have suitable runtime conversions for C-style APIs.

Refbit targets)

### Static `T` Types

These are also called 'Pack' types within Q.

Type      | Syntax                 | Description
---       | ---                    | ---
int       | **int int64 i64**      | Signed integer
word      | **word word64 u64**    | Unsigned integer
real      | **real r64**           | Float
real32    | **real32 r32**         | Float
--        |                        |
bool      | **bool bool64**        | Boolean 1/0 true/false
--        |                        |
ref       | **ref T**              | Pointer to T
--        |                        |
record    | **U**                  | User-defined record of T
array     | **[*bounds*]T**        | Fixed-size array of T
slice     | **slice\[\]T**   | Slice of T
--        |                        |
i8        | **int8 i8**            | Storage types
i16       | **int16 i16**          | ...
i32       | **int32 i32**          | ...
u8        | **word8 u8 byte**      | ...
u16       | **word16 u16**         | ...
u32       | **word32 u32**         | ...

#### Bounds ####

These are always fixed size, but can also defined a lower-bound:
```
    [N]T x            # bounds are 1..10 inclusive
    [A:N]T x          # bounds are A..N+A-1
    [A..B]T x         # bounds are A..B (same as [A:B-A+1])
    []T x             # unbounded (also used when a pointer target), starting from 1
    [A:]T x           # unbounded starting from A
```
Slice don't have a set length (it's a runtime property), but they do have a lower bounds, usually 1. (Rarely it can be set to something else using `slice[0:]T`)

**Q Limitations on Lower Bounds** Being dynamic, lower bounds are a runtime property. But is expensive to store a full 64-bit or even a 32-bit lower bounds. So within Q, these are limited:

**Lists** A lower bound is 16 bits so it limited to -32768 to +32767
**Arrays, Bits** A lower bound is just 1 bit is either 0 or 1
**Strings** Always 1
**Sets** Always 0

However, 99.9% of the time, lower bounds will be either 1 or 0

#### Storage Types

In M, these are only used as array elements, record fields, or pointer targets. Not as standalone variables, or for passing to/from functions.

Within FFIs however, storage types are sometimes used as parameter or return types. Although frequently because `int` within such APIs is considered a 32-bit type. I have to rename a C-type `int` as `int32` or `i32`.
