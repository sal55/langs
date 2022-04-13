## Q Types


### Dynamic Types

These are tagged variant types.

Var Type | Val/Ref | Mut | Share | Hint? | M Type     | FFI  | Description
---      | ---     | --- | ---   | ---   | ---        | ---  | ---
**Void**     | Val     | N  | Y     | --    | --         | --   | Default value; unassigned
--       |         |     |       |       |            |      |
**Int**      | Val     | Y   |  N    | Y     |int, i64    | Y    | int64/i64 signed
**Word**     | Val     | Y   |  N    | Y     |word, u64   | Y    | word64/u64 signed
**Real**     | Val     | Y   |  N    | Y     |real, r64   | Y    | real64/r64 float
**Decimal**  | Ref     | N   |  Y    | Y     |--          | --   | Arbitrary precision int/float
**Bool**     | Val     | Y   |  N    | Y     |bool        | Y    | 1/0 value
--       |         |     |       |       |            |      |
**Enum**     | Val     | Y   |  N    | Y     |int, i64    | Y    | Simple enumeration
--       |         |     |       |       |            |      |
**Range**    | Val     | Y   |  N    | Y     |--          | --   | i32:i32 pair
--       |         |     |       |       |            |      |
**String**   | Ref     | Y/N |  Y    | Y     |slice, stringz | Y | Flex UTF8 string
**List**     | Ref     | Y/N |  Y    | Y     |--          | -- | List of V
**Dict**     | Ref     | Y/N |  Y    | Y     |--          | -- | Collection of key:value pairs
**Set**      | Ref     | Y/N |  Y    | Y     |--          | -- | Pascal-type bit-set
**Array**    | Ref     | Y/N |  Y    | Y     |[]T         | --  | Flex Array of T
**Bits**     | Ref     | Y/N |  Y    | Y     |--          | --  | Flex Array of u1/u2/u4
**Userarray** | Ref     | Y/N |  Y    | Y     |[]T         | --  | Fixed Array of T
--       |         |     |       |       |            |      |
**Record**   | Ref     | Y/N |  Y    | Y     |--          | --  | Record of V
**Struct**   | Ref     | Y/N |  Y    | Y     |record      | Y   | Record of T
--       |         |     |       |       |            |      |
**Refvar**   | Val     | Y   | N     | -     |(Ref T)     | --   | Pointer to V
**Refpack**  | Val     | Y   | N     | -     |ref T       | Y    | Pointer to T
**Refbit**   | Val     | Y   | N     | -     |--          | --   | Pointer to B/bitfield
--       |         |     |       |       |            |      |
**Symbol**   | Val     | N   | N     | -     |ref Tnt     | Y   | Reference to ST entry
**Type**     | Val     | N   | N     | -     |int. i64    | Y   | Represents a type
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
