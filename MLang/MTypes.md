## M5 Type System

M types come in two varieties:

* Value Types, also called Pack or Packed Types
* Object Types, implemented as tagged variants

Value types came from the static-only types of the original M language,
and Object types from the dynamic Q scripting language.

### Value Types

**T** is any value type.

Type |  Syntax | Notes
--- | --- | ---
**Signed Integers** | 
i8 | **i8**, **int8**
i16 | **i16**, **int16**
i32 | **i32**, **int32**
i64 | **i64**, **int64**, **int**
**Unsigned Integers** | |
u8 | **u8**, **word8**, **byte**
u16 | **u16**, **word16**
u32 | **u32**, **word32**
u64 | **u64**, **word64**, **word**
**Character Types** |
c8 | **char** |
c64 | | (Internal type) |
**Floating Point** |
r32 | **r32**, **real32**
r64 | **r64**, **real64**, **real**
**Composite Types** |
Array | **[*bounds*]T** | Fixed size array
Record | **U** | (User type name) Mixed T
Slice | **slice[*lwb*]T** | Subarray or slice
Range | **range** | Two i64 values
**Pointers** |
Ref | **ref T** | Pointer to T
**Pointer Targets** |
void | **void** | Can only be used with **ref**
proc | **proc...** | Need full signature
function | **func ...**
label |**label**
u1 | **u1**, **bit** | 1-bit field
u2 | **u2** | 2-bit field
u4 | **u4** | 4-bit field
 **Misc** |
type | | Represents a type |
auto | **auto** | Infer from initialisation
enum | **enum...** |  (Forms int type) |
bitfield | |Internal type |
tuple |  | Internal type |

### Object Types

This initial implementation has really just the one tagged Variant type.
A Variant can contain any of these kinds of objects:

Type | Syntax |  Notes
--- | --- | ---
Int |  **(int)** |  int64
Word | **(word)** | word64
Real | **(real)** |  real64
Decimal | **decimal** |     Arbitrary precision int/float decimal type
String | **string** |           Flex string (strings and lists include slices)
List |  **list** |             Flex array of variants
Dict |  **dict** |             Flex set of key:value pairs
Set |   **set** |   Flex bit-set
Array |  **array**|  Flex array of T (user-defined arrays are fixed size)
Bits |  **bits** |    Flex array of u1/u2/u4
Record |  **U** | (User type with some object elements)
Packrecord |  **U** | (User type with value elements only)
Type |   |       Represents any type
Operator | |            Represents a built-in operator
Symbol |  |     Represents any symbol (used for function pointers)
Refvar |  **refvar**|            Pointer to any object type
Refpack | **refpack**|            Pointer to any T value type
Refbit |  **refbit**|            Pointer to a bit type u1/2/4, or bitfield 1-64 bits

A variant itself uses **variant**, but it's rare to have to write it, as it is either assumed, or it is the default when a type is omitted.

The above syntax is used for a type-hinted version (with parentheses to distinguish from a value type of the same name). Some of it is also needed to refer to the type a variant  might currently hold, for conversions and so on.
