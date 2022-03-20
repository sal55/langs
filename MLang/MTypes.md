## M5 Type System


### M Types

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
**Boolean Types** |
bool8 | **bool8** |
bool64 | **bool64**, "bool"|
**Character Types** |
c8 | **char** |
c64 | | (Internal type) |
**Floating Point** |
r32 | **r32**, **real32**
r64 | **r64**, **real64**, **real**
**Composite Types** |
Record | **T** | (T is a user type) Mixed type fields
Range | **range** | Two i64 values
**Arrays and Slices** |
Array | **[*bounds*]T** | Fixed size array
Slice | **slice[*lwb:*]T** | View into array
**Pointers** |
Ref | **ref T**| Pointer to any flat type
Refvar | **ref V**| Pointer to any variant type
Refbit | **ref B**| Pointer to a bit type
**Variants** |
Variant | **variant** | Contains types shown below
Vector | **vector[*bounds*]T** | Implemented as var flex arrays
**Special Pointer Targets** |
void | **ref void** | Can only be used with **ref**
proc | **ref proc...** | Need full signature
function | **ref func ...**
label |**ref label**
u1 | **ref u1**, **ref bit** | 1-bit field
u2 | **ref u2** | 2-bit field
u4 | **ref u4** | 4-bit field
 **Misc** |
type | | Represents a type
auto | **auto** | Infer from initialisation
bitfield | |Internal type |
tuple |  | Internal type |

### Key

**T** is any non-managed, fixed size type (also called 'pack' type)

**V** is variant type

**B** is any bit type (u1 u2 u4)

***lwb:*** Optional lower bound

***bounds*** Optional bounds:
 ```
     []          Unbounded (array), or dynamic (slices, vector, flex)
     [A:]        Same but using lower bound of A
     [N]         Length N, lower bound 1
     [A:N]       Length N, lower bound A
     [A..B]      Length B-A+1, lower bound A
```
Unbounded arrays are mainly for pointer targets

### Variants

A Variant is a 100% managed type can contain any of these kinds of objects:

Type | Notes
--- | ---
Int |  int64
Word | word64
Real |  real64
Decimal |  Arbitrary precision int/float decimal type
String |   Flex string or slice (using UTF8)
List |  Flex array or slice of V
Dict |  Flex set of key:value pairs, each of type V
Set |  Bit-set
Record | User type with named fields of type V
Type |   |       Represents any type
Operator |   Represents a built-in operator
Symbol |    Represents any symbol (used for function pointers)
Refvar |    Pointer to any variant type
-- |
Refpack |    Pointer to any type T
Refbit |    Pointer to any B type, also bitfields 1-64 bits
Array |    Flex array or slice of any T
Bits |    Flex array or slice of B bit types
Struct | Record of mixed types T

A variant has the type name **var**.
    
### Shared and Non-Shared Types

Most objects represented by variants share their data, so in `A := B := "XYZ"` both A and B refer to the exact same string.

Where objects are mutable, then an in-place change in one, will be visible from all shared references to the object.

Specifically, Strings, Lists, Records, Dicts and Sets are mutable. Ints, Reals, Ranges, Decimals are immutable; an attempt at in-place modifications, will result
in a new objects.

Note that objects created from a constructor have the immutable flag set. Constructors are:
```
    "ABC"              String literals
    (x, y, z)          List constructors (elements don't need to be constant)
    R(a, b, c)         Record constructors
    [i, j..k]          Set constructors
    [a:b, c:d]         Dict constructor
```
This was to ensure that a constant list such as `(10, 20, 30)` needs not need building each time it's encountered. It was also simpler have a rule that these are always immutable, whether the elements are constant or not. (If they are, it can be optimised.)

To create a mutable copy, use one of:
````
   A ::= (10, 20, 30)
   A := copy((10, 20, 30))
````
