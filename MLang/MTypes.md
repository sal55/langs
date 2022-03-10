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
Slice | **slice[*lwb:*]T** | View into array, vector, flex
Vector(**M**) | **vector[*lwb:*]T** | Dynamic, fixed-length array
Flex(**M**)| **flex[*lwb:*]T** | Dynamic growable array
**Pointers** |
Ref | **ref U**| Pointer to any type
**Variants** |
Variant(**M**) | **variant** | Contains types shown below
**Special Pointer Targets** |
void | **void** | Can only be used with **ref**
proc | **proc...** | Need full signature
function | **func ...**
label |**label**
u1 | **u1**, **bit** | 1-bit field
u2 | **u2** | 2-bit field
u4 | **u4** | 4-bit field
 **Misc** |
type | | Represents a type
auto | **auto** | Infer from initialisation
enum | **enum...** |  (Forms int type) |
bitfield | |Internal type |
tuple |  | Internal type |

### Key

**T** is any non-managed type (see below)

**U** is *any* type including managed types and variants

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

### Managed and Unmanaged Types

The types marked with **(M)** suffix in the above table are managed types. This is where the language initialises the type, allocates any storage needed, and recovers the storage when it's no longer required (overwrite with something new, or it goes out of scope).

These aren't allowed as elements of arrays, vectors, records and so on on. This is because the management of such structured nested types becomes complex.

Use Variants for complete freedom in this area. (Arrays of managed types could be allowed, but it would need some manual freeing of the elements.)

### Variants

A Variant is a 100% managed type can contain any of these kinds of objects (V is a variant type):

Type | Notes
--- | ---
Int |  int64
Word | word64
Real |  real64
Decimal |  Arbitrary precision int/float decimal type
String |   Flex string (strings and lists include slices)
List |  Flex array of V
Dict |  Flex set of key:value pairs, each of type V
Set |  Flex bit-set
Record | User type with named fields of type V
Type |   |       Represents any type
Operator |   Represents a built-in operator
Symbol |    Represents any symbol (used for function pointers)
Refvar |    Pointer to any variant type

A variant itself uses **variant** when declared, but it's rare to have to write it, as it is either assumed, or it is the default when a type is omitted:
```
    int a, b, c                 # Declare 3 ints
    var int a, b, c             # Using the optional `var` prefix
    var a, b, c                 # Missing type, assumes `variant`
    var variant a, b, c         # Make it explicit
    variant a,b ,c              # And drop the `var` prefix
```
The last 3 examples declare the same things. In practice these forms are used:
```
    int a, b, c                 # 3 ints
    var a, b, c                 # 3 variants
```
Just be aware that `var` doesn't mean the same thing as `variant`; it's not a type. It's a prefix like `let'.

In some contexts, eg. parameter lists where it is clear it is a declaration, the `var` can be dropped, so that `(a, b, c)` is 3 variant parameters.
    
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
This was to ensure that a constant list such as (10, 20, 30) needs not need building each time it's encountered. It was also simpler have a rule that these are always immutable, whether the elements are constant or not. (If they are, it can be optimised.)

To create a mutable copy, use one of:

   A ::= (10, 20, 30)
   A := copy((10, 20, 30))

