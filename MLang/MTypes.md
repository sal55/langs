## M5 Type System

M types come in two varieties:

* Value Types, also called Pack or Packed Types
* Object Types, implemented as tagged variants

Value types came from the static-only types of the original M language,
and Object types from the dynamic Q scripting language.

### Value Types

**T** is any value type.

Name |  Description
--- | ---
-- | **Signed Integers**
i8 |
i16 |
i32 |
i64 |
-- | **Unsigned Integers**
u8 |
u16 |
u32 |
u64 |
-- | **Character Types**
c8 |
c64 | (Internal type)
-- | **Floating Point**
r32 |
r64 |
-- | **Composite Types**
Array | Fixed size array of T
Record | User type record of mixed T elements
Slice | Subarray view or slice
Range | Two i64 values
-- | **Pointers**
Ref | Pointer to T
-- | **Pointer Targets**
void |
proc | Used in function pointers
function |
label |
u1 | 1, 2 and 4-bit fields
u2 |
u4 |
-- | **Misc**
type  | Represents a type
auto |                    Will infer type from initialisation
enum |                    User type
bitfield |                Internal type
tuple |                   Internal type

### Object Types

This initial implementation has really just the one tagged Variant type.
A Variant can contain any of these kinds of objects:

Name |   Description
--- | ---
Int |                 int64
Word |                word64
Real |                real64
Decimal |            Arbitrary precision int/float decimal type
String |             Flex string (strings and lists include slices)
List |               Flex array of variants
Dict |               Flex set of key:value pairs
Set |                Flex bit-set
Array |              Flex array of T (user-defined arrays are fixed size)
Bits |               Flex array of u1/u2/u4
Record |             Mixed Object-only, or Object/T elements
Packrecord |         Mixed T elements only
Type |                Represents any type
Operator |            Represents a built-in operator
Symbol |              Represents any symbol (used for function pointers)
Refvar |              Pointer to any object type
Refpack |             Pointer to any T value type
Refbit |              Pointer to a bit type u1/2/4, or bitfield 1-64 bits

### Type Syntax
```
    i8     int8             Aliases for the same type
    i16    int16
    i32    int32
    i64    int64   int

    u8     word8   byte 
    u16    word16
    u32    word32
    u64    word64  word
    
    c8             char
    
    r32    real32
    r64    real64  real
    
    [bounds]T               Array of T (may need to be preceded by 'var', 'mut' or 'let' in some contexts)
    U                       U is any user-defined type
    slice[lwb]T             Slice of T (lwb is fixed; upb/len set at runtime)
    range
    
    ref T                   Pointer
    ichar                   Special designation for ref char
    ref proc...             Pointer to proc or function; function signature needed
    ref func...
    ref void                Generic pointer
    
    auto                    (Type infered; not fully supported)
    
    variant                 Tagged variant
```
**variant** rarely needs to be written; it is usually the default type in syntax like this:
```
    var a, b, c             No type after 'var', so assumes variant
    (a, b, c)               In parameter list
```
Variant-subtypes can be specified directly; a variant is created initialised to that type, and type-checking or conversion is added to ensure that it stays the correct type:
```
    string                  Flex string
    list                    Flex list of variants (but list of strings for example is not possible; only top-level type can be set)
    dict
    U                       User-defined type of a record containing any object fields
```
Although classed as type-hinting, checks are added as indicated. I said it is not possible to denote a variant which is a List that contains only Strings, as such types
get get arbitrarily complex. But also, the types need to be known at compile-time to be of any benefit, or to put in the relevant checks. Copy or pass such a type-hinted variable to a regular variant, and that info no longer exists.

