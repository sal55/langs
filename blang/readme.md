## B Language Docs

### Overview

Upgrade of the 'M' systems language I've been developing since early 80s.

Targets x64 running x64.

Includes many features from my current dynamic language, such as flexible data types and first-class handling of most data, but within a statically typed, native-compiled language.

### Data Types

#### Numeric
    int8     i8                (Signed)
    int16    i16
    int32    i32
    int64    i64      int
    int128   i128
    
    word8    u8       byte     (Unsigned)
    word16   u16
    word32   u32
    word64   u64      word
    word128
    
    char
    wchar
    char64
    
    real32   r32               (Float)
    real64   r64      real
    
    bit      u1
    bit2     u2
    bit4     u4

#### Enumerations
    enum           Open or Typed sets of enumerated values

#### Compound
    Arrays         Fixed-length or unbounded array of simple types
    Bits           Fixed-length or unbounded array of bit types
    Records        Collection of simple (non-managed) types
    Range          Two in64 values
    Taggedunion    Declare enums and mixed collection of simple types
    Tuple          Anonymous collection of simple types (used for function return types)

#### Pointer and Slice
    ref T          Pointer to any type including bit type
    slice          Reference to array/subarray with length
    slice2d        Reference to table (not subtable) with width/height
    flex           Extensible reference to array/subarray
#### Managed (non-simple types)
    String         Flex, extensible, sharable string
    Array          Flex, extensible, sharable array of simple types
    Set            Array with unique set of elements
    Dict           Shareable collection of key:value pairs
    Record         Sharable, managed record
    Decimal        Sharable, managed decimal arbitrary precision integer/float
#### Special
    Void           Pointer target only
    Auto           Temporary/default type that needs to be inferred
    Type           Integer representing a type
    Proc           Pointer target only (function pointer)
    Label          Pointer target only
