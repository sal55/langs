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
    enum (red,green,blue)                 # Open enumeration (names not typed)
    type colours = enum (red,green,blue)  # Typed enumerations

#### Compound
    [bounds]T          Fixed-length or unbounded array of simple types or bits
    record name =      Collection of simple (non-managed) types
        T a,b,c
        U d,e
    end
    range              Two in64 values
    taggedunion name = Declare enums and mixed collection of simple types
        red:   T a
        green: U b
        blue:  V c
    end
        
    (T, U, V)          Anonymous collection of simple types (used for function return types)

#### Pointer and Slice
    ref T          Pointer to any type including bit type
    slice[]T       Reference to array/subarray with length
    slice2d[]T     Reference to table (not subtable) with width/height
    flex[]T        Extensible reference to array/subarray
#### Managed (non-simple types)
    string         Flex, extensible, sharable string
    array[]T       Flex, extensible, sharable array of simple types
    set T          Array with unique set of elements
    dict[T]U       Shareable collection of key:value pairs, of types T and U
    record ...     Sharable, managed record
    decimal        Sharable, managed decimal arbitrary precision integer/float
#### Special
    void           Pointer target only
    auto           Temporary/default type that needs to be inferred
    type           Integer representing a type
    proc           Pointer target only (function pointer)
    label          Pointer target only
