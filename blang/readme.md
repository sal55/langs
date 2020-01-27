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
    []T                Fixed-length or unbounded array of simple types or bits
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

### Array and Slice Bounds

Full bounds only used for fixed arrays. Slices and managed arrays only have an optional lower-bound, which is fixed at compile-time. Possibilities for simple arrays are:

    []T            Unbounded: size either set by init data, or not used when target of a pointer.
    [A:]           Unbounded array with lower bound of A
    [N]T           Bounds are 1..N inclusive, length is N
    [A..B]T        Bounds are A..B, length is B-A+1
    [A:N]T         Bounds are A..A+N-1, length is N

For slices, flex and managed arrays:

    []T            Lower bound is 1, length stored inside object (.len); bounds are 1..X.len
    [A:]T          Lower bounds is A; bounds are A..A+X.len-1

All an object X, for arrays, slices and strings, can be obtained with:

    X.lwb          Lower bound
    X.upb          Upper bound (both inclusive)
    X.len          Length
    X.bounds       Returns a range X.lwb..X.upb

For managed string objects, X.len and X.lenstr are the same thing. For lower level strings (eg. char arrays) containing terminated stringz sequences, then X.len gives the length of the array, while X.lenstr is the length of the string as determined by the zero terminator

    

