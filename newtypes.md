Type Summary

    Numeric
        Integer         Signed integers 8-128 bits
        Word            Unsigned integers 8-128 bits
        Real            Floats 32,64 bits
        Bit             Unsigned bits 1,2,4 bits (arrays and pointer targets only)

    Enums               Open or Typed (named types only) 8,16 bits

    Compound
        Arrays          Fixed-length or unbounded array of simple types
        Bits            Fixed-length or unbounded array of bits (used for bit-sets)
        Records         Collection of simple [non-managed] types (named types only)
        Range           Two int64 values
        Taggedunion     Declare enums and collection of simple types (experimental)
        Tuple           Anonymous collection of simple types

    Pointer
        Ref             Pointer to any type, or pointer to bit1/2/4
        Slice           Reference to array or subarray with length
        Slice2d         Reference to table (not subtable) with width and height 
        Flex            Extensible reference to array with length and capacity

    Managed
        String          Flex, extensible, sharable string, with auto memory management
        Array           Like String
        Set             Array with unique set of elements
        Dict            Sharable collection of key/value pairs
        Record          Sharable, managed record (not sure yet if simple types only)
        Decimal         Sharable, managed decimal integer/float unlimited precision

    Special
        Void            Pointer target only
        Auto            Temporary type that needs to be inferred
        Type            Integer representing a type
        Proc            Pointer target only (function pointer)
        Label           Pointer target only
