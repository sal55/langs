## Q Dynamic Language

Q is a dynamic, interpreted language. It is closely coupled with the M static language whose type system is summarised below.

### Q Variant Types


The interpreter for this dynamic language uses tagged variables and objects. The tag
will always indicate one of these types:


Type | Description
--- | ---
**Void**	|	Means unassigned
--- |
**Int**	|		64-bit signed integer
**Word**	|	64-bit unsigned integer
**Real**	|	64-bit floating point number
**Decimal**	|	Arbitrary-precision decimal-based integer/float number
**Range**	|	Pair of integers (limited to 2x32-bit)
--- |
**String**	|	Flex string of 8-bit bytes (ASCII/UTF8)
**List** 	|	Flex array of Variants
**Dict**	|	Flex dictionary of Variant key:value pairs
**Set**	|		Flex, Pascal-style bit-set
**Array**	|	Flex array of Packed elements
**Bits**	|	Flex array of Bit elements (not user-definable yet; that is of limited use)

**Pointer**	|	To Variant
**Refpack**	|	Pointer to packed value
**Refbit**	|	Pointer to Bit (1/2/4 bits) or Bitfield (1-64 bits)
--- |
**Record**	|	User-defined record of Variant fields
**Struct**	|	User-defined record of Packed fields
**Carray**	|	User-defined fixed array of Packed fields
--- | 
**Type**	|	Contains a value representing a type
**Operator**	| Represents an operator
**Symbol**	|	Symbol table entry (used also for function pointers)
--- |
**Return**	|	Internal type: return-address descriptor
**Exception** |	Internal type: exception handling



'Flex' means dynamically allocated and growable. Most types are mutable. However used-defined arrays, which will have specific bounds, cannot be made bigger.

### Packed Types

Fixed size primitive types, packed structs and packed fixed length arrays. Structs and arrays are always flat. Used with Variant/Packed Struct, Array and Refpack types, and for working with FFIs. When accessed from the dynamic interpreter, they are converted as needed to
or from a suitable Variant type:

Type | Description
--- | ---
**i8** |		Signed integers (also **int8** etc)
**i16**  |
**i32** |
**i64** |
--- | 
**u8** |		Unsigned integers (also **byte**, **word8** etc)
**u16** |
**u32** |
**u64** |
--- | 
**r32** |		Floating point (also **real32** etc)
**r64** |
--- | 
**stringc\*N** |Fixed-width counted string field (max length N chars)
**stringz\*N** |Fixed-width zero-terminated string field (max N-1 chars)
--- | 
**array** |	Fixed-length packed array of packed elements
**struct** |	Packed struct of packed fields
--- | 
**ref** |		Packed pointer to packed target
**cstring** |	Pointer to 8-bit zero-terminated string

### Bit Types

Type | Description
--- | ---
**u1**	|		1 bit (also bit, bit2, bit4)
**u2**	|		2 bits
**u4**	|		4 bits

The above are used in Bits (bit arrays), or as the target of Refbit (and are also the basic
Set element). There are also arbitrary bitfields up to 64 bits, which can be the target of Refbit, but
they are not classed as a type. (Longer bitfields exist as slices of bit-arrays. Most work with bits or bitfields is via operations rather than types.)

### Slicing

Taking a slice of a String, List, or Array yields another String, List
or Array, but it is now a view into the original. There is no separate
slice type.

### The Variant Descriptor

A Variant is a 16-byte or 128-bit structure which basically is one of:

**(Tag, Value)**	Int, Word, Real, Range, Pointer, Ref, Refbit, Void
				Type, Operator, Symbol

 **(Tag, Object)** All other types include a reference to a more elaborate
				reference-counted object with data stored on the heap

The interpreter manipulates only variants, and the stack is an array of variants. Any packed types or bit fields that are encountered, are converted to a suitable
variant type.

(This interpreter does not use the popular approach of encoding floats, integers and pointers/references into a single 64-bit value. Since it is targeted at 64-bit machines only, the variant type only occupies two machine words instead of one. That is not onerous, and is not a big overhead compared to all the others involved in interpretation.

Neither does it make everything a reference, equivalent to manipulating only Object values instead of (Tag, Object) or (Tag, Value). This would require that simple types such as ints and floats are shared, reference-counted objects too.)

### Assignment, Sharing and Copying

Types which exist on the heap (String, List etc) are reference-counted and normally shared:
````
B := A        # B is a shared copy of A; A's reference count is stepped
C ::= A       # C is an independent, deep copy of A
````
The rules are a little different for records, where even when using "::=" makes a shallow copy, not a deep one.


### Excluded Types

**Boolean** This has sometimes been used internally, but has never been exposed as a user type. Apart from **true** and **false** constants, I have never felt the need.

**Enumerations** These are mainly named constants, with very little support in the type system. (I'm hoping to be able to directly print an enum value by name rather than its ordinal value, but that's about it.) Better support would be nice, but it rapidly gets complicated and, in interpreted code, less efficient.

**Tagged Unions** I'd planned this for my static language last year, but lost interest. My requirements of tagged unions are more diverse and slightly more chaotic than would suit an inflexible language feature.

**Sum Types** I mean the alternatives sometimes denoted as T | U | V, even without the fancy stuff you see in Haskell. With dynamic types, you get a lot of flexibility and there are lots of workarounds. To make sure X only has types T, U or V, I can write **if X.type in \[T,U, V\]**.

**Generics** This is a big deal in static languages, but since this one is dynamic, you get most of the benefits for free.

## M Static Systems Language

This has deliberately been kept lower level, since it is intended that applications will be mainly written in Q.

### M Static Types

These are more or less the same as Q's Packed types, but which can be combined in unlimited ways and without needing to create so many intermediate user types.

Type | Aliases | | Description
--- | --- | --- | ---
**i8**  |  **int8** |  |	Signed integers
**i16**  | **int16** |   |
**i32** | **int32** | |
**i64** | **int64** | **int** |
**i128**| **int128** |
--- | 
**u8** | **word8** | **byte** |		Unsigned integers
**u16** | **word16** | |
**u32** | **word32** | |
**u64** | **word64** | **word** |
**u128** | **word128** | |
--- | 
**r32** | **real32** | |		Floating point
**r64** | **real64** | **real** |
--- | 
**c8** | **char**| |  8-bit character type
(**c64**)| | |  Internal type that arise due to widening to 64 bits
--- | 
**Array** |**[]T**||	Fixed-length or unbounded array of any other types except void
**Record** |||	Record of mixed types
--- | 
**Pointer** |**ref T**||    Pointer to any type
**ichar** |**ref char**|| Synonym for 'ref char', normally pointing to a zero-terminated string
**Slice** |**slice[]T**||  Slices of strings and arrays, a (pointer, length) pair
--- |
(**Range**)|| | Used internally for a..b constructs
(**Type**) ||| Used internally
(**Bitfield**) ||| Used internally
(**Enum**) ||| Used internally
(**Proc**) |||Only as target of pointer
(**Tuple**) ||| Used internally for multiple function return values
(**Auto**)  ||| Used for minor type inferencing
(**Label**) ||| Used internally
