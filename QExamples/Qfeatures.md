## Q Scripting Language

* Q is the companion scripting language to my M systems language. The two mostly share the same syntax (M needs static type declarations).

* It isn't as high level as typical scripting languages, but is does have dynamic types and some advanced enough data types to act as a scripting language for M.

* Q has its origins in add-on scripting languages I used for my applications, dating from the late 1980s.

* It's been standalone for about the last 20 years, but has been slowly evolving.

* Attempts to merge M and Q into a hybrid language have failed, so they stay separate, but closely coupled

### Identifiers

In Python, Lua and others, every user-identifier is effectively a variable, since even the name of a function, import or class can be assigned any value at any time. In Q, identifiers are categorised as one of these names at compile-time:

* Variable name
* Type or Record name
* Function or DLL function
* Import module name
* Label name
* Record field name
* Named constant
* Enumeration name

Only variables can be assigned to or modified. Seems obvious really. This allows more specific bytecodes to be generated, and enables more error-checking. And, importantly, means that if you've seen a function F defined, you KNOW that if you see F in the same scope, it is guaranteed to be that function.

If a variable function name is needed, you create a reference to it; so is P, Q are variables and F, G etc are function names:

    P := F
    Q := (F, G, H)        # list of functions

Now P works like a function name does in Python (however, some features of functions are not available when using P, such as keyword arguments).

### Variant and Packed Types

Q directly supports both its own Variant types, and low-level types as used in M (ie. the sort that are used in C and C-APIs). This is is unusual in script languages, which usually need add-ons or some strange syntax to manage the latter.

They are called 'Pack' types simply because the raw primitives are packed together in arrays and structs.

### Variant Types

All objects in Q are tagged with their type. These are the types available:

* Integer (i64, or less commonly u64)
* Real (r64, ie. floating point)
* Bignum (arbitrary precision integer or float)
* String (8-bit)
* List of variant objects
* Record (user-defined type of variant fields)
* Range (two i64 values)
* Set (Pascal-style bit-set)
* Dict (key/value data structure)
* Array of packed elements
* Array of bit elements (u1, u2, u4 types)
* Struct (see packed types)
* Symbol (reference to another kind of name: function, type etc)
* Pointer to variant
* Pointer to pack type or bitfield

These include slices of Strings, List, Array objects

### Packed Types

These can only be used as elements of Arrays, Structs, as Pointer targets, or as parameters and return types of foreign functions:

* Signed integers i8 i16 i32 i64 i128
* Unsigned integers u8 u16 u32 u64 u128
* Bit types u1 u2 u4 (for Array elements, or Pointer targets)
* Float types r32 r64
* Zero-terminated string field (of a struct)
* Counted string field
* Pointer to zero-terminated string
* Struct of packed types, including fixed-sized arrays and other structs
* Fixed-size array of packed types, including other fixed-size arrays and structs
* Pointer to packed type

All arrays are fixed size when used a packed type. But flex-sized arrays are possible in conjuction with variants, but then it will be a regular object:

    A := new(array, byte, random(1..1000))


### Record Types

I was hardly going to mention this until I remembered how few scripting languages have proper records. A record type is the only user type that doesn't involve packed types:
````
record date =
    var day, month, year
end
````

### Defining New Types

Apart from records, this is only used for new pack types:

    type vector = [4]real64

    A := new(array, vector, 100)


