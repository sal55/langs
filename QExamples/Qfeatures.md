## Q Scripting Language

* Q is the companion scripting language to my M systems language. The two mostly share the same syntax (M needs static type declarations).

* It isn't as high level as typical scripting languages, but is does have dynamic types and some advanced enough data types to act as a scripting language for M.

* Q has its origins in add-on scripting languages I used for my applications, dating from the late 1980s.

* It's been standalone for about the last 20 years, but has been slowly evolving.

* Attempts to merge M and Q into a hybrid language have failed, so they stay separate, but closely coupled

### Identifiers

In Python, Lua and others, every user-identifier is effectively a variable, since even the name of a function, import or class can be assigned any value at any time. In Q, identifiers are categorised as one of these names at compile-time, then they cannot change:

* Variable name
* Type or Record name
* Function or DLL function
* Import module name
* Label name
* Record field name
* Named constant
* Enumeration name

Only variables can be assigned to or modified. Seems obvious really. This allows more specific bytecodes to be generated, and enables more error-checking. And, importantly, means that if you've seen a function F defined, you then F will always be that function.

If a variable function name is needed, you create a reference to it; so if P, Q are variables and F, G etc are function names:

    P := F
    Q := (F, G, H)        # list of functions

Now P works like a function name does in Python (however, some features of functions are not available when using P, such as keyword arguments).

### Variant and Pack Types

Q directly supports both its own Variant types, and low-level types as used in M (ie. the sort that are used in C and C-APIs). This is is unusual in script languages, which usually need add-ons or some strange syntax to manage the latter.

They are called 'Pack' types simply because the raw primitives are packed together in arrays and structs.

### Variant Types

All objects in Q are tagged with their type. These are the types available:

* **Integer** (i64, or less commonly u64)
* **Real** (r64, ie. floating point)
* **Bignum** (arbitrary precision integer or float)
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

* Signed integers **i8 i16 i32 i64 i128**
* Unsigned integers **u8 u16 u32 u64 u128**
* Bit types **u1 u2 u4** (for Array elements, or Pointer targets)
* Float types **r32 r64**
* Zero-terminated string field (of a struct)
* Counted string field
* Pointer to zero-terminated string
* **Struct** of packed types, including fixed-sized arrays and other structs
* Fixed-size **Array** of packed types, including other fixed-size arrays and structs
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

### Compile Ahead of Time

All Q versions since the start have required ahead of time to explicit byte-code:

* Originally, a module at a time to an explicit bytecode file, with hot-loaded modules. So while running a Q program, another module could be edited, compiled and run
* More recently, a whole program at a time. Here, there is no hot-loading. The single bytecode file was a complete, self-contained representation of a program. Only the interpreter (another single file) was needed to run the application

I've now made it a little more dynamic: it still compiles a whole program, but always from source, and to internal bytecode. However, I am looking at being able to run multiple programs, some of which can be 'hot-loaded' like the original, and which can share data via a common environment.

### Interaction with M Host Language

Q is intended to me used more as an embedded language to an application. Q programs can easily call functions in the host M program, so for example if fann() is a function in M, then it called like this in Q:

    host.fann(10)
  
    fann(10)           # or like this given a declaration 'hostproc fann' or assignment fann:=host.fann.

In M, fann() is defined like this:

    exportq fann(int n)int = ...

All details of parameter and return types, argument checking etc, are taken care of.

### Interaction with C-API Libraries

An FFI for such libraries is built-in, but details of the functions (the bindings) all have to be specified. Example:

    importdll msvcrt =     # mapped to libc.so.6 on Linux
        function printf(string, ...)int32
        ....
    end

Usually such declarations are in a Q module, and that module is imported. So if the above is inside clib.q, then a program uses it like this:

    import clib
    
    printf("Hello")

There is no need to write clib.printf (unless printf clashes with something else).


### Interaction with M Libraries

If the M functions are not part of the host, but in a separate DLL library, then a similar importdll block to the above is needed.

However, when the library is built with the M compiler into a DLL, then it automatically creates a .exp exports file, intended for use from M programs. But the same file can be used by Q, using:

    importx lib

It need to use importx otherwise it will look for lib.q rather than lib.exp. (M also needs importx, there it would otherwise look for lib.m instead of lib.exp. lib.m would be the name of the implementation module, not the interface file.)

So, again, for talking to M, no messy declarations need to be written. Usually pack types are used for parameter passing, with checking and conversions done as needed.

Variants can be passed and returned too, allowing more flexibility in Q code, but then the M implementation code becomes harder to write.




