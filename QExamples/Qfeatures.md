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

### Summary of Basic Features

This is a list of what I consider basic features, that are often lacking in other scripting languages, or need add-on modules to implement:

* Pack types (often called C-types), mentioned above
* Named constants
* Static identifiers (that is, having proper variables, functions as mentioned)
* Case-insensitive source code - this is friendlier than case-sensitive
* Algol/Pascal-style syntax (this is used also by Lua etc but many use brace style, or Python-style)
* Number bases from 2 to 16 (apart from base 10, others have mixed support for 2, 8 and 16) ...
* ... plus the means to print in those bases
* Separators in numeric literals
* Some numeric suffixes, eg '10 million'
* *Simple* Enumerations
* Tabledata - used to define enumerations with corresponding parallel data
* Direct FFI for C-based APIs
* Bit-sets as used in Pascal, with logical bit operations
* Bit-arrays
* Bit and Bitfield operations (A.\[i\] and A.\[i..\j]
* Built-in records and structs, with constructors: d:=date(31,12,1999)
* As well as a standard int64 type, it has the less common word64 (u64) type
* Arbitrary precision integer *and* float type, using decimal.
* Print/Println as built-in *statements*
* Read/Readln as built-in *statements*
* Operators such as min, max, which can be used in augmented assignments: x min:= y
* Static variables inside functions
* Well-behaved default values on function parameters
* Character constants: 'A' is 65. In Python and Lua, 'A' is a string; you need ord('A') or string.byte('A') to convert
* Multi-character constants: 'ABCDEFGH' is an int value
* Separate Proc and Function keywords
* Start() and Main() functions: start() will be automatically executed is present in a module. main() will be automatically executed (ahead of start()) is this is the main module
* Goto
* Loop Break, Continue (called exit and next) and Redo from nested loops
* Swap operator
* Reference parameters
* Pointers and address-of operator
* Dedicated loop statements for end-less and repeat-N-times loops
* Switch statement (working with int index and constant case-expressions (here, when-expressions)
* Case statement for any type and variable expressions, but sequentially tested not instant.
* 2-way and N-way selectors
* View-based slicing of strings, lists, arrays
* Strinclude directive incorporates any text or binary file as a string literal
* Stop or Stop N as a quick way to stop execution
* Type conversion and type-punning
* Basic maths functions are built-in operators (just say sin(x), not math.sin(x))
* Built-in constants such as *pi*
* Augmented assignment like a+:=b (not present in Lua) and increment ++a (not present in Python)
* Assigments and increment inside expressions
* The whole syntax makes statements and expressions interchangeable (perhaps the only FP-like feature).
* Scope control: at module level, use 'global' attribute to export a function, variable, type etc.

### Summary of Missing Features

This is a list of popular language features which Q is never going to have. Mainly because I don't understand them, don't see the point, or think they are bad for a language. Many I also consider hard to implement:

To save some research, let's just say this is all the advanced features of Python plus a raft of buzzwords such as lambdas (and its calculus!), currying, pretty much anything to do with functional programming, anything involving elaborate type systems, and going over the top with concepts such as enumerations)

Also missing are extensive libraries. There is one small support library, otherwise there of libraries I found personally useful.

### Some Non-Implemented Features

Things which I don't have a strong objection to, but which aren't currently implemented. Some have existed in the past:

* List-comprehensions (was not used enough)
* Exception handling for user-code events (I had a simple version at one time, but required a different approach to error-handling than I was comfortable with)
* Exception handling for internal errors (I couldn't get this to work reliably; I will try again)

