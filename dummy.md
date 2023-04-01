

* Algol/Pascal Style Syntax (but without begin-end which are as bad as braces)

* Sane left-to-right type syntax; uses `=` for equality and `:=` for assignments; smaller and more intuitive set of operator precedences

* Most semicolons (here used to separate statements) can be omitted

* `func` (and `proc`) designators. In C functions are not marked at all; you have to infer where they are!

* All language features are always available. No need micromanage using C's 30 standard headers.

* Properly structured `switch` for integer control and constant test values

* `case` statement, like `switch` but works with any types and with non-constant test values

* Proper for-loops that iterate over an integer range, or the values in list. Plus dedicated endless loops and repeat-n-times loops. With of course multi-level breaks, *and* an optional `else` part

* Just one top-level namespace (no separate namespace for tags and labels)

* No block scopes (my preference); there is one scope per function

* Case insensitive (my preference)

* 1-based arrays by default (0-based and N-based are possible too)

* Modules with circular references. Module can introduce namespaces

* Define-once: everything is defined in only one place. (No separate interfaces and and definitons; interfaces are only needed between programs)

* Designed for whole-program compilation ('program' = one EXE or DLL file)

* Out-of-order definitions

* Tidy set of one-token basic types (eg. `u64` rather than 16 combinations of `long long unsigned [int]`) (C has uint64_t in stdint.h, but not really supported by the language.)

* Default 64-bit ints and floats (C could have 64-bit `int`, but is typically 32 bits, which means32-bit literals and a discontinuity between 32 and 64 bits)

* Separate Pointers and Arrays. (If `P` is a pointer-to-array in C, you need `(*P)[i]` to access an element: deref then index. But C also allows `*(P[i])`: index then deref. That is insane,and highly unsafe)

* Arrays with value semantics: `[10]int a, b; a := b`

* Slices (pointer and length pair)

* Properties such as `X.max`, (max value); `X.bytes` (byte size); `X.bits` (bitwidth); `X.len` (length of array); `X.typestr` (name of type), where `X` is a type or expression

* Built-in `read` and `print` statements
 
* Functions: keyword parameters and optional parameters with default values. (These two can be superimposed on imported functions defined via an FFI whose implementation language does not support it.) 

* Reference parameters

* Multiple function return values, and multiple assignments

* Expresson-based: statements and expression are interchangeable

* Named constants. C offers some combination of `#define`, `enum`, `const`, and soon `constexpr`, none of which tick all the boxes

* Table data: initialise parallel arrays, and optional enums, in table form. C requires hideous 'x-macros` to do a similar thing

* Bit and bitfield accesses: `A.[i]` and `A.[i..j]`

* Embedded text and binary files. (Apparently coming in C23.)

* Separators in numeric literals (hasn't quite made it to gcc 12.2 anyway)

* Chained comparisons: `if a = b = c`, range checks: `if a in b..c`, and check against N values: `if a in [b, c, d]`

* Extra built-in opss such as `max`, `max:=`, and fully overloaded `**` and `abs`.

* Far less UB than in C

* Compiler options to run directly from source, or produce a single amalgamated source file for a whole application.


I've listed 36 items; I could do 50 more. 
