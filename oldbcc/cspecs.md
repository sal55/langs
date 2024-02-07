### C Language Specs

This is an attempt to define the C language subset that MCC is comfortable dealing with.

Some features of C (whether C89, C99 or C11), I don't intend to support, some I'm planning to, or currently have limited or buggy support.

This doesn't mention temporary omissions in supplied headers, unless I decide for example that something like PRIxxx will not be supported.

A lot of these mean that arbitrary programs either will not compile, or will need to be modified to compile. However the resulting code should still be compatible with other compilers.

**Structs** Struct assignment works. I believe that in the revised code generator (8.10 onwards) struct expressions, struct passing and struct returns work. Not sure about applying member selection directly to returned struct values. Passing method for structs of 1,2,4,8 bytes don't use a standard convention so may be problems passing structs to foreign C functions.

**VLAs** Variable Length Arrays will not be supported. Nor the ability to specify a dynamic length for array types occuring in parameter lists, including the use of [*] and [static].

**\_Complex** Type will not be supported.

**long double** type is mapped to double.

**\_Bool** Type is recognised, but it mapped to (unsigned) char. Otherwise it is not supported.

**long int** type is recognised, but will be mapped to int on Windows. This means that int* and long* are compatible.

**char** type is always unsigned. There are only unsigned and signed char; there is no third 'plain' char type.

**Designated initialisers** are not supported yet.

**_Generic** is not supported yet (I'm never going to use it; it just seems interesting to implement).

**Compound Literals** are not supported yet, and will be restricted I think (no runtime elements)

**Initialised data** is still limited: data inside {...} must be constants only. Expressions involving addresses can only have a simple form.

**#if expressions** will use signed integers only

**\_\_DATA\_\_** and some similar macros not yet supported (they are recognised, but just return a message). Those for time and date will involve OS dependencies, of which MCC is currently free.

**Non-void functions** must end in a return statement.

**No implicit declarations** Variables, functions and parameters *must* have a base type. Functions must be declared or defined before use.

**Unicode** This is not supported, although L"abc" string constants are recognised (but generate an 8-bit string).

**Preprocessor** Some preprocessor aspects may not be implemented properly, for example 0x1234e-4 is parsed as (0x1234e minus 4), or 0x1234A, but C says it should be an illegal preprocessor token. If imaginative use of the preprocessor fails to yield the expected results, then it needs to be simplified (rather than hacking mcc's preprocessor and making it even more fragile).

**Bitfields** are recognised but not supported (they are just ints).

**\_\_MCC\_\_** This macro is set to 1 by MCC, so that code which is specific to mcc can be made conditional.

**#pragma pack** Only #pragma pack(1), and #pragma pack(pop) are recognised.

**stdint.h** is provided, by doesn't yet have the full range of types. Basically, signed and unsigned 8, 16, 32 and 64-bit ints are declared.

**Callback functions** Functions called from a foreign function (ie. any function, in C or otherwise, not compiled with mcc) should have a callback attribute. Implemented here using '#pragma $callback' just before it's first declared. This is specific to x64, but harmless elsewhere. **Note** compiling existing code without $callback, can crash if those functions are called from outside (I will try and fix this if the next phase when I try build a project in-memory).

**extern** declarations inside a function may not work.

**Linkage** Consistent attributes such as 'static' are required otherwise there will be an error.

**windows.h** A full set of headers for programming Windows is not provided. There is a windows.h header, but it only has a few hundred lines of declarations that are added as needed for specific programs. A full windows.h is a massive task, unless there is a way of automating the generation of via another compiler with a full windows.h.

**Type widths** Probably, this compiler will standardise on char=8 bits, short=16 bits, int=32 bits, and long long=64 bits. (long cannot be fixed like that as it might need to be 32 bits on Windows, and 64 bits on Linux.) These four types (excluding long) will always correspond to the fixed-width types in stdint.h.

**Label pointers** I *might* add these. They are not standard C, but are supported by gcc and clang.

**inline** and noreturn, volatile etc are recognised, but ignored.
