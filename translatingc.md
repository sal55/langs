## Translating C to Another Language

Here are some issues with that, even when the target is a similarly lower-level and 'unsafe' language like mine.

### You May Need a C Preprocessor

Either implement one, or employ an existing compiler to do that. However:

* C code may have conditional blocks which depend on things like environmental factors (which C compiler) or D-macros; which ones will you plug in?
* A **lot** of C code uses **#define**s; preprocessing will eliminate all those, leaving plain literals everywhere (too few programs use enums for named literals)
* It will also inline macros such as min/max, giving bloated code.
* Some code over-uses macros. which expands to bloated C that is not meant to ever be read. It will end up as gobbledygook in the target
* Preprocessing will also expand all headers used, including giant ones like windows.h and gtk.h. Each module may have half a million lines of declarations, and 100 lines of actual program!
* What to do about predefined macros such as \_\_LINE\_\_ or \_\_FILE\_\_?


### You Need To Implement Half of a C Compiler

Unless you can somehow utilise an existing one, that can generate a representation that you can use. But that doesn't make it much simpler! (I think Zig uses Clang; neither are small products, nor simple.)

### How Far Do You Go Translating Headers?

* This has already come up with preprocessors. But generating proper code may require types, enums, imports, variables etc that are defined in a header.
* What about system headers? Surely the target will not require all those fiddly details like the half-dozen versions of 'struct stat'?
* The same headers used in multiple modules will result in many duplicates sets of declarations

### Case-Sensitivity

C is case-sensitive; what happens when the target is case-insensitive (like mine)?

### Independent Modules

* The information you get from C will be from individual modules.
* You will need the precise set of source modules comprising the project to do a proper translation
* For, you may need, as well as the compiler front end, a tool to emulate the build system used (make, cmake etc) or somehow extract that information
* Even with all info collated, to have to cross-reference names, see who owns what, see what is either imported from an external library, or may be missing cpmpletely

### The Small Stuff

* C integer types are poorly defined. While **int** is usually 32 bits, **long** varies across machines; what are you supposed to do with a **long** type?
* How do you translate a **char** type? It is technically neither **unsigned char** nor **signed char**
* C conflates arrays and pointers too much. P\[i\], is allowed, so is A\[i\]. The target will not allow that. Since most C arrays in expressions reduce to a pointer, it may be necessary to turn everything into explicit pointer/offset accesses
* Most C struct layouts follow struct rules for alignment and padding. It is necessary duplicate the exact same layout in the target
* There are too many C operator precedences, and they're unintuitive. It may be necessary to just put parentheses around everything. Trying to avoid when the expression structure happens to match the precedences of the target is too error prone
* C's for-loops are a free-for-all. If the target does not support such a crude construct, they will need emulation with while-loops, or even gotos
* C's switch statement is another unstructured mess. Either perform analysis to see if fits into the properly structured version of the target, or emulate using gotos. But this loses the benefit of using switch.
* C may use VLAs that may not exist in the target (they don't in mine).
* C has a complex set of rules regarding mixed use of signed/unsigned integers, how they are widened and converted. You will to exactly emulate that behaviour, namely this:
````
       u8  u16 u32 u64  i8  i16 i32 i64

   u8   S   S   U   U    S   S   S   S
  u16   S   S   U   U    S   S   S   S
  u32   U   U   U   U    U   U   U   S
  u64   U   U   U   U    U   U   U   U

   i8   S   S   U   U    S   S   S   S
  i16   S   S   U   U    S   S   S   S
  i32   S   S   U   U    S   S   S   S
  i64   S   S   S   U    S   S   S   S
````
This only shows whether a combination is treated as signed or unsigned.
* **goto**: the target had better have this, or it will have its work cut out!
* **setjmp** and **longjmp**: urghh...
* Struct tags? Best to eliminate them I think. They don't exist anywhere else.
* Block scopes; some languages (eg. mine) don't have them. Together with being case-insensitive, this may mean 100 distinct versions of an identifier in a function, to be emulated using only one identifier in the target
* Label names can also clash as, in C, "L" can be used as both a variable/function name, and a label name, in the same scope
* Functions declared with an empty parameter list can be called with unchecked arguments. How to emulate this in a proper language?
* C's namespace handling (putting aside it unusual tag and label namespaces) is crude. Non-decorated/non-mangled names are used between modules, ie. in linker space, which may clash with other modules in the target language. (This is made worse because all functions/variables at module scope in C are exported unless made static)
* With things like printf, you can choose to keep those calls in, relying on the format codes being accurate. My experience however is that they are troublesome, and something you don't really want in a target language if it has something better or safer.


