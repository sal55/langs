## What Makes C Hard to Compile Part II


This goes through the other points and shows how they compare to my normal language.

(The comparison is not exact as it is a little higher level with its own features which may be harder to implement.)

### A Large Existing Code Base

The existing code base is 100-150Kloc.

### The Competition

There isn't any. Mine will always be the best compiler.

### The Preprocessor

There isn't one. There are embryonic macros, but AST-based not like the CPP.

### Identifiers and Keywords can be split across lines

Not possible.

### Octals, Hex and Floats

Octals, Hex and Floats will always be exactly that. Octals are written 8x377 not 0377.

### The white space between a macro name and its arguments

Macro expansion is done in a later pass, by AST manipulation. A macro call is parsed just like a function call.

### Macros in include file names

That can't happen.

### The Algorithm for finding an include file

There is an algorithm for module imports, which looks in a linear set of locations. No file paths are involved, and there is no concept of
a 'current' include file, so is straightforward.

In the case of a rarer textual include which does uses paths, the path does not form a new source file context, so the scope for searches does not change.

### Struct and Array Initialiser Shapes

These have to strictly follow the type structure. No exceptions.


### Initialising a char-array

Initialising has to be done like this:

    []char S = ('A','B','C')

Since this can tedious, I created special string constants:

    []char S = a"ABC"          # ('A', 'B', 'C')
    []char S = z"ABC"          # ('A', 'B', 'C', 0)
    
These a"..." and z"..." strings have the correct type.

### Where does the typedef go?

I have a 'type' statement and the keyword always goes on the left:

    type T = int
 
 
### int long unsigned long

All basic types are a single identifier

### How many consts do you need?

No 'const' in the language, as least in the C sense. Some places use/will use 'let' (or 'in' for parameter lists) which goes on the left, and written once.


### And where does it go?

See above.

### int, long and long long

Not relevant. I use 'int' to mean i64. The full set of signed integers is i8 to i128 with no gaps or overlaps.


### char, unsigned char and signed char

I have one 8-bit 'char' type which is unsigned.

### Bitfields Rule

I have a different scheme for bitfields where there is no ambiguity, and with precise control. (And there are anyway other direct methods of working with the bits of an integer, such as A.\[8..31\] for 24-bit field. One of those more more advanced features, but highly worthwhile.)

### VLAs, Variable Length Arrays

No VLAs.

### Variable types

No variable types.


### When int[] isn't an array

Here \[\]int is always an array. To make use of a pointer to array as a parameter, I'd use:

     ref[]int A           # explicit
     []int &A             # implicit reference parameter

In both cases, there is an actual array involved, unlike idiomatic C which uses pointers to the array's element type.


### Repeated declarations

Not allowed. Everything is always defined once. No separate declarations are needed.


### Type declaration syntax

Mine is very simple, written left-to-rght and self-contained.

### Empty declarations

Not allowed.


### Typedef an actual function

Not allowed.

### () parameter lists

Not allowed.


### Old style parameter lists

Not relevant.


### Function don't have their own syntax

Functions have a separate syntax:

    function F:int = ...        # actual function
    ref function:int G          # function pointe

### Mixed Arithmetic

Mixed arithmetic uses signed unless both are unsigned.

The table of possibilities is very regular, and can be reduced to:
````
         Uxx   Ixx
  Uxx    U     S
  Ixx    S     S
````


### Block Scopes and Declare Anywhere

No block scopes.

Things can be declared anywhere, but always have function-wide scope, so no competing lifetimes with similarly names entities.


### double x; ++x

Not allowed. ++ is defined on integers/pointers only.

### Break: Loop or Switch?

Not relevant. Switch doesn't need 'break'.

### Switch-case

My Switch syntax is well-formed. My 'default:' is written as 'else', and forms the last branch, just like statements such as 'if'.


### Extra Namespaces

There is one namespace inside functions. Struct tags don't exist.


### Declare Structs anywhere

I've already shown how it works:
````
record P =
    int x,y
end
````
Instances can only be created like this:
````
P a,b
````

### Implicit int

Not relevant.

### 17 Precedence Levels

There are fewer precedence levels.

### Standard Headers

Not relevant. Basic language features do not need dozens of tiny headers to be specified, or the headers to exist.

### Standard Library

Not relevant, as I can define the library as I like. If I want to use C via a FFI, then I just do:

    import clib


### const attribute

No 'const'.

### Predefined Macros

Not relevant.
