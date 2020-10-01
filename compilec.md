## What Makes C Hard to Compile

This is my personal view of what makes it hard to compile, based on my experience of trying to do so. Of course, it can be that hard since there are plenty of compilers that have managed; it's not impossible. But I'm comparing with other compilers for languages of my own.

It's a combination of quirks, difficult features, inconsistencies, ancient baggage, things you disgree with ... but these will all be listed.

### A Large Existing Code Base

With your own private language, you might only have a codebase of 10s or 100s of thousands of line of code to deal with. With C, there are billions of lines of code that it has to work with.

### The Competition

A big chunk of those billions of lines were probably designed for and developed with gcc. Gcc has had teams of developers working for 30 years to iron out most of the bugs and cover the hundreds of corner cases. The rest were probably developed with Visual C.

### The Preprocessor

I'll get this out of the way early. This is a huge obstacle to get over, which you find in few other languages. It took me a month to get something passable, but there are plenty of examples it won't compile or produces different results from the main compilers. Some examples follow.

### Identifiers and Keywords can be split across lines

This is something I only discovered three years after finishing my compiler:
````
i\
n\
t a\
\
b\
c\
;
````
This is 'int abc;'. Out of dozens of tokenisers I must have written, it has never occurred to me have have line breaks inside an identifier!

### Octals, Hex and Floats

These sound straightforward: 01234 is octal; 0x5678 is hex, 123e45 is a float. Or would be in any sane language. In C, they can be arguments to a macro:
````
    #define DEC(x) 1##x
    #define NAME(x) A##x
    #define STR(x) #x
    
    DEC(01234)              // forms the decimal number 101234
    NAME(123e45)            // forms the identifer 'A123e45'
    STR(0x5678)             // forms the string "0x5678"
````
It means you can't just convert to an integer or float represent and discard the original text; you have the original in case it will be processed by a macro like this.

### The white space between a macro name and its arguments


### Macros in include file names



### The Algorithm for finding an include file

### Struct and Array Initialisers shapes

### Initialising a char-array

### Where does the typedef go?

### int long unsigned long

### How many consts do you need?

### int, long and long long

### char, unsigned char and signed char

### Bitfields Rule

### VLAs

### Variable types

### When int[] isn't an array

### Repeated declarations

### Type declaration syntax

### () parameter lists

### Old style parameter lists

### 0x123e+2

### Mixed Arithmetic

### Block Scopes and Declare Anywhere

### double x; ++x

### Break: Loop or Switch?

### Switch-case

### Struct Tags

### Enum Tags

### Labels have their own namespace

### Declare Structs anywhere

### Implicit int

### 17 Precedence Levels

### Standard Headers

### windows.h

### const attribute

### __GNUC__ __MSVER__ ?

### Typedef an actual function
