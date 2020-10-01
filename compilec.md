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

If M is a function-like macro, then it can be invoked without arguments as just M, when it is not expanded, as with arguments: M(x,y).

There can be white-space between "M" and "(", but how much do you peek ahead to check that? At the character level this lookahead is tricky. It turns out there can an unlimited amount of white space including newlines and comments, but not macros/includes that expand to empty. I only only 0 or 1 spaces and it generally works, but...

### Macros in include file names

Who even knew you could do that? Who ever does? And how does it work? Examples:
````
   #define INCL <stdio.h>
   #include INCL
````
Here, the macro is made of the 5 tokens < stdio . h >

And here:

    #define INCL <1234.56.78>

It has tokens < 1234.56 78 >; one of those is a floating point number. One extra reason to keep as text, you don't want rounding errors inside file names!

There are probably other rules, but who cares. I did some of this just to tick a box.

### The Algorithm for finding an include file

I'd been using my compiler for months until I tried a program where it couldn't properly locate the include files. It turns out the algorithm for this is implementation defined. That doesn't help when a program has been developed with compiler X, which uses one method, and then later it fails with Y.

Here are the inputs:

* The places where it will look for <...> includes
* The places where it will look for "..." includes, which incorporate any -I options to the compiler
* The location of the current nested include file, the one that contains the "#include" we're processing
* Possibly, the whole stack of previous include locations
* The path inside the <...> or "...", which can be single file, or complex path; absolute or relative.

So it can't find the file at path specified (using different logic between <...> and "..."); where does it look next?

You'll find that one algorithm works for application A but fails for B, and vice versa.

### Struct and Array Initialiser Shapes

This is one of those jaw-dropping moments when you start to realise the sort of language you're dealing with. Start with:
````
typedef struct { int a,b,c;} S;
typedef struct { S s1,s2;} T;
````
Now initialise an array of S using the 'correct' way:
````
S x[] = {{10,20,30}, {40,50,60}, {70,80}};
````
3 entries, the last incomplete so padded with 0s. Now... get rid of all those internal {}s:
````
S x[] = {10,20,30, 40,50,60, 70,80};
````
It still works! Further, keep it the same but change it to an array of T:
````
T x[] = {10,20,30, 40,50,60, 70,80};
````
This is fine too! But how how many elements of x are there? You can't really tell.

If you write the data properly structured with internal {}, then C will warn about too many initialisers or extra {} etc. But have FEWER {} than are needed, and all checking is off. The data is just a linear stream of values like you might find in 1970's BASIC or FORTRAN. The correct formatting would be:

    T x[] = { {{10,20,30}, {40,50,60}}, {{70,80}} };

So x has one element and a bit. This aspect, I refused to support.

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
