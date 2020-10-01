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

If M is a function-like macro, then it can be invoked as M, with no expansion, or as M(x,y).

There can be white-space between "M" and "(", but how much do you peek ahead to check that "(" follows? At the character level this lookahead can be tricky. It turns out there can an unlimited amount of white space including newlines and comments, but not macros/includes that expand to empty. I only allow 0 or 1 spaces and it generally works, but...

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


If you write the data properly structured with internal {}, then C will warn about too many initialisers or extra {} etc. But have FEWER {} than are needed, and all checking is off. The data is just a linear stream of values like you might find in 1970's BASIC or FORTRAN, that can be used to initialise any data structure involving nested arrays and structs, no matter what the layout. Only the types have to be suitable.

The correct formatting would be:

    T x[] = { {{10,20,30}, {40,50,60}}, {{70,80}} };

So x has one element and a bit. This aspect, I refused to support.

### Initialising a char-array
Consider:
````
    char *s = "ABC";
    char t[]= "ABC";
````
"ABC" has type 'char*' in both case, but the left sides have types 'char*' and 'char[]' respectively. One of those doesn't match! Maybe you can argue that char[] 'decays' to char* in an expression, but you can't write 't = "ABC"'.

### Where does the typedef go?
This is well known:
````
    typedef const unsigned long int T;
````
The following less so:

    const unsigned long typedef int T;

The same applies to static; it can go anywhere, but they have to before the name being declared and its modifiers.

### int long unsigned long
```
unsigned long long int
unsigned long int long
int long unsigned long
long long unsigned
etc.
```
Any combinations are valid, including those that don't have 'int'. (Here's where I long for my own language where specifying a simple built-in type isn't such a palaver.)

### How many consts do you need?
````
const const const const int const const const spam;
````
Perfectly valid. But why does C allow so many? One argument is to allow types generated by macros. But then those could have duplicate static, int etc.

### And where does it go?

In the above example, const can before or after the type (as well as both!), or inside in the case of 'int const long'.

However I believe that where pointers are involved it (or they) go after the "\*", so apply to the previous \*, not any following \*.

### int, long and long long

Considering currenrt 32/64-bit systems, int is generally 32 bits, and long long is 64 bits. So where does 'long' fit in?

While it is generally 32 bits EXCEPT for 64-bit Linux, 32-bit long is incompatible with 32-bit int.

I've chose to make 'long' a synonym for 'int' for my Windows compiler where long is always 32 bits. But it means int* and long* are compatible, when they shouldn't be.

### char, unsigned char and signed char

Despite 'char' necessarily being either signed or unsigned, these are actually 3 different types, not two. char*, signed char* and unsigned char* are all incompatible types. I didn't about this until much later, but would have made the same decision in making 'char' a synonym for one of the other two.

Initially I chose 'unsigned char', but after coming across programs that relied on char being unsigned, I had to change it to signed char. (Where promptly a few other programs gave compiler errors!)

### Bitfields Rule

I don't think C specifies how these are laid out except they are implementation defined. But in that case anything goes, including making each a regular int as I do. But people do expect them to be narrow.

The trouble is that different compilers gives different results with certain combinations of successive bitfields depending on the base type used, even across different versions of gcc!

One major library (GTK) uses bitfields in its interfaces. Possibly it works by luck, because the combinations used don't trigger the anomaly. (Access to the bitfield values is not the issue, but it affects the overall struct sizes.

Use a larger struct size, as determined by *your* compiler, compared with the internal struct size set by the compiler used for the pre-built binarie, and things can go wrong.


### VLAs, Variable Length Arrays

On the face of it they sound simple (and in my language, they would be), but:

* The VLA parts actually refer to the *type*, not the variable. So a VLA can be used in a typeded, and typedef can be used for instantiating multiple variables, at different times, and with possible values of the expressions of the dimensions
* VLAs can be declared in a loop, with different sizes each time
* VLAs can be condionally declared
* Because of block scopes, there can be multiple VLAS in effect, with nested lifetimes, and multiple possible future VLAs
* Because of 'goto' and 'break', it is possible to enter and leave blocks containing active VLAs (entering might be restricting, but then it is your job to detect such possibilities)
* 'sizeof' is now no longer a compile-time constant, but is worked out at runtime. (And can involve a calculatoion for multi-dimensional VLAs)
* Multi-dimemnsional VLA access requires now multiple by variables, not constants.

Still think they are a simple feature? I decided not to support them.

### Variable types

This is associated with VLAs. I'm refering to code like this:
````
    void fn(int m, int m, int A[m][n]);
````
A is a flat 2D array, but with variable dimensions linked to earlier parameter names. (Here, the type of A is int(\*)\[\], or possible int(\*)\[n\], and possible only the last dimension needs to be linked as it affects indexing calculations.

What the hell happened to the nice, simple little language supposedly so easy to compile?

### When int[] isn't an array
````
    int A[]            // array of int
    void fn(int A[])   // pointer to int
````
But this discrepancy only affects the first dimension; the remaining ones are always arrays.


### Repeated declarations
At the module level, you can declare the same thing as many times as you like, provided it is not initialised or defined:
```
int spam, spam, spam, spam;
int spam, spam;
int spam;
int spam;
struct S;
struct S;
```
Some may be static, some not (but you have to check for consistency). At most one must be initialised. In the case of arrays, they all need to have the same dimensions, unless the dimension is unbounded (int A[]). Function declarations too:
```
void fn(int a,int b);
void fn(int b,int a);
```
The types need to match (char will not match signed char nor unsigned char), but not parameter names. A function declaration can follow a definition.


### Type declaration syntax
This is famous for being convoluted, fortunately parsing it is easier than writing it or reading it (have I really just said that?!)

Well, apart from the fiddly aspects of reading a type that might be (1) self-contained, as in a cast, or parameter with no name; (2) wrapped around a name; (3) spread out in three parts as in a normal declaration:
````
    const int unsigned A, B, **C[10][20]
````
where the 'const int unsigned', '\*\*' and '\[10\]\[20\]' have to be collated into one type (the first part is the base type, the others are modifiers).


### Empty declarations
That is:

    int;
    
Rarely used, and useless, yet some compilers support them. Maybe they can be used with macros that may or may not yield a name?

But it is a choice to be made. Presumably the grammar allows it, and it you follow the grammar, you might necessarily support it.

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
