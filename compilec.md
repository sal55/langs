## What Makes C Hard to Compile

This is my personal view of what makes it hard to compile, based on my experience of trying to do so. Of course, it can't be that hard since there are plenty of compilers that have managed it; it's not impossible. But I'm comparing with other compilers for languages of my own (see [compilem.md](compilem.md) where I go through all these points and show how my regular languages deals with them).

It's a combination of quirks, difficult features, inconsistencies, ancient baggage, things you disgree with ... but these will all be listed.


### A Large Existing Code Base

With C, there are billions of lines of code that it has to be able to deal with.

### The Competition

A big chunk of those billions of lines were probably designed for and developed with gcc. Gcc has had teams of developers working for 30 years to iron out most of the bugs and cover the hundreds of corner cases. The rest were probably developed with Visual C.

### The Preprocessor

I'll get this out of the way early. This is a huge obstacle to get over, which you find in few other languages. It took me a month to get something passable, but there are plenty of examples it won't compile or produces different results from the main compilers. It also makes accurate reporting of errors much harder.

Some examples follow.

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
This is 'int abc;'. Out of dozens of tokenisers I must have written, it has never occurred to me to have line breaks inside an identifier!

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
It means you can't just convert to an integer or float representation and discard the original text; you have to keep the original in case it will be processed by a macro like this.

### The white space between a macro name and its arguments

If M is a function-like macro, then it can be invoked as M, with no expansion, or as M(x,y).

There can be white-space between "M" and "(", but how much do you peek ahead to check that "(" follows? At the character level this lookahead can be tricky. It turns out there can be an unlimited amount of white space including newlines and comments, but not macros/includes that expand to empty. I only allow 0 or 1 spaces and it generally works, but...

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

So x has one element and a bit. This part of C, allowing unstructured init data, I refused to support. Besides, the algorithm the standard describes for dealing with it is complex.

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

The same applies to static; it can go anywhere, but they have to go before the name being declared and its modifiers.

### int long unsigned long
```
unsigned long long int
unsigned long int long
int long unsigned long
long long unsigned
etc.
```
Any combinations are valid, including those that don't have 'int'. In how many different ways can you specify one basic type? That's not counting 'const uint64_t', but that's usually defined on top of something like the above anyway.

### How many consts do you need?
````
const const const const int const const const spam;
````
Perfectly valid. But why does C allow so many? One argument is to allow types generated by macros. But then those could have duplicate static, int etc.

### And where does it go?

In the above example, const can before or after the type, or inside in the case of 'int const long', or all three!

However I believe that where pointers are involved it (or they) go after the "\*", so apply to the previous \*, not any following \*.

### int, long and long long

Considering currenrt 32/64-bit systems, int is generally 32 bits, and long long is 64 bits. So where does 'long' fit in?

While it is generally 32 bits EXCEPT for 64-bit Linux, 32-bit long is incompatible with 32-bit int.

I've chosen to make 'long' a synonym for 'int' for my Windows compiler where long is always 32 bits. But it means int* and long* are compatible, when they shouldn't be.

### char, unsigned char and signed char

Despite 'char' necessarily being either signed or unsigned, these are actually 3 different types, not two. char*, signed char* and unsigned char* are all incompatible types. I didn't know about this until much later, but would have made the same decision in making 'char' a synonym for one of the other two.

Initially I chose 'unsigned char', but after coming across programs that relied on char being unsigned, I had to change it to signed char. (Where promptly a few other programs gave compiler errors!)

### Bitfields Rule

I don't think C specifies how these are laid out except they are implementation defined. But in that case anything goes, including making each a regular int as I do. But people do expect them to be narrow.

The trouble is that different compilers gives different results with certain combinations of successive bitfields depending on the base type used, even across different versions of gcc!

One major library (GTK) uses bitfields in its interfaces. Possibly it works by luck, because the combinations used don't trigger the anomaly. (Access to the bitfield values is not the issue, but it affects the overall struct sizes.

Use a larger struct size, as determined by *your* compiler, compared with the internal struct size set by the compiler used for the pre-built binarie, and things can go wrong.)


### VLAs, Variable Length Arrays

On the face of it they sound simple (and in my language, they would be), but:

* The VLA parts actually refer to the *type*, not the variable. So a VLA can be used in a typedef, and the typedef can be used for instantiating multiple variables, at different times, and with possible values of the expressions of the dimensions
* VLAs can be declared in a loop, with different sizes each time
* VLAs can apparently be part of a struct. The mind boggles at how that might work when the structs are allocated on the heap and formed into linked lists and arrays.
* VLAs can be condionally declared
* Because of block scopes, there can be multiple VLAS in effect, with nested lifetimes, and multiple possible future VLAs
* Because of 'goto' and 'break', it is possible to enter and leave blocks containing active VLAs (entering might be restricted, but then it is your job to detect such infringements)
* 'sizeof' is now no longer a compile-time constant, but is worked out at runtime. (And can involve a calculation for multi-dimensional VLAs)
* Multi-dimemnsional VLA access requires now multiply by variables, not constants.

Still think they are a simple feature? I decided not to support them.

### Variable types

This is associated with VLAs. I'm refering to code like this:
````
    void fn(int m, int m, int A[m][n]);
````
A is a flat 2D array, but with variable dimensions linked to earlier parameter names. (Here, the type of A is int(\*)\[\], or possible int(\*)\[n\], and possible only the last dimension needs to be linked as it affects indexing calculations.


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
This is famous for being convoluted, fortunately parsing it is easier than writing it or reading it (about which other languages can that be said?)

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

### Typedef an actual function
````
    typedef int Fn(int a, int b);
````
This is not a function pointer, but a function. It creates a typedef for its signature. It can be used like this:
````
    Fn add, sub;           # function declarations
````
What is usually not supported, is to define functions. However my compiler (also Tiny C, two of the smallest) do allow this:
````
    Fn add{return a+b;}
    Fn sub{return a-b;}
````
This is just an example of something that few would ever have expected typedef to do.

### () parameter lists
This is legal C:

    void fn():
It means a function that takes an unspecified number of arguments of unspecified type. The compiler will not check. Further most compilers allow fn() to be called in inconsistent ways.

The issue here is whether to support such an unsafe feature. The problem is that so many programs wrongly use this form when they mean no parameters, rather than (void) (in C++ () does mean (void)). If not supported, many programs will not compile.

So this generally allowed unless explicitly disabled. Which adds to the problem, as the practice of using () in place of (void) is perpetuated.

I chose not to allow it, then had to backtrack and make it available, but via a legacy option ("-old") only. This means the compiler effectively biting its lip and saying nothing while knowingly passing obviously wrong code. But at least the default mode is the safe one.


### Old style parameter lists
Example:
````
void fn(a,b,c)
    int a,b,c;
{ ...
````
Well, this is an easy one - just don't bother supporting it . Except you won't be able to compile ancient programs. And if you do support it, now you have a alternative function syntax to support.

### Function don't have their own syntax

A function definition uses the same syntax as type declarations. This is odd if you're used to languages where functions are not variables. So:
````
    int F(void);                  # declaration an actual function (defined later, or externally)
    int (*G)void;                 # define a function pointer, ie. a varible
````
Not much of a difference. How about:

    void A(void), B(int), *C(char*);             # declare 3 functions, all sharing the same base type
    void A(void){}, B(int x){}, *C(char* s){};   # Not allowed

You have to specifically disallow defined several functions in a comma-separated list.


### Mixed Arithmetic

The general rule is, if you mixed signed and unsigned in a binary operation, then it will be done as unsigned. But when you look into the actual rules, it is not that simple. The combinations for all 8 integer types are are as follows (S = gives signed result; U = unsigned result):
````
      u8 u16 u32 u64  i8 i16 i32 i64

  u8   S   S   U   U   S   S   S   S
  u16  S   S   U   U   S   S   S   S
  u32  U   U   U   U   U   U   U   S
  u64  U   U   U   U   U   U   U   U

  i8   S   S   U   U   S   S   S   S
  i16  S   S   U   U   S   S   S   S
  i32  S   S   U   U   S   S   S   S
  i64  S   S   S   U   S   S   S   S 
````
I recently tested my compiler against this; is was close, but not enough. Although it is rare that actually makes a different, sometimes it will.

### Block Scopes and Declare Anywhere

Many languages have block scopes, but none of mine don't, so something else that makes it harder. I like all declared names to be identified by a fully qualified name. That's not possible with block scopes because they are anonymous; how do you disambiguate the 28 entities in one function call called 'A'? So this required a special approach (and a block number attribute per variable).

As for declare anywhere, that means that here:

    int A; {int X = A+1; float A; ....}

there are two As in scope during the same block.

### double x; ++x

++ and -- work on integers, or so you might have thought. In C, they work on floats too! This is not hard to deal with - once you know. But it's extra work. Hardware doesn't have special instructions for it, so it just means x+=1.0. I didn't bother with it because I disagreed with the use of ++ and -- which I believe should be for stepping between discrete values of a type.

### Break: Loop or Switch?

Well, exactly that: does 'break' means step to the end of the switch statement, or the end of the loop body?

### Switch-case

Everyone knows how bizarre Switch is, and how case labels can appear anywhere, at any level, within the statement that follows. As, actually, can 'default:', which doesn't need to be at the end:
````
    switch (1) default: case 10: case 20: switch (2) default: case 30: case 40:;
````
You just need make sure there is at most one default per switch, and make sure case labels are associated with the right switch.


### Extra Namespaces

This is another unusual feature: struct tags exist in their own namespace, one shared with enum tags. And labels exist have their own namespace, how useful!
````
A: struct A;int A; goto A;
````
So an identifier within a function, not only has a block number, now might also be in one of three namespaces: normal, tags and label.

This is little bit harder than my normal compilers where there can only one ONE instance of any identifier in a function, in ONE namespace. Compared with C's THREE namespaces, two of which can have UNLIMITED instances (label names have function-wide scope; at last something sensible!).

### Declare Structs anywhere
How to describe this messy bit of language design. These are some examples (not all will be valid in the same program):
````
struct S;                              # incomplete struct
struct S x,y;                          # this one needs to be reported
struct {int x,y;};                     # anonymous struct, with no instances (compilers expected to report this)
struct S {int x,y;};                   # define a tagged struct, no instances
struct {int x,y;} p,q;                 # define anonymous strict with instances
struct S {int x,y;} p,q;               # same but tagged
typedef struct S {int x,y;} P;         # same struct now defines a typedef

struct {
    int a,b;
    struct {int x,y;};                 # doubly anonymous struct
} p,q;

struct {
    int a,b;
    struct {int x,y;} c,d;             # locally defined anonymous struct
} p,q;

struct {
    int a,b;
    struct S c;                        # use either struct S, or P, for same type
    P d;
} p,q;
````
You might say, so what? Well you have these combinations:
* There may or may not have a struct tag
* There may or may not any members (the bit inside {...}. If so, then it defines a new struct, otherwise it's either a forward declaration, or using a struct already define.
* There may or may be be instances defined. If not, this may be an anonymous struct inside another struct.
* It may be defining a typedef.

Suppose 'struct S' has been defined, should the following work, and if so, what does it do:
````
struct {
  struct S;
} p = {10,20};
````
Apparently this does work, it creates an anonymous struct, which has two elements 'x' and 'y'.

Except it doesn't work on my compiler (nor on an older 2004 compiler 'DMC'). if it's all so simple, why has it taken 3 years to establish that this is not supported? C has all these freedoms and all this flexibility, but then the onus is on implementors to ensure all the possible combinations work.

### Implicit int
That is declarations like these at module scope:
```
    a,b,c;
    fn(void);           # most likely it will fn(), so a free-for-all
```
Or inside a function:
    
    G(10,20);
where no declaration for G has been provided. The compiler assumes the types in all cases are 'int'. I think the parameters of G too, although if I mix inconsistent calls, gcc will not report that, so may it assumes 'int G()'.

What's difficult about this? Like the above, it is about whether to support this side of the language, and how far to go in reporting such uses.

### 17 (or so) Precedence Levels

OK, this is not really that difficult to compile, apart from having to have 17 different levels of handling (and probably duplicated inside the preprocessor, although that misses out some ops).

### A = &A
Try:
````
    bool A;
    A = &A;
````
Normally, if A is of type T, then &A is of type T*. This seems anti-intuitive. I came across this recently within an example that was more like:

    struct S* p;
    bool b = p;

This really means 'b = p ? true : false', or perhaps 'b = !!p' will work. I think this would be a better fit for this language.

While this can be routine compilation, I nevertheless decided not to bother.


### Standard Headers
How much of an implementation should a compiler provide? What about standard headers? When I complained about Clang not having its own headers, or missing from a particular distribution of gcc/mingw, I was told that headers are separate from a compiler.

Yet all small Windows C compilers are self-contained, with their own headers. Where do the headers come from; shouldn't there be a standard set that comes with the platform, that all compilers share?

Apparently not. Neither is it practical to just borrow headers belonging to other compilers, as it doesn't work. Especially the bigger ones, the headers are full of implementation-specific macros and features, many built-in to the host compiler.

So in my case I had to make my own. And they are still incomplete, things get added as needed. Looking at other compilers' headers is soul-destroying, many are just patchworks of #if/#ifdef blocks, macros and typedefs.

They seem fond of inventing gratuitous typedefs (look at 'struct stat'), and you have to track down which of multiple conditional possibilties they might be.

With windows.h, that has been a complete slog getting it together, adding functions, types, macros as needed. I worked from other windows.h files (many comprise 100s of separate headers), online resources, test programs on other compilers, DLL dumps.

Every application that uses either the standard library or windows.h, just HAS to seek out every obscure function there is. At the moment, my windows.h is about 2,000 lines in one file. Windows.h for other compilers range from 20,000 to 200,000 unique lines, in up to 165 different headers.

### Standard Library

I decided to just use the C library that comes with Windows, msvcrt.dll. Although not official, every Windows system will have it.

But if you do have to provide one, well this particular library provides about 1500 functions, a lot of work!

### const attribute

Not my favourite feature. Although there is a very easy way to deal with 'const', ie. just ignore it completely, people do expect it to provide some protection.

It complicates the type system, and allows conversions in some cases but not others. So, passing 'char\*' to a 'const char*' parameter is OK, but not 'char \*\*' to 'const char \*\*'.

### Predefined Macros
Specifically implementation-specific ones that you are going to come across in application code:
````
#if !defined(_STDINT_H_) && (!defined(HAVE_STDINT_H) || !_HAVE_STDINT_H)
    #if defined(__GNUC__) || defined(__DMC__) || defined(__WATCOMC__)
        #define HAVE_STDINT_H   1
    #elif defined(_MSC_VER)
````

What if the code assume a set of compilers, where will yours fit into it?


