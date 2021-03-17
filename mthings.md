### Annoying things about C

#### (And how it works in my language)

* Braces as block delimiters (too insubstantial in my view for use across multiple lines)

(I use Algol-style syntax using block delimiters then ... end)

* Inconsistent use of braces; sometimes used, sometimes not

(Eg. if-then-else is if a then b;c else d end always

* Too many opportunities for errors caused by braces that or may not be there (eg. Apple bug), and dangling else

(Doesn't happen)

* The same } block delimiter used for all statement types

(Block delimiter can be 'end' or can be specific: end if, end while etc, but I also use 'od' for loops, 'fi' for ifs. See example code)

* Need to type semicolons despite 99% of all code being line-oriented

(Newlines are interpreted as ";" unless line clearly continues to next, eg. ends with ( or ,). So ";" hardly ever needs to be typed)

* Some uses of {} need to be followed with ";" and some don't; what are the rules?

(Not applicaable)

* Extra ";" are usually ignored, but here it changes the behaviour of the program:
````
    for (i=0; i<N; ++i);
    {
        printf("%d\n",i);
    }
````

(Can't happen, a loop HAS to have 'do' to start the block, error if missing)

* For some reason, you can't have a label just before "}", you have to write: LAB:;} with a semicolon

(Doesn't happen, labels are allowed anywhere)

* // comment syntax has flaws, eg the 2nd line is commented out here
````
    puts("One");  // File c:\abc\def\
    puts("Two");
````

(Doesn't happen)

* Non-nesting /\*..\*/ have further opportunities for silent bugs:
````
  puts("One"); /* ... 
  puts("Two"); /* ... */
````
(Doesn't happen)

* Limited support for binary literals (for a language that is 'close to the metal')

(I use 2x1011 for binary literals)

* No support for odd number bases other than 8, 10, 16

(I support bases 2 to 16, eg. 7x100 for base-7; this is the number 49 decimal)

* Leading zeros on numbers turn them into octal.

(Doesn't happen)

* No separators for numeric literals

(I allow _ and/or ' as separators)

* No raw string literals

(F"abc\def" is a raw string; the \ is part of the string not an escape)

* Allows = and == in conditionals, which are easily mixed up

(I use = for equality and := for assignment)


* Multi-dimensional index needs the fiddly-to-type A\[i\]\[j\]\[k\] instead of the more fluid A\[i,j,k\]

(I allow A[i,j,k])

* Case-sensitive, so need to remember if it was OneTwo or oneTwo or OneTwo or Onetwo

(All my languages are case-insensitive.)

* Multi-character constants like 'ABCD' not well-defined, and they stop at 32 bits

(Well defined in mine, and you can write 'ABCDEFGH' for 64 bit values, and 'ABCDEFGHIJKLMNOP' for 128 bits)


* Uses such impossible, convoluted, inside-out type declarations, that special algorithms and web-sites (cdecl.org) need to be used to sort them out

(I use simple Algol68-style left-to-right declararions; see examples)

* int* p, q, r doesn't declare the expected 3 pointers

(I'd write 'ref int p, q, r' to declare three pointers)

* Three identical arrays 'int a\[10\], b\[10\], c\[10\]' need the array dim three times (I know about typedef; but it's just a workaround)

(I'd write [10]int a,b,c for three identical arrays)

* Here: 'int a, b, (\*c)\[10\]', the type of c is in three parts: the * before the name; the \[10\] after the name, an the 'int' all the way at the start of the line

(Doesn't happen; 'c' has to be declared separately (as it's a completely different type!) as '[10]ref int c' - array 10 of pointer to int.)
 

* Basic types are char, short, int, long, long long; FIVE types to represent the FOUR common basic int types of 8, 16, 32, 64 bits. 5 into 4 doesn't go...

(I have i8 i16 i32 i64 for those 4 types, or u8 u16 u32 u64 for unsigned. With alias; see M docs)

* These types are poorly defined: long may or may not be the same width as int. Even if it is, int\* and long\* are incompatible. Sometimes long and long long are the same width.

(Doesn't happen)

* The basic char type might be signed or unsigned. But whichever it is, char\* is incompatible with both signed char\* and unsigned char\*

(I have a 'char' type - separate from byte which is same as u8 - which is unsigned.)

* C99 introduced int32_t, uint8_t etc. Great. Except they are usually defined on top of int, char, etc.

(Not relevant in M)

* Also, if you want to print a type like int64_t, printf still expects %d, %ld, or %lld; which to use? It's %ld on Linux64; %lld anywhere else. You are expected to use PRI64d etc, little-used macros

(If A is a ANY integer types, I just write 'print A' - what could be simpler?)

* Also, if you wanted to write an int64_t constant, you still need to choose between 123L and 123LL (there are little-known macros to deal with these, but this whole system of integer types is a huge mess)

(My constants are int64 anyway, but to force a different type I just cast: u64(123).)

* On the subject of printf, how crass is it to have to provide format codes to tell a compiler what it already knows: the type of an expression?

(Doesn't happen)

* How do you even determine the format code if you don't know the type? Code for clock_t anyone? size_t? (I think that one is %zu (of course!), but not universal.) It's crazy.

(Doesn't happen; just print X)

* OK, you figure out your expression (eg. i64\*u16+u32) is %lld. Then your revise your declarations, and/or the expression, and you have revise all printfs?

(Doesn't happen)

* The maximum value of int? That will be INT_MAX. Or MAX_INT. Or something. But how about INT32_T or UINT64_T?

(Maximum value is int.max. For any numeric type T, just write T.min or T.max, or T.bounds to get the range T.min..T.max, eg, 'x in int32.bounds')

* Suppose you want to find the maximum value a variable X can have; you need to go and dig up its type, and hardcode INT_MAX (etc). Then you change X's type...

(X.max etc)

* How many bits in a byte? Oh, C doesn't have bytes. How big is a char then? Well, it depends, but it will be CHAR_BIT

(8 bits - ALWAYS)

* How many bits in long long int? Only sizeof(long long int)\*CHAR_BIT.

(Use int.bitwidth etc)

* How many elememts in this array: 'int MyLongishArrayName\[\] = {....}'. Only sizeof(MyLongishArrayName)/sizeof(MyLongishArrayName\[0\]). Sheesh.

(MyLongishArrayName.len)

* How to have an array lower-bound other than 0? Oh, you can't. A bummer if you need to port a 1-based algorithm.

(My arrays can have any lower bounds, but defaults to 1; eg. [37..100]int A or ['A'..'Z']int count)

* Manipulate an array by value? You can't.

(Arrays are manipulated by values; except passing or returning arrays has to be done via pointers behind the
 scenes because the ABI says so)

* Arrays and Pointers are crazily mixed up. Create a pointer to array, so you need to dereference then index to get the element. Do index then dereference by mistake; it still works! (Ie. it compiles, but will probably crash, if you're lucky.)

(Arrays and pointers are distinct; what C allows can't happen)

* Pointers can be indexed just like arrays: 'int \*P; P\[i\]; int A; &A\[12345\]'

(Doesn't happen)

* Despite pointer-arrays being available within the language, eg. type T(*)[] for pointer to array of T, standard C idiom is to almost exclusively use T*,
 ie pointer to T, for dynamically allocated arrays or for parameter parsing. This can give rise to lots of confusion and a range of undetectable errors (eg.
C allows ANY pointer, whether it's to an array or not, to be index). However the syntax for pointer-to-array is ugly: (*A)[i], which is the likely reason.

(Doesn't happen; you define proper pointers to arrays for function parameters)

* Functions are not marked with a keyword; you have to disentangle type declarations, and/or take cues from indentation, to find out where a function even starts

(I use 'function' and 'proc')

* Subroutines that return no value have no special syntax, except they start with 'void'.

(I use 'proc' for those.)

* Function params with same type have to repeat the type: (int a, int b, int c) instead of (int a, b, c)

(I allow (int a,b,c)

* Array syntax int A[10], which normally defines a sequence of 10 ints, inside a parameter list as (int A[10]) now becomes a pointer to int! But only the first level

([10]int A, as I write it, is ALWAYS an array)

* Mix signed/unsigned integers, the result will be usually unsigned, but the rules are complex, depending on the sizes of the operands among other things. (Good luck with that format code to print such a result.)

(Mixed arithmetic is always signed. Only unsigned/unsigned is unsigned)

* BTW the results of mixed arithmetic are in this table; S means signed result; u means unsigned; the width will be 32 bits unless at least one operand is 64 bits:
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

( In mine, the upper left block is all Us; the rest is all Ss)

* Call a function F like this: F(x). Or like this (\*F)(x). Or this (\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*F)(x). C doesn't care.

(Doesn't happen)

* Syntax is a free-for-all: A(B) might call function A with parameter B, or declare B with type A.

(Doesn't happen)

* A\*B might multiply A by B, or define B with type pointer to A.

(Doesn't happen)

* Define a 'const unsigned long int' like that. Or as 'int long unsigned const'. Or 'as const const long const unsigned int const'. You can use as many const as you like, how handy.

(There is no const. There is no separate signed attribute; there are no length qualifiers; see my docs)

* How about const int const \* const \* const \*; which const applies to which part?

(Doesn't happen)

* Everyone knows you write typedefs as 'typedef const long int T'. How many know it can also be 'const long int typedef T'?

(Doesn't happen. I always write 'type T = <typespec>')

* Few know you can typedef an actual function: typedef int fred(int,int). (Try it.)

(Doesn't happen)

* Switch is totally crazy. Switch is followed by one statement, usually compound, and case labels can literally go anywhere even inside deeply nested statements

(My switch is well-behaved with a sane syntax)

* 'default:' can go anywhere, not just at the end

(I use a normal 'else' block, which is always last)

* Misspell 'default:' as 'defualt:', and its still valid; just now a wrong program

(Doesn't happen; misspell 'else', you get an error, not a label!)

* You can't do "case 'A'..'F'" (except when extensions exist). You can't do 'case 10, 20, 30:'

(I allow A..B and A,B,C,D)

* The code for each case: block automatically falls through to the next case. Very handy, except that 99% of the time you don't want that,
and have to explicitly write 'break'. (How switch-case works in C makes it harder to fix, as you want both cases here: case 10: case 11: \<code\> to share he same code, but that means fallthrough after case 10:)

(No break is needed. There is no fallthrough (but there is a new feature called 'recase'))

* Switch only works with an integer index, and constant case values.

(For non-integer and/or non-constant, I use the Case statemnt)

* Declare a function, and it's exported by default; you have to add 'static' to make it local (few people do this). Same with variables.

(Everything is local unless exported from a module using 'global')

* As well as it being too easy to inadvertently export functions and variables, if you build a shared library, then quite likely they will be exported to othr program to

(I use 'export' to export from a module /and/ a program)

* Missing function keyword parameters

(I have keyword parameters)

* Missing default function parameter values

(And default values)

* There are no proper reference parameters

(And proper references)

* This is no proper if-elsif-elsif-else statement, it is only if-else-if-else (the latter has a nested structure, the former is linear)

(I have if-elsif-else)

* There is no dedicated for-loop for simple iteration. What there is is another free-for-all, with everyone trying to cram as much into a for-header as they can. C might as well not have a while statement!

(My for-loops only do simple iterations over integer ranges or values)

* A simple iteration over A..B inclusive (for i=A,B in Lua), is for(i=A; i<=B; ++i), with the loop index written 3 times (the number of types I get it wrong), an everything spelled out exactly, as though you have to instruct a compiler just how a for-loop needs to be implemented.

(I write 'for i in A..B do', or 'for i:=A to B do')

* There is no 'named constant' feature. You have to choose between '#define' (no scope, reevaluates the expr etc), 'enum' (ints of 32 bits only), 'const int' (can't be in switch, can't be used for array bounds, or will advertently create VLAs)

(I use 'const A = 100')

* There are about 30 standard headers. You can have to keep adding tiny headers and sometimes taking them out again as code changes. It's a bloody nuisance; and which one is memset defined in again?

(For basic language features, no imports are needed. To access named library functions, there ar 3 main std libraries)

* 'extern' defines external functions and variables, except when they are defined in this module (ie. one in a shared header) ...

(Doesn't happen)

* ... and except where 'extern' is used inside a function, to declare a variable defined outside the function, but is static (see K&R2 page 31).

(Doesn't happen)

* Some compilers allow 'extern int A = 1234;'

(Doesn't happen)

* 'extern int A; static int A' appears to be legal; I wonder what it means?

(Doesn't happen)

* All compilers allow 'int A,A,A,A,A,A,A,A' at file scope.

(Doesn't happen)

* Define 'int ABC' in one module, and 'char\* ABC' in another, and that is an undetectable bug

(Doesn't happen; you have to use 'global' to export ABC, and it cannot clash with that ABC exported from elsewhere.)

* Functions need to be declared before use. So this either means defining them in a certain order in a module, or needing to use prototypes (ie. forward declarations) ...

(Doesn't happen; everything can be declared out-of-order; no forward or prototype declarations are ever needed; define once.)

* ... or the compiler might assume an implicit declaration

(Doesn't happen)

* Functions shared across modules usually need declaring in one place, and defining in another. Extra maintenance, and more annoyance. And opportunities for things to go wrong.

(Doesn't happen; define in one place, always.)

* Struct tags are a totally useless feature, except that you need them for self-referential pointers. And, weirdly, they live in their own namespace, one shared with enum tags.

(No struct tags)

* Struct declarations are another mess: 'struct tag {int a,b;}; declares a type. 'struct {int a,b} x; declares a type of sorts and a named instance. There are all sorts of combinations and many ways to declare anonymous struct definitions.

(Doesn't happen; structs (records) can only be declared in a Type or Record statement.)

* Structs are automatically padded according to arcane rules of the C language \[however, C is dominant that machine ABIs are now being based around existing practice in C\]

(No automatic padding. The $Caligned attribute is needed when a layout needs to match a struct used in a C library)

* Operator precedences: far too many, and many non-intuitive, so A<<B + C actually means A<<(B + C).

(There are about half the number of precedences, and are more intuitive: A<<B+C means (A<<B)+C)

* What is the relative precedence of bitwise ops &, | and ^? Why do they need to be different?

(They have the same precedence.)

* The really odd "->" operator, probably introduced because "(\*P).m" is too unwieldy compared with "P->m". But introduce one more ptr level, and you need "(\*PP)->m" anyway.

(-> doesn't exist. You write P^.m officially, but ^ can be omitted so you just type P.m)

* You can't break out of a loop more than one deep.

(I can exit from nested loops)

* You can't break out of a loop more anyway if the break statement is inside a switch statement (because, incredibly, C decided to overload the 'break' statement to do two different things)

(Doesn't happen)

* No way to redo a loop iteration, and no Python-style 'else' part

(I use 'redo'; for-loops can have an 'else' part)

* Reading numbers from console or file? No chance using scanf, it's too complicated! And inflexible.

(I use readln A,B,C)

* The 2-way selection operator ?: doesn't need parentheses, so nobody uses them, making it hard to see what's happening esp. with nested ?: as the precedence rules are too hard

(I write it as (a|b|c))

* There is no N-way selector

(I write that as (n | a,b,c,.... | z)

* The comma operator - what a waste of time. And people like to exploit it to save having to add {...}.

(I use (a;b;c))

* You can't use chained operators like a==b==c in a way that would be useful.

(I allow a=b=c and it means what you expect: test all are equal. Or a<=b<=c etc)

* There is no proper power operator like \*\*, and not one for integers AFAIK

(I have **, and it is properly overloaded for ints and floats)

* There is no proper abs operator (there are functions, and you have to use the right abs function for each kind of int or float; a palaver). A built-in abs would come with automatic overloading

(I have abs as an actual, overloaded operator)

* No direct type-punning. If the expression is an l-value, you have to do it by \*(T\*)&X.

(I type-pun even rvalues using T@(X)

* For some weird reason, labels have their own namespace: L: int L; goto L;

(Doesn't happen)

* I hate block scopes (this is not just a C thing). Only one global namespace at the module level, but inside a function,which usually you want to keep short, you can have millions. Or be able reuse the same 'A' identifier any number of times with different types.

(M doesn't have block scopes; only one scope within a function body(

* Having declarations in the middle of a block means that there are can be two instances of 'A' visible within the block, one from an outer scope, one after the declaration.

(Doesn't happen)

* Macros: C programmers do like to show off their prowess at undecipherable macros. It also makes it impossible to look at a piece of code and know what it will do. Especially popular when used in function declarations, if you can even recognise them as such.

(Doesn't happen; there is a simple macro system, but is well-behaved)

* Take 0x123D+2, ie, add 2 to 0x123D to get 0x123F. Not try it with 0x123E+2, which will often be an an illegal preprocessor token).

(Doesn't happen)

* Undefined behaviour. No further comment needed.

(Many C UBs are well-defined. Eg. signed integer overflow.)

* No bit or bitfield indexing, you have to use shifts and masks.

(I use A.[i] and A.[i..j])

* No built-in 'swap' feature

(I use swap(a,b))

* No built-in min and max operators

(I use max(a,b), also a max:=b)

* Take the array element A\[i\]\[j\], and write it as j\[i\[A\]\] (ie. as 2 1D array accesses instead of 1 2D access); it still works

(Doesn't happen)

* Functions decls with a () parameter list instead of (void) are valid. They mean that any number an types of arguments can be passed, unchecked. You can even mix and match: 'F(); F(123); F("one", "two", 3.0)' are all valid. For a language known to be unsafe anyway....

(Doesn't happen)

* Quirks just keep coming up all the time, things you didn't know where possible. Keyword/identifiers split across multiple lines using backslash...

(Doesn't happen)

* ... Macros inside #include statements: '#define HDR <file.ext>', '#include HDR', this will work (and the macro is stored as separate tokens "<", "file", "." and "ext"). You never know what to expect. C is supposed to be small and simple!

(Doesn't happen)

* Talking of includes: the rules for locating a file in an include file-specifier are more complicated than you'd think. You need to consider rel/abs paths, current dir, current stack of include files... Actually they are implementation defined

(Import statements take a module name, not a path, with simpler search rules.)

* Most C compilers seem to be incredibly laid-back, oblivious to serious errors unless you twist their arm. Look at the following function; there's something missing - a return statement which returns the pointer to allocated memory. Without that, it will return garbage. Yet most compilers will say nothing or merely warn, unless you pile on the options. What is the matter with them!
````
void* checkedmalloc(size_t n) {
    void *p = malloc(n);
    if (p==NULL) {
        puts("Malloc failure");
        exit(1);
    }
}
````

(My language requires a return statement)

* Initialise an object such as an array of a 3-element struct like this:
````
    T x[] = {{1,2,3}, {4,5,6}, {7}};         // 3 array elements, the last partial
````
But then try taking out some of the braces:
````
    T x[] = {1,2,3,4,5,6,7};
````
It still works. Which bit of data is initialising which part of the object? How many elements in the array?

C doesn't care with too few braces, it just sees a linear sequence of values. It only cares if there are too many.

(Doesn't happen; the initialisation data must exactly match the shape of the type being initialised)
