Annoying things about C

* Braces as block delimiters (too insubstantial in my view for use across multiple lines)

* Inconsistent use of braces; sometimes used, sometimes not

* Too many opportunities for errors caused by braces that or may not be there (eg. Apple bug), and dangling else

* The same } block delimiter used for all statement types

* Need to type semicolons despite 99% of all code being line-oriented

* // comment syntax has flaws, eg the 2nd line is commented out here
````
    puts("One");  // File c:\abc\def\
    puts("Two");
````
* Non-nesting /\*..\*/ have further opportunities for silent bugs:
````
  puts("One"); /* ... 
  puts("Two"); /* ... */
````
* Limited support for binary literals (for a language that is 'close to the metal')

* No support for odd number bases other than 8, 10, 16

* Leading zeros on numbers turn them into octal.

* No separators on numeric literals

* No raw string literals

* Allows = and == in conditionals, which are easily mixed up

* Multi-dimensional index needs the fiddly-to-type A\[i\]\[j\]\[k\] instead of the more fluid A\[i,j,k\]

* Case-sensitive, so need to remember if it was OneTwo or oneTwo or OneTwo or Onetwo

* Multi-character constants like 'ABCD' not well-defined, and they stop at 32 bits

* Uses such impossible, convoluted, inside-out type declarations, that special algorithms and web-sites (cdecl.org) need to be used to sort them out!

* int* p, q, r doesn't declare expected 3 pointers

* Three identical arrays 'int a\[10\], b\[10\], c\[10\]' need the array dim three times (yes I know about typedef; but it's just a workaround)

* Here: 'int a, b, (\*c)\[10\]', the type of c is in three parts: the * before the name; the \[10\] after the name, an the 'int' all the way at the start of the line!

* Basic types are char, short, int, long, long long; FIVE types to represent the FOUR common basic int types of 8, 16, 32, 64 bits. 5 into 4 doesn't go!

* These types are poorly defined: long may or may not be the same width as int. Even if it is, int\* and long\* are incompatible. Sometimes long and long long are the same width.

* The basic char type might be signed or unsigned. But whichever it is, char\* is incompatible with both signed char\* and unsigned char\*!

* C99 introduced int32_t, uint8_t etc. Great. Except they are usually defined on top of int, char, etc.

* Also, if you want to print a type like int64_t, printf still expects %d, %ld, or %lld; which to use? It's %ld on Linux64; %lld anywhere else. You expected to use PRI64d etc, little-used macros

* Also, if your wanted to write an int64_t constant, you still need to choose between 123L and 123LL (there are little-known macros to deal with these, but these whole system of integer types is a huge mess)

* On the subject of printf, how crass is it to have to provide format codes to tell a compiler what it already knows: the type of an expression?

* How do you even determine the format code if you don't know the type? Code for clock_t anyone? size_t? (I think that one is %zu (of course!), but not universal.) It's crazy.

* OK, you figure out your expression (eg. i64\*u16+u32) is %lld. The your revise your declarations, and/or the expression, and you have revise all printfs?

* The maximum value of int? That will be INT_MAX. Or MAX_INT. Or something. But how about INT32_T or UINT64_T?

* Suppose you want to find the maximum value a variable X can have; you need to go and dig up its type, and hardcode INT_MAX (etc). Then you change X's type...

* How many bits in a byte? Oh, C doesn't have bytes. How big is a char then? Well, it depends, but it will be CHAR_BIT

* How many bits in long long int? Only sizeof(long long int)\*CHAR_BIT.

* How many elememts in this array: 'int MyLongishArrayName\[\] = {....}'. Only sizeof(MyLongishArrayName)/sizeof(MyLongishArrayName\[0\]). Sheesh.

* How to have an array lower-bound other than 0? Oh, you can't. A bummer if you need to port a 1-based algorithm.

* Manipulate an array by value? You can't.

* Arrays and Pointers are crazily mixed up. Create a pointer to array, so you need to dereference then index to get the element. Do index and dereference by mistake; it still works! (Ie. it compiles, but will probably crash, if you're lucky.)

* Pointers can be indexed just like arrays: 'int \*P; P\[i\]; int A; &A\[12345\]'

* Functions are not marked with a keyword; you have to disentangle type declarations, and/or take cues from indentation, to find out where a function even starts!

* Subroutines that return no value have no special syntax, except they start with 'void'.

* Function params with same type have to repeat the type: (int a, int b, int c) instead of (int a, b, c)

* Array syntax int A[10], which normally defines a sequence of 10 ints, inside a parameter list as (int A[10]) now becomes a pointer to int! But only the first level

* Mix signed/unsigned integers, the result will be usually unsigned, but the rules are complex, depending on the sizes of the operands among other things. (Good luck with that format code to print such a result.)

* Call a function F like this: F(x). Or like this (\*F)(x). Or this (\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*F)(x). C doesn't care.

* Syntax is a free-for-all: A(B) might call function A with parameter B, or declare B with type A.

* A\*B might multiply A by B, or defined B with type pointer to A.

* Define a 'const unsigned long int' like that. Or as 'int long unsigned const'. Or 'as const const long const unsigned int const'. You can use as many const as you like, how handy.

* How about const int const \* const \* const \*; which const applies to which part?

* Everyone knows you write typedefs as 'typedef const long int T'. How many know it can also be 'const long int typedef T'?

* Few know you can typedef an actual function: typedef int fred(int,int). (Try it.)

* Switch: the craziness of this has been mentioned. Switch is followed by one statement, usually compound, and case labels can literally go anywhere even inside deeply nested statements

* You can't do "case 'A'..'F'" (except when extensions exist). You can't do 'case 10, 20, 30:'

* Misspell 'default:' as 'defualt:', and its still valid; just now a wrong program

* Switch only works with integer index, and constant case values.

* Declare a function, and it's exported by default; you have to add 'static' to make it local (few people do this). Same with variables.

* Missing function keyword parameters

* Missing default function parameter values

* There are no proper reference parameters

* This is no proper if-elsif-elsif-else statement, it is only if-else-if-else (the latter has a nested structure, the former is linear)

* There is no dedicated for-loop for simple iteration. What there is is another free-for-all, with everyone trying to cram as much into a for-header as they can. C might as well not have a while statement!

* A simple iteration over A..B inclusive (for i=A,B in Lua), is for(i=A; i<=B; ++i), with the loop index written 3 times (the number of types I get it wrong), an everything spelled out exactly, as though you have to instruct a compiler just how a for-loop needs to be implemented.

* There is no 'named constant' feature. You have to choose between '#define' (no scope, reevaluates the expr etc), 'enum' (ints of 32 bits only), 'const int' (can't be in switch, can't be used for array bounds, or will advertently create VLAs)

* There are about 30 standard headers. You can have to keep adding tiny headers and sometimes taking them out again as code changes. It's a bloody nuisance; and which one is memset defined in again?

* 'extern' defines external functions and variables, except when they are defined in this module (ie. one in a shared header) ...

* ... and except where 'extern' is used inside a function, to declare a variable defined outside the function, but is static (see K&R2 page 31).

* Some compilers allow 'extern int A = 1234;'.

* 'extern int A; static int A' appears to be legal; I wonder what it means?

* All compilers allow 'int A,A,A,A,A,A,A,A' at file scope.

* Define 'int ABC' in one module, and 'char\* ABC' in another, and that is an undetectable bug

* Functions need to be declared before use. So this either means defining them in a certain order in a module, or needing to use prototypes (ie. forward declarations)

* Functions shared across modules usually need declaring in one place, and defining in another. Extra maintenance, and more annoyance. And opportunities for things to go wrong.

* Struct tags are a totally useless feature, except that you need them for self-referential pointers. And, weirdly, they live in their own namespace, one shared with enum tags.

* Struct declarations are another mess: 'struct tag {int a,b;}; declares a type. 'struct {int a,b} x; declares a type of sorts and a named instance. There are all sorts of combinations and many ways to declaration anonymous struct definitions.

* Structs are automatically padded according to arcane rules of the C language (a bugbear for the 25 years I've been using C APIs from my language)

* Operator precedences: far too many, and many non-intuitive, so A<<B + C actually means A<<(B + C).

* The really odd "->" operator, probably introduced because "(\*P).m" is too unwieldy compared with "P->m". But introduce one more ptr level, and you need "(\*PP)->m" anyway.

* You can't break out of a loop more than one deep.

* You can't break out of a loop more anyway if also inside a switch statement.

* No way to redo a loop iteration, and no Python-style 'else' part

* Reading numbers from console or file? No chance using scanf, it's too complicated! And inflexible.

* The 2-way selection operator ?: doesn't need parentheses, so nobody uses them, making it hard to see what's happening esp. with nested ?: as the precedence rules are too hard

* There is no N-way selector

* The comma operator - what a waste of time. And people like to exploit it to save having to add {...}.

* You can't use chained operators like a==b==c in a way that would be useful.

* There is no proper power operator like \*\*, and not one for integers AFAIK

* There is no proper abs operator (there are functions, and you have to use the right abs function for each kind of int or float; a palaver). A built-in abs would come with automatic overloading

* No direct type-punning. If the expression is an l-value, you have to it by \*(T\*)&X.

* For some weird reason, labels have their own namespace: L: int L; goto L;

* I hate block scopes (this is not just a C thing). Only one global namespace at the module level, but inside a function,which usually you want to keep short, you can have millions. Or be able reuse the same 'A' identifier any number of times with different types.

* Having declarations in the middle of a block means that there are can be two instances of 'A' visible within the block, one from an outer scope, one after the declaration.

* Macros: C programmers do like to show off their prowess at undecipherable macros. It also makes it impossible to look at a piece of code and know what it will do. Especially popular when used in function declarations, if you can even recognise them as such.

* Take 0x123D+2, ie, add 2 to 0x123D to get 0x123F. Not try it with 0x123E+2, which will often be an an illegal preprocessor token).

* Undefined behaviour. No further comment needed.

* No bit or bitfield indexing, you have to use shifts and masks.

* No built-in 'swap' feature

* No built-in min and max operators

* Take the array element A\[i\]\[j\], and write it as j\[i\[A\]\] (ie. as 2 1D array accesses instead of 1 2D access); it still works!

* Functions decls with a () parameter list instead of (void) are valid. They mean that any number an types of arguments can be passed, unchecked. You can even mix and match: 'F(); F(123); F("one", "two", 3.0)' are all valid. For a language known to be unsafe anyway....

* Quirks just keep coming up all the time, things you didn't know where possible. Keyword/identifiers split across multiple lines using backslash...

* ... Macros inside #include statements: '#define HDR <file.ext>', '#include HDR', this will work (and the macro is stored as separate tokens "<", "file", "." and "ext"). You never know what to expect. C is supposed to be small and simple!

* Talking of includes: the rules for locatinf a file in an include file-specifier are more complicated than you'd think. You need to consider rel/abs paths, current dir, current stack of include files... Actually they are implementation defined

