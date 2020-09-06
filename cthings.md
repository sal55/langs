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
* Non-nesting /*..*/ have further opportunities for silent bugs:
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

* Multi-dimensional index needs the fiddly-to-type A[i][j][k] instead of the more fluid A[i,j,k]

* Case-sensitive, so need to remember if it was OneTwo or oneTwo or OneTwo or Onetwo

* Multi-character constants like 'ABCD' not well-defined, and they stop at 32 bits

* Uses such impossible, convoluted, inside-out type declarations, that special algorithms and web-sites (cdecl.org) need to be used to sort them out!

* int* p, q, r doesn't declare expected 3 pointers

* Three identical arrays 'int a[10], b[10], c[10]' need the array dim three times (yes I know about typedef; but it's just a workaround)

* Here: 'int a, b, (*c)[10]', the type of c is in three parts: the * before the name; the [10] after the name, an the 'int' all the way at the start of the line!

* Basic types are char, short, int, long, long long; FIVE types to represent the FOUR common basic int types of 8, 16, 32, 64 bits. 5 into 4 doesn't go!

* These types are poorly defined: long may or may not be the same width as int. Even if it is, int* and long* are incompatible.

* The basic char type might be signed or unsigned. But whichever it is, char* is incompatible with both signed char* and unsigned char*!

* C99 introduced int32_t, uint8_t etc. Great. Except they are usually defined on top of int, char, etc.

* Also, if you want to print a type like int64_t, printf still expects %d, %ld, or %lld; which to use? It's %ld on Linux64; %lld anywhere else. You expected to use PRI64d etc

* On the subject of printf, how crass is it to have to provide format codes to tell a compiler what it already knows: the type of an expression?

* How do you even determine the format code if you don't know the type? Code for clock_t anyone? size_t? (I think that one is %zu, but not universal.) It's crazy.

* OK, you figure out your expression (eg. i64*u16+u32) is %lld. The your revise your declarations, and/or the expression, and you have revise all printfs?

* The maximum value of int? That will be INT_MAX. Or MAX_INT. Or something. But how about INT32_T or UINT64_T?

* Suppose you want to find the maximum value a variable X can have; you need to go and dig up its type, and hardcode INT_MAX (etc). Then you change X's type...

* How many bits in a byte? Oh, C doesn't have bytes. How big is a char then? Well, it depends, but it will be CHAR_BIT

* How many bits in long long int? Only sizeof(long long int)*CHAR_BIT.

* How many elememts in this array: 'int MyLongishArrayName[] = {....}'. Only sizeof(MyLongishArrayName)/sizeof(MyLongishArrayName[0]). Sheesh.

* How to have an array lower-bound other than 0? Oh, you can't. A bummer if you need to port a 1-based algorithm.

* Manipulate an array by value? You can't.

* Arrays and Pointers are crazily mixed up. Create a pointer to array, so you need to deref then index to get the element. Now index and deref instead; it still works!

* Pointers can be indexed just like arrays: 'int *P; P[i]; int A; &A[12345'

* Functions are not marked with a keyword; you have to disentangle type declarations, and/or take cues from indentation, to find out where a function even starts!

* Subroutines that return no value have no special syntax, except they start with 'void'.

* Function params with same type have to repeat the type: (int a, int b, int c) instead of (int a, b, c)

* Array syntax int A[10], which normally defines a sequence of 10 ints, inside a parameter list as (int A[10]) now becomes a pointer to int! But only the first level

* Mix signed/unsigned integers, the result will be usually unsigned, but the rules are complex, depending on the sizes of the operands among other things. (Good luck with that format code)

* Call a function F like this: F(x). Or like this (*F)(x). Or this (****************F)(x). C doesn't care.

* Syntax is a free-for-all: A(B) might call function A with parameter B, or declare B with type A.

* A*B might multiply A by B, or defined B with type pointer to A

* Define an 'const unsigned long int' like that. Or as 'int long unsigned const'. Or 'as const const long const unsigned int const'.

* Everyone knows you wrote typedefs as 'typedef const long int T'. Or it can be 'const long int typedef T'.

* Few know you can typedef an actual function: typedef int fred(int,int). (Try it.)

* Switch: the craziness of this has been mentioned. Switch is followed by one statement, usually compounds, and case level can literally go anywhere even inside deeply nested statements

* You can't do case 'A'..'F' (except when extensions exist). You can't do case 10, 20, 30:

* Misspell 'default:' as 'defualt:', and its still valid; just now a wrong program

* Switch only works with integer index, and constant case values.

* Declare a function, and it's exported by default; you have to add 'static' to make it local (few do this). Same with variables.

* Missing function keyword parameters

* Missing default function parameter values

* There are no proper reference parameters

* This is no proper if-elsif-else-fi statement, it is only if-else-if-else (the latter has a nested structure, the former is linear)

* There is no dedicated for-loop for simple iteration. What there is is another free-for-all, with everyone trying to cram as much into a for-header as they can. C might as well not have a while statement!

* A simple iteration over A..B inclusive (for i=A,B in Lua), is for(i=A; i<=B; ++i), with the loop index written 3 times (the number of types I get it wrong), an everything spelled out exactly

* There is no 'named constant' feature. You have to choose between '#define' (no scope, reevaluates the expr etc), 'enum' (ints to 32 bits only), 'const int' (can't be in switch)

* There are about 30 standard headers. You can have to keep adding tiny headers and sometimes taking them out again as code change. It's a bloody nuisance; where is memset again?

* 'extern' defines external functions and variables, except when they are defined in this module (ie. one in a shared header)

* Some compilers allow 'extern int A = 1234;'

* All compilers allow 'int A,A,A,A,A,A,A,A' at file scope.

* Define 'int ABC' in one module, and 'char* ABC' in another, and that is an undetectable bug

* Functions need to be declared before use. So this either means defining them in a certain order in a module, or needing to use prototypes.

* Functions shared across modules usually need declaring in one place, and defining in another. Extra maintenance, and more annoyance. And opportunities for things to go wrong.

* Struct tags are a totally useless feature, except that you need them for self-referential pointers.

* Struct declarations are another mess: 'struct tag {int a,b;}; declares a type. 'struct {int a,b} x; declares a type of sorts and a named instance.

* Structs are automatically padded according to arcane rules of the C language (a bugbear for the 25 years I've been using C APIs from my language)

* Operator precedences: far too many, and non-intuitive, so A<<B + C actually means A<<(B + C).

* The really odd "->" operator, probably introduced because "(*P).m" is too unwieldy compared with "P->m". But introduce one more ptr level, and you need "(*PP)->m" anyway.

* You can't break out of a loop more than one deep.

* You can't break out of a loop more anyway if inside a switch statement.

* No way to redo a loop iteration, and no Python-style 'else' part

* Reading numbers from console or file? No chance using scanf, it's too complicated! And inflexible.

* The 2-way selection operator ?: doesn't need parentheses, so nobody uses them, making it hard to see what's happening esp. with nested ?:

* There is no N-way selector

* The comma operator - what a waste of time. And people like to exploit it to save having to add {...}.

* You can't use chained operators like a==b==c in a way that would be useful.

* There is no proper power operator like **.

* There is no proper abs operator (there are functions, and you have to use the right abs function for each kind of int or float; a palaver).

* No direct type-punning. If the expression is an l-value, you have to it by *(T*)&X.

* For some weird reason, labels have their own namespace: L: int L; goto L;

* I hate block scopes (this is not just a C thing). Only one global namespace at the module level, but inside a function, you can have millions.

* Having declarations in the middle of a block means that there are can be two instances of 'A' within the block, one from an outer scope, one after the declaration.

* Macros: C programmers do like to show off their prowess at undecipherable macros. It also makes it impossible to look at a piece of code and know what it will do.

* Take 0x123D+2, ie, add 2 to 0x123D to get 0x123F. Not try it with 0x123E+2, which will often be an an illegal preprocessor token).

* Undefined behaviour. No further comment needed.

* No bit or bitfield indexing, you have to use shifts and masks.

* No built-in 'swap' feature

* No built-in min and max operators

* Take the array element A[i][j], and write it as j[i[A]] (ie. as 2 1D array accesses instead of 1 2D access); it still works!

* Functions decls with a () parameter list instead of (void) are valid. They mean that any number an types of arguments can be passed, unchecked. 

* Quirks just keep coming up all the time, things you didn't know where possible. Keyword/identifiers split across multiple lines using backslash...

* ... Macros inside #include statements. You never know what to expect. C is supposed to be small and simple!

* Talking of includes: the rules for location a file in an include file-specifier are more complicated than you'd think. You need to consider rel/abs paths, current dir, current stack of includes...

OK, I think I've reached 100 annoyances.
