### Annoying things about C ...

and How I fixed them in my own systems language.

* Braces as block delimiters - I don't use braces (I use Algol68-style blocks like)

* Inconsistent use of braces - my scheme is 100% consistent

* Too many opportunities for errors - far fewer chances of making errors


* The same } block delimiter used for all statement types - different 'end's can be qualied

* Need to type semicolons despite 99% of all code being line-oriented - Mine uses semicolons too but 99% of them don't need to be typed because

* // comment syntax has flaws - no such flaws in mine
````
    puts("One");  // File c:\abc\def\
    puts("Two");
````
* Non-nesting /\*..\*/ have further opportunities for silent bugs - no block comments

* Limited support for binary literals - binary literals have always been available


* No support for odd number bases other than 8, 10, 16 - support is for bases 2 to 16

* Leading zeros on numbers turn them into octal - no such stupid scheme; octals are written 8x377 rather than 0377

* No separators on numeric literals - Any of _ ' ` can be used

* No raw string literals - raw strings are F"..."

* Allows = and == in conditionals, which are easily mixed up - I used := and =

* Multi-dimensional index needs the fiddly-to-type A\[i\]\[j\]\[k\] - I allow A\[i,j,k\]

* Case-sensitive, so need to remember if it was OneTwo or oneTwo or OneTwo or Onetwo - Case insensitive

* Multi-character constants like 'ABCD' not well-defined, and they stop at 32 bits - well-defined and go up to 128 bits

* Uses such impossible, convoluted, inside-out type declarations - type declarations go left to right, are not split up, and don't wrap themselves around names

* int* p, q, r doesn't declare expected 3 pointers - I'd use ref int p,q,r which is 3 pointers

* Three identical arrays 'int a\[10\], b\[10\], c\[10\]' need the array dim three times - I use [10]int a,b,c

* Here: 'int a, b, (\*c)\[10\]', the type of c is in three parts - can't happen (and can't mix different types in the same list)

* Basic types are char, short, int, long, long long; FIVE types to represent the FOUR common basic int types - I have FIVE types for FIVE sizes

* These types are poorly defined - All mine are strictly and consistently defined (actually, like most languages part from C and C++)

* The basic char type might be signed or unsigned - my char type is unsigned; always. And there is a BYTE types, also unsigned

* C99 introduced int32_t, uint8_t etc. Great. - I have a similar which are their own types, not defined on of something which is poorly defined.

* Also, if you want to print a type like int64_t, printf still expects %d, %ld, or %lld; - Not applicable

* Also, if your wanted to write an int64_t constant, you still need to choose between 123L and 123LL - not applicable (constants are at least 64 bits anyway, or can be cast

* On the subject of printf, - I don't use something as crude as printf, unless calls via the FFI

* How do you even determine the format code if you don't know the type? - Not applicable

* OK, you figure out your expression (eg. i64\*u16+u32) is %lld. - Not applicable

* The maximum value of int? That will be INT_MAX. Or MAX_INT. Or something. But how about INT32_T or UINT64_T? - If T is an integer type, then T.minvalue and T.maxvalue will give the limits

* Suppose you want to find the maximum value a variable X can have; you need to go and dig up its type, and hardcode INT_MAX (etc). Then you change X's type... - It's always X.minvalue and Y.maxvalue

* How many bits in a byte? Oh, C doesn't have bytes. How big is a char then? Well, it depends, but it will be CHAR_BIT - 8 bits

* How many bits in long long int? Only sizeof(long long int)\*CHAR_BIT. - i64.bitwidth will give 64 (but the name here always gives a hint)

* How many elememts in this array: 'int MyLongishArrayName\[\] = {....}'. - MyLongishArrayName.len

* How to have an array lower-bound other than 0? Oh, you can't. - I allow any array lower bound

* Manipulate an array by value? You can't. - The language manipulates arrays by value where possible (the ABI puts limits on that)

* Arrays and Pointers are crazily mixed up. - Mine are quite distinct

* Pointers can be indexed just like arrays: 'int \*P; P\[i\]; int A; &A\[12345\]' - Not possible

* Functions are not marked with a keyword - mine start with **function** or **proc**

* Subroutines that return no value have no special syntax, except they start with 'void' - mine start with **proc**

* Function params with same type have to repeat the type: (int a, int b, int c) instead of (int a, b, c) - I allow the latter

* Array syntax int A[10], which normally defines a sequence of 10 ints, inside a parameter list as (int A[10]) now becomes a pointer to int! But only the first level - A[10] is always an actual arraty

* Mix signed/unsigned integers, the result will be usually unsigned, but the rules are complex, depending on the sizes of the operands among other things - Mixed arithmetic will be signed, and the rules are far simpler: both unsigned, then unsigne; else signed

* Call a function F like this: F(x). Or like this (\*F)(x). Or this (\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*F)(x) - not possible

* Syntax is a free-for-all: A(B) might call function A with parameter B, or declare B with type A. - Not possible

* A\*B might multiply A by B, or defined B with type pointer to A - not possible

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
