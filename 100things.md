### C 100 Things - revised 2017

100+ small things about C that I have a problem with, find annoying, or which could easily have been part of the language.

**1** Braces {...} as block delimiters. I find these too thin and anaemic. They just aren't substantial enough for delimiting code across multiple lines.

**2** Block limiters all use the same } symbol to mark the end of any block (if, function, switch etc), leading to errors.

**3** Statement sequences either are inside {...} braces, or have no braces when there is just one (or no) statement. That means adding or removing braces as the number of statements changes.

**4** Because braces can be optional for 0 or 1 statements, a missing } (or {) doesn't immediately ring alarm bells, if another misaligned { (or }) is present.

**5** Braces can clutter the syntax because there is no way to tidily put them out of the way. And there are many different placement styles.

**6** While free-format syntax is good, the vast majority of code is line-oriented. Therefore it would be easy to have a scheme where end-of-line does the job of the semi-colon, with a few obvious exceptions. Then semi-colons can be virtually eliminated.

**7** Comments such as /\*...\*/ are not nested. That makes it awkward to quickly comment out a block of code. Suggestions such as using #if 0 are overkill. And a stray /\* can inadvertently comment out a block of code, until the end of the next comment. A very difficult to find bug when you don't have an editor that highlights the problem.

**8** Comment syntax has other assorted bugs where the line comment '// path c:\abc\' will comment out the following line too! Another is 'a = b/\*c' where the /\* will start a comment.

**9** C uses "=" for assignment, and "==" for comparison, leading to many problems when an expression containing "=" might be an assignment, or might be intended to be compare. (I use ":=" for assignment and "=" for compare, eliminating such problems.)

**10** It's not officially possible to write binary literals yet. Some implementations have it, and it might shortly be officially part of the language, but it will have taken nearly fifty years.

**11** Other than base 2, it's not possible enter numeric literals in other bases than 8, 10 and 16.

**12** It's not possible to have separators inside numeric literals.

**13** Numbers starting with a leading 0 are interpreted as octal!

**14** (There is no scale factor feature where you can write 1 million to mean 1000000 for example, or 180 degrees to mean 3.14159... radians.)

**15** I don't think C has a 'raw' string constant mode where \ in a string literal is interpreted literally.

**16** Array indexing requires multiple dimensions to be written [i][j][k] instead of the more fluidly typed [i,j,k]. This is one casualty of the 'comma operator'.

**17** C is case sensitive. Generating more problems than it solves IMO.

**18** I think multi-character constants such as 'ABC' are undefined.

**19** Type declarations are a complete mess. It is possible declare an int type without even using the word 'int': signed, unsigned, const, long (on some systems).

**20** The various parts of a type declaration can be sloppily written in any order: int long static unsigned const.

**21** Until stdint.h came along, it was necessary to write 'unsigned' for unsigned types, making some declarations over-long. 

**22** Short and Long sound a good idea but are unreliable as they often do nothing. long int might be the same as int, or or it might be wider.

**23** Even with stdint.h, the type designations look like int32\_t and uint64\_t (still ugly IMO).

**24** (And when you look at how int32\_t etc are implemented in stdint.h, they defined in terms of int and long anyway! So in reality C cannot specify an exact width type, but has to rely on a header using adhoc macros depending on the implementation.)

**25** Even a simple set of declarations is full of pit-falls: int\* p,q,r; doesn't declare three pointers, but one pointer and two ints. And if you need three pointers, you have to repeat the pointer specifier.

**26** Similarly, it is necessary to have int a[10],b[10],c[10]; to declare three arrays of the same size (and now you need a #define to simplify maintenance).

**27** As type specifications get more complex, mixing pointers, arrays, and especially function pointers, type declarations become impossible. So much so that special utilities exists to decode C type declarations into English, or to go from English to C! This is not a good situation.

**28** (C's syntax is 'interesting' in that a(b) can mean call function a with parameter b, or declare variable b with type a. Or, a\*b means multiply a by b, or declare variable b as a pointer to a!

**29** C uses a long list of macros, such as UINT\_MAX, to determine the limits of a type.

**30** The limit macro needs to be manually selected to match a type elsewhere. So if SHORT\_MIN is used to match a short type, them the macro needs changing when the short type changes.

**31** Limit macros can't be applied to values and expressions AFAIK

**32** To determine the bit-width of an expression, you need CHAR\_BIT\*sizeof(X) or some such thing.

**33** There is no simple way of determining the length of an array, without inventing some macro each time, but it will still need to be defining in terms of sizeof(A)/sizeof(A[0]).

**34** The signed char type is just Wrong. Using 'unsigned char' everywhere is a pain, but also sometimes causes type clashes with standard library functions defined with signed char. Further, char* is incompatible with both signed char* or unsigned char*, even though it necessarily has to be either signed or unsigned itself.

**35** Because the char type is synonymous with a 'byte' type, C allows character codes to be added or multiplied, for example. 

**36** While value arrays can be declared (eg int a[10]; outside a parameter list), they can't be used in expressions. (To allow the 'decay' of the array name.)

**37** Because value arrays don't officially exist, they are 'written out' of the type system in some contexts, such as in parameter lists. Then 'int a[10]' instead defines a pointer to int!

**38** With parameter lists, it is a nuisance, when parameter names are used, to have to repeat the type: fn(int a,int b,int c) instead of fn(int a,b,c)

**39** Functions in C really don't stand out: they start with a type spec just like any other declaration, rathan than, for example, just start with a 'function' prefix as they do in many other languages.

**40** There is no special treatment of void functions; they are still functions returning T, but T happens to be void. 

**41** For local functions, it is generally necessary to write separate forward declarations. Without these declarations, it is necessary to for functions to be in a certain order, and you lose the ability to easily copy and paste between sources without dragging in those declarations.

**42** File-scope functions and variables are automatically exported unless you add a 'static' attribute. This is contrary to what you might expect.

**43** Function parameters cannot be optional, or have a default value passed when one isn't supplied.

**44** Function arguments cannot be specified by keyword.

**45** Function have no proper, transparent 'pass-by-reference'.

**46** There is no 'named constant' feature, which is a lightweight scheme where a name is attached to a literal value. Instead C relies on a mish-mash of #define, enum, and const, none of them doing the doing the whole job satisfactorily.

**47** There are too many system header files (some 30 files I think).

**48** The method of sharing code and data across modules is another mess. An exported function needs to be defined in one place, and declared again in another, usually a header. Extra maintenance.

**49** With variables, you can sometimes get away with defining a shared variable once in a header, but I think there should officially be one declaration in a header, and another in the 'owner' module, which might contain any initialisation data.

**50** To make it possible to declare A in a header, and again in A's owner module which includes the same header, C allows this:  int a, a, a, a, a, a, a.... This is quite legal!

**51** It can also make it difficult to see which module owns a variable, when a program tries to use a single declaration of a variable.

**52** There is no system of module imports or namespaces.

**53** The names declared in an enum statement don't have a private namespace; they can clash with names in another enum in the same scope.

**54** Struct tags are a totally useless and confusing part of the language.

**55** Struct members can't be assigned to a specific offset, or at the same offset as another member (this latter role is largely taken care of with anonymous structs and unions, but sometimes a direct 'int b @ a' is simpler.)

**56** Struct members can't include anything other than a normal, storage-requiring variable.

**57** Struct members are automatically padded. (This is a nuisance when C is being used as a target language, as the source translator cannot predict exactly how the C compiler will handle the padding.)

**58** It's not possible to do Fortran-style equivalence between two variables or between a variable, and part of another.

**59** Array lower bounds can only be zero.

**60** C conflates arrays with pointers.

**61** When passing arrays to functions, then invariably a pointer to the first element is passed, not a pointer to the array.

**62** A function taking a pointer to 'T[]', and which actually takes a 'T\*' pointer, can be passed a pointer to a standalone variable of type T, without the compiler noticing anything wrong.

**63** C actually is perfectly capable of passing pointers to functions properly, as T(\*)[]. The trouble is that indexing such arrays means writing (\*A)[i] instead of A[i].

**64** So, in C, A[i] might index a normal value array, and B[i] might index a 'pointer to array' as it is normally done. You can't tell which is which.

**65** Similarly, f(a) might call an actual function, and g(a) might call via a pointer to a function; you can't tell.

**66** Deferencing of function pointers is another interesting quirk. Pointers to functions can be dereferenced as normal by prepending "\*", until you get to a single 'pointer-to-function. Then, any further "\*" are ignored. This means that (\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*f)(a) is quite legal, no matter whether f is a normal function, or pointer to function, or whatever.

**67** The (\*A)[i] notation for accessing proper array pointers, also comes up when accessing members of pointers to structs: (\*S).m. So everyone uses S->m instead.

**68** So, this is a situation where C provides correct, consistent syntax for dealing with pointers ((\*A[i] for array pointers, (\*S).m for struct pointers, and (\*g)(a) for function pointers), but they are so popular that everyone avoids them as much as possible! Instead preferring to write A[i] or S->m or g(a).

**69** The 'for' loop is too general purpose and too low-level. Several things need to be specified (including two extra instances of the loop variable), which can make it more error prone. It also seems to encourage people to write weird, non-standard loops, and all on one line, making it difficult to see at glance what it's doing, or what category of loop it is. (Ie. 'while' or simple iterative 'for')

**70** There is no simple, dedicated way to iterate over A to B, or 0 to N-1. Not without writing macros, which everyone will complain about (but a horrendously complicated macro to get around the lack of binary literals or whatever is fine!)

**71** There is no simple way way to repeat a block of code N times.

**72** There is no dedicated endless loop construct (you have to use while(1) or for(;;))

**73** It's not possible to break or continue out of a loop more than one level deep.

**74** It's not possible to break out of a loop from inside a switch statement

**75** There are no loop controls to restart or redo a loop iteration.

**76** There is no 'else' part to loops.

**77** Switch statements are seriously weird in how they work (in allowing case labels anywhere including deep inside nested statements)

**78** Switch case labels require 'break' 99% of the time. This should have been the default, with 'continue' or 'fallthrough' used only when needed.

**79** Case labels don't allow you to write case 'A','C','E': or case 10..20:

**80** You can't tell switch to not do range-checking (when you know for sure it will match a case label).

**81** Switch statements only work with integer expressions, and constant case labels.

**82** (There is no looping version of switch; you have to nest switch inside an endless loop, with an extra indent level.)

**83** C relies on functions to do basic i/o, such as printf. This has a number of problems, one of which is the amount of typing needing just to quickly print a few variables: printf("%d %d %d\n",a,b,c).

**84** Format strings require you to specify the exact type of a parameter, even though the compiler knows that perfectly well.

**85** Another problem is that when variables change type, then all format codes used for those variables throughout the program need to be updated.

**86** Yet another, is sometimes not knowing the type of something you're trying to print. Looking at the declarations isn't always helpful, when you have layers of macros and typedefs, or the type is supposed to be opaque. And when you do figure it out, then you have the problem of someone changing the declaration.

**87** I've never mastered the intricacies of the scanf() functions. (There ought to be simpler means of doing line-based input.)

**88** C doesn't have a direct way of exchanging the contents of two objects.

**89** There is the a?b:c conditional expression, but because parentheses are not mandatory, people don't use them, and it's difficult to know how it parses within a bigger expression, because of complex precedence rules.

**90** There are far too many operator precedence levels, and some are untuitive.

**91** It's not possible to have chained comparisons such as a==b==c (not with the expected meaning anyway).

**92** There is no N-way selection, as an extension of the two-way ?: selection operator. (With N-way selection, only one expression must be evaluated.)

**94** There is no proper power operator (eg. \*\*), and maths functions are also just implemented as ordinary (if compiler-aware) functions.

**95** There is no direct way of doing type-punning (ie. what you'd normally have to write as: \*(T\*)&X, and even that is limited to l-values).

**96** C allows standalone expressions that apparently do nothing, such as a+b. So an opportunity to detect likely errors (eg. a==b written instead of a=b) is lost.

**97** There is no painless way of picking the Nth bit out of an int. It requires masks and shifts.

**98** Similarly, extracting a bit field requires masking and shifting.

**99** With mixed sign arithmetic, C chooses to use unsigned for both. IMO this generates more problems than using signed for both.

**100** So A\[i\] selects the (i+1)th element of A (A being an array *or* pointer. But i\[A\] does the same thing!

**101** In the same way, A\[i\]\[j\] can end up being written as j\[i\[A\]\] if the rules are followed. But now, a 2D indexing operation turns into two nested 1D indexing ops!

**102** For some weird reason, labels have their own namespace, so variable names can also be used a label names: L: L(); or int L; L: L=0; goto L;

**103** While at file scope C gives you one shared by all names, inside a function, each nested {...} block creates a brand new scope. You can have hundreds of distinct scopes within a function.

**104** In addition, names can be declared part-way through a block, so that two versions of 'A' may be known within that block (in addition to two versions of struct A, plus of course you can have a label A).

**105** 'extern' declares a name imported from elsewhere, right? Unless it's declaring a name defined later on in the same file. And sometimes you can declare something extern, but initialise it at the same time!

**106** Plus, 'extern' can also be used in a function - to declare a name defined outside the function.

**107** I haven't touched on the preprocessor at all. But conside this: 0x123D+2 means, of course, the hex value 0x123D with 2 added. And 0x123F+2 means 0x123F with 2 added. But 0x123E+2 is supposed to be an illegal preprocessor token (a malformed hex floating point constant)
