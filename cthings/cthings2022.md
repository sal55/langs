C 100 Things - 2016

100 small things about C that I have a problem with, find annoying, or things which could have been part of the language, or how I solved the problem elsewhere.

(These are personal opinions. Most alternatives have been implemented by me in some language or other. This is an update of a similar list posted in 2014.)

None of the suggestions below would make the language significantly higher level. And none is seriously intended, now, to be a proposal to change C. I think the language was more-or-less fixed 40+ years ago, and most people I know are against change.

**1** Braces {...} as block delimiters. I find these too thin and anaemic. They just aren't substantial enough for delimiting code across multiple lines.

**2** Block limiters all use the same } symbol to mark the end of any block (if, function, switch etc), leading to errors.

**3** Statement sequences either are inside {...} braces, or have no braces when there is just one (or no) statement. That means adding or removing braces as the number of statements changes.

**4** Because braces can be optional for 0 or 1 statements, a missing } (or {) doesn't immediately ring alarm bells, if another misaligned { (or }) is present.

**5** Braces can clutter the syntax because there is no way to tidily put them out of the way. And there are many different placement styles.

**6** While free-format syntax is good, the vast majority of code is line-oriented. Therefore it would be easy to have a scheme where end-of-line does the job of the semi-colon, with a few obvious exceptions. Then semi-colons can be virtually eliminated.

**7** Comments such as /*...*/ are not nested. That makes it awkward to quickly comment out a block of code. Suggestions such as using #if 0 are overkill. And a stray /* can inadvertently comment out a block of code, until the end of the next comment. A very difficult to find bug when you don't have an editor that highlights the problem.

**8** Comment syntax has other assorted bugs where the line comment '// path c:\abc\' will comment out the following line too! Another is 'a = b/*c' where the /* will start a comment.

**9** C uses "=" for assignment, and "==" for comparison, leading to many problems when an expression containing "=" might be an assignment, or might be intended to be compare. (I use ":=" for assignment and "=" for compare, eliminating such problems.)

**10** It's not officially possible to write binary literals yet. Some implementations have it, and it might shortly be officially part of the language, but it will have taken nearly fifty years.

**11** Other than base 2, it's not possible enter numeric literals in other bases than 8, 10 and 16.

**12** It's not possible to have separators inside numeric literals.

**13** Numbers starting with a leading 0 are interpreted as octal!

**14** (I use a scale factor feature where I can write 1 million to mean 1000000, or 180 degrees to mean 3.14159... radians. The names 'million', 'degree' etc exist in their own name-space.)

**15** I don't think C has a 'raw' string constant mode where \ in a string literal is interpreted literally.

**16** Array indexing requires multiple dimensions to be written [i][j][k] instead of the more fluidly typed [i,j,k]. This is one casualty of the 'comma operator'.

**17** C is case sensitive. Generating more problems than it solves IMO.

**18** I think multi-character constants such as 'ABC' are undefined. (I define these so that they exactly match an equivalent string "ABC" in memory.)

**19** Type declarations are a complete mess. It is possible declare an int type without even using the word 'int': signed, unsigned, const, long (on some systems). (I insist on the 'int' being present.)

**20** The various parts of a type declaration can be sloppily written in any order: int long static unsigned const.

**21** Until stdint.h came along, it was necessary to write 'unsigned' for unsigned types, making some declarations over-long. (I use 'int' for signed and 'word' for unsigned; much tidier.)

**22** Short and Long sound a good idea but are unreliable as they often do nothing. long int might be the same as int, or or it might be wider.

**23** Even with stdint.h, the type designations look like int32_t and uint64_t (still ugly IMO). (I use a scheme where a fixed-width type such as int32_t can be written as int:32, int*4, or using standard macros int32 or i32. All built-in.)

**24** (And when you look at how int32_t etc are implemented in stdint.h, they defined in terms of int and long anyway! So in reality C cannot specify an exact width type, but has to rely on a header using adhoc macros depending on the implementation.)

**25** Even a simple set of declarations is full of pit-falls: int* p,q,r; doesn't declare three pointers, but one pointer and two ints. And if you need three pointers, you have to repeat the pointer specifier (I have the pointer attribute attached to the type, not the name.)

**26** Similarly, it is necessary to have int a[10],b[10],c[10]; to declare three arrays of the same size (and now you need a #define to simplify maintenance). (I use [10]int a,b,c)

**27** As type specifications get more complex, mixing pointers, arrays, and especially function pointers, type declarations become impossible. So much so that special utilities exists to decode C type declarations into English, or to go from English to C! This is not a good situation. (I use simple left-to-right type declarations that read almost like English. One or two schemes for C have been proposed, that can co-exist with the existing syntax, but no one was interested.)

**28** (C's syntax is 'interesting' in that a(b) can mean call function a with parameter b, or declare variable b with type a. Or, a*b means multiply a by b, or declare variable b as a pointer to a! No one has apparently ever questioned this ambiguity not even early on when it could have been fixed.)

**29** C uses a long list of macros, such as UINT_MAX, to determine the limits of a type. (I use attributes such as int.maxvalue. You don't need to remember the exact macro name.)

**30** The limit macro needs to be manually selected to match a type elsewhere. So if SHORT_MIN is used to match a short type, them the macro needs changing when the short type changes.

**31** Limit macros can't be applied to values and expressions AFAIK (but .minvalue etc can be).

**32** To determine the bit-width of an expression, you need CHAR_BIT*sizeof(X) or some such thing. (I use X.bitwidth.)

**33** There is no simple way of determining the length of an array, without inventing some macro each time, but it will still need to be defining in terms of sizeof(A)/sizeof(A[0]). (I use A.len)

**34** The signed char type is just Wrong. But for legacy reasons it is here to stay. Using 'unsigned char' everywhere is a pain, but also sometimes causes type clashes with standard library functions defined with signed char. (I use a dedicated 'byte' type for an unsigned 8-bit value, but also separate 8, 16 and 32-bit char types.)

**35** Because the char type is synonymous with a 'byte' type, C allows character codes to be added or multiplied, for example. (By having distinct char types, I can trap this.)

**36** While value arrays can be declared (eg int a[10]; outside a parameter list), they can't be used in expressions. (To allow the 'decay' of the array name.)

**37** Because value arrays don't officially exist, they are 'written out' of the type system in some contexts, such as in parameter lists. Then 'int a[10]' instead defines a pointer to int! (Much better to have raised an error in such a situation, and to have kept type declarations consistent instead of making them even more complex.)

**38** With parameter lists, it is a nuisance, when parameter names are used, to have to repeat the type: fn(int a,int b,int c) instead of fn(int a,b,c)

**39** Functions in C really don't stand out: they start with a type spec just like any other declaration (I use a 'function' prefix. When writing C, I often use a dummy 'function' macro to do the same job.)

**40** There is no special treatment of void functions; they are still functions returning T, but T happens to be void. (Sometimes it's good to unify these things, but I found having separate handling of void functions, as 'procs', to be much better.)

**41** For local functions, it is generally necessary to write separate forward declarations. Without these declarations, it is necessary to for functions to be in a certain order, and you lose the ability to easily copy and paste between sources without dragging in those declarations. (I've eliminated such forward declarations)

**42** File-scope functions and variables are automatically exported unless you add a 'static' attribute. This is contrary to what you might expect. (I require a 'global' attrubute to export names.)

**43** Function parameters cannot be optional, or have a default value passed when one isn't supplied.

**44** Function arguments cannot be specified by keyword.

**45** Function have no proper, transparent 'pass-by-reference'.

**46** There is no 'named constant' feature, which is a lightweight scheme where a name is attached to a literal value. Instead C relies on a mish-mash of #define, enum, and const, none of them doing the doing the whole job satisfactorily.

**47** There are too many system header files (25 I think). (I believe there is now a single system header than replaces the lot.)

**48** The method of sharing code and data across modules is another mess. An exported function needs to be defined in one place, and declared again in another, usually a header. Extra maintenance. (I just stick 'global' in front, and the whatever is needed to share it is done automatically.)

**49** With variables, you can sometimes get away with defining a shared variable once in a header, but I think there should officially be one declaration in a header, and another in the 'owner' module, which might contain any initialisation data.

**50** To make it possible to declare A in a header, and again in A's owner module which includes the same header, C allows this:  int a, a, a, a, a, a, a.... This is quite legal!

**51** It can also make it difficult to see which module owns a variable, when a program tries to use a single declaration of a variable.

**52** There is no system of module imports or namespaces (although that is a heavyweight feature.)

**53** The names declared in an enum statement don't have a private namespace; they can clash with names in another enum in the same scope.

**54** Struct tags are a totally useless and confusing part of the language.

**55** Struct members can't be assigned to a specific offset, or at the same offset as another member (this latter role is largely taken care of with anonymous structs and unions, but sometimes a direct 'int b @ a' is simpler.)

**56** Struct members can't include anything other than a normal, storage-requiring variable. (I allow named constants, types, enums and functions. Lightweight classes.)

**57** Struct members are automatically padded. (I use 'pack(1)' by default. This is necessary when C is being used as a target language, as the source translator cannot predict exactly how the C compiler will handle the padding.)

**58** It's not possible to do Fortran-style equivalence between two variables or between a variable, and part of another.

**59** Array bounds can only be zero. (I allow any lower bound.)

**60** C conflates arrays with pointers. (I keep them distinct.)

**61** When passing arrays to functions, then invariably a pointer to the first element is passed, not a pointer to the array.

**62** A function taking a pointer to 'T[]', and which actually takes a 'T*' pointer, can be passed a pointer to a standalone variable of type T, without the compiler noticing anything wrong.

**63** C actually is perfectly capable of passing pointers to functions properly, as T(*)[] or whatever the syntax might be. The trouble is that indexing such arrays means writing (*A)[i] instead of A[i].

**64** So, in C, A[i] might index a normal value array, and B[i] might index a 'pointer to array' as it is normally done. You can't tell which is which.

**65** Similarly, f(a) might call an actual function, and g(a) might call via a pointer to a function; you can't tell.

**66** Deferencing of function pointers is another interesting quirk. Pointers to functions can be dereferenced as normal by prepending "*", until you get to a single 'pointer-to-function. Then, any further "*" are ignored. This means that (***************f)(a) is quite legal, no matter whether f is a normal function, or pointer to function, or whatever.

**67** The (*A)[i] notation for accessing proper array pointers, also rears its ugly head when accessing members of pointers to structs: (*S).m. So everyone uses S->m instead.

**68** So, this is a situation where C provides correct, consistent syntax for dealing with pointers ((*A[i] for array pointers, (*S).m for struct pointers, and (*g)(a) for function pointers), but they are so popular that everyone avoids them as much as possible! Instead preferring to write A[i] or S->m or g(a) thanks to C's idiosyncratic design making it possible.

**69** The 'for' loop is too general purpose and too low-level. Several things need to be specified (including two extra instances of the loop variable), which can make it more error-prone. It also seems to encourage people to write weird, non-standard loops, and all on one line, making it difficult to see at glance what it's doing, or what category of loop it is. (Ie. 'while' or simple iterative 'for')

**70** There is no simple, dedicated way to iterate over A to B, or 0 to N-1. Not without writing macros, which everyone will complain about (but a horrendously complicated macro to get around the lack of binary literals or whatever is fine!)

**71** There is no simple way way to repeat a block of code N times.

**72** There is dedicated endless loop construct (you have to use while(1) or for(;;))

**73** It's not possible to break or continue out of a loop more than one level deep.

**74** It's not possible to break out of a loop from inside a switch statement.

**75** There are no loop controls to restart or redo a loop iteration.

**76** There is no 'else' part to loops.

**77** Switch statements are seriously weird in how they work (in allowing case labels anywhere including deep inside nested statements)

**78** Switch case labels require 'break' 99% of the time. This should have been the default, with 'continue' or 'fallthrough' used only when needed. (I understand this is due to the crude nature of the syntax, and that the implicit fallthrough is needed to make case 'A': case 'B': work. But it's still a nuisance and a source of errors when break is left out, and extra clutter when it is used.)

**79** Case labels don't allow you to write case 'A','C','E': or case 10..20:

**80** You can't tell switch to not do range-checking (when you know for sure it will match a case label).

**81** Switch statements only work with integer expressions, and constant case labels. (I have a version that handles any type, and runtime expresions for case labels, ie. just a more convenient way of writing an if-else if chain).

**82** (I have a looping version of switch. That saves an indent level but also it is more obvious what it is doing.)

**83** C relies on functions to do basic i/o, such as printf. This has a number of problems, one of which is the amount of typing needing just to quickly print a few variables: printf("%d %d %d\n",a,b,c). (You don't really need anything more than println a,b,c.)

**84** Format strings require you to specify the exact type of a parameter, even though the compiler knows that perfectly well.

**85** Another problem is that when variables change type, then all format codes used for those variables throughout the program need to be updated. (I have proposed using %? to automatically pick up the type and use a default format, but there was a lot of opposition.)

**86** Yet another, is sometimes not knowing the type of something you're trying to print. Looking at the declarations isn't always helpful, when you have layers of macros and typedefs, or the type is supposed to be opaque. And when you do figure it out, then you have the problem of someone changing the declaration.

**87** I've never mastered the intricacies of the scanf() functions. (There ought to be simpler means of doing line-based input.)

**88** C doesn't have a direct way of exchanging the contents of two objects. (I use swap(a,b).)

**89** There is the a?b:c conditional expression, but because parentheses are not mandatory, people don't use them, and it's difficult to know how it parses within a bigger expression, because of complex precedence rules.

**90** There are far too many precedence levels anyway, and some are untuitive.

**91** It's not possible to have chained comparisons such as a==b==c (not with the expected meaning anyway).

**92** (As an extension of a?b:c, I allow this syntax: (n | a, b, c, ...|z) to pick the nth alternative from a list, or z when n is out of range. Only one expression is evaluated from the list.)

**93** There are no (AFAIK) built-in min/max operators that are overloaded for all viable types.

**94** There is no proper power operator (eg. **), and maths functions are also just implemented as ordinary (if compiler-aware) functions.

**95** There is no direct way of doing type-punning (ie. what you'd normally have to write as: *(T*)&X).

**96** C allows standalone expressions that apparently do nothing, such as a+b. So an opportunity to detect likely errors (eg. a==b written instead of a=b) is lost.

**97** There is no painless way of picking the Nth bit out of an int. It requires masks and shifts. (I use A.[N] to extract the Nth bit.)

**98** Similarly, extracting a bit field requires masking and shifting. (I use A.[i..j], a.lsb etc)

**99** With mixed sign arithmetic, C chooses to use unsigned for both. IMO this generates more problems than using signed for both.

**100** There is no simple range type (eg. 100..200), which has innumerable uses (if (A in 10..20), or just returning two ints from a function). And there is no set type (ie. Pascal-style set of bits). This extends the range type: if (C in ['0'..'9','e','F','+']) ...

OK, that makes my hundred things. Beyond that, I might have suggested bit arrays and bit pointers as a useful addition to this level of language.

I might then have gone on to complain about the macro system, and the current crop of compilers and build tools...


