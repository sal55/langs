

## Comparing C with 'M'

### A Selection of Differences from C

**1** M is case-insensitive. I find that makes for a coding style that is easier on the eye, and gives some useful advantages that have been discussed at length elsewhere.

**2** It doesn't use brace syntax for block delimiting; it uses Algol-68 style syntax. (See Examples link)

**3** It doesn't have this thing where one statement needs no braces, but have two or more and you need to add them,
or remove statements and you have to remove braces when it gets down to one

**4** Because it doesn't have optional braces, it's not subject to some errors of C, where you can
 add an extra indented statement, but forget the braces; or missing/extra braces cancel out for
 errors.

**5** For the same reason, there will never be issues with dangling else.

**6** Extra semicolons are usually ignored. In C, it can have serious consequences:
````
    for (i=0; i<N; ++i);
    {
        printf("%d\n",i);
    }
````

**7** For some reason, C can't have a label immediately before a `}` brace, or in front
of a declaration. M allows them pretty much everywhere.

**8** Line comments in C have some flaws. eg. the 2nd line is commented out here:
````
    puts("One");  // File c:\abc\def\
    puts("Two");
````
M's line comments start with "!", and have no such issues.

**9** M no longer has block comments (they are relegated to the editor). C's `/*...*/` block comments had their own problems: non-nested, by missing out one \*/, they could unintentionally comment out a whole block of code. And they make things harder for a text editor as it would need to keep track block comments from 1000s of lines earlier in a file.

**10** M has binary literals written as `2x1011` (alternatively as `1011B`). While mosy C compilers
will support `0b1011` format, it is not standard C, and `-Wpedantic` will report a warning

**11** In C, use a leading 0 as in `0100`, and the number is assumed to be octal, so this
has the value 64. In M, it has the value 100 (it no longer has octals, which used to be written as `8x100`).

**12** (M not longer allows arbitrary number bases from 2 to 16, only 2, 10, 16 now.)

**13** M allows numeric separators `'` and `_`, for example `1'000'000` or `2x1011_1110_0001`.

**14** M uses scale factors such as `3 million`, or `4 billion`.

**15** M has raw string literals written `F"abc\def\ghi"`, where `"\"` is not used as an escape code.

**16** C doesn't properly define multi-character literals like `'ABCD'`. They are implementation-defined.
In M they are well defined (and on little-endian sytems, laid out so that in memory, `'ABCD'` is the
same as the string `"ABCD"`).

**17** C character constants are also limited to the same size as `int`, so usually 4 characters.
M has 64-bit types so they can go up to `'ABCDEFGH'`.

**18** C uses suffixes such as `-U`, `-L` and `-LL` to control the types of integer literals (but see below
about long/long long). M's literals have `i64` type unless their magnitude makes them `u64`. There are no suffixes; to force a particular type, a cast is used.

**19** Those `-LL` suffixes will anyway not work with types like `int64_t` (since that is usually implemented on top of either `long` or `long long`); special macros are needed. M has no such needs.

**20** The basic integer types in C are signed/unsigned versions of four 8/16/32/64-bit widths on most
current systems. Yet there are FIVE basic types: `char, short, int, long, long long`. Usually `long`
is the same width as `int` or `long long`, but is compatible with neither.

**21** C doesn't even solidly define what the basic types mean, except that `long long >= long >= int >= short >= char` with certain minimums.

**22** C99 defines `int32_t`, `uint64_t` etc, but usually on top of `int/long` etc. Yet the rest of C still uses
`"%lld"` formats (`long long)`; or `-LL` suffixes.

**23** Basic types have untidy long-winded denotations: `unsigned long long int`, that can be written
in any order, with parts missing: `long unsigned long`.

**24** C has `signed char, unsigned char`, but also plain `char` which is imcompatible with both of the first two.

**25** Additionally, plain char on most systems is a signed type, which is most inconvenient, and at odds which most other langauges.

**26** So here are the M enhancements: M has a logical type scheme with these integer types
````
   i8 i16 i32 i64        signed          (also int8 .. int64)
   u8 u16 u32 u64        unsigned        (also word8 .. word64)
   c8 c64				 char		     (also char8, char64)
   bool8 bool64          boolean			

   byte                  (alias for u8)
   int                   (alias for i64)
   word                  (alias for u64)
   char                  (alias for c8)
   bool					 (alias for bool8)

````
Each is written as one token only. Each is exactly defined. char is a thin wrapper around u8

**27** (M no longer supports 128-bit types. They may be reintroduced as part of a set of
vector types)

**28** --

**29** C's printf: where do you even start? If A is `int`, B is `long long int`, C is a `char*`, D is a pointer, E is a `uint64_t`, then
you might print those using:
````
    printf("%d %lld %s %p %ull", a,b,c,d,e);
````
You have to specify the exact format. The `%ull` is also correct for Windows or Linux32, but is wrong
for Linux64 (you need to use a macro like `PRId64`). In M it is just:
````
    print a,b,c,d,e
````

**30** On the same topic, try printing the result of an expression that involves `int32_t`, `clock_t` and `size_t`; what
format code to use? Even when you figure it out; you later change a declaraton or tweak the expression, and
it could be different. In M, nothing changes: there are no format codes that tell the compiler
what it already knows: the type of an expression.

**31** C uses `sizeof` for the byte-size of a type or expression. For the latter, suppose you define
a pointer `T* sizeOf`, then the size that that points to is `sizeof*sizeOf`. Which looks a little like
it is multiplying two variables! In M it is `sizeoOf.bytes` or `sizeof.bytes` (case-insensitive).

**32** `sizeof(T)` needs parentheses around a type but not around an expression: `sizeof X`. But an
expression might have parentheses anyway: `sizeof(A)-1`. Now it starts to get confusing... (Not an issue with M)

**33** To get the size of a fixed array A in C, you need `sizeof(A)/sizeof(A[0]`). Or, thanks to #32,
you can write it as `sizeof(A)/sizeof(A)[0]` - I think. In M it is `A.len`, with no need to repeat a possibly long name.

**34** The maximum value of int? In C it's `INT_MAX` or `MAX_INT`; one of those two. But you need limits.h. Of `uint64_t`? Now you need stdint.h or inttypes.h, and it's -- I don't know, some macro you have to go and look up. In M it's just `int.max` or in general `T.min` and `T.max` for any integer type.

**35** The maximum value of the type of an expression X? In C you can't do it. In M it is `X.max` or `X.min` for the minimum.

**39** How many bits in a type? C uses `CHAR_BIT` for bits in a char, so bits in `T` would be `sizeof(T)*CHAR_BIT`. In M: `T.bitwidth`.

**37** M also has `min/max` operators built-in that work on ints and floats: `min(x,y)`, `max(x,y)` (can also be written as infix)

**38** `Min/max` can be used in augmented assignments: `x min:= y`; equivalent to `x := min(x,y)`.

**39** M has `swap` to exchange any two values: `swap(A[i], A[i+1)`

**40** Perform an operation between two integer types, and there will be a wildering set of rules that will determine whether the operation is done as signed or unsigned, and what signedness the result will be:
````
       i8  i16 i32 i64  u8  u16 u32 u64

   i8   S   S   S   S    S   S   u   u
  i16   S   S   S   S    S   S   u   u
  i32   S   S   S   S    S   S   u   u
  i64   S   S   S   S    S   S   S   u

   u8   S   S   S   S    S   S   u   u
  u16   S   S   S   S    S   S   u   u
  u32   u   u   u   S    u   u   u   u
  u64   u   u   u   u    u   u   u   u
````
Notice that adding two unsigned bytes is done as signed! In M, it's a bit simpler:
````
       i8  i16 i32 i64  u8  u16 u32 u64

   i8   S   S   S   S    S   S   S   S
  i16   S   S   S   S    S   S   S   S
  i32   S   S   S   S    S   S   S   S
  i64   S   S   S   S    S   S   S   S

   u8   S   S   S   S    S   S   S   S
  u16   S   S   S   S    S   S   S   S
  u32   S   S   S   S    S   S   S   S
  u64   S   S   S   S    S   S   S   u
````

**41** C multi-dim array indexing is the horrible-to-type `A[i][j[k]`. In M it's the more fluid `A[i,j,k]`. Although C-style will still work.

**42** BTW C's comma operator `a,b,c` is the reason you can't use `A[i,j,k]`, as it would just mean `i; j; A[k]`. The comma operator is also responsible for a lot of bad C code, like trying to avoid braces in examples like `if (c) a=i, b=j;`

In M the nearest equivalent to C's a, b, c is `(a; b; c)`, with mandatory parentheses. a, b, c can all be statements too.

**43** In C, the array access `A[i]` can also be written as `i[A]`. More astonishingly, `A[i][j]` becomes `j[i[A]]` (A 2D index becomes two nested 1D indices!). M doen't have this 'feature'.

**44** A consequence of #43 is that you can write `i[A][A][A][A][A]...` It works because each `i[A]` so far yields an int (when A is an int array), used as index to the next A. I don't have this feature either.

**45** C arrays always start at 0. M arrays usually start from 1, but can start from anything including 0: `['A'..'Z']int counts`

**46** M includes `A.lwb` and `A.upb` for array bounds, as well as `A.len` for their length. Most of the time `A.lwb` is 1, and `A.upb=A.len`.

**47** C can't manipulate arrays by value; given `int A[10], B[10];` then `A=B` is not allowed. M allows `A:=B`.

**48** Further, M can pass arrays to functions and return them (although restricted by ABIs), and can compare them (here some ops are not implemented, but the language itself allows value arrays anywhere)

**49** In C, if A is an array, then you can treat it like a pointer: `*A`. In M that is not allowed.

**50** In C, if P is a pointer, you can treat it like array: `P[i]`. In M that is not allowed (you have to use pointer ops like `(P+i)^`)

**51** Given, in C, a pointer to array of array (eg. `T (*A)[10][20]`), you'd access an element like this:
````
    (*A)[i][j]         // dereference the pointer, index, index
````
However C allows ANY COMBINATION of array/pointer operations even though most will be wrong:
````
    *(A[i][i])        // index, index, then deref
    (*(A[i]))[j]      // index, deref, index
````
It is completely crazy, but it is a consequence of #49 and #50. In M, such a type can only be accessed as `A^[i,j]`; deref then double index. (The current language relaxes the need for the deref operator `^` in `A^[i] P^.m F^()`, but the deref is still performed; it does not completely disregard the type system)

**52** * Despite proper pointer-to-array syntax being available in C (see #51), such arrays are very rarely used in C. The normal idiom when you have an array of `T` to pass to a function, or to allocate dynamically, is to use a `T*` type (pointer to its first element), rather than `T(*)[]` (pointer to array).

This means you can't distinguish a `T*` parameter that represents an array, from one that represents a single value. One reason might be because the syntax to access an element is so unwieldly: `(*A)[i]`. M can pass arrays anyway, but often passes pointers to arrays, as the syntax is simpler: `A^[i]`, or `A[i]` as the `^` can be dropped. But it also has reference params (see below).

**53** C allows this: `int i; &i[123456]`. Or you can pass `&i` to a function expecting an array via a `int*` parameter.

**54** Time to get on to the elephant in the room: C's type declaration syntax. One of the worst language design mistakes ever. C's declarations can be so complex, you need to employ third party tools (eg. cdecl.org) to disentangle them. A special algorithm is needed to understand C declarations.

M doesn't need anything like that. Its declarations are written left-to-right, and styled after Algol 68.

**55** While most languages with static type declarations have types before a name, or after a name, only C wraps the type around the name: `int *A[10]`. M has the type to the left (for this example, array of pointer to int)

**56** With the declaration of D like `long long int A, B, C, (*(*D))[N]`, its type is split into 3 parts: at the extreme left, before the name, and after the name! With M, this type (pointer to pointer to array N of int) is written in one place as `ref ref[N] int`.

**57** The example in #56 also demonstrates declaring mixed types in one declaration, unique to C. For example this:
````
    int A, B[10], *C, (*D)(void);
````
declares an int, array of int, pointer to int, and I think a pointer to a function taking no params that returns int! (Without the parentheses, I think that D would be a function declaration. Who would put a function prototype in such a list?

Coding guidelines frown on declaring several names *of the same type* on one line, let alone mixed types of variables, let alone function declarations. M is traditional and can only declare things of the same type in such a declaration.

**58** C rules mean that `int* p,q,r` declare one pointer and two ints, but it looks like you're declaring three pointers. M uses `ref int p,q,r` for three pointers.

**59** To declare 3 arrays of the same size in C mean duplicating the array dimension: `int A[10], B[10], C[10]`.

In M, it is `[10]int A, B, C`. Less maintenance, and it also obvious `A, B, C` are the same size (the dimensions can be a more elaborate expression).

**60** When declaring several named parameters of the same type, in C you have to repeat them: `(double a, double b, double c)`.
In M you just write `(real a,b,c)`.

**61** When defining functions, C doesn't use any keyword to mark them out. You have to figure it out from the shape of the syntax. M uses a `function` keyword, as is common now, to make it easy to spot them.

**62** M likes to distinguish between functions that return a value, from those that don't:
````
 function F(int a,b )int = ...
 proc G(int a,b)
````
C uses a 'void' return type, such as in #63

**63** The simplest kind of function pointer in C can look like this:
````
void(*)(void)        # with no name (as in a cast, or unnamed parameter)
void(*P)(void)       # with a name

````
Now imagine what more complex one might look like. This example is a pointer to function taking no params and returning no value. In M the examples become:
````
ref proc
ref proc P
````
'ref' means 'pointer to'.

**64** M's functions can have default values:
````
function F(int A, B=0)
````
So this can be called as `F(10,20)`, or as `F(10)` which is equivalent to `F(10,0)`. Default values are useful for adding an extra parameter to an existing function, without disrupting existing calls, or doing so temporarily.

**65** M's functions can be called with keyword arguments. The example in #64 can be called with `F(B:20, A:20)`, or as `F(10,B:30)`, but it is more useful when there are lots of parameters.

**66** Since M has to declare its own versions of functions in external libraries, such as from WinAPI, it can apply default values and keyword arguments to those too. So I can write:
````
MessageBox(message:"Hello")
````
instead of:
````
MessageBox(nil, "", "Hello", 0)
````
(I can't quite remember the correct order for the above, but this is the point; I don't need to.)

**67** M's functions have reference parameters, indicated with a & just before the parameter name like C++. This then automatically adds & (address-of) before any arguments (so a parameter cannot be null), and automatically dereferences within the functions.

**68** In C, feel free to call a function as `F(x)`. Or as `(*F)(x)`. Or as `(************************F)(x)`. It doesn't care; M does.

**69** In C, define a function to return a value (not void). Then forget to return a value in the body. C compilers will either say nothing or just warn. M requires a return value or it will fail. (It can't detect unreachable code, so sometimes a dummy return value is needed).

**70** In C, define a function returning void, but then use return like this:
````
void F(void) {
	return F();
}
````
Many C compilers will pass this too! M disallows any expression after 'return' for such a function.

**71** In C, call a function without declaring it first (even one defined later on). In many compilers this is a not a downright error; they will just assume one returning int, and I think taking int arguments. In M you can only call a function that the compiler knows about (but it might be anywhere in the program; more on this later)

**72** In C, declare an imported function likes this:
````
extern void F();
````
This does NOT mean the same as `F(void)`, as so many think, ie. taking no arguments. It means the number and types of arguments are not known, and you are free to call F with whatever arguments you like, even mixing it up on different calls. M does not allow that.

**73** C's standard library, as well as many basic functions, are spread across 29 small system headers. This means every program having to keep adding includes, and removing them, as a program evolves.

M doesn't need headers (or 'imports') for features that are built-in a language such as types (C might need stdint/inttypes); min/max values (limits); print (stdio); operators (math, stdlib etc); and so on. There are 3 main imports: clib (for the C runtime); mlib (for named standard library functions); and oslib (for functions that wrap some OS-specific features).

**74** With C's `#include`, comes a complex implementation-defined algorithm for locating headers with the following inputs:
(1) whether the header is in "..." or <...>; (2) whether the header is a relative or absolute path; (3) the list of search locations given to the compiler; (4) the location of the current include or source file (and stack of include locations prior to this one)

M doesn't have any of this. C's use of headers is replace by a module system. Where M does retain textual includes, the it is a straight file name following simpler rules.

**75** As well as `include` that M rarely still uses (eg. to incorporate generated code), M has `sinclude`. 
This incorporates any text file into a program, as though it was a string constant. This is used, for example, to bundle library sources into my compilers. If this program is called test.m, then this prints itself:
````
proc main =
    println sinclude("test.m")
end
````

**76** There is also `binclude` to incorporate binary files; the result is suitable for initialising a byte array:
````
[]byte data = binclude("zip.exe")
````

**77** M has a full module import system. Imported modules can be imported in any order, and circular and mutual imports are allowed.

**78** M has out-of-order definitions throughput. Functions, variables, types etc can defined in any order.

**79** M has a whole-program compiler to go with #77 and #78. The compilation unit is the program (the modules and support files used to build one .exe or .dll file) rather than the module as with C.

**80** M has no declarations, only definitions. You define anything (function, variable, type, named constant, enum, macro) in exactly one place. It has a 'global' attribute to share it with other modules. Other modules use 'import' to make use of exported names.

**81** C's crude include mechanisms for sharing names across modules gives rise to some odd features:
````
    int A,A,A,A,A,A;    # duplicate declarations are fine
    extern int B=100;   # declared an import, and initialise it at the same time
````

**82** In C, any variable or function declared at file scope is automatically exported. Unless you use 'static'. M makes everything local unless 'global' is used.

**83** In C, you can declare (and export) a variable 'abc' of type double, say, in module B. In module A, you can declare an imported variable 'abc' of type char*. They will clash, and the program can go horribly wrong, but this cannot be detected.

In M this cannot happen since there is only one definition of 'abc'. (Shared within the program. It can still happen across programs, or libraries, when the bindings for an external library or written manually. But when the libraries are also in M, then the necessary bindings are genarated by the M compiler).

**84** How does C know which module 'owns' a variable? Even with matching types, if modules `A, B, C` all declare `abc`, but none initialise, which module exports it? This is usually up to a linker. Normally, at most one module can initialise a symbol.

**85** In C you are supposed to use `extern` for names that are imported, but few bother; programs still (somehow) work, but it's all very sloppy.

**86** In C, supposed you have modules A and B that want to share `double abc`, and C and D that share `char* abc`. That is not possible as they will clash.

In M it *is* possible. Partly because, with the module mechanism, if A/B do not import C or D, then only 'double abc' will be visible. But also because each abc lives in its own namespace, depending on which module defines it. This is not an issue unless both 'abc' are visible, then you need to write, for example, A.abc or C.abc to disambiguate.

**87** In C, if you have a project of 50 modules, and each includes the same large header, then compiling the whole project means processing the header 50 times. In M, the header (or module here) is processed exactly once.

**88** When turning a project into a DLL or shared library, then I suspect that with C, every file-level name in every module that is not static (ie most of them) will get exported from the DLL, and visible to other programs and libraries (and at risk of being written to).

With M, only names with the 'export' attribute will be exported from the program. ('global' shares between modules, 'export' additionally between programs)

**89** In C, only functions and variables are officially exported (as symbols and objects) between modules. Types, structs, enums have to be shared by making sure each module sees the definitions, usually by a shared header file. With M, 'global' works also with types, records, enums, named constants and macros.

**90** C has a crude textual macro system. M has never had anything on the scale of C. More recently, a simple macro system for expression terms only has been introduced. Macro names follow proper scope rules, and can be imported and exported.

**91** C is funny with namespaces, of which there are three sets (nothing to do with the namespaces that come from encapsulation). First there is the regular namespace for most identifiers. Then there is a separate namespace for struct and enum tags. Finally, labels have their own namespace. So you can have:
````
A:; struct A A; goto A;
````
A is used as a label; as a struct tag; and as a variable name. M has just these one namespace for these. (In all cases these are top-level names that do not appear after a ".", such as struct member names.)

**92** With lexical scopes, each C module (translation unit) really has one scope at file-level. But inside a function, each {...} block introducesa new scope; an unlimited number of them. This allows the same identifier, such as 'A', to be exist as multiple instances across different scopes, sometimes with overlapping lifetime, possible with different types. Sometimes two of them within the same block, when a new 'A' is declared half way through the block.

M has exactly 1 function scope. C only has a single function scope for label names.

**93** C struct handling is a mess. First of all, structs have this totally useless 'tag', which lives in its own aforementioned namespace, but they can also typedefed, just to add to the general confusion. M has no type tags.

**94** Structs can be declared literally anywhere: inside a typedef; by itself to defined a struct tag; or you can declare a struct, and some instances at the same time. Or declare a struct inside another struct. Or inside a parameter list. Or you can define incomplete structs to be filled in later. Or structs can be anonymous (no tag or typedef). (A mess, as I said).)

M's structs, or records, can only defined as a formal, named user-type:
````
record date =
    int d,m,y
end
````
Now you can use 'date' as a type. In C, you can do this too, but it is optional (and people like to throw in tags with the same name as the type):
````
typedef struct {int d,m,y;} date;
````

**95** Unfortunately, for self-referential structs, #94 won't work:
````
typedef struct {int d,m,y; date* nextdate;} date;
````
because the 'date' in `date*` is not yet defined. And you can't have a forward declaration of a typedef, only a struct tag. In M it's easy:
````
record date=
	int d,m,y
	ref date nextdate
end
````
Not just because the type name occurs first, because out-of-order definitions work too.

**96** Type aliases in C are created with `typedef` as everyone knows. Except with complex types, the old type will sometimes wrap itself around the new type name. It can get confusing. In M, it is this, *always*:
````
    type newtype = oldtype
````

**97** C being C, typedefs can be used to define multiple aliases, each with their own type modifiers:
````
    typedef int A, *B, C[4];
````
Here M requires three type statements:
````
type A = int
type B = ref int
type C = [4]int
````
**98** Few know that typedef can be used not just for normal types including function pointers, but actual functions (several functions if you want!)
````
    type int F(int a, int b);
````
However, compilers limit what can be done with these; some allow it to be used to declare multiple functions sharing the same signature, just different names; one or two allow function definitions too. It's all too confusing for M, which only has conventional functions.

**99** On the subject of typedefs, probably few know you can do this too:
````
const unsigned long long int typedef T;
````
In M, 'type' is strictly on the left! Unless you want `global type` that is.

**100** About forward references. It is annoying in C that when you call a function F(x), then F needs either to have been defined earlier on, or a prototype needs to have appeared first. (As mentioned, compilers are not that fussy and just make incorrect assumptions if F has not been encountered.)

It means writing functions in a certain order, or interrupting work to create a forward declaration, which needs to exactly match a definition, or generally making it harder to just move functions around.

In M, that is never a problem because of out-of-order definitions.

**101** C's for-loop is something I've never thought highly of; you need tell the compiler exactly how to implement it, as though it doesn't know how. Exa mple:
````
for (i=A; i<=B; ++i) {}
````
The loop variable occurs 3 times (the number of times I've got one wrong ... and that error can't be detected); you need to tell it the exact compare op, and exactly how to increment the loop variable (really?!). In M the equivalent is either of:
````
for i:=A to B do ... end
for i in A..B do ... end
````
The long-winded C version is needed even though 98% of such loops are for basic iterations ... or would be if people weren't encouraged to cram as much stuff into a one-line for-loop header as possible. (Which means, when perusing code, analysing such loops to figure out what behaviour the author intended.)

**102** For a repeat N-times loop, there is no special contruct; most people use `for`, with a dummy loop counter. M uses:
````
to N do ... end
````
**103** For an endless loop, people use while (1) or `for(;;)`, M uses:
````
do ... end
````
**104** M's for-loops have an 'else' portion like Python. It is executed on normal termination (eg. when search fails) and not on a break.

**105** C uses break and continue for an early exit from or (I assume) to proceed to the next iteration. M uses 'exit' and 'nextloop' for those. However, in C it is not possible to break out of a loop, if currently inside switch statement inside the loop.

**106** M additionally has a `redoloop` control, to rerun the current iteration

**107** All of M's loop controls work with nested loops. You need to specify the loop number, 1 (or omitted) being the current level. Most of the time however, they will use, for example, 'exit' for the innermost loop, or 'exit all' (same as 'exit 0') for the outermost.

**108** M's for-loop index is automatically declared as needed

**109** M for-loop iterations can be conditionally executed:
````
for i in A.bounds when A[i] do ... end   # execute body when A[i] is not zero
````

**110** As shown in #109, A.bounds can be used to extract a range to iterate over (see #101). A needs to be an object that carries its length, either a fixed-length array, or a slice (covered below)

**111** C's `while(c){}` and `do {} while(c)` loops in M would be `while c do ... end` and `repeat ... until c` (latter with reverse logic)

**112** For-loops can also iterate over values in an object (a new feature I haven't used much yet):
````
for x in A do ... end             # x can be automatically declared as a suitable type
for i,x in A do ... end           # here, i is the index used (iterates over A.lwb to A.upb)
````
A needs to be an array or slice

**113** C has Switch, a rather peculiar construction, which can contain case labels scattered higgledly-piggledy anywhere within the following statement. M's Switch is properly structured:
````
switch x
when a, b then ...
when c then ...
else ...
end
````
**114** C's Switch has an optional `default:` label, but again this can appear anywhere, including mixed up with case labels. M's version is just 'else' like other statements, and goes near the end. One issue with `default:` is that misspell it as `defualt:` for example, and the error is not detected; it's just a regular label.

**115** C requires you to write:
````
case A: case B: case C:
````
In M it is:
````
when A, B, C then     # (Some C extensions may allow a list)
````
**116** C requires you to write:
````
case 10: case 11: case 12: case 13:
````
In M it is
````
when 10..13 then    # Some C extensions may allow case 10...13, like gnuC I think)
````

**117** C Switch requires a 'break' statement after the code following each set of case labels, unless you specifically want to fallthrough. So C switch statements are usually littered with 'break', and you don't get a warning if you forget one.
M's switch doesn't need breaks. (It doesn't have fallthrough; it does have 'recase', but that's an experimental feature I won't cover here.)

**118** If you're inside a switch branch in C, you can't use 'break' if you are also inside a loop. (Not an issue with M)

**119** Both C and M switch statements take an integer control expression, and require constant case/when labels. But M has an alternative form using Case:
````
case X
when a, b then ...
when c then ...
else ...
end
````
It's a plug-in replacement for Switch, but X can be any type for which '=' is defined. And when-expressions can be runtime expressions. This version is implemented with sequential testing.

**120** Both M's `Switch` and `Case` expressions have looping versions: `Doswitch` and `Docase`. Here it is necessary to exit the loop using `exit, goto, return or stop`.

**120A** `doswitch` also comes as `doswitchu`, which uses more efficient multi-site dispatching which helps with branch prediction.

**120A** There is also `doswitchx(table)`, which does away with the extra indexing operation, as `table` is set up to point to its jumptable

The alternate for 120A/B is to use explicit label pointers and 'computed goto', only in gnu C.


**121** M has a 'long' form of if-statement written as:
````
if c1 then
elsif c2 or c3 then
elsif c4 then
else
end
````
Should be familiar to C users as the C preprocessor has a similar construct; it has more advanced syntax than C itself! This statement is inherently linear rather compared to a nested if-else-if-else chain.

**122** One alternative to #121 which I have a syntax for, but not sure if yet implemented, is this form of Case:
````
case               # note: no common control expression
when c1 then
when c2, c3 then
when c4 then
else
end
````
This is equivalent to the example in #121. In this form, maintenance is simpler (eg. you can delete the c1 line, or add another before it, without messing with if/elsif)

**123** This is an ugly feature which I do use, but am reluctant to share. But I allow my three kinds of 'long' conditional statements, `Switch`, `Case` and `If`, to be mixed up:
````
switch x1
when a, b then
when c then
elsif x2 then
elsecase x3 then
when d, e then
else
end
````
The alternative is increasingly nested statement with extra indents.

**123** M has these 'short' control flow statements: `goto, return, exit, redo, next, stop`. Each of these can have an optional conditional suffix:
````
goto L when c1
exit unless c2
return 5 when c3           # 'if' can start an expression, so can only use 'when' here
````
These are simply equivalent to 'if c1 then goto L fi' but shorter to write and the control statement is more prominent.

**124** 'Unless' was used in #123, and actually there is such a statement:
````
unless cond then
else
end
````
It's just 'if' but with reverse logic.

**125** In place of C's `exit(0)` (which requires stdlib.h), M just uses:
````
stop             # exit code 0
stop N           # exit code N
````
A very simple feature but I love using it.

**126** M has `goto` of course, with a couple of tweaks; the following are all equivalent:
````
goto L
go to L
L              # 'goto' is optional, if you don't like the idea of writing it
````
Another tweak is that labels need to be written as L:: not L: (at present ":" is heavily overloaded for other purposes)

**127** M has some new features for use in conditionals:
````
if A in B..C then          # true when A>=B and A<=C
if A not in B..C then
if A in X.bounds then      # (version not yet implemented; .bounds to be rolled out more)
````

**128** One more:
````
if c in [13,10,9] then       # (so c is a white-space character)
if c not in [13,10,9] then   # (so c isn't a white-space character)
````
The expression `x in [a,b,...]` is true when x matches at least one value in the list.

(Both `a..b` and `[a,b,c]` are constructs from my dynamic language which has actual range and bitset types. Here they are just syntax.)

**129** C allows you to write `A==B<=C`, but the result is not meaningful (eg. A=B yields 0 or 1, and that is compared with C). M has proper chained comparisons:
````
if A = B = C then        # all equal
if A <= B <= C then      # all in order
````
The implementation evaluates B only once (earlier ones did middle terms twice).

**130** M has experimental multiple function return values (limited to 3-4 scalar types):
````
function fn(int a,b)int,int =
	return (a+b, a-b)
end
````
See #132

**131** M has a form of multiple assignment:
````
(a, b, c) := (10, 20, 30)  # same as a:=10, b:=20, c:=30
(a,b) := (b, a)            # can rotate (this does a Python-style swap, but M has swap anyway)
````

**132** Multiple assignment is needed to make use of multple function returns:
````
(a, b) := fn(10,20)    # use both values
a := fn(10,20)         # discard 2nd
fn(10,20)              # discard both
````

**133** Probably you will have noticed by now that M uses "=" for equality, and ":=" for assignment. This makes them less
susceptible to being mixed up as they often are in C, especially in conditional expressions.
 
**134** For its binary operators, C has 10 precedence levels for 18 operators. Many have famously unintuitive precedences.
M has only 5 levels for the same operators, grouped into these categories
````
1 Mul etc
2 Add etc
3 Comparisons, = etc
4 And
5 Or
````
**135** Shift operators scale a number in the same way as multiple and divide, so in M they have the same precendence.
`a<<b+c` and `a*b+c` are both parsed as `(a<<b)+c` and `(a*b)+c`

**136** M has a power operator `**`, which is one higher level than multiply/divide. This is also properly overloaded for integers and floats.

**137** M also has a proper, overloaded `abs` operator. In C, you need abs, labs, llabs, fabs ... plus the appropriate headers.

**138** Some mathematical functions are built-in to M. They are classed as operators, which also means that parentheses are not needed, but they look better with them. They include `sqrt` and `sin, cos, tan`. Some are overloaded for ints too.

**139** M's `sqr` operator simply squares its operand (either int or float): `sqr(x)+sqr(y)`.

**140** M uses a very small number of built-in constants, one of them is `pi` (3.14159 etc). Another two are `true` (1) and `false` (0).

**141** Another special constant is `nil`, which has type 'ref void', used to initialise pointers. Unlike C, pointers cannot be initialised with zero.

**142** A big feature of M is named constants. C either used `#define` (very crude; no scoping; need to reevaluate each instance); or `enums` (limited to int32 types); or `const int` (not counted as compile-time expressions). In M:
````
const a     = 100          # type is infered
const int b = 200
const c     = a+b
````

**143** Like C, M has augmented assignments (`+=` or `+:=`). But unlike C, they do not return a value, so cannot be used in expressions. I just thought they made them too complex, especially with right-to-left precedences of these operators. Also there is some confusion about what:
````
a +:= b -:=c *:= d
````
might mean when a, b, c, d are mixed types.

**144,** M also has augmented assignments for unary operators:
````
-:=a          # a:=-a
abs:=a        # a:=abs a etc
inot:=a
````

**145** C's 2-way select operator is `a ? b : c`. Apart from being ugly, it does not require parentheses. Although they can be added, people tend to leave them out, making it hard to understand an expression because of complex precedence rules.

M writes it as `(a | b | c)`; parentheses are mandatory. (Note this is just another way of writing 'if a then b else c fi'; see below)

**146** M extends this to an N-way select construct:
````
(n | a, b, c, .... | z)
````
N is a 1-based index; it will evaluate the nth expression in the list if in range, otherwise z.

**147** Unlike C, 2-way and N-way operators can be used as Lvalues, eg:
````
(a | b | c) := 0       # set either b or c to zero
````

**148** A big difference from C is that in M, expressions and statements are equivalent (one more thing taken from Algol 68).

All statements return some value, even if it is `void`. `Return X` can be written as `X`, unless this is an early return, as the body of a function will have a value. In the case of a block (sequence of statements/expressions), the value of the last in the block is used.

**149** Because of #148, the following statement types can be used in expressions where a value is needed:
````
if-else-end
if-elsif-else-end
case-when-else-end
switch-when-else-end
````
For this purpose, each needs an 'else' part, otherwise there is a type error. Each of these can also be an Lvalue (used on the LHS of an assignment).

**150** `if a then b else c fi` has a compact form which looks like this: `(a|b|c)`. Yes, this is the 2-way select op from 145; it's not a separate feature, just a different way of writing 'if'. Note that #146 does not have a corresponding long form.

(Note that my own code uses block delimiters such as `fi` (as in `if-then-else-fi`), `od` and `esac`, but I've spared everyone that and use only `end` here, not even `end if` etc. M accepts alternatives.)

**151** I've already said that C's `{...}` braces are not used for M blocks (or for record defs etc). But C uses them also around initialisation data. M just uses regular (...) parentheses for that:
````
[10]int A = (10,20,30,40)
````

**152** C uses semicolons to terminate statements. M uses them to separate statements. But you will very rarely see semicolons in M code, for these reasons:

* End-of-line is interpreted as a semicolon (unless the last symbol was `(` `[` `,` or a binary operator).
* Extraneous semicolons are ignored

**153** M has bit indexing, written as `A.[i]`, which extracts the i'th bit of integer A as 0 or 1. The equivalent in C involves shifting and masking. Bits can also be assigned to: `A.[i] := x`; this sets the i'th bit of A to x (0 or 1). Bits are numbered from 0 (lsb) to 63 (msb).

**154** There is also bitfield extraction:
````
x := A.[i..j]      # extract bitfield of bits i to j inclusive into x
A.[i..j] := x      # insert a bitfield
````
**155** There are a number of built-in bit/bitfield operations, such as:
````
A.odd         # Same as A.[0]  (these two are read-only)
A.even        # Same as not A.[0]
A.lsb         # Same as A.[0..7]
````

**156** Casts in C are written as `(T)X` (where one problem is knowing the span of X). M has two forms:
````
cast(X, T)         # general syntax
T(X)               # compact syntax when T is simple (there is a grammar ambiguity with a complex T)
````
**157** There is also a neat automatic cast:
````
A := cast(B)
````
Here you don't need to go and hunt down the required type; whatever is demanded here, is applied to B. Provided it is allowed.

**158** Type-punning doesn't properly exist in C. You usually do things like `*(T*)&X`, but this only works when X is an Lvalue. In M:
````
cast@(X,T)
T@(X)
````
Both of these work with Rvalues, example `print int@(f(x)+1.0)`, which interprets the floating point result as an int.

**159** C programs extensively make use of `const`, but to me it adds too much clutter, and gives a false sense of security. It can be easy to get wrong too:
````
int const * P
````
The 'const' here I believe applies to the pointer target, not the pointer. M doesn't have any equivalent. But it is introducing 'let' as a prefix when declaring variables, which tries to stop you updating then. Not used much at the minute, but if you write a loop like this:
````
for i := A to B do ... end
````
then if 'i' is auto-declared, it will be using 'let', which stops you changing it inside the loop. (There are also `in`, `out` and `inout` attributes for function parameters, but I don't do much with those either. The fact is the programs still work without all this stuff!)

**160** This one is a very old feature which I'm always thinking of getting rid of, but it's still hanging in there:
````
real X
int A @ X
````
What this says is that A should share the same location in memory as X.

**161** M has recently introduced Slices, which are a kind of view into an array or string. A slice consists of a (pointer, length) pair. If a function takes a slice, you can pass it an normal array, and it will construct a slice; or pass it a slice of the array such as `A[i..j]`. It's too big a subject for here. (Also it's buggy at the minute)

**162** M has some limited reflection in that compiled executables contain a table of all the functions in a program, with names and addresses. This is used mainly to identify sets of handlers and build function tables are runtime, instead of having to maintain them manually.

**163** Briefly touched on, M retains print and println as *statements* rather than functions. They work like this:
````
println A, B, C
println =A, =B, =C                  # Each with a label
println @F, A, B, C                 # To a file
println @S, A, B, C                 # To a string
fprintln "# + # = #", A, B, A+B     # Formatted
println A:"Z S' B"                  # Show A as binary, with leading zeros, with ' separators every 4 digits
````

**164** Also retained are read and readln statements:
````
readln A, B, C
````
This waits for a line of input from the console, copies into a buffer, then reads 3 values into those variables.

**165** You may have noticed that in this C:
````
char S[] = "ABC";
char* T  = "ABC";
````
S and T are different types, yet the RHS is the same type; how is that possible? Well, M is stricter and it is not possible. `"ABC"` has type `char* (ref char)`
not `char[]` or `[]char`. S can be initialised in M as either:
````
[]char S = s"ABC"        # equivalent to ('A','B','C')
[]char S = b"ABC"        # equivalent to ('A','B','C',0)
````

A normal `(...)` expression can still be used, but would be more laborious.

**166** C has some very quirky behaviour initialising complex data of nested structs and arrays. Normally you'd write the data
organised with `{...}` into nested lists that match the type of the data. If you have too many {,}, it will complain. But if you have too few, it doesn't! Actually no matter how complex, how deeply tested T is here, this initialisation will always work:
````
T A = {10,20,30,40,50,60,70,...};
````
C will somehow organise that linear sequence into the right structure, but how do you know it is getting into the right places? This is very sloppy.

M requires that such data exactly matches the structure of T.

**167** Another thing that C does, is allow fewer entries than are necessary, and the rest are zeros. This is actually not too bad a feature, but M's strictness doesn't allow that at present. What is does have however is a built-in way to clear objects:
````
T A = empty   # these all do the same (new feature so allowing alternate possibilities)
A := empty
clear A
````
(`empty` has now been removed, only `clear` remains.
**168** My systems languages have always had inline assembly; M is no exception. Some C compilers have it, but they tend to use the gcc approach, which is very complicated and very ugly (I think you also need to enter the assembly code as strings or something). In M it's more like:
````
assem
    mov D0,[A]
    add D0,[B]
    mov [C],D0
end
````
No problems with stepping on the compiler's toes regarding register usage, as optimisation (such as it is) is turned off for functions that use inline assembly. (Usually, the whole body of a function will be assembly anyway.)

(I'm working on a scheme to to allow inline assembler to work with 'optimised', that is, work with its register allocator)

**169** M can extract the types of variables, print them out, compare them etc. Example:
````
    real x

    println i64.type           Shows 4 (internal code for i64)
    println i64.typestr        Shows "i64"
    println x.type             Shows 7
    println x.typestr          Shows "r64" (real = r64)

    println x.type = i64.type  Shows 0
````

**170** Since #171 uses `{,}`, I need to explain what M uses braces for. They were reserved to enclose deferred code, code which is executed later rather than when encountered. For example, lambda functions, but I never got round to that.
(But that syntax *is* used the companion Q language.)

**171** M has an extra attribute called 'exportq'. (Feature has been dropped.)

**172** M has an incredibly useful feature that I call Tabledata. Normally used to define a set of enums, and parallel arrays of data at the same time. Or sometimes just parallel arrays. The nearest C might have is the very ugly x-macros.

A good example is the [ax_tables.m](Examples/ax_tables.m). The "$" you see there is a device that picks up the last
enum name as a string literal.

(The feature has been split into `enumdata` (enumerations + parallel arrays) and `tabledata` (parallel arrays only).

**173** C is designed for separate compilation: compile all the modules, then use a linker. And actually, most projects use a makefile, or Cmake, or any of a set of increasingly bigger and more complex tools.

M is very different: the only tool is the compiler, and the only input it needs is one file: the name of the lead module. No build system needed. This is how the current M compiler (bb.exe) builds itself:
````
C:\bx>\m\mm mm
Compiling mm.m to mm.exe
````
Even on my slowish PC, this takes under 0.1 seconds. This is for 35+ modules, 30+Kloc, generating a 400KB executable.

**174** The M compiler also has an option to combine all sources of a project, and any support files, into a single .ma source file. This format is handy to upload to Github for example, as backup. Unlike ZIP, it is not binary, and bb.exe can build it directly in that format:
````
C:\bx>\m\mm -ma mm
Compiling mm.m mm.ma         # create the 1-file version

C:\bx>\m\mm mm.ma            # compile that version
Compiling mm.m---------- to mm.exe
````

**175** For generating DLL shared libraries from a project, bb.exe uses the -dll option:
````
C:\bx>\m\mm -dll bignum
Compiling bignum.m------ to bignum.dll
Writing exports file to bignum_lib.m
````
This also writes an exports file, which is an M module specifying bindings, that I'd have to write manually for other languages.
To use this library in a project, I write:
````
module bignum_lib
````
This picks up bignum_lib.m (otherwise 'module bignum' would simply import bignum.m itself, incorporating the implementation directly instead of using it as an external library).

**176** M has a form of Doc-strings, but it needs finishing off. Doc-strings are special comments that start with "#" rather than "!", just before and within any function. With the -docs option, all functions with doc-strings are output to a text file. This lists all the function signatures and all the doc-string comments.

(Temporarily removed)

**177** M has a form of runtime, dynamic library interface (what C programs might use the complex LIBFF for). I had intended to create special language features for it, but at the moment it exists as a standard library 'osdll', and implemented with inline ASM.

**178** Here are a few things I should have covered above (and the numbering means I can't go back).
First, C's dereference operator is the `*` which is written in prefix format, a bad choice has many uses end up
as `(*P)[i]` or `(*P).m` or `(*F)(x)`. But you rarely see these in code for various reasons (eg. see #179).

In M, the operator is "^" written after the pointer (taken from Pascal). Those examples become the cleaner `P^[i]`, `P^.m` and `F^(x)`. (However, the current M compiler allows those inbetween uses of "^" to be dropped; it will insert the derefs internally.)

**179** C has the odd "->" operator, which is used in place of `(*P).m`, so it becomes `P->m`. Why it was introduced, I don't know; maybe `(*P).m` was too ugly. But it only covers one level of indirection; here the left column shows how many `*` pointer levels the type of P has:
````
*  Normal       With ->      M         M (optional ^)

0  P.m          P.m          P.m       P.m
1  (*P).m       P->m         P^.m      P.m
2  (**P).m      (*P)->m      P^^.m     P.m
3  (***P).m     (**P)->m     P^^^.m    P.m
````
The -> helps with level 1, but looks ungainly with higher levels. M consistently looks cleaner; it never needs parentheses, and being able to omit ^ makes it even better.

**180** Struct members in C are always laid out according to alighnment needs. This makes it harder to construct an exact layout because of padding bytes being silently inserted. You need to use `pack(1)` to turn that off.

M always works with `pack(1)`. But to get a struct compatible with a C struct in an API can be difficult, so there a `$Caligned` flag is used to turn on C-style alignment of members. (I could only do that because I've written a C compiler and have the algorithm needed!)

**181** C has bitfields in structs, as independent members. But the way they work is implementation dependent. If you intend a bunch of bit fields to amount to 32 bits, say, and occupy one exact int, but you make a mistake and there are 33 bits, it it might use two ints or 8 bytes, or maybe 5 bytes. It will not tell you.

M doesn't have C-style bitfield members, but another scheme where you declare a normal member to contain the bitfields:
````
record dummy =
    byte flags : (scope:2, isglobal:1, id:4)
end
dummy A
````
Here, you can access those bitfields as though theye were normal members: A.scope:=2, print A.id, but they are contained within A.flags. You can access them all at once via the container member:  A.flags:=0. If the bitfields overflow the container type, it will tell you.

**182** Most C implementations still have 32-bit ints. M has moved on with 64-bit everything:
````
int       same as i64
word      same as u64
real      same as r64 ('real' mean floating point like C's float or double)
ref void  M only works on 64-bit machines so will be 64 bits too.
1234      has type i64 (in C it will be i32)

````
So everything is conveniently the same size.

**183** C has a preprocessor that allows conditional compilation of code. M has no conditional code except at the module
level; there is a scheme to map one module to another depending on certain flags.

(This is dropped. Different configurations of programs either use a different lead module with alternate
lists of modules, or modules can be commented in or out.)

This means a freedom from sourcecode plastered with #if and #ifdef blocks.

**184** M's macros have been briefly mentioned. Here's one actual example (used here in place of inline functions which don't exist):
````
global macro isframe(d) = d.id in [frameid, paramid]
````
They only work within expressions. The body of a macro is one expression, but since statements are expressions, and you
can write (s1; s2; s3), they can be as big as needed. But macro bodies cannot define new symbols. (They are mostly used with the
inline assembler)

**185** M has a companion scripting language called 'Q'. It is dynamically typed and interpreted. Its syntax is pretty much
the same as M's syntax. It is currently being reimplemented to be better suited for embedding.

Usually such pairings of languages are very different: C and Lua; C and Python, with interfacing between them more awkward.
Partly because C has no knowledge of their needs.

**186** M uses the `$` symbol for odd jobs; currently it has about 3 meanings depending on context. Not a very elegant feature, I'd prefer to keep quiet about it, but...
````
$ used in enumdata blocks will turn the last defined enum name into a string literal
$ inside an array index expression like A\[$\] is equivalent A\[A.upb\]; it's the index of the last element.
$ inside list of print items (most usefully at one end) emits a space (using " " would interact with the logic that
automatically spaces items out)
````
I guess it's a little like 'static' in C.

**187** One more thing about $ is that M allows it in identifiers. It is a far better choices for reserved or internal identifiers as it is more visible than _ or __ favoured by C. Many C compilers accept $, but some don't (tcc) or only in certain parts of an identifier (lccwin).

**188** C programs use a main() entry point; M normally does the same but it never take parameters:
````
proc main =
    println "Hello, World!"
end
````
Command line parameters are accessed via globals 'nsysparams' and 'sysparams'.

**189** If an M module includes a function with the name `start`, then it is automatically called from the program start-up code.

**190** M has primitive OO features in form of encapsulation: records can contain, not just members, but also named constants, types and functions:
````
record date =
    int d,m,y
    proc printd(date &d, ichar sep="/")=
        fprintln "#####",d.d, sep, d.m, sep, d.y
    end
end
...
date d:=(30,3,2021)
date.printd(d)          # or:
d.printd("-")
````
**191** M has a form of nested functions (limited in that they can't access stack-allocated variables of enclosing functions, but they can access static variables, types, named constants etc). (Nested functions are a gcc extension in C.)

**192** There is a special **istrue** operator, where istrue X is equivalent to !!X in C.

**193** There is a `sign` operator, applied to numeric expressions, which returns -1, 0, or 1.

**194** Enums in M are usually 'open' names as in C. So that the two 'green' enums for example in:
````
enum {red, green, blue};
enum {red, amber, green};
````
will clash. In M, enums can be defined under an umbrella record type:
````
record rgb =
	enumdata =
		red, green, blue
	end
end

record lights =
	enumdata =
		red, amber, green
	end
end
````
But now, you need to write `rgb.green` or `lights.green` to disambiguate (the type system really is not so sophisticated as it can work this out from context).

**194** There is a 'clamp' operator, used as `clamp(x, a,b)` which returns `x` but adjusted to be within the range `a..b` inclusive.

**195** In C, you can legally write like this:
````
a + b;
a == b;
````
A compiler may or may not warn. M doen't allow this. In order to evaluate such an expression (for the purpose of checking an expression, or to get it loaded into a register, or any other purpose) you have to write them as:
````
eval a + b
eval a = b
````

**196** Both languages allows hex floating constants (M also in binary and other bases). But C does it in a peculiar way:
````
0x100p10       In C, this is the value 262144.0
0x100p10       In M, this has the value 4722366482869645200000.0
````
Why the difference? In M, this means `0x100*(16**0x10)`, or `256*(16**16)` as you might expect, since `100e10` is `100*(10**10)`.

But in C, it means `0x100*(2**10)`, or `256*1024`. So although it supposedly in hex, the exponent is decimal, and represents a power of two!

(M now longer allows non-decimal float constants. But the point remains that 3 different bases are involved
in C's version.)

**197** C has reserved words like most languages, and like M, which cannot be used as identifiers. But M has a special escape for such names:
````
int `int, `for, `if;
`int := `for + `if
````
(The \` prefix wil also preserve case, allowing `abc`, `Abc` and `ABC` to be distinct identifiers just like C. This is sometimes used for external interfaces. It is also used for automatic translators from C into M.)

**198** #173 stated that the M only needs one input file to build. It used to need also the names of external
DLL libraries, but those are not brought inside the program, and are declared with the project info in
the lead module.

Even this is optional if the DLLs needed can be infered from the `importdll` blocks.

**199** This one may seem rather trivial, but I find it incredibly handy and miss it with other languages. In #198 I used the example:
````
mm file
````
Notice there is no extension used: I write 'file' not 'file.m". This is a compiler for the M language and its primary input will be a source file with a .m extension; why it is necessary to specify it? Same argument can be applied to many other languages including C. (My bcc C compiler also only needs 'bcc hello', every other ones NEEDS 'xcc hello.c'.)

**200** Just one more to end up with a round 200. C has functions strcmp and memcmp which returns a 3-way value. So to compare the equality of two strings may need:
````
if (strcmp(s,t)==0)
````
This is not so intuitive; it's easy to forget the ==0. M uses these two functions:
````
if eqstring(s,t)
if eqbytes(a,b)
````
which just return true or false. Very easy. Sure you can trivially emulate these in C; but here they are built-in. Here's an example from a 750Kloc C file called gcc.c:
````
# define STREQ(s1, s2) ((strcmp (s1, s2) == 0))
````
Seems to me that someone else thinks this is a good idea!
