## C3 Notes

I've made here some comparisons between working or tentative featueres of [C3](http://www.c3-lang.org/), and similar ones in my own systems language 'M'. (That one is now more of a private language.)

But there are also general comments.

### C to C3 Conversion

I made an attempt at a C to M conversion, but it was very poor. It the end it was simply used as a way to view C source, in my own more Algol-like M syntax, but the resulting code can't run as the semantics are too different. As one example, C's chaotic 'switch' statement cannot always be reliably translated.

I do make use of the translator for C library headers, to get a starting point that can be manually edited to end up with an interface file in my language. One killer here is dealing with C's crude textual macros (my language doesn't have text-based macros).

### Transpile-compile via gcc, tcc, clang

My M compiler only directly targets x64 native code for Windows. For anything else, I have reintroduced a C source target, which can be compiled with gcc or tcc (clang doesn't work on my machine), and which extends the range to 32-bit machines, Linux, and ARM processors.

But they have always had difficulties, and the C target doesn't support many features of mine, so it effectively works for a subset only. (One big difficulty was using printf format codes, either used implicity, or explicitly. When M calls printf as a foreign function, which format to use for my int64 types? To work across platforms, it needs to be a macro like PRId64, but that only exists if coding directly in C. Use of \*printf functions had to be all but eliminated.)

Another problem was that, while the M compiler is very fast (near instant for programs a few tens of thousands of lines) it would hit a brick wall as soon as gcc was invoked, so the favoured C 'back-end' compiler is tcc.

(This is significant because M is a whole-program compiler; all modules are compiled, and the output file, whether .exe or .c, is a single file. So for a C target, it will produce a large monolithic C file.)

### @bitcast

This is type-punning. While M normally casts as T(X) to convert X to T, type-punning uses T@(X) to reinterpret bits without conversion, which seems to be what @bitcast does.

It also works for expressions, while the equivalent in C only works for lvalues (eg. \*(T\*)&(X)), so another thing that won't easily translate to C.

(I recently made changes which mean that T(X) syntax is ambiguous - T can introduce a declaration, or a cast. So except for the simplest cases, casts are now written as cast(X,T) and cast@(X,T) for type-punning, which is not far from your bitcast.

Note that this also works as cast(X) where it will automatically cast to whatever type is called for, which is incredibly handy.)

### Precedence Levels

Looking only at binary operators, C had too many. C3 has fewer, but in M there are fewer still, as <<,>> have the same precedence as * and / (since they do the same job of scaling up or down).

And ^, | and & have the same as + and - (since there is no particular reason why one group should be higher or lower than the other, hence it makes it harder to remember which is which).

### Member access using . even for pointers

I've done similar. Older versions of M had to have the exact number of dereference operators (there, a postfix '^' like Pascal).

Now, the dereference can be added by the compiler, to give less cluttered code, if less transparent.

(But there remain tricky syntactic issues; what had been written ++P^.m for example, to increment the member .m, is parsed as (++P).m when written as ++P.m. I need to take care until I find a solution.)

### Array copy by values

As mentioned in my reddit post, this another feature that is a massive change from C. And one of the many things that make conversion *from* C hard (also indexing of pointers with array syntax).

(M also uses value arrays, but with limited support, as from my experience of having them for decades, they were rarely used. At least, there isn't a big hole in the type system as happens with C.)

### Fixed arrays

You say the length of the array is part of its type. This might be making the same mistake as Pascal (one of its biggest).

Typically, you will have a function that takes a pointer to array, but you might variously want to pass it a pointer to an int\[4\] array, or an int\[400\] array (with whatever arrangements to pass the length). Or are there are features to help with that (I think I remember seeing slices)?

Yes, looking further, there is a chart of types int\[4\], int\[\], int\[:\], int\[4\]\*, int\*, which I have to say looks somewhat bewildering (what's difference between copy and assign?).

(M uses these:

    [N]T        Array [N] of T
    ref[]T      Pointer to array of T (very rare that a bound is used, but ref[] is compatible
                with any ref[N], with lengths dealt with explicitly)
    slice[]T    View-slice array of T, comprising (pointer, length), always set at runtime

Slices not really working yet, but you can turn \[\]T into a slice, which is about as far as it goes. Slices are mainly a mechanism used in function parameters, so that you can pass a slice\[\]T value, or more usefully any \[\]T value, with the length taken care of implicitly.)

### Function named arguments

(Normally called keyword parameters.) You give this example:

    testNamed(times = 1);
    
Is "=" not an assignment in an expression as it is in C? If not, then that is another huge difference not mentioned in your list of changes.

It would be ambiguous here because 'times' could be a local variable, that you are setting to 1, then passing that same value to testNamed.

(M uses ":" for that purpose. There, "=", means equality, so it would still be ambiguous. In an older language, I did use "=" for named parameters; but I had to use (times=1) (equality) to disambiguate from times=1 (keyword parameter).)

### Naming Rules

You don't allow $ in identifiers. While not officially allowed in C, many compilers support $, so that if translating from C to C3, that could cause problems. As could that 31-character limit (why?).

The restriction on modules names (no upper case) sounds peculiar. With module systems I've seen (and with the one used by M), the module is also the main part of the file name.

### Build system

I found that a little confusing. My M compiler (called 'mm') can be run like this:
```
   mm prog                        # equivalent of 'compile'
   mm -run prog                   # equivalent of 'compile-run'
```
This builds (and can run) the entire program (= one .exe file) given only the lead module prog.m.

Do **init** etc serve the purpose of an IDE, but from the command-line? Some people may prefer to use their own!

I take it that the examples with "\[\[executable\]\]" and so on are written inside C3 source modules?

I find this part confusing too. (But then I've never managed to use make files either.)

### Importing modules

The module system I also found hard to understand. You use example files "file_a.c3" and "file_b.c3", but both start with the line "**module foo;**". But now I look further up, and you say that a module can consist of multile files. Now it is definitely confusing!

Also, since the module name, say "foo", is only found inside, not only one file, but possibly half a dozen files, how is the compiler going to find the files corresponding to module foo, when it sees 'import foo'? Does it have to look inside all of them?

Under Visibility you say: "All files in the same module...". You mean all files comprising the same module?

I'm sorry, but this does look a very poor way of implementing modules.

(M has a simpler module system. Each source file comprises a module, with the same name as the file (module names must be valid identifiers so that imposes a restriction).

So no 'module' keyword. To import a specific module, you say:

    import files

to import the source file files.m. To use an alias, write:

    import files as fs

However, most imported names, eg. files.openfile or fs.openfile, don't need a qualifer, provided there is no ambiguity, so can be written as just openfile.

One big feature here is that modules can be mutually imported with circular dependencies. Which was very tricky to do, but goes in hand with the language's out-of-order declarations.)
    
### Crazy Ideas

#### Variable Alias

M uses:

    real x
    int a @ &x         # (both 64 bits)
    
Here a and x share the same memory. Older languages of mine went further, with aliases like this:

    [32]byte a
    int b @ a[5]         # b occupies a[5..8] (int is 32 bits here)

However that is a rarely used feature now. (I copied it from Fortran's EQUIVALENCE statement, going back some decades).

#### Removing imports

I think that will make the module system even more hard to get your head around! If everything is now implicit.

Rethink once more perhaps...

#### Binary include

Yep, I have that, written like this:

    []byte file = bininclude("zip.exe")
    
Although this binary version is not yet used much (and is implemented inefficiently). I do extensively use this form:

    ichar file = strinclude "lib.m"       # ichar is like 'char&'

This incorporates an entire text file into the program source, as a string constant (with control characters changed to escape sequences). I used this to include, for example, all the standard headers of my C compiler, so that it operates from one executable file, nothing else.

(With my dynamic language where strings can include binary data, strinclude can already deal with binary files.)

#### Case as a range

That is fairly standard (even gcc-C has it), why is it crazy?

#### Extended case

I have a form of that, but I haven't really used it yet. If you want something crazy that I *do* use, try this. First, in M, there are these 3 kinds of long statements:

    if ... then ... elsif ... then .... end if
    case expr when a,b,c then ... when d then ... else ... end case
    switch expr when a,b,c then ....   (same as case, but for constant ints only using a jumptable)

The crazy thing is being able to mix them up like this:

    if x then
       ....
    elsif y then
       ....
    elsecase z
    when 10, 20 then
       ....
    when 30 then ....
    elsif a then       # back to if
    ....
    elseswitch....
    
So at the 'else' point, you can switch to any of the three forms. All to save some indentation and nesting.

Less crazy, but also used a lot, are looping versions of case and switch, called docase and doswitch.
    
#### Generic modules

Possibly that idea belongs in this crazy section. But in any case it requires a better example of exactly how it would be used. And what does it emulate, if anything: classes, templates?


### Enums

You have .min and .max, which looks interesting. However, isn't there a danger of an assumption that the set of enums will be consecutive between .min and .max values, or that people may try and iterate between those?

Because it seems enums can still be arbitrary sets of values: {a = 64, b=64, c=a-1, d=3}. Here min/max will be 3/64, with one intermediate value of 63, and two values sharing 64.

The naming also suggests that the enums can be used in normal arithmetic and that the ordering is important.

(M doesn't do much with basic enums (except enums names can refer to each other in any order), but it introduces a 'tabledata' feature which can define enums as well as parallel sets of data. Different enough from enums that I won't go into it further.)
