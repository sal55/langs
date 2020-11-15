## New BB Compiler for Mosaic/M Language

The compiler has been revised internally with a tidier organisation.

Intended to be largely compatible with the M language which itself has changed little.

Main differences:

* Modest code optimiser added (-opt option). This can make some programs a bit faster and a bit smaller.

* Can generate DLL files directly (-dll option)

* The FFI for C libraries can directly deal with passing and returning structs by value; no need to inject explicit pointers or references. However no need to update libraries already modified; they will still work

* Block types (such as the value structs, plus value arrays), can be passed to functions, but via an implicit pointer. But no copying it done so take care.

* Block types can be returned from M functions but, unlike the FFI C version, uses a pointer into the callee function, not the provided by the caller, so the value needs to be consumed immediately.

Probably a few other things that I forget off-hand.

### Operating differences in the language:

**Loops** such as **for i in A**, which iterates over A's bounds, must be rewritten as **for i in A.bounds**

**Forall** loops such as **forall x in A** should be rewritten as "for x in A"

**Previously**, within the context of one print statement such as **print a,b,,c**, separation logic was maintained added a space between a and b, and suppressed using ",,". No gaps are added between a and b in **print a; print b**.

Now, for print statements not using "@", that context is program-wide. **print a; print b** includes a space between a and b. For extra control, the "$" symbol replaces the ",," (although that still works in between items):

    print $,a             # Suppresses any space that is pending from last print
    print a,$             # Suppresses any following space
    print a,$,b           # Suppresses space between a and b

A few features of MM may not work on BB (let me know of any; I know that (a,b):=(c,d) not working right now

### Generating DLL Files

Use -dll option to generate .dll instead of .exe files. To export functions from M programs, they must be marked **export** not **global**. **global** will share functions across modules, but not outside the program. **export** will do both.

**export** can also be applied to types, named constants and macros; these are not physically part of the DLL files, so are handled by the language.

It can also applied to variables, but M at present has problems importing variables from DLL files.

The -dll option also creates a .exp file, which is an M source file (with extension .exp to avoid overwriting a .m module), containing an **importdll** block.

Example of an M program called bignum.m (a single module, but libraries can be multi-module too):

    bb -dll bignum
    
This creates bignum.dll and bignum.exp

Then the DLL can be easily used within another M program by writing:

    importx bignum

This imports bignum.exp, and automatically adds bignum.dll to the list of external libraries. If 'import bignum' was used instead, it would just build the library implementation bignum.m as part of the application.

he new option -docs would produce a text file (eg. bignum.txt) containing all the exported functions plus their doc-strings. So a function like:
````
# One
function func(int a,b,c)int =
# Two
# Three
....
end
````
Shows the 3 lines of comments next to the function signature. (Not complete; only the function name is shown, not the signature.)




