This post is inspired by the other thread Macros.

My relationship with macros has not been positive. I've generally managed to do without them completely other than crude versions I detail below. As typically implemented in other languages, they are just too complicated, too powerful (sometimes more powerful than the host language), and encourages people to show off. I just don't like the idea of having two languages in one: is this bit of code in the host language, or the macro language, or a third one implemented with macros?

Yet, there are quite a few uses-cases for macros that would be worthwhile; is a macro scheme possible that would allow these, while keeping a tight rein on the possibilities, and have an easy-to-understand scheme at the same time? I will show two attempts.

**Simple Textual Substitution**

I've played with *parameterless* versions, which just allow an alias for a bit of text. This was very sparingly used, but I considered too crude: the macro names were program-wide and lived outside the identifier scope levels.

When I started using whole-program compilers, with then indeterminate module loading order, it started to break down, and the scheme was no longer used.

**Expression Term Substition**

This is my current scheme. The point of these macros is to provide, with optional parameters, aliases for a term of an expression. The syntax to define one is:

    macro M1 = unit
    macro M2(x, y) = unit

where 'unit', in the parlance of my language, is a single expression or statement. When `M1` is expanded in bit of code, then it is replaced by the contents of that unit. When `M2(A, B)` is expanded, then additionally any instances of `x` or `y` in the unit are replaced by expressions `A` and `B`.

That's pretty much it. A macro name `M` follows normal scope rules like every other identifier. Scope resolutions of any names in arguments `A` and `B` works the same as a normal function call. Resolving other names within the expanded unit:

    macro M3 = z

is done according to which `z` names are visible at the point of invocation. ('Lexical scope'? I don't know the proper terms).

That's pretty much it.

**Restrictions**

* While a macro body is a single expression (or *unit*), multiple units can be written like this: `(unit; unit; unit)` and can span multiple lines.
* In my language, declarations and definitions are not classed as statements. So the body of a macro cannot create new names, or new ST entries. (So no local variables can be defined within a macro name).
* Macro names can only be expanded when they are top level names. So if `M` is a macro, then `M` is expanded, but not `A.M` when `A` is a record. The latter has too many issues: if `P` and `Q` where records of different types but both had a member `M`, both would be overridden by the macro.
* (Resolving `A.B` when `A` is a module or other namespace should work, (but I need to make a tweak for that)
* Like some other schemes, macro arguments can be evaluated more than once.

** Use-Cases **

* Simple aliasing or shortening of names:

    macro M = A
    macro M = A.B

* Aliasing of expression terms

    macro M = a + b -1
    macro pr(a, b) = a<<16 + b
    
* Aliasing of bitfields: instead of writing `A.[0..31]` I can do this so that I can write `A.[lowword]` (`A.B` restrictions mean this cant be written so that I can do `A.lowword`):

    macro lowword = 0..31         # a range is a valid term
    (macro lowword = \[0..15\]    # also valid, but as I explained I can't then write A.lowword)

* Inlining small functions: I don't have inlining in my compilers. Macros can be used for that purpose.

* Providing macro facilities for my in-line assembler:

    macro pushconst(a) =
        assem
            mov push a 
        end

    pushconst(1)                 # invoke from HLL code
    assem
        \*pushconst 1            # invoke from assembly
    end

So not that powerful, but then my needs are also simple. I don't need it for language-building.
