(Reply to request to see compiler sources. Below is the example posted and adapted for reddit. To see real sources, some older versions of projects are in [sources](sources). Look for `record unitrec`, since it exists in most (not 'aa' or 'px').)

Well, it's not closed source but it's messy, and it uses crude, ad hoc, manually managed unions (not the latest sum types and tagged unions of Rust etc) that most here will look down upon.

I doubt one can gain much insight from browsing sources of substantial applications, since what's needed is in the bigger picture, not the minutiae of source code. Plus the complexities of a real compiler will obscure things anyway.

However, below is the AST record for my current project:

- Original comments elided (tend to be dated and misleading)
- This AST node describes only executable code (not declarations or types)
- One node occupies exactly 64 bytes, and is called here a 'unit'
- symbol refers to a symbol table entry
- The type attribute discussed is in the field mode.
 
The latter is partly filled in during parsing (and many need to be tentative, because of out-of-order definitions of the language).

The next pass resolves names, and fills in a few more types (so that the node for a for example is filled in as i64 from its symbol table entry, once that is known).

The third pass is type analysis that fills in the rest. That ensures the types of `a` and `b` of my example are compatible (converted and/or promoted if not), and fills in the result of a + b in the node for binary add. (The final AST for this is shown below too.)
```
global type unit = ref unitrec    # for convenience

global record unitrec =
    byte tag
    byte simple
    [2]byte spare
    word32 pos: (sourceoffset:24, fileno:8)

    unit nextunit

# basic A, B, C subtrees (interpretation depends on tag info tables)
# Each can be a linked list for variable length constructs

    union
        struct
            union
                unit    a
                symbol  def
                symbol  labeldef
                int64   value
                word64  uvalue
                real64  xvalue
                ichar   svalue
                int64   range_lower
            end

            union
                unit    b
                int64   range_upper
            end

            unit        c
        end
        [3]unit abc             # allow access via index 1,2,3
    end

    union                       # variable stuff depends on tag
        struct
            word32 slength
            byte isastring
        end

        struct
            byte dottedname
            byte avcode
        end

        union
            struct              # (for inline assembler)
                byte reg
                byte regix
                byte scale
                byte prefixmode

                byte regsize
                byte cond
                byte spare2,spare3
            end
            word64 reginfo
        end

        union
            word32 length
            byte makearray
        end
        byte addroffirst

        word32 offset
        int32 whenlabel
        int32 swapvar

        struct
            union
                int16 bitopindex
                int16 opcindex
                int16 fnindex
                int16 condcode
                int16 asmopcode
                int16 bfcode
            end
        end
        int32 index
        [4]byte cmpgenop
    end

    int32 mode
    int32 convmode
    byte moduleno
    byte subprogno
    byte initlet
    byte isconst
    byte resultflag
    byte pclop
    byte istrueconst
    byte memmode
end
```
AST for `a + b` when both are `i64` (inside a function `main` inside module `u`):
```
i64---- - - 1 bin: <kadd>
i64---- - - - 1 name:a frameid u.main.a
i64---- - - - 2 name:b frameid u.main.b
```
