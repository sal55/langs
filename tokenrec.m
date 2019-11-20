!extract from cc_decls to help with cc_lex.m
!could not be inserted there, to avoid affecting the line number cross-references

global record tokenrec =        !should be 32-byte record
    union
        var int64 value             !64-bit int
        var real xvalue             !64-bit float
        var word64 uvalue           !64-bit word
        var ref char svalue         !pointer to string or charconst (not terminated)
        var ref strec symptr        !pointer to symbol table entry for name
    end
    var ref tokenrec nexttoken

    union
        struct
            var byte subcode
            var byte flags
        end
        var word16 subcodex
    end
    var byte symbol
    var byte fileno

    var word32 lineno

    var int32 length                    !length of name/string/char
    union
        var int32 numberoffset      !offset of numeric token within file[fileno]
        var int16 paramno               !for macro params
        var int16 pasteno
    end
end
