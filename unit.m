global type unit = ref unitrec

global record unitrec =
    union
        struct
            int16 tag
            union
                byte elemtype           !for array constructors
            end
            union
                byte nparams
                byte enumindex
            end
            int32 pos: (sourceoffset:24, moduleno:8)
        end
        ref void dummy
    end

    unit nextunit

    union
        struct
            union
                unit a
                symbol def
                symbol labeldef
                int64 value
                word64 uvalue
                real64 xvalue
                ichar svalue
                int64 range_lower
                int64 qlower
                int pclopcode
            end
            union
                unit b
                int64 range_upper
                int64 qupper
                int64 slength
                int16 mode
                [4]byte cmpgenop
                struct
                    int32 length
                    int32 lower
                end
                int64 index
            end
        end
        int128 value128
    end
end
