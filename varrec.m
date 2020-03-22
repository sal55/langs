global record varrec =
    union
        struct
            union
                struct
                    word16  tag
                    byte    hasref
                    byte    spare1
                end
                word32      tagx
            end
            word32          spare2
            union
                int64       value
                real64      xvalue
                word64      uvalue
                struct
                    int32   range_lower         !short range
                    int32   range_upper
                end
                object      objptr              !objects where hasref=1
                variant     varptr              !for refvar
                ref byte    refptr              !for refproc etc
            end
        end

        exceptionrec            uexcept
        returnrec               uret
        refrec                  uref
        operatorrec             uop
        iterrec                 uiter

    end
end
