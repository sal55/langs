
proc lexm =
!wrapper around lexreadtoken that applies macro expansion to names
    ref strec d
    static int doreset=0
    int newlineno

    do
        if tkptr then
            nextlx:=tkptr^
            tkptr:=tkptr.nexttoken
            if tkptr=nil then

                if nextlx.symbol=namesym and nextlx.symptr.nameid=macroid and peeklb() then
                    setfileno(sfileno)
                    nextlx.lineno:=slineno
                    doreset:=0
                    goto TEST1
                fi
                doreset:=1

            fi
            return
        fi

        if doreset then
            setfileno(sfileno)
            nextlx.lineno:=slineno
            doreset:=0
        fi

        if firstsymbol then
            firstsymbol:=0
            dospecialinclude()
        fi  
        lexreadtoken()
    TEST1::

        case nextlx.symbol
        when lexhashsym then

            if dolexdirective() then
                return
            fi

            next
        when namesym then
            d:=nextlx.symptr
            case d.symbol
            when predefmacrosym then

                sfileno:=getfileno()
                slineno:=nextlx.lineno
                expandpredefmacro(d.subcode,&nextlx,slineno)
                doreset:=1                  !can screw up line/file numbers
                return
            else
                if d.nameid<>macroid or noexpand then
                    return
                fi
            esac
        else
            return
        esac

!have a macro. Now see whether this should be expanded
        sfileno:=getfileno()
        slineno:=nextlx.lineno
        if d.attribs.ax_flmacro then                !function-like macro; need to peek for "("
            if not peeklb() then
                return
            fi
            tkptr:=expandfnmacro(d,nil,normaltk,1,newlineno)
            slineno:=newlineno
        else                                        !object-like macro: always expand
            tkptr:=expandobjmacro(d,nil,normaltk,1)
        fi

        if tkptr=nil then doreset:=1 fi             !immediately restore file/lineno

    od
end
