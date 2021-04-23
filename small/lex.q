import parser

var sourcecode
var lsptr
const etx=26

var namestarterset = ['A'..'Z','a'..'z','$','_']
var digitset       = ['0'..'9']
var nameset        = namestarterset+digitset

global proc lexinit(source)=
    sourcecode::=source+chr(etx)
    lsptr:=&sourcecode
end

global proc lex=
    do
        case c:=lsptr++^
        when ' ', '\t' then
             next all
        when namestarterset then
            tokenvalue:=chr(c)

            docase c:=lsptr^
            when nameset then
                tokenvalue +:= c
                ++lsptr
            else
                exit
            end docase

            case tokenvalue
            when "let" then   token:=tk_let
            when "print" then token:=tk_print
            else              token:=tk_name
            esac

        when digitset then
            s:=chr(c)
            docase c:=lsptr^
            when digitset,'.' then
                s +:= c
                ++lsptr
            else
                exit
            end docase

            tokenvalue:=longint(s)
            token:=tk_const

        elseswitch c
        when '+' then
            token:=tk_plus
        when '-' then
            token:=tk_minus
        when '*' then
            if lsptr^='*' then
                ++lsptr
                token:=tk_power
            else
                token:=tk_times
            fi
        when '/' then
            token:=tk_divide
        when '=' then
            token:=tk_equals
        when '(' then
            token:=tk_lbrack
        when ')' then
            token:=tk_rbrack
        when etx then
            --lsptr
            token:=tk_eof
        when 13 then
            ++lsptr
            token:=tk_newline
        when 10 then
            token:=tk_newline
        when '"' then
            tokenvalue:=""
            docase c:=lsptr^
            when '"' then
                ++lsptr
                if lsptr^='"' then
                    tokenvalue+:='"'
                    ++lsptr
                else
                    exit
                fi
            when 13,10, etx then
                serror("String not terminated")
            else
                tokenvalue+:=c
                ++lsptr
            end docase
            token:=tk_const
        when '#' then
            docase c:=lsptr^
            when 13,10 then
                next all
            when etx then
                token:=tk_eof
                exit
            else
                ++lsptr
            end docase
        when ',' then
            token:=tk_comma
        else
            token:=tk_error
        end
        return
    od
end
