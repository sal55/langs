import clib
import msys
import mlib
import parser

ichar sourcecode
ref char lsptr
const etx=26

global proc lexinit(ichar source)=
    int length:=strlen(source)
    sourcecode:=pcm_alloc(length+3)
    strcpy(sourcecode, source)
    (sourcecode+length)^:=etx
    (sourcecode+length+1)^:=0

    lsptr:=sourcecode
end

global proc lex=
    static[1024]char str
    int c, sx

    do
        switch c:=lsptr++^
        when ' ', '\t' then
             next all
        when 'A'..'Z','a'..'z','$','_' then
            str[1]:=c
            sx:=1

            doswitch c:=lsptr^
            when 'A'..'Z','a'..'z','$','_','0'..'9' then
                str[++sx]:= c
                ++lsptr
            else
                exit
            end

            str[++sx]:=0

            if eqstring(str,"let") then
                token:=tk_let
            elsif eqstring(str,"print") then
                token:=tk_print
            else
                token:=tk_name
                tokensvalue:=str
            fi

        when '0'..'9' then
            str[1]:=c
            sx:=1
            doswitch c:=lsptr^
            when '0'..'9','.' then
                str[++sx]:=c
                ++lsptr
            else
                exit
            end
            str[++sx]:=0

            tokenvalue:=strtoint(&.str)
            token:=tk_const

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
!       when '"' then
!           tokenvalue:=""
!           docase c:=lsptr^
!           when '"' then
!               ++lsptr
!               if lsptr^='"' then
!                   tokenvalue+:='"'
!                   ++lsptr
!               else
!                   exit
!               fi
!           when 13,10, etx then
!               serror("String not terminated")
!           else
!               tokenvalue+:=c
!               ++lsptr
!           end docase
!           token:=tk_const
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
