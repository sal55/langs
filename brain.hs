
program:="++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."+chr(0)
data:=new(array,byte, 30000,0)

pcptr:=&program
dataptr:=&data

doswitch pcptr++^
when '>' then ++dataptr
when '<' then --dataptr
when '+' then ++(dataptr^)
when '-' then --(dataptr^)
when '.' then print chr(dataptr^)
when ',' then dataptr^:=waitkey()
when '[' then
    if dataptr^=0 then
        count:=0
        docase pcptr++^
        when '[' then ++count
        when ']' then
            if count=0 then ++pcptr; exit fi
            --count
        esac
    fi
when ']' then
    if dataptr^ then
        count:=0
        pcptr-:=2
        docase pcptr--^
        when ']' then ++count
        when '[' then
            if count=0 then ++pcptr; exit fi
            --count
        end
    fi
when 0 then
    exit
end doswitch
