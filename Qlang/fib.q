function fib(n)=
    if n<3 then
        return 1
    else 
        return fib(n-1)+fib(n-2)
    fi
end

proc start=
    for i:=1 to 20 do
        println i,fib(i)
    od
end
