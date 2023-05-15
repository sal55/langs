func fib(int n)int=
	if n<3 then
		1
	else 
		fib(n-1)+fib(n-2)
	fi
end

proc main=
	for i to 36 do
		println i,fib(i)
	od
end


