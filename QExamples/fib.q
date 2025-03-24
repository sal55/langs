func fib(n)=
	if n<3 then
		1
	else 
		fib(n-1)+fib(n-2)
	fi
end

for i to 34 do
	println i,fib(i)
od
