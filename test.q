record rec =
	var name, tel
end

f:=openfile(sread("n"))

phonelist::=()

while not eof(f) do
	readln @f, name:"s", tel:"s"
	nextloop when name=""

	phonelist &:= rec(name, tel)
end

closefile(f)

sort(phonelist)

for i,x in phonelist do
	fprintln "#. # #", i:"3", x.name:"15jl", x.tel
end

proc sort(data)=
	repeat
		swapped:=0
		for i:=1 to data.len-1 do
			if data[i].name>data[i+1].name then
				swapped:=1
				swap(data[i], data[i+1])
			end
		end
	until not swapped
end
