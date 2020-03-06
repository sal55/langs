
function fibonacci(n)
    if n<3 then
        return 1
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

for n = 1, 40 do
    io.write(n," ",fibonacci(n), "\n")
end
io.write("...\n")
