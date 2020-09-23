osboxes@osboxes:~$ cat test.q
function setint(a,b)=
    c:=()
    forall x in a when x in b do
        c append:=x
    od
    return c
end

proc start=
    a:=(20,30,40, 50, 60,10)
    b:=(30,60,70,10)
    println setint(a,b)

    a:=[10,20..40,50]
    b:=[10,30..55,60]
    println a iand b
end

osboxes@osboxes:~$ ./qq test
Compiling test.q to test.pc
(30,60,10)
[10,30..40,50]
