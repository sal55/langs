import clib
const size = 819000

function sieve(int n)int =
    int i, k, prime, count
    [size]byte flags

    to n do
        count := 0
        for i to size do
            flags[i]:=1
        od

        for i to size do
            if flags[i] then
                prime:=i+i+3
                k:=i+prime
                while k<=size, k+:=prime do
                    flags[k]:=0
                od
                ++count
            fi
        od
    od
    return count
end

proc start=
  printf("Sieve (100) = %lld\n", sieve(100))
end
